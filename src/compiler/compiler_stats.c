// Copyright (c) 2026 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "../utils/json.h"
#include "../version.h"
#include "git_hash.h"
#include <time.h>

#if LLVM_AVAILABLE
extern const char *llvm_version;
#endif

#define STATS_SCHEMA_VERSION 1
#define STATS_MAX_PHASES 10
#define STATS_NODES 5
#define MB (1024.0 * 1024.0)

typedef struct
{
	const char *name;
	double time;
	const char *part_of;
} StatsPhase;

typedef struct
{
	const char *name;
	int size;
	int64_t memory;
} StatsNode;

typedef struct
{
	CompilerStats *stats;
	char generated_at[32];
	const char *backend;
	const char *target;
	const char *optimization;
	const char *host_os;
	const char *host_cpu;
	int host_cores;
	int64_t host_memory;
	StatsPhase phases[STATS_MAX_PHASES];
	int phase_count;
	double wall_time;
	double frontend_time;
	double codegen_wall_time;
	bool has_ir;
	bool has_parallel_codegen;
	bool has_module_codegen;
	bool single_module;
	StatsNode nodes[STATS_NODES];
	int64_t node_memory;
	int64_t node_elements;
	int64_t arena_memory;
	int64_t arena_allocations;
	int64_t string_memory;
	int64_t string_allocations;
} StatsReport;

typedef struct
{
	Module *module;
	Decl *generic;
	int count;
} GenericInstanceCount;

static const char *origin_names[STATS_ORIGINS] = {
	[STATS_ORIGIN_USER] = "User code",
	[STATS_ORIGIN_STDLIB] = "Standard library",
	[STATS_ORIGIN_RUNTIME] = "Compiler runtime",
	[STATS_ORIGIN_GENERATED] = "Generated",
};

static const char *optimization_names[] = {
	[OPT_SETTING_O0] = "-O0",
	[OPT_SETTING_O1] = "-O1",
	[OPT_SETTING_O2] = "-O2",
	[OPT_SETTING_O3] = "-O3",
	[OPT_SETTING_O4] = "-O4",
	[OPT_SETTING_O5] = "-O5",
	[OPT_SETTING_OSMALL] = "-Os",
	[OPT_SETTING_OTINY] = "-Oz",
};

static void stats_entry_accumulate(StatsEntry *to, StatsEntry *from)
{
	to->instances += from->instances;
	to->instructions += from->instructions;
	to->lines += from->lines;
	to->analysis_time += from->analysis_time;
	to->ir_gen_time += from->ir_gen_time;
	to->codegen_time += from->codegen_time;
}

void stats_list_add(StatsList *list, StatsEntry entry, bool in_top)
{
	stats_entry_accumulate(&list->total, &entry);
	if (in_top)
	{
		vec_add(list->top, entry);
		return;
	}
	stats_entry_accumulate(&list->other, &entry);
	list->other_count++;
}

StatsOrigin stats_module_origin(Module *module)
{
	const char *name = module->name->module;
	if (strcmp(name, "compiler_rt") == 0 || strncmp(name, "compiler_rt::", 13) == 0) return STATS_ORIGIN_RUNTIME;
	return module_is_stdlib(module) ? STATS_ORIGIN_STDLIB : STATS_ORIGIN_USER;
}

int64_t stats_physical_lines(File *file)
{
	if (!file || !file->contents || !file->content_len) return 0;
	const char *end = file->contents + file->content_len;
	int64_t lines = 0;
	for (const char *c = file->contents; (c = memchr(c, '\n', end - c)); c++) lines++;
	if (end[-1] != '\n') lines++;
	return lines;
}

static void stats_add_file(File *file, StatsOrigin origin)
{
	int64_t lines = stats_physical_lines(file);
	StatsOriginTotals *totals = &compiler.stats.origins[origin];
	totals->lines += lines;
	compiler.stats.source_lines += lines;
	if (file->kind == FILE_KIND_TEXT) return;
	totals->files++;
	compiler.stats.source_files++;
}

static int generic_instance_count_cmp(const void *a, const void *b)
{
	return ((const GenericInstanceCount *)b)->count - ((const GenericInstanceCount *)a)->count;
}

void stats_collect_after_sema(void)
{
	int file_count = vec_size(compiler.context.loaded_sources);
	StatsOrigin *file_origins = ccalloc(file_count ? file_count : 1, sizeof(StatsOrigin));
	bool stdin_added = false;
	FOREACH(Module *, module, compiler.context.module_list)
	{
		StatsOrigin origin = stats_module_origin(module);
		FOREACH(CompilationUnit *, unit, module->units)
		{
			File *file = unit->file;
			if (!file) continue;
			if (file->kind == FILE_KIND_STDIN)
			{
				if (!stdin_added) stats_add_file(file, origin);
				stdin_added = true;
				continue;
			}
			if (file->file_id >= 0 && file->file_id < file_count) file_origins[file->file_id] = origin;
		}
	}
	FOREACH(File *, file, compiler.context.loaded_sources)
	{
		switch (file->kind)
		{
			case FILE_KIND_SOURCE:
				stats_add_file(file, file_origins[file->file_id]);
				break;
			case FILE_KIND_TEXT:
				stats_add_file(file, STATS_ORIGIN_GENERATED);
				break;
			case FILE_KIND_GENERATED:
			case FILE_KIND_STDIN:
				break;
		}
	}
	free(file_origins);
	compiler.stats.types = vec_size(compiler.context.type);

	GenericInstanceCount *counts = NULL;
	FOREACH(Module *, module, compiler.context.module_list)
	{
		FOREACH(Decl *, generic, module->generic_sections)
		{
			int instances = vec_size(generic->generic_decl.instances);
			if (!instances) continue;
			compiler.stats.generic_instances += instances;
			FOREACH_IDX(i, GenericInstanceCount, entry, counts)
			{
				if (entry.generic->generic_decl.id != generic->generic_decl.id) continue; // NOLINT
				counts[i].count += instances;
				goto NEXT;
			}
			vec_add(counts, ((GenericInstanceCount) { .module = module, .generic = generic, .count = instances }));
NEXT:;
		}
	}
	if (!compiler.build.collect_stats_lists) return;
	int entries = vec_size(counts);
	if (!entries) return;
	qsort(counts, entries, sizeof(GenericInstanceCount), &generic_instance_count_cmp);
	for (int i = 0; i < entries; i++)
	{
		StatsEntry entry = { .id = counts[i].generic->generic_decl.id, .instances = counts[i].count };
		bool in_top = i < 10;
		if (in_top)
		{
			scratch_buffer_clear();
			scratch_buffer_append(counts[i].module->name->module);
			scratch_buffer_append(" {");
			FOREACH_IDX(j, const char *, param, counts[i].generic->generic_decl.parameters)
			{
				if (j) scratch_buffer_append(", ");
				scratch_buffer_append(param);
			}
			scratch_buffer_append("}");
			entry.name = scratch_buffer_copy();
		}
		stats_list_add(&compiler.stats.generic_list, entry, in_top);
	}
}

void stats_collect_object_size(const char **obj_files, int count)
{
	for (int i = 0; i < count; i++)
	{
		FILE *file = fopen(obj_files[i], "rb");
		if (!file) continue;
		if (!fseek(file, 0, SEEK_END))
		{
			long size = ftell(file);
			if (size > 0) compiler.stats.object_size += size;
		}
		fclose(file);
	}
}

static const char *optimization_name(void)
{
	OptimizationSetting setting = compiler.build.optsetting;
	if (setting < 0 || setting >= (int)ELEMENTLEN(optimization_names)) return "-";
	return optimization_names[setting];
}

static const char *backend_name(void)
{
	switch (compiler.build.backend)
	{
		case BACKEND_LLVM:
#if LLVM_AVAILABLE
			return str_printf("LLVM %s", llvm_version);
#else
			return "LLVM";
#endif
		case BACKEND_TB:
			return "TB";
		case BACKEND_C:
			return "C";
	}
	UNREACHABLE
}

static const char *str_dup_or_null(const char *str)
{
	return str ? str_dup(str) : NULL;
}

static void stats_report_phases(StatsReport *report)
{
	const CompilerStats *stats = report->stats;
	StatsPhase *phases = report->phases;
	int count = 0;
	double start = 0;
	if (stats->init_time >= 0)
	{
		phases[count++] = (StatsPhase) { "Initialization", stats->init_time - compiler.script_time, NULL };
		if (compiler.script_time > 0) phases[count++] = (StatsPhase) { "Scripts", compiler.script_time, NULL };
		start = stats->init_time;
	}
	if (stats->parsing_time >= 0)
	{
		phases[count++] = (StatsPhase) { "Parsing", stats->parsing_time - start, NULL };
		start = stats->parsing_time;
	}
	if (stats->sema_time >= 0)
	{
		phases[count++] = (StatsPhase) { "Semantic analysis", stats->sema_time - start, NULL };
		if (stats->generic_time > 0) phases[count++] = (StatsPhase) { "Generic instantiation", stats->generic_time, "Semantic analysis" };
		if (compiler.exec_time > 0) phases[count++] = (StatsPhase) { "$exec scripts", compiler.exec_time, "Semantic analysis" };
		start = stats->sema_time;
	}
	report->frontend_time = start;
	if (stats->ir_gen_time >= 0)
	{
		phases[count++] = (StatsPhase) { "IR generation", stats->ir_gen_time - start, NULL };
		start = stats->ir_gen_time;
	}
	if (stats->codegen_time >= 0)
	{
		report->codegen_wall_time = stats->codegen_time - start;
		phases[count++] = (StatsPhase) { "Codegen", report->codegen_wall_time, NULL };
		start = stats->codegen_time;
	}
	if (stats->link_time >= 0)
	{
		phases[count++] = (StatsPhase) { "Linking", stats->link_time - start, NULL };
		start = stats->link_time;
	}
	report->wall_time = start;
	report->phase_count = count;
}

static void stats_report_finalize(StatsReport *report)
{
	size_t arena_memory;
	size_t string_memory;
	int allocations;
	int string_allocations;
	arena_stats(&arena_memory, &allocations, &string_memory, &string_allocations);

	CompilerStats *stats = &compiler.stats;
	*report = (StatsReport) {
		.stats = stats,
		.arena_memory = (int64_t)arena_memory,
		.arena_allocations = allocations,
		.string_memory = (int64_t)string_memory,
		.string_allocations = string_allocations,
	};
	time_t now = time(NULL);
	strftime(report->generated_at, sizeof(report->generated_at), "%Y-%m-%dT%H:%M:%SZ", gmtime(&now));
	report->backend = backend_name();
	report->target = arch_os_target[compiler.build.arch_os_target];
	report->optimization = optimization_name();
	report->host_os = str_dup_or_null(host_os_name());
	report->host_cpu = str_dup_or_null(host_cpu_name());
	report->host_cores = cpus();
	report->host_memory = host_memory();
	stats_report_phases(report);

	report->single_module = compiler.build.single_module == SINGLE_MODULE_ON;
	report->has_ir = compiler.build.backend == BACKEND_LLVM && stats->ir_gen_time >= 0;
	report->has_parallel_codegen = stats->codegen_tasks > 1 && report->codegen_wall_time > 0;
	report->has_module_codegen = report->has_ir && stats->codegen_time >= 0 && !report->single_module;

	StatsNode *nodes = report->nodes;
	nodes[0] = (StatsNode) { "Ast", sizeof(Ast), stats->ast_memory };
	nodes[1] = (StatsNode) { "Decl", sizeof(Decl), stats->decl_memory };
	nodes[2] = (StatsNode) { "Expr", sizeof(Expr), stats->expr_memory };
	nodes[3] = (StatsNode) { "TypeInfo", sizeof(TypeInfo), stats->type_info_memory };
	nodes[4] = (StatsNode) { "SourceLoc", sizeof(SourceLoc), stats->sourceloc_memory };
	for (int i = 0; i < STATS_NODES; i++)
	{
		report->node_memory += nodes[i].memory;
		report->node_elements += nodes[i].memory / nodes[i].size;
	}
}

static double module_total_time(StatsEntry *entry)
{
	return entry->analysis_time + entry->ir_gen_time + entry->codegen_time;
}

// The module that took the most time, skipping the single module codegen entry.
static StatsEntry *stats_largest_module(StatsReport *report)
{
	if (!report->has_module_codegen) return NULL;
	FOREACH_IDX(i, StatsEntry, entry, report->stats->module_time_list.top)
	{
		if (entry.origin != STATS_ORIGIN_NONE) return &report->stats->module_time_list.top[i]; // NOLINT
	}
	return NULL;
}

static int64_t stats_arena_total(StatsReport *report)
{
	return report->node_memory + report->arena_memory + report->string_memory;
}

static const char *stats_share(double part, double total)
{
	return total > 0 ? stats_percent(part / total) : "-";
}

static const char *stats_ir_share(StatsReport *report, int64_t instructions)
{
	return stats_share((double)instructions, (double)report->stats->ir_instructions);
}

static void stats_print_info(const char *name, const char *value)
{
	printf(" %-28s%s\n", name, value);
}

static void stats_print_header(StatsReport *report)
{
	const CompilerStats *stats = report->stats;
	stats_print_info("Compiler", str_printf("c3c %s, commit %.12s", COMPILER_VERSION, GIT_HASH));
	stats_print_info("Backend", stats->linker ? str_printf("%s, linker %s", report->backend, stats->linker) : report->backend);
	stats_print_info("Target", str_printf("%s, %s", report->target, report->optimization));
	scratch_buffer_clear();
	scratch_buffer_append(report->host_os ? report->host_os : "Unknown OS");
	if (report->host_cpu) scratch_buffer_printf(", %s", report->host_cpu);
	scratch_buffer_printf(", %d logical cores", report->host_cores);
	if (report->host_memory) scratch_buffer_printf(", %.0f GB RAM", (double)report->host_memory / (MB * 1024));
	stats_print_info("Host", scratch_buffer_to_string());
	stats_print_info("Input", str_printf("%s files, %s physical lines, including the standard library and generated code",
	                                     stats_int(stats->source_files), stats_int(stats->source_lines)));
}

static void stats_print_summary(StatsReport *report)
{
	CompilerStats *stats = report->stats;
	double total = report->wall_time;
	stats_section("SUMMARY");
	putchar('\n');
	stats_print_info("Wall time", stats_duration(total));
	if (total > 0) stats_print_info("Throughput", stats_fmt("%.1f k physical lines/s", (double)stats->source_lines / total / 1000));
	if (report->frontend_time > 0)
	{
		stats_print_info("Frontend", str_printf("%s, %s %% (initialization, parsing and analysis)",
		                                        stats_duration(report->frontend_time), stats_share(report->frontend_time, total)));
	}
	if (report->has_parallel_codegen)
	{
		stats_print_info("Codegen wall / thread time", str_printf("%s / %s", stats_duration(report->codegen_wall_time),
		                                                          stats_duration(stats->codegen_thread_time)));
		stats_print_info("Effective parallelism", str_printf("%.2f× of %d threads, %d tasks", stats->codegen_thread_time / report->codegen_wall_time,
		                                                     stats->codegen_threads, stats->codegen_tasks));
	}
	stats_print_info("Arena memory", str_printf("%.1f MB, upper bound for the peak, LLVM excluded", (double)stats_arena_total(report) / MB));
	StatsPhase *largest = NULL;
	for (int i = 0; i < report->phase_count; i++)
	{
		StatsPhase *phase = &report->phases[i];
		if (!phase->part_of && (!largest || phase->time > largest->time)) largest = phase;
	}
	if (largest) stats_print_info("Largest phase", str_printf("%s, %s %% of wall time", largest->name, stats_share(largest->time, total)));
	StatsEntry *module = stats_largest_module(report);
	if (module)
	{
		stats_print_info("Largest module", str_printf("%s, %s %% of summed module time", module->name,
		                                              stats_share(module_total_time(module), module_total_time(&stats->module_time_list.total))));
	}
}

static void stats_print_phases(StatsReport *report)
{
	stats_section("PHASES");
	double longest = 0;
	for (int i = 0; i < report->phase_count; i++)
	{
		if (report->phases[i].time > longest) longest = report->phases[i].time;
	}
	stats_table(NULL, false, 26);
	stats_header("Phase", 2, "Wall (ms)", "%");
	for (int i = 0; i < report->phase_count; i++)
	{
		StatsPhase *phase = &report->phases[i];
		const char *name = phase->part_of ? str_printf("  %s", phase->name) : phase->name;
		if (!phase->part_of && strcmp(phase->name, "Codegen") == 0 && report->stats->codegen_threads > 1)
		{
			name = str_printf("Codegen (%d threads)", report->stats->codegen_threads);
		}
		stats_bar_row(name, longest > 0 ? phase->time / longest : 0, 2, stats_ms(phase->time), stats_share(phase->time, report->wall_time));
	}
	stats_table_rule();
	stats_total_row("Total", 2, stats_ms(report->wall_time), "100.0");
	putchar('\n');
	stats_note("Phases are exclusive and add up to the total, indented phases are included in the phase above them.");
	stats_note("Bars are scaled to the longest phase.");
}

static void stats_print_origins(StatsReport *report)
{
	CompilerStats *stats = report->stats;
	bool ir = report->has_ir;
	stats_section("CODE BY ORIGIN");
	stats_table(NULL, false, 22);
	stats_header("Origin", 6, "Files", "Lines", ir ? "Functions" : NULL, ir ? "IR instr." : NULL, ir ? "% IR" : NULL,
	             report->has_module_codegen ? "Codegen (ms)" : NULL);
	StatsOriginTotals total = { .files = 0 };
	for (int i = 0; i < STATS_ORIGINS; i++)
	{
		StatsOriginTotals *origin = &stats->origins[i];
		total.files += origin->files;
		total.lines += origin->lines;
		total.functions += origin->functions;
		total.ir_instructions += origin->ir_instructions;
		total.codegen_time += origin->codegen_time;
		stats_row(0, origin_names[i], 6, stats_int(origin->files), stats_int(origin->lines), stats_int(origin->functions),
		          stats_int(origin->ir_instructions), stats_ir_share(report, origin->ir_instructions), stats_ms(origin->codegen_time));
	}
	stats_table_rule();
	stats_total_row("Total", 6, stats_int(total.files), stats_int(total.lines), stats_int(total.functions),
	                stats_int(total.ir_instructions), stats_ir_share(report, total.ir_instructions), stats_ms(total.codegen_time));
	putchar('\n');
	stats_note("Lines are physical lines, including comments and blank lines, of every parsed file. Generated is");
	stats_note("code from $exec and $expand, which isn't counted as files, the test and benchmark runners and the");
	stats_note("generated main.");
	if (ir)
	{
		stats_note("Functions and IR instructions only count the code that is used, as unused code is removed before");
		stats_note("codegen. IR instructions are counted before optimization.");
	}
	if (report->has_module_codegen) stats_note("Codegen is thread time summed over the modules.");
	if (ir && report->single_module) stats_note("Codegen isn't shown, it isn't done per module with --single-module.");
}

static void stats_print_memory(StatsReport *report)
{
	stats_section("MEMORY");
	double largest = (double)report->node_memory;
	if ((double)report->arena_memory > largest) largest = (double)report->arena_memory;
	if ((double)report->string_memory > largest) largest = (double)report->string_memory;
	if (largest <= 0) largest = 1;

	stats_table("Allocators", false, 26);
	stats_header("Allocator", 2, "Used (KB)", "Allocations");
	stats_bar_row("AST node arenas", (double)report->node_memory / largest, 2, stats_kb(report->node_memory), stats_int(report->node_elements));
	stats_bar_row("Main arena", (double)report->arena_memory / largest, 2, stats_kb(report->arena_memory), stats_int(report->arena_allocations));
	stats_bar_row("String arena", (double)report->string_memory / largest, 2, stats_kb(report->string_memory), stats_int(report->string_allocations));
	stats_table_rule();
	stats_total_row("Total", 2, stats_kb(stats_arena_total(report)),
	                stats_int(report->node_elements + report->arena_allocations + report->string_allocations));
	putchar('\n');
	stats_note("Used is what the compiler's own arenas handed out, KB is 1024 bytes. LLVM and libc allocations are");
	stats_note("not included. The AST node arenas row is the total of the table below, those arenas are freed before");
	stats_note("codegen, the main and string arenas are kept until exit. Bars are scaled to the largest row.");

	int64_t largest_node = 1;
	for (int i = 0; i < STATS_NODES; i++)
	{
		if (report->nodes[i].memory > largest_node) largest_node = report->nodes[i].memory;
	}
	stats_table("AST node arenas", false, 26);
	stats_header("Node", 3, "Elem. size (B)", "Used (KB)", "Elements");
	for (int i = 0; i < STATS_NODES; i++)
	{
		StatsNode *node = &report->nodes[i];
		stats_bar_row(node->name, (double)node->memory / (double)largest_node, 3,
		              stats_int(node->size), stats_kb(node->memory), stats_int(node->memory / node->size));
	}
	stats_table_rule();
	stats_total_row("Total", 3, "", stats_kb(report->node_memory), stats_int(report->node_elements));
}

static void stats_print_output(StatsReport *report)
{
	CompilerStats *stats = report->stats;
	stats_section("OUTPUT");
	stats_table(NULL, false, 44);
	if (report->has_ir) stats_row(0, "Functions compiled", 1, stats_int(stats->functions));
	stats_row(0, "Types", 1, stats_int(stats->types));
	stats_row(0, "Generic instances", 1, stats_int(stats->generic_instances));
	if (report->has_ir) stats_row(0, "LLVM IR instructions, before optimization", 1, stats_int(stats->ir_instructions));
	if (!stats->object_size) return;
	stats_row(0, "Object files (KB)", 1, stats_kb(stats->object_size));
	putchar('\n');
	stats_note("Object files are the objects written by this compile, external objects and the linked output are");
	stats_note("not included.");
}

static void stats_print_other(StatsList *list, int columns, const char *a, const char *b, const char *c, const char *d,
                              const char *e, const char *f)
{
	if (!list->other_count) return;
	stats_row(0, str_printf("Other (%d more)", list->other_count), columns, a, b, c, d, e, f);
}

static void stats_print_generics(StatsReport *report)
{
	StatsList *generics = &report->stats->generic_list;
	StatsList *functions = &report->stats->generic_function_list;
	if (!generics->top && !functions->top) return;
	bool ir = report->has_ir;
	stats_section("GENERICS");
	if (generics->top)
	{
		stats_table("Most instantiated", true, ir ? 38 : 60);
		stats_header("Generic", 5, "Instances", ir ? "IR instr." : NULL, ir ? "% IR" : NULL,
		             ir ? "Analysis (ms)" : NULL, ir ? "IR gen (ms)" : NULL);
		FOREACH_IDX(i, StatsEntry, entry, generics->top)
		{
			stats_row(i + 1, entry.name, 5, stats_int(entry.instances), stats_int(entry.instructions), stats_ir_share(report, entry.instructions), // NOLINT
			          stats_ms(entry.analysis_time), stats_ms(entry.ir_gen_time)); // NOLINT
		}
		StatsEntry *other = &generics->other;
		stats_print_other(generics, 5, stats_int(other->instances), stats_int(other->instructions), stats_ir_share(report, other->instructions),
		                  stats_ms(other->analysis_time), stats_ms(other->ir_gen_time), NULL);
		StatsEntry *total = &generics->total;
		stats_table_rule();
		stats_total_row("Total", 5, stats_int(total->instances), stats_int(total->instructions), stats_ir_share(report, total->instructions),
		                stats_ms(total->analysis_time), stats_ms(total->ir_gen_time));
		if (ir)
		{
			putchar('\n');
			stats_note("The cost is that of the compiled functions of all instances.");
		}
	}
	if (functions->top)
	{
		stats_table("Largest generic functions, all instances together", true, 29);
		stats_header("Function", 6, "Instr.", "Instances", "Avg.", "% IR", "Analysis (ms)", "IR gen (ms)");
		FOREACH_IDX(i, StatsEntry, entry, functions->top)
		{
			stats_row(i + 1, entry.name, 6, stats_int(entry.instructions), stats_int(entry.instances), // NOLINT
			          stats_int(entry.instructions / entry.instances), stats_ir_share(report, entry.instructions), // NOLINT
			          stats_ms(entry.analysis_time), stats_ms(entry.ir_gen_time)); // NOLINT
		}
		StatsEntry *other = &functions->other;
		stats_print_other(functions, 6, stats_int(other->instructions), stats_int(other->instances), "",
		                  stats_ir_share(report, other->instructions), stats_ms(other->analysis_time), stats_ms(other->ir_gen_time));
		StatsEntry *total = &functions->total;
		stats_table_rule();
		stats_total_row("Total", 6, stats_int(total->instructions), stats_int(total->instances), "", stats_ir_share(report, total->instructions),
		                stats_ms(total->analysis_time), stats_ms(total->ir_gen_time));
	}
	if (!ir) return;
	putchar('\n');
	stats_note("IR instructions are counted before optimization, % IR is of all IR instructions. Analysis and IR gen");
	stats_note("are self times, excluding nested instantiations.");
}

static void stats_print_functions(StatsReport *report)
{
	StatsList *functions = &report->stats->function_list;
	if (!functions->top) return;
	stats_section("FUNCTIONS");
	stats_table("Largest by IR instructions", true, 44);
	stats_header("Function", 4, "Instr.", "% IR", "Analysis (ms)", "IR gen (ms)");
	FOREACH_IDX(i, StatsEntry, entry, functions->top)
	{
		stats_row(i + 1, entry.name, 4, stats_int(entry.instructions), stats_ir_share(report, entry.instructions), // NOLINT
		          stats_ms(entry.analysis_time), stats_ms(entry.ir_gen_time)); // NOLINT
	}
	StatsEntry *other = &functions->other;
	stats_print_other(functions, 4, stats_int(other->instructions), stats_ir_share(report, other->instructions),
	                  stats_ms(other->analysis_time), stats_ms(other->ir_gen_time), NULL, NULL);
	StatsEntry *total = &functions->total;
	stats_table_rule();
	stats_total_row("Total", 4, stats_int(total->instructions), stats_ir_share(report, total->instructions),
	                stats_ms(total->analysis_time), stats_ms(total->ir_gen_time));
	putchar('\n');
	stats_note("IR instructions are counted before optimization, % IR is of all IR instructions, which also");
	stats_note("includes globals and module setup. Analysis and IR gen are self times for the function.");
}

static void stats_print_modules(StatsReport *report)
{
	StatsList *sizes = &report->stats->module_size_list;
	StatsList *times = &report->stats->module_time_list;
	if (!sizes->top && !times->top) return;
	stats_section("MODULES");
	if (sizes->top)
	{
		stats_table("Largest by IR instructions", true, 44);
		stats_header("Module", 3, "IR instr.", "% IR", "Lines");
		FOREACH_IDX(i, StatsEntry, entry, sizes->top)
		{
			stats_row(i + 1, entry.name, 3, stats_int(entry.instructions), stats_ir_share(report, entry.instructions), stats_int(entry.lines)); // NOLINT
		}
		StatsEntry *other = &sizes->other;
		stats_print_other(sizes, 3, stats_int(other->instructions), stats_ir_share(report, other->instructions), stats_int(other->lines),
		                  NULL, NULL, NULL);
		StatsEntry *total = &sizes->total;
		stats_table_rule();
		stats_total_row("Total", 3, stats_int(total->instructions), stats_ir_share(report, total->instructions), stats_int(total->lines));
		putchar('\n');
		stats_note("Lines are the physical lines of the files of the module, used or not. A file with several modules");
		stats_note("counts for each of them.");
	}
	if (times->top)
	{
		double summed = module_total_time(&times->total);
		stats_table("Slowest by summed time", true, 30);
		stats_header("Module", 5, "Analysis (ms)", "IR gen (ms)", "Codegen (ms)", "Total (ms)", "%");
		FOREACH_IDX(i, StatsEntry, entry, times->top)
		{
			stats_row(i + 1, entry.name, 5, stats_ms(entry.analysis_time), stats_ms(entry.ir_gen_time), stats_ms(entry.codegen_time),
			          stats_ms(module_total_time(&entry)), stats_share(module_total_time(&entry), summed));
		}
		StatsEntry *other = &times->other;
		stats_print_other(times, 5, stats_ms(other->analysis_time), stats_ms(other->ir_gen_time), stats_ms(other->codegen_time),
		                  stats_ms(module_total_time(other)), stats_share(module_total_time(other), summed), NULL);
		stats_table_rule();
		stats_total_row("Total, summed", 5, stats_ms(times->total.analysis_time), stats_ms(times->total.ir_gen_time),
		                stats_ms(times->total.codegen_time), stats_ms(summed), "100.0");
		putchar('\n');
		stats_note("These are summed thread times, not wall time: codegen runs in parallel, so the total is larger");
		stats_note("than the wall time of the Codegen phase. Analysis only counts function bodies, as declarations");
		stats_note("are analysed for all modules together.");
		if (report->single_module) stats_note("With --single-module all codegen is in the one linked module.");
	}
}

static void stats_print_report(StatsReport *report)
{
	stats_set_colour(use_ansi());
	putchar('\n');
	stats_banner("C3 COMPILER STATISTICS");
	putchar('\n');
	stats_print_header(report);
	stats_print_summary(report);
	stats_print_phases(report);
	stats_print_origins(report);
	stats_print_memory(report);
	stats_print_output(report);
	if (compiler.build.stats == STATS_FULL)
	{
		stats_print_generics(report);
		stats_print_functions(report);
		stats_print_modules(report);
	}
	putchar('\n');
	stats_banner(stats_fmt("TOTAL  %s wall time", stats_duration(report->wall_time)));
	putchar('\n');
}

typedef enum
{
	JSON_INSTANCES = 1,
	JSON_INSTRUCTIONS = 2,
	JSON_LINES = 4,
	JSON_ANALYSIS = 8,
	JSON_IR_GEN = 16,
	JSON_CODEGEN = 32,
} JsonFields;

static void stats_json_entry(FILE *file, StatsEntry *entry, JsonFields fields)
{
	fputs("{ \"name\": ", file);
	json_write_string(file, entry->name);
	if (fields & JSON_INSTANCES) fprintf(file, ", \"instances\": %d", entry->instances); // NOLINT
	if (fields & JSON_INSTRUCTIONS) fprintf(file, ", \"ir_instructions\": %lld", (long long)entry->instructions); // NOLINT
	if (fields & JSON_LINES) fprintf(file, ", \"physical_lines\": %lld", (long long)entry->lines); // NOLINT
	if (fields & JSON_ANALYSIS) fprintf(file, ", \"analysis_ms\": %.3f", entry->analysis_time * 1000); // NOLINT
	if (fields & JSON_IR_GEN) fprintf(file, ", \"ir_gen_ms\": %.3f", entry->ir_gen_time * 1000); // NOLINT
	if (fields & JSON_CODEGEN) fprintf(file, ", \"codegen_ms\": %.3f", entry->codegen_time * 1000); // NOLINT
	fputs(" }", file);
}

static void stats_json_list(FILE *file, const char *key, StatsList *list, JsonFields fields)
{
	fprintf(file, ",\n  \"%s\": {\n    \"top\": [", key);
	FOREACH_IDX(i, StatsEntry, entry, list->top)
	{
		fputs(i ? ",\n      " : "\n      ", file);
		stats_json_entry(file, &entry, fields);
	}
	fputs(list->top ? "\n    ],\n    \"other\": " : "],\n    \"other\": ", file);
	StatsEntry other = list->other;
	other.name = NULL;
	stats_json_entry(file, &other, fields);
	fprintf(file, ",\n    \"other_count\": %d,\n    \"total\": ", list->other_count);
	StatsEntry total = list->total;
	total.name = NULL;
	stats_json_entry(file, &total, fields);
	fputs("\n  }", file);
}

static void stats_json_string(FILE *file, const char *key, const char *value)
{
	fprintf(file, ",\n  \"%s\": ", key);
	json_write_string(file, value);
}

static void stats_write_json(StatsReport *report, FILE *file)
{
	CompilerStats *stats = report->stats;
	fprintf(file, "{\n  \"schema_version\": %d", STATS_SCHEMA_VERSION);
	stats_json_string(file, "generated_at", report->generated_at);
	stats_json_string(file, "compiler_version", COMPILER_VERSION);
	stats_json_string(file, "commit", GIT_HASH);
	stats_json_string(file, "backend", report->backend);
	stats_json_string(file, "linker", stats->linker);
	stats_json_string(file, "target", report->target);
	stats_json_string(file, "optimization", report->optimization);
	stats_json_string(file, "host_os", report->host_os);
	stats_json_string(file, "host_cpu", report->host_cpu);
	fprintf(file, ",\n  \"host_logical_cores\": %d,\n  \"host_memory_bytes\": %lld", report->host_cores, (long long)report->host_memory);
	fprintf(file, ",\n  \"single_module\": %s", report->single_module ? "true" : "false");
	fprintf(file, ",\n  \"wall_time_ms\": %.3f", report->wall_time * 1000);
	fprintf(file, ",\n  \"codegen_wall_ms\": %.3f,\n  \"codegen_thread_ms\": %.3f", report->codegen_wall_time * 1000, stats->codegen_thread_time * 1000);
	fprintf(file, ",\n  \"codegen_tasks\": %d,\n  \"codegen_threads\": %d", stats->codegen_tasks, stats->codegen_threads);
	fprintf(file, ",\n  \"source_files\": %d,\n  \"source_physical_lines\": %lld", stats->source_files, (long long)stats->source_lines);
	fputs(",\n  \"phases\": [", file);
	for (int i = 0; i < report->phase_count; i++)
	{
		fputs(i ? ",\n    { \"name\": " : "\n    { \"name\": ", file);
		json_write_string(file, report->phases[i].name);
		fprintf(file, ", \"wall_ms\": %.3f, \"part_of\": ", report->phases[i].time * 1000);
		json_write_string(file, report->phases[i].part_of);
		fputs(" }", file);
	}
	fputs("\n  ]", file);

	fputs(",\n  \"origins\": [", file);
	for (int i = 0; i < STATS_ORIGINS; i++)
	{
		StatsOriginTotals *origin = &stats->origins[i];
		fputs(i ? ",\n    { \"name\": " : "\n    { \"name\": ", file);
		json_write_string(file, origin_names[i]);
		fprintf(file, ", \"files\": %d, \"physical_lines\": %lld, \"functions\": %d, \"ir_instructions\": %lld, \"codegen_thread_ms\": %.3f }",
		        origin->files, (long long)origin->lines, origin->functions, (long long)origin->ir_instructions, origin->codegen_time * 1000);
	}
	fputs("\n  ]", file);

	fputs(",\n  \"allocators\": [", file);
	fprintf(file, "\n    { \"name\": \"AST node arenas\", \"used_bytes\": %lld, \"allocations\": %lld, \"freed_before_codegen\": true },",
	        (long long)report->node_memory, (long long)report->node_elements);
	fprintf(file, "\n    { \"name\": \"Main arena\", \"used_bytes\": %lld, \"allocations\": %lld, \"freed_before_codegen\": false },",
	        (long long)report->arena_memory, (long long)report->arena_allocations);
	fprintf(file, "\n    { \"name\": \"String arena\", \"used_bytes\": %lld, \"allocations\": %lld, \"freed_before_codegen\": false }",
	        (long long)report->string_memory, (long long)report->string_allocations);
	fputs("\n  ]", file);
	fputs(",\n  \"ast_node_arenas\": [", file);
	for (int i = 0; i < STATS_NODES; i++)
	{
		StatsNode *node = &report->nodes[i];
		fprintf(file, "%s\n    { \"name\": \"%s\", \"element_size\": %d, \"used_bytes\": %lld, \"elements\": %lld }", i ? "," : "",
		        node->name, node->size, (long long)node->memory, (long long)(node->memory / node->size));
	}
	fputs("\n  ]", file);

	fprintf(file, ",\n  \"types\": %d,\n  \"generic_instances\": %d", stats->types, stats->generic_instances);
	if (report->has_ir)
	{
		fprintf(file, ",\n  \"functions_compiled\": %d,\n  \"ir_instructions\": %lld", stats->functions, (long long)stats->ir_instructions);
	}
	else
	{
		fputs(",\n  \"functions_compiled\": null,\n  \"ir_instructions\": null", file);
	}
	fprintf(file, ",\n  \"object_bytes\": %lld", (long long)stats->object_size);
	stats_json_list(file, "generics", &stats->generic_list, JSON_INSTANCES | JSON_INSTRUCTIONS | JSON_ANALYSIS | JSON_IR_GEN); // NOLINT
	stats_json_list(file, "generic_functions", &stats->generic_function_list, JSON_INSTANCES | JSON_INSTRUCTIONS | JSON_ANALYSIS | JSON_IR_GEN); // NOLINT
	stats_json_list(file, "functions", &stats->function_list, JSON_INSTRUCTIONS | JSON_ANALYSIS | JSON_IR_GEN); // NOLINT
	stats_json_list(file, "module_sizes", &stats->module_size_list, JSON_INSTRUCTIONS | JSON_LINES); // NOLINT
	stats_json_list(file, "module_times", &stats->module_time_list, JSON_ANALYSIS | JSON_IR_GEN | JSON_CODEGEN); // NOLINT
	fputs("\n}\n", file);
}

static const char *stats_default_json_filename(void)
{
	char base[64];
	time_t now = time(NULL);
	strftime(base, sizeof(base), "stats-%Y%m%d-%H%M%S", localtime(&now));
	const char *filename = str_printf("%s.json", base);
	for (int i = 2; file_exists(filename); i++) filename = str_printf("%s-%d.json", base, i);
	return filename;
}

static void stats_save_json(StatsReport *report)
{
	const char *filename = compiler.build.stats_file;
	if (!filename) filename = stats_default_json_filename();
	FILE *file = fopen(filename, "w");
	if (!file)
	{
		eprintf("Failed to write the statistics to '%s'.\n", filename);
		return;
	}
	stats_write_json(report, file);
	fclose(file);
	OUTF("Statistics saved to '%s'.\n", filename);
}

void compiler_print_stats(void)
{
	if (!compiler.build.collect_stats || compiler.stats.printed) return;
	compiler.stats.printed = true;
	StatsReport report;
	stats_report_finalize(&report);
	if (compiler.build.stats == STATS_JSON)
	{
		stats_save_json(&report);
		return;
	}
	stats_print_report(&report);
}
