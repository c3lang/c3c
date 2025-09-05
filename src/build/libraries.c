#include "build_internal.h"
#include "compiler/compiler_internal.h"

#define MANIFEST_FILE "manifest.json"

const char *manifest_default_keys[][2] = {
		{"sources", "Paths to library sources for targets, such as interface files."},
		{"c-sources", "Set the C sources to be compiled."},
		{"c-include-dirs", "Set the include directories for C sources."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags", "C compiler flags."},
		{"dependencies", "List of C3 libraries to also include."},
		{"exec", "Scripts run for all platforms."},
		{"linklib-dir", "Set the directory where to find linked libraries."},
		{"provides", "The library name"},
		{"targets", "The map of supported platforms"},
		{"vendor", "Vendor specific extensions, ignored by c3c."},
		{"wincrt", "Windows CRT linking: none, static, dynamic."}
};

const int manifest_default_keys_count = ELEMENTLEN(manifest_default_keys);

const char *manifest_target_keys[][2] = {
		{"sources", "Additional library sources to be compiled for this target."},
		{"sources-override", "Paths to library sources for this target, overriding global settings."},
		{"c-sources", "Additional C sources to be compiled for the target."},
		{"c-sources-override", "C sources to be compiled, overriding global settings."},
		{"c-include-dirs", "C source include directories for the target."},
		{"c-include-dirs-override", "Additional C source include directories for the target, overriding global settings."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags", "Additional C compiler flags for the target."},
		{"cflags-override", "C compiler flags for the target, overriding global settings."},
		{"dependencies", "List of C3 libraries to also include for this target."},
		{"exec", "Scripts to also run for the target."},
		{"linked-libraries", "Libraries linked by the linker for this target, overriding global settings."},
		{"link-args", "Linker arguments for this target."},
		{"vendor", "Vendor specific extensions, ignored by c3c."},
		{"wincrt", "Windows CRT linking: none, static, dynamic."}
};


const int manifest_target_keys_count = ELEMENTLEN(manifest_target_keys);

const char *manifest_deprecated_target_keys[] = { "none" };

const int manifest_deprecated_target_key_count = ELEMENTLEN(manifest_deprecated_target_keys);

static inline void parse_library_target(Library *library, LibraryTarget *target, const char *target_name,
                                        JSONObject *json);

static inline void parse_library_type(Library *library, LibraryTarget ***target_group, JSONObject *object)
{
	if (!object) return;
	if (object->type != J_OBJECT) error_exit("Expected a set of targets in %s.", library->dir);
	FOREACH_IDX(i, JSONObject *, member, object->members)
	{
		const char *key = object->keys[i];
		if (member->type != J_OBJECT) error_exit("Expected a list of properties for a target in %s.", library->dir);
		check_json_keys(manifest_target_keys, manifest_target_keys_count, manifest_deprecated_target_keys, manifest_deprecated_target_key_count, member, key, "--list-manifest-properties");
		LibraryTarget *library_target = CALLOCS(LibraryTarget);
		library_target->parent = library;
		library_target->win_crt = WIN_CRT_DEFAULT;
		ArchOsTarget target = arch_os_target_from_string(key);
		if (target == ARCH_OS_TARGET_DEFAULT)
		{
			error_exit("Invalid arch/os '%s' in %s.", key, library->dir);
		}
		library_target->arch_os = target;
		vec_add(*target_group, library_target);
		parse_library_target(library, library_target, key, member);
		if (library_target->win_crt == WIN_CRT_DEFAULT) library_target->win_crt = library->win_crt;
	}
}

static inline void parse_library_target(Library *library, LibraryTarget *target, const char *target_name,
                                        JSONObject *json)
{
	BuildParseContext context = { library->dir, target_name };
	target->link_flags = get_string_array(context, json, "link-args", false);

	target->linked_libs = get_string_array(context, json, "linked-libraries", false);
	target->dependencies = get_string_array(context, json, "dependencies", false);
	target->execs = get_string_array(context, json, "exec", false);
	target->cc = get_string(context, json, "cc", library->cc);
	target->cflags = get_cflags(context, json, library->cflags);
	target->source_dirs = library->source_dirs;
	target->csource_dirs = library->csource_dirs;
	target->cinclude_dirs = library->cinclude_dirs;
	target->win_crt = (WinCrtLinking)get_valid_string_setting(context, json, "wincrt", wincrt_linking, 0, 3, "'none', 'static' or 'dynamic'.");
	APPEND_STRING_LIST(&target->source_dirs, "sources");
	APPEND_STRING_LIST(&target->csource_dirs, "c-sources");
	APPEND_STRING_LIST(&target->cinclude_dirs, "c-include-dirs");
}

static Library *add_library(JSONObject *json, const char *dir)
{
	check_json_keys(manifest_default_keys, manifest_default_keys_count, NULL, 0, json, "library", "--list-manifest-properties");
	Library *library = CALLOCS(Library);
	library->dir = dir;
	BuildParseContext context = { dir, NULL };
	const char *provides = get_mandatory_string(context, json, "provides");
	DEBUG_LOG("Added library %s", provides);
	if (!str_is_valid_lowercase_name(provides))
	{
		char *res = strdup(provides);
		str_elide_in_place(res, 32);
		error_exit("Invalid 'provides' module name in %s, was '%s', the name should only contain alphanumerical letters and '_'.", library->dir, res);
	}
	library->provides = provides;
	library->execs = get_optional_string_array(context, json, "exec");
	library->linklib_dir = get_optional_string(context, json, "linklib-dir");
	library->dependencies = get_optional_string_array(context, json, "dependencies");
	library->cc = get_optional_string(context, json, "cc");
	library->cflags = get_cflags(context, json, NULL);
	library->win_crt = (WinCrtLinking)get_valid_string_setting(context, json, "wincrt", wincrt_linking, 0, 3, "'none', 'static' or 'dynamic'.");
	APPEND_STRING_LIST(&library->source_dirs, "sources");
	APPEND_STRING_LIST(&library->csource_dirs, "c-sources");
	APPEND_STRING_LIST(&library->cinclude_dirs, "c-include-dirs");
	parse_library_type(library, &library->targets, json_map_get(json, "targets"));
	return library;
}

static Library *find_library(Library **libs, size_t lib_count, const char *name)
{
	for (size_t i = 0; i < lib_count; i++)
	{
		if (str_eq(libs[i]->provides, name)) return libs[i];
	}
	error_exit("Required library '%s' could not be found. You can add additional library search paths using '--libdir' in case you forgot one.", name);
	UNREACHABLE
}

static void add_library_dependency(BuildTarget *build_target, Library *library, Library **library_list, size_t lib_count)
{
	if (library->target_used) return;
	LibraryTarget *target_found = NULL;
	FOREACH(LibraryTarget *, target, library->targets)
	{
		if (target->arch_os == build_target->arch_os_target)
		{
			target_found = target;
			break;
		}
	}
	if (!target_found)
	{
		error_exit("Library '%s' cannot be used with arch/os '%s'.", library->provides, arch_os_target[build_target->arch_os_target]);
	}
	library->target_used = target_found;
	FOREACH(const char *, dependency, library->dependencies)
	{
		add_library_dependency(build_target, find_library(library_list, lib_count, dependency), library_list, lib_count);
	}
	FOREACH(const char *, dependency, target_found->dependencies)
	{
		add_library_dependency(build_target,
							   find_library(library_list, lib_count, dependency),
							   library_list,
							   lib_count);
	}
}

INLINE void zip_check_err(const char *lib, const char *error)
{
	if (error) error_exit("Malformed compressed '%s' library: %s.", lib, error);
}

INLINE JSONObject* read_manifest(const char *lib, const char *manifest_data)
{
	JsonParser parser;
	json_init_string(&parser, manifest_data);
	JSONObject *json = json_parse(&parser);
	if (parser.error_message)
	{
		error_exit("Error on line %d reading '%s':'%s'", parser.line, lib, parser.error_message);
	}
	if (!json)
	{
		error_exit("Empty 'manifest.json' for library '%s'.", lib);
	}
	return json;
}

static inline JSONObject *resolve_zip_library(BuildTarget *build_target, const char *lib, const char **resulting_library)
{
	FILE *f = fopen(lib, "rb");
	if (!f) error_exit("Failed to open library '%s' for reading.", lib);
	ZipDirIterator iterator;

	// Find the manifest.
	ZipFile file;
	zip_check_err(lib, zip_dir_iterator(f, &iterator));
	do
	{
		if (iterator.current_file >= iterator.files) error_exit("Missing manifest in '%s'.", lib);
		zip_check_err(lib, zip_dir_iterator_next(&iterator, &file));
		if (strcmp(file.name, MANIFEST_FILE) == 0) break;
	} while (1);

	// Read the manifest.
	char *manifest_data;
	zip_check_err(lib, zip_file_read(f, &file, (void**)&manifest_data));

	// Parse the JSON
	JSONObject *json = read_manifest(lib, manifest_data);

	// Create the directory for the temporary files.
	const char *lib_name = filename(lib);
	scratch_buffer_clear();
	assert(build_target->build_dir);
	scratch_buffer_append(build_target->build_dir);
	scratch_buffer_printf("/unpacked_c3l/%s/", lib_name);
	char *lib_dir = scratch_buffer_copy();
	char *lib_dir_copy = scratch_buffer_copy();
	scratch_buffer_append("checksum.txt");
	const char *checksum_file = scratch_buffer_to_string();
	uint32_t crc = file.file_crc32;
	if (file_is_dir(lib_dir))
	{
		size_t len;
		char *data = file_exists(checksum_file) ? file_read_all(checksum_file, &len) : NULL;
		if (data && crc == strtoll(data, &data, 16)) goto DONE;
		file_delete_dir(lib_dir);
	}
	dir_make_recursive(lib_dir_copy);

	// Iterate through all files.
	zip_check_err(lib, zip_dir_iterator(f, &iterator));
	while (iterator.current_file < iterator.files)
	{
		zip_check_err(lib, zip_dir_iterator_next(&iterator, &file));
		if (file.uncompressed_size == 0 || file.name[0] == '.') continue;
		// Copy file.
		zip_file_write(f, &file, lib_dir, false);
	}
DONE:
	fclose(f);
	char buf[64];
	size_t len = snprintf(buf, 64, "%x", crc);
	file_write_all(checksum_file, buf, len);
	*resulting_library = lib_dir;
	return json;

}
void resolve_libraries(BuildTarget *build_target)
{
	DEBUG_LOG("Resolve libraries");
	static const char *c3lib_suffix = ".c3l";
	const char **c3_libs = NULL;
	unsigned libdir_count = vec_size(build_target->libdirs);
	if (libdir_count)
	{
		FOREACH(const char *, dir, build_target->libdirs)
		{
			DEBUG_LOG("Search %s", dir);
			file_add_wildcard_files(&c3_libs, dir, false, &c3lib_suffix, 1);
		}
	}
	else
	{
		// Default to '.'
		DEBUG_LOG("Search '.'");
		file_add_wildcard_files(&c3_libs, ".", false, &c3lib_suffix, 1);
	}
	Library *libraries[MAX_BUILD_LIB_DIRS * 2];
	size_t lib_count = 0;
	FOREACH(const char *, lib, c3_libs)
	{
		JSONObject *json;
		if (!file_is_dir(lib))
		{
			json = resolve_zip_library(build_target, lib, &lib);
		}
		else
		{
			const char *manifest_path = file_append_path(lib, MANIFEST_FILE);
			size_t size;
			json = read_manifest(lib, file_read_all(manifest_path, &size));
		}
		if (lib_count == MAX_BUILD_LIB_DIRS * 2) error_exit("Too many libraries added, exceeded %d.", MAX_BUILD_LIB_DIRS * 2);
		libraries[lib_count++] = add_library(json, lib);
	}
	FOREACH(const char *, lib_name, build_target->libs)
	{
		add_library_dependency(build_target, find_library(libraries, lib_count, lib_name), libraries, lib_count);
	}
	for (size_t i = 0; i < lib_count; i++)
	{
		Library *library = libraries[i];
		LibraryTarget *target = library->target_used;
		if (!target) continue;
		if (target->win_crt != WIN_CRT_DEFAULT)
		{
			if (build_target->win.crt_linking == WIN_CRT_DEFAULT)
			{
				build_target->win.crt_linking = library->win_crt;
			}
			else if (target->win_crt != build_target->win.crt_linking)
			{
				WARNING("'wincrt' mismatch between resolved build setting ('%s') and library '%s' ('%s'), "
						"library settings will be ignored.",
						wincrt_linking[build_target->win.crt_linking],
						library->dir,
						wincrt_linking[target->win_crt]);
			}
		}
		if (vec_size(target->csource_dirs))
		{
			vec_add(build_target->ccompiling_libraries, target);
		}
		if (target->source_dirs)
		{
			const char **files = target_expand_source_names(library->dir, target->source_dirs, c3_suffix_list, &build_target->object_files, 3, true);
			FOREACH(const char *, file, files)
			{
				vec_add(build_target->sources, file);
			}
		}
		else
		{
			// fallback if sources doesn't exist
			file_add_wildcard_files(&build_target->sources, library->dir, false, c3_suffix_list, 3);
		}
		vec_add(build_target->library_list, library);
		const char *lib_base = library->linklib_dir ? file_append_path_temp(library->dir, library->linklib_dir) : library->dir;
		const char *arch_os = arch_os_target[build_target->arch_os_target];
		char *libdir = file_append_path(lib_base, arch_os);
		if (file_is_dir(libdir))
		{
			vec_add(build_target->linker_libdirs, libdir);
		}
		else
		{
			// Fallback to the arch in case of a common one, e.g. "macos-x64" -> "macos"
			char *c = strchr(arch_os, '-');
			if (c)
			{
				size_t to_remove = strlen(arch_os) - (c - arch_os);
				libdir[strlen(libdir) - to_remove] = 0;
				if (file_is_dir(libdir))
				{
					vec_add(build_target->linker_libdirs, libdir);
				}
			}
		}
		if ((vec_size(library->execs) || vec_size(target->execs)) && build_target->trust_level < TRUST_FULL)
		{
			error_exit("Could not use library '%s' as it requires 'exec' trust level to execute (it "
			           "is currently '%s'). Use the '--trust=full' option to enable it.",
					   library->provides, trust_level[build_target->trust_level]);
		}
		FOREACH(const char *, exec, library->execs)
		{
			OUTF("Execute '%s' for library '%s':", exec, library->provides);
			puts(execute_cmd(exec, false, NULL, 2048));
		}
		FOREACH(const char *, exec, target->execs)
		{
			OUTF("Execute '%s' for library '%s':", exec, library->provides);
			puts(execute_cmd(exec, false, NULL, 2048));
		}
	}
}
