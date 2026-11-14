#include "build_internal.h"
#include "compiler/compiler_internal.h"

#define MANIFEST_FILE "manifest.json"

const char *manifest_default_keys[][2] = {
		{"authors", "Authors of the library."},
		{"langrev", "Version of the C3 language used."},
		{"version", "Version of the library."},
		{"sources", "Paths to library sources for targets, such as interface files."},
		{"c-sources", "Set the C sources to be compiled."},
		{"c-include-dirs", "Set the include directories for C sources."},
		{"cc", "Set C compiler."},
		{"cflags", "C compiler flags."},
		{"dependencies", "List of C3 libraries to also include."},
		{"exec", "Scripts run for all platforms."},
		{"linklib-dir", "Set the directory where to find linked libraries."},
		{"templates", "Predefined target configurations for users of this library."},
		{"provides", "The library name"},
		{"targets", "The map of supported platforms, if missing the library supports all platforms."},
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
		{"cc", "Set C compiler."},
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

static const char *manifest_deprecated_target_keys[] = { "none" };
static const int manifest_deprecated_target_key_count = ELEMENTLEN(manifest_deprecated_target_keys);

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
	BuildParseContext context = { library->dir, target_name, false };
	target->link_flags = get_string_array(context, json, "link-args", false);

	target->linked_libs = get_string_array(context, json, "linked-libraries", false);
	target->dependencies = get_string_array(context, json, "dependencies", false);
	convert_library_paths_to_plain_library_names(target->dependencies);
	target->execs = get_string_array(context, json, "exec", false);
	target->cc = get_string(context, json, "cc", library->cc);
	target->cflags = get_cflags(context, json, library->cflags);
	target->source_dirs = library->source_dirs;
	target->csource_dirs = library->csource_dirs;
	target->cinclude_dirs = library->cinclude_dirs;
	target->win_crt = (WinCrtLinking)get_valid_string_setting(context, json, "wincrt", wincrt_linking, 0, 5, "'none', 'static', 'static-debug', 'dynamic' or 'dynamic-debug'.");
	APPEND_STRING_LIST(&target->source_dirs, "sources");
	APPEND_STRING_LIST(&target->csource_dirs, "c-sources");
	APPEND_STRING_LIST(&target->cinclude_dirs, "c-include-dirs");
}

// Load the info of a library from the registry at `base`.
const char *load_fetch_info(const char *base, const char *name, LibraryInfo *info)
{
	const char *tmp_dir = dir_make_temp_dir();
	if (!tmp_dir) error_exit("Failed to create temp directory");
	const char *tmp_file = file_append_path(tmp_dir, "info.json");
	scratch_buffer_clear();
	scratch_buffer_printf("/info/%s", name);
	const char *error = download_file(base, scratch_buffer_to_string(), tmp_file, false);
	JSONObject *json = NULL;
	if (error) return error;

	size_t size;
	char *json_str = file_read_all(tmp_file, &size);

	JsonParser parser;
	json_init_string(&parser, json_str);
	json = json_parse(&parser);
	if (!json || json->type != J_OBJECT) return "corrupt library info";
	JSONObject *versions = json_map_get(json, "versions");
	if (!versions || versions->type != J_OBJECT) return "corrupt library info (no 'versions')";
	*info = (LibraryInfo) { .versions = versions };
	if (!json_get_string_from_key(json, "latest", &info->latest)) return "corrupt library info (no 'latest')";
	if (!json_get_string_from_key(json, "library", &info->library)) return "corrupt library info (no 'library')";
	if (!json_get_string_from_key(json, "description", &info->description)) return "corrupt library info (no 'description')";
	if (!json_get_string_from_key(json, "updated", &info->updated)) return "corrupt library info (no 'updated')";

	JSONObject *tags = json_map_get(json, "tags");
	if (tags)
	{
		if (tags->type != J_ARRAY) return "corrupt library info (invalid 'tags')";
		info->tags = tags->elements;
	}

	return NULL;
}

static Library *add_library(JSONObject *json, const char *dir, const char **libs, int libs_len)
{
	check_json_keys(manifest_default_keys, manifest_default_keys_count, NULL, 0, json, "library", "--list-manifest-properties");
	Library *library = CALLOCS(Library);
	library->dir = dir;
	BuildParseContext context = { dir, NULL, false };
	const char *provides = get_mandatory_string(context, json, "provides");
	
	if (str_findlist(provides, libs_len, libs) == -1)
	{
		DEBUG_LOG("Library '%s' not included as dependency, skipped.", provides);
		return NULL;
	}
	
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
	convert_library_paths_to_plain_library_names(library->dependencies);
	library->cc = get_optional_string(context, json, "cc");
	library->cflags = get_cflags(context, json, NULL);
	library->win_crt = (WinCrtLinking)get_valid_string_setting(context, json, "wincrt", wincrt_linking, 0, 5, "'none', 'static', 'static-debug', 'dynamic' or 'dynamic-debug'.");
	APPEND_STRING_LIST(&library->source_dirs, "sources");
	APPEND_STRING_LIST(&library->csource_dirs, "c-sources");
	APPEND_STRING_LIST(&library->cinclude_dirs, "c-include-dirs");
	JSONObject *lib_targets = json_map_get(json, "targets");
	if (lib_targets)
	{
		parse_library_type(library, &library->targets, lib_targets);
	}
	else
	{
		// Without any targets the library works for all platforms, e.g. a source library.
		LibraryTarget *library_target = CALLOCS(LibraryTarget);
		library_target->parent = library;
		library_target->arch_os = ARCH_OS_TARGET_DEFAULT;
		library_target->cc = library->cc;
		library_target->cflags = library->cflags;
		library_target->win_crt = library->win_crt;
		library_target->source_dirs = library->source_dirs;
		library_target->csource_dirs = library->csource_dirs;
		library_target->cinclude_dirs = library->cinclude_dirs;
		vec_add(library->targets, library_target);
	}
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
		if (target->arch_os == build_target->arch_os_target || target->arch_os == ARCH_OS_TARGET_DEFAULT)
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

static JSONObject *parse_manifest(const char *lib, const char *manifest_data, const char **error_ref)
{
	JsonParser parser;
	json_init_string(&parser, manifest_data);
	JSONObject *json = json_parse(&parser);
	if (parser.error_message)
	{
		*error_ref = str_printf("error on line %d reading '%s': %s", parser.line, lib, parser.error_message);
		return NULL;
	}
	if (!json)
	{
		*error_ref = str_printf("empty 'manifest.json' for library '%s'", lib);
	}
	return json;
}

INLINE JSONObject* read_manifest(const char *lib, const char *manifest_data)
{
	const char *error;
	JSONObject *json = parse_manifest(lib, manifest_data, &error);
	if (!json) error_exit("Failed to read manifest: %s", error);
	return json;
}

// Like read_library_manifest_for_path, but a broken manifest gives an error instead of exiting.
// A missing manifest returns NULL with a NULL error
JSONObject *load_library_manifest(const char *lib_path, const char **error_ref, const char **manifest_path_ref)
{
	*error_ref = NULL;
	if (file_is_dir(lib_path))
	{
		const char *manifest_path = file_append_path(lib_path, MANIFEST_FILE);
		if (!file_exists(manifest_path)) goto NOT_FOUND;
		size_t size;
		char *manifest_data = file_read_all(manifest_path, &size);
		*manifest_path_ref = manifest_path;
		return parse_manifest(lib_path, manifest_data, error_ref);
	}
	if (!file_exists(lib_path)) goto NOT_FOUND;

	FILE *f = fopen(lib_path, "rb");
	if (!f) goto NOT_FOUND;

	ZipDirIterator iterator;
	ZipFile file;
	const char *error = zip_dir_iterator(f, &iterator);
	while (!error && iterator.current_file < iterator.files)
	{
		error = zip_dir_iterator_next(&iterator, &file);
		if (error || strcmp(file.name, MANIFEST_FILE) != 0) continue;
		char *manifest_data;
		error = zip_file_read(f, &file, (void**)&manifest_data);
		if (error) break;
		fclose(f);
		*manifest_path_ref = lib_path;
		return parse_manifest(lib_path, manifest_data, error_ref);
	}
	fclose(f);
	if (error)
	{
		*error_ref = str_printf("malformed compressed '%s' library: %s", lib_path, error);
		return NULL;
	}
NOT_FOUND:
	return NULL;
}

JSONObject *read_library_manifest_for_path(const char *lib_path, const char **manifest_path_ref)
{
	const char *error;
	JSONObject *manifest = load_library_manifest(lib_path, &error, manifest_path_ref);
	if (error) error_exit("Failed to read manifest: %s", error);
	return manifest;
}

// The library at `lib_path` must have a manifest that provides `name`.
static const char *verify_library_provides(const char *lib_path, const char *name)
{
	const char *manifest_path;
	const char *error;
	JSONObject *manifest = load_library_manifest(lib_path, &error, &manifest_path);
	if (error) return error;
	if (!manifest) return "the library has no manifest";
	JSONObject *provides = manifest->type == J_OBJECT ? json_map_get(manifest, "provides") : NULL;
	if (!provides || provides->type != J_STRING) return "the library manifest has no 'provides'";
	if (!str_eq(provides->str, name)) return str_printf("the library provides '%s', not '%s'", provides->str, name);
	return NULL;
}

// Unpack the library files into `dir`, skipping empty entries and dotfiles. Returns an error string or NULL.
static const char *unpack_zip(FILE *f, const char *dir)
{
	ZipDirIterator iterator;
	ZipFile file;
	const char *error = zip_dir_iterator(f, &iterator);
	while (!error && iterator.current_file < iterator.files)
	{
		error = zip_dir_iterator_next(&iterator, &file);
		if (error) break;
		if (!file.uncompressed_size || file.name[0] == '.') continue;
		if (strstr(file.name, "..")) return "the library contains an invalid file path";
		error = zip_file_write(f, &file, dir, false);
	}
	return error;
}

static inline JSONObject *resolve_zip_library(BuildTarget *build_target, const char *lib, const char **resulting_library)
{
	FILE *f = fopen(lib, "rb");
	if (!f) error_exit("Failed to open library '%s' for reading.", lib);
	ZipDirIterator iterator;

	// Find the manifest and checksum all the files, so that changed sources invalidate the unpacked copy.
	ZipFile file;
	ZipFile manifest_file = { 0 };
	bool manifest_found = false;
	uint32_t crc = 0;
	zip_check_err(lib, zip_dir_iterator(f, &iterator));
	while (iterator.current_file < iterator.files)
	{
		zip_check_err(lib, zip_dir_iterator_next(&iterator, &file));
		crc = crc * 31 + file.file_crc32;
		if (strcmp(file.name, MANIFEST_FILE) != 0) continue;
		manifest_file = file;
		manifest_found = true;
	}
	if (!manifest_found) error_exit("Missing manifest in '%s'.", lib);

	// Read the manifest.
	char *manifest_data;
	zip_check_err(lib, zip_file_read(f, &manifest_file, (void**)&manifest_data));

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
	if (file_is_dir(lib_dir))
	{
		size_t len;
		char *data = file_exists(checksum_file) ? file_read_all(checksum_file, &len) : NULL;
		if (data && crc == strtoll(data, &data, 16)) goto DONE;
		file_delete_dir(lib_dir);
	}
	dir_make_recursive(lib_dir_copy);

	zip_check_err(lib, unpack_zip(f, lib_dir));
DONE:
	fclose(f);
	char buf[64];
	size_t len = snprintf(buf, 64, "%x", crc);
	file_write_all(checksum_file, buf, len);
	*resulting_library = lib_dir;
	return json;

}

static const char *unpack_c3l(const char *zip_path, const char *dest_dir)
{
	FILE *f = fopen(zip_path, "rb");
	if (!f) return "Failed to open the downloaded library";
	dir_make(dest_dir);
	const char *error = unpack_zip(f, dest_dir);
	fclose(f);
	if (error) return error;
	return file_exists(file_append_path(dest_dir, MANIFEST_FILE)) ? NULL : "the library has no manifest";
}

// Format is "<namespace>/<name>[@<version>]:<checksum>". Where the checksum must be 64 hex digits.
static const char *extract_library_checksum(const char *text)
{
	const char *colon = strrchr(text, ':');
	if (!colon) return NULL;
	const char *current = colon + 1;
	while (true)
	{
		char c = *(current++);
		if (!c) break;
		if (!char_is_hex(c)) return NULL;
	}
	if (current - colon != 64 + 2) return NULL;
	return colon + 1;
}

// "http://foo.bar.baz:80:namespace/name" -> "namespace/name"
static StringSlice extract_library_name(StringSlice slice)
{
	for (size_t i = slice.len; i > 0; i--)
	{
		if (slice.ptr[i - 1] != ':') continue;
		if (slice.ptr[i] == '/') return slice;
		return (StringSlice) { slice.ptr + i, slice.len - i };
	}
	return slice;
}

// A namespace or version goes into URLs: letters, digits, '_', '-', '.' and '+', starting with a letter or digit.
static bool is_valid_library_path_part(StringSlice part)
{
	if (!part.len || !(char_is_letter(part.ptr[0]) || char_is_digit(part.ptr[0]))) return false;
	for (size_t i = 0; i < part.len; i++)
	{
		char c = part.ptr[i];
		if (!char_is_alphanum_(c) && c != '-' && c != '.' && c != '+') return false;
	}
	return true;
}

static bool is_valid_scheme(const char *url)
{
	return str_start_with(url, "http://") || str_start_with(url, "https://"); // NOLINT
}

static const char *library_path_error(LibraryPathInfo *info, StringSlice namespace_name)
{
	if (info->url && !is_valid_scheme(info->url))
	{
		return str_printf("the url '%s' must start with http:// or https://", info->url);
	}
	if (info->checksum && !info->version) return "a checksum requires a version";
	if (!str_is_valid_lowercase_name(info->name)) return str_printf("invalid library name '%s'", info->name);
	if (info->version && !is_valid_library_path_part(slice_from_string(info->version)))
	{
		return str_printf("invalid version '%s'", info->version);
	}
	while (namespace_name.len)
	{
		if (!is_valid_library_path_part(slice_next_token(&namespace_name, '/'))) return str_printf("invalid namespace in '%s'", info->id);
	}
	return NULL;
}

// A library path has the format "[<url>:]<namespace>/<name>[@<version>][:<checksum>]", or is a plain
// library name. If it is invalid, "error" is set.
LibraryPathInfo parse_library_path(const char *path)
{
	const char *checksum = extract_library_checksum(path);
	StringSlice slice = slice_from_string(path);
	if (checksum) slice.len -= 65;
	StringSlice library = extract_library_name(slice);
	size_t library_len = library.len;
	// After slicing, the "library" will just contain the version (or be empty)
	StringSlice namespace_name_slice = slice_next_token(&library, '@');
	const char *namespace_name = slice_copy(namespace_name_slice);
	// This can be a plain entry fetched and added manually, then you just have "raylib6"
	if (!strchr(namespace_name, '/'))
	{
		return (LibraryPathInfo) {
			.id = path,
			.name = path,
			.error = str_is_valid_lowercase_name(path) ? NULL : "expected a library name or " LIBRARY_PATH_FORMAT,
		};
	}
	LibraryPathInfo info = {
		.use_remote_fetch = true,
		.id = namespace_name,
		.name = strrchr(namespace_name, '/') + 1,
		.version = namespace_name_slice.len < library_len ? slice_copy(library) : NULL,
		.checksum = checksum,
	};
	if (namespace_name_slice.ptr != slice.ptr) info.url = str_copy(path, namespace_name_slice.ptr - slice.ptr - 1);
	info.error = library_path_error(&info, namespace_name_slice);
	return info;
}

const char *library_path_info_to_string(LibraryPathInfo *dependency)
{
	scratch_buffer_clear();
	if (dependency->url) scratch_buffer_printf("%s:", dependency->url);
	scratch_buffer_append(dependency->id);
	if (dependency->version) scratch_buffer_printf("@%s", dependency->version);
	if (dependency->checksum) scratch_buffer_printf(":%s", dependency->checksum);
	return scratch_buffer_copy();
}

void convert_library_paths_to_plain_library_names(const char **libs)
{
	FOREACH_IDX(i, const char *, lib, libs)
	{
		LibraryPathInfo info = parse_library_path(lib);
		if (info.error) error_exit("Invalid dependency '%s': %s.", lib, info.error);
		libs[i] = info.name;
	}
}

void resolve_libraries(BuildTarget *build_target)
{
	DEBUG_LOG("Resolve libraries");
	static const char *c3lib_suffix = ".c3l";
	const char **c3_libs = NULL;
	int libdir_count = vec_size(build_target->libdirs);
	int libs_len = vec_size(build_target->libs);
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
	int lib_count = 0;
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
		Library *library = add_library(json, lib, build_target->libs, libs_len);
		if (!library) continue;
		libraries[lib_count++] = library;
	}
	
	FOREACH(const char *, lib_name, build_target->libs)
	{
		add_library_dependency(build_target, find_library(libraries, lib_count, lib_name), libraries, lib_count);
	}
	for (int i = 0; i < lib_count; i++)
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
			const char **files = target_expand_source_names(library->dir, target->source_dirs, c3_suffix_list, &build_target->object_files, &build_target->llvm_ir_files, 3, true);
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
			const char *c = strchr(arch_os, '-');
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

static const char *c_suffix_list[2] = { ".c", ".m" };
static const char *header_suffix_list[5] = { ".h", ".hh", ".hpp", ".hxx", ".inc" };
static const char *source_lib_doc_files[4] = { "LICENSE", "LICENSE.md", "LICENSE.txt", "README.md" };

// The name inside the library for a path, which must be relative to and inside the project.
static const char *source_lib_path(const char *path)
{
	char *name = str_dup(path);
	for (char *c = name; *c; c++)
	{
		if (*c == '\\') *c = '/';
	}
	while (name[0] == '.' && name[1] == '/') name += 2;
	bool valid = name[0] && name[0] != '/' && name[1] != ':' && strlen(name) < ZIP_MAX_NAME;
	for (const char *part = name; valid && part; part = strchr(part, '/'))
	{
		if (*part == '/') part++;
		if (part[0] == '.' && part[1] == '.' && (part[2] == '/' || !part[2])) valid = false;
	}
	if (!valid)
	{
		error_exit("'%s' cannot be added to a source library, paths must be inside the project directory.", path);
	}
	return name;
}

static const char **source_lib_paths(const char **paths)
{
	const char **result = NULL;
	FOREACH(const char *, path, paths) vec_add(result, source_lib_path(path));
	return result;
}

static void source_lib_add_files(ZipEntry **entries, const char **files)
{
	FOREACH(const char *, file, files)
	{
		ZipEntry entry = { .name = source_lib_path(file), .file_path = file };
		vec_add(*entries, entry);
	}
}

static void manifest_add_string(const char *key, const char *value)
{
	if (!value) return;
	scratch_buffer_printf(",\n  \"%s\": \"", key);
	scratch_buffer_append_in_quote(value);
	scratch_buffer_append_char('"');
}

static void manifest_add_list(const char *key, const char **values)
{
	if (!vec_size(values)) return;
	scratch_buffer_printf(",\n  \"%s\": [", key);
	FOREACH_IDX(i, const char *, value, values)
	{
		scratch_buffer_append(i ? ", \"" : " \"");
		scratch_buffer_append_in_quote(value);
		scratch_buffer_append_char('"');
	}
	scratch_buffer_append(" ]");
}

static int source_lib_entry_compare(const void *a, const void *b)
{
	return strcmp(((const ZipEntry *)a)->name, ((const ZipEntry *)b)->name);
}

void build_source_library(BuildTarget *target)
{
	const char *name = target->output_name ? target->output_name : target->name;
	if (!name) error_exit("A source library needs a name, use '-o <name>' to set it.");
	if (!str_is_valid_lowercase_name(name))
	{
		error_exit("Invalid source library name '%s', the name should only contain lowercase alphanumerical letters and '_'.", name);
	}
	ZipEntry *entries = NULL;

	// The C3 sources.
	const char **sources = target_expand_source_names(NULL, target->source_dirs, c3_suffix_list, NULL, NULL, 3, true);
	if (!vec_size(sources)) error_exit("No sources were found for the source library '%s'.", name);
	source_lib_add_files(&entries, sources);

	// The C sources, with the headers next to them and in the include dirs.
	const char **csources = target_expand_source_names(NULL, target->csource_dirs, c_suffix_list, NULL, NULL, 2, false);
	source_lib_add_files(&entries, csources);
	const char **header_dirs = NULL;
	FOREACH(const char *, csource, csources)
	{
		char *dir;
		if (!file_namesplit(csource, NULL, &dir) || str_findlist(dir, vec_size(header_dirs), header_dirs) != -1) continue;
		vec_add(header_dirs, dir);
		const char **headers = NULL;
		file_add_wildcard_files(&headers, dir, false, header_suffix_list, ELEMENTLEN(header_suffix_list));
		source_lib_add_files(&entries, headers);
	}
	FOREACH(const char *, include_dir, target->cinclude_dirs)
	{
		const char **headers = NULL;
		file_add_wildcard_files(&headers, include_dir, true, header_suffix_list, ELEMENTLEN(header_suffix_list));
		source_lib_add_files(&entries, headers);
	}
	for (size_t i = 0; target->project_dir && i < ELEMENTLEN(source_lib_doc_files); i++)
	{
		const char *doc_file = source_lib_doc_files[i];
		if (!file_exists(doc_file) || file_is_dir(doc_file)) continue;
		ZipEntry entry = { .name = doc_file, .file_path = doc_file };
		vec_add(entries, entry);
	}
	if (vec_size(target->linker_libs) || vec_size(target->link_args))
	{
		WARNING("'linked-libraries' and 'link-args' are not stored in source libraries, prefer using `@link` instead.");
	}

	// Strings must be prepared before the manifest, which uses the scratch buffer.
	const char **authors = NULL;
	if (target->project_dir)
	{
		FOREACH(AuthorEntry, author, target->authors)
		{
			vec_add(authors, author.email ? str_printf("%s <%s>", author.author, author.email) : author.author); // NOLINT
		}
	}
	const char **source_patterns = source_lib_paths(target->source_dirs);
	const char **csource_patterns = source_lib_paths(target->csource_dirs);
	const char **include_patterns = source_lib_paths(target->cinclude_dirs);
	scratch_buffer_clear();
	scratch_buffer_printf("{\n  \"provides\": \"%s\"", name);
	if (target->project_dir)
	{
		manifest_add_string("version", target->version);
		manifest_add_list("authors", authors);
		manifest_add_string("langrev", target->langrev);
	}
	manifest_add_list("dependencies", target->libs);
	manifest_add_list("sources", source_patterns);
	manifest_add_list("c-sources", csource_patterns);
	manifest_add_list("c-include-dirs", include_patterns);
	manifest_add_string("cc", target->cc);
	manifest_add_string("cflags", target->cflags);
	scratch_buffer_append("\n}\n");
	size_t manifest_size = scratch_buffer_len();
	ZipEntry manifest = { .name = MANIFEST_FILE, .data = scratch_buffer_copy(), .size = manifest_size };
	vec_add(entries, manifest);

	// Sort for a stable order, and remove files which were found more than once.
	int total = vec_size(entries);
	qsort(entries, total, sizeof(ZipEntry), source_lib_entry_compare);
	int count = 0;
	for (int i = 0; i < total; i++)
	{
		if (count && str_eq(entries[i].name, entries[count - 1].name)) continue;
		entries[count++] = entries[i];
	}
	vec_resize(entries, count);
	const char *output = file_append_path(target->output_dir, str_cat(name, ".c3l"));
	if (file_exists(output))
	{
		if (file_is_dir(output)) file_delete_dir(output); else file_delete_file(output);
	}
	file_create_folders(output);
	if (target->no_compress)
	{
		for (int i = 0; i < count; i++)
		{
			const char *path = file_append_path(output, entries[i].name);
			file_create_folders(path);
			if (entries[i].file_path)
			{
				file_copy_file(entries[i].file_path, path, true);
				continue;
			}
			if (!file_write_all(path, entries[i].data, entries[i].size)) error_exit("Failed to write '%s'.", path);
		}
	}
	else
	{
		const char *error = zip_create(output, entries, count);
		if (error) error_exit("Failed to create the source library '%s': %s.", output, error);
	}
	OUTF("Source library '%s' created.\n", output);
}

// Download a library to `destination`, unpacking it into a directory of that name unless `decompress` is false.
// A non-NULL `checksum` must match the downloaded file, and a non-NULL `name` the "provides" of its manifest.
static const char *download_library(const char *url, const char *resource, const char *destination, bool progress, bool decompress, const char *checksum, const char *name)
{
	// Work in a unique directory next to the destination: the final move stays on one filesystem,
	// and a failure leaves any existing copy alone.
	char *parent;
	if (!file_namesplit(destination, NULL, &parent)) return "Invalid library destination";
	dir_make_recursive(parent);
	const char *stage_dir = dir_make_temp_dir_in(parent);
	if (!stage_dir) return "Failed to create a temporary directory for the library";
	const char *result = file_append_path(stage_dir, "library.c3l");
	const char *error = download_file(url, resource, result, progress);
	if (error) goto END;
	if (checksum)
	{
		const char *sha256 = sha256_file_hex(result);
		if (!sha256)
		{
			error = "Failed to read the downloaded library";
			goto END;
		}
		if (!str_start_with(checksum, "sha256:"))
		{
			error = str_printf("unsupported checksum '%s'", checksum);
			goto END;
		}
		if (!hex_equals(sha256, checksum + 7))
		{
			error = str_printf("checksum mismatch (expected %s, got sha256:%s)", checksum, sha256);
			goto END;
		}
	}
	if (decompress)
	{
		const char *zip_file = result;
		result = file_append_path(stage_dir, "library");
		error = unpack_c3l(zip_file, result);
		if (error) goto END;
	}
	if (name)
	{
		error = verify_library_provides(result, name);
		if (error) goto END;
	}
	if (!file_replace(result, destination)) error = "Failed to replace the existing library";
	END:
	file_delete_dir(stage_dir);
	return error;
}

static const char *vendor_fetch_single(const char* lib, const char* path, bool progress, bool decompress)
{
	const char *resource = str_printf("/c3lang/vendor/releases/download/latest/%s.c3l", lib);
	const char *destination = file_append_path(path, str_printf("%s.c3l", lib));
	return download_library("https://github.com", resource, destination, progress, decompress, NULL, NULL);
}

// Fetch a registry library as <path>/<name>.c3l: the version of the dependency (the latest if it has
// none) from the registry of the dependency (the default one if it has none).
// A dependency with a checksum must have a version, and match the checksum.
const char *registry_fetch_single(BuildOptions *options, LibraryPathInfo *dependency, const char *path, bool progress)
{
	const char *prefixed_checksum_req = dependency->checksum ? str_cat("sha256:", dependency->checksum) : NULL;
	const char *base = dependency->url ? dependency->url : registry_url(options);
	LibraryInfo info;
	const char *error = load_fetch_info(base, dependency->id, &info);
	if (error) return error;

	// 2. Resolve the version: the requested one, or "latest" from the record.
	const char *version = dependency->version ? dependency->version : info.latest;
	if (!is_valid_library_path_part(slice_from_string(version))) return str_printf("invalid version '%s'", version);
	JSONObject *entry = json_map_get(info.versions, version);
	if (!entry || entry->type != J_OBJECT) return str_printf("no such version '%s'", version ? version : "latest");

	// 3. Verify checksum match.
	const char *json_checksum = NULL;
	json_get_string_from_key(entry, "checksum", &json_checksum);
	if (prefixed_checksum_req)
	{
		// Hex equals will be safe here, even though it steps through the prefix.
		if (json_checksum && !hex_equals(json_checksum, prefixed_checksum_req))
		{
			return str_printf("version '%s' has the checksum %s in the repository, not the requested %s", version, json_checksum, prefixed_checksum_req);
		}
		json_checksum = prefixed_checksum_req;
	}
	const char *destination = file_append_path(path, str_printf("%s.c3l", dependency->name));
	// The repo may say where the library is stored, otherwise the repo serves it itself.
	const char *artifact_url = NULL;
	json_get_string_from_key(entry, "url", &artifact_url);
	if (artifact_url)
	{
		if (!is_valid_scheme(artifact_url))
		{
			return str_printf("invalid download url '%s'", artifact_url);
		}
		return download_library(artifact_url, "", destination, progress, !options->fetch_compressed, json_checksum, dependency->name);
	}
	return download_library(base, str_printf("/download/%s@%s", dependency->id, version), destination, progress, !options->fetch_compressed, json_checksum, dependency->name);
}

void fetch_lib(BuildOptions *options, bool is_vendor, const char **libraries_to_fetch)
{
	if (!download_available())
	{
		error_exit("The '%s' command requires libcurl to download libraries.\n"
				   "Please ensure libcurl is installed on your system.", is_vendor ? "vendor-fetch" : "fetch");
	}
	bool ansi = use_ansi();
	// check if there is a project JSON file (--path has already been applied)
	if (file_exists(PROJECT_JSON5) || file_exists(PROJECT_JSON))
	{
		const char** deps_dirs =  get_project_dependency_directories();
		int num_lib = vec_size(deps_dirs);
		if (num_lib > 0) options->vendor_download_path = deps_dirs[0];
	}
	int count = 0;
	const char** fetched_libraries = NULL;
	int total_libraries = vec_size(libraries_to_fetch);

	for (int i = 0; i < total_libraries; i++)
	{
		const char *lib = libraries_to_fetch[i];
		const char *recorded = lib; // entry added to the project dependency list
		const char *error;
		if (is_vendor)
		{
			printf("Fetching library '%s'...\n", lib);
			(void)fflush(stdout);
			error = vendor_fetch_single(lib, options->vendor_download_path, ansi, !options->fetch_compressed);
		}
		else
		{
			LibraryPathInfo library = parse_library_path(lib);
			if (library.error || !library.use_remote_fetch)
			{
				printf("Skipping invalid library '%s', %s.\n", lib, library.use_remote_fetch ? library.error : "expected " LIBRARY_PATH_FORMAT);
				continue;
			}
			printf("Fetching library '%s'...\n", lib);
			(void)fflush(stdout);
			error = registry_fetch_single(options, &library, options->vendor_download_path, ansi);
			// The project only lists the name: where it came from is up to the user to write down.
			recorded = library.name;
		}
		if (!error)
		{
			vec_add(fetched_libraries, recorded);
			count++;
		}
		else
		{
			if (ansi)
			{
				printf("\033[2K\033[31mFailed to fetch library '%s': %s\033[0m\n", lib, error);
			}
			else
			{
				printf("Failed: '%s'\n", error);
			}
			(void)fflush(stdout);
		}
	}

	if (ansi && total_libraries > 1) printf("\033[2K");

	// add fetched library to the dependency list
	add_libraries_to_project_file(fetched_libraries);

	if (!count)	error_exit("Error: Failed to download any libraries.");
	if (count < vec_size(libraries_to_fetch)) error_exit("Error: Only some libraries were downloaded.");

	if (ansi) printf("\033[32mFetching complete.\033[0m\t\t\n");

}
