#include "compiler_internal.h"
#include "../utils/json.h"

#define MANIFEST_FILE "manifest.json"

static inline JSONObject *get_mandatory(Library *library, JSONObject *object, const char *key)
{
	JSONObject *value = json_obj_get(object, key);
	if (!value) error_exit("The mandatory '%s' field was missing in '%s'.", library->dir);
	return value;
}

static inline const char *get_mandatory_string(Library *library, JSONObject *object, const char *key)
{
	JSONObject *value = get_mandatory(library, object, key);
	if (value->type != J_STRING) error_exit("Expected string value for '%s' in '%s'.", library->dir);
	return value->str;
}

static inline JSONObject *get_optional_string_array(Library *library, JSONObject *object, const char *key)
{
	JSONObject *value = json_obj_get(object, key);
	if (!value) return NULL;
	if (value->type != J_ARRAY) error_exit("Expected an array value for '%s' in '%s'.", library->dir);
	for (int i = 0; i < value->array_len; i++)
	{
		JSONObject *val = value->elements[i];
		if (val->type != J_STRING) error_exit("Expected only strings in array '%s' in '%s'.", library->dir);
	}
	return value;
}

static inline const char **get_optional_string_array_as_array(Library *library, JSONObject *object, const char *key)
{
	JSONObject *array = get_optional_string_array(library, object, key);
	if (!array || !array->array_len) return NULL;
	const char **array_result = VECNEW(const char*, array->array_len);
	for (size_t i = 0; i < array->array_len; i++)
	{
		vec_add(array_result, array->elements[i]->str);
	}
	return array_result;
}


static inline void parse_provides(Library *library, JSONObject *object)
{
	const char *provides = get_mandatory_string(library, object, "provides");
	if (!str_is_valid_lowercase_name(provides))
	{
		char *res = strdup(provides);
		str_ellide_in_place(res, 32);
		error_exit("Invalid 'provides' module name in %s, was '%s'.", library->dir, json_obj_get(object, "provides")->str);
	}
	library->provides = provides;
}

static inline void parse_execs(Library *library, JSONObject *object)
{
	library->execs = get_optional_string_array_as_array(library, object, "execs");
}
static inline void parse_depends(Library *library, JSONObject *object)
{
	library->depends = get_optional_string_array_as_array(library, object, "depends");
}

static inline void parse_library_run(Library *library, LibraryTarget *target, JSONObject *object)
{
	target->execs = get_optional_string_array_as_array(library, object, "execs");
}
static inline void parse_library_target(Library *library, LibraryTarget *target, JSONObject *object)
{
	target->link_flags = get_optional_string_array_as_array(library, object, "linkflags");
	target->linked_libs = get_optional_string_array_as_array(library, object, "linked-libs");
	target->depends = get_optional_string_array_as_array(library, object, "depends");
}
static inline void parse_library_type(Library *library, LibraryTarget ***target_group, JSONObject *object)
{
	if (!object) return;
	if (object->type != J_OBJECT) error_exit("Expected a set of targets in %s.", library->dir);
	for (size_t i = 0; i < object->member_len; i++)
	{
		JSONObject *member = object->members[i];
		const char *key = object->keys[i];
		if (member->type != J_OBJECT) error_exit("Expected a list of properties for a target in %s.", library->dir);
		LibraryTarget *library_target = CALLOCS(LibraryTarget);
		ArchOsTarget target = arch_os_target_from_string(key);
		if (target == ARCH_OS_TARGET_DEFAULT)
		{
			error_exit("Invalid arch/os '%s' in %s.", key, library->dir);
		}
		library_target->arch_os = target;
		vec_add(*target_group, library_target);
		parse_library_target(library, library_target, member);
	}
}

static Library *add_library(JSONObject *object, const char *dir)
{
	Library *library = CALLOCS(Library);
	library->dir = dir;
	parse_provides(library, object);
	parse_depends(library, object);
	parse_library_type(library, &library->targets, json_obj_get(object, "targets"));
	return library;
}

static Library *find_library(Library **libs, size_t lib_count, const char *name)
{
	for (size_t i = 0; i < lib_count; i++)
	{
		if (strcmp(libs[i]->provides, name) == 0)
		{
			return libs[i];
			break;
		}
	}
	error_exit("Required library '%s' could not be found.\n", name);
}

static void add_library_dependency(Library *library, Library **library_list, size_t lib_count)
{
	if (library->target_used) return;
	LibraryTarget *target_found = NULL;
	VECEACH(library->targets, j)
	{
		LibraryTarget *target = library->targets[j];
		if (target->arch_os == active_target.arch_os_target)
		{
			target_found = target;
			break;
		}
	}
	if (!target_found)
	{
		error_exit("Library '%s' cannot be used with arch/os '%s'.", library->provides, arch_os_target[active_target.arch_os_target]);
	}
	library->target_used = target_found;
	VECEACH(library->depends, i)
	{
		add_library_dependency(find_library(library_list, lib_count, library->depends[i]), library_list, lib_count);
	}
	VECEACH(target_found->depends, i)
	{
		add_library_dependency(find_library(library_list, lib_count, target_found->depends[i]),
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
	json_init_string(&parser, manifest_data, &malloc_arena);
	JSONObject *json = json_parse(&parser);
	if (parser.error_message)
	{
		error_exit("Error on line %d reading '%s':'%s'", parser.line, lib, parser.error_message);
	}
	return json;
}

static inline JSONObject *resolve_zip_library(const char *lib, const char **resulting_library)
{
	FILE *f = fopen(lib, "rb");
	if (!f) error_exit("Failed to open library '%s' for reading.", lib);
	ZipDirIterator iterator;
	const char *error;

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
	scratch_buffer_append(active_target.build_dir ? active_target.build_dir : "_temp_build");
	scratch_buffer_printf("/_c3l/%s_%x/", lib_name, file.file_crc32);
	const char *lib_dir = scratch_buffer_copy();
	dir_make_recursive(scratch_buffer_to_string());
	scratch_buffer_append_char('/');
	char *dir = scratch_buffer_to_string();

	// Iterate through all files.
	zip_check_err(lib, zip_dir_iterator(f, &iterator));
	while (iterator.current_file < iterator.files)
	{
		zip_check_err(lib, zip_dir_iterator_next(&iterator, &file));
		if (file.uncompressed_size == 0 || file.name[0] == '.') continue;
		// Copy file.
		zip_file_write(f, &file, dir, false);
	}
	fclose(f);
	*resulting_library = lib_dir;
	return json;

}
void resolve_libraries(void)
{
	static const char *c3lib_suffix = ".c3l";
	const char **c3_libs = NULL;
	VECEACH(active_target.libdirs, i)
	{
		file_add_wildcard_files(&c3_libs, active_target.libdirs[i], false, &c3lib_suffix, 1);
	}
	JsonParser parser;
	Library *libraries[MAX_LIB_DIRS * 2];
	size_t lib_count = 0;
	VECEACH(c3_libs, i)
	{
		const char *lib = c3_libs[i];
		JSONObject *json;
		if (!file_is_dir(lib))
		{
			json = resolve_zip_library(lib, &lib);
		}
		else
		{
			const char *manifest_path = file_append_path(lib, MANIFEST_FILE);
			size_t size;
			json = read_manifest(lib, file_read_all(manifest_path, &size));
		}
		if (lib_count == MAX_LIB_DIRS * 2) error_exit("Too many libraries added, exceeded %d.", MAX_LIB_DIRS * 2);
		libraries[lib_count++] = add_library(json, lib);
	}
	VECEACH(active_target.libs, i)
	{
		const char *lib_name = active_target.libs[i];
		add_library_dependency(find_library(libraries, lib_count, lib_name), libraries, lib_count);
	}
	for (size_t i = 0; i < lib_count; i++)
	{
		Library *library = libraries[i];
		LibraryTarget *target = library->target_used;
		if (!target) continue;
		file_add_wildcard_files(&active_target.sources, library->dir, false, c3_suffix_list, 3);
		vec_add(active_target.library_list, library);
		const char *libdir = file_append_path(library->dir, arch_os_target[active_target.arch_os_target]);
		if (file_is_dir(libdir)) vec_add(active_target.linker_libdirs, libdir);
		if ((vec_size(library->execs) || vec_size(target->execs)) && active_target.trust_level < TRUST_FULL)
		{
			error_exit("Could not use library '%s' as it requires the 'exec' trust level (it "
			           "is currently %d). Use the '-t' option to enable it.",
					   library->provides, active_target.trust_level + 1);
		}
		FOREACH_BEGIN(const char *exec, library->execs)
			printf("] Execute '%s' for library '%s':", exec, library->provides);
			puts(execute_cmd(exec));
		FOREACH_END();
		FOREACH_BEGIN(const char *exec, target->execs)
			printf("] Execute '%s' for library '%s':", exec, library->provides);
			puts(execute_cmd(exec));
		FOREACH_END();
	}
}