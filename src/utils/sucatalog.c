#include "sucatalog.h"

#include "lib.h"

void sucatalog_init(SuCatalog *catalog, const char *path)
{
	size_t size = 0;
	catalog->buffer = file_read_all(path, &size);
	catalog->start = catalog->buffer;
}

const char *sucatalog_next(SuCatalog *catalog)
{
	const char *start = strstr(catalog->start, "<array>");
	if (!start) return NULL;

	start += 7;
	char *end = strstr(start, "</array>");
	if (!end) error_exit("Missing </array> in sucatalog");
	*end = 0;
	end += 8;

	catalog->start = end;

	return start;
}
