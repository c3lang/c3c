#pragma once

typedef struct {
	char *buffer;

	const char *start;
} SuCatalog;

void sucatalog_init(SuCatalog *catalog, const char *path);

const char *sucatalog_next(SuCatalog *catalog);