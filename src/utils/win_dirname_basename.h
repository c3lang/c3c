#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO I made these super quickly so they probably have some bugs from cases I didn't think of.
// A better implementation of these functions may be necessary.

char* basename(char* path)
{
	size_t len = strlen(path);
	size_t lastSlash = 0;
	for (size_t i = len - 2; i > 0; i--)
	{
		if (path[i] == '\\' || path[i] == '/')
		{
			if (i == len)
			{
				continue;
			}
			lastSlash = i;
			break;
		}
		if (i == 0)
		{
			// according to `man 3 basename` if there is no /, the original path should be returned
			char* newStr = (char*)malloc(len);
			strcpy(newStr, path);
			return newStr;
		}
	}
	size_t newSize = len - lastSlash - 1;
	char* newStr = (char*)malloc(newSize);
	strncpy(newStr, path + lastSlash + 1, newSize);
	return newStr;
}

char* dirname(char* path)
{
	size_t len = strlen(path);
	size_t lastSlash = 0;
	for (size_t i = len - 2; i > 0; i--)
	{
		if (path[i] == '\\' || path[i] == '/')
		{
			if (i == len)
			{
				continue;
			}
			lastSlash = i;
			break;
		}
		if (i == 0)
		{
			// according to `man 3 basename` if there is no /, "." should be returned
			char* newStr = (char*)malloc(2);
			strcpy(newStr, ".");
			return newStr;
		}
	}
	// size_t newSize = len - lastSlash - 1;
	char* newStr = (char*)malloc(lastSlash);
	strncpy(newStr, path, lastSlash);
	return newStr;
}