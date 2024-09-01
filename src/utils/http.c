// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "lib.h"

#if PLATFORM_WINDOWS

#include <windows.h>
#include <winhttp.h>

static inline wchar_t *char_to_wchar(const char *str)
{
	size_t size = strlen(str) + 1;
	wchar_t *wc = malloc(sizeof(wchar_t) * size);
	mbstowcs(wc, str, size);
	return wc;
}

const char *download_file(const char *url, const char *resource, const char *file_path)
{

	LPSTR pszOutBuffer;
	bool results = false;
	HINTERNET hSession = NULL,
			hConnect = NULL,
			hRequest = NULL;


	bool is_https = memcmp("https://", url, 8) == 0;
	url = url + (is_https ? 8 : 7);

	// Use WinHttpOpen to obtain a session handle.
	HINTERNET session = WinHttpOpen(L"C3C/1.0", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
									WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

	if (!session) error_exit("Failed to create http session.");
	// Specify an HTTP server.

	wchar_t *wurl = char_to_wchar(url);
	HINTERNET connect = WinHttpConnect(session, wurl, is_https ? INTERNET_DEFAULT_HTTPS_PORT : INTERNET_DEFAULT_HTTP_PORT,
									   0);
	if (!connect) error_exit("Failed to connect to '%s'", url);
	free(wurl);

	// Create an HTTP request handle.
	wchar_t *wresource = char_to_wchar(resource);
	HINTERNET request = WinHttpOpenRequest(connect, L"GET", wresource, NULL,
										   WINHTTP_NO_REFERER,
										   WINHTTP_DEFAULT_ACCEPT_TYPES, is_https ? WINHTTP_FLAG_SECURE : 0);

	if (!connect) error_exit("Failed to connect to '%s'.", url);
	free(wresource);

	FILE* file = fopen(file_path, "w+b");
	if (!file) return str_printf("Failed to open file '%s' for output", file_path);


	// Send a request.
	bool result = WinHttpSendRequest(request, WINHTTP_NO_ADDITIONAL_HEADERS, 0,
									 WINHTTP_NO_REQUEST_DATA, 0, 0, 0);
	bool success = false;
	if (!result || !WinHttpReceiveResponse(request, NULL)) goto END;

	DWORD dwStatusCode = 0;
	DWORD dwSize = sizeof(dwStatusCode);
	if (!WinHttpQueryHeaders(request,
							 WINHTTP_QUERY_STATUS_CODE | WINHTTP_QUERY_FLAG_NUMBER,
							 WINHTTP_HEADER_NAME_BY_INDEX,
							 &dwStatusCode, &dwSize, WINHTTP_NO_HEADER_INDEX))
	{
		error_exit("Failed to get status code when requesting 'http%s://%s%s'\n",
				   (is_https ? "s": ""), url, resource);
	}

	if (dwStatusCode != 200) goto END;

	char buffer[8192];
	while (1)
	{
		DWORD dw_size = 0;
		if (!WinHttpReadData(request, (LPVOID)buffer, sizeof(buffer), &dw_size)) goto END;
		fwrite(buffer, (size_t)dw_size, (size_t)1, file);
		if (!WinHttpQueryDataAvailable(request, &dw_size)) goto END;
		if (!dw_size) break;
	}
	success = true;
END:
	fclose(file);
	WinHttpCloseHandle(request);
	WinHttpCloseHandle(connect);
	WinHttpCloseHandle(session);
	if (!success)
	{
		remove(file_path);
		return str_printf("Failed to retrieve '%s%s' (Status code %d).", url, resource, dwStatusCode);
	}
	return NULL;
}

#elif CURL_FOUND
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
	return fwrite(ptr, size, nmemb, (FILE *)stream);
}

const char *download_file(const char *url, const char *resource, const char *file_path)
{
	CURL *curl_handle = curl_easy_init();
	if (!curl_handle) error_exit("Could not initialize cURL subsystem.");
	FILE* file = fopen(file_path, "w+b");
	if (!file) return str_printf("Failed to open file '%s' for output", file_path);
	const char *total_url = str_printf("%s%s", url, resource);
	curl_easy_setopt(curl_handle, CURLOPT_URL, total_url);
	curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
	curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 0L);
	// Enable this later
	curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);
	curl_easy_setopt(curl_handle, CURLOPT_FAILONERROR, 1L);
	curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);
	curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, file);
	CURLcode result = curl_easy_perform(curl_handle);
	if (curl_easy_perform(curl_handle) != CURLE_OK)
	{

		fclose(file);
		remove(file_path);
		return curl_easy_strerror(result);
	}
	fclose(file);
	return NULL;
}

#endif