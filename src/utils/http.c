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
	HINTERNET hSession = NULL, hConnect = NULL, hRequest = NULL;

	bool is_https = memcmp("https://", url, 8) == 0;
	const char *hostname_and_path = url + (is_https ? 8 : 7);
	const char *slash = strchr(hostname_and_path, '/');
	const char *hostname = hostname_and_path;
	const char *url_path = "";

	if (slash)
	{
		hostname = str_copy(hostname_and_path, slash - hostname_and_path);
		url_path = slash;
	}

	// Use WinHttpOpen to obtain a session handle.
	hSession = WinHttpOpen(L"C3C/1.0", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
						   WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

	if (!hSession) error_exit("Failed to create http session.");

	DWORD redirect_policy = WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS;
	WinHttpSetOption(hSession, WINHTTP_OPTION_REDIRECT_POLICY, &redirect_policy,
					 sizeof(redirect_policy));

	wchar_t *wurl = char_to_wchar(hostname);
	hConnect = WinHttpConnect(
		hSession, wurl,
		is_https ? INTERNET_DEFAULT_HTTPS_PORT : INTERNET_DEFAULT_HTTP_PORT, 0);
	if (!hConnect) error_exit("Failed to connect to '%s'", url);
	free(wurl);

	// Create an HTTP request handle.
	char *full_resource = str_cat(url_path, resource);
	wchar_t *wresource = char_to_wchar(full_resource);
	hRequest = WinHttpOpenRequest(hConnect, L"GET", wresource, NULL,
                                  WINHTTP_NO_REFERER,
                                  WINHTTP_DEFAULT_ACCEPT_TYPES, is_https ? WINHTTP_FLAG_SECURE : 0);

	if (!hRequest) error_exit("Failed to create request for '%s'.", url);
	free(wresource);

	FILE *file = fopen(file_path, "w+b");
	if (!file) return str_printf("Failed to open file '%s' for output", file_path);

	// Send a request.
	bool result = WinHttpSendRequest(hRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0,
									 WINHTTP_NO_REQUEST_DATA, 0, 0, 0);
	bool success = false;
	DWORD dwStatusCode = 0;

	if (!result || !WinHttpReceiveResponse(hRequest, NULL)) goto END;

	DWORD dwSize = sizeof(dwStatusCode);
	if (!WinHttpQueryHeaders(hRequest,
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
		if (!WinHttpReadData(hRequest, (LPVOID)buffer, sizeof(buffer), &dw_size)) goto END;
		if (dw_size == 0) break;
		fwrite(buffer, (size_t)dw_size, (size_t)1, file);
	}
	success = true;
END:
	fclose(file);
	WinHttpCloseHandle(hRequest);
	WinHttpCloseHandle(hConnect);
	WinHttpCloseHandle(hSession);
	if (!success)
	{
		remove(file_path);
		return str_printf("Failed to retrieve '%s%s' (Status code %d).", url, resource, dwStatusCode);
	}
	return NULL;
}

bool download_available(void)
{
	return true;
}

#elif PLATFORM_POSIX

#include <dlfcn.h>

typedef void CURL;
typedef int CURLcode;
typedef int CURLoption;

#define CURLE_OK 0
#define CURLOPT_URL 10002
#define CURLOPT_USERAGENT 10018
#define CURLOPT_FOLLOWLOCATION 52
#define CURLOPT_VERBOSE 41
#define CURLOPT_NOPROGRESS 43
#define CURLOPT_FAILONERROR 45
#define CURLOPT_WRITEFUNCTION 20011
#define CURLOPT_WRITEDATA 10001
#define CURLOPT_CAINFO 10065

static void *libcurl = NULL;
static CURL* (*ptr_curl_easy_init)(void);
static CURLcode (*ptr_curl_easy_setopt)(CURL *, CURLoption, ...);
static CURLcode (*ptr_curl_easy_perform)(CURL *);
static void (*ptr_curl_easy_cleanup)(CURL *);
static const char* (*ptr_curl_easy_strerror)(CURLcode);

static bool load_curl(void)
{
	if (libcurl) return true;
	const char *names[] = {
#ifdef __APPLE__
		"libcurl.4.dylib",
		"libcurl.dylib",
		"/usr/lib/libcurl.4.dylib",
		"/usr/lib/libcurl.dylib",
		"/opt/homebrew/lib/libcurl.dylib",
		"/usr/local/lib/libcurl.dylib"
#else
		"libcurl.so.4",
		"libcurl.so",
		"libcurl.so.3",
		"libcurl-gnutls.so.4",
		"libcurl-nss.so.4"
#endif
	};

	for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); i++)
	{
		libcurl = dlopen(names[i], RTLD_LAZY);
		if (libcurl) break;
	}

	if (!libcurl) return false;

	ptr_curl_easy_init = dlsym(libcurl, "curl_easy_init");
	ptr_curl_easy_setopt = dlsym(libcurl, "curl_easy_setopt");
	ptr_curl_easy_perform = dlsym(libcurl, "curl_easy_perform");
	ptr_curl_easy_cleanup = dlsym(libcurl, "curl_easy_cleanup");
	ptr_curl_easy_strerror = dlsym(libcurl, "curl_easy_strerror");

	if (!ptr_curl_easy_init || !ptr_curl_easy_setopt || !ptr_curl_easy_perform || !ptr_curl_easy_cleanup || !ptr_curl_easy_strerror)
	{
		dlclose(libcurl);
		libcurl = NULL;
		return false;
	}

	return true;
}

bool download_available(void)
{
	return load_curl();
}

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
	return fwrite(ptr, size, nmemb, (FILE *)stream);
}

const char *download_file(const char *url, const char *resource, const char *file_path)
{
	if (!load_curl())
	{
		return "This build of c3c lacks cURL support and cannot download files automatically.\n"
			   "Please ensure libcurl is installed on your system.";
	}

	CURL *curl_handle = ptr_curl_easy_init();
	if (!curl_handle) return "Could not initialize cURL subsystem.";

	FILE *file = fopen(file_path, "w+b");
	if (!file)
	{
		ptr_curl_easy_cleanup(curl_handle);
		return str_printf("Failed to open file '%s' for output", file_path);
	}

	const char *total_url = str_printf("%s%s", url, resource);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_URL, total_url);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "C3C/1.0");
	ptr_curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 0L);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_FAILONERROR, 1L);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);
	ptr_curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, file);

	CURLcode result = ptr_curl_easy_perform(curl_handle);
	if (result != CURLE_OK)
	{
		fclose(file);
		remove(file_path);
		const char *err_msg = str_dup(ptr_curl_easy_strerror(result));
		ptr_curl_easy_cleanup(curl_handle);
		return err_msg;
	}

	fclose(file);
	ptr_curl_easy_cleanup(curl_handle);
	return NULL;
}

#endif