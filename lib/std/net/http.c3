module std::net::http;
/*
enum HttpStatus
{
	PENDING,
	COMPLETED,
	FAILED
}

struct Http
{
	HttpStatus status;
	int status_code;
	String reason;
	String content_type;
	String response_data;
}

fn Http* http_get(String url, Allocator using = allocator::temp())
{
	return null;
}

fn Http* http_post(String url, Allocator using = allocator::temp())
{
	return null;
}

fn void Http.destroy(Http* this)
{
}

fn HttpStatus Http.process(Http* this)
{
	unreachable();
}

// Common across implementations

struct HttpInternal @private
{
	inline Http http;
	Allocator allocator;
	int connect_pending;
	int request_sent;
	char[256] address;
	char[256] request_header;
	char* request_header_large;
	String request_data;
	char[1024] reason_phrase;
	char[256] content_type;

	usz data_size;
	usz data_capacity;
	void* data;
}

fn String! parse_url(String url, String* port, String* resource) @private
{
	if (url[:7] != "http://") return NetError.INVALID_URL?;
	url = url[7..];

	usz end_index = url.index_of(":") ?? url.index_of("/") ?? url.len;
	String address = url[:end_index];
	String end_part = url[end_index..];
	if (!end_part.len)
	{
		*port = "80";
		*resource = {};
		return address;
	}
	switch (end_part[0])
	{
		case ':':
			end_index = end_part.index_of("/") ?? end_part.len;
			end_part[:end_index].to_uint() ?? NetError.INVALID_URL?!;
			*port = end_part[:end_index];
			*resource = url[end_index..];
		case '/':
			*port = "80";
			*resource = end_part;
		default:
			unreachable();
	}
	return address;
}

fn Socket! http_internal_connect(String address, uint port) @private
{
	return tcp::connect_async(address, port)!;
}

fn HttpInternal* http_internal_create(usz request_data_size, Allocator allocator) @private
{
	HttpInternal* internal = allocator.alloc(HttpInternal.sizeof + request_data_size)!!;
	internal.status = PENDING;
	internal.status_code = 0;
	internal.response_data = {};
	internal.allocator = allocator;
	internal.connect_pending = 1;
	internal.request_sent = 0;
//	internal.reason = "";
//	internal.content_type = "";
	internal.data_size = 0;
	internal.data_capacity = 64 * 1024;
	internal.data = allocator.alloc(internal.data_capacity)!!;
	internal.request_data = {};
	return internal;
}

fn Http*! http_get(String url, Allocator allocator = allocator::temp())
{
$if env::WIN32:
	int[1024] wsa_data;
	if (_wsa_startup(1, &wsa_data) != 0) return NetError.GENERAL_ERROR?;
$endif
	uint port;
	String resource;
	String address = parse_url(url, &port, &resource)?;
	Socket socket = tcp::connect(address, port)?;
	HttpInternal* internal = http_internal_create(0, allocator);
	internal.socket = socket;

	char* request_header;
	usz request_header_len = 64 + resource.len + address.len + port.len;
	if (request_header_len < sizeof(internal.request_header))
	{
		internal.request_header_large = null;
		request_header = internal.request_header;
	}
	else
	{
		internal.request_header_large = (char*)allocator.malloc(request_header_len + 1);
		request_header = internal.request_header_large;
	}
	int default_http_port = port == "80";
	sprintf( request_header, "GET %s HTTP/1.0\r\nHost: %s%s%s\r\n\r\n", resource, address, default_http_port ? "" : ":", default_http_port ? "" : port );

	return internal;
}

fn Http*! http_post(String url, Allocator allocator = allocator::temp())
{
$if env::OS_TYPE == OsType::WIN32:
	int[1024] wsa_data;
	if (_wsa_startup(1, &wsa_data) != 0) return NetError.GENERAL_ERROR?;
$endif
	String port;
	String resource;
	String address = parse_url(url, &port, &resource)?;
	Socket socket = http_internal_connect(address, port)?;
	HttpInternal* internal = http_internal_create(0, allocator);
	internal.socket = socket;

	char* request_header;
	uz request_header_len = 64 + resource.len + address.len + port.len;
	if (request_header_len < sizeof(internal.request_header))
	{
		internal.request_header_large = null;
		request_header = internal.request_header;
	}
	else
	{
		internal.request_header_large = (char*)allocator.malloc(request_header_len + 1);
		request_header = internal.request_header_large;
	}
	int default_http_port = port == "80";
	sprintf( request_header, "POST %s HTTP/1.0\r\nHost: %s%s%s\r\nContent-Length: %d\r\n\r\n", resource, address, default_http_port ? "" : ":", default_http_port ? "" : port,
		(int) size );

	internal->request_data_size = size;
	internal->request_data = ( internal + 1 );
	memcpy( internal->request_data, data, size );

	return internal;
}

fn HttpStatus Http.process(Http* http)
{
	HttpInternal* internal = (HttpInternal*)http;
	if (http.status == HttpStatus.FAILED) return http.status;
	if (internal.connect_pending)
	{
		fd_set sockets_to_check;
		FD_ZERO(&sockets_to_check);
		FD_SET( internal->socket, &sockets_to_check );
		struct timeval timeout; timeout.tv_sec = 0; timeout.tv_usec = 0;
		// check if socket is ready for send
		if( select( (int)( internal->socket + 1 ), NULL, &sockets_to_check, NULL, &timeout ) == 1 )
		{
			int opt = -1;
			socklen_t len = sizeof( opt );
			if( getsockopt( internal->socket, SOL_SOCKET, SO_ERROR, (char*)( &opt ), &len) >= 0 && opt == 0 )
			{
				internal->connect_pending = 0; // if it is, we're connected
			}
		}
	}
	if (internal.connect_pending) retur http.status;
	if (!internal.request_sent)
	{
		char* request_header = internal->request_header_large ?
			internal.request_header_large : internal.request_header;
		if (send(internal.socket, request_header, (int) strlen( request_header ), 0) == -1)
		{
			return http.status = FAILED;
		}
		if (internal.request_data_size)
		{
			int res = send(internal.socket, (char const*)internal->request_data, (int) internal->request_data_size, 0 );
			if (res == -1)
			{
				http.status = HTTP_STATUS_FAILED;
				return http.status;
			}
		}
		internal.request_sent = 1;
		return http.status;
	}
	// check if socket is ready for recv
	fd_set sockets_to_check;
	FD_ZERO( &sockets_to_check );
	#pragma warning( push )
	#pragma warning( disable: 4548 ) // expression before comma has no effect; expected expression with side-effect
	FD_SET( internal->socket, &sockets_to_check );
	#pragma warning( pop )
	struct timeval timeout; timeout.tv_sec = 0; timeout.tv_usec = 0;
}
http_status_t http_process( http_t* http )
	{


	while( select( (int)( internal->socket + 1 ), &sockets_to_check, NULL, NULL, &timeout ) == 1 )
		{
		char buffer[ 4096 ];
		int size = recv( internal->socket, buffer, sizeof( buffer ), 0 );
		if( size == -1 )
			{
			http->status = HTTP_STATUS_FAILED;
			return http->status;
			}
		else if( size > 0 )
			{
			size_t min_size = internal->data_size + size + 1;
			if( internal->data_capacity < min_size )
				{
				internal->data_capacity *= 2;
				if( internal->data_capacity < min_size ) internal->data_capacity = min_size;
				void* new_data = HTTP_MALLOC( memctx, internal->data_capacity );
				memcpy( new_data, internal->data, internal->data_size );
				HTTP_FREE( memctx, internal->data );
				internal->data = new_data;
				}
			memcpy( (void*)( ( (uintptr_t) internal->data ) + internal->data_size ), buffer, (size_t) size );
			internal->data_size += size;
			}
		else if( size == 0 )
			{
			char const* status_line = (char const*) internal->data;

			int header_size = 0;
			char const* header_end = strstr( status_line, "\r\n\r\n" );
			if( header_end )
				{
				header_end += 4;
				header_size = (int)( header_end - status_line );
				}
			else
				{
				http->status = HTTP_STATUS_FAILED;
				return http->status;
				}

			// skip http version
			status_line = strchr( status_line, ' ' );
			if( !status_line )
				{
				http->status = HTTP_STATUS_FAILED;
				return http->status;
				}
			++status_line;

			// extract status code
			char status_code[ 16 ];
			char const* status_code_end = strchr( status_line, ' ' );
			if( !status_code_end )
				{
				http->status = HTTP_STATUS_FAILED;
				return http->status;
				}
			memcpy( status_code, status_line, (size_t)( status_code_end - status_line ) );
			status_code[ status_code_end - status_line ] = 0;
			status_line = status_code_end + 1;
			http->status_code = atoi( status_code );

			// extract reason phrase
			char const* reason_phrase_end = strstr( status_line, "\r\n" );
			if( !reason_phrase_end )
				{
				http->status = HTTP_STATUS_FAILED;
				return http->status;
				}
			size_t reason_phrase_len = (size_t)( reason_phrase_end - status_line );
			if( reason_phrase_len >= sizeof( internal->reason_phrase ) )
				reason_phrase_len = sizeof( internal->reason_phrase ) - 1;
			memcpy( internal->reason_phrase, status_line, reason_phrase_len );
			internal->reason_phrase[ reason_phrase_len ] = 0;
			status_line = reason_phrase_end + 1;

			// extract content type
			char const* content_type_start = strstr( status_line, "Content-Type: " );
			if( content_type_start )
				{
				content_type_start += strlen( "Content-Type: " );
				char const* content_type_end = strstr( content_type_start, "\r\n" );
				if( content_type_end )
					{
					size_t content_type_len = (size_t)( content_type_end - content_type_start );
					if( content_type_len >= sizeof( internal->content_type ) )
						content_type_len = sizeof( internal->content_type ) - 1;
					memcpy( internal->content_type, content_type_start, content_type_len );
					internal->content_type[ content_type_len ] = 0;
					}
				}

			http->status =  http->status_code < 300 ? HTTP_STATUS_COMPLETED : HTTP_STATUS_FAILED;
			http->response_data = (void*)( ( (uintptr_t) internal->data ) + header_size );
			http->response_size = internal->data_size - header_size;

			// add an extra zero after the received data, but don't modify the size, so ascii results can be used as
			// a zero terminated string. the size returned will be the string without this extra zero terminator.
			( (char*)http->response_data )[ http->response_size ] = 0;
			return http->status;
			}
		}

	return http->status;
	}


void http_release( http_t* http )
	{
	http_internal_t* internal = (http_internal_t*) http;
	#ifdef _WIN32
		closesocket( internal->socket );
	#else
		close( internal->socket );
	#endif

	if( internal->request_header_large) HTTP_FREE( memctx, internal->request_header_large );
	HTTP_FREE( memctx, internal->data );
	HTTP_FREE( memctx, internal );
	#ifdef _WIN32
		WSACleanup();
	#endif
	}


#endif /* HTTP_IMPLEMENTATION */

/*
revision history:
	1.0     first released version
*/

/*
------------------------------------------------------------------------------
This software is available under 2 licenses - you may choose the one you like.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2016 Mattias Gustavsson
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
*/*/