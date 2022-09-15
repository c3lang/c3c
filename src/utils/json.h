#pragma once
typedef enum
{
	J_OBJECT,
	J_STRING,
	J_ARRAY,
	J_NUMBER,
	J_BOOL,
	J_ERROR,
} JSONType;


typedef struct JSONObject_
{
	JSONType type;
	union
	{
		bool b;
		const char *str;
		double f;
		struct
		{
			struct JSONObject_ **elements;
			size_t array_len;
		};
		struct
		{
			struct JSONObject_ **members;
			const char **keys;
			size_t member_len;
		};
	};
} JSONObject;

typedef enum JSONTokenType_
{
	T_LBRACE,
	T_LBRACKET,
	T_COMMA,
	T_COLON,
	T_RBRACE,
	T_RBRACKET,
	T_STRING,
	T_NUMBER,
	T_ERROR,
	T_TRUE,
	T_FALSE,
	T_NULL,
	T_EOF,
} JSONTokenType;

typedef void*(JsonAllocator)(size_t);

typedef struct
{
	unsigned line;
	const char *current;
	JSONTokenType current_token_type;
	const char *error_message;
	JsonAllocator *allocator;
	const char *last_string;
	double last_number;
} JsonParser;

void json_init_string(JsonParser *parser, const char *str, JsonAllocator *allocator);
JSONObject *json_parse(JsonParser *parser);
char *json_to_str(JSONObject *obj);
JSONObject *json_obj_get(JSONObject *obj, const char *key);
JSONObject *json_new_object(JsonAllocator *allocator, JSONType type);
JSONObject *json_string(JsonAllocator *allocator, const char *str);
JSONObject *json_number(JsonAllocator *allocator, double d);
JSONObject *json_bool(JsonAllocator *allocator, bool b);
JSONObject *json_object(JsonAllocator *allocator, size_t members_size);
JSONObject *json_array(JsonAllocator *allocator, size_t array_size);
void json_free(JSONObject **ptr);