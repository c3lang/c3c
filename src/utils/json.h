#pragma once
typedef enum
{
	J_OBJECT,
	J_STRING,
	J_ARRAY,
	J_NUMBER,
	J_BOOL,
	J_ERROR,
	J_COMMENT_LINE
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
	T_COMMENT
} JSONTokenType;

typedef void *(JsonAllocator)(size_t);
typedef void *(JsonDeallocator)(void *);

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
JSONObject *json_obj_get(JSONObject *obj, const char *key);
INLINE JSONObject *json_new_object(JsonAllocator *allocator, JSONType type);
void json_free(JsonDeallocator *deallocator, JSONObject **ptr);
void print_json_to_file(JSONObject *obj, FILE *file);


INLINE JSONObject *json_new_object(JsonAllocator *allocator, JSONType type)
{
	JSONObject *obj = allocator(sizeof(JSONObject));
	obj->type = type;
	return obj;
}
