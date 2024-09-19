#pragma once
typedef enum
{
	J_OBJECT,
	J_STRING,
	J_ARRAY,
	J_NUMBER,
	J_BOOL,
	J_ERROR
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
		};
		struct
		{
			struct JSONObject_ **members;
			const char **keys;
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
	T_EOF
} JSONTokenType;

typedef struct
{
	unsigned line;
	const char *current;
	JSONTokenType current_token_type;
	const char *error_message;
	const char *last_string;
	double last_number;
} JsonParser;

void json_init_string(JsonParser *parser, const char *str);
JSONObject *json_parse(JsonParser *parser);
JSONObject *json_map_get(JSONObject *obj, const char *key);
void json_map_set(JSONObject *obj, const char *key, JSONObject *value);
INLINE JSONObject *json_new_object(JSONType type);
void print_json_to_file(JSONObject *obj, FILE *file);

INLINE JSONObject *json_new_string(const char *str)
{
	JSONObject *obj = malloc_arena(sizeof(JSONObject));
	*obj = (JSONObject) { .type = J_STRING, .str = str };
	return obj;
}

INLINE JSONObject *json_new_map(void)
{
	JSONObject *obj = malloc_arena(sizeof(JSONObject));
	*obj = (JSONObject) { .type = J_OBJECT };
	return obj;
}

INLINE JSONObject *json_new_object(JSONType type)
{
	JSONObject *obj = malloc_arena(sizeof(JSONObject));
	*obj = (JSONObject) { .type = type };
	return obj;
}
