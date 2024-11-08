#include "lib.h"
#include "json.h"

#define PRINTF(file, string, ...) fprintf(file, string, ##__VA_ARGS__) /* NOLINT */

JSONObject error = { .type = J_ERROR };
JSONObject true_val = { .type = J_BOOL, .b = true };
JSONObject false_val = { .type = J_BOOL, .b = false };
JSONObject zero_val = { .type = J_NUMBER, .f = 0.0 };

#define CONSUME(token_) do { if (!consume(parser, token_)) { json_error(parser, "Unexpected character encountered."); return &error; } } while(0)

static inline void json_skip_whitespace(JsonParser *parser)
{
	char c;
	while (1)
	{
	RETRY:
		switch (parser->current[0])
		{
			case '/':
				c = parser->current[1];
				if (c == '/')
				{
					parser->current++;
					while ((c = (++parser->current)[0]) && c != '\n') { }
					goto RETRY;
				}
				if (c == '*')
				{
					parser->current++;
					while ((c = (++parser->current)[0]))
					{
						if (c == '*' && parser->current[1] == '/')
						{
							parser->current += 2;
							goto RETRY;
						}
					}
					goto RETRY;
				}
				return;
			case '\n':
				parser->line++;
			case '\r':
			case ' ':
			case '\v':
			case '\t':
				parser->current++;
				continue;
			default:
				return;
		}
	}
}

static bool json_match(JsonParser *parser, const char *str)
{
	const char *curr = parser->current;
	while (str[0] != '\0')
	{
		if (str++[0] != curr++[0]) return false;
	}
	return true;
}


void json_error(JsonParser *parser, const char *error_message)
{
	if (!parser->error_message) parser->error_message = error_message;
	parser->current_token_type = T_ERROR;
}

static void json_parse_number(JsonParser *parser)
{
	char c = parser->current[0];
	bool negate = c == '-';
	if (negate) c = (++parser->current)[0];
	double value = 0;
	while (c >= '0' && c <= '9')
	{
		value = value * 10.0 + (c - '0');
		c = (++parser->current)[0];
	}
	double decimals = 0;
	double divide = 10.0;
	if (c == '.')
	{
		c = (++parser->current)[0];
		while (c >= '0' && c <= '9')
		{
			decimals = decimals + (c - '0') / divide;
			divide *= 10.0;
			c = (++parser->current)[0];
		}
	}
	value += decimals;
	parser->current_token_type = T_NUMBER;
	parser->last_number = value;
}

static void json_parse_string(JsonParser *parser)
{
	parser->current_token_type = T_STRING;
	const char *current = ++parser->current;
	char c;
	while (c = current++[0], c != '\0' && c != '"')
	{
		if (c == '\\' && current[0] != '\0') current++;
	}
	size_t max_size = current - parser->current;
	char *str = malloc_arena(max_size + 1);
	char *str_current = str;
	while (1)
	{
		c = parser->current++[0];
		if (c == '\0')
		{
			json_error(parser, "Unterminated string.");
			return;
		}
		if (c == '"')
		{
			parser->last_string = str;
			str_current[0] = '\0';
			return;
		}
		if (c != '\\')
		{
			str_current++[0] = c;
			continue;
		}
		c = parser->current++[0];
		switch (c)
		{
			case '\\':
			case '"':
			case '/':
				break;
			case 'b':
				c = '\b';
				break;
			case 'n':
				c = '\n';
				break;
			case 'f':
				c = '\f';
				break;
			case 'r':
				c = '\r';
				break;
			case 't':
				c = '\t';
				break;
			case 'u':
			{
				char u1 = parser->current++[0];
				char u2 = parser->current++[0];
				char u3 = parser->current++[0];
				char u4 = parser->current++[0];
				if (!char_is_hex(u1) || !char_is_hex(u2) || !char_is_hex(u3) || !char_is_hex(u4))
				{
					json_error(parser, "Invalid hex in \\u escape sequence.");
					return;
				}
				c = (char_hex_to_nibble(u1) << 12) + (char_hex_to_nibble(u2) << 8) + (char_hex_to_nibble(u3) << 4) + char_hex_to_nibble(u4);
				break;
			}
			default:
				json_error(parser, "Invalid escape sequence.");
				return;
		}
		str_current++[0] = c;
	}


	UNREACHABLE
}

static inline void json_lexer_advance(JsonParser *parser)
{
	json_skip_whitespace(parser);
	switch (parser->current[0])
	{
		case '\0':
			parser->current_token_type = T_EOF;
			return;
		case '{':
			parser->current_token_type = T_LBRACE;
			parser->current++;
			return;
		case '}':
			parser->current_token_type = T_RBRACE;
			parser->current++;
			return;
		case '[':
			parser->current_token_type = T_LBRACKET;
			parser->current++;
			return;
		case ']':
			parser->current_token_type = T_RBRACKET;
			parser->current++;
			return;
		case ':':
			parser->current_token_type = T_COLON;
			parser->current++;
			return;
		case ',':
			parser->current_token_type = T_COMMA;
			parser->current++;
			return;
		case '"':
			json_parse_string(parser);
			return;
		case '-':
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			json_parse_number(parser);
			return;
		case 't':
			if (!json_match(parser, "true"))
			{
				json_error(parser, "Unexpected symbol, I expected maybe 'true' here.");
				return;
			}
			parser->current += 4;
			parser->current_token_type = T_TRUE;
			return;
		case 'f':
			if (!json_match(parser, "false"))
			{
				json_error(parser, "Unexpected symbol, I expected maybe 'false' here.");
				return;
			}
			parser->current += 5;
			parser->current_token_type = T_FALSE;
			return;
		case 'n':
			if (!json_match(parser, "null"))
			{
				json_error(parser, "Unexpected symbol, I expected maybe 'null' here.");
				return;
			}
			parser->current += 4;
			parser->current_token_type = T_NULL;
			return;
		default:
			json_error(parser, "Unexpected symbol found.");
			return;
	}
	UNREACHABLE
}

static inline bool consume(JsonParser *parser, JSONTokenType token)
{
	if (parser->current_token_type == token)
	{
		json_lexer_advance(parser);
		return true;
	}
	return false;
}

JSONObject *json_parse_array(JsonParser *parser)
{
	CONSUME(T_LBRACKET);
	JSONObject *array = json_new_object(J_ARRAY);
	if (consume(parser, T_RBRACKET))
	{
		return array;
	}

	size_t capacity = 16;
	size_t index = 0;
	while (1)
	{
		JSONObject *parsed = json_parse(parser);
		if (parser->error_message) return &error;
		vec_add(array->elements, parsed);

		if (consume(parser, T_RBRACKET)) break;
		CONSUME(T_COMMA);
		// Allow trailing comma
		if (consume(parser, T_RBRACKET)) break;
	}
	return array;
}

JSONObject *json_parse_object(JsonParser *parser)
{
	CONSUME(T_LBRACE);
	JSONObject *obj = json_new_map();
	if (consume(parser, T_RBRACE))
	{
		return obj;
	}

	size_t index = 0;
	while (1)
	{
		const char *key = parser->last_string;

		CONSUME(T_STRING);
		CONSUME(T_COLON);

		JSONObject *value = json_parse(parser);

		if (parser->error_message) return NULL;
		vec_add(obj->keys, key);
		vec_add(obj->members, value);

		if (consume(parser, T_RBRACE)) break;
		if (!consume(parser, T_COMMA))
		{
			json_error(parser, "Expected a comma.");
			return NULL;
		}
		// Allow trailing comma
		if (consume(parser, T_RBRACE)) break;
	}
	return obj;

}

void json_map_set(JSONObject *obj, const char *key, JSONObject *value)
{
	FOREACH_IDX(i, const char *, a_key, obj->keys)
	{
		if (str_eq(a_key, key))
		{
			obj->members[i] = value;
		}
	}
	vec_add(obj->members, value);
	vec_add(obj->keys, key);
}

JSONObject *json_map_get(JSONObject *obj, const char *key)
{
	ASSERT0(obj->type == J_OBJECT);
	FOREACH_IDX(i, const char *, a_key, obj->keys)
	{
		if (str_eq(a_key, key)) return obj->members[i];
	}
	return NULL;
}

JSONObject *json_parse(JsonParser *parser)
{
	if (parser->error_message) return &error;
	switch (parser->current_token_type)
	{
		case T_EOF:
			return NULL;
		case T_ERROR:
			UNREACHABLE
		case T_LBRACE:
			return json_parse_object(parser);
		case T_LBRACKET:
			return json_parse_array(parser);
		case T_COMMA:
		case T_RBRACE:
		case T_RBRACKET:
		case T_COLON:
			json_error(parser, "Unexpected character.");
			return NULL;
		case T_STRING:
		{
			JSONObject *obj = json_new_string(parser->last_string);
			json_lexer_advance(parser);
			return obj;
		}
		case T_NUMBER:
		{
			JSONObject *obj = NULL;
			if (parser->last_number == 0)
			{
				json_lexer_advance(parser);
				return &zero_val;
			}
			obj = json_new_object(J_NUMBER);
			obj->type = J_NUMBER;
			obj->f = parser->last_number;
			json_lexer_advance(parser);
			return obj;
		}
		case T_TRUE:
			json_lexer_advance(parser);
			return &true_val;
		case T_FALSE:
			json_lexer_advance(parser);
			return &false_val;
		case T_NULL:
			json_lexer_advance(parser);
			return NULL;
	}
	UNREACHABLE
}

void json_init_string(JsonParser *parser, const char *str)
{
	parser->current = str;
	parser->error_message = NULL;
	parser->line = 1;
	json_lexer_advance(parser);
}

bool is_freable(JSONObject *obj)
{
	if (obj == &error) return false;
	if (obj == &true_val) return false;
	if (obj == &false_val) return false;
	if (obj == &zero_val) return false;
	return true;
}

static inline void print_indent(int indent_level, FILE *file)
{
	for (int i = 0; i < indent_level; i++)
	{
		fputs("  ", file);
	}
}

static inline void print_json(JSONObject *obj, int indent_level, FILE *file)
{
	if (obj == NULL)
	{
		return;
	}

	switch (obj->type)
	{
		case J_BOOL:
			PRINTF(file, "%s", obj->b ? "true" : "false");
			break;
		case J_STRING:
			PRINTF(file, "\"%s\"", obj->str);
			break;
		case J_NUMBER:
			PRINTF(file, "%f", obj->f);
			break;
		case J_ARRAY:
			fputs(" [ ", file);
			if (!vec_size(obj->elements))
			{
				fputs(" ]", file);
				break;
			}

			bool should_print_item_per_line = false;
			FOREACH(JSONObject *, object, obj->elements)
			{
				if (object->type == J_OBJECT)
				{
					should_print_item_per_line = true;
					break;
				}
			}
			if (!should_print_item_per_line && vec_size(obj->elements) < 5)
			{
				FOREACH_IDX(i, JSONObject *, object, obj->elements)
				{
					if (i != 0) fputs(", ", file);
					print_json(object, indent_level, file);
				}
				fputs(" ]", file);
				break;
			}

			{
				fputs("\n", file);
				FOREACH_IDX(i, JSONObject *, object, obj->elements)
				{
					if (i != 0) fputs(",\n", file);
					print_indent(indent_level + 1, file);
					print_json(object, indent_level + 1, file);
				}
				fputs("\n ]", file);
			}
			break;
		case J_OBJECT:
		{
			fputs("{\n", file);
			FOREACH_IDX(i, JSONObject *, object, obj->members)
			{
				if (i != 0) fputs(",\n", file);
				print_indent(indent_level + 1, file);
				PRINTF(file, "\"%s\": ", obj->keys[i]);
				print_json(object, indent_level + 1, file);
			}
			fputs("\n", file);
			print_indent(indent_level, file);
			fputs("}", file);
			break;
		}
		default:
			break;
	}
}

void print_json_to_file(JSONObject *obj, FILE *file)
{
	print_json(obj, 0, file);
}

