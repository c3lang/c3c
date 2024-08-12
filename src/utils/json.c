#include "lib.h"
#include "json.h"


#define PRINTF(file, string, ...) fprintf(file, string, ##__VA_ARGS__) /* NOLINT */

JSONObject error = { .type = J_ERROR };
JSONObject true_val = { .type = J_BOOL, .b = true };
JSONObject false_val = { .type = J_BOOL, .b = false };
JSONObject zero_val = { .type = J_NUMBER, .f = 0.0 };
JSONObject empty_array_val = { .type = J_ARRAY, .array_len = 0 };
JSONObject empty_obj_val = { .type = J_OBJECT, .member_len = 0 };

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

static void json_parse_comment_line(JsonParser *parser)
{
	parser->current_token_type = T_COMMENT;

	const char *current = ++parser->current;
	char c;
	while (c = current++[0], c != '\0' && c != '\n') { current++; }

	size_t max_size = current - parser->current;
	char *str = parser->allocator(max_size + 1);
	char *str_current = str;
	parser->last_string = str;
	str_current[0] = '\0';

	while (1)
	{
		c = parser->current++[0];
		if (c == '\0')
		{
			str_current[0] = '\0';
			return;
		}
		if (c == '\n')
		{
			parser->last_string = str;
			str_current[0] = '\0';

			return;
		}

		str_current++[0] = c;
	}

	UNREACHABLE

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
	char *str = parser->allocator(max_size + 1);
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
	case '/':
		char c = parser->current[1];
		if (c == '/')
		{
			parser->current++;
			json_parse_comment_line(parser);
		}
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
	if (consume(parser, T_RBRACKET))
	{
		return &empty_array_val;
	}
	size_t capacity = 16;
	JSONObject *array = json_new_object(parser->allocator, J_ARRAY);
	JSONObject **elements = parser->allocator(sizeof(JSONObject *) * capacity);
	size_t index = 0;
	while (1)
	{
		JSONObject *parsed = json_parse(parser);

		if (parser->error_message) return &error;
		if (index >= capacity)
		{
			JSONObject **elements_old = elements;
			size_t copy_size = capacity * sizeof(JSONObject *);
			capacity *= 2;
			elements = parser->allocator(sizeof(JSONObject *) * capacity);
			memcpy(elements, elements_old, copy_size);
		}
		elements[index++] = parsed;

		if (parsed->type == J_COMMENT_LINE)
		{
			if (consume(parser, T_RBRACKET)) break;
			continue;
		}

		if (consume(parser, T_RBRACKET)) break;
		CONSUME(T_COMMA);
		// Allow trailing comma
		if (consume(parser, T_RBRACKET)) break;
	}
	array->elements = elements;
	array->array_len = index;
	return array;
}

JSONObject *json_parse_object(JsonParser *parser)
{
	CONSUME(T_LBRACE);

	if (consume(parser, T_RBRACE))
	{
		return &empty_obj_val;
	}

	size_t capacity = 16;
	JSONObject *obj = json_new_object(parser->allocator, J_OBJECT);
	JSONObject **elements = parser->allocator(sizeof(JSONObject *) * capacity);
	const char **keys = parser->allocator(sizeof(JSONObject *) * capacity);
	size_t index = 0;
	while (1)
	{
		if (consume(parser, T_RBRACE))
		{
			break;
		}

		// Parse COMMENT LINE
		if (parser->current_token_type == T_COMMENT)
		{
			JSONObject *value = json_parse(parser);


			if (parser->error_message) return NULL;
			if (index >= capacity)
			{
				JSONObject **elements_old = elements;
				const char **keys_old = keys;
				size_t copy_size = capacity * sizeof(void *);
				capacity *= 2;
				elements = parser->allocator(sizeof(JSONObject *) * capacity);
				keys = parser->allocator(sizeof(JSONObject *) * capacity);
				memcpy(elements, elements_old, copy_size);
				memcpy(keys, keys_old, copy_size);
			}

			keys[index] = NULL;
			elements[index++] = value;

			continue;
		}

		const char *key = parser->last_string;

		CONSUME(T_STRING);
		CONSUME(T_COLON);

		JSONObject *value = json_parse(parser);


		if (parser->error_message) return NULL;
		if (index >= capacity)
		{
			JSONObject **elements_old = elements;
			const char **keys_old = keys;
			size_t copy_size = capacity * sizeof(void *);
			capacity *= 2;
			elements = parser->allocator(sizeof(JSONObject *) * capacity);
			keys = parser->allocator(sizeof(JSONObject *) * capacity);
			memcpy(elements, elements_old, copy_size);
			memcpy(keys, keys_old, copy_size);
		}

		keys[index] = key;
		elements[index++] = value;

		if (consume(parser, T_COMMA)) continue;

		if (consume(parser, T_RBRACE)) break;
		if (!consume(parser, T_COMMA))
		{
			json_error(parser, "Expected a comma.");
			return NULL;
		}
		// Allow trailing comma
		if (consume(parser, T_RBRACE)) break;
	}
	obj->members = elements;
	obj->keys = keys;
	obj->member_len = index;
	return obj;

}

JSONObject *json_obj_get(JSONObject *obj, const char *key)
{
	assert(obj->type == J_OBJECT);
	for (unsigned i = 0; i < obj->member_len; i++)
	{
		if (obj->keys[i] != 0 && strcmp(obj->keys[i], key) == 0) return obj->elements[i];
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
		JSONObject *obj = json_new_object(parser->allocator, J_STRING);
		obj->type = J_STRING;
		obj->str = parser->last_string;
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
		obj = json_new_object(parser->allocator, J_NUMBER);
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
	case T_COMMENT:
		JSONObject *obj = json_new_object(parser->allocator, J_COMMENT_LINE);
		obj->type = J_COMMENT_LINE;
		obj->str = parser->last_string;
		json_lexer_advance(parser);
		return obj;
	}
	UNREACHABLE
}

void json_init_string(JsonParser *parser, const char *str, JsonAllocator *allocator)
{
	parser->current = str;
	parser->allocator = allocator;
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
	if (obj == &empty_array_val) return false;
	if (obj == &empty_obj_val) return false;
	return true;
}

void json_free(JsonDeallocator *deallocator, JSONObject **ptr)
{
	JSONObject *obj = *ptr;

	if (!is_freable(obj)) return;

	switch (obj->type)
	{
	case J_OBJECT:
		for (size_t i = 0; i < obj->member_len; i++)
		{
			json_free(deallocator, &obj->members[i]);
			deallocator((char *)obj->keys[i]);
		}
		deallocator(obj->keys);
		deallocator(obj->members);
		break;
	case J_ARRAY:
		for (size_t i = 0; i < obj->array_len; i++)
		{
			json_free(deallocator, &obj->elements[i]);
		}
		deallocator(obj->elements);
		break;
	case J_STRING:
		deallocator((char *)obj->str);
		break;
	default:
		break;
	}
	deallocator(*ptr);
	*ptr = NULL;
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

		if (obj->array_len == 0)
		{
			fputs(" ]", file);
			break;
		}

		bool should_print_item_per_line = false;

		for (size_t i = 0; i < obj->array_len; i++)
		{
			if (obj->elements[i]->type == J_OBJECT)
			{
				should_print_item_per_line = true;
				break;
			}

			if (obj->elements[i]->type == J_COMMENT_LINE)
			{
				should_print_item_per_line = true;
				break;
			}
		}

		if (!should_print_item_per_line && obj->array_len < 5)
		{
			for (size_t i = 0; i < obj->array_len; i++)
			{
				if (i != 0) fputs(", ", file);
				print_json(obj->elements[i], indent_level, file);
			}
			fputs(" ]", file);
			break;
		}

		fputs("\n", file);
		for (size_t i = 0; i < obj->array_len; i++)
		{
			if (obj->elements[i]->type == J_COMMENT_LINE)
			{
				print_json(obj->elements[i], indent_level, file);
				continue;
			}

			print_indent(indent_level + 1, file);
			print_json(obj->elements[i], indent_level + 1, file);
			fputs(",\n", file);
		}

		fputs(" ]", file);
		break;
	case J_OBJECT:
		fputs("{\n", file);
		for (size_t i = 0; i < obj->member_len; i++)
		{
			if (obj->members[i]->type == J_COMMENT_LINE)
			{
				print_json(obj->members[i], indent_level, file);
				continue;
			}

			print_indent(indent_level + 1, file);
			PRINTF(file, "\"%s\": ", obj->keys[i]);
			print_json(obj->members[i], indent_level + 1, file);
			fputs(",\n", file);
		}
		print_indent(indent_level, file);
		fputs("}", file);
		break;
	case J_COMMENT_LINE:
		print_indent(indent_level + 1, file);
		PRINTF(file, "//%s\n", obj->str);
		break;
	default:
		break;
	}
}

void print_json_to_file(JSONObject *obj, FILE *file)
{
	print_json(obj, 0, file);
}

