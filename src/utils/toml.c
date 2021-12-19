/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include "toml.h"
#include "lib.h"

#if (defined(_MSC_VER) && _MSC_VER < 1800) || defined(_ADI_COMPILER)
#define va_copy(a, b) ((a) = (b))
#endif

char *toml_strdup(TOML_CONST char *str)
{
    size_t len = strlen(str) + 1;
    void *new = cmalloc(len);
    if (new == NULL)
    {
        return NULL;
    }

    return memcpy(new, str, len);
}

char *toml_strndup(TOML_CONST char *str, size_t n)
{
    char *result = cmalloc(n + 1);
    if (result == NULL)
    {
        return NULL;
    }

    result[n] = 0;
    return memcpy(result, str, n);
}

int toml_vasprintf(char **str, TOML_CONST char *format, va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);
	int size = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);

    if (size < 0)
    {
        return size;
    }

    *str = malloc((unsigned)size + 1);
    if (*str == NULL)
    {
        return -1;
    }

    return vsprintf(*str, format, args);
}

int toml_asprintf(char **str, TOML_CONST char *format, ...)
{
    va_list args;
    va_start(args, format);
    int size = toml_vasprintf(str, format, args);
    va_end(args);
    return size;
}

void toml_err_init(TomlErr *err)
{
    err->code = TOML_OK;
    err->message = NULL;
    err->_is_literal = TOML_FALSE;
}

void toml_set_err_v(TomlErr *err, int code, TOML_CONST char *format, va_list args)
{
    if (err != NULL)
    {
        assert(err->code == TOML_OK);
        err->code = code;
        toml_vasprintf(&err->message, format, args);
        err->_is_literal = TOML_FALSE;
    }
}

void toml_set_err(TomlErr *err, int code, TOML_CONST char *format, ...)
{
    if (err != NULL)
    {
        assert(err->code == TOML_OK);
        va_list args;
        va_start(args, format);
        toml_set_err_v(err, code, format, args);
        va_end(args);
    }
}

void toml_set_err_literal(TomlErr *err, int code, TOML_CONST char *message)
{
    if (err != NULL)
    {
        assert(err->code == TOML_OK);
        err->code = code;
        err->message = (char *) message;
        err->_is_literal = TOML_TRUE;
    }
}

void toml_clear_err(TomlErr *err)
{
    if (err)
    {
        if (!err->_is_literal)
        {
            free(err->message);
        }
        toml_err_init(err);
    }
}

void toml_err_move(TomlErr *to, TomlErr *from)
{
    if (to == NULL)
    {
        if (!from->_is_literal)
        {
            free(from->message);
        }
    }
    else
    {
        assert(to->code == TOML_OK);
        *to = *from;
        toml_err_init(from);
    }
}

#ifdef _MSC_VER
static __inline size_t toml_roundup_pow_of_two_size_t(size_t x)
#else

static inline size_t toml_roundup_pow_of_two_size_t(size_t x)
#endif
{
    size_t v = x;
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
#if SIZE_MAX == 0xffff
    v |= v >> 8;
#elif SIZE_MAX == 0xffffffff
    v |= v >> 8;
    v |= v >> 16;
#elif SIZE_MAX == 0xffffffffffffffffll
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
#endif
    v++;
    return v;
}

TomlString *toml_string_new(TomlErr *error)
{
    TomlString *self = cmalloc(sizeof(TomlString));
    if (self == NULL)
    {
        toml_set_err_literal(error, TOML_ERR_NOMEM, "out of memory");
        return NULL;
    }

    self->str = NULL;
    self->len = 0;
    self->_capacity = 0;

    return self;
}

TomlString *toml_string_new_string(TOML_CONST char *str, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlString *self = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    toml_string_append_string(self, str, &err);

    cleanup:
    if (err.code != TOML_OK)
    {
        free(self);
        self = NULL;
    }
    toml_err_move(error, &err);
    return self;
}

TomlString *toml_string_new_nstring(TOML_CONST char *str, size_t len, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlString *self = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    toml_string_append_nstring(self, str, len, &err);

    cleanup:
    if (err.code != TOML_OK)
    {
        free(self);
        self = NULL;
    }
    toml_err_move(error, &err);
    return self;
}

void toml_string_check_expand(TomlString *self, size_t len_to_add, TomlErr *error)
{
    if (self->len + len_to_add + 1 > self->_capacity)
    {
        size_t new_capacity = toml_roundup_pow_of_two_size_t(self->len + len_to_add + 1);
        new_capacity = new_capacity >= 8 ? new_capacity : 8;
        void *p = realloc(self->str, new_capacity);
        if (p == NULL)
        {
            toml_set_err_literal(error, TOML_ERR_NOMEM, "out of memory");
            return;
        }
        self->str = p;
        self->_capacity = new_capacity;
    }
}

void toml_string_append_char(TomlString *self, char ch, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    toml_string_check_expand(self, 1, &err);
    if (err.code != TOML_OK) goto cleanup;

    self->str[self->len] = ch;
    self->str[self->len + 1] = 0;
    self->len++;

    cleanup:
    toml_err_move(error, &err);
}

void toml_string_append_string(TomlString *self, TOML_CONST char *str, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    size_t len = strlen(str);

    toml_string_check_expand(self, len, &err);
    if (err.code != TOML_OK) goto cleanup;

    memcpy(&self->str[self->len], str, len + 1);
    self->len += len;

    cleanup:
    toml_err_move(error, &err);
}

void toml_string_append_nstring(TomlString *self, TOML_CONST char *str, size_t len, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    toml_string_check_expand(self, len, &err);
    if (err.code != TOML_OK) goto cleanup;

    memcpy(&self->str[self->len], str, len);
    self->len += len;
    self->str[self->len] = 0;

    cleanup:
    toml_err_move(error, &err);
}

void toml_string_free(TomlString *self)
{
    if (self != NULL)
    {
        free(self->str);
        free(self);
    }
}

TomlString *toml_string_copy(TOML_CONST TomlString *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlString *str = NULL;

    str = toml_string_new_nstring(self->str, self->len, &err);
    if (err.code != TOML_OK)
    {
        toml_err_move(error, &err);
    }

    return str;
}

int toml_string_equals(TOML_CONST TomlString *self, TOML_CONST TomlString *other)
{
    if (self == other)
    {
        return TOML_TRUE;
    }

    if (self->len != other->len)
    {
        return TOML_FALSE;
    }

    if (self->str == other->str)
    {
        return TOML_TRUE;
    }

    for (size_t i = 0; i < self->len; i++)
    {
        if (self->str[i] != other->str[i])
        {
            return TOML_FALSE;
        }
    }

    return TOML_TRUE;
}

struct _TomlTable
{
    TomlKeyValue *keyvals;
    size_t len;
    size_t capacity;
};

TomlTable *toml_table_new(TomlErr *err)
{
    TomlTable *self = cmalloc(sizeof(TomlTable));
    if (self == NULL)
    {
        toml_set_err_literal(err, TOML_ERR_NOMEM, "out of memory");
    }
    else
    {
        self->keyvals = NULL;
        self->len = 0;
        self->capacity = 0;
    }
    return self;
}

void toml_table_free(TomlTable *self)
{
    if (self != NULL)
    {
        for (size_t i = 0; i < self->len; i++)
        {
            toml_string_free(self->keyvals[i].key);
            toml_value_free(self->keyvals[i].value);
        }
        free(self->keyvals);
        free(self);
    }
}

void toml_table_check_expand(TomlTable *self, TomlErr *err)
{
    if (self->len + 1 > self->capacity)
    {
        size_t new_capacity = self->capacity > 0 ? self->capacity * 2 : 8;
        void *p = realloc(self->keyvals, sizeof(TomlKeyValue) * new_capacity);
        if (p == NULL)
        {
            toml_set_err_literal(err, TOML_ERR_NOMEM, "out of memory");
            return;
        }
        self->keyvals = p;
        self->capacity = new_capacity;
    }
}

void toml_table_set_by_string(TomlTable *self, TomlString *key,
        TomlValue *value, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue **slot = NULL;
    for (size_t i = 0; i < self->len; i++)
    {
        if (toml_string_equals(self->keyvals[i].key, key))
        {
            slot = &self->keyvals[i].value;
        }
    }

    if (slot == NULL)
    {
        toml_table_check_expand(self, &err);
        if (err.code != TOML_OK)
        {
            toml_err_move(error, &err);
            return;
        }

        self->keyvals[self->len].key = key;
        self->keyvals[self->len].value = value;
        self->len++;
    }
    else
    {
        *slot = value;
    }
}

TomlValue *toml_table_get_by_string(TOML_CONST TomlTable *self, TOML_CONST TomlString *key)
{
    TomlValue *value = NULL;
    for (size_t i = 0; i < self->len; i++)
    {
        if (toml_string_equals(self->keyvals[i].key, key))
        {
            value = self->keyvals[i].value;
        }
    }
    return value;
}

TomlValue *toml_table_getn(TOML_CONST TomlTable *self, TOML_CONST char *key, size_t key_len)
{
    TomlString str = { (char *) key, key_len, 0 };
    return toml_table_get_by_string(self, &str);
}

TomlValue *toml_table_get(TOML_CONST TomlTable *self, TOML_CONST char *key)
{
    return toml_table_getn(self, key, strlen(key));
}

void toml_table_setn(TomlTable *self, TOML_CONST char *key, size_t key_len,
        TomlValue *value, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlString *str = NULL;

    str = toml_string_new_nstring(key, key_len, &err);
    if (err.code != TOML_OK) goto error;

    toml_table_set_by_string(self, str, value, &err);
    if (err.code != TOML_OK) goto error;

    goto cleanup;

    error:
    toml_string_free(str);

    cleanup:
    toml_err_move(error, &err);
}

void toml_table_set(TomlTable *self, TOML_CONST char *key, TomlValue *value, TomlErr *error)
{
    toml_table_setn(self, key, strlen(key), value, error);
}

struct _TomlTableIter
{
    TomlTable *table;
    TomlKeyValue *keyval;
};

TomlTableIter *toml_table_iter_new(TomlTable *table, TomlErr *error)
{
    TomlTableIter *self = cmalloc(sizeof(TomlTableIter));
    if (self == NULL)
    {
        toml_set_err_literal(error, TOML_ERR_NOMEM, "out of memory");
        return NULL;
    }
    self->table = table;
    self->keyval = table->keyvals;
    return self;
}

void toml_table_iter_free(TomlTableIter *self)
{
    free(self);
}

TomlKeyValue *toml_table_iter_get(TomlTableIter *self)
{
    return self->keyval;
}

int toml_table_iter_has_next(TomlTableIter *self)
{
    return self->keyval != NULL;
}

void toml_table_iter_next(TomlTableIter *self)
{
    if (self->keyval < self->table->keyvals + self->table->len)
    {
        self->keyval++;
    }

    if (self->keyval >= self->table->keyvals + self->table->len)
    {
        self->keyval = NULL;
    }
}

TomlArray *toml_array_new(TomlErr *error)
{
    TomlArray *self = cmalloc(sizeof(TomlArray));
    if (self == NULL)
    {
        toml_set_err_literal(error, TOML_ERR_NOMEM, "out of memory");
        return NULL;
    }

    self->elements = NULL;
    self->len = 0;
    self->_capacity = 0;
    return self;
}

void toml_array_free(TomlArray *self)
{
    if (self != NULL)
    {
        for (size_t i = 0; i < self->len; i++)
        {
            toml_value_free(self->elements[i]);
        }
        free(self->elements);
        free(self);
    }
}

void toml_array_check_expand(TomlArray *self, TomlErr *error)
{
    if (self->len + 1 > self->_capacity)
    {
        size_t new_capacity = self->_capacity > 0 ? self->_capacity * 2 : 8;
        void *p = realloc(self->elements, sizeof(TomlValue *) * new_capacity);
        if (p == NULL)
        {
            toml_set_err_literal(error, TOML_ERR_NOMEM, "out of memory");
            return;
        }
        self->elements = p;
        self->_capacity = new_capacity;
    }
}

void toml_array_append(TomlArray *self, TomlValue *value, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    toml_array_check_expand(self, &err);
    if (err.code != TOML_OK) goto cleanup;

    self->elements[self->len++] = value;

    cleanup:
    toml_err_move(error, &err);
}

TomlValue *toml_value_new(TomlType type, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->type = type;
    switch (type)
    {
        case TOML_TABLE:
            self->value.table = NULL;
            break;
        case TOML_ARRAY:
            self->value.array = NULL;
            break;
        case TOML_STRING:
            self->value.string = NULL;
            break;
        case TOML_INTEGER:
            self->value.integer = 0;
            break;
        case TOML_FLOAT:
            self->value.float_ = 0.0;
            break;
        case TOML_BOOLEAN:
            self->value.boolean = TOML_FALSE;
            break;
        case TOML_DATETIME:
            self->value.datetime = ccalloc(1, sizeof(TomlDateTime));
            if (self->value.datetime == NULL)
            {
                toml_set_err(error, TOML_ERR_NOMEM, "out of memory");
            }
            break;
    }

    cleanup:
    toml_err_move(error, &err);
    return self;
}

TomlValue *toml_value_new_string(TOML_CONST char *str, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.string = toml_string_new_string(str, &err);
    if (err.code != TOML_OK)
    {
        free(self);
        self = NULL;
        goto cleanup;
    }

    self->type = TOML_STRING;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

TomlValue *toml_value_new_nstring(TOML_CONST char *str, size_t len, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.string = toml_string_new_nstring(str, len, &err);
    if (err.code != TOML_OK)
    {
        free(self);
        self = NULL;
        goto cleanup;
    }

    self->type = TOML_STRING;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

TomlValue *toml_value_new_table(TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.table = toml_table_new(&err);
    if (err.code != TOML_OK)
    {
        free(self);
        self = NULL;
        goto cleanup;
    }

    self->type = TOML_TABLE;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

TomlValue *toml_value_new_array(TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.array = toml_array_new(&err);
    if (err.code != TOML_OK)
    {
        free(self);
        self = NULL;
        goto cleanup;
    }

    self->type = TOML_ARRAY;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

#if defined(_MSC_VER)
TomlValue *toml_value_new_integer(long long integer, TomlErr *error)
#else

TomlValue *toml_value_new_integer(long integer, TomlErr *error)
#endif
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.integer = integer;
    self->type = TOML_INTEGER;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

TomlValue *toml_value_new_float(double float_, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.float_ = float_;
    self->type = TOML_FLOAT;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

TomlValue *toml_value_new_datetime(TomlErr *error)
{
    return toml_value_new(TOML_DATETIME, error);
}

TomlValue *toml_value_new_boolean(int boolean, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *self = cmalloc(sizeof(TomlValue));
    if (self == NULL)
    {
        toml_set_err_literal(&err, TOML_ERR_NOMEM, "out of memory");
        goto cleanup;
    }

    self->value.boolean = boolean;
    self->type = TOML_BOOLEAN;

    cleanup:
    toml_err_move(error, &err);
    return self;
}

void toml_value_free(TomlValue *self)
{
    if (self != NULL)
    {
        switch (self->type)
        {
            case TOML_STRING:
                toml_string_free(self->value.string);
                break;
            case TOML_TABLE:
                toml_table_free(self->value.table);
                break;
            case TOML_ARRAY:
                toml_array_free(self->value.array);
                break;
            case TOML_DATETIME:
                free(self->value.datetime);
                break;
            default:
                break;
        }
        free(self);
    }
}

typedef struct _TomlParser
{
    TOML_CONST char *begin;
    TOML_CONST char *end;
    TOML_CONST char *ptr;
    int lineno;
    int colno;
    char *filename;
} TomlParser;

TomlParser *toml_parser_new(TOML_CONST char *str, size_t len, TomlErr *error)
{
    TomlParser *self = cmalloc(sizeof(TomlParser));
    if (self == NULL)
    {
        toml_set_err_literal(error, TOML_OK, "out of memory");
        return NULL;
    }

    self->begin = str;
    self->end = str + len;
    self->ptr = str;
    self->lineno = 1;
    self->colno = 1;
    self->filename = NULL;

    return self;
}

void toml_parser_free(TomlParser *self)
{
    if (self != NULL)
    {
        free(self->filename);
        free(self);
    }
}

void toml_move_next(TomlParser *self)
{
    if (self->ptr < self->end)
    {
        if (*self->ptr == '\n')
        {
            self->lineno++;
            self->colno = 1;
        }
        else
        {
            self->colno++;
        }
        self->ptr++;
    }
}

void toml_next_n(TomlParser *self, int n)
{
    for (int i = 0; i < n; i++)
    {
        toml_move_next(self);
    }
}

TomlString *toml_parse_bare_key(TomlParser *self, TomlErr *error)
{
    TOML_CONST char *str = self->ptr;
    size_t len = 0;

    while (self->ptr < self->end)
    {
        char ch = *self->ptr;

        if (!(isalnum(ch) || ch == '_' || ch == '-'))
        {
            break;
        }

        len++;
        toml_move_next(self);
    }

    return toml_string_new_nstring(str, len, error);
}

char toml_hex_char_to_int(char ch)
{
    assert(isxdigit(ch));
    if (isdigit(ch))
    {
        return ch - '0';
    }
    else if (islower(ch))
    {
        return ch - 'a' + 10;
    }
    else if (isupper(ch))
    {
        return ch - 'A' + 10;
    }
    return 0;
}

void toml_encode_unicode_scalar(TomlString *result, TomlParser *parser, int n, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    unsigned int scalar = 0;

    if (parser->ptr + n > parser->end)
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid unicode scalar",
                parser->filename, parser->lineno, parser->colno);
        goto cleanup;
    }

    for (int i = 0; i < n; i++)
    {
        char ch = *parser->ptr;
        if (isxdigit(ch))
        {
            scalar = scalar * 16 + (unsigned)toml_hex_char_to_int(ch);
            toml_move_next(parser);
        }
        else
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid unicode scalar",
                    parser->filename, parser->lineno, parser->colno);
            goto cleanup;
        }
    }

    if ((scalar >= 0xd800 && scalar <= 0xdfff) ||
            (scalar >= 0xfffe && scalar <= 0xffff))
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid unicode scalar",
                parser->filename, parser->lineno, parser->colno);
        goto cleanup;
    }

    if (scalar <= 0x7f)
    {
        toml_string_append_char(result, (char) scalar, &err);
        goto cleanup;
    }

    if (scalar <= 0x7ff)
    {
        toml_string_append_char(result, (char)(0xc0 | (scalar >> 6)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | (scalar & 0x3f)), &err);
        goto cleanup;
    }

    if (scalar <= 0xffff)
    {
        toml_string_append_char(result, (char)(0xe0 | (scalar >> 12)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 6) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | (scalar & 0x3f)), &err);
        goto cleanup;
    }

    if (scalar <= 0x1fffff)
    {
        toml_string_append_char(result, (char)(0xf0 | (scalar >> 18)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 12) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 6) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | (scalar & 0x3f)), &err);
        goto cleanup;
    }

    if (scalar <= 0x3ffffff)
    {
    	toml_string_append_char(result, (char)(0xf8 | (scalar >> 24)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 18) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 12) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 6) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | (scalar & 0x3f)), &err);
        goto cleanup;
    }

    if (scalar <= 0x7fffffff)
    {
    	toml_string_append_char(result, (char)(0xfc | (scalar >> 30)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 24) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 18) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 12) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | ((scalar >> 6) & 0x3f)), &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_string_append_char(result, (char)(0x80 | (scalar & 0x3f)), &err);
        goto cleanup;
    }

    toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid unicode scalar",
            parser->filename, parser->lineno, parser->colno);

    cleanup:
    toml_err_move(error, &err);
}

TomlString *toml_parse_basic_string(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlString *result = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    while (self->ptr < self->end && *self->ptr != '\"' && *self->ptr != '\n')
    {
        char ch1 = *self->ptr;
        if (ch1 == '\\')
        {
            if (self->ptr >= self->end) break;

            toml_move_next(self);
            char ch2 = *self->ptr;

            if (ch2 == '\"')
            {
                toml_string_append_char(result, '\"', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'b')
            {
                toml_string_append_char(result, '\b', &err);
                toml_move_next(self);
            }
            else if (ch2 == 't')
            {
                toml_string_append_char(result, '\t', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'n')
            {
                toml_string_append_char(result, '\n', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'f')
            {
                toml_string_append_char(result, '\f', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'r')
            {
                toml_string_append_char(result, '\r', &err);
                toml_move_next(self);
            }
            else if (ch2 == '\\')
            {
                toml_string_append_char(result, '\\', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'u')
            {
                toml_move_next(self);
                toml_encode_unicode_scalar(result, self, 4, &err);
            }
            else if (ch2 == 'U')
            {
                toml_move_next(self);
                toml_encode_unicode_scalar(result, self, 8, &err);
            }
            else
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid escape charactor");
                goto cleanup;
            }
        }
        else
        {
            toml_string_append_char(result, ch1, &err);
            toml_move_next(self);
        }
        if (err.code != TOML_OK) goto cleanup;
    }

    if (self->ptr >= self->end || *self->ptr != '\"' || *self->ptr == '\n')
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unterminated basic string",
                self->filename, self->lineno, self->colno);
        goto cleanup;
    }

    toml_move_next(self);

    cleanup:
    toml_err_move(error, &err);
    return result;
}

TomlString *toml_parse_literal_string(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlString *result = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    while (self->ptr < self->end && *self->ptr != '\'' && *self->ptr != '\n')
    {
        toml_string_append_char(result, *self->ptr, &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_move_next(self);
    }

    if (self->ptr >= self->end || *self->ptr != '\'' || *self->ptr == '\n')
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unterminated literal string",
                self->filename, self->lineno, self->colno);
    }

    toml_move_next(self);

    cleanup:
    toml_err_move(error, &err);
    return result;
}

TomlValue *toml_parse_basic_string_value(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *value = NULL;

    TomlString *string = toml_parse_basic_string(self, &err);
    if (err.code != TOML_OK) goto cleanup;

    value = toml_value_new(TOML_STRING, &err);
    if (err.code != TOML_OK) goto cleanup;

    value->value.string = string;

    cleanup:
    toml_err_move(error, &err);
    return value;
}

TomlValue *toml_parse_literal_string_value(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlValue *value = NULL;

    TomlString *string = toml_parse_literal_string(self, &err);
    if (err.code != TOML_OK) goto cleanup;

    value = toml_value_new(TOML_STRING, &err);
    if (err.code != TOML_OK) goto cleanup;

    value->value.string = string;

    cleanup:
    toml_err_move(error, &err);
    return value;
}

TomlValue *toml_parse_multi_line_basic_string(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlValue *value = NULL;

    TomlString *result = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    if (*self->ptr == '\n')
    {
        toml_move_next(self);
    }

    while (self->ptr + 3 <= self->end && strncmp(self->ptr, "\"\"\"", 3) != 0)
    {
        char ch1 = *self->ptr;

        if (ch1 == '\\')
        {
            if (self->ptr + 3 > self->end || strncmp(self->ptr, "\"\"\"", 3) == 0)
            {
                break;
            }

            toml_move_next(self);
            char ch2 = *self->ptr;

            if (ch2 == '\"')
            {
                toml_string_append_char(result, '\"', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'b')
            {
                toml_string_append_char(result, '\b', &err);
                toml_move_next(self);
            }
            else if (ch2 == 't')
            {
                toml_string_append_char(result, '\t', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'n')
            {
                toml_string_append_char(result, '\n', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'f')
            {
                toml_string_append_char(result, '\f', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'r')
            {
                toml_string_append_char(result, '\r', &err);
                toml_move_next(self);
            }
            else if (ch2 == '\\')
            {
                toml_string_append_char(result, '\\', &err);
                toml_move_next(self);
            }
            else if (ch2 == 'u')
            {
                toml_move_next(self);
                toml_encode_unicode_scalar(result, self, 4, &err);
            }
            else if (ch2 == 'U')
            {
                toml_move_next(self);
                toml_encode_unicode_scalar(result, self, 8, &err);
            }
            else if (ch2 == '\n')
            {
                do
                {
                    toml_move_next(self);
                } while (self->ptr + 3 <= self->end && isspace(*self->ptr));
            }
            else
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid escape charactor",
                        self->filename, self->lineno, self->colno);
                goto cleanup;
            }
        }
        else
        {
            toml_string_append_char(result, ch1, &err);
            toml_move_next(self);
        }
    }
    if (err.code != TOML_OK) goto cleanup;

    if (self->ptr + 3 > self->end || strncmp(self->ptr, "\"\"\"", 3) != 0)
    {
        toml_set_err(&err, TOML_ERR_SYNTAX,
                "%s:%d:%d: unterminated multi-line basic string",
                self->filename, self->lineno, self->colno);
        goto cleanup;
    }

    toml_next_n(self, 3);

    value = toml_value_new(TOML_STRING, &err);
    if (err.code != TOML_OK) goto cleanup;

    value->value.string = result;

    cleanup:
    toml_err_move(error, &err);
    return value;
}

TomlValue *toml_parse_multi_line_literal_string(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlValue *value = NULL;

    TomlString *result = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    if (*self->ptr == '\n')
    {
        toml_move_next(self);
    }

    while (self->ptr + 3 <= self->end && strncmp(self->ptr, "\'\'\'", 3) != 0)
    {
        toml_string_append_char(result, *self->ptr, &err);
        if (err.code != TOML_OK) goto cleanup;
        toml_move_next(self);
    }

    if (self->ptr + 3 > self->end || strncmp(self->ptr, "\'\'\'", 3) != 0)
    {
        toml_set_err(&err, TOML_ERR_SYNTAX,
                "%s:%d:%d: unterminated multi-line literal string",
                self->filename, self->lineno, self->colno);
        goto cleanup;
    }

    toml_next_n(self, 3);

    value = toml_value_new(TOML_STRING, &err);
    if (err.code != TOML_OK) goto cleanup;

    value->value.string = result;

    cleanup:
    toml_err_move(error, &err);
    return value;
}

TomlValue *toml_parse_datetime(TOML_CONST char *str, size_t len, TomlErr *error)
{
    (void) str;
    (void) len;
    return toml_value_new(TOML_DATETIME, error);
}

TomlValue *toml_parse_int_or_float_or_time(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlString *str = NULL;
    TomlValue *result = NULL;

    char type = 'i';
    int base = 10;

    str = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    // Determine nan and inf type as float, as we cannot determine by dot.
    // But do not strip it because we will append it to the string later
    if (self->ptr + 3 <= self->end &&
            (strncmp(self->ptr, "nan", 3) == 0 || strncmp(self->ptr, "inf", 3) == 0))
    {
        type = 'f';
    }

    if (self->ptr + 4 <= self->end &&
            (strncmp(self->ptr, "+nan", 4) == 0 ||
                    strncmp(self->ptr, "-nan", 4) == 0 ||
                    strncmp(self->ptr, "+inf", 4) == 0 ||
                    strncmp(self->ptr, "-inf", 4) == 0))
    {
        type = 'f';
    }

    // If there is a base prefix, set the base and strip the prefix,
    // because strtoll() do not recognize 0o and 0b
    if (self->ptr + 2 <= self->end)
    {
        if (strncmp(self->ptr, "0x", 2) == 0)
        {
            base = 16;
            toml_next_n(self, 2);
        }
        else if (strncmp(self->ptr, "0o", 2) == 0)
        {
            base = 8;
            toml_next_n(self, 2);
        }
        else if (strncmp(self->ptr, "0b", 2) == 0)
        {
            base = 2;
            toml_next_n(self, 2);
        }
    }

    char last_char = 0;
    int has_exp = TOML_FALSE;
    while (self->ptr < self->end)
    {
        if (*self->ptr == '+' || *self->ptr == '-')
        {
            if (last_char == 0 || ((last_char == 'e' || last_char == 'E') && !has_exp))
            {
                if (last_char != 0)
                {
                    type = 'f';
                    has_exp = TOML_TRUE;
                }
                toml_string_append_char(str, *self->ptr, &err);
                if (err.code != TOML_OK) goto cleanup;
            }
            else
            {
                break;
            }
        }
        else if (isalnum(*self->ptr))
        {
            if ((*self->ptr == 'e' || *self->ptr == 'E') && base == 10)
            {
                type = 'f';
            }

            toml_string_append_char(str, *self->ptr, &err);
            if (err.code != TOML_OK) goto cleanup;
        }
        else if (*self->ptr == '.')
        {
            if (type == 'i')
            {
                type = 'f';
                toml_string_append_char(str, *self->ptr, &err);
                if (err.code != TOML_OK) goto cleanup;
            }
            else
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid float",
                        self->filename, self->lineno, self->colno);
                goto cleanup;
            }
        }
        else if (*self->ptr == '_')
        {
            if (type == 't')
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid datetime",
                        self->filename, self->lineno, self->colno);
                goto cleanup;
            }

            if (!isalnum(last_char))
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid integer or float",
                        self->filename, self->lineno, self->colno);
                goto cleanup;
            }
        }
        else if (*self->ptr == '-')
        {
            type = 't';
            toml_string_append_char(str, *self->ptr, &err);
        }
        else
        {
            break;
        }

        last_char = *self->ptr;
        toml_move_next(self);
    }

    if (last_char == '_')
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid integer or float or datetime",
                self->filename, self->lineno, self->colno);
        goto cleanup;
    }

    if (type == 'i')
    {
        char *end = NULL;
        long long n = strtoll(str->str, &end, base);
        if (end < str->str + str->len)
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid integer",
                    self->filename, self->lineno, self->colno);
            goto cleanup;
        }
        result = toml_value_new_integer(n, &err);
        if (err.code != TOML_OK) goto cleanup;
    }
    else if (type == 'f')
    {
        char *end = NULL;
        double n = strtod(str->str, &end);
        if (end < str->str + str->len)
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: invalid float");
            goto cleanup;
        }
        result = toml_value_new_float(n, &err);
        if (err.code != TOML_OK) goto cleanup;
    }
    else if (type == 't')
    {
        result = toml_parse_datetime(str->str, str->len, &err);
        if (err.code != TOML_OK) goto cleanup;
    }

    cleanup:
    toml_string_free(str);
    toml_err_move(error, &err);
    return result;
}

TomlValue *toml_parse_bool(TomlParser *self, TomlErr *error)
{
    if (self->ptr + 4 <= self->end && strncmp(self->ptr, "true", 4) == 0 &&
            (self->ptr + 4 == self->end || isspace(*(self->ptr + 4))))
    {
        toml_next_n(self, 4);
        return toml_value_new_boolean(TOML_TRUE, error);
    }

    if (self->ptr + 5 <= self->end && strncmp(self->ptr, "false", 5) == 0 &&
            (self->ptr + 5 == self->end || isspace(*(self->ptr + 5))))
    {
        toml_next_n(self, 5);
        return toml_value_new_boolean(TOML_FALSE, error);
    }

    return NULL;
}

TomlValue *toml_parse_array(TomlParser *self, TomlErr *error);

TomlValue *toml_parse_inline_table(TomlParser *self, TomlErr *error);

TomlValue *toml_parse_value(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlValue *value = NULL;

    char ch = *self->ptr;

    if (strncmp(self->ptr, "\"\"\"", 3) == 0)
    {
        toml_next_n(self, 3);
        value = toml_parse_multi_line_basic_string(self, &err);
    }
    else if (strncmp(self->ptr, "\'\'\'", 3) == 0)
    {
        toml_next_n(self, 3);
        value = toml_parse_multi_line_literal_string(self, &err);
    }
    else if (ch == '\"')
    {
        toml_move_next(self);
        value = toml_parse_basic_string_value(self, &err);
    }
    else if (ch == '\'')
    {
        toml_move_next(self);
        value = toml_parse_literal_string_value(self, &err);
    }
    else if (isdigit(ch) || ch == '+' || ch == '-' || ch == '.' || ch == 'n' || ch == 'i')
    {
        value = toml_parse_int_or_float_or_time(self, &err);
    }
    else if (ch == 't' || ch == 'f')
    {
        value = toml_parse_bool(self, &err);
    }
    else if (ch == '[')
    {
        toml_move_next(self);
        value = toml_parse_array(self, &err);
    }
    else if (ch == '{')
    {
        toml_move_next(self);
        value = toml_parse_inline_table(self, &err);
    }
    else
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unexpected token",
                self->filename, self->lineno, self->colno);
    }

    toml_err_move(error, &err);
    return value;
}

void toml_parse_key_value(TomlParser *self, TomlTable *table, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlString *key = NULL;
    TomlValue *value = NULL;

    while (self->ptr < self->end)
    {
        char ch;

        ch = *self->ptr;
        while (self->ptr < self->end && isspace(ch))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (self->ptr == self->end) break;

        if (isalnum(ch) || ch == '_')
        {
            key = toml_parse_bare_key(self, &err);
        }
        else if (ch == '\"')
        {
            toml_move_next(self);
            key = toml_parse_basic_string(self, &err);
        }
        else if (ch == '\'')
        {
            toml_move_next(self);
            key = toml_parse_literal_string(self, &err);
        }
        else if (ch == '[')
        {
            break;
        }
        else if (ch == '#')
        {
            do
            {
                toml_move_next(self);
                ch = *self->ptr;
            } while (self->ptr < self->end && ch != '\n');
            toml_move_next(self);
            continue;
        }
        else
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unexpected token",
                    self->filename, self->lineno, self->colno);
            goto error;
        }

        ch = *self->ptr;
        while (self->ptr < self->end && (ch == ' ' || ch == '\t'))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (self->ptr == self->end)
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unterminated key value pair");
            goto error;
        }

        if (ch != '=')
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unexpected token",
                    self->filename, self->lineno, self->colno);
            goto error;
        }

        toml_move_next(self);

        ch = *self->ptr;
        while (self->ptr < self->end && (ch == ' ' || ch == '\t'))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (self->ptr == self->end)
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unterminated key value pair");
            goto error;
        }

        value = toml_parse_value(self, &err);
        if (err.code != TOML_OK) goto error;

        toml_table_set_by_string(table, key, value, &err);
        if (err.code != TOML_OK) goto error;

        key = NULL;
        value = NULL;

        while (self->ptr < self->end && (*self->ptr == ' ' || *self->ptr == '\t'))
        {
            toml_move_next(self);
        }

        if (self->ptr == self->end)
        {
            break;
        }

        if (*self->ptr == '#')
        {
            do
            {
                toml_move_next(self);
            } while (self->ptr < self->end && *self->ptr != '\n');
        }

        if (*self->ptr == '\r')
        {
            toml_move_next(self);
        }

        if (*self->ptr == '\n')
        {
            toml_move_next(self);
        }
        else
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: new line expected",
                    self->filename, self->lineno, self->colno);
            goto error;
        }
    }

    goto cleanup;

    error:
    toml_value_free(value);
    toml_string_free(key);

    cleanup:
    toml_err_move(error, &err);
}

TomlValue *toml_parse_array(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlValue *array = NULL;
    TomlValue *value = NULL;

    array = toml_value_new_array(&err);
    if (err.code != TOML_OK) goto cleanup;

    while (self->ptr < self->end)
    {
        if (isspace(*self->ptr))
        {
            while (self->ptr < self->end && isspace(*self->ptr))
            {
                toml_move_next(self);
            }
        }
        else if (*self->ptr == '#')
        {
            do
            {
                toml_move_next(self);
            } while (self->ptr < self->end && *self->ptr != '\n');
            toml_move_next(self);
        }
        else if (*self->ptr == '\n')
        {
            toml_move_next(self);
        }
        else if (*self->ptr == ']')
        {
            toml_move_next(self);
            break;
        }
        else
        {
            value = toml_parse_value(self, &err);
            if (err.code != TOML_OK) goto error;

            toml_array_append(array->value.array, value, &err);
            if (err.code != TOML_OK) goto error;

            value = NULL;

            while (self->ptr < self->end)
            {
                if (isspace(*self->ptr))
                {
                    do
                    {
                        toml_move_next(self);
                    } while (self->ptr < self->end && isspace(*self->ptr));
                }
                else if (*self->ptr == '#')
                {
                    do
                    {
                        toml_move_next(self);
                    } while (self->ptr < self->end && *self->ptr != '\n');
                }
                else
                {
                    break;
                }
            }

            if (self->ptr < self->end && *self->ptr == ',')
            {
                toml_move_next(self);
            }
        }
    }

    goto cleanup;

    error:
    toml_value_free(value);

    cleanup:
    toml_err_move(error, &err);
    return array;
}

TomlValue *toml_parse_inline_table(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlValue *table = NULL;

    table = toml_value_new_table(&err);
    if (err.code != TOML_OK) goto cleanup;

    while (self->ptr < self->end)
    {
        TomlString *key = NULL;
        TomlValue *value = NULL;
        char ch;

        ch = *self->ptr;
        while (self->ptr < self->end && (ch == ' ' || ch == '\t'))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (isalnum(ch) || ch == '_')
        {
            key = toml_parse_bare_key(self, &err);
        }
        else if (ch == '\"')
        {
            toml_move_next(self);
            key = toml_parse_basic_string(self, &err);
        }
        else if (ch == '\'')
        {
            toml_move_next(self);
            key = toml_parse_literal_string(self, &err);
        }
        else if (ch == '}')
        {
            break;
        }
        else
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unexpected token",
                    self->filename, self->lineno, self->colno);
            goto cleanup;
        }

        ch = *self->ptr;
        while (self->ptr < self->end && (ch == ' ' || ch == '\t'))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (self->ptr == self->end)
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unterminated key value pair");
            goto cleanup;
        }

        if (ch != '=')
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unexpected token",
                    self->filename, self->lineno, self->colno);
            goto cleanup;
        }

        toml_move_next(self);

        ch = *self->ptr;
        while (self->ptr < self->end && (ch == ' ' || ch == '\t'))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (self->ptr == self->end)
        {
            toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unterminated key value pair");
            goto cleanup;
        }

        value = toml_parse_value(self, &err);
        if (err.code != TOML_OK) goto cleanup;

        toml_table_set_by_string(table->value.table, key, value, &err);
        if (err.code != TOML_OK) goto cleanup;

        while (self->ptr < self->end && (*self->ptr == ' ' || *self->ptr == '\t'))
        {
            toml_move_next(self);
        }

        if (*self->ptr == ',')
        {
            toml_move_next(self);
        }
        else if (*self->ptr == '}')
        {
            toml_move_next(self);
            break;
        }
    }

    cleanup:
    toml_err_move(error, &err);
    return table;
}

TomlTable *toml_walk_table_path(TomlParser *parser, TomlTable *table,
        TomlArray *key_path, int is_array,
        int create_if_not_exist, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlTable *real_table = table;
    TomlValue *new_table = NULL;
    TomlValue *array = NULL;
    TomlString *part_copy = NULL;

    if (is_array)
    {
        size_t i = 0;
        for (; i < key_path->len - 1; i++)
        {
            TomlString *part = key_path->elements[i]->value.string;
            TomlValue *t = toml_table_get_by_string(real_table, part);
            if (t == NULL)
            {
                if (create_if_not_exist)
                {
                    new_table = toml_value_new_table(&err);
                    if (err.code != TOML_OK) goto error;

                    part_copy = toml_string_copy(part, &err);
                    if (err.code != TOML_OK) goto error;

                    toml_table_set_by_string(real_table, part_copy, new_table, &err);
                    if (err.code != TOML_OK) goto error;

                    real_table = new_table->value.table;

                    part_copy = NULL;
                    new_table = NULL;
                }
                else
                {
                    real_table = NULL;
                    break;
                }
            }
            else
            {
                real_table = t->value.table;
            }
        }

        TomlString *part = key_path->elements[i]->value.string;
        TomlValue *t = toml_table_get_by_string(real_table, part);
        if (t == NULL)
        {
            if (create_if_not_exist)
            {
                array = toml_value_new_array(&err);
                if (err.code != TOML_OK) goto error;

                new_table = toml_value_new_table(&err);
                if (err.code != TOML_OK) goto error;

                toml_array_append(array->value.array, new_table, &err);
                if (err.code != TOML_OK) goto error;

                part_copy = toml_string_copy(part, &err);
                if (err.code != TOML_OK) goto error;

                toml_table_set_by_string(real_table, part_copy, array, &err);
                if (err.code != TOML_OK) goto error;

                real_table = new_table->value.table;

                part_copy = NULL;
                array = NULL;
                new_table = NULL;
            }
            else
            {
                real_table = NULL;
            }
        }
        else
        {
            if (t->type != TOML_ARRAY)
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: this key was not an array",
                        parser->filename, parser->lineno, parser->colno);
                goto error;
            }

            TomlValue *temp_table = toml_value_new_table(&err);
            if (err.code != TOML_OK) goto error;

            toml_array_append(t->value.array, temp_table, &err);
            if (err.code != TOML_OK) goto error;

            real_table = temp_table->value.table;
        }
    }
    else
    {
        for (size_t i = 0; i < key_path->len; i++)
        {
            TomlString *part = key_path->elements[i]->value.string;
            TomlValue *t = toml_table_get_by_string(real_table, part);
            if (t == NULL)
            {
                if (create_if_not_exist)
                {
                    new_table = toml_value_new_table(&err);
                    if (err.code != TOML_OK) goto error;

                    part_copy = toml_string_copy(part, &err);
                    if (err.code != TOML_OK) goto error;

                    toml_table_set_by_string(real_table, part_copy, new_table, &err);
                    if (err.code != TOML_OK) goto error;

                    real_table = new_table->value.table;

                    part_copy = NULL;
                    new_table = NULL;
                }
                else
                {
                    real_table = NULL;
                    break;
                }
            }
            else
            {
                if (t->type == TOML_ARRAY)
                {
                    real_table = t->value.array->elements[t->value.array->len - 1]->value.table;
                }
                else if (t->type == TOML_TABLE)
                {
                    real_table = t->value.table;
                }
            }
        }
    }

    goto cleanup;

    error:
    toml_string_free(part_copy);
    toml_value_free(new_table);
    toml_value_free(array);

    cleanup:
    toml_err_move(error, &err);
    return real_table;
}

void toml_parse_table(TomlParser *self, TomlTable *table, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlArray *key_path = NULL;
    int is_array = TOML_FALSE;
    TomlTable *real_table = table;

    key_path = toml_array_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    if (self->ptr < self->end && *self->ptr == '[')
    {
        is_array = TOML_TRUE;
        toml_move_next(self);
    }

    while (1)
    {
        if (*self->ptr == ' ' || *self->ptr == '\t')
        {
            do
            {
                toml_move_next(self);
            } while (*self->ptr < *self->end &&
                    (*self->ptr == ' ' || *self->ptr == '\t'));
        }
        else if (*self->ptr == ']')
        {
            if (is_array)
            {
                if (self->ptr + 2 <= self->end && strncmp(self->ptr, "]]", 2) == 0)
                {
                    toml_next_n(self, 2);
                    break;
                }
            }
            else
            {
                toml_move_next(self);
                break;
            }
        }
        else
        {
            TomlString *key_part = NULL;
            TomlValue *key_part_value = NULL;

            if (isalnum(*self->ptr) || *self->ptr == '_')
            {
                key_part = toml_parse_bare_key(self, &err);
            }
            else if (*self->ptr == '\"')
            {
                toml_move_next(self);
                key_part = toml_parse_basic_string(self, &err);
            }
            else if (*self->ptr == '\'')
            {
                toml_move_next(self);
                key_part = toml_parse_literal_string(self, &err);
            }
            else
            {
                toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: unexpected token",
                        self->filename, self->lineno, self->colno);
            }
            if (err.code != TOML_OK) goto cleanup;

            key_part_value = toml_value_new(TOML_STRING, &err);
            if (err.code != TOML_OK) goto cleanup;

            key_part_value->value.string = key_part;

            toml_array_append(key_path, key_part_value, &err);
            if (err.code != TOML_OK) goto cleanup;

            while (self->ptr < self->end &&
                    (*self->ptr == ' ' || *self->ptr == '\t'))
            {
                toml_move_next(self);
            }

            if (self->ptr < self->end && *self->ptr == '.')
            {
                toml_move_next(self);
            }
        }
    }

    if (key_path->len == 0)
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: empty table name",
                self->filename, self->lineno, self->colno);
        goto cleanup;
    }

    while (self->ptr < self->end &&
            (*self->ptr == ' ' || *self->ptr == '\t' || *self->ptr == '\r'))
    {
        toml_move_next(self);
    }

    if (self->ptr < self->end && *self->ptr != '\n')
    {
        toml_set_err(&err, TOML_ERR_SYNTAX, "%s:%d:%d: new line expected",
                self->filename, self->lineno, self->colno);
        goto cleanup;
    }

    real_table = toml_walk_table_path(self, table, key_path, is_array, TOML_TRUE, &err);
    if (err.code != TOML_OK) goto cleanup;

    toml_parse_key_value(self, real_table, &err);

    cleanup:
    toml_array_free(key_path);
    toml_err_move(error, &err);
}

TomlTable *toml_parse(TomlParser *self, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;

    TomlTable *table = NULL;

    table = toml_table_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    while (self->ptr < self->end)
    {
        char ch = *self->ptr;

        while (self->ptr < self->end && isspace(ch))
        {
            toml_move_next(self);
            ch = *self->ptr;
        }

        if (ch == '#')
        {
            do
            {
                toml_move_next(self);
                ch = *self->ptr;
            } while (self->ptr < self->end && ch != '\n');
            toml_move_next(self);
        }
        else if (ch == '[')
        {
            toml_move_next(self);
            toml_parse_table(self, table, &err);
            if (err.code != TOML_OK) goto cleanup;
        }
        else if (isalnum(ch) || ch == '_' || ch == '-')
        {
            toml_parse_key_value(self, table, &err);
            if (err.code != TOML_OK) goto cleanup;
        }
        else if (ch == ' ' || ch == '\t' || ch == '\r')
        {
            do
            {
                toml_move_next(self);
                ch = *self->ptr;
            } while (ch == ' ' || ch == '\t' || ch == '\r');
        }
        else if (ch == '\n')
        {
            toml_move_next(self);
        }
    }

    cleanup:
    toml_err_move(error, &err);
    return table;
}

TomlTable *toml_load_nstring_filename(TOML_CONST char *str, size_t len,
        TOML_CONST char *filename, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlParser *parser = NULL;
    TomlTable *table = NULL;

    parser = toml_parser_new(str, len, &err);
    if (err.code != TOML_OK) goto cleanup;

    parser->filename = toml_strdup(filename);

    table = toml_parse(parser, &err);
    if (err.code != TOML_OK) goto cleanup;

    cleanup:
    toml_parser_free(parser);
    toml_err_move(error, &err);
    return table;
}

TomlTable *toml_load_nstring(TOML_CONST char *str, size_t len, TomlErr *error)
{
    return toml_load_nstring_filename(str, len, "<string>", error);
}

TomlTable *toml_load_string(TOML_CONST char *str, TomlErr *error)
{
    return toml_load_nstring(str, sizeof(str), error);
}

TomlTable *toml_load_file_filename(FILE *file, TOML_CONST char *filename, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlTable *table = NULL;
    TomlString *str = NULL;

    str = toml_string_new(&err);
    if (err.code != TOML_OK) goto cleanup;

    toml_string_check_expand(str, 4095, &err);
    if (err.code != TOML_OK) goto cleanup;

    size_t count;
    size_t bytes_to_read;
    do
    {
        bytes_to_read = str->_capacity - str->len - 1;

        count = fread(str->str, 1, bytes_to_read, file);

        if (str->len + 1 >= str->_capacity)
        {
            toml_string_check_expand(str, str->_capacity * 2, &err);
            if (err.code != TOML_OK) goto cleanup;
        }

        str->len += count;
    } while (count == bytes_to_read);

    str->str[str->len] = 0;

    if (ferror(file))
    {
        toml_set_err_literal(&err, TOML_ERR_IO, "error when reading file");
        goto cleanup;
    }

    table = toml_load_nstring_filename(str->str, str->len, filename, &err);

    cleanup:
    toml_string_free(str);
    toml_err_move(error, &err);
    return table;
}

TomlTable *toml_load_file(FILE *file, TomlErr *error)
{
    return toml_load_file_filename(file, "<stream>", error);
}

TomlTable *toml_load_filename(TOML_CONST char *filename, TomlErr *error)
{
    TomlErr err = TOML_ERR_INIT;
    TomlTable *table = NULL;
    FILE *f = NULL;

    f = fopen(filename, "r");
    if (f == NULL)
    {
        toml_set_err(&err, TOML_ERR_IO, "cannot open file: %s", filename);
        goto cleanup;
    }

    table = toml_load_file_filename(f, filename, &err);

    cleanup:
    if (f != NULL) fclose(f);
    toml_err_move(error, &err);
    return table;
}
