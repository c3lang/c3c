/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __TOML_H__
#define __TOML_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define TOML_FALSE  0
#define TOML_TRUE   1

#if !defined(_MSC_VER) || _MSC_VER >= 1800
#define TOML_CONST const
#else
#define TOML_CONST
#endif

enum
{
    TOML_OK,
    TOML_ERR,
    TOML_ERR_IO,
    TOML_ERR_NOMEM,
    TOML_ERR_SYNTAX,
};

typedef struct
{
    int code;
    char *message;
    int _is_literal;
} TomlErr;

typedef struct
{
    char *str;
    size_t len;
    size_t _capacity;
} TomlString;

typedef struct _TomlValue TomlValue;

typedef struct
{
    TomlValue **elements;
    size_t len;
    size_t _capacity;
} TomlArray;

typedef struct
{
    TomlString *key;
    TomlValue *value;
} TomlKeyValue;

typedef struct _TomlTable TomlTable;
typedef struct _TomlTableIter TomlTableIter;

typedef struct
{
    int year;
    int month;
    int day;
    int hour;
    int minute;
    double second;
    int offset_hour;
    int offset_minute;
} TomlDateTime;

typedef enum
{
    TOML_TABLE,
    TOML_ARRAY,
    TOML_STRING,
    TOML_INTEGER,
    TOML_FLOAT,
    TOML_DATETIME,
    TOML_BOOLEAN,
} TomlType;

struct _TomlValue
{
    TomlType type;
    union
    {
        TomlTable *table;
        TomlArray *array;
        TomlString *string;
#if defined(_MSC_VER)
        long long       integer;
#else
        long integer;
#endif
        double float_;
        TomlDateTime *datetime;
        int boolean;
    } value;
};

char *toml_strdup(TOML_CONST char *str);

char *toml_strndup(TOML_CONST char *str, size_t n);

int toml_vasprintf(char **str, TOML_CONST char *format, va_list args);

int toml_asprintf(char **str, TOML_CONST char *format, ...);

#define TOML_ERR_INIT {TOML_OK, NULL, TOML_FALSE}

void toml_err_init(TomlErr *err);

void toml_clear_err(TomlErr *err);

void toml_err_move(TomlErr *to, TomlErr *from);

void toml_set_err_v(TomlErr *err, int code, TOML_CONST char *format, va_list args);

void toml_set_err(TomlErr *err, int code, TOML_CONST char *format, ...);

void toml_set_err_literal(TomlErr *err, int code, TOML_CONST char *message);

TomlString *toml_string_new(TomlErr *err);

TomlString *toml_string_new_string(TOML_CONST char *str, TomlErr *err);

TomlString *toml_string_new_nstring(TOML_CONST char *str, size_t len, TomlErr *err);

void toml_string_append_char(TomlString *self, char ch, TomlErr *err);

void toml_string_append_string(TomlString *self, TOML_CONST char *str, TomlErr *err);

void toml_string_append_nstring(TomlString *self, TOML_CONST char *str, size_t len, TomlErr *err);

TomlString *toml_string_copy(TOML_CONST TomlString *self, TomlErr *err);

void toml_string_free(TomlString *self);

int toml_string_equals(TOML_CONST TomlString *self, TOML_CONST TomlString *other);

TomlTable *toml_table_new(TomlErr *err);

void toml_table_free(TomlTable *self);

void toml_table_set_by_string(TomlTable *self, TomlString *key,
        TomlValue *value, TomlErr *err);

TomlValue *toml_table_get_by_string(TOML_CONST TomlTable *self, TOML_CONST TomlString *key);

void toml_table_set(TomlTable *self, TOML_CONST char *key, TomlValue *value, TomlErr *err);

void toml_table_setn(TomlTable *self, TOML_CONST char *key, size_t key_len,
        TomlValue *value, TomlErr *err);

TomlValue *toml_table_get(TOML_CONST TomlTable *self, TOML_CONST char *key);

TomlValue *toml_table_getn(TOML_CONST TomlTable *self, TOML_CONST char *key, size_t key_len);

TomlTableIter *toml_table_iter_new(TomlTable *table, TomlErr *err);

void toml_table_iter_free(TomlTableIter *self);

TomlKeyValue *toml_table_iter_get(TomlTableIter *self);

int toml_table_iter_has_next(TomlTableIter *self);

void toml_table_iter_next(TomlTableIter *self);

TomlArray *toml_array_new(TomlErr *err);

void toml_array_free(TomlArray *self);

void toml_array_append(TomlArray *self, TomlValue *value, TomlErr *err);

TomlValue *toml_value_new(TomlType type, TomlErr *err);

TomlValue *toml_value_new_string(TOML_CONST char *str, TomlErr *err);

TomlValue *toml_value_new_nstring(TOML_CONST char *str, size_t len, TomlErr *err);

TomlValue *toml_value_new_table(TomlErr *err);

TomlValue *toml_value_new_array(TomlErr *err);

#if defined(_MSC_VER)
TomlValue *toml_value_new_integer(long long integer, TomlErr *err);
#else

TomlValue *toml_value_new_integer(long integer, TomlErr *err);

#endif

TomlValue *toml_value_new_float(double flt, TomlErr *err);

TomlValue *toml_value_new_datetime(TomlErr *err);

TomlValue *toml_value_new_boolean(int boolean, TomlErr *err);

void toml_value_free(TomlValue *self);

TomlTable *toml_load_string(TOML_CONST char *str, TomlErr *err);

TomlTable *toml_load_nstring(TOML_CONST char *str, size_t len, TomlErr *err);

TomlTable *toml_load_file(FILE *file, TomlErr *err);

TomlTable *toml_load_filename(TOML_CONST char *filename, TomlErr *err);

/* TODO: implement dump functions
char *toml_dump_string(TOML_CONST TomlTable *self, TomlErr *err);
TomlString *toml_dump_nstring(TOML_CONST TomlTable *self, TomlErr *err);
void toml_dump_file(TOML_CONST TomlTable *self, FILE *file, TomlErr *err);
*/

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: __TOML_H__ */
