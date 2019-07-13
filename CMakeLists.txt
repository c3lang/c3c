cmake_minimum_required(VERSION 3.13)
project(c3c C)

set(CMAKE_C_STANDARD 11)

add_executable(c3c main.c build/build_options.c build/build_options.h build/project_creation.c build/project_creation.h utils/string_utils.c utils/string_utils.h utils/file_utils.c utils/file_utils.h utils/errors.c utils/errors.h)