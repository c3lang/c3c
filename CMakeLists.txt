cmake_minimum_required(VERSION 3.13)
project(c3c C)
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

set(CMAKE_C_STANDARD 11)

include_directories(
        "${CMAKE_SOURCE_DIR}/src/"
        "${CMAKE_SOURCE_DIR}/build/")

add_executable(c3c
        src/main.c
        src/build/build_options.c
        src/build/project_creation.c
        src/utils/errors.c
        src/utils/file_utils.c
        src/utils/string_utils.c
        src/compiler/lexer.c src/compiler/lexer.h src/compiler_tests/tests.c src/compiler_tests/tests.h src/compiler_tests/benchmark.c src/compiler_tests/benchmark.h)