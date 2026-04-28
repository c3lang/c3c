# embed.cmake
# Usage: cmake -DINPUT=file -DOUTPUT=file.h -DVAR_NAME=var_name -P embed.cmake

file(READ "${INPUT}" file_data HEX)
string(REGEX MATCHALL ".." hex_bytes "${file_data}")
set(array_items "")
set(count 0)
foreach(byte IN LISTS hex_bytes)
    string(APPEND array_items "0x${byte}, ")
    math(EXPR count "${count} + 1")
    if (count EQUAL 16)
        string(APPEND array_items "\n    ")
        set(count 0)
    endif()
endforeach()

set(header_content "/* Auto-generated from ${INPUT} */\n#pragma once\nstatic const unsigned char ${VAR_NAME}[] = {\n    ${array_items}0x00\n};\nstatic const unsigned int ${VAR_NAME}_len = sizeof(${VAR_NAME}) - 1;\n")

file(WRITE "${OUTPUT}" "${header_content}")
