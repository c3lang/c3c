#!/usr/bin/awk -f

BEGIN { Out = 1 }
{
    line = ""
    for (i = 1; i <= length($0); i++) {
        char = substr($0, i, 1)
        next_char = substr($0, i+1, 1)

        if (Out) {
            if (char == "\"" && prev_char != "\\") { string = !string }
            if (!string && char == "/" && next_char == "*") { Out = 0; i++; continue }
            if (!string && char == "/" && next_char == "/") { break }
            line = line char
        } else {
            if (char == "*" && next_char == "/") { Out = 1; i++; continue }
        }
        prev_char = char
    }
    if (line ~ /[^[:space:]]/) print line
}