#!/usr/bin/env python3
import sys
from pathlib import Path

if len(sys.argv) != 4:
    print(f"Usage: {sys.argv[0]} <input_file> <output_file> <var_name>")
    sys.exit(1)

input_path = Path(sys.argv[1])
output_path = Path(sys.argv[2])
var_name = sys.argv[3]

data = input_path.read_bytes()

lines = []
for i in range(0, len(data), 16):
    chunk = data[i:i + 16]
    hex_str = ", ".join(f"0x{b:02x}" for b in chunk)
    lines.append(f"    {hex_str},")

formatted_array = "\n".join(lines)
if formatted_array:
    formatted_array += "\n"

content = f"""/* Auto-generated from {input_path.name} */
#pragma once
static const unsigned char {var_name}[] = {{
{formatted_array}    0x00
}};
static const unsigned int {var_name}_len = sizeof({var_name}) - 1;
"""

output_path.parent.mkdir(parents=True, exist_ok=True)
output_path.write_text(content, encoding="utf-8")