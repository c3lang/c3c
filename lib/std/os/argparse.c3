<*
## High-Level API: `@parse`

The `@parse` macro parses a command-line argument slice. It automatically extracts option names, processes qualifiers, maps values to variables, and handles sequential arrays, enums, callbacks, and environment fallbacks.

Options can be specified using either:
* **Triplets**: `short_name, long_name, &assignee`
* **Quartets**: `short_name, long_name, description, &assignee`

You can freely mix triplets and quartets within the same `@parse` invocation.

### Basic Example

```c3
import std::io;
import std::os::argparse;

fn int main(String[] args)
{
	bool help = false;
	int port = 8080;
	String host = "127.0.0.1";

	sz? result = argparse::@parse(args,
		"h*", "help*", "Show help manual",         &help,
		"p",  "port",                              &port,
		"s",  "server", "The server host address", &host
	);

	if (catch err = result)
	{
		if (err == argparse::HELP_REQUESTED) return 0;

		io::eprintfn("Command-line error: %s", err);
		return 1;
	}

	io::printfn("Starting server on %s:%d", host, port);
	return 0;
}
```

---

## Option Qualifiers

Qualifiers can be appended to option names (either short or long) to modify their behavior at compile time.

### 1. Help Qualifier (`*`)
Any flag designated with `*` acts as a help trigger. When matched, the macro automatically formats and prints a left-aligned options block using the descriptions provided in the quartets, then returns `argparse::HELP_REQUESTED`.
```c3
"h*", "help*", "Show this help screen", &help
```

### 2. Optional Suffix (`?`)
By default, non-boolean variables require an argument (e.g. `--port 8080`). Appending `?` to the option name makes its argument optional. If the flag is provided without an argument, boolean targets are set to `true`, and other types retain their defaults or are parsed if a value is present.
```c3
"v?", "verbose?", "Toggle verbose output", &verbose
```

### 3. Incremental Counters (`+`)
Appending `+` to an option name increments the target numeric variable every time the flag is specified.

The parser supports both standard stacked short flags (e.g., `-vvv`) as well as repeated occurrences of flags.
```c3
"v+", "verbose+", "Increase verbosity", &verbosity
// Passing "-vvv -v" results in verbosity = 4
// Passing "-vv --verbose -vvv -v" results in verbosity = 7
```

---

## Advanced Features

### 1. Environment Variable Fallbacks
Long options can be mapped to environment variable fallbacks using `=env:VAR_NAME`. If the flag is omitted from the command-line invocation, the parser queries the specified environment variable before finishing.

```c3
int port = 8080;

argparse::@parse(args,
	"p", "port=env:PORT", "Port number (defaults to $PORT)", &port
);
```

### 2. Custom Validation Callbacks
You can pass a callback function of type `fn void? (String)` as an option assignee. The callback is invoked with the parsed option value. Returning a fault from this function aborts option parsing and propagates the fault out of `@parse`.

```c3
fn void? validate_port(String value)
{
	int? port = value.to_int();
	if (catch err = port) return argparse::INVALID_VALUE~;
	if (port < 1024) return argparse::INVALID_VALUE~;
	return;
}

// ...
argparse::@parse(args,
	"p", "port", "Custom validated port", &validate_port
);
```

### 3. Sequential Array Collection
Passing a pointer to a fixed-size array collects sequential occurrences of that option flag into the array.

If the array capacity is exceeded, the parser returns `argparse::OUT_OF_BOUNDS`.

```c3
String[3] input_files = {};

argparse::@parse(args,
	"f", "file", "Target files to process", &input_files
);
// Passing "-f file1.txt -f file2.txt" populates input_files[0] and input_files[1]
```

### 4. Automatic Enum Mapping
If you pass a pointer to an `enum`, the parser case-insensitively maps CLI string values to the enum member names, falling back to matching integer ordinal indices.

```c3
enum LogLevel { DEBUG, INFO, WARNING, ERROR }
LogLevel level = LogLevel.INFO;

argparse::@parse(args,
	"l", "level", "Select logging level", &level
);
// Values like "debug", "DEBUG", "2" are mapped automatically.
```

### 5. Strict Option Parsing Mode
If the first parameter after `args` is a compile-time `bool` set to `true`, the parser is configured in strict POSIX mode. Option-seeking terminates immediately upon encountering the first raw positional argument. Any subsequent flags are treated as positional arguments.

```c3
bool verbose = false;
String[3] positionals = {};

argparse::@parse(args, true,
	"v", "verbose", "Verbose mode", &verbose,
	&positionals
);
```

### 6. Positional Arguments Capture
If a final trailing argument (not part of a triplet or quartet) is provided to `@parse`, it acts as a positional target array. Unhandled positional arguments are stored there in order.

```c3
String[10] positional_args = {};

argparse::@parse(args,
	"v", "verbose", "Verbose mode", &verbose,
	&positional_args // Stores up to 10 positional arguments
);
```

### 7. End-of-Options Marker (`--`)
The standard double-hyphen marker `--` disables option-parsing for any subsequent arguments. All arguments following `--` are treated strictly as positional arguments, even if they begin with a hyphen.

---

## Low-Level API: `ArgParser`

For manual or non-standard argument processing, you can bypass the compile-time macro and use the zero-allocation `ArgParser` struct directly.

```c3
import std::io;
import std::os::argparse;

fn void parse_manually(String[] args)
{
	ArgParser parser;
	parser.init(args);

	bool is_option;
	while (try token = parser.next(&is_option))
	{
		if (is_option)
		{
			if (token == "p" || token == "port")
			{
				if (try val = parser.consume_value())
				{
					io::printfn("Custom Port: %s", val);
				}
			}
		}
		else
		{
			io::printfn("Custom Positional: %s", token);
		}
	}
}
```
*>
module std::os::argparse;

import std::io;
import std::os::env;

faultdef
	MISSING_ARGUMENT,
	ILLEGAL_OPTION,
	HELP_REQUESTED,
	OUT_OF_BOUNDS,
	INVALID_VALUE,
	INVALID_ENUM_VALUE,
	END_OF_ARGS;

struct ArgParser
{
	String[] args;
	sz index;
	String short_remain;
	String pending_value;
	bool has_pending_value;
	bool strict_mode;
	bool stop_options;
}

fn void ArgParser.init(ArgParser* self, String[] args, bool strict_mode = false)
{
	*self = {
		.args = args,
		.index = 1,
		.strict_mode = strict_mode
	};
}

fn String? ArgParser.next(ArgParser* self, bool* is_option)
{
	*is_option = false;

	if (self.short_remain.len > 0)
	{
		String opt = self.short_remain[:1];
		self.short_remain = self.short_remain[1..];
		*is_option = true;
		return opt;
	}

	if (self.index >= self.args.len) return END_OF_ARGS~;

	String arg = self.args[self.index];

	if (arg == "--")
	{
		self.index++;
		self.stop_options = true;
		if (self.index >= self.args.len) return END_OF_ARGS~;
		arg = self.args[self.index];
	}

	if (self.stop_options)
	{
		self.index++;
		return arg;
	}

	if (arg.starts_with("--"))
	{
		self.index++;
		*is_option = true;
		String opt_part = arg[2..];
		if (try eq_idx = opt_part.index_of_char('='))
		{
			self.pending_value = opt_part[eq_idx + 1..];
			self.has_pending_value = true;
			return opt_part[:eq_idx];
		}
		return opt_part;
	}

	// Avoid treating negative numbers as options
	if (arg.starts_with("-") && arg.len > 1 && (arg[1] < '0' || arg[1] > '9'))
	{
		self.index++;
		*is_option = true;
		self.short_remain = arg[2..];
		return arg[1:1];
	}

	if (self.strict_mode)
	{
		self.stop_options = true;
	}
	self.index++;
	return arg;
}

fn bool ArgParser.has_value(ArgParser* self)
{
	if (self.has_pending_value) return true;
	if (self.short_remain.len > 0) return true;
	if (self.index >= self.args.len) return false;

	String next_arg = self.args[self.index];
	if (next_arg.starts_with("-"))
	{
		// If it's a single "-" or a negative number (like "-10"), it is a valid value
		if (next_arg.len == 1 || (next_arg[1] >= '0' && next_arg[1] <= '9'))
		{
			return true;
		}
		return false;
	}
	return true;
}

fn String? ArgParser.consume_value(ArgParser* self)
{
	switch
	{
		case self.has_pending_value:
			self.has_pending_value = false;
			return self.pending_value;

		case self.short_remain.len > 0:
			defer self.short_remain = "";
			return self.short_remain;

		case self.index >= self.args.len:
			return MISSING_ARGUMENT~;
	}

	defer self.index++;
	return self.args[self.index];
}

<* Parses a string to a primitive type. *>
macro void? @parse_value(String val_str, $Type, #assignee) @local
{
	$switch ($Type):
		$case int::typeid:
		$case short::typeid:
		$case uint::typeid:
		$case ushort::typeid:
			int? parsed = val_str.to_int();
			if (catch err = parsed) return INVALID_VALUE~;
			#assignee = ($Type)parsed;
		$case long::typeid:
		$case ulong::typeid:
			long? parsed = val_str.to_long();
			if (catch err = parsed) return INVALID_VALUE~;
			#assignee = ($Type)parsed;
		$case float::typeid:
			float? parsed = val_str.to_float();
			if (catch err = parsed) return INVALID_VALUE~;
			#assignee = parsed;
		$case double::typeid:
			double? parsed = val_str.to_double();
			if (catch err = parsed) return INVALID_VALUE~;
			#assignee = parsed;
		$case String::typeid:
			#assignee = val_str;
		$case bool::typeid:
			#assignee = (val_str == "true" || val_str == "1");
		$default:
			$error "Type `" +++ $Type::name +++ "` is unsupported by the standard option parser.";
	$endswitch
}

struct OptionMeta @private
{
	bool is_help;
	bool is_inc;
	bool is_opt;
	String short_clean;
	String long_opt_name;
	String env_var_name;
	bool has_env_fallback;
}

<* Extracts and cleans option metadata from raw flags at compile-time. *>
macro OptionMeta @clean_option_meta($short_raw, $long_raw) @private
{
	bool $is_help = ($short_raw.len > 0 &&& $short_raw[^1] == '*') ||| ($long_raw.len > 0 &&& $long_raw[^1] == '*');
	bool $is_inc  = ($short_raw.len > 0 &&& $short_raw[^1] == '+') ||| ($long_raw.len > 0 &&& $long_raw[^1] == '+');
	bool $is_opt  = ($short_raw.len > 0 &&& $short_raw[^1] == '?') ||| ($long_raw.len > 0 &&& $long_raw[^1] == '?');

	String $short_clean = ($is_help ||| $is_inc ||| $is_opt) ??? ($short_raw.len > 0 ??? $short_raw[:$short_raw.len - 1] : "") : $short_raw;
	String $long_clean  = ($is_help ||| $is_inc ||| $is_opt) ??? ($long_raw.len > 0 ??? $long_raw[0 : $long_raw.len - 1] : "") : $long_raw;

	String $long_opt_name = $long_clean;
	String $env_var_name;
	bool $has_env_fallback;

	$if $long_clean.len > 5:
		$for int $e_idx = 0; $e_idx < $long_clean.len - 4; ++$e_idx:
			$if $long_clean[$e_idx : 5] == "=env:":
				$has_env_fallback = true;
				$long_opt_name = $long_clean[0 : $e_idx];
				$env_var_name = $long_clean[$e_idx + 5..];
			$endif
		$endfor
	$endif

	return (OptionMeta) {
		.is_help = $is_help,
		.is_inc = $is_inc,
		.is_opt = $is_opt,
		.short_clean = $short_clean,
		.long_opt_name = $long_opt_name,
		.env_var_name = $env_var_name,
		.has_env_fallback = $has_env_fallback
	};
}

<* Parses and maps a string value to an Enum type safely. *>
macro void? @parse_enum(String val, $TargetType, #assignee) @private
{
	bool enum_matched;
	$for int $e = 0; $e < $TargetType::len; ++$e:
		if (!enum_matched && val.compare_to_ignore_case($TargetType::names[$e]) == 0)
		{
			#assignee = $TargetType::values[$e];
			enum_matched = true;
		}
	$endfor

	if (!enum_matched)
	{
		int? ordinal = val.to_int();
		if (try ord = ordinal)
		{
			if (ord >= 0 && ord < $TargetType::len)
			{
				#assignee = $TargetType::from_ordinal(ord);
				enum_matched = true;
			}
		}
	}

	if (!enum_matched) return INVALID_ENUM_VALUE~;
	return;
}

<*
 @param #args : "The command-line argument slice (typically String[] args from main)."
 @return "On success, the index of the first argument that was not handled (not consumed as a flag or flag value). On failure, returns a parsing fault or HELP_REQUESTED."
 @require $defined(String[] a = #args) : "Expected '#args' to be compatible with String[]"
*>
macro sz? @parse(#args, ...)
{
	bool $strict_mode;
	int $vaarg_start;
	$if @kindof($vaarg[0]) == BOOL:
		$strict_mode = $vaarg[0];
		$vaarg_start = 1;
	$endif

	int $total_options;
	bool $has_positional_target;
	int $positional_idx = -1;
	int $i = $vaarg_start;

	$for ; $i < $vaarg.len; :
		$if $i == $vaarg.len - 1:
			$positional_idx = $i;
			$has_positional_target = true;
			$i++;
		$else
			$if $i + 2 >= $vaarg.len:
				$error "Invalid arguments to `@parse`: missing assignee pointer for option '" +++ $vaarg[$i] +++ "'.";
			$endif
			$total_options++;
			$if @kindof($vaarg[$i+2]) == POINTER ||| @kindof($vaarg[$i+2]) == FUNC:
				$i += 3;
			$else
				$if $i + 3 >= $vaarg.len:
					$error "Invalid arguments to `@parse`: missing assignee pointer for option '" +++ $vaarg[$i] +++ "' with description.";
				$endif
				$if @kindof($vaarg[$i+3]) != POINTER &&& @kindof($vaarg[$i+3]) != FUNC:
					$error "Invalid arguments to `@parse`: expected assignee pointer or callback for option '" +++ $vaarg[$i] +++ "', but got '" +++ $Typeof($vaarg[$i+3])::name +++ "'.";
				$endif
				$i += 4;
			$endif
		$endif
	$endfor

	int $options_end = $has_positional_target ? $vaarg.len - 1 : $vaarg.len;

	ArgParser parser;
	parser.init(#args, $strict_mode);

	sz[$total_options > 0 ??? $total_options : 1] array_indexes = {};
	bool[$total_options > 0 ??? $total_options : 1] option_parsed = {};
	sz positional_index = 0;
	bool print_def = false;

	bool is_opt;
	while (try opt = parser.next(&is_opt))
	{
		if (is_opt)
		{
			bool option_matched = false;

			int $opt_idx;
			int $j = $vaarg_start;
			$for ; $j < $options_end; :
				int $curr_j = $j;
				String $short_raw = $vaarg[$curr_j];
				String $long_raw = $vaarg[$curr_j+1];

				int $assignee_offset = 2;
				String $desc;
				$if @kindof($vaarg[$curr_j+2]) == POINTER ||| @kindof($vaarg[$curr_j+2]) == FUNC:
					$assignee_offset = 2;
					$j += 3;
				$else
					$desc = $vaarg[$curr_j+2];
					$assignee_offset = 3;
					$j += 4;
				$endif

				OptionMeta $meta = @clean_option_meta($short_raw, $long_raw);

				if (($meta.short_clean.len > 0 && opt == $meta.short_clean) || ($meta.long_opt_name.len > 0 && opt == $meta.long_opt_name))
				{
					option_matched = true;
					option_parsed[$opt_idx] = true;

					$switch:
						$case $meta.is_help:
							// Print help menu
							io::printfn("Usage: %s [options]", #args[0]);
							io::printfn("Options:");
							int $h_idx = $vaarg_start;
							$for ; $h_idx < $options_end; :
								int $curr_h = $h_idx;
								String $h_short = $vaarg[$curr_h];
								String $h_long  = $vaarg[$curr_h+1];

								int $h_assignee_offset = 2;
								String $h_desc;
								$if @kindof($vaarg[$curr_h+2]) == POINTER ||| @kindof($vaarg[$curr_h+2]) == FUNC:
									$h_assignee_offset = 2;
									$h_idx += 3;
								$else
									$h_desc = $vaarg[$curr_h+2];
									$h_assignee_offset = 3;
									$h_idx += 4;
								$endif

								OptionMeta $h_meta = @clean_option_meta($h_short, $h_long);

								// Print aligned option strings
								if ($h_meta.short_clean.len > 0 && $h_meta.long_opt_name.len > 0)
								{
									io::printf("  -%s, --%-15s ", $h_meta.short_clean, $h_meta.long_opt_name);
								}
								else if ($h_meta.short_clean.len > 0)
								{
									io::printf("  -%-20s ", $h_meta.short_clean);
								}
								else if ($h_meta.long_opt_name.len > 0)
								{
									io::printf("      --%-15s ", $h_meta.long_opt_name);
								}

								$switch:
									$case @kindof($vaarg[$curr_h + $h_assignee_offset]) == FUNC:
										io::printfn("%s", $h_desc);
									$default:
										var $HType = $Typeof(*$vaarg[$curr_h + $h_assignee_offset]);
										print_def = true;

										$switch ($HType::kind):
											$case BOOL:
												print_def = *$vaarg[$curr_h + $h_assignee_offset];
											$case ARRAY:
												print_def = false;
												foreach (elem : *$vaarg[$curr_h + $h_assignee_offset])
												{
													var $ElemType = $Typeof(elem);
													$if $ElemType == String::typeid:
														if (elem.len > 0) print_def = true;
													$else
														if (elem != 0) print_def = true;
													$endif
												}
											$default:
												$if $HType == String::typeid:
													print_def = (*$vaarg[$curr_h + $h_assignee_offset]).len > 0;
												$endif
										$endswitch

										if (print_def)
										{
											$if $HType == String::typeid:
												io::printfn("%s (default: \"%s\")", $h_desc, *$vaarg[$curr_h + $h_assignee_offset]);
											$else
												io::printfn("%s (default: %s)", $h_desc, *$vaarg[$curr_h + $h_assignee_offset]);
											$endif
										}
										else
										{
											io::printfn("%s", $h_desc);
										}
								$endswitch
							$endfor
							return HELP_REQUESTED~;

						$case $meta.is_inc:
							(*$vaarg[$curr_j + $assignee_offset])++;

						$case @kindof($vaarg[$curr_j + $assignee_offset]) == FUNC:
							($vaarg[$curr_j + $assignee_offset])(parser.consume_value()!)!;

						$default:
							var $TargetType = $Typeof(*$vaarg[$curr_j + $assignee_offset]);

							$switch ($TargetType::kind):
								$case ARRAY:
									sz idx = array_indexes[$opt_idx]++;
									if (idx >= (*$vaarg[$curr_j + $assignee_offset]).len) return OUT_OF_BOUNDS~;
									var $ElementType = $Typeof((*$vaarg[$curr_j + $assignee_offset])[0]);
									@parse_value(parser.consume_value(), $ElementType::typeid, (*$vaarg[$curr_j + $assignee_offset])[idx])!;

								$case ENUM:
									@parse_enum(parser.consume_value(), $TargetType, *$vaarg[$curr_j + $assignee_offset])!;

								$case BOOL:
									$if $meta.is_opt:
										if (parser.has_value())
										{
											String val = parser.consume_value()!;
											*$vaarg[$curr_j + $assignee_offset] = (val == "true" || val == "1");
										}
										else
										{
											*$vaarg[$curr_j + $assignee_offset] = true;
										}
									$else
										*$vaarg[$curr_j + $assignee_offset] = true;
									$endif

								$default:
									$if $meta.is_opt:
										if (parser.has_value()) @parse_value(parser.consume_value(), $TargetType::typeid, *$vaarg[$curr_j + $assignee_offset])!;
									$else
										@parse_value(parser.consume_value(), $TargetType::typeid, *$vaarg[$curr_j + $assignee_offset])!;
									$endif
							$endswitch
					$endswitch
				}

				$opt_idx++;
			$endfor

			if (!option_matched) return ILLEGAL_OPTION~;
		}
		else
		{
			$if $has_positional_target:
				var pos_target = $vaarg[$positional_idx];
				if (positional_index < (*pos_target).len)
				{
					(*pos_target)[positional_index++] = opt;
				}
				else
				{
					return OUT_OF_BOUNDS~;
				}
			$else
				return parser.index - 1;
			$endif
		}
	}

	// Apply environment fallbacks for any unparsed options
	int $fallback_idx;
	int $k = $vaarg_start;
	$for ; $k < $options_end; :
		int $curr_k = $k;
		String $short_raw = $vaarg[$curr_k];
		String $long_raw = $vaarg[$curr_k+1];

		int $assignee_offset = 2;
		$if @kindof($vaarg[$curr_k+2]) == POINTER ||| @kindof($vaarg[$curr_k+2]) == FUNC:
			$assignee_offset = 2;
			$k += 3;
		$else
			$assignee_offset = 3;
			$k += 4;
		$endif

		OptionMeta $k_meta = @clean_option_meta($short_raw, $long_raw);

		$if $k_meta.has_env_fallback:
			if (!option_parsed[$fallback_idx])
			{
				String? env_val = env::tget_var($k_meta.env_var_name);
				if (try val = env_val)
				{
					$switch:
						$case @kindof($vaarg[$curr_k + $assignee_offset]) == FUNC:
							($vaarg[$curr_k + $assignee_offset])(val)!;
						$default:
							var $TargetType = $Typeof(*$vaarg[$curr_k + $assignee_offset]);
							$switch ($TargetType::kind):
								$case BOOL:
									*$vaarg[$curr_k + $assignee_offset] = (val == "true" || val == "1");
								$case ENUM:
									@parse_enum(val, $TargetType, *$vaarg[$curr_k + $assignee_offset])!;
								$default:
									@parse_value(val, $TargetType::typeid, *$vaarg[$curr_k + $assignee_offset])!;
							$endswitch
					$endswitch
				}
			}
		$endif

		$fallback_idx++;
	$endfor

	return parser.index;
}