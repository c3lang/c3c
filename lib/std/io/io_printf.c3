module std::io;
import std::collections::map;
import libc;

const int PRINTF_NTOA_BUFFER_SIZE = 256;
const int PRINTF_FTOA_BUFFER_SIZE = 256;
const float PRINTF_MAX_FLOAT = 1e9;
const uint PRINTF_DEFAULT_FLOAT_PRECISION = 6;

fault PrintFault
{
	BUFFER_EXCEEDED,
	INTERNAL_BUFFER_EXCEEDED,
	INVALID_FORMAT_STRING,
	MISSING_ARG,
	INVALID_ARGUMENT_TYPE,
}

fault FormattingFault
{
	UNTERMINATED_FORMAT,
	MISSING_ARG,
	INVALID_WIDTH_ARG,
	INVALID_FORMAT_TYPE,
}

typedef OutputFn = fn void!(char c, void* buffer);
typedef ToStringFunction = fn String(void* value, Allocator *using);
typedef ToFormatFunction = fn void!(void* value, Formatter* formatter);
typedef FloatType = double;

fn usz! printf(String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putchar_fn);
    return formatter.vprintf(format, args);
}

define printfln = printfn;

fn usz! printfn(String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putchar_fn);
    usz len = formatter.vprintf(format, args)?;
	putchar('\n');
	return len + 1;
}

fn usz! VarString.printf(VarString* str, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn, str);
	return formatter.vprintf(format, args);
}

fn usz! VarString.printfn(VarString* str, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn, str);
	usz len = formatter.vprintf(format, args)?;
	str.append('\n');
	return len + 1;
}

fn char[]! bprintf(char[] buffer, String format, args...) @maydiscard
{
	Formatter formatter;
	BufferData data = { .buffer = buffer };
    formatter.init(&out_buffer_fn, &data);
    usz size = formatter.vprintf(format, args)?;
    return buffer[:data.written];
}

fn usz! File.printf(File file, String format, args...) @maydiscard
{
	Formatter formatter;
    formatter.init(&out_fputchar_fn, &file);
    return formatter.vprintf(format, args)?;
}

fn usz! File.printfn(File file, String format, args...) @maydiscard
{
	Formatter formatter;
    formatter.init(&out_fputchar_fn, &file);
    usz len = formatter.vprintf(format, args)?;
	file.putc('\n')?;
	file.flush();
	return len + 1;
}

fn usz! Formatter.printf(Formatter* this, String format, args...)
{
	return this.vprintf(format, args) @inline;
}

struct Formatter
{
	void *data;
	OutputFn out_fn;
	struct
	{
		PrintFlags flags;
    	uint width;
    	uint prec;
    	usz idx;
	}
}

bitstruct PrintFlags : uint
{
	bool zeropad : 0;
	bool left : 1;
	bool plus : 2;
	bool space : 3;
	bool hash : 4;
	bool uppercase : 5;
	bool precision : 6;
	bool adapt_exp : 7;
}

fn void Formatter.init(Formatter* this, OutputFn out_fn, void* data = null)
{
	*this = { .data = data, .out_fn = out_fn};
}

/**
 * @require $checks($Type a, a.to_string()) || $checks($Type a, a.to_format(&&Formatter{}))  "Expected a type with 'to_string' or 'to_format' defined"
 * @require !$checks($Type a, a.to_string()) || $checks($Type a, a.to_string(&&Allocator{})) "Expected 'to_string' to take an allocator as argument."
 * @require !$checks($Type a, a.to_format(&&Formatter{})) || $checks($Type a, Formatter b, a.to_format(&b)) "Expected 'to_format' to take a Formatter as argument."
 */
macro void formatter_register_type($Type)
{
	$if ($checks($Type a, a.to_format(&&Formatter {}))):
		if (!toformat_functions.table.len)
		{
			toformat_functions.init(64);
		}
		toformat_functions.set($Type.typeid, (ToFormatFunction)&$Type.to_format);
	$else:
		if (!tostring_functions.table.len)
		{
			tostring_functions.init(64);
		}
		tostring_functions.set($Type.typeid, (ToStringFunction)&$Type.to_string);
	$endif;
}


static initialize @priority(101)
{
	if (!toformat_functions.table.len)
	{
		toformat_functions.init(64);
	}
	if (!tostring_functions.table.len)
	{
		tostring_functions.init(64);
	}
}

fn void! Formatter.out(Formatter* this, char c) @private
{
	this.out_fn(c, this.data)?;
}

macro bool! Formatter.print_with_function(Formatter* this, variant arg)
{
	if (try to_format = toformat_functions.get(arg.type))
	{
		PrintFlags old = this.flags;
		uint old_width = this.width;
		uint old_prec = this.prec;
		defer
		{
			this.flags = old;
			this.width = old_width;
			this.prec = old_prec;
		}
		to_format(arg.ptr, this)?;
		return true;
    }
    if (try to_string = tostring_functions.get(arg.type))
    {
		PrintFlags old = this.flags;
		uint old_width = this.width;
		uint old_prec = this.prec;
		defer
		{
			this.flags = old;
			this.width = old_width;
			this.prec = old_prec;
		}
		@pool()
		{
			this.out_substr(to_string(arg.ptr, mem::temp()))?;
			return true;
		};
    }
    return false;
}

fn void! Formatter.out_str(Formatter* this, variant arg) @private
{
	switch (arg.type.kindof)
	{
		case TYPEID:
			return this.out_substr("<typeid>");
		case VOID:
        	return this.out_substr("void");
		case ANYERR:
		case FAULT:
        	return this.out_substr((*(anyerr*)arg.ptr).nameof);
		case VARIANT:
			return this.out_str(*(variant*)arg);
		case ENUM:
			if (this.print_with_function(arg)?) return;
        	return this.out_substr(arg.type.names[types::variant_to_int(arg, usz)!!]);
		case STRUCT:
			if (this.print_with_function(arg)?) return;
			return this.out_substr("<struct>");
		case UNION:
			if (this.print_with_function(arg)?) return;
			return this.out_substr("<union>");
		case BITSTRUCT:
			if (this.print_with_function(arg)?) return;
			return this.out_substr("<bitstruct>");
		case FUNC:
			if (this.print_with_function(arg)?) return;
			return this.out_substr("<function>");
		case OPTIONAL:
			unreachable();
		case DISTINCT:
			if (this.print_with_function(arg)?) return;
			if (arg.type == VarString.typeid)
			{
				return this.out_substr(((VarString*)arg).str());
			}
			if (arg.type == DString.typeid)
			{
				return this.out_substr(((DString*)arg).str());
			}
        	return this.out_str(variant { arg.ptr, arg.type.inner });
		case POINTER:
			if (this.print_with_function(arg)?) return;
			return this.ntoa_variant(arg, 16);
		case SIGNED_INT:
		case UNSIGNED_INT:
			return this.ntoa_variant(arg, 10);
		case FLOAT:
			return this.ftoa(float_from_variant(arg)!!);
		case ARRAY:
			if (this.print_with_function(arg)?) return;
			// this is SomeType[*] so grab the "SomeType"
			typeid inner = arg.type.inner;
			usz size = inner.sizeof;
			usz len = arg.type.len;
			// Pretend this is a String
			void* ptr = (void*)arg.ptr;
			this.out('[')?;
			for (usz i = 0; i < len; i++)
			{
				if (i != 0) this.out_substr(", ")?;
				this.out_str(variant { ptr, inner })?;
				ptr += size;
			}
			return this.out(']');
		case VECTOR:
			if (this.print_with_function(arg)?) return;
			// this is SomeType[*] so grab the "SomeType"
			typeid inner = arg.type.inner;
			usz size = inner.sizeof;
			usz len = arg.type.len;
			// Pretend this is a String
			void* ptr = (void*)arg.ptr;
			this.out_substr("[<")?;
			for (usz i = 0; i < len; i++)
			{
				if (i != 0) this.out_substr(", ")?;
				this.out_str(variant { ptr, inner })?;
				ptr += size;
			}
			return this.out_substr(">]");
		case SUBARRAY:
			if (this.print_with_function(arg)?) return;
			// this is SomeType[] so grab the "SomeType"
			typeid inner = arg.type.inner;
			if (inner == char.typeid)
			{
				return this.out_substr(*(String*)arg);
			}
			usz size = inner.sizeof;
            // Pretend this is a String
			String* temp = (void*)arg.ptr;
			void* ptr = (void*)temp.ptr;
			usz len = temp.len;
			this.out('[')?;
			for (usz i = 0; i < len; i++)
			{
				if (i != 0) this.out_substr(", ")?;
				this.out_str(variant { ptr, inner })?;
				ptr += size;
			}
			this.out(']')?;
		case BOOL:
			if (*(bool*)arg.ptr)
			{
				return this.out_substr("true");
			}
			else
			{
				return this.out_substr("false");
			}
		default:
			if (this.print_with_function(arg)?) return;
			return this.out_substr("Invalid type");
	}
}



fn void! out_buffer_fn(char c, void *data) @private
{
	BufferData *buffer_data = data;
	if (buffer_data.written >= buffer_data.buffer.len) return PrintFault.BUFFER_EXCEEDED!;
	buffer_data.buffer[buffer_data.written++] = c;
}

fn void! out_null_fn(char c @unused, void* data @unused) @private
{
}

fn void! out_putchar_fn(char c, void* data @unused) @private
{
	libc::putchar(c);
}

fn void! out_fputchar_fn(char c, void* data) @private
{
	File* f = data;
	f.putc(c)?;
}

fn void! out_string_append_fn(char c, void* data) @private
{
	VarString* s = data;
	s.append_char(c);
}


struct BufferData @private
{
	char[] buffer;
	usz written;
}


fn usz! Formatter.vprintf(Formatter* this, String format, variant[] variants)
{
	if (!this.out_fn)
	{
		// use null output function
		this.out_fn = &out_null_fn;
	}
	usz format_len = format.len;
	usz variant_index = 0;
	for (usz i = 0; i < format_len; i++)
	{
		// format specifier?  %[flags][width][.precision][length]
		char c = format[i];
		if (c != '%')
		{
			// no
			this.out(c)?;
			continue;
		}
		i++;
		if (i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
		c = format[i];
		if (c == '%')
		{
			this.out(c)?;
			continue;
		}
		// evaluate flags
		this.flags = {};
		while FLAG_EVAL: (true)
		{
			switch (c)
			{
				case '0': this.flags.zeropad = true;
				case '-': this.flags.left = true;
				case '+': this.flags.plus = true;
				case ' ': this.flags.space = true;
				case '#': this.flags.hash = true;
				default: break FLAG_EVAL;
			}
			if (++i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
			c = format[i];
		}
		// evaluate width field
		int w = printf_parse_format_field(variants.ptr, variants.len, &variant_index, format.ptr, format.len, &i)?;
		c = format[i];
		if (w < 0)
		{
			this.flags.left = true;
			w = -w;
		}
		this.width = w;
		// evaluate precision field
		this.prec = 0;
		if (c == '.')
		{
			this.flags.precision = true;
			if (++i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
			int prec = printf_parse_format_field(variants.ptr, variants.len, &variant_index, format.ptr, format.len, &i)?;
			this.prec = prec < 0 ? 0 : prec;
			c = format[i];
		}

		// evaluate specifier
		uint base = 0;
		if (variant_index >= variants.len) return PrintFault.MISSING_ARG!;
		variant current = variants[variant_index++];
		switch (c)
		{
			case 'd':
				base = 10;
				this.flags.hash = false;
			case 'X' :
				this.flags.uppercase = true;
				nextcase;
			case 'x' :
				base = 16;
			case 'O':
				this.flags.uppercase = true;
				nextcase;
			case 'o' :
				base = 8;
			case 'B':
				this.flags.uppercase = true;
				nextcase;
			case 'b' :
				base = 2;
		    case 'F' :
		        this.flags.uppercase = true;
		        nextcase;
		    case 'f':
		        this.ftoa(float_from_variant(current)!!)?;
				continue;
			case 'E':
		        this.flags.uppercase = true;
		        nextcase;
            case 'e':
				this.etoa(float_from_variant(current)!!)?;
				continue;
			case 'G':
				this.flags.uppercase = true;
				nextcase;
			case 'g':
				this.flags.adapt_exp = true;
				this.etoa(float_from_variant(current)!!)?;
				continue;
			case 'c':
				this.out_char(current)?;
				continue;
			case 's':
				this.out_str(current)?;
				continue;
			case 'p':
				this.flags.zeropad = true;
				this.flags.hash = true;
				base = 16;
			default:
				return PrintFault.INVALID_FORMAT_STRING!;
		}
		if (base != 10)
		{
			this.flags.plus = false;
			this.flags.space = false;
		}
		// ignore '0' flag when precision is given
		if (this.flags.precision) this.flags.zeropad = false;

		bool is_neg;
		uint128 v = int_from_variant(current, &is_neg)!!;

		this.ntoa(v, is_neg, base)?;
	}
	// termination
	//	out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);

	// return written chars without terminating \0
	return this.idx;
}

typedef StringFunctionMap @private = HashMap<typeid, ToStringFunction>;
typedef FormatterFunctionMap @private = HashMap<typeid, ToFormatFunction>;

FormatterFunctionMap toformat_functions @private;
StringFunctionMap tostring_functions @private;
