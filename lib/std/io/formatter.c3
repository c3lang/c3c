module std::io;
import std::collections::map;
import libc;

// It is vitally important that the formatter doesn't do any allocations,
// or it ceases to be useful in constrained environments.

const int PRINTF_NTOA_BUFFER_SIZE = 256;

interface Printable
{
	fn String to_constant_string() @optional;
	fn usz? to_format(Formatter* formatter) @optional;
}

faultdef BUFFER_EXCEEDED, INTERNAL_BUFFER_EXCEEDED, INVALID_FORMAT, NOT_ENOUGH_ARGUMENTS, INVALID_ARGUMENT;

alias OutputFn = fn void?(void* buffer, char c);
alias FloatType = double;


macro bool is_struct_with_default_print($Type)
{
	return ($Type.kindof == STRUCT ||| $Type.kindof == BITSTRUCT)
		&&& !$defined($Type.to_format)
		&&& !$defined($Type.to_constant_string);
}

<*
 Introspect a struct and print it to a formatter

 @require $kindof(value) == STRUCT || $kindof(value) == BITSTRUCT : `This macro is only valid on macros`
*>
macro usz? struct_to_format(value, Formatter* f, bool $force_dump)
{
	var $Type = $typeof(value);
	usz total = f.print("{ ")!;
	$foreach $i, $member : $Type.membersof:
		$if $i > 0:
			total += f.print(", ")!;
		$endif
		$if $member.nameof != "":
			total += f.printf("%s: ", $member.nameof)!;
		$endif
		$if ($force_dump &&& ($member.typeid.kindof == STRUCT || $member.typeid.kindof == BITSTRUCT)) |||
			is_struct_with_default_print($member.typeid):
			total += struct_to_format($member.get(value), f, $force_dump)!;
		$else
			total += f.printf("%s", $member.get(value))!;
		$endif
	$endforeach
	return total + f.print(" }");
}

fn usz? ReflectedParam.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("[Parameter '%s']", self.name);
}

fn usz? Formatter.printf(&self, String format, args...)
{
	return self.vprintf(format, args) @inline;
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
		fault first_fault;
	}
}

bitstruct PrintFlags : uint
{
	bool zeropad;
	bool left;
	bool plus;
	bool space;
	bool hash;
	bool uppercase;
	bool precision;
}

fn void Formatter.init(&self, OutputFn out_fn, void* data = null)
{
	*self = { .data = data, .out_fn = out_fn};
}



fn usz? Formatter.print_with_function(&self, Printable arg)
{
	if (&arg.to_format)
	{
		PrintFlags old = self.flags;
		uint old_width = self.width;
		uint old_prec = self.prec;
		defer
		{
			self.flags = old;
			self.width = old_width;
			self.prec = old_prec;
		}
		if (!arg) return formatter_out_substr(self, "(null)");
		return arg.to_format(self);
	}
	if (&arg.to_constant_string)
	{
		PrintFlags old = self.flags;
		uint old_width = self.width;
		uint old_prec = self.prec;
		defer
		{
			self.flags = old;
			self.width = old_width;
			self.prec = old_prec;
		}
		if (!arg) return formatter_out_substr(self, "(null)");
		return formatter_out_substr(self, arg.to_constant_string());
	}
	return NOT_FOUND~;
}


fn void? out_null_fn(void* data @unused, char c @unused) @private
{
}

macro usz? @report_fault(Formatter* f, $fault)
{
	(void)formatter_out_substr(f, $fault);
	return INVALID_FORMAT~;
}

macro usz? @wrap_bad(Formatter* f, #action)
{
	usz? len = #action;
	if (catch err = len)
	{
		switch (err)
		{
			case BUFFER_EXCEEDED:
			case INTERNAL_BUFFER_EXCEEDED:
				return f.first_err(err)~;
			default:
				err = f.first_err(INVALID_ARGUMENT);
				formatter_out_substr(f, "<INVALID>")!;
				return err~;
		}
	}
	return len;
}



fn usz? Formatter.vprintf(&self, String format, any[] anys)
{
	self.first_fault = {};
	if (!self.out_fn)
	{
		// use null output function
		self.out_fn = &out_null_fn;
	}
	usz total_len;
	usz format_len = format.len;
	usz variant_index = 0;
	for (usz i = 0; i < format_len; i++)
	{
		// format specifier?  %[flags][width][.precision][length]
		char c = format[i];
		if (c != '%')
		{
			// no
			total_len += self.print_char(c)!;
			continue;
		}
		i++;
		if (i >= format_len) return @report_fault(self, "%ERR");
		c = format[i];
		if (c == '%')
		{
			total_len += self.print_char(c)!;
			continue;
		}
		// evaluate flags
		self.flags = {};
		while FLAG_EVAL: (true)
		{
			switch (c)
			{
				case '0': self.flags.zeropad = true;
				case '-': self.flags.left = true;
				case '+': self.flags.plus = true;
				case ' ': self.flags.space = true;
				case '#': self.flags.hash = true;
				default: break FLAG_EVAL;
			}
			if (++i >= format_len) return @report_fault(self, "%ERR");
			c = format[i];
		}
		// evaluate width field
		int? w = printf_parse_format_field(anys.ptr, anys.len, &variant_index, format.ptr, format.len, &i);
		if (catch w) return @report_fault(self, "%ERR");
		c = format[i];
		if (w < 0)
		{
			self.flags.left = true;
			w = -w;
		}
		self.width = (uint)w;
		// evaluate precision field
		self.prec = 0;
		if (c == '.')
		{
			self.flags.precision = true;
			if (++i >= format_len) return @report_fault(self, "<BAD FORMAT>");
			int? prec = printf_parse_format_field(anys.ptr, anys.len, &variant_index, format.ptr, format.len, &i);
			if (catch prec) return @report_fault(self, "<BAD FORMAT>");
			self.prec = (uint)(prec < 0 ? 0 : prec);
			c = format[i];
		}

		// evaluate specifier
		uint base = 0;
		if (variant_index >= anys.len)
		{
			self.first_err(NOT_ENOUGH_ARGUMENTS);
			total_len += formatter_out_substr(self, "<MISSING>")!;
			continue;
		}
		any current = anys[variant_index++];
		switch (c)
		{
			case 'd':
				base = 10;
				self.flags.hash = false;
			case 'X' :
				self.flags.uppercase = true;
				nextcase;
			case 'x' :
				base = 16;
			case 'O':
				self.flags.uppercase = true;
				nextcase;
			case 'o' :
				base = 8;
			case 'B':
				self.flags.uppercase = true;
				nextcase;
			case 'b' :
				base = 2;
			case 'A':
				self.flags.uppercase = true;
				nextcase;
			case 'a':
				total_len += @wrap_bad(self, formatter_atoa(self, float_from_any(current)))!;
				continue;
			case 'F' :
				self.flags.uppercase = true;
				nextcase;
			case 'f':
				total_len += @wrap_bad(self, formatter_ftoa(self, float_from_any(current)))!;
				continue;
			case 'E':
				self.flags.uppercase = true;
				nextcase;
			case 'e':
				total_len += @wrap_bad(self, formatter_etoa(self, float_from_any(current)))!;
				continue;
			case 'G':
				self.flags.uppercase = true;
				nextcase;
			case 'g':
				total_len += @wrap_bad(self, formatter_gtoa(self, float_from_any(current)))!;
				continue;
			case 'c':
				total_len += formatter_out_char(self, current)!;
				continue;
			case 'H':
				self.flags.uppercase = true;
				nextcase;
			case 'h':
				total_len += formatter_out_hex_buffer(self, current)!;
				continue;
			case 's':
				total_len += formatter_out_str_pad(self, current)!;
				continue;
			case 'p':
				self.flags.zeropad = true;
				self.flags.hash = true;
				base = 16;
			default:
				self.first_err(INVALID_FORMAT);
				total_len += formatter_out_substr(self, "<BAD FORMAT>")!;
				continue;
		}
		if (base != 10)
		{
			self.flags.plus = false;
			self.flags.space = false;
		}
		// ignore '0' flag when precision is given
		if (self.flags.precision) self.flags.zeropad = false;

		bool is_neg;
		total_len += @wrap_bad(self, formatter_ntoa(self, int_from_any(current, &is_neg), is_neg, base))!;
	}

	if (self.first_fault) return self.first_fault~;
	return total_len;
}

fn usz? Formatter.out(&self, char c) @deprecated("Use print_char") => self.print_char(c);

fn usz? Formatter.print_char(&self, char c)
{
	if (catch err = self.out_fn(self.data, c))
	{
		if (self.first_fault) return self.first_fault~;
		self.first_fault = err;
		return err~;
	}
	return 1;
}

fn usz? Formatter.print(&self, String str)
{
	if (!self.out_fn)
	{
		// use null output function
		self.out_fn = &out_null_fn;
	}
	foreach (c : str) self.print_char(c)!;
	return str.len;
}
