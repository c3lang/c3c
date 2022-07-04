module std::io;
import libc;

const PRINTF_NTOA_BUFFER_SIZE = 123;

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

/*
private macro usize out_rev(#output, usize idx, char* buf, uint len, uint width, PrintFlags flags)
{
	usize start_idx = idx;
	// pad spaces up to given width
    if (!flags.left && !flags.zeropad)
    {
        for (usize i = len; i < width; i++)
        {
            output(' ');
            idx++;
        }
	}

	while (len)
	{
		output(buf[--len]);
		idx++;
	}

	// append pad spaces up to given width
	if (flags & FLAGS_LEFT)
	{
		while (idx - start_idx < width)
		{
			output(' ');
			idx++;
		}
	}
	return idx;
}

private macro usize ntoa_format(#output, uint idx, uint len, char* buf, uint prec, bool is_negative, PrintFlags flags)
{
	if (!flags.left)
	{
		if (width && flags.zeropad && (is_negative || flags.plus || flags.space))
		{
			width--;
		}
		while (len < prec && len < PRINTF_NTOA_BUFFER_SIZE) buf[len++] = '0';
		if (flags.zeropad)
		{
			while (len < width && len < PRINTF_NTOA_BUFFER_SIZE) buf[len++] = '0';
		}
	}
	if (flags.hash)
	{
		if (!flags.precision && len && (len == prec || len == width))
		{
			len--;
			if (len && base == 16)
			{
				len--;
			}
		}
		if (len < PRINTF_NTOA_BUFFER_SIZE)
		{
			switch (base)
			{
				case 16:
					buf[len++] = flags.uppercase ? 'X' : 'x';
				case 8:
					buf[len++] = flags.uppercase ? 'O' : 'o';
				case 2:
					buf[len++] = flags.uppercase ? 'B' : 'b';
				default:
					buf[len++] = '0';
			}
		}
	}
	if (len < PRINTF_NTOA_BUFFER_SIZE)
	{
		switch (true)
		{
			case is_negative:
				buf[len++] = '-';
			case flags.plus:
				buf[len++] = '+';
			case flags.space:
				buf[len++] = ' ';
		}
	}
	return out_rev(#output, idx, maxlen, buf, len, width, flags);
}
*/

private macro void @format_print_str_variant(#output, variant arg)
{
	if (arg.type.kind == TypeKind.POINTER)
	{
		libc::printf("%p", *((void**)arg));
		return;
	}
	switch (arg)
	{
		case char[]:
			@format_print(#output, *arg);
		case bool:
			@format_print(#output, *arg ? (char[])"true" : (char[])"false");
		case long:
			libc::printf("%lld", (CLongLong)*arg);
		case int:
			libc::printf("%d", *arg);
		case short:
			libc::printf("%d", *arg);
		case ichar:
			libc::printf("%d", *arg);
		case char:
			libc::printf("%u", *arg);
		default:
			print("Invalid type");
	}
}

private macro void @format_print(#output, char[] str)
{
	foreach (char c : str) #output(c);
}

private macro uint @simple_atoi(char* buf, usize &len, usize maxlen)
{
	uint i = 0;
	while (len < maxlen)
    {
        char c = buf[len];
        if (c < '0' || c > '9') return i;
        i = i * 10 + c - '0';
        len++;
    }
    return i;
}

private macro void @printf_generic(#output, char[] format, variant[] args)
{
	usize idx = 0;
	char* format_ptr = format.ptr;
	usize format_len = format.len;
	usize arg_index = 0;
	for (usize i = 0; i < format_len; i++)
	{
		char c = format_ptr[i];
		if (c != '%')
		{
			#output(c);
			idx++;
			continue;
		}
		i++;
	    // format specifier?  %[flags][width][.precision][length]
	    PrintFlags flags;
	    while FLAG_EVAL: (1)
	    {
	        if (i == format_len)
	        {
	            @format_print("<unterminated format>");
	            break;
	        }
	        c = format_ptr[i];
	        switch (c)
	        {
	            case '0':
	                flags.zeropad = true;
	            case '-':
	                flags.left = true;
	            case '+':
	                flags.plus = true;
	            case ' ':
	                flags.space = true;
	            case '#':
	                flags.hash = true;
	            default:
	                break FLAG_EVAL;
	        }
	        i++;
	    }
	    // evaluate width field
	    uint width = 0;
		if (c >= '0' && c <= '9')
		{
			width = @simple_atoi(format_ptr, i, format_len);
		}
		else if (c == '*')
		{
			i++;
	        if (i == format_len)
	        {
	            @format_print("<unterminated format>");
	            break;
	        }
			c = format_ptr[i];
			width = {|
				if (arg_index >= args.len)
                {
                    @format_print("<missing arg>");
                    return 0;
                }
				variant val = args[arg_index++];
        		if (!types::kind_is_int(val.type_id.kind))
        		{
        		    @format_print("<invalid width arg>");
        		    return 0;
        		}
				uint! w = types::variant_to_int(val, uint);
				if (catch w)
				{
					@format_print("<invalid width arg size>");
					return 0;
				}
				return w;
			|};
		}
	}

/*
    precision = 0U;
    if (*format == '.') {
      flags |= FLAGS_PRECISION;
      format++;
      if (_is_digit(*format)) {
        precision = _atoi(&format);
      }
      else if (*format == '*') {
        const int prec = (int)va_arg(va, int);
        precision = prec > 0 ? (unsigned int)prec : 0U;
        format++;
      }
    }

    // evaluate length field
    switch (*format) {
      case 'l' :
        flags |= FLAGS_LONG;
        format++;
        if (*format == 'l') {
          flags |= FLAGS_LONG_LONG;
          format++;
        }
        break;
      case 'h' :
        flags |= FLAGS_SHORT;
        format++;
        if (*format == 'h') {
          flags |= FLAGS_CHAR;
          format++;
        }
        break;
#if defined(PRINTF_SUPPORT_PTRDIFF_T)
      case 't' :
        flags |= (sizeof(ptrdiff_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
        format++;
        break;
#endif
      case 'j' :
        flags |= (sizeof(intmax_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
        format++;
        break;
      case 'z' :
        flags |= (sizeof(size_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
        format++;
        break;
      default :
        break;
    }

    // evaluate specifier
    switch (*format) {
      case 'd' :
      case 'i' :
      case 'u' :
      case 'x' :
      case 'X' :
      case 'o' :
      case 'b' : {
        // set the base
        unsigned int base;
        if (*format == 'x' || *format == 'X') {
          base = 16U;
        }
        else if (*format == 'o') {
          base =  8U;
        }
        else if (*format == 'b') {
          base =  2U;
        }
        else {
          base = 10U;
          flags &= ~FLAGS_HASH;   // no hash for dec format
        }
        // uppercase
        if (*format == 'X') {
          flags |= FLAGS_UPPERCASE;
        }

        // no plus or space flag for u, x, X, o, b
        if ((*format != 'i') && (*format != 'd')) {
          flags &= ~(FLAGS_PLUS | FLAGS_SPACE);
        }

        // ignore '0' flag when precision is given
        if (flags & FLAGS_PRECISION) {
          flags &= ~FLAGS_ZEROPAD;
        }

        // convert the integer
        if ((*format == 'i') || (*format == 'd')) {
          // signed
          if (flags & FLAGS_LONG_LONG) {
#if defined(PRINTF_SUPPORT_LONG_LONG)
            const long long value = va_arg(va, long long);
            idx = _ntoa_long_long(out, buffer, idx, maxlen, (unsigned long long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
#endif
          }
          else if (flags & FLAGS_LONG) {
            const long value = va_arg(va, long);
            idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
          }
          else {
            const int value = (flags & FLAGS_CHAR) ? (char)va_arg(va, int) : (flags & FLAGS_SHORT) ? (short int)va_arg(va, int) : va_arg(va, int);
            idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned int)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
          }
        }
        else {
          // unsigned
          if (flags & FLAGS_LONG_LONG) {
#if defined(PRINTF_SUPPORT_LONG_LONG)
            idx = _ntoa_long_long(out, buffer, idx, maxlen, va_arg(va, unsigned long long), false, base, precision, width, flags);
#endif
          }
          else if (flags & FLAGS_LONG) {
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
          }
          else {
            const unsigned int value = (flags & FLAGS_CHAR) ? (unsigned char)va_arg(va, unsigned int) : (flags & FLAGS_SHORT) ? (unsigned short int)va_arg(va, unsigned int) : va_arg(va, unsigned int);
            idx = _ntoa_long(out, buffer, idx, maxlen, value, false, base, precision, width, flags);
          }
        }
        format++;
        break;
      }
#if defined(PRINTF_SUPPORT_FLOAT)
      case 'f' :
      case 'F' :
        if (*format == 'F') flags |= FLAGS_UPPERCASE;
        idx = _ftoa(out, buffer, idx, maxlen, va_arg(va, double), precision, width, flags);
        format++;
        break;
#if defined(PRINTF_SUPPORT_EXPONENTIAL)
      case 'e':
      case 'E':
      case 'g':
      case 'G':
        if ((*format == 'g')||(*format == 'G')) flags |= FLAGS_ADAPT_EXP;
        if ((*format == 'E')||(*format == 'G')) flags |= FLAGS_UPPERCASE;
        idx = _etoa(out, buffer, idx, maxlen, va_arg(va, double), precision, width, flags);
        format++;
        break;
#endif  // PRINTF_SUPPORT_EXPONENTIAL
#endif  // PRINTF_SUPPORT_FLOAT
      case 'c' : {
        unsigned int l = 1U;
        // pre padding
        if (!(flags & FLAGS_LEFT)) {
          while (l++ < width) {
            out(' ', buffer, idx++, maxlen);
          }
        }
        // char output
        out((char)va_arg(va, int), buffer, idx++, maxlen);
        // post padding
        if (flags & FLAGS_LEFT) {
          while (l++ < width) {
            out(' ', buffer, idx++, maxlen);
          }
        }
        format++;
        break;
      }

      case 's' : {
        const char* p = va_arg(va, char*);
        unsigned int l = _strnlen_s(p, precision ? precision : (size_t)-1);
        // pre padding
        if (flags & FLAGS_PRECISION) {
          l = (l < precision ? l : precision);
        }
        if (!(flags & FLAGS_LEFT)) {
          while (l++ < width) {
            out(' ', buffer, idx++, maxlen);
          }
        }
        // string output
        while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
          out(*(p++), buffer, idx++, maxlen);
        }
        // post padding
        if (flags & FLAGS_LEFT) {
          while (l++ < width) {
            out(' ', buffer, idx++, maxlen);
          }
        }
        format++;
        break;
      }

      case 'p' : {
        width = sizeof(void*) * 2U;
        flags |= FLAGS_ZEROPAD | FLAGS_UPPERCASE;
#if defined(PRINTF_SUPPORT_LONG_LONG)
        const bool is_ll = sizeof(uintptr_t) == sizeof(long long);
        if (is_ll) {
          idx = _ntoa_long_long(out, buffer, idx, maxlen, (uintptr_t)va_arg(va, void*), false, 16U, precision, width, flags);
        }
        else {
#endif
          idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned long)((uintptr_t)va_arg(va, void*)), false, 16U, precision, width, flags);
#if defined(PRINTF_SUPPORT_LONG_LONG)
        }
#endif
        format++;
        break;
      }

      case '%' :
        out('%', buffer, idx++, maxlen);
        format++;
        break;

      default :
        out(*format, buffer, idx++, maxlen);
        format++;
        break;
    }
  }

  // termination
  out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);

  // return written chars without terminating \0
  return (int)idx;*/
}

private macro void @formatting(#output, char[] format, variant[] args)
{
	usize len = format.len;
	usize arg_len = args.len;
	usize current_arg = 0;
	for (usize i = 0; i < len; i++)
	{
		char c = format[i];
		if (c != '%' || i == len - 1)
		{
			#output(c);
			continue;
		}
		c = format[++i];
		switch (c)
		{
			case 's':
				if (current_arg >= arg_len)
				{
					@format_print(#output, "<missing arg>");
					continue;
				}
				@format_print_str_variant(#output, args[current_arg++]);
			default:
				@format_print(#output, "Unknown specifier");
		}
	}
}

fn void printf(char[] format, args... )
{
	@formatting(putchar, format, args);
}