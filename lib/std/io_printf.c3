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

private macro usize! @printf_integer(#output, variant, usize idx, uint precision, uint width, PrintFlags flags)
{
	return 0;
}

private macro usize! @printf_efloat(#output, variant, usize idx, uint precision, uint width, PrintFlags flags)
{
	return 0;
}

private macro usize! @printf_char(#output, variant, usize idx, uint precision, uint width, PrintFlags flags)
{
	return 0;
}

private macro usize! @printf_ptr(#output, variant, usize idx, uint precision, uint width, PrintFlags flags)
{
	return 0;
}

private macro usize! @printf_str(#output, variant, usize idx, uint precision, uint width, PrintFlags flags)
{
	return 0;
}

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

private fn uint simple_atoi(char* buf, usize maxlen, usize* len_ptr) @inline
{
	uint i = 0;
	usize len = *len_ptr;
	while (len < maxlen)
    {
        char c = buf[len];
        if (c < '0' || c > '9') break;
        i = i * 10 + c - '0';
        len++;
    }
    *len_ptr = len;
    return i;
}

fault FormattingFault
{
	UNTERMINATED_FORMAT,
	MISSING_ARG,
	INVALID_WIDTH_ARG,
	INVALID_FORMAT_TYPE,
}

private fn void! printf_advance_format(usize format_len, usize *index_ptr) @inline
{
	usize val = ++(*index_ptr);
	if (val <= format_len) return FormattingFault.UNTERMINATED_FORMAT!;
}

private fn variant! next_variant(variant* args_ptr, usize args_len, usize* arg_index_ptr) @inline
{
	if (*arg_index_ptr >= args_len) return FormattingFault.MISSING_ARG!;
	return args_ptr[(*arg_index_ptr)++];
}

private fn uint! printf_parse_format_field(variant* args_ptr, usize args_len, usize* args_index_ptr, char* format_ptr, usize format_len, usize* index_ptr) @inline
{
	char c = format_ptr[*index_ptr];
	if (c >= '0' && c <= '9') return simple_atoi(format_ptr, format_len, index_ptr);
	if (c != '*') return 0;
	printf_advance_format(format_len, index_ptr)?;
	variant val = next_variant(args_ptr, args_len, args_index_ptr)?;
    if (!types::kind_is_int(val.type.kind)) return FormattingFault.INVALID_WIDTH_ARG!;
    uint! intval = types::variant_to_int(val, uint);
	if (catch intval) return FormattingFault.INVALID_WIDTH_ARG!;
	return intval;
}

private macro usize! @printf_generic(#output, char[] format, variant[] args)
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
	    printf_advance_format(format_len, &i)?;
	    c = format_ptr[i];
	    if (c == '%')
	    {
	        #output(c);
	        idx++;
	        continue;
	    }
	    // format specifier?  %[flags][width][.precision][length]
	    PrintFlags flags;
	    while FLAG_EVAL: (1)
	    {
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
	    	printf_advance_format(format_len, &i)?;
	        c = format_ptr[i];
	    }
	    // evaluate width field
	    uint width = printf_parse_format_field(args.ptr, args.len, &arg_index, format_ptr, format_len, &i)?;
	    c = format_ptr[i];
		uint precision = 0;
		if (c == '.')
		{
			flags.precision = true;
			printf_advance_format(format_len, &i)?;
			precision = printf_parse_format_field(args.ptr, args.len, &arg_index, format_ptr, format_len, &i)?;
			c = format_ptr[i];
        }

        uint base = void;
        switch (c)
        {
            case 'd':
                flags.hash = false;
                base = 10;
            case 'X':
                flags.uppercase = true;
                nextcase;
            case 'x':
                base = 16;
            case 'o':
                base = 8;
            case 'b':
                base = 2;
            case 'F':
                flags.uppercase = true;
                nextcase;
            case 'f':
				idx = @printf_efloat(#output, next_variant(args.ptr, args.len, &arg_index), idx, precision, width, flags)?;
				continue;
			case 'E':
				flags.uppercase = true;
				nextcase;
			case 'e':
				idx = @printf_efloat(#output, next_variant(args.ptr, args.len, &arg_index), idx, precision, width, flags)?;
				continue;
			case 'G':
				flags.uppercase = true;
				nextcase;
			case 'g':
				flags.adapt_exp = true;
				nextcase 'e';
			case 'c':
				idx = @printf_char(#output, next_variant(args.ptr, args.len, &arg_index), idx, precision, width, flags)?;
				continue;
			case 'p':
				idx = @printf_ptr(#output, next_variant(args.ptr, args.len, &arg_index), idx, precision, width, flags)?;
				continue;
			case 's':
				idx = @printf_str(#output, next_variant(args.ptr, args.len, &arg_index), idx, precision, width, flags)?;
				continue;
			case 'n':
				idx = @printf_str(#output, &&"\n", idx, precision, width, flags)?;
				continue;

			default:
				return FormattingFault.INVALID_FORMAT_TYPE!;
        }
        if (base != 10)
        {
            flags.plus = false;
            flags.space = false;
        }
        if (flags.precision) flags.zeropad = false;
		idx = @printf_integer(#output, next_variant(args.ptr, args.len, &arg_index), idx, precision, width, flags)?;
	}
	return idx;
/*
    precision = 0U;


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

fn void printf2(char[] format, args...)
{
	@printf_generic(putchar, format, args)!!;
}

fn void printf(char[] format, args... )
{
	@formatting(putchar, format, args);
}