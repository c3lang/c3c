module std::encoding::hex;
import std::encoding @norecurse;

// The implementation is based on https://www.rfc-editor.org/rfc/rfc4648

fn String encode_into(char[] dst, char[] src)
{
	return (String)dst[:encode_bytes_into(dst, src)];
}

fn char[]? decode_into(char[] dst, char[] src)
{
	return dst[:decode_bytes_into(dst, src)!];
}

fn String encode(Allocator allocator, char[] code)
{
	char[] data = alloc::alloc_array(allocator, char, encode_len(code.len));
	return (String)data[:encode_bytes_into(data, code)];
}

fn char[]? decode(Allocator allocator, char[] code)
{
	char[] data = alloc::alloc_array(allocator, char, decode_len(code.len));
	return data[:decode_bytes_into(data, code)!];
}

fn String tencode(char[] code) @inline => encode(tmem, code);
fn char[]? tdecode(char[] code) @inline => decode(tmem, code);


<*
 Calculate the size of the encoded data.
 @param n : "Size of the input to be encoded."
 @return "The size of the input once encoded."
*>
fn sz encode_len(sz n) => n * 2;

<*
 Encode the content of src into dst, which must be properly sized.
 @param dst : "The encoded input."
 @param src : "The input to be encoded."
 @return "The encoded size."
 @require dst.len >= encode_len(src.len) : "Destination array is not large enough"
*>
fn sz encode_bytes_into(char[] dst, char[] src)
{
	sz j = 0;
	foreach (v : src)
	{
		dst[j] = HEXALPHABET[v >> 4];
		dst[j + 1] = HEXALPHABET[v & 0x0f];
		j = j + 2;
	}
	return src.len * 2;
}

<*
 Calculate the size of the decoded data.
 @param n : "Size of the input to be decoded."
 @return "The size of the input once decoded."
*>
macro sz decode_len(sz n) => n / 2;


<*
 Decodes src into bytes. Returns the actual number of bytes written to dst.

 Expects that src only contains hexadecimal characters and that src has even
 length.

 @param dst : "The decoded input."
 @param src : "The input to be decoded."
 @require src.len % 2 == 0 : "src is not of even length"
 @require dst.len >= decode_len(src.len) : "Destination array is not large enough"
 @return? encoding::INVALID_CHARACTER
*>
fn sz? decode_bytes_into(char[] dst, char[] src)
{
	sz i;
	for (sz j = 1; j < src.len; j += 2)
	{
		char a = HEXREVERSE[src[j - 1]];
		char b = HEXREVERSE[src[j]];
		if (a > 0x0f || b > 0x0f) return encoding::INVALID_CHARACTER~;
		dst[i] = (a << 4) | b;
		i++;
	}
	return i;
}

const char[*] HEXALPHABET @private = "0123456789abcdef";
const char[*] HEXREVERSE @private =
x`ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  00010203040506070809ffffffffffff
  ff0a0b0c0d0e0fffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ff0a0b0c0d0e0fffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff
  ffffffffffffffffffffffffffffffff`;
