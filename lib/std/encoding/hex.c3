module std::encoding::hex;
import std::encoding @norecurse;

// The implementation is based on https://www.rfc-editor.org/rfc/rfc4648

fn String encode_buffer(char[] code, char[] buffer)
{
	return (String)buffer[:encode_bytes(code, buffer)];
}

fn char[]? decode_buffer(char[] code, char[] buffer)
{
	return buffer[:decode_bytes(code, buffer)!];
}

fn String encode(Allocator allocator, char[] code)
{
	char[] data = allocator::alloc_array(allocator, char, encode_len(code.len));
	return (String)data[:encode_bytes(code, data)];
}

fn char[]? decode(Allocator allocator, char[] code)
{
	char[] data = allocator::alloc_array(allocator, char, decode_len(code.len));
	return data[:decode_bytes(code, data)!];
}

fn String tencode(char[] code) @inline => encode(tmem, code);
fn char[]? tdecode(char[] code) @inline => decode(tmem, code);


<*
 Calculate the size of the encoded data.
 @param n : "Size of the input to be encoded."
 @return "The size of the input once encoded."
*>
fn usz encode_len(usz n) => n * 2;

<*
 Encode the content of src into dst, which must be properly sized.
 @param src : "The input to be encoded."
 @param dst : "The encoded input."
 @return "The encoded size."
 @require dst.len >= encode_len(src.len) : "Destination array is not large enough"
*>
fn usz encode_bytes(char[] src, char[] dst)
{
	usz j = 0;
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
macro usz decode_len(usz n) => n / 2;

<*
 Decodes src into bytes. Returns the actual number of bytes written to dst.

 Expects that src only contains hexadecimal characters and that src has even
 length.

 @param src : "The input to be decoded."
 @param dst : "The decoded input."
 @require src.len % 2 == 0 : "src is not of even length"
 @require dst.len >= decode_len(src.len) : "Destination array is not large enough"
 @return? encoding::INVALID_CHARACTER
*>
fn usz? decode_bytes(char[] src, char[] dst)
{
	usz i;
	for (usz j = 1; j < src.len; j += 2)
	{
		char a = HEXREVERSE[src[j - 1]];
		char b = HEXREVERSE[src[j]];
		if (a > 0x0f || b > 0x0f) return encoding::INVALID_CHARACTER?;
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
