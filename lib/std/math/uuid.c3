module std::math::uuid;
import std::encoding::hex @public;
import std::math::random @public;
import std::time::os;
import std::io;

<*
UUID as defined in RFC 9562
*>
typedef Uuid (Printable) = char[16];

<*
@return "The UUID variant bits"
*>
fn int Uuid.variant(&self) => (*self)[8] >> 4;

<*
@return "The UUID version number"
*>
fn int Uuid.version(&self) => (*self)[6] >> 4;

<*
Retrieve the timestamp from a version 7 UUID.
@require self.version() == 7
*>
fn Time Uuid.time_v7(&self)
{
	long msecond_timestamp =
		(ulong)(*self)[0] << 40
		| (ulong)(*self)[1] << 32
		| (ulong)(*self)[2] << 24
		| (ulong)(*self)[3] << 16
		| (ulong)(*self)[4] << 8
		| (ulong)(*self)[5];
	return (Time)(msecond_timestamp * 1000);
}

<*
This is a deprecated alias for generate_v4, maintained for backwards
compatibility.
@return "A version 4 UUID from the default random"
@ensure return.version() == 4
*>
fn Uuid generate() => generate_v4();

<*
@return "A version 4 UUID from the default random"
@ensure return.version() == 4
*>
fn Uuid generate_v4()
{
	random::init_default_random();
	return generate_v4_from_random(&random::default_random);
}

<*
This is a deprecated alias for generate_v4_from_random, maintained for backwards
compatibility.
@return "A version 4 UUID from the given random"
@ensure return.version() == 4
*>
fn Uuid generate_from_random(Random random) => generate_v4_from_random(random);

<*
@return "A version 4 UUID from the given random"
@ensure return.version() == 4
*>
fn Uuid generate_v4_from_random(Random random)
{
	Uuid uuid @noinit;
	random.next_bytes(&(char[16])uuid);
	uuid[6] = (uuid[6] & 0b0000_1111) | 0b0100_0000;
	uuid[8] = (uuid[8] & 0b0011_1111) | 0b1000_0000;
	return uuid;
}

<*
@return "A version 7 UUID from the default random"
@ensure return.version() == 7
*>
fn Uuid generate_v7() @feat(LIBC)
{
	random::init_default_random();
	return generate_v7_from_random(&random::default_random);
}

<*
@return "A version 7 UUID from the given random"
@ensure return.version() == 7
*>
fn Uuid generate_v7_from_random(Random random) @feat(LIBC)
{
	Uuid uuid @noinit;

	// Pack the timestamp into the first 48 bits
	long usecond_timestamp = (long)os::native_timestamp();
	long msecond_timestamp = usecond_timestamp / 1000;
	uuid[0] = (char)((msecond_timestamp & 0xFF0000000000) >> 40);
	uuid[1] = (char)((msecond_timestamp & 0x00FF00000000) >> 32);
	uuid[2] = (char)((msecond_timestamp & 0x0000FF000000) >> 24);
	uuid[3] = (char)((msecond_timestamp & 0x000000FF0000) >> 16);
	uuid[4] = (char)((msecond_timestamp & 0x00000000FF00) >> 8);
	uuid[5] = (char)(msecond_timestamp & 0x0000000000FF);

	random.next_bytes(uuid[6..15]);
	uuid[6] = (uuid[6] & 0b0000_1111) | 0b0111_0000;
	uuid[8] = (uuid[8] & 0b0011_1111) | 0b1000_0000;

	return uuid;
}

fn sz? Uuid.to_format(&self, Formatter* formatter) @dynamic
{
	char[36] result @noinit;
	self.write_into_string(&result);
	return formatter.print((String)&result);
}

fn String Uuid.to_string(&self, Allocator allocator)
{
	char[36]* result = alloc::alloc(allocator, char[36]);
	self.write_into_string(result);
	return (String)result;
}

<*
@param [&out] dest
*>
fn void Uuid.write_into_string(&self, char[36]* dest)
{
	(*dest)[0] = hex::HEXALPHABET[(*self)[0] >> 4];
	(*dest)[1] = hex::HEXALPHABET[(*self)[0] & 0x0F];
	(*dest)[2] = hex::HEXALPHABET[(*self)[1] >> 4];
	(*dest)[3] = hex::HEXALPHABET[(*self)[1] & 0x0F];
	(*dest)[4] = hex::HEXALPHABET[(*self)[2] >> 4];
	(*dest)[5] = hex::HEXALPHABET[(*self)[2] & 0x0F];
	(*dest)[6] = hex::HEXALPHABET[(*self)[3] >> 4];
	(*dest)[7] = hex::HEXALPHABET[(*self)[3] & 0x0F];
	(*dest)[8] = '-';
	(*dest)[9] = hex::HEXALPHABET[(*self)[4] >> 4];
	(*dest)[10] = hex::HEXALPHABET[(*self)[4] & 0x0F];
	(*dest)[11] = hex::HEXALPHABET[(*self)[5] >> 4];
	(*dest)[12] = hex::HEXALPHABET[(*self)[5] & 0x0F];
	(*dest)[13] = '-';
	(*dest)[14] = hex::HEXALPHABET[(*self)[6] >> 4];
	(*dest)[15] = hex::HEXALPHABET[(*self)[6] & 0x0F];
	(*dest)[16] = hex::HEXALPHABET[(*self)[7] >> 4];
	(*dest)[17] = hex::HEXALPHABET[(*self)[7] & 0x0F];
	(*dest)[18] = '-';
	(*dest)[19] = hex::HEXALPHABET[(*self)[8] >> 4];
	(*dest)[20] = hex::HEXALPHABET[(*self)[8] & 0x0F];
	(*dest)[21] = hex::HEXALPHABET[(*self)[9] >> 4];
	(*dest)[22] = hex::HEXALPHABET[(*self)[9] & 0x0F];
	(*dest)[23] = '-';
	(*dest)[24] = hex::HEXALPHABET[(*self)[10] >> 4];
	(*dest)[25] = hex::HEXALPHABET[(*self)[10] & 0x0F];
	(*dest)[26] = hex::HEXALPHABET[(*self)[11] >> 4];
	(*dest)[27] = hex::HEXALPHABET[(*self)[11] & 0x0F];
	(*dest)[28] = hex::HEXALPHABET[(*self)[12] >> 4];
	(*dest)[29] = hex::HEXALPHABET[(*self)[12] & 0x0F];
	(*dest)[30] = hex::HEXALPHABET[(*self)[13] >> 4];
	(*dest)[31] = hex::HEXALPHABET[(*self)[13] & 0x0F];
	(*dest)[32] = hex::HEXALPHABET[(*self)[14] >> 4];
	(*dest)[33] = hex::HEXALPHABET[(*self)[14] & 0x0F];
	(*dest)[34] = hex::HEXALPHABET[(*self)[15] >> 4];
	(*dest)[35] = hex::HEXALPHABET[(*self)[15] & 0x0F];
}

excuse INVALID_UUID_STRING;

<*
@return "A UUID parsed from the given string"
@return? INVALID_UUID_STRING : "If the given string is not valid RFC 9562 format"
*>
fn Uuid? from_string(String str)
{
	if (str.len != 36) return INVALID_UUID_STRING~;

	Uuid result @noinit;
	result[0] = byte_from_hex(str[0], str[1])!;
	result[1] = byte_from_hex(str[2], str[3])!;
	result[2] = byte_from_hex(str[4], str[5])!;
	result[3] = byte_from_hex(str[6], str[7])!;
	if (str[8] != '-') return INVALID_UUID_STRING~;
	result[4] = byte_from_hex(str[9], str[10])!;
	result[5] = byte_from_hex(str[11], str[12])!;
	if (str[13] != '-') return INVALID_UUID_STRING~;
	result[6] = byte_from_hex(str[14], str[15])!;
	result[7] = byte_from_hex(str[16], str[17])!;
	if (str[18] != '-') return INVALID_UUID_STRING~;
	result[8] = byte_from_hex(str[19], str[20])!;
	result[9] = byte_from_hex(str[21], str[22])!;
	if (str[23] != '-') return INVALID_UUID_STRING~;
	result[10] = byte_from_hex(str[24], str[25])!;
	result[11] = byte_from_hex(str[26], str[27])!;
	result[12] = byte_from_hex(str[28], str[29])!;
	result[13] = byte_from_hex(str[30], str[31])!;
	result[14] = byte_from_hex(str[32], str[33])!;
	result[15] = byte_from_hex(str[34], str[35])!;

	return result;
}

fn char? byte_from_hex(char first_digit, char second_digit) @private
{
	char upper_bits = hex::HEXREVERSE[first_digit];
	if (upper_bits == 255) return INVALID_UUID_STRING~;

	char lower_bits = hex::HEXREVERSE[second_digit];
	if (lower_bits == 255) return INVALID_UUID_STRING~;

	return upper_bits << 4 | lower_bits;
}
