module std::bits;

<*
 @require types::is_intlike($Typeof(i)) : `The input must be an integer or integer vector`
*>
macro reverse(i) => $$bitreverse(i);

<*
 @require types::is_intlike($Typeof(i)) : `The input must be an integer or integer vector`
*>
macro bswap(i) @builtin => $$bswap(i);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int uint[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int uint[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int uint[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro uint[<*>] uint[<*>].fshl(hi, uint[<*>] lo, uint[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro uint[<*>] uint[<*>].fshr(hi, uint[<*>] lo, uint[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro uint[<*>] uint[<*>].rotl(self, uint[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro uint[<*>] uint[<*>].rotr(self, uint[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int int[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int int[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int int[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro int[<*>] int[<*>].fshl(hi, int[<*>] lo, int[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro int[<*>] int[<*>].fshr(hi, int[<*>] lo, int[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro int[<*>] int[<*>].rotl(self, int[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro int[<*>] int[<*>].rotr(self, int[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int ushort[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int ushort[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int ushort[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro ushort[<*>] ushort[<*>].fshl(hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro ushort[<*>] ushort[<*>].fshr(hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro ushort[<*>] ushort[<*>].rotl(self, ushort[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro ushort[<*>] ushort[<*>].rotr(self, ushort[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int short[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int short[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int short[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro short[<*>] short[<*>].fshl(hi, short[<*>] lo, short[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro short[<*>] short[<*>].fshr(hi, short[<*>] lo, short[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro short[<*>] short[<*>].rotl(self, short[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro short[<*>] short[<*>].rotr(self, short[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int char[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int char[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int char[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro char[<*>] char[<*>].fshl(hi, char[<*>] lo, char[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro char[<*>] char[<*>].fshr(hi, char[<*>] lo, char[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro char[<*>] char[<*>].rotl(self, char[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro char[<*>] char[<*>].rotr(self, char[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int ichar[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int ichar[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int ichar[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro ichar[<*>] ichar[<*>].fshl(hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro ichar[<*>] ichar[<*>].fshr(hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro ichar[<*>] ichar[<*>].rotl(self, ichar[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro ichar[<*>] ichar[<*>].rotr(self, ichar[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int ulong[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int ulong[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int ulong[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro ulong[<*>] ulong[<*>].fshl(hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro ulong[<*>] ulong[<*>].fshr(hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro ulong[<*>] ulong[<*>].rotl(self, ulong[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro ulong[<*>] ulong[<*>].rotr(self, ulong[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int long[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int long[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int long[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro long[<*>] long[<*>].fshl(hi, long[<*>] lo, long[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro long[<*>] long[<*>].fshr(hi, long[<*>] lo, long[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro long[<*>] long[<*>].rotl(self, long[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro long[<*>] long[<*>].rotr(self, long[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int uint128[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int uint128[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int uint128[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro uint128[<*>] uint128[<*>].fshl(hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro uint128[<*>] uint128[<*>].fshr(hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro uint128[<*>] uint128[<*>].rotl(self, uint128[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro uint128[<*>] uint128[<*>].rotr(self, uint128[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int int128[<*>].popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int int128[<*>].ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int int128[<*>].clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro int128[<*>] int128[<*>].fshl(hi, int128[<*>] lo, int128[<*>] shift) => $$fshl(hi, lo, shift);
<* 
Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro int128[<*>] int128[<*>].fshr(hi, int128[<*>] lo, int128[<*>] shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro int128[<*>] int128[<*>].rotl(self, int128[<*>] shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro int128[<*>] int128[<*>].rotr(self, int128[<*>] shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int uint.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int uint.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int uint.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro uint uint.fshl(hi, uint lo, uint shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro uint uint.fshr(hi, uint lo, uint shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro uint uint.rotl(self, uint shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro uint uint.rotr(self, uint shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int int.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int int.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int int.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro int int.fshl(hi, int lo, int shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro int int.fshr(hi, int lo, int shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro int int.rotl(self, int shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro int int.rotr(self, int shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro ushort.popcount(self) => $$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro ushort.ctz(self) => $$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro ushort.clz(self) => $$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro ushort ushort.fshl(hi, ushort lo, ushort shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro ushort ushort.fshr(hi, ushort lo, ushort shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro ushort ushort.rotl(self, ushort shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro ushort ushort.rotr(self, ushort shift) => $$fshr(self, self, shift);

<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int short.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int short.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int short.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro short short.fshl(hi, short lo, short shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro short short.fshr(hi, short lo, short shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro short short.rotl(self, short shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro short short.rotr(self, short shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int char.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int char.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int char.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro char char.fshl(hi, char lo, char shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro char char.fshr(hi, char lo, char shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro char char.rotl(self, char shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro char char.rotr(self, char shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int ichar.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int ichar.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int ichar.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro ichar ichar.fshl(hi, ichar lo, ichar shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro ichar ichar.fshr(hi, ichar lo, ichar shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro ichar ichar.rotl(self, ichar shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro ichar ichar.rotr(self, ichar shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int ulong.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int ulong.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
 macro int ulong.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro ulong ulong.fshl(hi, ulong lo, ulong shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro ulong ulong.fshr(hi, ulong lo, ulong shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro ulong ulong.rotl(self, ulong shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro ulong ulong.rotr(self, ulong shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int long.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int long.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int long.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro long long.fshl(hi, long lo, long shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro long long.fshr(hi, long lo, long shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro long long.rotl(self, long shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro long long.rotr(self, long shift) => $$fshr(self, self, shift);

<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int uint128.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int uint128.ctz(self) => (int)$$ctz(self);
 <* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int uint128.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro uint128 uint128.fshl(hi, uint128 lo, uint128 shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro uint128 uint128.fshr(hi, uint128 lo, uint128 shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro uint128 uint128.rotl(self, uint128 shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro uint128 uint128.rotr(self, uint128 shift) => $$fshr(self, self, shift);


<* This is a popcount instruction, it counts the amount of bits turned to one *>
macro int int128.popcount(self) => (int)$$popcount(self);
<* Count Trailing Zeroes counts how far left the first (right to left) bit flipped to one is *>
macro int int128.ctz(self) => (int)$$ctz(self);
<* Count Leading Zeroes counts how far right the first (left to right) bit flipped to one is *>
macro int int128.clz(self) => (int)$$clz(self);
<*
 Funnel Shift Left(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it left by *shift* bits, and returns the upper bits.
*>
macro int128 int128.fshl(hi, int128 lo, int128 shift) => $$fshl(hi, lo, shift);
<* 
 Funnel Shift Right(*hi*, *lo*, *shift*) concatenates *hi* (high half) and *lo* (low half), shifts it right by *shift* bits, and returns the lower bits.
*>
macro int128 int128.fshr(hi, int128 lo, int128 shift) => $$fshr(hi, lo, shift);
<* Rotates the bits left, wrapping around so the bits on the high end move to the low end*>
macro int128 int128.rotl(self, int128 shift) => $$fshl(self, self, shift);
<* Rotates the bits right, wrapping around so the bits on the low end move to the high end*>
macro int128 int128.rotr(self, int128 shift) => $$fshr(self, self, shift);