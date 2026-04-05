module std::bits;

<*
 @require types::is_intlike($typeof(i)) : `The input must be an integer or integer vector`
*>
macro reverse(i) => $$bitreverse(i);

<*
 @require types::is_intlike($typeof(i)) : `The input must be an integer or integer vector`
*>
macro bswap(i) @builtin => $$bswap(i);

macro int uint[<*>].popcount(self) => (int)$$popcount(self);
macro int uint[<*>].ctz(self) => (int)$$ctz(self);
macro int uint[<*>].clz(self) => (int)$$clz(self);
macro uint[<*>] uint[<*>].fshl(hi, uint[<*>] lo, uint[<*>] shift) => $$fshl(hi, lo, shift);
macro uint[<*>] uint[<*>].fshr(hi, uint[<*>] lo, uint[<*>] shift) => $$fshr(hi, lo, shift);
macro uint[<*>] uint[<*>].rotl(self, uint[<*>] shift) => $$fshl(self, self, shift);
macro uint[<*>] uint[<*>].rotr(self, uint[<*>] shift) => $$fshr(self, self, shift);

macro int int[<*>].popcount(self) => (int)$$popcount(self);
macro int int[<*>].ctz(self) => (int)$$ctz(self);
macro int int[<*>].clz(self) => (int)$$clz(self);
macro int[<*>] int[<*>].fshl(hi, int[<*>] lo, int[<*>] shift) => $$fshl(hi, lo, shift);
macro int[<*>] int[<*>].fshr(hi, int[<*>] lo, int[<*>] shift) => $$fshr(hi, lo, shift);
macro int[<*>] int[<*>].rotl(self, int[<*>] shift) => $$fshl(self, self, shift);
macro int[<*>] int[<*>].rotr(self, int[<*>] shift) => $$fshr(self, self, shift);

macro int ushort[<*>].popcount(self) => (int)$$popcount(self);
macro int ushort[<*>].ctz(self) => (int)$$ctz(self);
macro int ushort[<*>].clz(self) => (int)$$clz(self);
macro ushort[<*>] ushort[<*>].fshl(hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshl(hi, lo, shift);
macro ushort[<*>] ushort[<*>].fshr(hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshr(hi, lo, shift);
macro ushort[<*>] ushort[<*>].rotl(self, ushort[<*>] shift) => $$fshl(self, self, shift);
macro ushort[<*>] ushort[<*>].rotr(self, ushort[<*>] shift) => $$fshr(self, self, shift);

macro int short[<*>].popcount(self) => (int)$$popcount(self);
macro int short[<*>].ctz(self) => (int)$$ctz(self);
macro int short[<*>].clz(self) => (int)$$clz(self);
macro short[<*>] short[<*>].fshl(hi, short[<*>] lo, short[<*>] shift) => $$fshl(hi, lo, shift);
macro short[<*>] short[<*>].fshr(hi, short[<*>] lo, short[<*>] shift) => $$fshr(hi, lo, shift);
macro short[<*>] short[<*>].rotl(self, short[<*>] shift) => $$fshl(self, self, shift);
macro short[<*>] short[<*>].rotr(self, short[<*>] shift) => $$fshr(self, self, shift);

macro int char[<*>].popcount(self) => (int)$$popcount(self);
macro int char[<*>].ctz(self) => (int)$$ctz(self);
macro int char[<*>].clz(self) => (int)$$clz(self);
macro char[<*>] char[<*>].fshl(hi, char[<*>] lo, char[<*>] shift) => $$fshl(hi, lo, shift);
macro char[<*>] char[<*>].fshr(hi, char[<*>] lo, char[<*>] shift) => $$fshr(hi, lo, shift);
macro char[<*>] char[<*>].rotl(self, char[<*>] shift) => $$fshl(self, self, shift);
macro char[<*>] char[<*>].rotr(self, char[<*>] shift) => $$fshr(self, self, shift);

macro int ichar[<*>].popcount(self) => (int)$$popcount(self);
macro int ichar[<*>].ctz(self) => (int)$$ctz(self);
macro int ichar[<*>].clz(self) => (int)$$clz(self);
macro ichar[<*>] ichar[<*>].fshl(hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshl(hi, lo, shift);
macro ichar[<*>] ichar[<*>].fshr(hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshr(hi, lo, shift);
macro ichar[<*>] ichar[<*>].rotl(self, ichar[<*>] shift) => $$fshl(self, self, shift);
macro ichar[<*>] ichar[<*>].rotr(self, ichar[<*>] shift) => $$fshr(self, self, shift);

macro int ulong[<*>].popcount(self) => (int)$$popcount(self);
macro int ulong[<*>].ctz(self) => (int)$$ctz(self);
macro int ulong[<*>].clz(self) => (int)$$clz(self);
macro ulong[<*>] ulong[<*>].fshl(hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshl(hi, lo, shift);
macro ulong[<*>] ulong[<*>].fshr(hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshr(hi, lo, shift);
macro ulong[<*>] ulong[<*>].rotl(self, ulong[<*>] shift) => $$fshl(self, self, shift);
macro ulong[<*>] ulong[<*>].rotr(self, ulong[<*>] shift) => $$fshr(self, self, shift);

macro int long[<*>].popcount(self) => (int)$$popcount(self);
macro int long[<*>].ctz(self) => (int)$$ctz(self);
macro int long[<*>].clz(self) => (int)$$clz(self);
macro long[<*>] long[<*>].fshl(hi, long[<*>] lo, long[<*>] shift) => $$fshl(hi, lo, shift);
macro long[<*>] long[<*>].fshr(hi, long[<*>] lo, long[<*>] shift) => $$fshr(hi, lo, shift);
macro long[<*>] long[<*>].rotl(self, long[<*>] shift) => $$fshl(self, self, shift);
macro long[<*>] long[<*>].rotr(self, long[<*>] shift) => $$fshr(self, self, shift);

macro int uint128[<*>].popcount(self) => (int)$$popcount(self);
macro int uint128[<*>].ctz(self) => (int)$$ctz(self);
macro int uint128[<*>].clz(self) => (int)$$clz(self);
macro uint128[<*>] uint128[<*>].fshl(hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshl(hi, lo, shift);
macro uint128[<*>] uint128[<*>].fshr(hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshr(hi, lo, shift);
macro uint128[<*>] uint128[<*>].rotl(self, uint128[<*>] shift) => $$fshl(self, self, shift);
macro uint128[<*>] uint128[<*>].rotr(self, uint128[<*>] shift) => $$fshr(self, self, shift);

macro int int128[<*>].popcount(self) => (int)$$popcount(self);
macro int int128[<*>].ctz(self) => (int)$$ctz(self);
macro int int128[<*>].clz(self) => (int)$$clz(self);
macro int128[<*>] int128[<*>].fshl(hi, int128[<*>] lo, int128[<*>] shift) => $$fshl(hi, lo, shift);
macro int128[<*>] int128[<*>].fshr(hi, int128[<*>] lo, int128[<*>] shift) => $$fshr(hi, lo, shift);
macro int128[<*>] int128[<*>].rotl(self, int128[<*>] shift) => $$fshl(self, self, shift);
macro int128[<*>] int128[<*>].rotr(self, int128[<*>] shift) => $$fshr(self, self, shift);

macro int uint.popcount(self) => (int)$$popcount(self);
macro int uint.ctz(self) => (int)$$ctz(self);
macro int uint.clz(self) => (int)$$clz(self);
macro uint uint.fshl(hi, uint lo, uint shift) => $$fshl(hi, lo, shift);
macro uint uint.fshr(hi, uint lo, uint shift) => $$fshr(hi, lo, shift);
macro uint uint.rotl(self, uint shift) => $$fshl(self, self, shift);
macro uint uint.rotr(self, uint shift) => $$fshr(self, self, shift);

macro int int.popcount(self) => (int)$$popcount(self);
macro int int.ctz(self) => (int)$$ctz(self);
macro int int.clz(self) => (int)$$clz(self);
macro int int.fshl(hi, int lo, int shift) => $$fshl(hi, lo, shift);
macro int int.fshr(hi, int lo, int shift) => $$fshr(hi, lo, shift);
macro int int.rotl(self, int shift) => $$fshl(self, self, shift);
macro int int.rotr(self, int shift) => $$fshr(self, self, shift);

macro ushort.popcount(self) => $$popcount(self);
macro ushort.ctz(self) => $$ctz(self);
macro ushort.clz(self) => $$clz(self);
macro ushort ushort.fshl(hi, ushort lo, ushort shift) => $$fshl(hi, lo, shift);
macro ushort ushort.fshr(hi, ushort lo, ushort shift) => $$fshr(hi, lo, shift);
macro ushort ushort.rotl(self, ushort shift) => $$fshl(self, self, shift);
macro ushort ushort.rotr(self, ushort shift) => $$fshr(self, self, shift);

macro int short.popcount(self) => (int)$$popcount(self);
macro int short.ctz(self) => (int)$$ctz(self);
macro int short.clz(self) => (int)$$clz(self);
macro short short.fshl(hi, short lo, short shift) => $$fshl(hi, lo, shift);
macro short short.fshr(hi, short lo, short shift) => $$fshr(hi, lo, shift);
macro short short.rotl(self, short shift) => $$fshl(self, self, shift);
macro short short.rotr(self, short shift) => $$fshr(self, self, shift);

macro int char.popcount(self) => (int)$$popcount(self);
macro int char.ctz(self) => (int)$$ctz(self);
macro int char.clz(self) => (int)$$clz(self);
macro char char.fshl(hi, char lo, char shift) => $$fshl(hi, lo, shift);
macro char char.fshr(hi, char lo, char shift) => $$fshr(hi, lo, shift);
macro char char.rotl(self, char shift) => $$fshl(self, self, shift);
macro char char.rotr(self, char shift) => $$fshr(self, self, shift);

macro int ichar.popcount(self) => (int)$$popcount(self);
macro int ichar.ctz(self) => (int)$$ctz(self);
macro int ichar.clz(self) => (int)$$clz(self);
macro ichar ichar.fshl(hi, ichar lo, ichar shift) => $$fshl(hi, lo, shift);
macro ichar ichar.fshr(hi, ichar lo, ichar shift) => $$fshr(hi, lo, shift);
macro ichar ichar.rotl(self, ichar shift) => $$fshl(self, self, shift);
macro ichar ichar.rotr(self, ichar shift) => $$fshr(self, self, shift);

macro int ulong.popcount(self) => (int)$$popcount(self);
macro int ulong.ctz(self) => (int)$$ctz(self);
macro int ulong.clz(self) => (int)$$clz(self);
macro ulong ulong.fshl(hi, ulong lo, ulong shift) => $$fshl(hi, lo, shift);
macro ulong ulong.fshr(hi, ulong lo, ulong shift) => $$fshr(hi, lo, shift);
macro ulong ulong.rotl(self, ulong shift) => $$fshl(self, self, shift);
macro ulong ulong.rotr(self, ulong shift) => $$fshr(self, self, shift);

macro int long.popcount(self) => (int)$$popcount(self);
macro int long.ctz(self) => (int)$$ctz(self);
macro int long.clz(self) => (int)$$clz(self);
macro long long.fshl(hi, long lo, long shift) => $$fshl(hi, lo, shift);
macro long long.fshr(hi, long lo, long shift) => $$fshr(hi, lo, shift);
macro long long.rotl(self, long shift) => $$fshl(self, self, shift);
macro long long.rotr(self, long shift) => $$fshr(self, self, shift);

macro int uint128.popcount(self) => (int)$$popcount(self);
macro int uint128.ctz(self) => (int)$$ctz(self);
macro int uint128.clz(self) => (int)$$clz(self);
macro uint128 uint128.fshl(hi, uint128 lo, uint128 shift) => $$fshl(hi, lo, shift);
macro uint128 uint128.fshr(hi, uint128 lo, uint128 shift) => $$fshr(hi, lo, shift);
macro uint128 uint128.rotl(self, uint128 shift) => $$fshl(self, self, shift);
macro uint128 uint128.rotr(self, uint128 shift) => $$fshr(self, self, shift);

macro int int128.popcount(self) => (int)$$popcount(self);
macro int int128.ctz(self) => (int)$$ctz(self);
macro int int128.clz(self) => (int)$$clz(self);
macro int128 int128.fshl(hi, int128 lo, int128 shift) => $$fshl(hi, lo, shift);
macro int128 int128.fshr(hi, int128 lo, int128 shift) => $$fshr(hi, lo, shift);
macro int128 int128.rotl(self, int128 shift) => $$fshl(self, self, shift);
macro int128 int128.rotr(self, int128 shift) => $$fshr(self, self, shift);
