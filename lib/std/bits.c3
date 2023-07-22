module std::bits;

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro reverse(i) => $$bitreverse(i);

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro bswap(i) @builtin => $$bswap(i);

macro uint[<*>].popcount(self) => $$popcount(self);
macro uint[<*>].ctz(self) => $$ctz(self);
macro uint[<*>].clz(self) => $$clz(self);
macro uint[<*>] uint[<*>].fshl(uint[<*>] hi, uint[<*>] lo, uint[<*>] shift) => $$fshl(hi, lo, shift);
macro uint[<*>] uint[<*>].fshr(uint[<*>] hi, uint[<*>] lo, uint[<*>] shift) => $$fshr(hi, lo, shift);
macro uint[<*>] uint[<*>].rotl(self, uint[<*>] shift) => $$fshl(self, self, shift);
macro uint[<*>] uint[<*>].rotr(self, uint[<*>] shift) => $$fshr(self, self, shift);

macro int[<*>].popcount(self) => $$popcount(self);
macro int[<*>].ctz(self) => $$ctz(self);
macro int[<*>].clz(self) => $$clz(self);
macro int[<*>] int[<*>].fshl(int[<*>] hi, int[<*>] lo, int[<*>] shift) => $$fshl(hi, lo, shift);
macro int[<*>] int[<*>].fshr(int[<*>] hi, int[<*>] lo, int[<*>] shift) => $$fshr(hi, lo, shift);
macro int[<*>] int[<*>].rotl(self, int[<*>] shift) => $$fshl(self, self, shift);
macro int[<*>] int[<*>].rotr(self, int[<*>] shift) => $$fshr(self, self, shift);

macro ushort[<*>].popcount(ushort[<*>] self) => $$popcount(self);
macro ushort[<*>].ctz(ushort[<*>] self) => $$ctz(self);
macro ushort[<*>].clz(ushort[<*>] self) => $$clz(self);
macro ushort[<*>] ushort[<*>].fshl(ushort[<*>] hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshl(hi, lo, shift);
macro ushort[<*>] ushort[<*>].fshr(ushort[<*>] hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshr(hi, lo, shift);
macro ushort[<*>] ushort[<*>].rotl(ushort[<*>] self, ushort[<*>] shift) => $$fshl(self, self, shift);
macro ushort[<*>] ushort[<*>].rotr(ushort[<*>] self, ushort[<*>] shift) => $$fshr(self, self, shift);

macro short[<*>].popcount(short[<*>] self) => $$popcount(self);
macro short[<*>].ctz(short[<*>] self) => $$ctz(self);
macro short[<*>].clz(short[<*>] self) => $$clz(self);
macro short[<*>] short[<*>].fshl(short[<*>] hi, short[<*>] lo, short[<*>] shift) => $$fshl(hi, lo, shift);
macro short[<*>] short[<*>].fshr(short[<*>] hi, short[<*>] lo, short[<*>] shift) => $$fshr(hi, lo, shift);
macro short[<*>] short[<*>].rotl(short[<*>] self, short[<*>] shift) => $$fshl(self, self, shift);
macro short[<*>] short[<*>].rotr(short[<*>] self, short[<*>] shift) => $$fshr(self, self, shift);

macro char[<*>].popcount(char[<*>] self) => $$popcount(self);
macro char[<*>].ctz(char[<*>] self) => $$ctz(self);
macro char[<*>].clz(char[<*>] self) => $$clz(self);
macro char[<*>] char[<*>].fshl(char[<*>] hi, char[<*>] lo, char[<*>] shift) => $$fshl(hi, lo, shift);
macro char[<*>] char[<*>].fshr(char[<*>] hi, char[<*>] lo, char[<*>] shift) => $$fshr(hi, lo, shift);
macro char[<*>] char[<*>].rotl(char[<*>] self, char[<*>] shift) => $$fshl(self, self, shift);
macro char[<*>] char[<*>].rotr(char[<*>] self, char[<*>] shift) => $$fshr(self, self, shift);

macro ichar[<*>].popcount(ichar[<*>] self) => $$popcount(self);
macro ichar[<*>].ctz(ichar[<*>] self) => $$ctz(self);
macro ichar[<*>].clz(ichar[<*>] self) => $$clz(self);
macro ichar[<*>] ichar[<*>].fshl(ichar[<*>] hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshl(hi, lo, shift);
macro ichar[<*>] ichar[<*>].fshr(ichar[<*>] hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshr(hi, lo, shift);
macro ichar[<*>] ichar[<*>].rotl(ichar[<*>] self, ichar[<*>] shift) => $$fshl(self, self, shift);
macro ichar[<*>] ichar[<*>].rotr(ichar[<*>] self, ichar[<*>] shift) => $$fshr(self, self, shift);

macro ulong[<*>].popcount(ulong[<*>] self) => $$popcount(self);
macro ulong[<*>].ctz(ulong[<*>] self) => $$ctz(self);
macro ulong[<*>].clz(ulong[<*>] self) => $$clz(self);
macro ulong[<*>] ulong[<*>].fshl(ulong[<*>] hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshl(hi, lo, shift);
macro ulong[<*>] ulong[<*>].fshr(ulong[<*>] hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshr(hi, lo, shift);
macro ulong[<*>] ulong[<*>].rotl(ulong[<*>] self, ulong[<*>] shift) => $$fshl(self, self, shift);
macro ulong[<*>] ulong[<*>].rotr(ulong[<*>] self, ulong[<*>] shift) => $$fshr(self, self, shift);

macro long[<*>].popcount(long[<*>] self) => $$popcount(self);
macro long[<*>].ctz(long[<*>] self) => $$ctz(self);
macro long[<*>].clz(long[<*>] self) => $$clz(self);
macro long[<*>] long[<*>].fshl(long[<*>] hi, long[<*>] lo, long[<*>] shift) => $$fshl(hi, lo, shift);
macro long[<*>] long[<*>].fshr(long[<*>] hi, long[<*>] lo, long[<*>] shift) => $$fshr(hi, lo, shift);
macro long[<*>] long[<*>].rotl(long[<*>] self, long[<*>] shift) => $$fshl(self, self, shift);
macro long[<*>] long[<*>].rotr(long[<*>] self, long[<*>] shift) => $$fshr(self, self, shift);

macro uint128[<*>].popcount(uint128[<*>] self) => $$popcount(self);
macro uint128[<*>].ctz(uint128[<*>] self) => $$ctz(self);
macro uint128[<*>].clz(uint128[<*>] self) => $$clz(self);
macro uint128[<*>] uint128[<*>].fshl(uint128[<*>] hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshl(hi, lo, shift);
macro uint128[<*>] uint128[<*>].fshr(uint128[<*>] hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshr(hi, lo, shift);
macro uint128[<*>] uint128[<*>].rotl(uint128[<*>] self, uint128[<*>] shift) => $$fshl(self, self, shift);
macro uint128[<*>] uint128[<*>].rotr(uint128[<*>] self, uint128[<*>] shift) => $$fshr(self, self, shift);

macro int128[<*>].popcount(int128[<*>] self) => $$popcount(self);
macro int128[<*>].ctz(int128[<*>] self) => $$ctz(self);
macro int128[<*>].clz(int128[<*>] self) => $$clz(self);
macro int128[<*>] int128[<*>].fshl(int128[<*>] hi, int128[<*>] lo, int128[<*>] shift) => $$fshl(hi, lo, shift);
macro int128[<*>] int128[<*>].fshr(int128[<*>] hi, int128[<*>] lo, int128[<*>] shift) => $$fshr(hi, lo, shift);
macro int128[<*>] int128[<*>].rotl(int128[<*>] self, int128[<*>] shift) => $$fshl(self, self, shift);
macro int128[<*>] int128[<*>].rotr(int128[<*>] self, int128[<*>] shift) => $$fshr(self, self, shift);

macro uint.popcount(self) => $$popcount(self);
macro uint.ctz(self) => $$ctz(self);
macro uint.clz(self) => $$clz(self);
macro uint uint.fshl(uint hi, uint lo, uint shift) => $$fshl(hi, lo, shift);
macro uint uint.fshr(uint hi, uint lo, uint shift) => $$fshr(hi, lo, shift);
macro uint uint.rotl(uint self, uint shift) => $$fshl(self, self, shift);
macro uint uint.rotr(uint self, uint shift) => $$fshr(self, self, shift);

macro int.popcount(self) => $$popcount(self);
macro int.ctz(self) => $$ctz(self);
macro int.clz(self) => $$clz(self);
macro int int.fshl(int hi, int lo, int shift) => $$fshl(hi, lo, shift);
macro int int.fshr(int hi, int lo, int shift) => $$fshr(hi, lo, shift);
macro int int.rotl(int self, int shift) => $$fshl(self, self, shift);
macro int int.rotr(int self, int shift) => $$fshr(self, self, shift);

macro ushort.popcount(self) => $$popcount(self);
macro ushort.ctz(self) => $$ctz(self);
macro ushort.clz(self) => $$clz(self);
macro ushort ushort.fshl(ushort hi, ushort lo, ushort shift) => $$fshl(hi, lo, shift);
macro ushort ushort.fshr(ushort hi, ushort lo, ushort shift) => $$fshr(hi, lo, shift);
macro ushort ushort.rotl(ushort self, ushort shift) => $$fshl(self, self, shift);
macro ushort ushort.rotr(ushort self, ushort shift) => $$fshr(self, self, shift);

macro short.popcount(self) => $$popcount(self);
macro short.ctz(self) => $$ctz(self);
macro short.clz(self) => $$clz(self);
macro short short.fshl(short hi, short lo, short shift) => $$fshl(hi, lo, shift);
macro short short.fshr(short hi, short lo, short shift) => $$fshr(hi, lo, shift);
macro short short.rotl(short self, short shift) => $$fshl(self, self, shift);
macro short short.rotr(short self, short shift) => $$fshr(self, self, shift);

macro char.popcount(self) => $$popcount(self);
macro char.ctz(self) => $$ctz(self);
macro char.clz(self) => $$clz(self);
macro char char.fshl(char hi, char lo, char shift) => $$fshl(hi, lo, shift);
macro char char.fshr(char hi, char lo, char shift) => $$fshr(hi, lo, shift);
macro char char.rotl(char self, char shift) => $$fshl(self, self, shift);
macro char char.rotr(char self, char shift) => $$fshr(self, self, shift);

macro ichar.popcount(self) => $$popcount(self);
macro ichar.ctz(self) => $$ctz(self);
macro ichar.clz(self) => $$clz(self);
macro ichar ichar.fshl(ichar hi, ichar lo, ichar shift) => $$fshl(hi, lo, shift);
macro ichar ichar.fshr(ichar hi, ichar lo, ichar shift) => $$fshr(hi, lo, shift);
macro ichar ichar.rotl(ichar self, ichar shift) => $$fshl(self, self, shift);
macro ichar ichar.rotr(ichar self, ichar shift) => $$fshr(self, self, shift);

macro ulong.popcount(self) => $$popcount(self);
macro ulong.ctz(self) => $$ctz(self);
macro ulong.clz(self) => $$clz(self);
macro ulong ulong.fshl(ulong hi, ulong lo, ulong shift) => $$fshl(hi, lo, shift);
macro ulong ulong.fshr(ulong hi, ulong lo, ulong shift) => $$fshr(hi, lo, shift);
macro ulong ulong.rotl(ulong self, ulong shift) => $$fshl(self, self, shift);
macro ulong ulong.rotr(ulong self, ulong shift) => $$fshr(self, self, shift);

macro long.popcount(self) => $$popcount(self);
macro long.ctz(self) => $$ctz(self);
macro long.clz(self) => $$clz(self);
macro long long.fshl(long hi, long lo, long shift) => $$fshl(hi, lo, shift);
macro long long.fshr(long hi, long lo, long shift) => $$fshr(hi, lo, shift);
macro long long.rotl(long self, long shift) => $$fshl(self, self, shift);
macro long long.rotr(long self, long shift) => $$fshr(self, self, shift);

macro uint128.popcount(self) => $$popcount(self);
macro uint128.ctz(self) => $$ctz(self);
macro uint128.clz(self) => $$clz(self);
macro uint128 uint128.fshl(uint128 hi, uint128 lo, uint128 shift) => $$fshl(hi, lo, shift);
macro uint128 uint128.fshr(uint128 hi, uint128 lo, uint128 shift) => $$fshr(hi, lo, shift);
macro uint128 uint128.rotl(self, uint128 shift) => $$fshl(self, self, shift);
macro uint128 uint128.rotr(self, uint128 shift) => $$fshr(self, self, shift);

macro int128.popcount(self) => $$popcount(self);
macro int128.ctz(self) => $$ctz(self);
macro int128.clz(self) => $$clz(self);
macro int128 int128.fshl(int128 hi, int128 lo, int128 shift) => $$fshl(hi, lo, shift);
macro int128 int128.fshr(int128 hi, int128 lo, int128 shift) => $$fshr(hi, lo, shift);
macro int128 int128.rotl(self, int128 shift) => $$fshl(self, self, shift);
macro int128 int128.rotr(self, int128 shift) => $$fshr(self, self, shift);
