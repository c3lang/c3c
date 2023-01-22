module std::bits;


/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro reverse(i) => $$bitreverse(i);

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro bswap(i) @builtin => $$bswap(i);


macro uint[<*>].popcount(uint[<*>] i) => $$popcount(i);
macro uint[<*>].ctz(uint[<*>] i) => $$ctz(i);
macro uint[<*>].clz(uint[<*>] i) => $$clz(i);
macro uint[<*>] uint[<*>].fshl(uint[<*>] hi, uint[<*>] lo, uint[<*>] shift) => $$fshl(hi, lo, shift);
macro uint[<*>] uint[<*>].fshr(uint[<*>] hi, uint[<*>] lo, uint[<*>] shift) => $$fshr(hi, lo, shift);
macro uint[<*>] uint[<*>].rotl(uint[<*>] i, uint[<*>] shift) => $$fshl(i, i, shift);
macro uint[<*>] uint[<*>].rotr(uint[<*>] i, uint[<*>] shift) => $$fshr(i, i, shift);

macro int[<*>].popcount(int[<*>] i) => $$popcount(i);
macro int[<*>].ctz(int[<*>] i) => $$ctz(i);
macro int[<*>].clz(int[<*>] i) => $$clz(i);
macro int[<*>] int[<*>].fshl(int[<*>] hi, int[<*>] lo, int[<*>] shift) => $$fshl(hi, lo, shift);
macro int[<*>] int[<*>].fshr(int[<*>] hi, int[<*>] lo, int[<*>] shift) => $$fshr(hi, lo, shift);
macro int[<*>] int[<*>].rotl(int[<*>] i, int[<*>] shift) => $$fshl(i, i, shift);
macro int[<*>] int[<*>].rotr(int[<*>] i, int[<*>] shift) => $$fshr(i, i, shift);

macro ushort[<*>].popcount(ushort[<*>] i) => $$popcount(i);
macro ushort[<*>].ctz(ushort[<*>] i) => $$ctz(i);
macro ushort[<*>].clz(ushort[<*>] i) => $$clz(i);
macro ushort[<*>] ushort[<*>].fshl(ushort[<*>] hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshl(hi, lo, shift);
macro ushort[<*>] ushort[<*>].fshr(ushort[<*>] hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshr(hi, lo, shift);
macro ushort[<*>] ushort[<*>].rotl(ushort[<*>] i, ushort[<*>] shift) => $$fshl(i, i, shift);
macro ushort[<*>] ushort[<*>].rotr(ushort[<*>] i, ushort[<*>] shift) => $$fshr(i, i, shift);

macro short[<*>].popcount(short[<*>] i) => $$popcount(i);
macro short[<*>].ctz(short[<*>] i) => $$ctz(i);
macro short[<*>].clz(short[<*>] i) => $$clz(i);
macro short[<*>] short[<*>].fshl(short[<*>] hi, short[<*>] lo, short[<*>] shift) => $$fshl(hi, lo, shift);
macro short[<*>] short[<*>].fshr(short[<*>] hi, short[<*>] lo, short[<*>] shift) => $$fshr(hi, lo, shift);
macro short[<*>] short[<*>].rotl(short[<*>] i, short[<*>] shift) => $$fshl(i, i, shift);
macro short[<*>] short[<*>].rotr(short[<*>] i, short[<*>] shift) => $$fshr(i, i, shift);

macro char[<*>].popcount(char[<*>] i) => $$popcount(i);
macro char[<*>].ctz(char[<*>] i) => $$ctz(i);
macro char[<*>].clz(char[<*>] i) => $$clz(i);
macro char[<*>] char[<*>].fshl(char[<*>] hi, char[<*>] lo, char[<*>] shift) => $$fshl(hi, lo, shift);
macro char[<*>] char[<*>].fshr(char[<*>] hi, char[<*>] lo, char[<*>] shift) => $$fshr(hi, lo, shift);
macro char[<*>] char[<*>].rotl(char[<*>] i, char[<*>] shift) => $$fshl(i, i, shift);
macro char[<*>] char[<*>].rotr(char[<*>] i, char[<*>] shift) => $$fshr(i, i, shift);

macro ichar[<*>].popcount(ichar[<*>] i) => $$popcount(i);
macro ichar[<*>].ctz(ichar[<*>] i) => $$ctz(i);
macro ichar[<*>].clz(ichar[<*>] i) => $$clz(i);
macro ichar[<*>] ichar[<*>].fshl(ichar[<*>] hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshl(hi, lo, shift);
macro ichar[<*>] ichar[<*>].fshr(ichar[<*>] hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshr(hi, lo, shift);
macro ichar[<*>] ichar[<*>].rotl(ichar[<*>] i, ichar[<*>] shift) => $$fshl(i, i, shift);
macro ichar[<*>] ichar[<*>].rotr(ichar[<*>] i, ichar[<*>] shift) => $$fshr(i, i, shift);

macro ulong[<*>].popcount(ulong[<*>] i) => $$popcount(i);
macro ulong[<*>].ctz(ulong[<*>] i) => $$ctz(i);
macro ulong[<*>].clz(ulong[<*>] i) => $$clz(i);
macro ulong[<*>] ulong[<*>].fshl(ulong[<*>] hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshl(hi, lo, shift);
macro ulong[<*>] ulong[<*>].fshr(ulong[<*>] hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshr(hi, lo, shift);
macro ulong[<*>] ulong[<*>].rotl(ulong[<*>] i, ulong[<*>] shift) => $$fshl(i, i, shift);
macro ulong[<*>] ulong[<*>].rotr(ulong[<*>] i, ulong[<*>] shift) => $$fshr(i, i, shift);

macro long[<*>].popcount(long[<*>] i) => $$popcount(i);
macro long[<*>].ctz(long[<*>] i) => $$ctz(i);
macro long[<*>].clz(long[<*>] i) => $$clz(i);
macro long[<*>] long[<*>].fshl(long[<*>] hi, long[<*>] lo, long[<*>] shift) => $$fshl(hi, lo, shift);
macro long[<*>] long[<*>].fshr(long[<*>] hi, long[<*>] lo, long[<*>] shift) => $$fshr(hi, lo, shift);
macro long[<*>] long[<*>].rotl(long[<*>] i, long[<*>] shift) => $$fshl(i, i, shift);
macro long[<*>] long[<*>].rotr(long[<*>] i, long[<*>] shift) => $$fshr(i, i, shift);

macro uint128[<*>].popcount(uint128[<*>] i) => $$popcount(i);
macro uint128[<*>].ctz(uint128[<*>] i) => $$ctz(i);
macro uint128[<*>].clz(uint128[<*>] i) => $$clz(i);
macro uint128[<*>] uint128[<*>].fshl(uint128[<*>] hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshl(hi, lo, shift);
macro uint128[<*>] uint128[<*>].fshr(uint128[<*>] hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshr(hi, lo, shift);
macro uint128[<*>] uint128[<*>].rotl(uint128[<*>] i, uint128[<*>] shift) => $$fshl(i, i, shift);
macro uint128[<*>] uint128[<*>].rotr(uint128[<*>] i, uint128[<*>] shift) => $$fshr(i, i, shift);

macro int128[<*>].popcount(int128[<*>] i) => $$popcount(i);
macro int128[<*>].ctz(int128[<*>] i) => $$ctz(i);
macro int128[<*>].clz(int128[<*>] i) => $$clz(i);
macro int128[<*>] int128[<*>].fshl(int128[<*>] hi, int128[<*>] lo, int128[<*>] shift) => $$fshl(hi, lo, shift);
macro int128[<*>] int128[<*>].fshr(int128[<*>] hi, int128[<*>] lo, int128[<*>] shift) => $$fshr(hi, lo, shift);
macro int128[<*>] int128[<*>].rotl(int128[<*>] i, int128[<*>] shift) => $$fshl(i, i, shift);
macro int128[<*>] int128[<*>].rotr(int128[<*>] i, int128[<*>] shift) => $$fshr(i, i, shift);

macro uint.popcount(uint i) => $$popcount(i);
macro uint.ctz(uint i) => $$ctz(i);
macro uint.clz(uint i) => $$clz(i);
macro uint uint.fshl(uint hi, uint lo, uint shift) => $$fshl(hi, lo, shift);
macro uint uint.fshr(uint hi, uint lo, uint shift) => $$fshr(hi, lo, shift);
macro uint uint.rotl(uint i, uint shift) => $$fshl(i, i, shift);
macro uint uint.rotr(uint i, uint shift) => $$fshr(i, i, shift);

macro int.popcount(int i) => $$popcount(i);
macro int.ctz(int i) => $$ctz(i);
macro int.clz(int i) => $$clz(i);
macro int int.fshl(int hi, int lo, int shift) => $$fshl(hi, lo, shift);
macro int int.fshr(int hi, int lo, int shift) => $$fshr(hi, lo, shift);
macro int int.rotl(int i, int shift) => $$fshl(i, i, shift);
macro int int.rotr(int i, int shift) => $$fshr(i, i, shift);

macro ushort.popcount(ushort i) => $$popcount(i);
macro ushort.ctz(ushort i) => $$ctz(i);
macro ushort.clz(ushort i) => $$clz(i);
macro ushort ushort.fshl(ushort hi, ushort lo, ushort shift) => $$fshl(hi, lo, shift);
macro ushort ushort.fshr(ushort hi, ushort lo, ushort shift) => $$fshr(hi, lo, shift);
macro ushort ushort.rotl(ushort i, ushort shift) => $$fshl(i, i, shift);
macro ushort ushort.rotr(ushort i, ushort shift) => $$fshr(i, i, shift);

macro short.popcount(short i) => $$popcount(i);
macro short.ctz(short i) => $$ctz(i);
macro short.clz(short i) => $$clz(i);
macro short short.fshl(short hi, short lo, short shift) => $$fshl(hi, lo, shift);
macro short short.fshr(short hi, short lo, short shift) => $$fshr(hi, lo, shift);
macro short short.rotl(short i, short shift) => $$fshl(i, i, shift);
macro short short.rotr(short i, short shift) => $$fshr(i, i, shift);

macro char.popcount(char i) => $$popcount(i);
macro char.ctz(char i) => $$ctz(i);
macro char.clz(char i) => $$clz(i);
macro char char.fshl(char hi, char lo, char shift) => $$fshl(hi, lo, shift);
macro char char.fshr(char hi, char lo, char shift) => $$fshr(hi, lo, shift);
macro char char.rotl(char i, char shift) => $$fshl(i, i, shift);
macro char char.rotr(char i, char shift) => $$fshr(i, i, shift);

macro ichar.popcount(ichar i) => $$popcount(i);
macro ichar.ctz(ichar i) => $$ctz(i);
macro ichar.clz(ichar i) => $$clz(i);
macro ichar ichar.fshl(ichar hi, ichar lo, ichar shift) => $$fshl(hi, lo, shift);
macro ichar ichar.fshr(ichar hi, ichar lo, ichar shift) => $$fshr(hi, lo, shift);
macro ichar ichar.rotl(ichar i, ichar shift) => $$fshl(i, i, shift);
macro ichar ichar.rotr(ichar i, ichar shift) => $$fshr(i, i, shift);

macro ulong.popcount(ulong i) => $$popcount(i);
macro ulong.ctz(ulong i) => $$ctz(i);
macro ulong.clz(ulong i) => $$clz(i);
macro ulong ulong.fshl(ulong hi, ulong lo, ulong shift) => $$fshl(hi, lo, shift);
macro ulong ulong.fshr(ulong hi, ulong lo, ulong shift) => $$fshr(hi, lo, shift);
macro ulong ulong.rotl(ulong i, ulong shift) => $$fshl(i, i, shift);
macro ulong ulong.rotr(ulong i, ulong shift) => $$fshr(i, i, shift);

macro long.popcount(long i) => $$popcount(i);
macro long.ctz(long i) => $$ctz(i);
macro long.clz(long i) => $$clz(i);
macro long long.fshl(long hi, long lo, long shift) => $$fshl(hi, lo, shift);
macro long long.fshr(long hi, long lo, long shift) => $$fshr(hi, lo, shift);
macro long long.rotl(long i, long shift) => $$fshl(i, i, shift);
macro long long.rotr(long i, long shift) => $$fshr(i, i, shift);

macro uint128.popcount(uint128 i) => $$popcount(i);
macro uint128.ctz(uint128 i) => $$ctz(i);
macro uint128.clz(uint128 i) => $$clz(i);
macro uint128 uint128.fshl(uint128 hi, uint128 lo, uint128 shift) => $$fshl(hi, lo, shift);
macro uint128 uint128.fshr(uint128 hi, uint128 lo, uint128 shift) => $$fshr(hi, lo, shift);
macro uint128 uint128.rotl(uint128 i, uint128 shift) => $$fshl(i, i, shift);
macro uint128 uint128.rotr(uint128 i, uint128 shift) => $$fshr(i, i, shift);

macro int128.popcount(int128 i) => $$popcount(i);
macro int128.ctz(int128 i) => $$ctz(i);
macro int128.clz(int128 i) => $$clz(i);
macro int128 int128.fshl(int128 hi, int128 lo, int128 shift) => $$fshl(hi, lo, shift);
macro int128 int128.fshr(int128 hi, int128 lo, int128 shift) => $$fshr(hi, lo, shift);
macro int128 int128.rotl(int128 i, int128 shift) => $$fshl(i, i, shift);
macro int128 int128.rotr(int128 i, int128 shift) => $$fshr(i, i, shift);
