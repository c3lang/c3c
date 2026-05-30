#define MINIZ_NO_ZLIB_COMPATIBLE_NAMES
#include "miniz.h"

const char *zlibVersion(void)
{
    return mz_version();
}

int inflateInit_(mz_streamp strm, const char *version, int stream_size)
{
    (void)version;
    (void)stream_size;
    return mz_inflateInit(strm);
}

int inflateInit2_(mz_streamp strm, int windowBits, const char *version, int stream_size)
{
    (void)version;
    (void)stream_size;
    return mz_inflateInit2(strm, windowBits);
}

int inflate(mz_streamp strm, int flush)
{
    return mz_inflate(strm, flush);
}

int inflateEnd(mz_streamp strm)
{
    return mz_inflateEnd(strm);
}

int inflateReset(mz_streamp strm)
{
    return mz_inflateReset(strm);
}

int inflateReset2(mz_streamp strm, int windowBits)
{
    (void)windowBits;
    return mz_inflateReset(strm);
}

int deflateInit_(mz_streamp strm, int level, const char *version, int stream_size)
{
    (void)version;
    (void)stream_size;
    return mz_deflateInit(strm, level);
}

int deflateInit2_(mz_streamp strm, int level, int method, int windowBits, int memLevel, int strategy,
                  const char *version, int stream_size)
{
    (void)version;
    (void)stream_size;
    return mz_deflateInit2(strm, level, method, windowBits, memLevel, strategy);
}

int deflate(mz_streamp strm, int flush)
{
    return mz_deflate(strm, flush);
}

int deflateEnd(mz_streamp strm)
{
    return mz_deflateEnd(strm);
}

int deflateReset(mz_streamp strm)
{
    return mz_deflateReset(strm);
}

unsigned long deflateBound(mz_streamp strm, unsigned long sourceLen)
{
    return mz_deflateBound(strm, sourceLen);
}

int compress(unsigned char *dest, unsigned long *destLen, const unsigned char *source, unsigned long sourceLen)
{
    return mz_compress(dest, destLen, source, sourceLen);
}

int compress2(unsigned char *dest, unsigned long *destLen, const unsigned char *source, unsigned long sourceLen,
              int level)
{
    return mz_compress2(dest, destLen, source, sourceLen, level);
}

unsigned long compressBound(unsigned long sourceLen)
{
    return mz_compressBound(sourceLen);
}

int uncompress(unsigned char *dest, unsigned long *destLen, const unsigned char *source, unsigned long sourceLen)
{
    return mz_uncompress(dest, destLen, source, sourceLen);
}

unsigned long crc32(unsigned long crc, const unsigned char *buf, size_t len)
{
    return mz_crc32(crc, buf, len);
}

unsigned long adler32(unsigned long adler, const unsigned char *buf, size_t len)
{
    return mz_adler32(adler, buf, len);
}

// https://github.com/madler/zlib/blob/master/adler32.c
#define BASE 65521U /* largest prime smaller than 65536 */
static unsigned long adler32_combine_(unsigned long adler1, unsigned long adler2, long long len2)
{
    unsigned long sum1;
    unsigned long sum2;
    unsigned int rem;

    /* for negative len, return invalid adler32 as a clue for debugging */
    if (len2 < 0)
        return 0xffffffffUL;

    rem = (unsigned int)(len2 % BASE);

    sum1 = adler1 & 0xffff;
    sum2 = (rem * sum1) % BASE;
    sum1 += (adler2 & 0xffff) + BASE - 1;
    sum2 += ((adler1 >> 16) & 0xffff) + ((adler2 >> 16) & 0xffff) + BASE - rem;
    if (sum1 >= BASE)
        sum1 -= BASE;
    if (sum1 >= BASE)
        sum1 -= BASE;
    if (sum2 >= ((unsigned long)BASE << 1))
        sum2 -= ((unsigned long)BASE << 1);
    if (sum2 >= BASE)
        sum2 -= BASE;
    return sum1 | (sum2 << 16);
}

unsigned long adler32_combine(unsigned long adler1, unsigned long adler2, long len2)
{
    return adler32_combine_(adler1, adler2, len2);
}

unsigned long adler32_combine64(unsigned long adler1, unsigned long adler2, long long len2)
{
    return adler32_combine_(adler1, adler2, len2);
}
