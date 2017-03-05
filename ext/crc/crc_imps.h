/*
 * author:: dearblue <dearblue@users.noreply.github.com>
 * license:: Creative Commons License Zero (CC0 / Public Domain)
 *
 * This is a general CRC calcurator.
 *
 * It's used slice-by-16 algorithm with byte-order free.
 * This is based on the Intel's slice-by-eight algorithm.
 *
 * Worst point is need more memory!
 *
 * reference:
 * * https://sourceforge.net/projects/slicing-by-8/
 * * xz-utils
 *      * http://tukaani.org/xz/
 *      * xz-5.2.2/src/liblzma/check/crc32_fast.c
 *      * xz-5.2.2/src/liblzma/check/crc32_tablegen.c
 */

#if     !defined(CRC_TYPE) || \
        !defined(CRC_BITREFLECT) || \
        !defined(CRC_BUILD_TABLES) || \
        !defined(CRC_UPDATE) || \
        !defined(CRC_BUILD_REFLECT_TABLES) || \
        !defined(CRC_REFLECT_UPDATE)
#   error not defined CRC_TYPE, CRC_BITREFLECT, CRC_BUILD_TABLES, CRC_UPDATE, CRC_BUILD_REFLECT_TABLES or CRC_REFLECT_UPDATE before include this file
#endif

#ifndef CRC_INLINE
#   define CRC_INLINE
#endif

/*
 * [bitsize]
 *      CRC bit-size. 32 for CRC-32, 7 for CRC-7, and so on.
 *
 * [table]
 *      lookup table. total byte size is sizeof(CRC_TYPE[slicesize][256]).
 *
 *      16384 for CRC-32:slicesize=16.
 *
 * [poly]
 *      polynomial. not reflected.
 *
 * [slicesize]
 *      table slice size.
 *
 *      1 for standard lookup-table, 4 for slice-by-four, 16 for slice-by-16, and so on.
 */
static CRC_INLINE void
CRC_BUILD_TABLES(int bitsize, CRC_TYPE table[][256], CRC_TYPE poly, int slicesize)
{
    static const int typebits = sizeof(CRC_TYPE) * CHAR_BIT;
    CRC_TYPE (*t)[256] = table;
    int s, b, i;
    poly <<= (typebits - bitsize);
    for (s = 0; s < slicesize; s ++) {
        CRC_TYPE *p = *t;
        for (b = 0; b < 256; b ++) {
            CRC_TYPE r = (s == 0) ? ((CRC_TYPE)b << (typebits - 8)) : t[-1][b];
            for (i = 0; i < 8; i ++) {
                r = (r << 1) ^ (poly & -(r >> (typebits - 1)));
            }
            *p ++ = r;
        }
        t ++;
    }
}

/*
 * [bitsize]
 *      CRC bit-size. 32 for CRC-32, 7 for CRC-7, and so on.
 *
 * [table]
 *      lookup table. total byte size is sizeof(CRC_TYPE[slicesize][256]).
 *
 *      16384 for CRC-32:slicesize=16.
 *
 * [poly]
 *      polynomial. not reflected.
 *
 * [slicesize]
 *      table slice size.
 *
 *      1 for standard lookup-table, 4 for slice-by-four, 16 for slice-by-16, and so on.
 */
static CRC_INLINE void
CRC_BUILD_REFLECT_TABLES(int bitsize, CRC_TYPE table[][256], CRC_TYPE poly, int slicesize)
{
    static const int typebits = sizeof(CRC_TYPE) * CHAR_BIT;
    CRC_TYPE (*t)[256] = table;
    poly = CRC_BITREFLECT(poly << (typebits - bitsize));
    int s, b;
    for (s = 0; s < slicesize; s ++) {
        CRC_TYPE *p = *t;
        for (b = 0; b < 256; b ++) {
            int i;
            CRC_TYPE r = (s == 0) ? b : t[-1][b];
            for (i = 0; i < 8; i ++) {
                r = (r >> 1) ^ (poly & -(r & 1));
            }
            *p ++ = r;
        }
        t ++;
    }
}

static CRC_TYPE
CRC_UPDATE(int bitsize, const CRC_TYPE table[16][256],
        const char *p, const char *pp, CRC_TYPE state)
{
    static const int typebits = sizeof(CRC_TYPE) * CHAR_BIT;
    const char *pp16 = ((pp - p) & ~0x0f) + p;
    state <<= typebits - bitsize;
    for (; p < pp16; p += 16) {
        state = table[15][(uint8_t)p[ 0] ^                   (uint8_t)(state >> (typebits -   8))     ] ^
                table[14][(uint8_t)p[ 1] ^ (typebits >   8 ? (uint8_t)(state >> (typebits -  16)) : 0)] ^
                table[13][(uint8_t)p[ 2] ^ (typebits >  16 ? (uint8_t)(state >> (typebits -  24)) : 0)] ^
                table[12][(uint8_t)p[ 3] ^ (typebits >  24 ? (uint8_t)(state >> (typebits -  32)) : 0)] ^
                table[11][(uint8_t)p[ 4] ^ (typebits >  32 ? (uint8_t)(state >> (typebits -  40)) : 0)] ^
                table[10][(uint8_t)p[ 5] ^ (typebits >  40 ? (uint8_t)(state >> (typebits -  48)) : 0)] ^
                table[ 9][(uint8_t)p[ 6] ^ (typebits >  48 ? (uint8_t)(state >> (typebits -  56)) : 0)] ^
                table[ 8][(uint8_t)p[ 7] ^ (typebits >  56 ? (uint8_t)(state >> (typebits -  64)) : 0)] ^
                table[ 7][(uint8_t)p[ 8] ^ (typebits >  64 ? (uint8_t)(state >> (typebits -  72)) : 0)] ^
                table[ 6][(uint8_t)p[ 9] ^ (typebits >  72 ? (uint8_t)(state >> (typebits -  80)) : 0)] ^
                table[ 5][(uint8_t)p[10] ^ (typebits >  80 ? (uint8_t)(state >> (typebits -  88)) : 0)] ^
                table[ 4][(uint8_t)p[11] ^ (typebits >  88 ? (uint8_t)(state >> (typebits -  96)) : 0)] ^
                table[ 3][(uint8_t)p[12] ^ (typebits >  96 ? (uint8_t)(state >> (typebits - 104)) : 0)] ^
                table[ 2][(uint8_t)p[13] ^ (typebits > 104 ? (uint8_t)(state >> (typebits - 112)) : 0)] ^
                table[ 1][(uint8_t)p[14] ^ (typebits > 112 ? (uint8_t)(state >> (typebits - 120)) : 0)] ^
                table[ 0][(uint8_t)p[15] ^ (typebits > 120 ? (uint8_t)(state >> (typebits - 128)) : 0)];
    }

    static const int sh = typebits - 8;
    for (; p < pp; p ++) {
        state = table[0][(uint8_t)*p ^ (uint8_t)(state >> sh)] ^ (state << 8);
    }

    return state >> (typebits - bitsize);
}

/*
 * [bitsize]
 *      CRC bit-size. 32 for CRC-32, 7 for CRC-7, and so on.
 *
 * [table]
 *      lookup table.
 *
 * [p]
 *      input sequence.
 *
 * [pp]
 *      end of input sequence.
 *
 *      example, p + len(p).
 *
 * [state]
 *      internal state.
 *
 *      example, state ^ xor_output.
 */
static CRC_INLINE CRC_TYPE
CRC_REFLECT_UPDATE(int bitsize, const CRC_TYPE table[16][256],
        const char *p, const char *pp, CRC_TYPE state)
{
    static const int typebits = sizeof(CRC_TYPE) * CHAR_BIT;
    const char *pp16 = ((pp - p) & ~0x0f) + p;
    for (; p < pp16; p += 16) {
        state = table[15][(uint8_t)p[ 0] ^                   (uint8_t)(state >>   0)     ] ^
                table[14][(uint8_t)p[ 1] ^ (typebits >   8 ? (uint8_t)(state >>   8) : 0)] ^
                table[13][(uint8_t)p[ 2] ^ (typebits >  16 ? (uint8_t)(state >>  16) : 0)] ^
                table[12][(uint8_t)p[ 3] ^ (typebits >  24 ? (uint8_t)(state >>  24) : 0)] ^
                table[11][(uint8_t)p[ 4] ^ (typebits >  32 ? (uint8_t)(state >>  32) : 0)] ^
                table[10][(uint8_t)p[ 5] ^ (typebits >  40 ? (uint8_t)(state >>  40) : 0)] ^
                table[ 9][(uint8_t)p[ 6] ^ (typebits >  48 ? (uint8_t)(state >>  48) : 0)] ^
                table[ 8][(uint8_t)p[ 7] ^ (typebits >  56 ? (uint8_t)(state >>  56) : 0)] ^
                table[ 7][(uint8_t)p[ 8] ^ (typebits >  64 ? (uint8_t)(state >>  64) : 0)] ^
                table[ 6][(uint8_t)p[ 9] ^ (typebits >  72 ? (uint8_t)(state >>  72) : 0)] ^
                table[ 5][(uint8_t)p[10] ^ (typebits >  80 ? (uint8_t)(state >>  80) : 0)] ^
                table[ 4][(uint8_t)p[11] ^ (typebits >  88 ? (uint8_t)(state >>  88) : 0)] ^
                table[ 3][(uint8_t)p[12] ^ (typebits >  96 ? (uint8_t)(state >>  96) : 0)] ^
                table[ 2][(uint8_t)p[13] ^ (typebits > 104 ? (uint8_t)(state >> 104) : 0)] ^
                table[ 1][(uint8_t)p[14] ^ (typebits > 112 ? (uint8_t)(state >> 112) : 0)] ^
                table[ 0][(uint8_t)p[15] ^ (typebits > 120 ? (uint8_t)(state >> 120) : 0)];
    }

    for (; p < pp; p ++) {
        state = table[0][(uint8_t)*p ^ (uint8_t)state] ^ (state >> 8);
    }

    return state;
}

#ifdef CRC_REFLECT_UPDATE_WITH_BITWISE

/*
 * no lookup tables with bitwise.
 *
 * reference::
 *      http://www.hackersdelight.org/hdcodetxt/crc.c.txt#crc32b
 */
static CRC_INLINE CRC_TYPE
CRC_REFLECT_UPDATE_WITH_BITWISE(const int bitsize, CRC_TYPE poly,
        const char *p, const char *pp, CRC_TYPE state)
{
    poly = CRC_BITREFLECT(poly) >> (32 - bitsize);

    for (; p < pp; p ++) {
        state ^= (uint8_t)*p;
        int i;
        for (i = 0; i < 8; i ++) {
            state = (state >> 1) ^ (poly & -(state & 1));
        }
    }

    return state;
}

#undef CRC_REFLECT_UPDATE_WITH_BITWISE
#endif

#ifdef CRC_REFLECT_UPDATE_WITH_BITPACK

/*
 * loopless for each bits.
 *
 * reference::
 *      http://www.hackersdelight.org/hdcodetxt/crc.c.txt#crc32h
 */
static CRC_INLINE CRC_TYPE
CRC_REFLECT_UPDATE_WITH_BITPACK(const int bitsize, const CRC_TYPE poly,
        const char *p, const char *pp, CRC_TYPE state)
{
    const CRC_TYPE g0 = CRC_BITREFLECT(poly) >> (32 - bitsize),
                   g1 = (g0 >> 1) ^ (g0 & -(g0 & 1)),
                   g2 = (g1 >> 1) ^ (g0 & -(g1 & 1)),
                   g3 = (g2 >> 1) ^ (g0 & -(g2 & 1)),
                   g4 = (g3 >> 1) ^ (g0 & -(g3 & 1)),
                   g5 = (g4 >> 1) ^ (g0 & -(g4 & 1)),
                   g6 = (g5 >> 1) ^ (g0 & -(g5 & 1)),
                   g7 = (g6 >> 1) ^ (g0 & -(g6 & 1));

    for (; p < pp; p ++) {
        const uint8_t s1 = state ^ *p;
        state = (g7 & -(CRC_TYPE)((s1 >> 0) & 1)) ^ (g6 & -(CRC_TYPE)((s1 >> 1) & 1)) ^
                (g5 & -(CRC_TYPE)((s1 >> 2) & 1)) ^ (g4 & -(CRC_TYPE)((s1 >> 3) & 1)) ^
                (g3 & -(CRC_TYPE)((s1 >> 4) & 1)) ^ (g2 & -(CRC_TYPE)((s1 >> 5) & 1)) ^
                (g1 & -(CRC_TYPE)((s1 >> 6) & 1)) ^ (g0 & -(CRC_TYPE)((s1 >> 7) & 1)) ^
                (state >> 8);
    }

    return state;
}

#undef CRC_REFLECT_UPDATE_WITH_BITPACK
#endif

#undef CRC_TYPE
#undef CRC_BITREFLECT
#undef CRC_BUILD_TABLES
#undef CRC_UPDATE
#undef CRC_BUILD_REFLECT_TABLES
#undef CRC_REFLECT_UPDATE
#undef CRC_INLINE
