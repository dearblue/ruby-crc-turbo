#include <ruby.h>
#include <ruby/version.h>
#include <stdint.h>

enum {
    TYPE_MASK      =   0xff,
    TYPE_UINT8_T   =      1,
    TYPE_UINT16_T  =      2,
    TYPE_UINT32_T  =      4,
    TYPE_UINT64_T  =      8,
    TYPE_UINT128_T =     16,

    REFLECT_INPUT  = 0x0100,
    REFLECT_OUTPUT = 0x0200,

    TABLE_NOTREADY = 0x1000,
};

#define SWITCH_BY_TYPE(FLAGS, STMT_U8, STMT_U16, STMT_U32, STMT_U64, STMT_U128) \
    switch ((FLAGS) & TYPE_MASK) {                                              \
    case TYPE_UINT8_T:      { STMT_U8; break; }                                 \
    case TYPE_UINT16_T:     { STMT_U16; break; }                                \
    case TYPE_UINT32_T:     { STMT_U32; break; }                                \
    case TYPE_UINT64_T:     { STMT_U64; break; }                                \
 /* case TYPE_UINT128_T:    { STMT_U128; break; } */                            \
    default: { rb_bug(" [INVALID TYPE FLAGS: 0x%02X] ", (FLAGS) & TYPE_MASK); } \
    }                                                                           \

static inline uint8_t
to_uint8(VALUE num)
{
    unsigned long n;
    rb_big_pack(num, &n, 1);
    return n;
}

static inline VALUE
conv_uint8(uint8_t n)
{
    return INT2FIX(n);
}

static inline uint16_t
to_uint16(VALUE num)
{
    unsigned long n;
    rb_big_pack(num, &n, 1);
    return n;
}

static inline VALUE
conv_uint16(uint16_t n)
{
    return INT2FIX(n);
}

static inline uint32_t
to_uint32(VALUE num)
{
    unsigned long n;
    rb_big_pack(num, &n, 1);
    return n;
}

static inline VALUE
conv_uint32(uint32_t n)
{
    return UINT2NUM(n);
}

static inline uint64_t
to_uint64(VALUE num)
{
    if (sizeof(unsigned long) < 8) {
        static const int len = sizeof(uint64_t) / sizeof(unsigned long);
        unsigned long tmp[len];
        rb_big_pack(num, tmp, len);
        const unsigned long *p = tmp + len;
        uint64_t n = 0;
        while (p -- > tmp) {
            n <<= sizeof(unsigned long) * CHAR_BIT;
            n |= *p;
        }
        return n;
    } else {
        unsigned long n;
        rb_big_pack(num, &n, 1);
        return n;
    }
}

static inline VALUE
conv_uint64(uint64_t n)
{
    return ULL2NUM(n);
}

#ifdef HAVE_TYPE_UINT128_T
static inline uint128_t
to_uint128(VALUE num)
{
    if (sizeof(unsigned long) < 16) {
        static const int len = sizeof(uint128_t) / sizeof(unsigned long);
        unsigned long tmp[len];
        rb_big_pack(num, tmp, len);
        const unsigned long *p = tmp + len;
        uint128_t n = 0;
        while (p -- > tmp) {
            n <<= sizeof(unsigned long) * CHAR_BIT;
            n |= *p;
        }
        return n;
    } else {
        unsigned long n;
        rb_big_pack(num, &n, 1);
        return n;
    }
}

static inline VALUE
conv_uint128(uint128_t n)
{
    if (sizeof(unsigned long) >= 16) {
        return ULL2NUM(n);
    } else {
        static const int len = sizeof(uint128_t) / sizeof(unsigned long);
        unsigned long tmp[len];
        memset(tmp, 0, sizeof(tmp));
        unsigned long *p = tmp;
        while (n > 0) {
            *p ++ = n;
            n >>= sizeof(unsigned long) * CHAR_BIT;
        }
        return rb_big_unpack(tmp, len);
    }
}
#endif /* HAVE_TYPE_UINT128_T */

/*
 * bitreflect: 整数値のビットを逆順にする
 *
 * 高速化のためにビットをまとめて入れ替えていく (この例では8ビット整数値)
 *
 *        最初は4ビット    次は2ビット      最後に1ビット
 *  [abcdefgh] => [efgh abcd] => [gh ef] [cd ab] => [h g] [f e] [d c] [b a]
 */
static inline uint8_t
bitreflect8(uint8_t n)
{
    n =  (n >> 4)         | ( n         << 4);
    n = ((n >> 2) & 0x33) | ((n & 0x33) << 2);
    n = ((n >> 1) & 0x55) | ((n & 0x55) << 1);
    return n;
}

static inline uint16_t
bitreflect16(uint16_t n)
{
    n =  (n >> 8)           | ( n           << 8);
    n = ((n >> 4) & 0x0f0f) | ((n & 0x0f0f) << 4);
    n = ((n >> 2) & 0x3333) | ((n & 0x3333) << 2);
    n = ((n >> 1) & 0x5555) | ((n & 0x5555) << 1);
    return n;
}

static inline uint32_t
bitreflect32(uint32_t n)
{
    n =  (n >> 16)               | ( n               << 16);
    n = ((n >>  8) & 0x00ff00ff) | ((n & 0x00ff00ff) <<  8);
    n = ((n >>  4) & 0x0f0f0f0f) | ((n & 0x0f0f0f0f) <<  4);
    n = ((n >>  2) & 0x33333333) | ((n & 0x33333333) <<  2);
    n = ((n >>  1) & 0x55555555) | ((n & 0x55555555) <<  1);
    return n;
}

static inline uint64_t
bitreflect64(uint64_t n)
{
    n =  (n >> 32)                       | ( n                       << 32);
    n = ((n >> 16) & 0x0000ffff0000ffff) | ((n & 0x0000ffff0000ffff) << 16);
    n = ((n >>  8) & 0x00ff00ff00ff00ff) | ((n & 0x00ff00ff00ff00ff) <<  8);
    n = ((n >>  4) & 0x0f0f0f0f0f0f0f0f) | ((n & 0x0f0f0f0f0f0f0f0f) <<  4);
    n = ((n >>  2) & 0x3333333333333333) | ((n & 0x3333333333333333) <<  2);
    n = ((n >>  1) & 0x5555555555555555) | ((n & 0x5555555555555555) <<  1);
    return n;

#if 0
    /* 優位な差はない */
    n = ( n                       >> 32) | ( n                       << 32);
    n = ((n & 0xffff0000ffff0000) >> 16) | ((n & 0x0000ffff0000ffff) << 16);
    n = ((n & 0xff00ff00ff00ff00) >>  8) | ((n & 0x00ff00ff00ff00ff) <<  8);
    n = ((n & 0xf0f0f0f0f0f0f0f0) >>  4) | ((n & 0x0f0f0f0f0f0f0f0f) <<  4);
    n = ((n & 0xcccccccccccccccc) >>  2) | ((n & 0x3333333333333333) <<  2);
    n = ((n & 0xaaaaaaaaaaaaaaaa) >>  1) | ((n & 0x5555555555555555) <<  1);
    return n;

    /* 1割前後遅い */
    n = ((n >>  1) & 0x5555555555555555) | ((n & 0x5555555555555555) <<  1);
    n = ((n >>  2) & 0x3333333333333333) | ((n & 0x3333333333333333) <<  2);
    n = ((n >>  4) & 0x0f0f0f0f0f0f0f0f) | ((n & 0x0f0f0f0f0f0f0f0f) <<  4);
    n = ((n >>  8) & 0x00ff00ff00ff00ff) | ((n & 0x00ff00ff00ff00ff) <<  8);
    n = ((n >> 16) & 0x0000ffff0000ffff) | ((n & 0x0000ffff0000ffff) << 16);
    n =  (n >> 32)                       | ( n                       << 32);
    return n;
#endif
}

#ifdef HAVE_TYPE_UINT128_T
static inline uint128_t
bitreflect128(uint128_t n)
{
    static const uint128_t mask32 = ((uint128_t)0x00000000ffffffffull << 64) | 0x00000000ffffffffull;
    static const uint128_t mask16 = ((uint128_t)0x0000ffff0000ffffull << 64) | 0x0000ffff0000ffffull;
    static const uint128_t mask8  = ((uint128_t)0x00ff00ff00ff00ffull << 64) | 0x00ff00ff00ff00ffull;
    static const uint128_t mask4  = ((uint128_t)0x0f0f0f0f0f0f0f0full << 64) | 0x0f0f0f0f0f0f0f0full;
    static const uint128_t mask2  = ((uint128_t)0x3333333333333333ull << 64) | 0x3333333333333333ull;
    static const uint128_t mask1  = ((uint128_t)0x5555555555555555ull << 64) | 0x5555555555555555ull;

    n =  (n >> 64)           | ( n           << 64);
    n = ((n >> 32) & mask32) | ((n & mask32) << 32);
    n = ((n >> 16) & mask16) | ((n & mask16) << 16);
    n = ((n >>  8) &  mask8) | ((n &  mask8) <<  8);
    n = ((n >>  4) &  mask4) | ((n &  mask4) <<  4);
    n = ((n >>  2) &  mask2) | ((n &  mask2) <<  2);
    n = ((n >>  1) &  mask1) | ((n &  mask1) <<  1);
    return n;
}
#endif

static inline int
bitsize_to_type(int bitsize)
{
    if (bitsize <= 8) {
        return TYPE_UINT8_T;
    } else if (bitsize <= 16) {
        return TYPE_UINT16_T;
    } else if (bitsize <= 32) {
        return TYPE_UINT32_T;
#ifdef HAVE_TYPE_UINT128_T
    } else if (bitsize > 64) {
        return TYPE_UINT128_T;
#endif
    } else {
        return TYPE_UINT64_T;
    }
}


#define IMP_BUILD_TABLE(NAME, TYPE)                                         \
    static void                                                             \
    NAME(TYPE table[8][256], int bitsize, TYPE poly)                        \
    {                                                                       \
        static const int typesize = sizeof(TYPE) * CHAR_BIT;                \
        poly <<= (typesize - bitsize);                                      \
        TYPE (*t)[256] = table;                                             \
        int s, b, i;                                                        \
        for (s = 0; s < 8; s ++) {                                          \
            TYPE *p = *t;                                                   \
            for (b = 0; b < 256; b ++) {                                    \
                TYPE r = (s == 0) ? ((TYPE)b << (typesize - 8)) : t[-1][b]; \
                for (i = 0; i < 8; i ++) {                                  \
                    r = (r << 1) ^ (poly & -(r >> (typesize - 1)));         \
                }                                                           \
                *p ++ = r;                                                  \
            }                                                               \
            t ++;                                                           \
        }                                                                   \
    }                                                               

#define IMP_CRC_UPDATE(NAME, TYPE)                                                                        \
    static TYPE                                                                                           \
    NAME(int bitsize, const TYPE table[8][256],                                                           \
            const char *p, const char *pp, TYPE state)                                                    \
    {                                                                                                     \
        static const int typesize = sizeof(TYPE) * CHAR_BIT;                                              \
        const char *pp8 = ((pp - p) & ~0x07) + p;                                                         \
        state <<= typesize - bitsize;                                                                     \
        for (; p < pp8; p += 8) {                                                                         \
            state = table[7][(uint8_t)p[0] ^                  (uint8_t)(state >> (typesize -  8))     ] ^ \
                    table[6][(uint8_t)p[1] ^ (typesize >  8 ? (uint8_t)(state >> (typesize - 16)) : 0)] ^ \
                    table[5][(uint8_t)p[2] ^ (typesize > 16 ? (uint8_t)(state >> (typesize - 24)) : 0)] ^ \
                    table[4][(uint8_t)p[3] ^ (typesize > 24 ? (uint8_t)(state >> (typesize - 32)) : 0)] ^ \
                    table[3][(uint8_t)p[4] ^ (typesize > 32 ? (uint8_t)(state >> (typesize - 40)) : 0)] ^ \
                    table[2][(uint8_t)p[5] ^ (typesize > 40 ? (uint8_t)(state >> (typesize - 48)) : 0)] ^ \
                    table[1][(uint8_t)p[6] ^ (typesize > 48 ? (uint8_t)(state >> (typesize - 56)) : 0)] ^ \
                    table[0][(uint8_t)p[7] ^ (typesize > 56 ? (uint8_t)(state >> (typesize - 64)) : 0)];  \
        }                                                                                                 \
                                                                                                          \
        int sh = typesize - 8;                                                                            \
        for (; p < pp; p ++) {                                                                            \
            state = table[0][(uint8_t)*p ^ (uint8_t)(state >> sh)] ^ (state << 8);                        \
        }                                                                                                 \
                                                                                                          \
        return state >> (typesize - bitsize);                                                             \
    }                                                                                                     \

IMP_BUILD_TABLE(build_table8_u8, uint8_t);
IMP_CRC_UPDATE(crc_update_u8, uint8_t);
IMP_BUILD_TABLE(build_table8_u16, uint16_t);
IMP_CRC_UPDATE(crc_update_u16, uint16_t);
IMP_BUILD_TABLE(build_table8_u32, uint32_t);
IMP_CRC_UPDATE(crc_update_u32, uint32_t);
IMP_BUILD_TABLE(build_table8_u64, uint64_t);
IMP_CRC_UPDATE(crc_update_u64, uint64_t);
#ifdef HAVE_TYPE_UINT128_T
IMP_BUILD_TABLE(build_table8_u128, uint128_t);
IMP_CRC_UPDATE(crc_update_u128, uint128_t);
#endif

#undef IMP_BUILD_TABLE

#define IMP_BUILD_REFLECT_TABLE(NAME, TYPE, BITREFLECT)             \
    static void                                                     \
    NAME(TYPE table[8][256], int bitsize, TYPE poly)                \
    {                                                               \
        static const int typesize = sizeof(TYPE) * CHAR_BIT;        \
        poly = BITREFLECT(poly << (typesize - bitsize));            \
        TYPE (*t)[256] = table;                                     \
        int s, b, i;                                                \
        for (s = 0; s < 8; s ++) {                                  \
            TYPE *p = *t;                                           \
            for (b = 0; b < 256; b ++) {                            \
                TYPE r = (s == 0) ? (TYPE)b : t[-1][b];             \
                for (i = 0; i < 8; i ++) {                          \
                    r = (r >> 1) ^ (poly & -(r & 1));               \
                }                                                   \
                *p ++ = r;                                          \
            }                                                       \
            t ++;                                                   \
        }                                                           \
    }                                                               \

#define IMP_CRC_REFLECT_UPDATE(NAME, TYPE)                                                   \
    static TYPE                                                                              \
    NAME(int bitsize, const TYPE table[8][256],                                              \
            const char *p, const char *pp, TYPE state)                                       \
    {                                                                                        \
        static const int typesize = sizeof(TYPE) * CHAR_BIT;                                 \
        const char *pp8 = ((pp - p) & ~0x07) + p;                                            \
        for (; p < pp8; p += 8) {                                                            \
            state = table[7][(uint8_t)p[0] ^                  (uint8_t)(state >>  0)     ] ^ \
                    table[6][(uint8_t)p[1] ^ (typesize >  8 ? (uint8_t)(state >>  8) : 0)] ^ \
                    table[5][(uint8_t)p[2] ^ (typesize > 16 ? (uint8_t)(state >> 16) : 0)] ^ \
                    table[4][(uint8_t)p[3] ^ (typesize > 24 ? (uint8_t)(state >> 24) : 0)] ^ \
                    table[3][(uint8_t)p[4] ^ (typesize > 32 ? (uint8_t)(state >> 32) : 0)] ^ \
                    table[2][(uint8_t)p[5] ^ (typesize > 40 ? (uint8_t)(state >> 40) : 0)] ^ \
                    table[1][(uint8_t)p[6] ^ (typesize > 48 ? (uint8_t)(state >> 48) : 0)] ^ \
                    table[0][(uint8_t)p[7] ^ (typesize > 56 ? (uint8_t)(state >> 56) : 0)];  \
        }                                                                                    \
                                                                                             \
        for (; p < pp; p ++) {                                                               \
            state = table[0][(uint8_t)*p ^ (uint8_t)state] ^ (state >> 8);                   \
        }                                                                                    \
                                                                                             \
        return state;                                                                        \
    }                                                                                        \

IMP_BUILD_REFLECT_TABLE(build_reflect_table8_u8, uint8_t, bitreflect8);
IMP_CRC_REFLECT_UPDATE(crc_reflect_update_u8, uint8_t);
IMP_BUILD_REFLECT_TABLE(build_reflect_table8_u16, uint16_t, bitreflect16);
IMP_CRC_REFLECT_UPDATE(crc_reflect_update_u16, uint16_t);
IMP_BUILD_REFLECT_TABLE(build_reflect_table8_u32, uint32_t, bitreflect32);
IMP_CRC_REFLECT_UPDATE(crc_reflect_update_u32, uint32_t);
IMP_BUILD_REFLECT_TABLE(build_reflect_table8_u64, uint64_t, bitreflect64);
IMP_CRC_REFLECT_UPDATE(crc_reflect_update_u64, uint64_t);
#ifdef HAVE_TYPE_UINT128_T
IMP_BUILD_REFLECT_TABLE(build_reflect_table8_u128, uint128_t, bitreflect128);
IMP_CRC_REFLECT_UPDATE(crc_reflect_update_u128, uint128_t);
#endif

#undef IMP_BUILD_REFLECT_TABLE
#undef IMP_CRC_REFLECT_UPDATE

/*
 *
 * ここから ruby のターン
 *
 */

typedef struct anyuint_t
{
    union {
        uint8_t as8;
        uint16_t as16;
        uint32_t as32;
        uint64_t as64;
#ifdef HAVE_TYPE_UINT128_T
        uint128_t as128;
#endif
    };
} anyuint_t;

struct generator
{
    int bitsize;
    int flags; /* int type, refin, refout */
    anyuint_t bitmask, polynomial, initial, xorout;
    union {
        uint8_t as8[8][256];
        uint16_t as16[8][256];
        uint32_t as32[8][256];
        uint64_t as64[8][256];
#ifdef HAVE_TYPE_UINT128_T
        uint128_t as128[8][256];
#endif
    } table[0];
};

static VALUE mCRC;          /* module CRC */
static VALUE mUtils;        /* module CRC::Utils */
static VALUE cGenerator;    /* class CRC::Generator */
static ID generator_iv_name;

static const rb_data_type_t generator_type = {
    .wrap_struct_name = "crc-turbo.CRC::Generator",
    .function.dmark = NULL,
    .function.dsize = NULL,
    .function.dfree = (void *)-1,
};

static void
check_generator_notinit(VALUE obj)
{
    struct generator *p;
    TypedData_Get_Struct(obj, struct generator, &generator_type, p);
    if (p) { rb_raise(rb_eArgError, "already initialized object - #<%s:0x%p>", rb_obj_classname(obj), (void *)obj); }
}

static struct generator *
get_generator(VALUE obj)
{
    struct generator *p;
    TypedData_Get_Struct(obj, struct generator, &generator_type, p);
    if (!p) { rb_raise(rb_eArgError, "wrong initialized object - #<%s:0x%p>", rb_obj_classname(obj), (void *)obj); }
    return p;
}

static VALUE
generator_alloc(VALUE mod)
{
    return TypedData_Wrap_Struct(mod, &generator_type, NULL);
}

static void
generator_init_args(int argc, VALUE argv[], int *flags, int *bitsize, VALUE *poly, VALUE *init, VALUE *xorout, VALUE *name)
{
    rb_check_arity(argc, 2, 7);
    *bitsize = NUM2INT(argv[0]);
    if (*bitsize < 1 || *bitsize > 64) {
        rb_raise(rb_eArgError, "wrong bitsize (expect 1..64, but given %d)", *bitsize);
    } else if (*bitsize <= 8) {
        *flags = TYPE_UINT8_T;
    } else if (*bitsize <= 16) {
        *flags = TYPE_UINT16_T;
    } else if (*bitsize <= 32) {
        *flags = TYPE_UINT32_T;
    } else if (*bitsize <= 64) {
        *flags = TYPE_UINT64_T;
    }

    *poly = argv[1];
    *init = (argc > 2) ? argv[2] : INT2FIX(0);
    *flags |= (argc > 3 && !RTEST(argv[3])) ? 0 : REFLECT_INPUT;
    *flags |= (argc > 4 && !RTEST(argv[4])) ? 0 : REFLECT_OUTPUT;
    *xorout = (argc > 5) ? argv[5] : INT2FIX(~0);
    *name = (argc > 6 && !NIL_P(argv[6])) ? rb_String(argv[6]) : Qnil;
}

/*
 * call-seq:
 *  initialize(bitsize, polynomial, initial_state = 0, reflect_input = true, reflect_output = true, xor_output = ~0, name = nil)
 */
static VALUE
generator_init(int argc, VALUE argv[], VALUE obj)
{
    int flags, bitsize;
    VALUE poly, init, xorout, name;
    check_generator_notinit(obj);
    generator_init_args(argc, argv, &flags, &bitsize, &poly, &init, &xorout, &name);

    struct generator *p;
    size_t allocsize = sizeof(struct generator) + (flags & TYPE_MASK) * 256 * 8;
    RTYPEDDATA_DATA(obj) = p = (struct generator *)ALLOC_N(char, allocsize);
    p->bitsize = bitsize;
    p->flags = flags | TABLE_NOTREADY;

    /*
     * bitmask の代入でわざわざ1ビット分を後から行う理由は、
     * 例えば uint8_t に対して << 8 をすると何もしないため、
     * これへの対処を目的とする。
     */
#define INIT_GENERATOR(TYPE, AS, TO_UINT, CONV_UINT, P, BITSIZE, POLY, INIT, XOROUT)   \
    P->bitmask.AS = ~(~(TYPE)0 << 1 << (BITSIZE - 1));                              \
    P->polynomial.AS = P->bitmask.AS & TO_UINT(POLY);                               \
    P->initial.AS = P->bitmask.AS & TO_UINT(INIT);                                  \
    P->xorout.AS = P->bitmask.AS & TO_UINT(XOROUT);                                 \

    SWITCH_BY_TYPE(flags,
            INIT_GENERATOR(uint8_t, as8, to_uint8, conv_uint8, p, bitsize, poly, init, xorout),
            INIT_GENERATOR(uint16_t, as16, to_uint16, conv_uint16, p, bitsize, poly, init, xorout),
            INIT_GENERATOR(uint32_t, as32, to_uint32, conv_uint32, p, bitsize, poly, init, xorout),
            INIT_GENERATOR(uint64_t, as64, to_uint64, conv_uint64, p, bitsize, poly, init, xorout),
            INIT_GENERATOR(uint128_t, as128, to_uint128, conv_uint128, p, bitsize, poly, init, xorout));

#undef INIT_GENERATOR

    rb_ivar_set(obj, generator_iv_name, name);

    return obj;
}

static VALUE
generator_bitsize(VALUE t)
{
    return INT2FIX(get_generator(t)->bitsize);
}

static VALUE
generator_bitmask(VALUE t)
{
    struct generator *p = get_generator(t);
    SWITCH_BY_TYPE(p->flags,
            return conv_uint8(p->bitmask.as8),
            return conv_uint16(p->bitmask.as16),
            return conv_uint32(p->bitmask.as32),
            return conv_uint64(p->bitmask.as64),
            return conv_uint128(p->bitmask.as128));
}

static VALUE
generator_polynomial(VALUE t)
{
    struct generator *p = get_generator(t);
    SWITCH_BY_TYPE(p->flags,
            return conv_uint8(p->bitmask.as8),
            return conv_uint16(p->bitmask.as16),
            return conv_uint32(p->bitmask.as32),
            return conv_uint64(p->bitmask.as64),
            return conv_uint128(p->bitmask.as128));
}

static VALUE
generator_initial_state(VALUE t)
{
    struct generator *p = get_generator(t);
    SWITCH_BY_TYPE(p->flags,
            return conv_uint8(p->initial.as8),
            return conv_uint16(p->initial.as16),
            return conv_uint32(p->initial.as32),
            return conv_uint64(p->initial.as64),
            return conv_uint128(p->initial.as128));
}

static VALUE
generator_table8(VALUE t)
{
    struct generator *p = get_generator(t);
    rb_raise(rb_eNotImpError, "");
}

static VALUE
generator_reflect_input(VALUE t)
{
    struct generator *p = get_generator(t);
    return (p->flags & REFLECT_INPUT) ? Qtrue : Qfalse;
}

static VALUE
generator_reflect_output(VALUE t)
{
    struct generator *p = get_generator(t);
    return (p->flags & REFLECT_OUTPUT) ? Qtrue : Qfalse;
}

static VALUE
generator_xor_output(VALUE t)
{
    struct generator *p = get_generator(t);
    SWITCH_BY_TYPE(p->flags,
            return conv_uint8(p->xorout.as8),
            return conv_uint16(p->xorout.as16),
            return conv_uint32(p->xorout.as32),
            return conv_uint64(p->xorout.as64),
            return conv_uint128(p->xorout.as128));
}

static VALUE
generator_name(VALUE t)
{
    // get_generator で初期化の確認
    get_generator(t);

    return rb_ivar_get(t, generator_iv_name);
}

static VALUE
generator_set_name(VALUE t, VALUE name)
{
    // get_generator で初期化の確認
    get_generator(t);

    rb_ivar_set(t, generator_iv_name, rb_String(name));
    return name;
}

static VALUE
generator_update(VALUE t, VALUE seq, VALUE state)
{
    struct generator *p = get_generator(t);

    if (p->flags & TABLE_NOTREADY) {
        if (p->flags & REFLECT_INPUT) {
            SWITCH_BY_TYPE(p->flags,
                    build_reflect_table8_u8(p->table[0].as8, p->bitsize, p->polynomial.as8),
                    build_reflect_table8_u16(p->table[0].as16, p->bitsize, p->polynomial.as16),
                    build_reflect_table8_u32(p->table[0].as32, p->bitsize, p->polynomial.as32),
                    build_reflect_table8_u64(p->table[0].as64, p->bitsize, p->polynomial.as64),
                    build_reflect_table8_u128(p->table[0].as128, p->bitsize, p->polynomial.as128));
        } else {
            SWITCH_BY_TYPE(p->flags,
                    build_table8_u8(p->table[0].as8, p->bitsize, p->polynomial.as8),
                    build_table8_u16(p->table[0].as16, p->bitsize, p->polynomial.as16),
                    build_table8_u32(p->table[0].as32, p->bitsize, p->polynomial.as32),
                    build_table8_u64(p->table[0].as64, p->bitsize, p->polynomial.as64),
                    build_table8_u128(p->table[0].as128, p->bitsize, p->polynomial.as128));
        }
        p->flags ^= TABLE_NOTREADY;
    }

    rb_check_type(seq, RUBY_T_STRING);
    const char *q = RSTRING_PTR(seq);
    const char *qq = q + RSTRING_LEN(seq);
    if (p->flags & REFLECT_INPUT) {
        SWITCH_BY_TYPE(p->flags,
                return conv_uint8(crc_reflect_update_u8(p->bitsize, p->table[0].as8, q, qq, to_uint8(state))),
                return conv_uint16(crc_reflect_update_u16(p->bitsize, p->table[0].as16, q, qq, to_uint16(state))),
                return conv_uint32(crc_reflect_update_u32(p->bitsize, p->table[0].as32, q, qq, to_uint32(state))),
                return conv_uint64(crc_reflect_update_u64(p->bitsize, p->table[0].as64, q, qq, to_uint64(state))),
                return conv_uint128(crc_reflect_update_u128(p->bitsize, p->table[0].as128, q, qq, to_uint128(state))));
    } else {
        SWITCH_BY_TYPE(p->flags,
                return conv_uint8(crc_update_u8(p->bitsize, p->table[0].as8, q, qq, to_uint8(state))),
                return conv_uint16(crc_update_u16(p->bitsize, p->table[0].as16, q, qq, to_uint16(state))),
                return conv_uint32(crc_update_u32(p->bitsize, p->table[0].as32, q, qq, to_uint32(state))),
                return conv_uint64(crc_update_u64(p->bitsize, p->table[0].as64, q, qq, to_uint64(state))),
                return conv_uint128(crc_update_u128(p->bitsize, p->table[0].as128, q, qq, to_uint128(state))));
    }
}

/*
 * module CRC::Utils
 */

static VALUE
utils_s_bitref8(VALUE mod, VALUE num)
{
    return conv_uint8(bitreflect8(to_uint8(num)));
}

static VALUE
utils_s_bitref16(VALUE mod, VALUE num)
{
    return conv_uint16(bitreflect16(to_uint16(num)));
}

static VALUE
utils_s_bitref32(VALUE mod, VALUE num)
{
    return conv_uint32(bitreflect32(to_uint32(num)));
}

static VALUE
utils_s_bitref64(VALUE mod, VALUE num)
{
    return conv_uint64(bitreflect64(to_uint64(num)));
}

static VALUE
utils_s_bitref128(VALUE mod, VALUE num)
{
#ifdef HAVE_TYPE_UINT128_T
    return conv_uint128(bitreflect128(to_uint128(num)));
#endif
    rb_raise(rb_eNotImpError, "");
}

/*
 * library crc/_turbo.so
 */

void
Init__turbo(void)
{
    generator_iv_name = rb_intern("crc-turbo.CRC::Generator.name");

    mCRC = rb_define_module("CRC");

    mUtils = rb_define_module_under(mCRC, "Utils");
    rb_define_method(mUtils, "bitreflect8", utils_s_bitref8, 1);
    rb_define_method(mUtils, "bitreflect16", utils_s_bitref16, 1);
    rb_define_method(mUtils, "bitreflect32", utils_s_bitref32, 1);
    rb_define_method(mUtils, "bitreflect64", utils_s_bitref64, 1);
    rb_define_method(mUtils, "bitreflect128", utils_s_bitref128, 1);

    cGenerator = rb_define_class_under(mCRC, "Generator", rb_cObject);
    rb_define_alloc_func(cGenerator, generator_alloc);
    rb_define_method(cGenerator, "initialize", generator_init, -1);
    rb_define_method(cGenerator, "bitsize", generator_bitsize, 0);
    rb_define_method(cGenerator, "bitmask", generator_bitmask, 0);
    rb_define_method(cGenerator, "polynomial", generator_polynomial, 0);
    rb_define_method(cGenerator, "initial_state", generator_initial_state, 0);
    rb_define_method(cGenerator, "table8", generator_table8, 0);
    rb_define_method(cGenerator, "reflect_input", generator_reflect_input, 0);
    rb_define_method(cGenerator, "reflect_output", generator_reflect_output, 0);
    rb_define_method(cGenerator, "xor_output", generator_xor_output, 0);
    rb_define_method(cGenerator, "name", generator_name, 0);
    rb_define_method(cGenerator, "name=", generator_set_name, 1);
    rb_define_method(cGenerator, "update", generator_update, 2);
}
