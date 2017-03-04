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

//#define SNNIPET(BITSIZE, AS, TYPE, TOUINT, CONVUINT)
#define SWITCH_BY_TYPE(TYPE, SNNIPET)                                                            \
    switch ((TYPE)) {                                                                            \
    case TYPE_UINT8_T:   { SNNIPET(  8,   as8,   uint8_t,   to_uint8,   conv_uint8); break; }    \
    case TYPE_UINT16_T:  { SNNIPET( 16,  as16,  uint16_t,  to_uint16,  conv_uint16); break; }    \
    case TYPE_UINT32_T:  { SNNIPET( 32,  as32,  uint32_t,  to_uint32,  conv_uint32); break; }    \
    case TYPE_UINT64_T:  { SNNIPET( 64,  as64,  uint64_t,  to_uint64,  conv_uint64); break; }    \
 /* case TYPE_UINT128_T: { SNNIPET(128, as128, uint128_t, to_uint128, conv_uint128); break; } */ \
    default: { rb_bug(" [INVALID TYPE FLAGS: 0x%02X] ", (TYPE)); }                               \
    }                                                                                            \

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

static inline unsigned long
bitreflect_ulong(unsigned long n)
{
    if (sizeof(unsigned long) == sizeof(uint64_t)) {
        return bitreflect64(n);
    } else if (sizeof(unsigned long) == sizeof(uint32_t)) {
        return bitreflect32(n);
    } else {
        // FIXME
        return ~0;
    }
}

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

#define CRC_TYPE                    uint8_t
#define CRC_BITREFLECT              bitreflect8
#define CRC_BUILD_TABLES            crc_build_tables_u8
#define CRC_UPDATE                  crc_update_u8
#define CRC_BUILD_REFLECT_TABLES    crc_build_reflect_tables_u8
#define CRC_REFLECT_UPDATE          crc_reflect_update_u8
#include "crc_imps.h"

#define CRC_TYPE                    uint16_t
#define CRC_BITREFLECT              bitreflect16
#define CRC_BUILD_TABLES            crc_build_tables_u16
#define CRC_UPDATE                  crc_update_u16
#define CRC_BUILD_REFLECT_TABLES    crc_build_reflect_tables_u16
#define CRC_REFLECT_UPDATE          crc_reflect_update_u16
#include "crc_imps.h"

#define CRC_TYPE                    uint32_t
#define CRC_BITREFLECT              bitreflect32
#define CRC_BUILD_TABLES            crc_build_tables_u32
#define CRC_UPDATE                  crc_update_u32
#define CRC_BUILD_REFLECT_TABLES    crc_build_reflect_tables_u32
#define CRC_REFLECT_UPDATE          crc_reflect_update_u32
#include "crc_imps.h"

#define CRC_TYPE                    uint64_t
#define CRC_BITREFLECT              bitreflect64
#define CRC_BUILD_TABLES            crc_build_tables_u64
#define CRC_UPDATE                  crc_update_u64
#define CRC_BUILD_REFLECT_TABLES    crc_build_reflect_tables_u64
#define CRC_REFLECT_UPDATE          crc_reflect_update_u64
#include "crc_imps.h"

#ifdef HAVE_TYPE_UINT128_T
#   define CRC_TYPE                 uint128_t
#   define CRC_BITREFLECT           bitreflect128
#   define CRC_BUILD_TABLES         crc_build_tables_u128
#   define CRC_UPDATE               crc_update_u128
#   define CRC_BUILD_REFLECT_TABLES crc_build_reflect_tables_u128
#   define CRC_REFLECT_UPDATE       crc_reflect_update_u128
#   include "crc_imps.h"
#endif


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

struct crc_module
{
    uint32_t bitsize:10;
    uint32_t type:10;
    uint32_t reflect_input:1;
    uint32_t reflect_output:1;

    anyuint_t bitmask, polynomial, initial, xorout;
    const void *table; /* entity is String buffer as instance variable */
};

static VALUE cCRC;          /* class CRC */
static VALUE mUtils;        /* module CRC::Utils */
static ID ext_iv_name;
static ID ext_iv_module;
static ID ext_iv_table_buffer;

static const rb_data_type_t ext_type = {
    .wrap_struct_name = "crc-turbo.CRC.module",
    .function.dmark = NULL,
    .function.dsize = NULL,
    .function.dfree = (void *)-1,
};

static struct crc_module *
get_modulep(VALUE obj)
{
    struct crc_module *p;
    obj = rb_ivar_get(obj, ext_iv_module);
    if (NIL_P(obj)) { return NULL; }
    TypedData_Get_Struct(obj, struct crc_module, &ext_type, p);
    return p;
}

static struct crc_module *
get_module(VALUE obj)
{
    struct crc_module *p = get_modulep(obj);
    if (!p) { rb_raise(rb_eTypeError, "wrong initialized object - #<%s:0x%p>", rb_obj_classname(obj), (void *)obj); }
    return p;
}

static void
ext_init_args(int argc, VALUE argv[], int *flags, int *bitsize, VALUE *poly, VALUE *init, VALUE *xorout, VALUE *name)
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

static VALUE
ext_s_new(int argc, VALUE argv[], VALUE crc)
{
    if (get_modulep(crc)) {
        return rb_call_super(argc, argv);
    } else {
        int flags, bitsize;
        VALUE poly, init, xorout, name;
        ext_init_args(argc, argv, &flags, &bitsize, &poly, &init, &xorout, &name);

        struct crc_module *p;
        VALUE crcmod = TypedData_Make_Struct(crc, struct crc_module, &ext_type, p);

        p->bitsize = bitsize;
        p->type = flags & TYPE_MASK;
        p->reflect_input = ((flags & REFLECT_INPUT) != 0) ? 1 : 0;
        p->reflect_output = ((flags & REFLECT_OUTPUT) != 0) ? 1 : 0;
        p->table = NULL;

        /*
         * bitmask の代入でわざわざ1ビット分を後から行う理由は、
         * 例えば uint8_t に対して << 8 をすると何もしないため、
         * これへの対処を目的とする。
         */
#define SNNIPET_INIT_MOD(BITSIZE, AS, TYPE, TOUINT, CONVUINT)  \
        p->bitmask.AS = ~(~(TYPE)0 << 1 << (bitsize - 1));     \
        p->polynomial.AS = p->bitmask.AS & TOUINT(poly);       \
        p->initial.AS = p->bitmask.AS & TOUINT(init);          \
        p->xorout.AS = p->bitmask.AS & TOUINT(xorout);         \

        SWITCH_BY_TYPE(p->type, SNNIPET_INIT_MOD);

        VALUE newcrc = rb_define_class_id(0, crc);
        rb_ivar_set(newcrc, ext_iv_module, crcmod);
        rb_ivar_set(newcrc, ext_iv_name, name);

        rb_extend_object(newcrc, rb_const_get(cCRC, rb_intern("Calcurator")));

        return newcrc;
    }
}

static VALUE
ext_bitsize(VALUE t)
{
    return INT2FIX(get_module(t)->bitsize);
}

static VALUE
ext_bitmask(VALUE t)
{
    struct crc_module *p = get_module(t);

#define SNNIPET_BITMASK(BITSIZE, AS, TYPE, TOUINT, CONVUINT) \
    return CONVUINT(p->bitmask.AS);                          \

    SWITCH_BY_TYPE(p->type, SNNIPET_BITMASK);
}

static VALUE
ext_polynomial(VALUE t)
{
    struct crc_module *p = get_module(t);

#define SNNIPET_POLYNOMIAL(BITSIZE, AS, TYPE, TOUINT, CONVUINT) \
    return CONVUINT(p->polynomial.AS);                          \

    SWITCH_BY_TYPE(p->type, SNNIPET_POLYNOMIAL);
}

static VALUE
ext_initial_crc(VALUE t)
{
    struct crc_module *p = get_module(t);

#define SNNIPET_INITIAL_CRC(BITSIZE, AS, TYPE, TOUINT, CONVUINT) \
    return CONVUINT(p->initial.AS);                              \

    SWITCH_BY_TYPE(p->type, SNNIPET_INITIAL_CRC);
}

static VALUE
ext_table(VALUE t)
{
    struct crc_module *p = get_module(t);
    rb_raise(rb_eNotImpError, "");
}

static VALUE
ext_reflect_input(VALUE t)
{
    struct crc_module *p = get_module(t);
    return (p->reflect_input != 0) ? Qtrue : Qfalse;
}

static VALUE
ext_reflect_output(VALUE t)
{
    struct crc_module *p = get_module(t);
    return (p->reflect_output != 0) ? Qtrue : Qfalse;
}

static VALUE
ext_xor_output(VALUE t)
{
    struct crc_module *p = get_module(t);

#define SNNIPET_XOR_OUTPUT(BITSIZE, AS, TYPE, TOUINT, CONVUINT) \
    return CONVUINT(p->xorout.AS);                              \

    SWITCH_BY_TYPE(p->type, SNNIPET_XOR_OUTPUT);
}

static VALUE
ext_name(VALUE t)
{
    // get_module で初期化の確認
    get_module(t);

    return rb_ivar_get(t, ext_iv_name);
}

static VALUE
ext_set_name(VALUE t, VALUE name)
{
    // get_module で初期化の確認
    get_module(t);

    rb_ivar_set(t, ext_iv_name, rb_String(name));
    return name;
}

static VALUE
ext_update(VALUE t, VALUE seq, VALUE state)
{
    struct crc_module *p = get_module(t);
    rb_check_type(seq, RUBY_T_STRING);
    const char *q = RSTRING_PTR(seq);
    const char *qq = q + RSTRING_LEN(seq);

    if (!p->table) {
        size_t tablebytes = (p->type) * 16 * 256;
        VALUE tablebuf = rb_str_buf_new(tablebytes);
        rb_str_set_len(tablebuf, tablebytes);
        void *table = RSTRING_PTR(tablebuf);
        if (p->reflect_input) {
#define SNNIPET_BUILD_REFTABLE(BITSIZE, AS, TYPE, TOUINT, CONVUINT)                       \
            crc_build_reflect_tables_u##BITSIZE(p->bitsize, table, p->polynomial.AS, 16); \

            SWITCH_BY_TYPE(p->type, SNNIPET_BUILD_REFTABLE);
        } else {
#define SNNIPET_BUILD_TABLE(BITSIZE, AS, TYPE, TOUINT, CONVUINT)                  \
            crc_build_tables_u##BITSIZE(p->bitsize, table, p->polynomial.AS, 16); \

            SWITCH_BY_TYPE(p->type, SNNIPET_BUILD_TABLE);
        }
        rb_ivar_set(t, ext_iv_table_buffer, tablebuf);
        rb_obj_freeze(tablebuf);
        p->table = table;
    }

    if (p->reflect_input) {
#define SNNIPET_REFUPDATE(BITSIZE, AS, TYPE, TOUINT, CONVUINT)      \
        return CONVUINT(crc_reflect_update_u##BITSIZE(              \
                    p->bitsize, p->table, q, qq, TOUINT(state)));   \

        SWITCH_BY_TYPE(p->type, SNNIPET_REFUPDATE);
    } else {
#define SNNIPET_UPDATE(BITSIZE, AS, TYPE, TOUINT, CONVUINT)         \
        return CONVUINT(crc_update_u##BITSIZE(                      \
                    p->bitsize, p->table, q, qq, TOUINT(state)));   \

        SWITCH_BY_TYPE(p->type, SNNIPET_UPDATE);
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
    if (sizeof(unsigned long) < 16) {
        static const int len = sizeof(char[16]) / sizeof(unsigned long);
        unsigned long tmp[len + 1];
        rb_big_pack(num, tmp, len);
        unsigned long *p = tmp;
        unsigned long *q = p + len;
        while (p < q) {
            unsigned long tmp1 = bitreflect_ulong(*p);
            *p ++ = bitreflect_ulong(*-- q);
            *q = tmp1;
        }
        tmp[len] = 0;
        return rb_big_unpack(tmp, len + 1);
    } else {
        unsigned long n[2];
        rb_big_pack(num, n, 1);
        n[0] =  (unsigned long)bitreflect64(n[0] >> 64)       |
               ((unsigned long)bitreflect64(n[0]      ) << 64);
        n[1] = 0;
        return rb_big_unpack(n, 2);
    }
}

/*
 * library crc/_turbo.so
 */

void
Init__turbo(void)
{
    ext_iv_name = rb_intern("crc-turbo.CRC.name");
    ext_iv_module = rb_intern("crc-turbo.CRC.module");
    ext_iv_table_buffer = rb_intern("crc-turbo.CRC.table-buffer");

    cCRC = rb_define_class("CRC", rb_cObject);
    rb_define_singleton_method(cCRC, "new", ext_s_new, -1);
    rb_define_singleton_method(cCRC, "bitsize", ext_bitsize, 0);
    rb_define_singleton_method(cCRC, "bitmask", ext_bitmask, 0);
    rb_define_singleton_method(cCRC, "polynomial", ext_polynomial, 0);
    rb_define_singleton_method(cCRC, "initial_crc", ext_initial_crc, 0);
    rb_define_singleton_method(cCRC, "table", ext_table, 0);
    rb_define_singleton_method(cCRC, "reflect_input?", ext_reflect_input, 0);
    rb_define_singleton_method(cCRC, "reflect_output?", ext_reflect_output, 0);
    rb_define_singleton_method(cCRC, "xor_output", ext_xor_output, 0);
    rb_define_singleton_method(cCRC, "name", ext_name, 0);
    rb_define_singleton_method(cCRC, "name=", ext_set_name, 1);
    rb_define_singleton_method(cCRC, "update", ext_update, 2);

    mUtils = rb_define_module_under(cCRC, "Utils");
    rb_define_method(mUtils, "bitreflect8", utils_s_bitref8, 1);
    rb_define_method(mUtils, "bitreflect16", utils_s_bitref16, 1);
    rb_define_method(mUtils, "bitreflect32", utils_s_bitref32, 1);
    rb_define_method(mUtils, "bitreflect64", utils_s_bitref64, 1);
    rb_define_method(mUtils, "bitreflect128", utils_s_bitref128, 1);
}
