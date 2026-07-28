#include "../../../../../src/opt/targ_const_info.h"
#include "../../../../../src/com/xcominc.h"

static void test_xsplit()
{
    {
    char const* s = "a\\b\\c";
    xcom::StrBufVec sbv;
    xcom::xsplit(s, "\\", sbv);
    ASSERT0(sbv.getStrBufNum() == 3);
    ASSERT0(sbv.getStrBuf(0)->is_equal("a"));
    ASSERT0(sbv.getStrBuf(1)->is_equal("b"));
    ASSERT0(sbv.getStrBuf(2)->is_equal("c"));
    }
    {
    char const* s = "a/b/c";
    xcom::StrBufVec sbv;
    xcom::xsplit(s, "/", sbv);
    ASSERT0(sbv.getStrBufNum() == 3);
    ASSERT0(sbv.getStrBuf(0)->is_equal("a"));
    ASSERT0(sbv.getStrBuf(1)->is_equal("b"));
    ASSERT0(sbv.getStrBuf(2)->is_equal("c"));
    }
    {
    char const* s = "a,bb,ccc";
    xcom::StrBufVec sbv;
    xcom::xsplit(s, ",", sbv);
    ASSERT0(sbv.getStrBufNum() == 3);
    ASSERT0(sbv.getStrBuf(0)->is_equal("a"));
    ASSERT0(sbv.getStrBuf(1)->is_equal("bb"));
    ASSERT0(sbv.getStrBuf(2)->is_equal("ccc"));
    }
    {
    char const* s = "aaa bb c";
    xcom::StrBufVec sbv;
    xcom::xsplit(s, " ", sbv);
    ASSERT0(sbv.getStrBufNum() == 3);
    ASSERT0(sbv.getStrBuf(0)->is_equal("aaa"));
    ASSERT0(sbv.getStrBuf(1)->is_equal("bb"));
    ASSERT0(sbv.getStrBuf(2)->is_equal("c"));
    }
}

static void test_leading_zero()
{
    {
    UINT64 a = 0x6000FFFFffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 1);
    }
    {
    UINT64 a = 0x8000FFFFffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 0);
    }
    {
    UINT64 a = 0xd000FFFFffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 0);
    }
    {
    UINT64 a = 0x7800FFFFffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 1);
    }
    {
    UINT64 a = 0x2fffFFFeffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 2);
    }
    {
    UINT64 a = 0x0fffFFFf7fffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 4);
    }
    {
    UINT64 a = 0x00000007ffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 29);
    }
    {
    UINT64 a = 0x00000000ffffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 32);
    }
    {
    UINT64 a = 0x000000007fffFFFF;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 33);
    }
    {
    UINT64 a = 0x0000000000000001;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 63);
    }
    {
    UINT64 a = 0x0000000000000000;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 64);
    }
    {
    UINT32 a = 0xFFFFffff;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 0);
    }
    {
    UINT32 a = 0x80000000;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 0);
    }
    {
    UINT32 a = 0x10000000;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 3);
    }
    {
    UINT32 a = 0x00000001;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 31);
    }
    {
    UINT32 a = 0x00000000;
    UINT n = xcom::countLeadingZero(a);
    ASSERT0(n == 32);
    }
}

static void test_leading_one()
{
    {
    UINT64 a = 0x4000000000000001;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 0);
    }
    {
    UINT64 a = 0x8000000000000001;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 1);
    }
    {
    UINT64 a = 0xe000000000000001;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 3);
    }
    {
    UINT64 a = 0xFFFFfffe00000000;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 31);
    }
    {
    UINT64 a = 0xFFFFffff00001111;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 32);
    }
    {
    UINT64 a = 0xFFFFffff80001111;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 33);
    }
    {
    UINT64 a = 0xFFFFffffA0001111;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 33);
    }
    {
    UINT64 a = 0xFFFFffffFFFFFFFe;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 63);
    }
    {
    UINT64 a = 0xFFFFffffFFFFFFFf;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 64);
    }
    {
    UINT32 a = 0xFFFFffff;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 32);
    }
    {
    UINT32 a = 0x8FFFffff;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 1);
    }
    {
    UINT32 a = 0x7FFFffff;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 0);
    }
    {
    UINT32 a = 0xfFFFfffe;
    UINT n = xcom::countLeadingOne(a);
    ASSERT0(n == 31);
    }
}


static void test_int_hash()
{
    IntSet2StrMap<HOST_INT> map;
    Vector<HOST_INT> b;
    b.append(1);
    b.append(3);
    b.append(9);
    CHAR const* src = "test";
    map.set(b, src);

    Vector<HOST_INT> b2;
    b2.append(1);
    b2.append(3);
    b2.append(10);
    CHAR const* src2 = "test2";
    map.set(b2, src2);

    Vector<HOST_INT> b3;
    b3.append(2);
    b3.append(3);
    b3.append(10);
    CHAR const* src3 = "test3";
    map.set(b3, src3);

    Vector<HOST_INT> b4;
    b4.append(2);
    b4.append(3);
    CHAR const* src4 = "test4";
    map.set(b4, src4);

    Vector<HOST_INT> b5;
    b5.append(1);
    CHAR const* src5 = "test5";
    map.set(b5, src5);

    Vector<HOST_INT> b6;
    b6.append(0);
    CHAR const* src6 = "test6";
    map.set(b6, src6);

    FileObj fo("tmp.log");
    map.dump(fo.getFileHandler(), 0);
    //==---- DUMP IntSetMap ----==
    //---- NumOfNode:9 ----
    //0x0
    //  0x0: mapped_addr: 0xf408ba38, mapped_content: test6
    //  0x2
    //    0x3: mapped_addr: 0xf3fd1268, mapped_content: test4
    //      0xa: mapped_addr: 0xf408ba24, mapped_content: test3
    //  0x1: mapped_addr: 0xf408ba30, mapped_content: test5
    //    0x3
    //      0xa: mapped_addr: 0xf408ba1c, mapped_content: test2
    //      0x9: mapped_addr: 0xf408ba14, mapped_content: test

    CHAR const* mapped;
    bool dx;
    dx = map.find(b, mapped);
    ASSERT0(dx);
    ASSERT0(::strcmp(src, mapped) == 0);

    dx = map.find(b2, mapped);
    ASSERT0(dx);
    ASSERT0(::strcmp(src2, mapped) == 0);

    dx = map.find(b3, mapped);
    ASSERT0(dx);
    ASSERT0(::strcmp(src3, mapped) == 0);

    dx = map.find(b4, mapped);
    ASSERT0(dx);
    ASSERT0(::strcmp(src4, mapped) == 0);

    dx = map.find(b5, mapped);
    ASSERT0(dx);
    ASSERT0(::strcmp(src5, mapped) == 0);

    dx = map.find(b6, mapped);
    ASSERT0(dx);
    ASSERT0(::strcmp(src6, mapped) == 0);
}


static void test_binary()
{
    LONGLONG res = xcom::xatoll("0b11000000");
    ASSERT0(res == 192);
    res = xcom::xatoll("0b11111111000000000000");
    ASSERT0(res == 1044480);
    res = xcom::xatoll("0b10011111001001001001");
    ASSERT0(res == 651849);
    res = xcom::xatoll("0b1");
    ASSERT0(res == 1);
    res = xcom::xatoll("0b0");
    ASSERT0(res == 0);
    res = xcom::xatoll("0b10");
    ASSERT0(res == 2);
    res = xcom::xatoll("0b100");
    ASSERT0(res == 4);
    res = xcom::xatoll("0b101");
    ASSERT0(res == 5);
    res = xcom::xatoll("0b11111111111111111111111111111111");
    ASSERT0(res == 0xffffFFFF);
    res = xcom::xatoll("0b1111111111111111111111111111111111111111111111111111111111111111");
    ASSERT0(res == 0xffffFFFFffffFFFF);
    res = xcom::xatoll("0b0000000000000000000000000000000000000000000000000000000000000000");
    ASSERT0(res == 0);
    res = xcom::xatoll("0b0000000000");
    ASSERT0(res == 0);
}


static void test_nroot()
{
    {
        Float a(27);
        Float res;
        bool succ = xcom::xnroot(a, 3, res);
        ASSERT0(succ && res == 3.0);
    }
    {
        Float a(4);
        Float res;
        bool succ = xcom::xnroot(a, 2, res);
        ASSERT0(succ && res == 2.0);
    }
    {
        Float a(0.0625);
        Float res;
        bool succ = xcom::xnroot(a, 2, res);
        ASSERT0(succ && res == 0.25);
    }
    {
        Float a(0.125);
        Float res;
        bool succ = xcom::xnroot(a, 3, res);
        ASSERT0(succ && res == 0.5);
    }
    {
        Float a(-0.125);
        Float res;
        bool succ = xcom::xnroot(a, 3, res);
        ASSERT0(succ && res == -0.5);
    }
    {
        Float a(-4);
        Float res;
        bool succ = xcom::xnroot(a, 2, res);
        ASSERT0(!succ);
    }
}


static void test_log()
{
    {
        double x = xcom::xlog(2, 8);
        ASSERT0(x == 3.0);
    }
    {
        double x = xcom::xlog(2.5, 15.625);
        ASSERT0(x == 3.0);
    }
    {
        double x = xcom::xlog(0.0001, 0.000000000001);
        ASSERT0(x == 3.0);
    }
}


static int test_get_high_nbit()
{
    UINT64 x = 0x12345678abcd4321;
    UINT64 exp[65] = {
        0,
        //--
        0x0,
        0x0,
        0x0,
        0x1,
        0x2,
        0x4,
        0x9,
        0x12,
        0x24,
        0x48,
        0x91,
        0x123,
        0x246,
        0x48d,
        0x91a,
        0x1234,
        0x2468,
        0x48d1,
        0x91a2,
        0x12345,
        0x2468a,
        0x48d15,
        0x91a2b,
        0x123456,
        0x2468ac,
        0x48d159,
        0x91a2b3,
        0x1234567,
        0x2468acf,
        0x48d159e,
        0x91a2b3c,
        0x12345678,
        0x2468acf1,
        0x48d159e2,
        0x91a2b3c5,
        0x12345678a,
        0x2468acf15,
        0x48d159e2a,
        0x91a2b3c55,
        0x12345678ab,
        0x2468acf157,
        0x48d159e2af,
        0x91a2b3c55e,
        0x12345678abc,
        0x2468acf1579,
        0x48d159e2af3,
        0x91a2b3c55e6,
        0x12345678abcd,
        0x2468acf1579a,
        0x48d159e2af35,
        0x91a2b3c55e6a,
        0x12345678abcd4,
        0x2468acf1579a8,
        0x48d159e2af350,
        0x91a2b3c55e6a1,
        0x12345678abcd43,
        0x2468acf1579a86,
        0x48d159e2af350c,
        0x91a2b3c55e6a19,
        0x12345678abcd432,
        0x2468acf1579a864,
        0x48d159e2af350c8,
        0x91a2b3c55e6a190,
        0x12345678abcd4321,
    };
    for (UINT i = 1; i <= 64; i++) {
        UINT64 v = xcom::get64BitValueHighNBit(x, i);
        //printf("\n0x%llx", v);
        ASSERT0(exp[i] == v);
    }
    //for (UINT i = 1; i <= 64; i++) {
    //    UINT64 z = exp[i];
    //    z = z << (64-i);
    //    printf("\n0x%llx", z);
    //}
    return 0;
}

UINT64 xxx(UINT64 val, UINT bitnum)
{
    ASSERT0((bitnum >= 1) && (bitnum <= 64));
    UINT s = 64 - bitnum;
    return (val << s) >> s;
}

static int test_get_low_nbit()
{
    UINT64 x = 0x12345678abcd4321;
    UINT64 exp[65] = {
        0,
        //--
        0x1,
        0x1,
        0x1,
        0x1,
        0x1,
        0x21,
        0x21,
        0x21,
        0x121,
        0x321,
        0x321,
        0x321,
        0x321,
        0x321,
        0x4321,
        0x4321,
        0x14321,
        0x14321,
        0x54321,
        0xd4321,
        0xd4321,
        0xd4321,
        0x4d4321,
        0xcd4321,
        0x1cd4321,
        0x3cd4321,
        0x3cd4321,
        0xbcd4321,
        0xbcd4321,
        0x2bcd4321,
        0x2bcd4321,
        0xabcd4321,
        0xabcd4321,
        0xabcd4321,
        0xabcd4321,
        0x8abcd4321,
        0x18abcd4321,
        0x38abcd4321,
        0x78abcd4321,
        0x78abcd4321,
        0x78abcd4321,
        0x278abcd4321,
        0x678abcd4321,
        0x678abcd4321,
        0x1678abcd4321,
        0x1678abcd4321,
        0x5678abcd4321,
        0x5678abcd4321,
        0x5678abcd4321,
        0x5678abcd4321,
        0x45678abcd4321,
        0x45678abcd4321,
        0x145678abcd4321,
        0x345678abcd4321,
        0x345678abcd4321,
        0x345678abcd4321,
        0x345678abcd4321,
        0x2345678abcd4321,
        0x2345678abcd4321,
        0x2345678abcd4321,
        0x12345678abcd4321,
        0x12345678abcd4321,
        0x12345678abcd4321,
        0x12345678abcd4321,
    };
    for (UINT i = 1; i <= 64; i++) {
        UINT64 v = xxx(x, i);
        //UINT64 v = xcom::get64BitValueLowNBit(x, i);
        //printf("\n0x%llx", v);
        ASSERT0(exp[i] == v);
    }
    //for (UINT i = 1; i <= 64; i++) {
    //    UINT64 z = exp[i];
    //    z = z << (64-i);
    //    printf("\n0x%llx", z);
    //}
    return 0;
}

static void testComf()
{
    test_xsplit();
    test_leading_zero();
    test_leading_one();
    test_int_hash();
    test_binary();
    test_nroot();
    test_log();
    test_get_high_nbit();
    test_get_low_nbit();
}

int main()
{
    testComf();
    return 0;
}
