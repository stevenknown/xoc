union DWunion {
    long long ll_value;
    struct {
    unsigned int low;
    unsigned int high;
    } s;
};

#if defined(L_lshrdi3)
unsigned long long
__lshrdi3 (unsigned long long src1, unsigned int src2)
{
    union DWunion x, result;
    unsigned int v;

    src2 &= 0x3f;
    if (src2 == 0) {
    return src1;
    }

    v = src2 & 0x1f;
    x.ll_value = src1;

    if (src2 >= 32) {
    result.s.high = 0;
    result.s.low = x.s.high >> v;
    } else {
    unsigned int carries = x.s.high << (32 - v);

    result.s.high = x.s.high >> v;
    result.s.low = (x.s.low >> v) | carries;
    }

    return result.ll_value;
}
#endif

#if defined(L_ashrdi3)
long long
__ashrdi3 (long long src1, unsigned int src2)
{
    union DWunion x, result;
    unsigned int v;

    src2 = src2 & 0x3f;
    if (src2 == 0) {
    return src1;
    }

    v = src2 & 0x1f;
    x.ll_value = src1;

    if (src2 >= 32) {
    /* result.s.high = 1..1 or 0..0 */
    result.s.high = (int)x.s.high >> 31;
    result.s.low = (int)x.s.high >> v;
    } else {
    unsigned int carries = x.s.high << (32 - v);

    result.s.high = (int)x.s.high >> v;
    result.s.low = (x.s.low >> v) | carries;
    }

    return result.ll_value;
}
#endif

#if defined(L_ashldi3)
long long
__ashldi3 (long long src1, unsigned int src2)
{
    union DWunion x, result;
    unsigned int v;

    src2 = src2 & 0x3f;
    if (src2 == 0) {
    return src1;
    }

    v = src2 & 0x1f;
    x.ll_value = src1;

    if (src2 >= 32) {
    result.s.low = 0;
    result.s.high = x.s.low << v;
    } else {
    unsigned int carries = x.s.low >> (32 - v);

    result.s.low = x.s.low << v;
    result.s.high = (x.s.high << v) | carries;
    }

    return result.ll_value;
}
#endif
