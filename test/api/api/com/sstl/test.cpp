#include "../../../../../src/opt/targ_const_info.h"
#include "../../../../../src/com/xcominc.h"

void testFixSizeVector()
{
    {
        FixSizeVector<UINT, 8> a;
        ASSERT0(a.get_last_idx() == VEC_UNDEF);

        a.set(0, 1);
        ASSERT0(a.get_last_idx() == 0);

        a.set(7, 2);
        ASSERT0(a.get_last_idx() == 7);

        a.set(0, 0);
        ASSERT0(a.get_capacity() == 8);
        ASSERT0(a.get_elem_count() == 8);
        ASSERT0(a.get_last_idx() == 7);

        FixSizeVector<UINT, 8> b;
        b.set(0, 10);
        b.set(7, 11);
        b.set(5, 13);
        b.set(2, 12);

        a.copy(b);
    }
}

int main()
{
    testFixSizeVector();
    return 0;
}
