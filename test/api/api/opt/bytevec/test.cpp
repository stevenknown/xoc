#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void test1()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    xoc::g_do_opt_float = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("zz.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    g_dump_opt.is_dump_all = true;
    xcom::DefFixedStrBuf str;
    str.strcat("rce_fold.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("bar");
    ASSERT0(rg && rg->is_function());
    {
        BYTEVec bv;
        bv.append(3); 
        bv.append(0xD); 
        bv.append(0xc); 
        bv.append(0x1); 
        bv.append(0x9); 
        bv.append(0xF); 
        bv.append(0xB); 
        bv.append(0x4); 
        bv.append(0xc); 
        bv.append(0x12); 
        bv.append(0x34); 
        bv.append(0xAB); 
        bv.append(0xEF); 
        bv.dump(rg);
        ASSERT0(bv.get(0) == 3); 
        ASSERT0(bv.get(2) == 0xc); 
        ASSERT0(bv.get(10) == 0x34); 
        ASSERT0(bv.get(12) == 0xEF); 
        
        BYTE arr[5] = {0x77, 0x88, 0x99, 0xAA, 0xBB };
        bv.append(arr, 5);
        bv.dump(rg);
        ASSERT0(bv.get(13) == 0x77); 
        ASSERT0(bv.get(14) == 0x88); 
        ASSERT0(bv.get(15) == 0x99); 
        ASSERT0(bv.get(16) == 0xAA); 
        ASSERT0(bv.get(17) == 0xBB); 
        ASSERT0(bv.get_elem_count() == 18); 

        BYTEVec src;
        src.append(0x11);
        src.append(0x22);
        src.append(0x33);
        src.append(0x44);
        src.append(0x55);
        bv.append(src);
        bv.dump(rg);
        ASSERT0(bv.get(18) == 0x11); 
        ASSERT0(bv.get(19) == 0x22); 
        ASSERT0(bv.get(20) == 0x33); 
        ASSERT0(bv.get(21) == 0x44); 
        ASSERT0(bv.get(22) == 0x55); 
        ASSERT0(bv.get_elem_count() ==23); 
    }
}

void testBYTEVec()
{
    test1();
}

int main()
{
    testBYTEVec();
    return 0;
}
