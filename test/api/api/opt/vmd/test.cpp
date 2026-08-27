#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void test_VMDLiveness()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("test.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_irid = true;
    g_dump_opt.is_dump_all = true;
    xcom::DefFixedStrBuf str;
    str.strcat("testcase5.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    Region * rg = rm.getRegion(2);
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAggressiveAA(*oc);
    rg->doDUAna(*oc);
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_MDSSA_MGR, PASS_UNDEF);
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    ASSERT0(mdssamgr != nullptr);
    VMDLivenessMgr * vmdlivemgr = (VMDLivenessMgr*)rg->getPassMgr()->
        registerPass(PASS_VMDLIVENESS_MGR);
    vmdlivemgr->perform(*oc);
    {
        PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
            queryPass(PASS_PRSSA_MGR);
        if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
        MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
            queryPass(PASS_MDSSA_MGR);
        if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
    }
}


void testMDSSA()
{
    test_VMDLiveness();
}

int main()
{
    testMDSSA();
    return 0;
}
