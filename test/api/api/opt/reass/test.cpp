#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void testcase1_func(
    RegionMgr & rm, CHAR const* rgname, UINT actnum)
{
    Region * rg = rm.getRegion(rgname);
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    if (rg->getIRList() != nullptr) {
        ASSERT0(rg->getBBList()->is_empty());
        SimpCtx simp(oc);
        simp.setSimpCFS();
        simp.setSimpLandLor();
        simp.setSimpLnot();
        simp.setSimpToLowestHeight();
        rg->setIRList(rg->getIRSimp()->simplifyStmtList(
            rg->getIRList(), &simp));
        rg->constructBBList();
        ASSERT0(xoc::verifyIRandBB(rg->getBBList(), rg));
    }
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAggressiveAA(*oc);
    rg->doDUAna(*oc);
    Pass * pass = rg->getPassMgr()->registerPass(PASS_ALGE_REASSOCIATE);
    bool doit = pass->perform(*oc);

    //Some case doesn't doit.
    //ASSERT0(doit);
    PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_PRSSA_MGR);
    if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
}

static void testcase1()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("test.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();

    //ONLY USE FOR DEBUG.
    //xoc::g_dump_opt.is_dump_all = true;
    //xoc::g_dump_opt.is_dump_irid = true;
    xoc::g_dump_opt.setDumpPass(PASS_ALGE_REASSOCIATE, true);
    xcom::DefFixedStrBuf str;
    str.strcat("test_alge_reass.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    testcase1_func(rm, "foo", 10);
    testcase1_func(rm, "foo1", 6);
    testcase1_func(rm, "foo2", 0);
    testcase1_func(rm, "foo3", 3);
    testcase1_func(rm, "foo4", 12);
    testcase1_func(rm, "foo5", 7);
}

void testAlgeReass()
{
    testcase1();
}

int main()
{
    testAlgeReass();
    return 0;
}
