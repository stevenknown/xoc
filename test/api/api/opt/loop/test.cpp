#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void test1()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    xoc::g_do_opt_float = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("tmp.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_irid = true;
    g_dump_opt.is_dump_all = true;
    xcom::DefFixedStrBuf str;
    str.strcat("loopinfo.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("s85");
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    PassMgr * passmgr = rg->getPassMgr();
    ASSERT0(rg && rg->is_function());
    if (rg->getIRList() != nullptr) {
        ASSERT0(rg->getBBList()->is_empty());
        SimpCtx simp(oc);
        simp.setSimpCFS();
        //simp.setSimpLandLor();
        //simp.setSimpLnot();
        //simp.setSimpToLowestHeight();
        rg->setIRList(rg->getIRSimp()->simplifyStmtList(
            rg->getIRList(), &simp));
        rg->constructBBList();
        ASSERT0(xoc::verifyIRandBB(rg->getBBList(), rg));
    }
    passmgr->checkValidAndRecompute(oc, PASS_CFG, PASS_LOOP_INFO, PASS_UNDEF);
    rg->doAggressiveAA(*oc);
    rg->doDUAna(*oc);
    IRCFG * cfg = rg->getCFG();
    {
        LI<IRBB> * li = const_cast<IRCFG*>(cfg)->getLoopInfo();
        LoopInfoIter it;
        UINT i = 0;
        for (li = xoc::iterInitLoopInfo(li, it);
             li != nullptr; li = xoc::iterNextLoopInfo(it), i++) {
            //li->dump(rg);
        }
        ASSERT0(i == 13);
    }
    {
        //PassMgr ask destroy PRSSAMgr before region destroy.
        PRSSAMgr * prssa = (PRSSAMgr*)passmgr->queryPass(PASS_PRSSA_MGR);
        if (prssa != nullptr) {
            prssa->destruction(*oc);
        }
    }
    {
        //PassMgr ask destroy MDSSAMgr before region destroy.
        MDSSAMgr * ssa = (MDSSAMgr*)passmgr->queryPass(PASS_MDSSA_MGR);
        if (ssa != nullptr) {
            ssa->destruction(*oc);
        }
    }
}

void testLoopInfo()
{
    test1();
}

int main()
{
    testLoopInfo();
    return 0;
}
