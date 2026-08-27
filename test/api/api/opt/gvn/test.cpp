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
    g_dump_opt.is_dump_irid = true;
    g_dump_opt.is_dump_all = true;
    xcom::DefFixedStrBuf str;
    str.strcat("isomo.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("foo");
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    IRMgr * irmgr = rg->getIRMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    VarMgr * varmgr = rg->getVarMgr();
    PassMgr * passmgr = rg->getPassMgr();
    Type const* boolty = tymgr->getBool();
    Type const* i32 = tymgr->getI32();
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
    passmgr->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAggressiveAA(*oc);
    rg->doDUAna(*oc);
    {
        GVN * gvn= (GVN*)passmgr->registerPass(PASS_GVN);
        ASSERT0(gvn);
        VN const* vn1 = gvn->registerVNviaINT(1);
        VN const* vn2 = gvn->registerVNviaINT(2);
        VN const* vn3 = gvn->registerVNviaINT(3);
        VN const* vn4 = gvn->registerVNviaINT(4);
        VN const* res = gvn->registerMultiTupleVN(
            IR_ADD, 4, vn1, vn2, vn3, vn4);
        ASSERT0(res);
        gvn->dumpAllVN();
        gvn->destroy();
    }
    {
        //PassMgr asks to destroy PRSSAMgr before the region destroy.
        PRSSAMgr * prssa = (PRSSAMgr*)passmgr->queryPass(PASS_PRSSA_MGR);
        if (prssa != nullptr) {
            prssa->destruction(*oc);
        }
    }
    {
        //PassMgr asks to destroy MDSSAMgr before the region destroy.
        MDSSAMgr * mdssa = (MDSSAMgr*)passmgr->queryPass(PASS_MDSSA_MGR);
        if (mdssa != nullptr) {
            mdssa->destruction(*oc);
        }
    }
}

void testGVN()
{
    test1();
}

int main()
{
    testGVN();
    return 0;
}
