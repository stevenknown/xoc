#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void test4()
{
    enableO3();
    Stack<bool> optst;
    g_opt_level = OPT_LEVEL3;

    bool org1 = xoc::g_do_cfg_opt;

    xoc::g_dump_opt.is_dump_all = true;
    xoc::g_do_opt_float = true;
    xoc::g_is_support_dynamic_type = true;
    xoc::g_do_cfg_opt = false;

    ARMRegionMgr rm;
    rm.getLogMgr()->init("zz.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    g_dump_opt.is_dump_all = true;
    xcom::DefFixedStrBuf str;
    str.strcat("../../../../../test");
    str.strcat("/compile.gr/rce_fold4.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("main");
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    IRMgr * irmgr = rg->getIRMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    VarMgr * varmgr = rg->getVarMgr();
    PassMgr * passmgr = rg->getPassMgr();
    Type const* boolty = tymgr->getBool();
    Type const* i32 = tymgr->getI32();
    Type const* f64 = tymgr->getF64();
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
    RCE * rce = (RCE*)passmgr->registerPass(PASS_RCE);
    ASSERT0(rce);

    //Precheck
    ASSERT0(rg->getBB(1));
    UINT orgnum = rg->getBB(1)->getIRList().get_elem_count();
    ASSERT0(rg->getBB(1)->getIRList().get_tail()->is_truebr());
    bool changed = rce->perform(*oc);
    ASSERT0(changed);

    //Verify truber has been removed.
    ASSERT0(rg->getBB(1)->getIRList().get_tail()->is_goto());
    ASSERT0(orgnum == rg->getBB(1)->getIRList().get_elem_count());
    {
        //PassMgr ask destroy PRSSAMgr before region destroy.
        PRSSAMgr * prssa = (PRSSAMgr*)passmgr->queryPass(PASS_PRSSA_MGR);
        if (prssa != nullptr) {
            prssa->destruction(*oc);
        }
    }
    {
        MDSSAMgr * ssa = (MDSSAMgr*)passmgr->queryPass(PASS_MDSSA_MGR);
        if (ssa != nullptr) {
            ssa->destruction(*oc);
        }
    }
    xoc::g_do_cfg_opt = org1;
}

static void test3()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    xoc::g_do_opt_float = true;
    xoc::g_is_support_dynamic_type = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("zz.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    g_dump_opt.is_dump_all = true;
    xcom::DefFixedStrBuf str;
    str.strcat("../../../../../test");
    str.strcat("/compile.gr/rce_fold3.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("main");
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    IRMgr * irmgr = rg->getIRMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    VarMgr * varmgr = rg->getVarMgr();
    PassMgr * passmgr = rg->getPassMgr();
    Type const* boolty = tymgr->getBool();
    Type const* i32 = tymgr->getI32();
    Type const* f64 = tymgr->getF64();
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
    RCE * rce = (RCE*)passmgr->registerPass(PASS_RCE);
    ASSERT0(rce);
    //Precheck
    ASSERT0(rg->getBB(1));
    ASSERT0(rg->getBB(1)->getIRList().get_elem_count() == 3);
    ASSERT0(rg->getBB(1)->getIRList().get_head_nth(0)->is_st());
    ASSERT0(rg->getBB(1)->getIRList().get_head_nth(1)->is_st());
    ASSERT0(rg->getBB(1)->getIRList().get_head_nth(2)->is_truebr());
    bool changed = rce->perform(*oc);
    ASSERT0(changed);
    //Verify truber has been removed.
    ASSERT0(rg->getBB(1));
    ASSERT0(rg->getBB(1)->getIRList().get_elem_count() == 2);
    ASSERT0(rg->getBB(1)->getIRList().get_head_nth(0)->is_st());
    ASSERT0(rg->getBB(1)->getIRList().get_head_nth(1)->is_st());
    {
        //PassMgr ask destroy PRSSAMgr before region destroy.
        PRSSAMgr * prssa = (PRSSAMgr*)passmgr->queryPass(PASS_PRSSA_MGR);
        if (prssa != nullptr) {
            prssa->destruction(*oc);
        }
    }
    {
        MDSSAMgr * ssa = (MDSSAMgr*)passmgr->queryPass(PASS_MDSSA_MGR);
        if (ssa != nullptr) {
            ssa->destruction(*oc);
        }
    }
}

static void test2()
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
    str.strcat("../../../../../test");
    str.strcat("/compile.gr/rce_fold.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("zoo");
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    IRMgr * irmgr = rg->getIRMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    VarMgr * varmgr = rg->getVarMgr();
    PassMgr * passmgr = rg->getPassMgr();
    Type const* boolty = tymgr->getBool();
    Type const* i32 = tymgr->getI32();
    Type const* f64 = tymgr->getF64();
    Var * ad = varmgr->findVarByName("ad");
    ASSERT0(ad);

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
        //falsebr label _$L1 id:29
        //    gt:bool id:5
        //        ld:f64 'ad' id:3 attachinfo:MDSSA
        //        fpconst:f64 5.000000 id:4
        IR * e1 = rg->getIR(5);
        ASSERT0(e1 && e1->getStmt()->is_falsebr());
        IR * anti = irmgr->buildBinaryOpSimp(IR_GT, boolty,
            irmgr->buildLoad(ad, f64),
            irmgr->buildImmFP(5.0, f64));
        ASSERT0(e1->isIREqual(anti, irmgr, true));
    }
    {
        //falsebr label _$L2 id:31
        //    le:bool id:17
        //        ld:f64 'ad' id:15 attachinfo:MDSSA
        //        fpconst:f64 5.000000 id:16
        IR * e1 = rg->getIR(17);
        ASSERT0(e1 && e1->getStmt()->is_falsebr());
        IR * anti = irmgr->buildBinaryOpSimp(IR_LE, boolty,
            irmgr->buildLoad(ad, f64),
            irmgr->buildImmFP(5.0, f64));
        ASSERT0(e1->isIREqual(anti, irmgr, true));
    }
    {
        //PassMgr ask destroy PRSSAMgr before region destroy.
        PRSSAMgr * prssa = (PRSSAMgr*)passmgr->queryPass(PASS_PRSSA_MGR);
        if (prssa != nullptr) {
            prssa->destruction(*oc);
        }
    }
    {
        MDSSAMgr * ssa = (MDSSAMgr*)passmgr->queryPass(PASS_MDSSA_MGR);
        if (ssa != nullptr) {
            ssa->destruction(*oc);
        }
    }
}

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
    str.strcat("../../../../../test");
    str.strcat("/compile.gr/rce_fold.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    ASSERT0(succ);
    Region * rg = rm.getRegion("bar");
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    IRMgr * irmgr = rg->getIRMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    VarMgr * varmgr = rg->getVarMgr();
    PassMgr * passmgr = rg->getPassMgr();
    Type const* boolty = tymgr->getBool();
    Type const* i32 = tymgr->getI32();
    Var * a = varmgr->findVarByName("a");
    ASSERT0(a);

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
        // falsebr label _$L1 id:29
        //     le:bool id:5
        //         ld:i32 'a' id:3
        //         intconst:i32 5|0x5 id:4
        IR * e1 = rg->getIR(5);
        ASSERT0(e1 && e1->getStmt()->is_falsebr());
        IR * anti = irmgr->buildBinaryOpSimp(IR_LE, boolty,
            irmgr->buildLoad(a, i32),
            irmgr->buildImmInt(5, i32));
        ASSERT0(e1->isIREqual(anti, irmgr, true));
    }
    {
        //  falsebr label _$L2 id:31
        //      gt:bool id:17
        //          ld:i32 'a' id:15
        //          intconst:i32 5|0x5 id:16
        IR * e1 = rg->getIR(17);
        ASSERT0(e1 && e1->getStmt()->is_falsebr());
        IR * anti = irmgr->buildBinaryOpSimp(IR_GT, boolty,
            irmgr->buildLoad(a, i32),
            irmgr->buildImmInt(5, i32));
        ASSERT0(e1->isIREqual(anti, irmgr, true));
    }
    {
        //PassMgr ask destroy PRSSAMgr before region destroy.
        PRSSAMgr * prssa = (PRSSAMgr*)passmgr->queryPass(PASS_PRSSA_MGR);
        if (prssa != nullptr) {
            prssa->destruction(*oc);
        }
    }
    {
        MDSSAMgr * ssa = (MDSSAMgr*)passmgr->queryPass(PASS_MDSSA_MGR);
        if (ssa != nullptr) {
            ssa->destruction(*oc);
        }
    }
}

void testRCE()
{
    test1();
    test2();
    test3();
    test4();
}

int main()
{
    testRCE();
    return 0;
}
