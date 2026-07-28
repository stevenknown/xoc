#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void test_isRegionLiveIn()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("zz.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    Region * rg = rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initDbxMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    rg->initAttachInfoMgr();

    //st:i32 'd'
    //    add:i32
    //        ld:i32 't2'
    //        intconst:i32 23;
    //falsebr label _L1
    //    gt:bool
    //        ld:i32 'i'
    //        intconst:i32 0;
    //st:i32 'e'
    //    intconst:i32 20;
    //label _L1;
    //return
    //    ld:i32 't1';

    Type const* i32 = rg->getTypeMgr()->getI32();
    Var * d = rg->getVarMgr()->registerVar("d", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * e = rg->getVarMgr()->registerVar("e", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * t2 = rg->getVarMgr()->registerVar("t2", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * t1 = rg->getVarMgr()->registerVar("t1", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * i = rg->getVarMgr()->registerVar("i", i32, 1, VAR_LOCAL, SS_UNDEF);
    IRMgr * irmgr = rg->getIRMgr();
    TypeMgr * tm = rg->getTypeMgr();
    Sym const* li_sym = rg->getRegionMgr()->addToSymbolTab("_L1");
    LabelInfo const* li = rg->genCustomLabel(li_sym);
    IR * load_t1 = irmgr->buildLoad(t1);
    IR * load_t2 = irmgr->buildLoad(t2);
    IR * load_i = irmgr->buildLoad(i);
    IR * std = irmgr->buildStore(d,
        irmgr->buildBinaryOpSimp(IR_ADD, tm->getI32(),
            load_t2,
            irmgr->buildImmInt(23, i32)));
    IR * falsebr = irmgr->buildBranch(false,
        irmgr->buildBinaryOpSimp(IR_GT, tm->getBool(),
            load_i,
            irmgr->buildImmInt(0, i32)),
        li);
    IR * ste = irmgr->buildStore(e,
        irmgr->buildImmInt(20, i32));
    IR * label = irmgr->buildLabel(li);
    IR * ret = irmgr->buildReturn(
        load_t1);

    IR * irlst = nullptr;
    xcom::add_next(&irlst, std);
    xcom::add_next(&irlst, falsebr);
    xcom::add_next(&irlst, ste);
    xcom::add_next(&irlst, label);
    xcom::add_next(&irlst, ret);
    rg->setIRList(irlst);
    OptCtx * oc = rg->getRegionMgr()->getAndGenOptCtx(rg);
    PreAnaBeforeOpt preana(rg);
    preana.perform(*oc);
    rg->constructBBList();
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAA(*oc);
    rg->doDUAna(*oc);
    ASSERT0(xoc::isRegionLiveIn(load_t1, rg));
    ASSERT0(xoc::isRegionLiveIn(load_t2, rg));
    ASSERT0(xoc::isRegionLiveIn(load_i, rg));
    PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_PRSSA_MGR);
    if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
}


static void test_findKillingDef_2()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("zz.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    xcom::DefFixedStrBuf str;
    str.strcat("../../../../../test");
    str.strcat("/api/api/testcase2.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    Region * rg = rm.getRegion(2);
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAA(*oc);
    rg->doDUAna(*oc);
    IR const* ret = rg->getBBList()->get_tail()->getIRList().get_tail();
    ASSERT0(ret->is_return());
    ASSERT0(RET_exp(ret) && RET_exp(ret)->is_ld());
    IR const* kd = xoc::findKillingDef(RET_exp(ret), rg, oc);
    ASSERT0(kd == nullptr);
    PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_PRSSA_MGR);
    if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
}


static void test_findKillingDef_1()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("zz.log", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    xcom::DefFixedStrBuf str;
    str.strcat("../../../../../test");
    str.strcat("/api/api/testcase1.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    Region * rg = rm.getRegion(2);
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAA(*oc);
    rg->doDUAna(*oc);
    IR const* ret = rg->getBBList()->get_tail()->getIRList().get_tail();
    ASSERT0(ret->is_return());
    ASSERT0(RET_exp(ret) && RET_exp(ret)->is_ld());
    rg->getCFG()->dumpDOT(nullptr, 0xFFFF);
    IR const* kd = xoc::findKillingDef(RET_exp(ret), rg, oc);
    ASSERT0(kd && kd->id() == 3 && kd->is_st());
    PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_PRSSA_MGR);
    if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
}


static void test_findKillingDef_3()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
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
    str.strcat("/api/api/testcase3.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    Region * rg = rm.getRegion(2);
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAggressiveAA(*oc);
    rg->doDUAna(*oc);
    IR const* stmt = rg->getBBList()->get_tail()->getIRList().
        get_tail_nth(1);
    ASSERT0(stmt && stmt->is_stpr() && stmt->getRHS()->is_ild());
    IR const* exp = stmt->getRHS();
    ASSERT0(exp && exp->is_ild());
    IR const* kd = xoc::findKillingDef(exp, rg, oc);
    ASSERT0(kd == nullptr);
    PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_PRSSA_MGR);
    if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
}

static void test_findKillingDef_5()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
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
    str.strcat("/api/api/testcase5.gr");
    bool succ = xoc::readGRAndConstructRegion(&rm, str.getBuf());
    Region * rg = rm.getRegion(2);
    ASSERT0(rg && rg->is_function());
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    rg->getPassMgr()->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    rg->doAggressiveAA(*oc);
    rg->doDUAna(*oc);
    IR const* stmt = rg->getBBList()->get_tail()->getIRList().
        get_tail_nth(1);
    ASSERT0(stmt && stmt->is_stpr() && stmt->getRHS()->is_ild());
    IR const* exp = stmt->getRHS();
    ASSERT0(exp && exp->is_ild());
    IR const* kd = xoc::findKillingDef(exp, rg, oc);
    ASSERT0(kd && kd->is_ist() && kd->id() == 8);
    PRSSAMgr * prssamgr = (PRSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_PRSSA_MGR);
    if (prssamgr != nullptr) { prssamgr->destruction(*oc); }
    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
        queryPass(PASS_MDSSA_MGR);
    if (mdssamgr != nullptr) { mdssamgr->destruction(*oc); }
}

void testMDSSA()
{
    test_isRegionLiveIn();
    test_findKillingDef_1();
    test_findKillingDef_2();
    test_findKillingDef_3();
    test_findKillingDef_5();
}

int main()
{
    testMDSSA();
    return 0;
}
