#include "../../header_for_xgen.h"
#include "../../enable_opt.h"
#include <cstdint>
#include <iostream>
#include <math.h>
#include <time.h>
#include <type_traits>
#include <regex>

static void testMultiRes1()
{
    ARMRegionMgr rm;
    rm.getLogMgr()->init("test.log", false);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = false;
    g_do_cg = true;
    Region * rg = rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    MultiResConvert * cvt = (MultiResConvert*)rg->getPassMgr()->registerPass(
        PASS_MULTI_RES_CVT);
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    IR * respr1 = rg->getIRMgr()->buildPR(u8);
    IR * respr2 = rg->getIRMgr()->buildPR(u8);
    IR * respr3 = rg->getIRMgr()->buildPR(u8);
    IR * respr4 = rg->getIRMgr()->buildPR(u8);
    IR * reslst = nullptr;
    xcom::add_next(&reslst, respr1);
    xcom::add_next(&reslst, respr2);
    xcom::add_next(&reslst, respr3);
    xcom::add_next(&reslst, respr4);
 
    IR * rhs = rg->getIRMgr()->buildBinaryOp(
        IR_ADD, u16, rg->getIRMgr()->buildPRdedicated(respr1->getPrno(), u8),
        rg->getIRMgr()->buildPRdedicated(respr2->getPrno(), u8));
    IR * brd = cvt->getIRMgr()->buildBroadCast(rhs, reslst, u8);
    xoc::note(rg, "\nBEFORE CALL:buildStorePRWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(brd, rg);

    IR * newlst = cvt->buildStorePRWithMultiResAndConvertBySplit(u8, brd);
    xoc::note(rg, "\nAFTER CALL:buildStorePRWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(newlst, rg);
 
    OptCtx oc(rg);
    rg->addToIRList(newlst);
    g_opt_level = OPT_LEVEL3;
    enableO3();
    rg->process(&oc);
}

static void testMultiRes2_2()
{
    ARMRegionMgr rm;
    rm.getLogMgr()->init("test.log", false);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = false;
    Region *rg = rm.newRegion(REGION_FUNC);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    MultiResConvert * cvt = (MultiResConvert*)rg->getPassMgr()->registerPass(
        PASS_MULTI_RES_CVT);
    rm.addToRegionTab(rg);
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    Type const* u32 = rm.getTypeMgr()->getU32();
    IR * respr1 = rg->getIRMgr()->buildPR(u8);
    IR * respr2 = rg->getIRMgr()->buildPR(u8);
    Var * srcv = rg->getVarMgr()->registerVar(
        "srcv", rg->getTypeMgr()->getU32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * memv1 = rg->getVarMgr()->registerVar(
        "memv1", rg->getTypeMgr()->getU32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * memv2 = rg->getVarMgr()->registerVar(
        "memv2", rg->getTypeMgr()->getU32(), 1, VAR_GLOBAL, SS_UNDEF);
    rg->addToVarTab(srcv);
    rg->addToVarTab(memv1);
    rg->addToVarTab(memv2);
    IR * resmem3 = rg->getIRMgr()->buildLoad(memv1);
    IR * resmem4 = rg->getIRMgr()->buildLoad(memv2);
    IR * reslst = nullptr;
    xcom::add_next(&reslst, respr1);
    xcom::add_next(&reslst, respr2);
    xcom::add_next(&reslst, resmem3);
 
    IR * brd = cvt->getIRMgr()->buildBroadCast(
        rg->getIRMgr()->buildLoad(srcv, u16), reslst, u8);
    xoc::note(rg, "\nBEFORE CALL:buildStoreWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(brd, rg);
    IR * newlst = cvt->buildStoreWithMultiResAndConvertBySplit(
        memv2, u32, brd);
    xoc::note(rg, "\nAFTER CALL:buildStoreWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(newlst, rg);
}

static void testMultiRes2()
{
    ARMRegionMgr rm;
    rm.getLogMgr()->init("test.log", false);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = false;
    Region *rg = rm.newRegion(REGION_FUNC);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    MultiResConvert * cvt = (MultiResConvert*)rg->getPassMgr()->registerPass(
        PASS_MULTI_RES_CVT);
    rm.addToRegionTab(rg);
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    IR * respr1 = rg->getIRMgr()->buildPR(u8);
    IR * respr2 = rg->getIRMgr()->buildPR(u8);
    IR * respr3 = rg->getIRMgr()->buildPR(u8);
    IR * respr4 = rg->getIRMgr()->buildPR(u8);
    IR * reslst = nullptr;
    xcom::add_next(&reslst, respr1);
    xcom::add_next(&reslst, respr2);
    xcom::add_next(&reslst, respr3);
    xcom::add_next(&reslst, respr4);
 
    IR * rhs = rg->getIRMgr()->buildBinaryOp(
        IR_ADD, u16, rg->getIRMgr()->buildPRdedicated(respr1->getPrno(), u8),
        rg->getIRMgr()->buildPRdedicated(respr2->getPrno(), u8));
 
    Var * v = rg->getVarMgr()->registerVar(
        "xx", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * v2 = rg->getVarMgr()->registerVar(
        "yy", rg->getTypeMgr()->getU32(), 1, VAR_GLOBAL, SS_UNDEF);
    rg->addToVarTab(v);
    IR * brd = cvt->getIRMgr()->buildBroadCast(rhs, reslst, u8);
    xoc::note(rg, "\nBEFORE CALL:buildStoreWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(brd, rg);
    IR * newlst = cvt->buildStoreWithMultiResAndConvertBySplit(v2, brd);
    xoc::note(rg, "\nAFTER CALL:buildStoreWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(newlst, rg);
}

static void testMultiRes3()
{
    ARMRegionMgr rm;
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    rm.getLogMgr()->init("test.log", false);
    g_dump_opt.is_dump_ir_id = false;
    Region *rg = rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    MultiResConvert * cvt = (MultiResConvert*)rg->getPassMgr()->registerPass(
        PASS_MULTI_RES_CVT);
    Var * v1 = rg->getVarMgr()->registerVar(
        "zz", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * v2 = rg->getVarMgr()->registerVar(
        "yy", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * iv = rg->getVarMgr()->registerVar(
        "iv", rg->getTypeMgr()->getU32(), 1, VAR_GLOBAL, SS_UNDEF);
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    Type const* u32 = rm.getTypeMgr()->getU32();
    Type const* b = rm.getTypeMgr()->getBool();
    IR * res1 = rg->getIRMgr()->buildPR(u8);
    IR * res2 = rg->getIRMgr()->buildPR(u8);
    IR * res3 = rg->getIRMgr()->buildPR(u8);
    IR * res4 = rg->getIRMgr()->buildLoad(v1, u8);
    IR * reslst = nullptr;
    xcom::add_next(&reslst, res1);
    xcom::add_next(&reslst, res2);
    xcom::add_next(&reslst, res3);
    //xcom::add_next(&reslst, res4);

    //origin stmt:
    //  $1,$2,$3 = broadcast(zz + $2)
    //after convert:
    //  $1 = dummyuse(broadcast(zz + $2))
    //  $2 = dummyuse(broadcast(zz + $2), $2) #RHS's $2 is used to build DU
    //                                        #chain, and to simulate the
    //                                        #effect of origin $2 in RHS.
    //  $3 = dummyuse(broadcast(zz + $2))
    //  st yy = dummyuse(broadcast(zz + $2),$1,$2,$3)
    IR * rhs = rg->getIRMgr()->buildBinaryOp(
        IR_ADD, u16, cvt->getIRMgr()->buildLoad(v1, u8),
        rg->dupIsomoExpTree(res2));
    IR * brd = cvt->getIRMgr()->buildBroadCast(rhs, reslst, u8);
    xoc::note(rg, "\nBEFORE CALL:buildStoreWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(brd, rg);
    brd = cvt->buildStoreWithMultiResAndConvertBySplit(v2, brd);
    ASSERT0(brd->verifyTree(rg));
    xoc::note(rg, "\nAFTER CALL:buildStoreWithMultiResAndConvertBySplit");
    xoc::dumpIRListH(brd, rg);

    IR * doloop = rg->getIRMgr()->buildDoLoop(
        rg->getIRMgr()->buildId(iv),
        rg->getIRMgr()->buildImmInt(1, u32),
        rg->getIRMgr()->buildBinaryOpSimp(IR_LE, b,
            rg->getIRMgr()->buildLoad(iv),
            rg->getIRMgr()->buildImmInt(100, u32)),
        rg->getIRMgr()->buildBinaryOpSimp(IR_ADD, u32,
            rg->getIRMgr()->buildLoad(iv),
            rg->getIRMgr()->buildImmInt(1, u32)),
        brd);
    IR * newlst = nullptr;
    xcom::add_next(&newlst, doloop);

    IR * ret = rg->getIRMgr()->buildReturn(rg->getIRMgr()->buildLoad(v1, u8));
    xcom::add_next(&newlst, ret);

    OptCtx oc(rg);
    rg->addToIRList(newlst);
    ASSERT0(xoc::verifyIRList(newlst, nullptr, rg));
    g_opt_level = OPT_LEVEL3;
    enableO3();
    rg->process(&oc);
}

static void testMultiRes4Loop()
{
    ARMRegionMgr rm;
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    rm.getLogMgr()->init("test.log", false);
    g_dump_opt.is_dump_ir_id = false;
    xoc::g_do_rp = false;
    xoc::g_do_cp = true;
    xoc::g_do_cp_aggressive = true;
    xoc::g_do_dce = true;
    xoc::g_do_dce_aggressive = true;
    xoc::g_do_licm = true;
    xoc::g_do_gcse = true;
    xoc::g_do_rce = true;
    xoc::g_do_lftr = true;
    xoc::g_do_prssa = true;
    xoc::g_do_mdssa = true;
    xoc::g_infer_type = true;
    xoc::g_do_vect = true;
    xoc::g_do_cfg_opt = true;
    xoc::g_do_cfg_remove_empty_bb = true;
    xoc::g_do_cfg_remove_unreach_bb = true;
    xoc::g_do_cfg_remove_trampolin_bb = true;
    xoc::g_do_invert_brtgt = true;
    xoc::g_do_cfg_remove_redundant_branch = true;
    xoc::g_do_cfg_remove_trampolin_branch = true;
    xoc::g_do_cfg_remove_redundant_label = true;
    xoc::g_is_support_dynamic_type = true;

    Region *rg = rm.newRegion(REGION_FUNC);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    MultiResConvert * cvt = (MultiResConvert*)rg->getPassMgr()->registerPass(
        PASS_MULTI_RES_CVT);
    rm.addToRegionTab(rg);
    Var * srcv = rg->getVarMgr()->registerVar(
        "src", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * res1v = rg->getVarMgr()->registerVar(
        "res1", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * res2v = rg->getVarMgr()->registerVar(
        "res2", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * iv = rg->getVarMgr()->registerVar(
        "iv", rg->getTypeMgr()->getU32(), 1, VAR_LOCAL, SS_UNDEF);
    Var * dummyvar = rg->getVarMgr()->registerVar(
        "dummyvar", rg->getTypeMgr()->getAny(), 1, VAR_LOCAL|VAR_FAKE,
        SS_UNDEF);
    rg->addToVarTab(srcv);
    rg->addToVarTab(res1v);
    rg->addToVarTab(res2v);
    rg->addToVarTab(dummyvar);
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    Type const* u32 = rm.getTypeMgr()->getU32();
    Type const* b = rm.getTypeMgr()->getBool();
    Type const* any = rm.getTypeMgr()->getAny();

#if 1
    IR * res1 = rg->getIRMgr()->buildLoad(res1v, u8);
    IR * res2 = rg->getIRMgr()->buildLoad(res2v, u8);
    IR * reslst = nullptr;
    xcom::add_next(&reslst, res1);
    xcom::add_next(&reslst, res2);

    IR * res3 = rg->getIRMgr()->buildLoad(srcv, u8);
    xcom::add_next(&reslst, res3);

    IR * brd = cvt->getIRMgr()->buildBroadCast(
        rg->getIRMgr()->buildLoad(srcv), reslst, u8);
    xoc::note(rg, "\nBEFORE CALL:buildStoreWithMultiResAndConvertByExtract");
    xoc::dumpIRListH(brd, rg);
    //IR * brdstmt = cvt->buildStoreWithMultiResAndConvert(res2v, brd);
    //IR * brdstmt = cvt->buildStorePRWithMultiResAndConvert(any, brd);
    IR * brdstmt = cvt->buildStoreWithMultiResAndConvertByExtract(
        dummyvar, brd);
    xoc::note(rg, "\nAFTER CALL:buildStoreWithMultiResAndConvertByExtract");
    xoc::dumpIRListH(brdstmt, rg);
#else
    IR * brdstmt = nullptr;
    IR * nst1 = rg->getIRMgr()->buildStore(res1v,
        rg->getIRMgr()->buildLoad(srcv));
    IR * nst2 = rg->getIRMgr()->buildStore(res2v,
        rg->getIRMgr()->buildLoad(srcv));
    xcom::add_next(&brdstmt, nst1);
    xcom::add_next(&brdstmt, nst2);
    xoc::dumpIRListH(brdstmt, rg);
#endif

    IR * doloop = rg->getIRMgr()->buildDoLoop(
        rg->getIRMgr()->buildId(iv),
        rg->getIRMgr()->buildImmInt(1, u32),
        rg->getIRMgr()->buildBinaryOpSimp(IR_LE, b,
            rg->getIRMgr()->buildLoad(iv),
            rg->getIRMgr()->buildImmInt(100, u32)),
        rg->getIRMgr()->buildBinaryOpSimp(IR_ADD, u32,
            rg->getIRMgr()->buildLoad(iv),
            rg->getIRMgr()->buildImmInt(1, u32)),
        brdstmt);
    IR * newlst = nullptr;
    xcom::add_next(&newlst, doloop);

    IR * ret = rg->getIRMgr()->buildReturn(rg->getIRMgr()->
        buildLoad(res1v, u8));
    xcom::add_next(&newlst, ret);

    OptCtx oc(rg);
    rg->addToIRList(newlst);
    g_opt_level = OPT_LEVEL3;
    enableO3();
    rg->process(&oc);
}

static void testMultiRes4Loop2()
{
    ARMRegionMgr rm;
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initIRDescFlagSet();
    rm.initTargInfo();
    rm.getLogMgr()->init("test.log", false);
    g_dump_opt.is_dump_ir_id = false;
    xoc::g_do_rp = false;
    xoc::g_do_cp = true;
    xoc::g_do_cp_aggressive = true;
    xoc::g_do_dce = true;
    xoc::g_do_dce_aggressive = true;
    xoc::g_do_licm = true;
    xoc::g_do_gcse = true;
    xoc::g_do_rce = true;
    xoc::g_do_lftr = true;
    xoc::g_do_prssa = true;
    xoc::g_do_mdssa = true;
    xoc::g_infer_type = true;
    xoc::g_do_vect = true;
    xoc::g_do_cfg_opt = true;
    xoc::g_do_cfg_remove_empty_bb = true;
    xoc::g_do_cfg_remove_unreach_bb = true;
    xoc::g_do_cfg_remove_trampolin_bb = true;
    xoc::g_do_invert_brtgt = true;
    xoc::g_do_cfg_remove_redundant_branch = true;
    xoc::g_do_cfg_remove_trampolin_branch = true;
    xoc::g_do_cfg_remove_redundant_label = true;
    xoc::g_is_support_dynamic_type = true;
    Region * rg = rm.newRegion(REGION_FUNC);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    MultiResConvert * cvt = (MultiResConvert*)rg->getPassMgr()->registerPass(
        PASS_MULTI_RES_CVT);
    rm.addToRegionTab(rg);
    Var * srcv = rg->getVarMgr()->registerVar(
        "src", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * res1v = rg->getVarMgr()->registerVar(
        "res1", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * res2v = rg->getVarMgr()->registerVar(
        "res2", rg->getTypeMgr()->getI32(), 1, VAR_GLOBAL, SS_UNDEF);
    Var * iv = rg->getVarMgr()->registerVar(
        "iv", rg->getTypeMgr()->getU32(), 1, VAR_LOCAL, SS_UNDEF);
    Var * dummyvar = rg->getVarMgr()->registerVar(
        "dummyvar", rg->getTypeMgr()->getU32(), 1,
        VAR_LOCAL|VAR_FAKE, SS_UNDEF);
    rg->addToVarTab(srcv);
    rg->addToVarTab(res1v);
    rg->addToVarTab(res2v);
    rg->addToVarTab(dummyvar);
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    Type const* u32 = rm.getTypeMgr()->getU32();
    Type const* b = rm.getTypeMgr()->getBool();
    Type const* any = rm.getTypeMgr()->getAny();
    IR * res1 = rg->getIRMgr()->buildLoad(res1v, u8);
    IR * res2 = rg->getIRMgr()->buildLoad(res2v, u8);
    IR * reslst = nullptr;
    xcom::add_next(&reslst, res1);
    xcom::add_next(&reslst, res2);

    IR * res3 = rg->getIRMgr()->buildLoad(srcv, u32);
    xcom::add_next(&reslst, res3);

    xoc::note(rg, "\nBEFORE CALL:buildStoreWithMultiResAndConvertByExtract");
    IR * brd = cvt->getIRMgr()->buildBroadCast(
        rg->getIRMgr()->buildLoad(srcv), reslst, u8);
    xoc::dumpIRListH(brd, rg);

    //IR * brdstmt = cvt->buildStoreWithMultiResAndConvert(res2v, brd);
    //IR * brdstmt = cvt->buildStorePRWithMultiResAndConvert(any, brd);
    IR * brdstmt = cvt->buildStoreWithMultiResAndConvertByExtract(
        dummyvar, brd);
    xoc::note(rg, "\nAFTER CALL:buildStoreWithMultiResAndConvertByExtract");
    xoc::dumpIRListH(brdstmt, rg);

    IR * doloop = rg->getIRMgr()->buildDoLoop(
        rg->getIRMgr()->buildId(srcv),
        rg->getIRMgr()->buildImmInt(1, u32),
        rg->getIRMgr()->buildBinaryOpSimp(IR_LE, b,
            rg->getIRMgr()->buildLoad(srcv),
            rg->getIRMgr()->buildImmInt(100, u32)),
        rg->getIRMgr()->buildBinaryOpSimp(IR_ADD, u32,
            rg->getIRMgr()->buildLoad(srcv),
            rg->getIRMgr()->buildImmInt(1, u32)),
        brdstmt);
    IR * newlst = nullptr;
    xcom::add_next(&newlst, doloop);

    IR * ret = rg->getIRMgr()->buildReturn(rg->getIRMgr()->
        buildLoad(res1v, u8));
    xcom::add_next(&newlst, ret);

    OptCtx oc(rg);
    rg->addToIRList(newlst);
    g_opt_level = OPT_LEVEL3;
    enableO3();
    rg->process(&oc);
}

void testMultiRes()
{
    testMultiRes1();
    testMultiRes2();
    testMultiRes2_2();
    testMultiRes3();
    testMultiRes4Loop();
    testMultiRes4Loop2();
}

int main()
{
    testMultiRes();
    return 0;
}
