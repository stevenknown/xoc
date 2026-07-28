#include "../../header_for_xgen.h"
#include "../../enable_opt.h"

static void testLinearRep()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("aa.tmp", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    Region *rg = rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    IRMgr * irmgr = rg->getIRMgr();
    VarMgr * varmgr = rg->getVarMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    Type const *u8 = rm.getTypeMgr()->getU8();
    Type const *u16 = rm.getTypeMgr()->getU16();
    Type const *u32 = rm.getTypeMgr()->getU32();
    Var * k = varmgr->registerVar("k", u32, 1, VAR_LOCAL, SS_UNDEF);
    Var * a0 = varmgr->registerVar("a0", u32, 1, VAR_LOCAL, SS_UNDEF);
    Var * a1 = varmgr->registerVar("a1", u32, 1, VAR_LOCAL, SS_UNDEF);
    {
        //3*k+a0
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a0, u32));
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e1, k, lr, ctx);
        ASSERT0(succ);
        ASSERT0(lr.getCoeff()->is_const() &&
                CONST_int_val(lr.getCoeff()) == 3);
        ASSERT0(lr.getVar(rg) == k);
        ASSERT0(lr.getAddend()->is_ld() &&
                lr.getAddend()->getIdinfo() == a0);
    }
    {
        //(3*k+a0)*(a1)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a0, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                e1,
                irmgr->buildLoad(a1, u32));
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        //COEFF:
        //    mul:u32 id:19
        //        ld:u32 'a1' id:17
        //        intconst:u32 3|0x3 id:18
        //* VAR:
        //    ld:u32 'k' id:9
        //+ ADDEND:
        //    mul:u32 id:22
        //        ld:u32 'a0' id:21
        //        ld:u32 'a1' id:20
        bool succ = lrmgr.inferAndConstructLinearRep(e2, k, lr, ctx);
        ASSERT0(succ);
        ASSERT0(lr.getCoeff()->is_mul() &&
                BIN_opnd0(lr.getCoeff())->is_ld() &&
                BIN_opnd0(lr.getCoeff())->getIdinfo() == a1 &&
                BIN_opnd1(lr.getCoeff())->is_const() &&
                CONST_int_val(BIN_opnd1(lr.getCoeff())) == 3);
        ASSERT0(lr.getVar(rg) == k);
        ASSERT0(lr.getAddend()->is_mul() &&
                BIN_opnd0(lr.getAddend())->is_ld() &&
                BIN_opnd0(lr.getAddend())->getIdinfo() == a0 &&
                BIN_opnd1(lr.getAddend())->is_ld() &&
                BIN_opnd1(lr.getAddend())->getIdinfo() == a1);
    }
    {
        //(a1)*(3*k+a0)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a0, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildLoad(a1, u32),
                e1);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e2, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(succ);
        ASSERT0(lr.getCoeff()->is_mul() &&
                BIN_opnd0(lr.getCoeff())->is_ld() &&
                BIN_opnd0(lr.getCoeff())->getIdinfo() == a1 &&
                BIN_opnd1(lr.getCoeff())->is_const() &&
                CONST_int_val(BIN_opnd1(lr.getCoeff())) == 3);
        ASSERT0(lr.getVar(rg) == k);
        ASSERT0(lr.getAddend()->is_mul() &&
                BIN_opnd0(lr.getAddend())->is_ld() &&
                BIN_opnd0(lr.getAddend())->getIdinfo() == a1 &&
                BIN_opnd1(lr.getAddend())->is_ld() &&
                BIN_opnd1(lr.getAddend())->getIdinfo() == a0);
    }
    {
        //(a1)*(3*k+0)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildImmInt(0, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildLoad(a1, u32),
                e1);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        //COEFF:
        //    mul:u32 id:19
        //        ld:u32 'a1' id:17
        //        intconst:u32 3|0x3 id:18
        //* VAR:
        //    ld:u32 'k' id:9
        bool succ = lrmgr.inferAndConstructLinearRep(e2, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(succ);
        ASSERT0(lr.getCoeff()->is_mul() &&
                BIN_opnd0(lr.getCoeff())->is_ld() &&
                BIN_opnd0(lr.getCoeff())->getIdinfo() == a1 &&
                BIN_opnd1(lr.getCoeff())->is_const() &&
                CONST_int_val(BIN_opnd1(lr.getCoeff())) == 3);
        ASSERT0(lr.getVar(rg) == k);
        ASSERT0(lr.getAddend() == nullptr);
    }
    {
        //(a1)*(3*k)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildLoad(a1, u32),
                e1);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e2, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(succ);
        ASSERT0(lr.getCoeff()->is_mul() &&
                BIN_opnd0(lr.getCoeff())->is_ld() &&
                BIN_opnd0(lr.getCoeff())->getIdinfo() == a1 &&
                BIN_opnd1(lr.getCoeff())->is_const() &&
                CONST_int_val(BIN_opnd1(lr.getCoeff())) == 3);
        ASSERT0(lr.getVar(rg) == k);
        ASSERT0(lr.getAddend() == nullptr);
    }
    {
        //(3*k)*(2*k)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(k, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(!succ);
    }
    {
        //(3*k+a1)*(2*k)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a1, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(k, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(!succ);
    }
    {
        //(3*k+a1)*(2*k+a0)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a1, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a0, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(!succ);
    }
    {
        //(3*k)*(2*k+a0)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildLoad(a0, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(!succ);
    }
    {
        //(3*k+8)*(2*k)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildImmInt(8, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(k, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(!succ);
    }
    {
        //(3*k+8)*(2*a0)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildImmInt(8, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(a0, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(succ);
        //COEFF:
        //    mul:u32 id:140
        //        ld:u32 'a0' id:141
        //        intconst:u32 6|0x6 id:145
        //* VAR:
        //    ld:u32 'k' id:127
        //+ ADDEND:
        //    mul:u32 id:144
        //        ld:u32 'a0' id:146
        //        intconst:u32 16|0x10 id:148
        ASSERT0(lr.getCoeff()->is_mul() &&
                BIN_opnd0(lr.getCoeff())->is_ld() &&
                BIN_opnd0(lr.getCoeff())->getIdinfo() == a0 &&
                BIN_opnd1(lr.getCoeff())->is_const() &&
                CONST_int_val(BIN_opnd1(lr.getCoeff())) == 6);
        ASSERT0(lr.getVar(rg) == k);
        ASSERT0(lr.getAddend()->is_mul() &&
                BIN_opnd0(lr.getAddend())->is_ld() &&
                BIN_opnd0(lr.getAddend())->getIdinfo() == a0 &&
                BIN_opnd1(lr.getAddend())->is_const() &&
                CONST_int_val(BIN_opnd1(lr.getAddend())) == 16);

    }
    {
        //(3*k+8)*(2*a0+9)
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildImmInt(8, u32));
        IR * e2 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(a0, u32)),
            irmgr->buildImmInt(9, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        ASSERT0(succ);
        //COEFF:
        //    mul:u32 id:173
        //        add:u32 id:167
        //            mul:u32 id:168
        //                ld:u32 'a0' id:169
        //                intconst:u32 2|0x2 id:170
        //            intconst:u32 9|0x9 id:171
        //        intconst:u32 3|0x3 id:172
        //* VAR:
        //    ld:u32 'k' id:149
        //+ ADDEND:
        //    mul:u32 id:180
        //        add:u32 id:174
        //            mul:u32 id:175
        //                ld:u32 'a0' id:176
        //                intconst:u32 2|0x2 id:177
        //            intconst:u32 9|0x9 id:178
        //        intconst:u32 8|0x8 id:179
        IR const* mul = lr.getCoeff();
        ASSERT0(mul->is_mul() &&
                BIN_opnd0(mul)->is_add() &&
                BIN_opnd1(mul)->is_const() &&
                CONST_int_val(BIN_opnd1(mul)) == 3);
        IR * add = BIN_opnd0(mul);
        ASSERT0(add->is_add() &&
                BIN_opnd0(add)->is_mul() &&
                BIN_opnd1(add)->is_const() &&
                CONST_int_val(BIN_opnd1(add)) == 9);
        IR * mul2 = BIN_opnd0(add);
        ASSERT0(mul2->is_mul() &&
                BIN_opnd0(mul2)->is_ld() &&
                BIN_opnd0(mul2)->getIdinfo() == a0 &&
                BIN_opnd1(mul2)->is_const() &&
                CONST_int_val(BIN_opnd1(mul2)) == 2);
        ASSERT0(lr.getVar(rg) == k);

        IR const* mul3 = lr.getAddend();
        ASSERT0(mul3->is_mul() &&
                BIN_opnd0(mul3)->is_add() &&
                BIN_opnd1(mul3)->is_const() &&
                CONST_int_val(BIN_opnd1(mul3)) == 8);
        IR * add2 = BIN_opnd0(mul3);
        ASSERT0(add2->is_add() &&
                BIN_opnd0(add2)->is_mul() &&
                BIN_opnd1(add2)->is_const() &&
                CONST_int_val(BIN_opnd1(add2)) == 9);
        IR * mul4 = BIN_opnd0(add2);
        ASSERT0(mul4->is_mul() &&
                BIN_opnd0(mul4)->is_ld() &&
                BIN_opnd0(mul4)->getIdinfo() == a0 &&
                BIN_opnd1(mul4)->is_const() &&
                CONST_int_val(BIN_opnd1(mul4)) == 2);
    }
    {
        //(a1)*(3*a0)
        IR * e1 = irmgr->buildLoad(a1, u32);
        IR * e2 = irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(3, u32),
                irmgr->buildLoad(a0, u32));
        IR * e3 = irmgr->buildBinaryOpSimp(IR_MUL, u32, e1, e2);
        OptCtx oc(rg);
        //rg->addToIRList(lst);
        //rg->process(&oc);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e3, k, lr, ctx);
        //lr.dump(rg);
        //NOCOEFF
        //NOVAR
        //+ ADDEND:
        //    mul:u32 id:193        
        //        ld:u32 'a1' id:192        
        //        mul:u32 id:189            
        //            ld:u32 'a0' id:190            
        //            intconst:u32 3|0x3 id:191
        ASSERT0(succ);
        ASSERT0(lr.getCoeff() == nullptr);
        ASSERT0(!lr.hasVar());
        IR const* mul = lr.getAddend();
        ASSERT0(mul->is_mul() &&
                BIN_opnd0(mul)->is_ld() &&
                BIN_opnd0(mul)->getIdinfo() == a1 &&
                BIN_opnd1(mul)->is_mul());
        IR * mul2 = BIN_opnd1(mul);
        ASSERT0(mul2->is_mul() &&
                BIN_opnd0(mul2)->is_ld() &&
                BIN_opnd0(mul2)->getIdinfo() == a0 &&
                BIN_opnd1(mul2)->is_const() &&
                CONST_int_val(BIN_opnd1(mul2)) == 3);
    }
}

static void testChainRec3(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    { //{1,+,1} * {1,+,1} * {1,+,1} = {1,+,7,+,12,+,6}
    ChainRec cr1(IVVal(HOST_INT(1), u32), IVVal(HOST_INT(1), u32));
    ChainRec cr2;
    bool succ = mgr.doMul(cr1, cr1, cr2);
    ASSERT0(succ);
    succ = mgr.doMul(cr2, cr1, cr2);
    ASSERT0(succ);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(7), u32),
                           IVVal(HOST_INT(12), u32),
                           IVVal(HOST_INT(6), u32)));
    }
}

static void testChainRec2(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    ChainRec cr2;
    //x={0,+,1}, f(x)=x^3 + 2*x^2 + 3*x + 7
    { //x^3 = {0,+,1} * {0,+,1} * {0,+,1} = {0,+,1,+,6,+,6}
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    bool succ = mgr.doMul(cr1, cr1, cr2);
    ASSERT0(succ);
    succ = mgr.doMul(cr2, cr1, cr2);
    ASSERT0(succ);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(6), u32),
                           IVVal(HOST_INT(6), u32)));
    }

    ChainRec cr3;
    { //2*x^2 = 2*{0,+,1}*{0,+,1} = {0,+,2,+,4}
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    bool succ = mgr.doMul(cr1, HOST_INT(2), cr3);
    ASSERT0(succ);
    succ = mgr.doMul(cr1, cr3, cr3);
    ASSERT0(succ);
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(2), u32),
                           IVVal(HOST_INT(4), u32)));
    }

    ChainRec cr4;
    { //3*x+7 = 3*{0,+,1}+7 = {7,+,3}
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    bool succ = mgr.doMul(cr1, HOST_INT(3), cr4);
    ASSERT0(succ);
    succ = mgr.doAdd(cr4, HOST_INT(7), cr4);
    ASSERT0(succ);
    ASSERT0(cr4.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(7), u32),
                           IVVal(HOST_INT(3), u32)));
    }

    ChainRec cr5;
    { //3*x = 3*{0,+,1} = {0,+,3}
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    bool succ = mgr.doMul(cr1, HOST_INT(3), cr5);
    ASSERT0(succ);
    ASSERT0(cr5.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(3), u32)));
    }

    ChainRec cr6;
    {
    //x^3 + 2*x^2 = {0,+,3,+,10,+,6}
    bool succ = mgr.doAdd(cr2, cr3, cr6);
    ASSERT0(succ);
    ASSERT0(cr6.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(3), u32),
                           IVVal(HOST_INT(10), u32),
                           IVVal(HOST_INT(6), u32)));
    }

    {
    //x^3 + 2*x^2 + 3*x = {0,+,1,+,6,+,6}
    ChainRec cr7;
    bool succ = mgr.doAdd(cr6, cr5, cr7);
    ASSERT0(succ);
    ASSERT0(cr7.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(6), u32),
                           IVVal(HOST_INT(10), u32),
                           IVVal(HOST_INT(6), u32)));
    }

    bool succ = mgr.doAdd(cr2, cr3, cr3);
    ASSERT0(succ);
    succ = mgr.doAdd(cr3, cr4, cr4);
    ASSERT0(succ);
    ASSERT0(cr4.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(7), u32),
                           IVVal(HOST_INT(6), u32),
                           IVVal(HOST_INT(10), u32),
                           IVVal(HOST_INT(6), u32)));
}


static void testChainRec1(Region * rg, ChainRecMgr & mgr, Var * a0,
                          Type const* f32, Type const* u32)
{
    IRMgr * irmgr = rg->getIRMgr();
    { //12 + {7,+,3} = {19,+,3}
    ChainRec cr2(IVVal(HOST_INT(7), u32), IVVal(HOST_INT(3), u32));
    ChainRec cr3;
    bool succ = mgr.doAdd(cr2, (HOST_INT)12, cr3);
    ASSERT0(succ);
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(19), u32),
                           IVVal(HOST_INT(3), u32)));
    }

    { //12 * {7,+,3} = {19,+,3}
    ChainRec cr2(IVVal(HOST_INT(7), u32), IVVal(HOST_INT(3), u32));
    ChainRec cr3;
    bool succ = mgr.doMul(cr2, (HOST_INT)12, cr3);
    ASSERT0(succ);
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(84), u32),
                           IVVal(HOST_INT(36), u32)));
    }

    { //{7,+,3} + {1,+,1}
    ChainRec cr1(IVVal(HOST_INT(7), u32), IVVal(HOST_INT(3), u32));
    ChainRec cr2(IVVal(HOST_INT(1), u32), IVVal(HOST_INT(1), u32));
    ChainRec cr3;
    bool succ = mgr.doAdd(cr1, cr2, cr3);
    ASSERT0(succ);
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(8), u32),
                           IVVal(HOST_INT(4), u32)));
    }

    { //{0,+,1} * {0,+,1}
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    ChainRec cr2(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    ChainRec cr3;
    bool succ = mgr.doMul(cr1, cr2, cr3);
    ASSERT0(succ);
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(2), u32)));
    }

    { //{0,+,1,+,6,+,6} + {0,+,2,+,4} = {0,+,3,+,10,+,6}
    ChainRec cr1(IVVal(HOST_INT(6), u32), IVVal(HOST_INT(6), u32));
    ChainRec cr2(IVVal(HOST_INT(1), u32), IVVal(&cr1, u32));
    ChainRec cr3(IVVal(HOST_INT(0), u32), IVVal(&cr2, u32));

    ChainRec cr4(IVVal(HOST_INT(2), u32), IVVal(HOST_INT(4), u32));
    ChainRec cr5(IVVal(HOST_INT(0), u32), IVVal(&cr4, u32));
 
    ChainRec cr6;
    bool succ = mgr.doAdd(cr3, cr5, cr6);
    ASSERT0(succ);
    ASSERT0(cr6.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(3), u32),
                           IVVal(HOST_INT(10), u32),
                           IVVal(HOST_INT(6), u32)));
    }

    { //{0,+,3,+,10,+,6} + {7,+,3} = {7,+,6,+,10,+,6}
    ChainRec cr1(IVVal(HOST_INT(10), u32), IVVal(HOST_INT(6), u32));
    ChainRec cr2(IVVal(HOST_INT(3), u32), IVVal(&cr1, u32));
    ChainRec cr3(IVVal(HOST_INT(0), u32), IVVal(&cr2, u32));

    ChainRec cr4(IVVal(HOST_INT(7), u32), IVVal(HOST_INT(3), u32));
 
    ChainRec cr5;
    bool succ = mgr.doAdd(cr3, cr4, cr5);
    ASSERT0(succ);
    ASSERT0(cr5.isEqual(rg->getIRMgr(), 4, IVVal(HOST_INT(7), u32),
                           IVVal(HOST_INT(6), u32),
                           IVVal(HOST_INT(10), u32),
                           IVVal(HOST_INT(6), u32)));
    }

    {
    //{x,+,1}*{x,+,1}-1, x is loop invariant exp. => {x^2-1,+,x+x+1,+,2}
    IVVal x(irmgr->buildLoad(a0, u32));
    ChainRec cr1(x, IVVal(HOST_INT(1), u32), IR_ADD);
    bool succ = mgr.doMul(cr1, cr1, cr1);
    ASSERT0(succ);
    bool succ2 = mgr.doSub(cr1, (HOST_INT)1, cr1);
    ASSERT0(succ2);

    IR const* s1initexp = irmgr->buildBinaryOpSimp(
        IR_SUB, u32,
        irmgr->buildBinaryOpSimp(
            IR_MUL, u32,
            irmgr->buildLoad(a0, u32),
            irmgr->buildLoad(a0, u32)),
        irmgr->buildImmInt(1, u32));
    IR const* s2initexp = irmgr->buildBinaryOpSimp(
        IR_ADD, u32,
        irmgr->buildBinaryOpSimp(
            IR_ADD, u32,
            irmgr->buildLoad(a0, u32),
            irmgr->buildLoad(a0, u32)),
        irmgr->buildImmInt(1, u32));
    ASSERT0(cr1.isEqual(rg->getIRMgr(), 3, IVVal(s1initexp),
                           IVVal(s2initexp),
                           IVVal(HOST_INT(2), u32)));
    }

    {
    //{x-1,+,1}*{x+1,+,1}, x is loop invariant exp. => {x^2-1,+,x+x+1,+,2}
    IVVal x_sub_1(irmgr->buildBinaryOpSimp(IR_SUB, u32,
        irmgr->buildLoad(a0, u32), irmgr->buildImmInt(1,u32)));
    ChainRec cr1(x_sub_1, IVVal(HOST_INT(1), u32), IR_ADD);

    IVVal x_add_1(irmgr->buildBinaryOpSimp(IR_ADD, u32,
        irmgr->buildLoad(a0, u32), irmgr->buildImmInt(1,u32)));
    ChainRec cr2(x_add_1, IVVal(HOST_INT(1), u32), IR_ADD);

    ChainRec cr3;
    bool succ = mgr.doMul(cr1, cr2, cr3);
    ASSERT0(succ);

    IR const* s1initexp = irmgr->buildBinaryOpSimp(
        IR_MUL, u32,
        irmgr->buildBinaryOpSimp(
            IR_SUB, u32,
            irmgr->buildLoad(a0, u32),
            irmgr->buildImmInt(1, u32)),
        irmgr->buildBinaryOpSimp(
            IR_ADD, u32,
            irmgr->buildLoad(a0, u32),
            irmgr->buildImmInt(1, u32)));

    IR const* s2initexp =
        irmgr->buildBinaryOpSimp(
            IR_ADD, u32,
                irmgr->buildBinaryOpSimp(
                    IR_ADD, u32,
                    irmgr->buildBinaryOpSimp(
                        IR_SUB, u32,
                            irmgr->buildLoad(a0, u32),
                            irmgr->buildImmInt(1, u32)),
                    irmgr->buildBinaryOpSimp(
                        IR_ADD, u32,
                            irmgr->buildLoad(a0, u32),
                            irmgr->buildImmInt(1, u32))),
                irmgr->buildImmInt(1, u32));
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 3, IVVal(s1initexp),
                           IVVal(s2initexp),
                           IVVal(HOST_INT(2), u32)));
    }

    { //1 - {1,+,2} ==> {0,+,-2}
    ChainRec cr1(IVVal(HOST_INT(1), u32), IVVal(HOST_INT(2), u32));
    ChainRec cr2;
    bool succ = mgr.doSub((HOST_INT)1, cr1, cr2);
    ASSERT0(succ);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(-2), u32)));
    }

    { //1.5 - {1.1,+,2.2} ==> {0.4,+,-2.2}
    ChainRec cr1(IVVal(HOST_FP(1.1), f32), IVVal(HOST_FP(2.2), f32));
    ChainRec cr2;
    bool succ = mgr.doSub((HOST_FP)1.5, cr1, cr2);
    ASSERT0(succ);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 2, IVVal(HOST_FP(0.4), f32),
                           IVVal(HOST_FP(-2.2), f32)));
    }

    { //a0 - {1.1,+,2.2} ==> {a0-1.1,+,-2.2}
    ChainRec cr1(IVVal(HOST_FP(1.1), f32), IVVal(HOST_FP(2.2), f32));
    ChainRec cr2;
    IR * exp = irmgr->buildLoad(a0, f32);
    bool succ = mgr.doSub(exp, cr1, cr2);
    ASSERT0(succ);
    IR const* s1initexp =
        irmgr->buildBinaryOpSimp(
            IR_ADD, u32,
                irmgr->buildLoad(a0, f32),
                irmgr->buildImmFP(-1.1, f32));
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 2, IVVal(s1initexp),
                           IVVal((HOST_FP)-2.2, f32)));
    }
}


static void testChainRec4(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    { //(i+1)*(i+1) - i*i - 2*i ==> 1
    //(i+1)*(i+1)
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(1), u32));
    ChainRec cr2;
    bool succ = mgr.doAdd(cr1, (HOST_INT)1, cr2);
    ASSERT0(succ);
    bool succ2 = mgr.doMul(cr2, cr2, cr2);
    ASSERT0(succ2);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(3), u32),
                           IVVal(HOST_INT(2), u32)));
    //i*i
    ChainRec cr3;
    bool succ3 = mgr.doMul(cr1, cr1, cr3);
    ASSERT0(succ3);
    ASSERT0(cr3.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(2), u32)));
    //2*i
    ChainRec cr4;
    bool succ4 = mgr.doMul(cr1, (HOST_INT)2, cr4);
    ASSERT0(succ4);
    ASSERT0(cr4.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(0), u32),
                           IVVal(HOST_INT(2), u32)));

    //i*i + 2*i ==> 0,+,3,+,2
    ChainRec cr10;
    bool succ15 = mgr.doAdd(cr3, cr4, cr10);
    ASSERT0(succ15);
    ASSERT0(cr10.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(0), u32),
                            IVVal(HOST_INT(3), u32),
                            IVVal(HOST_INT(2), u32)));
    ChainRec cr16;
    bool succ7 = mgr.doSub(cr2, cr10, cr16);
    ASSERT0(succ7);
    ASSERT0(cr16.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(1), u32),
                            IVVal(HOST_INT(0), u32)));

    //(i+1)*(i+1) - i*i - 2*i
    ChainRec cr5;
    bool succ5 = mgr.doSub(cr2, cr3, cr5);
    ASSERT0(succ5);
    ASSERT0(cr5.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(2), u32)));
    ChainRec cr6;
    bool succ6 = mgr.doSub(cr5, cr4, cr6);
    ASSERT0(succ6);
    IVVal res6(&cr6, cr6.getInit().getDType());
    mgr.refine(res6);
    ASSERT0(cr6.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(1), u32),
                           IVVal(HOST_INT(0), u32)));
    ASSERT0(res6.getInt() == 1);
    }
}


static void testChainRecCompVal(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    {
    //{7,+,6,+,10,+,6}
    ChainRec cr3(IVVal(HOST_INT(10), u32), IVVal(HOST_INT(6), u32));
    ChainRec cr2(IVVal(HOST_INT(6), u32), IVVal(&cr3, u32));
    ChainRec cr1(IVVal(HOST_INT(7), u32), IVVal(&cr2, u32));
    //cr1.dumpComputedValue(mgr);
    IVValVec valvec;
    bool succ = mgr.computeValue(cr1, 7, valvec);
    ASSERT0(succ);
    ASSERT0(valvec.get(0).getInt() == 7);
    ASSERT0(valvec.get(1).getInt() == 13);
    ASSERT0(valvec.get(2).getInt() == 29);
    ASSERT0(valvec.get(3).getInt() == 61);
    ASSERT0(valvec.get(4).getInt() == 115);
    ASSERT0(valvec.get(5).getInt() == 197);
    ASSERT0(valvec.get(6).getInt() == 313);
    }

    {
    //{1,*,2,+,1}
    ChainRec cr2(IVVal(HOST_INT(2), u32), IVVal(HOST_INT(1), u32), IR_ADD);
    ChainRec cr1(IVVal(HOST_INT(1), u32), IVVal(&cr2, u32), IR_MUL);
    //cr1.dumpComputedValue(mgr);
    IVValVec valvec;
    bool succ = mgr.computeValue(cr1, 7, valvec);
    ASSERT0(succ);
    ASSERT0(valvec.get(0).getInt() == 1);
    ASSERT0(valvec.get(1).getInt() == 2);
    ASSERT0(valvec.get(2).getInt() == 6);
    ASSERT0(valvec.get(3).getInt() == 24);
    ASSERT0(valvec.get(4).getInt() == 120);
    ASSERT0(valvec.get(5).getInt() == 720);
    }

    {
    //{0,+,1,*,2}
    ChainRec cr2(IVVal(HOST_INT(1), u32), IVVal(HOST_INT(2), u32), IR_MUL);
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(&cr2, u32), IR_ADD);
    //cr1.dumpComputedValue(mgr);
    IVValVec valvec;
    bool succ = mgr.computeValue(cr1, 7, valvec);
    ASSERT0(succ);
    ASSERT0(valvec.get(0).getInt() == 0);
    ASSERT0(valvec.get(1).getInt() == 1);
    ASSERT0(valvec.get(2).getInt() == 3);
    ASSERT0(valvec.get(3).getInt() == 7);
    ASSERT0(valvec.get(4).getInt() == 15);
    ASSERT0(valvec.get(5).getInt() == 31);
    ASSERT0(valvec.get(6).getInt() == 63);
    }

    {
    //{1,*,1,+,1}
    ChainRec cr2(IVVal(HOST_INT(1), u32), IVVal(HOST_INT(1), u32), IR_ADD);
    ChainRec cr1(IVVal(HOST_INT(1), u32), IVVal(&cr2, u32), IR_MUL);
    //cr1.dumpComputedValue(mgr);
    IVValVec valvec;
    bool succ = mgr.computeValue(cr1, 7, valvec);
    ASSERT0(succ);
    ASSERT0(valvec.get(0).getInt() == 1);
    ASSERT0(valvec.get(1).getInt() == 1);
    ASSERT0(valvec.get(2).getInt() == 2);
    ASSERT0(valvec.get(3).getInt() == 6);
    ASSERT0(valvec.get(4).getInt() == 24);
    ASSERT0(valvec.get(5).getInt() == 120);
    }
}


static void testChainRecComputeByLinRep(Region * rg, ChainRecMgr & mgr,
                                        Type const* u32)
{
    {
        Var * k = rg->getVarMgr()->registerVar(
            "k", u32, 1, VAR_LOCAL, SS_UNDEF);
        //2*k+3
        IRMgr * irmgr = rg->getIRMgr();
        IR * e1 = irmgr->buildBinaryOpSimp(IR_ADD, u32,
            irmgr->buildBinaryOpSimp(IR_MUL, u32,
                irmgr->buildImmInt(2, u32),
                irmgr->buildLoad(k, u32)),
            irmgr->buildImmInt(3, u32));
        OptCtx oc(rg);
        LinearRepMgr lrmgr(rg, oc);
        LRInferCtx ctx;
        LinearRep lr;
        bool succ = lrmgr.inferAndConstructLinearRep(e1, k, lr, ctx);
        ASSERT0(succ);
        //{2,+,5}
        ChainRec cr1(IVVal(HOST_INT(2), u32), IVVal(HOST_INT(5), u32));
        ChainRec cr2;
        cr2.computeByLinRep(lr, cr1, mgr);
        ASSERT0(cr2.isEqual(rg->getIRMgr(), 2, IVVal(HOST_INT(7), u32),
                               IVVal(HOST_INT(10), u32)));
    }
}


static void testChainRecMulExp(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    IRMgr * irmgr = rg->getIRMgr();
    { //$x * {$y*$z,+,{0,+,0}} ==> {$x*$y*$z,+,0} ==> $x*$y*$z
    IR * x = irmgr->buildPR(u32);
    IR * y_mul_z = irmgr->buildBinaryOpSimp(IR_MUL, u32,
        irmgr->buildPR(u32),
        irmgr->buildPR(u32));
    IR * y = BIN_opnd0(y_mul_z);
    IR * z = BIN_opnd1(y_mul_z);
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(0), u32));
    ChainRec cr2(IVVal(y_mul_z), IVVal(&cr1, u32));
    bool succ = mgr.doMul(cr2, x, cr2);
    ASSERT0(succ);

    IR const* s1initexp = irmgr->buildBinaryOpSimp(
        IR_MUL, u32,
        irmgr->buildPRdedicated(x->getPrno(), u32),
        irmgr->buildBinaryOpSimp(
            IR_MUL, u32,
            irmgr->buildPRdedicated(y->getPrno(), u32),
            irmgr->buildPRdedicated(z->getPrno(), u32)));
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 2, IVVal(s1initexp),
                           IVVal(HOST_INT(0), u32)));
    IVVal cr2_val;
    bool succ2 = mgr.refine(cr2, cr2_val);
    ASSERT0(succ2);
    ASSERT0(!cr2.isSanity() && cr2_val.isEqual(s1initexp, rg->getIRMgr()));
    }
}


static void testChainRec5(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    { //{1,+,1} * {3,+,2}
    ChainRec cr1(IVVal(HOST_INT(1), u32), IVVal(HOST_INT(1), u32));
    ChainRec cr2(IVVal(HOST_INT(3), u32), IVVal(HOST_INT(2), u32));
    bool succ = mgr.doMul(cr1, cr2, cr2);
    ASSERT0(succ);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(3), u32),
                           IVVal(HOST_INT(7), u32),
                           IVVal(HOST_INT(4), u32)));
    }

    { //{3,+,7,+,4} + {1,+,3,+,2} + {3,+,2}
      //= {4,+,10,+,6} + {3,+,2}
      //= {7,+,12,+,6}
    ChainRec cr1(IVVal(HOST_INT(7), u32), IVVal(HOST_INT(4), u32));
    ChainRec cr2(IVVal(HOST_INT(3), u32), IVVal(&cr1, u32));

    ChainRec cr3(IVVal(HOST_INT(3), u32), IVVal(HOST_INT(2), u32));
    ChainRec cr4(IVVal(HOST_INT(1), u32), IVVal(&cr3, u32));

    ChainRec cr5(IVVal(HOST_INT(3), u32), IVVal(HOST_INT(2), u32));
    bool succ = mgr.doAdd(cr2, cr4, cr2);
    ASSERT0(succ);
    succ = mgr.doAdd(cr2, cr5, cr2);
    ASSERT0(succ);
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 3, IVVal(HOST_INT(7), u32),
                           IVVal(HOST_INT(12), u32),
                           IVVal(HOST_INT(6), u32)));
    }
}


static void testChainRecMulVar(Region * rg, ChainRecMgr & mgr, Type const* u32)
{
    IRMgr * irmgr = rg->getIRMgr();
    { //Var($x) * {$y*$z,+,{0,+,0}} ==> {$x*$y*$z,+,0} ==> $x*$y*$z
    Var * x = rg->getVarMgr()->registerVar(
        "x", u32, 1, VAR_LOCAL|VAR_IS_PR, SS_UNDEF);
    MD const* xmd = rg->getMDMgr()->genMDForVar(x, 0);
    IR * y_mul_z = irmgr->buildBinaryOpSimp(IR_MUL, u32,
        irmgr->buildPR(u32),
        irmgr->buildPR(u32));
    IR * y = BIN_opnd0(y_mul_z);
    IR * z = BIN_opnd1(y_mul_z);
    ChainRec cr1(IVVal(HOST_INT(0), u32), IVVal(HOST_INT(0), u32));
    ChainRec cr2(IVVal(y_mul_z), IVVal(&cr1, u32));
    IR const* pr_x = irmgr->buildPRdedicated(x->getPrno(), u32);
    bool succ = mgr.doMul(cr2, IVVal(xmd, pr_x, u32), cr2);
    ASSERT0(succ);

    IR const* s1initexp = irmgr->buildBinaryOpSimp(
        IR_MUL, u32,
        irmgr->buildPRdedicated(x->getPrno(), u32),
        irmgr->buildBinaryOpSimp(
            IR_MUL, u32,
            irmgr->buildPRdedicated(y->getPrno(), u32),
            irmgr->buildPRdedicated(z->getPrno(), u32)));
    ASSERT0(cr2.isEqual(rg->getIRMgr(), 2, IVVal(s1initexp),
                           IVVal(HOST_INT(0), u32)));
    IVVal cr2_val;
    bool succ2 = mgr.refine(cr2, cr2_val);
    ASSERT0(succ2);
    ASSERT0(!cr2.isSanity() && cr2_val.isEqual(s1initexp, rg->getIRMgr()));
    }
}


static void testChainRec()
{
    enableO3();
    g_opt_level = OPT_LEVEL3;
    xoc::g_dump_opt.is_dump_all = true;
    ARMRegionMgr rm;
    rm.getLogMgr()->init("aa.tmp", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    rm.initTargInfo();
    g_dump_opt.is_dump_ir_id = true;
    Region *rg = rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    IRMgr * irmgr = rg->getIRMgr();
    VarMgr * varmgr = rg->getVarMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    PassMgr * passmgr = rg->getPassMgr();
    IVR * ivr = (IVR*)passmgr->registerPass(PASS_IVR);
    ASSERT0(ivr);
    Type const *u8 = rm.getTypeMgr()->getU8();
    Type const *u16 = rm.getTypeMgr()->getU16();
    Type const *u32 = rm.getTypeMgr()->getU32();
    Type const *f32 = rm.getTypeMgr()->getF32();
    Var * k = varmgr->registerVar("k", u32, 1, VAR_LOCAL, SS_UNDEF);
    Var * a0 = varmgr->registerVar("a0", u32, 1, VAR_LOCAL, SS_UNDEF);
    Var * a1 = varmgr->registerVar("a1", u32, 1, VAR_LOCAL, SS_UNDEF);
    OptCtx * oc = rm.getAndGenOptCtx(rg);
    ChainRecMgr mgr(rg, oc, ivr);
    testChainRec1(rg, mgr, a0, f32, u32);
    testChainRec2(rg, mgr, u32);
    testChainRec3(rg, mgr, u32);
    testChainRec4(rg, mgr, u32);
    testChainRec5(rg, mgr, u32);
    testChainRecCompVal(rg, mgr, u32);
    testChainRecComputeByLinRep(rg, mgr, u32);
    testChainRecMulExp(rg, mgr, u32);
    testChainRecMulVar(rg, mgr, u32);
}

static void testStrTabOption()
{
    RegionMgr rm;
    rm.getLogMgr()->init("aa.tmp", true);
    {
    StrTabOption a;
    a.addString("a,b,c,");
    a.addString(",,,");
    a.addString(",dd,ee, ,ff.");
    a.addString(" mm nn .,-,.+,@,##,!,...");
    ASSERT0(!a.find(".."));
    ASSERT0(!a.find("  "));
    ASSERT0(!a.find("ff"));
    ASSERT0(!a.find("."));
    ASSERT0(a.find("..."));
    ASSERT0(a.find(" "));
    ASSERT0(a.find("a"));
    ASSERT0(a.find("b"));
    ASSERT0(a.find("c"));
    ASSERT0(a.find("dd"));
    ASSERT0(a.find("ee"));
    ASSERT0(a.find("ff."));
    ASSERT0(a.find(" mm nn ."));
    ASSERT0(a.find("-"));
    ASSERT0(a.find(".+"));
    ASSERT0(a.find("@"));
    ASSERT0(a.find("##"));
    ASSERT0(a.find("!"));
    a.remove("a");
    a.remove("...");
    ASSERT0(!a.find("a"));
    ASSERT0(!a.find("..."));
    a.remove("...");
    a.remove(" ");
    a.remove("a");
    a.remove("b");
    a.remove("c");
    a.remove("dd");
    a.remove("ee");
    a.remove("ff.");
    a.remove(" mm nn .");
    a.remove("-");
    a.remove(".+");
    a.remove("@");
    a.remove("##");
    a.remove("!");
    ASSERT0(!a.find("..."));
    ASSERT0(!a.find(" "));
    ASSERT0(!a.find("a"));
    ASSERT0(!a.find("b"));
    ASSERT0(!a.find("c"));
    ASSERT0(!a.find("dd"));
    ASSERT0(!a.find("ee"));
    ASSERT0(!a.find("ff."));
    ASSERT0(!a.find(" mm nn ."));
    ASSERT0(!a.find("-"));
    ASSERT0(!a.find(".+"));
    ASSERT0(!a.find("@"));
    ASSERT0(!a.find("##"));
    ASSERT0(!a.find("!"));

    }
}
 
//This file tests the utils functions in src/opt/ directory.
static void testOptUtils()
{
    testStrTabOption();
}

static void testExpononet()
{
    ARMRegionMgr rm;
    rm.getLogMgr()->init("aa.tmp", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    g_dump_opt.is_dump_all = true;
    ARMRegion *rg = (ARMRegion*)rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    {
        Var * rgvar = rm.getVarMgr()->registerVar(
            "firstrg", rm.getTypeMgr()->getAny(), 1, VAR_GLOBAL, SS_UNDEF);
        rgvar->setFlag(VAR_IS_FUNC);
        rg->setRegionVar(rgvar);
        //CGMgr * cgmgr = xgen::allocCGMgr(&rm);
        //rm.setCGMgr(cgmgr);
    }
    IRMgr * irmgr = rg->getIRMgr();
    VarMgr * varmgr = rg->getVarMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    Type const* u32 = rm.getTypeMgr()->getU32();
    Type const* i32 = rm.getTypeMgr()->getI32();
    Type const* f32 = rm.getTypeMgr()->getF32();
    Type const* f64 = rm.getTypeMgr()->getF64();
    Var * k = varmgr->registerVar("k", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * x = varmgr->registerVar("x", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * n = varmgr->registerVar("n", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * a0 = varmgr->registerVar("a0", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * a1 = varmgr->registerVar("a1", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * gg = varmgr->registerVar("g", i32, 1, VAR_GLOBAL, SS_UNDEF);
    {
        IR * e0 = irmgr->buildBinaryOpSimp(
            IR_EXPONENT, f32,
            irmgr->buildImmFP(xcom::Float::getE(), f64),
            irmgr->buildImmInt(0, i32));
        xoc::g_opt_level = OPT_LEVEL3;
        xoc::g_do_opt_float = true;
        IR * stmt = irmgr->buildStore(gg, e0);
        Refine * refine = (Refine*)rg->getPassMgr()->
            registerPass(PASS_REFINE);
        OptCtx oc(rg);
        RefineCtx rc(&oc);
        bool change;
        IR * e1 = refine->refineExpression(e0, change, rc);
        IR const* anti = irmgr->buildImmFP(1, f64);
        ASSERT0(anti->isIREqual(e1, irmgr, true));
    }
    {
        IR * e0 = irmgr->buildBinaryOpSimp(
            IR_EXPONENT, f32,
            irmgr->buildImmFP(xcom::Float::getE(), f64),
            irmgr->buildImmInt(1, i32));
        xoc::g_opt_level = OPT_LEVEL3;
        xoc::g_do_opt_float = true;
        IR * stmt = irmgr->buildStore(gg, e0);
        Refine * refine = (Refine*)rg->getPassMgr()->
            registerPass(PASS_REFINE);
        OptCtx oc(rg);
        RefineCtx rc(&oc);
        bool change;
        IR * e1 = refine->refineExpression(e0, change, rc);
        IR const* anti = irmgr->buildImmFP(xcom::Float::getE(), f64);
        ASSERT0(anti->isIREqual(e1, irmgr, true));
    }
}


static void testLog()
{
    ARMRegionMgr rm;
    rm.getLogMgr()->init("aa.tmp", true);
    rm.initTypeMgr();
    rm.initVarMgr();
    g_dump_opt.is_dump_all = true;
    ARMRegion *rg = (ARMRegion*)rm.newRegion(REGION_FUNC);
    rm.addToRegionTab(rg);
    rg->initPassMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    {
        Var * rgvar = rm.getVarMgr()->registerVar(
            "firstrg", rm.getTypeMgr()->getAny(), 1, VAR_GLOBAL, SS_UNDEF);
        rgvar->setFlag(VAR_IS_FUNC);
        rg->setRegionVar(rgvar);
        //CGMgr * cgmgr = xgen::allocCGMgr(&rm);
        //rm.setCGMgr(cgmgr);
    }
    IRMgr * irmgr = rg->getIRMgr();
    VarMgr * varmgr = rg->getVarMgr();
    TypeMgr * tymgr = rg->getTypeMgr();
    Type const* u8 = rm.getTypeMgr()->getU8();
    Type const* u16 = rm.getTypeMgr()->getU16();
    Type const* u32 = rm.getTypeMgr()->getU32();
    Type const* i32 = rm.getTypeMgr()->getI32();
    Type const* f32 = rm.getTypeMgr()->getF32();
    Type const* f64 = rm.getTypeMgr()->getF64();
    Var * k = varmgr->registerVar("k", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * x = varmgr->registerVar("x", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * n = varmgr->registerVar("n", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * a0 = varmgr->registerVar("a0", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * a1 = varmgr->registerVar("a1", i32, 1, VAR_GLOBAL, SS_UNDEF);
    Var * gg = varmgr->registerVar("g", i32, 1, VAR_GLOBAL, SS_UNDEF);
    {
        IR * e0 = irmgr->buildBinaryOp(
            IR_LOG, f32, irmgr->buildLoad(x), irmgr->buildLoad(x));
        xoc::g_opt_level = OPT_LEVEL3;
        xoc::g_do_opt_float = true;
        Refine * refine = (Refine*)rg->getPassMgr()->
            registerPass(PASS_REFINE);
        OptCtx oc(rg);
        RefineCtx rc(&oc);
        bool change;
        IR * e1 = refine->refineExpression(e0, change, rc);
        IR const* anti = irmgr->buildImmFP(1, f32);
        ASSERT0(anti->isIREqual(e1, irmgr, true));
    }
}


//This file tests the IR operation.
static void testIROp()
{
    testExpononet();
    testLog();
}

static void testPlugins()
{
    testLinearRep();
    testChainRec();
    testOptUtils();
    testIROp();
}

int main()
{
    testPlugins();
    return 0;
}
