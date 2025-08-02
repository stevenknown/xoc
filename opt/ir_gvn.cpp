/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

class VNTypeDesc {
public:
    VN_TYPE vt;
    CHAR const* vt_name;

public:
    static CHAR const* getVTName(VN_TYPE vt);
};

static VNTypeDesc g_vntype_desc[] = {
    { VN_UNKNOWN, "", },
    { VN_OP, "OP", },
    { VN_MDDEF, "MDDEF", },
    { VN_VMD, "VMD", },
    { VN_VAR, "VAR", },
    { VN_INT, "INT", },
    { VN_FP, "FP", },
    { VN_STR, "STR", },
    { VN_MC_INT, "MC_INT", },
    { VN_CONST, "CONST", },
    { VN_NUM, "", },
};


CHAR const* VNTypeDesc::getVTName(VN_TYPE vt)
{
    ASSERT0(vt > VN_UNKNOWN && vt < VN_NUM);
    return g_vntype_desc[vt].vt_name;
}


static void dumpFailedReason(
    IR const* ir, Region const* rg, InferCtx const& ctx, CHAR const* reason)
{
    if (!rg->isLogMgrInit()) { return; }
    if (ctx.getActMgr() == nullptr) { return; }
    ASSERT0(reason);
    xcom::DefFixedStrBuf buf;
    ctx.getActMgr()->dump(
        "InferEVN:can not infer out EVN for %s, because %s",
        xoc::dumpIRName(ir, buf), reason);
}


static void dumpFailedInferEVNForCVT(
    IR const* ir, InferEVN const* evn, InferCtx const& ctx)
{
    ASSERT0(evn);
    ASSERT0(ir->is_cvt());
    if (!evn->getRegion()->isLogMgrInit()) { return; }
    if (ctx.getActMgr() == nullptr) { return; }
    xcom::DefFixedStrBuf ty1;
    evn->getTypeMgr()->dump_type(ir->getType(), ty1);
    xcom::DefFixedStrBuf ty2;
    evn->getTypeMgr()->dump_type(CVT_exp(ir)->getType(), ty2);
    xcom::DefFixedStrBuf reason;
    reason.strcat(
        "convert %s to %s might change EVN", ty2.getBuf(), ty1.getBuf());
    dumpFailedReason(ir, evn->getRegion(), ctx, reason.getBuf());
}


//
//START VN
//
void VN::dump(Region const* rg, OUT xcom::StrBuf & buf) const
{
    class Dump : public xoc::DumpToBuf {
    public:
        VN const* vn;
    public:
        Dump(Region const* rg, xcom::StrBuf & buf) : DumpToBuf(rg, buf) {}

        //User defined dump behaviors.
        virtual void dumpUserInfo() const override
        { vn->dump(getRegion()); }
    };
    Dump dumpbuf(rg, buf);
    dumpbuf.vn = this;
    dumpbuf.dump();
}


void VN::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    prt(rg, "VN%u,%s", id(), VNTypeDesc::getVTName(getType()));
    switch (getType()) {
    case VN_OP:
    case VN_MC_INT:
    case VN_CONST:
        break;
    case VN_MDDEF: {
        MDDef const* mddef = getVNMDDef();
        ASSERT0(mddef);
        prt(rg, ":");
        mddef->dump(rg);
        break;
    }
    case VN_VMD: {
        VMD const* vmd = getVNVMD();
        ASSERT0(vmd);
        xcom::FixedStrBuf<32> tbuf;
        prt(rg, ":%s", vmd->dump(tbuf));
        break;
    }
    case VN_INT: {
        Type const* d = rg->getTypeMgr()->getHostIntType();
        prt(rg, ":");
        xoc::dumpHostInteger(getVNIntVal(), d, rg, rg->getTypeMgr(), false);
        break;
    }
    case VN_FP: {
        Type const* d = rg->getTypeMgr()->getHostFPType();
        prt(rg, ":");
        xoc::dumpHostFP(getVNFPVal(), d, rg, rg->getTypeMgr());
        break;
    }
    case VN_STR: {
        Sym const* sym = getVNStrVal();
        ASSERT0(sym);
        ASSERT0(sym->getStr());
        prt(rg, ":'%s'", sym->getStr());
        break;
    }
    case VN_VAR: {
        Var const* var = getVNVar();
        ASSERT0(var);
        xcom::StrBuf buf(16);
        VarMgr const* vm = rg->getVarMgr();
        ASSERT0(vm);
        prt(rg, ":", var->dump(buf, vm));
        break;
    }
    default: UNREACHABLE();
    }
}
//END VN


//
//START IRCAndVNHash
//
IRCAndVNHash::~IRCAndVNHash()
{
}


VN const* IRCAndVNHash::registerVN(IR_CODE irt, UINT vnnum, ...)
{
    ASSERT0(vnnum >= 1);
    va_list ptr;
    va_start(ptr, vnnum);
    UINT i = 0;
    IntList ilst;
    ilst.append_head((IntType)irt);
    for (IntType vnid = va_arg(ptr, IntType);
         i < vnnum; vnid = va_arg(ptr, IntType), i++) {
        ASSERT0(vnid != VNID_UNDEF);
        ilst.append_tail(vnid);
    }
    va_end(ptr);
    VN * newvn = registerVN(ilst);
    VN_type(newvn) = VN_OP;
    VN_op(newvn) = irt;
    return newvn;
}


VN const* IRCAndVNHash::registerVN(IR_CODE irt, MOD IntList & ilst)
{
    ASSERT0(irt <= IR_CODE_NUM); //NOTE: IR_CODE_NUM means MDPhi.
    ilst.append_head((IntType)irt);
    VN * newvn = registerVN(ilst);
    ASSERT0(newvn);
    VN_type(newvn) = VN_OP;
    VN_op(newvn) = irt;
    return newvn;
}


VN * IRCAndVNHash::registerVN(IntList const& ilst)
{
    VN * vn = nullptr;
    m_intset2vn.find(ilst, vn);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    m_intset2vn.set(ilst, newvn);
    return newvn;
}


void IRCAndVNHash::clean()
{
    m_intset2vn.clean();
}


void IRCAndVNHash::dump(Region const* rg, UINT indent) const
{
    if (!rg->isLogMgrInit()) { return; }
    m_intset2vn.dump(rg->getLogMgr()->getFileHandler(), indent);
}
//END IRCAndVNHash


//
//START InferEVN
//
InferEVN::InferEVN(GVN * gvn) : m_gvn(gvn), m_ircvnhash(m_gvn)
{
    ASSERT0(gvn);
    m_rg = gvn->getRegion();
    ASSERT0(m_rg);
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
}


void InferEVN::clean()
{
    m_irid2vn.clean();
    m_prno2vn.clean();
    m_vmd2vn.clean();
    m_mdphi2vn.clean();
    m_ircvnhash.clean();
}


void InferEVN::dumpIR2VN() const
{
    if (m_irid2vn.get_elem_count() == 0) { return; }
    VN const* x = nullptr;
    note(getRegion(), "\n-- IR2VN --");
    IR2VNIter it1;
    for (UINT id = m_irid2vn.get_first(it1, &x);
         x != nullptr; id = m_irid2vn.get_next(it1, &x)) {
        IR const* ir = getRegion()->getIR(id);
        ASSERT0(ir);
        note(getRegion(), "\n%s:", DumpIRName().dump(ir));
        x->dump(m_rg);
    }
}


void InferEVN::dumpForTest() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    note(getRegion(), "\n==-- DUMP InferEVN --==");
    dumpIR2VN();
}


void InferEVN::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    note(getRegion(), "\n==-- DUMP InferEVN --==");
    m_rg->getLogMgr()->incIndent(2);
    dumpIR2VN();

    note(getRegion(), "\n-- PRNO2VN --");
    PRNO2VNIter it4;
    VN const* x = nullptr;
    for (PRNO prno = m_prno2vn.get_first(it4, &x);
         x != nullptr; prno = m_prno2vn.get_next(it4, &x)) {
        ASSERT0(prno != PRNO_UNDEF);
        note(getRegion(), "\n$%u:", prno);
        x->dump(m_rg);
    }

    note(getRegion(), "\n-- MDPhi2VN --");
    MDPhi2VNIter it2;
    for (UINT id = m_mdphi2vn.get_first(it2, &x);
         x != nullptr; id = m_mdphi2vn.get_next(it2, &x)) {
        note(getRegion(), "\nmdphi%u:", id);
        x->dump(m_rg);
    }

    note(getRegion(), "\n-- VMD2VN --");
    VMD2VNIter it3;
    for (UINT id = m_vmd2vn.get_first(it3, &x);
         x != nullptr; id = m_irid2vn.get_next(it3, &x)) {
        VMD const* vmd = m_mdssamgr->getVMD(id);
        ASSERT0(vmd && vmd->is_md());
        note(getRegion(), "\n");
        vmd->dump(m_rg);
        note(getRegion(), ":");
        x->dump(m_rg);
    }

    note(getRegion(), "\n-- IRCAndVNHash --");
    m_ircvnhash.dump(m_rg, m_rg->getLogMgr()->getIndent());
    m_rg->getLogMgr()->decIndent(2);
}


VN const* InferEVN::allocVNForExtStmt(IR const* ir, InferCtx & ctx)
{
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    VN_type(newvn) = VN_OP;
    VN_op(newvn) = ir->getCode();
    setVN(ir, newvn);
    return newvn;
}


VN const* InferEVN::allocVNForMDDef(MDDef const* mddef, InferCtx & ctx)
{
    VN const* vn = getVN(mddef);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    VN_type(newvn) = VN_MDDEF;
    VN_mddef(newvn) = mddef;
    setVN(mddef, newvn);
    return newvn;
}


VN const* InferEVN::allocVNForVMD(VMD const* vmd, InferCtx & ctx)
{
    VN const* vn = getVN(vmd);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    VN_type(newvn) = VN_VMD;
    VN_vmd(newvn) = vmd;
    setVN(vmd, newvn);
    return newvn;
}


VN const* InferEVN::allocVNForPRNO(PRNO prno)
{
    ASSERT0(prno != PRNO_UNDEF);
    VN const* vn = getVN(prno);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    VN_type(newvn) = VN_OP;
    VN_op(newvn) = IR_PR;
    setVN(prno, newvn);
    return newvn;
}


VN const* InferEVN::allocVNForStmt(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_PR:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_CALL:
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    SWITCH_CASE_UNCONDITIONAL_BRANCH_OP:
    case IR_RETURN:
    case IR_REGION: {
        VN const* vn = getVN(ir);
        if (vn != nullptr) { return vn; }
        VN * newvn = m_gvn->allocVN();
        VN_type(newvn) = VN_OP;
        VN_op(newvn) = ir->getCode();
        setVN(ir, newvn);
        return newvn;
    }
    default: allocVNForExtStmt(ir, ctx);
    }
    return nullptr;
}


VN const* InferEVN::inferVNViaBaseAndOfst(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->isIndirectMemOp() || ir->isArrayOp());
    ASSERTN(getVN(ir) == nullptr,
            ("has to check VN before invoke the function"));
    IR const* base = ir->getBase();
    ASSERT0(base);
    VN const* basevn = inferExp(base, ctx);
    if (basevn == nullptr) { return nullptr; }

    //Register VN by ir's kid and its offset.
    VN const* ofstvn = inferIntConst(ir->getOffset());
    ASSERT0(ofstvn);
    VN const* vn = m_ircvnhash.registerVN(
        ir->getCode(), 2, (VNHashInt)basevn->id(), (VNHashInt)ofstvn->id());
    ASSERT0(vn);
    setVN(ir, vn);
    return vn;
}


IR_CODE InferEVN::mapMDDef2IRCode(MDDef const* mddef) const
{
    ASSERT0(mddef->is_phi());
    return IR_CODE_NUM;
}


VN const* InferEVN::inferMDPhi(MDPhi const* phi, InferCtx & ctx)
{
    ASSERT0(phi->is_phi());
    VN const* vn = getVN(phi);
    if (vn != nullptr) { return vn; }
    if (ctx.isVisited(phi)) {
        return allocVNForMDDef(phi, ctx);
    }
    ctx.setVisited(phi);
    IRCAndVNHash::IntList ilst;
    for (IR const* id = phi->getOpndList();
         id != nullptr; id = id->get_next()) {
        VN const* vn = inferExp(id, ctx);
        if (vn == nullptr) { return nullptr; }
        ilst.append_tail((VNHashInt)vn->id());
    }
    //CASE:Do NOT move the getVN before the iteration of PHI's operands.
    //Because PHI's VN might be generated during the iteration.
    VN const* newvn = getVN(phi);
    if (newvn != nullptr) {
        //CASE:The inference processing encountered a cycle when infering the
        //second operand MD17v2 of phi. The inference meets the 'phi' first,
        //then perform inference to each operands of phi. When
        //operand-inference returned, 'phi' may have already assigned a VN if
        //there exist a DefUse chain cycle of MD17.
        //e.g:MD17v1 = MDPhi(MD17v0, MD17v2);
        //    MD17v2 = NEG(MD17v1);
        return newvn;
    }
    newvn = m_ircvnhash.registerVN(mapMDDef2IRCode(phi), ilst);
    ASSERT0(newvn);
    setVN(phi, newvn);
    return newvn;
}


VN const* InferEVN::inferIndirectStmt(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir->isIndirectMemOp());
    if (ctx.isVisited(ir)) { return getVN(ir); }
    ctx.setVisited(ir);
    VN const* vn1 = inferExp(ir->getRHS(), ctx);
    if (vn1 == nullptr) { return nullptr; }
    VN const* vn2 = inferExp(ir->getBase(), ctx);
    if (vn2 == nullptr) { return nullptr; }

    //Register VN by ir's kid and its offset.
    VN const* ofstvn = inferIntConst(ir->getOffset());
    ASSERT0(ofstvn);
    VN const* vn = m_ircvnhash.registerVN(
        ir->getCode(), 3, (VNHashInt)vn1->id(),
        (VNHashInt)vn2->id(), (VNHashInt)ofstvn->id());
    ASSERT0(vn);

    //Stmt ir's VN might be generated while inferring expression.
    VN const* stmtvn = getVN(ir);
    if (stmtvn == nullptr) {
        setVN(ir, vn);
        return vn;
    }
    return stmtvn;
}


VN const* InferEVN::inferDirectStmt(IR const* ir, InferCtx & ctx)
{
    if (ctx.isVisited(ir)) { return getVN(ir); }
    ctx.setVisited(ir);
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    vn = inferExp(ir->getRHS(), ctx);

    //Stmt ir's VN might be generated while inferring expression.
    VN const* stmtvn = getVN(ir);
    if (vn != nullptr && stmtvn == nullptr) {
        setVN(ir, vn);
        return vn;
    }
    return stmtvn;
}


VN const* InferEVN::inferStmt(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT:
        return inferDirectStmt(ir, ctx);
    SWITCH_CASE_INDIRECT_MEM_STMT:
        return inferIndirectStmt(ir, ctx);
    case IR_GETELEM:
    case IR_SETELEM:
    case IR_PHI:
    SWITCH_CASE_EXT_WRITE_PR:
        return inferVNByIterKid(ir, ctx);
    SWITCH_CASE_WRITE_ARRAY:
        return inferWriteArray(ir, ctx);
    SWITCH_CASE_CALL:
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    SWITCH_CASE_UNCONDITIONAL_BRANCH_OP:
    case IR_RETURN:
    case IR_REGION:
        UNREACHABLE();
        return nullptr;
    default: inferExtStmt(ir, ctx);
    }
    return nullptr;
}


VN const* InferEVN::inferLiveinVMDForDirectExp(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isDirectMemOp() || ir->is_id());
    ASSERT0(useMDSSADU());
    MD const* mustref = ir->getMustRef();
    if (mustref == nullptr) {
        //Can not evaluate correct VN for 'ir'.
        return nullptr;
    }
    MDSSAInfo const* mdssainfo = MDSSAMgr::getMDSSAInfoIfAny(ir);
    ASSERT0(mdssainfo && !mdssainfo->isEmptyVOpndSet());
    VMD const* mustref_vmd = (VMD*)mdssainfo->getVOpndForMD(
        mustref->id(), m_mdssamgr);
    ASSERTN(mustref_vmd, ("unmatched mdssainfo to ir"));
    if (!mustref_vmd->isLiveIn()) {
        //Can not evaluate correct VN for 'ir'.
        return nullptr;
    }
    return allocVNForVMD(mustref_vmd, ctx);
}


VN const* InferEVN::inferDirectExpViaMDPhi(
    IR const* ir, MDDef const* mdssadef, InferCtx & ctx)
{
    ASSERT0(mdssadef->is_phi());
    MD const* mustref = ir->getMustRef();
    ASSERT0(mustref);
    if (!mustref->is_exact()) { return nullptr; }
    if (mdssadef->getResult()->mdid() != mustref->id()) {
        //CASE: MDPhi7:VMD59:MD6V7 <- (ID id:475 MD6V5 BB6),
        //      (ID id:476 MD6V9 BB14)
        //      ...=ld:i32:offset(4):storage_space(stack) 'listSmall' MD25
        //MD6 cover MD25, however in order to infer out the correct EVN,
        //these two MD must be exactly equal.
        dumpFailedReason(ir, m_rg, ctx,
            "the nearest Def is MDPhi whose result-MD is not equal to "
            "ir's MustRef");
        return nullptr;
    }

    //Infer Phi.
    VN const* phivn = nullptr;
    if (allowCrossPhi()) {
        phivn = inferMDPhi((MDPhi const*)mdssadef, ctx);
    }
    if (phivn != nullptr) {
        ASSERT0(getVN(mdssadef) == phivn);
        setVN(ir, phivn);
        return phivn;
    }
    //Allocate VN for Phi.
    phivn = allocVNForMDDef(mdssadef, ctx);
    ASSERT0(phivn);
    setVN(ir, phivn);
    return phivn;
}


VN const* InferEVN::inferDirectExpViaMDSSA(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isDirectMemOp() || ir->isReadPR() || ir->is_id());
    ASSERTN(getVN(ir) == nullptr, ("caller should avoid this case"));
    if (!useMDSSADU()) { return nullptr; }
    MDDef const* mdssadef = m_mdssamgr->findNearestDef(ir, true);
    if (mdssadef == nullptr) {
        return inferLiveinVMDForDirectExp(ir, ctx);
    }
    if (mdssadef->is_phi()) {
        return inferDirectExpViaMDPhi(ir, mdssadef, ctx);
    }
    IR const* occ = mdssadef->getOcc();
    ASSERT0(occ);
    if (occ->getMustRef() != ir->getMustRef()) {
        return nullptr;
    }
    //For now, nearest Dom Def is the exact-def of 'ir', we attempt to
    //allocate VN for Dom Def.
    VN const* kdefvn = allocVNForMDDef(mdssadef, ctx);
    ASSERT0(kdefvn);
    setVN(ir, kdefvn);
    return kdefvn;
}


VN const* InferEVN::inferDirectExpViaPRSSA(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->isReadPR());
    if (!usePRSSADU()) { return nullptr; }
    IR const* prssadef = m_prssamgr->findKillingDefStmt(ir);
    if (prssadef != nullptr) {
        ASSERT0(prssadef->is_stmt());
        return allocVNForStmt(prssadef, ctx);
    }
    ASSERT0(ir->getSSAInfo());
    IR const* d = ir->getSSAInfo()->getDef();
    if (d == nullptr) {
        //ir is LiveIn PR.
        return allocVNForPRNO(ir->getPrno());
    }
    //Can NOT determine PR's VN by DU chain.
    return nullptr;
}


VN const* InferEVN::inferDirectExpViaSSA(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_exp());
    if (ir->isReadPR()) {
        return inferDirectExpViaPRSSA(ir, ctx);
    }
    ASSERT0(ir->isDirectMemOp() || ir->is_id());
    return inferDirectExpViaMDSSA(ir, ctx);
}


VN const* InferEVN::inferAndGenVNForKillingDef(
    IR const* exp, IR const* killdef, InferCtx & ctx)
{
    ASSERT0(exp && killdef);
    ASSERT0(exp->is_exp() && killdef->is_stmt());
    ASSERT0(killdef->hasResult());
    VN const* kdefvn = getVN(killdef);
    if (kdefvn != nullptr) {
        setVN(exp, kdefvn);
        return kdefvn;
    }
    kdefvn = inferStmt(killdef, ctx);
    if (kdefvn != nullptr) {
        ASSERT0(getVN(killdef) == kdefvn);
        setVN(exp, kdefvn);
        return kdefvn;
    }
    ASSERT0(getVN(killdef) == nullptr);
    ASSERT0(getVN(exp) == nullptr);
    if (killdef->is_phi()) {
        //PHI's VN should be inferred by its operands.
        return nullptr;
    }
    //Allocate a dedicated VN for given killdef.
    kdefvn = allocVNForStmt(killdef, ctx);
    ASSERT0(kdefvn);
    setVN(exp, kdefvn);
    return kdefvn;
}


VN const* InferEVN::inferDirectExp(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isDirectMemOp() || ir->isReadPR() || ir->is_id());
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    IR * kdef = xoc::findKillingDef(ir, m_rg);
    if (kdef == nullptr) {
        return inferDirectExpViaSSA(ir, ctx);
    }
    ASSERT0(kdef->hasResult());
    ASSERT0(ir->getMustRef());
    ASSERT0(kdef->getMustRef());
    if (ir->getMustRef() != kdef->getMustRef()) {
        //CASE:ieee/930529-1.c
        //MD12 -- base:Var14(d):local,align(8),mc,mem_size:8,storage_space:
        //        stack,decl:'' -- ofst:0 -- size:1
        //MD13 -- base:Var14(d):local,align(8),mc,mem_size:8,storage_space:
        //        stack,decl:'' -- ofst:1 -- size:1
        //stpr $8:u8 id:8 attachinfo:Dbx
        //    ld:u8:storage_space(stack) 'd' id:7 attachinfo:MDSSA
        //  ----
        //  EMD12 : MD11,MD12
        //stpr $9:u8 id:16 attachinfo:Dbx
        //    ld:u8:offset(1):storage_space(stack) 'd' id:15 attachinfo:MDSSA
        //  ----
        //  EMD13 : MD11,MD13
        //Because the base is same, but the must ref DM is different due to
        //different offset, which means these two loads cannot share the
        //same VN.
        return nullptr;
    }
    return inferAndGenVNForKillingDef(ir, kdef, ctx);
}


VN const* InferEVN::inferArrayKidOp(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->isArrayOp() && (ir->is_stmt() || ir->is_exp()));
    VN const* basevn = inferExp(ir->getBase(), ctx);
    if (basevn == nullptr) { return nullptr; }
    IRCAndVNHash::IntList lst;
    lst.append_head((VNHashInt)basevn->id());
    for (IR const* sub = ARR_sub_list(ir);
         sub != nullptr; sub = sub->get_next()) {
        VN const* subvn = inferExp(sub, ctx);
        if (subvn == nullptr) { return nullptr; }
        lst.append_tail((VNHashInt)subvn->id());
    }
    return m_ircvnhash.registerVN(ir->getCode(), lst);
}


VN const* InferEVN::inferWriteArray(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir && ir->is_stmt());
    if (ctx.isVisited(ir)) { return getVN(ir); }
    ctx.setVisited(ir);
    VN const* kidvn = inferArrayKidOp(ir, ctx);
    if (kidvn == nullptr) { return nullptr; }
    VN const* rhsvn = inferExp(ir->getRHS(), ctx);
    if (rhsvn == nullptr) { return nullptr; }

    //Register VN by ir's kid and its offset.
    VN const* ofstvn = inferIntConst(ir->getOffset());
    ASSERT0(ofstvn);
    VN const* vn = m_ircvnhash.registerVN(
        ir->getCode(), 3, (VNHashInt)kidvn->id(),
        (VNHashInt)rhsvn->id(), (VNHashInt)ofstvn->id());
    ASSERT0(vn);

    //Stmt ir's VN might be generated while inferring expression.
    VN const* stmtvn = getVN(ir);
    if (stmtvn == nullptr) {
        setVN(ir, vn);
        return vn;
    }
    return stmtvn;
}


VN const* InferEVN::inferIntConst(HOST_INT val)
{
    return m_gvn->computeIntConst(val);
}


VN const* InferEVN::inferConst(IR const* ir, InferCtx & ctx)
{
    bool change;
    VN const* vn = m_gvn->computeConst(ir, change);
    if (vn != nullptr) {
        setVN(ir, vn);
    }
    return vn;
}


VN const* InferEVN::inferVNViaArrayKidAndOfst(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->isArrayOp());
    ASSERTN(getVN(ir) == nullptr,
            ("has to check VN before invoke the function"));
    VN const* kidvn = inferArrayKidOp(ir, ctx);
    if (kidvn == nullptr) { return nullptr; }

    //Register VN by ir's kid and its offset.
    VN const* ofstvn = inferIntConst(ir->getOffset());
    ASSERT0(ofstvn);
    VN const* newvn = m_ircvnhash.registerVN(
        ir->getCode(), 2, (VNHashInt)kidvn->id(),
        (VNHashInt)ofstvn->id());
    ASSERT0(newvn);
    setVN(ir, newvn);
    return newvn;
}


VN const* InferEVN::inferArray(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->isArrayOp() && ir->is_exp());
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    IR * kdef = xoc::findKillingDef(ir, m_rg);
    if (kdef == nullptr) {
        //CASE:exec/evn.c
        //...=arr($1,$x) #S1
        //starr($2,$y)=...
        //...=arr($1,$x) #S2
        //VN of arr($1,$x) in #S1 can NOT be inferred through $1 and ARR code
        //because starr($2,$y) may alias with arr($1,$x).
        //return inferVNViaArrayKidAndOfst(ir, ctx);
        return nullptr;
    }
    return inferAndGenVNForKillingDef(ir, kdef, ctx);
}


VN const* InferEVN::inferIndirectMemExp(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->isIndirectMemOp() && ir->is_exp());
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    IR * kdef = xoc::findKillingDef(ir, m_rg);
    if (kdef == nullptr) {
        //CASE:exec/evn.c
        //...=ild($1) #S1
        //ist($2)=...
        //...=ild($1) #S2
        //VN of ild($1) in #S1 can NOT be inferred through $1 and ILD code
        //because ist($2) may alias with ild($1).
        //return inferVNViaBaseAndOfst(ir, ctx);
        return nullptr;
    }
    ASSERT0(kdef->hasResult());
    ASSERT0(ir->getMustRef());
    ASSERT0(kdef->getMustRef());
    if (ir->getMustRef() != kdef->getMustRef()) {
        //CASE:exec/pr58726.c
        //MD10 -- base:Var11(b):global,hasInitVal,array,addr_taken,align(4),
        //        byte(0x0,0x0,0x0,0x0),mc,mem_size:4,storage_space:global,
        //        decl:'' -- ofst:0 -- size:4
        //MD56 -- base:Var11(b):global,hasInitVal,array,addr_taken,align(4),
        //        byte(0x0,0x0,0x0,0x0),mc,mem_size:4,storage_space:global,
        //        decl:'' -- ofst:0 -- size:1
        //ist:u32:storage_space(any) id:10 attachinfo:Dbx,MDSSA
        //    $14:*<1> id:9
        //    $5:u32 id:8
        //  ----
        //  EMD10 : MD2,MD10,MD15,MD56
        //stpr $18:u8 id:11 attachinfo:Dbx
        //    ild:u8:storage_space(any) id:7 attachinfo:MDSSA
        //        $14:*<1> id:1
        //  ----
        //  EMD56 : MD2,MD10,MD15,MD56
        //Because the base is same, but the must ref DM is different due to
        //different size, which means these two indirect loads cannot share
        //the same VN.
        return nullptr;
    }
    return inferAndGenVNForKillingDef(ir, kdef, ctx);
}


VN const* InferEVN::inferExtExp(IR const* ir, InferCtx & ctx)
{
    //Target Dependent Code.
    return inferVNByIterKid(ir, ctx);
}


VN const* InferEVN::inferExtStmt(IR const* ir, InferCtx & ctx)
{
    if (ctx.isVisited(ir)) { return getVN(ir); }
    ctx.setVisited(ir);
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    return inferVNByIterKid(ir, ctx);
}


VN const* InferEVN::inferVNByIterKid(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->is_stmt() || ir->is_exp());
    if (ir->is_stmt()) {
        if (ctx.isVisited(ir)) { return getVN(ir); }
        ctx.setVisited(ir);
    }
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    IRCAndVNHash::IntList lst;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        for (IR const* x = kid; x != nullptr; x = x->get_next()) {
            VN const* vn = inferExp(x, ctx);
            if (vn == nullptr) {
                return nullptr;
            }
            ASSERT0(!vn->is_unknown());
            lst.append_tail((VNHashInt)vn->id());
        }
    }
    vn = m_ircvnhash.registerVN(ir->getCode(), lst);
    ASSERT0(vn);

    //ir may stmt or exp, its VN might be generated while inferring expression.
    VN const* irvn = getVN(ir);
    if (irvn == nullptr) {
        setVN(ir, vn);
        return vn;
    }
    return irvn;
}


VN const* InferEVN::registerCvtVN(VN const* v0, Type const* srcty,
    Type const* tgtty)
{
    ASSERT0(v0 && srcty && tgtty);
    ASSERT0(srcty->is_scalar());
    ASSERT0(tgtty->is_scalar());
    VN * srctyvn = m_gvn->registerVNviaINT(srcty->getDType());
    VN * tgttyvn = m_gvn->registerVNviaINT(tgtty->getDType());
    return registerCvtVN(v0, srctyvn, tgttyvn);
}


VN const* InferEVN::registerCvtVN(VN const* v0, VN const* v1, VN const* v2)
{
    ASSERT0(v0 && v1 && v2);
    return m_ircvnhash.registerVN(IR_CVT, 3, v0->id(), v1->id(), v2->id());
}


VN const* InferEVN::inferCvt(IR const* cvt, InferCtx & ctx)
{
    ASSERT0(cvt->is_cvt());
    VN const* cvtvn = getVN(cvt);
    if (cvtvn != nullptr) { return cvtvn; }
    IR const* cvtexp = CVT_exp(cvt);
    VN const* x = inferExp(cvtexp, ctx);
    if (!m_gvn->isCvtChangeVN(cvt)) {
        if (x != nullptr) {
            setVN(cvt, x);
        }
        return x;
    }
    if (x == nullptr) { return nullptr; }
    if (!cvt->getType()->is_scalar() || !cvtexp->getType()->is_scalar()) {
        dumpFailedInferEVNForCVT(cvt, this, ctx);
        return nullptr;
    }
    VN const* newvn = m_gvn->registerCvtVN(
        x, cvt->getType(), cvtexp->getType());
    ASSERT0(newvn);
    setVN(cvt, newvn);
    return newvn;
}


VN const* InferEVN::inferExp(IR const* ir, InferCtx & ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_exp());
    if (ir->is_cvt()) { return inferCvt(ir, ctx); }
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
    case IR_ID: {
        //Althrough IR_ID does not represent a real operation, it still
        //take participate in EVN analysis because it may describe the
        //possible VN live-in MDPhi.
        return inferDirectExp(ir, ctx);
    }
    SWITCH_CASE_INDIRECT_MEM_EXP:
        return inferIndirectMemExp(ir, ctx);
    SWITCH_CASE_READ_ARRAY:
        return inferArray(ir, ctx);
    case IR_LDA:
    case IR_CONST:
        return inferConst(ir, ctx);
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
    case IR_SELECT:
        ASSERTN(!ir->is_cvt(), ("register VN for CVT specially"));
        return inferVNByIterKid(ir, ctx);
    case IR_CASE:
    case IR_DUMMYUSE:
        return nullptr;
    SWITCH_CASE_EXT_EXP:
        return inferExtExp(ir, ctx);
    default: UNREACHABLE();
    }
    return nullptr;
}


void InferEVN::dumpBBListWithEVN() const
{
    //The class dumps IR with user defined attributes.
    class DumpIRWithEVN : public IRDumpCustomBaseFunc {
    public:
        InferEVN const* infer_evn;
    public:
        virtual void dumpCustomAttr(
            OUT xcom::DefFixedStrBuf & buf, Region const* rg, IR const* ir,
            DumpFlag dumpflag) const override
        {
            ASSERT0(infer_evn);
            VN const* vn = const_cast<InferEVN*>(infer_evn)->getVN(ir);
            if (vn == nullptr) { return; }
            xcom::StrBuf tbuf(32);
            vn->dump(rg, tbuf);
            buf.strcat(" (E%s)", tbuf.getBuf());
        }
    };
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE);
    DumpIRWithEVN cf;

    //User defined attributes are VN info.
    cf.infer_evn = this;
    IRDumpCtx<> dumpwithevn(4, f, nullptr, &cf);

    //Dump BB list with context.
    BBDumpCtxMgr<> ctx(&dumpwithevn);
    ASSERT0(m_rg->getBBList());
    xoc::dumpBBList(m_rg->getBBList(), m_rg, false, &ctx);
}


void InferEVN::dumpBBListWithEVN(CHAR const* filename)
{
    ASSERT0(filename);
    FileObj fo(filename, true, false);
    FILE * h = fo.getFileHandler();
    m_rg->getLogMgr()->push(h, filename);
    dumpBBListWithEVN();
    m_rg->getLogMgr()->pop();
}


class VFCleanEVN {
public:
    InferEVN * infer_evn;
public:
    VFCleanEVN(InferEVN * evn) : infer_evn(evn) { ASSERT0(infer_evn); }
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    { infer_evn->cleanVN(ir); return true; }
};

class IterCleanEVN : public VisitIRTree<VFCleanEVN> {
public:
    IterCleanEVN(IR const* ir, VFCleanEVN & vf)
        : VisitIRTree(vf) { visit(ir); }
};

void InferEVN::cleanVNIRTree(IR const* ir)
{
    VFCleanEVN vf(this);
    IterCleanEVN it(ir, vf);
}
//END InferEVN


//
//START GVN
//
GVN::GVN(Region * rg) : Pass(rg)
{
    ASSERT0(rg != nullptr);
    m_md_sys = m_rg->getMDSystem();
    m_du = m_rg->getDUMgr();
    m_tm = m_rg->getTypeMgr();
    m_cfg = m_rg->getCFG();
    ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
    m_is_vn_fp = false;

    //VN is restrict than EVN, thus the different VN means the given
    //two expressions or stmts have different value, this may confuse passes
    //like branch condition propagation.
    m_compute_vn_by_isomo_domdef = false;
    m_is_comp_lda_string = !m_rg->getRegionMgr()->isRegardAllStringAsSameMD();
    m_is_alloc_livein_vn = false;
    m_pool = nullptr;
    m_vn_vec = nullptr;
    m_refine = nullptr;
    init();
}


GVN::~GVN()
{
    destroy();
}


VN * GVN::allocVN()
{
    VN * vn = m_free_lst.remove_head();
    if (vn == nullptr) {
        vn = (VN*)xmalloc(sizeof(VN));
    } else {
        vn->clean();
    }
    VN_id(vn) = m_vn_count++;
    if (m_vn_vec != nullptr) {
        m_vn_vec->set(vn->id(), vn);
    }
    return vn;
}


bool GVN::isUnary(IR_CODE irt) const
{
    return xoc::isUnaryOp(irt);
}


bool GVN::isBinary(IR_CODE irt) const
{
    //Regard LDA as binary op.
    return xoc::isBinaryOp(irt) || irt == IR_LDA || irt == IR_GETELEM;
}


bool GVN::isTriple(IR_CODE irt) const
{
    return irt == IR_SETELEM || irt == IR_SELECT;
}


bool GVN::isQuad(IR_CODE irt) const
{
    return irt == IR_ILD;
}


bool GVN::isQuint(IR_CODE irt) const
{
    return irt == IR_ARRAY;
}


void GVN::init()
{
    if (m_pool != nullptr) { return; }
    m_vn_count = VNID_UNDEF + 1;
    m_zero_vn = nullptr;
    m_mc_zero_vn = nullptr;
    m_mdssamgr = nullptr;
    m_prssamgr = nullptr;
    m_oc = nullptr;
    m_infer_evn = nullptr;
    List<IRBB*> * bbl = m_rg->getBBList();
    UINT n = 0;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        n += bb->getNumOfIR();
    }
    m_stmt2domdef.init(MAX(4, xcom::getNearestPowerOf2(n/2)));
    m_pool = smpoolCreate(sizeof(VN) * 4, MEM_COMM);
    if (g_dump_opt.isDumpGVN()) {
        m_vn_vec = new xcom::Vector<VN const*>(32);
    } else {
        m_vn_vec = nullptr;
    }
}


void GVN::destroy()
{
    if (m_pool == nullptr) { return; }
    destroyLocalUsed();
    smpoolDelete(m_pool);
    m_pool = nullptr;
    if (m_vn_vec != nullptr) {
        delete m_vn_vec;
        m_vn_vec = nullptr;
    }
    if (m_infer_evn != nullptr) {
        delete m_infer_evn;
        m_infer_evn = nullptr;
    }
}


void GVN::destroyLocalUsed()
{
    m_md2vn.destroy(); //will be initialized dynamically.
    m_ll2vn.destroy(); //will be initialized dynamically.
    m_fp2vn.destroy(); //will be initialized dynamically.
    m_str2vn.destroy(); //will be initialized dynamically.

    m_def2ildtab.destroy();
    m_def2ildtab.init();

    m_def2arrtab.destroy();
    m_def2arrtab.init();

    m_def2sctab.destroy();
    m_def2sctab.init();

    UINT bucket_size = m_stmt2domdef.get_bucket_size();
    m_stmt2domdef.destroy();
    m_stmt2domdef.init(bucket_size);

    for (UINT2Ptr * v = m_tab_lst.get_head();
         v != nullptr; v = m_tab_lst.get_next()) {
        delete v;
    }
    m_tab_lst.destroy();
    m_tab_lst.init();

    m_irc_vec.destroy();
    m_irc_vec.init();
}


void GVN::cleanVNIRTree(IR const* ir)
{
    if (ir == nullptr) { return; }
    ASSERTN(!ir->is_undef(), ("ir has been freed"));
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * k = ir->getKid(i); k != nullptr; k = k->get_next()) {
            cleanVNIRTree(k);
        }
    }
    //Do NOT insert ir into mapping table if it does not have a VN.
    setVNIfFind(ir, nullptr);
}


void GVN::cleanIR2VN()
{
    xcom::TTab<UINT> inlst;
    IR2VNIter it;
    VN const* x;
    for (m_ir2vn.get_first(it, &x); x != nullptr; m_ir2vn.get_next(it, &x)) {
        if (!inlst.find(x->id())) {
            inlst.append(x->id());
            m_free_lst.append_tail(const_cast<VN*>(x));
        }
    }
    m_ir2vn.clean();
    MDPhi2VNIter it2;
    for (m_mdphi2vn.get_first(it2, &x);
         x != nullptr; m_mdphi2vn.get_next(it2, &x)) {
        if (!inlst.find(x->id())) {
            inlst.append(x->id());
            m_free_lst.append_tail(const_cast<VN*>(x));
        }
    }
    m_mdphi2vn.clean();
}


void GVN::reset()
{
    m_vn_count = VNID_UNDEF + 1;
    m_zero_vn = nullptr;
    m_mc_zero_vn = nullptr;
    destroyLocalUsed();
    cleanIR2VN();
    if (m_vn_vec != nullptr) {
        //VN vector is optional, it is usually used to dump.
        m_vn_vec->clean();
    }
    if (m_infer_evn != nullptr) {
        m_infer_evn->clean();
    }
}


void GVN::dumpAllVN() const
{
    if (m_vn_vec == nullptr) { return; }
    note(getRegion(), "\n==-- DUMP ALL VN --==");
    m_rg->getLogMgr()->incIndent(2);
    for (UINT i = 0; i < m_vn_vec->get_elem_count(); i++) {
        VN const* vn = m_vn_vec->get(i);
        if (vn == nullptr) { continue; }
        note(m_rg, "\n");
        vn->dump(m_rg);
    }
    m_rg->getLogMgr()->decIndent(2);
}


void GVN::dumpMiscMap() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpGVN()) { return; }
    note(getRegion(), "\n==-- DUMP IR2VN --==");
    {
        m_rg->getLogMgr()->incIndent(2);
        xcom::StrBuf buf(16);
        IR2VNIter it;
        VN const* vn;
        for (UINT irid = m_ir2vn.get_first(it, &vn);
             vn != nullptr; irid = m_ir2vn.get_next(it, &vn)) {
            IR const* ir = m_rg->getIR(irid);
            ASSERT0(ir);
            note(m_rg, "\n%s:", dumpIRName(ir, buf));
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(getRegion(), "\n==-- DUMP MDPhi2VN --==");
    {
        m_rg->getLogMgr()->incIndent(2);
        MDPhi2VNIter it;
        VN const* vn;
        for (UINT phiid = m_mdphi2vn.get_first(it, &vn);
             vn != nullptr; phiid = m_mdphi2vn.get_next(it, &vn)) {
            note(m_rg, "\nMDPhi%u:", phiid);
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(getRegion(), "\n==-- DUMP Longlong2VN --==");
    if (m_ll2vn.is_init()) {
        m_rg->getLogMgr()->incIndent(2);
        Longlong2VNIter it;
        for (VN const* vn = m_ll2vn.get_first_elem(it);
             vn != nullptr; vn = m_ll2vn.get_next_elem(it)) {
            note(m_rg, "\n");
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(getRegion(), "\n==-- DUMP LonglongMC2VN --==");
    if (m_llmc2vn.is_init()) {
        m_rg->getLogMgr()->incIndent(2);
        LonglongMC2VNIter it;
        for (VN const* vn = m_llmc2vn.get_first_elem(it);
             vn != nullptr; vn = m_llmc2vn.get_next_elem(it)) {
            note(m_rg, "\n");
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(getRegion(), "\n==-- DUMP FP2VN --==");
    if (m_fp2vn.is_init()) {
        m_rg->getLogMgr()->incIndent(2);
        FP2VNIter it;
        for (VN const* vn = m_fp2vn.get_first_elem(it);
             vn != nullptr; vn = m_fp2vn.get_next_elem(it)) {
            note(m_rg, "\n");
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(getRegion(), "\n==-- DUMP Str2VN --==");
    if (m_str2vn.is_init()) {
        m_rg->getLogMgr()->incIndent(2);
        Sym2VNIter it;
        for (VN const* vn = m_str2vn.get_first_elem(it);
             vn != nullptr; vn = m_str2vn.get_next_elem(it)) {
            note(m_rg, "\n");
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(getRegion(), "\n==-- DUMP MD2VN --==");
    if (m_md2vn.is_init()) {
        m_rg->getLogMgr()->incIndent(2);
        MD2VNIter it;
        for (VN const* vn = m_md2vn.get_first_elem(it);
             vn != nullptr; vn = m_md2vn.get_next_elem(it)) {
            note(m_rg, "\n");
            vn->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
}


VN * GVN::registerVNviaMD(MD const* md)
{
    if (m_md2vn.get_bucket_size() == 0) {
        m_md2vn.init(10); //to be evaluated
    }
    VN * x = m_md2vn.get(md);
    if (x == nullptr) {
        x = allocVN();
        VN_type(x) = VN_VAR;
        VN_var(x) = md->get_base();
        m_md2vn.set(md, x);
    }
    return x;
}


VN * GVN::registerVNviaINT(LONGLONG v)
{
    if (v == 0) {
        if (m_zero_vn == nullptr) {
            m_zero_vn = allocVN();
            VN_type(m_zero_vn) = VN_INT;
            VN_int_val(m_zero_vn) = 0;
        }
        return m_zero_vn;
    }
    if (m_ll2vn.get_bucket_size() == 0) {
        m_ll2vn.init(10); //To be reevaluated
    }
    VN * vn = m_ll2vn.get(v);
    if (vn != nullptr) {
        return vn;
    }
    vn = allocVN();
    VN_type(vn) = VN_INT;
    VN_int_val(vn) = v;
    m_ll2vn.set(v, vn);
    return vn;
}


VN * GVN::registerVNviaMC(LONGLONG v)
{
    if (v == 0) {
        if (m_mc_zero_vn == nullptr) {
            m_mc_zero_vn = allocVN();
            VN_type(m_mc_zero_vn) = VN_MC_INT;
            VN_int_val(m_mc_zero_vn) = 0;
        }
        return m_mc_zero_vn;
    }

    if (m_llmc2vn.get_bucket_size() == 0) {
        m_llmc2vn.init(10/*TO reevaluate*/);
    }

    VN * vn = m_llmc2vn.get(v);
    if (vn != nullptr) {
        return vn;
    }

    vn = allocVN();
    VN_type(vn) = VN_MC_INT;
    VN_int_val(vn) = v;
    m_llmc2vn.set(v, vn);
    return vn;
}


VN * GVN::registerVNviaVar(Var const* v)
{
    VN * vn = m_var2vn.get(v->id());
    if (vn != nullptr) {
        return vn;
    }
    vn = allocVN();
    VN_type(vn) = VN_VAR;
    VN_var(vn) = v;
    m_var2vn.set(v->id(), vn);
    return vn;
}


VN * GVN::registerVNviaSTR(Sym const* v)
{
    if (m_str2vn.get_bucket_size() == 0) {
        m_str2vn.init(16/*TO reevaluate*/);
    }
    VN * vn = m_str2vn.get(v);
    if (vn != nullptr) {
        return vn;
    }
    vn = allocVN();
    VN_type(vn) = VN_STR;
    VN_str_val(vn) = v;
    m_str2vn.set(v, vn);
    return vn;
}


VN * GVN::registerVNviaFP(double v)
{
    if (m_fp2vn.get_bucket_size() == 0) {
        m_fp2vn.init(10/*TO reevaluate*/);
    }
    VN * vn = m_fp2vn.get(v);
    if (vn != nullptr) {
        return vn;
    }
    vn = allocVN();
    VN_type(vn) = VN_FP;
    VN_fp_val(vn) = v;
    m_fp2vn.set(v, vn);
    return vn;
}


VN * GVN::registerUnaVN(IR_CODE irt, VN const* v0)
{
    ASSERT0(isUnary(irt));
    return registerMultiTupleVN(irt, 1, v0);
}


VN * GVN::registerBinVN(IR_CODE irt, VN const* v0, VN const* v1)
{
    ASSERT0(v0 && v1);
    ASSERT0(isBinary(irt));
    return registerMultiTupleVN(irt, 2, v0, v1);
}


VN * GVN::registerTripleVN(
    IR_CODE irt, VN const* v0, VN const* v1, VN const* v2)
{
    ASSERT0(v0 && v1 && v2);
    ASSERT0(isTriple(irt));
    return registerMultiTupleVN(irt, 3, v0, v1, v2);
}


VN * GVN::registerQuadVN(
    IR_CODE irt, VN const* v0, VN const* v1, VN const* v2, VN const* v3)
{
    ASSERT0(v0 && v1 && v2 && v3);
    ASSERT0(isQuad(irt));
    return registerMultiTupleVN(irt, 4, v0, v1, v2, v3);
}


VN * GVN::registerQuintVN(
    IR_CODE irt, VN const* v0, VN const* v1, VN const* v2, VN const* v3,
    VN const* v4)
{
    ASSERT0(v0 && v1 && v2 && v3 && v4);
    ASSERT0(isQuint(irt));
    return registerMultiTupleVN(irt, 5, v0, v1, v2, v3, v4);
}


VN * GVN::registerMultiTupleVN(IR_CODE irt, UINT vnnum, ...)
{
    ASSERT0(vnnum > 0);
    UINT2Ptr * tab = (UINT2Ptr*)m_irc_vec.get(irt);
    if (tab == nullptr) {
        tab = new UINT2Ptr();
        m_tab_lst.append_tail(tab);
        m_irc_vec.set(irt, (void*)tab);
    }
    va_list ptr;
    va_start(ptr, vnnum);
    VN const* vn = nullptr;
    for (UINT i = 0; i < vnnum - 1; i++) {
        vn = (VN const*)va_arg(ptr, VN const*);
        ASSERT0(vn);
        UINT2Ptr * subtab = (UINT2Ptr*)tab->get(vn->id());
        if (subtab == nullptr) {
            subtab = new UINT2Ptr();
            m_tab_lst.append_tail((UINT2Ptr*)subtab);
            tab->set(vn->id(), subtab);
        }
        tab = subtab;
    }
    //Alloc new VN according to the last input VN id.
    vn = (VN const*)va_arg(ptr, VN const*);
    ASSERT0(vn);
    VN * res = (VN*)tab->get(vn->id());
    if (res == nullptr) {
        res = allocVN();
        VN_type(res) = VN_OP;
        VN_op(res) = irt;
        tab->set(vn->id(), res);
    }
    va_end(ptr);
    return res;
}


//Memory location may be parameter or global variable.
//'emd': exact md
VN * GVN::allocLiveinVN(IR const* exp, MD const* emd, bool & change)
{
    ASSERT0(getVN(exp) == nullptr);
    VN * x = registerVNviaMD(emd);
    change = true;
    setVNOnce(exp, x);
    return x;
}


//Only compute memory operation's vn.
VN const* GVN::computePR(IR const* exp, bool & change)
{
    SSAInfo * ssainfo = PR_ssainfo(exp);
    ASSERT0(exp->isReadPR() && ssainfo);
    IR const* def = ssainfo->getDef();
    if (def == nullptr) {
        ASSERT0(exp->getRefMD());
        if (isAllocLiveinVN() || exp->getRefMD()->is_restrict()) {
            VN const* expv = getVN(exp);
            if (expv != nullptr) { return expv; }
            VN * x = allocLiveinVN(exp, exp->getRefMD(), change);
            if (exp->getRefMD()->is_restrict()) {
                //The restrict hint tell us that exp's value is unique
                //and constant.
                VN_type(x) = VN_CONST;
            }
            return x;
        }
        return nullptr;
    }
    VN const* defvn = getVN(def);
    VN const* ux = getVN(exp);
    if (defvn != ux) {
        setVN(exp, defvn);
        change = true;
    }
    return defvn;
}


bool GVN::hasOverlappedDef(IR const* exp) const
{
    MD const* emd = exp->getExactRef();
    ASSERT0(emd);
    ASSERT0(exp && exp->is_exp());
    DUSet const* defset = exp->readDUSet();
    if (defset == nullptr) { return false; }

    //Check if some may-def or overlapped-def disrupts the emd.
    //Skip the DEF which has effect MD but does not overlapped
    //with emd.
    DUSetIter di = nullptr;
    UINT defcount = 0;
    for (BSIdx i = defset->get_first(&di);
         i != BS_UNDEF; i = defset->get_next(i, &di), defcount++) {
        IR const* dir = m_rg->getIR(i);
        ASSERT0(dir->is_stmt());
        MD const* xd = const_cast<IR*>(dir)->getMustRef();
        if (xd == nullptr) {
            MDSet const* xds = const_cast<IR*>(dir)->getMayRef();
            if (xds != nullptr && xds->is_contain(emd, m_rg)) {
                ASSERT0(getVN(exp) == nullptr);
                //exp's value is may defined, here we can not
                //determine if exp have an individual VN.
                return true;
            }
            continue;
        }

        if (xd == emd || xd->is_overlap(emd)) {
            ASSERT0(getVN(exp) == nullptr);
            //exp's value is overlapped with nonkilling DEF,
            //thus we can not determine if exp have an individual VN.
            return true;
        }
    }
    return false;
}


VN const* GVN::inferVNViaPhi(IR const* exp, IR const* phi, bool & change)
{
    VN const* phivn = getVN(phi);
    VN const* expvn = getVN(exp);
    if (phivn != expvn) {
        setVN(exp, phivn);
        change = true;
    }
    return phivn;
}


VN const* GVN::inferVNViaMDPhi(IR const* exp, MDPhi const* phi, bool & change)
{
    VN const* phivn = getVN(phi);
    VN const* expvn = getVN(exp);
    if (phivn != expvn) {
        setVN(exp, phivn);
        change = true;
    }
    return phivn;
}


//Infer the VN of 'exp' via killing def.
//exp: expression.
//kdef: killing-def.
VN const* GVN::inferVNViaKillingDef(
    IR const* exp, IR const* kdef, OUT bool & change)
{
    VN const* defvn = getVN(kdef);
    VN const* ux = getVN(exp);
    if (defvn != ux) {
        setVN(exp, defvn);
        change = true;
    }
    return defvn;
}


//Infer the VN of 'exp' via dominated-killing-def.
//exp: expression.
//kdef: killing-def.
VN const* GVN::inferVNViaDomKillingDef(
    IR const* exp, IR const* kdef, OUT bool & change)
{
    VN const* defvn = nullptr;
    //Infer exp's VN only if 'kdef' is the dom-def.
    IR const* exp_stmt = exp->getStmt();
    ASSERT0(exp_stmt->is_stmt());
    IRBB * b1 = kdef->getBB();
    IRBB * b2 = exp_stmt->getBB();
    ASSERT0(b1 && b2);
    if ((b1 != b2 && m_cfg->is_dom(b1->id(), b2->id())) ||
        (b1 == b2 && b1->is_dom(kdef, exp_stmt, true))) {
        defvn = getVN(kdef);
    }
    VN const* ux = getVN(exp);
    if (defvn != ux) {
        setVN(exp, defvn);
        change = true;
    }
    return defvn;
}


VN const* GVN::inferVNViaHint(IR const* exp, MD const* md, bool & change)
{
    ASSERT0(exp && md);
    if (isAllocLiveinVN() || md->is_restrict()) {
        VN const* expx = getVN(exp);
        if (expx != nullptr) { return expx; }
        VN * x = allocLiveinVN(exp, md, change);
        if (md->is_restrict()) {
            //The restrict hint tell us that exp's value is unique
            //and constant.
            VN_type(x) = VN_CONST;
        }
        return x;
    }
    return nullptr;
}


VN const* GVN::inferNonPRVNViaHint(IR const* exp, bool & change)
{
    return inferVNViaHint(exp, exp->getExactRef(), change);
}


VN const* GVN::inferPRVNViaHint(IR const* exp, bool & change)
{
    return inferVNViaHint(exp, exp->getMustRef(), change);
}


VN const* GVN::inferVNThroughCFG(IR const* exp, bool & change)
{
    ASSERT0(exp->getExactRef() || exp->isReadPR());
    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        if (!usePRSSADU()) { goto CLASSIC_DU; }
        ASSERT0(PR_ssainfo(exp));
        IR * d = PR_ssainfo(exp)->getDef();
        if (d != nullptr && d->is_phi()) {
            return inferVNViaPhi(exp, d, change);
        }
        return inferPRVNViaHint(exp, change);
    }
    if (exp->isMemRefNonPR()) {
        if (!useMDSSADU()) { goto CLASSIC_DU; }
        ASSERT0(m_mdssamgr->getMDSSAInfoIfAny(exp));
        MDDef const* d = m_mdssamgr->findMustMDDef(exp);
        if (d != nullptr) {
            if (d->is_phi()) {
                return inferVNViaMDPhi(exp, (MDPhi const*)d, change);
            }
            //The function only attempt to infer VN through CFG, namely PHI.
            //If there is a MustDef exist, we still can not say it is the
            //DEF that produce the VN of 'exp'.
            //e.g: st x = ...
            //     call foo();
            //     ... = x
            //  where 'st x' is the MustDef of 'ld x', but not the killing DEF
            //  of ld x.
            return nullptr;
        }
        //Infer the nonpr-memref's VN through region livein information if
        //exp does not have any DEF.
        return inferNonPRVNViaHint(exp, change);
    }
CLASSIC_DU:
    if (exp->isReadPR() && useClassicPRDU()) {
        if (hasOverlappedDef(exp)) {
            return nullptr;
        }
        return inferPRVNViaHint(exp, change);
    }
    if (exp->isMemRefNonPR() && useClassicNonPRDU()) {
        if (hasOverlappedDef(exp)) {
            return nullptr;
        }
        //Infer the nonpr-memref's VN through region livein information if
        //exp does not have any DEF.
        return inferNonPRVNViaHint(exp, change);
    }
    return nullptr;
}


//Only compute memory operation's vn.
VN const* GVN::computeExactMemory(IR const* exp, bool & change)
{
    ASSERT0(exp->isMemOpnd());
    MD const* emd = exp->getExactRef();
    if (emd == nullptr) { return nullptr; }
    IR const* kd = xoc::findKillingDef(exp, m_rg);
    if (kd != nullptr) {
        return inferVNViaDomKillingDef(exp, kd, change);
    }
    return inferVNThroughCFG(exp, change);
}


//Compute VN for ild according to anonymous domdef.
VN const* GVN::computeILoadByAnonDomDef(
    IR const* ild, VN const* mlvn, IR const* domdef, bool & change)
{
    ASSERT0(ild->is_ild() && getDUMgr()->isMayDef(domdef, ild, false));
    ILD_VNE2VN * vnexp_map = m_def2ildtab.get(domdef);
    UINT dtsz = ild->getTypeSize(m_tm);
    VNE_ILD vexp(mlvn->id(), ILD_ofst(ild), dtsz);
    //NOTE:
    //    foo();
    //    ild(v1); //s1
    //    goo();
    //    ild(v1); //s2
    //    vn of s1 should not same as s2.
    if (vnexp_map == nullptr) {
        vnexp_map = new ILD_VNE2VN(m_pool, 16); //bsize to be evaluate.
        m_def2ildtab.set(domdef, vnexp_map);
    }

    VN * ildvn = vnexp_map->get(&vexp);
    if (ildvn == nullptr) {
        ildvn = allocVN();
        VN_type(ildvn) = VN_OP;
        VN_op(ildvn) = IR_ILD;
        vnexp_map->setv((OBJTY)&vexp, ildvn);
    }

    setVN(ild, ildvn);
    change = true;
    return ildvn;
}


//Try to compute VN for ild according to isomorphic ist domdef.
//Note we can not determine whether the IST is the exactly MustDef of ild,
//thus given the premise that 'domdef' is the DomDef of 'ild', we attempt
//to compute VN by infer domdef's base VN and Offset.
static VN const* computeILoadByIsomoIStDomDef(
    IR const* ild, VN const* ildbasevn, IR const* domdef, bool & change,
    GVN * gvn)
{
    if (!gvn->tryComputeVNByIsomoDomDef()) { return nullptr; }
    ASSERT0(domdef && domdef->is_ist());
    ASSERT0(ild && ild->is_ild());
    ASSERT0(domdef->getOffset() == ild->getOffset());

    //The offset is isomorphic.
    //Check if IR expression is isomorphic.
    VN const* domdefbasevn = gvn->getVN(domdef->getBase());
    if (domdefbasevn == nullptr || domdefbasevn != ildbasevn) {
        return nullptr;
    }
    //By given the premise that 'domdef' is the DomDef of 'ild', we attempt
    //to compute VN by infering domdef's base VN, Offset and DomDef's id.
    VN const* unique_vn = gvn->getVN(domdef);
    if (unique_vn == nullptr) {
        unique_vn = gvn->registerQuadVN(
            IR_ILD, ildbasevn, gvn->registerVNviaINT(ILD_ofst(ild)),
            gvn->registerVNviaINT(ild->getTypeSize(gvn->getTypeMgr())),
            gvn->registerVNviaINT(domdef->id()));
        gvn->setVNOnce(domdef, unique_vn);
    }
    gvn->setVNOnce(ild, unique_vn);
    change = true;
    return unique_vn;
}


VN const* GVN::computeILoad(IR const* exp, bool & change)
{
    ASSERT0(exp->is_ild());
    VN const* expbasevn = computeVN(ILD_base(exp), change);
    if (expbasevn == nullptr) {
        ASSERT0(getVN(exp) == nullptr);
        ASSERT0(getVN(ILD_base(exp)) == nullptr);
        return nullptr;
    }

    VN const* evn = getVN(exp);
    if (evn != nullptr) { return evn; }

    evn = computeExactMemory(exp, change);
    if (evn != nullptr) { return evn; }

    IR const* domdef = xoc::findNearestDomDef(exp, m_rg);
    if (domdef == nullptr) {
        return nullptr;
    }
    if (domdef->getMustRef() == nullptr || exp->getMustRef() == nullptr ||
        !domdef->getMustRef()->is_exact() || !exp->getMustRef()->is_exact()) {
        return nullptr;
    }

    if (domdef->is_ist() && domdef->getOffset() == exp->getOffset()) {
        //domdef is ist and the offset is matched.
        //Check if IR expression is match.
        return computeILoadByIsomoIStDomDef(
            exp, expbasevn, domdef, change, this);
    }

    //Offset will be distinguished in computeILoadByAnonDomDef(), thus we do
    //not need to differentiate the various offset of ild and ist here.
    //if (domdef->is_ist() && IST_ofst(domdef) != ILD_ofst(exp)) {
    //    return nullptr;
    //}
    ASSERT0(!domdef->is_ist() || domdef->getOffset() != exp->getOffset());
    return computeILoadByAnonDomDef(exp, expbasevn, domdef, change);
}


void GVN::computeArrayAddrRef(IR const* ir, bool & change)
{
    ASSERT0(ir->is_starray());
    computeVN(ARR_base(ir), change);
    for (IR const* s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        computeVN(s, change);
    }
}


//Compute VN for array according to anonymous domdef.
VN const* GVN::computeArrayByAnonDomDef(
    IR const* arr, VN const* basevn, VN const* ofstvn, IR const* domdef,
    bool & change)
{
    ASSERT0(arr->is_array() && m_du->isMayDef(domdef, arr, false));
    ARR_VNE2VN * vnexp_map = m_def2arrtab.get(domdef);
    UINT dtsz = arr->getTypeSize(m_tm);
    VNE_ARR vexp(basevn->id(), ofstvn->id(), ARR_ofst(arr), dtsz);

    //NOTE:
    // foo();
    // array(v1); //s1
    // goo();
    // array(v1); //s2
    // vn of s1 should not same as s2.
    if (vnexp_map == nullptr) {
        vnexp_map = new ARR_VNE2VN(m_pool, 16); //bsize to be evaluated.
        m_def2arrtab.set(domdef, vnexp_map);
    }
    VN * vn = vnexp_map->get(&vexp);
    if (vn == nullptr) {
        vn = allocVN();
        VN_type(vn) = VN_OP;
        VN_op(vn) = IR_ARRAY;
        vnexp_map->setv((OBJTY)&vexp, vn);
    }
    setVNOnce(arr, vn);
    change = true;
    return vn;
}


static VN const* computeArrayByIsomoStArrDomDef(
    IR const* arr, VN const* arrbasevn, VN const* arrsublist_vn,
    IR const* domdef, bool & change, GVN * gvn)
{
    if (!gvn->tryComputeVNByIsomoDomDef()) { return nullptr; }
    ASSERT0(arr->is_array());
    ASSERT0(domdef->is_starray());
    ASSERTN(((CArray*)domdef)->getDimNum() == 1,
            ("only handle one dim array."));
    ASSERT0(arr->getOffset() == domdef->getOffset());

    //Check if VN expression is match.
    IR const* defbase = ARR_base(domdef);
    VN const* defbase_vn = gvn->getVN(defbase);
    if (defbase_vn == nullptr || defbase_vn != arrbasevn) {
        return nullptr;
    }
    VN const* o = gvn->getVN(ARR_sub_list(domdef));
    if (o == nullptr || o != arrsublist_vn) {
        return nullptr;
    }

    VN const* def_vn = gvn->getVN(domdef);
    if (def_vn == nullptr) {
        def_vn = gvn->registerQuintVN(
            IR_ARRAY, arrbasevn, arrsublist_vn,
            gvn->registerVNviaINT(ARR_ofst(arr)),
            gvn->registerVNviaINT(arr->getTypeSize(gvn->getTypeMgr())),
            gvn->registerVNviaINT(domdef->id()));
        gvn->setVNOnce(domdef, def_vn);
    }
    gvn->setVNOnce(arr, def_vn);
    change = true;
    return def_vn;
}


VN const* GVN::computeArray(IR const* exp, bool & change)
{
    ASSERT0(exp->is_array());
    for (IR const* s = ARR_sub_list(exp); s != nullptr; s = s->get_next()) {
        computeVN(s, change);
    }
    VN const* evn = getVN(exp);
    if (evn != nullptr) { return evn; }

    evn = computeExactMemory(exp, change);
    if (evn != nullptr) {
        return evn;
    }

    VN const* abase_vn = computeVN(ARR_base(exp), change);
    VN const* sublist_vn = nullptr;
    if (((CArray*)exp)->getDimNum() == 1) {
        //only handle one dim array.
        if (abase_vn == nullptr) {
            return nullptr;
        }
        sublist_vn = getVN(ARR_sub_list(exp));
        if (sublist_vn == nullptr) {
            return nullptr;
        }
    } else {
        return nullptr;
    }

    IR const* domdef = xoc::findNearestDomDef(exp, m_rg);
    if (domdef == nullptr) {
        return nullptr;
    }
    if (domdef->getMustRef() == nullptr || exp->getMustRef() == nullptr ||
        !domdef->getMustRef()->is_exact() || !exp->getMustRef()->is_exact()) {
        return nullptr;
    }
    if (domdef->is_starray() && ARR_ofst(domdef) == ARR_ofst(exp)) {
        return computeArrayByIsomoStArrDomDef(
            exp, abase_vn, sublist_vn, domdef, change, this);
    }
    return computeArrayByAnonDomDef(exp, abase_vn, sublist_vn, domdef, change);
}


VN const* GVN::computeScalarByAnonDomDef(
    IR const* exp, IR const* domdef, bool & change)
{
    ASSERT0((exp->is_ld() || exp->is_pr()) &&
            m_du->isMayDef(domdef, exp, false));
    SCVNE2VN * vnexp_map = m_def2sctab.get(domdef);
    UINT dtsz = exp->getTypeSize(m_tm);
    MD const* md = exp->getExactRef();
    ASSERT0(md);
    VNE_SC vexp(MD_id(md), exp->getOffset(), dtsz);
    //NOTE:
    //    foo();
    //    v1; //s1
    //    goo();
    //    v1; //s2
    //    vn of s1 should not same as s2.
    if (vnexp_map == nullptr) {
        vnexp_map = new SCVNE2VN(m_pool, 16); //bsize to be evaluate.
        m_def2sctab.set(domdef, vnexp_map);
    }
    VN * vn = vnexp_map->get(&vexp);
    if (vn == nullptr) {
        vn = allocVN();
        VN_type(vn) = VN_VAR;
        vnexp_map->setv((OBJTY)&vexp, vn);
    }
    setVNOnce(exp, vn);
    change = true;
    return vn;
}


VN const* GVN::computeInexactScalarByClassicDU(IR const* exp, bool & change)
{
    DUSet const* du = exp->readDUSet();
    if (du == nullptr || du->get_elem_count() == 0) {
        //If exact MD DU is empty, should keep it as unknown status.
        return nullptr;
    }
    IR const* domdef = m_du->findNearestDomDef(exp, du);
    if (domdef == nullptr || domdef->getExactRef() == nullptr) {
        return nullptr;
    }
    if (domdef->is_st() && ST_ofst(domdef) != exp->getOffset()) {
        return nullptr;
    }
    if (!domdef->is_st() && !domdef->is_stpr()) {
        return computeScalarByAnonDomDef(exp, domdef, change);
    }
    switch (exp->getCode()) {
    case IR_LD:
        if (domdef->is_stpr() || (LD_idinfo(exp) != ST_idinfo(domdef))) {
            return nullptr;
        }
        break;
    SWITCH_CASE_READ_PR:
        if (domdef->is_st() || PR_no(exp) != STPR_no(domdef)) {
            return nullptr;
        }
        break;
    default: ASSERTN(0, ("unsupport"));
    }
    VN const* uni_vn = getVN(domdef);
    if (uni_vn == nullptr) {
        VN * t = allocVN();
        VN_type(t) = VN_VAR;
        uni_vn = t;
        setVNOnce(domdef, uni_vn);
    }
    setVNOnce(exp, uni_vn);
    change = true;
    return uni_vn;
}


VN const* GVN::computeScalar(IR const* exp, bool & change)
{
    ASSERT0(exp && exp->is_exp());
    VN const* evn = getVN(exp);
    if (evn != nullptr) { return evn; }
    if (exp->isReadPR() && usePRSSADU()) {
        return computePR(exp, change);
    }
    evn = computeExactMemory(exp, change);
    if (evn != nullptr) { return evn; }
    if (exp->getExactRef() == nullptr) {
        //Can not handle inexact MD.
        return nullptr;
    }

    //TBD:does it necessary to compute nearest-dom-def again to judge whether
    //the inexact-ref of 'exp' has VN?
    //return computeInexactScalarByClassicDU(exp, change);
    return nullptr;
}


VN const* GVN::computeSelect(IR const* exp, bool & change)
{
    VN const* vn1 = computeVN(SELECT_det(exp), change);
    VN const* vn2 = computeVN(SELECT_trueexp(exp), change);
    VN const* vn3 = computeVN(SELECT_falseexp(exp), change);
    if (vn1 == nullptr || vn2 == nullptr || vn3 == nullptr) {
        if (getVN(exp) != nullptr) {
            setVN(exp, nullptr);
            change = true;
        }
        return nullptr;
    }
    VN const* x = registerTripleVN(exp->getCode(), vn1, vn2, vn3);
    if (getVN(exp) != x) {
        setVN(exp, x);
        change = true;
    }
    return x;
}


VN const* GVN::computeBin(IR const* exp, bool & change)
{
    VN const* vn1 = computeVN(BIN_opnd0(exp), change);
    VN const* vn2 = computeVN(BIN_opnd1(exp), change);
    if (vn1 == nullptr || vn2 == nullptr) {
        if (getVN(exp) != nullptr) {
            setVN(exp, nullptr);
            change = true;
        }
        return nullptr;
    }
    VN const* x = nullptr;
    if (vn1->is_int() && vn2->is_int() &&
        !m_refine->mayCauseHardWareException(
            exp->getCode(), VN_int_val(vn1), VN_int_val(vn2))) {
        HOST_INT val = m_refine->calcBinIntVal(
            exp, VN_int_val(vn1), VN_int_val(vn2));
        x = computeIntConst(val);
    } else {
        x = registerBinVN(exp->getCode(), vn1, vn2);
    }
    if (getVN(exp) != x) {
        setVN(exp, x);
        change = true;
    }
    return x;
}


VN const* GVN::computeIntConst(HOST_INT val)
{
    return registerVNviaINT(val);
}


VN const* GVN::computeConst(IR const* exp, bool & change)
{
    VN const* x = getVN(exp);
    if (x != nullptr) { return x; }
    if (exp->is_lda()) {
        x = registerVNviaVar(exp->getIdinfo());
    } else if (exp->is_int()) {
        x = registerVNviaINT(CONST_int_val(exp));
    } else if (exp->is_ptr()) {
        //Regard PTR as INT because the bit-width of pointer of target machine
        //is always longer than integer.
        x = registerVNviaINT(CONST_int_val(exp));
    } else if (exp->is_mc()) {
        x = registerVNviaMC(CONST_int_val(exp));
    } else if (exp->is_fp()) {
        if (!m_is_vn_fp) {
            return nullptr;
        }
        x = registerVNviaFP(CONST_fp_val(exp));
    } else if (exp->is_str()) {
        x = registerVNviaSTR(CONST_str_val(exp));
    } else if (exp->is_any()) {
        return nullptr;
    } else  {
        ASSERTN(0, ("unsupport const type"));
    }
    ASSERT0(x);
    setVN(exp, x);
    change = true;
    return x;
}


VN const* GVN::computeLda(IR const* exp, bool & change)
{
    ASSERT0(exp->is_lda());
    Var * v = LDA_idinfo(exp);
    VN const* basevn = nullptr;
    if (v->is_string()) {
        if (m_is_comp_lda_string) {
            MD const* emd = m_rg->getMDMgr()->genMDForVar(v, LDA_ofst(exp));
            ASSERTN(emd && emd->is_effect(),
                    ("string should have effect MD"));
            basevn = registerVNviaMD(emd);
        } else {
            basevn = nullptr;
        }
    } else {
        MD const* emd = m_rg->getMDMgr()->genMDForVar(v, LDA_ofst(exp));
        ASSERTN(emd && emd->is_effect(), ("expect effect MD"));
        basevn = registerVNviaMD(emd);
    }
    if (basevn == nullptr) {
        VN const* vn = getVN(exp);
        if (vn != nullptr) {
            ASSERT0(vn->is_const());
            setVN(exp, nullptr);
            change = true;
        }
        return nullptr;
    }
    VN const* ofstvn = registerVNviaINT(LDA_ofst(exp));
    VN * x = registerBinVN(IR_LDA, basevn, ofstvn);

    //LDA is always unique and constant for individual Var.
    VN_type(x) = VN_CONST;
    if (getVN(exp) != x) {
        setVN(exp, x);
        change = true;
    }
    return x;
}


bool GVN::isCvtChangeVN(IR const* cvt) const
{
    ASSERT0(cvt);
    IR const* cvtexp = CVT_exp(cvt);
    ASSERT0(cvtexp);
    if ((!cvt->is_int() && !cvt->is_ptr()) ||
        (!cvtexp->is_int() && !cvtexp->is_ptr())) {
        //Only integer data type (include pointer) might keep VN unchanged.
        return true;
    }
    if (cvt->is_ptr() && cvtexp->is_ptr()) {
        //CASE: pointer conversion does not change the VN.
        //e.g:p:*<1> <-- p:*<4>
        return false;
    }
    if (cvt->is_int() && cvtexp->is_int() &&
        (m_tm->getByteSize(cvt->getType()) <
         m_tm->getByteSize(cvtexp->getType()))) {
        //CASE: We regard OP that reduce the value-range will
        //change the VN of 'cvtexp'.
        //e.g: 0xffff:u16 <-- 0xffffFFFF:u32
        return true;
    }
    //Signed or unsigned value movement or expansion does not change the VN.
    return false;
}


VN const* GVN::computeCvt(IR const* cvt, bool & change)
{
    ASSERT0(cvt->is_cvt());
    IR const* cvtexp = CVT_exp(cvt);
    VN const* x = computeVN(cvtexp, change);
    if (x == nullptr) {
        if (getVN(cvt) != nullptr) {
            setVN(cvt, nullptr);
            change = true;
        }
        return nullptr;
    }
    if (!isCvtChangeVN(cvt)) {
        if (getVN(cvt) != x) {
            setVN(cvt, x);
            change = true;
        }
        return x;
    }
    if (!cvt->getType()->is_scalar())  { return nullptr; }
    if (!cvtexp->getType()->is_scalar())  { return nullptr; }
    x = registerCvtVN(x, cvt->getType(), cvtexp->getType());
    if (getVN(cvt) != x) {
        setVN(cvt, x);
        change = true;
    }
    return x;
}


VN const* GVN::computeUna(IR const* exp, bool & change)
{
    VN const* x = computeVN(UNA_opnd(exp), change);
    if (x == nullptr) {
        if (getVN(exp) != nullptr) {
            setVN(exp, nullptr);
            change = true;
        }
        return nullptr;
    }
    x = registerUnaVN(exp->getCode(), x);
    if (getVN(exp) != x) {
        setVN(exp, x);
        change = true;
    }
    return x;
}


VN const* GVN::computeVN(IR const* exp, bool & change)
{
    ASSERT0(exp);
    if (exp->is_cvt()) { return computeCvt(exp, change); }
    switch (exp->getCode()) {
    SWITCH_CASE_BIN: return computeBin(exp, change);
    SWITCH_CASE_UNA: return computeUna(exp, change);
    case IR_LDA: return computeLda(exp, change);
    //case IR_ID:
    //Note IR_ID should not participate in GVN analysis because it does not
    //represent a real operation.
    case IR_LD:
    case IR_ID:
    SWITCH_CASE_READ_PR: return computeScalar(exp, change);
    case IR_ARRAY: return computeArray(exp, change);
    case IR_ILD: return computeILoad(exp, change);
    case IR_SELECT: return computeSelect(exp, change);
    case IR_CONST: return computeConst(exp, change);
    case IR_CASE:
    case IR_DUMMYUSE:
        return nullptr;
    default: return computeExtExp(exp, change);
    }
    return nullptr;
}


void GVN::processPhi(IR const* ir, bool & change)
{
    VN const* phivn = nullptr;
    IR const* p = PHI_opnd_list(ir);
    if (p != nullptr) {
        phivn = computePhiOpnd(p, change);
        p = p->get_next();
    }
    for (; p != nullptr; p = p->get_next()) {
        VN const* opndvn = computePhiOpnd(p, change);
        if (phivn != nullptr && phivn != opndvn) {
            phivn = nullptr;
        }
    }
    if (getVN(ir) != phivn) {
        setVN(ir, phivn);
        change = true;
    }
}


void GVN::processCall(IR const* ir, bool & change)
{
    for (IR const* p = CALL_arg_list(ir); p != nullptr; p = p->get_next()) {
        computeVN(p, change);
    }
    //CASE:the VN of a call's result PR should be determined by its use.
    //TBD:readonly call's VN could be computed through hashing each parameters.
    //VN const* x = getVN(ir);
    //if (x == nullptr) {
    //    VN * t = allocVN();
    //    VN_type(t) = VN_VAR;
    //    change = true;
    //    x = t;
    //    setVNOnce(ir, x);
    //}
    return;
}


void GVN::processRegion(IR const* ir, bool & change)
{
    //Region effect should be simluated via call-stmt
    //which will be handled.
}


bool GVN::isSameMemLocForIndirectOp(IR const* ir1, IR const* ir2) const
{
    IR const* irbase1 = const_cast<IR*>(ir1)->getBase();
    IR const* irbase2 = const_cast<IR*>(ir2)->getBase();
    ASSERT0(irbase1 && irbase2);
    VN const* base1 = getVN(irbase1);
    VN const* base2 = getVN(irbase2);
    if (base1 == nullptr || base2 == nullptr) {
        return hasSameValueBySSA(irbase1, irbase2);
    }
    return base1 == base2;
}


//Return true if the value of ir1 and ir2 are definitely same, otherwise
//return false to indicate unknown.
bool GVN::hasSameValueByPRSSA(IR const* ir1, IR const* ir2) const
{
    if (!usePRSSADU()) { return false; }
    return PRSSAMgr::hasSameValue(ir1, ir2);
}


bool GVN::hasSameValueByMDSSA(IR const* ir1, IR const* ir2) const
{
    if (!useMDSSADU()) { return false; }
    return MDSSAMgr::hasSameValue(ir1, ir2);
}


bool GVN::hasDifferentValue(
    VN const* vn1, Type const* vn1type, VN const* vn2,
    Type const* vn2type) const
{
    if (vn1 == vn2) { return false; }
    if (!vn1type->is_int() || !vn2type->is_int()) { return false; }
    //For now, only support integer type VN comparison.
    return false;
}


bool GVN::hasDifferentValue(
    VN const* vn1, IR const* ir1, VN const* vn2, IR const* ir2) const
{
    return isConstVN(vn1, ir1) && isConstVN(vn2, ir2) &&
           hasDifferentValue(vn1, ir1->getType(), vn2, ir2->getType());
}


bool GVN::hasDifferentValue(VN const* vn1, VN const* vn2) const
{
    ASSERT0(vn1 && vn2);
    return vn1->is_const() && vn2->is_const() && vn1 != vn2;
}


bool GVN::isConstVN(VN const* irvn, IR const* ir) const
{
    ASSERT0(ir && irvn);
    if (irvn->is_const()) {
        ASSERT0(getVN(ir) == irvn);
        return true;
    }
    if (ir->is_cvt()) {
        IR const* leaf = ((CCvt*)ir)->getLeafExp();
        ASSERT0(leaf);
        VN const* leafvn = getVN(leaf);
        return leafvn != nullptr && leafvn->is_const();
    }
    return irvn->is_const();

}


bool GVN::hasConstVN(IR const* ir) const
{
    ASSERT0(ir);
    VN const* vn = getVN(ir);
    if (vn == nullptr) { return false; }
    if (ir->is_cvt()) {
        IR const* leaf = ((CCvt*)ir)->getLeafExp();
        ASSERT0(leaf);
        VN const* leafvn = getVN(leaf);
        return leafvn != nullptr && leafvn->is_const();
    }
    return vn->is_const();
}


bool GVN::hasSameValueBySSA(IR const* ir1, IR const* ir2) const
{
    if (!ir1->isIsomoTo(ir2, getIRMgr(), true,
            IsomoFlag(ISOMO_UNDEF|ISOMO_CK_CONST_VAL))) {
        //Only check the IR tree structure and type.
        return false;
    }
    ConstIRIter it1;
    ConstIRIter it2;
    IR const* k1 = iterInitC(ir1, it1, false);
    IR const* k2 = iterInitC(ir2, it2, false);
    for (; k1 != nullptr; k1 = iterNextC(it1, true),
         k2 = iterNextC(it2, true)) {
        ASSERT0(k2);
        if (!k1->isMemRef()) {
            ASSERT0(!k2->isMemRef());
            continue;
        }
        if (k1->isPROp()) {
            ASSERT0(k2->isPROp());
            //Try determining by DU chain.
            if (hasSameValueByPRSSA(k1, k2)) {
                continue;
            }
            //Try determining by VN.
            VN const* k1vn = getVN(k1);
            VN const* k2vn = getVN(k2);
            if (k1vn != nullptr && k2vn != nullptr && k1vn == k2vn) {
                continue;
            }
            //We have no knowledge about k1 and k2.
            return false;
        }
        if (k1->isMemRefNonPR()) {
            ASSERT0(k2->isMemRefNonPR());
            //Try determining by DU chain.
            if (hasSameValueByMDSSA(k1, k2)) {
                continue;
            }
            //Try determining by VN.
            VN const* k1vn = getVN(k1);
            VN const* k2vn = getVN(k2);
            if (k1vn != nullptr && k2vn != nullptr && k1vn == k2vn) {
                continue;
            }
            //We have no knowledge about k1 and k2.
            return false;
        }
        UNREACHABLE();
    }
    ASSERT0(k1 == nullptr && k2 == nullptr);
    return true;
}


static bool isSameMemLocForIRList(
    IR const* irlst1, IR const* irlst2, GVN const* gvn)
{
    IR const* s2 = irlst2;
    for (IR const* s1 = irlst1; s1 != nullptr;
         s1 = s1->get_next(), s2 = s2->get_next()) {
        ASSERT0(s2);
        VN const* vs1 = gvn->getVN(s1);
        VN const* vs2 = gvn->getVN(s2);
        if (vs1 == nullptr || vs2 == nullptr) {
            if (!gvn->hasSameValueBySSA(s1, s2)) {
                return false;
            }
        }
        if (vs1 != vs2) {
            return false;
        }
    }
    return true;
}


bool GVN::isSameMemLocForArrayOp(IR const* ir1, IR const* ir2) const
{
    IR const* irbase1 = const_cast<IR*>(ir1)->getBase();
    IR const* irbase2 = const_cast<IR*>(ir2)->getBase();
    ASSERT0(irbase1 && irbase2);
    VN const* base1 = getVN(irbase1);
    VN const* base2 = getVN(irbase2);
    if ((base1 == nullptr || base2 == nullptr) &&
        !hasSameValueBySSA(irbase1, irbase2)) {
        return false;
    }
    if (base1 != base2) {
        return false;
    }
    return isSameMemLocForIRList(ARR_sub_list(ir1), ARR_sub_list(ir2), this);
}


//Return true if ir1 and ir2 represent identical memory location, otherwise
//return false to tell caller we do not know more about these object.
//Note this function does NOT consider data type that ir1 or ir2 referrenced.
bool GVN::isSameMemLoc(IR const* ir1, IR const* ir2) const
{
    ASSERT0(ir1 && ir2);
    if (ir1 == ir2) { return true; }
    if (ir1->getOffset() != ir2->getOffset()) { return false; }
    if ((ir1->is_st() || ir1->is_ld()) && (ir2->is_st() || ir2->is_ld())) {
        return ir1->getIdinfo() == ir2->getIdinfo();
    }
    if (ir1->isIndirectMemOp() && ir2->isIndirectMemOp()) {
        return isSameMemLocForIndirectOp(ir1, ir2);
    }
    if (ir1->isArrayOp() && ir2->isArrayOp() && ir1->isSameArrayStruct(ir2)) {
        return isSameMemLocForArrayOp(ir1, ir2);
    }
    MD const* must1 = ir1->getRefMD();
    MD const* must2 = ir2->getRefMD();
    return must1 == must2 && must1 != nullptr;
}


void GVN::processGETELEM(IR * ir, bool & change)
{
    VN const* v1 = computeVN(GETELEM_base(ir), change);
    VN const* v2 = computeVN(GETELEM_ofst(ir), change);
    if (v1 == nullptr || v2 == nullptr) { return; }
    //Regard getelem as binary operation.
    VN const* x = registerBinVN(IR_GETELEM, v1, v2);
    if (getVN(ir) != x) {
        setVN(ir, x);
        change = true;
    }
}


void GVN::copyVN(IR const* from, IR const* to)
{
    VN const* vn = getVN(from);
    if (vn == nullptr) { return; }
    setVN(to, vn);
}


void GVN::processSETELEM(IR * ir, bool & change)
{
    VN const* v1 = computeVN(SETELEM_base(ir), change);
    VN const* v2 = computeVN(SETELEM_val(ir), change);
    VN const* v3 = computeVN(SETELEM_ofst(ir), change);
    if (v1 == nullptr || v2 == nullptr || v3 == nullptr) { return; }

    //Do NOT clean ir's VN, because its VN may be set by its dominated
    //use-stmt ILD.
    VN const* x = registerTripleVN(ir->getCode(), v1, v2, v3);
    if (getVN(ir) != x) {
        setVN(ir, x);
        change = true;
    }
}


void GVN::processDirectMemOp(IR * ir, bool & change)
{
    ASSERT0(ir->is_stmt() && ir->hasRHS());
    IR * rhs = ir->getRHS();
    if (rhs == nullptr) {
        //Virtual OP may not have RHS.
        return;
    }
    VN const* x = computeVN(rhs, change);
    if (x == nullptr) {
        //CASE:Any VN indicates the unique value, thus do NOT assign VN to an
        //IR if we can not determine its unique value.
        return;
    }
    //Do NOT clean ir's VN, because its VN may be set by its dominated
    //use-stmt ILD.
    if (getVN(ir) != x) {
        //ir's VN is nullptr only the first iteration computation.
        //ASSERT0(getVN(ir) == nullptr);
        setVN(ir, x);
        change = true;
    }
}


void GVN::processIndirectMemOp(IR * ir, bool & change)
{
    computeVN(ir->getBase(), change);
    processDirectMemOp(ir, change);
}


void GVN::processSTARRAY(IR * ir, bool & change)
{
    computeArrayAddrRef(ir, change);
    processDirectMemOp(ir, change);
}


void GVN::processExtStmt(IR * ir, bool & change)
{
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_STMT:
        if (ir->getCode() == IR_VSTPR) {
            processDirectMemOp(ir, change);
            break;
        }
        UNREACHABLE();
        break;
    default: UNREACHABLE();
    }
}


void GVN::processStmt(IR * ir, bool & change)
{
    ASSERT0(ir);
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        processDirectMemOp(ir, change);
        return;
    case IR_STARRAY:
        processSTARRAY(ir, change);
        return;
    SWITCH_CASE_INDIRECT_MEM_STMT:
        processIndirectMemOp(ir, change);
        return;
    SWITCH_CASE_CALL:
        processCall(ir, change);
        return;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        computeVN(BR_det(ir), change);
        return;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO:
        computeVN(ir->getValExp(), change);
        return;
    case IR_RETURN:
        if (RET_exp(ir) != nullptr) {
            computeVN(RET_exp(ir), change);
        }
        return;
    case IR_REGION:
        processRegion(ir, change);
        return;
    case IR_PHI:
        processPhi(ir, change);
        return;
    case IR_SETELEM:
        processSETELEM(ir, change);
        return;
    case IR_GETELEM:
        processGETELEM(ir, change);
        return;
    case IR_GOTO:
        return;
    default: processExtStmt(ir, change);
    }
}


VN const* GVN::computePhiPROpnd(IR const* exp, bool & change)
{
    ASSERT0(exp->isReadPR());
    IR const* kd = xoc::findKillingDef(exp, m_rg);
    if (kd != nullptr) {
        return inferVNViaKillingDef(exp, kd, change);
    }
    return inferVNThroughCFG(exp, change);
}


VN const* GVN::computePhiOpnd(IR const* exp, bool & change)
{
    ASSERT0(exp);
    switch (exp->getCode()) {
    SWITCH_CASE_READ_PR: return computePhiPROpnd(exp, change);
    case IR_CONST: return computeConst(exp, change);
    case IR_LDA: return computeLda(exp, change);
    case IR_CVT: return computeCvt(exp, change);
    default: UNREACHABLE();
    }
    return nullptr;
}


VN * GVN::registerCvtVN(VN const* v0, Type const* srcty, Type const* tgtty)
{
    ASSERT0(v0 && srcty && tgtty);
    ASSERT0(srcty->is_scalar());
    ASSERT0(tgtty->is_scalar());
    VN * srctyvn = registerVNviaINT(srcty->getDType());
    VN * tgttyvn = registerVNviaINT(tgtty->getDType());
    return registerCvtVN(v0, srctyvn, tgttyvn);
}


VN * GVN::registerCvtVN(VN const* v0, VN const* v1, VN const* v2)
{
    ASSERT0(v0 && v1 && v2);
    return registerMultiTupleVN(IR_CVT, 3, v0, v1, v2);
}


VN const* GVN::computeMDPhiMemOpnd(IR const* exp, bool & change)
{
    ASSERT0(exp->isMemOpnd());
    MD const* emd = exp->getExactRef();
    if (emd == nullptr) { return nullptr; }
    IR const* kd = xoc::findKillingDef(exp, m_rg);
    if (kd != nullptr) {
        return inferVNViaKillingDef(exp, kd, change);
    }
    return inferVNThroughCFG(exp, change);
}


VN const* GVN::computeMDPhiOpnd(IR const* exp, bool & change)
{
    ASSERT0(exp);
    switch (exp->getCode()) {
    case IR_ID: return computeMDPhiMemOpnd(exp, change);
    case IR_CONST: return computeConst(exp, change);
    case IR_CVT: return computeUna(exp, change);
    default: UNREACHABLE();
    }
    return nullptr;
}


void GVN::processMDPhi(MDPhi const* phi, bool & change)
{
    VN const* phivn = nullptr;
    IR const* p = MDPHI_opnd_list(phi);
    if (p != nullptr) {
        phivn = computeMDPhiOpnd(p, change);
        p = p->get_next();
    }
    for (; p != nullptr; p = p->get_next()) {
        VN const* opndvn = computeMDPhiOpnd(p, change);
        if (phivn != nullptr && phivn != opndvn) {
            phivn = nullptr;
        }
    }
    if (getVN(phi) != phivn) {
        ASSERT0(getVN(phi) == nullptr);
        setVN(phi, phivn);
        change = true;
    }
}


void GVN::processMDPhi(IRBB * bb, bool & change)
{
    if (!useMDSSADU()) { return; }
    MDPhiList * philist = m_mdssamgr->getPhiList(bb->id());
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        processMDPhi(phi, change);
    }
}


void GVN::processBB(IRBB * bb, bool & change)
{
    processMDPhi(bb, change);
    IRListIter ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
        processStmt(ct->val(), change);
    }
}


void GVN::assignRHSVN()
{
    BBList const* bbl = m_rg->getBBList();
    BBListIter it;
    for (IRBB * bb = bbl->get_head(&it);
         bb != nullptr; bb = bbl->get_next(&it)) {
        BBIRListIter irit;
        for (IR const* ir = bb->getIRList().get_head(&irit);
             ir != nullptr; ir = bb->getIRList().get_next(&irit)) {
            if (!ir->hasRHS()) { continue; }
            VN const* resvn = getVN(ir);
            IR const* rhs = ir->getRHS();
            if (rhs == nullptr) {
                //Virtual OP may not have RHS.
                continue;
            }
            if (resvn == nullptr) {
                ASSERT0(getVN(rhs) == nullptr);
                continue;
            }
            ASSERT0(getVN(rhs) == nullptr || getVN(rhs) == resvn);
            setVN(rhs, resvn);
        }
    }
}


void GVN::processBBListInRPO()
{
    RPOVexList * vlst = m_cfg->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == m_rg->getBBList()->get_elem_count());
    UINT count = 0;
    bool change = true;
    while (change && count < 100) {
        change = false;
        for (Vertex const* v = vlst->get_head();
             v != nullptr; v = vlst->get_next()) {
            processBB(m_cfg->getBB(v->id()), change);
        }
        count++;
    }
    ASSERT0(!change);
}


void GVN::dumpIR2VN() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    IR2VNIter it;
    VN const* x;
    for (UINT i = m_ir2vn.get_first(it, &x); x != nullptr;
         i = m_ir2vn.get_next(it, &x)) {
        IR const* ir = getRegion()->getIR(i);
        ASSERT0(ir);
        note(getRegion(), "\n%s:", DumpIRName().dump(ir));
        x->dump(m_rg);
    }
}


void GVN::dumpBBListWithVN() const
{
    //The class dumps IR with user defined attributes.
    class DumpIRWithVN : public IRDumpCustomBaseFunc {
    public:
        GVN const* gvn;
    public:
        virtual void dumpCustomAttr(
            OUT xcom::DefFixedStrBuf & buf, Region const* rg, IR const* ir,
            DumpFlag dumpflag) const override
        {
            ASSERT0(gvn);
            VN const* vn = const_cast<GVN*>(gvn)->getVN(ir);
            if (vn == nullptr) { return; }
            xcom::StrBuf tbuf(32);
            vn->dump(rg, tbuf);
            buf.strcat(" (%s)", tbuf.getBuf());
        }
    };
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE);
    DumpIRWithVN cf;
    cf.gvn = this;
    IRDumpCtx<> dumpwithvn(4, f, nullptr, &cf);
    ASSERT0(m_rg->getBBList());
    BBDumpCtxMgr<> ctx(&dumpwithvn);
    xoc::dumpBBList(m_rg->getBBList(), m_rg, false, &ctx);
}


void GVN::dumpBBListWithVN(CHAR const* filename) const
{
    ASSERT0(filename);
    FileObj fo(filename, true, false);
    FILE * h = fo.getFileHandler();
    m_rg->getLogMgr()->push(h, filename);
    dumpBBListWithVN();
    m_rg->getLogMgr()->pop();
}


bool GVN::dumpForTest() const
{
    dumpIR2VN();
    if (m_infer_evn != nullptr) {
        m_infer_evn->dumpForTest();
    }
    return true;
}

bool GVN::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpGVN()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    if (g_dump_opt.isDumpForTest()) { return dumpForTest(); }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    getRegion()->getLogMgr()->incIndent(2);
    dumpAllVN();
    dumpBBListWithVN();
    getRegion()->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


bool GVN::calcCondMustValEQ(
    IR const* ir, bool & must_true, bool & must_false) const
{
    ASSERT0(ir->is_eq());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    VN const* v1 = getVN(op0);
    VN const* v2 = getVN(op1);
    if (v1 == nullptr || v2 == nullptr) { return false; }
    if (hasSameValue(v1, v2)) {
        must_true = true;
        must_false = false;
        return true;
    }
    if (hasDifferentValue(v1, op0, v2, op1)) {
        must_true = false;
        must_false = true;
        return true;
    }
    return false;
}


bool GVN::calcCondMustValNE(
    IR const* ir, bool & must_true, bool & must_false) const
{
    ASSERT0(ir->is_ne());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    VN const* v1 = getVN(op0);
    VN const* v2 = getVN(op1);
    if (v1 == nullptr || v2 == nullptr) { return false; }
    if (hasDifferentValue(v1, op0, v2, op1)) {
        must_true = true;
        must_false = false;
        return true;
    }
    if (hasSameValue(v1, v2)) {
        must_true = false;
        must_false = true;
        return true;
    }
    return false;
}


bool GVN::calcCondMustValLAndLOr(
    IR const* ir, bool & must_true, bool & must_false) const
{
    ASSERT0(ir->is_land() || ir->is_lor());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    VN const* v1 = getVN(op0);
    VN const* v2 = getVN(op1);
    if (v1 == nullptr || v2 == nullptr) { return false; }
    if (VN_type(v1) == VN_INT && VN_type(v2) == VN_INT) {
        //CASE:given two operand, the comparision rule is that if one of
        //operands is unsigned, the comparision will treat both two operand as
        //unsigned integer value.
        //e.g: given op0 is unsigned value 0, op1 is signed value -1, the
        //comparision result is op0 < op1.
        bool is_unsigned = op0->is_uint() || op1->is_uint();
        if (is_unsigned) {
            HOST_UINT uv1 = (HOST_UINT)VN_int_val(v1);
            HOST_UINT uv2 = (HOST_UINT)VN_int_val(v2);
            if (uv1 && uv2) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        if (VN_int_val(v1) && VN_int_val(v2)) {
            must_true = true;
            must_false = false;
        } else {
            must_true = false;
            must_false = true;
        }
        return true;
    }
    return false;
}


bool GVN::calcCondMustValLEGE(
    IR const* ir, bool & must_true, bool & must_false) const
{
    ASSERT0(ir->is_le() || ir->is_ge());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    VN const* v1 = getVN(op0);
    VN const* v2 = getVN(op1);
    if (v1 == nullptr || v2 == nullptr) { return false; }
    if (v1 == v2) {
        must_true = true;
        must_false = false;
        return true;
    }
    if (VN_type(v1) == VN_INT && VN_type(v2) == VN_INT) {
        //CASE:given two operand, the comparision rule is that if one of
        //operands is unsigned, the comparision will treat both two operand as
        //unsigned integer value.
        //e.g: given op0 is unsigned value 0, op1 is signed value -1, the
        //comparision result is op0 < op1.
        bool is_unsigned = op0->is_uint() || op1->is_uint();
        if (ir->is_le()) {
            if (is_unsigned) {
                HOST_UINT uv1 = (HOST_UINT)VN_int_val(v1);
                HOST_UINT uv2 = (HOST_UINT)VN_int_val(v2);
                if (uv1 <= uv2) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            }
            if (VN_int_val(v1) <= VN_int_val(v2)) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        if (ir->is_ge()) {
            if (is_unsigned) {
                HOST_UINT uv1 = (HOST_UINT)VN_int_val(v1);
                HOST_UINT uv2 = (HOST_UINT)VN_int_val(v2);
                if (uv1 >= uv2) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            }
            if (VN_int_val(v1) >= VN_int_val(v2)) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        return false;
    }
    return false;
}


InferEVN * GVN::getAndGenInferEVN()
{
    if (m_infer_evn == nullptr) {
        m_infer_evn = allocInferEVN();
    }
    return m_infer_evn;
}


bool GVN::calcCondMustValLTGT(
    IR const* ir, bool & must_true, bool & must_false) const
{
    ASSERT0(ir->is_lt() || ir->is_gt());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    VN const* v1 = getVN(op0);
    VN const* v2 = getVN(op1);
    if (v1 == nullptr || v2 == nullptr) { return false; }
    if (v1 == v2) {
        must_true = false;
        must_false = true;
        return true;
    }
    if (VN_type(v1) == VN_INT && VN_type(v2) == VN_INT) {
        //CASE:given two operand, the comparision rule is that if one of
        //operands is unsigned, the comparision will treat both two operand as
        //unsigned integer value.
        //e.g: given op0 is unsigned value 0, op1 is signed value -1, the
        //comparision result is op0 < op1.
        bool is_unsigned = op0->is_uint() || op1->is_uint();
        if (ir->is_lt()) {
            if (is_unsigned) {
                HOST_UINT uv1 = (HOST_UINT)VN_int_val(v1);
                HOST_UINT uv2 = (HOST_UINT)VN_int_val(v2);
                if (uv1 < uv2) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            }
            if (VN_int_val(v1) < VN_int_val(v2)) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        if (ir->is_gt()) {
            if (is_unsigned) {
                HOST_UINT uv1 = (HOST_UINT)VN_int_val(v1);
                HOST_UINT uv2 = (HOST_UINT)VN_int_val(v2);
                if (uv1 > uv2) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            }
            if (VN_int_val(v1) > VN_int_val(v2)) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        return false;
    }
    return false;
}


bool GVN::calcCondMustValBin(
    IR const* ir, bool & must_true, bool & must_false) const
{
    ASSERT0(ir->isBinaryOp());
    switch (ir->getCode()) {
    case IR_LAND:
    case IR_LOR:
        return calcCondMustValLAndLOr(ir, must_true, must_false);
    case IR_LT:
    case IR_GT:
        return calcCondMustValLTGT(ir, must_true, must_false);
    case IR_LE:
    case IR_GE:
        return calcCondMustValLEGE(ir, must_true, must_false);
    case IR_NE:
        return calcCondMustValNE(ir, must_true, must_false);
    case IR_EQ:
        return calcCondMustValEQ(ir, must_true, must_false);
    default: UNREACHABLE();
    }
    return false;
}


//Return true if GVN is able to determine the result of 'ir', otherwise
//return false that GVN know nothing about ir.
bool GVN::calcCondMustVal(
    IR const* ir, bool & must_true, bool & must_false) const
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    if (ir->is_lnot()) {
        VN const* v = getVN(UNA_opnd(ir));
        if (v == nullptr) { return false; }
        if (VN_type(v) == VN_INT) {
            if (!VN_int_val(v)) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        if (VN_type(v) == VN_FP) {
            if (VN_fp_val(v) == 0.0) {
                must_true = true;
                must_false = false;
                return true;
            }
            must_true = false;
            must_false = true;
            return true;
        }
        if (VN_type(v) == VN_STR) {
            must_true = false;
            must_false = true;
            return true;
        }
        return false;
    }
    ASSERT0(ir->isBinaryOp());
    return calcCondMustValBin(ir, must_true, must_false);
}


void GVN::initDepPass(OptCtx & oc)
{
    m_refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    ASSERT0(m_refine);
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_RPO, PASS_DOM, PASS_UNDEF);
}


//GVN try to assign a value numbers to expressions.
bool GVN::perform(OptCtx & oc)
{
    if (m_rg->getBBList()->is_empty()) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    m_oc = &oc;
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //GVN use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //GVN use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    START_TIMER(t, getPassName());
    reset();
    initDepPass(oc);
    processBBListInRPO();
    assignRHSVN();
    destroyLocalUsed();
    END_TIMER(t, getPassName());
    if (g_dump_opt.isDumpAfterPass()) {
       dump();
    }
    set_valid(true);
    return true;
}
//END GVN


void cleanVNForIRTreeList(GVN * gvn, IR const* ir)
{
    for (IR const* p = ir; p != nullptr; p = p->get_next()) {
        cleanVNForIRTree(gvn, p);
    }
}


void cleanVNForIRTree(GVN * gvn, IR const* ir)
{
    if (gvn != nullptr) { gvn->cleanVNIRTree(ir); }
}

} //namespace xoc
