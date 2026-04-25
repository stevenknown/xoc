/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

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
    VN_kind(newvn) = VN_OP;
    VN_op(newvn) = irt;
    return newvn;
}


VN const* IRCAndVNHash::registerVN(IR_CODE irt, MOD IntList & ilst)
{
    ASSERT0(irt <= IR_CODE_NUM); //NOTE: IR_CODE_NUM means MDPhi.
    ilst.append_head((IntType)irt);
    VN * newvn = registerVN(ilst);
    ASSERT0(newvn);
    VN_kind(newvn) = VN_OP;
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


bool InferEVN::is_valid() const
{
    return m_gvn->is_valid();
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
    VN_kind(newvn) = VN_OP;
    VN_op(newvn) = ir->getCode();
    setVN(ir, newvn);
    return newvn;
}


VN const* InferEVN::allocVNForMDDef(MDDef const* mddef, InferCtx & ctx)
{
    VN const* vn = getVN(mddef);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    VN_kind(newvn) = VN_MDDEF;
    VN_mddef(newvn) = mddef;
    setVN(mddef, newvn);
    return newvn;
}


VN const* InferEVN::allocVNForVMD(VMD const* vmd, InferCtx & ctx)
{
    VN const* vn = getVN(vmd);
    if (vn != nullptr) { return vn; }
    VN * newvn = m_gvn->allocVN();
    VN_kind(newvn) = VN_VMD;
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
    VN_kind(newvn) = VN_OP;
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
        VN_kind(newvn) = VN_OP;
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
    if (killdef->hasSideEffect(true)) { return nullptr; }
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
    IR * kdef = xoc::findKillingDef(ir, m_rg, &ctx.getOptCtx());
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
    IR * kdef = xoc::findKillingDef(ir, m_rg, &ctx.getOptCtx());
    if (kdef == nullptr) {
        //CASE:exec/evn.c
        //...=arr($1,$x) #S1
        //starr($2,$y)=...
        //...=arr($1,$x) #S2
        //VN of arr($1,$x) in #S1 can NOT be inferred through $1 and ARR code
        //because starr($2,$y) may alias with arr($1,$x).
        if (mayExistAlias()) { return nullptr; }
        return inferVNViaArrayKidAndOfst(ir, ctx);
    }
    return inferAndGenVNForKillingDef(ir, kdef, ctx);
}


VN const* InferEVN::inferIndirectMemExp(IR const* ir, InferCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->isIndirectMemOp() && ir->is_exp());
    VN const* vn = getVN(ir);
    if (vn != nullptr) { return vn; }
    IR * kdef = xoc::findKillingDef(ir, m_rg, &ctx.getOptCtx());
    if (kdef == nullptr) {
        //CASE:exec/evn.c
        //...=ild($1) #S1
        //ist($2)=...
        //...=ild($1) #S2
        //VN of ild($1) in #S1 can NOT be inferred through $1 and ILD code
        //because ist($2) may alias with ild($1).
        if (mayExistAlias()) { return nullptr; }
        return inferVNViaBaseAndOfst(ir, ctx);
    }
    ASSERT0(kdef->hasResult());

    //CASE:Following case illustrates that even 'ir' does not have a MustRef,
    //the killing-def is also can be found.
    //e.g:int foo()
    //    {
    //      int a,b,*p;
    //      if (i) p = &a;
    //      else p = &b;
    //      *p = 10;
    //      c = *p; #*p does not have a MustRef. However the compiler could
    //              #find its killing-def.
    //    }
    //ASSERT0(ir->getMustRef());
    //ASSERT0(kdef->getMustRef());

    if (ir->getMustRef() != nullptr &&
        (ir->getMustRef() != kdef->getMustRef())) {
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
    if (ir->hasSideEffect(true)) { return nullptr; }
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

} //namespace xoc
