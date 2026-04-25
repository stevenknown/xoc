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
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

static void dumpActLoopRedDep(
    Vectorization const* vect, VectCtx const& ctx, MDDef const* mddef)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    xcom::StrBuf tmp(32);
    ASSERT0(ctx.getBIV());
    IV const* iv = ctx.getBIV();
    MD const* resmd = mddef->getResultMD(ctx.getMDSystem());
    ASSERT0(resmd);
    mgr.dumpAct(
        "MD%u is unsuitable to be vectorized, "
        "because it is not BIV and leads to loop-reduce dependence, "
        "the BIV is %s",
        resmd->id(), iv->dump(ctx.getVarMgr(), tmp));
}


static void dumpActLoopRedDep(
    Vectorization const* vect, VectCtx const& ctx, IR const* ir)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    xcom::StrBuf tmp(32);
    ASSERT0(ctx.getBIV());
    IV const* iv = ctx.getBIV();
    mgr.dumpAct(
        "%s is unsuitable to be vectorized, "
        "because it is not BIV and leads to loop-reduce dependence, "
        "the BIV is %s",
        DumpIRName().dump(ir), iv->dump(ctx.getVarMgr(), tmp));
}


static void dumpActLoopRedDep(
    Vectorization const* vect, VectCtx const& ctx, LoopDepInfo const& info)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    ASSERT0(ctx.getBIV());
    BIV const* biv = ctx.getBIV();
    if (biv->isRefBIV(info.getSrc())) {
        dumpActLoopRedDep(vect, ctx, info.getSrc());
        return;
    }
    if (info.isTgtIR()) {
        dumpActLoopRedDep(vect, ctx, info.getTgtIR());
        return;
    }
    if (info.isTgtMDDef()) {
        dumpActLoopRedDep(vect, ctx, info.getTgtMDDef());
        return;
    }
    UNREACHABLE();
}


static void dumpActTargUnsupport(Vectorization const* vect, IR const* ir)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct(
        "%s is unsuitable to be vectorized, "
        "because target machine does not support this type of "
        "vector operation.",
        DumpIRName().dump(ir));
}


static void dumpActComplexType(Vectorization const* vect, IR const* ir)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct("%s's type is too complicated to vectorize",
                DumpIRName().dump(ir));
}


static void dumpActNonRelaxLinRep(
    Vectorization const* vect, IR const* ir, IVLinearRep const& linrep)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpLinRepAct(linrep, "%s is not relax linear-representation of IV",
                      DumpIRName().dump(ir));
}


static void dumpActNonLinRep(
    Vectorization const* vect, IR const* ir, IVLinearRep const& linrep)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpLinRepAct(linrep, "%s is not linear-representation of IV",
                      DumpIRName().dump(ir));
}


static void dumpActNonLinRepBase(
    Vectorization const* vect, IR const* ir, IVLinearRep const& linrep)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpLinRepAct(linrep,
        "%s does not access memory via linear-representation of IV",
        DumpIRName().dump(ir));
}


static void dumpLoopCarDep(
    Vectorization const* vect, IR const* ir, LoopDepInfo const& info)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf tmp(16);
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct(ir, "%s and %s may have loop-carried dependence",
                DumpIRName().dump(info.getSrc()), info.dumpTgt(tmp));
}


static void dumpUnsuitableResCand(Vectorization const* vect, IR const* ir)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct(ir,
        "It is unsuitable to be vectorized when collecting result candidate");
}


static void dumpActNonLinRep(Vectorization const* vect, IR const* ir)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct(
        "%s is not linear-representation of IV, thus it can not be vectorized.",
        DumpIRName().dump(ir));
}


static void dumpActSinceLoopCarrDep(
    Vectorization const* vect, LoopDepInfoSet const& set)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    xcom::StrBuf tmp(16);
    LoopDepInfoSetIter it;
    for (LoopDepInfo const* ldi = set.get_first(it);
         !it.end(); ldi = set.get_next(it)) {
        ASSERT0(ldi);
        if (!ldi->isLoopCarr()) { continue; }
        mgr.dumpAct(
            "%s and %s may have %s dependence "
            "that prevented vectorization.",
            DumpIRName().dump(ldi->getSrc()), ldi->dumpTgt(tmp),
            ldi->getDepName());
    }
}


static void dumpStrideNoInt(
    Vectorization const* vect, VectAccDesc const& accdesc)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf tmp(32);
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct(
        "IV '%s' step is not constant integer, it is %s",
        accdesc.getIV()->getStmtOccVar()->get_name()->getStr(),
        accdesc.getIV()->getStepVal().dump(vect->getRegion(), 2, tmp));
}


static void dumpStrideNeedMask(
    Vectorization const* vect, VectAccDesc const& accdesc,
    HOST_INT stridebytesize, HOST_INT ir_size)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpAct(accdesc.getIndexVarExp(),
        "The stride byte size is %u. However it is not equal to the "
        "data type size %u of each memory accessing of the IR occurrence.",
        stridebytesize, ir_size);
}


static void dumpStrideNoLinRep(
    Vectorization const* vect, VectAccDesc const& accdesc)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dumpLinRepAct(accdesc.getLinearAcc(),
        "linear-representation has non-integer coefficient of IV "
        "during make vector-op");
}


static void dumpIVBound(IVBoundInfo const& bi, Vectorization const* vect,
                        LI<IRBB> const* li)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    xcom::StrBuf buf(32);
    vect->getRegion()->getLogMgr()->incIndent(2);
    bi.dumpBuf(vect->getRegion(), buf);
    vect->getRegion()->getLogMgr()->decIndent(2);
    mgr.dump("Find IV boundary of LOOP%u is:%s", li->id(), buf.getBuf());
}


static void dumpIVNoBound(Vectorization const* vect, LI<IRBB> const* li)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dump("COMPUTE IV BOUND:can not compute the end bound of LOOP%u",
             li->id());
}


static void dumpVectUseLICM(Vectorization const* vect)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dump("Vectorization uses LICM info to determine whether an operation "
             "can be vectorized.");
}


static void dumpNoResVect(Vectorization const* vect)
{
    if (!vect->getRegion()->isLogMgrInit()) { return; }
    VectActMgr & mgr = const_cast<VectActMgr&>(vect->getActMgr());
    mgr.dump("Vectorization can not find the result vector "
             "operation from candidate stmt list");
}


//Collect stmts that outside loop body as the first-round effect IRs to DCE.
static void collectStmtOutsideLoop(
    LI<IRBB> const* li, BBList const* bblst, OUT ConstIRList & efflst,
    OUT DCECtx & dcectx)
{
    BBListIter it;
    for (IRBB * bb = bblst->get_head(&it);
         bb != nullptr; bb = bblst->get_next(&it)) {
        if (li->isInsideLoop(bb->id())) {
            BBIRListIter irit;
            for (IR const* ir = bb->getIRList().get_head(&irit);
                 ir != nullptr; ir = bb->getIRList().get_next(&irit)) {
                if (ir->is_phi()) {
                    //CASE:rp7.c, do NOT remove PHI because its value may be
                    //referenced outside loop.
                    //  BB2:
                    //  phi $13 = $12, $9; #S1
                    //  truebr $13 L2;     #S2
                    //  ....
                    //  BB4:L2
                    //  return $13;        #S3
                    //After DCE, #S1's operand will be updated to:
                    //  BB2:
                    //  phi $13 = $12;     #S1
                    //  BB4:L2
                    //  return $13;        #S3
                    continue;
                }
                dcectx.addExcludeStmt(ir);
            }
            continue;
        }
        BBIRListIter irit;
        for (IR const* ir = bb->getIRList().get_head(&irit);
             ir != nullptr; ir = bb->getIRList().get_next(&irit)) {
            efflst.append_tail(ir);
        }
    }
}


//
//START VectAccDesc
//
void VectAccDesc::set(IVLinearRep const& rep)
{
    ASSERT0(m_occ);
    m_linear_access = rep;
}


bool VectAccDesc::computeIndexVarStrideElemNum(HOST_UINT & elemnum) const
{
    HOST_INT coeff_of_var = m_linear_access.getIntCoeff();
    IV const* iv = getIV();
    if (!iv->isStepValInt()) { return false; }
    elemnum = coeff_of_var * iv->getStepValInt();
    return true;
}


bool VectAccDesc::computeIndexVarStrideByteSize(
    TypeMgr const* tm, HOST_INT & bytesize) const
{
    HOST_UINT elemnum;
    if (!computeIndexVarStrideElemNum(elemnum)) { return false; }
    ASSERT0(tm);
    IR const* occ = getOcc();
    ASSERT0(occ);
    if (occ->isIndirectMemOp()) {
        //To indirect-op, the element size is always treated as one byte.
        //Thus the elemnum is equal to the total byte size.
        bytesize = elemnum;
        return true;
    }
    ASSERT0(occ->isArrayOp());
    HOST_INT ir_size = (HOST_INT)tm->getByteSize(occ->getType());
    bytesize = elemnum * ir_size;
    return true;
}


void VectAccDesc::dump(Region const* rg) const
{
    note(rg, "\n-- VectAccDesc --");
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nOCC:");
    if (m_occ != nullptr) {
        rg->getLogMgr()->incIndent(2);
        xoc::dumpIR(m_occ, rg);
        rg->getLogMgr()->decIndent(2);
    }
    note(rg, "\nLINEARREP:");
    rg->getLogMgr()->incIndent(2);
    m_linear_access.dump(rg);
    rg->getLogMgr()->decIndent(2);
    rg->getLogMgr()->decIndent(2);
}
//END VectAccDesc


//
//START VectAccDescMgr
//
void VectAccDescMgr::clean()
{
    IR2DescIter it;
    VectAccDesc * desc;
    for (m_ir2desc.get_first(it, &desc); !it.end();
         m_ir2desc.get_next(it, &desc)) {
        delete desc;
    }
    m_ir2desc.clean();
}


void VectAccDescMgr::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n-- VectAccDescMgr --");
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nTHERE ARE %u RECOGNIZED VECTOR OPERATION",
         m_ir2desc.get_elem_count());
    IR2DescIter it;
    VectAccDesc * desc;
    for (IR const* ir = m_ir2desc.get_first(it, &desc);
         !it.end(); ir = m_ir2desc.get_next(it, &desc)) {
        ASSERT0(desc);
        note(rg, "\n----");
        xoc::dumpIR(ir, rg);
        rg->getLogMgr()->incIndent(2);
        desc->dump(rg);
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);
}


VectAccDesc * VectAccDescMgr::genDesc(IR const* ir)
{
    VectAccDesc * desc = m_ir2desc.get(ir);
    if (desc != nullptr) { return desc; }
    desc = new VectAccDesc(ir);
    m_ir2desc.set(ir, desc);
    return desc;
}
//END VectAccDescMgr


//
//START VectActMgr
//
void VectActMgr::dumpAct(CHAR const* format, ...) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    VectActMgr * pthis = const_cast<VectActMgr*>(this);
    ASSERT0(format);
    va_list args;
    va_start(args, format);
    pthis->dump_args(format, args);
    va_end(args);
}


void VectActMgr::dumpAct(IR const* ir, CHAR const* format, ...) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    ASSERT0(ir);
    xcom::StrBuf tmpbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    VectActMgr * pthis = const_cast<VectActMgr*>(this);
    ActHandler acth = pthis->dump("VECTORIZE:");
    if (format != nullptr) {
        acth.info->strcat("%s", tmpbuf.getBuf());
    }
    getRegion()->getLogMgr()->incIndent(2);
    xcom::StrBuf irbuf(64);
    xoc::dumpIRToBuf(ir, getRegion(), irbuf);
    getRegion()->getLogMgr()->decIndent(2);
    acth.info->strcat(irbuf);
}


void VectActMgr::dumpLinRepAct(
    IVLinearRep const& linrep, CHAR const* format, ...) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    ASSERTN(format, ("no action info"));
    VectActMgr * pthis = const_cast<VectActMgr*>(this);
    xcom::StrBuf tmpbuf(64);
    va_list args;
    va_start(args, format);
    tmpbuf.vstrcat(format, args);
    va_end(args);
    pthis->dump("LinearRepAct:%s", tmpbuf.buf);
}
//END VectActMgr


//
//START VectOp
//
void VectOp::dump(Region const* rg) const
{
    TypeMgr const* tm = rg->getTypeMgr();
    note(rg, "\nVECTOP id:%u", id());
    rg->getLogMgr()->incIndent(2);

    note(rg, "\nVECTOPCODE:");
    if (getOcc() != nullptr) {
        prt(rg, "%s", IRNAME(getOcc()));
    }

    note(rg, "\nOCC:");
    if (getOcc() != nullptr) {
        rg->getLogMgr()->incIndent(2);
        xoc::dumpIR(getOcc(), rg);
        rg->getLogMgr()->decIndent(2);
    }

    note(rg, "\nEXPECTED_TYPE:");
    if (getExpectType() != nullptr) {
        rg->getLogMgr()->incIndent(2);
        note(rg, "\n");
        getExpectType()->dump(tm);
        rg->getLogMgr()->decIndent(2);
    }

    note(rg, "\nOPND:");
    for (UINT i = 0; i < m_opvec.get_elem_count(); i++) {
        rg->getLogMgr()->incIndent(2);
        VectOp const* opnd = getOpnd(i);
        ASSERT0(opnd);
        opnd->dump(rg);
        rg->getLogMgr()->decIndent(2);
    }

    rg->getLogMgr()->decIndent(2);
}


bool VectOp::verify() const
{
    IR const* occ = getOcc();
    if (occ != nullptr) {
        ASSERT0(getExpectType());
        if (occ->isBinaryOp() || occ->isUnaryOp() || occ->is_select()) {
            ASSERT0(getNumOfOpnd() == IR_MAX_KID_NUM(occ));
        }
    }
    for (UINT i = 0; i < m_opvec.get_elem_count(); i++) {
        VectOp const* opnd = getOpnd(i);
        ASSERT0(opnd);
        ASSERT0(opnd->verify());
    }
    return true;
}


bool VectOp::isOpnd(IR const* ir) const
{
    VectOp * pthis = const_cast<VectOp*>(this);
    UINT cnt = pthis->getOpndVec().get_elem_count();
    for (UINT i = 0; i < cnt; i++) {
        if (getOpndOcc(i) == ir) {
            return true;
        }
    }
    return false;
}


void VectOp::addOpnd(IR const* occ, Type const* expty, MOD VectOpMgr & mgr)
{
    ASSERT0(occ && expty);
    VectOp * vop = mgr.alloc();
    VECTOP_occ(vop) = occ;
    VECTOP_expected_type(vop) = expty;
    addOpnd(vop);
}
//END VectOp


//
//START VectCtx
//
VectCtx::VectCtx(LI<IRBB> const* li, IVBoundInfo const* bi, OptCtx & oc,
                 Vectorization * vect, ActMgr * am, IVRCtx const* ivrctx)
    : PassCtx(&oc, am)
{
    ASSERT0(li && bi);
    m_li = li;
    m_vect = vect;
    m_ivr = m_vect->getIVR();
    m_cfg = getRegion()->getCFG();
    m_irmgr = getRegion()->getIRMgr();
    m_iv_bound_info = bi;
    m_licm_anactx = nullptr;
    m_vm = getRegion()->getVarMgr();
    m_mdsys = getRegion()->getMDSystem();
    m_ifcvs = (IfConversion*)getRegion()->getPassMgr()->registerPass(
        PASS_IF_CONVERSION);
    ASSERT0(m_ifcvs);
    m_lrmgr = new LinearRepMgr(getRegion(), oc);
    m_vectaccdesc_mgr = new VectAccDescMgr();
    ASSERT0(getGVN());
    m_infer_evn = getGVN()->getAndGenInferEVN();
    m_epilloop_comp_remain = nullptr;
    m_ivrctx = ivrctx;
}


VectCtx::~VectCtx()
{
    delete m_lrmgr;
    delete m_vectaccdesc_mgr;
    m_infer_evn = nullptr;
    for (IR const* t = getPrerequisiteOpList().get_head();
         t != nullptr; t = getPrerequisiteOpList().get_next()) {
        ASSERTN(t->getBB() == nullptr, ("stmt has been inserted into a BB"));
        getRegion()->freeIRTree(const_cast<IR*>(t));
    }
    for (IR const* t = getInitOpList().get_head();
         t != nullptr; t = getInitOpList().get_next()) {
        ASSERTN(t->getBB() == nullptr, ("stmt has been inserted into a BB"));
        getRegion()->freeIRTree(const_cast<IR*>(t));
    }
}


void VectCtx::CandList::append(IR * ir)
{
    if (find(ir)) { return; }
    xcom::EList<IR*, IR2Holder>::append_tail(ir);
}


void VectCtx::ResCandList::append(IR * ir)
{
    if (find(ir)) { return; }
    xcom::EList<IR*, IR2Holder>::append_tail(ir);
}


void VectCtx::ResConstCandList::append(IR const* ir)
{
    if (find(ir)) { return; }
    xcom::EList<IR const*, ConstIR2Holder>::append_tail(ir);
}


void VectCtx::VOpList::append(VectOp * vop)
{
    ASSERT0(!find(vop));
    xcom::List<VectOp*>::append_tail(vop);
}


void VectCtx::ResOpList::append(IR * ir)
{
    ASSERT0(!find(ir));
    xcom::List<IR*>::append_tail(ir);
}


void VectCtx::GenedStmtList::append(IR * ir)
{
    ASSERT0(!find(ir));
    IRList::append_tail(ir);
}


bool VectCtx::isRedStmt(IR const* ir) const
{
    ASSERT0(IVBI_iv(*m_iv_bound_info)->getRedStmt());
    return ir == IVBI_iv(*m_iv_bound_info)->getRedStmt();
}


bool VectCtx::isTCImm() const
{
    return m_iv_bound_info->isTCImm();
}


bool VectCtx::isIVEndBoundStmt(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    ASSERT0(m_iv_bound_info->getBound());
    return ir == m_iv_bound_info->getBound();
}


BIV const* VectCtx::getBIV() const
{
    return IVBI_iv(*m_iv_bound_info);
}


static void dumpMandaPath(VectCtx const& ctx)
{
    note(ctx.getRegion(), "\n-- MANDATORY PATH:");
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    for (BSIdx i = pctx.getMandaPath().get_first();
         i != BS_UNDEF; i = pctx.getMandaPath().get_next(i)) {
        if (i != pctx.getMandaPath().get_first()) {
            prt(pctx.getRegion(), ",");
        }
        prt(pctx.getRegion(), "%u", i);
    }
}


void VectCtx::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    note(m_rg, "\n==-- DUMP VectCtx --==");
    m_rg->getLogMgr()->incIndent(2);
    if (getLI() != nullptr) {
        getLI()->dump(getRegion());
    }
    dumpMandaPath(*this);
    if (getIVBoundInfo() != nullptr) {
        getIVBoundInfo()->dump(m_rg);
    }
    getVectAccDescMgr().dump(m_rg);
    VectCtx * pthis = const_cast<VectCtx*>(this);
    if (pthis->getCandList().get_elem_count() > 0) {
        //Dump collected IR stmt that can be vector candidate.
        note(m_rg, "\n-- CAND STMT LIST --");
        m_rg->getLogMgr()->incIndent(2);
        CandListIter it;
        for (IR * ir = pthis->getCandList().get_head(&it);
             ir != nullptr; ir = pthis->getCandList().get_next(&it)) {
            xoc::dumpIR(ir, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getInitOpList().get_elem_count() > 0) {
        note(m_rg, "\n-- INIT STMT LIST --");
        m_rg->getLogMgr()->incIndent(2);
        for (IR const* op = pthis->getInitOpList().get_head();
             op != nullptr; op = pthis->getInitOpList().get_next()) {
            xoc::dumpIR(op, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getPrerequisiteOpList().get_elem_count() > 0) {
        note(m_rg, "\n-- PREREQUISITE STMT LIST --");
        m_rg->getLogMgr()->incIndent(2);
        for (IR const* op = pthis->getPrerequisiteOpList().get_head();
             op != nullptr; op = pthis->getPrerequisiteOpList().get_next()) {
            xoc::dumpIR(op, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getResCandList().get_elem_count() > 0) {
        note(m_rg, "\n-- RESULT-CAND STMT LIST --");
        m_rg->getLogMgr()->incIndent(2);
        VectCtx::ResCandListIter it;
        for (pthis->getResCandList().get_head(&it);
             it != nullptr; pthis->getResCandList().get_next(&it)) {
            IR * ir = it->val();
            xoc::dumpIR(ir, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getCandVOpList().get_elem_count() > 0) {
        //Dump well-formatted vector-operation descriptor that will be
        //the final IR stmt with vector-type.
        note(m_rg, "\n-- MADE CAND VECT-OP DESCRIPTOR --");
        m_rg->getLogMgr()->incIndent(2);
        for (VectOp const* vop = pthis->getCandVOpList().get_head();
             vop != nullptr; vop = pthis->getCandVOpList().get_next()) {
            vop->dump(m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getMainLoopResOpList().get_elem_count() > 0) {
        //Dump the finally generated vector operation.
        note(m_rg, "\n-- GENERATED MAIN LOOP RESULT VECTOR-OP LIST --");
        m_rg->getLogMgr()->incIndent(2);
        for (IR const* op = pthis->getMainLoopResOpList().get_head();
             op != nullptr; op = pthis->getMainLoopResOpList().get_next()) {
            ASSERT0(op->is_stmt());
            xoc::dumpIR(op, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getEpilLoopResOpList().get_elem_count() > 0) {
        //Dump the finally generated vector operation.
        note(m_rg, "\n-- GENERATED EPILOG LOOP RESULT VECTOR-OP LIST --");
        m_rg->getLogMgr()->incIndent(2);
        for (IR const* op = pthis->getEpilLoopResOpList().get_head();
             op != nullptr; op = pthis->getEpilLoopResOpList().get_next()) {
            ASSERT0(op->is_stmt());
            xoc::dumpIR(op, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getGeneratedStmtList().get_elem_count() > 0) {
        //Dump totally generated stmts in current vectorization phase.
        note(m_rg, "\n-- ALL GENERATED STMT LIST --");
        m_rg->getLogMgr()->incIndent(2);
        IRListIter it;
        for (IR const* stmt = pthis->getGeneratedStmtList().get_head(&it);
             stmt != nullptr;
             stmt = pthis->getGeneratedStmtList().get_next(&it)) {
            ASSERT0(stmt->is_stmt());
            xoc::dumpIR(stmt, m_rg);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    if (getActMgr() != nullptr) {
        getActMgr()->dump();
    }
    m_rg->getLogMgr()->decIndent(2);
}


bool VectCtx::isIV(IR const* ir) const
{
    ASSERT0(m_ivr);
    return m_ivr->isIV(m_li, ir, nullptr);
}


bool VectCtx::verify() const
{
    VectCtx * pthis = const_cast<VectCtx*>(this);
    ConstIRTab irtab;
    IRListIter lstit;
    for (IR const* s = pthis->getGeneratedStmtList().get_head(&lstit);
         s != nullptr; s = pthis->getGeneratedStmtList().get_next(&lstit)) {
        ASSERTN(!irtab.find(s), ("replicated stmt in list"));
        irtab.append(s);
    }
    return true;
}


void VectCtx::cleanIfVectFailed()
{
    getCandList().clean();
    getPrerequisiteOpList().clean();
}


void VectCtx::cleanAfterLoopReconstruct()
{
    m_li = nullptr;
    m_iv_bound_info = nullptr;
    getVectAccDescMgr().clean();
    getCandList().clean();

    //NOTE: the stmt that recorded in prereq-list should be freed after
    //new vector operation generated because all vector-op dependent prereq-op
    //are duplicated from original prereq-op.
    //getPrerequisiteOpList().clean();
    getResCandList().clean();
    getCandVOpList().clean();
}


void VectCtx::addGeneratedStmtFromBB(IRBB const* bb)
{
    BBIRListIter it;
    for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         ir != nullptr; ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (ir->is_label()) { continue; }
        getGeneratedStmtList().append(ir);
    }
}


void VectCtx::recordEpillLoopCompRemain(IR const* comp_remain)
{
    ASSERT0(comp_remain);
    ASSERT0(comp_remain->is_stmt() || comp_remain->is_exp());
    ASSERT0(m_epilloop_comp_remain == nullptr);
    m_epilloop_comp_remain = comp_remain;
}


UINT VectCtx::getCurInd() const
{
    return getRegion()->getLogMgr()->getIndent();
}
//END VectCtx


//
//START GenerateMask
//
class GenerateMask {
public:
    //Return true if generate mask-op successful.
    bool tryGenerateMaskVectOp(MOD VectCtx & ctx);
};

bool GenerateMask::tryGenerateMaskVectOp(MOD VectCtx & ctx)
{
    IVBoundInfo const* boundinfo = ctx.getIVBoundInfo();
    //ASSERTN(!boundinfo->isTCImm(), ("no need to generate loop"));
    BIV const* biv = boundinfo->getBIV();
    ASSERT0(biv);
    //IR * iv = ctx.getVect()->buildRefIV(biv);
    //IR * init = biv->genInitExp(m_irmgr);
    //IR * step = genBIVStepExpWithMaxVectorElemNum(biv, ctx);
    //IR * det = biv->genBoundExp(*boundinfo, m_ivr, m_irmgr, m_rg);

    IR const* ivref = nullptr;
    IR const* bound_stmt = boundinfo->getBound();
    ASSERT0(bound_stmt && bound_stmt->isConditionalBr());
    bool is_closed_range = false;
    IR const* upper_bound = nullptr;
    if (!ctx.getIVR()->extractIVBoundExpFromStmt(
            biv, bound_stmt, &ivref, &upper_bound, &is_closed_range)) {
        ctx.getActMgr()->dumpAct(
            "can not generate mask op for LOOP%u because extracting"
            " loop-boundary info is failed.", ctx.getLI()->id());
        return false;
    }
    ASSERT0(ivref && upper_bound);
    DUMMYUSE(is_closed_range);
    IR * iv = ctx.getRegion()->dupIRTree(ivref);

    //remain = upper_bound - iv;
    //mask = (1 << remain) - 1;
    //Replace vect_op with masked_vect_op, which masked_vop = vect_op, mask;
    Type const* ivty = iv->getType();
    IRMgrExt * irmgrext = ctx.getIRMgrExt();
    Region * rg = ctx.getRegion();
    IR * comp_remain = irmgrext->buildStorePR(
        irmgrext->buildBinaryOpSimp(
            IR_SUB, ivty, rg->dupIRTree(upper_bound), iv));
    ctx.getActMgr()->dumpAct(
        "the trip-count computation of remain loop is:%s\n"
        "where %s\n is IV",
        DumpIRTree().dump(comp_remain, rg, ctx.getCurInd() + 2),
        DumpIRTree().dump(iv, rg, ctx.getCurInd() + 2));

    IR * comp_mask = irmgrext->buildStorePR(
        irmgrext->buildBinaryOpSimp(IR_SUB, ivty,
            irmgrext->buildBinaryOpSimp(IR_LSL, ivty,
                irmgrext->buildImmInt(1, ivty),
                rg->dupIsomoExpTree(comp_remain)),
            irmgrext->buildImmInt(1, ivty)));
    ctx.getActMgr()->dumpAct(
        "the mask computation of remain part data is: %s\n",
        DumpIRTree().dump(comp_mask, rg, ctx.getCurInd() + 2));

    ctx.recordEpillLoopCompRemain(comp_remain);
    ctx.getEpilLoopMaskOpList().append_tail(comp_remain);
    ctx.getEpilLoopMaskOpList().append_tail(comp_mask);
    VectCtx::ResOpListIter it;
    for (IR const* vop = ctx.getMainLoopResOpList().get_head(&it);
         vop != nullptr; vop = ctx.getMainLoopResOpList().get_next(&it)) {
        ASSERT0(vop->hasRHS());
        //IR * dup_vop = rg->dupIsomoStmtExceptRHS(vop);
        IR * masked_vop = irmgrext->buildMaskOp(
            rg->dupIRTreeList(vop->getRHS()),
            rg->dupIsomoExpTree(comp_mask),
            CMaskOp::UNDISTURBED, vop->getType());
        IR * dup_vop = irmgrext->buildSelectStoreStmtViaIsomoIR(
            vop, masked_vop, rg->dupIsomoExpTree(comp_mask), vop->getType());
        ASSERT0(dup_vop->isPartialStoreStmt());
        ctx.getEpilLoopResOpList().append_tail(dup_vop);
        ctx.getActMgr()->dumpAct(
            "generate mask operation for remain part data:%s\n",
            DumpIRTree().dump(dup_vop, rg, ctx.getCurInd() + 2));
        ASSERT0(xoc::verifyIRList(dup_vop, ctx.getRegion()));
    }
    return true;
}
//END GenerateMask


//
//START Vectorization
//
bool Vectorization::useLICM() const
{
    return m_licm != nullptr && m_licm->is_valid();
}


HOST_UINT Vectorization::getMaxVectorByteSize(
    Type const* elemty, VectCtx const& ctx) const
{
    ASSERT0(elemty);
    HOST_UINT maxelemnum = getMaxVectorElemNum(elemty, ctx);
    ASSERT0(maxelemnum > 0);
    return maxelemnum * m_tm->getByteSize(elemty);
}


HOST_UINT Vectorization::getMaxVectorElemNum(
    Type const* elemty, VectCtx const& ctx) const
{
    IVBoundInfo const* bi = ctx.getIVBoundInfo();
    ASSERT0(bi);
    HOST_UINT maxelemnum = 0;
    if (bi->isTCImm()) {
        HOST_INT tripcount = bi->getTCImm();
        ASSERT0(tripcount > 0);
        maxelemnum = tripcount;
    } else {
        HOST_UINT maxsz = getMaxVectorRegisterByteSize();
        maxelemnum = maxsz / m_tm->getByteSize(elemty);
    }
    ASSERT0(maxelemnum > 0);
    return maxelemnum;
}


static bool isLoopBound(IR const* ir, VectCtx const& ctx)
{
    IVBoundInfo const* bi = ctx.getIVBoundInfo();
    return ir == bi->getBound();
}


bool VectCtx::isOnMandaPath(IRBB const* bb) const
{
    ASSERT0(bb);
    return const_cast<VectCtx*>(this)->getMandaPath().is_contain(bb->id());
}


static bool isDiamondRegionOnMandatoryPath(
    DiamondRegion const& dr, VectCtx const& ctx)
{
    ASSERT0(dr.verify(ctx.getCFG()));
    if (!ctx.isOnMandaPath(dr.top)) { return false; }
    if (!ctx.isOnMandaPath(dr.bottom)) { return false; }
    if (!dr.isTri()) {
        if (!ctx.isOnMandaPath(dr.left) && !ctx.isOnMandaPath(dr.right)) {
            //There are two successors of 'top'.
            //Thus, there must be at least one successor on the mandatory path.
            return false;
        }
    }
    return true;
}


static bool isIVRelatedForArrayOp(IR const* ir, VectCtx const& ctx)
{
    ASSERT0(ir->isArrayOp());
    IR const* subexp = ((CArray*)ir)->getSubExpOfLowestDim();
    ASSERT0(subexp);
    IVR const* ivr = ctx.getIVR();
    ASSERT0(ivr);
    if (ivr->isLinearRepOfIV(ctx.getLI(), subexp, nullptr, *ctx.getIVRCtx())) {
        dumpActNonLinRep(ctx.getVect(), subexp);
        return true;
    }
    return false;
}


static bool isIVRelatedForIndirectOp(IR const* ir, VectCtx const& ctx)
{
    ASSERT0(ir->isIndirectMemOp());
    IR const* base = ir->getBase();
    IVR const* ivr = ctx.getIVR();
    ASSERT0(ivr);
    if (ivr->isLinearRepOfIV(ctx.getLI(), base, nullptr, *ctx.getIVRCtx())) {
        dumpActNonLinRep(ctx.getVect(), base);
        return true;
    }
    return false;
}


static bool isIVRelated(IR const* ir, VectCtx const& ctx)
{
    if (ctx.isIV(ir)) { return true; }
    if (ir->isIndirectMemOp()) {
        return isIVRelatedForIndirectOp(ir, ctx);
    }
    if (ir->isArrayOp()) {
        return isIVRelatedForArrayOp(ir, ctx);
    }
    dumpActNonLinRep(ctx.getVect(), ir);
    return false;
}


static bool isBranchOpndIVRelated(IR const* ir, VectCtx const& ctx)
{
    ASSERT0(ir->isBranch());
    if (ir->isUnconditionalBr()) { return true; }
    if (!ir->is_truebr() && !ir->is_falsebr()) { return false; }
    IR const* opnd0 = BIN_opnd0(BR_det(ir));
    IR const* opnd1 = BIN_opnd1(BR_det(ir));
    if (!isIVRelated(opnd0, ctx)) {
        return false;
    }
    if (!isIVRelated(opnd1, ctx)) {
        return false;
    }
    return true;
}


static bool isDiamondRegionBalancedForVect(
    DiamondRegion const& dr, VectCtx const& ctx)
{
    //Check if there is a large difference in the number of IR stmts
    //between diamond body of the diamond region. To avoid imbalance.
    ASSERT0(dr.verify(ctx.getCFG()));
    UINT truebodynum = dr.left->getNumOfIR();
    UINT falsebodynum = dr.right->getNumOfIR();
    ASSERT0(dr.verify(ctx.getCFG()));
    UINT maxnum = MAX(truebodynum, falsebodynum);
    UINT minnum = MIN(truebodynum, falsebodynum);
    if (maxnum >= 2 * minnum) {
        //TBD:This is a heuristic strategy to determine the balance.
        return false;
    }
    return true;
}


static void addGotoInDiamondRegionToCandList(
    DiamondRegion const& dr, MOD VectCtx & ctx)
{
    ASSERT0(dr.verify(ctx.getCFG()));
    IR * lastleft = const_cast<IRBB*>(dr.left)->getLastIR();
    ASSERT0(lastleft);
    if (lastleft->isUnconditionalBr()) {
        ctx.getCandList().append(lastleft);
    }
    IR * lastright = const_cast<IRBB*>(dr.right)->getLastIR();
    ASSERT0(lastright);
    if (lastright->isUnconditionalBr()) {
        ctx.getCandList().append(lastright);
    }
}


static bool isCondBranchLegalToVect(IR const* ir, MOD VectCtx & ctx)
{
    DiamondRegion dr;
    IfCvsCtx ifctx(*ctx.getOptCtx(), ctx.getLI(), ctx.getIfCvs(),
                   ctx.getActMgr());
    if (!IfConversion::findDiamondRegion(ir, ifctx, dr)) { return false; }
    if (!isDiamondRegionOnMandatoryPath(dr, ctx)) { return false; }
    if (!isDiamondRegionBalancedForVect(dr, ctx)) { return false; }
    if (!isBranchOpndIVRelated(ir, ctx)) { return false; }
    addGotoInDiamondRegionToCandList(dr, ctx);
    return true;
}


static bool isUncondBranchLegalToVect(IR const* ir, MOD VectCtx & ctx)
{
    ASSERT0(ir->isUnconditionalBr());
    if (ctx.getCandList().find(const_cast<IR*>(ir))) {
        return true;
    }
    LI<IRBB> const* li = ctx.getLI();
    IRCFG const* cfg = ctx.getCFG();
    IRBB * backedge_bb = xoc::findBackEdgeStartBB(li, cfg);
    if (ir->getBB() == backedge_bb) {
        //ir's branch target is the loop-head.
        ASSERT0(cfg->isUniqueSucc(ir->getBB(), li->getLoopHead()));
        ctx.getCandList().append(const_cast<IR*>(ir));
        return true;
    }
    return false;
}


static bool isBranchLegalToVect(IR const* ir, MOD VectCtx & ctx)
{
    ASSERT0(ir->isBranch());
    if (isLoopBound(ir, ctx)) { return true; }
    if (ir->isConditionalBr()) {
        return isCondBranchLegalToVect(ir, ctx);
    }
    if (ir->isUnconditionalBr()) {
        return isUncondBranchLegalToVect(ir, ctx);
    }
    UNREACHABLE();
    return true;
}


bool Vectorization::isLoopInv(IR const* ir, VectCtx const& ctx) const
{
    if (ir->isConstExp()) { return true; }
    InvStmtList const* pinvstmtlist = nullptr;
    if (is_aggressive() && ctx.getLICMAnaCtx() != nullptr) {
        pinvstmtlist = &ctx.getLICMAnaCtx()->getInvStmtList();
    }
    if (ir->is_stmt()) {
        if (pinvstmtlist != nullptr &&
            pinvstmtlist->find(const_cast<IR*>(ir))) {
            return true;
        }
        return false;
    }
    //ir must be exp.
    return xoc::isLoopInvariant(
        ir, ctx.getLI(), m_rg, pinvstmtlist, true, ctx.getOptCtx());
}


//Return true is ir is legal to vectorize.
//ir: stmt.
static bool isStmtLegalToVect(IR const* ir, MOD VectCtx & ctx)
{
    ASSERT0(ir);
    if (ir->is_vec()) { return false; }
    if (ir->isNoMove(true) || ir->hasSideEffect(true) || ir->isDummyOp()) {
        ctx.getActMgr()->dumpAct(ir, "illegal to be vector operation");
        return false;
    }
    if (ir->isBranch()) {
        if (!isBranchLegalToVect(ir, ctx)) {
            ctx.getActMgr()->dumpAct(
                ir, "it is not a suitable branch to be vectorized");
            return false;
        }
        return true;
    }
    if ((ir->isCallStmt() && !ir->isReadOnly()) || ir->is_region()) {
        //TODO: support call/region.
        ctx.getActMgr()->dumpAct(
            ir, "can not vectorize the ir, because there is not yet support"
                " for analyzing function that is not read-only");
        return false;
    }
    return true;
}


bool Vectorization::findSuitableVectOpnd(
    IR const* start, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(start && start->is_exp());
    if (isSuitableToBeVect(start, ctx)) {
        getActMgr().dumpAct(start,
           "findSuitableVectOpnd:find the appropriate vector operand");
        return true;
    }
    if (start->isMemRef()) {
        //Keep searching and try inferring the suitable vector operand through
        //DefUse chain.
        getActMgr().dumpAct(start,
            "findSuitableVectOpnd:seems IR is unsuitable to be vector"
            " operand, try more analysis through DU chain");
        MD const* mustuse = start->getMustRef();
        if (mustuse == nullptr) { return false; }
        IR const* killdef = xoc::findKillingDef(start, m_rg, ctx.getOptCtx());
        if (killdef == nullptr) { return false; }
        if (killdef->is_phi()) {
            //Usually, if killdef is unsuitable PHI operation, it should be
            //intercepted at checkScalarStmt(). However in order to find more
            //opportunities, the function doese not check if killdef is IV
            //operation. Therefore, if we still could not handle the PHI in
            //this function, return false to inform caller 'start' is
            //unsuitable to be vector operand.
            if (!ctx.isIV(killdef) &&
                !xoc::isPhiLoopInvariant(killdef, ctx.getLI(), m_rg)) {
                DumpIRName dn;
                getActMgr().dumpAct(start,
                    "findSuitableVectOpnd:DEF is %s and does not form a"
                    " cycle of IV", dn.dump(killdef));
                return false;
            }
            return true;
        }
        ASSERT0(killdef->is_stmt() && killdef->hasResult());

        //If the vector operation dependent value is still scalar, we have to
        //hoist it to the same vector operation.
        //if (!ctx.isResCand(killdef)) { return false; }
        IR const* rhs = killdef->getRHS();
        ASSERT0(rhs);
        return checkExp(rhs, ctx, ldactx, set);
    }
    return checkExp(start, ctx, ldactx, set);
}


IR * Vectorization::pickOutRedStmt(MOD VectCtx & ctx) const
{
    VectCtx::CandListIter it;
    for (IR * ir = ctx.getCandList().get_head(&it);
         ir != nullptr; ir = ctx.getCandList().get_next(&it)) {
        if (!ctx.isRedStmt(ir)) { continue; }
        IR * redstmt = ir;
        ctx.getCandList().remove(it);
        return redstmt;
    }
    return nullptr;
}


IR * Vectorization::pickOutIVEndBoundStmt(MOD VectCtx & ctx) const
{
    VectCtx::CandListIter it;
    for (IR * ir = ctx.getCandList().get_head(&it);
         ir != nullptr; ir = ctx.getCandList().get_next(&it)) {
        if (!ctx.isIVEndBoundStmt(ir)) { continue; }
        IR * bstmt = ir;
        ctx.getCandList().remove(it);
        return bstmt;
    }
    return nullptr;
}


IR * Vectorization::pickOutBackEdgeJumpStmt(MOD VectCtx & ctx) const
{
    IRBB * backedge_bb = xoc::findBackEdgeStartBB(ctx.getLI(), m_cfg);
    ASSERT0(backedge_bb);
    IR const* jmp = backedge_bb->getLastIR();
    ASSERT0(jmp->isUnconditionalBr());
    VectCtx::CandListIter it;
    for (IR * ir = ctx.getCandList().get_head(&it);
         ir != nullptr; ir = ctx.getCandList().get_next(&it)) {
        if (ir != jmp) { continue; }

        //Pick out the branch.
        ctx.getCandList().remove(it);
        return ir;
    }
    return nullptr;
}


bool Vectorization::isDirectOpLegalToVect(
    IR const* ir, VectCtx const& ctx) const
{
    ASSERT0(ir->isDirectMemOp() || ir->isReadPR());
    if (!ir->getType()->is_simplex()) {
        dumpActComplexType(this, ir);
        return false;
    }
    if (isLoopInv(ir, ctx)) { return true; }
    xoc::LoopDepInfo info;
    if (xoc::isLoopCarried(
        ir, m_rg, is_aggressive(), false, ctx.getLI(), m_gvn, info,
        ctx.getOptCtx())) {
        return false;
    }
    if (xoc::hasLoopReduceDepForIRTree(ir, m_rg, ctx.getLI())) {
        return true;
    }
    return true;
}


bool Vectorization::checkLinRepForIndirectOp(
    IR const* ir, VectCtx const& ctx, OUT IVLinearRep & linrep) const
{
    ASSERT0(ir->isIndirectMemOp());
    if (!ir->getType()->is_simplex()) {
        dumpActComplexType(this, ir);
        return false;
    }
    IR const* base = ir->getBase();
    if (m_ivr->isLinearRepOfIV(ctx.getLI(), base, &linrep, *ctx.getIVRCtx())) {
        //CASE:linrep may describe DIV.
        //ASSERTN(linrep.getIV() == ctx.getBIV(),
        //        ("linear-rep is not about to IV"));
        return true;
    }
    dumpActNonLinRep(this, base, linrep);
    if (m_ivr->isRelaxLinearRepOfIV(ctx.getLI(), base,
            &ctx.getLICMAnaCtx()->getInvStmtList(), ctx.getOptCtx(),
            &linrep, ctx.getLinearRepMgr())) {
        return true;
    }
    dumpActNonRelaxLinRep(this, base, linrep);
    return false;
}


bool Vectorization::checkLinRepForArrayOp(
    IR const* ir, VectCtx const& ctx, OUT IVLinearRep & linrep) const
{
    ASSERT0(ir->isArrayOp());
    IR const* subexp = ((CArray*)ir)->getSubExpOfLowestDim();
    ASSERT0(subexp);
    if (!m_ivr->isLinearRepOfIV(
            ctx.getLI(), subexp, &linrep, *ctx.getIVRCtx())) {
        dumpActNonLinRep(this, subexp);
        return false;
    }
    //CASE:linrep may be base on the DIV, thus no matter whether the linrep
    //is based on BIV or DIV, it can be vectorized legally.
    //ASSERTN(linrep.getIV() == ctx.getBIV(),
    //        ("linear-rep should be based on BIV"));
    return true;
}


static bool tryIfConversion(MOD VectCtx & ctx)
{
    VectCtx::CandListIter it;
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    VectCtx::ResCandList & rescand = pctx.getResCandList();
    Vectorization const* vect = ctx.getVect();
    IfCvsCtx ifctx(
        *ctx.getOptCtx(), ctx.getLI(), ctx.getIfCvs(), ctx.getActMgr());
AGAIN:
    for (IR * ir = pctx.getCandList().get_head(&it);
         ir != nullptr; ir = pctx.getCandList().get_next(&it)) {
        if (!ir->isBranch()) { continue; }
        ifctx.getGenedList().clean();
        DiamondRegion dr;
        if (!IfConversion::findDiamondRegion(ir, ifctx, dr)) {
            dumpUnsuitableResCand(vect, ir);
            rescand.clean();
            return false;
        }
        if (!IfConversion::tryConvertDiamondRegion(dr, ifctx)) {
            dumpUnsuitableResCand(vect, ir);
            rescand.clean();
            return false;
        }
        //Remove the original ir out of the candidate-list, because it has been
        //changed and freed.
        //ir has been changed or recycled.
        pctx.getCandList().remove(ir);

        //Append the new generated stmt into result-candidate-list.
        for (IR * g = ifctx.getGenedList().get_head();
             g != nullptr; g = ifctx.getGenedList().get_next()) {
            rescand.append(g);
        }

        //Rescan candidate-list.
        goto AGAIN;
    }
    return true;
}


//The function collects stmt that is suitable to vectorize from
//given candidate list.
//Return true if find legal stmt that can be vectorized.
static void collectResultCand(VectCtx const& ctx)
{
    VectCtx::CandListIter it;
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    VectCtx::ResCandList & rescand = pctx.getResCandList();
    GVN const* gvn = ctx.getGVN();
    Vectorization const* vect = ctx.getVect();
    bool is_aggr = vect->is_aggressive();
    OptCtx const* oc = ctx.getOptCtx();
    for (IR * ir = pctx.getCandList().get_head(&it);
         ir != nullptr; ir = pctx.getCandList().get_next(&it)) {
        xoc::LoopDepInfo info;
        if (xoc::isLoopCarried(ir, ctx.getRegion(), is_aggr, true, rescand,
                               ctx.getLI(), gvn, info, oc)) {
            //TODO:Handle overlapped stmt by applying loop peeling or
            //loop fission.
            dumpLoopCarDep(vect, ir, info);
            rescand.clean();
            return;
        }
        if (ir->isArrayOp() || ir->isIndirectMemOp()) {
            //Usually, array operation and indirect operation have the most
            //opportunity to do vectorization.
            //Leave the legality checking to followed functions.
            rescand.append(ir);
            continue;
        }
        if (ir->isDirectMemOp() || ir->isPROp()) {
            //Leave the loop reduction checking and array-subscript dependent
            //expresssion checking to followed functions.
            rescand.append(ir);
            continue;
        }
        dumpUnsuitableResCand(vect, ir);
        rescand.clean();
        return;
    }
}


Type const* Vectorization::makeVectType(Type const* elemty, VectCtx const& ctx)
{
    if (!elemty->is_simplex()) { return nullptr; }
    HOST_UINT maxelemnum = getMaxVectorElemNum(elemty, ctx);
    ASSERT0(maxelemnum > 0);
    return m_tm->getVectorType((UINT)maxelemnum, elemty->getDType());
}


bool Vectorization::makeVectOpndByConst(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    return false;
}


IR * Vectorization::buildVectPRResult(Type const* restype, IR * rhs, IRBB * bb)
{
    ASSERT0(restype && bb);
    IR * stmt = m_irmgr->buildStorePR(restype, rhs);
    stmt->setBB(bb);
    return stmt;
}


void Vectorization::addUseToRelatedPROp(MOD IR * exp) const
{
    ASSERT0(exp && exp->is_exp());
    ASSERT0(usePRSSADU());
    m_prssamgr->addUseToDedicatedPRNO(exp);
}


void Vectorization::addDUChain(IR * stmt, IR * exp, VectCtx const& ctx)
{
    xoc::buildDUChain(stmt, exp, m_rg, *ctx.getOptCtx());
}


bool Vectorization::makeVectOpndByBin(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(!ir->is_vec());
    Type const* newopndty = makeVectType(ir->getType(), ctx);
    ASSERT0(newopndty && newopndty->is_vector());
    VECTOP_expected_type(&vectop) = newopndty;
    VECTOP_occ(&vectop) = ir;

    //Generate the prerequiste vector operation of 'vectop'.
    VectOp * vop0 = m_vectop_mgr.alloc();
    VECTOP_expected_type(vop0) = vectop.getExpectType();
    vectop.addOpnd(vop0);
    IR const* op0 = BIN_opnd0(ir);
    bool op0_is_vec = makeVectOpnd(op0, ctx, *vop0);

    //Generate the prerequiste vector operation of 'vectop'.
    VectOp * vop1 = m_vectop_mgr.alloc();
    VECTOP_expected_type(vop1) = vectop.getExpectType();
    vectop.addOpnd(vop1);
    IR const* op1 = BIN_opnd1(ir);
    bool op1_is_vec = makeVectOpnd(op1, ctx, *vop1);

    //Check if operand has been vectorized.
    if (op0_is_vec && op1_is_vec) { return true; }
    if (!op0_is_vec && !op1_is_vec) {
        getActMgr().dumpAct(ir,
            "both operands can not be transformed to vector operation");
        return false;
    }
    //At least one of operands has been vectorized.
    if (op0_is_vec && canBeValidOpndInVectOp(op1, ctx, vectop)) {
        Type const* newopndty = vectop.getExpectType();
        ASSERT0(newopndty && newopndty->is_vector());
        VECTOP_occ(vop1) = op1;
        VECTOP_expected_type(vop1) = newopndty;
        return true;
    }
    //At least one of operands has been vectorized.
    if (op1_is_vec && canBeValidOpndInVectOp(op0, ctx, vectop)) {
        Type const* newopndty = vectop.getExpectType();
        ASSERT0(newopndty && newopndty->is_vector());
        VECTOP_occ(vop0) = op0;
        VECTOP_expected_type(vop0) = newopndty;
        return true;
    }
    getActMgr().dumpAct(ir,
        "operand can not be transformed to vector operation");
    return false;
}


bool Vectorization::makeVectOpndByUna(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->isUnaryOp());
    ASSERT0(!ir->is_vec());
    Type const* newopndty = makeVectType(ir->getType(), ctx);
    ASSERT0(newopndty && newopndty->is_vector());
    VECTOP_expected_type(&vectop) = newopndty;
    VECTOP_occ(&vectop) = ir;

    //Generate the prerequiste vector operation of 'vectop'.
    VectOp * vop0 = getVectOpMgr().alloc();
    VECTOP_expected_type(vop0) = vectop.getExpectType();
    vectop.addOpnd(vop0);
    IR const* op0 = UNA_opnd(ir);
    bool op0_is_vec = makeVectOpnd(op0, ctx, *vop0);

    //Check if operand has been vectorized.
    if (op0_is_vec) { return true; }
    if (canBeValidOpndInVectOp(op0, ctx, vectop)) {
        Type const* newopndty = vectop.getExpectType();
        ASSERT0(newopndty && newopndty->is_vector());
        VECTOP_occ(vop0) = op0;
        VECTOP_expected_type(vop0) = newopndty;
        return true;
    }
    getActMgr().dumpAct(
        ir, "operand can not be transformed to vector operation");
    return false;
}


bool Vectorization::makeVectOpndBySelect(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->is_select());
    ASSERT0(!ir->is_vec());
    Type const* newopndty = makeVectType(ir->getType(), ctx);
    Type const* newdetty = makeVectType(SELECT_det(ir)->getType(), ctx);
    ASSERT0(newopndty && newopndty->is_vector());
    ASSERT0(newdetty && newdetty->is_vector());
    VECTOP_expected_type(&vectop) = newopndty;
    VECTOP_occ(&vectop) = ir;

    //Generate the prerequiste vector operation of 'vectop'.
    VectOp * vop0 = m_vectop_mgr.alloc();
    VECTOP_expected_type(vop0) = vectop.getExpectType();
    vectop.addOpnd(vop0);
    IR const* det = SELECT_det(ir);
    ASSERT0(det);
    bool det_is_vec = makeVectOpnd(det, ctx, *vop0);

    //Generate the prerequiste vector operation of 'vectop'.
    VectOp * vop1 = m_vectop_mgr.alloc();
    VECTOP_expected_type(vop1) = vectop.getExpectType();
    vectop.addOpnd(vop1);
    IR const* trueexp = SELECT_trueexp(ir);
    bool true_is_vec = false;
    if (trueexp != nullptr) {
        true_is_vec = makeVectOpnd(trueexp, ctx, *vop1);
    }

    //Generate the prerequiste vector operation of 'vectop'.
    VectOp * vop2 = m_vectop_mgr.alloc();
    VECTOP_expected_type(vop2) = vectop.getExpectType();
    vectop.addOpnd(vop2);
    IR const* falseexp = SELECT_falseexp(ir);
    bool false_is_vec = false;
    if (falseexp != nullptr) {
        false_is_vec = makeVectOpnd(falseexp, ctx, *vop2);
    }

    //Check if operand has been vectorized.
    if (det_is_vec && true_is_vec && false_is_vec) { return true; }
    if (!true_is_vec && !false_is_vec) {
        getActMgr().dumpAct(ir,
            "both true and false parts can not be transformed "
            "to vector operation");
        return false;
    }
    if (det_is_vec) {
        //Determinator expression should be vectorizable.
        Type const* newopndty = vectop.getExpectType();
        ASSERT0(newopndty && newopndty->is_vector());
        VECTOP_occ(vop0) = det;
        VECTOP_expected_type(vop0) = newdetty;
    } else {
        getActMgr().dumpAct(ir,
            "determintor expression can not be transformed "
            "to vector operation");
        return false;
    }

    //At least one of operands has been vectorized.
    if (true_is_vec) {
        if (falseexp == nullptr) {
            ASSERT0(vectop.isEmptyOpnd(2));
            ASSERT0(vop2->isEmpty());
            return true;
        } else if (canBeValidOpndInVectOp(falseexp, ctx, vectop)) {
            Type const* newopndty = vectop.getExpectType();
            ASSERT0(newopndty && newopndty->is_vector());
            VECTOP_occ(vop2) = falseexp;
            VECTOP_expected_type(vop2) = newopndty;
            return true;
        }
    }
    //At least one of operands has been vectorized.
    if (false_is_vec) {
        if (trueexp == nullptr) {
            ASSERT0(vectop.isEmptyOpnd(1));
            ASSERT0(vop1->isEmpty());
            return true;
        } else if (canBeValidOpndInVectOp(trueexp, ctx, vectop)) {
            Type const* newopndty = vectop.getExpectType();
            ASSERT0(newopndty && newopndty->is_vector());
            VECTOP_occ(vop1) = trueexp;
            VECTOP_expected_type(vop1) = newopndty;
            return true;
        }
    }
    getActMgr().dumpAct(ir,
        "operands can not be transformed to vector operation");
    return false;
}


bool Vectorization::makeVectOpndByReadPR(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->isReadPR());
    ASSERT0(!ir->is_vec());
    ASSERT0(isDirectOpLegalToVect(ir, ctx));
    if (isLoopInv(ir, ctx)) {
        //ir has been guaranteed in linear-rep checking stage.
        checkAndRecomputeVectType(ir, ctx, vectop);

        //Vectorization operation is: res <- opnd[i:i+tripcount]
        VECTOP_occ(&vectop) = ir;
        return true;
    }
    return makeVectOpndByDUChain(ir, ctx, vectop);
}


bool Vectorization::makeVectOpndByDirect(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->isDirectMemOp());
    ASSERT0(ir->is_exp());
    ASSERT0(!ir->is_vec());
    ASSERT0(isDirectOpLegalToVect(ir, ctx));
    if (isLoopInv(ir, ctx)) {
        //ir has been guaranteed in linear-rep checking stage.
        checkAndRecomputeVectType(ir, ctx, vectop);

        //Vectorization operation is: res <- opnd[i:i+tripcount]
        VECTOP_occ(&vectop) = ir;
        return true;
    }
    return makeVectOpndByDUChain(ir, ctx, vectop);
}


bool Vectorization::makeVectOpndByDUChain(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir && ir->is_exp());
    MD const* mustuse = ir->getMustRef();
    if (mustuse == nullptr) { return false; }
    IR const* killdef = xoc::findKillingDef(ir, m_rg, ctx.getOptCtx());
    if (killdef == nullptr) { return false; }
    if (killdef->is_phi()) {
        //If killdef is unsuitable PHI operation, it should be intercepted
        //at checkScalarStmt(). Usually the killdef formed a cycle of IV.
        //Thus it's suitable to be vector operand.
        ASSERT0(ctx.isIV(killdef));
        return false;
    }
    ASSERT0(killdef->is_stmt() && killdef->hasResult());

    //If the vector operation dependent value is still scalar, we have to
    //hoist it to the same vector operation.
    //if (!ctx.isResCand(killdef)) { return false; }
    IR * rhs = killdef->getRHS();
    ASSERT0(rhs);
    return makeVectOpnd(rhs, ctx, vectop);
}


bool Vectorization::makeVectOpndByIndirect(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->isIndirectMemOp());
    ASSERT0(ir->is_exp());
    ASSERT0(!ir->is_vec());
    ASSERT0(ctx.getBIV()->getStepValInt() == 1); //TODO
    IVLinearRep lr;
    if (checkLinRepForIndirectOp(ir, ctx, lr)) {
        //ir has been guaranteed in linear-rep checking stage.
        checkAndRecomputeVectType(ir, ctx, vectop);

        //Vectorization operation is: res <- opnd[i:i+tripcount]
        VECTOP_occ(&vectop) = ir;
        return true;
    }
    return makeVectOpndByDUChain(ir, ctx, vectop);
}


bool Vectorization::makeVectOpndByArray(
    IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(ir->isArrayOp());
    ASSERT0(ir->is_exp());
    ASSERT0(!ir->is_vec());
    ASSERT0(ctx.getBIV()->getStepValInt() == 1); //TODO
    ASSERT0(ir->getType()->is_simplex());
    IVLinearRep lr;
    if (checkLinRepForArrayOp(ir, ctx, lr)) {
        //ir has been guaranteed in linear-rep checking stage.
        IR const* subexp = ((CArray*)ir)->getSubExpOfLowestDim();
        ASSERT0_DUMMYUSE(subexp);
        IVLinearRep linrep;
        ASSERT0(m_ivr->isMultipleOfIV(
            ctx.getLI(), subexp, &linrep, *ctx.getIVRCtx()));
        ASSERT0(linrep.isCoeffEqualTo(1));
        checkAndRecomputeVectType(ir, ctx, vectop);

        //Vectorization operation is: res <- opnd[i:i+tripcount]
        VECTOP_occ(&vectop) = ir;
        return true;
    }
    return makeVectOpndByDUChain(ir, ctx, vectop);
}


bool Vectorization::isSuitableToBeVect(IR const* ir, VectCtx const& ctx) const
{
    ASSERT0(ir->is_stmt() || ir->is_exp());
    if (!ir->getType()->is_simplex()) {
        dumpActComplexType(this, ir);
        return false;
    }
    if (ir->isConstExp()) { return true; }
    if (ir->isArrayOp()) {
        IVLinearRep linrep;
        if (!checkLinRepForArrayOp(ir, ctx, linrep)) {
            dumpActNonLinRepBase(this, ir, linrep);
            return false;
        }
        VectAccDesc * accdesc = ctx.getVectAccDescMgr().genDesc(ir);
        accdesc->set(linrep);
        if (!isStrideSuitableToVect(*accdesc, ctx)) {
            return false;
        }
        return true;
    }
    if (ir->isIndirectMemOp()) {
        IVLinearRep linrep;
        if (!checkLinRepForIndirectOp(ir, ctx, linrep)) {
            dumpActNonLinRepBase(this, ir, linrep);
            return false;
        }
        VectAccDesc * accdesc = ctx.getVectAccDescMgr().genDesc(ir);
        accdesc->set(linrep);
        if (!isStrideSuitableToVect(*accdesc, ctx)) {
            return false;
        }
        return true;
    }
    if (ir->isMemRef()) {
        //If ir is unsuitable scalar operation, it should be intercepted
        //at checkScalarStmt().
        if (ir->is_stmt()) {
            getActMgr().dumpAct(
                ir, "isSuitableToBeVect:scalar stmt can not be vectorized");
            return false;
        }
        if (!isLoopInv(ir, ctx)) {
            getActMgr().dumpAct(ir,
                "isSuitableToBeVect:scalar expression is loop variant,"
                " it may prevent vectorization");
            return false;
        }
        return true;
    }
    return false;
}


bool Vectorization::isStrideSuitableToVect(
    VectAccDesc const& accdesc, VectCtx const& ctx) const
{
    //Target Dependent Code.
    if (!accdesc.getIV()->isStepValInt()) {
        ASSERT0(accdesc.getIV()->getStmtOccVar());
        dumpStrideNoInt(this, accdesc);
        return false; //TODO:support vectorization with stride.
    }
    ASSERT0(accdesc.getLinearAcc().hasVar());

    //CASE:We should permit that access-index can be both DIV and BIV.
    //if (accdesc.getIndexVar(m_rg) != ctx.getBIV()->getExpOccVar()) {
    //    //Index-Var is not BIV, and it is at least DIV.
    //    ASSERTN(accdesc.getIV()->is_div(), ("at least is one kind of IV"));
    //    return false;
    //}
    if (!accdesc.getLinearAcc().hasIntCoeff()) {
        dumpStrideNoLinRep(this, accdesc);
        return false;
    }
    IR const* occ = accdesc.getOcc();
    ASSERT0(occ);
    HOST_INT ir_size = (HOST_INT)m_tm->getByteSize(occ->getType());
    HOST_INT stridebytesize = 0;
    bool has_const_stride = accdesc.computeIndexVarStrideByteSize(
        m_tm, stridebytesize);
    if (!has_const_stride) { return false; }
    if (stridebytesize != ir_size) {
        //The stride size is NOT equal to the data-type byte size
        //of each memory accessing of 'occ'.
        //TODO:Use mask or collect vect-operation.
        dumpStrideNeedMask(this, accdesc, stridebytesize, ir_size);
        return false; //TODO:support vectorization with stride or mask-acc.
    }
    return true;
}


bool Vectorization::canHoistToVecType(IR const* ir, VectCtx const& ctx) const
{
    ASSERT0(ir->is_stmt() || ir->is_exp());
    if (!ir->getType()->is_simplex()) {
        dumpActComplexType(this, ir);
        return false;
    }
    return canBeVectCand(ir);
}


bool Vectorization::makeVectResult(
    IR const* res, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(res->is_stmt());
    ASSERT0(canHoistToVecType(res, ctx) || res->isPROp());
    Type const* expty = makeVectType(res->getType(), ctx);
    ASSERT0(expty && expty->is_vector());
    VECTOP_expected_type(&vectop) = expty;
    VECTOP_occ(&vectop) = res;
    return true;
}


bool Vectorization::makeVectOpnd(
    IR const* exp, MOD VectCtx & ctx, MOD VectOp & vectop)
{
    ASSERT0(exp && exp->is_exp());
    ASSERTN(ctx.getBIV()->getStepValInt() == 1,
            ("TODO:support vectorization with stride."));
    ASSERT0(exp->getType()->is_simplex());
    if (exp->is_const()) {
        return makeVectOpndByConst(exp, ctx, vectop);
    }
    if (exp->isBinaryOp()) {
        return makeVectOpndByBin(exp, ctx, vectop);
    }
    if (exp->isUnaryOp()) {
        return makeVectOpndByUna(exp, ctx, vectop);
    }
    if (exp->isArrayOp()) {
        return makeVectOpndByArray(exp, ctx, vectop);
    }
    if (exp->isIndirectMemOp()) {
        return makeVectOpndByIndirect(exp, ctx, vectop);
    }
    if (exp->isDirectMemOp()) {
        return makeVectOpndByDirect(exp, ctx, vectop);
    }
    if (exp->isReadPR()) {
        return makeVectOpndByReadPR(exp, ctx, vectop);
    }
    if (exp->is_select()) {
        return makeVectOpndBySelect(exp, ctx, vectop);
    }
    UNREACHABLE();
    return false;
}


//The function check whether the expected vector type is legal to given ir.
//Return true if the expected vector type that generated by caller changed.
bool Vectorization::checkAndRecomputeVectType(
    IR const* ir, VectCtx const& ctx, MOD VectOp & vectop)
{
    ASSERTN(VECTOP_expected_type(&vectop),
            ("caller does not given expected vector type"));
    Type const* newty = makeVectType(ir->getType(), ctx);
    ASSERT0(newty && newty->is_vector());
    VECTOP_expected_type(&vectop) = newty;
    return true;
}


bool Vectorization::makeVectOp(MOD VectCtx & ctx)
{
    IVBoundInfo const* bi = ctx.getIVBoundInfo();
    ASSERT0(bi);
    if (bi->isTCImm() && bi->getTCImm() == 0) {
        //TODO: remove the trip-count zero loop.
        return false;
    }
    VectCtx::CandListIter it;
    for (IR const* res = ctx.getResCandList().get_head(&it);
         res != nullptr; res = ctx.getResCandList().get_next(&it)) {
        ASSERT0(res->hasRHS());
        VectOp * vectop = m_vectop_mgr.alloc();
        if (!makeVectResult(res, ctx, *vectop)) { return false; }
        IR const* rhs = res->getRHS();
        VectOp * opnd = m_vectop_mgr.alloc();
        VECTOP_expected_type(opnd) = vectop->getExpectType();
        if (!makeVectOpnd(rhs, ctx, *opnd)) { return false; }
        vectop->addOpnd(opnd);
        ASSERT0(vectop->verify());

        //NOTE: all allocated vectop will be destructed by VectOpMgr at end.
        ctx.getCandVOpList().append(vectop);
    }
    return true;
}


//Return true if exp's MD reference is overlapped with other-stmt that in
//the current loop.
//Note the funcion will skip the stmt of 'exp' located.
bool Vectorization::checkLoopCarrDep(
    VectCtx const& ctx, LoopDepInfoSet const& set) const
{
    if (getLoopDepAna()->containLoopCarrDep(set)) {
        dumpActSinceLoopCarrDep(this, set);
        return false;
    }
    return true; //check passed.
}


bool Vectorization::isLoopRedDepOfBIV(
    BIV const* biv, VectCtx const& ctx, LoopDepInfo const& info) const
{
    ASSERT0(biv);
    if (!info.isLoopRed()) { return false; }
    if (biv->isRefBIV(info.getSrc())) {
        return true;
    }
    if (info.isTgtIR()) {
        return biv->isRefBIV(info.getTgtIR());
    }
    if (info.isTgtMDDef()) {
        MD const* defedmd = info.getTgtMDDef()->getResultMD(
            m_rg->getMDSystem());
        ASSERT0(defedmd);
        return biv->isRefBIV(defedmd);
    }
    UNREACHABLE();
    return false;
}


bool Vectorization::isLoopRedDepOfBIV(
    VectCtx const& ctx, LoopDepInfo const& info) const
{
    if (!info.isLoopRed()) { return false; }
    BIV const* biv = ctx.getBIV();
    ASSERT0(biv);
    return isLoopRedDepOfBIV(biv, ctx, info);
}


bool Vectorization::checkLoopReduceDep(
    VectCtx const& ctx, LoopDepInfoSet const& set) const
{
    xcom::StrBuf tmp(16);
    BIV const* biv = ctx.getBIV();
    ASSERT0(biv);
    LoopDepInfoSetIter it;
    for (LoopDepInfo const* ldi = set.get_first(it);
         !it.end(); ldi = set.get_next(it)) {
        if (!ldi->isLoopRed()) { continue; }
        if (isLoopRedDepOfBIV(ctx, *ldi))  { continue; }
        dumpActLoopRedDep(this, ctx, *ldi);
    }
    return true; //check passed.
}


IR * Vectorization::genConstByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    return genScalarByVectOp(vop, ctx);
}


bool Vectorization::needStoreValueToPR(VectOp const& vop) const
{
    //Target Dependent Code.
    return true;
}


IR * Vectorization::genSelectByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.getNumOfOpnd() == 3);
    ASSERT0(vop.getOpnd(0) && vop.getOpnd(1) && vop.getOpnd(2));
    VectOp const* vop0 = vop.getOpnd(0);
    ASSERT0(vop0);
    ASSERT0(!vop0->isEmpty());
    IR * det = genExpByVectOp(*vop0, ctx);
    ASSERT0(det);

    IR * opnd0 = nullptr;
    VectOp const* vop1 = vop.getOpnd(1);
    ASSERT0(vop0);
    if (!vop1->isEmpty()) {
        opnd0 = genExpByVectOp(*vop1, ctx);
    }
    IR * opnd1 = nullptr;
    VectOp const* vop2 = vop.getOpnd(2);
    ASSERT0(vop2);
    if (!vop2->isEmpty()) {
        opnd1 = genExpByVectOp(*vop2, ctx);
    }
    IR * selop = m_irmgr->buildSelect(
        det, opnd0, opnd1, vop.getExpectType());
    if (!needStoreValueToPR(vop)) { return selop; }
    getActMgr().dumpAct(selop,
        "generate stpr operation to hold the intermediate value:");
    IR * stpr = m_irmgr->buildStorePR(vop.getExpectType(), selop);
    ctx.getMainLoopResOpList().append_tail(stpr);
    return m_rg->dupIsomoExpTree(stpr);
}


IR * Vectorization::genBinByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.getNumOfOpnd() == 2);
    ASSERT0(vop.getOpnd(0) && vop.getOpnd(1));
    IR * opnd0 = genExpByVectOp(*vop.getOpnd(0), ctx);
    IR * opnd1 = genExpByVectOp(*vop.getOpnd(1), ctx);
    IR * binop = m_irmgr->buildBinaryOpSimp(
        vop.getOccCode(), vop.getExpectType(), opnd0, opnd1);
    if (!needStoreValueToPR(vop)) { return binop; }
    getActMgr().dumpAct(binop,
        "generate stpr operation to hold the intermediate value:");
    IR * stpr = m_irmgr->buildStorePR(vop.getExpectType(), binop);
    ctx.getMainLoopResOpList().append_tail(stpr);
    return m_rg->dupIsomoExpTree(stpr);
}


IR * Vectorization::genUnaByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.getNumOfOpnd() == 1);
    ASSERT0(vop.getOpnd(0));
    IR * opnd0 = genExpByVectOp(*vop.getOpnd(0), ctx);
    IR * unaop = m_irmgr->buildUnaryOp(
        vop.getOccCode(), vop.getExpectType(), opnd0);
    if (!needStoreValueToPR(vop)) { return unaop; }
    getActMgr().dumpAct(unaop,
        "generate stpr operation to hold the intermediate value:");
    IR * stpr = m_irmgr->buildStorePR(vop.getExpectType(), unaop);
    ctx.getMainLoopResOpList().append_tail(stpr);
    return m_rg->dupIsomoExpTree(stpr);
}


IR * Vectorization::genArrayByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    return genIndirectByVectOp(vop, ctx);
}

 
IR * Vectorization::genIndirectByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.getNumOfOpnd() == 0);
    IR * opnd = m_rg->dupIRTree(vop.getOcc());
    opnd->setType(vop.getExpectType());
    return opnd;
}


IR * Vectorization::genDirectByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    return genScalarByVectOp(vop, ctx);
}


IR * Vectorization::genScalarByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.getNumOfOpnd() == 0);
    IR const* occ = vop.getOcc();
    ASSERT0(occ);
    ASSERT0(canBeValidOpndInVectOp(occ, ctx, vop));
    IR * opnd = m_rg->dupIRTree(occ);
    getActMgr().dumpAct(opnd,
        "scalar operation has been regarded as vector operand");

    //Scalar operand has to keep its original type unchanged.
    //CASE: Some target exposed ADD vector operation that enable a vector
    //  type operand and a scalar type operand.
    //    ld:<u32x4> x = add ld:<u32x4> y, $z:u32;
    //  is legal target instruction.
    //  In this case, the expected type of $z is <u32x4>. However, if we set
    //  the type of $z to <u32x4>, the IR will be:
    //    ld:<u32x4> x = add ld:<u32x4> y, $z:<u32x4>;
    //  the subsequent code generation pass might generate incorrect
    //  MInst that is inconsistent with the original IR behaviors.
    //opnd->setType(vop.getExpectType());
    return opnd;
}


IR * Vectorization::genPRByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    return genScalarByVectOp(vop, ctx);
}


IR * Vectorization::genExpByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    switch (vop.getOccCode()) {
    case IR_CONST: return genConstByVectOp(vop, ctx);
    case IR_SELECT: return genSelectByVectOp(vop, ctx);
    SWITCH_CASE_BIN: return genBinByVectOp(vop, ctx);
    SWITCH_CASE_UNA: return genUnaByVectOp(vop, ctx);
    SWITCH_CASE_READ_ARRAY: return genArrayByVectOp(vop, ctx);
    SWITCH_CASE_INDIRECT_MEM_EXP: return genIndirectByVectOp(vop, ctx);
    SWITCH_CASE_READ_PR: return genPRByVectOp(vop, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP: return genDirectByVectOp(vop, ctx);
    default: UNREACHABLE();
    }
    //Target Dependent Code.
    return nullptr;
}


void Vectorization::genStmtByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.verify());
    ASSERT0(vop.getOcc() && vop.getExpectType());
    ASSERT0(vop.getNumOfOpnd() == 1);
    IR * rhs = genRHSByVectOp(*vop.getOpnd(0), ctx);
    IR * stmt = m_rg->dupIsomoStmt(vop.getOcc(), rhs);
    ASSERT0(stmt->is_stmt());
    stmt->setType(vop.getExpectType());
    ASSERT0(stmt->is_vec());
    ASSERT0(stmt->hasRHS());
    ctx.getMainLoopResOpList().append_tail(stmt);
}


IR * Vectorization::genRHSByVectOp(VectOp const& vop, OUT VectCtx & ctx)
{
    ASSERT0(vop.getOcc() && vop.getOcc()->is_exp());
    return genExpByVectOp(vop, ctx);
}


void Vectorization::genIRByVectOp(OUT VectCtx & ctx)
{
    for (VectOp * vop = ctx.getCandVOpList().get_head();
         vop != nullptr; vop = ctx.getCandVOpList().get_next()) {
        genStmtByVectOp(*vop, ctx);
    }
}


bool Vectorization::estimateVectOp(VectCtx const& ctx)
{
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    for (VectOp const* vop = pctx.getCandVOpList().get_head();
         vop != nullptr; vop = pctx.getCandVOpList().get_next()) {
        ASSERT0_DUMMYUSE(vop);
    }
    return true;
}


//In C++, local declared class should NOT be used in template parameters of a
//template class. Because the template class may be instanced outside the
//function and the local type in function is invisible.
class VFToSSARegion {
public:
    bool visitIR(IR * ir, OUT bool & is_terminate)
    {
        if (!ir->isPROp()) { return true; }
        if (propstmt.find(ir->getPrno())) {
            ssarg.add(ir);
        } else {
            ASSERT0(ir->isReadPR());
            vect->addUseToRelatedPROp(ir);
        }
        return true;
    }
public:
    xcom::TTab<PRNO> & propstmt;
    SSARegion & ssarg;
    Vectorization const* vect;
public:
    VFToSSARegion(xcom::TTab<PRNO> & g, SSARegion & s, Vectorization const* v)
        : propstmt(g), ssarg(s), vect(v) {}
};


bool Vectorization::constructSSARegion(
    VectCtx const& ctx, IRBB * root, OUT SSARegion & ssarg) const
{
    class IterTree : public VisitIRTree<VFToSSARegion> {
    public:
        IterTree(VFToSSARegion & vf) : VisitIRTree<VFToSSARegion>(vf) {}
    };
    ASSERT0(usePRSSADU() || useMDSSADU());

    //Add PR operations which need to transform to PRSSA.
    VectCtx & pctx = const_cast<VectCtx&>(ctx);

    //Collect PRNO that defined by new generated stmt.
    xcom::TTab<PRNO> propstmt;
    VectCtx::GenedStmtListIter lstit;
    for (IR * s = pctx.getGeneratedStmtList().get_head(&lstit);
         s != nullptr; s = pctx.getGeneratedStmtList().get_next(&lstit)) {
        ASSERT0(s->is_stmt());
        ssarg.add(s->getBB());
        if (s->isPROp()) {
            propstmt.append(s->getPrno());
        }
    }
    VFToSSARegion vf(propstmt, ssarg, this);
    IterTree it(vf);
    for (IR * s = pctx.getGeneratedStmtList().get_head(&lstit);
         s != nullptr; s = pctx.getGeneratedStmtList().get_next(&lstit)) {
        it.visit(s);
    }
    //Set root of SSARegion.
    ssarg.setRootBB(ssarg.findRootBB());
    ssarg.addAllBBUnderRoot();

    //NOTE: SSA region's may contain multiple BB, because if trip-count
    //is NOT immediate, a do-loop will generated, thus there will be a list
    //of newbb inserted into CFG.
    return true;
}


bool Vectorization::addDUChainForNonPROp(
    MOD SSARegion & ssarg, MOD OptCtx * oc) const
{
    if (!useMDSSADU()) { return false; }
    bool succ = m_mdssamgr->constructDesignatedRegion(ssarg);
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, *oc));
    return succ;
}


bool Vectorization::addDUChainForPROp(
    MOD SSARegion & ssarg, MOD OptCtx * oc) const
{
    if (!usePRSSADU()) { return false; }
    oc->setInvalidPRLiveness();
    ASSERT0(m_prssamgr);
    bool succ = m_prssamgr->constructDesignatedRegion(ssarg);
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, *oc));
    return succ;
}


static bool collectMandaPath(MOD VectCtx & ctx)
{
    LI<IRBB> const* li = ctx.getLI();
    return li->findMandatoryPath(ctx.getMandaPath(), ctx.getCFG());
}


bool Vectorization::collectStmtCand(MOD VectCtx & ctx) const
{
    LI<IRBB> const* li = ctx.getLI();
    ASSERT0(li->getLoopHead()->id());
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        ASSERT0(li->getLoopHead()->id() == i ||
                m_cfg->is_dom(li->getLoopHead()->id(), i));
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb);
        BBIRListIter it;
        for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
             ir != nullptr;
             ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
            if (ir->is_phi()) {
                //Phi does not participate in analysis and vectorization.
                continue;
            }
            if (ctx.getCandList().find(ir)) {
                //ir has been appended to candidate-list in previous scanning.
                continue;
            }
            if (!isStmtLegalToVect(ir, ctx)) {
                return false;
            }
            ctx.getCandList().append(ir);
        }
    }
    return true;
}


void Vectorization::pickOutIrrelevantStmtCand(MOD VectCtx & ctx) const
{
    IR * redstmt = pickOutRedStmt(ctx);
    ASSERT0_DUMMYUSE(redstmt);
    IR * ivendboundstmt = pickOutIVEndBoundStmt(ctx);
    ASSERT0_DUMMYUSE(ivendboundstmt);
    IR * backjmp = pickOutBackEdgeJumpStmt(ctx);
    ASSERT0_DUMMYUSE(backjmp);
}


bool Vectorization::checkResultCand(
    VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    VectCtx::CandListIter it;
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    for (IR const* res = pctx.getResCandList().get_head(&it);
         res != nullptr; res = pctx.getResCandList().get_next(&it)) {
        if (!checkStmt(res, ctx)) {
            return false;
        }
        ASSERT0(res->hasRHS());
        //Check whether the RHS expression of result-candidate
        //can be regarded as vector operand in vector operation.
        if (!checkExp(res->getRHS(), ctx, ldactx, set)) {
            return false;
        }
    }
    return true; //check passed.
}


bool Vectorization::checkConst(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isConstExp());
    return findSuitableVectOpnd(ir, ctx, ldactx, set);
}


bool Vectorization::checkUna(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isUnaryOp());
    return findSuitableVectOpnd(UNA_opnd(ir), ctx, ldactx, set);
}


bool Vectorization::checkArrayOp(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isArrayOp());
    return findSuitableVectOpnd(ir, ctx, ldactx, set);
}


bool Vectorization::checkIndirectOp(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isIndirectMemOp());
    return findSuitableVectOpnd(ir, ctx, ldactx, set);
}


bool Vectorization::checkReadPR(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isReadPR());
    return findSuitableVectOpnd(ir, ctx, ldactx, set);
}


bool Vectorization::checkSelect(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->is_select());
    IR const* det = SELECT_det(ir);
    if (!findSuitableVectOpnd(det, ctx, ldactx, set)) {
        getActMgr().dumpAct(det,
            "determinator expression is unsuitable to be vectorized.");
        return false;
    }
    IR const* trueexp = SELECT_trueexp(ir);
    if (trueexp != nullptr &&
        !findSuitableVectOpnd(trueexp, ctx, ldactx, set)) {
        getActMgr().dumpAct(det,
            "determinator expression is unsuitable to be vectorized.");
        return false;
    }
    IR const* falseexp = SELECT_falseexp(ir);
    if (falseexp != nullptr &&
        !findSuitableVectOpnd(falseexp, ctx, ldactx, set)) {
        getActMgr().dumpAct(det,
            "determinator expression is unsuitable to be vectorized.");
        return false;
    }
    return true;
}


bool Vectorization::checkDirectOp(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isDirectMemOp());
    return findSuitableVectOpnd(ir, ctx, ldactx, set);
}


bool Vectorization::checkBin(
    IR const* ir, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(ir->isBinaryOp());
    if (!findSuitableVectOpnd(BIN_opnd0(ir), ctx, ldactx, set) ||
        !findSuitableVectOpnd(BIN_opnd1(ir), ctx, ldactx, set)) {
        return false;
    }
    return true;
}


bool Vectorization::checkStmt(IR const* stmt, VectCtx const& ctx) const
{
    ASSERT0(stmt && stmt->is_stmt());
    return isSuitableToBeVect(stmt, ctx);
}


//The function checks whether the exp can be regarded as vector-operand of
//a vector operation.
bool Vectorization::checkExp(
    IR const* exp, VectCtx const& ctx, LoopDepCtx const& ldactx,
    LoopDepInfoSet const& set) const
{
    ASSERT0(exp && exp->is_exp());
    if (!exp->getType()->is_simplex()) { return false; }
    if (exp->is_const()) {
        return checkConst(exp, ctx, ldactx, set);
    }
    if (exp->isBinaryOp()) {
        return checkBin(exp, ctx, ldactx, set);
    }
    if (exp->isUnaryOp()) {
        return checkUna(exp, ctx, ldactx, set);
    }
    if (exp->isArrayOp()) {
        return checkArrayOp(exp, ctx, ldactx, set);
    }
    if (exp->isIndirectMemOp()) {
        return checkIndirectOp(exp, ctx, ldactx, set);
    }
    if (exp->isDirectMemOp()) {
        return checkDirectOp(exp, ctx, ldactx, set);
    }
    if (exp->isReadPR()) {
        return checkReadPR(exp, ctx, ldactx, set);
    }
    if (exp->is_select()) {
        return checkSelect(exp, ctx, ldactx, set);
    }
    dumpActTargUnsupport(this, exp);
    return false;
}


bool Vectorization::canBeValidOpndInVectOp(
    IR const* ir, VectCtx const& ctx, VectOp const& vectop) const
{
    ASSERT0(ir && ir->is_exp());
    return ir->is_const() || ir->is_vec() || isLoopInv(ir, ctx);
}


bool Vectorization::canBeVectCand(IR const* ir) const
{
    ASSERT0(ir);
    return ir->isIndirectMemOp() || ir->isArrayOp();
}


static void pickOutDepScalarOpRecur(
    IR const* ir, Vectorization const* vect,
    MOD IREList & lst, VectCtx const& ctx);

//In C++, local declared class should NOT be used in template parameters of a
//template class. Because the template class may be instanced outside the
//function and the local type in function is invisible.
class VFToPick {
public:
    bool visitIR(IR const* x, OUT bool & is_terminate)
    {
        if (!x->isMemRef()) { return true; }
        IR * xdef = xoc::findKillingDef(x, rg, ctx.getOptCtx());
        if (xdef == nullptr) { return true; }
        if (xdef->is_phi()) {
            //CASE: the scalar operation might not form a cycle of IV.
            //If it is the situation, vector candidates that depend
            //on the scalar operation would not be vectorized.
            //However, the function only performs coarsen granularity
            //check for scalar operations that obviously prevent
            //vectorization. Therefore, even through 'xdef' is neither
            //an IV nor loop-invariant operation, we still keep the
            //iteration going in order to find more opportunities.
            //if (!ctx.isIV(xdef) &&
            //    !xoc::isPhiLoopInvariant(xdef, ctx.getLI(), rg)) {
            //    return false;
            //}
            return true;
        }
        IREListIter xdeflstit;
        if (lst.find(xdef, &xdeflstit)) {
            ASSERT0(xdef->hasRHS());
            ASSERT0(xdef->getRHS());
            pickOutDepScalarOpRecur(xdef->getRHS(), vect, lst, ctx);
            ASSERTN(lst.find(xdef), ("xdef has been removed already"));
            lst.remove(xdeflstit);
        }
        return true;
    }
public:
    Region const* rg;
    IREList & lst;
    VectCtx const& ctx;
    Vectorization const* vect;
public:
    VFToPick(IREList & plst, VectCtx const& pctx, Vectorization const* pvect)
        : lst(plst), ctx(pctx), vect(pvect) { rg = ctx.getRegion(); }
};


static void pickOutDepScalarOpRecur(
    IR const* ir, Vectorization const* vect,
    MOD IREList & lst, VectCtx const& ctx)
{
    class IterTree : public VisitIRTree<VFToPick> {
    public: IterTree(VFToPick & vf) : VisitIRTree<VFToPick>(vf) {}
    };
    //Pick out scalar operations that related to 'ir' along DU chain.
    //e.g: given ir is S3, thus S1, S3 should be picked out together.
    //loop {
    // S1 t1 = i * n;
    // S2 t2 = j * m;
    // S3 t3 = t1 + k;
    // S4 t4 = t2 + k;
    // S5 a[t3] = ...;
    //}
    VFToPick vf(lst, ctx, vect);
    IterTree it(vf);
    it.visit(ir);
}


void Vectorization::pickOutDepScalarOp(
    IR const* ir, MOD IREList & lst, VectCtx const& ctx) const
{
    ASSERT0(canBeVectCand(ir));
    ASSERT0(ir->is_exp() || ir->is_stmt());
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }

        //kid may be RHS of stmt.
        //CASE:Do NOT skip picking up of RHS of IST|STARRAY or any other stmts
        //that has RHS expression.
        //e.g:compile/vect13_saxpy.c
        //Stmt #S1 should be transformed to vector type, since $3 is just
        //a middle value that is used to store 'x' to memory, namely, 'x' will
        //be vector load operation. As a result, #S1 does NOT prevent
        //vectorization.
        //  stpr $3:f32 = (ild:f32 id:24 (ld:*<4> 'x')) #S1
        //  ist:f32 id:33
        //    ld:*<4> 'y' //ist base
        //    add:f32 id:32 //ist RHS
        //      $3:f32 id:60
        //      ild:f32 id:31 (ld:*<4> 'y')
        for (IR const* x = kid; x != nullptr; x = x->get_next()) {
            pickOutDepScalarOpRecur(x, this, lst, ctx);
        }
    }
}


void Vectorization::pickOutTransferScalarOpOfVectOp(
    MOD IREList & sclst, IRList const& veccandlst, VectCtx const& ctx) const
{
    IRListIter lstit;
    for (IR const* ir = veccandlst.get_head(&lstit);
         ir != nullptr; ir = veccandlst.get_next(&lstit)) {
        pickOutDepScalarOp(ir, sclst, ctx);
    }
}


void Vectorization::collectCandStmtToBeAnalyze(
    OUT IREList & sclst, OUT IRList & veccandlst, VectCtx const& ctx) const
{
    VectCtx::CandListIter it;
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    //Collect the scalar operations need to analyze.
    for (IR * ir = pctx.getCandList().get_head(&it);
         ir != nullptr; ir = pctx.getCandList().get_next(&it)) {
        ASSERT0(ir->is_stmt());
        if (ir->is_phi()) { continue; }
        if (ir->isDirectMemOp() && !ir->is_vec()) {
            sclst.append_tail(ir);
            pctx.getPrerequisiteOpList().append(ir);
            continue;
        }
        if (ir->isPROp() && !ir->is_vec()) {
            sclst.append_tail(ir);
            pctx.getPrerequisiteOpList().append(ir);
            continue;
        }
        if (canBeVectCand(ir)) {
            veccandlst.append_tail(ir);
            continue;
        }
        UNREACHABLE();
    }
}


bool Vectorization::checkScalarStmt(VectCtx const& ctx) const
{
    IREList sclst;
    IRList veccandlst;
    collectCandStmtToBeAnalyze(sclst, veccandlst, ctx);
    if (veccandlst.get_elem_count() == 0) {
        getActMgr().dumpAct(
            "there is no any stmt that can be vectorized"
            " during scalar checking process.");
        return false;
    }
    pickOutTransferScalarOpOfVectOp(sclst, veccandlst, ctx);

    //If there is scalar operation can not be picked out, means the scalar
    //operation prevents vectorization.
    bool no_sc_prevent_vect = true;
    IRListIter lstit;
    for (IR const* ir = sclst.get_head(&lstit);
         ir != nullptr; ir = sclst.get_next(&lstit)) {
        if (ir->is_phi() &&
            (ctx.isIV(ir) || xoc::isPhiLoopInvariant(ir, ctx.getLI(), m_rg))) {
            //Some kind of PHI does not prevent vectorization.
            //e.g:PHI may belong to the cycle consist of IV
            //reduce-operations.
            //or redundant PHI, exec/rp.c
            //  BB2:
            //  phi $40 = $41, $42;
            //  false br BB4;
            //  BB3:
            //  NO USE OF $40;
            //  BB4:
            //  ... = $40;
            //The loop body does not have any USE of PHI result.
            continue;
        }
        getActMgr().dumpAct(ir, "the scalar operation can not be vectorized");
        no_sc_prevent_vect = false;
        break;
    }
    return no_sc_prevent_vect;
}


void Vectorization::pickOutScalarStmt(MOD VectCtx & ctx) const
{
    VectCtx::ResCandListIter it;
    VectCtx::ResCandListIter nextit;
    for (ctx.getResCandList().get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        ctx.getResCandList().get_next(&nextit);
        IR * ir = it->val();
        if (ir->isDirectMemOp() && !ir->is_vec()) {
            ctx.getResCandList().remove(it);
            continue;
        }
        if (ir->isPROp() && !ir->is_vec()) {
            ctx.getResCandList().remove(it);
            continue;
        }
        ASSERT0(canBeVectCand(ir));
    }
}


void Vectorization::analyzeDep(
    VectCtx const& ctx, OUT LoopDepCtx & ldactx, OUT LoopDepInfoSet & set) const
{
    VectCtx::CandListIter it;
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    for (IR * ir = pctx.getCandList().get_head(&it);
         ir != nullptr; ir = pctx.getCandList().get_next(&it)) {
        ASSERT0(ir->is_stmt());
        getLoopDepAna()->analyzeDepForIRTree(
            ir, pctx.getCandList(), set, ldactx);
    }
}


bool Vectorization::vectorize(MOD VectCtx & ctx)
{
    if (!collectMandaPath(ctx)) { goto FAILED; }
    if (!collectStmtCand(ctx)) { goto FAILED; }
    pickOutIrrelevantStmtCand(ctx);
    tryIfConversion(ctx);
    collectResultCand(ctx);
    if (ctx.getResCandList().get_elem_count() == 0) {
        dumpNoResVect(this);

        //There is no need to consider operand candidates when there
        //is no effective and vectorizable output, because illegal result
        //will either lead to loop-carried dependences or loop-reduce
        //dependences to operand candidates, which both prevent vectorization.
        goto FAILED;
    }
    {
        LoopDepInfoSet set;
        LoopDepCtx ldactx(ctx.getLI(), &m_am, ctx.getOptCtx());
        analyzeDep(ctx, ldactx, set);
        if (!checkLoopReduceDep(ctx, set)) { goto FAILED; }
        if (!checkLoopCarrDep(ctx, set)) { goto FAILED; }
        if (!checkScalarStmt(ctx)) { goto FAILED; }
        pickOutScalarStmt(ctx);
        if (!checkResultCand(ctx, ldactx, set)) { goto FAILED; }
    }
    if (!makeVectOp(ctx)) { goto FAILED; }
    if (!estimateVectOp(ctx)) { goto FAILED; }
    genIRByVectOp(ctx);
    ASSERT0(ctx.verify());
    return true;
FAILED:
    ctx.cleanIfVectFailed();
    return false;
}


bool Vectorization::postProcessAfterReconstructLoop(
    VectCtx const& ctx, MOD OptCtx & oc)
{
    //Remove dissociated vertex.
    IRCfgOptCtx coctx(&oc);
    if (useMDSSADU() && oc.is_dom_valid()) {
        //CFG opt will attempt to maintain MDSSA information and update DOM
        //info as much as possible.
        CFGOPTCTX_need_update_dominfo(&coctx) = true;
    } else {
        //Otherwise maintenance is costly and dispensable.
    }
    bool removed = false;
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    if (m_cfg->performMiscOpt(coctx)) {
        //NOTE: DOM info will changed without maintaining.
        //CFG has been changed, thus remove empty BB to produce more
        //optimization opportunities.
        //TODO: DO not recompute whole SSA/MDSSA. Instead, update
        //SSA/MDSSA info especially PHI operands incrementally.
        removed = true;
    }
    if (!useMDSSADU() || !oc.is_dom_valid()) {
        //If DomInfo is invalid, then MDSSA info is also not maintained.
        oc.setInvalidMDSSA();

        //The pass prefers using SSA, thus rebuild SSA if invalid.
        m_rg->getPassMgr()->checkValidAndRecompute(
            &oc, PASS_MDSSA_MGR, PASS_DOM, PASS_LOOP_INFO, PASS_CDG,
            PASS_UNDEF);
    }
    if (!usePRSSADU()) {
        //By default, the pass is enabled only if PRSSA enabled. Therefore,
        //maintain PRSSA if necessary.
        m_rg->getPassMgr()->checkValidAndRecompute(
            &oc, PASS_PRSSA_MGR, PASS_DOM, PASS_UNDEF);
    }
    removed |= m_dce->removeRedundantPhi(oc);

    //Make sure LoopInfo and IVR have been recomputed.
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_LOOP_INFO, PASS_IVR, PASS_UNDEF);
    return removed;
}


static void dupStmtToList(
    OUT IR ** head, MOD IR ** last, VectCtx::ResConstCandList const& lst,
    Region const* rg)
{
    ASSERT0(head && last);
    VectCtx::ResConstCandListIter it;
    for (IR const* op = lst.get_head(&it);
         op != nullptr; op = lst.get_next(&it)) {
        xcom::add_next(head, last, rg->dupIRTree(op));
    }
}


static void addPrerequisiteOpToBBAndRecordGeneratedOp(
    MOD VectCtx & ctx, MOD IRBB * bb)
{
    for (IR const* op = ctx.getPrerequisiteOpList().get_head();
         op != nullptr; op = ctx.getPrerequisiteOpList().get_next()) {
        IR * dupop = ctx.getRegion()->dupIRTree(op);

        //Compute MDRef for new dupop.
        xoc::computeMustAndMayRefForDirectOpForTree(
            dupop, ctx.getRegion(), false);
        bb->getIRList().append_tail_ex(dupop);
        ctx.getGeneratedStmtList().append(dupop);
    }
}


static void addVectOpToBBAndRecordGeneratedOp(MOD VectCtx & ctx, MOD IRBB * bb)
{
    for (IR * vectop = ctx.getMainLoopResOpList().get_head();
         vectop != nullptr; vectop = ctx.getMainLoopResOpList().get_next()) {
        //Compute MDRef for new dupop.
        xoc::computeMustAndMayRefForDirectOpForTree(
            vectop, ctx.getRegion(), false);
        bb->getIRList().append_tail_ex(vectop);
        ctx.getGeneratedStmtList().append(vectop);
    }
}


static IR const* findExposedUseInPhiList(PRNO prno, IRBB const* bb)
{
    IRBB * pbb = const_cast<IRBB*>(bb);
    for (IR const* ir = pbb->getIRList().get_head();
         ir != nullptr; ir = pbb->getIRList().get_next()) {
        if (!ir->is_phi()) {
            //There is no reference of PRNO in phi-list.
            return nullptr;
        }
        for (IR const* opnd = PHI_opnd_list(ir);
             opnd != nullptr; opnd = opnd->get_next()) {
            if (opnd->getPrno() == prno) {
                return opnd;
            }
        }
    }
    //There is no reference of PRNO in phi-list.
    return nullptr;
}


//The function attempts to find the exposed USE to IV variable in loophead.
static IR const* findRefToInitValOfIVInLoopHead(VectCtx const& ctx)
{
    IVBoundInfo const* bd = ctx.getIVBoundInfo();
    ASSERT0(bd);
    BIV const* biv = bd->getBIV();
    ASSERT0(biv);
    IR const* initstmt = biv->getInitStmt();
    if (initstmt == nullptr) {
        //Current loop is NOT a normalized loop.
        return nullptr;
    }
    ASSERT0(ctx.getLI());
    IRBB const* loophead = ctx.getLI()->getLoopHead();
    ASSERT0(loophead);
    if (loophead->getNumOfPred() != 2) {
        //Current loop is NOT a normalized loop. Loophead of a normalized loop
        //has only two CFG predecessors, one is live-in edge from outside loop
        //BB, another is backward-edge from the BB that belongs to loop-body.
        return nullptr;
    }
    if (initstmt->isMemRefNonPR()) { return initstmt; }
    ASSERT0(initstmt->isPROp());

    //IV's init-val should be referenced by PHI in loophead, whereas
    //the other operands of PHI is the result of reduction operation.
    return findExposedUseInPhiList(initstmt->getPrno(), loophead);
}


static void appendStmtToList(
    OUT IR ** head, MOD IR ** last, VectCtx::ResOpList & lst)
{
    ASSERT0(head && last);
    VectCtx::ResOpListIter it;
    for (IR * vectop = lst.get_head(&it);
         vectop != nullptr; vectop = lst.get_next(&it)) {
        ASSERTN(!vectop->is_phi(), ("PHI should not be considered"));
        xcom::add_next(head, last, vectop);
    }
}


IR * Vectorization::buildRefIV(BIV const* biv) const
{
    Var * ivvar = biv->getExpOccMD()->get_base();
    ASSERT0(ivvar);
    return buildRefIV(biv, ivvar->getType());
}


IR * Vectorization::buildRefIV(BIV const* biv, Type const* ty) const
{
    ASSERT0(biv && ty);
    Var * ivvar = biv->getExpOccMD()->get_base();
    if (ivvar->is_pr()) {
        return m_irmgr->buildPRdedicated(ivvar->getPrno(), ty);
    }
    return m_irmgr->buildId(ivvar);
}


//In C++, local declared class should NOT be used in template parameters of a
//template class. Because the template class may be instanced outside the
//function and the local type in function is invisible.
class VFToPreOp {
public:
    bool succ_gen_prerep_ops;
    VectCtx::ResConstCandList * initoplst;
    VectCtx::ResConstCandList * prereqlst;
    LI<IRBB> const* li;
    Region const* rg;
    VectCtx const* ctx;
    IVR const* ivr;
    IRTab handled_phi_tab;
public:
    VFToPreOp() { succ_gen_prerep_ops = true; }
    bool tryGenInitDef(OUT IR ** initdef, IR const* phi) const
    {
        ASSERT0(phi->is_phi());
        //Unsuitable Phi should be  intercepted at checkScalarStmt().
        //Usually the Phi formed a cycle of IV, otherwise we can not
        //generate initial stmt for Phi operands.
        IV const* iv = nullptr;
        if (!ivr->isIV(phi, &iv)) { return false; }
        *initdef = ivr->tryBuildInitStmtOfIV(phi, iv);
        if ((*initdef) == nullptr) { return false; }
        return true;
    }
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        if (ir->is_stmt() || !ir->isMemRef()) { return true; }
        IR * kdef = xoc::findKillingDef(ir, rg, ctx->getOptCtx());
        if (kdef == nullptr) { return true; }
        if (!li->isInsideLoop(kdef->getBB()->id())) { return true; }
        if (!kdef->is_phi()) {
            ASSERT0(prereqlst->find(kdef));
            return true;
        }
        if (handled_phi_tab.find(kdef)) { return true; }

        //Find and genereta stmt to compute initial value of
        //phi's result PR.
        IR * initdef = nullptr;
        bool find = tryGenInitDef(&initdef, kdef);
        if (!find) {
            succ_gen_prerep_ops = false;
            is_terminate = true;
            return false;
        }
        ASSERT0(initdef);
        initoplst->append_tail(initdef);
        handled_phi_tab.append(kdef);
        return true;
    }
};


static IRBB * insertPlaceHolderBeforeLoop(VectCtx const& ctx)
{
    LI<IRBB> const* li = ctx.getLI();
    ASSERT0(li);
    IRBB * placeholder = nullptr;
    xoc::insertPreheader(
        li, ctx.getRegion(), &placeholder, ctx.getOptCtx(), true);
    return placeholder;
}


static void transferIRToBBAndUpdateCtx(
    MOD IRBB * placeholder, MOD VectCtx & ctx, IR * newirlist)
{
    ASSERT0(newirlist);
    //Append new IRs into placeholder.
    BBIRList & irlst = placeholder->getIRList();
    for (IR * x = xcom::removehead(&newirlist);
         x != nullptr; x = xcom::removehead(&newirlist)) {
        if (x->is_label()) {
            //Transfer Label to BBIRList also, because the subsequently
            //BB reconstruction will split the BB into multiple legalism BB.
            ((xcom::EList<IR*, IR2Holder>&)irlst).append_tail(x);

            //Label operation will be simplified into BB attached info.
            //Do NOT use BBIRList's API. And no need to record it.
            continue;
        }
        irlst.append_tail(x);
    }
}


//Pick out label and undef stmt from effect-list that can not be handled
//by DCE.
static void pickOutBBIllegalStmt(MOD ConstIRList & efflist)
{
    ConstIRListIter it;
    ConstIRListIter nextit;
    for (efflist.get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        efflist.get_next(&nextit);
        IR const* ir = it->val();
        if (ir->is_undef() || ir->is_label()) {
            //There are stmts that have been removed for some reason.
            efflist.remove(it);
            continue;
        }
        ASSERT0(ir->is_stmt());
    }
}


static void removeDesignatedStmtsByDCE(
    ConstIRList & efflst, VectCtx const& ctx, MOD DCECtx & dcectx)
{
    DeadCodeElim * dce = ctx.getVect()->getDCE();
    ASSERT0(dce);
    //Here we just call DCE to remove original loop.
    bool remove_branch_stmt = false;
    bool org_is_dce_aggr = dce->isAggressive();
    dce->setAggressive(true);

    //Here we just call DCE to remove original loop.
    bool removed = dce->performByEffectIRList(
        efflst, dcectx, remove_branch_stmt);
    ASSERT0(removed);

    //Recovery original options.
    dce->setAggressive(org_is_dce_aggr);
}


static void removeOrgScalarLoop(MOD VectCtx & ctx)
{
    //Note after this function CFG and SSA may changed if branch-op removed.
    DCECtx dcectx(ctx.getOptCtx(), *ctx.getVect()->getSBSMgr().getSegMgr(),
                  ctx.getActMgr());
    ConstIRList efflst;
    collectStmtOutsideLoop(
        ctx.getLI(), ctx.getCFG()->getBBList(), efflst, dcectx);
    removeDesignatedStmtsByDCE(efflst, ctx, dcectx);

    //Clean context info after CFG rebuilt.
    ctx.cleanAfterLoopReconstruct();
}


static IR * simplifyVectLoop(
    VectCtx const& ctx, IR * loop, OUT bool & need_recst_bblst)
{
    ASSERT0(loop->is_doloop());
    SimpCtx simpctx(ctx.getOptCtx());
    simpctx.setSimpCFS();
    SIMP_cfs_only(&simpctx) = true;
    IRSimp * simppass = (IRSimp*)ctx.getRegion()->getPassMgr()->
        queryPass(PASS_IRSIMP);
    ASSERT0(simppass);
    IR * newloop = simppass->simplifyStmt(loop, &simpctx);
    ASSERT0(newloop);
    need_recst_bblst = simpctx.needReconstructBBList();

    //Assign MD reference for new generated IRs.
    ctx.getRegion()->getMDMgr()->assignMD(newloop, true, true);
    return newloop;
}


//Usually, if a target machine supports mask operation, the epilog operation
//always be a several mask operations which no need to build a loop. Otherwise
//we have to build a loop which we call the loop epilog-loop to compute the
//remaining data.
class ConstructLoop {
public:
    ConstructLoop() {}

    //Construct vectorized main loop according to 'ctx'.
    //Return the new BB that holds the main loop.
    IRBB * constructVectMainLoop(
        MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const;

    //Construct vectorized epilog loop according to 'ctx'.
    //Return the new BB that holds the epilog loop.
    IRBB * constructVectEpilLoop(
        MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const;

    //Construct vectorized mask operations according to 'ctx'.
    //Return the new BB that holds these operations.
    IRBB * constructVectEpilOp(
        MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const;

    //Construct vectorized mask operations or epilog-loop according to 'ctx'.
    //Return the new BB that holds these operations.
    IRBB * constructVectEpilOpOrLoop(
        MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const;

    IR * genVectMainLoop(MOD VectCtx & ctx) const;
    IR * genVectEpilLoop(MOD VectCtx & ctx) const;
    IR * genVectEpilOpList(MOD VectCtx & ctx) const;
    IR * genBIVStepExpWithMaxVectorElemNum(
        BIV const* biv, VectCtx const& ctx) const;
    IR * genBIVStepExpByImm(
        BIV const* biv, VectCtx const& ctx, HOST_UINT elemnum) const;
    IR * genBIVStepExpByExp(
        BIV const* biv, VectCtx const& ctx, IR const* comp_elemnum) const;
    bool genDepOpOutsideLoop(MOD VectCtx & ctx) const;
};


IR * ConstructLoop::genBIVStepExpByExp(
    BIV const* biv, VectCtx const& ctx, IR const* comp_elemnum) const
{
    ASSERT0(comp_elemnum);
    ASSERT0(comp_elemnum->is_stmt() || comp_elemnum->is_exp());
    ASSERT0(biv->getInitValType());
    Type const* ty = biv->getInitValType();
    ASSERT0(biv->isInc() || biv->isDec());
    IR_CODE irc = biv->isInc() ? IR_ADD : IR_SUB;
    ASSERTN(biv->getStepValInt() == 1,
            ("TODO:need to support stride vector op"));
    return ctx.getIRMgr()->buildBinaryOpSimp(
        irc, ty,
        ctx.getVect()->buildRefIV(biv, ty),
        ctx.getIRMgr()->buildBinaryOpSimp(
            IR_MUL, ty,
            ctx.getIRMgr()->buildImmInt(biv->getStepValInt(), ty),
            ctx.getRegion()->dupIsomoExpTree(comp_elemnum)));
}


IR * ConstructLoop::genBIVStepExpByImm(
    BIV const* biv, VectCtx const& ctx, HOST_UINT elemnum) const
{
    ASSERT0(elemnum > 0);
    ASSERT0(biv->getInitValType());
    Type const* ty = biv->getInitValType();
    ASSERT0(biv->isInc() || biv->isDec());
    IR_CODE irc = biv->isInc() ? IR_ADD : IR_SUB;
    ASSERTN(biv->getStepValInt() == 1,
            ("TODO:need to support stride vector op"));
    return ctx.getIRMgr()->buildBinaryOpSimp(
        irc, ty,
        ctx.getVect()->buildRefIV(biv, ty),
        ctx.getIRMgr()->buildImmInt(biv->getStepValInt() * elemnum, ty));
}


IR * ConstructLoop::genBIVStepExpWithMaxVectorElemNum(
    BIV const* biv, VectCtx const& ctx) const
{
    ASSERT0(biv->getInitValType());
    Type const* ty = biv->getInitValType();
    HOST_UINT elemnum = ctx.getVect()->getMaxVectorElemNum(ty, ctx);
    return genBIVStepExpByImm(biv, ctx, elemnum);
}


IR * ConstructLoop::genVectEpilOpList(MOD VectCtx & ctx) const
{
    Region * rg = ctx.getRegion();
    IR * reslst = nullptr;
    IR * last = nullptr;
    if (!genDepOpOutsideLoop(ctx)) { return nullptr; }
    dupStmtToList(&reslst, &last, ctx.getPrerequisiteOpList(), rg);
    appendStmtToList(&reslst, &last, ctx.getEpilLoopMaskOpList());
    appendStmtToList(&reslst, &last, ctx.getEpilLoopResOpList());
    ASSERT0(reslst);
    return reslst;
}


IR * ConstructLoop::genVectEpilLoop(MOD VectCtx & ctx) const
{
    Region * rg = ctx.getRegion();
    IR * reslst = nullptr;
    IR * last = nullptr;
    IVBoundInfo const* boundinfo = ctx.getIVBoundInfo();
    ASSERTN(!boundinfo->isTCImm(), ("no need to generate loop"));
    BIV const* biv = boundinfo->getBIV();
    ASSERT0(biv);
    IR * iv = ctx.getVect()->buildRefIV(biv);

    //Init the tailog-loop IV with the value of main-loop's IV.
    IR * init = rg->dupIRTree(iv);

    //Gen the step of IV.
    IR const* comp_remain = ctx.getEpilLoopCompRemain();
    ASSERT0(comp_remain && comp_remain->is_stmt());
    IR * step = genBIVStepExpByExp(biv, ctx, comp_remain);

    //Gen the DET of Loop.
    IR * det = biv->genBoundExp(*boundinfo, ctx.getIVR(), ctx.getIRMgr(), rg);

    //Gen the epilog-loop.
    IR * doloop = ctx.getIRMgr()->buildDoLoop(iv, init, det, step, nullptr);
    if (!genDepOpOutsideLoop(ctx)) { return nullptr; }
    IR * bodylast = nullptr;
    dupStmtToList(
        &LOOP_body(doloop), &bodylast, ctx.getPrerequisiteOpList(), rg);
    appendStmtToList(
        &LOOP_body(doloop), &bodylast, ctx.getEpilLoopMaskOpList());
    appendStmtToList(&LOOP_body(doloop), &bodylast, ctx.getEpilLoopResOpList());
    ASSERT0(LOOP_body(doloop));
    xcom::add_next(&reslst, &last, doloop);
    return reslst;
}


IR * ConstructLoop::genVectMainLoop(MOD VectCtx & ctx) const
{
    IVBoundInfo const* boundinfo = ctx.getIVBoundInfo();
    ASSERTN(!boundinfo->isTCImm(), ("no need to generate loop"));
    BIV const* biv = boundinfo->getBIV();
    ASSERT0(biv);
    IR * iv = ctx.getVect()->buildRefIV(biv);
    IR * init = biv->genInitExp(ctx.getIRMgr());
    IR * step = genBIVStepExpWithMaxVectorElemNum(biv, ctx);
    IR * det = biv->genBoundExp(
        *boundinfo, ctx.getIVR(), ctx.getIRMgr(), ctx.getRegion());
    IR * doloop = ctx.getIRMgr()->buildDoLoop(iv, init, det, step, nullptr);
    if (!genDepOpOutsideLoop(ctx)) { return nullptr; }
    IR * reslst = nullptr;
    IR * last = nullptr;

    //Init Op has been in 'init'.
    //Region * rg = ctx.getRegion();
    //dupStmtToList(
    //    &reslst, &last, ctx.getInitOpList(), rg);

    IR * bodylast = nullptr;
    dupStmtToList(
        &LOOP_body(doloop), &bodylast, ctx.getPrerequisiteOpList(),
        ctx.getRegion());
    appendStmtToList(&LOOP_body(doloop), &bodylast, ctx.getMainLoopResOpList());
    ASSERT0(LOOP_body(doloop));
    xcom::add_next(&reslst, &last, doloop);
    ctx.getActMgr()->dumpAct(doloop, "generate vectorized main loop:");
    return reslst;
}


bool ConstructLoop::genDepOpOutsideLoop(MOD VectCtx & ctx) const
{
    class IterTree : public VisitIRTree<VFToPreOp> {
    public: IterTree(VFToPreOp & vf) : VisitIRTree<VFToPreOp>(vf) {}
    };
    VFToPreOp vf;
    vf.initoplst = &ctx.getInitOpList();
    vf.prereqlst = &ctx.getPrerequisiteOpList();
    vf.li = ctx.getLI();
    vf.rg = ctx.getRegion();
    vf.ctx = &ctx;
    vf.ivr = ctx.getIVR();
    IterTree it(vf);
    for (IR const* op = ctx.getPrerequisiteOpList().get_head();
         op != nullptr; op = ctx.getPrerequisiteOpList().get_next()) {
        ASSERTN(!op->is_phi(), ("PHI should not be considered"));
        it.visit(op);
        if (!vf.succ_gen_prerep_ops) { return false; }
    }
    return true; //success
}


IRBB * ConstructLoop::constructVectMainLoop(
    MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const
{
    IR * loop = genVectMainLoop(ctx);
    ASSERT0(loop);
    IR * newloop = simplifyVectLoop(ctx, loop, need_recst_bblst);
    ASSERT0(newloop);
    ASSERT0(xoc::verifyIRList(newloop, ctx.getRegion()));

    //Insert a placeholder BB to hold new loop.
    IRBB * placeholder = insertPlaceHolderBeforeLoop(ctx);
    ASSERT0(placeholder);

    //Append stmts of newloop into placeholder.
    transferIRToBBAndUpdateCtx(placeholder, ctx, newloop);
    ctx.addGeneratedStmtFromBB(placeholder);
    return placeholder;
}


IRBB * ConstructLoop::constructVectEpilLoop(
    MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const
{
    IR * loop = genVectEpilLoop(ctx);
    ASSERT0(loop);
    IR * epilstmts = simplifyVectLoop(ctx, loop, need_recst_bblst);
    ASSERT0(epilstmts);
    ASSERT0(xoc::verifyIRList(epilstmts, ctx.getRegion()));

    //Insert a placeholder BB to hold new epilog-loop.
    IRBB * placeholder = insertPlaceHolderBeforeLoop(ctx);
    ASSERT0(placeholder);

    //Append stmts of epilstmts into placeholder.
    transferIRToBBAndUpdateCtx(placeholder, ctx, epilstmts);
    ctx.addGeneratedStmtFromBB(placeholder);
    return placeholder;
}


IRBB * ConstructLoop::constructVectEpilOp(
    MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const
{
    GenerateMask gm;
    if (!gm.tryGenerateMaskVectOp(ctx)) {
        return nullptr;
    }
    IR * epilstmts = genVectEpilOpList(ctx);
    ASSERT0(epilstmts);

    //Insert a placeholder BB to hold new epilog ops.
    IRBB * placeholder = insertPlaceHolderBeforeLoop(ctx);
    ASSERT0(placeholder);

    //Append stmts of epilstmts into placeholder.
    transferIRToBBAndUpdateCtx(placeholder, ctx, epilstmts);
    ctx.addGeneratedStmtFromBB(placeholder);
    return placeholder;
}


IRBB * ConstructLoop::constructVectEpilOpOrLoop(
    MOD VectCtx & ctx, MOD OptCtx & oc, OUT bool & need_recst_bblst) const
{
    IRBB * placeholder = constructVectEpilOp(ctx, oc, need_recst_bblst);
    if (placeholder != nullptr) {
        //Epilog-ops are successful.
        return placeholder;
    }
    placeholder = constructVectEpilLoop(ctx, oc, need_recst_bblst);
    ASSERTN(placeholder, ("epilog-loop is always successful."));
    return placeholder;
}


//
//START ReconstructLoopWithVariantTC
//
class ReconstructLoopWithVariantTC {
public:
    void constructVectLoopAndUpdateCFG(
        MOD VectCtx & ctx, MOD OptCtx & oc);

    //Return true if the reconstruction is successful.
    //changed: set to true if CFG or IR changed.
    bool reconstruct(
        MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc);
};


bool ReconstructLoopWithVariantTC::reconstruct(
    MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc)
{
    ASSERT0(!ctx.isTCImm());
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    ASSERT0_DUMMYUSE(pctx.getMainLoopResOpList().get_elem_count() > 0);
    ASSERT0(ctx.getVect()->usePRSSADU() && ctx.getVect()->useMDSSADU());
    constructVectLoopAndUpdateCFG(ctx, oc);
    changed = true;
    return true; //success
}


//The function generates main vector loop, and insert all these BBs into
//CFG. Note the function will recompute DU chain after construct the
//vector-main-loop.
void ReconstructLoopWithVariantTC::constructVectLoopAndUpdateCFG(
    MOD VectCtx & ctx, MOD OptCtx & oc)
{
    //Do the collection of the relationship between Phi operand and its
    //corresponding predecessors BB ahead of time. Because the subsequent
    //simplification and BB list reconstruction might generate new BBs that
    //do not have any CFG information.
    SortPredByBBId sortpred(ctx.getCFG());
    sortpred.collectPhiOpnd2PredBB();
    bool need_recst_bblst = false;
    ASSERT0(ctx.getGeneratedStmtList().get_elem_count() == 0);
    ASSERT0(ctx.getMainLoopResOpList().get_elem_count() != 0);
    ASSERT0(ctx.getEpilLoopMaskOpList().get_elem_count() == 0);
    ASSERT0(ctx.getEpilLoopResOpList().get_elem_count() == 0);

    //Generate new vectorized loop.
    ConstructLoop cl;
    cl.constructVectMainLoop(ctx, oc, need_recst_bblst);
    IRBB * epilloop_placeholder = cl.constructVectEpilOpOrLoop(
        ctx, oc, need_recst_bblst);
    ASSERT0(need_recst_bblst);

    //Collect original scalar-loop stmts to remove them after reconstruct BB.
    DCECtx dcectx(ctx.getOptCtx(), *ctx.getVect()->getSBSMgr().getSegMgr(),
                  ctx.getActMgr());

    //Collect original loop stmt before BB reconstruction.
    ConstIRList efflst;
    collectStmtOutsideLoop(
        ctx.getLI(), ctx.getCFG()->getBBList(), efflst, dcectx);

    //Some IR placement is not legal to satisfy the constrains to form a BB,
    //thus reconstruct BB list immediately.
    bool change = ctx.getRegion()->reconstructBBList(oc);
    ASSERT0(change);

    //Currently, CFG has been invalided.
    //No need to update MDSSA.
    oc.setInvalidMDSSA();
    oc.setInvalidPRSSA();

    //CFG need to be rebuilt if there are BBs that have been splitted.
    ctx.getCFG()->rebuild(oc, &sortpred);
    ctx.getRegion()->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_MDSSA_MGR, PASS_PRSSA_MGR, PASS_UNDEF);
    ASSERT0(ctx.getVect()->usePRSSADU() && ctx.getVect()->useMDSSADU());

    if (epilloop_placeholder != nullptr) {
        pickOutBBIllegalStmt(efflst);
        removeDesignatedStmtsByDCE(efflst, ctx, dcectx);
    }

    //New loop has been generated, LoopInfo and IVR need to be recomputed.
    oc.setInvalidPass(PASS_LOOP_INFO);
    oc.setInvalidPass(PASS_IVR);

    //Clean context info after CFG rebuilt.
    ctx.cleanAfterLoopReconstruct();
}
//END ReconstructLoopWithVariantTC


//
//START ReconstructLoopWithImmTC
//
class ReconstructLoopWithImmTC {
public:
    bool addVectOpAndDepOpToBB(MOD VectCtx & ctx, MOD IRBB * bb) const;
    bool addDUChainForVectOp(VectCtx const& ctx, MOD IRBB * root) const;
    bool addReduceIVOpToBBAndRecordGeneratedOp(
        MOD VectCtx & ctx, MOD IRBB * bb) const;

    IRBB * constructVectMainBB(MOD VectCtx & ctx) const;

    //Return true if the reconstruction is successful.
    //changed: set to true if CFG or IR changed.
    bool reconstruct(
        MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc);
};


bool ReconstructLoopWithImmTC::addReduceIVOpToBBAndRecordGeneratedOp(
    MOD VectCtx & ctx, MOD IRBB * bb) const
{
    IVBoundInfo const* bd = ctx.getIVBoundInfo();
    ASSERT0(bd);
    BIV const* biv = bd->getBIV();
    ASSERT0(biv);
    IR const* exp = biv->getInitExp();
    ASSERT0(exp);
    Type const* ty = exp->getType();
    HOST_UINT step = ctx.getVect()->getMaxVectorElemNum(ty, ctx);

    //Find the EXP that use the result of reduce-operation's result.
    IR const* exp_use_redres = findRefToInitValOfIVInLoopHead(ctx);
    ASSERT0(exp_use_redres);
    IR * redstmt = ctx.getIVR()->tryBuildReduceStmtOfIV(
        exp_use_redres, biv, step);
    if (redstmt == nullptr) {
        ctx.getActMgr()->dumpAct(exp_use_redres,
            "the reduce-operation in placeholder BB is too "
            "complicated to build, the situation prevents the "
            "legal vector-operation generation");
        return false;
    }
    //Compute MDRef for new dupop.
    xoc::computeMustAndMayRefForDirectOpForTree(
        redstmt, ctx.getRegion(), false);
    bb->getIRList().append_tail_ex(redstmt);
    ctx.getGeneratedStmtList().append(redstmt);
    return true;
}


bool ReconstructLoopWithImmTC::addDUChainForVectOp(
    VectCtx const& ctx, MOD IRBB * root) const
{
    OptCtx * oc = ctx.getOptCtx();
    ASSERT0(ctx.getVect()->usePRSSADU() && ctx.getVect()->useMDSSADU());
    if (!oc->is_dom_valid()) {
        //SSA's DfMgr need domset info.
        ctx.getRegion()->getPassMgr()->checkValidAndRecompute(
            oc, PASS_DOM, PASS_UNDEF);
    }
    //Even through all vectorized operations are placed in identical BB,
    //the followed functions aslo may iterate DomTree for some reason.
    //Therefore we recompute DomTree here. And in most testcases, it is
    //not costly.
    //Note if trip-count is immediate, there is only one BB generated. If
    //trip-count is variable, a do-loop generated which will be simplified
    //to multiple BBs after.
    xcom::DomTree domtree;
    ctx.getCFG()->genDomTree(domtree);
    xcom::DefMiscBitSetMgr sbsmgr;
    SSARegion ssarg(&sbsmgr, domtree, ctx.getRegion(), oc, ctx.getActMgr());
    Vectorization const* vect = ctx.getVect();
    if (!vect->constructSSARegion(ctx, root, ssarg)) { return false; }
    if (!vect->addDUChainForPROp(ssarg, oc)) { return false; }
    if (!vect->addDUChainForNonPROp(ssarg, oc)) { return false; }
    return true;
}


IRBB * ReconstructLoopWithImmTC::constructVectMainBB(MOD VectCtx & ctx) const
{
    //Insert a placeholder BB to hold new loop.
    IRBB * placeholder = insertPlaceHolderBeforeLoop(ctx);
    if (!addVectOpAndDepOpToBB(ctx, placeholder)) { return nullptr; }

    //Add DU chain to satisfy the verification in subsequent DCE and
    //CFG optimizations.
    //addDUChainForVectOp(ctx, m_cfg->getEntry(), &oc); //hack remove!
    addDUChainForVectOp(ctx, placeholder); //hack open!!
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(ctx.getRegion(), *ctx.getOptCtx()));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(ctx.getRegion(), *ctx.getOptCtx()));
    return placeholder;
}


bool ReconstructLoopWithImmTC::addVectOpAndDepOpToBB(
    MOD VectCtx & ctx, MOD IRBB * bb) const
{
    ASSERT0(bb);
    ConstructLoop cl;
    if (!cl.genDepOpOutsideLoop(ctx)) { return false; }
    addPrerequisiteOpToBBAndRecordGeneratedOp(ctx, bb);
    addVectOpToBBAndRecordGeneratedOp(ctx, bb);
    return addReduceIVOpToBBAndRecordGeneratedOp(ctx, bb);
}


bool ReconstructLoopWithImmTC::reconstruct(
    MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc)
{
    ASSERT0(ctx.isTCImm());
    VectCtx & pctx = const_cast<VectCtx&>(ctx);
    ASSERT0_DUMMYUSE(pctx.getMainLoopResOpList().get_elem_count() > 0);
    ASSERT0(ctx.getVect()->usePRSSADU() && ctx.getVect()->useMDSSADU());
    constructVectMainBB(ctx);
    removeOrgScalarLoop(ctx);
    changed = true;
    return true; //always be successful.
}
//END ReconstructLoopWithImmTC


bool Vectorization::reconstructLoop(
    MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc)
{
    if (ctx.isTCImm()) {
        ReconstructLoopWithImmTC recon;
        recon.reconstruct(ctx, changed, oc);
    } else if (is_aggressive()) {
        ReconstructLoopWithVariantTC recon;
        recon.reconstruct(ctx, changed, oc);
    } else { return false; }

    //Remove the empty loop structures after DCE finished.
    //NOTE: SSA info may unavaliable.
    bool removed = postProcessAfterReconstructLoop(ctx, oc);
    ASSERT0(m_dumgr->verifyMDRef());
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    changed |= removed;
    return true; //always be successful
}


bool Vectorization::tryVectorizeLoop(LI<IRBB> * li, MOD OptCtx & oc)
{
    if (!li->isInnerMost()) { return false; }
    IVBoundInfo bi;
    IVRCtx ivrctx(getRegion(), &oc, (ActMgr*)&getActMgr());
    if (!m_ivr->computeIVBound(li, bi, ivrctx)) {
        dumpIVNoBound(this, li);
        return false;
    }
    dumpIVBound(bi, this, li);
    if (bi.isTCImm() && bi.getTCImm() == 0) {
        //TODO: Remove the loop which trip-count is zero.
        ASSERTN(0, ("NEED TO BE IMPLEMENTED"));
    }
    VectCtx vectctx(li, &bi, oc, this, (ActMgr*)&getActMgr(), &ivrctx);
    LICMAnaCtx anactx(m_rg, li, m_licm, &oc);
    if (is_aggressive()) {
        //Use Loop Invariant info to determine whether a variable can be
        //vectorized.
        m_licm->analyszInvariantOp(anactx);
        vectctx.setLICMAnaCtx(&anactx);
        dumpVectUseLICM(this);
    }
    if (!vectorize(vectctx)) {
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpVectorization()) {
        vectctx.dump();
    }
    bool changed = false;
    bool succ = reconstructLoop(vectctx, changed, oc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpVectorization()) {
        //After loop reconstruction, LoopInfo, IV, and misc vector-candidate-
        //op information collected have become invalided. Here just dump
        //informations that generated by reconstruction.
        vectctx.dump();
    }
    ASSERT0(xoc::verifyIRandBB(m_rg));
    return succ ? changed : false;
}


bool Vectorization::doLoopTree(LI<IRBB> * li, OptCtx & oc)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != nullptr; tli = tli->get_next()) {
        bool lchanged_inner = doLoopTree(tli->getInnerList(), oc);
        changed |= lchanged_inner;
        if (lchanged_inner) {
            //Inner Loop may have been destroyed, reperform doLoopTree().
            return changed;
        }
        bool lchanged_cur_loop = tryVectorizeLoop(tli, oc);
        changed |= lchanged_cur_loop;
        if (lchanged_cur_loop) {
            //CASE:compile.gr/gcse2.gr, LI3 has been vectorized, then
            //loop-tree should be reconstructed.
            //tli may have been destroyed, reperform doLoopTree().
            return changed;
        }
    }
    return changed;
}


void Vectorization::reset()
{
    m_am.clean();
    cleanAfterPass();
}


bool Vectorization::dump() const
{
    m_am.dump();
    return true;
}


bool Vectorization::initDepPass(MOD OptCtx & oc)
{
    if (!oc.is_ref_valid()) { return false; }

    //Initialize pass object since they might be destructed at any moment.
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_dumgr = m_rg->getDUMgr();
    m_irmgr = m_rg->getIRMgr();
    m_oc = &oc;
    m_licm = nullptr;
    m_dce = nullptr;
    m_ivr = nullptr;
    m_gvn = nullptr;
    if (!usePRSSADU() || !useMDSSADU()) {
        //Vectorization prefer using SSA instead of classic DU.
        return false;
    }
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_DOM, PASS_LOOP_INFO, PASS_IVR, PASS_UNDEF);
    m_ivr = (IVR*)m_rg->getPassMgr()->queryPass(PASS_IVR);
    ASSERT0(m_ivr && m_ivr->is_valid());
    m_dce = (DeadCodeElim*)m_rg->getPassMgr()->registerPass(PASS_DCE);
    if (m_dce == nullptr) {
        //Vectorization use DCE to reconstruct loop.
        return false;
    }
    m_loopdepana = (LoopDepAna*)m_rg->getPassMgr()->registerPass(
        PASS_LOOP_DEP_ANA);
    if (m_loopdepana == nullptr) {
        return false;
    }
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_LOOP_DEP_ANA, PASS_UNDEF);
    if (is_aggressive()) {
        //Note the pass still not force LICM to work yet.
        //The pass just utilize the analysis ability of LICM.
        m_licm = (LICM*)m_rg->getPassMgr()->registerPass(PASS_LICM);
        ASSERT0(m_licm);
        m_gvn = (GVN*)m_rg->getPassMgr()->registerPass(PASS_GVN);
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_GVN, PASS_UNDEF);
        ASSERT0(useGVN());
        m_ivr->setAggressive(true);
    }
    return true;
}


bool Vectorization::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    START_TIMER(t, getPassName());
    if (!initDepPass(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    reset();
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    bool changed = false;
    bool lchanged = false;
    do {
        lchanged = doLoopTree(m_cfg->getLoopInfo(), oc);
        changed |= lchanged;
    } while (lchanged);
    if (!changed) {
        cleanAfterPass();
        m_rg->getLogMgr()->cleanBuffer();
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpVectorization()) {
        dump();
    }
    cleanAfterPass();

    //The pass does not devastate IVR information. However, new IV might be
    //inserted.
    //DU chain and DU reference should be maintained.
    ASSERT0(m_dumgr->verifyMDRef() && verifyClassicDUChain(m_rg, oc));
    oc.setInvalidIfDUMgrLiveChanged();
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
