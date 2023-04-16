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

//
//START BIV
//
IR * BIV::genInitExp(IRMgr * irmgr) const
{
    ASSERT0(getInitValType());
    if (isInitConstInt()) {
        ASSERT0(getInitValInt());
        return irmgr->buildImmInt(*getInitValInt(), getInitValType());
    }
    if (isInitConstFP()) {
        ASSERT0(getInitValFP());
        return irmgr->buildImmFP(*getInitValFP(), getInitValType());
    }
    if (isInitVar()) {
        ASSERT0(getInitValMD());
        return irmgr->buildLoad(getInitValMD()->get_base(), getInitValType());
    }
    ASSERT0(0); //TODO:need to support.
    return nullptr;
}


IR * BIV::genStepExp(IRMgr * irmgr) const
{
    ASSERT0(getInitValType());
    Type const* ty = getInitValType();
    IR_CODE irc = is_inc() ? IR_ADD : IR_SUB;
    return irmgr->buildBinaryOp(irc, ty,
        irmgr->buildLoad(getOccMD()->get_base(), ty),
        irmgr->buildImmInt(getStep(), ty));
}


IR * BIV::genBoundExp(IVBoundInfo const& boundinfo, IVR const* ivr,
                      IRMgr * irmgr, Region * rg) const
{
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    ivr->extractIVBoundExpFromStmt(this, boundinfo.getBound(), &ivref, &bexp,
                                   &is_closed_range);
    ASSERT0(ivref && bexp);
    if (is_inc()) {
        IR_CODE irc;
        if (is_closed_range) {
            irc = IR_LE;
        } else {
            irc = IR_LT;
        }
        return irmgr->buildCmp(irc, rg->dupIRTree(ivref), rg->dupIRTree(bexp));
    }
    IR_CODE irc;
    if (is_closed_range) {
        irc = IR_GE;
    } else {
        irc = IR_GT;
    }
    return irmgr->buildCmp(irc, rg->dupIRTree(ivref), rg->dupIRTree(bexp));
}


void BIV::dump(Region const* rg) const
{
    BIV * iv = const_cast<BIV*>(this);
    note(rg, "\nBIV(MD%d", iv->getOccMD()->id());

    //Dump initval.
    if (iv->hasInitVal()) {
        if (iv->isInitConstInt()) {
            prt(rg, ",init=%lld",
                (BIVIntType)*iv->getInitValInt());
        } else if (iv->isInitConstFP()) {
            prt(rg, ",init=%f",
                (BIVFpType)*iv->getInitValFP());
        } else {
            prt(rg, ",init=MD%u",
                (MDIdx)iv->getInitValMD()->id());
        }
    }

    //Dump monotone direction.
    if (iv->is_inc()) {
        prt(rg, ",step=%lld", (BIVIntType)iv->getStep());
    } else {
        prt(rg, ",step=-%lld", (BIVIntType)iv->getStep());
    }

    prt(rg, ")");
    rg->getLogMgr()->incIndent(2);

    //Dump BIV's def-stmt.
    note(rg, "\nDEF-STMT:");
    ASSERT0(iv->getRedStmt());
    rg->getLogMgr()->incIndent(2);
    dumpIR(iv->getRedStmt(), rg, nullptr, IR_DUMP_KID);
    rg->getLogMgr()->decIndent(2);

    //Dump BIV's occ-exp.
    note(rg, "\nOCC-EXP:");
    ASSERT0(iv->getRedStmt());
    rg->getLogMgr()->incIndent(2);
    dumpIR(iv->getRedStmt(), rg, nullptr, IR_DUMP_KID);
    rg->getLogMgr()->decIndent(2);

    //Dump BIV's init-stmt.
    if (iv->getInitStmt() != nullptr) {
        note(rg, "\nINIT-STMT:");
        rg->getLogMgr()->incIndent(2);
        dumpIR(iv->getInitStmt(), rg, nullptr, IR_DUMP_KID);
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);
}
//END BIV


//
//START DIV
//
void DIV::dump(Region const* rg) const
{
    DIV * iv = const_cast<DIV*>(this);
    ASSERT0(iv && iv->getRedStmt());
    note(rg, "\nDIV");

    //Dump div occurrence.
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nOCC:");

    rg->getLogMgr()->incIndent(2);
    dumpIR(iv->getRedStmt(), rg, nullptr, IR_DUMP_KID);
    rg->getLogMgr()->decIndent(2);

    rg->getLogMgr()->decIndent(2);

    //Dump linear-representation of div.
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nLINREP:");
    iv->getLinRep()->dump(rg);
    rg->getLogMgr()->decIndent(2);
}
//END DIV


//
//START IVBoundInfo
//
void IVBoundInfo::dump(Region const* rg) const
{
    ASSERT0(rg);
    note(rg, "\n==-- DUMP IVBoundInfo --==");
    BIV const* iv = IVBI_iv(*this);
    iv->dump(rg);
    xcom::StrBuf lbuf(32);
    xcom::StrBuf buf(32);
    if (IVBI_is_tc_imm(*this)) {
        xoc::dumpHostInt(IVBI_tc_imm(*this), false, false, lbuf);
        buf.strcat("\nTRIPCOUNT IS IMM:%s", lbuf.buf);

        lbuf.clean();
        xoc::dumpHostInt(IVBI_tc_init_val_imm(*this), false, false, lbuf);
        buf.strcat("\nINIT_VAL IS IMM:%s", lbuf.buf);

        lbuf.clean();
        xoc::dumpHostInt(IVBI_tc_end_val_imm(*this), false, false, lbuf);
        buf.strcat("\nEND_VAL IS IMM:%s", lbuf.buf);
    } else {
        IR const* exp = IVBI_tc_exp(*this);
        ASSERT0(exp && exp->is_exp());
        buf.strcat("\nTRIPCOUNT IS EXP:", IRNAME(exp), exp->id());
        {
            DumpBufferSwitch buff(rg->getLogMgr());
            xoc::dumpIR(exp, rg);
            ASSERT0(rg->getLogMgr()->getBuffer());
            buf.strcat(rg->getLogMgr()->getBuffer()->getBuf());
        }
    }
    lbuf.clean();
    xoc::dumpHostInt((HOST_INT)iv->getStep(), false, false, lbuf);
    buf.strcat("\nSTEP IS IMM:%s", lbuf.buf);
    note(rg, buf.buf);
}
//END IVBoundInfo


//Return true if IV is increment, otherwise return false.
bool IV::isInc() const
{
    if (is_biv()) { return ((BIV*)this)->is_inc(); }
    ASSERT0(0); //TODO
    return false;
}


IR const* IV::getRhsOccOfRedStmt() const
{
    ASSERT0(getRedStmt() && getRedStmt()->getRefMD());
    IR const* ref = getRedStmt()->getRHS()->getOpndMem(
        getRedStmt()->getRefMD());
    ASSERT0(ref);
    //There should be only single occurrences in RHS of reduction.
    return ref;
}


bool LinearRep::isCoeffEqualTo(HOST_INT v) const
{
    if (coeff == nullptr) { return false; }
    ASSERT0(coeff->is_const());
    if (!coeff->is_int()) { return false; }
    return CONST_int_val(coeff) == v;
}


void LinearRep::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    UINT const ind = 4;

    //Print coefficent.
    if (coeff != nullptr) {
        note(rg, "\n COEFF:");
        rg->getLogMgr()->incIndent(ind);
        dumpIR(coeff, rg, nullptr, IR_DUMP_KID);
        rg->getLogMgr()->decIndent(ind);
    }
    ASSERT0(var);

    //Print variable.
    if (coeff != nullptr) {
        note(rg, "\n* VAR");
    } else {
        note(rg, "\n  VAR:");
    }
    rg->getLogMgr()->incIndent(ind);
    dumpIR(var, rg, nullptr, IR_DUMP_KID);
    rg->getLogMgr()->decIndent(ind);

    //Print addend.
    if (addend != nullptr) {
        if (addend_sign == IR_ADD) {
            note(rg, "\n+");
        } else {
            ASSERT0(addend_sign == IR_SUB);
            note(rg, "\n-");
        }
        prt(rg, " ADDEND:");
        rg->getLogMgr()->incIndent(ind);
        dumpIR(addend, rg, nullptr, IR_DUMP_KID);
        rg->getLogMgr()->decIndent(ind);
    }
}


//
//START IVR
//
//iv: IV info that will be modified
bool IVR::computeInitVal(IR const* ir, OUT BIV * iv)
{
    if (!ir->is_st() && !ir->is_ist() && !ir->is_stpr() && !ir->is_starray()) {
        return false;
    }

    BIV_init_stmt(iv) = ir;
    IR const* v = ST_rhs(ir); //v is the initial value.
    if (v->is_cvt()) {
        v = ((CCvt*)v)->getLeafExp();
    }

    BIV_initv_data_type(iv) = v->getType();
    if (v->is_const()) {
        if (v->is_int()) {
            if (!iv->hasInitVal()) {
                BIV_initv_int(iv) = (BIVIntType*)xmalloc(sizeof(BIVIntType));
            }
            *BIV_initv_int(iv) = CONST_int_val(v);
            BIV_initv_kind(iv) = IV_INIT_VAL_IS_INT;
            return true;
        }
        if (v->is_fp()) {
            if (!iv->hasInitVal()) {
                BIV_initv_fp(iv) = (BIVFpType*)xmalloc(sizeof(BIVFpType));
            }
            *BIV_initv_fp(iv) = CONST_fp_val(v);
            BIV_initv_kind(iv) = IV_INIT_VAL_IS_FP;
            return true;
        }
        UNREACHABLE(); //TODO: support more kind of init-value.
        return false;
    }

    MD const* md = v->getExactRef();
    if (md != nullptr) {
        BIV_initv_md(iv) = md;
        BIV_initv_kind(iv) = IV_INIT_VAL_IS_VAR;
        return true;
    }

    BIV_initv_int(iv) = nullptr;
    return false;
}


//Find initialze value of IV, if found the value return true, otherwise
//return false.
//iv: IV info that will be modified
bool IVR::findInitVal(IRSet const& defset, OUT BIV * iv)
{
    IR const* rhs_occ = iv->getRhsOccOfRedStmt();
    ASSERT0(iv->getRedStmt()->is_stmt());
    IR const* domdef = findNearestDomDef(rhs_occ, defset, m_rg, true);
    if (domdef == nullptr) { return false; }

    MD const* emd = nullptr;
    if (m_is_only_handle_exact_md) {
        emd = domdef->getExactRef();
    } else {
        emd = domdef->getMustRef();
    }

    if (emd == nullptr || emd != iv->getOccMD()) {
        return false;
    }

    LI<IRBB> const* li = iv->getLI();
    ASSERT0(li);
    IRBB * dbb = domdef->getBB();
    if (dbb == li->getLoopHead() || !li->isInsideLoop(dbb->id())) {
        return computeInitVal(domdef, iv);
    }
    return false;
}


IR * IVR::findMatchedOcc(MD const* biv, IR * start)
{
    ASSERT0(start->is_exp());
    //Note there may be multiple occurrences matched biv.
    //We just return the first if meet.
    return start->getOpndMem(biv);
}


//Extract BIV info from linear-representation.
//Return true if the function extracted correct BIV information, and
//record the BIV into 'biv', otherwise return false.
bool IVR::extractBIV(IR const* def, LinearRep const& lr, IRSet const& defset,
                     LI<IRBB> const* li, OUT BIV ** biv)
{
    ASSERT0(def->is_stmt() && lr.is_valid());
    if (lr.addend_sign != IR_ADD && lr.addend_sign != IR_SUB) {
        return false;
    }

    MD const* bivref = def->getRefMD();
    if (m_is_strictly_match_pattern) {
        //Make sure self modify stmt is monotonic.
        //Strictly match the pattern: i = i + CONST.
        if (lr.coeff == nullptr ||
            (lr.coeff->is_const() && lr.coeff->is_int() &&
             CONST_int_val(lr.coeff) == 1)) {
            ; //coeff is 1.
        } else {
            return false;
        }
        if (lr.var->getRefMD() != bivref) {
            return false;
        }
    }

    IR const* addend = lr.addend;
    if (addend->is_int()) {
        if (!addend->is_const()) {
            //TODO: support FP and MD step type.
            return false;
        }
    } else if (g_is_support_dynamic_type && addend->is_const()) {
        //TODO: support dynamic const type as the addend of ADD/SUB.
        return false;
    } else {
        return false;
    }

    if (m_is_only_handle_exact_md && bivref != lr.var->getExactRef()) {
        return false;
    }

    //Here only record integer step-type.
    //TODO: support FP and MD step type.
    BIVIntType step = lr.addend_sign == IR_ADD ? CONST_int_val(addend) :
                                                 -CONST_int_val(addend);
    ASSERTN(step != 0, ("should be optimized before"));

    ASSERT0(biv);
    *biv = allocBIV();
    IV_li(*biv) = li;
    IV_reduction_stmt(*biv) = const_cast<IR*>(def);
    BIV_step(*biv) = step;
    BIV_is_inc(*biv) = step > 0 ? true : false;
    return findInitVal(defset, *biv);
}


void IVR::recordDIV(LI<IRBB> const* li, IR * red, LinearRep * linrep)
{
    ASSERT0(li && red && red->is_stmt() && linrep);
    DIV * x = allocDIV();
    IV_li(x) = li;
    IV_reduction_stmt(x) = red;
    DIV_linrep(x) = linrep;

    //Same occurrence may correspond to multiple LoopInfo.
    //e.g: c4.c
    //  while () {
    //    while () {
    //      ...
    //      i++;
    //    }
    //  }
    DIVList * ivlst = m_li2divlst.get(li->id());
    if (ivlst == nullptr) {
        ivlst = (DIVList*)xmalloc(sizeof(DIVList));
        ivlst->init(m_sc_pool);
        m_li2divlst.set(li->id(), ivlst);
    }
    ivlst->append_head(x);
}


//biv: MD reference of BIV
//li: LoopInfo related to 'biv'.
//red: the reduction stmt of IV.
//defset: the DEF set of RHS occurrence in 'red'.
//step: record the step constant of BIV.
//is_increment: record the monotonicity of IV.
void IVR::recordBIV(BIV * biv)
{
    //Same occurrence may correspond to multiple LoopInfo.
    //e.g: c4.c
    //  while () {
    //    while () {
    //      ...
    //      i++;
    //    }
    //  }
    ASSERT0(biv->getLI());
    BIVList * ivlst = m_li2bivlst.get(biv->getLI()->id());
    if (ivlst == nullptr) {
        ivlst = (BIVList*)xmalloc(sizeof(BIVList));
        ivlst->init(m_sc_pool);
        m_li2bivlst.set(biv->getLI()->id(), ivlst);
    }
    ivlst->append_head(biv);
}


//Return true if ir indicates BIV reference in given loop 'li'.
bool IVR::isBIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const
{
    MD const* ref = ir->getRefMD();
    if (ref == nullptr || !ref->is_exact()) { return false; }

    BIVList const* bivlst = m_li2bivlst.get(li->id());
    if (bivlst == nullptr) { return false; }

    for (BIVListIter it = bivlst->get_head();
         it != bivlst->end(); it = bivlst->get_next(it)) {
        BIV const* tiv = it->val();
        ASSERT0(tiv);
        if (ref == tiv->getOccMD()) {
            if (iv != nullptr) {
                *iv = tiv;
            }
            return true;
        }
    }
    return false;
}


//Return true if ir indicates DIV reference in given loop 'li'.
bool IVR::isDIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const
{
    MD const* ref = ir->getRefMD();
    if (ref == nullptr || !ref->is_exact()) { return false; }

    DIVList * divlst = m_li2divlst.get(li->id());
    if (divlst == nullptr) { return false; }

    for (DIVListIter it = divlst->get_head();
         it != divlst->end(); it = divlst->get_next(it)) {
        DIV const* tiv = it->val();
        ASSERT0(tiv);
        if (ref == tiv->getOccMD()) {
            if (iv != nullptr) {
                *iv = tiv;
            }
            return true;
        }
    }
    return false;
}


//Return true if ir indicates IV reference in given loop 'li'.
bool IVR::isIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const
{
    if (isBIV(li, ir, iv)) { return true; }
    return isDIV(li, ir, iv);
}


static bool isIVrecur(IVR const* ivr, LI<IRBB> const* li, IR const* ir,
                      IV const** iv)
{
    for (LI<IRBB> const* tli = li; tli != nullptr; tli = tli->get_next()) {
        if (isIVrecur(ivr, tli->getInnerList(), ir, iv)) { return true; }
        if (ivr->isIV(tli, ir, iv)) { return true; }
    }
    return false;
}


//Return true if ir indicates IV reference.
//iv: record related IV information if ir is IV.
bool IVR::isIV(IR const* ir, OUT IV const** iv) const
{
    MD const* ref = ir->getRefMD();
    if (ref != nullptr && ref->is_exact()) {
        return isIVrecur(this, m_cfg->getLoopInfo(), ir, iv);
    }
    return false;
}


//Return true if there are multiple-definition of MD reference represeted by
//'ir' inside given loop 'li'.
//set: record the DEF stmt set of reduction variable.
bool IVR::hasMultiDefInLoop(IR const* ir, LI<IRBB> const* li,
                            OUT IRSet * set) const
{
    IR const* exp = nullptr;
    IR const* stmt_of_exp = nullptr;
    if (ir->is_stmt()) {
        stmt_of_exp = ir;
        //Find an USE of 'ir' that is inside in loop 'li'.
        collectUseSet(ir, m_mdssamgr, set);
        IRSetIter it = nullptr;
        for (BSIdx i = set->get_first(&it);
             i != BS_UNDEF; i = set->get_next(i, &it)) {
            IR * e = m_rg->getIR(i);
            ASSERT0(e || e->is_exp());
            IRBB const* bb = nullptr;
            if (e->is_id()) {
                ASSERTN(ID_phi(e), ("For now, ID is operand of PHI"));
                bb = ID_phi(e)->getBB();
            } else {
                bb = e->getStmt()->getBB();
            }
            ASSERT0(bb);
            if (li->isInsideLoop(bb->id())) {
                exp = e;
                break;
            }
        }
    } else {
        exp = ir;
        stmt_of_exp = exp->getStmt();
    }
    set->clean();
    if (exp == nullptr) {
        //ir is stmt, it does not have any USE in loop.
        //The MD reference of ir is not suite to be IV.
        //Thus return true to prevent subsequent processing.
        return true;
    }
    ASSERT0(exp && exp->is_exp());
    ASSERT0(stmt_of_exp);
    MDSSAInfo const* ssainfo = m_mdssamgr->getMDSSAInfoIfAny(exp);
    if (ssainfo == nullptr) { return false; }

    xoc::collectDefSet(exp, m_mdssamgr, set);
    IRSetIter it = nullptr;
    for (BSIdx i = set->get_first(&it);
         i != BS_UNDEF; i = set->get_next(i, &it)) {
        IR * stmt = m_rg->getIR(i);
        ASSERT0(stmt || stmt->is_stmt());
        if (stmt != stmt_of_exp && li->isInsideLoop(stmt->getBB()->id())) {
            set->clean();
            return true;
        }
    }
    return false;
}


//Return true if ir is reduction-operation.
//Note the function will use classic-DU/PRSSA/MDSSA to do analysis.
//set: record the DEF stmt set of reduction variable.
bool IVR::isReductionOp(IR const* ir, LI<IRBB> const* li,
                        OUT LinearRep * lr, OUT IRSet * set) const
{
    MD const* mustref = ir->getRefMD();
    if (mustref == nullptr ||
        (m_is_only_handle_exact_md && !mustref->is_exact())) {
        return false;
    }

    //Extract linear-representation of variable.
    ASSERT0(ir->getRHS());
    LinearRep linrep;
    if (!isLinearRepOfMD(li, ir->getRHS(), mustref, &linrep)) {
        return false;
    }

    //The coefficient and addend should be loop-invariant.
    ASSERT0(linrep.is_valid());
    if (linrep.addend == nullptr ||
        linrep.addend->is_const() ||
        isLoopInvariant(linrep.addend, li, m_rg, nullptr, true)) {
        ; //nothing to do
    } else { return false; }

    if (linrep.coeff == nullptr ||
        linrep.coeff->is_const() ||
        isLoopInvariant(linrep.coeff, li, m_rg, nullptr, true)) {
        ; //nothing to do
    } else { return false; }

    //The variable can be defined only once inside current loop.
    if (hasMultiDefInLoop(linrep.var, li, set)) {
        return false;
    }

    if (lr != nullptr) {
        lr->copy(linrep);
    }
    return true;
}


bool IVR::isSelfModByDUSet(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    DUSet const* useset = ir->readDUSet();
    if (useset == nullptr) { return false; }

    //SelfMod pattern under SSA mode.
    // x<-SelfModOP(x, 1)
    //Iterate all USEs to check if the stmt of USE is identical to 'ir'.
    DUSetIter it = nullptr;
    for (BSIdx i = useset->get_first(&it);
         i != BS_UNDEF; i = useset->get_next(i, &it)) {
        IR const* use = m_rg->getIR(i);
        ASSERT0(use->is_exp());

        IR const* use_stmt = use->getStmt();
        ASSERT0(use_stmt && use_stmt->is_stmt());

        if (use_stmt == ir) {
            return true;
        }
    }
    return false;
}


bool IVR::isSelfModByPRSSA(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    SSAInfo const* ssainfo = ir->getSSAInfo();
    if (ssainfo == nullptr) { return false; }

    //SelfMod pattern under SSA mode.
    // pr3<-...
    // pr4<-PHI(pr3, pr5)
    // pr5<-SelfModOP(pr4, 1)
    SSAUseIter it;
    for (BSIdx u = SSA_uses(ssainfo).get_first(&it);
         u != BS_UNDEF; u = SSA_uses(ssainfo).get_next(u, &it)) {
        IR const* use = m_rg->getIR(u);
        ASSERT0(use && use->is_exp());

        if (use->getStmt()->is_phi() &&
            ir->getOpndPR(PHI_prno(use->getStmt())) != nullptr) {
            return true;
        }
    }
    return false;
}


bool IVR::isSelfModByMDSSA(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    MDSSAInfo const* ssainfo = m_mdssamgr->getMDSSAInfoIfAny(ir);
    if (ssainfo == nullptr) { return false; }

    //SelfMod pattern under SSA mode.
    // md3<-...
    // md4<-PHI(md3, md5)
    // md5<-SelfModOP(md4, 1)
    VOpndSetIter it;
    for (BSIdx u = const_cast<MDSSAInfo*>(ssainfo)->getVOpndSet()->
             get_first(&it);
         u != BS_UNDEF; u = const_cast<MDSSAInfo*>(ssainfo)->
             getVOpndSet()->get_next(u, &it)) {
        VMD * vopnd = (VMD*)m_mdssamgr->getVOpnd(u);
        ASSERT0(vopnd && vopnd->is_md());

        VMD::UseSetIter vit;
        for (INT i = vopnd->getUseSet()->get_first(vit);
             !vit.end(); i = vopnd->getUseSet()->get_next(vit)) {
            IR * use = m_rg->getIR(i);
            ASSERT0(use && (use->isMemRef() || use->is_id()));

            //Only consider PHI.
            if (!use->is_id()) { continue; }

            ASSERT0(ID_phi(use));
            if (ir->getOpndMem(ID_phi(use)->getResultMD(m_mdsys)) != nullptr) {
                return true;
            }
        }
    }
    return false;
}


//Return true if ir is self-modified, e.g: x = op(x).
bool IVR::isSelfMod(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    if (ir->isWritePR() && usePRSSADU()) {
        return isSelfModByPRSSA(ir);
    }
    if (ir->isMemRef() && useMDSSADU()) {
        return isSelfModByMDSSA(ir);
    }
    return isSelfModByDUSet(ir);
}


//Find Basic IV.
void IVR::findBIV(LI<IRBB> const* li, IDTab & tmp)
{
    IRBB const* back_start_bb = xoc::findBackEdgeStartBB(li, m_cfg);
    if (back_start_bb == nullptr) {
        //TODO: support more sophisiticated CFG pattern.
        return;
    }

    IRSet set(getSegMgr()); //for tmp used, it must be clean before return.
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        //if ((UINT)i == headi) { continue; }
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());

        if ((UINT)i != back_start_bb->id() &&
            !m_cfg->is_dom(i, back_start_bb->id())) {
            //bb should dominate backedge start BB.
            //TODO: consider the convergence of IV reduction.
            //e.g:  LoopHeadBB
            //       |      |
            //       V      V
            //   i=i+1      i=i+1
            //   |__         ___|
            //      |        |
            //      V        V
            //      goto LoopHeadBB //backedge start BB
            continue;
        }

        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            if (ir->isCallStmt()) {
                //TODO: callstmt may be intrinsic operation.
                continue;
            }
            if (!ir->is_st() && !ir->is_ist() && !ir->is_stpr() &&
                !ir->is_starray()) {
                continue;
            }

            //ir is the candidate.
            LinearRep lr;
            set.clean();
            if (isReductionOp(ir, li, &lr, &set)) {
                BIV * biv = nullptr;
                if (!extractBIV(ir, lr, set, li, &biv)) {
                    continue;
                }
                recordBIV(biv);
            }
        }
    }
    set.clean();
}


//Return true if ir can be regard as induction expression.
//'defs': def list of 'ir'.
bool IVR::scanExp(IR const* ir, LI<IRBB> const* li, IDTab const& bivset)
{
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    case IR_CONST:
    case IR_LDA:
        return true;
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_INDIRECT_MEM_EXP:
    SWITCH_CASE_READ_ARRAY:
    SWITCH_CASE_READ_PR: {
        MD const* irmd = ir->getExactRef();
        if (irmd == nullptr) { return false; }
        if (bivset.find(irmd->id())) {
            return true;
        }
        if (xoc::isLoopInvariant(ir, li, m_rg, nullptr, false)) {
            return true;
        }
        return false;
    }
    SWITCH_CASE_ARITH:
    SWITCH_CASE_SHIFT:
        if (!scanExp(BIN_opnd0(ir), li, bivset)) { return false; }
        if (!scanExp(BIN_opnd1(ir), li, bivset)) { return false; }
        return true;
    case IR_CVT:
        return scanExp(CVT_exp(ir), li, bivset);
    default:;
    }
    return false;
}


//Return true if ir is coefficent of linear-representation.
bool IVR::isCoeff(LI<IRBB> const* li, IR const* ir) const
{
    return isLoopInvariant(ir, li, m_rg, nullptr, true);
}


//Return true if ir is addend of linear-representation.
bool IVR::isAddend(LI<IRBB> const* li, IR const* ir) const
{
    return isLoopInvariant(ir, li, m_rg, nullptr, true);
}


static bool isMDEqual(MD const* md, IR const* ir)
{
    return md == ir->getRefMD();
}


//Return true if ir is linear-representation of BIV.
//e.g: if i is IV, a*i is the linear-represetation of i.
//li: given the LoopInfo.
//ir: IR expression that to be analyzed.
//selfmd: indicates the MD of self-modified variable.
//linrep: record and output linear-representation if exist.
bool IVR::isLinearRepOfMD(LI<IRBB> const* li, IR const* ir, MD const* selfmd,
                          OUT LinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    if (ir->is_add() || ir->is_sub()) {
        //LinearRep of IV:a*i +/- b.
        if (isLinearRepOfMD(li, BIN_opnd0(ir), selfmd, linrep) &&
            isAddend(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                linrep->addend = BIN_opnd1(ir);
                linrep->addend_sign = ir->getCode();
            }
            return true;
        }
        if (isLinearRepOfMD(li, BIN_opnd1(ir), selfmd, linrep) &&
            isAddend(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                linrep->addend = BIN_opnd0(ir);
                linrep->addend_sign = ir->getCode();
            }
            return true;
        }
        return false;
    }

    if (ir->is_mul()) {
        //LinearRep of IV:a*i.
        if (isMDEqual(selfmd, BIN_opnd0(ir)) && isCoeff(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd1(ir);
                linrep->var = BIN_opnd0(ir);
            }
            return true;
        }
        if (isMDEqual(selfmd, BIN_opnd1(ir)) && isCoeff(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd0(ir);
                linrep->var = BIN_opnd1(ir);
            }
            return true;
        }
    }

    if (isMDEqual(selfmd, ir)) {
        //1*i is linear-rep of i.
        if (linrep != nullptr) {
            linrep->var = ir;
        }
        return true;
    }
    return false;
}


bool IVR::isLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                          OUT LinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    IV const* iv = nullptr;
    if (isIV(li, ir, &iv)) {
        if (linrep != nullptr) {
            linrep->coeff = m_irmgr->buildImmInt(1, ir->getType());
            linrep->iv = iv;
            linrep->var = ir;
        }
        return true;
    }
    if (ir->is_mul()) {
        if (isIV(li, BIN_opnd0(ir), &iv) && isCoeff(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd1(ir);
                linrep->iv = iv;
                linrep->var = BIN_opnd0(ir);
            }
            return true;
        }
        if (isIV(li, BIN_opnd1(ir), &iv) && isCoeff(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd0(ir);
                linrep->iv = iv;
                linrep->var = BIN_opnd1(ir);
            }
            return true;
        }
    }
    return false;
}


//Return true if ir is linear-representation.
bool IVR::isLinearRep(LI<IRBB> const* li, IR const* ir,
                      LinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    if (ir->is_add() || ir->is_sub()) {
        //LinearRep of IV:a*i +/- b.
        if (isLinearRepOfIV(li, BIN_opnd0(ir), linrep) &&
            isAddend(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                ASSERT0(linrep->iv);
                linrep->addend = BIN_opnd1(ir);
                linrep->addend_sign = ir->getCode();
            }
            return true;
        }
        if (isLinearRepOfIV(li, BIN_opnd1(ir), linrep) &&
            isAddend(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                ASSERT0(linrep->iv);
                linrep->addend = BIN_opnd0(ir);
                linrep->addend_sign = ir->getCode();
            }
            return true;
        }
        return false;
    }
    if (ir->is_mul()) {
        //LinearRep of IV:a*i.
        IV const* iv = nullptr;
        if (isIV(li, BIN_opnd0(ir), &iv) && isCoeff(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd1(ir);
                linrep->var = BIN_opnd0(ir);
                linrep->iv = iv;
            }
            return true;
        }
        if (isIV(li, BIN_opnd1(ir), &iv) && isCoeff(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd0(ir);
                linrep->var = BIN_opnd1(ir);
                linrep->iv = iv;
            }
            return true;
        }
        return false;
    }
    return false;
}


void IVR::findDIVByStmt(IR * ir, LI<IRBB> const* li,
                        BIVList const& bivlst, OUT IRSet & set)
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR: {
        MD const* ref = ir->getRefMD();
        if (ref == nullptr || !ref->is_exact() ||
            isBIV(li, ir, nullptr)) {
            return;
        }
        LinearRep linrep;
        if (isLinearRep(li, ir->getRHS(), &linrep) &&
            !hasMultiDefInLoop(ir, li, &set)) {
            //ir is DIV.
            ASSERT0(linrep.is_valid());
            LinearRep * r = allocLinearRep();
            r->copy(linrep);
            recordDIV(li, ir, r);
        }
        return;
    }
    default:;
    }
}


void IVR::findDIV(LI<IRBB> const* li, BIVList const& bivlst)
{
    if (bivlst.get_elem_count() == 0) { return; }
    IRSet set(getSegMgr());
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            findDIVByStmt(ir, li, bivlst, set);
        }
    }
    set.clean();
}


void IVR::dump_recur(LI<IRBB> const* li, UINT indent) const
{
    Region const* rg = getRegion();
    INT orgindent = rg->getLogMgr()->getIndent();
    while (li != nullptr) {
        note(getRegion(), "\n\n==-- LOOP INFO --==\n");
        //Dump loopinfo.
        //for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
        note(getRegion(), "\nLI%d:BB%d", li->id(), li->getLoopHead()->id());
        prt(getRegion(), ",BODY:");
        for (BSIdx i = li->getBodyBBSet()->get_first();
             i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
            prt(getRegion(), "%d,", i);
        }

        BIVList const* bivlst = m_li2bivlst.get(li->id());
        if (bivlst != nullptr) {
            for (BIVListIter it = bivlst->get_head();
                 it != bivlst->end(); it = bivlst->get_next(it)) {
                BIV const* iv = it->val();
                ASSERT0(iv);
                iv->dump(getRegion());
            }
        }

        DIVList * divlst = m_li2divlst.get(li->id());
        if (divlst != nullptr) {
            for (DIVListIter it = divlst->get_head();
                 it != divlst->end(); it = divlst->get_next(it)) {
                DIV const* iv = it->val();
                iv->dump(getRegion());
            }
        }

        dump_recur(LI_inner_list(li), indent + 2);
        li = LI_next(li);
    }
    rg->getLogMgr()->setIndent(orgindent);
}


//Dump IVR info for loop.
bool IVR::dump() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    dump_recur(m_cfg->getLoopInfo(), 0);
    return Pass::dump();
}


void IVR::clean()
{
    for (VecIdx i = 0; i <= m_li2bivlst.get_last_idx(); i++) {
        BIVList * ivlst = m_li2bivlst.get(i);
        if (ivlst == nullptr) { continue; }
        ivlst->clean();
    }
    for (VecIdx i = 0; i <= m_li2divlst.get_last_idx(); i++) {
        DIVList * ivlst = m_li2divlst.get(i);
        if (ivlst == nullptr) { continue; }
        ivlst->clean();
    }
}


IR const* IVR::findBIVBoundStmt(LI<IRBB> const* li,
                                OUT BIV const** biv) const
{
    ASSERT0(li);
    BIVList const* bivlst = getBIVList(li);
    if (bivlst == nullptr) {
        //There is no biv.
        return nullptr;
    }
    //Loop head can not change the control flow of loop.
    //The loop head must be inside the loop body.
    List<UINT> endlst;
    li->findAllLoopEndBB(m_cfg, endlst);
    if (endlst.get_elem_count() > 1) {
        //Multiple exit BB.
        return nullptr;
    }
    UINT bbid = endlst.get_head();
    ASSERT0(bbid != BBID_UNDEF);
    IRBB * bb = m_cfg->getBB(bbid);
    ASSERT0(bb);
    IR * bstmt = bb->getLastIR();
    ASSERTN(bstmt && bstmt->isConditionalBr(), ("weird exit BB"));
    for (BIVListIter it = bivlst->get_head();
         it != bivlst->end(); it = bivlst->get_next(it)) {
        BIV const* iv = it->val();
        ASSERT0(iv);
        if (isBIVBoundStmt(iv, li, bstmt)) {
            if (biv != nullptr) { *biv = iv; }
            return bstmt;
        }
    }
    if (biv != nullptr) { *biv = nullptr; }
    return nullptr;
}


bool IVR::extractIVBoundExpFromStmt(IV const* iv, IR const* stmt,
                                    OUT IR const** ivref,
                                    OUT IR const** bexp,
                                    OUT bool * is_closed_range) const
{
    ASSERT0(iv && stmt && stmt->is_stmt());
    if (!stmt->isConditionalBr()) { return false; }
    IR const* compare_exp = BR_det(stmt);
    if (!compare_exp->is_relation()) { return false; }
    switch (compare_exp->getCode()) {
    case IR_LE:
    case IR_GE:
        *is_closed_range = true;
    default:
        *is_closed_range = false;
    }
    if (!extractIVBoundExp(iv, compare_exp, ivref, bexp)) {
        return false;
    }
    ASSERT0(ivref && bexp);
    return true;
}


bool IVR::extractIVBoundExp(IV const* iv, IR const* compare_exp,
                            OUT IR const** ivref,
                            OUT IR const** bexp) const
{
    ASSERT0(compare_exp && compare_exp->is_relation());
    IR const* opnd0 = BIN_opnd0(compare_exp);
    IR const* opnd1 = BIN_opnd1(compare_exp);
    ASSERT0(ivref && bexp);
    *ivref = nullptr;
    *bexp = nullptr;
    if (opnd0->isMemOpnd()) {
        MD const* mustref = opnd0->getMustRef();
        if (mustref == nullptr) { return false; }
        if (mustref == iv->getOccMD()) {
            *ivref = opnd0;
            *bexp = opnd1;
            return true;
        }
    }
    if (opnd1->isMemOpnd()) {
        MD const* mustref = opnd1->getMustRef();
        if (mustref == nullptr) { return false; }
        if (mustref == iv->getOccMD()) {
            *ivref = opnd1;
            *bexp = opnd0;
            return true;
        }
    }
    return false;
}


bool IVR::isBIVBoundExp(BIV const* biv, IR const* compare_exp,
                        IR const* ivref) const
{
    ASSERT0(biv && compare_exp->is_exp() && ivref->is_exp() &&
            ivref->getParent() == compare_exp);
    if (!compare_exp->is_relation()) { return false; }
    IR const* opnd0 = BIN_opnd0(compare_exp);
    IR const* opnd1 = BIN_opnd1(compare_exp);
    if (biv->isInc()) {
        //Basic IV is in incremental order.
        if (ivref == opnd0 &&
           (compare_exp->is_lt() || compare_exp->is_le() ||
            compare_exp->is_ne())) {
            //compare_exp may be: iv<UB, iv<=UB, or iv!=UB.
            return true;
        }
        if (ivref == opnd1 &&
           (compare_exp->is_gt() || compare_exp->is_ge() ||
            compare_exp->is_ne())) {
            //compare_exp may be: UB>iv, UB>=iv, or UB!=iv.
            return true;
        }
        return false;
    }
    //Basic IV is in decremental order.
    if (ivref == opnd0 &&
        (compare_exp->is_gt() || compare_exp->is_ge() ||
         compare_exp->is_ne())) {
         //compare_exp may be: iv>UB, iv>=UB, or iv!=UB.
         return true;
    }
    if (ivref == opnd1 &&
        (compare_exp->is_lt() || compare_exp->is_le() ||
         compare_exp->is_ne())) {
         //compare_exp may be: UB<iv, UB<=iv, or UB!=iv.
         return true;
    }
    return false;
}


bool IVR::isBIVBoundStmt(BIV const* biv, LI<IRBB> const* li,
                         IR const* stmt) const
{
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    if (!extractIVBoundExpFromStmt(biv, stmt, &ivref, &bexp,
                                   &is_closed_range)) {
        return false;
    }
    ASSERT0(ivref && bexp);
    DUMMYUSE(is_closed_range);
    ASSERT0(stmt->isConditionalBr());
    IR const* compare_exp = BR_det(stmt);
    if (stmt->is_falsebr() &&
        xoc::isBranchTargetOutSideLoop(li, m_cfg, stmt) &&
        isBIVBoundExp(biv, compare_exp, ivref)) {
        return true;
    }
    if (stmt->is_truebr() &&
        xoc::isBranchTargetOutSideLoop(li, m_cfg, stmt)) {
        IR * newir = IR::invertIRCode(const_cast<IR*>(compare_exp), m_rg);
        ASSERT0_DUMMYUSE(newir == compare_exp);
        bool res = isBIVBoundExp(biv, compare_exp, ivref);
        IR::invertIRCode(const_cast<IR*>(compare_exp), m_rg);
        return res;
    }
    return false;
}


bool IVR::computeConstValOfExp(IR const* exp, OUT HOST_INT & val) const
{
    ASSERT0(exp && exp->is_exp());
    if (exp->isConstInt()) {
        //Bound expression is constant.
        val = CONST_int_val(exp);
        return true;
    }
    if (useGVN()) {
        //Determine whether the expression has constant value.
        ASSERT0(m_gvn);
        VN const* vn = m_gvn->getVN(exp);
        if (vn == nullptr) {
            //We know nothing about value of exp.
            return false;
        }
        if (vn->getType() != VN_INT) {
            return false;
        }
        val = VN_int_val(vn);
        return true;
    }
    return false;
}


bool IVR::computeConstInitValOfBIV(BIV const* biv, OUT HOST_INT & val) const
{
    if (biv->isInitVar() && useGVN()) {
        ASSERT0(m_gvn);
        IR const* initval = biv->getInitStmt();
        ASSERT0(initval && initval->hasRHS());
        VN const* vn = m_gvn->getVN(initval->getRHS());
        if (vn == nullptr) {
            //We know nothing about initial value of IV.
            return false;
        }
        if (vn->getType() != VN_INT) {
            return false;
        }
        val = VN_int_val(vn);
        return true;
    }
    if (biv->isInitConstInt()) {
        val = *biv->getInitValInt();
        return true;
    }
    return false;
}


void IVR::setAggressive(bool doit)
{
    m_is_aggressive = doit;
    m_gvn = (GVN*)m_rg->getPassMgr()->registerPass(PASS_GVN);
}


bool IVR::computeIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                         MOD IVRCtx & ivrctx) const
{
    if (computeConstIVBound(li, bi)) { return true; }
    if (computeExpIVBound(li, bi, ivrctx)) { return true; }
    return false;
}


IR * IVR::genTripCountExp(BIV const* biv, IR const* initexp,
                          IR const* boundexp, HOST_INT step,
                          MOD IVRCtx & ivrctx) const
{
    IR const* iv_initval = nullptr;
    IR const* iv_endval = nullptr;
    if (biv->is_inc()) {
        iv_initval = initexp;
        iv_endval = boundexp;
    } else {
        iv_initval = boundexp;
        iv_endval = initexp;
    }
    DUMMYUSE(iv_initval);
    Type const* ty = iv_endval->getType();
    IR * tripcount_exp = m_irmgr->buildBinaryOp(IR_DIV, ty,
        m_irmgr->buildBinaryOp(IR_SUB, ty,
                               m_rg->dupIRTree(boundexp),
                               m_rg->dupIRTree(initexp)),
        m_irmgr->buildImmInt(step, ty));
    Refine * refine = (Refine*)m_rg->getPassMgr()->queryPass(PASS_REFINE);
    if (refine != nullptr) {
        //Perform peephole optimization to ir.
        //Return updated ir if optimization performed.
        IR * refineIR(IR * ir, bool & change, RefineCtx & rc);
        RefineCtx rc(ivrctx.getOptCtx());
        bool change;
        tripcount_exp = refine->refineIRUntilUnchange(tripcount_exp,
                                                      change, rc);
    }
    return tripcount_exp;
}


bool IVR::computeExpIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                            MOD IVRCtx & ivrctx) const
{
    BIV const* biv = nullptr;
    IR const* ubstmt = findBIVBoundStmt(li, &biv);
    if (ubstmt == nullptr) {
        //The shape of 'li' is denormal, we almost can not figure the result
        //out in a high probability.
        return false;
    }
    ASSERT0(ubstmt->is_stmt());
    ASSERT0(biv);
    HOST_INT step = (HOST_INT)biv->getStep();
    if (step == 0) {
        //Step is 0, may cause infinit loop.
        return false;
    }
    ASSERT0(biv);
    if (!biv->hasInitVal()) { return false; }

    //Get the initial value of IV.
    IR const* initexp = biv->getInitExp();

    //Compute the finish value of IV.
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    extractIVBoundExpFromStmt(biv, ubstmt, &ivref, &bexp, &is_closed_range);
    ASSERT0(ivref && bexp);
    IR * tripcount_exp = genTripCountExp(biv, initexp, bexp, step, ivrctx);
    ASSERT0(tripcount_exp);
    IVBI_is_tc_imm(bi) = false;
    IVBI_tc_exp(bi) = tripcount_exp;
    IVBI_iv(bi) = biv;
    IVBI_iv_end_bound_stmt(bi) = ubstmt;
    IVBI_is_end_bound_closed(bi) = is_closed_range;
    return true;
}


bool IVR::computeConstIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi) const
{
    BIV const* biv = nullptr;
    IR const* ubstmt = findBIVBoundStmt(li, &biv);
    if (ubstmt == nullptr) {
        //The shape of 'li' is denormal, we almost can not figure the result
        //out in a high probability.
        return false;
    }
    ASSERT0(biv);
    if (!biv->hasInitVal()) { return false; }

    //Compute the initial value of IV.
    HOST_INT iv_initval = 0;
    if (!computeConstInitValOfBIV(biv, iv_initval)) { return false; }

    //Compute the finish value of IV.
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    extractIVBoundExpFromStmt(biv, ubstmt, &ivref, &bexp, &is_closed_range);
    ASSERT0(ivref && bexp);
    HOST_INT iv_endval = 0;
    //Whether the end bound is constant value.
    if (!computeConstValOfExp(bexp, iv_endval)) { return false; }
    HOST_INT step = (HOST_INT)biv->getStep();
    if (step == 0) {
        //Step is 0, may cause infinit loop.
        return false;
    }
    HOST_INT trip_count;
    if (biv->is_inc()) {
        trip_count = (iv_endval - iv_initval) / step;
    } else {
        trip_count = (iv_initval - iv_endval) / step;
    }
    if (trip_count < 0) { return false; }
    IVBI_is_tc_imm(bi) = true;
    IVBI_tc_imm(bi) = trip_count;
    IVBI_iv(bi) = biv;
    IVBI_tc_init_val_imm(bi) = iv_initval;
    IVBI_tc_end_val_imm(bi) = iv_endval;
    IVBI_iv_end_bound_stmt(bi) = ubstmt;
    IVBI_is_end_bound_closed(bi) = is_closed_range;
    return true;
}


bool IVR::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();

    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());
    clean();
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DU_REF, PASS_DOM,
                                               PASS_LOOP_INFO, PASS_RPO,
                                               PASS_UNDEF);
    m_du = (DUMgr*)m_rg->getPassMgr()->queryPass(PASS_DU_MGR);
    if (is_aggressive() && !useGVN()) {
        m_gvn = (GVN*)m_rg->getPassMgr()->registerPass(PASS_GVN);
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_GVN, PASS_UNDEF);
        ASSERT0(useGVN());
    }
    LI<IRBB> const* li = m_cfg->getLoopInfo();
    if (li == nullptr) {
        END_TIMER(t, getPassName());
        set_valid(true);
        return false;
    }

    IDTab tmp;
    CLoopInfoIter it;
    for (LI<IRBB> const* tli = iterInitLoopInfoC(li, it);
         tli != nullptr; tli = iterNextLoopInfoC(it)) {
        tmp.clean(); //tmp is used to record exact/effect MD which be modified.
        findBIV(tli, tmp);
        BIVList const* bivlst = m_li2bivlst.get(tli->id());
        if (bivlst != nullptr) {
            findDIV(tli, *bivlst);
        }
    }

    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpIVR()) {
        dump();
    }
    set_valid(true);
    END_TIMER(t, getPassName());
    return false;
}
//END IVR

} //namespace xoc
