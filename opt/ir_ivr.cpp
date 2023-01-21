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
            if (iv->getInitValInt() == nullptr) {
                BIV_initv_int(iv) = (BIVIntType*)xmalloc(sizeof(BIVIntType));
            }
            *BIV_initv_int(iv) = CONST_int_val(v);
            BIV_initv_kind(iv) = IV_INIT_VAL_IS_INT;
            return true;
        }
        if (v->is_fp()) {
            if (iv->getInitValFp() == nullptr) {
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
    IRBB const* back_start_bb = findBackedgeStartBB(li, m_cfg);
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



//Return true if ir is linear-representation of BIV.
//e.g: if i is IV, a*i is the linear-represetation of i.
bool IVR::isLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                          LinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    IV const* iv = nullptr;
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
    if (isIV(li, ir, &iv)) {
        //1*i is linear-rep of i.
        if (linrep != nullptr) {
            linrep->iv = iv;
            linrep->var = ir;
        }
        return true;
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
                linrep->addend = BIN_opnd1(ir);
                linrep->addend_sign = ir->getCode();
            }
            return true;
        }
        if (isLinearRepOfIV(li, BIN_opnd1(ir), linrep) &&
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
    while (li != nullptr) {
        note(getRegion(), "\n\n==-- LOOP INFO --==\n");
        //Dump loopinfo.
        for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
        prt(getRegion(), "LI%d:BB%d", li->id(), li->getLoopHead()->id());
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
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "BIV(MD%d", iv->getOccMD()->id());

                if (iv->hasInitVal()) {
                    if (iv->isInitConst()) {
                        prt(getRegion(), ",init=%lld",
                            (BIVIntType)*iv->getInitValInt());
                    } else {
                        prt(getRegion(), ",init=MD%d",
                            (INT)iv->getInitValMD()->id());
                    }
                }

                if (iv->is_inc()) {
                    prt(getRegion(), ",step=%lld", (BIVIntType)iv->getStep());
                } else {
                    prt(getRegion(), ",step=-%lld", (BIVIntType)iv->getStep());
                }

                prt(getRegion(), ")");
                getRegion()->getLogMgr()->incIndent(2);

                //Dump BIV's def-stmt.
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "DEF-STMT:");
                ASSERT0(iv->getRedStmt());
                getRegion()->getLogMgr()->incIndent(2);
                dumpIR(iv->getRedStmt(), m_rg, nullptr, IR_DUMP_KID);
                getRegion()->getLogMgr()->decIndent(2);

                //Dump BIV's occ-exp.
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "OCC-EXP:");
                ASSERT0(iv->getRedStmt());
                getRegion()->getLogMgr()->incIndent(2);
                dumpIR(iv->getRedStmt(), m_rg, nullptr, IR_DUMP_KID);
                getRegion()->getLogMgr()->decIndent(2);

                //Dump BIV's init-stmt.
                if (iv->getInitStmt() != nullptr) {
                    note(getRegion(), "\n");
                    for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                    prt(getRegion(), "INIT-STMT:");
                    getRegion()->getLogMgr()->incIndent(2);
                    dumpIR(iv->getInitStmt(), m_rg, nullptr, IR_DUMP_KID);
                    getRegion()->getLogMgr()->decIndent(2);
                }

                getRegion()->getLogMgr()->decIndent(2);
            }
        }

        DIVList * divlst = m_li2divlst.get(li->id());
        if (divlst != nullptr) {
            for (DIVListIter it = divlst->get_head();
                 it != divlst->end(); it = divlst->get_next(it)) {
                DIV const* iv = it->val();
                ASSERT0(iv && iv->getRedStmt());
                note(getRegion(), "\nDIV");

                //Dump div occurrence.
                getRegion()->getLogMgr()->incIndent(2);
                note(getRegion(), "\nOCC:");

                getRegion()->getLogMgr()->incIndent(2);
                dumpIR(iv->getRedStmt(), m_rg, nullptr, IR_DUMP_KID);
                getRegion()->getLogMgr()->decIndent(2);

                getRegion()->getLogMgr()->decIndent(2);

                //Dump linear-representation of div.
                getRegion()->getLogMgr()->incIndent(2);
                note(getRegion(), "\nLINREP:");
                iv->getLinRep()->dump(m_rg);
                getRegion()->getLogMgr()->decIndent(2);
            }
        }

        dump_recur(LI_inner_list(li), indent + 2);
        li = LI_next(li);
    }
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
