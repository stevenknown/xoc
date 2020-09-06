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
//START IVR
//
bool IVR::computeInitVal(IR const* ir, IV * iv)
{
    if (!ir->is_st() && !ir->is_ist() && !ir->is_stpr() && !ir->is_starray()) {
        return false;
    }

    IV_initv_stmt(iv) = ir;
    IR const* v = ST_rhs(ir); //v is the initial value.
    if (v->is_cvt()) {
        v = ((CCvt*)v)->getLeafExp();
    }

    IV_initv_data_type(iv) = v->getType();
    if (v->is_const() && v->is_int()) {
        if (IV_initv_i(iv) == NULL) {
            IV_initv_i(iv) = (LONGLONG*)xmalloc(sizeof(LONGLONG));
        }
        *IV_initv_i(iv) = CONST_int_val(v);
        IV_initv_type(iv) = IV_INIT_VAL_IS_CONST;
        return true;
    }

    MD const* md = v->getExactRef();
    if (md != NULL) {
        IV_initv_md(iv) = md;
        IV_initv_type(iv) = IV_INIT_VAL_IS_VAR;
        return true;
    }

    IV_initv_i(iv) = NULL;
    return false;
}


//Find initialze value of IV, if found return true, otherwise return false.
bool IVR::findInitVal(IV * iv)
{
    DUSet const* defs = IV_iv_occ(iv)->readDUSet();
    ASSERT0(defs);
    ASSERT0(IV_iv_occ(iv)->is_exp());
    ASSERT0(IV_iv_def(iv)->is_stmt());
    IR const* domdef = m_du->findDomDef(IV_iv_occ(iv),
                                        IV_iv_def(iv),
                                        defs, true);
    if (domdef == NULL) { return false; }

    MD const* emd = NULL;
    if (m_is_only_handle_exact_md) {
        emd = domdef->getExactRef();
    } else {
        emd = domdef->getEffectRef();
    }

    if (emd == NULL || emd != IV_iv(iv)) {
        return false;
    }

    LI<IRBB> const* li = IV_li(iv);
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
    IRIter ii;
    for (IR * x = iterInit(start, ii); x != NULL; x = iterNext(ii)) {
        MD const* xmd = x->getRefMD();
        if (xmd != NULL && xmd == biv) {
            //Note there may be multiple occurrences matched biv.
            //We just return the first if meet.
            return x;
        }
    }
    return NULL;
}


bool IVR::matchIVUpdate(MD const* biv,
                           IR const* def,
                           IR ** occ,
                           IR ** delta,
                           bool & is_increment)
{
    ASSERT0(def->is_st());
    IR * rhs = ST_rhs(def);
    if (rhs->is_add()) {
        is_increment = true;
    } else if (rhs->is_sub()) {
        is_increment = false;
    } else {
        //Not monotonic operation.
        return false;
    }

    //Make sure self modify stmt is monotonic.
    IR * op0 = BIN_opnd0(rhs);
    if (m_is_strictly_match_pattern) {
        //Strictly match the pattern: i = i + 1.
        MD const* occmd = op0->getRefMD();
        if (occmd == NULL || occmd != biv) {
            return false;
        }
    } else {
        op0 = findMatchedOcc(biv, op0);
        if (op0 == NULL) { return false; }
    }

    IR * op1 = BIN_opnd1(rhs);
    if (op1->is_int()) {
        ;
    } else if (g_is_support_dynamic_type && op1->is_const()) {
        //TODO: support dynamic const type as the addend of ADD/SUB.
        return false;
    } else {
        return false;
    }

    if (m_is_only_handle_exact_md) {
        MD const* op0md = op0->getExactRef();
        if (op0md == NULL || op0md != biv) {
            return false;
        }
    }

    ASSERT0(op0 && op1);
    *occ = op0;
    *delta = op1;
    return true;
}


void IVR::recordIV(MD * biv,
                   LI<IRBB> const* li,
                   IR * def,
                   IR * occ,
                   IR * delta,
                   bool is_increment)
{
    ASSERT0(biv && li && def && occ && delta && delta->is_const());
    IV * x = allocIV();
    IV_iv(x) = biv;
    IV_li(x) = li;
    IV_iv_occ(x) = occ;
    IV_iv_def(x) = def;
    IV_step(x) = CONST_int_val(delta);
    IV_is_inc(x) = is_increment;
    findInitVal(x);

    //Same occ may correspond to multiple Loop Info.
    //e.g: c4.c
    //  while () {
    //    while () {
    //      ...
    //      i++;
    //    }
    //  }
    m_ir2iv.set(occ, x);
    m_ir2iv.set(def, x);

    SList<IV*> * ivlst = m_li2bivlst.get(li->id());
    if (ivlst == NULL) {
        ivlst = (SList<IV*>*)xmalloc(sizeof(SList<IV*>));
        ivlst->init(m_sc_pool);
        m_li2bivlst.set(li->id(), ivlst);
    }
    ivlst->append_head(x);
}


//Find Basic IV.
//'map_md2defcount': record the number of definitions to MD.
//'map_md2defir': map MD to definition stmt.
void IVR::findBIV(LI<IRBB> const* li,
                  xcom::BitSet & tmp,
                  Vector<UINT> & map_md2defcount,
                  UINT2IR & map_md2defir)
{
    IRBB * head = li->getLoopHead();
    UINT headi = head->id();
    tmp.clean(); //tmp is used to record exact/effect MD which be modified.
    map_md2defir.clean();
    map_md2defcount.clean();
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        //if ((UINT)i == headi) { continue; }
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(bb->id()));
        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            if (!ir->is_st() &&
                !ir->is_ist() &&
                !ir->is_stpr() &&
                !ir->is_starray() &&
                !ir->isCallStmt()) {
                continue;
            }

            MD const* emd = m_du->get_must_def(ir);
            if (emd != NULL) {
                //Only handle Must-Def stmt.
                if (!m_is_only_handle_exact_md ||
                    (m_is_only_handle_exact_md && emd->is_exact())) {
                    UINT emdid = MD_id(emd);
                    tmp.bunion(emdid);
                    UINT c = map_md2defcount.get(emdid) + 1;
                    map_md2defcount.set(emdid, c);
                    if (c == 1) {
                        //There is only one DEF of 'emdid'.
                        ASSERT0(map_md2defir.get(emdid) == NULL);
                        map_md2defir.setAlways(emdid, ir);
                        map_md2defcount.set(emdid, 1);
                    } else {
                        //For locality and performance, we do not remove
                        //node of TMap. Just reset the mapped value to be NULL.
                        map_md2defir.setAlways(emdid, NULL);
                        tmp.diff(emdid);
                    }
                }
            }
            MDSet const* maydef = m_du->getMayDef(ir);
            if (maydef == NULL) { continue; }

            //Check MayDef of stmt because 'ir' may-kill other definitions.
            for (INT i2 = tmp.get_first(); i2 != -1; i2 = tmp.get_next(i2)) {
                MD const* md = m_md_sys->getMD(i2);
                ASSERT0(!m_is_only_handle_exact_md || md->is_exact());
                if (maydef->is_contain(md)) {
                    map_md2defcount.set(i2, 0);

                    //For performance, we do not remove the TN of TMap.
                    //Just the mapped value to be NULL.
                    map_md2defir.setAlways(i2, NULL);
                    tmp.diff(i2);
                }
            }
        } //end for each IR.
    } //for each IRBB.

    //Begin to find BIV.

    //First, find the stmt that is single def-stmt of exact/effect MD.
    bool find = false;
    List<MD*> sdlst; //record list of MD that has single DEF stmt.
    for (INT i = tmp.get_first(); i != -1; i = tmp.get_next(i)) {
        if (map_md2defcount.get(i) != 1) { continue; }
        IR * def = map_md2defir.get(i);
        ASSERT0(def);

        //def stmt is reach-in of loop head.
        if (m_du->genInReachDef(headi, NULL)->is_contain(def->id())) {
            //It looks MD 'i' is biv.
            sdlst.append_head(m_md_sys->getMD(i));
            find = true;
        }
    }
    if (!find) { return; }

    //Find induction operation from the candidate stmt list.
    for (MD * biv = sdlst.get_head(); biv != NULL; biv = sdlst.get_next()) {
        IR * def = map_md2defir.get(MD_id(biv));
        ASSERT0(def);
        if (!def->is_st() &&
            !def->is_stpr() &&
            !def->is_ist() &&
            !def->is_starray()) {
            continue;
        }

        //Find the self modify stmt: i = i + ...
        DUSet const* useset = def->readDUSet();
        if (useset == NULL) { continue; }
        bool selfmod = false;
        DUIter di = NULL;
        for (INT i = useset->get_first(&di);
             i >= 0; i = useset->get_next(i, &di)) {
            IR const* use = m_rg->getIR(i);
            ASSERT0(use->is_exp());

            IR const* use_stmt = use->getStmt();
            ASSERT0(use_stmt && use_stmt->is_stmt());

            if (use_stmt == def) {
                selfmod = true;
                break;
            }
        }

        if (!selfmod) { continue; }

        IR * occ = NULL;
        IR * delta = NULL;
        bool is_increment = false;
        if (!matchIVUpdate(biv, def, &occ, &delta, is_increment)) { continue; }

        recordIV(biv, li, def, occ, delta, is_increment);
    }
}


//Return true if 'ir' is loop invariant expression.
//'defs': def set of 'ir'.
bool IVR::is_loop_invariant(LI<IRBB> const* li, IR const* ir)
{
    ASSERT0(ir->is_exp());
    DUSet const* defs = ir->readDUSet();
    if (defs == NULL) { return true; }
    DUIter di = NULL;
    for (INT i = defs->get_first(&di); i >= 0; i = defs->get_next(i, &di)) {
        IR const* d = m_rg->getIR(i);
        ASSERT0(d->is_stmt() && d->getBB());
        if (li->isInsideLoop(d->getBB()->id())) {
            return false;
        }
    }
    return true;
}


//Return true if ir can be regard as induction expression.
//'defs': def list of 'ir'.
bool IVR::scanExp(IR const* ir,
                  LI<IRBB> const* li,
                  xcom::BitSet const& ivmds)
{
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    case IR_CONST:
    case IR_LDA:
        return true;
    case IR_LD:
    case IR_ILD:
    case IR_ARRAY:
    case IR_PR: {
        MD const* irmd = ir->getExactRef();
        if (irmd == NULL) { return false; }
        if (ivmds.is_contain(MD_id(irmd))) {
            return true;
        }
        if (is_loop_invariant(li, ir)) {
            return true;
        }
        return false;
    }
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        if (!scanExp(BIN_opnd0(ir), li, ivmds)) { return false; }
        if (!scanExp(BIN_opnd1(ir), li, ivmds)) { return false; }
        return true;
    case IR_CVT:
        return scanExp(CVT_exp(ir), li, ivmds);
    default:;
    }
    return false;
}


//Try to add IV expresion into div list if 'e' do not exist in the list.
void IVR::addDIVList(LI<IRBB> const* li, IR const* e)
{
    SList<IR const*> * divlst = m_li2divlst.get(li->id());
    if (divlst == NULL) {
        divlst = (SList<IR const*>*)xmalloc(sizeof(SList<IR const*>));
        divlst->init(m_sc_pool);
        m_li2divlst.set(li->id(), divlst);
    }

    bool find = false;
    for (xcom::SC<IR const*> * sc = divlst->get_head();
         sc != divlst->end(); sc = divlst->get_next(sc)) {
        IR const* ive = sc->val();
        ASSERT0(ive);
        if (ive->isIREqual(e, true)) {
            find = true;
            break;
        }
    }
    if (find) { return; }
    divlst->append_head(e);
}


void IVR::findDIV(LI<IRBB> const* li,
                  xcom::SList<IV*> const& bivlst,
                  xcom::BitSet & tmp)
{
    if (bivlst.get_elem_count() == 0) { return; }

    tmp.clean(); //for tmp used.
    for (xcom::SC<IV*> * sc = bivlst.get_head();
         sc != bivlst.end(); sc = bivlst.get_next(sc)) {
        IV * iv = sc->val();
        ASSERT0(iv);
        tmp.bunion(MD_id(IV_iv(iv)));
    }

    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(bb->id()));
        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            switch (ir->getCode()) {
            case IR_ST:
            case IR_STPR:
            case IR_IST:
            case IR_STARRAY: {
                IR * rhs = ir->getRHS();
                if (scanExp(rhs, li, tmp)) {
                    addDIVList(li, rhs);
                }
                break;
            }
            case IR_CALL:
            case IR_ICALL:
                for (IR * p = CALL_param_list(ir);
                     p != NULL; p = p->get_next()) {
                    if (scanExp(p, li, tmp)) {
                        addDIVList(li, p);
                    }
                }
                break;
            case IR_TRUEBR:
            case IR_FALSEBR:
                if (scanExp(BR_det(ir), li, tmp)) {
                    addDIVList(li, BR_det(ir));
                }
                break;
            default:;
            }
        }
    }
}


void IVR::_dump(LI<IRBB> * li, UINT indent)
{
    while (li != NULL) {
        note(getRegion(), "\n");
        for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
        prt(getRegion(), "LI%d:BB%d", li->id(), li->getLoopHead()->id());
        prt(getRegion(), ",BODY:");
        for (INT i = li->getBodyBBSet()->get_first();
             i != -1; i = li->getBodyBBSet()->get_next(i)) {
            prt(getRegion(), "%d,", i);
        }

        xcom::SList<IV*> * bivlst = m_li2bivlst.get(li->id());
        if (bivlst != NULL) {
            for (xcom::SC<IV*> * sc = bivlst->get_head();
                 sc != bivlst->end(); sc = bivlst->get_next(sc)) {
                IV * iv = sc->val();
                ASSERT0(iv);
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "BIV(md%d", MD_id(IV_iv(iv)));

                if (IV_is_inc(iv)) {
                    prt(getRegion(), ",step=%lld", (LONGLONG)IV_step(iv));
                } else {
                    prt(getRegion(), ",step=-%lld", (LONGLONG)IV_step(iv));
                }

                if (iv->has_init_val()) {
                    if (iv->isInitConst()) {
                        prt(getRegion(), ",init=%lld", (LONGLONG)*IV_initv_i(iv));
                    } else {
                        prt(getRegion(), ",init=md%d", (INT)MD_id(IV_initv_md(iv)));
                    }
                }
                prt(getRegion(), ")");

                //Dump IV's def-stmt.
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "Def-Stmt:");
                ASSERT0(IV_iv_def(iv));
                getRegion()->getLogMgr()->setIndent(indent + 2);
                dumpIR(IV_iv_def(iv), m_rg, NULL, IR_DUMP_KID);

                //Dump IV's occ-exp.
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "Occ-Exp:");
                ASSERT0(IV_iv_occ(iv));
                getRegion()->getLogMgr()->setIndent(indent + 2);
                dumpIR(IV_iv_occ(iv), m_rg, NULL, IR_DUMP_KID);

                //Dump IV's init-stmt.
                if (iv->getInitValStmt() != NULL) {
                    note(getRegion(), "\n");
                    for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                    prt(getRegion(), "Init-Stmt:");
                    getRegion()->getLogMgr()->setIndent(indent + 2);
                    dumpIR(iv->getInitValStmt(), m_rg, NULL, IR_DUMP_KID);
                }
            }
        } else { prt(getRegion(), "(NO BIV)"); }

        xcom::SList<IR const*> * divlst = m_li2divlst.get(li->id());
        if (divlst != NULL) {
            if (divlst->get_elem_count() > 0) {
                note(getRegion(), "\n");
                for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
                prt(getRegion(), "DIV:");
            }
            for (xcom::SC<IR const*> * sc = divlst->get_head();
                 sc != divlst->end(); sc = divlst->get_next(sc)) {
                IR const* iv = sc->val();
                ASSERT0(iv);
                getRegion()->getLogMgr()->setIndent(indent + 2);
                dumpIR(iv, m_rg, NULL, IR_DUMP_KID);
            }
        } else { prt(getRegion(), "(NO DIV)"); }

        _dump(LI_inner_list(li), indent + 2);
        li = LI_next(li);
    }
}


//Dump IVR info for loop.
void IVR::dump()
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    _dump(m_cfg->getLoopInfo(), 0);
}


void IVR::clean()
{
    for (INT i = 0; i <= m_li2bivlst.get_last_idx(); i++) {
        SList<IV*> * ivlst = m_li2bivlst.get(i);
        if (ivlst == NULL) { continue; }
        ivlst->clean();
    }
    for (INT i = 0; i <= m_li2divlst.get_last_idx(); i++) {
        SList<IR const*> * ivlst = m_li2divlst.get(i);
        if (ivlst == NULL) { continue; }
        ivlst->clean();
    }
    m_ir2iv.clean();
}


bool IVR::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    UINT n = m_rg->getBBList()->get_elem_count();
    if (n == 0) { return false; }
    ASSERT0(m_cfg && m_du && m_md_sys && m_tm);

    m_rg->checkValidAndRecompute(&oc, PASS_REACH_DEF,
                                 PASS_DU_REF, PASS_DOM, PASS_LOOP_INFO,
                                 PASS_DU_CHAIN, PASS_RPO, PASS_UNDEF);

    if (!OC_is_ref_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    //Check PR DU chain.
    PRSSAMgr * ssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->queryPass(
        PASS_PR_SSA_MGR));
    if (ssamgr != NULL && ssamgr->is_valid()) {
        m_ssamgr = ssamgr;
    } else {
        m_ssamgr = NULL;
    }
    if (!OC_is_pr_du_chain_valid(oc) && m_ssamgr == NULL) { 
        //At least one kind of DU chain should be avaiable.
        END_TIMER(t, getPassName());
        return false;
    }
    //Check NONPR DU chain.
    MDSSAMgr * mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR));
    if (mdssamgr != NULL && mdssamgr->is_valid()) {
        m_mdssamgr = mdssamgr;
    } else {
        m_mdssamgr = NULL;
    }
    if (!OC_is_nonpr_du_chain_valid(oc) && m_mdssamgr == NULL) {
        //At least one kind of DU chain should be avaiable.
        END_TIMER(t, getPassName());
        return false;
    }

    m_du = (DUMgr*)m_rg->getPassMgr()->queryPass(PASS_DU_MGR);
    LI<IRBB> const* li = m_cfg->getLoopInfo();
    clean();
    if (li == NULL) {
        END_TIMER(t, getPassName());
        return false;
    }

    xcom::BitSet tmp;
    xcom::Vector<UINT> map_md2defcount;
    UINT2IR map_md2defir;

    xcom::List<LI<IRBB> const*> worklst;
    while (li != NULL) {
        worklst.append_tail(li);
        li = LI_next(li);
    }

    while (worklst.get_elem_count() > 0) {
        LI<IRBB> const* x = worklst.remove_head();
        findBIV(x, tmp, map_md2defcount, map_md2defir);
        xcom::SList<IV*> const* bivlst = m_li2bivlst.get(LI_id(x));
        if (bivlst != NULL && bivlst->get_elem_count() > 0) {
            findDIV(x, *bivlst, tmp);
        }
        x = LI_inner_list(x);
        while (x != NULL) {
            worklst.append_tail(x);
            x = LI_next(x);
        }
    }
    if (g_is_dump_after_pass && g_dump_opt.isDumpIVR()) {
        dump();
    }
    END_TIMER(t, getPassName());
    return false;
}
//END IVR

} //namespace xoc
