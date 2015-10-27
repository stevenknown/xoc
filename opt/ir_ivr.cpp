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
#include "ir_ivr.h"

namespace xoc {

//
//START IR_IVR
//
bool IR_IVR::computeInitVal(IR const* ir, IV * iv)
{
    if (!ir->is_st() && !ir->is_ist()) {
        return false;
    }

    IR const* v = ST_rhs(ir);
    if (v->is_cvt()) {
        //You should have performed refineIR to optimize cvt.
        v = CVT_exp(v);
    }

    if (v->is_const() && v->is_int(m_dm)) {
        if (IV_initv_i(iv) == NULL) {
            IV_initv_i(iv) = (LONGLONG*)xmalloc(sizeof(LONGLONG));
        }
        *IV_initv_i(iv) = CONST_int_val(v);
        IV_initv_type(iv) = IV_INIT_VAL_IS_INT;
        return true;
    }

    MD const* md = v->get_exact_ref();
    if (md != NULL) {
        IV_initv_md(iv) = md;
        IV_initv_type(iv) = IV_INIT_VAL_IS_VAR;
        return true;
    }
    IV_initv_i(iv) = NULL;
    return false;
}


bool IR_IVR::findInitVal(IV * iv)
{
    IR const* def = IV_iv_def(iv);
    ASSERT0(def->is_stmt());
    DUSet const* defs = def->get_duset_c();
    ASSERT0(defs);
    IR const* domdef = m_du->findDomDef(IV_iv_occ(iv), def, defs, true);
    if (domdef == NULL) { return false; }

    MD const* md = domdef->get_exact_ref();
    if (md == NULL || md != IV_iv(iv)) {
        return false;
    }
    LI<IRBB> const* li = IV_li(iv);
    ASSERT0(li);
    IRBB * dbb = domdef->get_bb();
    if (dbb == LI_loop_head(li) || !li->is_inside_loop(BB_id(dbb))) {
        return computeInitVal(domdef, iv);
    }
    return false;
}


/* Find all iv.
'map_md2defcount': record the number of definitions to MD.
'map_md2defir': map MD to define stmt. */
void IR_IVR::findBIV(LI<IRBB> const* li, BitSet & tmp,
                      Vector<UINT> & map_md2defcount,
                      UINT2IR & map_md2defir)
{
    IRBB * head = LI_loop_head(li);
    UINT headi = BB_id(head);
    tmp.clean(); //tmp is used to record exact MD which be modified.
    map_md2defir.clean();
    map_md2defcount.clean();
    for (INT i = LI_bb_set(li)->get_first();
         i != -1; i = LI_bb_set(li)->get_next(i)) {
        //if ((UINT)i == headi) { continue; }
        IRBB * bb = m_cfg->get_bb(i);
        ASSERT0(bb && m_cfg->get_vertex(BB_id(bb)));
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            if (!ir->is_st() && !ir->is_ist() && !ir->is_calls_stmt()) {
                continue;
            }

            MD const* exact_md = m_du->get_must_def(ir);
            if (exact_md != NULL) {
                if (exact_md->is_exact()) {
                    tmp.bunion(MD_id(exact_md));
                    UINT c = map_md2defcount.get(MD_id(exact_md)) + 1;
                    map_md2defcount.set(MD_id(exact_md), c);
                    if (c == 1) {
                        map_md2defir.set(MD_id(exact_md), ir);
                    } else {
                        map_md2defir.setAlways(MD_id(exact_md), NULL);
                        tmp.diff(MD_id(exact_md));
                    }
                }
            }

            //May kill other definitions.
            MDSet const* maydef = m_du->get_may_def(ir);
            if (maydef == NULL) { continue; }

            for (INT i = tmp.get_first(); i != -1; i = tmp.get_next(i)) {
                MD const* md = m_md_sys->get_md(i);
                ASSERT0(md->is_exact());
                if (maydef->is_contain(md)) {
                    map_md2defcount.set(i, 0);
                    tmp.diff(i);
                }
            }
        } //end for
    }

    //Find biv.
    bool find = false;
    List<MD*> sdlst; //single def md lst.
    for (INT i = tmp.get_first(); i != -1; i = tmp.get_next(i)) {
        if (map_md2defcount.get(i) != 1) { continue; }
        IR * def = map_md2defir.get(i);
        ASSERT0(def);
        if (m_du->get_avail_in_reach_def(headi)->is_contain(IR_id(def))) {
            //MD i is biv.
            sdlst.append_head(m_md_sys->get_md(i));
            find = true;
        }
    }
    if (!find) { return; }

    for (MD * biv = sdlst.get_head(); biv != NULL; biv = sdlst.get_next()) {
        IR * def = map_md2defir.get(MD_id(biv));
        ASSERT0(def);
        if (!def->is_st() && !def->is_stpr() && !def->is_ist()) {
            continue;
        }

        //Find the self modify stmt: i = i + ...
        DUSet const* useset = def->get_duset_c();
        if (useset == NULL) { continue; }
        bool selfmod = false;
        DU_ITER di = NULL;
        for (INT i = useset->get_first(&di);
             i >= 0; i = useset->get_next(i, &di)) {
            IR const* use = m_ru->get_ir(i);
            ASSERT0(use->is_exp());

            IR const* use_stmt = use->get_stmt();
            ASSERT0(use_stmt && use_stmt->is_stmt());

            if (use_stmt == def) {
                selfmod = true;
                break;
            }
        }
        if (!selfmod) { continue; }

        IR * stv = ST_rhs(def);
        if (!stv->is_add() && !stv->is_sub()) { continue; }

        //Make sure self modify stmt is monotonic.
        IR * op0 = BIN_opnd0(stv);
        IR * op1 = BIN_opnd1(stv);
        if (!op1->is_int(m_dm)) { continue; }

        MD const* op0md = op0->get_exact_ref();
        if (op0md == NULL || op0md != biv) { continue; }

        //Work out IV.
        IV * x = newIV();
        IV_iv(x) = biv;
        IV_li(x) = li;
        IV_iv_occ(x) = op0;
        IV_iv_def(x) = def;
        IV_step(x) = CONST_int_val(op1);
        if (stv->is_add()) {
            IV_is_inc(x) = true;
        } else {
            IV_is_inc(x) = false;
        }
        findInitVal(x);
        SList<IV*> * ivlst = m_li2bivlst.get(LI_id(li));
        if (ivlst == NULL) {
            ivlst = (SList<IV*>*)xmalloc(sizeof(SList<IV*>));
            ivlst->set_pool(m_sc_pool);
            m_li2bivlst.set(LI_id(li), ivlst);
        }
        ivlst->append_head(x);
    }
}


//Return true if 'ir' is loop invariant expression.
//'defs': def set of 'ir'.
bool IR_IVR::is_loop_invariant(LI<IRBB> const* li, IR const* ir)
{
    ASSERT0(ir->is_exp());
    DUSet const* defs = ir->get_duset_c();
    if (defs == NULL) { return true; }
    DU_ITER di = NULL;
    for (INT i = defs->get_first(&di); i >= 0; i = defs->get_next(i, &di)) {
        IR const* d = m_ru->get_ir(i);
        ASSERT0(d->is_stmt() && d->get_bb());
        if (li->is_inside_loop(BB_id(d->get_bb()))) {
            return false;
        }
    }
    return true;
}


//Return true if ir can be regard as induction expression.
//'defs': def list of 'ir'.
bool IR_IVR::scanExp(IR const* ir, LI<IRBB> const* li, BitSet const& ivmds)
{
    ASSERT0(ir->is_exp());
    switch (IR_code(ir)) {
    case IR_CONST:
    case IR_LDA:
        return true;
    case IR_LD:
    case IR_ILD:
    case IR_ARRAY:
    case IR_PR:
        {
            MD const* irmd = ir->get_exact_ref();
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


//Try to add iv expresion into div list if 'ive' do not exist in the list.
void IR_IVR::addDIVList(LI<IRBB> const* li, IR const* e)
{
    SList<IR const*> * divlst = m_li2divlst.get(LI_id(li));
    if (divlst == NULL) {
        divlst = (SList<IR const*>*)xmalloc(sizeof(SList<IR const*>));
        divlst->set_pool(m_sc_pool);
        m_li2divlst.set(LI_id(li), divlst);
    }

    bool find = false;
    for (SC<IR const*> * sc = divlst->get_head();
         sc != divlst->end(); sc = divlst->get_next(sc)) {
        IR const* ive = sc->val();
        ASSERT0(ive);
        if (ive->is_ir_equal(e, true)) {
            find = true;
            break;
        }
    }
    if (find) { return; }
    divlst->append_head(e);
}


void IR_IVR::findDIV(IN LI<IRBB> const* li, IN SList<IV*> const& bivlst,
                      BitSet & tmp)
{
    if (bivlst.get_elem_count() == 0) { return; }

    tmp.clean();
    for (SC<IV*> * sc = bivlst.get_head();
         sc != bivlst.end(); sc = bivlst.get_next(sc)) {
        IV * iv = sc->val();
        ASSERT0(iv);
        tmp.bunion(MD_id(IV_iv(iv)));
    }

    for (INT i = LI_bb_set(li)->get_first();
         i != -1; i = LI_bb_set(li)->get_next(i)) {
        IRBB * bb = m_cfg->get_bb(i);
        ASSERT0(bb && m_cfg->get_vertex(BB_id(bb)));
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            switch (IR_code(ir)) {
            case IR_ST:
            case IR_STPR:
            case IR_IST:
                {
                    IR * rhs = ir->get_rhs();
                    if (scanExp(rhs, li, tmp)) {
                        addDIVList(li, rhs);
                    }
                }
                break;
            case IR_CALL:
            case IR_ICALL:
                for (IR * p = CALL_param_list(ir);
                     p != NULL; p = IR_next(p)) {
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


void IR_IVR::_dump(LI<IRBB> * li, UINT indent)
{
    while (li != NULL) {
        fprintf(g_tfile, "\n");
        for (UINT i = 0; i < indent; i++) { fprintf(g_tfile, " "); }
        fprintf(g_tfile, "LI%d:BB%d", LI_id(li), BB_id(LI_loop_head(li)));
        fprintf(g_tfile, ",BODY:");
        for (INT i = LI_bb_set(li)->get_first();
             i != -1; i = LI_bb_set(li)->get_next(i)) {
            fprintf(g_tfile, "%d,", i);
        }

        SList<IV*> * bivlst = m_li2bivlst.get(LI_id(li));
        if (bivlst != NULL) {
            for (SC<IV*> * sc = bivlst->get_head();
                 sc != bivlst->end(); sc = bivlst->get_next(sc)) {
                IV * iv = sc->val();
                ASSERT0(iv);

                fprintf(g_tfile, "\n");
                for (UINT i = 0; i < indent; i++) { fprintf(g_tfile, " "); }
                fprintf(g_tfile, "BIV(md%d", MD_id(IV_iv(iv)));
                if (IV_is_inc(iv)) {
                    fprintf(g_tfile, ",step=%lld", IV_step(iv));
                } else {
                    fprintf(g_tfile, ",step=-%lld", IV_step(iv));
                }
                if (IV_initv_i(iv) != NULL) {
                    if (IV_initv_type(iv) == IV_INIT_VAL_IS_INT) {
                        fprintf(g_tfile, ",init=%lld", *IV_initv_i(iv));
                    } else {
                        ASSERT0(IV_initv_type(iv) == IV_INIT_VAL_IS_VAR);
                        fprintf(g_tfile, ",init=md%d", MD_id(IV_initv_md(iv)));
                    }
                }
                fprintf(g_tfile, ")");
            }
        }

        SList<IR const*> * divlst = m_li2divlst.get(LI_id(li));
        if (divlst != NULL) {
            if (divlst->get_elem_count() > 0) {
                fprintf(g_tfile, "\n");
                for (UINT i = 0; i < indent; i++) { fprintf(g_tfile, " "); }
            }
            for (SC<IR const*> * sc = divlst->get_head();
                 sc != divlst->end(); sc = divlst->get_next(sc)) {
                IR const* iv = sc->val();
                ASSERT0(iv);
                fprintf(g_tfile, "\n");
                g_indent = indent;
                dump_ir(iv, m_dm);
            }
        }

        _dump(LI_inner_list(li), indent + 2);
        li = LI_next(li);
        fflush(g_tfile);
    }
}


//Dump IVR info for loop.
void IR_IVR::dump()
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==---- DUMP IVR -- ru:'%s' ----==", m_ru->get_ru_name());
    _dump(m_cfg->get_loop_info(), 0);
    fflush(g_tfile);
}


void IR_IVR::clean()
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

}


bool IR_IVR::perform(OptCTX & oc)
{
    START_TIMER_AFTER();
    UINT n = m_ru->get_bb_list()->get_elem_count();
    if (n == 0) { return false; }

    m_ru->checkValidAndRecompute(&oc, PASS_AVAIL_REACH_DEF, PASS_DU_REF,
                                    PASS_DOM, PASS_LOOP_INFO, PASS_DU_CHAIN,
                                    PASS_RPO, PASS_UNDEF);

    LI<IRBB> const* li = m_cfg->get_loop_info();
    clean();
    if (li == NULL) { return false; }

    BitSet tmp;
    Vector<UINT> map_md2defcount;
    UINT2IR map_md2defir;

    List<LI<IRBB> const*> worklst;
    while (li != NULL) {
        worklst.append_tail(li);
        li = LI_next(li);
    }

    while (worklst.get_elem_count() > 0) {
        LI<IRBB> const* x = worklst.remove_head();
        findBIV(x, tmp, map_md2defcount, map_md2defir);
        SList<IV*> const* bivlst = m_li2bivlst.get(LI_id(x));
        if (bivlst != NULL && bivlst->get_elem_count() > 0) {
            findDIV(x, *bivlst, tmp);
        }
        x = LI_inner_list(x);
        while (x != NULL) {
            worklst.append_tail(x);
            x = LI_next(x);
        }
    }

    //dump();
    END_TIMER_AFTER(get_pass_name());
    return false;
}
//END IR_IVR

} //namespace xoc
