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
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"
#include "ir_gvn.h"

namespace xoc {

//
//START IR_GVN
//
typedef Vector<Vector<Vector<Vector<VN*>*>*>*> VEC4;
typedef Vector<Vector<Vector<VN*>*>*> VEC3;
typedef Vector<Vector<VN*>*> VEC2;
typedef Vector<VN*> VEC1;

IR_GVN::IR_GVN(Region * rg)
{
    ASSERT0(rg != NULL);
    m_ru = rg;
    m_md_sys = m_ru->getMDSystem();
    m_du = m_ru->getDUMgr();
    m_tm = m_ru->getTypeMgr();
    m_cfg = m_ru->getCFG();
    ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
    m_vn_count = 1;
    m_is_vn_fp = false;
    m_is_valid = false;
    m_is_comp_ild_vn_by_du = true;
    m_is_comp_lda_string = false;
    m_zero_vn = NULL;
    m_mc_zero_vn = NULL;

    List<IRBB*> * bbl = rg->getBBList();
    UINT n = 0;
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        n += bb->getNumOfIR();
    }
    m_stmt2domdef.init(MAX(4, getNearestPowerOf2(n/2)));
    m_pool = smpoolCreate(sizeof(VN) * 4, MEM_COMM);
}


IR_GVN::~IR_GVN()
{
    for (Vector<VN*> * v = m_vec_lst.get_head();
         v != NULL; v = m_vec_lst.get_next()) {
        delete v;
    }
    smpoolDelete(m_pool);
}


void IR_GVN::clean()
{
    m_vn_count = 1;
    if (m_md2vn.get_bucket_size() != 0) {
        m_md2vn.clean();
    }
    if (m_ll2vn.get_bucket_size() != 0) {
        m_ll2vn.clean();
    }
    if (m_fp2vn.get_bucket_size() != 0) {
        m_fp2vn.clean();
    }
    if (m_str2vn.get_bucket_size() != 0) {
        m_str2vn.clean();
    }
    xcom::BitSet inlst;
    for (INT k = 0; k <= m_ir2vn.get_last_idx(); k++) {
        VN * x = m_ir2vn.get(k);
        if (x != NULL && !inlst.is_contain(VN_id(x))) {
            inlst.bunion(VN_id(x));
            m_free_lst.append_tail(x);
        }
    }
    m_ir2vn.clean();
    for (Vector<VN*> * v = m_vnvec_lst.get_head();
         v != NULL; v = m_vnvec_lst.get_next()) {
        v->clean();
    }

    m_def2ildtab.clean();
    m_def2arrtab.clean();
    m_def2sctab.clean();
    m_stmt2domdef.clean();
    m_zero_vn = NULL;
    m_mc_zero_vn = NULL;
}


bool IR_GVN::verify()
{
    for (INT i = 0; i <= m_irt_vec.get_last_idx(); i++) {
        if (isBinaryOp((IR_TYPE)i) || i == IR_LDA) {
            Vector<Vector<VN*>*> * v0_vec = m_irt_vec.get(i);
            if (v0_vec == NULL) { continue; }
            for (INT j = 0; j <= v0_vec->get_last_idx(); j++) {
                Vector<VN*> * v1_vec = v0_vec->get(j);
                if (v1_vec == NULL) { continue; }
                //v1_vec->clean();
            }
        } else if (i == IR_BNOT || i == IR_LNOT ||
                   i == IR_NEG || i == IR_CVT) {
            Vector<VN*> * v0_vec = (Vector<VN*>*)m_irt_vec.get(i);
            if (v0_vec == NULL) { continue; }
            //v0_vec->clean();
        } else if (is_triple((IR_TYPE)i)) {
            VEC3 * v0_vec = (VEC3*)m_irt_vec.get(i);
            if (v0_vec == NULL) { continue; }
            for (INT j = 0; j <= v0_vec->get_last_idx(); j++) {
                VEC2 * v1_vec = v0_vec->get(j);
                if (v1_vec == NULL) { continue; }
                for (INT k = 0; k <= v1_vec->get_last_idx(); k++) {
                    if (v1_vec == NULL) { continue; }

                    //VEC1 * v2_vec = v1_vec->get(k);
                    //v2_vec->clean();
                }
            }
        } else if (is_quad((IR_TYPE)i)) {
            //
        } else {
            ASSERT0(m_irt_vec.get(i) == NULL);
        }
    }
    return true;
}


VN * IR_GVN::registerVNviaMD(MD const* md)
{
    if (m_md2vn.get_bucket_size() == 0) {
        m_md2vn.init(10); //to be evaluated
    }
    VN * x = m_md2vn.get(md);
    if (x == NULL) {
        x = newVN();
        VN_type(x) = VN_VAR;
        m_md2vn.set(md, x);
    }
    return x;
}


VN * IR_GVN::registerVNviaINT(LONGLONG v)
{
    if (v == 0) {
        if (m_zero_vn == NULL) {
            m_zero_vn = newVN();
            VN_type(m_zero_vn) = VN_INT;
            VN_int_val(m_zero_vn) = 0;
        }
        return m_zero_vn;
    }

    if (m_ll2vn.get_bucket_size() == 0) {
        m_ll2vn.init(10/*TO reevaluate*/);
    }

    VN * vn = m_ll2vn.get(v);
    if (vn != NULL) {
        return vn;
    }

    vn = newVN();
    VN_type(vn) = VN_INT;
    VN_int_val(vn) = v;
    m_ll2vn.set(v, vn);
    return vn;
}


VN * IR_GVN::registerVNviaMC(LONGLONG v)
{
    if (v == 0) {
        if (m_mc_zero_vn == NULL) {
            m_mc_zero_vn = newVN();
            VN_type(m_mc_zero_vn) = VN_MC_INT;
            VN_int_val(m_mc_zero_vn) = 0;
        }
        return m_mc_zero_vn;
    }

    if (m_llmc2vn.get_bucket_size() == 0) {
        m_llmc2vn.init(10/*TO reevaluate*/);
    }

    VN * vn = m_llmc2vn.get(v);
    if (vn != NULL) {
        return vn;
    }

    vn = newVN();
    VN_type(vn) = VN_MC_INT;
    VN_int_val(vn) = v;
    m_llmc2vn.set(v, vn);
    return vn;
}


VN * IR_GVN::registerVNviaSTR(SYM const* v)
{
    if (m_str2vn.get_bucket_size() == 0) {
        m_str2vn.init(16/*TO reevaluate*/);
    }

    VN * vn = m_str2vn.get(v);
    if (vn != NULL) {
        return vn;
    }
    vn = newVN();
    VN_type(vn) = VN_STR;
    VN_str_val(vn) = v;
    m_str2vn.set(v, vn);
    return vn;
}


VN * IR_GVN::registerVNviaFP(double v)
{
    if (m_fp2vn.get_bucket_size() == 0) {
        m_fp2vn.init(10/*TO reevaluate*/);
    }
    VN * vn = m_fp2vn.get(v);
    if (vn != NULL) {
        return vn;
    }
    vn = newVN();
    VN_type(vn) = VN_FP;
    VN_fp_val(vn) = v;
    m_fp2vn.set(v, vn);
    return vn;
}


VN * IR_GVN::registerUnaVN(IR_TYPE irt, VN const* v0)
{
    Vector<VN*> * v1_vec = (Vector<VN*>*)m_irt_vec.get(irt);
    if (v1_vec == NULL) {
        v1_vec = new Vector<VN*>();
        m_vec_lst.append_tail(v1_vec);
        m_vnvec_lst.append_tail(v1_vec);
        m_irt_vec.set(irt, (Vector<Vector<VN*>*>*)v1_vec);
    }
    VN * res = v1_vec->get(VN_id(v0));
    if (res == NULL) {
        res = newVN();
        VN_type(res) = VN_OP;
        VN_op(res) = irt;
        v1_vec->set(VN_id(v0), res);
    }
    return res;
}


VN * IR_GVN::registerBinVN(IR_TYPE irt, VN const* v0, VN const* v1)
{
    ASSERT0(v0 && v1);
    if (is_commutative(irt) && (VN_id(v0) > VN_id(v1))) {
        return registerBinVN(irt, v1, v0);
    } else if (irt == IR_GT) {
        return registerBinVN(IR_LT, v1, v0);
    } else if (irt == IR_GE) {
        return registerBinVN(IR_LE, v1, v0);
    }
    VEC2 * v0_vec = m_irt_vec.get(irt);
    if (v0_vec == NULL) {
        v0_vec = new VEC2();
        m_vec_lst.append_tail((Vector<VN*>*)v0_vec);
        m_irt_vec.set(irt, v0_vec);
    }

    VEC1 * v1_vec = v0_vec->get(VN_id(v0));
    if (v1_vec == NULL) {
        v1_vec = new VEC1();
        m_vec_lst.append_tail((Vector<VN*>*)v1_vec);
        m_vnvec_lst.append_tail(v1_vec);
        v0_vec->set(VN_id(v0), v1_vec);
    }

    VN * res = v1_vec->get(VN_id(v1));
    if (res == NULL) {
        res = newVN();
        VN_type(res) = VN_OP;
        VN_op(res) = irt;
        v1_vec->set(VN_id(v1), res);
    }
    return res;
}


VN * IR_GVN::registerTripleVN(
        IR_TYPE irt,
        VN const* v0,
        VN const* v1,
        VN const* v2)
{
    ASSERT0(v0 && v1 && v2);
    ASSERT0(is_triple(irt));
    VEC3 * v0_vec = (VEC3*)m_irt_vec.get(irt);
    if (v0_vec == NULL) {
        v0_vec = new VEC3();
        m_vec_lst.append_tail((VEC1*)v0_vec);
        m_irt_vec.set(irt, (VEC2*)v0_vec);
    }

    VEC2 * v1_vec = v0_vec->get(VN_id(v0));
    if (v1_vec == NULL) {
        v1_vec = new VEC2();
        m_vec_lst.append_tail((VEC1*)v1_vec);
        v0_vec->set(VN_id(v0), v1_vec);
    }

    VEC1 * v2_vec = v1_vec->get(VN_id(v1));
    if (v2_vec == NULL) {
        v2_vec = new VEC1();
        m_vec_lst.append_tail((VEC1*)v2_vec);
        m_vnvec_lst.append_tail(v2_vec);
        v1_vec->set(VN_id(v1), v2_vec);
    }

    VN * res = v2_vec->get(VN_id(v2));
    if (res == NULL) {
        res = newVN();
        VN_type(res) = VN_OP;
        VN_op(res) = irt;
        v2_vec->set(VN_id(v2), res);
    }
    return res;
}


VN * IR_GVN::registerQuadVN(
        IR_TYPE irt,
        VN const* v0,
        VN const* v1,
        VN const* v2,
        VN const* v3)
{
    ASSERT0(v0 && v1 && v2 && v3);
    ASSERT0(is_quad(irt));
    VEC4 * v0_vec = (VEC4*)m_irt_vec.get(irt);
    if (v0_vec == NULL) {
        v0_vec = new VEC4();
        m_vec_lst.append_tail((VEC1*)v0_vec);
        m_irt_vec.set(irt, (VEC2*)v0_vec);
    }

    VEC3 * v1_vec = v0_vec->get(VN_id(v0));
    if (v1_vec == NULL) {
        v1_vec = new VEC3();
        m_vec_lst.append_tail((VEC1*)v1_vec);
        v0_vec->set(VN_id(v0), v1_vec);
    }

    VEC2 * v2_vec = v1_vec->get(VN_id(v1));
    if (v2_vec == NULL) {
        v2_vec = new VEC2();
        m_vec_lst.append_tail((VEC1*)v2_vec);
        v1_vec->set(VN_id(v1), v2_vec);
    }

    VEC1 * v3_vec = v2_vec->get(VN_id(v2));
    if (v3_vec == NULL) {
        v3_vec = new VEC1();
        m_vec_lst.append_tail(v3_vec);
        m_vnvec_lst.append_tail(v3_vec);
        v2_vec->set(VN_id(v2), v3_vec);
    }

    VN * res = v3_vec->get(VN_id(v3));
    if (res == NULL) {
        res = newVN();
        VN_type(res) = VN_OP;
        VN_op(res) = irt;
        v3_vec->set(VN_id(v3), res);
    }
    return res;
}



//Memory location may be parameter or global variable.
//'emd': exact md
VN * IR_GVN::allocLiveinVN(IR const* exp, MD const* emd, bool & change)
{
    VN * x = m_ir2vn.get(IR_id(exp));
    if (x == NULL) {
        x = registerVNviaMD(emd);
        change = true;
        m_ir2vn.set(IR_id(exp), x);
    }
    return x;
}


//Only compute memory operation's vn.
VN * IR_GVN::computePR(IR const* exp, bool & change)
{
    SSAInfo * ssainfo = PR_ssainfo(exp);
    ASSERT0(exp->isReadPR() && ssainfo);

    IR const* def = ssainfo->get_def();
    if (def == NULL) {
        ASSERT0(exp->getRefMD());
        return allocLiveinVN(exp, exp->getRefMD(), change);
    }

    VN * defvn = m_ir2vn.get(def->id());

    VN * ux = m_ir2vn.get(IR_id(exp));
    if (defvn != ux) {
        m_ir2vn.set(IR_id(exp), defvn);
        change = true;
    }

    return defvn;
}


//Only compute memory operation's vn.
VN * IR_GVN::computeExactMemory(IR const* exp, bool & change)
{
    ASSERT0(exp->isMemoryOpnd());
    MD const* emd = exp->getExactRef();
    if (emd == NULL) { return NULL; }

    IR const* ed = m_du->getExactAndUniqueDef(exp);
    if (ed != NULL) {
        VN * defvn = NULL;
        IR const* exp_stmt = exp->getStmt();
        ASSERT0(exp_stmt->is_stmt());
        IRBB * b1 = ed->getBB();
        IRBB * b2 = exp_stmt->getBB();
        ASSERT0(b1 && b2);
        if ((b1 != b2 && m_cfg->is_dom(BB_id(b1), BB_id(b2))) ||
            (b1 == b2 && b1->is_dom(ed, exp_stmt, true))) {
            defvn = m_ir2vn.get(IR_id(ed));
        }

        VN * ux = m_ir2vn.get(IR_id(exp));
        if (defvn != ux) {
            m_ir2vn.set(IR_id(exp), defvn);
            change = true;
        }
        return defvn;
    }

    DUSet const* defset = exp->readDUSet();
    if (defset == NULL) {
        return allocLiveinVN(exp, emd, change);
    } else {
        //Check if some may-def or overlapped-def disrupts the emd.
        //Omit the DEF which has effect md and it does not overlapped
        //with emd.
        DUIter di = NULL;
        UINT defcount = 0;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di), defcount++) {
            IR const* dir = m_ru->getIR(i);
            ASSERT0(dir->is_stmt());
            MD const* xd = m_du->get_must_def(dir);
            if (xd == NULL) {
                MDSet const* xds = m_du->getMayDef(dir);
                if (xds != NULL && xds->is_contain(emd)) {
                    ASSERT0(m_ir2vn.get(IR_id(exp)) == NULL);
                    //exp's value is may defined, here we can not
                    //determine if exp have an individual VN.
                    return NULL;
                }
            } else {
                if (xd == emd || xd->is_overlap(emd)) {
                    ASSERT0(m_ir2vn.get(IR_id(exp)) == NULL);
                    //exp's value is defined by nonkilling define,
                    //here we can not determine if exp have an
                    //individual VN.
                    return NULL;
                }
            }
        }

        if (defcount == 0) {
            return allocLiveinVN(exp, emd, change);
        }
    }
    return allocLiveinVN(exp, emd, change);
}


//Compute VN for ild according to anonymous domdef.
VN * IR_GVN::computeIloadByAnonDomDef(
        IR const* ild,
        VN const* mlvn,
        IR const* domdef,
        bool & change)
{
    ASSERT0(ild->is_ild() && m_du->is_may_def(domdef, ild, false));
    ILD_VNE2VN * vnexp_map = m_def2ildtab.get(domdef);
    UINT dtsz = ild->getTypeSize(m_tm);
    VNE_ILD vexp(VN_id(mlvn), ILD_ofst(ild), dtsz);
    //NOTE:
    //    foo();
    //    ild(v1); //s1
    //    goo();
    //    ild(v1); //s2
    //    vn of s1 should not same as s2.
    if (vnexp_map == NULL) {
        vnexp_map = new ILD_VNE2VN(m_pool, 16); //bsize to be evaluate.
        m_def2ildtab.set(domdef, vnexp_map);
    }

    VN * ildvn = vnexp_map->get(&vexp);
    if (ildvn == NULL) {
        ildvn = newVN();
        VN_type(ildvn) = VN_OP;
        VN_op(ildvn) = IR_ILD;
        vnexp_map->setv((OBJTY)&vexp, ildvn);
    }

    m_ir2vn.set(IR_id(ild), ildvn);
    change = true;
    return ildvn;
}


VN * IR_GVN::computeIload(IR const* exp, bool & change)
{
    ASSERT0(exp->is_ild());
    VN * mlvn = computeVN(ILD_base(exp), change);
    if (mlvn == NULL) {
        ASSERT0(m_ir2vn.get(IR_id(exp)) == NULL);
        ASSERT0(m_ir2vn.get(IR_id(ILD_base(exp))) == NULL);
        return NULL;
    }

    VN * evn = m_ir2vn.get(IR_id(exp));
    if (evn != NULL) { return evn; }

    evn = computeExactMemory(exp, change);
    if (evn != NULL) { return evn; }

    DUSet const* defset = exp->readDUSet();
    if (defset == NULL || defset->get_elem_count() == 0) {
        VN * v = registerTripleVN(IR_ILD, mlvn,
                                  registerVNviaINT(ILD_ofst(exp)),
                                  registerVNviaINT(exp->getTypeSize(m_tm)));
        m_ir2vn.set(IR_id(exp), v);
        return v;
    }

    IR const* exp_stmt = const_cast<IR*>(exp)->getStmt();
    IR const* domdef = m_stmt2domdef.get(exp_stmt);
    if (domdef == NULL) {
        domdef = m_du->findDomDef(exp, exp_stmt, defset, false);
        if (domdef != NULL) {
            m_stmt2domdef.set(exp_stmt, domdef);
        }
    }
    if (domdef == NULL) { return NULL; }

    //ofst will be distinguished in computeIloadByAnonDomDef(), so
    //we do not need to differentiate the various offset of ild and ist.
    //if (domdef->is_ist() && IST_ofst(domdef) != ILD_ofst(exp)) {
    //    return NULL;
    //}

    if (!domdef->is_ist() ||
        domdef->is_starray() ||
        IST_ofst(domdef) != ILD_ofst(exp)) {
        return computeIloadByAnonDomDef(exp, mlvn, domdef, change);
    }

    //domdef is ist and the offset is matched.
    //Check if IR expression is match.
    VN const* mcvn = m_ir2vn.get(IR_id(IST_base(domdef)));
    if (mcvn == NULL || mcvn != mlvn) {
        return NULL;
    }
    VN * uni_vn = m_ir2vn.get(IR_id(domdef));
    if (uni_vn == NULL) {
        uni_vn = registerTripleVN(IR_ILD, mlvn,
                        registerVNviaINT(ILD_ofst(exp)),
                        registerVNviaINT(exp->getTypeSize(m_tm)));
        m_ir2vn.set(IR_id(domdef), uni_vn);
    }
    m_ir2vn.set(IR_id(exp), uni_vn);
    change = true;
    return uni_vn;
}


void IR_GVN::computeArrayAddrRef(IR const* ir, bool & change)
{
    ASSERT0(ir->is_starray());
    computeVN(ARR_base(ir), change);
    for (IR const* s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        computeVN(s, change);
    }
}


//Compute VN for array according to anonymous domdef.
VN * IR_GVN::computeArrayByAnonDomDef(
        IR const* arr,
        VN const* basevn,
        VN const* ofstvn,
        IR const* domdef,
        bool & change)
{
    ASSERT0(arr->is_array() && m_du->is_may_def(domdef, arr, false));
    ARR_VNE2VN * vnexp_map = m_def2arrtab.get(domdef);
    UINT dtsz = arr->getTypeSize(m_tm);
    VNE_ARR vexp(VN_id(basevn), VN_id(ofstvn), ARR_ofst(arr), dtsz);
    //NOTE:
    // foo();
    // array(v1); //s1
    // goo();
    // array(v1); //s2
    // vn of s1 should not same as s2.
    if (vnexp_map == NULL) {
        vnexp_map = new ARR_VNE2VN(m_pool, 16); //bsize to be evaluate.
        m_def2arrtab.set(domdef, vnexp_map);
    }
    VN * vn = vnexp_map->get(&vexp);
    if (vn == NULL) {
        vn = newVN();
        VN_type(vn) = VN_OP;
        VN_op(vn) = IR_ARRAY;
        vnexp_map->setv((OBJTY)&vexp, vn);
    }
    m_ir2vn.set(IR_id(arr), vn);
    change = true;
    return vn;
}


VN * IR_GVN::computeArray(IR const* exp, bool & change)
{
    ASSERT0(exp->is_array());
    for (IR const* s = ARR_sub_list(exp); s != NULL; s = s->get_next()) {
        computeVN(s, change);
    }
    VN * evn = m_ir2vn.get(IR_id(exp));
    if (evn != NULL) { return evn; }

    evn = computeExactMemory(exp, change);
    if (evn != NULL) {
        return evn;
    }

    VN const* abase_vn = NULL;
    VN const* aofst_vn = NULL;
    if (((CArray*)exp)->getDimNum() == 1) {
        //only handle one dim array.
        abase_vn = computeVN(ARR_base(exp), change);
        if (abase_vn == NULL) {
            return NULL;
        }
        aofst_vn = m_ir2vn.get(IR_id(ARR_sub_list(exp)));
        if (aofst_vn == NULL) {
            return NULL;
        }
    } else {
        return NULL;
    }

    DUSet const* du = exp->readDUSet();
    if (du == NULL || du->get_elem_count() == 0) {
        //Array does not have any DEF.
        VN * x = registerQuadVN(IR_ARRAY, abase_vn, aofst_vn,
                                registerVNviaINT(ARR_ofst(exp)),
                                registerVNviaINT(exp->getTypeSize(m_tm)));
        if (m_ir2vn.get(IR_id(exp)) != x) {
            m_ir2vn.set(IR_id(exp), x);
            change = true;
        }
        return x;
    }

    IR const* exp_stmt = const_cast<IR*>(exp)->getStmt();
    ASSERT0(exp_stmt->is_stmt());
    IR const* domdef = m_du->findDomDef(exp, exp_stmt, du, false);
    if (domdef == NULL) {
        return NULL;
    }
    if (domdef->is_starray() && ARR_ofst(domdef) != ARR_ofst(exp)) {
        return NULL;
    }
    if (!domdef->is_starray()) {
        return computeArrayByAnonDomDef(exp, abase_vn,
                                        aofst_vn, domdef, change);
    }

    ASSERT0(domdef->is_starray());
    //Check if VN expression is match.
    IR const* narr = IST_base(domdef);
    ASSERTN(((CArray*)narr)->getDimNum() == 1, ("only handle one dim array."));

    VN const* b = m_ir2vn.get(IR_id(ARR_base(narr)));
    if (b == NULL || b != abase_vn) {
        return NULL;
    }
    VN const* o = m_ir2vn.get(IR_id(ARR_sub_list(narr)));
    if (o == NULL || o != aofst_vn) {
        return NULL;
    }

    VN * uni_vn = m_ir2vn.get(IR_id(domdef));
    if (uni_vn == NULL) {
        uni_vn = registerQuadVN(IR_ARRAY, abase_vn, aofst_vn,
                                registerVNviaINT(ARR_ofst(exp)),
                                registerVNviaINT(exp->getTypeSize(m_tm)));
        m_ir2vn.set(IR_id(domdef), uni_vn);
        m_ir2vn.set(IR_id(narr), uni_vn);
    }
    m_ir2vn.set(IR_id(exp), uni_vn);
    change = true;
    return uni_vn;
}


VN * IR_GVN::computeScalarByAnonDomDef(
        IR const* exp,
        IR const* domdef,
        bool & change)
{
    ASSERT0((exp->is_ld() || exp->is_pr()) &&
            m_du->is_may_def(domdef, exp, false));
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
    if (vnexp_map == NULL) {
        vnexp_map = new SCVNE2VN(m_pool, 16); //bsize to be evaluate.
        m_def2sctab.set(domdef, vnexp_map);
    }
    VN * vn = vnexp_map->get(&vexp);
    if (vn == NULL) {
        vn = newVN();
        VN_type(vn) = VN_VAR;
        vnexp_map->setv((OBJTY)&vexp, vn);
    }
    m_ir2vn.set(IR_id(exp), vn);
    change = true;
    return vn;
}


VN * IR_GVN::computeScalar(IR const* exp, bool & change)
{
    VN * evn = m_ir2vn.get(IR_id(exp));
    if (evn != NULL) { return evn; }

    if (exp->isReadPR() && PR_ssainfo(exp) != NULL) {
        return computePR(exp, change);
    }

    evn = computeExactMemory(exp, change);
    if (evn != NULL) { return evn; }

    if (exp->getExactRef() == NULL) {
        //Can not handle inexact MD.
        return NULL;
    }

    DUSet const* du = exp->readDUSet();
    ASSERTN(du, ("If exact MD DU is empty, should assigned region LiveIn VN"));
    ASSERT0(du->get_elem_count() > 0);

    IR const* exp_stmt = const_cast<IR*>(exp)->getStmt();
    IR const* domdef = m_du->findDomDef(exp, exp_stmt, du, false);

    if (domdef == NULL) { return NULL; }

    if (domdef->is_st() && ST_ofst(domdef) != exp->getOffset()) {
        return NULL;
    }

    if (!domdef->is_st() && !domdef->is_stpr()) {
        return computeScalarByAnonDomDef(exp, domdef, change);
    }

    switch (exp->getCode()) {
    case IR_LD:
        if (domdef->is_stpr() || (LD_idinfo(exp) != ST_idinfo(domdef))) {
            return NULL;
        }
        break;
    case IR_PR:
        if (domdef->is_st() || PR_no(exp) != STPR_no(domdef)) {
            return NULL;
        }
        break;
    default: ASSERTN(0, ("unsupport"));
    }

    VN * uni_vn = m_ir2vn.get(IR_id(domdef));
    if (uni_vn == NULL) {
        uni_vn = newVN();
        VN_type(uni_vn) = VN_VAR;
        m_ir2vn.set(IR_id(domdef), uni_vn);
    }

    m_ir2vn.set(IR_id(exp), uni_vn);
    change = true;
    return uni_vn;
}


VN * IR_GVN::computeVN(IR const* exp, bool & change)
{
    ASSERT0(exp);
    switch (exp->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND: //logical and &&
    case IR_LOR: //logical or ||
    case IR_BAND: //inclusive and &
    case IR_BOR: //inclusive or |
    case IR_XOR: //exclusive or
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ: //==
    case IR_NE: //!=
    case IR_ASR:
    case IR_LSR:
    case IR_LSL: {
        VN * vn1 = computeVN(BIN_opnd0(exp), change);
        VN * vn2 = computeVN(BIN_opnd1(exp), change);
        if (vn1 == NULL || vn2 == NULL) {
            if (m_ir2vn.get(IR_id(exp)) != NULL) {
                m_ir2vn.set(IR_id(exp), NULL);
                change = true;
            }
            return NULL;
        }
        VN * x = registerBinVN(exp->getCode(), vn1, vn2);
        if (m_ir2vn.get(IR_id(exp)) != x) {
            m_ir2vn.set(IR_id(exp), x);
            change = true;
        }
        return x;
    }
    case IR_BNOT: //bitwise not
    case IR_LNOT: //logical not
    case IR_NEG: { //negative
        VN * x = computeVN(UNA_opnd(exp), change);
        if (x == NULL) {
            if (m_ir2vn.get(IR_id(exp)) != NULL) {
                m_ir2vn.set(IR_id(exp), NULL);
                change = true;
            }
            return NULL;
        }
        x = registerUnaVN(exp->getCode(), x);
        if (m_ir2vn.get(IR_id(exp)) != x) {
            m_ir2vn.set(IR_id(exp), x);
            change = true;
        }
        return x;
    }
    case IR_CVT: { //type convertion
        VN * x = computeVN(CVT_exp(exp), change);
        if (x == NULL) {
            if (m_ir2vn.get(IR_id(exp)) != NULL) {
                m_ir2vn.set(IR_id(exp), NULL);
                change = true;
            }
            return NULL;
        }
        x = registerUnaVN(exp->getCode(), x);
        if (m_ir2vn.get(IR_id(exp)) != x) {
            m_ir2vn.set(IR_id(exp), x);
            change = true;
        }
        return x;
    }
    case IR_LDA: {
        VAR * v = LDA_idinfo(exp);
        VN * basevn = NULL;
        if (v->is_string()) {
            if (m_is_comp_lda_string) {
                MD const* emd = m_ru->genMDforVAR(v);
                ASSERTN(emd && emd->is_effect(),
                       ("string should have effect MD"));
                basevn = registerVNviaMD(emd);
            } else {
                basevn = NULL;
            }
        } else {
            MD const* emd = m_ru->genMDforVAR(v);
            ASSERTN(emd && emd->is_effect(), ("string should have effect MD"));
            basevn = registerVNviaMD(emd);
        }

        if (basevn == NULL) {
            if (m_ir2vn.get(IR_id(exp)) != NULL) {
                m_ir2vn.set(IR_id(exp), NULL);
                change = true;
            }
            return NULL;
        }

        VN * ofstvn = registerVNviaINT(LDA_ofst(exp));
        VN * x = registerBinVN(IR_LDA, basevn, ofstvn);
        if (m_ir2vn.get(IR_id(exp)) != x) {
            m_ir2vn.set(IR_id(exp), x);
            change = true;
        }
        return x;
    }
    //case IR_ID:
    case IR_LD:
    case IR_PR:
        return computeScalar(exp, change);
    case IR_ARRAY:
        return computeArray(exp, change);
    case IR_ILD:
        return computeIload(exp, change);
    case IR_CONST: {
        VN * x = m_ir2vn.get(IR_id(exp));
        if (x != NULL) { return x; }

        if (exp->is_int()) {
            x = registerVNviaINT(CONST_int_val(exp));
        } else if (exp->is_mc()) {
            x = registerVNviaMC(CONST_int_val(exp));
        } else if (exp->is_fp()) {
            if (!m_is_vn_fp) {
                return NULL;
            }
            x = registerVNviaFP(CONST_fp_val(exp));
        } else if (exp->is_str()) {
            x = registerVNviaSTR(CONST_str_val(exp));
        } else {
            ASSERTN(0, ("unsupport const type"));
        }

        ASSERT0(x);
        m_ir2vn.set(IR_id(exp), x);
        change = true;
        return x;
    }
    default: break;
    }
    return NULL;
}


void IR_GVN::processPhi(IR const* ir, bool & change)
{
    VN * phivn = NULL;

    IR const* p = PHI_opnd_list(ir);
    if (p != NULL) {
        phivn = computeVN(p, change);
        p = p->get_next();
    }

    for (; p != NULL; p = p->get_next()) {
        VN * opndvn = computeVN(p, change);
        if (phivn != NULL && phivn != opndvn) {
            phivn = NULL;
        }
    }

    if (m_ir2vn.get(ir->id()) != phivn) {
        ASSERT0(m_ir2vn.get(ir->id()) == NULL);
        m_ir2vn.set(ir->id(), phivn);
        change = true;
    }
}


void IR_GVN::processCall(IR const* ir, bool & change)
{
    for (IR const* p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
        computeVN(p, change);
    }

    VN * x = m_ir2vn.get(ir->id());
    if (x == NULL) {
        x = newVN();
        VN_type(x) = VN_VAR;
        change = true;
        m_ir2vn.set(ir->id(), x);
    }
    return;
}


void IR_GVN::processRegion(IR const* ir, bool & change)
{
    DUMMYUSE(change);
    DUMMYUSE(ir);
    UNREACHABLE(); //TODO
}


void IR_GVN::processBB(IRBB * bb, bool & change)
{
    xcom::C<IR*> * ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end();
         ct = BB_irlist(bb).get_next(ct)) {
        IR * ir = ct->val();
        ASSERT0(ir);

        switch (ir->getCode()) {
        case IR_ST:
            {
                VN * x = computeVN(ST_rhs(ir), change);
                if (x == NULL) { break; }

                //IST's vn may be set by its dominated use-stmt ILD.
                if (m_ir2vn.get(ir->id()) != x) {
                    ASSERT0(m_ir2vn.get(ir->id()) == NULL);
                    m_ir2vn.set(ir->id(), x);
                    change = true;
                }
                return;
            }
            break;
        case IR_STPR:
            {
                VN * x = computeVN(STPR_rhs(ir), change);
                if (x == NULL) { break; }

                if (m_ir2vn.get(ir->id()) != x) {
                    ASSERT0(m_ir2vn.get(ir->id()) == NULL);
                    m_ir2vn.set(ir->id(), x);
                    change = true;
                }
                return;
            }
            break;
        case IR_STARRAY:
            {
                computeArrayAddrRef(ir, change);

                VN * x = computeVN(STARR_rhs(ir), change);
                if (x == NULL) { break; }

                //IST's vn may be set by its dominated use-stmt ILD.
                if (m_ir2vn.get(ir->id()) != x) {
                    ASSERT0(m_ir2vn.get(ir->id()) == NULL);
                    m_ir2vn.set(ir->id(), x);
                    change = true;
                }
                return;
            }
            break;
        case IR_IST:
            {
                computeVN(IST_base(ir), change);

                VN * x = computeVN(ir->getRHS(), change);
                if (x == NULL) { return; }

                //IST's vn may be set by its dominated use-stmt ILD.
                if (m_ir2vn.get(ir->id()) != x) {
                    ASSERT0(m_ir2vn.get(ir->id()) == NULL);
                    m_ir2vn.set(ir->id(), x);
                    change = true;
                }
            }
            break;
        case IR_CALL:
        case IR_ICALL:
            processCall(ir, change);
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            computeVN(BR_det(ir), change);
            break;
        case IR_SWITCH:
            computeVN(SWITCH_vexp(ir), change);
            break;
        case IR_IGOTO:
            computeVN(IGOTO_vexp(ir), change);
            break;
        case IR_RETURN:
            if (RET_exp(ir) != NULL) {
                computeVN(RET_exp(ir), change);
            }
            break;
        case IR_REGION:
            processRegion(ir, change);
            break;
        case IR_PHI:
            processPhi(ir, change);
            break;
        case IR_GOTO: break;
        default: UNREACHABLE();
        }
    }
}


void IR_GVN::dump_ir2vn()
{
    if (g_tfile == NULL) { return; }
    for (INT k = 0; k <= m_ir2vn.get_last_idx(); k++) {
        VN * x = m_ir2vn.get(k);
        if (x != NULL) {
            fprintf(g_tfile,"\nIR%d : vn%d, %d", k, VN_id(x), (INT)VN_type(x));
        }
    }
    fflush(g_tfile);
}


void IR_GVN::dump_h1(IR const* k, VN * x)
{
    note("\n\t%s", IRTNAME(k->getCode()));
    if (k->is_pr()) {
        prt("%d", PR_no(k));
    }
    prt(" id:%d ", IR_id(k));
    if (x != NULL) {
        prt("vn%d", VN_id(x));
    } else {
        prt("--");
    }
}


void IR_GVN::dump_bb(UINT bbid)
{
    if (g_tfile == NULL) { return; }
    IRBB * bb = m_cfg->getBB(bbid);
    ASSERT0(bb);

    ConstIRIter ii;
    note("\n-- BB%d ", BB_id(bb));
    dumpBBLabel(bb->getLabelList(), g_tfile);
    note("\n");
    for (IR * ir = BB_first_ir(bb);
         ir != NULL; ir = BB_next_ir(bb)) {
        dumpIR(ir, m_ru->getTypeMgr());
        note("\n");
        VN * x = m_ir2vn.get(ir->id());
        if (x != NULL) {
            prt("vn%d", VN_id(x));
        }

        prt(" <- {");

        switch (ir->getCode()) {
        case IR_ST:
            ii.clean();
            for (IR const* k = iterInitC(ST_rhs(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_STPR:
            ii.clean();
            for (IR const* k = iterInitC(STPR_rhs(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_IST:
            ii.clean();
            for (IR const* k = iterInitC(IST_rhs(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }

            ii.clean();
            for (IR const* k = iterInitC(IST_base(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_CALL:
        case IR_ICALL:
            ii.clean();
            for (IR const* k = iterInitC(CALL_param_list(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            ii.clean();
            for (IR const* k = iterInitC(BR_det(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_SWITCH:
            ii.clean();
            for (IR const* k = iterInitC(SWITCH_vexp(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_IGOTO:
            ii.clean();
            for (IR const* k = iterInitC(IGOTO_vexp(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_RETURN:
            ii.clean();
            for (IR const* k = iterInitC(RET_exp(ir), ii);
                 k != NULL; k = iterNextC(ii)) {
                VN * x2 = m_ir2vn.get(IR_id(k));
                dump_h1(k, x2);
            }
            break;
        case IR_GOTO: break;
        case IR_REGION:
            UNREACHABLE(); //TODO
            break;
        default: UNREACHABLE();
        }
        prt(" }");
    }
    fflush(g_tfile);
}


void IR_GVN::dump()
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP GVN -- rg:'%s' ----==", m_ru->getRegionName());
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        dump_bb(BB_id(bb));
    }
    fflush(g_tfile);
}


//Return true if gvn is able to determine the result of 'ir'.
bool IR_GVN::calcCondMustVal(IR const* ir, bool & must_true, bool & must_false)
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    if (ir->is_lnot()) {
        VN const* v = m_ir2vn.get(IR_id(UNA_opnd(ir)));
        if (v == NULL) { return false; }

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

    VN const* v1 = m_ir2vn.get(IR_id(BIN_opnd0(ir)));
    VN const* v2 = m_ir2vn.get(IR_id(BIN_opnd1(ir)));
    if (v1 == NULL || v2 == NULL) { return false; }

    switch (ir->getCode()) {
    case IR_LAND:
    case IR_LOR:
        if (VN_type(v1) == VN_INT && VN_type(v2) == VN_INT) {
            if (VN_int_val(v1) && VN_int_val(v2)) {
                must_true = true;
                must_false = false;
            } else {
                must_true = false;
                must_false = true;
            }
            return true;
        }
        break;
    case IR_LT:
    case IR_GT:
        if (v1 == v2) {
            must_true = false;
            must_false = true;
            return true;
        }

        if (VN_type(v1) == VN_INT && VN_type(v2) == VN_INT) {
            if (ir->is_lt()) {
                if (VN_int_val(v1) < VN_int_val(v2)) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            } else if (ir->is_gt()) {
                if (VN_int_val(v1) > VN_int_val(v2)) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            }
        }
        break;
    case IR_LE:
    case IR_GE:
        if (v1 == v2) {
            must_true = true;
            must_false = false;
            return true;
        }

        if (VN_type(v1) == VN_INT && VN_type(v2) == VN_INT) {
            if (ir->is_le()) {
                if (VN_int_val(v1) <= VN_int_val(v2)) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            } else if (ir->is_ge()) {
                if (VN_int_val(v1) >= VN_int_val(v2)) {
                    must_true = true;
                    must_false = false;
                } else {
                    must_true = false;
                    must_false = true;
                }
                return true;
            }
        }
        break;
    case IR_NE:
        if (v1 != v2) {
            must_true = true;
            must_false = false;
            return true;
        } else {
            must_true = false;
            must_false = true;
            return true;
        }
        break;
    case IR_EQ:
        if (v1 == v2) {
            must_true = true;
            must_false = false;
            return true;
        } else {
            must_true = false;
            must_false = true;
            return true;
        }
        break;
    default: UNREACHABLE();
    }
    return false;
}


bool IR_GVN::reperform(OptCtx & oc)
{
    clean();
    return perform(oc);
}


//GVN try to assign a value numbers to expressions.
bool IR_GVN::perform(OptCtx & oc)
{
    BBList * bbl = m_ru->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }

    START_TIMER(t, getPassName());
    m_ru->checkValidAndRecompute(&oc, PASS_DU_CHAIN,
        PASS_DU_REF, PASS_RPO, PASS_DOM, PASS_UNDEF);

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }

    List<IRBB*> * tbbl = m_cfg->getBBListInRPO();
    ASSERT0(tbbl->get_elem_count() == bbl->get_elem_count());

    UINT count = 0;
    bool change = true;

    #ifdef DEBUG_GVN
    while (change && count < 10) {
        change = false;
    #endif
        for (IRBB * bb = tbbl->get_head();
             bb != NULL; bb = tbbl->get_next()) {
            processBB(bb, change);
        } //end for BB
        count++;
    #ifdef DEBUG_GVN
    } //end while
    ASSERT0(!change && count <= 2);
    #endif

    //dump();
    END_TIMER(t, getPassName());
    ASSERT0(verify());
    m_is_valid = true;
    return true;
}
//END IR_GVN

} //namespace xoc
