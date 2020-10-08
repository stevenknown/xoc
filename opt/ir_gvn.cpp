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
//START GVN
//
GVN::GVN(Region * rg)
{
    ASSERT0(rg != NULL);
    m_rg = rg;
    m_md_sys = m_rg->getMDSystem();
    m_du = m_rg->getDUMgr();
    m_tm = m_rg->getTypeMgr();
    m_cfg = m_rg->getCFG();
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
    m_stmt2domdef.init(MAX(4, xcom::getNearestPowerOf2(n/2)));
    m_pool = smpoolCreate(sizeof(VN) * 4, MEM_COMM);
}


GVN::~GVN()
{
    for (VEC1 * v = m_vec_lst.get_head();
         v != NULL; v = m_vec_lst.get_next()) {
        delete v;
    }
    smpoolDelete(m_pool);
}


void GVN::clean()
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
    for (VEC1 * v = m_vnvec_lst.get_head();
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


bool GVN::verify()
{
    for (INT i = 0; i <= m_irt_vec.get_last_idx(); i++) {
        if (isBinaryOp((IR_TYPE)i) || i == IR_LDA) {
            VEC2 * v0_vec = m_irt_vec.get(i);
            if (v0_vec == NULL) { continue; }
            for (INT j = 0; j <= v0_vec->get_last_idx(); j++) {
                VEC1 * v1_vec = v0_vec->get(j);
                if (v1_vec == NULL) { continue; }
                //v1_vec->clean();
            }
        } else if (i == IR_BNOT || i == IR_LNOT ||
                   i == IR_NEG || i == IR_CVT) {
            VEC1 * v0_vec = (VEC1*)m_irt_vec.get(i);
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


VN * GVN::registerVNviaMD(MD const* md)
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


VN * GVN::registerVNviaINT(LONGLONG v)
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
        m_ll2vn.init(10); //To be reevaluated
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


VN * GVN::registerVNviaMC(LONGLONG v)
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


VN * GVN::registerVNviaSTR(Sym const* v)
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


VN * GVN::registerVNviaFP(double v)
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


VN * GVN::registerUnaVN(IR_TYPE irt, VN const* v0)
{
    VEC1 * v1_vec = (VEC1*)m_irt_vec.get(irt);
    if (v1_vec == NULL) {
        v1_vec = new VEC1();
        m_vec_lst.append_tail(v1_vec);
        m_vnvec_lst.append_tail(v1_vec);
        m_irt_vec.set(irt, (VEC2*)v1_vec);
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


VN * GVN::registerBinVN(IR_TYPE irt, VN const* v0, VN const* v1)
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
        m_vec_lst.append_tail((VEC1*)v0_vec);
        m_irt_vec.set(irt, v0_vec);
    }

    VEC1 * v1_vec = v0_vec->get(VN_id(v0));
    if (v1_vec == NULL) {
        v1_vec = new VEC1();
        m_vec_lst.append_tail((VEC1*)v1_vec);
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


VN * GVN::registerTripleVN(IR_TYPE irt,
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


VN * GVN::registerQuadVN(IR_TYPE irt,
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
VN * GVN::allocLiveinVN(IR const* exp, MD const* emd, bool & change)
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
VN * GVN::computePR(IR const* exp, bool & change)
{
    SSAInfo * ssainfo = PR_ssainfo(exp);
    ASSERT0(exp->isReadPR() && ssainfo);

    IR const* def = ssainfo->getDef();
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
VN * GVN::computeExactMemory(IR const* exp, bool & change)
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
        if ((b1 != b2 && m_cfg->is_dom(b1->id(), b2->id())) ||
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
            IR const* dir = m_rg->getIR(i);
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
VN * GVN::computeILoadByAnonDomDef(IR const* ild,
                                      VN const* mlvn,
                                      IR const* domdef,
                                      bool & change)
{
    ASSERT0(ild->is_ild() && m_du->isMayDef(domdef, ild, false));
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


VN * GVN::computeILoad(IR const* exp, bool & change)
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

    //ofst will be distinguished in computeILoadByAnonDomDef(), so
    //we do not need to differentiate the various offset of ild and ist.
    //if (domdef->is_ist() && IST_ofst(domdef) != ILD_ofst(exp)) {
    //    return NULL;
    //}

    if (!domdef->is_ist() ||
        domdef->is_starray() ||
        IST_ofst(domdef) != ILD_ofst(exp)) {
        return computeILoadByAnonDomDef(exp, mlvn, domdef, change);
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


void GVN::computeArrayAddrRef(IR const* ir, bool & change)
{
    ASSERT0(ir->is_starray());
    computeVN(ARR_base(ir), change);
    for (IR const* s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        computeVN(s, change);
    }
}


//Compute VN for array according to anonymous domdef.
VN * GVN::computeArrayByAnonDomDef(IR const* arr,
                                      VN const* basevn,
                                      VN const* ofstvn,
                                      IR const* domdef,
                                      bool & change)
{
    ASSERT0(arr->is_array() && m_du->isMayDef(domdef, arr, false));
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


VN * GVN::computeArray(IR const* exp, bool & change)
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


VN * GVN::computeScalarByAnonDomDef(IR const* exp,
                                       IR const* domdef,
                                       bool & change)
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


VN * GVN::computeScalar(IR const* exp, bool & change)
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


VN * GVN::computeVN(IR const* exp, bool & change)
{
    ASSERT0(exp);
    switch (exp->getCode()) {
    SWITCH_CASE_BIN: {
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
    SWITCH_CASE_UNA: {
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
    case IR_LDA: {
        Var * v = LDA_idinfo(exp);
        VN * basevn = NULL;
        if (v->is_string()) {
            if (m_is_comp_lda_string) {
                MD const* emd = m_rg->genMDforVAR(v);
                ASSERTN(emd && emd->is_effect(),
                       ("string should have effect MD"));
                basevn = registerVNviaMD(emd);
            } else {
                basevn = NULL;
            }
        } else {
            MD const* emd = m_rg->genMDforVAR(v);
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
    //Note IR_ID should not participate in GVN analysis because it does not
    //represent a real operation.
    case IR_LD:
    case IR_PR:
        return computeScalar(exp, change);
    case IR_ARRAY:
        return computeArray(exp, change);
    case IR_ILD:
        return computeILoad(exp, change);
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


void GVN::processPhi(IR const* ir, bool & change)
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


void GVN::processCall(IR const* ir, bool & change)
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


void GVN::processRegion(IR const* ir, bool & change)
{
    //Region effect should be simluated via call-stmt
    //which will be handled.
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
        IR const* irbase1 = const_cast<IR*>(ir1)->getBase();
        IR const* irbase2 = const_cast<IR*>(ir2)->getBase();
        ASSERT0(irbase1 && irbase2);
        VN const* base1 = mapIR2VN(irbase1);
        VN const* base2 = mapIR2VN(irbase2);
        return base1 == base2;
    }
    if (ir1->isArrayOp() && ir2->isArrayOp() && ir1->isSameArrayStruct(ir2)) {
        IR const* irbase1 = const_cast<IR*>(ir1)->getBase();
        IR const* irbase2 = const_cast<IR*>(ir2)->getBase();
        ASSERT0(irbase1 && irbase2);
        VN const* base1 = mapIR2VN(irbase1);
        VN const* base2 = mapIR2VN(irbase2);
        if (base1 != base2) {
            return false;
        }

        IR const* s2 = ARR_sub_list(ir2);
        for (IR const* s1 = ARR_sub_list(ir1); s1 != NULL;
             s1 = s1->get_next(), s2 = s2->get_next()) {
            ASSERT0(s2);    
            VN const* vs1 = mapIR2VN(s1);
            VN const* vs2 = mapIR2VN(s2);
            if (vs1 != vs2) {
                return false;
            }
        }
        return true;
    }

    MD const* must1 = ir1->getRefMD();
    MD const* must2 = ir2->getRefMD();
    return must1 == must2 && must1 != NULL;
}


void GVN::processBB(IRBB * bb, bool & change)
{
    IRListIter ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
        IR * ir = ct->val();
        ASSERT0(ir);
        switch (ir->getCode()) {
        case IR_ST:
        case IR_STPR: {
                VN * x = computeVN(ir->getRHS(), change);
                if (x == NULL) { break; }

                //ST's vn may be set by its dominated use-stmt ILD.
                if (m_ir2vn.get(ir->id()) != x) {
                    //ir2vn is NULL only the first iteration computation.
                    //ASSERT0(m_ir2vn.get(ir->id()) == NULL);
                    m_ir2vn.set(ir->id(), x);
                    change = true;
                }
                //return; keep going BB irlist.
            }
            break;
        case IR_STARRAY: {
                computeArrayAddrRef(ir, change);

                VN * x = computeVN(STARR_rhs(ir), change);
                if (x == NULL) { break; }

                //STARRAY's vn may be set by its dominated use-stmt ILD.
                if (m_ir2vn.get(ir->id()) != x) {
                    //ir2vn is NULL only the first iteration computation.
                    //ASSERT0(m_ir2vn.get(ir->id()) == NULL);
                    m_ir2vn.set(ir->id(), x);
                    change = true;
                }
                //return; keep going BB irlist.
            }
            break;
        case IR_IST: {
                computeVN(IST_base(ir), change);

                VN * x = computeVN(ir->getRHS(), change);
                if (x == NULL) { break; }

                //IST's vn may be set by its dominated use-stmt ILD.
                if (m_ir2vn.get(ir->id()) != x) {
                    //ir2vn is NULL only the first iteration computation.
                    //ASSERT0(m_ir2vn.get(ir->id()) == NULL);
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


void GVN::dumpIR2VN() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    for (INT k = 0; k <= m_ir2vn.get_last_idx(); k++) {
        VN * x = m_ir2vn.get(k);
        if (x != NULL) {
            note(getRegion(), "\nIR%d : vn%d, %d",
                 k, VN_id(x), (INT)VN_type(x));
        }
    }
}


void GVN::dump_h1(IR const* k, VN const* x) const
{
    ASSERT0(k);    
    if (k->is_pr()) {
        note(getRegion(), "\n\t$%d", PR_no(k));
    } else {
        note(getRegion(), "\n\t%s", IRTNAME(k->getCode()));
    }
    prt(getRegion(), " id:%d ", k->id());
    if (x != NULL) {
        prt(getRegion(), "vn%d", VN_id(x));
    } else {
        prt(getRegion(), "--");
    }
}


void GVN::dumpBB(UINT bbid) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    IRBB * bb = m_cfg->getBB(bbid);
    ASSERT0(bb);

    ConstIRIter ii;
    note(getRegion(), "\n-- BB%d ", bb->id());
    dumpBBLabel(bb->getLabelList(), getRegion());
    note(getRegion(), "\n");
    for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        dumpIR(ir, m_rg);
        note(getRegion(), "\n");
        VN * x = m_ir2vn.get(ir->id());
        if (x != NULL) {
            prt(getRegion(), "vn%d", VN_id(x));
        }

        prt(getRegion(), " <- {");
        ii.clean();
        bool dumped = false;
        for (IR const* k = iterRhsInitC(ir, ii);
             k != NULL; k = iterNextC(ii)) {
            dumped = true;
            VN const* vn = m_ir2vn.get(k->id());
            dump_h1(k, vn);
        }
        if (dumped) {
            note(getRegion(), "\n");
        }
        prt(getRegion(), " }");
    }
}


bool GVN::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        dumpBB(bb->id());
    }
    return true;
}


//Return true if GVN is able to determine the result of 'ir', otherwise
//return false that GVN know nothing about ir.
bool GVN::calcCondMustVal(IR const* ir,
                          bool & must_true,
                          bool & must_false) const
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


bool GVN::reperform(OptCtx & oc)
{
    clean();
    return perform(oc);
}


//GVN try to assign a value numbers to expressions.
bool GVN::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    m_prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR);
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
    m_rg->checkValidAndRecompute(&oc, PASS_RPO, PASS_DOM, PASS_UNDEF);
    List<IRBB*> * tbbl = m_cfg->getRPOBBList();
    ASSERT0(tbbl);
    ASSERT0(tbbl->get_elem_count() == bbl->get_elem_count());
    UINT count = 0;
    bool change = true;
    while (change && count < 100) {
        change = false;
        for (IRBB * bb = tbbl->get_head();
             bb != NULL; bb = tbbl->get_next()) {
            processBB(bb, change);
        }
        count++;
    }
    ASSERT0(!change);

    if (g_is_dump_after_pass && g_dump_opt.isDumpGVN()) {
        dump();
    }
    END_TIMER(t, getPassName());
    ASSERT0(verify());
    m_is_valid = true;
    return true;
}
//END GVN

} //namespace xoc
