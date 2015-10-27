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
//START RegionMgr
//
RegionMgr::~RegionMgr()
{
    for (INT id = 0; id <= m_id2ru.get_last_idx(); id++) {
        Region * ru = m_id2ru.get(id);
        if (ru != NULL) { delete ru; }
    }

    if (m_md_sys != NULL) {
        delete m_md_sys;
        m_md_sys = NULL;
    }

    if (m_var_mgr != NULL) {
        delete m_var_mgr;
        m_var_mgr = NULL;
    }

    if (m_call_graph != NULL) {
        delete m_call_graph;
        m_call_graph = NULL;
    }
}


/* Map string variable to dedicated string md.
CASE: android/external/tagsoup/src/org/ccil/cowan/tagsoup/HTMLSchema.java
    This method allocates 3000+ string variable.
    Each string has been taken address.
    It will bloat may_point_to_set.
    So we need to regard all string variables as one if necessary. */
MD const* RegionMgr::getDedicateStrMD()
{
    if (m_is_regard_str_as_same_md) {
        //Regard all string variables as single one.
        if (m_str_md == NULL) {
            SYM * s  = addToSymbolTab("Dedicated_VAR_be_regarded_as_string");
            VAR * sv = get_var_mgr()->
                        registerStringVar("Dedicated_String_Var", s, 1);
            VAR_allocable(sv) = false;
            VAR_is_addr_taken(sv) = true;
            MD md;
            MD_base(&md) = sv;
            MD_ty(&md) = MD_UNBOUND;
            ASSERT0(MD_base(&md)->is_string());
            MD const* e = m_md_sys->registerMD(md);
            ASSERT0(MD_id(e) > 0);
            m_str_md = e;
        }
        return m_str_md;
    }
    return NULL;
}


//Register exact MD for each global variable.
void RegionMgr::registerGlobalMDS()
{
    //Only top region can do initialize MD for global variable.
    ASSERT0(m_var_mgr);
    ID2VAR * varvec = m_var_mgr->get_var_vec();
    for (INT i = 0; i <= varvec->get_last_idx(); i++) {
        VAR * v = varvec->get(i);
        if (v == NULL) { continue; }

        if (VAR_is_fake(v) ||
            VAR_is_local(v) ||
            VAR_is_func_unit(v) ||
            VAR_is_func_decl(v)) {
            continue;
        }
        ASSERT0(VAR_is_global(v) && VAR_allocable(v));
        if (v->is_string() && getDedicateStrMD() != NULL) {
            continue;
        }

        MD md;
        MD_base(&md) = v;
        MD_ofst(&md) = 0;
        MD_size(&md) = v->getByteSize(get_type_mgr());
        MD_ty(&md) = MD_EXACT;
        m_md_sys->registerMD(md);
    }
}

CallGraph * RegionMgr::allocCallGraph(UINT edgenum, UINT vexnum)
{
    return new CallGraph(edgenum, vexnum, this);
}


VarMgr * RegionMgr::allocVarMgr()
{
    return new VarMgr(this);
}


Region * RegionMgr::allocRegion(REGION_TYPE rt)
{
    return new Region(rt, this);
}


Region * RegionMgr::newRegion(REGION_TYPE rt)
{
    Region * ru = allocRegion(rt);
    UINT free_id = m_free_ru_id.remove_head();
    if (free_id == 0) {
        REGION_id(ru) = m_ru_count++;
    } else {
        REGION_id(ru) = free_id;
    }
    return ru;
}


//Record new region and delete it when RegionMgr destroy.
void RegionMgr::set_region(Region * ru)
{
    ASSERT(REGION_id(ru) > 0, ("should generate new region via newRegion()"));
    ASSERT0(m_id2ru.get(REGION_id(ru)) == NULL);
    m_id2ru.set(REGION_id(ru), ru);
}


void RegionMgr::deleteRegion(Region * ru)
{
    ASSERT0(ru);
    UINT id = REGION_id(ru);

    //User may not record the region.
    //ASSERT(get_region(id), ("Does not belong to current region mgr"));
    delete ru;

    if (id != 0) {
        m_id2ru.set(id, NULL);
        m_free_ru_id.append_head(id);
    }
}


//Process a RU_FUNC region.
void RegionMgr::processFuncRegion(IN Region * func)
{
    ASSERT0(func->is_function());
    if (func->get_ir_list() == NULL) { return; }
    func->process();
    tfree();
}


IPA * RegionMgr::allocIPA(Region * program)
{
    return new IPA(this, program);
}


//Scan call site and build call graph.
CallGraph * RegionMgr::initCallGraph(Region * top, bool scan_call)
{
    ASSERT0(m_call_graph == NULL);
    UINT vn = 0, en = 0;
    IR * irs = top->get_ir_list();
    while (irs != NULL) {
        if (irs->is_region()) {
            vn++;
            Region * ru = REGION_ru(irs);
            List<IR const*> * call_list = ru->get_call_list();
            if (scan_call) {
                ru->scanCallList(*call_list);
            }
            ASSERT0(call_list);
            if (call_list->get_elem_count() == 0) {
                irs = IR_next(irs);
                continue;
            }
            for (IR const* ir = call_list->get_head();
                 ir != NULL; ir = call_list->get_next()) {
                en++;
            }
        }
        irs = IR_next(irs);
    }

    vn = MAX(4, xcom::getNearestPowerOf2(vn));
    en = MAX(4, xcom::getNearestPowerOf2(en));
    m_call_graph = allocCallGraph(vn, en);
    m_call_graph->build(top);
    return m_call_graph;
}


//#define MEMLEAKTEST
#ifdef MEMLEAKTEST
static void test_ru(RegionMgr * rm)
{
    Region * ru = NULL;
    Region * program = rm->get_region(1);
    ASSERT0(program);
    for (IR * ir = program->get_ir_list(); ir != NULL; ir = IR_next(ir)) {
        if (ir->is_region()) {
            ru = REGION_ru(ir);
            break;
        }
    }

    ASSERT0(ru);

    INT i = 0;
    VAR * v = ru->get_ru_var();
    Region * x = new Region(RU_FUNC, rm);
    while (i < 10000) {
        x->init(RU_FUNC, rm);
        x->set_ru_var(v);
        IR * irs = ru->get_ir_list();
        x->set_ir_list(x->dupIRTreeList(irs));
        //g_do_ssa = true;
        x->process();
        //VarMgr * vm = x->get_var_mgr();
        //vm->dump();
        //MDSystem * ms = x->get_md_sys();
        //ms->dumpAllMD();
        tfree();
        x->destroy();
        i++;
    }
    return;
}
#endif


//Process top-level region unit.
//Top level region unit should be program unit.
void RegionMgr::processProgramRegion(Region * program)
{
    ASSERT0(program);
    registerGlobalMDS();
    ASSERT(program->is_program(), ("TODO: support more operation."));
    OptCTX oc;
    if (g_do_inline) {
        //Need to scan call-list.
        CallGraph * callg = initCallGraph(program, true);
        UNUSED(callg);
        //callg->dump_vcg(get_dt_mgr(), NULL);

        OC_is_callg_valid(oc) = true;

        Inliner inl(this, program);
        inl.perform(oc);
    }

    //Test mem leak.
    //test_ru(this);

    for (IR * ir = program->get_ir_list(); ir != NULL; ir = IR_next(ir)) {
        if (ir->is_region()) {
            processFuncRegion(REGION_ru(ir));
        } else {
            ASSERT(0, ("TODO"));
        }
    }

    if (g_do_ipa) {
        if (!OC_is_callg_valid(oc)) {
            //processFuncRegion has scanned and collected call-list.
            //Thus it does not need to scan call-list here.
            initCallGraph(program, false);
            OC_is_callg_valid(oc) = true;
        }

        IPA * ipa = allocIPA(program);
        ipa->perform(oc);
        delete ipa;
    }
}
//END RegionMgr

} //namespace xoc
