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

//Record memory reference for region.
#define REGION_refinfo(r) ((r)->m_ref_info)

static void set_irt_size(IR * ir, UINT)
{
    #ifdef CONST_IRT_SZ
    IR_irt_size(ir) = irt_sz;
    #else
    DUMMYUSE(ir);
    #endif
}


static UINT getIRTypeSize(IR const* ir)
{
    #ifdef CONST_IRT_SZ
    return IR_irt_size(ir);
    #else
    return IRTSIZE(ir->getCode());
    #endif
}


//
//START AnalysisInstrument
//
AnalysisInstrument::AnalysisInstrument(Region * rg) :
    m_mds_mgr(rg, &m_sbs_mgr),
    m_mds_hash_allocator(&m_sbs_mgr),
    m_mds_hash(&m_mds_hash_allocator)
{
    m_rg = rg;
    m_call_list = nullptr;
    m_return_list = nullptr;
    m_ir_list = nullptr;
    m_pass_mgr = nullptr;

    //Counter of IR_PR, and do not use '0' as prno.
    m_pr_count = PRNO_UNDEF + 1;
    m_du_pool = smpoolCreate(sizeof(DU) * 4, MEM_CONST_SIZE);
    m_sc_labelinfo_pool = smpoolCreate(sizeof(xcom::SC<LabelInfo*>) * 4,
        MEM_CONST_SIZE);
    ::memset(m_free_tab, 0, sizeof(m_free_tab));
}


#ifdef _DEBUG_
static bool verifyVar(Region * rg, VarMgr * vm, Var * v)
{
    CHECK0_DUMMYUSE(v);
    CHECK0_DUMMYUSE(vm);
    if (rg->is_function() || rg->is_eh() ||
        rg->getRegionType() == REGION_INNER) {
        //If var is global but unallocable, it often be
        //used as placeholder or auxilary var.

        //For these kind of regions, there are only local variable or
        //unablable global variable is legal.
        ASSERT0(VAR_is_local(v) || VAR_is_unallocable(v));
    } else if (rg->is_program()) {
        //Theoretically, only global variable is legal in program region.
        //However even if the program region there may be local
        //variables, e.g: PR, a kind of local variable.
        //ASSERT0(VAR_is_global(v));
    } else {
        ASSERTN(0, ("unsupport variable type."));
    }
    return true;
}
#endif


//Free md's id and local-var's id back to MDSystem and VarMgr.
//The index of MD and Var is important resource if there
//are a lot of REGIONs in RegionMgr.
//Note this function does NOT process GLOBAL variable.
static void destroyVARandMD(Region * rg)
{
    VarMgr * varmgr = rg->getVarMgr();
    MDSystem * mdsys = rg->getMDSystem();
    VarTabIter c;
    ConstMDIter iter;
    VarTab * vartab = rg->getVarTab();
    ASSERT0(vartab);
    for (Var * v = vartab->get_first(c); v != nullptr; v = vartab->get_next(c)) {
        ASSERT0(verifyVar(rg, varmgr, v));
        mdsys->removeMDforVAR(v, iter);
        varmgr->destroyVar(v);
    }
}


AnalysisInstrument::~AnalysisInstrument()
{
    #ifdef DEBUG_SEG
    //Just dump the seg info if you really need to see.
    //DefSegMgr * segmgr = m_sbs_mgr.getSegMgr();
    //dumpSegMgr(segmgr, getRegion()->getLogMgr()->getFileHandler());
    #endif

    //Destroy pass manager.
    if (m_pass_mgr != nullptr) {
        delete m_pass_mgr;
        m_pass_mgr = nullptr;
    }

    //Free AIContainer's internal structure.
    //The vector of AIContainer must be destroyed explicitly.
    INT l = m_ir_vector.get_last_idx();
    for (INT i = 1; i <= l; i++) {
        IR * ir = m_ir_vector.get(i);
        ASSERT0(ir);
        //if (ir->is_region()) {
        //    //All region should be deleted by regionmgr.
        //    m_ru_mgr->deleteRegion(REGION_ru(ir));
        //}
        ir->freeDUset(m_sbs_mgr);
    }

    //Free local Var id and related MD id, and destroy the memory.
    destroyVARandMD(m_rg);

    //Destroy reference info.
    if (REGION_refinfo(m_rg) != nullptr) {
        REF_INFO_mayuse(REGION_refinfo(m_rg)).clean(m_sbs_mgr);
        REF_INFO_maydef(REGION_refinfo(m_rg)).clean(m_sbs_mgr);

        //REGION_refinfo allocated in pool.
        REGION_refinfo(m_rg) = nullptr;
    }

    //Destory CALL list.
    if (m_call_list != nullptr) {
        delete m_call_list;
        m_call_list = nullptr;
    }

    //Destory RETURN list.
    if (m_return_list != nullptr) {
        delete m_return_list;
        m_return_list = nullptr;
    }

    ////////////////////////////////////////////////////////////
    //Do NOT destroy member which allocated in pool after here//
    ////////////////////////////////////////////////////////////
    //Destroy all DUSet which allocated in the du_pool.
    smpoolDelete(m_du_pool);
    smpoolDelete(m_sc_labelinfo_pool);
    m_du_pool = nullptr;
    m_sc_labelinfo_pool = nullptr;
    m_ir_list = nullptr;
}


size_t AnalysisInstrument::count_mem() const
{
    size_t count = sizeof(*this);
    if (m_call_list != nullptr) {
        count += m_call_list->count_mem();
    }
    count += smpoolGetPoolSize(m_du_pool);
    count += smpoolGetPoolSize(m_sc_labelinfo_pool);
    count += m_ir_bb_mgr.count_mem();
    count += m_prno2var.count_mem();
    count += m_bs_mgr.count_mem();
    count += m_sbs_mgr.count_mem();
    count += m_mds_mgr.count_mem();
    count += m_mds_hash.count_mem();
    count += m_ir_vector.count_mem();
    count += m_ir_bb_list.count_mem();
    //m_free_du_list has been counted in m_du_pool.
    return count;
}
//END AnalysisInstrument


//
//START Region
//
void Region::init(REGION_TYPE rt, RegionMgr * rm)
{
    m_u2.s1b1 = 0;
    REGION_type(this) = rt;
    REGION_blackbox_data(this) = nullptr;
    m_var = nullptr;
    m_region_mgr = nullptr;
    REGION_id(this) = 0;
    REGION_parent(this) = nullptr;
    REGION_refinfo(this) = nullptr;
    REGION_analysis_instrument(this) = nullptr;
    if (is_program() || is_function() || is_eh() || is_inner()) {
        //All these Regions could involve ir stmt list.
        REGION_analysis_instrument(this) = new AnalysisInstrument(this);
    }
    REGION_region_mgr(this) = rm;
    m_pool = smpoolCreate(256, MEM_COMM);
    m_ru_var_tab.init();
}


void Region::destroy()
{
    if (!is_blackbox()) {
        destroyPassMgr();
        if (getAnalysisInstrument() != nullptr) {
            delete REGION_analysis_instrument(this);
        }
    }
    REGION_analysis_instrument(this) = nullptr;
    //MDSET destroied by MDSetMgr.
    REGION_refinfo(this) = nullptr;
    REGION_id(this) = 0;
    REGION_parent(this) = nullptr;
    REGION_type(this) = REGION_UNDEF;
    //Destroy all IR. IR allocated in the pool.
    smpoolDelete(m_pool);
    m_pool = nullptr;
    m_var = nullptr;
    m_ru_var_tab.destroy();
}


size_t Region::count_mem() const
{
    //Because analysis_instrument is pointer, sizeof(Region) does
    //not contain its class size.
    size_t count = sizeof(Region);
    if ((is_inner() || is_function() || is_eh() || is_program()) &&
        getAnalysisInstrument() != nullptr) {
        count += getAnalysisInstrument()->count_mem();
    }
    count += m_ru_var_tab.count_mem();
    count -= sizeof(m_ru_var_tab);
    if (m_ref_info != nullptr) {
        count += m_ref_info->count_mem();
    }
    ASSERT0(m_pool);
    count += smpoolGetPoolSize(m_pool);
    return count;
}


//Split list of ir into basic block.
//'irs': a list of ir.
//'bbl': a list of bb.
//'ctbb': marker current bb container.
//Note if CFG is invalid, it will not be updated.
BBListIter Region::splitIRlistIntoBB(IR * irs, BBList * bbl,
                                     BBListIter ctbb, OptCtx const& oc)
{
    bool cfg_is_valid = false;
    IRCFG * cfg = getCFG();
    if (cfg != nullptr && oc.is_cfg_valid()) {
        //Note if CFG is invalid, it will not be updated.
        cfg_is_valid = true;
    }

    IRBB * newbb = allocBB();
    if (cfg_is_valid) {
        cfg->addBB(newbb);
    }
    ctbb = bbl->insert_after(newbb, ctbb);
    while (irs != nullptr) {
        IR * ir = xcom::removehead(&irs);
        if (IRBB::isLowerBoundary(ir)) {
            BB_irlist(newbb).append_tail(ir);
            newbb = allocBB();
            if (cfg_is_valid) {
                cfg->addBB(newbb);
            }
            ctbb = bbl->insert_after(newbb, ctbb);
        } else if (IRBB::isUpperBoundary(ir)) {
            ASSERT0(ir->is_label());

            newbb = allocBB();
            if (cfg_is_valid) {
                cfg->addBB(newbb);
            }
            ctbb = bbl->insert_after(newbb, ctbb);

            //Regard label-info as add-on info that attached on newbb, and
            //'ir' will be dropped off.
            LabelInfo const* li = ir->getLabel();
            newbb->addLabel(li);
            if (cfg_is_valid) {
                cfg->addLabel(newbb, li);
            }
            if (!LABELINFO_is_try_start(li) && !LABELINFO_is_pragma(li)) {
                BB_is_target(newbb) = true;
            }
            freeIRTree(ir); //free label ir.
        } else {
            BB_irlist(newbb).append_tail(ir);
        }
    }
    return ctbb;
}


bool Region::evaluateConstInteger(IR const* ir, OUT ULONGLONG * const_value)
{
    switch (ir->getCode()) {
    case IR_CONST:
        if (!ir->is_int()) { return false; }
        *const_value = CONST_int_val(ir);
        return true;
    SWITCH_CASE_BIN: {
        IR const* opnd0 = BIN_opnd0(ir);
        IR const* opnd1 = BIN_opnd1(ir);

        //TODO: Handle the case if opnd0's type is different with opnd1.
        if (!opnd0->is_int() || !opnd1->is_int()) { return false; }
        if (opnd0->is_uint() ^ opnd1->is_uint()) { return false; }

        ULONGLONG lvalue = 0, rvalue = 0;
        if (!evaluateConstInteger(BIN_opnd0(ir), &lvalue)) { return false; }
        if (!evaluateConstInteger(BIN_opnd1(ir), &rvalue)) { return false; }

        if (opnd0->is_uint()) {
            switch (ir->getCode()) {
            case IR_ADD: *const_value = lvalue + rvalue; break;
            case IR_MUL: *const_value = lvalue * rvalue; break;
            case IR_SUB: *const_value = lvalue - rvalue; break;
            case IR_DIV: *const_value = lvalue / rvalue; break;
            case IR_REM: *const_value = lvalue % rvalue; break;
            case IR_MOD: *const_value = lvalue % rvalue; break;
            case IR_LAND: *const_value = lvalue && rvalue; break;
            case IR_LOR:  *const_value = lvalue || rvalue; break;
            case IR_BAND: *const_value = lvalue & rvalue; break;
            case IR_BOR:  *const_value = lvalue | rvalue; break;
            case IR_XOR:  *const_value = lvalue ^ rvalue; break;
            case IR_LT: *const_value= lvalue < rvalue; break;
            case IR_LE: *const_value= lvalue <= rvalue; break;
            case IR_GT: *const_value= lvalue > rvalue; break;
            case IR_GE: *const_value= lvalue >= rvalue; break;
            case IR_EQ: *const_value= lvalue == rvalue; break;
            case IR_NE: *const_value= lvalue != rvalue; break;
            case IR_ASR: *const_value = lvalue >> rvalue; break;
            case IR_LSR: *const_value = lvalue >> rvalue; break;
            case IR_LSL: *const_value = lvalue << rvalue; break;
            default: return false;
            }
        } else {
            LONGLONG lv = (LONGLONG)lvalue;
            LONGLONG rv = (LONGLONG)rvalue;
            LONGLONG res = 0;
            switch (ir->getCode()) {
            case IR_ADD:  res = lv + rv; break;
            case IR_SUB:  res = lv - rv; break;
            case IR_MUL:  res = lv * rv; break;
            case IR_DIV:  res = lv / rv; break;
            case IR_REM:  res = lv % rv; break;
            case IR_MOD:  res = lv % rv; break;
            case IR_LAND: res = lv && rv; break;
            case IR_LOR:  res = lv || rv; break;
            case IR_BAND: res = lv & rv; break;
            case IR_BOR:  res = lv | rv; break;
            case IR_XOR:  res = lv ^ rv; break;
            case IR_LT: res = lv < rv; break;
            case IR_LE: res = lv <= rv; break;
            case IR_GT: res = lv > rv; break;
            case IR_GE: res = lv >= rv; break;
            case IR_EQ: res = lv == rv; break;
            case IR_NE: res = lv != rv; break;
            case IR_ASR: res = lv >> rv; break;
            case IR_LSR: res = lv >> rv; break;
            case IR_LSL: res = lv << rv; break;
            default: return false;
            }
            *const_value = (ULONGLONG)res;
        }
        return true;
    }
    case IR_BNOT: //bitwise not
    case IR_LNOT: //logical not
    case IR_NEG: { //negative
        if (!UNA_opnd(ir)->is_int()) { return false; }

        ULONGLONG value = 0;
        if (!evaluateConstInteger(UNA_opnd(ir), &value)) { return false; }

        switch (ir->getCode()) {
        case IR_BNOT: *const_value = ~value; break;
        case IR_LNOT: *const_value = !value; break;
        case IR_NEG:  *const_value = (ULONGLONG)(-(LONGLONG)value); break;
        default: return false;
        }
        return true;
    }
    case IR_PR: {
        IR * defstmt = nullptr;
        SSAInfo const* ssainfo = PR_ssainfo(ir);
        if (ssainfo != nullptr) {
            defstmt = SSA_def(ssainfo);
            if (defstmt != nullptr && !defstmt->is_stpr()) {
                return false;
            }
        } else {
            DUSet const* defset = ir->readDUSet();
            if (defset == nullptr || defset->get_elem_count() != 1) {
                return false;
            }

            DUIter di = nullptr;
            defstmt = getIR(defset->get_first(&di));
            ASSERT0(defstmt && defstmt->is_stmt());

            if (!defstmt->is_stpr()) { return false; }
        }
        return evaluateConstInteger(STPR_rhs(defstmt), const_value);
    }
    case IR_CVT:
        return evaluateConstInteger(CVT_exp(ir), const_value);
    default:;
    }
    return false;
}


//Register gloval variable located in program region.
void Region::registerGlobalVAR()
{
    MD const * common_string_var_md = getRegionMgr()->genDedicateStrMD();
    if (common_string_var_md != nullptr) {
        ASSERT0(is_program());
        addToVarTab(common_string_var_md->get_base());
    }
}


//Find the boundary IR in BB and split single BB into two BB.
//Note this function will update BB list incremently, it means
//current IR's simplication should NOT dependent on previous
//IR|BB.
//e.g: Given BB1 has one stmt:
//    BB1:
//    a = b||c
//after some simplification, we get:
//    truebr(b != 0), L1
//    truebr(c != 0), L1
//    pr = 0
//    goto L2
//    L1:
//    pr = 1
//    L2:
//    a = pr
//
//where IR list should be splitted into :
//    BB1:
//    truebr(b != 0), L1
//    BB2:
//    truebr(c != 0), L1
//    BB3:
//    pr = 0
//    goto L2
//    BB4:
//    L1:
//    pr = 1
//    BB5:
//    L2:
//    a = pr
bool Region::reconstructBBList(OptCtx & oc)
{
    START_TIMER(t, "Reconstruct IRBB list");
    //Note if CFG is invalid, it will not be updated.
    bool change = false;
    BBListIter ctbb;
    BBList * bbl = getBBList();
    for (bbl->get_head(&ctbb); ctbb != nullptr; bbl->get_next(&ctbb)) {
        IRBB * bb = ctbb->val();
        IRListIter ctir;
        BBIRList * irlst = &BB_irlist(bb);

        IR * tail = irlst->get_tail();
        for (irlst->get_head(&ctir); ctir != nullptr; irlst->get_next(&ctir)) {
            IR * ir = ctir->val();
            if (IRBB::isLowerBoundary(ir) && ir != tail) {
                change = true;

                IR * restirs = nullptr; //record rest part in bb list after 'ir'.
                IR * last = nullptr;
                irlst->get_next(&ctir);

                for (C<IR*> * next_ctir = ctir;
                     ctir != nullptr; ctir = next_ctir) {
                    irlst->get_next(&next_ctir);
                    irlst->remove(ctir);
                    xcom::add_next(&restirs, &last, ctir->val());
                }

                ctbb = splitIRlistIntoBB(restirs, bbl, ctbb, oc);
                break;
            } else if (IRBB::isUpperBoundary(ir)) {
                ASSERT0(ir->is_label());
                change = true;
                IR * restirs = nullptr; //record rest part in bb list after 'ir'.
                IR * last = nullptr;
                for (C<IR*> * next_ctir = ctir;
                     ctir != nullptr; ctir = next_ctir) {
                    irlst->get_next(&next_ctir);
                    irlst->remove(ctir);
                    xcom::add_next(&restirs, &last, ctir->val());
                }

                ctbb = splitIRlistIntoBB(restirs, bbl, ctbb, oc);
                break;
            }
        }
    }

    END_TIMER(t, "Reconstruct IRBB list");

    if (change) {
        //Must rebuild CFG and all other structures which are
        //closely related to CFG.
        oc.setInvalidIfCFGChanged();
    }
    return change;
}


//Construct IR list from IRBB list.
//clean_ir_list: clean bb's ir list if it is true.
IR * Region::constructIRlist(bool clean_ir_list)
{
    START_TIMER(t, "Construct IR list from BB");
    IR * ret_list = nullptr;
    IR * last = nullptr;
    BBListIter ct;
    for (getBBList()->get_head(&ct);
         ct != getBBList()->end();
         ct = getBBList()->get_next(ct)) {
        IRBB * bb = ct->val();
        xcom::C<LabelInfo const*> * lct;
        for (bb->getLabelList().get_head(&lct);
             lct != bb->getLabelList().end();
             lct = bb->getLabelList().get_next(lct)) {
            LabelInfo const* li = lct->val();
            //insertbefore_one(&ret_list, ret_list, buildLabel(li));
            xcom::add_next(&ret_list, &last, buildLabel(li));
        }

        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            //insertbefore_one(&ret_list, ret_list, ir);
            xcom::add_next(&ret_list, &last, ir);
            if (clean_ir_list) {
                ir->setBB(nullptr);
            }
        }
        if (clean_ir_list) {
            BB_irlist(bb).clean();
        }
    }

    //ret_list = reverse_list(ret_list);
    END_TIMER(t, "Construct IR list from BB");
    return ret_list;
}


//1. Split list of IRs into basic-block list.
//2. Set BB propeties. e.g: entry-bb, exit-bb.
void Region::constructBBList()
{
    if (getIRList() == nullptr) { return; }
    START_TIMER(t, "Construct IRBB list");
    IRBB * cur_bb = nullptr;
    IR * pointer = getIRList();
    while (pointer != nullptr) {
        if (cur_bb == nullptr) {
            cur_bb = allocBB();
        }

        //Insert IR into individual BB.
        ASSERT0(pointer->isStmtInBB() || pointer->is_lab());
        IR * cur_ir = pointer;
        pointer = IR_next(pointer);
        IR_next(cur_ir) = IR_prev(cur_ir) = nullptr;

        if (IRBB::isLowerBoundary(cur_ir)) {
            BB_irlist(cur_bb).append_tail(cur_ir);
            //Generate new BB.
            getBBList()->append_tail(cur_bb);
            cur_bb = allocBB();
            continue;
        }

        if (cur_ir->is_label()) {
            getBBList()->append_tail(cur_bb);

            //Generate new BB.
            cur_bb = allocBB();

            //label info be seen as add-on info attached on bb, and
            //'ir' be dropped off.
            bool not_merge_label = true;
            for (;;) {
                cur_bb->addLabel(LAB_lab(cur_ir));
                freeIR(cur_ir);
                if (not_merge_label) {
                    break;
                }
                if (pointer != nullptr && pointer->is_label()) {
                    cur_ir = pointer;
                    pointer = IR_next(pointer);
                    IR_next(cur_ir) = IR_prev(cur_ir) = nullptr;
                } else {
                    break;
                }
            }

            BB_is_target(cur_bb) = true;
            continue;
        }

        if (cur_ir->isMayThrow()) {
            BB_irlist(cur_bb).append_tail(cur_ir);

            //Generate new BB.
            getBBList()->append_tail(cur_bb);
            cur_bb = allocBB();
            continue;
        }

        //Note that PHI should be placed followed after a LABEL immediately.
        //That is a invalid phi if it has only one operand.
        BB_irlist(cur_bb).append_tail(cur_ir);
    } //end while

    ASSERT0(cur_bb != nullptr);
    getBBList()->append_tail(cur_bb);

    //cur_bb is the last bb, it is also the exit bb.
    //IR_BB_is_func_exit(cur_bb) = true;
    END_TIMER(t, "Construct IRBB list");
}


//Do general check for safe optimizing.
// CASE 1:
//     Do NOT optimize the code if meet the VIOLATE variable!
//     e.g1:
//         volatile int x, y;
//         int a[SIZE];
//         void f(void)
//         {
//           for (int i = 0; i < SIZE; i++)
//             a[i] = x + y;
//         }
//
//         Some compilers will hoist the expression (x + y)
//         out of the loop as shown below, which is an
//         incorrect optimization.
//     e.g2:
//         ## Incorrect Definiation! x,y are NOT const.
//         const violate int x,y;
bool Region::isSafeToOptimize(IR const* ir)
{
    if (ir->is_volatile()) {
        return false;
    }

    //Check kids.
    for(INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            if (!isSafeToOptimize(kid)) {
                return false;
            }
        }
    }
    return true;
}


//Return true if Var belongs to current region.
bool Region::isRegionVAR(Var const* var) const
{
    return const_cast<Region*>(this)->getVarTab()->find(const_cast<Var*>(var));
}


bool Region::isRegionIR(IR const* ir) const
{
    Vector<IR*> * vec = getIRVec();
    for (INT i = 0; i <= vec->get_last_idx(); i++) {
        if (ir == vec->get(i)) { return true; }
    }
    return false;
}


//Generate Var corresponding to PR load or write.
Var * Region::genVarForPR(UINT prno, Type const* type)
{
    ASSERT0(type);
    Var * pr_var = mapPR2Var(prno);
    if (pr_var != nullptr) { return pr_var; }

    //Create a new PR Var.
    CHAR name[128];
    sprintf(name, "pr%d", prno);
    ASSERT0(strlen(name) < 128);
    pr_var = getVarMgr()->registerVar(name, type, 0, VAR_LOCAL | VAR_IS_PR);
    setMapPR2Var(prno, pr_var);
    VAR_prno(pr_var) = prno;

    //Set the pr-var to be unallocable, means do NOT add
    //pr-var immediately as a memory-variable.
    //For now, it is only be regarded as a pseduo-register.
    //And set it to allocable if the PR is in essence need to be
    //allocated in memory.
    VAR_is_unallocable(pr_var) = true;
    addToVarTab(pr_var);
    return pr_var;
}


//Generate MD corresponding to PR load or write.
MD const* Region::genMDForPR(UINT prno, Type const* type)
{
    ASSERT0(type);
    Var * pr_var = mapPR2Var(prno);
    if (pr_var == nullptr) {
        pr_var = genVarForPR(prno, type);
    }

    MD md;
    MD_base(&md) = pr_var; //correspond to Var
    MD_ofst(&md) = 0;
    if (pr_var->getType()->is_any()) {
        MD_ty(&md) = MD_UNBOUND;
    } else {
        MD_ty(&md) = MD_EXACT;
        MD_size(&md) = getTypeMgr()->getByteSize(pr_var->getType());
    }
    MD const* e = getMDSystem()->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


//Get function unit.
Region * Region::getFuncRegion()
{
    Region * rg = this;
    while (!rg->is_function()) { rg = rg->getParent(); }
    ASSERTN(rg != nullptr, ("Not in func unit"));
    return rg;
}


CHAR const* Region::getRegionName() const
{
    if (getRegionVar() != nullptr) {
        ASSERT0(getRegionVar()->get_name());
        return SYM_name(getRegionVar()->get_name());
    }

    //Miss region variable.
    return nullptr;
}


//Use HOST_INT type describes the value.
//The value can not exceed ir type's value range.
HOST_INT Region::getIntegerInDataTypeValueRange(IR * ir) const
{
    ASSERT0(ir->is_const() && ir->is_int());
    UINT bitsz = getTypeMgr()->getDTypeBitSize(
        TY_dtype(ir->getType()));
    ASSERTN(sizeof(HOST_INT) * BIT_PER_BYTE >= bitsz,
        ("integer might be truncated"));
    switch (bitsz) {
    case 8: return (HOST_INT)(((UINT8)(INT8)CONST_int_val(ir)));
    case 16: return (HOST_INT)(((UINT16)(INT16)CONST_int_val(ir)));
    case 32: return (HOST_INT)(((UINT32)(INT32)CONST_int_val(ir)));
    case 64: return (HOST_INT)(((UINT64)(INT64)CONST_int_val(ir)));
    case 128:
        #ifdef INT128
        return (HOST_INT)(((UINT128)(INT128)CONST_int_val(ir)));
        #endif
    default: ASSERTN(0, ("TODO:need to support"));
    }
    return 0;
}


HOST_INT Region::getMaxInteger(UINT bitsize, bool is_signed) const
{
    ASSERTN(sizeof(HOST_INT) * BIT_PER_BYTE >= bitsize,
        ("integer might be truncated"));
    if (is_signed) {
        switch (bitsize) {
        case 8: return (HOST_INT)(((UINT8)(INT8)-1) >> 1);
        case 16: return (HOST_INT)(((UINT16)(INT16)-1) >> 1);
        case 32: return (HOST_INT)(((UINT32)(INT32)-1) >> 1);
        case 64: return (HOST_INT)(((UINT64)(INT64)-1) >> 1);
        case 128:
            #ifdef INT128
            return (HOST_INT)(((UINT128)(INT128)-1) >> 1);
            #endif
        default: ASSERTN(0, ("TODO:need to support"));
        }
        return 0;
    }

    switch (bitsize) {
    case 8: return (HOST_INT)(((UINT8)(INT8)-1));
    case 16: return (HOST_INT)(((UINT16)(INT16)-1));
    case 32: return (HOST_INT)(((UINT32)(INT32)-1));
    case 64: return (HOST_INT)(((UINT64)(INT64)-1));
    case 128:
        #ifdef INT128
        return (HOST_INT)(((UINT128)(INT128)-1));
        #endif
    default: ASSERTN(0, ("TODO:need to support"));
    }
    return 0;
}


HOST_INT Region::getMinInteger(UINT bitsize, bool is_signed) const
{
    switch (bitsize) {
    case 8:
        return (HOST_INT)
            ((UINT8)(~(UINT8)getMaxInteger(bitsize, is_signed)));
    case 16:
        return (HOST_INT)
            ((UINT16)(~(UINT16)getMaxInteger(bitsize, is_signed)));
    case 32:
        return (HOST_INT)
            ((UINT32)(~(UINT32)getMaxInteger(bitsize, is_signed)));
    case 64:
        return (HOST_INT)
            ((UINT64)(~(UINT64)getMaxInteger(bitsize, is_signed)));
    case 128:
        #ifdef INT128
        return (HOST_INT)
            ((UINT128)(~(UINT128)getMaxInteger(bitsize, is_signed)));
        #endif
    default: ASSERTN(0, ("TODO:need to support"));
    }
    return 0;
}


//Free ir, ir's sibling, and all its kids.
//We can only utilizing the function to free the IR
//which allocated by 'allocIR'.
//
//NOTICE: If ir's sibling is not nullptr, that means the IR is
//a high level type. IRBB consists of only middle/low level IR.
void Region::freeIRTreeList(IR * ir)
{
    if (ir == nullptr) { return; }
    IR * head = ir, * next = nullptr;
    while (ir != nullptr) {
        next = ir->get_next();
        xcom::remove(&head, ir);
        freeIRTree(ir);
        ir = next;
    }
}


//Free IRBB list.
//We can only utilizing the function to free the IRBB
//which allocated by 'allocBB'.
//NOTICE: bb will not be destroyed, it is just recycled.
void Region::freeIRBBList(BBList & bbl)
{
    IRBBMgr * mgr = getBBMgr();
    for (IRBB * bb = bbl.remove_head(); bb != nullptr; bb = bbl.remove_head()) {
        mgr->freeBB(bb);
    }
    bbl.clean();
}


//Free ir, and all its kids.
//We can only utilizing the function to free
//the IR which allocated by 'allocIR'.
void Region::freeIRTreeList(IRList & irs)
{
    IRListIter next;
    IRListIter ct;
    for (irs.get_head(&ct); ct != irs.end(); ct = next) {
        IR * ir = ct->val();
        next = irs.get_next(ct);
        ASSERTN(ir->is_single(),
                ("do not allow sibling node, need to simplify"));
        irs.remove(ir);
        freeIRTree(ir);
    }
}


//Free ir and all its kids, except its sibling node.
//We can only utilizing the function to free the
//IR which allocated by 'allocIR'.
void Region::freeIRTree(IR * ir)
{
    if (ir == nullptr) { return; }
    ASSERTN(!ir->is_undef(), ("ir has been freed"));
    ASSERTN(ir->is_single(), ("chain list should be cut off"));
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            freeIRTreeList(kid);
        }
    }
    freeIR(ir);
}


//This function erases all informations of ir and
//append it into free_list for next allocation.
//If Attach Info exist, this function will erase it rather than delete.
//If DU info exist, this function will retrieve it back
//to region for next allocation.
//Note that this function does NOT free ir's kids and siblings.
void Region::freeIR(IR * ir)
{
    ASSERT0(ir);
    ASSERTN(ir->is_single(), ("chain list should be cut off"));
    #ifdef _DEBUG_
    ASSERT0(!getAnalysisInstrument()->
            m_has_been_freed_irs.is_contain(ir->id()));
    getAnalysisInstrument()->m_has_been_freed_irs.bunion(ir->id());
    #endif

    ASSERT0(getMiscBitSetMgr());
    ir->freeDUset(*getMiscBitSetMgr());

    AIContainer * res_ai = IR_ai(ir);
    if (res_ai != nullptr) {
        //AICont will be reinitialized till next setting.
        res_ai->destroy();
    }

    DU * du = ir->cleanDU();
    if (du != nullptr) {
        DU_md(du) = nullptr;
        DU_mds(du) = nullptr;
        getAnalysisInstrument()->m_free_du_list.append_head(du);
    }

    //Zero clearing all data fields.
    UINT res_id = ir->id();
    UINT res_irt_sz = getIRTypeSize(ir);
    ::memset(ir, 0, res_irt_sz);
    IR_id(ir) = res_id;
    IR_ai(ir) = res_ai;
    set_irt_size(ir, res_irt_sz);

    UINT idx = res_irt_sz - sizeof(IR);
    IR * head = getAnalysisInstrument()->m_free_tab[idx];
    if (head != nullptr) {
        IR_next(ir) = head;
        IR_prev(head) = ir;
    }
    getAnalysisInstrument()->m_free_tab[idx] = ir;
}


//This function find Var via iterating Var table of current region.
Var * Region::findVarViaSymbol(Sym const* sym) const
{
    ASSERT0(sym);
    VarTab * vtab = const_cast<Region*>(this)->getVarTab();
    VarTabIter c;
    for (Var * v = vtab->get_first(c); v != nullptr; v = vtab->get_next(c)) {
        if (v->get_name() == sym) {
            return v;
        }
    }
    return nullptr;
}


//This function iterate Var table of current region to
//find all Var which are formal parameter.
//in_decl_order: if it is true, this function will sort the formal
//parameters in the Left to Right order according to their declaration.
//e.g: foo(a, b, c), varlst will be {a, b, c}.
void Region::findFormalParam(OUT List<Var const*> & varlst, bool in_decl_order)
{
    VarTabIter c;
    VarTab * vt = getVarTab();
    ASSERT0(vt);

    if (in_decl_order)  {
        //Sort parameter in declaration order.
        for (Var const* v = vt->get_first(c);
             v != nullptr; v = vt->get_next(c)) {
            if (!VAR_is_formal_param(v)) { continue; }

            xcom::C<Var const*> * ctp;
            bool find = false;
            for (Var const* p = varlst.get_head(&ctp);
                 p != nullptr; p = varlst.get_next(&ctp)) {
                if (v->getFormalParamPos() < p->getFormalParamPos()) {
                    varlst.insert_before(v, ctp);
                    find = true;
                    break;
                }
            }

            if (!find) {
                varlst.append_tail(v);
            }
        }
        return;
    }

    //Unordered
    for (Var const* v = vt->get_first(c); v != nullptr; v = vt->get_next(c)) {
        if (VAR_is_formal_param(v)) {
            varlst.append_tail(v);
        }
    }
}


//This function find the formal parameter variable by given position.
Var const* Region::findFormalParam(UINT position) const
{
    VarTabIter c;
    VarTab * vt = const_cast<Region*>(this)->getVarTab();
    ASSERT0(vt);
    for (Var const* v = vt->get_first(c); v != nullptr; v = vt->get_next(c)) {
        if (VAR_is_formal_param(v) && VAR_formal_param_pos(v) == position) {
            return v;
        }
    }
    return nullptr;
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* Region::allocPRMD(IR * pr)
{
    ASSERT0(pr->is_pr());
    MD const* md = genMDForPR(pr);
    setMustRef(pr, md);
    pr->cleanRefMDSet();
    return md;
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* Region::allocPhiMD(IR * phi)
{
    ASSERT0(phi->is_phi());
    MD const* md = genMDForPR(phi);
    setMustRef(phi, md);
    phi->cleanRefMDSet();
    return md;
}


MD const* Region::allocIdMD(IR * ir)
{
    ASSERT0(ir->is_id());
    MD const* t = genMDForId(ir);
    setMustRef(ir, t);
    ir->cleanRefMDSet();
    return t;
}


MD const* Region::allocLoadMD(IR * ir)
{
    ASSERT0(ir->is_ld());
    MD const* t = genMDForLoad(ir);
    ASSERT0(t);
    ir->cleanRefMDSet();

    //TO BE REMOVED: genMDForLoad has consider the Offset of ir.
    //if (LD_ofst(ir) != 0) {
    //    MD t2(*t);
    //    ASSERT0(t2.is_exact());
    //    MD_ofst(&t2) += LD_ofst(ir);
    //    MD_size(&t2) = ir->getTypeSize(getTypeMgr());
    //    MD const* entry = getMDSystem()->registerMD(t2);
    //    ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
    //    t = entry; //regard MD with offset as return result.
    //}

    setMustRef(ir, t);
    return t;
}


MD const* Region::allocStorePRMD(IR * ir)
{
    ASSERT0(ir->is_stpr());
    MD const* md = genMDForPR(ir);
    setMustRef(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* Region::allocCallResultPRMD(IR * ir)
{
    ASSERT0(ir->isCallStmt());
    MD const* md = genMDForPR(ir);
    setMustRef(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* Region::allocSetelemMD(IR * ir)
{
    ASSERT0(ir->is_setelem());
    MD const* md = genMDForPR(ir);
    IR const* ofst = SETELEM_ofst(ir);
    ASSERT0(ofst);
    if (md->is_exact()) {
        if (ofst->is_const()) {
            ASSERTN(ofst->is_int(), ("offset of SETELEM must be integer."));

            //Accumulating offset of identifier.
            //e.g: struct {int a,b; } s; s.a = 10
            //generate: st s:offset(4) = 10;
            MD t(*md);
            ASSERT0(ir->getTypeSize(getTypeMgr()) > 0);
            MD_ofst(&t) += (UINT)CONST_int_val(ofst);
            MD_size(&t) = ir->getTypeSize(getTypeMgr());
            MD const* entry = getMDSystem()->registerMD(t);
            ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
            md = entry; //regard MD with offset as return result.
        } else {
            //Offset is variable.
            //e.g: vector<4xi32> v; v[i] = 34;
            //will generate:
            //    st $1 = ld v;
            //    setelem $1 = 34, ld i;
            //    st v = $1;

            MD t(*md);
            ASSERT0(ir->getTypeSize(getTypeMgr()) > 0);
            MD_ty(&t) = MD_RANGE;
            MD_ofst(&t) = 0;
            MD_size(&t) = ir->getTypeSize(getTypeMgr());
            MD const* entry = getMDSystem()->registerMD(t);
            ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
            md = entry; //regard MD with range as return result.
        }
    }

    setMustRef(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* Region::allocGetelemMD(IR * ir)
{
    ASSERT0(ir->is_getelem());
    MD const* md = genMDForPR(ir);
    setMustRef(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* Region::allocStoreMD(IR * ir)
{
    ASSERT0(ir->is_st());
    MD const* md = genMDForStore(ir);
    ASSERT0(md);
    ir->cleanRefMDSet();

    //TO BE REMOVED: genMDForStore has considered the Offset of ir.
    //if (ST_ofst(ir) != 0) {
    //    //Accumulating offset of identifier.
    //    //e.g: struct {int a,b; } s; s.a = 10
    //    //generate: st('s', ofst:4) = 10
    //    MD t(*md);
    //    ASSERT0(t.is_exact());
    //    ASSERT0(ir->getTypeSize(getTypeMgr()) > 0);
    //    MD_ofst(&t) += ST_ofst(ir);
    //    MD_size(&t) = ir->getTypeSize(getTypeMgr());
    //    MD const* entry = getMDSystem()->registerMD(t);
    //    ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
    //    md = entry; //regard MD with offset as return result.
    //}

    setMustRef(ir, md);
    return md;
}


//Alloc MD for const string.
MD const* Region::allocStringMD(Sym const* string)
{
    ASSERT0(string);
    MD const* strmd = getRegionMgr()->genDedicateStrMD();
    if (strmd != nullptr) { return strmd; }

    Var * v = getVarMgr()->registerStringVar(nullptr, string, MEMORY_ALIGNMENT);
    //Set string address to be taken only if it is base of LDA.
    //VAR_is_addr_taken(v) = true;
    MD md;
    MD_base(&md) = v;
    MD_size(&md) = (UINT)strlen(SYM_name(string)) + 1;
    MD_ofst(&md) = 0;
    MD_ty(&md) = MD_EXACT;
    ASSERT0(v->is_string());

    MD const* e = getMDSystem()->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


//Allocate PassMgr
PassMgr * Region::allocPassMgr()
{
    return new PassMgr(this);
}


void Region::dumpIRList() const
{
    if (!isLogMgrInit()) { return; }
    note(this, "\n==---- DUMP IR LIST '%s' ----==", getRegionName());
    if (getIRList() == nullptr) { return; }
    xoc::dumpIRList(getIRList(), this);
}


void Region::dumpBBList(bool dump_inner_region) const
{
    if (getBBList() == nullptr) { return; }
    xoc::dumpBBList(getBBList(), this, dump_inner_region);
}


AnalysisInstrument * Region::getAnalysisInstrument() const
{
    ASSERT0(is_function() || is_program() || is_inner() || is_eh());
    return REGION_analysis_instrument(this);
}


void Region::dumpFreeTab() const
{
    if (!isLogMgrInit()) { return; }
    note(this, "\n==-- DUMP Region Free Table --==");
    for (UINT i = 0; i <= MAX_OFFSET_AT_FREE_TABLE; i++) {
        IR * lst = getAnalysisInstrument()->m_free_tab[i];
        if (lst == nullptr) { continue; }

        UINT sz = i + sizeof(IR);

        UINT count = 0;
        for (IR * ir = lst; ir != nullptr; ir = ir->get_next()) {
            count++;
        }

        note(this, "\nirsize(%d), num(%d):", sz, count);

        for (IR * ir = lst; ir != nullptr; ir = ir->get_next()) {
            ASSERT0(getIRTypeSize(ir) == sz);
            prt(this, "ir(%d),", ir->id());
        }
    }
}


void Region::assignMDImpl(IR * x, bool assign_pr, bool assign_nonpr)
{
    ASSERT0(x);
    switch (x->getCode()) {
    case IR_PR:
        if (assign_pr) {
            allocPRMD(x);
        }
        break;
    case IR_STPR:
        if (assign_pr) {
            allocStorePRMD(x);
        }
        break;
    case IR_GETELEM:
        if (assign_pr) {
            allocGetelemMD(x);
        }
        break;
    case IR_SETELEM:
        if (assign_pr) {
            allocSetelemMD(x);
        }
        break;
    case IR_PHI:
        if (assign_pr) {
            allocPhiMD(x);
        }
        break;
    case IR_CALL:
    case IR_ICALL:
        if (assign_pr && x->hasReturnValue()) {
            allocCallResultPRMD(x);
        }
        break;
    case IR_ST:
        if (assign_nonpr) {
            allocStoreMD(x);
        }
        break;
    case IR_LD:
        if (assign_nonpr) {
            allocLoadMD(x);
        }
        break;
    case IR_ID:
        if (assign_nonpr) {
            if (ID_info(x)->is_string()) {
                allocStringMD(ID_info(x)->get_name());
            } else {
                allocIdMD(x);
            }
        }
        break;
    default: ASSERT0(!x->isReadPR() && !x->isWritePR());
    }
}


//Assign MD for ST/LD/ReadPR/WritePR operations.
//is_only_assign_pr: true if assign MD for each ReadPR/WritePR operations.
void Region::assignMD(bool assign_pr, bool assign_nonpr)
{
    if (getIRList() != nullptr) {
        assignMDForIRList(getIRList(), assign_pr, assign_nonpr);
        return;
    }
    if (getBBList() != nullptr) {
        assignMDForBBList(getBBList(), assign_pr, assign_nonpr);
    }
}


void Region::assignMDForBBList(BBList * lst, bool assign_pr, bool assign_nonpr)
{
    ASSERT0(lst);
    IRIter ii;
    for (IRBB * bb = lst->get_head(); bb != nullptr; bb = lst->get_next()) {
        assignMDForBB(bb, ii, assign_pr, assign_nonpr);
    }
}


void Region::assignMDForBB(IRBB * bb,
                           IRIter & ii,
                           bool assign_pr,
                           bool assign_nonpr)
{
    BBIRListIter ct;
    for (xoc::IR * ir = bb->getIRList()->get_head(&ct);
         ir != nullptr; ir = bb->getIRList()->get_next(&ct)) {
        for (IR * x = iterInit(ir, ii);
           x != nullptr; x = iterNext(ii)) {
           assignMDImpl(x, assign_pr, assign_nonpr);
        }
    }
}


void Region::assignMDForIRList(IR * lst, bool assign_pr, bool assign_nonpr)
{
    for (IR * ir = lst; ir != nullptr; ir = ir->get_next()) {
        switch (ir->getCode()) {
        case IR_PR:
        case IR_LD:
        case IR_ID:
            assignMDImpl(ir, assign_pr, assign_nonpr);
            break;
        default: {
            //Iterate rest of ir in lst.
            IRIter ii;
            for (IR * x = iterInit(lst, ii);
                 x != nullptr; x = iterNext(ii)) {
                assignMDImpl(x, assign_pr, assign_nonpr);
            }
            return;
        }
        }
    }
}


//Generate IR, invoke freeIR() or freeIRTree() if it is useless.
//NOTE: Do NOT invoke ::free() to free IR, because all
//    IR are allocated in the pool.
IR * Region::allocIR(IR_TYPE irt)
{
    IR * ir = nullptr;
    UINT idx = IRTSIZE(irt) - sizeof(IR);
    ASSERTN(idx < 1000, ("weird index"));
    bool lookup = false; //lookup freetab will save more memory, but slower.

    #ifndef CONST_IRT_SZ
    //If one is going to lookup freetab, IR_irt_size() must be defined.
    ASSERT0(!lookup);
    #endif

    if (lookup) {
        for (; idx <= MAX_OFFSET_AT_FREE_TABLE; idx++) {
            ir = getAnalysisInstrument()->m_free_tab[idx];
            if (ir == nullptr) { continue; }

            getAnalysisInstrument()->m_free_tab[idx] = ir->get_next();
            if (ir->get_next() != nullptr) {
                IR_prev(ir->get_next()) = nullptr;
            }
            break;
        }
    } else {
        ir = getAnalysisInstrument()->m_free_tab[idx];
        if (ir != nullptr) {
            getAnalysisInstrument()->m_free_tab[idx] = ir->get_next();
            if (ir->get_next() != nullptr) {
                IR_prev(ir->get_next()) = nullptr;
            }
        }
    }

    if (ir == nullptr) {
        ir = (IR*)xmalloc(IRTSIZE(irt));
        INT v = MAX(getIRVec()->get_last_idx(), 0);
        IR_id(ir) = (UINT)(v+1);
        getIRVec()->set(ir->id(), ir);
        set_irt_size(ir, IRTSIZE(irt));
    } else {
        ASSERT0(ir->get_prev() == nullptr);
        IR_next(ir) = nullptr;
        #ifdef _DEBUG_
        getAnalysisInstrument()->m_has_been_freed_irs.diff(ir->id());
        #endif
    }
    IR_code(ir) = irt;
    return ir;
}


//Duplication 'ir' and kids, and its sibling, return list of new ir.
//Duplicate irs start from 'ir' to the end of list.
//The duplication includes AI, except DU info, SSA info.
IR * Region::dupIRTreeList(IR const* ir)
{
    IR * new_list = nullptr;
    while (ir != nullptr) {
        IR * newir = dupIRTree(ir);
        xcom::add_next(&new_list, newir);
        ir = ir->get_next();
    }
    return new_list;
}

//Duplicate 'ir' and its kids, but without ir's sibiling node.
//The duplication includes AI, except DU info, SSA info.
IR * Region::dupIRTree(IR const* ir)
{
    if (ir == nullptr) { return nullptr; }
    IR * newir = dupIR(ir);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            IR * newkid_list = dupIRTreeList(kid);
            newir->setKid(i, newkid_list);
        } else { ASSERT0(newir->getKid(i) == nullptr); }
    }
    return newir;
}


//Duplication all contents of 'src', includes AI, except DU info,
//SSA info, kids and siblings IR.
IR * Region::dupIR(IR const* src)
{
    if (src == nullptr) { return nullptr; }
    IR_TYPE irt = src->getCode();
    IR * res = allocIR(irt);
    ASSERTN(res != nullptr && src != nullptr, ("res/src is nullptr"));

    UINT res_id = IR_id(res);
    AIContainer * res_ai = IR_ai(res);
    UINT res_irt_sz = getIRTypeSize(res);
    ::memcpy(res, src, IRTSIZE(irt));
    IR_id(res) = res_id;
    IR_ai(res) = res_ai;
    set_irt_size(res, res_irt_sz);
    IR_next(res) = IR_prev(res) = IR_parent(res) = nullptr;
    res->cleanDU(); //Do not copy DU info.
    res->clearSSAInfo(); //Do not copy SSA info.
    if (IR_ai(src) != nullptr) { //need to copy AIContainer.
        if (IR_ai(res) == nullptr) {
            IR_ai(res) = allocAIContainer();
        }
        IR_ai(res)->copy(IR_ai(src), this);
    }
    if (res->isMemoryRef()) {
        res->copyRef(src, this);
    }
    return res;
}

void Region::scanCallListImpl(OUT UINT & num_inner_region,
                              IR * irlst,
                              OUT List<IR const*> * call_list,
                              OUT List<IR const*> * ret_list,
                              bool scan_inner_region)
{
    for (IR const* t = irlst; t != nullptr; t = t->get_next()) {
        switch (t->getCode()) {
        case IR_CALL:
        case IR_ICALL:
            if (!CALL_is_intrinsic(t)) {
                ASSERT0(call_list);
                call_list->append_tail(t);
            }
            break;
        case IR_DO_WHILE:
        case IR_WHILE_DO:
            scanCallListImpl(num_inner_region, LOOP_body(t),
                             call_list, ret_list, scan_inner_region);
            break;
        case IR_DO_LOOP:
            scanCallListImpl(num_inner_region, LOOP_init(t),
                             call_list, ret_list, scan_inner_region);
            scanCallListImpl(num_inner_region, LOOP_step(t), call_list,
                             ret_list, scan_inner_region);
            scanCallListImpl(num_inner_region, LOOP_body(t), call_list,
                             ret_list, scan_inner_region);
            break;
        case IR_IF:
            scanCallListImpl(num_inner_region, IF_truebody(t), call_list,
                             ret_list, scan_inner_region);
            scanCallListImpl(num_inner_region, IF_falsebody(t), call_list,
                             ret_list, scan_inner_region);
            break;
        case IR_SWITCH:
            scanCallListImpl(num_inner_region, SWITCH_body(t), call_list,
                             ret_list, scan_inner_region);
            break;
        case IR_REGION:
            num_inner_region++;
            if (scan_inner_region) {
                REGION_ru(t)->scanCallAndReturnList(
                    num_inner_region, call_list, ret_list, true);
            }
            break;
        case IR_RETURN:
            if (ret_list != nullptr) {
                ret_list->append_tail(t);
            }
            break;
        default:;
        }
    }
}


//num_inner_region: count the number of inner regions.
void Region::scanCallAndReturnList(OUT UINT & num_inner_region,
                                   OUT List<IR const*> * call_list,
                                   OUT List<IR const*> * ret_list,
                                   bool scan_inner_region)
{
    if (getIRList() != nullptr) {
        scanCallListImpl(num_inner_region, getIRList(),
                         call_list, ret_list, scan_inner_region);
    } else {
        for (IRBB * bb = getBBList()->get_head();
             bb != nullptr; bb = getBBList()->get_next()) {
            IR * t = BB_last_ir(bb);
            if (t == nullptr) { continue; }
            ASSERT0(t->isStmtInBB());
            ASSERT0(call_list);
            if (t != nullptr && t->isCallStmt()) {
                call_list->append_tail(t);
            } else if (ret_list != nullptr && t->is_return()) {
                ret_list->append_tail(t);
            } else if (scan_inner_region && t->is_region()) {
                num_inner_region++;
                REGION_ru(t)->scanCallAndReturnList(
                    num_inner_region, call_list, ret_list, true);
            }
        }
    }
}


void Region::prescanBBList(BBList const* bblst)
{
    BBListIter bbit;
    for (IRBB const* bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        BBIRListIter irct;
        for (IR const* ir = const_cast<IRBB*>(bb)->getIRList()->
                 get_head(&irct);
             ir != nullptr;
             ir = const_cast<IRBB*>(bb)->getIRList()->get_next(&irct)) {
            prescanIRList(ir);
        }
    }
}


//Prepare informations for analysis phase, such as record
//which variables have been taken address for both
//global and local variable.
void Region::prescanIRList(IR const* ir)
{
    for (; ir != nullptr; ir = ir->get_next()) {
        switch (ir->getCode()) {
        case IR_ST:
            prescanIRList(ST_rhs(ir));
            break;
        case IR_CALL:
        case IR_ICALL:
            if (g_do_call_graph && !CALL_is_intrinsic(ir)) {
                CIRList * cl = getCallList();
                ASSERT0(cl);
                cl->append_tail(ir);
            }

            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != nullptr) {
                    ASSERT0(IR_parent(k) == ir);
                    prescanIRList(k);
                }
            }
            break;
        case IR_LDA: {
            ASSERT0(LDA_idinfo(ir));
            Var * v = LDA_idinfo(ir);
            if (v->is_string()) {
                if (getRegionMgr()->genDedicateStrMD() != nullptr) {
                    //Treat all string variable as the same one.
                    break;
                }
                Var * sv = getVarMgr()->registerStringVar(nullptr, VAR_string(v),
                                                          MEMORY_ALIGNMENT);
                ASSERT0(sv);
                VAR_is_addr_taken(sv) = true;
            } else if (v->is_label()) {
                ; //do nothing.
            } else {
                //General variable.
                IR const* parent = ir->getParent();
                ASSERT0(parent);
                if (parent->isArrayOp() && parent->isArrayBase(ir)) {
                    ; //nothing to do.
                } else {
                    //If LDA is the base of ARRAY, say (&a)[..], its
                    //address does not need to mark as address taken.
                    VAR_is_addr_taken(LDA_idinfo(ir)) = true;
                }

                // ...=&x.a, address of 'x.a' is taken.
                MD md;
                MD_base(&md) = LDA_idinfo(ir); //correspond to Var
                MD_ofst(&md) = LDA_ofst(ir);
                MD_size(&md) = ir->getTypeSize(getTypeMgr());
                MD_ty(&md) = MD_EXACT;
                getMDSystem()->registerMD(md);
            }
            break;
        }
        case IR_ID:
            //Array base must not be ID. It could be
            //LDA or computational expressions.
            //In C, array base address could be assgined to other variable.
            //Its address should be marked as taken.
            // And it's parent must be LDA.
            //e.g: Address of 'a' is taken.
            //    int a[10];
            //    int * p;
            //    p = a;
            ASSERT0(ID_info(ir));
            break;
        case IR_CONST:
        case IR_LD:
        case IR_GOTO:
        case IR_LABEL:
        case IR_PR:
        case IR_BREAK:
        case IR_CONTINUE:
        case IR_PHI:
        case IR_REGION:
            break;
        case IR_RETURN:
            if (g_do_call_graph) {
                CIRList * cl = getReturnList();
                ASSERT0(cl);
                cl->append_tail(ir);
            }

            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != nullptr) {
                    ASSERT0(IR_parent(k) == ir);
                    prescanIRList(k);
                }
            }
             break;
        default:
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != nullptr) {
                    ASSERT0(IR_parent(k) == ir);
                    prescanIRList(k);
                }
            }
        }
    }
}


//Dump IR and memory usage.
void Region::dumpMemUsage() const
{
    if (!isLogMgrInit()) { return; }

    size_t count = count_mem();
    CHAR const* str = nullptr;
    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else if (count < 1024 * 1024 * 1024) { count /= 1024 * 1024; str = "MB"; }
    else { count /= 1024 * 1024 * 1024; str = "GB"; }
    note(this, "\n'%s' use %lu%s memory", getRegionName(), count, str);

    if (is_blackbox()) {
        //Blackbox does not contain stmt.
        return;
    }

    Vector<IR*> * v = getIRVec();
    UINT nid = 0;
    UINT nld = 0;
    UINT nst = 0;
    UINT nlda = 0;
    UINT ncall = 0;
    UINT nicall = 0;
    UINT nstpr = 0;
    UINT npr = 0;
    UINT nist = 0;
    UINT nbin = 0;
    UINT nuna = 0;
    UINT nstarr = 0;
    UINT narr = 0;
    UINT ncvt = 0;
    UINT ncbr = 0;
    UINT nncbr = 0;
    for (int i = 0; i <= v->get_last_idx(); i++) {
        IR * ir = v->get(i);
        if (ir == nullptr) { continue; }

        if (ir->is_id()) nid++;
        else if (ir->is_ld()) nld++;
        else if (ir->is_st()) nst++;
        else if (ir->is_lda()) nlda++;
        else if (ir->is_call()) ncall++;
        else if (ir->is_icall()) nicall++;
        else if (ir->is_stpr()) nstpr++;
        else if (ir->is_starray()) nstarr++;
        else if (ir->is_array()) narr++;
        else if (ir->is_cvt()) ncvt++;
        else if (ir->isConditionalBr()) ncbr++;
        else if (ir->isUnconditionalBr()) nncbr++;
        else if (ir->is_pr()) npr++;
        else if (ir->is_ist()) nist++;
        else if (ir->isBinaryOp()) nbin++;
        else if (ir->isUnaryOp()) nuna++;
    }

    UINT total = (v->get_last_idx() + 1);
    if (total == 0) {
        note(this, "\nThe number of IR Total:0");
        return;
    }

    note(this, "\nThe number of IR Total:%u, id:%u(%.1f)%%, "
         "ld:%u(%.1f)%%, "
         "st:%u(%.1f)%%, "
         "lda:%u(%.1f)%%,"
         "call:%u(%.1f)%%, "
         "icall:%u(%.1f)%%, "
         "pr:%u(%.1f)%%, "
         "stpr:%u(%.1f)%%, "
         "ist:%u(%.1f)%%,"
         "bin:%u(%.1f)%%, "
         "una:%u(%.1f)%%, "
         "starray:%u(%.1f)%%, "
         "array:%u(%.1f)%%, "
         "cvt:%u(%.1f)%%, "
         "condbr:%u(%.1f)%%, "
         "uncondbr:%u(%.1f)%%, ",
         total,
         nid, ((double)nid) / ((double)total) * 100,
         nld, ((double)nld) / ((double)total) * 100,
         nst, ((double)nst) / ((double)total) * 100,
         nlda, ((double)nlda) / ((double)total) * 100,
         ncall, ((double)ncall) / ((double)total) * 100,
         nicall, ((double)nicall) / ((double)total) * 100,
         npr, ((double)npr) / ((double)total) * 100,
         nstpr, ((double)nstpr) / ((double)total) * 100,
         nist, ((double)nist) / ((double)total) * 100,
         nbin, ((double)nbin) / ((double)total) * 100,
         nuna, ((double)nuna) / ((double)total) * 100,
         nstarr, ((double)nstarr) / ((double)total) * 100,
         narr, ((double)narr) / ((double)total) * 100,
         ncvt, ((double)ncvt) / ((double)total) * 100,
         ncbr, ((double)ncbr) / ((double)total) * 100,
         nncbr, ((double)ncbr) / ((double)total) * 100);
}


void Region::dumpGR(bool dump_inner_region) const
{
    note(this, "\n//====---- Dump region '%s' ----====", getRegionName());
    note(this, "\nregion ");
    switch (getRegionType()) {
    case REGION_PROGRAM: prt(this, "program "); break;
    case REGION_BLACKBOX: prt(this, "blackbox "); break;
    case REGION_FUNC: prt(this, "func "); break;
    case REGION_INNER: prt(this, "inner "); break;
    default: ASSERT0(0); //TODO
    }
    if (getRegionVar() != nullptr) {
        xcom::StrBuf buf(32);
        prt(this, "%s ", compositeName(getRegionVar()->get_name(), buf));
    }
    prt(this, "(");
    dumpParameter();
    prt(this, ")");
    prt(this, " {\n");
    getLogMgr()->incIndent(DUMP_INDENT_NUM);
    dumpVarTab();
    if (!is_blackbox()) {
        DumpGRCtx ctx;
        ctx.dump_inner_region = dump_inner_region;
        ctx.cfg = getCFG();
        if (getIRList() != nullptr) {
            dumpGRList(getIRList(), getTypeMgr(), &ctx);
        } else {
            dumpGRInBBList(getBBList(), getTypeMgr(), &ctx);
        }
    }
    getLogMgr()->decIndent(DUMP_INDENT_NUM);
    note(this, "\n}");
}


void Region::dumpVarTab() const
{
    VarTab * vt = const_cast<Region*>(this)->getVarTab();
    if (vt->get_elem_count() == 0) { return; }
    VarTabIter c;
    StrBuf buf(64);
    bool sort = true; //sort variable in index ascending order.
    if (is_blackbox()) {
        //Blackbox region does not contain SBitSetMgr field, thus
        //it can not create SBitSet.
        sort = false;
    }
    if (!sort) {
        for (Var * v = vt->get_first(c); v != nullptr; v = vt->get_next(c)) {
            if (v->is_formal_param() || v->is_pr()) { continue; }
            buf.clean();
            note(this, "\n%s;", v->dumpGR(buf, getTypeMgr()));
        }
        return;
    }

    //Sort var in id order.
    DefSBitSet set(getMiscBitSetMgr()->getSegMgr());
    for (Var * v = vt->get_first(c); v != nullptr; v = vt->get_next(c)) {
        if (v->is_formal_param() || v->is_pr()) { continue; }
        set.bunion(v->id());
    }
    DefSBitSetIter cur = nullptr;
    for (INT id = set.get_first(&cur); id >= 0; id = set.get_next(id, &cur)) {
        Var * v = getVarMgr()->get_var(id);
        ASSERT0(v);
        buf.clean();
        note(this, "\n%s;", v->dumpGR(buf, getTypeMgr()));
    }
    set.clean();
}


//Dump formal parameter list.
void Region::dumpParameter() const
{
    if (!is_function()) { return; }
    VarTabIter c;
    Vector<Var*> fpvec;
    for (Var * v = const_cast<Region*>(this)->getVarTab()->get_first(c);
         v != nullptr;
         v = const_cast<Region*>(this)->getVarTab()->get_next(c)) {
        if (VAR_is_formal_param(v)) {
            ASSERT0(!v->is_pr());
            fpvec.set(v->getFormalParamPos(), v);
        }
    }
    if (fpvec.get_last_idx() < 0) { return; }
    StrBuf buf(32);
    for (INT i = 0; i <= fpvec.get_last_idx(); i++) {
        Var * v = fpvec.get(i);
        if (i != 0) {
            prt(this, ",");
        }
        if (v == nullptr) {
            //This position may be reserved for other use.
            //ASSERT0(v);
            prt(this, "undefined");
            continue;
        }
        buf.clean();
        prt(this, "%s", v->dumpGR(buf, getTypeMgr()));
    }
}


void Region::dump(bool dump_inner_region) const
{
    if (!isLogMgrInit()) { return; }
    dumpVARInRegion();

    //Dump imported variables referenced.
    MDSet * ru_maydef = getMayDef();
    if (ru_maydef != nullptr) {
        note(this, "\nRegionMayDef(OuterRegion):");
        ru_maydef->dump(getMDSystem(), true);
    }

    MDSet * ru_mayuse = getMayUse();
    if (ru_mayuse != nullptr) {
        note(this, "\nRegionMayUse(OuterRegion):");
        ru_mayuse->dump(getMDSystem(), true);
    }

    dumpMemUsage();
    if (is_blackbox()) { return; }

    IR * irlst = getIRList();
    if (irlst != nullptr) {
        note(this, "\n==---- IR List ----==");
        xoc::dumpIRList(irlst, this, nullptr,
            IR_DUMP_KID | IR_DUMP_SRC_LINE |
            (dump_inner_region ? IR_DUMP_INNER_REGION : 0));
        return;
    }
    dumpBBList(dump_inner_region);
}


//Dump all irs and ordering by IR_id.
void Region::dumpAllocatedIR() const
{
    if (!isLogMgrInit()) { return; }
    note(this, "\n==---- DUMP ALL IR INFO ----==");
    INT n = getIRVec()->get_last_idx();
    INT i = 1;
    getLogMgr()->incIndent(2);
    UINT num_has_du = 0;

    //Dump which IR has allocate DU structure.
    while (i <= n) {
        IR * ir = getIRVec()->get(i);
        ASSERT0(ir);
        i++;
        DU * du = ir->getDU();
        if (du != nullptr) {
            num_has_du++;
        }
    }
    if (i > 0) {
        note(this, "\nTotal IR %d, total DU allocated %d, rate:(%.1f)%%",
             i, num_has_du, (float)num_has_du / (float)i * 100);
    }
    //

    //Dump IR dispers in free tab.
    note(this, "\n==---- Dump IR dispersed in free tab ----==");
    for (UINT w = 0; w < MAX_OFFSET_AT_FREE_TABLE + 1; w++) {
        IR * lst = getAnalysisInstrument()->m_free_tab[w];
        note(this, "\nbyte(%d)", (INT)(w + sizeof(IR)));
        if (lst == nullptr) { continue; }

        UINT num = 0;
        IR * p = lst;
        while (p != nullptr) { p = p->get_next(); num++; }
        prt(this, ", num%d : ", num);

        while (lst != nullptr) {
            prt(this, "%s", IRNAME(lst));
            lst = IR_next(lst);
            if (lst != nullptr) {
                prt(this, ", ");
            }
        }
    }

    note(this, "\n==---- DUMP IR allocated ----==");

    StrBuf buf(64); //record data-type.
    TypeMgr * dm = getTypeMgr();

    i = 1;
    while (i <= n) {
        IR * ir = getIRVec()->get(i);
        ASSERT0(ir);
        Type const* d = nullptr;
        if (!ir->is_undef()) {
            d = IR_dt(ir);
            ASSERT0(d);
            if (d == nullptr) {
                note(this, "\nid(%d): %s 0x%.8x", ir->id(), IRNAME(ir), ir);
            } else {
                buf.clean();
                note(this, "\nid(%d): %s r:%s 0x%.8x",
                     ir->id(), IRNAME(ir), dm->dump_type(d, buf), ir);
            }
        } else {
            note(this, "\nid(%d): undef 0x%.8x", ir->id(), ir);
        }

        i++;

        DU * du = ir->getDU();
        if (du != nullptr) {
            prt(this, " has du");
        }
    }
    getLogMgr()->decIndent(2);
}


//Dump Region's IR BB list.
//DUMP ALL BBList DEF/USE/OVERLAP_DEF/OVERLAP_USE.
void Region::dumpRef(UINT indent) const
{
    if (!isLogMgrInit()) { return; }
    note(this, "\n\n==---- DUMP DUMgr: IR REFERENCE '%s' ----==\n",
         getRegionName());
    BBList * bbs = getBBList();
    ASSERT0(bbs);
    if (bbs->get_elem_count() != 0) {
        getMDSystem()->dump(false);
    }

    //Dump imported variables referenced.
    note(this, "\n==----==");
    MDSet * ru_maydef = getMayDef();
    if (ru_maydef != nullptr) {
        note(this, "\nRegionMayDef(OuterRegion):");
        ru_maydef->dump(getMDSystem(), true);
    }

    MDSet * ru_mayuse = getMayUse();
    if (ru_mayuse != nullptr) {
        note(this, "\nRegionMayUse(OuterRegion):");
        ru_mayuse->dump(getMDSystem(), true);
    }

    for (IRBB * bb = bbs->get_head(); bb != nullptr; bb = bbs->get_next()) {
        note(this, "\n--- BB%d ---", bb->id());
        dumpBBRef(bb, indent);
    }
}


void Region::dumpBBRef(IN IRBB * bb, UINT indent) const
{
    if (!isLogMgrInit()) { return; }
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        ir->dumpRef(const_cast<Region*>(this), indent);
    }
}


PassMgr * Region::initPassMgr()
{
    if (getAnalysisInstrument()->m_pass_mgr != nullptr) {
        return getAnalysisInstrument()->m_pass_mgr;
    }
    getAnalysisInstrument()->m_pass_mgr = allocPassMgr();
    return getAnalysisInstrument()->m_pass_mgr;
}


void Region::destroyPassMgr()
{
    if (getAnalysisInstrument() == nullptr ||
        ANA_INS_pass_mgr(getAnalysisInstrument()) == nullptr) {
        return;
    }
    delete ANA_INS_pass_mgr(getAnalysisInstrument());
    ANA_INS_pass_mgr(getAnalysisInstrument()) = nullptr;
}


static bool verifyMDRefForIR(IR const* ir, ConstIRIter & cii, Region * rg)
{
    for (IR const* t = iterInitC(ir, cii); t != nullptr; t = iterNextC(cii)) {
        switch (t->getCode()) {
        case IR_ID:
            //We do not need MD or MDSET information of IR_ID.
            //ASSERT0(t->getExactRef());
            ASSERT0(t->getRefMDSet() == nullptr);
            break;
        case IR_LD:
            if (g_is_support_dynamic_type) {
                ASSERTN(t->getEffectRef(), ("type is at least effect"));
                ASSERTN(!t->getEffectRef()->is_pr(),
                        ("MD can not present a PR."));
            } else {
                ASSERTN(t->getExactRef(), ("type must be exact"));
                ASSERTN(!t->getExactRef()->is_pr(),
                        ("MD can not present a PR."));
            }

            //MayUse of ld may not empty.
            //e.g: cvt(ld(id(x,i8)), i32) x has exact md4(size=1), and
            //an overlapped md5(size=4).

            if (t->getRefMDSet() != nullptr) {
                ASSERT0(rg->getMDSetHash()->find(*t->getRefMDSet()));
            }
            break;
        case IR_PR:
            if (g_is_support_dynamic_type) {
                ASSERTN(t->getEffectRef(), ("type is at least effect"));
                ASSERTN(t->getEffectRef()->is_pr(),
                        ("MD must present a PR."));
            } else {
                ASSERTN(t->getExactRef(), ("type must be exact"));
                ASSERTN(t->getExactRef()->is_pr(),
                        ("MD must present a PR."));
            }
            ASSERT0(t->getRefMDSet() == nullptr);
            break;
        case IR_STARRAY: {
            MD const* must = t->getEffectRef();
            MDSet const* may = t->getRefMDSet();
            DUMMYUSE(must);
            DUMMYUSE(may);
            ASSERT0(must || (may && !may->is_empty()));
            if (must != nullptr) {
                //PR can not be accessed by indirect operation.
                ASSERT0(!must->is_pr());
            }

            if (may != nullptr) {
                //PR can not be accessed by indirect operation.
                MDSetIter iter;
                for (INT i = may->get_first(&iter);
                     i >= 0; i = may->get_next(i, &iter)) {
                    MD const* x = rg->getMDSystem()->getMD(i);
                    DUMMYUSE(x);
                    ASSERT0(x && !x->is_pr());
                    ASSERT0(!x->get_base()->is_readonly());
                }
                ASSERT0(rg->getMDSetHash()->find(*may));
            }
            break;
        }
        case IR_ARRAY:
        case IR_ILD: {
            MD const* mustuse = t->getEffectRef();
            MDSet const* mayuse = t->getRefMDSet();
            DUMMYUSE(mustuse);
            DUMMYUSE(mayuse);
            ASSERT0(mustuse || (mayuse && !mayuse->is_empty()));
            if (mustuse != nullptr) {
                //PR can not be accessed by indirect operation.
                ASSERT0(!mustuse->is_pr());
            }

            if (mayuse != nullptr) {
                //PR can not be accessed by indirect operation.
                MDSetIter iter;
                for (INT i = mayuse->get_first(&iter);
                     i >= 0; i = mayuse->get_next(i, &iter)) {
                    MD const* x = rg->getMDSystem()->getMD(i);
                    DUMMYUSE(x);
                    ASSERT0(x && !x->is_pr());
                }
                ASSERT0(rg->getMDSetHash()->find(*mayuse));
            }
            break;
        }
        case IR_ST: {
            ASSERT0(!t->getRefMD()->get_base()->is_readonly());
            if (g_is_support_dynamic_type) {
                ASSERTN(t->getEffectRef(),
                        ("type is at least effect"));
                ASSERTN(!t->getEffectRef()->is_pr(),
                        ("MD can not present a PR."));
            } else {
                ASSERTN(t->getExactRef(), ("type must be exact"));
                ASSERTN(!t->getExactRef()->is_pr(),
                        ("MD can not present a PR."));
            }
            //ST may modify overlapped memory object.
            if (t->getRefMDSet() != nullptr) {
                ASSERT0(rg->getMDSetHash()->find(*t->getRefMDSet()));
            }
            break;
        }
        case IR_SETELEM:
            if (g_is_support_dynamic_type) {
                ASSERTN(t->getEffectRef(), ("type is at least effect"));
                ASSERTN(t->getEffectRef()->is_pr(),
                        ("MD must present a PR."));
            } else {
                MD const* md = t->getEffectRef();
                ASSERT0(md);
                if (!md->is_exact()) {
                    ASSERTN(md->is_range(), ("type must be range"));
                }
                ASSERTN(md->is_pr(), ("MD must present a PR."));
            }
            ASSERT0(t->getRefMDSet() == nullptr);
            break;
        case IR_STPR:
        case IR_GETELEM:
            if (g_is_support_dynamic_type) {
                ASSERTN(t->getEffectRef(), ("type is at least effect"));
                ASSERTN(t->getEffectRef()->is_pr(),
                        ("MD must present a PR."));
            } else {
                ASSERTN(t->getExactRef(), ("type must be exact"));
                ASSERTN(t->getExactRef()->is_pr(),
                        ("MD must present a PR."));
            }
            ASSERT0(t->getRefMDSet() == nullptr);
            break;
        case IR_IST: {
            MD const* mustdef = t->getRefMD();
            if (mustdef != nullptr) {
                //mustdef may be fake object, e.g: global memory.
                //ASSERT0(mustdef->is_effect());

                //PR can not be accessed by indirect operation.
                ASSERT0(!mustdef->is_pr());
            }

            MDSet const* maydef = t->getRefMDSet();
            ASSERT0(mustdef != nullptr ||
                    (maydef != nullptr && !maydef->is_empty()));
            if (maydef != nullptr) {
                //PR can not be accessed by indirect operation.
                MDSetIter iter;
                for (INT i = maydef->get_first(&iter);
                     i >= 0; i = maydef->get_next(i, &iter)) {
                    MD const* x = rg->getMDSystem()->getMD(i);
                    DUMMYUSE(x);
                    ASSERT0(x && !x->is_pr());
                }
                ASSERT0(rg->getMDSetHash()->find(*maydef));
            }
            break;
        }
        case IR_CALL:
        case IR_ICALL:
            if (t->getRefMDSet() != nullptr) {
                ASSERT0(rg->getMDSetHash()->find(*t->getRefMDSet()));
            }
            break;
        case IR_PHI:
            ASSERT0(t->getEffectRef() && t->getEffectRef()->is_pr());
            ASSERT0(t->getRefMDSet() == nullptr);
            break;
        SWITCH_CASE_BIN:
        SWITCH_CASE_UNA:
        //CVT should not have any reference. Even if the
        //operation will genrerate different type memory
        //accessing.
        case IR_CONST:
        case IR_LDA:
        case IR_SELECT:
        case IR_CASE:
        case IR_BREAK:
        case IR_CONTINUE:
        case IR_TRUEBR:
        case IR_FALSEBR:
        case IR_GOTO:
        case IR_IGOTO:
        case IR_SWITCH:
        case IR_RETURN:
        case IR_REGION:
            ASSERT0(t->getRefMD() == nullptr && t->getRefMDSet() == nullptr);
            break;
        default: ASSERTN(0, ("unsupport ir type"));
        }
    }
    return true;
}


//Verify MD reference to each stmts and expressions which described memory.
bool Region::verifyMDRef()
{
    ConstIRIter cii;
    BBList * bbl = getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            cii.clean();
            verifyMDRefForIR(ir, cii, this);
        }
    }
    return true;
}


//Ensure that each IR in ir_list must be allocated in current region.
bool Region::verifyIROwnership()
{
    IR const* ir = getIRList();
    if (ir == nullptr) { return true; }
    for (; ir != nullptr; ir = ir->get_next()) {
        ASSERTN(getIR(ir->id()) == ir,
                ("ir id:%d is not allocated in region %s", getRegionName()));
    }
    return true;
}


//Verify cond/uncond target label.
bool Region::verifyBBlist(BBList & bbl)
{
    LAB2BB lab2bb;
    for (IRBB * bb = bbl.get_head(); bb != nullptr; bb = bbl.get_next()) {
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != nullptr; li = bb->getLabelList().get_next()) {
            lab2bb.set(li, bb);
        }
    }

    for (IRBB * bb = bbl.get_head(); bb != nullptr; bb = bbl.get_next()) {
        IR * last = BB_last_ir(bb);
        if (last == nullptr) { continue; }

        if (last->isConditionalBr()) {
            ASSERTN(lab2bb.get(BR_lab(last)),
                    ("branch target cannot be nullptr"));
        } else if (last->isMultiConditionalBr()) {
            ASSERT0(last->is_switch());
            for (IR * c = SWITCH_case_list(last);
                 c != nullptr; c = c->get_next()) {
                ASSERTN(lab2bb.get(CASE_lab(last)),
                        ("case branch target cannot be nullptr"));
            }
            if (SWITCH_deflab(last) != nullptr) {
                ASSERTN(lab2bb.get(SWITCH_deflab(last)),
                        ("default target cannot be nullptr"));
            }
        } else if (last->isUnconditionalBr()) {
            if (last->is_goto()) {
                ASSERTN(lab2bb.get(GOTO_lab(last)), ("target cannot be nullptr"));
            } else {
                for (IR * caseexp = IGOTO_case_list(last); caseexp != nullptr;
                    caseexp = caseexp->get_next()) {
                    ASSERTN(lab2bb.get(CASE_lab(caseexp)),
                            ("target cannot be nullptr"));
                }
            }
        }
    }
    return true;
}


//Dump all MD that related to Var.
void Region::dumpVarMD(Var * v, UINT indent) const
{
    StrBuf buf(64);
    ConstMDIter iter;
    MDTab * mdtab = getMDSystem()->getMDTab(v);
    if (mdtab != nullptr) {
        MD const* x = mdtab->get_effect_md();
        if (x != nullptr) {
            prtIndent(this, indent);
            buf.clean();
            x->dump(buf, getTypeMgr());
            note(this, "\n%s", buf.buf);
        }

        OffsetTab * ofstab = mdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, nullptr);
                 md != nullptr; md = ofstab->get_next(iter, nullptr)) {
                prtIndent(this, indent);
                buf.clean();
                md->dump(buf, getTypeMgr());
                note(this, "\n%s", buf.buf);
            }
        }
    }
}


//Dump each Var in current region's Var table.
void Region::dumpVARInRegion() const
{
    if (!isLogMgrInit()) { return; }
    StrBuf buf(64);
    LogMgr * lm = getLogMgr();

    //Dump Region name.
    if (getRegionVar() != nullptr) {
        note(this, "\n==---- REGION(%d):%s:", id(), getRegionName());
        getRegionVar()->dumpVARDecl(buf);
        prt(this, "%s ----==", buf.buf);
    } else {
        note(this, "\n==---- REGION(%d): ----==", id());
    }

    Region * pthis = const_cast<Region*>(this);
    //Dump formal parameter list.
    if (is_function()) {
        bool has_param = false;
        VarTabIter c;
        for (Var * v = pthis->getVarTab()->get_first(c);
             v != nullptr; v = pthis->getVarTab()->get_next(c)) {
            if (VAR_is_formal_param(v)) {
                has_param = true;
                break;
            }
        }

        if (has_param) {
            note(this, "\nFORMAL PARAMETERS:");
            c.clean();
            Vector<Var*> fpvec;
            for (Var * v = pthis->getVarTab()->get_first(c);
                 v != nullptr; v = pthis->getVarTab()->get_next(c)) {
                if (VAR_is_formal_param(v)) {
                    fpvec.set(v->getFormalParamPos(), v);
                }
            }
            for (INT i = 0; i <= fpvec.get_last_idx(); i++) {
                Var * v = fpvec.get(i);
                if (v == nullptr) {
                    //This position may be reserved for other use.
                    //ASSERT0(v);
                    lm->incIndent(2);
                    note(this, "\n--");
                    prt(this, " param%d", i);
                    lm->decIndent(2);
                    continue;
                }
                buf.clean();
                v->dump(buf, getTypeMgr());
                lm->incIndent(2);
                note(this, "\n%s", buf.buf);
                prt(this, " param%d", i);
                lm->incIndent(2);
                dumpVarMD(v, lm->getIndent());
                lm->decIndent(2);
                lm->decIndent(2);
            }
        }
    }

    //Dump local varibles.
    VarTab * vt = pthis->getVarTab();
    if (vt->get_elem_count() > 0) {
        note(this, "\nVARIABLES:%d", vt->get_elem_count());
        lm->incIndent(2);
        VarTabIter c;

        //Sort Var in ascending order because test tools will compare
        //Var dump information and require all variable have to be ordered.
        xcom::List<Var*> varlst;
        for (Var * v = vt->get_first(c); v != nullptr; v = vt->get_next(c)) {
            C<Var*> * ct = nullptr;
            bool find = false;
            for (varlst.get_head(&ct); ct != nullptr;
                 ct = varlst.get_next(ct)) {
                Var * v_in_lst = C_val(ct);
                if (v_in_lst->id() > v->id()) {
                    varlst.insert_before(v, ct);
                    find = true;
                    break;
                }
            }
            if (!find) {
                varlst.append_tail(v);
            }
        }
        C<Var*> * ct = nullptr;
        for (varlst.get_head(&ct); ct != nullptr; ct = varlst.get_next(ct)) {
            Var * v = C_val(ct);
            buf.clean();
            v->dump(buf, getTypeMgr());
            note(this, "\n%s", buf.buf);
            lm->incIndent(2);
            dumpVarMD(v, lm->getIndent());
            lm->decIndent(2);
        }
        lm->decIndent(2);
    }
}


bool Region::partitionRegion()
{
    //----- DEMO CODE ----------
    IR * ir = getIRList();
    IR * start_pos = nullptr;
    IR * end_pos = nullptr;
    while (ir != nullptr) {
        if (ir->is_label()) {
            LabelInfo const* li = LAB_lab(ir);
            if (LABELINFO_type(li) == L_CLABEL &&
                strcmp(SYM_name(LABELINFO_name(li)), "REGION_START") == 0) {
                start_pos = ir;
                break;
            }
        }
        ir = ir->get_next();
    }
    if (ir == nullptr) return false;
    ir = ir->get_next();
    while (ir != nullptr) {
        if (ir->is_label()) {
            LabelInfo const* li = LAB_lab(ir);
            if (LABELINFO_type(li) == L_CLABEL &&
                strcmp(SYM_name(LABELINFO_name(li)), "REGION_END") == 0) {
                end_pos = ir;
                break;
            }
        }
        ir = ir->get_next();
    }
    if (start_pos == nullptr || end_pos == nullptr) return false;
    ASSERT0(start_pos != end_pos);
    //----------------

    //Generate IR region.
    Type const* type = getTypeMgr()->getMCType(0);
    Var * ruv = getVarMgr()->registerVar("inner_ru",
        type, 1, VAR_LOCAL|VAR_FAKE);
    VAR_is_unallocable(ruv) = true;
    addToVarTab(ruv);

    Region * inner_ru = getRegionMgr()->allocRegion(REGION_INNER);
    inner_ru->setRegionVar(ruv);
    IR * ir_ru = buildRegion(inner_ru);
    copyDbx(ir, ir_ru, inner_ru);
    //------------

    ir = IR_next(start_pos);
    while (ir != end_pos) {
        IR * t = ir;
        ir = ir->get_next();
        xcom::remove(&getAnalysisInstrument()->m_ir_list, t);
        IR * inner_ir = inner_ru->dupIRTree(t);
        freeIRTree(t);
        inner_ru->addToIRList(inner_ir);
    }
    xoc::dumpIRList(inner_ru->getIRList(), this);
    insertafter_one(&start_pos, ir_ru);
    xoc::dumpIRList(getIRList(), this);
    //-------------
    OptCtx oc;
    bool succ = REGION_ru(ir_ru)->process(&oc);
    ASSERT0(succ);
    DUMMYUSE(succ);

    xoc::dumpIRList(getIRList(), this);

    //Merger IR list in inner-region to outer region.
    //xcom::remove(&getAnalysisInstrument()->m_ir_list, ir_ru);
    //IR * head = inner_ru->constructIRlist();
    //insertafter(&split_pos, dupIRTreeList(head));
    //dumpIRList(getIRList(), this);

    delete inner_ru;
    return false;
}


//Check and rescan call list of region if one of elements in list changed.
void Region::updateCallAndReturnList(bool scan_inner_region)
{
    if (readCallList() == nullptr) { return; }
    UINT num_inner_region = 0;
    CIRList * clst = getCallList();
    if (clst->get_elem_count() == 0) { return; }

    CIRListIter ct;
    for (clst->get_head(&ct); ct != clst->end(); ct = clst->get_next(ct)) {
        IR const* c = ct->val();
        ASSERT0(c);
        if (!c->isCallStmt()) {
            //Call stmt has changed, then rescanning is needed.
            scanCallAndReturnList(num_inner_region, scan_inner_region);
            return;
        }
    }

    CIRList * retlst = getReturnList();
    if (retlst->get_elem_count() == 0) { return; }

    for (retlst->get_head(&ct);
         ct != retlst->end(); ct = retlst->get_next(ct)) {
        IR const* c = ct->val();
        ASSERT0(c);
        if (!c->is_return()) {
            //Return stmt has changed, then rescanning is needed.
            scanCallAndReturnList(num_inner_region, scan_inner_region);
            return;
        }
    }
}


bool Region::processBBList(OptCtx & oc)
{
    if (getBBList() == nullptr || getBBList()->get_elem_count() == 0) {
        return true;
    }

    START_TIMER(t, "PreScan");
    prescanBBList(getBBList());
    END_TIMER(t, "PreScan");
    if (g_is_dump_after_pass && g_dump_opt.isDumpALL()) {
        note(this, "\n==--- DUMP PRIMITIVE IRBB LIST ----==");
        dumpBBList();
    }

    return MiddleProcess(oc);
}


bool Region::processIRList(OptCtx & oc)
{
    if (getIRList() == nullptr) { return true; }

    START_TIMER(t, "PreScan");
    prescanIRList(getIRList());
    END_TIMER(t, "PreScan");
    if (g_is_dump_after_pass && g_dump_opt.isDumpALL()) {
        note(this, "\n==--- DUMP PRIMITIVE IR LIST ----==");
        dumpIRList();
    }
    if (!HighProcess(oc)) { return false; }
    ASSERT0(verifyMDDUChain(this));

    if (g_opt_level != OPT_LEVEL0) {
        //O0 does not build DU ref and DU chain.
        ASSERT0(verifyMDRef());
    }

    if (!MiddleProcess(oc)) { return false; }
    return true;
}


static void post_process(Region * rg, OptCtx * oc)
{
    PRSSAMgr * ssamgr = (PRSSAMgr*)rg->getPassMgr()->queryPass(
        PASS_PR_SSA_MGR);
    if (ssamgr != nullptr && ssamgr->is_valid()) {
        ssamgr->destruction(oc);
        rg->getPassMgr()->destroyPass(ssamgr);
    }

    MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->queryPass(
        PASS_MD_SSA_MGR);
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->destruction(oc);
        rg->getPassMgr()->destroyPass(mdssamgr);
    }

    if (!g_retain_pass_mgr_for_region) {
        rg->destroyPassMgr();
    }

    oc->set_all_invalid();
    rg->updateCallAndReturnList(true);
}


static void do_ipa(Region * rg, OptCtx * oc)
{
    if (!oc->is_callg_valid()) {
        //processFuncRegion has scanned and collected call-list.
        //Thus it does not need to scan call-list here.
        rg->getRegionMgr()->buildCallGraph(*oc, true, true);
    }

    if (oc->is_callg_valid()) {
        IPA * ipa = (IPA*)rg->getPassMgr()->registerPass(PASS_IPA);
        ipa->perform(*oc);
        rg->getPassMgr()->destroyPass(ipa);
    }
}


static void do_inline(Region * rg, OptCtx * oc)
{
    //Need to scan call-list.
    rg->getRegionMgr()->buildCallGraph(*oc, true, true);
    if (oc->is_callg_valid()) {
        Inliner * inl = (Inliner*)rg->getPassMgr()->registerPass(PASS_INLINER);
        inl->perform(*oc);
        rg->getPassMgr()->destroyPass(inl);
    }
}


//Return true if all passes finished normally, otherwise return false.
bool Region::process(OptCtx * oc)
{
    ASSERTN(oc, ("Need OptCtx"));
    ASSERT0(verifyIROwnership());
    if (getIRList() == nullptr && getBBList()->get_elem_count() == 0) {
        return true;
    }

    initPassMgr();
    if (g_do_inline && is_program()) {
        do_inline(this, oc);
    }

    getPassMgr()->registerPass(PASS_REFINE)->perform(*oc);
    if (getIRList() != nullptr) {
        if (!processIRList(*oc)) { goto ERR_RETURN; }
    } else {
        if (!processBBList(*oc)) { goto ERR_RETURN; }
    }

    if (g_do_ipa && is_program()) {
        do_ipa(this, oc);
    }

    post_process(this, oc);
    //oc->set_all_invalid();

    return true;

ERR_RETURN:
    post_process(this, oc);
    oc->set_all_invalid();
    return false;
}
//END Region

} //namespace xoc
