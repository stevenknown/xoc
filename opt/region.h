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
#ifndef __REGION_H__
#define __REGION_H__

namespace xoc {
class IPA;
class CfsMgr;
class IR_DU_MGR;
class IR_AA;
class IR_EXPR_TAB;

//Make sure IR_ICALL is the largest ir.
#define MAX_OFFSET_AT_FREE_TABLE    (sizeof(CICall) - sizeof(IR))

//Analysis Instrument.
//Record Data structure for IR analysis and transformation.
#define ANA_INS_ru_mgr(a)    ((a)->m_ru_mgr)
#define ANA_INS_pass_mgr(a)  ((a)->m_pass_mgr)
#define ANA_INS_var_tab(a)   ((a)->m_ru_var_tab)
class AnalysisInstrument {
public:
    Region * m_ru;
    UINT m_pr_count; //counter of IR_PR.

    SMemPool * m_pool;
    SMemPool * m_du_pool;

    //Indicate a list of IR.
    IR * m_ir_list;

    List<IR const*> * m_call_list; //record CALL/ICALL in region.

    RegionMgr * m_ru_mgr; //Region manager.
    PassMgr * m_pass_mgr; //PASS manager.

    //Record vars defined in current region,
    //include the subregion defined variables.
    //All LOCAL vars in the tab will be destroyed during region destruction.
    VarTab m_ru_var_tab;
    IR * m_free_tab[MAX_OFFSET_AT_FREE_TABLE + 1];
    Vector<VAR*> m_prno2var; //map prno to related VAR.
    Vector<IR*> m_ir_vector; //record IR which have allocated.
    BitSetMgr m_bs_mgr;
    DefMiscBitSetMgr m_sbs_mgr;
    MDSetMgr m_mds_mgr;
    MDSetHash m_mds_hash;
    List<DU*> m_free_du_list;
    IRBBMgr m_ir_bb_mgr; //Allocate the basic block.
    BBList m_ir_bb_list; //record a list of basic blocks.

    #ifdef _DEBUG_
    BitSet m_has_been_freed_irs;
    #endif
public:
    explicit AnalysisInstrument(Region * ru);
    COPY_CONSTRUCTOR(AnalysisInstrument);
    ~AnalysisInstrument();

    UINT count_mem();
    bool verify_var(VarMgr * vm, VAR * v);
};


//Region referrence info.
#define REF_INFO_maydef(ri)     ((ri)->may_def_mds)
#define REF_INFO_mustdef(ri)    ((ri)->must_def_mds)
#define REF_INFO_mayuse(ri)     ((ri)->may_use_mds)
class RefInfo {
public:
    MDSet must_def_mds; //Record the MD set for Region usage
    MDSet may_def_mds; //Record the MD set for Region usage
    MDSet may_use_mds; //Record the MD set for Region usage

    UINT count_mem()
    {
        UINT c = must_def_mds.count_mem();
        c += may_def_mds.count_mem();
        c += may_use_mds.count_mem();
        c += sizeof(MDSet*) * 3;
        return c;
    }
};


//
//START Region
//
//Record unique id in RegionMgr.
#define REGION_id(r)                    ((r)->id)
#define REGION_type(r)                  ((r)->m_ru_type)
#define REGION_parent(r)                ((r)->m_parent)

//Record analysis data structure for code region.
#define REGION_analysis_instrument(r)   ((r)->m_u1.m_ana_ins)

//Record the binary data for black box region.
#define REGION_blx_data(r)              ((r)->m_u1.m_blx_data)

//Set to true if Region is expected to be inlined.
#define REGION_is_expect_inline(r)      ((r)->m_u2.s1.is_expect_inline)

//Set to true if Region can be inlined.
#define REGION_is_inlinable(r)          ((r)->m_u2.s1.is_inlinable)

//True value means MustDef, MayDef, MayUse are available.
#define REGION_is_mddu_valid(r)         ((r)->m_u2.s1.is_du_valid)

//Record memory reference for region.
#define REGION_refinfo(r)               ((r)->m_ref_info)

//True if PR is unique which do not consider PR's data type.
#define REGION_is_pr_unique_for_same_number(r)    \
            ((r)->m_u2.s1.isPRUniqueForSameNo)

//True if region does not modify any memory and live-in variables which
//include VAR and PR.
//This property is very useful if region is a blackbox.
//And readonly region will alleviate the burden of optimizor.
#define REGION_is_readonly(r)            ((r)->m_u2.s1.is_readonly)

class Region {
friend class RegionMgr;
protected:
    union {
        AnalysisInstrument * m_ana_ins; //Analysis instrument.
        void * m_blx_data; //Black box data.
    } m_u1;

    void * xmalloc(UINT size)
    {
        ASSERT(REGION_analysis_instrument(this)->m_pool != NULL,
               ("pool does not initialized"));
        void * p = smpoolMalloc(size, REGION_analysis_instrument(this)->m_pool);
        ASSERT0(p != NULL);
        memset(p, 0, size);
        return p;
    }

public:
    RefInfo * m_ref_info; //record use/def info if Region has.
    REGION_TYPE m_ru_type; //region type.
    UINT id; //region unique id.
    VAR * m_var; //record VAR if RU has.
    Region * m_parent; //record parent region.
    union {
        struct {
            BYTE is_expect_inline:1; //see above macro declaration.
            BYTE is_du_valid:1;  //see above macro declaration.

            /* Set to true if there is single map between
            PR and MD. If PR may corresponde to multiple
            MD, set it to false.

            e.g: If one use same pr with different type U8 and U32,
            there will have two mds refer to pr1(U8) and pr1(U32),
            MD2->pr1(U8), MD8->pr1(U32). */
            BYTE isPRUniqueForSameNo:1;

            //True if region does not modify any live-in variables
            //which include VAR and PR. We say the region is readonly.
            BYTE is_readonly:1;

            //True if region can be inlined. Default is false for
            //conservative purpose.
            BYTE is_inlinable:1;
        } s1;
        BYTE s1b1;
    } m_u2;

public:
    explicit Region(REGION_TYPE rt, RegionMgr * rm) { init(rt, rm); }
    COPY_CONSTRUCTOR(Region);
    virtual ~Region() { destroy(); }

    //Add var which used inside current or inner Region.
    //Once the region destructing, all local vars are deleted.
    void addToVarTab(VAR * v)
    { REGION_analysis_instrument(this)->m_ru_var_tab.append(v); }

    //Add irs into IR list of current region.
    void addToIRList(IR * irs)
    { add_next(&REGION_analysis_instrument(this)->m_ir_list, irs); }

    //The function generates new MD for given pr.
    //It should be called if new PR generated in optimzations.
    inline MD const* allocRefForPR(IR * pr)
    {
        MD const* md = genMDforPR(pr);
        pr->set_ref_md(md, this);
        pr->cleanRefMDSet();
        return md;
    }

    inline DU * allocDU()
    {
        DU * du = REGION_analysis_instrument(this)->m_free_du_list.remove_head();
        if (du == NULL) {
            du = (DU*)smpoolMallocConstSize(sizeof(DU),
                            REGION_analysis_instrument(this)->m_du_pool);
            memset(du, 0, sizeof(DU));
        }
        return du;
    }

    IR * buildContinue();
    IR * buildBreak();
    IR * buildCase(IR * casev_exp, LabelInfo const* case_br_lab);
    IR * buildDoLoop(IR * det, IR * init, IR * step, IR * loop_body);
    IR * buildDoWhile(IR * det, IR * loop_body);
    IR * buildWhileDo(IR * det, IR * loop_body);
    IR * buildIf(IR * det, IR * true_body, IR * false_body);
    IR * buildSwitch(
            IR * vexp,
            IR * case_list,
            IR * body,
            LabelInfo const* default_lab);
    IR * buildPRdedicated(UINT prno, Type const* type);
    IR * buildPR(Type const* type);
    UINT buildPrno(Type const* type);
    IR * buildBranch(bool is_true_br, IR * det, LabelInfo const* lab);
    IR * buildId(IN VAR * var_info);
    IR * buildIlabel();
    IR * buildLabel(LabelInfo const* li);
    IR * buildCvt(IR * exp, Type const* tgt_ty);
    IR * buildGoto(LabelInfo const* li);
    IR * buildIgoto(IR * vexp, IR * case_list);
    IR * buildPointerOp(IR_TYPE irt, IR * lchild, IR * rchild);
    IR * buildCmp(IR_TYPE irt, IR * lchild, IR * rchild);
    IR * buildJudge(IR * exp);
    IR * buildBinaryOpSimp(
            IR_TYPE irt,
            Type const* type,
            IR * lchild,
            IR * rchild);
    IR * buildBinaryOp(IR_TYPE irt, Type const* type,
                       IN IR * lchild, IN IR * rchild);
    IR * buildUnaryOp(IR_TYPE irt, Type const* type, IN IR * opnd);
    IR * buildLogicalNot(IR * opnd0);
    IR * buildLogicalOp(IR_TYPE irt, IR * opnd0, IR * opnd1);
    IR * buildImmInt(HOST_INT v, Type const* type);
    IR * buildImmFp(HOST_FP fp, Type const* type);
    IR * buildLda(IR * lda_base);
    IR * buildLoad(VAR * var, Type const* type);
    IR * buildLoad(VAR * var);
    IR * buildIload(IR * base, Type const* type);
    IR * buildIload(IR * base, UINT ofst, Type const* type);
    IR * buildStore(VAR * lhs, IR * rhs);
    IR * buildStore(VAR * lhs, Type const* type, IR * rhs);
    IR * buildStore(VAR * lhs, Type const* type, UINT ofst, IR * rhs);
    IR * buildStorePR(UINT prno, Type const* type, IR * rhs);
    IR * buildStorePR(Type const* type, IR * rhs);
    IR * buildIstore(IR * lhs, IR * rhs, Type const* type);
    IR * buildIstore(IR * lhs, IR * rhs, UINT ofst, Type const* type);
    IR * buildString(SYM * strtab);
    IR * buildStoreArray(
            IR * base,
            IR * sublist,
            Type const* type,
            Type const* elemtype,
            UINT dims,
            TMWORD const* elem_num,
            IR * rhs);
    IR * buildArray(
            IR * base,
            IR * sublist,
            Type const* type,
            Type const* elemtype,
            UINT dims,
            TMWORD const* elem_num);
    IR * buildReturn(IR * ret_exp);
    IR * buildSelect(IR * det, IR * true_exp, IR * false_exp, Type const* type);
    IR * buildPhi(UINT prno, Type const* type, UINT num_opnd);
    IR * buildRegion(Region * ru);
    IR * buildIcall(
            IR * callee,
            IR * param_list,
            UINT result_prno = 0,
            Type const* type = NULL);
    IR * buildCall(
            VAR * callee,
            IR * param_list,
            UINT result_prno = 0,
            Type const* type = NULL);
    IR * constructIRlist(bool clean_ir_list);
    void constructIRBBlist();
    HOST_INT calcIntVal(IR_TYPE ty, HOST_INT v0, HOST_INT v1);
    double calcFloatVal(IR_TYPE ty, double v0, double v1);
    UINT count_mem();
    void checkValidAndRecompute(OptCTX * oc, ...);

    virtual void destroy();
    void destroyPassMgr();
    IR * dupIR(IR const* ir);
    IR * dupIRTree(IR const* ir);
    IR * dupIRTreeList(IR const* ir);

    void dump_all_stmt();
    void dump_all_ir();
    void dumpVARInRegion(INT indent = 0);
    void dump_var_md(VAR * v, UINT indent);
    void dump_free_tab();
    void dump_mem_usage(FILE * h);
    void dump_bb_usage(FILE * h);

    void freeIR(IR * ir);
    void freeIRTree(IR * ir);
    void freeIRTreeList(IR * ir);
    void freeIRTreeList(IRList & irs);
    IR * foldConstFloatUnary(IR * ir, bool & change);
    IR * foldConstFloatBinary(IR * ir, bool & change);
    IR * foldConstIntUnary(IR * ir, bool & change);
    IR * foldConstIntBinary(IR * ir, bool & change);
    IR * foldConst(IR * ir, bool & change);

    UINT get_irt_size(IR * ir)
    {
        #ifdef CONST_IRT_SZ
        return IR_irt_size(ir);
        #else
        return IRTSIZE(IR_code(ir));
        #endif
    }
    SMemPool * get_pool() { return REGION_analysis_instrument(this)->m_pool; }
    MDSystem * get_md_sys() { return get_region_mgr()->get_md_sys(); }
    TypeMgr * get_type_mgr() const { return get_region_mgr()->get_type_mgr(); }
    TargInfo * get_targ_info() const
    { return get_region_mgr()->get_targ_info(); }

    UINT get_pr_count() const
    { return REGION_analysis_instrument(this)->m_pr_count; }

    VAR * get_ru_var() const { return m_var; }

    inline RegionMgr * get_region_mgr() const
    {
        ASSERT0(is_function() || is_program());
        ASSERT0(REGION_analysis_instrument(this));
        return ANA_INS_ru_mgr(REGION_analysis_instrument(this));
    }

    IR * get_ir_list() const
    { return REGION_analysis_instrument(this)->m_ir_list; }

    VarMgr * get_var_mgr() const { return get_region_mgr()->get_var_mgr(); }

    VarTab * get_var_tab()
    { return &REGION_analysis_instrument(this)->m_ru_var_tab; }

    BitSetMgr * get_bs_mgr()
    { return &REGION_analysis_instrument(this)->m_bs_mgr; }

    DefMiscBitSetMgr * getMiscBitSetMgr()
    { return &REGION_analysis_instrument(this)->m_sbs_mgr; }

    MDSetMgr * get_mds_mgr()
    { return &REGION_analysis_instrument(this)->m_mds_mgr; }

    BBList * get_bb_list()
    { return &REGION_analysis_instrument(this)->m_ir_bb_list; }

    IRBBMgr * get_bb_mgr()
    { return &REGION_analysis_instrument(this)->m_ir_bb_mgr; }

    //Get MDSetHash.
    MDSetHash * get_mds_hash()
    { return &REGION_analysis_instrument(this)->m_mds_hash; }

    //Return IR pointer via the unique IR_id.
    inline IR * get_ir(UINT irid)
    {
        ASSERT0(REGION_analysis_instrument(this)->m_ir_vector.get(irid));
        return REGION_analysis_instrument(this)->m_ir_vector.get(irid);
    }

    //Return the vector that record all allocated IRs.
    Vector<IR*> * get_ir_vec()
    { return &REGION_analysis_instrument(this)->m_ir_vector; }

    //Return PassMgr.
    PassMgr * get_pass_mgr() const
    { return REGION_analysis_instrument(this)->m_pass_mgr; }

    //Return IR_CFG.
    IR_CFG * get_cfg() const
    {
        return get_pass_mgr() != NULL ?
                   (IR_CFG*)get_pass_mgr()->query_opt(PASS_CFG) : NULL;
    }

    IR_AA * get_aa() const
    {
        return get_pass_mgr() != NULL ?
                (IR_AA*)get_pass_mgr()->query_opt(PASS_AA) : NULL;
    }

    //Return DU info manager.
    IR_DU_MGR * get_du_mgr() const
    {
        return get_pass_mgr() != NULL ?
                (IR_DU_MGR*)get_pass_mgr()->query_opt(PASS_DU_MGR) : NULL;
    }

    CHAR const* get_ru_name() const;
    Region * get_func_unit();

    //Allocate and return a list of call, that indicate all IR_CALL in
    //current Region.
    inline List<IR const*> * get_call_list()
    {
        if (REGION_analysis_instrument(this)->m_call_list == NULL) {
            REGION_analysis_instrument(this)->m_call_list =
                                        new List<IR const*>();
        }
        return REGION_analysis_instrument(this)->m_call_list;
    }

    //Compute the most conservative MD reference information.
    void genDefaultRefInfo();

    //Get the MustUse of Region.
    inline MDSet * get_must_def()
    {
        if (m_ref_info != NULL) { return &REF_INFO_mustdef(m_ref_info); }
        return NULL;
    }

    //Get the MayDef of Region.
    inline MDSet * get_may_def()
    {
        if (m_ref_info != NULL) { return &REF_INFO_maydef(m_ref_info); }
        return NULL;
    }

    //Get the MayUse of Region.
    inline MDSet * get_may_use()
    {
        if (m_ref_info != NULL) { return &REF_INFO_mayuse(m_ref_info); }
        return NULL;
    }

    Region * getTopRegion()
    {
        Region * ru = this;
        while (REGION_parent(ru) != NULL) {
            ru = REGION_parent(ru);
        }
        return ru;
    }

    //Allocate a internal LabelInfo that is not declared by compiler user.
    inline LabelInfo * genIlabel()
    {
        LabelInfo * li = genIlabel(RM_label_count(get_region_mgr()));
        RM_label_count(get_region_mgr())++;
        return li;
    }

    //Allocate a LabelInfo accroding to given 'labid'.
    LabelInfo * genIlabel(UINT labid)
    {
        ASSERT0(labid <= RM_label_count(get_region_mgr()));
        LabelInfo * li = newInternalLabel(get_pool());
        LABEL_INFO_num(li) = labid;
        return li;
    }

    //Allocate MD for IR_PR.
    MD const* genMDforPR(UINT prno, Type const* type);

    //Generate MD corresponding to PR load or write.
    MD const* genMDforPR(IR const* ir)
    {
        ASSERT0(ir->is_write_pr() || ir->is_read_pr() || ir->is_calls_stmt());
        return genMDforPR(ir->get_prno(), IR_dt(ir));
    }

    //Generate MD for VAR.
    MD const* genMDforVar(VAR * var, Type const* type)
    {
        ASSERT0(var && type);
        MD md;
        MD_base(&md) = var;

        if (type->is_void()) {
            MD_ty(&md) = MD_UNBOUND;
        } else {
            MD_size(&md) = get_type_mgr()->get_bytesize(type);
            MD_ty(&md) = MD_EXACT;
        }

        MD const* e = get_md_sys()->registerMD(md);
        ASSERT0(MD_id(e) > 0);
        return e;
    }

    //Generate MD for IR_ST.
    MD const* genMDforStore(IR const* ir)
    {
        ASSERT0(ir->is_st());
        return genMDforVar(ST_idinfo(ir), IR_dt(ir));
    }

    //Generate MD for IR_LD.
    MD const* genMDforLoad(IR const* ir)
    {
        ASSERT0(ir->is_ld());
        return genMDforVar(LD_idinfo(ir), IR_dt(ir));
    }

    //Generate MD for IR_ID.
    MD const* genMDforId(IR const* ir)
    {
        ASSERT0(ir->is_id());
        return genMDforVar(ID_info(ir), IR_dt(ir));
    }

    //Return the tyid for array index, the default is unsigned 32bit.
    inline Type const* getTargetMachineArrayIndexType()
    {
        return get_type_mgr()->getSimplexTypeEx(get_type_mgr()->
                    get_uint_dtype(WORD_LENGTH_OF_TARGET_MACHINE));
    }

    //Perform high level optmizations.
    virtual bool HighProcess(OptCTX & oc);

    //Initialze Region.
    void init(REGION_TYPE rt, RegionMgr * rm);

    void initRefInfo()
    {
        if (m_ref_info != NULL) { return; }
        m_ref_info = (RefInfo*)xmalloc(sizeof(RefInfo));
    }

    //Allocate and initialize pass manager.
    PassMgr * initPassMgr();

    //Invert condition for relation operation.
    virtual void invertCondition(IR ** cond);

    bool isSafeToOptimize(IR const* ir);

    //Check and insert data type CVT if it is necessary.
     IR * insertCvt(IR * parent, IR * kid, bool & change);
    void insertCvtForBinaryOp(IR * ir, bool & change);

    //Return true if the tree height is not great than 2.
    //e.g: tree a + b is lowest height , but a + b + c is not.
    //Note that if ARRAY or ILD still not be lowered at the moment, regarding
    //it as a whole node. e.g: a[1][2] + b is the lowest height.
    bool is_lowest_heigh(IR const* ir) const;
    bool is_lowest_heigh_exp(IR const* ir) const;

    //Return true if Region name is equivalent to 'n'.
    bool isRegionName(CHAR const* n) const
    { return strcmp(get_ru_name(), n) == 0; }

    bool is_undef() const { return REGION_type(this) == RU_UNDEF; }
    bool is_function() const { return REGION_type(this) == RU_FUNC; }
    bool is_blackbox() const { return REGION_type(this) == RU_BLX; }
    bool is_program() const { return REGION_type(this) == RU_PROGRAM; }
    bool is_subregion() const { return REGION_type(this) == RU_SUB; }
    bool is_eh() const { return REGION_type(this) == RU_EH; }

    //Perform middle level IR optimizations which are implemented
    //accroding to control flow info and data flow info.
    virtual bool MiddleProcess(OptCTX & oc);

    //Map from prno to related VAR.
    VAR * mapPR2Var(UINT prno)
    { return REGION_analysis_instrument(this)->m_prno2var.get(prno); }

    //Allocate PassMgr
    virtual PassMgr * allocPassMgr();

    //Allocate AliasAnalysis.
    virtual IR_AA * allocAliasAnalysis();

    //Allocate IRBB.
    IRBB * newBB();

    //Allocate an IR.
    IR * newIR(IR_TYPE irt);

    //Allocate AttachInfo.
    inline AttachInfo * newAI()
    {
        AttachInfo * ai = (AttachInfo*)xmalloc(sizeof(AttachInfo));
        ASSERT0(ai);
        ai->init();
        return ai;
    }

    //Peephole optimizations.
    IR * refineBand(IR * ir, bool & change);
    IR * refineBor(IR * ir, bool & change);
    IR * refineCvt(IR * ir, bool & change, RefineCTX & rc);
    IR * refineLand(IR * ir, bool & change);
    IR * refineLor(IR * ir, bool & change);
    IR * refineXor(IR * ir, bool & change);
    IR * refineAdd(IR * ir, bool & change);
    IR * refineSub(IR * ir, bool & change);
    IR * refineMul(IR * ir, bool & change, RefineCTX & rc);
    IR * refineRem(IR * ir, bool & change);
    IR * refineDiv(IR * ir, bool & change, RefineCTX & rc);
    IR * refineNe(IR * ir, bool & change, RefineCTX & rc);
    IR * refineEq(IR * ir, bool & change, RefineCTX & rc);
    IR * refineMod(IR * ir, bool & change);
    IR * refineCall(IR * ir, bool & change, RefineCTX & rc);
    IR * refineIcall(IR * ir, bool & change, RefineCTX & rc);
    IR * refineSwitch(IR * ir, bool & change, RefineCTX & rc);
    IR * refineReturn(IR * ir, bool & change, RefineCTX & rc);
    IR * refinePhi(IR * ir, bool & change, RefineCTX & rc);
    IR * refineBr(IR * ir, bool & change, RefineCTX & rc);
    IR * refineSelect(IR * ir, bool & change, RefineCTX & rc);
    IR * refineBranch(IR * ir);
    IR * refineArray(IR * ir, bool & change, RefineCTX & rc);
    IR * refineNeg(IR * ir, bool & change);
    IR * refineNot(IR * ir, bool & change, RefineCTX & rc);
    IR * refineBinaryOp(IR * ir, bool & change, RefineCTX & rc);
    IR * refineLda(IR * ir, bool & change, RefineCTX & rc);
    IR * refineLoad(IR * ir);
    IR * refineIload1(IR * ir, bool & change);
    IR * refineIload2(IR * ir, bool & change);
    IR * refineIload(IR * ir, bool & change, RefineCTX & rc);
    IR * refineDetViaSSAdu(IR * ir, bool & change);
    IR * refineDet(IR * ir_list, bool & change, RefineCTX & rc);
    IR * refineStore(IR * ir, bool & change, RefineCTX & rc);
    IR * refineStoreArray(IR * ir, bool & change, RefineCTX & rc);
    IR * refineIstore(IR * ir, bool & change, RefineCTX & rc);
    IR * refineIR(IR * ir, bool & change, RefineCTX & rc);
    IR * refineIRlist(IR * ir_list, bool & change, RefineCTX & rc);
    bool refineStmtList(IN OUT BBIRList & ir_list, RefineCTX & rc);
    bool refineBBlist(IN OUT BBList * ir_bb_list, RefineCTX & rc);
    IR * reassociation(IR * ir, bool & change);
    bool reconstructBBlist(OptCTX & oc);

    C<IRBB*> * splitIRlistIntoBB(IR * irs, BBList * bbl, C<IRBB*> * ctbb);
    IR * simplifyKids(IR * ir, SimpCTX * cont);
    IR * simplifyStore(IR * ir, SimpCTX * cont);
    IR * simplifyStorePR(IR * ir, SimpCTX * cont);
    IR * simplifyStoreArray(IR * ir, SimpCTX * ctx);
    IR * simplifySetelem(IR * ir, SimpCTX * ctx);
    IR * simplifyGetelem(IR * ir, SimpCTX * ctx);
    IR * simplifyIstore(IR * ir, SimpCTX * cont);
    IR * simplifyCall(IR * ir, SimpCTX * cont);
    IR * simplifyIf (IR * ir, SimpCTX * cont);
    IR * simplifyWhileDo(IR * ir, SimpCTX * cont);
    IR * simplifyDoWhile (IR * ir, SimpCTX * cont);
    IR * simplifyDoLoop(IR * ir, SimpCTX * cont);
    IR * simplifyDet(IR * ir, SimpCTX * cont);
    IR * simplifyJudgeDet(IR * ir, SimpCTX * cont);
    IR * simplifySelect(IR * ir, SimpCTX * cont);
    IR * simplifySwitch (IR * ir, SimpCTX * cont);
    IR * simplifyIgoto(IR * ir, SimpCTX * cont);
    IR * simplifyArrayAddrExp(IR * ir, SimpCTX * cont);
    IR * simplifyArray(IR * ir, SimpCTX * cont);
    IR * simplifyExpression(IR * ir, SimpCTX * cont);
    IR * simplifyStmt(IR * ir, SimpCTX * cont);
    IR * simplifyStmtList(IR * ir, SimpCTX * cont);
    void simplifyBB(IRBB * bb, SimpCTX * cont);
    void simplifyBBlist(BBList * bbl, SimpCTX * cont);
    IR * simplifyLogicalNot(IN IR * ir, SimpCTX * cont);
    IR * simplifyLogicalOrAtFalsebr(IN IR * ir, LabelInfo const* tgt_label);
    IR * simplifyLogicalOrAtTruebr(IN IR * ir, LabelInfo const* tgt_label);
    IR * simplifyLogicalOr(IN IR * ir, SimpCTX * cont);
    IR * simplifyLogicalAndAtTruebr(IN IR * ir, LabelInfo const* tgt_label);
    IR * simplifyLogicalAndAtFalsebr(IN IR * ir, LabelInfo const* tgt_label);
    IR * simplifyLogicalAnd(IN IR * ir, SimpCTX * cont);
    IR * simplifyLogicalDet(IR * ir, SimpCTX * cont);
    IR * simplifyLda(IR * ir, SimpCTX * cont);
    void set_irt_size(IR * ir, UINT)
    {
        #ifdef CONST_IRT_SZ
        IR_irt_size(ir) = irt_sz;
        #else
        UNUSED(ir);
        #endif
    }
    void setMapPR2Var(UINT prno, VAR * pr_var)
    { REGION_analysis_instrument(this)->m_prno2var.set(prno, pr_var); }

    void set_ru_var(VAR * v) { m_var = v; }
    void set_ir_list(IR * irs) { REGION_analysis_instrument(this)->m_ir_list = irs; }
    void set_blx_data(void * d) { REGION_blx_data(this) = d; }
    IR * StrengthReduce(IN OUT IR * ir, IN OUT bool & change);
    void scanCallList(OUT List<IR const*> & call_list);

    void lowerIRTreeToLowestHeight(OptCTX & oc);

    void prescan(IR const* ir);
    bool partitionRegion();
    virtual void process(); //Entry to process region-unit.

    bool verifyBBlist(BBList & bbl);
    bool verifyIRinRegion();
};
//END Region

} //namespace xoc
#endif
