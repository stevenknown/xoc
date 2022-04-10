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
class DUMgr;
class AliasAnalysis;
class ExprTab;
class MDSSAMgr;
class PRSSAMgr;
class Pass;
class PassMgr;
class IRSimp;
class CDG;

//Region MD referrence info.
#define REF_INFO_maydef(ri) ((ri)->may_def_mds)
#define REF_INFO_mayuse(ri) ((ri)->may_use_mds)
class RefInfo {
    COPY_CONSTRUCTOR(RefInfo);
public:
    MDSet * may_def_mds; //Record the MD set for Region usage
    MDSet * may_use_mds; //Record the MD set for Region usage
public:
    //Count memory usage for current object.
    size_t count_mem()
    {
        size_t c = sizeof(RefInfo);
        return c;
    }
};


//
//START Region
//
//Record unique id in RegionMgr.
#define REGION_id(r) ((r)->m_id)
#define REGION_type(r) ((r)->m_rg_type)
#define REGION_parent(r) ((r)->m_parent)
#define REGION_region_mgr(r) ((r)->m_region_mgr)

//Set to true if Region is expected to be inlined.
#define REGION_is_expect_inline(r) ((r)->m_u2.s1.is_expect_inline)

//Set to true if Region can be inlined.
#define REGION_is_inlinable(r) ((r)->m_u2.s1.is_inlinable)

//True value means MustDef, MayDef, MayUse are available.
#define REGION_is_ref_valid(r) ((r)->m_u2.s1.is_ref_valid)

//True if region does not modify any memory and live-in variables which
//include Var and PR.
//This property is very useful if region is a blackbox.
//And readonly region will alleviate the burden of optimizor.
#define REGION_is_readonly(r) ((r)->m_u2.s1.is_readonly)

//Record memory reference for region.
#define REGION_refinfo(r) ((r)->m_ref_info)

//This class is base class to describe Region.
//A program organized in a set of region, each region has it own IR stmt
//list, local Var table, and many kinds of analysis and transformation
//modules.
//
//The kind of region can be blackbox, subregion, exception handling,
//function unit, and program unit.
//* blackbox
//  The region is a black box that attached customized data.
//  Black box is a single entry, single exit region.
//
//* subregion
//  This kind of region serves as a unit of compilation. Subregion
//  can be nested one inside another, which contains a list of IR.
//  Subregion is single entry, multiple exits region. The different
//  between subregion and function unit is subregion has neither
//  epilog and prolog part of function, nor the landing part of
//  exception handling. In general, subregions should be compiled
//  inside-out. An additional use of this node is to specify a region
//  to be parallelized.
//
//* exception handling
//  It is region that represent exception handler and try block.
//
//* function unit
//  The region represent normal function unit.
//  Function unit may be multiple entries, and multiple exits.
//  Most of optimizations apply on function unit. In general, the
//  compilation of region unit include following phases:
//    * Prescan IR list to specify which variable is taken address.
//    * High level process to perform miscellaneous high
//      level IR transformations accroding to syntactics analysis.
//      This process is also responsible to simpify high level IR
//      down to low level, and construct basic blocks.
//    * Both high level and middle level will perform these passes,
//      * Build control flow graph.
//      * Flow sensitive or insensitive alias analysis.
//      * Def-Use analysis.
//      * Control dependence analysis.
//    * Middle level optimizations are all based on low level IR,
//      such as copy propagation, dead code elimination, global value
//      numbering, and global common subexpression elimination.
//
//* program unit
//  The region represent all program unit. A program unit contains a
//  number of function units.
//  Program unit is single entry and multiple exits region.
//  XOC apply interprocedural analysis and optimization and inlining
//  on program unit.
class Region {
    friend class RegionMgr;
    COPY_CONSTRUCTOR(Region);
protected:
    //Record the binary data for black box region.
    #define REGION_blackbox_data(r) ((r)->m_u1.m_blx_data)
    //Record analysis data structure for code region.
    #define REGION_analysis_instrument(r) ((r)->m_u1.m_ana_ins)
    union {
        AnalysisInstrument * m_ana_ins; //Analysis instrument.
        void * m_blx_data; //Black box data.
    } m_u1;

    //Record vars defined in current region, include the subregion defined
    //variables. All LOCAL vars in the tab will be destroyed at region
    //destruction.
    VarTab m_rg_var_tab;
    SMemPool * m_pool;
protected:
    //Allocate PassMgr
    virtual PassMgr * allocPassMgr();

    //Allocate AttachInfoMgr
    virtual AttachInfoMgr * allocAttachInfoMgr();

    //Generate IR, invoke freeIR() or freeIRTree() if it is useless.
    //NOTE: Do NOT invoke ::free() to free IR, because all
    //    IR are allocated in the pool.
    IR * allocIR(IR_TYPE irt);

    void doBasicAnalysis(OptCtx & oc);

    AnalysisInstrument * getAnalysisInstrument() const;

    void HighProcessImpl(OptCtx & oc);

    void scanCallListImpl(OUT UINT & num_inner_region, IR * irlst,
                          OUT List<IR const*> * call_list,
                          OUT List<IR const*> * ret_list,
                          bool scan_inner_region);

    virtual void postSimplify(SimpCtx const& simp, MOD OptCtx & oc);
    bool processIRList(OptCtx & oc);
    bool processBBList(OptCtx & oc);
    void prescanIRList(IR const* ir);
    void prescanBBList(BBList const* bblst);
    bool partitionRegion();
    bool performSimplify(OptCtx & oc);
public:
    REGION_TYPE m_rg_type; //region type.
    UINT m_id; //region unique id.
    Var * m_var; //record Var if RU has.
    Region * m_parent; //record parent region.
    RegionMgr * m_region_mgr; //Region manager.
    RefInfo * m_ref_info; //record USE/DEF MD info to current region.
    union {
        struct {
            BYTE is_expect_inline:1; //see above macro declaration.
            BYTE is_ref_valid:1;  //see above macro declaration.

            //True if region does not modify any live-in variables
            //which include Var and PR. We say the region is readonly.
            BYTE is_readonly:1;

            //True if region can be inlined. Default is false for
            //conservative purpose.
            BYTE is_inlinable:1;
        } s1;
        BYTE s1b1;
    } m_u2;
public:
    explicit Region(REGION_TYPE rt, RegionMgr * rm) { init(rt, rm); }
    virtual ~Region() { destroy(); }

    //Add var which used inside current or inner Region.
    //Once the region destructing, all local vars are deleted.
    void addToVarTab(Var * v) { m_rg_var_tab.append(v); }

    //Add irs into IR list of current region.
    void addToIRList(IR * irs)
    { xcom::add_next(&ANA_INS_ir_list(getAnalysisInstrument()), irs); }

    //The function generates new MD for all operations to PR.
    //It should be called if new PR generated in optimzations.
    inline MD const* allocRefForPR(IR * pr)
    {
        MD const* md = getMDMgr()->genMDForPR(pr);
        pr->setRefMD(md, this);
        pr->cleanRefMDSet();
        return md;
    }

    //The function generates new MD for given LD.
    //It should be called if LD generated in optimzations.
    inline MD const* allocRefForLoad(IR * ld)
    {
        MD const* md = getMDMgr()->genMDForLoad(ld);
        ld->setRefMD(md, this);

        //Do NOT clean MDSet because transformation may combine ILD(LDA)
        //into LD and carry MDSet from ILD.
        //ld->cleanRefMDSet();
        return md;
    }

    //The function generates new MD for given ST.
    //It should be called if new PR generated in optimzations.
    inline MD const* allocRefForStore(IR * st)
    {
        MD const* md = getMDMgr()->genMDForStore(st);
        st->setRefMD(md, this);

        //Do NOT clean MDSet because transformation may combine IST(LDA)
        //into ST and carry MDSet from IST.
        //st->cleanRefMDSet();
        return md;
    }

    //The function generates new MD for given ST.
    //It should be called if new PR generated in optimzations.
    inline MD const* allocRefForId(IR * id)
    {
        MD const* md = getMDMgr()->genMDForId(id);
        id->setRefMD(md, this);
        id->cleanRefMDSet();
        return md;
    }

    inline MD const* allocRef(IR * ir)
    {
        switch (ir->getCode()) {
        case IR_PR: return allocRefForPR(ir);
        case IR_LD: return allocRefForLoad(ir);
        case IR_ST: return allocRefForStore(ir);
        case IR_ID: return allocRefForId(ir);
        default: UNREACHABLE();
        }
        return nullptr;
    }

    //Allocate DU reference that describes memory reference to IR.
    inline DU * allocDU()
    {
        DU * du = ANA_INS_free_du_list(getAnalysisInstrument()).remove_head();
        if (du == nullptr) {
            du = (DU*)smpoolMallocConstSize(sizeof(DU),
                ANA_INS_du_pool(getAnalysisInstrument()));
            ::memset(du, 0, sizeof(DU));
        }
        return du;
    }

    //Allocate basic block.
    IRBB * allocBB() { return getBBMgr()->allocBB(); }

    //Allocate AIContainer that describes attach info to IR.
    AIContainer * allocAIContainer()
    {
        ASSERT0(getAnalysisInstrument()->getAttachInfoMgr());
        return getAnalysisInstrument()->getAttachInfoMgr()->allocAIContainer();
    }

    //Note the returned ByteBuf does not need to free by user.
    ByteBuf * allocByteBuf(UINT bytesize)
    {
        ByteBuf * buf = (ByteBuf*)xmalloc(sizeof(ByteBuf));
        BYTEBUF_size(buf) = bytesize;
        BYTEBUF_buffer(buf) = (BYTE*)xmalloc(bytesize);
        return buf;
    }

    //Build store operation to store 'rhs' to store value to be one of the
    //element of a PR.
    //type: data type of targe pr.
    //offset: byte offset to the start of result PR.
    //rhs: value expected to store.
    IR * buildSetElem(Type const* type, IR * base, IR * val, IR * offset);

    //Build store operation to store 'rhs' to store value to be one of the
    //element of a PR.
    //prno: target prno.
    //type: data type of targe pr.
    //base: base of source.
    //value: value that need to be set.
    //offset: byte offset to the start of result PR.
    //rhs: value expected to store.
    IR * buildSetElem(UINT prno, Type const* type, IR * base, IR * val,
                      IR * offset);

    //Build store operation to get value from 'base', and store the result PR.
    //prno: result prno.
    //type: data type of targe pr.
    //offset: byte offset to the start of PR.
    //base: hold the value that expected to extract.
    IR * buildGetElem(UINT prno, Type const* type, IR * base, IR * offset);

    //Build store operation to get value from 'rhs', and store the result PR.
    //type: data type of targe pr.
    //offset: byte offset to the start of rhs PR.
    //base: hold the value that expected to extract.
    IR * buildGetElem(Type const* type, IR * base, IR * offset);

    //Build IR_CONTINUE operation.
    IR * buildContinue();

    //Build IR_BREAK operation.
    IR * buildBreak();

    //Build IR_CASE operation.
    IR * buildCase(IR * casev_exp, LabelInfo const* case_br_lab);

    //Build Do Loop stmt.
    //iv: induction variable.
    //det: determinate expression.
    //loop_body: stmt list.
    //init: record the stmt that initialize iv.
    //step: record the stmt that update iv.
    IR * buildDoLoop(IR * iv, IR * init, IR * det, IR * step, IR * loop_body);

    //Build Do While stmt.
    //det: determinate expression.
    //loop_body: stmt list.
    IR * buildDoWhile(IR * det, IR * loop_body);

    //Build While Do stmt.
    //det: determinate expression.
    //loop_body: stmt list.
    IR * buildWhileDo(IR * det, IR * loop_body);

    //Build IF stmt.
    //det: determinate expression.
    //true_body: stmt list.
    //false_body: stmt list.
    IR * buildIf(IR * det, IR * true_body, IR * false_body);

    //Build SWITCH multi-select stmt.
    //vexp: expression to determine which case entry will be target.
    //case_list: case entry list. case entry is consist of expression and label.
    //    Note that case list is optional.
    //body: stmt list.
    //default_lab: label indicates the default choice, the label is optional.
    //
    //NOTE: Do not set parent for stmt in 'body'.
    IR * buildSwitch(IR * vexp, IR * case_list, IR * body,
                     LabelInfo const* default_lab);

    //Build PR and assign dedicated PRNO.
    //Return IR_PR operation by specified prno and type id.
    IR * buildPRdedicated(UINT prno, Type const* type);

    //Build PR that PRNO assiged by Region.
    //Return IR_PR operation by specified type id.
    IR * buildPR(Type const* type);

    //Build PR that PRNO assiged by Region.
    IR * buildPR(DATA_TYPE dt)
    { return buildPR(getTypeMgr()->getSimplexType(dt)); }

    //Generate a PR number by specified prno and type id.
    //This operation will allocate new PR number.
    //Note the function does NOT generate Var for generated PR no.
    UINT buildPrno(Type const* type);

    //Generate a PR number by specified prno and type id.
    //This operation will allocate new PR number.
    UINT buildPrno(DATA_TYPE dt)
    { return buildPrno(getTypeMgr()->getSimplexType(dt)); }

    //Build IR_TRUEBR or IR_FALSEBR operation.
    IR * buildBranch(bool is_true_br, IR * det, LabelInfo const* lab);

    //Build Identifier.
    IR * buildId(Var * var_info);

    //Build internal label operation.
    IR * buildIlabel();

    //Build label operation.
    IR * buildLabel(LabelInfo const* li);

    //Build IR_CVT operation.
    //exp: the expression to be converted.
    //tgt_ty: the target type that you want to convert.
    IR * buildCvt(IR * exp, Type const* tgt_ty);

    //Build unconditional GOTO.
    IR * buildGoto(LabelInfo const* li);

    //Build IR_IGOTO unconditional multi-branch operation.
    //vexp: expression to determine which case entry will be target.
    //case_list: case entry list. case entry is consist of expression and label.
    IR * buildIgoto(IR * vexp, IR * case_list);

    //The function will check and build pointer arithmetic operation.
    //To build pointer arithemtic, the addend of pointer must be
    //product of the pointer-base-size and rchild if lchild is pointer.
    IR * buildPointerOp(IR_TYPE irt, IR * lchild, IR * rchild);

    //Build compare operation.
    IR * buildCmp(IR_TYPE irt, IR * lchild, IR * rchild);
    //Build judgement operation.
    //This function build operation that comparing with 0 by NE node.
    //e.g: output is (exp != 0).
    //This function always used as helper function to convient to
    //generate det-expression if it is not relational/logical.
    IR * buildJudge(IR * exp);

    //Build binary operation without considering pointer arithmetic.
    IR * buildBinaryOpSimp(IR_TYPE irt, Type const* type, IR * lchild,
                           IR * rchild);

    //Build binary operation.
    //If rchild/lchild is pointer, the function will attemp to generate pointer
    //arithmetic operation instead of normal binary operation.
    IR * buildBinaryOp(IR_TYPE irt, Type const* type, IN IR * lchild,
                       IN IR * rchild);
    IR * buildBinaryOp(IR_TYPE irt, DATA_TYPE dt, IN IR * lchild,
                       IN IR * rchild);

    //Build unary operation.
    IR * buildUnaryOp(IR_TYPE irt, Type const* type, IN IR * opnd);

    //Build unary operation.
    IR * buildUnaryOp(IR_TYPE irt, DATA_TYPE dt, IN IR * opnd)
    { return buildUnaryOp(irt, getTypeMgr()->getSimplexType(dt), opnd); }

    //Build IR_LNOT operation.
    IR * buildLogicalNot(IR * opnd0);

    //Build Logical operations, include IR_LAND, IR_LOR, IR_XOR.
    IR * buildLogicalOp(IR_TYPE irt, IR * opnd0, IR * opnd1);

    //Build IR_CONST operation.
    //The expression indicates an integer.
    //v: value of integer.
    //type: integer type.
    IR * buildImmInt(HOST_INT v, Type const* type);

    //Build IR_CONST operation.
    //The expression indicates an integer.
    //v: value of integer.
    //type: integer type.
    IR * buildImmInt(HOST_INT v, DATA_TYPE dt)
    { return buildImmInt(v, getTypeMgr()->getSimplexType(dt)); }

    //Build IR_CONST operation.
    //The expression indicates a float point number.
    IR * buildImmFp(HOST_FP fp, Type const* type);

    //Build IR_CONST operation.
    //The expression indicates value with dynamic type.
    IR * buildImmAny(HOST_INT v);

    //Build IR_CONST operation.
    //The expression indicates a float point number.
    IR * buildImmFp(HOST_FP fp, DATA_TYPE dt)
    { return buildImmFp(fp, getTypeMgr()->getSimplexType(dt)); }

    //Build IR_LDA operation.
    //var: variable that will be taken address.
    IR * buildLda(Var * var);
    IR * buildLdaString(CHAR const* varname, Sym const* string);
    IR * buildLdaString(CHAR const* varname, CHAR const * string);

    //Build IR_LD operation.
    //Load value from variable with type 'type'.
    //var: indicates the variable which value will be loaded.
    //ofst: memory byte offset relative to var.
    //type: result type of value.
    IR * buildLoad(Var * var, TMWORD ofst, Type const* type);

    //Build IR_LD operation.
    //Load value from variable with type 'type'.
    //var: indicates the variable which value will be loaded.
    //type: result type of value.
    IR * buildLoad(Var * var, Type const* type)
    { return buildLoad(var, 0, type); }

    //Build IR_LD operation.
    //Load value from variable with type 'type'.
    //var: indicates the variable which value will be loaded.
    //Result type of value is the type of variable carried.
    IR * buildLoad(Var * var)
    { ASSERT0(var); return buildLoad(var, VAR_type(var)); }

    //Build IR_ILD operation.
    //Result is either register or memory chunk, and the size of ILD
    //result equals to 'pointer_base_size' of 'addr'.
    //base: memory address of ILD.
    //ofst: memory byte offset relative to base.
    //ptbase_or_mc_size: if result of ILD is pointer, this parameter records
    //   pointer_base_size; or if result is memory chunk, it records
    //   the size of memory chunk.
    //NOTICE: The ofst of ILD requires to maintain when after return.
    IR * buildILoad(IR * base, Type const* type);
    IR * buildILoad(IR * base, TMWORD ofst, Type const* type);

    //Build IR_ST operation.
    //lhs: memory variable, described target memory location.
    //rhs: value expected to store.
    IR * buildStore(Var * lhs, IR * rhs);

    //Build IR_ST operation.
    //lhs: target memory location.
    //type: result data type.
    //rhs: value expected to store.
    IR * buildStore(Var * lhs, Type const* type, IR * rhs);

    //Build IR_ST operation.
    //lhs: target memory location.
    //type: result data type.
    //ofst: memory byte offset relative to lhs.
    //rhs: value expected to store.
    IR * buildStore(Var * lhs, Type const* type, TMWORD ofst, IR * rhs);

    //Build store operation to store 'rhs' to new pr with type and prno.
    //prno: target prno.
    //type: data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(UINT prno, Type const* type, IR * rhs);

    //Build store operation to store 'rhs' to new pr with type and prno.
    //prno: target prno.
    //dt: the simplex data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(UINT prno, DATA_TYPE dt, IR * rhs)
    { return buildStorePR(prno, getTypeMgr()->getSimplexType(dt), rhs); }

    //Build store operation to store 'rhs' to new pr with type.
    //type: data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(Type const* type, IR * rhs);

    //Build store operation to store 'rhs' to new pr with type.
    //dt: the simplex data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(DATA_TYPE dt, IR * rhs)
    { return buildStorePR(getTypeMgr()->getSimplexType(dt), rhs); }

    //Build IR_IST operation.
    //lhs: target memory location pointer.
    //rhs: value expected to store.
    //type: result type of indirect memory operation, note type is not the
    //data type of lhs.
    IR * buildIStore(IR * base, IR * rhs, Type const* type);
    IR * buildIStore(IR * base, IR * rhs, TMWORD ofst, Type const* type);

    //Build IR_CONST operation.
    //The result IR indicates a string.
    IR * buildString(Sym const* strtab);

    //Build IR_STARRAY operation.
    //STARRAY will write value that indicated by 'rhs' into element of array.
    //base: base of array operation, it is either LDA or pointer.
    //sublist: subscript expression list.
    //type: result type of array operator.
    //    Note that type may NOT be equal to elem_tyid, accroding to
    //    ARR_ofst(). If ARR_ofst() is not zero, that means array
    //    elem is MC, or VECTOR, and type should be type of member
    //    to MC/VECTOR.
    //    e.g: struct S{ int a,b,c,d;}
    //        struct S pa[100];
    //        If youe do access pa[1].c
    //        type should be int rather than struct S.
    //        and elem_tyid should be struct S.
    //
    //elem_tyid: record element-data-type.
    //    e.g:vector<int,8> g[100];
    //        elem_size is sizeof(vector<int,8>) = 32
    //        elem_type is vector.
    //    e.g1: struct S{ int a,b,c,d;}
    //        struct S * pa[100];
    //        elem_size is sizeof(struct S *)
    //        elem_type is PTR.
    //    e.g2:
    //        struct S pa[100];
    //        elem_size is sizeof(struct S)
    //        elem_type is struct S
    //
    //dims: indicate the array dimension.
    //elem_num: point to an integer array that indicate
    //    the number of element for in dimension.
    //    The length of the integer array should be equal to 'dims'.
    //    e.g: int g[12][24];
    //        elem_num points to an array with 2 value, [12, 24].
    //        the 1th dimension has 12 elements, and the 2th dimension has 24
    //        elements, which element type is D_I32.
    //    Note the parameter may be nullptr.
    //rhs: value expected to store.
    IR * buildStoreArray(IR * base, IR * sublist, Type const* type,
                         Type const* elemtype, UINT dims,
                         TMWORD const* elem_num_buf, IR * rhs);

    //Build ARRAY operation.
    //ARRAY will load an element.
    //base: base of array operation, it is either LDA or pointer.
    //sublist: subscript expression list.
    //type: result type of array operator.
    //    Note that type may NOT be equal to elem_tyid, accroding to
    //    ARR_ofst(). If ARR_ofst() is not zero, that means array
    //    elem is MC, or VECTOR, and type should be type of member
    //    to MC/VECTOR.
    //    e.g: struct S{ int a,b,c,d;}
    //        struct S pa[100];
    //        If youe do access pa[1].c
    //        type should be int rather than struct S.
    //        and elem_tyid should be struct S.
    //
    //elem_tyid: record element-data-type.
    //    e.g:vector<int,8> g[100];
    //        elem_size is sizeof(vector<int,8>) = 32
    //        elem_type is vector.
    //    e.g1: struct S{ int a,b,c,d;}
    //        struct S * pa[100];
    //        elem_size is sizeof(struct S *)
    //        elem_type is PTR.
    //    e.g2:
    //        struct S pa[100];
    //        elem_size is sizeof(struct S)
    //        elem_type is struct S
    //
    //dims: indicate the array dimension.
    //elem_num: point to an integer array that indicate
    //    the number of element for in dimension.
    //    The length of the integer array should be equal to 'dims'.
    //    e.g: int g[12][24];
    //        elem_num points to an array with 2 value, [12, 24].
    //        the 1th dimension has 12 elements, and the 2th dimension has 24
    //        elements, which element type is D_I32.
    //    Note the parameter may be nullptr.
    IR * buildArray(IR * base, IR * sublist, Type const* type,
                    Type const* elemtype, UINT dims,
                    TMWORD const* elem_num_buf);

    //Build IR_RETURN operation.
    IR * buildReturn(IR * ret_exp);

    //Build conditionally selected expression.
    //The result depends on the predicator's value.
    //e.g: x = a > b ? 10 : 100
    //Note predicator may not be judgement expression.
    IR * buildSelect(IR * det, IR * true_exp, IR * false_exp, Type const* type);

    //Build IR_PHI operation.
    //prno: result PR of PHI.
    IR * buildPhi(UINT prno, Type const* type, IR * opnd_list);
    IR * buildPhi(UINT prno, Type const* type, UINT num_opnd);

    //Build IR_REGION operation.
    IR * buildRegion(Region * rg);

    //Build IR_ICALL operation.
    //res_list: reture value list.
    //result_prno: indicate the result PR which hold the return value.
    //    0 means the call does not have a return value.
    //type: result PR data type.
    //    0 means the call does not have a return value.
    IR * buildICall(IR * callee, IR * param_list, UINT result_prno,
                    Type const* type);
    IR * buildICall(IR * callee, IR * param_list)
    { return buildICall(callee, param_list, 0, getTypeMgr()->getAny()); }

    //Build IR_CALL operation.
    //res_list: reture value list.
    //result_prno: indicate the result PR which hold the return value.
    //    0 means the call does not have a return value.
    //type: result PR data type.
    IR * buildCall(Var * callee, IR * param_list, UINT result_prno,
                   Type const* type);
    IR * buildCall(Var * callee,  IR * param_list)
    { return buildCall(callee, param_list, 0, getTypeMgr()->getAny()); }

    //Construct IR list from BB list.
    //clean_ir_list: clean bb's ir list if it is true.
    IR * constructIRlist(bool clean_ir_list);

    //Construct BB list from IR list.
    //1. Split list of IRs into basic-block list.
    //2. Set BB propeties. e.g: entry-bb, exit-bb.
    void constructBBList();

    //Count memory usage for current object.
    size_t count_mem() const;

    //The funtion enables reallocating resource without by destructing region
    //object. Note the function should work together with init() to complete
    //the reallocation of resource.
    virtual void destroy();
    void destroyPassMgr();
    void destroyAttachInfoMgr();

    //Duplication all contents of 'src', includes AttachInfo, except DU info,
    //SSA info, kids and siblings IR.
    IR * dupIR(IR const* ir);

    //Duplicate 'ir' and its kids, but without ir's sibiling node.
    //The duplication includes AI, except DU info, SSA info.
    IR * dupIRTree(IR const* ir);

    //Duplication 'ir' and kids, and its sibling, return list of new ir.
    //Duplicate irs start from 'ir' to the end of list.
    //The duplication includes AI, except DU info, SSA info.
    IR * dupIRTreeList(IR const* ir);

    //filename: dump BB list into given filename.
    void dumpBBList(CHAR const* filename, bool dump_inner_region = true) const;
    void dumpBBList(bool dump_inner_region = true) const;
    void dumpIRList(UINT dumpflag = IR_DUMP_COMBINE) const;

    //Dump all irs and ordering by IR_id.
    void dumpAllocatedIR() const;

    //Dump each Var in current region's Var table.
    void dumpVARInRegion() const;

    //Dump all MD that related to Var.
    void dumpVarMD(Var * v, UINT indent) const;
    void dumpFreeTab() const;

    //Dump IR and memory usage.
    void dumpMemUsage() const;

    //Dump formal parameter list.
    void dumpParameter() const;
    void dumpVarTab() const;

    //Dump GR through LogMgr.
    void dumpGR(bool dump_inner_region = true) const;

    //Dump Region's IR BB list.
    //DUMP ALL BBList DEF/USE/OVERLAP_DEF/OVERLAP_USE.
    void dumpRef(UINT indent = 4) const;
    void dumpBBRef(IN IRBB * bb, UINT indent) const;
    void dump(bool dump_inner_region) const;

    bool evaluateConstInteger(IR const* ir, OUT ULONGLONG * const_value);

    //This function erases all informations of ir and
    //append it into free_list for next allocation.
    //If Attach Info exist, this function will erase it rather than delete.
    //If DU info exist, this function will retrieve it back
    //to region for next allocation.
    //Note that this function does NOT free ir's kids and siblings.
    void freeIR(IR * ir);

    //Free ir and all its kids, except its sibling node.
    //We can only utilizing the function to free the
    //IR which allocated by 'allocIR'.
    void freeIRTree(IR * ir);

    //Free ir, ir's sibling, and all its kids.
    //We can only utilizing the function to free the IR
    //which allocated by 'allocIR'.
    //NOTICE: If ir's sibling is not nullptr, that means the IR is
    //a high level type. IRBB consists of only middle/low level IR.
    void freeIRTreeList(IR * ir);

    //Free ir, and all its kids.
    //We can only utilizing the function to free
    //the IR which allocated by 'allocIR'.
    void freeIRTreeList(IRList & irs);

    //Free IRBB list.
    //We can only utilizing the function to free the IRBB
    //which allocated by 'allocBB'.
    //NOTICE: bb will not be destroyed, it is just recycled.
    void freeIRBBList(BBList & bbl);

    //This function iterate Var table of current region to
    //find all Var which are formal parameter.
    //in_decl_order: if it is true, this function will sort the formal
    //parameters in the Left to Right order according to their declaration.
    //e.g: foo(a, b, c), varlst will be {a, b, c}.
    void findFormalParam(OUT List<Var const*> & varlst, bool in_decl_order);

    //This function find the formal parameter variable by given position.
    Var const* findFormalParam(UINT position) const;

    //This function find Var via iterating Var table of current region.
    Var * findVarViaSymbol(Sym const* sym) const;

    REGION_TYPE getRegionType() const { return REGION_type(this); }

    //Get the data in which blackbox region contained.
    void * getBlackBoxData() const
    { ASSERT0(is_blackbox()); return REGION_blackbox_data(this); }

    MDSystem * getMDSystem() const { return getRegionMgr()->getMDSystem(); }
    TypeMgr * getTypeMgr() const { return getRegionMgr()->getTypeMgr(); }
    MDMgr * getMDMgr() { return &ANA_INS_md_mgr(getAnalysisInstrument()); }

    //Get TargInfo that describes current target machine.
    TargInfo * getTargInfo() const
    { ASSERT0(getRegionMgr()); return getRegionMgr()->getTargInfo(); }

    //Get general memory pool of current region.
    SMemPool * get_pool() const { return m_pool; }

    //Get memory pool to allocate Single-List-Container.
    SMemPool * getSCPool() const
    { return ANA_INS_sc_labelinfo_pool(getAnalysisInstrument()); }

    //Get the maximum PR no.
    UINT getPRCount() const 
    { return ANA_INS_pr_count(getAnalysisInstrument()); }

    //Get the variable which represent current region.
    Var * getRegionVar() const { return m_var; }

    //Get the RegionMgr that current region belongs to.
    RegionMgr * getRegionMgr() const { return REGION_region_mgr(this); }

    //Get IR list if any.
    IR * getIRList() const { return ANA_INS_ir_list(getAnalysisInstrument()); }

    //Get the VarMgr related to current RegionMgr.
    VarMgr * getVarMgr() const
    { ASSERT0(getRegionMgr()); return getRegionMgr()->getVarMgr(); }

    //Get a variable table that includes all variables
    //declared in current region.
    VarTab * getVarTab() { return &m_rg_var_tab; }

    //Get BitSetMgr of current region.
    xcom::BitSetMgr * getBitSetMgr() const
    { return &ANA_INS_bs_mgr(getAnalysisInstrument()); }

    //Get Sparse|Dense BitSetMgr of current region.
    xcom::DefMiscBitSetMgr * getMiscBitSetMgr() const
    { return &ANA_INS_sbs_mgr(getAnalysisInstrument()); }

    //Get MDSetMgr of current region.
    MDSetMgr * getMDSetMgr() const
    { return &ANA_INS_mds_mgr(getAnalysisInstrument()); }

    //Get IRBB list if any.
    BBList * getBBList() const
    { return &ANA_INS_ir_bb_list(getAnalysisInstrument()); }

    //Get IRBBMgr of current region.
    IRBBMgr * getBBMgr() const
    { return &ANA_INS_ir_bb_mgr(getAnalysisInstrument()); }

    //Get MDSetHash.
    MDSetHash * getMDSetHash() const
    { return &ANA_INS_mds_hash(getAnalysisInstrument()); }

    //Return IR pointer via the unique IR_id.
    IR * getIR(UINT irid) const
    { return ANA_INS_ir_vec(getAnalysisInstrument()).get(irid); }

    //Return the vector that record all allocated IRs.
    Vector<IR*> * getIRVec() const
    { return &ANA_INS_ir_vec(getAnalysisInstrument()); }

    //Return PassMgr.
    PassMgr * getPassMgr() const
    { return ANA_INS_pass_mgr(getAnalysisInstrument()); }

    //Return AttachInfoMgr.
    AttachInfoMgr * getAttachInfoMgr() const
    { return ANA_INS_ai_mgr(getAnalysisInstrument()); }

    //Return IRSimp.
    IRSimp * getIRSimp() const
    {
        return getPassMgr() != nullptr ?
               (IRSimp*)getPassMgr()->registerPass(PASS_IRSIMP) : nullptr;
    }

    //Return CDG.
    CDG * getCDG() const
    {
        return getPassMgr() != nullptr ?
               (CDG*)getPassMgr()->queryPass(PASS_CDG) : nullptr;
    }

    //Return IRCFG.
    IRCFG * getCFG() const
    {
        return getPassMgr() != nullptr ?
               (IRCFG*)getPassMgr()->queryPass(PASS_CFG) : nullptr;
    }

    //Get Alias Analysis.
    AliasAnalysis * getAA() const
    {
        return getPassMgr() != nullptr ?
               (AliasAnalysis*)getPassMgr()->queryPass(PASS_AA) : nullptr;
    }

    //Return DU info manager.
    DUMgr * getDUMgr() const
    {
        return getPassMgr() != nullptr ?
               (DUMgr*)getPassMgr()->queryPass(PASS_DU_MGR) : nullptr;
    }

    //Return MDSSA manager.
    MDSSAMgr * getMDSSAMgr() const
    {
        return getPassMgr() != nullptr ?
               (MDSSAMgr*)getPassMgr()->queryPass(PASS_MD_SSA_MGR) : nullptr;
    }

    //Return PRSSA manager.
    PRSSAMgr * getPRSSAMgr() const
    {
        return getPassMgr() != nullptr ?
               (PRSSAMgr*)getPassMgr()->queryPass(PASS_PR_SSA_MGR) : nullptr;
    }

    Region * getParent() const { return REGION_parent(this); }
    CHAR const* getRegionName() const;
    Region * getFuncRegion();

    //Allocate and return all CALLs in the region.
    inline CIRList * getCallList()
    {
        if (ANA_INS_call_list(getAnalysisInstrument()) == nullptr) {
            ANA_INS_call_list(getAnalysisInstrument()) = new CIRList();
        }
        return ANA_INS_call_list(getAnalysisInstrument());
    }

    //Allocate and return a list of IR_RETURN in current Region.
    inline CIRList * getReturnList()
    {
        if (ANA_INS_return_list(getAnalysisInstrument()) == nullptr) {
            ANA_INS_return_list(getAnalysisInstrument()) = new CIRList();
        }
        return ANA_INS_return_list(getAnalysisInstrument());
    }

    //Get the MayDef MDSet of Region.
    MDSet * getMayDef() const
    { return m_ref_info != nullptr ? REF_INFO_maydef(m_ref_info) : nullptr; }

    //Generate the MayDef MDSet of Region.
    MDSet * genMayDef()
    {
        if (m_ref_info == nullptr) { return nullptr; }
        REF_INFO_maydef(m_ref_info) = getMDSetMgr()->alloc();
        return REF_INFO_maydef(m_ref_info);
    }

    //Get the MayUse MDSet of Region.
    MDSet * getMayUse() const
    { return m_ref_info != nullptr ? REF_INFO_mayuse(m_ref_info) : nullptr; }

    //Generate the MayUse MDSet of Region.
    MDSet * genMayUse()
    {
        if (m_ref_info == nullptr) { return nullptr; }
        REF_INFO_mayuse(m_ref_info) = getMDSetMgr()->alloc();
        return REF_INFO_mayuse(m_ref_info);
    }

    //Get the top parent level region.
    Region * getTopRegion()
    {
        Region * rg = this;
        while (rg->getParent() != nullptr) {
            rg = rg->getParent();
        }
        return rg;
    }

    //Allocate a internal LabelInfo that is not declared by compiler user.
    LabelInfo * genILabel()
    {
        LabelInfo * li = genILabel(RM_label_count(getRegionMgr()));
        RM_label_count(getRegionMgr())++;
        return li;
    }

    //Allocate a LabelInfo accroding to given 'labid'.
    LabelInfo * genILabel(UINT labid)
    {
        ASSERT0(labid <= RM_label_count(getRegionMgr()));
        LabelInfo * li = allocInternalLabel(get_pool());
        LABELINFO_num(li) = labid;
        return li;
    }

    LabelInfo * genPragmaLabel(CHAR const* lab)
    { return genPragmaLabel(getRegionMgr()->addToSymbolTab(lab));}
    LabelInfo * genPragmaLabel(Sym const* labsym)
    {
        ASSERT0(labsym);
        return allocPragmaLabel(labsym, get_pool());
    }

    LabelInfo * genCustomLabel(CHAR const* lab)
    { return genCustomLabel(getRegionMgr()->addToSymbolTab(lab));}
    LabelInfo * genCustomLabel(Sym const* labsym)
    {
        ASSERT0(labsym);
        return allocCustomerLabel(labsym, get_pool());
    }

    //Allocate Var for PR.
    Var * genVarForPR(UINT prno, Type const* type);

    //Return the tyid for array index, the default is unsigned 32bit.
    inline Type const* getTargetMachineArrayIndexType()
    {
        return getTypeMgr()->getSimplexTypeEx(
            getTypeMgr()->getDType(WORD_LENGTH_OF_TARGET_MACHINE, false));
    }

    //Use HOST_INT type describes the value.
    //The value can not exceed ir type's value range.
    HOST_INT getIntegerInDataTypeValueRange(IR * ir) const;
    HOST_INT getMaxInteger(UINT bitsize, bool is_signed) const;
    HOST_INT getMinInteger(UINT bitsize, bool is_signed) const;

    //Get the VarMgr related to current RegionMgr.
    LogMgr * getLogMgr() const
    { ASSERT0(getRegionMgr()); return getRegionMgr()->getLogMgr(); }

    //Perform high level optmizations.
    virtual bool HighProcess(OptCtx & oc);

    UINT id() const { return REGION_id(this); }

    //Initialze Region.
    //The funtion enables reallocating resource without by destructing region
    //object. Note the function should work together with destroy() to complete
    //the reallocation of resource.
    void init(REGION_TYPE rt, RegionMgr * rm);

    void initRefInfo()
    {
        if (m_ref_info != nullptr) { return; }
        m_ref_info = (RefInfo*)xmalloc(sizeof(RefInfo));
    }

    //Allocate and initialize pass manager.
    PassMgr * initPassMgr();    
    //Allocate and initialize attachinfo manager.
    AttachInfoMgr * initAttachInfoMgr();
    bool isSafeToOptimize(IR const* ir);

    //Return true if ir belongs to current region.
    bool isRegionIR(IR const* ir) const;

    //Return true if Var belongs to current region.
    bool isRegionVAR(Var const* var) const;

    //Return true if Region name is equivalent to 'n'.
    //This function is helper function to faciltate user identify Region.
    bool isRegionName(CHAR const* n) const
    {
        ASSERTN(getRegionName(), ("Region does not have name"));
        return strcmp(getRegionName(), n) == 0;
    }

    //Return true if Region type is undefined.
    bool is_undef() const { return getRegionType() == REGION_UNDEF; }

    //Return true if Region indicates function.
    //Function contains complete prolog and epilog,
    //analysis instructment as well.
    bool is_function() const { return getRegionType() == REGION_FUNC; }

    //Return true if Region indicates a blackbox.
    //Note blackbox Region does NOT contain analysis instrument.
    bool is_blackbox() const { return getRegionType() == REGION_BLACKBOX; }

    //Return true if Region indicates whole program.
    bool is_program() const { return getRegionType() == REGION_PROGRAM; }

    //Return true if current Region is an inner region that embeded other one.
    bool is_inner() const { return getRegionType() == REGION_INNER; }

    //Return true if Region is exception handler.
    bool is_eh() const { return getRegionType() == REGION_EH; }

    //Return true if Region only contain normal read MD, except volatile read.
    bool is_readonly() const { return REGION_is_readonly(this); }

    //Return true if Region's MD reference has been computed and is avaiable.
    bool is_ref_valid() const { return REGION_is_ref_valid(this); }

    bool isLogMgrInit() const { return getRegionMgr()->isLogMgrInit(); }

    //Perform middle level IR optimizations which are implemented
    //accroding to control flow info and data flow info.
    virtual bool MiddleProcess(OptCtx & oc);

    //Map from prno to related Var.
    Var * mapPR2Var(UINT prno)
    { return ANA_INS_prno2var(getAnalysisInstrument()).get(prno); }

    //Construct BB list by destructing CFG.
    bool reconstructBBList(OptCtx & oc);
    void registerGlobalVAR();

    //Split IR list into list of basic blocks.
    //irs: a list of ir.
    //bbl: a list of bb.
    //ctbb: marker current bb container.
    //Note if CFG is invalid, it will not be updated.
    BBListIter splitIRlistIntoBB(IN IR * irs, OUT BBList * bbl,
                                 BBListIter ctbb, OptCtx const& oc);

    //Assign variable to given PR.
    void setMapPR2Var(UINT prno, Var * pr_var)
    { ANA_INS_prno2var(getAnalysisInstrument()).set(prno, pr_var); }

    //Set the counter of PR.
    //Note 'cnt' will be assigned to next new PR, so it should have not be
    //referenced.
    void setPRCount(UINT cnt)
    { ANA_INS_pr_count(getAnalysisInstrument()) = cnt; }
    void setRegionVar(Var * v) { m_var = v; }
    void setIRList(IR * irs) { ANA_INS_ir_list(getAnalysisInstrument()) = irs; }
    void setBlackBoxData(void * d) { REGION_blackbox_data(this) = d; }

    //Collect information of CALL and RETURN in current region.
    //num_inner_region: count the number of inner regions.
    void scanCallAndReturnList(OUT UINT & num_inner_region,
                               OUT List<IR const*> * call_list,
                               OUT List<IR const*> * ret_list,
                               bool scan_inner_region);
    //Collect information of CALL and RETURN in current region.
    void scanCallAndReturnList(OUT UINT & num_inner_region,
                               bool scan_inner_region)
    {
        getCallList()->clean();
        getReturnList()->clean(); //Scan RETURN as well.
        scanCallAndReturnList(num_inner_region, getCallList(),
                              getReturnList(), scan_inner_region);
    }

    //Do simplification that lowering IR tree to lowest tree height.
    //The lowering should conform to the restriction declared in 'oc'.
    void lowerIRTreeToLowestHeight(OptCtx & oc);

    //This function is main entry to process current region.
    virtual bool process(OptCtx * oc);

    //The function collect information that IPA may used.
    //Check and rescan call-list of region if something changed.
    void updateCallAndReturnList(bool scan_inner_region);

    //Ensure that each IR in ir_list must be allocated in crrent region.
    bool verifyIROwnership();

    //Verify MD reference to each stmts and expressions which described memory.
    bool verifyMDRef();

    //Allocate memory from region pool.
    void * xmalloc(UINT size);
};
//END Region

} //namespace xoc
#endif
