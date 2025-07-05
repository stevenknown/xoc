/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __IR_MGR_H__
#define __IR_MGR_H__

namespace xoc {

//These two macros describe the largest IR code and its declaration.
//For now the largest IR is IR_ICALL, and its class declaration is CICall,
//user has to redefine these two macros in target_const_info.h if larger IR
//class defined.
#ifndef LARGEST_IR_CLASS
#define LARGEST_IR_CLASS CICall
#endif
#ifndef LARGEST_IR_CODE
#define LARGEST_IR_CODE IR_ICALL
#endif

//The macro describes the byte offset in IR free-table.
#define MAX_OFFSET_AT_FREE_TABLE (sizeof(LARGEST_IR_CLASS) - sizeof(IR))

#define IRID_UNDEF 0 //The undefined value of IR's id.

#define INIT_STMT_PLACE_HOLDER_NAME "#init_placeholder"

class IRMgr : public Pass {
    COPY_CONSTRUCTOR(IRMgr);
protected:
    UINT m_ir_count;
    SMemPool * m_ir_pool;
    TypeMgr * m_tm;
    RegionMgr * m_rm;
    VarMgr * m_vm;
    Vector<IR*> m_ir_vec; //record IR which have allocated. ir id is dense
    IR * m_free_tab[MAX_OFFSET_AT_FREE_TABLE + 1];
    Var * m_init_placeholder_var;
    #ifdef _DEBUG_
    xcom::BitSet m_has_been_freed_irs;
    #endif
protected:
    SMemPool * getIRPool() const { return m_ir_pool; }
    IR * pickFreeIR(UINT idx, bool lookup);
    IR * xmalloc(UINT size);
public:
    explicit IRMgr(Region * rg);
    virtual ~IRMgr();

    //Generate IR, invoke freeIR() or freeIRTree() if it is useless.
    //NOTE: Do NOT invoke ::free() to free IR, because all
    //    IR are allocated in the pool.
    IR * allocIR(IR_CODE irc);
    IR * allocIR(IR_CODE irc, bool lookup);

    //Build alloca ir.
    //Allocate stack space based on the size.
    IR * buildAlloca(IR * size);

    //Build ARRAY operation.
    //ARRAY will load an element.
    //base: base of array operation, it is either LDA or pointer.
    //sublist: subscript expression list.
    //type: result type of array operator.
    //    Note that type may NOT be equal to elem_tyid, according to
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

    //Build binary operation.
    //If rchild/lchild is pointer, the function will attemp to generate pointer
    //arithmetic operation instead of normal binary operation.
    IR * buildBinaryOp(IR_CODE irt, Type const* type, IN IR * lchild,
                       IN IR * rchild);
    IR * buildBinaryOp(IR_CODE irt, DATA_TYPE dt, IN IR * lchild,
                       IN IR * rchild);

    //Build binary operation without considering pointer arithmetic.
    IR * buildBinaryOpSimp(IR_CODE irt, Type const* type, IR * lchild,
                           IR * rchild);

    //Build IR_TRUEBR or IR_FALSEBR operation.
    IR * buildBranch(bool is_true_br, IR * det, LabelInfo const* lab);

    //Build IR_BREAK operation.
    IR * buildBreak();

    //Build IR_CALL operation.
    //res_list: reture value list.
    //result_prno: indicate the result PR which hold the return value.
    //    0 means the call does not have a return value.
    //type: result PR data type.
    IR * buildCall(Var * callee, IR * param_list, UINT result_prno,
                   Type const* type);
    IR * buildCall(Var * callee,  IR * param_list)
    { return buildCall(callee, param_list, 0, m_tm->getAny()); }

    //Build IR_CASE operation.
    IR * buildCase(IR * casev_exp, LabelInfo const* case_br_lab);

    //Build IR_CONTINUE operation.
    IR * buildContinue();

    //Build compare operation.
    IR * buildCmp(IR_CODE irt, IR * lchild, IR * rchild);

    //Build IR_CVT operation.
    //exp: the expression to be converted.
    //tgt_ty: the target type that you want to convert.
    IR * buildCvt(IR * exp, Type const* tgt_ty);

    //Build Do Loop stmt.
    //iv: the induction variable.
    //det: the determinate expression of iv.
    //loop_body: stmt list.
    //init: record the expression that represent the initial value of iv.
    //step: record the expression that represent the update behavior of iv.
    IR * buildDoLoop(IR * iv, IR * init, IR * det, IR * step, IR * loop_body);

    //Build Do While stmt.
    //det: determinate expression.
    //loop_body: stmt list.
    IR * buildDoWhile(IR * det, IR * loop_body);

    //Build IR_DUMMYUSE operation.
    //A dummyuse operation indicates that IR is a real operation.
    //e.g: the dummyuse of a CallStmt represents the variables that may be
    //referenced by the call.
    //for example: call fopen(h, dummyuse(errno));
    IR * buildDummyUse(IR * use_list);

    //This represents that the current CFA is at an offset
    //from the stack pointer.
    //e.g: .cfi_def_cfa_offset 99.
    IR *  buildDwarfCFICfaOffset(Type const* tp, IR * offset);

    //The command to establish the CFA (Caller Frame Address)
    //sets a base value starting from a specific base on the stack.
    //e.g: .cfi_def_cfa $30, 32.
    //Representing that the CFA is currently
    //at the location of register $30 plus an offset of 32.
    IR *  buildDwarfCFIDefCfa(Type const* tp, IR * reg_num, IR * offset);

    //Representing the current location of the CFA (Caller Frame Address).
    //e.g: .cfi_offset $15, -16.
    //This indicates that the current position of the CFA is
    //at the location of register 15 minus an offset of 16.
    IR *  buildDwarfCFIOffset(Type const* tp, IR * reg_num, IR * offset);

    //This indicates that the register has been restored to its original state.
    //e.g: .cfi_restore $26.
    IR *  buildDwarfCFIRestore(Type const* tp, IR * reg_num);

    //Indicating that the value of this register
    //has remained unchanged throughout.
    //e.g: .cfi_same_value $26.
    IR *  buildDwarfCFISameValue(Type const* tp, IR * reg_num);

    //Build store operation to get value from 'rhs', and store the result PR.
    //type: data type of targe pr.
    //offset: byte offset to the start of rhs PR.
    //base: hold the value that expected to extract.
    IR * buildGetElem(Type const* type, IR * base, IR * offset);

    //Build store operation to get value from 'base', and store the result PR.
    //prno: result prno.
    //type: data type of targe pr.
    //offset: byte offset to the start of PR.
    //base: hold the value that expected to extract.
    IR * buildGetElem(PRNO prno, Type const* type, IR * base, IR * offset);

    //Build unconditional GOTO.
    IR * buildGoto(LabelInfo const* li);

    //Build IR_ICALL operation.
    //res_list: reture value list.
    //result_prno: indicate the result PR which hold the return value.
    //    0 means the call does not have a return value.
    //type: result PR data type.
    //    0 means the call does not have a return value.
    IR * buildICall(IR * callee, IR * param_list, UINT result_prno,
                    Type const* type);
    IR * buildICall(IR * callee, IR * param_list)
    { return buildICall(callee, param_list, 0, m_tm->getAny()); }

    //Build Identifier.
    IR * buildId(Var * var);

    //Build IF stmt.
    //det: determinate expression.
    //true_body: stmt list.
    //false_body: stmt list.
    IR * buildIf(IR * det, IR * true_body, IR * false_body);

    //Build IR_IGOTO unconditional multi-branch operation.
    //vexp: expression to determine which case entry will be target.
    //case_list: case entry list. case entry is consist of expression and label.
    IR * buildIgoto(IR * vexp, IR * case_list);

    //Build internal label operation.
    IR * buildIlabel();

    //Build a placeholder operation that is used to remark a lexicographic
    //order in IR list.
    IR * buildInitPlaceHolder(IR * exp);

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

    //Build IR to load value from PR.
    IR * buildIloadFromPR(PRNO prno_base, Type const* tp)
    {
        ASSERT0(prno_base != PRNO_UNDEF && tp);
        return buildIloadFromPR(buildPrno(tp), prno_base, tp);
    }
    IR * buildIloadFromPR(PRNO prno_dst, PRNO prno_src, Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && prno_src != PRNO_UNDEF && tp);
        return buildStorePR(prno_dst, tp, buildILoad(
            buildPRdedicated(prno_src, m_tm->getAny()), tp));
    }

    //Build IR_CONST operation.
    //The expression indicates value with dynamic type.
    IR * buildImmAny(HOST_INT v);

    //Build IR_CONST operation.
    //The expression indicates a float point number.
    IR * buildImmFP(HOST_FP fp, Type const* type);

    //Build IR_CONST operation.
    //The expression indicates a float point number.
    IR * buildImmFP(HOST_FP fp, DATA_TYPE dt)
    { return buildImmFP(fp, m_tm->getSimplexType(dt)); }

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
    { return buildImmInt(v, m_tm->getSimplexType(dt)); }

    //Build IR_CONST operation.
    //The expression indicates a tensor value.
    IR * buildImmTensor(TenVal * tensor_val, Type const* type);

    //Build IR_IST operation.
    //lhs: target memory location pointer.
    //rhs: value expected to store.
    //type: result type of indirect memory operation, note type is not the
    //data type of lhs.
    IR * buildIStore(IR * base, IR * rhs, Type const* type);
    IR * buildIStore(IR * base, IR * rhs, TMWORD ofst, Type const* type);
    IR * buildIStoreToPR(PRNO prno_dst, Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && tp);
        return buildIStoreToPR(prno_dst, buildPrno(tp), tp);
    }
    IR * buildIStoreToPR(PRNO prno_dst, PRNO prno_src, Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && prno_src != PRNO_UNDEF && tp);
        return buildIStoreToPR(prno_dst, buildPRdedicated(prno_src, tp), tp);
    }
    IR * buildIStoreToPR(PRNO prno_dst, IR * ir, Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && ir && ir->is_exp() && tp);
        return buildIStore(buildPRdedicated(prno_dst, m_tm->getAny()), ir, tp);
    }
    IR * buildIStoreToPR(PRNO prno_dst, PRNO prno_src, TMWORD ofst,
                         Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && prno_src != PRNO_UNDEF && tp);
        return buildIStoreToPR(prno_dst, buildPRdedicated(prno_src, tp), ofst,
                               tp);
    }
    IR * buildIStoreToPR(PRNO prno_dst, IR * ir, TMWORD ofst, Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && ir && ir->is_exp() && tp);
        return buildIStore(buildPRdedicated(prno_dst, m_tm->getAny()), ir,
                           ofst, tp);
    }

    //Build judgement operation.
    //This function build operation that comparing with 0 by NE node.
    //e.g: output is (exp != 0).
    //This function always used as helper function to convient to
    //generate det-expression if it is not relational/logical.
    IR * buildJudge(IR * exp);

    //Build label operation.
    IR * buildLabel(LabelInfo const* li);

    //Build IR_LDA operation.
    //var: variable that will be taken address.
    IR * buildLda(Var * var);
    IR * buildLdaString(CHAR const* varname, Sym const* string);
    IR * buildLdaString(CHAR const* varname, CHAR const* string);

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

    //Build IR to load address of variable and save it.
    IR * buildLoadAddrOfVar(Var * var)
    {
        ASSERT0(var);
        Type const* tp = m_tm->getTargMachRegisterType();
        return buildLoadAddrOfVar(buildPrno(tp), var);
    }
    IR * buildLoadAddrOfVar(PRNO prno, Var * var)
    {
        ASSERT0(prno != PRNO_UNDEF && var);
        Type const* tp = m_tm->getTargMachRegisterType();
        return buildStorePR(prno, tp, buildLda(var));
    }

    //Build IR_LNOT operation.
    IR * buildLogicalNot(IR * opnd0);

    //Build Logical operations, include IR_LAND, IR_LOR, IR_XOR.
    IR * buildLogicalOp(IR_CODE irt, IR * opnd0, IR * opnd1);

    //Build move operation.
    IR * buildMove(PRNO prno_dst, PRNO prno_src, Type const* tp)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && prno_src != PRNO_UNDEF);
        ASSERT0(tp);
        return buildMove(prno_dst, prno_src, tp, tp);
    }

    IR * buildMove(PRNO prno_dst, PRNO prno_src,
                   Type const* tp_dst, Type const* tp_src)
    {
        ASSERT0(prno_dst != PRNO_UNDEF && prno_src != PRNO_UNDEF);
        ASSERT0(tp_dst && tp_src);
        return buildStorePR(prno_dst, tp_dst,
            buildPRdedicated(prno_src, tp_src));
    }

    //Build IR_PHI operation.
    //prno: result PR of PHI.
    IR * buildPhi(PRNO prno, Type const* type, IR * opnd_list);
    IR * buildPhi(PRNO prno, Type const* type, UINT num_opnd);

    //Build PR that PRNO assiged by Region.
    //Return IR_PR operation by specified type id.
    IR * buildPR(Type const* type);

    //Build PR that PRNO assiged by Region.
    IR * buildPR(DATA_TYPE dt)
    { return buildPR(m_tm->getSimplexType(dt)); }

    //Build PR and assign dedicated PRNO.
    //Return IR_PR operation by specified prno and type id.
    IR * buildPRdedicated(PRNO prno, Type const* type);

    //Generate a PR number by specified prno and type id.
    //This operation will allocate new PR number.
    //Note the function does NOT generate Var for generated PRNO.
    UINT buildPrno(Type const* type);

    //Generate a PR number by specified prno and type id.
    //This operation will allocate new PR number.
    UINT buildPrno(DATA_TYPE dt)
    { return buildPrno(m_tm->getSimplexType(dt)); }

    //The function will check and build pointer arithmetic operation.
    //To build pointer arithemtic, the addend of pointer must be
    //product of the pointer-base-size and rchild if lchild is pointer.
    IR * buildPointerOp(IR_CODE irt, IR * lchild, IR * rchild);

    //Build IR_REGION operation.
    //Note 'rg' must be subregion to current region. And only program or
    //function region can have a function region as subregion.
    IR * buildRegion(Region * rg);

    //Build IR_RETURN operation.
    IR * buildReturn(IR * ret_exp);

    //Build conditionally selected expression.
    //The result depends on the predicator's value.
    //e.g: x = a > b ? 10 : 100
    //Note predicator may not be judgement expression.
    IR * buildSelect(IR * det, IR * true_exp, IR * false_exp, Type const* type);

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
    IR * buildSetElem(PRNO prno, Type const* type, IR * base, IR * val,
                      IR * offset);

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

    //Build IR_STARRAY operation.
    //STARRAY will write value that indicated by 'rhs' into element of array.
    //base: base of array operation, it is either LDA or pointer.
    //sublist: subscript expression list.
    //type: result type of array operator.
    //    Note that type may NOT be equal to elem_tyid, according to
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

    //Build store operation to store 'rhs' to new pr with type and prno.
    //prno: target prno.
    //type: data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(PRNO prno, Type const* type, IR * rhs);

    //Build store operation to store 'rhs' to new pr with type and prno.
    //prno: target prno.
    //dt: the simplex data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(PRNO prno, DATA_TYPE dt, IR * rhs)
    { return buildStorePR(prno, m_tm->getSimplexType(dt), rhs); }

    //Build store operation to store 'rhs' to new pr with type.
    //type: data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(Type const* type, IR * rhs);

    //Build store operation to store 'rhs' to new pr, the store operation
    //will reference type of 'rhs'.
    //rhs: value expected to store.
    IR * buildStorePR(IR * rhs)
    { return buildStorePR(rhs->getType(), rhs); }

    //Build store operation to store 'rhs' to new pr with type.
    //dt: the simplex data type of targe pr.
    //rhs: value expected to store.
    IR * buildStorePR(DATA_TYPE dt, IR * rhs)
    { return buildStorePR(m_tm->getSimplexType(dt), rhs); }

    //Build a STPR IR to save const value of given IR.
    IR * buildStprFromConst(IR * ir, Type const* tp);

    //Build IR_CONST operation.
    //The result IR indicates a string.
    IR * buildString(Sym const* strtab);

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

    //Build ternary operation.
    IR * buildTernaryOp(IR_CODE irt, Type const* type, IN IR * opnd0,
                        IN IR * opnd1, IN IR * opnd2);

    //Build unary operation.
    IR * buildUnaryOp(IR_CODE irt, Type const* type, IN IR * opnd);

    //Build unary operation.
    IR * buildUnaryOp(IR_CODE irt, DATA_TYPE dt, IN IR * opnd)
    { return buildUnaryOp(irt, m_tm->getSimplexType(dt), opnd); }

    //Build While Do stmt.
    //det: determinate expression.
    //loop_body: stmt list.
    IR * buildWhileDo(IR * det, IR * loop_body);

    size_t count_mem() const;

    virtual bool dump() const;
    void dumpFreeTab() const;

    //This function erases all informations of ir and
    //append it into free_list for next allocation.
    //If Attach Info exist, this function will erase it rather than deletion.
    //Note that this function does NOT free ir's kids and siblings.
    void freeIR(IR * ir);

    IR * getFreeTabIRHead(UINT idx)
    {
        ASSERT0(idx < MAX_OFFSET_AT_FREE_TABLE + 1);
        return m_free_tab[idx];
    }

    //Generate a variable to inform compiler that this is a placeholder.
    Var * genInitPlaceHolderVar();
    Var * getInitPlaceHolderVar() const { return m_init_placeholder_var; }

    //The function attempts to generate DummyUse expression of CallStmt.
    //The DummyUse expression is used to represent the possible memory object
    //access during the Call processing.
    //For now, if DUMgr can not infer more precise MayRef of the CallStmt,
    //for conservative purposes, the worst-case MayRef will be used as the
    //reference for the DummyUse expression.
    //e.g: given foo(i:u64), if we do not known where i pointed to and
    //whether the body of foo will dereference i, the DummyUse of foo will be
    //the worst-case.
    void genDummyuseForCallStmt(IR * ir) const;

    UINT getIRCount() const { return m_ir_count; }

    //Return the vector that record all allocated IRs.
    Vector<IR*> & getIRVec() { return m_ir_vec; }

    virtual CHAR const* getPassName() const { return "IRMgr"; }
    virtual PASS_TYPE getPassType() const { return PASS_IRMGR; }

    //Return true if the ir is IR_ALLOCA.
    virtual bool isAllocaOp(IR const* ir) const
    {
        //Target Dependent Code
        return ir->hasRHS() && ir->getRHS()->is_alloca();
    }

    //Return true if ir tree is isomorphic to src.
    //ir: root of IR tree.
    //src: root of IR tree to be compared.
    //is_cmp_kid: it is true if comparing kids as well.
    //flag: record the checking condition while compare two given ir expression
    //      or stmt.
    //      e.g: If ISOMO_CK_CODE is set, the comparison of IST and ILD will
    //      return false.
    bool isIRIsomorphic(IR const* ir, IR const* src,
                        bool is_cmp_kid, IsomoFlag const& flag) const;
    bool isArrayIsomorphic(IR const* ir, IR const* src,
                           bool is_cmp_kid, IsomoFlag const& flag) const;
    bool isIRListIsomorphic(IR const* ir1lst, IR const* ir2lst,
                            bool is_cmp_kid, IsomoFlag const& flag) const;
    virtual bool isIRIsomorphicExtOp(
        IR const* ir, IR const* src, bool is_cmp_kid,
        IsomoFlag const& flag) const
    { ASSERTN(0, ("Target Dependent Code")); return false; }


    //stpr $0:u64
    //  case
    //    intconst:u64 0|0x0
    //    label LABEL
    //This IR represents saving position of a label in the instruction list,
    //that is, the PC value in the final machine instruction.
    //Note that the instruction combinations for this semantics may differ on
    //different architectures.
    virtual bool isPCSavingOp(IR const* ir) const
    {
        ASSERT0(ir);
        return ir->is_stpr() && ir->getRHS() != nullptr &&
               ir->getRHS()->is_case();
    }

    virtual IR * getCaseIRInPCSavingOp(IR const* ir) const
    {
        ASSERT0(isPCSavingOp(ir));
        IR * caseir = ir->getRHS();
        ASSERT0(caseir && CASE_vexp(caseir) && CASE_lab(caseir));
        return caseir;
    }

    //Note if the ir-count changed, the new generated IR id will start from the
    //new ir-count.
    void setIRCount(UINT cnt) { m_ir_count = cnt; }
    virtual bool perform(OptCtx & oc) { DUMMYUSE(oc); return false; }

    bool verify() const;
    static bool verify(Region const* rg);
    static bool verifyWhenFreeIR(IR const* ir, Region const* rg);
};

} //namespace xoc
#endif
