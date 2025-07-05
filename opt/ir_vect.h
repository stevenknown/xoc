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
#ifndef _IR_VECT_H_
#define _IR_VECT_H_

namespace xoc {

class IVBoundInfo;
class LICM;
class LICMAnaCtx;
class IVLinearRep;
class VectCtx;
class Vectorization;
class VectOp;
class VectOpMgr;

#define DEFAULT_MAX_VECTOR_REGISTER_BYTE_SIZE 512
#define VECTOP_ID_UNDEF 0

class VectActMgr : public ActMgr {
    COPY_CONSTRUCTOR(VectActMgr);
public:
    VectActMgr(Region const* rg) : ActMgr(rg) {}

    //Dump misc action that related to given ir.
    //format: the reason.
    void dumpAct(CHAR const* format, ...) const;
    void dumpAct(IR const* ir, CHAR const* format, ...) const;
    void dumpLinRepAct(IVLinearRep const& linrep,
                       CHAR const* format, ...) const;
};


//The class describes a vector operand or result.
class VectAccDesc {
    COPY_CONSTRUCTOR(VectAccDesc);
public:
    //Record the IR exp|stmt that is going to be vectorized.
    IR const* m_occ;

    //Record the linear access pattern of vector operation.
    IVLinearRep m_linear_access;
public:
    VectAccDesc(IR const* occ) : m_occ(occ) { ASSERT0(occ); }

    //Compute the byte size of stride according to index variable.
    //Return true if the stride byte size is immediate, and record the stride
    //to 'bytesize', otherwise return false that indicates the stride size is
    //compile-time variable.
    bool computeIndexVarStrideByteSize(
        TypeMgr const* tm, OUT HOST_INT & bytesize) const;

    //Compute the number of elements of stride according to index variable.
    //Return true if the stride is immediate, and record the number in
    //'elemnum', otherwise return false that indicates the stride is
    //compile-time variable.
    bool computeIndexVarStrideElemNum(OUT HOST_UINT & elemnum) const;

    void dump(Region const* rg) const;

    //Get the variable that described the index of vector operation.
    Var const* getIndexVar(Region const* rg) const
    { return m_linear_access.getVar(rg); }

    //Get the IR expression that described the index of vector operation.
    IR const* getIndexVarExp() const { return m_linear_access.getVarExp(); }

    //Get the expression|stmt that will be vectorized.
    IR const* getOcc() const { return m_occ; }

    //Get the linear access pattern of recognized vector operation.
    IVLinearRep const& getLinearAcc() const { return m_linear_access; }

    //Get the IV that will be the index of vector operation.
    IV const* getIV() const { return getLinearAcc().getIV(); }

    void set(IVLinearRep const& rep);
};


class VectAccDescMgr {
    COPY_CONSTRUCTOR(VectAccDescMgr);
    typedef xcom::TMap<IR const*, VectAccDesc*> IR2Desc;
    typedef xcom::TMapIter<IR const*, VectAccDesc*> IR2DescIter;
    IR2Desc m_ir2desc;
public:
    VectAccDescMgr() {}
    ~VectAccDescMgr() { clean(); }
    void clean();
    void dump(Region const* rg) const;
    VectAccDesc * genDesc(IR const* ir);
    VectAccDesc * getDesc(IR const* ir) const { return m_ir2desc.get(ir); }
};


class VectOpDesc {
public:
    typedef xcom::Vector<VectAccDesc> VectAccDescVec;
    IR_CODE vectopcode; //record the IR operation with vector type.
    //Record the operand of vectorized operation.
    VectAccDescVec vectopnd_desc_vec;
    VectAccDesc vectres_desc;
};


class VectOpOpnd {
    //ALLOW COPY CONSTRUCTOR.
public:
    //Record the operand of vectorized operation.
    IR const* opnd;

    //Record the operand's vector type. By default, the operand's
    //type should be equal to result's vector type.
    Type const* type;
public:
    VectOpOpnd() : opnd(nullptr), type(nullptr) {}
    VectOpOpnd(IR const* op, Type const* t) : opnd(op), type(t) {}
    VectOpOpnd(IR const* op) : opnd(op), type(op->getType()) {}
};
typedef xcom::Vector<VectOp const*> VectOpOpndVec;


//The class represents the formatted vector operation informations.
//The formatted information can be used to generate concrete IR stmt with
//vector type.
#define VECTOP_id(v) ((v)->m_id)
#define VECTOP_occ(v) ((v)->m_occ)
#define VECTOP_expected_type(v) ((v)->m_expected_type)
class VectOp {
    COPY_CONSTRUCTOR(VectOp);
public:
    UINT m_id;

    //The occurrence records the original IR before vectorization that will
    //be transformed to the expected vector operation.
    IR const* m_occ;

    //Record the expected vector type. By default, the operand's
    //expected vector type should be equal to result's vector type.
    Type const* m_expected_type;

    //Record the operand of vectorized operation.
    VectOpOpndVec m_opvec;
public:
    VectOp() { m_occ = nullptr; m_expected_type = nullptr; }

    void addOpnd(VectOp const* o) { m_opvec.append(o); }
    void addOpnd(IR const* occ, Type const* expty, MOD VectOpMgr & mgr);

    void dump(Region const* rg) const;

    IR_CODE getOccCode() const
    {
        ASSERT0(m_occ);
        return m_occ->getCode();
    }
    IR const* getOcc() const { return m_occ; }
    Type const* getExpectType() const { return m_expected_type; }
    UINT getNumOfOpnd() const { return m_opvec.get_elem_count(); }
    IR const* getOpndOcc(UINT idx) const { return getOpnd(idx)->getOcc(); }
    VectOp const* getOpnd(UINT idx) const
    {
        ASSERT0(idx < m_opvec.get_elem_count());
        ASSERT0(m_opvec.get(idx));
        return m_opvec.get(idx);
    }
    Type const* getOpndType(UINT idx) const
    { return getOpnd(idx)->getExpectType(); }
    VectOpOpndVec & getOpndVec() { return m_opvec; }

    //Return true if ir is one of the operand of current vector operation.
    bool isOpnd(IR const* ir) const;
    UINT id() const { return m_id; }

    bool verify() const;
};


class VectOpMgr {
    COPY_CONSTRUCTOR(VectOpMgr);
protected:
    xcom::List<VectOp*> m_oplst;
    UINT m_cnt;
public:
    VectOpMgr() { m_cnt = VECTOP_ID_UNDEF + 1; }
    ~VectOpMgr() { clean(); }
    VectOp * alloc()
    {
        VectOp * op = new VectOp();
        VECTOP_id(op) = m_cnt;
        m_cnt++;
        m_oplst.append_tail(op);
        return op;
    }
    void clean()
    {
        for (VectOp * op = m_oplst.get_head();
             op != nullptr; op = m_oplst.get_next()) {
            delete op;
        }
        m_oplst.clean();
    }
};


class VectCtx {
    COPY_CONSTRUCTOR(VectCtx);
public:
    typedef xcom::List<IR*> CandList;
    typedef xcom::List<IR*>::Iter CandListIter;
    typedef xcom::EList<IR*, IR2Holder> ResCandList;
    typedef xcom::EList<IR*, IR2Holder>::Iter ResCandListIter;
    typedef xcom::List<VectOp*> VOpList;
    typedef xcom::List<IR*> ResVOpList;
public:
    Vectorization * m_vect;
    Region * m_rg;
    IRMgr * m_irmgr;
    LI<IRBB> const* m_li;
    IVR const* m_ivr;
    CandList m_stmt_cand_list;
    IVBoundInfo const* m_iv_bound_info;
    LICMAnaCtx const* m_licm_anactx;
    LinearRepMgr * m_lrmgr;
    VectAccDescMgr * m_vectaccdesc_mgr;
    InferEVN * m_infer_evn;
    GVN * m_gvn;
    VarMgr * m_vm;
    MDSystem * m_mdsys;
    OptCtx const& m_oc;
    ResCandList m_rescand;
    ResCandList m_prerequisite_list;
    VOpList m_candvop_list;
    IRList m_generated_stmt_list;

    //Propagate information bottom up.
    //Record the generated vectorized operations. These vector operations will
    //replace original scalar operation and loop structure.
    ResVOpList m_resvop_list;
public:
    VectCtx(LI<IRBB> const* li, IVBoundInfo const* bi, OptCtx const& oc,
            Vectorization * vect, GVN * gvn);
    ~VectCtx()
    {
        delete m_lrmgr;
        delete m_vectaccdesc_mgr;
        m_infer_evn = nullptr;
    }

    //Add a new vectoized-operation into candidate operation list.
    //Note the vectorized-operation in the candidate operation list will be
    //executed sequentially.
    void addCandVOp(VectOp * vectop);
    void addPrerequisiteOp(IR * op);
    void addStmtCand(IR * ir);
    void addGeneratedStmt(IR * ir);

    void dump() const;

    Region * getRegion() const { return m_rg; }
    OptCtx const* getOptCtx() const { return &m_oc; }
    CandList & getCandList() { return m_stmt_cand_list; }
    BIV const* getBIV() const;
    VarMgr const* getVarMgr() const { return m_vm; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    MDSystem const* getMDSystem() const { return m_mdsys; }
    IVBoundInfo const* getIVBoundInfo() const { return m_iv_bound_info; }
    LI<IRBB> const* getLI() const { return m_li; }
    LICMAnaCtx const* getLICMAnaCtx() const { return m_licm_anactx; }
    VOpList & getCandVOpList() { return m_candvop_list; }
    ResVOpList & getResVOpList() { return m_resvop_list; }
    ResCandList & getResCandList() { return m_rescand; }
    ResCandList & getPrerequisiteOpList() { return m_prerequisite_list; }
    IRList & getGeneratedStmtList() { return m_generated_stmt_list; }
    LinearRepMgr & getLinearRepMgr() const { return *m_lrmgr; }
    VectAccDescMgr & getVectAccDescMgr() const { return *m_vectaccdesc_mgr; }
    InferEVN & getInferEVN() const { return *m_infer_evn; }

    bool isIV(IR const* ir) const;
    bool isRedStmt(IR const* ir) const;
    bool isIVEndBoundStmt(IR const* ir) const;
    bool isResCand(IR const* ir) const
    { return m_rescand.find(const_cast<IR*>(ir)); }

    //The function is a wrapper of IVBoundInfo.
    //Return true if trip-count is immediate.
    bool isTCImm() const;

    void removeStmtCand(IR * ir) { m_stmt_cand_list.remove(ir); }

    void setLICMAnaCtx(LICMAnaCtx const* anactx) { m_licm_anactx = anactx; }

    bool verify() const;
};


//This class represents loop vectorization.
class Vectorization : public Pass {
    COPY_CONSTRUCTOR(Vectorization);
    bool m_is_aggressive;
    SMemPool * m_pool;
    IVR * m_ivr;
    GVN * m_gvn;
    LICM * m_licm;
    DeadCodeElim * m_dce;
    IRCFG * m_cfg;
    TypeMgr * m_tm;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    DUMgr * m_dumgr;
    IRMgr * m_irmgr;
    LoopDepAna * m_loopdepana;
    OptCtx * m_oc;
    VectActMgr m_am;

    //VectOp is usually used inside each loop transformation.
    VectOpMgr m_vectop_mgr;
    DefMiscBitSetMgr m_sbs_mgr;
protected:
    bool addVectOpAndDepOpToBB(MOD VectCtx & ctx, MOD IRBB * bb) const;
    bool addReduceIVOpToBBAndRecordGeneratedOp(
        MOD VectCtx & ctx, MOD IRBB * bb) const;
    void addDUChain(IR * stmt, IR * exp, VectCtx const& ctx);

    //The function try to add DU chain for generated vector operations and
    //update the DU chain for removed operations.
    //Note the function doesn't do anything if SSA info is unavailable.
    //Return true if maintenance has succeeded.
    bool addDUChainForPROp(MOD SSARegion & ssarg, MOD OptCtx * oc) const;

    //The function try to add DU chain for generated vector operations and
    //update the DU chain for removed operations.
    //Note the function doesn't do anything if SSA info is unavailable.
    //Return true if maintenance has succeeded.
    bool addDUChainForNonPROp(MOD SSARegion & ssarg, MOD OptCtx * oc) const;

    //The function try to add DU chain for generated vector operations and
    //update the DU chain and DomInfo for removed operations.
    //Note the function doesn't do anything if SSA info is unavailable.
    //Return true if maintenance has succeeded.
    bool addDUChainForVectOp(
        VectCtx const& ctx, MOD IRBB * root, MOD OptCtx * oc) const;

    void analyzeDep(VectCtx const& ctx, OUT LDACtx & ldactx,
                    OUT LoopDepInfoSet & set) const;

    IR * buildVectPRResult(Type const* restype, IR * rhs, IRBB * bb);
    IR * buildRefIV(BIV const* biv) const;
    IR * buildRefIV(BIV const* biv, Type const* ty) const;

    //The function generates main vector loop, and insert all these BBs into
    //CFG. Note the function will recompute DU chain after construct the
    //vector-main-loop.
    //Return the root BB of the loop.
    IRBB * constructVectMainLoopAndUpdateCFG(
        MOD VectCtx & ctx, MOD OptCtx & oc) const;

    //The function generates main vector single BB, and insert the BB into
    //CFG. Note the function will maintain DU chain incrementally after
    //generating and inserting the BB.
    //Return the generated main BB.
    IRBB * constructVectMainBB(MOD VectCtx & ctx, MOD OptCtx & oc) const;
    bool constructSSARegion(VectCtx const& ctx, IRBB * root,
                            OUT SSARegion & ssarg) const;

    //Do some clean operations after current pass performed.
    void cleanAfterPass() { m_vectop_mgr.clean(); }

    //The function check whether the expected vector type is legal to given ir.
    //Return true if the expected vector type that generated by caller changed.
    bool checkAndRecomputeVectType(
        IR const* ir, VectCtx const& ctx, MOD VectOp & vectop);
    bool checkConst(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                    LoopDepInfoSet const& set) const;
    bool checkUna(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                  LoopDepInfoSet const& set) const;
    bool checkArrayOp(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                      LoopDepInfoSet const& set) const;
    bool checkIndirectOp(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                         LoopDepInfoSet const& set) const;
    bool checkDirectOp(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                       LoopDepInfoSet const& set) const;
    bool checkReadPR(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                     LoopDepInfoSet const& set) const;
    bool checkBin(IR const* ir, VectCtx const& ctx, LDACtx const& ldactx,
                  LoopDepInfoSet const& set) const;

    //The function checks wether 'exp' can be vectorized. If not, the function
    //will try searching through DefUse chain of 'exp' to find vectorizable
    //IR expression.
    //Return true if exp can be vectorized.
    bool checkExp(IR const* exp, VectCtx const& ctx, LDACtx const& ldactx,
                  LoopDepInfoSet const& set) const;

    //The function checks wether 'stmt' can be vectorized. If it is, the
    //function will also check whether it's RHS can be vectorized.
    //Return true if both stmt and its RHS can be vectorized.
    bool checkStmt(IR const* stmt, VectCtx const& ctx) const;

    //The function checks whether the result-candidate stmt may prevent
    //vectorization.
    //Return true if all stmts of result-candiate are legal to be vectoize.
    bool checkResultCand(VectCtx const& ctx, LDACtx const& ldactx,
                         LoopDepInfoSet const& set) const;

    //Check if there is scalar stmt that perform loop-reduce operation that
    //may prevent vectorization.
    //Return true if both stmt and its RHS can be vectorized.
    bool checkLoopReduceDep(VectCtx const& ctx,
                            LoopDepInfoSet const& set) const;

    //Return true if exp's MD reference is overlapped with other-stmt that in
    //the current loop.
    //Note the funcion will skip the stmt of 'exp' located.
    bool checkLoopCarrDep(VectCtx const& ctx, LoopDepInfoSet const& set) const;

    //Check if there is scalar stmt that may prevent vectorization.
    //Return true if both stmt and its RHS can be vectorized.
    bool checkScalarStmt(VectCtx const& ctx) const;

    //The function collect stmt that is suitable to vectorize from
    //given candidate list.
    //Return true if find legal stmt that can be vectorized.
    void collectResultCand(
        VectCtx const& ctx, OUT VectCtx::ResCandList & rescand) const;
    bool collectStmtCand(MOD VectCtx & ctx) const;
    void collectCandStmtToBeAnalyze(
        OUT IREList & sclst, OUT IRList & veccandlst, VectCtx const& ctx) const;

    //Return true if ir is a memory opearation that accesses memory through
    //a linear represented address expression.
    bool checkLinRepForArrayOp(
        IR const* ir, VectCtx const& ctx, OUT IVLinearRep & linrep) const;

    //Return true if ir is a memory opearation that accesses memory through
    //a linear represented address expression.
    bool checkLinRepForIndirectOp(
        IR const* ir, VectCtx const& ctx, OUT IVLinearRep & linrep) const;

    //Return true if ir can be vectorization operation candidate.
    bool canBeVectCand(IR const* ir) const;

    //Return true if ir can be valid operand in given vector operation.
    //This is an interface that described target dependent functions.
    //e.g:On some target machine, the vector operation is valid, and given
    //x is <3, 11, 2, 7>, perform add ild:vec<u32x4> x, #2, the result of x is
    //<5, 13, 4, 9>. The operation will add #2 to each elements in x.
    virtual bool canBeValidOpndInVectOp(
        IR const* ir, VectCtx const& ctx, VectOp const& vectop) const;
    bool canHoistToVecType(IR const* ir, VectCtx const& ctx) const;

    //doReplacement candidate IRs to preheader BB.
    //This function will maintain RPO if new BB inserted.
    //Return true if BB or STMT changed.
    bool doReplacement(OUT IRBB * prehead, OUT LI<IRBB> * li,
                       OUT bool & insert_bb, MOD VectCtx & ctx);

    //Return true if code changed.
    bool doLoopTree(LI<IRBB> * li, OUT bool & du_set_info_changed,
                    OUT bool & insert_bb, OptCtx & oc);

    bool estimateVectOp(VectCtx const& ctx);

    bool findSuitableVectOpnd(
        IR const* start, VectCtx const& ctx, LDACtx const& ldactx,
        LoopDepInfoSet const& set) const;

    DefMiscBitSetMgr & getSBSMgr() { return m_sbs_mgr; }
    VectOpMgr & getVectOpMgr() { return m_vectop_mgr; }
    LoopDepAna * getLoopDepAna() const { return m_loopdepana; }
    OptCtx * getOptCtx() const { return m_oc; }
    bool genDepOpOutsideLoop(MOD VectCtx & ctx) const;
    IR * genVectMainLoop(MOD VectCtx & ctx) const;
    IR * genBIVStrideStepExp(BIV const* biv, VectCtx const& ctx) const;
    IR * genRHSByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genConstByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genBinByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genUnaByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genArrayByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genIndirectByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genDirectByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genPRByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    IR * genScalarByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    virtual IR * genExpByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    virtual void genStmtByVectOp(VectOp const& vop, OUT VectCtx & ctx);
    virtual void genIRByVectOp(MOD VectCtx & ctx);

    bool hasUniqueBranchTarget(IR const* ir, LabelInfo const** tgtlab) const;

    //Add BB into SSARegion incrementally to update PRSSA.
    bool isIncrementalAddBB() const
    {
        //TBD:It looks like there is no more benefit to add BB incrementally
        //when CFG is complicated.
        return false;
    }
    bool isLoopInv(IR const* ir, VectCtx const& ctx) const;
    bool isDirectOpLegalToVect(IR const* ir, VectCtx const& ctx) const;
    bool isArrayOpLegalToVect(IR const* ir, VectCtx const& ctx) const;
    bool isLoopRedDepOfBIV(
        BIV const* biv, VectCtx const& ctx, LoopDepInfo const& info) const;
    bool isLoopRedDepOfBIV(VectCtx const& ctx, LoopDepInfo const& info) const;

    //Return true if ir is suitable to vectorize.
    //ir: stmt or expression.
    //accdesc: if 'ir' can be vectorized, accdesc will record the vector
    //         operation description.
    //Note the prerequisite is that 'ir' is legal to vectorize.
    virtual bool isSuitableToBeVect(IR const* ir, VectCtx const& ctx) const;

    //Return true if the step of IV of accdesc is suitable to vectorize.
    virtual bool isStrideSuitableToVect(
        VectAccDesc const& linrep, VectCtx const& ctx) const;

    //Return true is ir is legal to vectorize.
    //ir: stmt.
    bool isStmtLegalToVect(
        IR const* ir, LI<IRBB> const* li, IVBoundInfo const& bi,
        VectCtx const& ctx) const;
    bool isBranchLegalToVect(
        IR const* ir, LI<IRBB> const* li, IVBoundInfo const& bi) const;

    virtual Type const* makeVectType(Type const* elemty, VectCtx const& ctx);

    //The function try to generate vector operation through DefUse chain that
    //computes the anticipated vectorized intermediate value, which may be
    //stored in a vector-typed PR.
    //The intermediate value is always used to compose more complex vector
    //operation.
    //Return the true if vector operation generated.
    //ir: the original, scalar operation usually, IR expression.
    bool makeVectOpndByDUChain(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);

    //The function makes new vector operation according to given scalar
    //operation.
    virtual bool makeVectOpnd(
        IR const* exp, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByReadPR(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByConst(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByDirect(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByBin(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByUna(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByIndirect(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectOpndByArray(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);
    virtual bool makeVectResult(
        IR const* ir, MOD VectCtx & ctx, MOD VectOp & vectop);

    //The function will make vector operation according to the collected
    //result candidate in ResCandList in 'ctx'.
    //Note the function does not change the CFG and IR. All operations in
    //ResCandList shoudl be vectorized legally.
    //TODO:perform loop fission to peel the stmt that can not be vectorized.
    virtual bool makeVectOp(MOD VectCtx & ctx);

    //The function check whether vop's result has to store into a temporary
    //PR. The function is target dependent.
    //Return true if a temporary PR is necessary.
    virtual bool needStoreValueToPR(VectOp const& vop) const;

    //The function picks out all dependent stmts of each kid of 'ir'.
    void pickOutDepScalarOp(
        IR const* ir, MOD IREList & lst, VectCtx const& ctx) const;

    //Pick out scalar operations that are used to transfer middle value of
    //some operand of vector operation. Collect all these potential vector
    //candidates from scalar stmt list.
    //e.g:stpr $x = 0; #S1
    //    st:vec<i32*16> y = $x + ld:vec<i32*16> z; #S2
    //Because #S2 uses $x, the #S1 is a dependent scalar stmt.
    void pickOutTransferScalarOpOfVectOp(
        MOD IREList & sclst, IRList const& veccandlst,
        VectCtx const& ctx) const;
    void pickOutScalarStmt(MOD VectCtx & ctx) const;
    void pickOutIrrelevantStmtCand(MOD VectCtx & ctx) const;
    IR * pickOutBackEdgeJumpStmt(MOD VectCtx & ctx) const;
    IR * pickOutRedStmt(MOD VectCtx & ctx) const;
    IR * pickOutIVEndBoundStmt(MOD VectCtx & ctx) const;
    bool postProcessAfterReconstructLoop(VectCtx const& ctx, MOD OptCtx & oc);

    //Reconstruct CFG and perform misc CFG optimization if necessary.
    //Return true if CFG or IR changed.
    bool reconstructLoop(
        MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc);
    bool reconstructLoopWithExactTC(
        MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc);
    bool reconstructLoopWithVariantTC(
        MOD VectCtx & ctx, OUT bool & changed, MOD OptCtx & oc);
    void removeOrgScalarLoop(MOD VectCtx & ctx, MOD OptCtx & oc);
    void reset();

    bool tryVectorizeLoop(LI<IRBB> * li, MOD OptCtx & oc);

    bool useLICM() const;
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
    bool useGVN() const
    { return m_gvn != nullptr && m_gvn->is_valid(); }

    //The function try to generate legal vector operation according to info
    //in 'ctx'.
    bool vectorize(MOD VectCtx & ctx);
public:
    explicit Vectorization(Region * rg) : Pass(rg), m_am(rg)
    {
        ASSERT0(rg != nullptr);
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_pool = smpoolCreate(sizeof(LFRInfo) * 4, MEM_COMM);
        m_oc = nullptr;
        m_dumgr = nullptr;
        m_irmgr = nullptr;
        m_licm = nullptr;
        m_dce = nullptr;
        m_ivr = nullptr;
        m_gvn = nullptr;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_loopdepana = nullptr;
        m_is_aggressive = false;
    }
    virtual ~Vectorization() { smpoolDelete(m_pool); }

    //The function add 'exp' to the UseSet of deicated PRNO.
    void addUseToRelatedPROp(MOD IR * exp) const;

    void dumpAllAct() const { getActMgr().dump(); }
    bool dump(VectCtx const& ctx) const;
    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Vectorization"; }
    PASS_TYPE getPassType() const { return PASS_VECT; }
    VectActMgr const& getActMgr() const { return m_am; }
    IVR const* getIVR() const { return m_ivr; }

    //Return the maximum number of element in target machine vector register.
    HOST_UINT getMaxVectorElemNum(Type const* elemty, VectCtx const& ctx) const;

    //Return the bytesize of the maximum vector type in target machine
    //vector register.
    HOST_UINT getMaxVectorByteSize(Type const* elemty,
                                   VectCtx const& ctx) const;

    //The function return the number of maximum element in target machine
    //vector register.
    virtual HOST_UINT getMaxVectorRegisterByteSize() const
    {
        //Target Dependent Code.
        return DEFAULT_MAX_VECTOR_REGISTER_BYTE_SIZE;
    }

    //Return true if user ask to perform aggressive optimization that without
    //consideration of compilation time and memory.
    bool is_aggressive() const { return m_is_aggressive; }

    virtual bool perform(OptCtx & oc);

    //Set to true if user ask to perform aggressive optimization that without
    //consideration of compilation time and memory.
    void setAggressive(bool doit) { m_is_aggressive = doit; }
};

} //namespace xoc
#endif
