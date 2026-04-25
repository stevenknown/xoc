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
#ifndef _IR_LCSE_H_
#define _IR_LCSE_H_

namespace xoc {

class LCSE;

class LCSECtx : public PassCtx {
    COPY_CONSTRUCTOR(LCSECtx);
protected:
    LCSE * m_lcse;
    MDMgr * m_mdmgr;
public:
    //Record lived expression during analysis.
    xcom::BitSet avail_ir_expr;

    //Record the stmt as the position where the CSE begins to be available.
    //e.g:original code:
    //  ...=a+b  #S0
    //  .......
    //  ...=a+b  #S1
    //  .......
    //  ...=a+b  #S2
    //where a+b is CSE. However, there are three CSEs.
    //The object records the map between CSE and stmt at #S0.
    xcom::Vector<IR*> map_ie2avail_pos;
    xcom::Vector<IR*> map_ie2avail_exp_in_pos;

    //Record statement that computes the CSE.
    //e.g:original code:
    //  ...=a+b  #S1
    //where a+b is CSE, after hoisting the CSE,
    //  $t=a+b #S0
    //  ...=$t #S1
    //#S0 is the stmt that computes the CSE.
    //The object records the map between CSE and #S0.
    //NOTE: #S1 is the avail_pos of CSE.
    xcom::Vector<IR*> map_ie2avail_cse_comp_stmt;
    MDSet tmp; //for temp usage.
public:
    LCSECtx(OptCtx & oc, ActMgr * am, LCSE * lcse);
    ~LCSECtx();
    void clean();
    void dump() const;
    LCSE * getLCSE() const { return m_lcse; }
    MDMgr * getMDMgr() const { return m_mdmgr; }
    DefMiscBitSetMgr & getSBSMgr();

    //Return the avail_pos stmt if exist.
    IR * getAvailPos(ExprRep const* ie) const
     { return map_ie2avail_pos.get(ie->id()); }

    //Return the IR expression in avail_pos stmt if exist.
    IR * getAvailExpInPos(ExprRep const* ie) const
    { return map_ie2avail_exp_in_pos.get(ie->id()); }

    void invalidAvailPosAndComp(ExprRep const* ie);

    IR * recordAvailCseComp(ExprRep const* ie, IR * avail_cse_stmt)
    {
        ASSERT0(avail_cse_stmt && avail_cse_stmt->is_stmt());
        map_ie2avail_cse_comp_stmt.set(ie->id(), avail_cse_stmt);
        return avail_cse_stmt;
    }
    IR * recordAvailPos(
        ExprRep const* ie, IR * avail_pos_stmt, IR * avail_exp_in_pos)
    {
        ASSERT0(avail_exp_in_pos && avail_pos_stmt);
        ASSERT0(avail_exp_in_pos->is_exp());
        ASSERT0(avail_exp_in_pos->getParent() == avail_pos_stmt);
        map_ie2avail_pos.set(ie->id(), avail_pos_stmt);
        map_ie2avail_exp_in_pos.set(ie->id(), avail_exp_in_pos);
        return avail_pos_stmt;
    }
};


//LCSE
//Perform Local Common Subexpression Elimination.
class LCSE : public Pass {
    friend class LCSECtx;
    friend class LCSEIntlImpl;
    COPY_CONSTRUCTOR(LCSE);
protected:
    bool m_enable_filter; //filter determines which expression can be CSE.
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    TypeMgr * m_tm;
    ExprTab * m_expr_tab;
    DUMgr * m_du;
    DefMiscBitSetMgr m_misc_bs_mgr;
    ActMgr m_am;
protected:
    DefMiscBitSetMgr & getSBSMgr() { return m_misc_bs_mgr; }

    bool initDepPass(OptCtx const& oc);
    bool isAvailable(IR * def_stmt, IR * use_exp) const;

    //If there exists other IRs between 'def_stmt' and 'use_exp' within the
    //same basic block that may redefine or impact 'use_exp', then 'use_exp'
    //cannot be replaced by the common subexpression from 'def_stmt'.
    //E.g. BB1
    //  c = a + b #S1
    //  ...
    //  a = 1     #S2  ==> #S2 should block #S1 and #S3 from CSE because the
    //  ...                value of 'a' has changed.
    //  d = a + b #S3
    //Parameters:
    //  def_stmt: the statement that holds the common subexpression (c in #S1).
    //  use_exp: the expression that is to be replaced (a + b in #S3).
    bool isAvailableInSameBB(IR * def_stmt, IR * use_exp) const;

    bool processExpTreeOfReturnOp(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processExpTree(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processExpTreeList(
        MOD IRBB * bb, MOD IR * ir, IR * explist, MOD LCSECtx & ctx);
    bool processExpTreeOfMultiCondBranchOp(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    void processResult(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processStmt(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processExpTreeOfCallStmt(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processExpTreeOfDirectMemOp(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processExpTreeOfIndirectMemOp(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    virtual bool processExpTreeOfCondBranchOp(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processRHS(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processBase(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    bool processExpTreeOfWriteArray(
        MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx);
    virtual void processExtStmt(MOD IRBB *, MOD IR * ir, MOD LCSECtx &)
    {
        //Target Dependent Code.
        ASSERT0(ir->is_stmt());
        ASSERT0(ir->isExtOp());
    }
    virtual bool processExpTreeOfExtOp(MOD IRBB *, MOD IR * ir, MOD LCSECtx &)
    {
        //Target Dependent Code.
        ASSERT0(ir->is_stmt());
        ASSERT0(ir->isExtOp());
        return false;
    }

    //Return true if common expression has been substituted.
    bool processBB(MOD IRBB * bb, MOD LCSECtx & ctx);
    bool processBBList(OptCtx & oc);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit LCSE(Region * rg);
    virtual ~LCSE() {}

    virtual bool canBeCandidate(IR * ir);

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Local Command Subexpression Elimination"; }
    PASS_TYPE getPassType() const { return PASS_LCSE; }
    ExprTab * getExprTab() const { return m_expr_tab; }
    ActMgr & getActMgr() { return m_am; }

    void set_enable_filter(bool is_enable) { m_enable_filter = is_enable; }
    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
