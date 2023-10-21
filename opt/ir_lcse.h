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

//LCSE
//Perform Local Common Subexpression Elimination.
class LCSE : public Pass {
    COPY_CONSTRUCTOR(LCSE);
protected:
    bool m_enable_filter; //filter determines which expression can be CSE.
    PRSSAMgr * m_ssamgr;
    MDSSAMgr * m_mdssamgr;
    TypeMgr * m_tm;
    ExprTab * m_expr_tab;
    DUMgr * m_du;
    DefMiscBitSetMgr m_misc_bs_mgr;
protected:
    IR * hoistCse(IRBB * bb,  IR * ir_pos, ExprRep * ie);
    bool processUse(IN IRBB * bb, IN IR * ir,
                    MOD xcom::BitSet & avail_ir_expr,
                    MOD Vector<IR*> & map_expr2avail_pos,
                    MOD Vector<IR*> & map_expr2avail_pr);
    bool processDef(IN IRBB * bb, IN IR * ir,
                    MOD xcom::BitSet & avail_ir_expr,
                    MOD Vector<IR*> & map_expr2avail_pos,
                    MOD Vector<IR*> & map_expr2avail_pr,
                    IN MDSet & tmp);
    bool processBranch(IN IRBB * bb, IN IR * ir,
                       MOD xcom::BitSet & avail_ir_expr,
                       MOD Vector<IR*> & map_expr2avail_pos,
                       MOD Vector<IR*> & map_expr2avail_pr);
    IR * processExp(IN IRBB * bb, IN ExprRep * ie,
                    IN IR * stmt, MOD xcom::BitSet & avail_ir_expr,
                    MOD Vector<IR*> & map_expr2avail_pos,
                    MOD Vector<IR*> & map_expr2avail_pr);
    bool processParamList(IN IRBB * bb, IN IR * ir,
                          MOD xcom::BitSet & avail_ir_expr,
                          MOD Vector<IR*> & map_expr2avail_pos,
                          MOD Vector<IR*> & map_expr2avail_pr);
    bool processRHS(IN IRBB * bb, IN IR * ir,
                    MOD xcom::BitSet & avail_ir_expr,
                    MOD Vector<IR*> & map_expr2avail_pos,
                    MOD Vector<IR*> & map_expr2avail_pr);
public:
    explicit LCSE(Region * rg);
    virtual ~LCSE() {}

    bool canBeCandidate(IR * ir);

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const { return Pass::dump(); }

    virtual CHAR const* getPassName() const
    { return "Local Command Subexpression Elimination"; }
    PASS_TYPE getPassType() const { return PASS_LCSE; }

    void set_enable_filter(bool is_enable) { m_enable_filter = is_enable; }
    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
