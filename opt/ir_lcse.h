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

//IR_LCSE
//Perform Local Common Subexpression Elimination.
class IR_LCSE : public Pass {
protected:
    bool m_enable_filter; //filter determines which expression can be CSE.
    Region * m_ru;
    TypeMgr * m_tm;
    IR_EXPR_TAB * m_expr_tab;
    IR_DU_MGR * m_du;
    BSVec<ExpRep*> * m_expr_vec;
    DefMiscBitSetMgr m_misc_bs_mgr;

    IR * hoist_cse(IRBB * bb,  IR * ir_pos, ExpRep * ie);
    bool processUse(IN IRBB * bb, IN IR * ir,
                    IN OUT xcom::BitSet & avail_ir_expr,
                    IN OUT Vector<IR*> & map_expr2avail_pos,
                    IN OUT Vector<IR*> & map_expr2avail_pr);
    bool processDef(IN IRBB * bb, IN IR * ir,
                    IN OUT xcom::BitSet & avail_ir_expr,
                    IN OUT Vector<IR*> & map_expr2avail_pos,
                    IN OUT Vector<IR*> & map_expr2avail_pr,
                    IN MDSet & tmp);
    bool processBranch(
                    IN IRBB * bb, IN IR * ir,
                    IN OUT xcom::BitSet & avail_ir_expr,
                    IN OUT Vector<IR*> & map_expr2avail_pos,
                    IN OUT Vector<IR*> & map_expr2avail_pr);
    IR * processExp(IN IRBB * bb, IN ExpRep * ie,
                    IN IR * stmt, IN OUT xcom::BitSet & avail_ir_expr,
                    IN OUT Vector<IR*> & map_expr2avail_pos,
                    IN OUT Vector<IR*> & map_expr2avail_pr);
    bool processParamList(
                    IN IRBB * bb, IN IR * ir,
                    IN OUT xcom::BitSet & avail_ir_expr,
                    IN OUT Vector<IR*> & map_expr2avail_pos,
                    IN OUT Vector<IR*> & map_expr2avail_pr);
    bool processRhsOfStore(
                    IN IRBB * bb, IN IR * ir,
                    IN OUT xcom::BitSet & avail_ir_expr,
                    IN OUT Vector<IR*> & map_expr2avail_pos,
                    IN OUT Vector<IR*> & map_expr2avail_pr);
public:
    explicit IR_LCSE(Region * rg);
    COPY_CONSTRUCTOR(IR_LCSE);
    virtual ~IR_LCSE() {}

    bool canBeCandidate(IR * ir);
    virtual CHAR const* getPassName() const
    { return "Local Command Subscript Elimination"; }

    PASS_TYPE getPassType() const { return PASS_LCSE; }

    void set_enable_filter(bool is_enable) { m_enable_filter = is_enable; }
    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
