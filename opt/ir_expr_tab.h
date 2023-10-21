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
#ifndef _IR_EXPR_TAB_
#define _IR_EXPR_TAB_

namespace xoc {

#define EXPR_id(i) ((i)->id)
#define EXPR_ir(i) ((i)->ir)
#define EXPR_next(i) ((i)->next)
#define EXPR_prev(i) ((i)->prev)
#define EXPR_occ_list(i) ((i)->occ_list)
class ExprRep {
    COPY_CONSTRUCTOR(ExprRep);
public:
    UINT id;
    IR * ir;
    ExprRep * next;
    ExprRep * prev;
    IREList occ_list;
public:
    ExprRep() { id = 0; next = prev = nullptr; }
    void clean();
    void dump(Region const* rg) const;
    IREList & getOccList() { return occ_list; }
};


typedef BSVec<ExprRep*> ExprRepVec;

//IR Expression Table, scanning statement to
//evaluate the hash value of expression.
//Compute LIVE IN and LIVE OUT IR expressions for each BB.
#define IR_EXPR_TAB_LEVEL1_HASH_BUCKET 256
#define IR_EXPR_TAB_LEVEL2_HASH_BUCKET 128
class ExprTab : public Pass {
    COPY_CONSTRUCTOR(ExprTab);
protected:
    UINT m_expr_count; //the encodeExp-number expression.
    TypeMgr * m_tm;
    xcom::BitSetMgr * m_bs_mgr;
    SMemPool * m_pool;
    SMemPool * m_sc_pool;
    MDSetMgr * m_md_set_mgr; //alloca MS_SET.
    ExprRepVec m_ir_expr_vec;
    //Record allocated object. used by destructor.
    SList<ExprRep*> m_ir_expr_lst;
    ConstIRIter m_iter; //for tmp use.
    Vector<ExprRep*> m_map_ir2exprep;
    ExprRep ** m_level1_hash_tab[IR_EXPR_TAB_LEVEL1_HASH_BUCKET];
protected:
    ExprRep * allocExprRep();
    void cleanHashTab();
    HOST_UINT compute_hash_key(IR const* ir) const;
    HOST_UINT compute_hash_key_for_tree(IR const* ir);
    ExprRep * encodeBaseOfIST(IR * ir)
    {
        ASSERT0(ir->getParent()->is_ist());
        if (ir->is_array()) { return nullptr; }
        return encodeExp(ir);
    }
    void reset();
    void * xmalloc(INT size);
public:
    explicit ExprTab(Region * rg);
    virtual ~ExprTab();

    //Append IR tree expression into HASH table and return the entry-info.
    //If 'ir' has already been inserted in the table with an ExprRep,
    //get that and return.
    ExprRep * appendExp(IR * ir);

    size_t count_mem() const;

    bool dump() const;

    ExprRep * encodeExp(IR * ir);
    void encodeStmt(IR const* ir);

    //Encode expression for single BB.
    //Scan IR statement literally, and encoding it for generating
    //the unique id for each individual expressions, and update
    //the 'GEN-SET' and 'KILL-SET' of IR-EXPR for BB as well as.
    void encodeBB(IRBB const* bb);

    //Return entry-info if expression has been entered into HASH table,
    //otherwise return nullptr.
    ExprRep * findExp(IR * ir);

    //Return ExprRep by given id.
    ExprRep * getExp(UINT id) const { return m_ir_expr_vec.get(id); }
    ExprRepVec & getExpVec() { return m_ir_expr_vec; }

    //If 'ir' has been inserted in the table with an ExprRep,
    //get that and return.
    ExprRep * mapIR2ExprRep(IR const* ir) const;

    //Remove occurence of ExprRep.
    IR * removeOcc(IR * occ);

    //Remove all exp for given stmt out of occ list in expr-tab.
    void removeOccs(IR * ir);

    //Remove IR tree expression out of HASH table and return the removed
    //entry-info if it was existed.
    ExprRep * removeExp(IR * ir);

    void setMapIR2ExprRep(IR const* ir, ExprRep * ie);

    PASS_TYPE getPassType() const { return PASS_EXPR_TAB; }
    virtual CHAR const* getPassName() const { return "Expr Table"; }

    virtual bool perform(MOD OptCtx & oc);
};

} //namespace xoc
#endif
