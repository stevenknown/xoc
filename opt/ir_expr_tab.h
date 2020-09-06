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

#define EXPR_id(i) (i)->id
#define EXPR_ir(i) (i)->ir
#define EXPR_next(i) (i)->next
#define EXPR_prev(i) (i)->prev
#define EXPR_occ_list(i) (i)->occ_list
class ExpRep {
public:
    UINT id;
    IR * ir;
    ExpRep * next;
    ExpRep * prev;
    IRList occ_list;

public:
    ExpRep()
    {
        id = 0;
        next = prev = NULL;
    }
    COPY_CONSTRUCTOR(ExpRep);
    ~ExpRep() {}
};


//IR Expression Table, scanning statement to
//evaluate the hash value of expression.
//Compute LIVE IN and LIVE OUT IR expressions for each BB.
#define IR_EXPR_TAB_LEVEL1_HASH_BUCKET 256
#define IR_EXPR_TAB_LEVEL2_HASH_BUCKET 128
class ExprTab : public Pass {
    UINT m_expr_count; //the encode-number expression.
    Region * m_rg;
    TypeMgr * m_tm;
    BSVec<ExpRep*> m_ir_expr_vec;

    //Record allocated object. used by destructor.
    SList<ExpRep*> m_ir_expr_lst;

    ConstIRIter m_iter; //for tmp use.
    SMemPool * m_pool;
    SMemPool * m_sc_pool;
    ExpRep ** m_level1_hash_tab[IR_EXPR_TAB_LEVEL1_HASH_BUCKET];
    Vector<ExpRep*> m_map_ir2ir_expr;
    MDSetMgr * m_md_set_mgr; //alloca MS_SET.
    xcom::BitSetMgr * m_bs_mgr;

    void * xmalloc(INT size);
    inline UINT compute_hash_key(IR const* ir);
    inline UINT compute_hash_key_for_tree(IR * ir);
    inline ExpRep * encode_istore_memaddr(IN IR * ir)
    {
        ASSERT0(IR_parent(ir)->is_ist());
        if (ir->is_array()) {
            return NULL;
        }
        return encode_expr(ir);
    }

public:
    explicit ExprTab(Region * rg);
    COPY_CONSTRUCTOR(ExprTab);
    ~ExprTab();

    ExpRep * append_expr(IR * ir);
    void clean_occ_list();
    size_t count_mem();

    void dump_ir_expr_tab();
    ExpRep * encode_expr(IN IR * ir);
    void encode_bb(IRBB * bb);

    ExpRep * find_expr(IR * ir);

    ExpRep * get_expr(UINT id) { return m_ir_expr_vec.get(id); }
    BSVec<ExpRep*> * get_expr_vec() { return &m_ir_expr_vec; }

    ExpRep * map_ir2ir_expr(IR const* ir);
    ExpRep * new_ir_expr();

    IR * remove_occ(IR * occ);
    void remove_occs(IR * ir);
    ExpRep * remove_expr(IR * ir);
    void reperform(IN OUT OptCtx & oc);

    void set_map_ir2ir_expr(IR const* ir, ExpRep * ie);

    Region * getRegion() const { return m_rg; }
    PASS_TYPE getPassType() const { return PASS_EXPR_TAB; }
    virtual CHAR const* getPassName() const { return "Expr Table"; }

    virtual bool perform(IN OUT OptCtx & oc);
};

} //namespace xoc
#endif
