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
#ifndef _IR_IVR_H_
#define _IR_IVR_H_

namespace xoc {

#define IV_INIT_VAL_IS_INT    0
#define IV_INIT_VAL_IS_VAR    1

//IV INFO.
#define IV_iv(d)            ((d)->iv)
#define IV_li(d)            ((d)->li)
#define IV_iv_def(d)        ((d)->iv_def)
#define IV_iv_occ(d)        ((d)->iv_occ)
#define IV_step(d)            ((d)->step)
#define IV_initv_i(d)        ((d)->u1.init_val_int)
#define IV_initv_md(d)        ((d)->u1.init_val_md)
#define IV_initv_type(d)    ((d)->init_val_type)
#define IV_is_inc(d)        ((d)->is_inc)
class IV {
public:
    LI<IRBB> const* li;
    MD * iv;
    IR * iv_def; //the unique stmt that defined iv in loop body.
    IR * iv_occ; //occrrence of iv in loop body.
    LONGLONG step; //step during each iteration, may be negative.
    union {
        LONGLONG * init_val_int; //integer initial value.
        MD const* init_val_md; //initial value is variable.
    } u1;
    BYTE is_inc:1; //true if iv is increment, or false means iv is decrement.
    BYTE init_val_type:1; //initial value may be integer or variable.
};


typedef TMap<UINT, IR*> UINT2IR;


//Induction Variable Recognization.
class IR_IVR : public Pass {
protected:
    Region * m_ru;
    MDSystem * m_md_sys;
    TypeMgr * m_dm;
    IR_DU_MGR * m_du;
    IR_CFG * m_cfg;
    SMemPool * m_pool;
    SMemPool * m_sc_pool;
    Vector<SList<IV*>*> m_li2bivlst;
    Vector<SList<IR const*>*> m_li2divlst;

    void addDIVList(LI<IRBB> const* li, IR const* e);
    IR const* computeDomDef(IR const* exp, IR const* exp_stmt,
                           SList<IR const*> * defs, bool omit_self);
    bool computeInitVal(IR const* ir, IV * iv);

    void findBIV(LI<IRBB> const* li, BitSet & tmp,
                  Vector<UINT> & map_md2defcount,
                  UINT2IR & map_md2defir);
    void findDIV(IN LI<IRBB> const* li, IN SList<IV*> const& bivlst,
                  BitSet & tmp);
    bool findInitVal(IV * iv);

    inline IV * newIV() { return (IV*)xmalloc(sizeof(IV)); }

    void _dump(LI<IRBB> * li, UINT indent);
    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }
    bool scanExp(IR const* ir, LI<IRBB> const* li, BitSet const& ivmds);
public:
    explicit IR_IVR(Region * ru)
    {
        ASSERT0(ru != NULL);
        m_ru = ru;
        m_md_sys = ru->get_md_sys();
        m_du = ru->get_du_mgr();
        m_cfg = ru->get_cfg();
        m_dm = ru->get_type_mgr();
        m_pool = smpoolCreate(sizeof(IV) * 4, MEM_COMM);
        m_sc_pool = smpoolCreate(sizeof(SC<IV*>) * 4, MEM_CONST_SIZE);
    }
    COPY_CONSTRUCTOR(IR_IVR);
    virtual ~IR_IVR()
    {
        smpoolDelete(m_pool);
        smpoolDelete(m_sc_pool);
    }

    void clean();
    void dump();
    Vector<SList<IV*>*> const* get_li2bivlst_map() const
    { return &m_li2bivlst; }

    virtual CHAR const* get_pass_name() const
    { return "Induction Variable Recogization"; }

    PASS_TYPE get_pass_type() const { return PASS_IVR; }

    bool is_loop_invariant(LI<IRBB> const* li, IR const* ir);
    virtual bool perform(OptCTX & oc);
};

} //namespace xoc
#endif
