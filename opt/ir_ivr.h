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

class PRSSAMgr;
class MDSSAMgr;

#define IV_INIT_VAL_IS_CONST 1
#define IV_INIT_VAL_IS_VAR 0


//IV INFO.
#define IV_iv(d) ((d)->iv)
#define IV_li(d) ((d)->li)
#define IV_iv_def(d) ((d)->iv_def)
#define IV_iv_occ(d) ((d)->iv_occ)
#define IV_step(d) ((d)->step)
#define IV_initv_stmt(d) ((d)->init_val_stmt)
#define IV_initv_i(d) ((d)->u1.init_val_int)
#define IV_initv_md(d) ((d)->u1.init_val_md)
#define IV_initv_data_type(d) ((d)->init_val_data_type)
#define IV_initv_type(d) ((d)->init_val_type)
#define IV_is_inc(d) ((d)->is_inc)
class IV {
public:
    LI<IRBB> const* li;
    MD * iv;
    IR * iv_def; //the unique stmt that defined iv in loop body.
    IR * iv_occ; //occrrence of iv in loop body.
    LONGLONG step; //step during each iteration, may be negative.
    Type const* init_val_data_type; //record the Type of init value.
    IR const* init_val_stmt; //the unique stmt that initialize
                             //the IV outside the loop.
    union {
        LONGLONG * init_val_int; //integer initial value.
        MD const* init_val_md; //initial value is variable.
    } u1;
    BYTE is_inc:1; //true if iv is increment, or false means iv is decrement.
    BYTE init_val_type:1; //initial value may be integer or variable.

public:
    COPY_CONSTRUCTOR(IV);

    bool has_init_val() const { return IV_initv_i(this) != NULL; }

    //Return true if initial value is const.
    bool isInitConst() const
    { return IV_initv_type(this) == IV_INIT_VAL_IS_CONST; }

    Type const* getInitValType() const { return IV_initv_data_type(this); }
    IR const* getInitValStmt() const { return IV_initv_stmt(this); }
};


typedef TMap<UINT, IR*> UINT2IR;


//Induction Variable Recognization.
class IVR : public Pass {
    COPY_CONSTRUCTOR(IVR);
protected:
    Region * m_rg;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    DUMgr * m_du;
    IRCFG * m_cfg;
    SMemPool * m_pool;
    SMemPool * m_sc_pool;
    Vector<SList<IV*>*> m_li2bivlst;
    Vector<SList<IR const*>*> m_li2divlst;
    PRSSAMgr * m_ssamgr;
    MDSSAMgr * m_mdssamgr;

    //True if IVR pass only find BIV and DIV for exact MD IR.
    //Note if IR_ST, IR_LD, IR_PR, IR_STPR are VOID, the MD is inexact.
    bool m_is_only_handle_exact_md;

    //True if only strictly match the monotonic code pattern: i=i+1.
    bool m_is_strictly_match_pattern;

    //Map from a Def and Occ of basic induction var to its IV info.
    //This field will supply fast accessing to IV when giving an IR.
    TMap<IR const*, IV*> m_ir2iv;

protected:
    void addDIVList(LI<IRBB> const* li, IR const* e);
    IR const* computeDomDef(IR const* exp,
                            IR const* exp_stmt,
                            SList<IR const*> * defs,
                            bool omit_self);
    bool computeInitVal(IR const* ir, IV * iv);

    void findBIV(LI<IRBB> const* li,
                 xcom::BitSet & tmp,
                 Vector<UINT> & map_md2defcount,
                 UINT2IR & map_md2defir);
    void findDIV(LI<IRBB> const* li,
                 SList<IV*> const& bivlst,
                 xcom::BitSet & tmp);

    //Find initialze value of IV, if found return true,
    //otherwise return false.
    bool findInitVal(IV * iv);
    IR * findMatchedOcc(MD const* biv, IR * start);

    inline IV * allocIV() { return (IV*)xmalloc(sizeof(IV)); }

    void _dump(LI<IRBB> * li, UINT indent);
    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }
    bool matchIVUpdate(MD const* biv,
                       IR const* def,
                       IR ** occ,
                       IR ** delta,
                       bool & is_increment);
    bool scanExp(IR const* ir, LI<IRBB> const* li, xcom::BitSet const& ivmds);
    void recordIV(MD * biv,
                  LI<IRBB> const* li,
                  IR * def,
                  IR * occ,
                  IR * delta,
                  bool is_increment);

public:
    explicit IVR(Region * rg)
    {
        ASSERT0(rg != NULL);
        m_rg = rg;
        m_md_sys = rg->getMDSystem();
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_pool = smpoolCreate(sizeof(IV) * 4, MEM_COMM);
        m_sc_pool = smpoolCreate(sizeof(xcom::SC<IV*>) * 4, MEM_CONST_SIZE);
        m_is_only_handle_exact_md = true;
        m_is_strictly_match_pattern = false;
        m_ssamgr = NULL;
        m_mdssamgr = NULL;
    }
    virtual ~IVR()
    {
        smpoolDelete(m_pool);
        smpoolDelete(m_sc_pool);
    }

    void clean();
    void dump();

    //Given loop id, return the BIV list.
    SList<IV*> const* getBIVList(UINT loopid) const
    { return m_li2bivlst.get(loopid); }

    Region * getRegion() const { return m_rg; }
    IV const* getIV(IR const* ir) { return m_ir2iv.get(ir); }
    virtual CHAR const* getPassName() const
    { return "Induction Variable Recogization"; }
    PASS_TYPE getPassType() const { return PASS_IVR; }

    bool is_loop_invariant(LI<IRBB> const* li, IR const* ir);

    void setOnlyHandleExactMD(bool doit) { m_is_only_handle_exact_md = doit; }
    void setStrictlyMatchPattern(bool strictly)
    { m_is_strictly_match_pattern = strictly; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
