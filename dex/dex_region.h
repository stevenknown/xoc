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
#ifndef _DEX_REGION_H_
#define _DEX_REGION_H_

class Dex2IR;

#define DR_functype(dr)     ((dr)->m_functype)
#define DR_funcname(dr)     ((dr)->m_funcname)
#define DR_classname(dr)    ((dr)->m_classname)

class DexRegion : public Region {
protected:
    VAR2PR m_var2pr; //map from var id to prno.
    Dex2IR * m_dex2ir;
    Prno2Vreg * m_prno2v;
    TypeIndexRep * m_type_index_rep;
    UINT m_paramnum;
    UINT m_org_vregnum;
    CHAR const* m_src_path; //record source file path of class.
    DexFile const* m_dexfile;
    DexMethod const* m_dexmethod;
    SMemPool * m_sc_pool; //a pool to hold the SC<LabelInfo*>
    Var2UINT m_var2fieldid; //map VAR to field-id.

protected:
    bool is_64bit(IR const* ir)
    { return getTypeMgr()->get_bytesize(ir->get_type())== 8; }

public:
    CHAR const* m_functype;
    CHAR const* m_funcname;
    CHAR const* m_classname;

public:
    DexRegion(REGION_TYPE rt, RegionMgr * rm) : Region(rt, rm)
    {
        m_dex2ir = NULL;
        m_prno2v = NULL;
        m_type_index_rep = NULL;
        m_paramnum = 0;
        m_org_vregnum = 0;
        m_src_path;
        m_dexfile = NULL;
        m_dexmethod = NULL;
        m_classname = NULL;
        m_funcname = NULL;
        m_functype = NULL;
        m_sc_pool = smpoolCreate(sizeof(SC<LabelInfo*>), MEM_CONST_SIZE);
    }
    virtual ~DexRegion() { smpoolDelete(m_sc_pool); }

    IR * gen_and_add_sib(IR * ir, UINT prno);
    DexPassMgr * getDexPassMgr() { return (DexPassMgr*)getPassMgr(); }
    Var2UINT * getVAR2Fieldid() { return &m_var2fieldid; }

    virtual bool HighProcess(OptCtx & oc);
    virtual bool MiddleProcess(OptCtx & oc);

    virtual PassMgr * allocPassMgr();
    virtual IR_AA * allocAliasAnalysis();

    SMemPool * get_sc_pool() const { return m_sc_pool; }

    void setDex2IR(Dex2IR * dex2ir) { m_dex2ir = dex2ir; }
    Dex2IR * getDex2IR() { return m_dex2ir; }

    void setPrno2Vreg(Prno2Vreg * p2v) { m_prno2v = p2v; }
    Prno2Vreg * getPrno2Vreg() { return m_prno2v; }

    void setTypeIndexRep(TypeIndexRep * tr) { m_type_index_rep = tr; }
    TypeIndexRep * getTypeIndexRep() { return m_type_index_rep; }

    void setParamNum(UINT num) { m_paramnum = num; }
    UINT getParamNum() const { return m_paramnum; }

    void setOrgVregNum(UINT num) { m_org_vregnum = num; }
    UINT getOrgVregNum() const { return m_org_vregnum; }

    void setClassSourceFilePath(CHAR const* path) { m_src_path = path; }
    CHAR const* getClassSourceFilePath() const { return m_src_path; }

    void setDexFile(DexFile const* df) { m_dexfile = df; }
    DexFile const* getDexFile() const { return m_dexfile; }

    void setDexMethod(DexMethod const* dm) { m_dexmethod = dm; }
    DexMethod const* getDexMethod() const { return m_dexmethod; }

    void updateRAresult(RA & ra, Prno2Vreg & prno2v);

    bool verifyRAresult(RA & ra, Prno2Vreg & prno2v);

    void process_group_bb(IRBB * bb, List<IR*> & lst);
    void process_group();
    void processSimply();
    virtual bool process(OptCtx * oc);
};

#endif
