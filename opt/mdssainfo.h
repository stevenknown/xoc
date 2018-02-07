/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
#ifndef _MDSSAINFO_H_
#define _MDSSAINFO_H_

namespace xoc {

class MDDef;
class UseDefMgr;
class VMD;
class MDDefSet;
class MDSSAMgr;

//Mapping from MD id to vector of VMD.
class UINT2VMDVec {
protected:
    UINT m_threshold;
    Vector<Vector<VMD*>*> m_mdid2vmdvec_vec;
    TMap<UINT, Vector<VMD*>*> m_mdid2vmdvec_map;
public:
    UINT2VMDVec(UINT threshold = 1000) { init(threshold); }
    COPY_CONSTRUCTOR(UINT2VMDVec);
    ~UINT2VMDVec() { destroy(); }

    UINT count_mem() const;

    void init(UINT threshold = 1000)
    {
        m_threshold = threshold;
        m_mdid2vmdvec_vec.init();
        m_mdid2vmdvec_map.init();
    }

    void destroy()
    {
        m_mdid2vmdvec_vec.destroy();
        m_mdid2vmdvec_map.destroy();
    }

    Vector<VMD*> * get(UINT mdid)
    {
        if (mdid < m_threshold) {
            return m_mdid2vmdvec_vec.get(mdid);
        }
        return m_mdid2vmdvec_map.get(mdid);
    }
    Vector<Vector<VMD*>*> * getVec() { return &m_mdid2vmdvec_vec; }
    TMap<UINT, Vector<VMD*>*> * getMap() { return &m_mdid2vmdvec_map; }

    void set(UINT mdid, Vector<VMD*> * vmdvec);
};


//Mapping from MD id to Stack of VMD.
typedef Vector<Stack<VMD*>*> UINT2VMDStack;


typedef enum _VOPND_CODE {
    VOPND_UNDEF = 0,
    VOPND_MD,
    VOPND_CONST,
} VOPND_CODE;


//Virtual Operand.
#define VOPND_id(v)         ((v)->m_id)
#define VOPND_code(v)       ((v)->m_code)
class VOpnd {
public:
    VOPND_CODE m_code;
    UINT m_id;

public:
    VOpnd();
    COPY_CONSTRUCTOR(VOpnd);
    ~VOpnd();

    void clean()
    {
        VOPND_id(this) = 0;
        VOPND_code(this) = VOPND_UNDEF;
    }
    VOPND_CODE code() const { return VOPND_code(this); }

    bool is_const() const { return VOPND_code(this) == VOPND_CONST; }
    bool is_md() const { return VOPND_code(this) == VOPND_MD; }
    UINT id() const { return VOPND_id(this); }
};


#define VCONST_val(v)      (((VConst*)v)->m_const_val)
class VConst : public VOpnd {
public:
    IR const* m_const_val;
public:
    VConst();
    COPY_CONSTRUCTOR(VConst);
    ~VConst();

    void clean()
    {
        VOpnd::clean();
        VCONST_val(this) = NULL;
    }
};


//Versioning MD.
#define VMD_mdid(v)       (((VMD*)v)->m_mdid)
#define VMD_version(v)    (((VMD*)v)->m_version)
#define VMD_def(v)        (((VMD*)v)->m_def_stmt)
#define VMD_occs(v)       (((VMD*)v)->m_occs)
class VMD : public VOpnd {
public:
    UINT m_version;
    UINT m_mdid;
    MDDef * m_def_stmt;
    IRSet m_occs;

public:
    VMD();
    VMD(DefSegMgr * sm);
    COPY_CONSTRUCTOR(VMD);
    ~VMD();

    void clean()
    {
        VOpnd::clean();
        VMD_def(this) = NULL;
        VMD_mdid(this) = 0;
        VMD_version(this) = 0;
        VMD_occs(this).clean();
    }

    void init(DefSegMgr * sm)
    {
        clean();
        VMD_occs(this).init(sm);
    }

    void destroy() { VMD_occs(this).destroy(); }
    void dump(Region * rg, UseDefMgr * usedefmgr);

    MDDef * getDef() const { return VMD_def(this); }
    IRSet * getOccSet() { return &VMD_occs(this); }

    UINT mdid() const
    {
        ASSERT0(is_md());
        return VMD_mdid(this);
    }

    UINT version() const
    {
        ASSERT0(is_md());
        return VMD_version(this);
    }
};


//Set of Virtual Operand.
class VOpndSet : public DefSBitSetCore {
public:
    VOpndSet() { DefSBitSetCore::init(); }
    COPY_CONSTRUCTOR(VOpndSet);

    //should call clean() before destruction,
    //otherwise it will incur SegMgr assertion.
    ~VOpndSet() {}

    void append(VOpnd const* v, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(v->id(), m); }

    bool find(VOpnd const* v) const
    {
        ASSERT0(v);
        return DefSBitSetCore::is_contain(v->id());
    }

    void remove(VOpnd const* v, DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        DefSBitSetCore::diff(v->id(), m);
    }
};

typedef SEGIter * VOpndSetIter;

class MDSSAInfo : public MDSSAInfoAttachInfo {
protected:
    VOpndSet m_vopnd_set;

public:
    MDSSAInfo() {}
    COPY_CONSTRUCTOR(MDSSAInfo);

    void init() { BaseAttachInfo::init(AI_MD_SSA); }
    void destroy(DefMiscBitSetMgr & m) { m_vopnd_set.clean(m); }

    VOpndSet * getVOpndSet() { return &m_vopnd_set; }

    bool isUseReachable(IN UseDefMgr * usedefmgr, IR const* exp);

    VOpndSet const* readVOpndSet() const { return &m_vopnd_set; }

    //Collect all USE, where USE is IR expression.
    void collectUse(OUT DefSBitSetCore & set,
            IN UseDefMgr * usedefmgr,
            IN DefMiscBitSetMgr * bsmgr);

    void removeUse(IR const* exp, IN UseDefMgr * usedefmgr);
};


//This class represent MD Definition.
#define MDDEF_id(m)       (((MDDef*)m)->m_id)
#define MDDEF_bb(m)       (((MDDef*)m)->m_bb)
#define MDDEF_is_phi(m)   (((MDDef*)m)->m_is_phi)
#define MDDEF_result(m)   (((MDDef*)m)->m_result)
#define MDDEF_prev(m)     (((MDDef*)m)->m_prev)
#define MDDEF_nextset(m)  (((MDDef*)m)->m_nextset)
#define MDDEF_occ(m)      (((MDDef*)m)->m_occ)
class MDDef {
public:
    UINT m_id;
    VMD * m_result; //the MD defined.
    MDDef * m_prev; //the nearest previous MDDef.
    MDDefSet * m_nextset; //the nearest next MDDefs.
    IRBB * m_bb;
    IR * m_occ; //record IR stmt.
    BYTE m_is_phi:1; //is MDPhi.

public:
    MDDef();
    COPY_CONSTRUCTOR(MDDef);

    //Before destruction, invoke clean() to free memory resource.
    ~MDDef();

    IRBB * getBB() const { return MDDEF_bb(this); }
    VMD * getResult() const { return MDDEF_result(this); }
    MDDef * getPrev() const { return MDDEF_prev(this); }
    MDDefSet * getNextSet() const { return MDDEF_nextset(this); }
    IR * getOcc() const { return MDDEF_occ(this); }

    UINT id() const { return MDDEF_id(this); }
    void init(bool is_phi)
    {
        MDDEF_bb(this) = NULL;
        MDDEF_result(this) = NULL;
        MDDEF_is_phi(this) = (BYTE)is_phi;
        MDDEF_prev(this) = NULL;
        MDDEF_nextset(this) = NULL;
        MDDEF_occ(this) = NULL;
    }
    bool is_phi() const { return MDDEF_is_phi(this); }
};


//Set of MDDef.
class MDDefSet : public DefSBitSetCore {
public:
    MDDefSet() { DefSBitSetCore::init(); }
    COPY_CONSTRUCTOR(MDDefSet);

    //should call clean() before destruction,
    //otherwise it will incur SegMgr assertion.
    ~MDDefSet() {}

    void append(MDDef const* v, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(v->id(), m); }

    bool find(MDDef const* v) const
    {
        ASSERT0(v);
        return DefSBitSetCore::is_contain(v->id());
    }

    void remove(MDDef const* v, DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        DefSBitSetCore::diff(v->id(), m);
    }
};


//This class represent MD phi operation.
#define MDPHI_opnd_list(p)   (((MDPhi*)p)->m_opnd_list)
class MDPhi : public MDDef {
public:
    IR * m_opnd_list;
public:
    MDPhi();
    COPY_CONSTRUCTOR(MDPhi);
    ~MDPhi();

    void init()
    {
        MDDef::init(true);
        m_opnd_list = NULL;
    }

    void dump(Region * rg, UseDefMgr * mgr);

    IR * getOpndList() const { return m_opnd_list; }
    VMD * getOpndVMD(IR const* opnd, UseDefMgr const* mgr) const;

    void replaceOpnd(IR * oldopnd, IR * newopnd);
};


class MDPhiList : public SList<MDPhi*> {
public:
    MDPhiList();
    COPY_CONSTRUCTOR(MDPhiList);
    ~MDPhiList();
};


class UseDefMgr {
friend class MDSSAMgr;
protected:
    SMemPool * m_phi_pool;
    SMemPool * m_def_pool;
    SMemPool * m_defset_pool;
    SMemPool * m_vopnd_sc_pool;
    SMemPool * m_vconst_pool;
    SMemPool * m_vmd_pool;
    SMemPool * m_philist_pool;
    SMemPool * m_philist_sc_pool;
    SMemPool * m_mdssainfo_pool;
    Region * m_ru;
    MDSystem * m_md_sys;
    DefMiscBitSetMgr * m_sbs_mgr;
    SC<VOpnd*> * m_free_sc_list;
    UINT m_def_count;
    UINT m_vopnd_count;
    Vector<MDSSAInfo*> m_mdssainfo_vec;
    Vector<MDDef*> m_def_vec;
    Vector<VOpnd*> m_vopnd_vec;
    Vector<MDPhiList*> m_philist_vec; //record the Phi list of BB.
    UINT2VMDVec m_map_md2vmd; //record version for each MD.
protected:
    void cleanOrDestroy(bool is_reinit);
    void destroyMD2VMDVec();
public:
    UseDefMgr(Region * rg);
    COPY_CONSTRUCTOR(UseDefMgr);
    ~UseDefMgr() { cleanOrDestroy(false); }

    void reinit() { cleanOrDestroy(true); }

    MDSSAInfo * allocMDSSAInfo();
    MDPhi * allocMDPhi(UINT mdid, UINT num_operands);
    MDDef * allocMDDef();
    MDDefSet * allocMDDefSet();
    SC<VOpnd*> * allocSCVOpnd(VOpnd * opnd);
    VConst * allocVConst(IR const* ir);
    VMD * allocVMD(UINT mdid, UINT version);
    size_t count_mem();

    MDSSAInfo * genMDSSAInfo(IR * ir);
    MDSSAInfo * getMDSSAInfo(IR const* ir) const;
    SC<VOpnd*> ** getFreeSCListAddress() { return &m_free_sc_list; }
    Vector<VOpnd*> * getVOpndVec() { return &m_vopnd_vec; }
    VOpnd * getVOpnd(UINT i) const { return m_vopnd_vec.get(i); }
    MDPhiList * genBBPhiList(UINT bbid);
    MDDef * getMDDef(UINT id) const { return m_def_vec.get(id); }
    Region * getRegion() const { return m_ru; }

    void setMDSSAInfo(IR * ir, MDSSAInfo * mdssainfo);
};

} //namespace xoc
#endif
