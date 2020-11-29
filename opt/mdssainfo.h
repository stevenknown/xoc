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

//Initial version is an abstract description to indicate the imported DEF of
//each VMD. Especially for parameter, global variablers, imported variables.
//Local variables should have explicitly definition that VMD version must not
//be initial version.
#define MDSSA_INIT_VERSION 0
#define MDDEF_UNDEF 0

class MDDef;
class UseDefMgr;
class VMD;
class MDDefSet;
class MDSSAMgr;

class VMDVec : public Vector<VMD*> {
    COPY_CONSTRUCTOR(VMDVec);
public:
    VMDVec() {}
    //Find the VMD that have MD defined at given BB.
    VMD * findVMD(UINT bbid) const;
};


//Mapping from MD id to vector of VMD.
class UINT2VMDVec {
    COPY_CONSTRUCTOR(UINT2VMDVec);
protected:
    UINT m_threshold;
    Vector<VMDVec*> m_mdid2vmdvec_vec;
    TMap<UINT, VMDVec*> m_mdid2vmdvec_map;

public:
    UINT2VMDVec(UINT threshold = 1000) { init(threshold); }
    ~UINT2VMDVec() { destroy(); }
    //Count memory usage for current object.
    size_t count_mem() const;

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

    VMDVec * get(UINT mdid)
    {
        if (mdid < m_threshold) {
            return m_mdid2vmdvec_vec.get(mdid);
        }
        return m_mdid2vmdvec_map.get(mdid);
    }
    Vector<VMDVec*> * getVec() { return &m_mdid2vmdvec_vec; }
    TMap<UINT, VMDVec*> * getMap() { return &m_mdid2vmdvec_map; }

    void set(UINT mdid, VMDVec * vmdvec);
};


//Mapping from MD id to Stack of VMD.
typedef Vector<Stack<VMD*>*> UINT2VMDStack;


typedef enum _VOPND_CODE {
    VOPND_UNDEF = 0,
    VOPND_MD,
    VOPND_CONST,
} VOPND_CODE;


//Virtual Operand.
//This class represents the virtual operand that a stmt may defined or
//an expression may used. There are only 2 kind of virtual operand, MD and
//CONST.
//e.g: var a;
//     st a:any = 10;
//  Assume MD of a is MD10;
//  The virtual operand of 'a' represent MD10, and its code is VOPND_MD.
//e.g2: mdphi a_v1 = (a_v0, L1), (0x20, L3)
//  Given mdphi operation, the second operand is 0x20, a immediate.
//  Thus it's virtual operand code is VOPND_CONST.
#define VOPND_id(v) ((v)->m_id)
#define VOPND_code(v) ((v)->m_code)
class VOpnd {
    COPY_CONSTRUCTOR(VOpnd);
public:
    VOPND_CODE m_code;
    UINT m_id;

public:
    VOpnd();
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


//Virtual Const Operand.
//This class represents the virtual const operand that inherit from VOpnd.
//e.g: mdphi a_v1 = (a_v0, L1), (0x20, L3)
//  Given mdphi operation, the second operand is 0x20, a immediate.
//  Thus it's virtual operand code is VOPND_CONST.
#define VCONST_val(v) (((VConst*)v)->m_const_val)
class VConst : public VOpnd {
    COPY_CONSTRUCTOR(VConst);
public:
    IR const* m_const_val;

public:
    VConst();
    ~VConst();

    void clean()
    {
        VOpnd::clean();
        VCONST_val(this) = nullptr;
    }
};


//Versioning MD.
//This class represents the versioned MD.
//In MDSSA form, reference MD of stmt/expression will be versioned.
//Each MD will be assigned an unique version number at individual
//stmt/exp operation. Note the MD that without definition will be
//assigned zero version.
//e.g: st a:any = 10 + ld b; //S1
//     st a:any = 20; //S2
//  Assume a's reference MD is MD10.
//  Stmt S1 generate MD10v1, whereas stmt S2 generate MD10v2.
//  Assume b's reference MD is MD11, ld b generate MD11v0.
#define VMD_mdid(v) (((VMD*)v)->m_mdid)
#define VMD_version(v) (((VMD*)v)->m_version)
#define VMD_def(v) (((VMD*)v)->m_def_stmt)
#define VMD_occs(v) (((VMD*)v)->m_occs)
class VMD : public VOpnd {
    COPY_CONSTRUCTOR(VMD);
public:
    UINT m_version; //unique version of MD.
    UINT m_mdid; //id of current virtual MD.
    MDDef * m_def_stmt; //each versioned MD has an unique Definition.
    IRSet m_occs; //each versioned MD has a set of USE occurrences.

public:
    VMD();
    VMD(xcom::DefSegMgr * sm);
    ~VMD();

    void clean();

    void init(xcom::DefSegMgr * sm)
    {
        clean();
        VMD_occs(this).init(sm);
    }

    void destroy() { VMD_occs(this).destroy(); }
    void dump(Region const* rg) const; //Concisely dump
    void dump(Region const* rg, UseDefMgr const* usedefmgr) const;

    MDDef * getDef() const { return VMD_def(this); }
    IRSet * getUseSet() { return &VMD_occs(this); }

    //Return true if current VOpnd should have DEF stmt.
    bool hasDef() const { return version() != MDSSA_INIT_VERSION; }

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

typedef xcom::SEGIter * VOpndSetIter;

//Set of Virtual Operand.
class VOpndSet : public DefSBitSetCore {
    COPY_CONSTRUCTOR(VOpndSet);
public:
    VOpndSet() { xcom::DefSBitSetCore::init(); }
    //Should call clean() before destruction,
    //otherwise it will incur SegMgr assertion.
    ~VOpndSet() {}

    void append(VOpnd const* v, xcom::DefMiscBitSetMgr & m)
    { xcom::DefSBitSetCore::bunion(v->id(), m); }

    bool find(VOpnd const* v) const
    {
        ASSERT0(v);
        return xcom::DefSBitSetCore::is_contain(v->id());
    }

    void remove(VOpnd const* v, xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), m);
    }
};

typedef xcom::SEGIter * VOpndSetIter;

//Generate MDSSAInfo for individual memory-ref IR stmt/exp since each IR
//has its own specific MDSSA Memory Reference information.
//It sounds there might be some waste to memory if many IRs mdssa-reference
//could be represented by same MDSSAInfo. Nevertheless, the postulation
//is quite experimentally, and in practical very rarelly.
class MDSSAInfo : public MDSSAInfoAttachInfo {
    COPY_CONSTRUCTOR(MDSSAInfo);
protected:
    VOpndSet m_vopnd_set;

public:
    MDSSAInfo() {}

    //Add given IR expression to occurence set.
    //exp: IR expression to be added.
    void addUse(IR const* exp, IN UseDefMgr * usedefmgr);

    //Collect all USE, where USE is IR expression.
    void collectUse(OUT IRSet * set, UseDefMgr const* usedefmgr);

    void init() { BaseAttachInfo::init(AI_MD_SSA); }
    bool isUseReachable(IN UseDefMgr * usedefmgr, IR const* exp);

    void destroy(xcom::DefMiscBitSetMgr & m) { m_vopnd_set.clean(m); }
    void dump(MDSSAMgr const* mgr) const;

    VOpndSet * getVOpndSet() { return &m_vopnd_set; }

    VOpndSet const* readVOpndSet() const { return &m_vopnd_set; }

    //Remove given IR expression from occurence set.
    //exp: IR expression to be removed.
    void removeUse(IR const* exp, IN UseDefMgr * usedefmgr);
};


//This class represent MD Definition.
//A SSA definition of MD must be placed in BB. It could be either IR stmt
//or MDPhi.
#define MDDEF_id(m) (((MDDef*)m)->m_id)
#define MDDEF_bb(m) (((MDDef*)m)->m_bb)
#define MDDEF_is_phi(m) (((MDDef*)m)->m_is_phi)
#define MDDEF_result(m) (((MDDef*)m)->m_result)
#define MDDEF_prev(m) (((MDDef*)m)->m_prev)
#define MDDEF_nextset(m) (((MDDef*)m)->m_nextset)
#define MDDEF_occ(m) (((MDDef*)m)->m_occ)
class MDDef {
    COPY_CONSTRUCTOR(MDDef);
public:
    UINT m_id;
    VMD * m_result; //the MD defined.
    MDDef * m_prev; //the nearest previous MDDef.
    MDDefSet * m_nextset; //the nearest next MDDefs.
    IRBB * m_bb; //the BB in which phi placed.
    IR * m_occ; //record IR stmt.
    BYTE m_is_phi:1; //is MDPhi.

public:
    MDDef();

    //Before destruction, invoke clean() of m_nextset
    //to free memory resource.
    ~MDDef();

    void clean() { init(false); MDDEF_id(this) = MDDEF_UNDEF; }

    IRBB * getBB() const { return MDDEF_bb(this); }
    VMD * getResult() const { return MDDEF_result(this); }
    MDDef * getPrev() const { return MDDEF_prev(this); }
    MDDefSet * getNextSet() const { return MDDEF_nextset(this); }
    IR * getOcc() const { return MDDEF_occ(this); }

    UINT id() const { return MDDEF_id(this); }
    void init(bool is_phi)
    {
        MDDEF_bb(this) = nullptr;
        MDDEF_result(this) = nullptr;
        MDDEF_is_phi(this) = (BYTE)is_phi;
        MDDEF_prev(this) = nullptr;
        MDDEF_nextset(this) = nullptr;
        MDDEF_occ(this) = nullptr;
    }
    bool is_phi() const { return MDDEF_is_phi(this); }
    bool is_valid() const { return id() != MDDEF_UNDEF; }
};


typedef xcom::DefSBitSetIter MDDefSetIter;

//Set of MDDef.
class MDDefSet : public DefSBitSetCore {
    COPY_CONSTRUCTOR(MDDefSet);
public:
    MDDefSet() { xcom::DefSBitSetCore::init(); }

    //should call clean() before destruction,
    //otherwise it will incur SegMgr assertion.
    ~MDDefSet() {}

    void append(MDDef const* v, xcom::DefMiscBitSetMgr & m)
    { xcom::DefSBitSetCore::bunion(v->id(), m); }

    bool find(MDDef const* v) const
    {
        ASSERT0(v);
        return xcom::DefSBitSetCore::is_contain(v->id());
    }

    void remove(MDDef const* v, xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), m);
    }
};


//This class represent MD phi operation.
#define MDPHI_opnd_list(p) (((MDPhi*)p)->m_opnd_list)
class MDPhi : public MDDef {
    COPY_CONSTRUCTOR(MDPhi);
public:
    IR * m_opnd_list;
public:
    MDPhi();
    ~MDPhi();

    void clean() { init(); MDDEF_id(this) = MDDEF_UNDEF; }

    void init()
    {
        MDDef::init(true);
        m_opnd_list = nullptr;
    }

    void dump(Region const* rg, UseDefMgr const* mgr) const;

    IR * getOpndList() const { return m_opnd_list; }
    UINT getOpndNum() const { return xcom::cnt_list(getOpndList()); }
    VMD * getOpndVMD(IR const* opnd, UseDefMgr const* mgr) const;

    //Insert operand at given position.
    IR * insertOpndAt(MDSSAMgr * mgr, UINT pos, IRBB const* pred);

    void replaceOpnd(IR * oldopnd, IR * newopnd);
    void removeOpnd(IR * ir)
    {
        ASSERT0(xcom::in_list(getOpndList(), ir));
        xcom::remove(&MDPHI_opnd_list(this), ir);
    }

    bool hasNoOpnd() const { return getOpndList() == nullptr; }
};


class MDPhiList : public SList<MDPhi*> {
    COPY_CONSTRUCTOR(MDPhiList);
public:
    MDPhiList();
    ~MDPhiList();
};


typedef xcom::Vector<MDDef*> MDDefVec;

//This class manages MDSSAInfo object allocation and destroy.
class UseDefMgr {
    COPY_CONSTRUCTOR(UseDefMgr);
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
    Region * m_rg;
    MDSystem * m_md_sys;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::SC<VOpnd*> * m_free_sc_list;
    UINT m_def_count;
    UINT m_vopnd_count;
    xcom::Vector<MDSSAInfo*> m_mdssainfo_vec;
    MDDefVec m_def_vec;
    xcom::Vector<VOpnd*> m_vopnd_vec;
    xcom::Vector<MDPhiList*> m_philist_vec; //record the Phi list of BB.
    UINT2VMDVec m_map_md2vmd; //record version for each MD.
    MDSSAMgr * m_mdssa_mgr;

protected:
    void cleanOrDestroy(bool is_reinit);
    void destroyMD2VMDVec();

public:
    UseDefMgr(Region * rg, MDSSAMgr * mgr);
    ~UseDefMgr() { cleanOrDestroy(false); }

    MDSSAInfo * allocMDSSAInfo();
    MDPhi * allocMDPhi(UINT mdid, UINT num_operands);
    MDDef * allocMDDef();
    MDDefSet * allocMDDefSet();
    xcom::SC<VOpnd*> * allocSCVOpnd(VOpnd * opnd);
    VConst * allocVConst(IR const* ir);
    VMD * allocVMD(UINT mdid, UINT version);

    //Count memory usage for current object.
    size_t count_mem() const;
    void cleanMDSSAInfo(IR * ir);

    //Get MDSSAInfo of ir.
    MDSSAInfo * genMDSSAInfo(IR * ir);
    static MDSSAInfo * getMDSSAInfo(IR const* ir);
    Region * getRegion() const { return m_rg; }
    xcom::SC<VOpnd*> ** getFreeSCListAddress() { return &m_free_sc_list; }
    xcom::Vector<VOpnd*> * getVOpndVec() { return &m_vopnd_vec; }

    //Get specific VOpnd.
    VOpnd * getVOpnd(UINT i) const { return m_vopnd_vec.get(i); }

    //Get MDPhi list of specific BB, or generate the list if not exist.
    MDPhiList * genBBPhiList(UINT bbid);

    //Get MDPhi list of specific BB.
    MDPhiList * getBBPhiList(UINT bbid) const
    { return m_philist_vec.get(bbid); }

    MDDef * getMDDef(UINT id) const { return m_def_vec.get(id); }
    MDDefVec const* getMDDefVec() const { return &m_def_vec; }

    //Return VMD vector according to given mdid.
    VMDVec * getVMDVec(UINT mdid) const
    { return const_cast<UseDefMgr*>(this)->m_map_md2vmd.get(mdid); }

    //Get Versioned MD object by giving MD id and MD version.
    VMD * getVMD(UINT mdid, UINT version) const;

    void reinit() { cleanOrDestroy(true); }

    //Set MDSSAInfo of ir.
    void setMDSSAInfo(IR * ir, MDSSAInfo * mdssainfo);
};

} //namespace xoc
#endif
