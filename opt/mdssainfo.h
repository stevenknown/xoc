/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

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
    //Return true if the function find DEF in given BB.
    bool hasDefInBB(UINT bbid) const;
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
        VOPND_id(this) = VOPND_UNDEF;
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
    class UseSet : public xcom::TTab<UINT> {
    public:
        UINT append(UINT irid)
        {
            ASSERT0(irid != IR_UNDEF);
            return TTab<UINT>::append(irid);
        }
        void dump(Region const* rg) const;
    };
    typedef xcom::TTabIter<UINT> UseSetIter;

    UINT m_version; //unique version of MD.
    UINT m_mdid; //id of current virtual MD.
    MDDef * m_def_stmt; //each versioned MD has an unique Definition.
    UseSet m_occs; //each versioned MD has a set of USE occurrences.
public:
    //Add an USE occurrence.
    void addUse(IR const* ir) { VMD_occs(this).append(ir->id()); }
    void addUseSet(IRSet const& set, Region const* rg);

    void clean();
    void cleanUseSet() { VMD_occs(this).clean(); }

    //Return true if ir is an USE of current VMD.
    bool isUse(IR const* ir) const
    { return const_cast<VMD*>(this)->getUseSet()->find(ir->id()); }
    void init() { VMD_occs(this).init(); clean(); }

    void destroy() { VMD_occs(this).destroy(); }
    void dump(Region const* rg) const; //Concisely dump
    void dump(Region const* rg, UseDefMgr const* udmgr) const;
    CHAR const* dump(OUT xcom::StrBuf & buf) const;

    //Return true 'exp' is in the UseSet.
    //exp: IR expression to be found.
    bool findUse(IR const* exp) { return VMD_occs(this).find(exp->id()); }

    MDDef * getDef() const { return VMD_def(this); }
    UseSet * getUseSet() { return &VMD_occs(this); }
    MD const* getMD(MDSystem const* sys) const
    { return const_cast<MDSystem*>(sys)->getMD(mdid()); }

    //Return true if current VOpnd should have DEF stmt.
    bool hasDef() const { return version() != MDSSA_INIT_VERSION; }

    //Return true if current VOpnd has at least one USE.
    bool hasUse() const { return VMD_occs(this).get_elem_count() > 0; }

    //Return true if VMD is live-in version to current Region.
    bool isLiveIn() const { return version() == MDSSA_INIT_VERSION; }

    UINT mdid() const
    {
        ASSERT0(is_md());
        return VMD_mdid(this);
    }

    //Remove given IR expression from occurence set.
    //exp: IR expression to be removed.
    void removeUse(IR const* exp) { VMD_occs(this).remove(exp->id()); }

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

    void copy(VOpndSet const& src, xcom::DefMiscBitSetMgr & m)
    { xcom::DefSBitSetCore::copy(src, m); }

    void dump(Region const* rg) const;

    bool find(VOpnd const* v) const
    {
        ASSERT0(v);
        return xcom::DefSBitSetCore::is_contain(v->id());
    }

    //Return the unique MD if current set has only one element.
    VOpnd * get_unique(MDSSAMgr const* mgr) const;

    void remove(VOpnd const* v, xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), m);
    }
    void remove(VOpnd const* v, VOpndSetIter prev_it, VOpndSetIter it,
                xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), prev_it, it, m);
    }
};

typedef xcom::SEGIter * VOpndSetIter;

//Generate MDSSAInfo for individual memory-ref IR stmt/exp since each IR
//has its own specific MDSSA Memory Reference information.
//It sounds there might be some waste to memory if many IRs mdssa-reference
//could be represented by same MDSSAInfo.
//e.g: ... = ld x; //S1
//  where x has been assigned MDSSAInfo.
//  If you duplicate x at other occurrence, MDSSAInfo will be duplicated too.
//     ... = ld x; //S1
//     some stmts;
//     ... = ld x; //S2
//  Otherwise, if x at S1 and S2 shared the same MDSSAInfo object.
//  When some pass is going to remove S1, then the pass will remove VOpnd of x
//  from VOpndSet of MDSSAInfo. This will incur MDSSAInfo at S2 changed too.
//  This is incorrect for S2, and will lead assertion during verify().
//Nevertheless, the postulation is quite experimentally, and in practical
//very rarelly.
class MDSSAInfo : public MDSSAInfoAttachInfo {
    COPY_CONSTRUCTOR(MDSSAInfo);
    VOpndSet m_vopnd_set;
public:
    MDSSAInfo() {}

    //Add given IR expression to occurence set.
    //exp: IR expression to be added.
    void addUse(IR const* exp, UseDefMgr * mgr);
    void addUseSet(MDSSAInfo const* src, UseDefMgr * mgr);
    void addUseSet(IRSet const& set, IN UseDefMgr * mgr);
    void addVOpnd(VOpnd const* vopnd, UseDefMgr * mgr);

    void cleanVOpndSet(UseDefMgr * mgr);
    void copyVOpndSet(VOpndSet const& src, UseDefMgr * mgr);
    void copy(MDSSAInfo const& src, UseDefMgr * mgr)
    {
        ASSERT0(this != &src);
        copyVOpndSet(src.readVOpndSet(), mgr);
    }
    //The function copies partial contents in 'src' that are related to
    //specific MD 'md' to current MDSSAInfo.
    void copyBySpecificMD(MDSSAInfo const& src, MD const* md, UseDefMgr * mgr);

    //Return true if current MDSSAInfo contains given MD only.
    bool containSpecificMDOnly(MDIdx mdid, UseDefMgr const* udmgr) const;

    void destroy(xcom::DefMiscBitSetMgr & m) { m_vopnd_set.clean(m); }
    void dump(MDSSAMgr const* mgr) const;

    VOpndSet * getVOpndSet() { return &m_vopnd_set; }
    VOpnd * getVOpndForMD(UINT mdid, MDSSAMgr const* mgr) const;

    void init() { BaseAttachInfo::init(AI_MD_SSA); }

    //Return true if all definition of vopnds can reach 'exp'.
    bool isMustDef(UseDefMgr const* udmgr, IR const* exp) const;

    //Return true if current ssainfo is equal to src.
    bool isEqual(MDSSAInfo const& src) const;

    //Return true if 'ir' is an USE of one of VOpnds of current MDSSAInfo.
    //vmd: optional, if it is not NULL, record the VMD if ir is USE of the VMD.
    bool isUse(OUT VMD const** vmd, IR const* ir, MDSSAMgr const* mgr) const;

    //Return true if current MDSSAInfo does not have any VOpnd.
    bool isEmptyVOpndSet() const { return readVOpndSet().is_empty(); }

    //Return true if all VOpnds in current MDSSAInfo are live-in version.
    bool isLiveInVOpndSet(UseDefMgr const* mgr) const;

    VOpndSet const& readVOpndSet() const { return m_vopnd_set; }

    //Remove given IR expression from occurence set.
    //exp: IR expression to be removed.
    void removeUse(IR const* exp, IN UseDefMgr * udmgr);

    //Remove given IR expression from UseSet of each vopnd in MDSSAInfo.
    //Note current MDSSAInfo is the SSA info of 'exp'.
    //exp: IR expression to be removed.
    void removeSpecificUse(IR const* exp, MDIdx mdid, UseDefMgr * mgr);

    //Return true if there is VMD renamed.
    //Note current MDSSAInfo is the SSA info of 'exp', the VOpndSet will be
    //vmd: intent to be swap-in.
    bool renameSpecificUse(IR const* exp, MOD VMD * vmd, UseDefMgr * mgr);

    //Remove vopnd from current MDSSAInfo.
    void removeVOpnd(VOpnd const* vopnd, UseDefMgr * mgr);
};


//This class represent MD Definition.
//A SSA definition of MD must be placed in BB. It could be either IR stmt
//or MDPhi.
#define MDDEF_id(m) (((MDDef*)m)->m_id)
#define MDDEF_is_phi(m) (((MDDef*)m)->m_is_phi)
#define MDDEF_result(m) (((MDDef*)m)->m_result)
#define MDDEF_prev(m) (((MDDef*)m)->m_prev)
#define MDDEF_nextset(m) (((MDDef*)m)->m_nextset)
class MDDef {
    COPY_CONSTRUCTOR(MDDef);
    void eraseDefInfo()
    {
        ASSERT0(getResult()->getDef() == this);
        VMD_def(getResult()) = nullptr;
    }
public:
    BYTE m_is_phi:1; //true if MDDef indicates MDPhi.
    UINT m_id; //the unique ID.
    VMD * m_result; //the VMD defined.

    //The nearest previous MDDef. Note MDPhi does not have previous DEF.
    MDDef * m_prev;
    MDDefSet * m_nextset; //the nearest next MDDefs.
public:
    MDDef();
    //Before destruction, invoke clean() of m_nextset
    //to free memory resource.
    ~MDDef();

    void clean()
    {
        eraseDefInfo();
        init(false);
        //TBD:Keep id unchanged to facilitate MDDef dump.
        //MDDEF_id(this) = MDDEF_UNDEF;
    }
    void cleanNextSet(UseDefMgr * mgr);

    IRBB * getBB() const;
    IR * getOcc() const;
    VMD * getResult() const { return MDDEF_result(this); }
    MDDef * getPrev() const { return MDDEF_prev(this); }
    MDDefSet * getNextSet() const { return MDDEF_nextset(this); }
    MD const* getResultMD(MDSystem const* sys) const
    { return getResult()->getMD(sys); }

    //Note real-use does not include IR_ID.
    bool hasOutsideLoopRealUse(LI<IRBB> const* li, Region const* rg) const;

    //Return true if n is the Next DEF of current DEF.
    bool isNext(MDDef const* n) const;

    //Return true if ir is an USE of current MDDef.
    bool isUse(IR const* ir) const { return getResult()->isUse(ir); }

    //Return true if n is the Next DEF, and n may not be the
    //immediate-next-def to current DEF.
    //The function will access all next DEFs recursively.
    bool isInNextSet(MDDef const* n, UseDefMgr const* mgr) const;
    UINT id() const { return MDDEF_id(this); }
    void init(bool is_phi)
    {
        MDDEF_result(this) = nullptr;
        MDDEF_is_phi(this) = (BYTE)is_phi;
        MDDEF_prev(this) = nullptr;
        MDDEF_nextset(this) = nullptr;
    }
    bool is_phi() const { return MDDEF_is_phi(this); }
    bool is_valid() const { return id() != MDDEF_UNDEF; }
    //Return true if MDDef's result referenced given 'mdid'.
    bool isRefMD(MDIdx mdid) const { return getResult()->mdid() == mdid; }
    bool isRefSameMDWith(IR const* ir) const;
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

    //Return true if there is at least one element in MDDefSet that dominates v.
    bool hasAtLeastOneElemDom(MDDef const* v, MDSSAMgr const* mgr) const;

    void remove(MDDef const* v, xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), m);
    }
};


//This class represent MD stmt operation.
#define MDDEFSTMT_occ(m) (((MDDefStmt*)m)->m_occ)
class MDDefStmt : public MDDef {
public:
    IR * m_occ; //record the real IR stmt.
public:
    void clean() { init(); MDDEF_id(this) = MDDEF_UNDEF; }

    void init()
    {
        MDDef::init(false);
        MDDEFSTMT_occ(this) = nullptr;
    }
    IRBB * getBB() const { return getOcc()->getBB(); }
    IR * getOcc() const { return MDDEFSTMT_occ(this); }
};


//This class represent MD phi operation.
#define MDPHI_opnd_list(p) (((MDPhi*)p)->m_opnd_list)
#define MDPHI_bb(m) (((MDPhi*)m)->m_bb)
class MDPhi : public MDDef {
    COPY_CONSTRUCTOR(MDPhi);
    void dumpOpnd(IR const* opnd, IRBB const* pred, Region const* rg,
                  UseDefMgr const* mgr) const;
public:
    IR * m_opnd_list;
    IRBB * m_bb; //the BB in which phi placed.
public:
    void clean() { init(); MDDEF_id(this) = MDDEF_UNDEF; }

    void init()
    {
        MDDef::init(true);
        MDPHI_opnd_list(this) = nullptr;
        MDPHI_bb(this) = nullptr;
    }

    void dump(Region const* rg, UseDefMgr const* mgr) const;

    //Return true if MDPhi has given number of operand at least.
    bool hasNumOfOpndAtLeast(UINT n) const
    {
        IR * o = getOpndList();
        UINT i;
        for (i = 0; i < n && o != nullptr;
             i++, o = o->get_next()) {}
        return i == n;
    }

    IRBB * getBB() const { return MDPHI_bb(this); }

    //Get the No.idx operand, start at 0.
    IR * getOpnd(UINT idx) const;

    //Get the operand list.
    IR * getOpndList() const { return m_opnd_list; }

    //Get the number of operands.
    UINT getOpndNum() const { return xcom::cnt_list(getOpndList()); }

    //Get VMD for given operand. Note 'opnd' must belong to current phi.
    VMD * getOpndVMD(IR const* opnd, UseDefMgr const* mgr) const;

    //Return true if Phi defined real stmt at DefDef chain.
    //Note Phi should not have previous def.
    bool isDefRealStmt() const
    { return getNextSet() != nullptr && !getNextSet()->is_empty(); }

    void insertOpndAfter(IR * marker, IR * opnd)
    {
        ASSERT0(marker && opnd && opnd->is_id());
        ASSERT0(xcom::in_list(MDPHI_opnd_list(this), marker));
        ASSERT0(!xcom::in_list(MDPHI_opnd_list(this), opnd));
        ASSERT0(opnd->getRefMD() && opnd->getRefMDSet() == nullptr);
        xcom::insertafter(&marker, opnd);
        ID_phi(opnd) = this; //Record ID's host PHI.
    }
    //Insert operand at given position.
    IR * insertOpndAt(MDSSAMgr * mgr, UINT pos, IRBB const* pred,
                      OptCtx const& oc);

    void replaceOpnd(MOD IR * oldopnd, MOD IR * newopnd);
    void removeOpnd(IR * ir)
    {
        ASSERT0(xcom::in_list(getOpndList(), ir));
        xcom::remove(&MDPHI_opnd_list(this), ir);
    }

    bool hasNoOpnd() const { return getOpndList() == nullptr; }
};


typedef xcom::SC<MDPhi*> * MDPhiListIter;

class MDPhiList : public SList<MDPhi*> {
    COPY_CONSTRUCTOR(MDPhiList);
public:
    MDPhiList();
    ~MDPhiList();
};


typedef xcom::Vector<MDDef*> MDDefVec;
typedef xcom::Vector<VOpnd*> VOpndVec;


//This class manages MDSSAInfo object allocation and destroy.
class UseDefMgr {
    COPY_CONSTRUCTOR(UseDefMgr);
friend class MDSSAMgr;
protected:
    UINT m_def_count;
    UINT m_vopnd_count;
    SMemPool * m_phi_pool;
    SMemPool * m_defstmt_pool;
    SMemPool * m_defset_pool;
    SMemPool * m_vopnd_sc_pool;
    SMemPool * m_vconst_pool;
    SMemPool * m_vmd_pool;
    SMemPool * m_philist_pool;
    SMemPool * m_philist_sc_pool;
    SMemPool * m_mdssainfo_pool;
    Region * m_rg;
    IRMgr * m_irmgr;
    MDSystem * m_md_sys;
    MDSSAMgr * m_mdssa_mgr;
    xcom::SC<VOpnd*> * m_free_sc_list;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::Vector<MDSSAInfo*> m_mdssainfo_vec;
    xcom::Vector<VOpnd*> m_vopnd_vec;
    xcom::Vector<MDPhiList*> m_philist_vec; //record the Phi list of BB.
    MDDefVec m_def_vec;
    UINT2VMDVec m_map_md2vmd; //record version for each MD.

protected:
    void buildMDPhiOpnd(MDPhi * phi, UINT mdid, UINT num_operands);
    void cleanOrDestroy(bool is_reinit);
    void destroyMD2VMDVec();

public:
    UseDefMgr(Region * rg, MDSSAMgr * mgr);
    ~UseDefMgr() { cleanOrDestroy(false); }

    //Allocate MDPhi and initialize with the number of operands.
    //Each operands has zero version to mdid.
    MDPhi * allocMDPhi(UINT mdid);
    MDSSAInfo * allocMDSSAInfo();
    MDPhi * allocMDPhi(UINT mdid, UINT num_operands);
    MDDefStmt * allocMDDefStmt();
    MDDefSet * allocMDDefSet();
    xcom::SC<VOpnd*> * allocSCVOpnd(VOpnd * opnd);
    VConst * allocVConst(IR const* ir);
    VMD * allocVMD(UINT mdid, UINT version);

    //Count memory usage for current object.
    size_t count_mem() const;

    //Remove MDSSAInfo of 'ir'.
    void cleanMDSSAInfo(IR * ir);

    //Generate MDSSAInfo for individual Non-PR IR stmt/exp since each IR
    //has its own specific MDSSA Memory Reference information.
    //It sounds there might be some waste to memory if many IRs mdssa-reference
    //could be represented by same MDSSAInfo. Nevertheless, the postulation
    //is quite experimentally, and in practical very rarelly.
    MDSSAInfo * genMDSSAInfo(MOD IR * ir);

    //Get MDSSAInfo of 'ir' if any.
    static MDSSAInfo * getMDSSAInfo(IR const* ir);
    Region * getRegion() const { return m_rg; }

    //Get a cached SC container to manipulate VOpnd.
    xcom::SC<VOpnd*> ** getFreeSCListAddress() { return &m_free_sc_list; }

    //Get a vector that record all VOpnds.
    VOpndVec * getVOpndVec() { return &m_vopnd_vec; }

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
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr;  }
    MDSSAMgr * getMDSSAMgr() const { return m_mdssa_mgr; }

    //Reinitialize current object rather than destruction.
    void reinit() { cleanOrDestroy(true); }

    //The function remove and clean all information of 'vmd' from MDSSAMgr.
    void removeVMD(VMD * vmd);

    //The function remove and clean all information of a DEF from MDSSAMgr.
    void removeMDDef(MDDef * def);

    //Set 'mdssainfo' to ir.
    void setMDSSAInfo(IR * ir, MDSSAInfo * mdssainfo);
};

} //namespace xoc
#endif
