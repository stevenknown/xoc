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
#ifndef _REGSSAINFO_H_
#define _REGSSAINFO_H_

namespace xoc {

//Initial version is an abstract description to indicate the imported DEF of
//each VReg. Especially for parameter, global variablers, imported variables.
//Local variables should have explicitly definition that VReg version must not
//be initial version.
#define REGSSA_INIT_VERSION 0
#define REGDEF_UNDEF 0

class RegDef;
class RegDUMgr;
class VReg;
class RegDefSet;
class RegSSAMgr;
class RegSSAInfo;
class RegSSAStatus;

class VRegVec : public Vector<VReg*> {
    COPY_CONSTRUCTOR(VRegVec);
public:
    VRegVec() {}
    //Find the VReg that have Reg defined at given BB.
    //Return true if the function find DEF in given BB.
    bool hasDefInBB(UINT bbid) const;
};

typedef xcom::Vector<RegSSAInfo*> UINT2RegSSAInfo;

//Mapping from Reg to vector of VReg.
class UINT2VRegVec {
    COPY_CONSTRUCTOR(UINT2VRegVec);
protected:
    UINT m_threshold;
    Vector<VRegVec*> m_reg2vregvec_vec;
    TMap<UINT, VRegVec*> m_reg2vregvec_map;
public:
    UINT2VRegVec(UINT threshold = 1000) { init(threshold); }
    ~UINT2VRegVec() { destroy(); }
    //Count memory usage for current object.
    size_t count_mem() const;

    void init(UINT threshold = 1000)
    {
        m_threshold = threshold;
        m_reg2vregvec_vec.init();
        m_reg2vregvec_map.init();
    }

    void destroy()
    {
        m_reg2vregvec_vec.destroy();
        m_reg2vregvec_map.destroy();
    }

    VRegVec * get(xgen::Reg reg)
    {
        if (reg < m_threshold) {
            return m_reg2vregvec_vec.get(reg);
        }
        return m_reg2vregvec_map.get(reg);
    }
    Vector<VRegVec*> * getVec() { return &m_reg2vregvec_vec; }
    TMap<UINT, VRegVec*> * getMap() { return &m_reg2vregvec_map; }

    void set(Reg reg, VRegVec * vregvec);
};


typedef enum _VROPND_CODE {
    VROPND_UNDEF = 0,
    VROPND_REG,
    VROPND_CONST,
} VROPND_CODE;


//Virtual Operand.
//This class represents the virtual operand that a stmt may defined or
//an expression may used. There are only 2 kind of virtual operand, Reg and
//CONST.
//e.g: stpr $a:any = 10;
//  Assume Reg of a is R10;
//  The virtual operand of 'a' represent R10, and its code is VROPND_REG.
//e.g2: regphi $a_v1 = ($a_v0, L1), (0x20, L3)
//  Given regphi operation, the second operand is 0x20, a immediate.
//  Thus it's virtual operand code is VROPND_CONST.
#define VROPND_id(v) ((v)->m_id)
#define VROPND_code(v) ((v)->m_code)
class VROpnd {
    COPY_CONSTRUCTOR(VROpnd);
public:
    VROPND_CODE m_code;
    UINT m_id; //the unique id of current VROpnd.
public:
    VROpnd();
    ~VROpnd();

    void clean()
    {
        VROPND_id(this) = VROPND_UNDEF;
        VROPND_code(this) = VROPND_UNDEF;
    }
    VROPND_CODE code() const { return VROPND_code(this); }

    bool is_const() const { return VROPND_code(this) == VROPND_CONST; }
    bool is_reg() const { return VROPND_code(this) == VROPND_REG; }
    UINT id() const { return VROPND_id(this); }
};


//Virtual Const Operand.
//This class represents the virtual const operand that inherit from VROpnd.
//e.g: regphi a_v1 = (a_v0, L1), (0x20, L3)
//  Given regphi operation, the second operand is 0x20, a immediate.
//  Thus it's virtual operand code is VROPND_CONST.
#define VRCONST_val(v) (((VRConst*)v)->m_const_val)
class VRConst : public VROpnd {
    COPY_CONSTRUCTOR(VRConst);
public:
    IR const* m_const_val;
public:
    VRConst();
    ~VRConst();
    void clean()
    {
        VROpnd::clean();
        VCONST_val(this) = nullptr;
    }
};


//Versioning Reg.
//This class represents the versioned Reg.
//In RegSSA form, reference Reg of stmt/expression will be versioned.
//Each Reg will be assigned an unique version number at individual
//stmt/exp operation. Note the Reg that without definition will be
//assigned zero version.
//e.g: st a:any = 10 + ld b; //S1
//     st a:any = 20; //S2
//  Assume a's reference Reg is R10.
//  Stmt S1 generate R10v1, whereas stmt S2 generate R10v2.
//  Assume b's reference Reg is R11, ld b generate R11v0.
#define VREG_reg(v) (((VReg*)v)->m_reg)
#define VREG_version(v) (((VReg*)v)->m_version)
#define VREG_def(v) (((VReg*)v)->m_def_stmt)
#define VREG_occs(v) (((VReg*)v)->m_occs)

typedef xcom::FixedStrBuf<32> VRegFixedStrBuf;

class VReg : public VROpnd {
    COPY_CONSTRUCTOR(VReg);
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
public:
    UINT m_version; //unique version of Reg.
    Reg m_reg; //phyical register of current virtual Reg.
    RegDef * m_def_stmt; //each versioned Reg has an unique Definition.
    UseSet m_occs; //each versioned Reg has a set of USE occurrences.
public:
    //Add an USE occurrence.
    void addUse(IR const* ir) { VREG_occs(this).append(ir->id()); }
    void addUseSet(IRSet const& set, Region const* rg);

    void clean();
    void cleanUseSet() { VREG_occs(this).clean(); }

    void destroy() { VREG_occs(this).destroy(); }
    void dump(LinearScanRA const* ra) const; //Concisely dump
    void dump(Region const* rg, RegDUMgr const* mgr) const;
    CHAR const* dumpToBuf(
        OUT StrBuf & outbuf, Region const* rg, RegDUMgr const* mgr,
        UINT indent) const;
    CHAR const* dump(LinearScanRA const* ra, OUT VRegFixedStrBuf & buf) const;
    static void dumpRegAndVer(
        Region const* rg, Reg r, UINT ver, LinearScanRA const* ra);
    static CHAR const* dumpRegAndVer(
        OUT VRegFixedStrBuf & buf, Reg r, UINT ver, LinearScanRA const* ra);

    //Return true 'exp' is in the UseSet.
    //exp: IR expression to be found.
    bool findUse(IR const* exp) { return VREG_occs(this).find(exp->id()); }

    RegDef * getDef() const { return VREG_def(this); }
    UseSet * getUseSet() { return &VREG_occs(this); }

    //Return true if current VROpnd should have DEF stmt.
    bool hasDef() const { return version() != REGSSA_INIT_VERSION; }

    //Return true if current VROpnd has at least one USE.
    bool hasUse() const { return VREG_occs(this).get_elem_count() > 0; }

    //Return true if VReg is live-in version to current Region.
    bool isLiveIn() const { return version() == REGSSA_INIT_VERSION; }

    //Return true if ir is an USE of current VReg.
    bool isUse(IR const* ir) const
    { return const_cast<VReg*>(this)->getUseSet()->find(ir->id()); }
    void init() { VREG_occs(this).init(); clean(); }

    Reg reg() const
    {
        ASSERT0(is_reg());
        return VREG_reg(this);
    }

    //Remove given IR expression from occurence set.
    //exp: IR expression to be removed.
    void removeUse(IR const* exp) { VREG_occs(this).remove(exp->id()); }

    UINT version() const
    {
        ASSERT0(is_reg());
        return VREG_version(this);
    }
};

typedef xcom::DefSEGIter * VROpndSetIter;

//Set of Virtual Operand.
class VROpndSet : public DefSBitSetCore {
    COPY_CONSTRUCTOR(VROpndSet);
public:
    VROpndSet() { xcom::DefSBitSetCore::init(); }
    //Should call clean() before destruction,
    //otherwise it will incur SegMgr assertion.
    ~VROpndSet() {}

    void append(VROpnd const* v, xcom::DefMiscBitSetMgr & m)
    { xcom::DefSBitSetCore::bunion(v->id(), m); }

    void copy(VROpndSet const& src, xcom::DefMiscBitSetMgr & m)
    { xcom::DefSBitSetCore::copy(src, m); }

    void dump(Region const* rg) const;

    bool find(VROpnd const* v) const
    {
        ASSERT0(v);
        return xcom::DefSBitSetCore::is_contain(v->id());
    }

    //Return the unique Reg if current set has only one element.
    VROpnd * get_unique(RegSSAMgr const* mgr) const;

    void remove(VROpnd const* v, xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), m);
    }
    void remove(VROpnd const* v, VROpndSetIter prev_it, VROpndSetIter it,
                xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), prev_it, it, m);
    }
};

typedef xcom::DefSEGIter * VROpndSetIter;

//Generate RegSSAInfo for individual memory-ref IR stmt/exp since each IR
//has its own specific RegSSA Memory Reference information.
//It sounds there might be some waste to memory if many IRs regssa-reference
//could be represented by same RegSSAInfo.
//e.g: ... = ld x; //S1
//  where x has been assigned RegSSAInfo.
//  If you duplicate x at other occurrence, RegSSAInfo will be duplicated too.
//     ... = ld x; //S1
//     some stmts;
//     ... = ld x; //S2
//  Otherwise, if x at S1 and S2 shared the same RegSSAInfo object.
//  When some pass is going to remove S1, then the pass will remove VROpnd of x
//  from VROpndSet of RegSSAInfo. This will incur RegSSAInfo at S2 changed too.
//  This is incorrect for S2, and will lead assertion during verify().
//Nevertheless, the postulation is quite experimentally, and in practical
//very rarelly.
class RegSSAInfo {
    COPY_CONSTRUCTOR(RegSSAInfo);
    VROpndSet m_vropnd_set;
public:
    RegSSAInfo() {}

    //Add given IR expression to occurence set.
    //exp: IR expression to be added.
    void addUse(IR const* exp, RegDUMgr * mgr);
    void addUseSet(RegSSAInfo const* src, RegDUMgr * mgr);
    void addUseSet(IRSet const& set, IN RegDUMgr * mgr);
    void addVROpnd(VROpnd const* vopnd, RegDUMgr * mgr);

    void cleanVROpndSet(RegDUMgr * mgr);
    void copyVROpndSet(VROpndSet const& src, RegDUMgr * mgr);
    void copy(RegSSAInfo const& src, RegDUMgr * mgr)
    {
        ASSERT0(this != &src);
        copyVROpndSet(src.readVROpndSet(), mgr);
    }
    //The function copies partial contents in 'src' that are related to
    //specific Reg 'reg' to current RegSSAInfo.
    void copyBySpecificReg(RegSSAInfo const& src, Reg reg, RegDUMgr * mgr);

    //Return true if current RegSSAInfo contains given Reg only.
    bool containSpecificRegOnly(Reg reg, RegDUMgr const* mgr) const;

    void destroy(xcom::DefMiscBitSetMgr & m) { m_vropnd_set.clean(m); }
    void dump(RegSSAMgr const* mgr) const;

    VROpndSet * getVROpndSet() { return &m_vropnd_set; }
    VROpnd * getVROpndForReg(Reg reg, RegSSAMgr const* mgr) const;
    VROpnd * getUniqueVROpnd(RegSSAMgr const* mgr) const
    { return readVROpndSet().get_unique(mgr); }
    UINT getVROpndNum() const { return readVROpndSet().get_elem_count(); }

    //The function looks for the first RegDef that exactly define 'reg' in
    //current VROpndSet.
    RegDef const* findCoverRegDef(RegSSAMgr const* mgr, Reg reg) const;

    void init() { m_vropnd_set.init(); }

    //Return true if all definition of vopnds can reach 'exp'.
    bool isMustDef(RegDUMgr const* mgr, IR const* exp) const;

    //Return true if current ssainfo is equal to src.
    bool isEqual(RegSSAInfo const& src) const;

    //Return true if 'ir' is an USE of one of VROpnds of current RegSSAInfo.
    //vreg: optional, if it is not NULL, record the VReg if ir is USE
    //      of the VReg.
    bool isUse(OUT VReg const** vreg, IR const* ir, RegSSAMgr const* mgr) const;

    //Return true if current RegSSAInfo does not have any VROpnd.
    bool isEmptyVROpndSet() const { return readVROpndSet().is_empty(); }

    //Return true if all VROpnds in current RegSSAInfo are live-in version.
    //e.g:int g1, g2; //g, g2 is global variable.
    //  foo() { g2 = 10; return g1; }
    //  the Reg reference of LD:'g1' is {R7V0,R2V3}
    //  The function return false in the case because the MayRef includes
    //  R2V3 which is not livein.
    bool isLiveInVROpndSet(RegDUMgr const* mgr) const;
    bool isLiveInVROpndSet(RegSSAMgr const* mgr) const;

    VROpndSet const& readVROpndSet() const { return m_vropnd_set; }

    //Remove given IR expression from occurence set.
    //exp: IR expression to be removed.
    void removeUse(IR const* exp, IN RegDUMgr * mgr);

    //Remove given IR expression from UseSet of each vopnd in RegSSAInfo.
    //Note current RegSSAInfo is the SSA info of 'exp'.
    //exp: IR expression to be removed.
    void removeSpecificUse(IR const* exp, Reg reg, RegDUMgr * mgr);

    //Note current RegSSAInfo is the SSA info of 'exp'.
    //vreg: intent to be swap-in.
    //Return true if exp's VROpndSet info changed.
    bool renameOrAddSpecificUse(
        IR const* exp, MOD VReg * vreg, RegDUMgr * mgr);

    //Remove vopnd from current RegSSAInfo.
    void removeVROpnd(VROpnd const* vopnd, RegDUMgr * mgr);
};


//This class represents Reg Definition.
//A SSA definition of Reg must be placed in BB. It could be either IR stmt
//or RegPhi.
#define REGDEF_id(m) (((RegDef*)m)->m_id)
#define REGDEF_is_phi(m) (((RegDef*)m)->m_is_phi)
#define REGDEF_result(m) (((RegDef*)m)->m_result)
#define REGDEF_prev(m) (((RegDef*)m)->m_prev)
#define REGDEF_nextset(m) (((RegDef*)m)->m_nextset)
class RegDef {
    COPY_CONSTRUCTOR(RegDef);
    void eraseDefInfo()
    {
        ASSERT0(getResult()->getDef() == this);
        VREG_def(getResult()) = nullptr;
    }
public:
    bool m_is_phi; //true if RegDef indicates RegPhi.
    UINT m_id; //the unique ID.
    VReg * m_result; //the VReg defined.

    //The nearest previous RegDef. Note RegPhi does not have previous DEF.
    RegDef * m_prev;
    RegDefSet * m_nextset; //the nearest next RegDefs.
public:
    RegDef();
    //Before destruction, invoke clean() of m_nextset
    //to free memory resource.
    ~RegDef();

    void clean()
    {
        eraseDefInfo();
        init(false);
        //TBD:Keep id unchanged to facilitate RegDef dump.
        //REGDEF_id(this) = REGDEF_UNDEF;
    }
    void cleanNextSet(RegDUMgr * mgr);

    void dump(Region const* rg, RegDUMgr const* dumgr) const;

    IRBB * getBB() const;
    IR * getOcc() const;
    VReg * getResult() const { return REGDEF_result(this); }
    RegDef * getPrev() const { return REGDEF_prev(this); }
    RegDefSet * getNextSet() const { return REGDEF_nextset(this); }
    Reg getResultReg() const { return getResult()->reg(); }

    //Note real-use does not include IR_ID.
    bool hasOutsideLoopRealUse(LI<IRBB> const* li, Region const* rg) const;

    //Return true if n is the Next DEF of current DEF.
    bool isNext(RegDef const* n) const;

    //Return true if ir is an USE of current RegDef.
    bool isUse(IR const* ir) const { return getResult()->isUse(ir); }

    //Return true if n is the Next DEF, and n may not be the
    //immediate-next-def to current DEF.
    //The function will access all next DEFs recursively.
    bool isInNextSet(RegDef const* n, RegDUMgr const* mgr) const;
    UINT id() const { return REGDEF_id(this); }
    void init(bool is_phi)
    {
        REGDEF_result(this) = nullptr;
        REGDEF_is_phi(this) = (BYTE)is_phi;
        REGDEF_prev(this) = nullptr;
        REGDEF_nextset(this) = nullptr;
    }
    bool is_phi() const { return REGDEF_is_phi(this); }
    bool is_valid() const { return id() != REGDEF_UNDEF; }

    //Return true if RegDef's result referenced given 'reg'.
    bool isRefReg(Reg reg) const { return getResult()->reg() == reg; }
};


typedef xcom::DefSBitSetIter RegDefSetIter;

//Set of RegDef.
class RegDefSet : public xcom::DefSBitSetCore {
    COPY_CONSTRUCTOR(RegDefSet);
public:
    RegDefSet() { xcom::DefSBitSetCore::init(); }

    //should call clean() before destruction,
    //otherwise it will incur SegMgr assertion.
    ~RegDefSet() {}

    void append(RegDef const* v, xcom::DefMiscBitSetMgr & m)
    { xcom::DefSBitSetCore::bunion(v->id(), m); }

    bool find(RegDef const* v) const
    {
        ASSERT0(v);
        return xcom::DefSBitSetCore::is_contain(v->id());
    }

    //Return true if there is at least one element in RegDefSet that
    //dominates v.
    bool hasAtLeastOneElemDom(RegDef const* v, RegSSAMgr const* mgr) const;

    void remove(RegDef const* v, xcom::DefMiscBitSetMgr & m)
    {
        ASSERT0(v);
        xcom::DefSBitSetCore::diff(v->id(), m);
    }
};


//This class represent Reg stmt operation.
#define REGDEFSTMT_occ(m) (((RegDefStmt*)m)->m_occ)
class RegDefStmt : public RegDef {
public:
    IR * m_occ; //record the real IR stmt.
public:
    void clean() { init(); REGDEF_id(this) = REGDEF_UNDEF; }

    void init()
    {
        RegDef::init(false);
        REGDEFSTMT_occ(this) = nullptr;
    }
    IRBB * getBB() const { return getOcc()->getBB(); }
    IR * getOcc() const { return REGDEFSTMT_occ(this); }
};


//This class represent Reg phi operation.
#define REGPHI_opnd_list(p) (((RegPhi*)p)->m_opnd_list)
#define REGPHI_bb(m) (((RegPhi*)m)->m_bb)
class RegPhi : public RegDef {
    COPY_CONSTRUCTOR(RegPhi);
    void dumpOpnd(IR const* opnd, IRBB const* pred, Region const* rg,
                  RegDUMgr const* mgr) const;
public:
    IR * m_opnd_list;
    IRBB * m_bb; //the BB in which phi placed.
public:
    void clean() { init(); REGDEF_id(this) = REGDEF_UNDEF; }

    void init()
    {
        RegDef::init(true);
        REGPHI_opnd_list(this) = nullptr;
        REGPHI_bb(this) = nullptr;
    }

    void dump(Region const* rg, RegDUMgr const* mgr) const;

    //Return true if RegPhi has given number of operand at least.
    bool hasNumOfOpndAtLeast(UINT n) const
    {
        IR * o = getOpndList();
        UINT i;
        for (i = 0; i < n && o != nullptr; i++, o = o->get_next()) {}
        return i == n;
    }

    IRBB * getBB() const { return REGPHI_bb(this); }

    //Get the No.idx operand, start at 0.
    IR * getOpnd(UINT idx) const;

    //Get the operand list.
    IR * getOpndList() const { return m_opnd_list; }

    //Get the number of operands.
    UINT getOpndNum() const { return xcom::cnt_list(getOpndList()); }

    //Get VReg for given operand. Note 'opnd' must belong to current phi.
    VReg * getOpndVReg(IR const* opnd, RegDUMgr const* mgr) const;

    //Return true if Phi defined real stmt at DefDef chain.
    //Note Phi should not have previous def.
    bool isDefRealStmt() const
    { return getNextSet() != nullptr && !getNextSet()->is_empty(); }
    void insertOpndAfter(IR * marker, IR * opnd);

    //Insert operand at given position.
    //pos: position of operand, start at 0.
    //     Each operand correspond to in-edge on CFG.
    //NOTE: the function will attempt to find the latest live-in version
    //of the new operand Reg of the PHI.
    //st: record the status when the function can not find the Dom-LiveIn-Def.
    IR * insertOpndAt(RegSSAMgr * mgr, UINT pos, IRBB const* pred,
                      OptCtx const& oc, OUT RegSSAStatus & st);

    void replaceOpnd(MOD IR * oldopnd, MOD IR * newopnd);
    void removeOpnd(IR * ir)
    {
        ASSERT0(xcom::in_list(getOpndList(), ir));
        xcom::remove(&REGPHI_opnd_list(this), ir);
    }

    bool hasNoOpnd() const { return getOpndList() == nullptr; }
};


typedef xcom::SC<RegPhi*> * RegPhiListIter;

class RegPhiList : public SList<RegPhi*> {
    COPY_CONSTRUCTOR(RegPhiList);
public:
    RegPhiList();
    ~RegPhiList();
};

class CompareConstRegDefFunc {
public:
    bool is_less(RegDef const* t1, RegDef const* t2) const
    { return t1->id() < t2->id(); }
    bool is_equ(RegDef const* t1, RegDef const* t2) const { return t1 == t2; }
    RegDef const* createKey(RegDef const* t) { return t; }
};

typedef xcom::Vector<RegDef*> RegDefVec;
typedef xcom::Vector<VROpnd*> VROpndVec;

//This class manages RegSSAInfo object allocation and destroy.
class RegDUMgr {
    COPY_CONSTRUCTOR(RegDUMgr);
    friend class RegSSAMgr;
protected:
    UINT m_def_count;
    UINT m_vropnd_count;
    SMemPool * m_phi_pool;
    SMemPool * m_defstmt_pool;
    SMemPool * m_defset_pool;
    SMemPool * m_vropnd_sc_pool;
    SMemPool * m_vrconst_pool;
    SMemPool * m_vreg_pool;
    SMemPool * m_philist_pool;
    SMemPool * m_philist_sc_pool;
    SMemPool * m_regssainfo_pool;
    Region * m_rg;
    IRMgr * m_irmgr;
    RegSSAMgr * m_regssa_mgr;
    TargInfoMgr * m_timgr;
    xcom::SC<VROpnd*> * m_free_sc_list;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::Vector<RegSSAInfo*> m_regssainfo_vec;
    xcom::Vector<VROpnd*> m_vropnd_vec;
    xcom::Vector<RegPhiList*> m_philist_vec; //record the Phi list of BB.
    RegDefVec m_def_vec; //map from id to a RegDef object.
    UINT2VRegVec m_map_reg2vreg; //record version for each Reg.
    UINT2RegSSAInfo m_irid2regssainfo; //record regssainfo for each IR.
protected:
    void buildRegPhiOpnd(RegPhi * phi, Reg reg, UINT num_operands);
    void cleanOrDestroy(bool is_reinit);
    void destroyReg2VRegVec();
    void destroyAllRegSSAInfo();
public:
    RegDUMgr(Region * rg, RegSSAMgr * mgr);
    ~RegDUMgr() { cleanOrDestroy(false); }

    //Allocate RegPhi and initialize with the number of operands.
    //Each operands has zero version to reg.
    RegPhi * allocRegPhi(Reg reg);
    RegSSAInfo * allocRegSSAInfo();
    RegPhi * allocRegPhi(Reg reg, UINT num_operands);
    RegDefStmt * allocRegDefStmt();
    RegDefSet * allocRegDefSet();
    xcom::SC<VROpnd*> * allocSCVROpnd(VROpnd * opnd);
    VRConst * allocVRConst(IR const* ir);
    VReg * allocVReg(Reg reg, UINT version);

    //Count memory usage for current object.
    size_t count_mem() const;

    //Remove RegSSAInfo of 'ir'.
    void cleanRegSSAInfo(IR * ir);
    void cleanAllRegSSAInfo();

    //Generate RegSSAInfo for individual Non-PR IR stmt/exp since each IR
    //has its own specific RegSSA Memory Reference information.
    //It sounds there might be some waste to memory if many IRs regssa-reference
    //could be represented by same RegSSAInfo. Nevertheless, the postulation
    //is quite experimentally, and in practical very rarelly.
    RegSSAInfo * genRegSSAInfo(MOD IR * ir);

    //Get RegSSAInfo of 'ir' if any.
    RegSSAInfo * getRegSSAInfo(IR const* ir) const;
    Region * getRegion() const { return m_rg; }

    //Get a cached SC container to manipulate VROpnd.
    xcom::SC<VROpnd*> ** getFreeSCListAddress() { return &m_free_sc_list; }

    //Get a vector that record all VROpnds.
    VROpndVec * getVROpndVec() { return &m_vropnd_vec; }

    //Get specific VROpnd.
    VROpnd * getVROpnd(UINT i) const { return m_vropnd_vec.get(i); }

    //Get RegPhi list of specific BB, or generate the list if not exist.
    RegPhiList * genBBPhiList(UINT bbid);

    //Get RegPhi list of specific BB.
    RegPhiList * getBBPhiList(UINT bbid) const
    { return m_philist_vec.get(bbid); }

    RegDef * getRegDef(UINT id) const { return m_def_vec.get(id); }
    RegDefVec const* getRegDefVec() const { return &m_def_vec; }

    //Return VReg vector according to given reg.
    VRegVec * getVRegVec(Reg reg) const
    { return const_cast<RegDUMgr*>(this)->m_map_reg2vreg.get(reg); }

    //Get Versioned-Reg object by giving Reg and Reg version.
    VReg * getVReg(Reg reg, UINT version) const;
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr;  }
    RegSSAMgr * getRegSSAMgr() const { return m_regssa_mgr; }
    LinearScanRA const* getRA() const;
    TargInfoMgr * getTIMgr() const { return m_timgr; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    IRMgrExt * getIRMgrExt() const { return (IRMgrExt*)m_irmgr; }
    Reg getReg(IR const* exp) const;

    bool isRefReg(IR const* ir, Reg reg) const;

    //Reinitialize current object rather than destruction.
    void reinit() { cleanOrDestroy(true); }

    //The function remove and clean all information of 'vreg' from RegSSAMgr.
    void removeVReg(VReg * vreg);

    //The function remove and clean all information of a DEF from RegSSAMgr.
    void removeRegDef(RegDef * def);

    //Set 'regssainfo' to ir.
    void setRegSSAInfo(IR * ir, RegSSAInfo * regssainfo);
};

} //namespace xoc
#endif
