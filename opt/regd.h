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
#ifndef _REGD_H_
#define _REGD_H_

using namespace xgen;

namespace xoc {

class RegDSystem;
class Region;
class RegDSet;
class SRegSet;

#define REGD_UNDEF 0 //Undefined RegD index

typedef UINT REGDIdx;

//Unique id of register descriptor object.
#define REGD_id(rd) ((rd)->m_uid)

//Each Reg has a base, it is corresponding to an unique RegFile.
#define REGD_base(rd) ((rd)->m_base)

//Record the offset from base if Reg is exact.
#define REGD_ofst(rd) ((rd)->m_ofst)

//Record the byte size of memory object if Reg is exact or range,
//or it is not available.
#define REGD_size(rd) ((rd)->m_size)
#define REGD_reg(rd) ((rd)->m_reg)

//The class represents the register descriptor.
class RegD {
public:
    REGDIdx m_uid; //unique id.
    TMWORD m_ofst; //bit offsets relative to 'base'
    TMWORD m_size; //bit size of the register
    Reg m_reg; //record the register.

    //Record the base register-file. Note the base is just used to differetiate
    //the registers group. Thus it may not equal to the REGFILE that queried
    //from TargInfoMgr.
    //e.g: For ARM, S0 belong to regfile RF_S and D0 belongs to regfile RF_D,
    //however their base are both RF_F, the float-pointer-regfile.
    REGFILE m_base;
public:
    RegD() { clean(); }
    RegD(RegD const& rd)
    {
        //Do not copy id.
        REGD_id(this) = 0;
        copy(&rd);
    }

    void copy(RegD const* rd)
    {
        ASSERT0(rd && this != rd);
        REGD_base(this) = REGD_base(rd);
        REGD_ofst(this) = REGD_ofst(rd);
        REGD_size(this) = REGD_size(rd);
        REGD_reg(this) = REGD_reg(rd);
    }
    void clean()
    {
        REGD_id(this) = 0;
        REGD_ofst(this) = 0;
        REGD_size(this) = 0;
        REGD_base(this) = RF_UNDEF;
        REGD_reg(this) = REG_UNDEF;
    }

    REGFILE get_base() const { return REGD_base(this); }
    TMWORD getBitOfst() const { return REGD_ofst(this); }
    TMWORD getBitSize() const { return REGD_size(this); }
    Reg getReg() const { return REGD_reg(this); }

    REGDIdx id() const { return REGD_id(this); }

    //Return true if current rd exactly cover 'm', such as:
    //CASE1:
    //  current rd: |-------|
    //  m:            |----|
    //CASE2:
    //  current rd: |----------|
    //  m:            |--...--|
    bool is_exact_cover(RegD const* m) const;

    //Return true if current rd intersect but may be not cover 'm', such as:
    //current rd: |-------|
    //m:            |-------|
    bool is_overlap(RegD const* m) const;

    //Return true if src is definitly equal to current rd.
    bool is_equ(RegD const& src) const
    {
        ASSERT0(this != &src);
        return *this == src;
    }

    RegD const& operator = (RegD const&);
    bool operator == (RegD const& src) const
    {
        ASSERT0(this != &src);
        return REGD_base(this) == REGD_base(&src) &&
               REGD_ofst(this) == REGD_ofst(&src) &&
               REGD_size(this) == REGD_size(&src) &&
               REGD_reg(this) == REGD_reg(&src);
    }

    //Dump rd into 'buf'.
    CHAR * dump(StrBuf & buf, RegDSystem const* sys) const;

    //Dump rd to file.
    void dump(RegDSystem const* rg) const;
};


typedef TMapIter<RegD*, RegD*> RegDIter;
typedef TMapIter<RegD const*, RegD const*> ConstRegDIter;

class RDCompareOffset {
public:
    RegD const* createKey(RegD const* t) { return t; }
    bool is_less(RegD const* t1, RegD const*  t2) const
    {
        ASSERT0(REGD_base(t1) == REGD_base(t2));
        return ((((ULONGLONG)REGD_ofst(t1)) << 1) |
                (((ULONGLONG)REGD_size(t1)) << 32)) <
               ((((ULONGLONG)REGD_ofst(t2)) << 1) |
                (((ULONGLONG)REGD_size(t2)) << 32));
    }
    bool is_equ(RegD const* t1, RegD const* t2) const
    {
        ASSERT0(REGD_base(t1) == REGD_base(t2));
        return ((((ULONGLONG)REGD_ofst(t1)) << 1) |
                (((ULONGLONG)REGD_size(t1)) << 32)) ==
               ((((ULONGLONG)REGD_ofst(t2)) << 1) |
                (((ULONGLONG)REGD_size(t2)) << 32));
    }
};


class RegDSetHashAllocator {
    MiscBitSetMgr<> * m_sbs_mgr;
public:
    RegDSetHashAllocator(MiscBitSetMgr<> * sbsmgr)
    { ASSERT0(sbsmgr); m_sbs_mgr = sbsmgr; }

    SBitSetCore<> * alloc() { return m_sbs_mgr->allocSBitSetCore(); }
    void free(SBitSetCore<> * set) { m_sbs_mgr->freeSBitSetCore(set); }
    MiscBitSetMgr<> * getBsMgr() const { return m_sbs_mgr; }
};


class RegDSetHash : public SBitSetCoreHash<RegDSetHashAllocator> {
public:
    RegDSetHash(RegDSetHashAllocator * allocator) :
        SBitSetCoreHash<RegDSetHashAllocator>(allocator) {}
    virtual ~RegDSetHash() {}

    RegDSet const* append(SBitSetCore<> const& set)
    {
        return (RegDSet const*)SBitSetCoreHash<RegDSetHashAllocator>::
            append(set);
    }
    void dump(Region * rg);
};


class SRegSetHashAllocator {
    MiscBitSetMgr<> * m_sbs_mgr;
public:
    SRegSetHashAllocator(MiscBitSetMgr<> * sbsmgr)
    { ASSERT0(sbsmgr); m_sbs_mgr = sbsmgr; }

    SBitSetCore<> * alloc() { return m_sbs_mgr->allocSBitSetCore(); }
    void free(SBitSetCore<> * set) { m_sbs_mgr->freeSBitSetCore(set); }
    MiscBitSetMgr<> * getBsMgr() const { return m_sbs_mgr; }
};


class SRegSetHash : public SBitSetCoreHash<SRegSetHashAllocator> {
public:
    SRegSetHash(SRegSetHashAllocator * allocator) :
        SBitSetCoreHash<SRegSetHashAllocator>(allocator) {}
    virtual ~SRegSetHash() {}

    SRegSet const* append(SBitSetCore<> const& set)
    {
        return (SRegSet const*)SBitSetCoreHash<SRegSetHashAllocator>::
            append(set);
    }
    void dump(Region * rg);
    MiscBitSetMgr<> * getBsMgr() const { return get_allocator()->getBsMgr(); }
};


//RegD hashed by REGD_ofst.
class RDOffsetTab : public TMap<RegD const*, RegD const*, RDCompareOffset> {
public:
    //Return the entry.
    RegD const* find(RegD const* rd)
    {
        return TMap<RegD const*, RegD const*, RDCompareOffset>::
            get(rd, nullptr);
    }
    void append(RegD const* rd)
    { TMap<RegD const*, RegD const*, RDCompareOffset>::set(rd, rd); }
};


//Each Var corresponds to an unqiue RegDTab.
class RegDTab {
    COPY_CONSTRUCTOR(RegDTab);
protected:
    RDOffsetTab m_ofst_tab;
public:
    RegDTab() {}

    void append(RegD const* rd) { m_ofst_tab.append(rd); }

    void clean() { m_ofst_tab.clean(); }
    //Count memory usage for current object.
    size_t count_mem() const { return m_ofst_tab.count_mem(); }

    RegD const* find(RegD const* rd) { return m_ofst_tab.find(rd); }

    RDOffsetTab * get_ofst_tab() {  return &m_ofst_tab; }
    UINT get_elem_count()
    {
        UINT elems = m_ofst_tab.get_elem_count();
        return elems;
    }
    void get_elems(OUT Vector<RegD const*> & rdv, ConstRegDIter & iter)
    {
        REGDIdx idx = REGD_UNDEF;
        for (RegD const* rd = m_ofst_tab.get_first(iter, nullptr);
             rd != nullptr; rd = m_ofst_tab.get_next(iter, nullptr)) {
            rdv.set(idx++, rd);
        }
    }

    void init(UINT hash_bucket_size);
};


//The class represents the Sparse Register Set.
typedef xcom::DefSEGIter * SRegSetIter;

class SRegSet : public DefSBitSetCore {
    COPY_CONSTRUCTOR(SRegSet);
public:
    SRegSet() {}
    ~SRegSet() {} //should call clean() before destruction.
};


//The class represents the overlapped Reg Description Set.
//Note: one must call clean() to reclamition before deletion or destruction.
typedef xcom::DefSEGIter * RegDSetIter;

class RegDSet : public DefSBitSetCore {
    COPY_CONSTRUCTOR(RegDSet);
public:
    RegDSet() {}
    ~RegDSet() {} //should call clean() before destruction.

    void bunion(RegDSet const& pt, DefMiscBitSetMgr & mbsmgr);
    void bunion(RegD const* rd, DefMiscBitSetMgr & mbsmgr)
    { bunion(REGD_id(rd), mbsmgr); }
    void bunion(REGDIdx rd, DefMiscBitSetMgr & mbsmgr);
    void bunion_pure(REGDIdx rd, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(rd, m); }
    void bunion_pure(RegDSet const& rds, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(rds, m); }

    void diff(RegD const* rd, DefMiscBitSetMgr & m)
    {
        ASSERT0(rd);
        DefSBitSetCore::diff(REGD_id(rd), m);
    }
    void diff(REGDIdx id, DefMiscBitSetMgr & m) { DefSBitSetCore::diff(id, m); }
    void diff(RegDSet const& rds, DefMiscBitSetMgr & m)
    {
        ASSERT0(this != &rds);

        //TBD: Does it necessary to judge if either current
        //RegD or input RegD is FULL_MEM?
        //As we observed, passes that utilize RegD relationship add
        //REGD2 to according IR's RegDSet, which can keep global variables
        //and REGD2 dependence.
        //e.g:
        //  #mustdef=REGD10, maydef={REGD2, REGD10}, g is global variable that
        //  #represented in Program Region.
        //  g=10;
        //
        //  #maydef={REGD2, REGD10}
        //  foo();
        //if (((DefSBitSetCore const&)rds).is_contain(REGD_FULL_MEM)) {
        //    clean(m);
        //    return;
        //}
        DefSBitSetCore::diff(rds, m);
    }
    //This function will walk through whole current RegDSet and differenciate
    //overlapped elements.
    //Note this function is very costly.
    void diffAllOverlapped(
        REGDIdx id, DefMiscBitSetMgr & m, RegDSystem const* sys);
    void dump(RegDSystem const* sys, bool detail) const;

    bool is_contain_pure(REGDIdx rd) const
    { return DefSBitSetCore::is_contain(rd); }
    bool is_contain_pure(RegDSet const& rds) const
    { return DefSBitSetCore::is_contain(rds); }

    //Return true if set contained rd.
    bool is_contain(RegD const* rd) const;

    //Return true if rd is overlap with the elements in set.
    bool is_overlap(RegD const* rd) const;

    //Return true if rd overlaps with element in current RegDSet.
    //Note this function will iterate all elements which is costly.
    //Use it carefully.
    bool is_overlap_ex(RegD const* rd, RegDSystem const* sys) const;

    //Return true current set is equivalent to rds, whereas every element
    //in set is exact.
    bool is_exact_equal(RegDSet const& rds, RegDSystem const* sys) const;

    //Return true if set intersect with 'rds'.
    bool is_intersect(RegDSet const& rds) const
    {
        ASSERT0(this != &rds);
        return DefSBitSetCore::is_intersect(rds);
    }

    //Return the unique RegD if current set has only one element.
    RegD const* get_unique_rd(RegDSystem const* sys) const;
};


typedef xcom::Vector<RegDSet const*> Reg2RegDSet;
typedef xcom::Vector<SRegSet const*> Reg2SRegSet;
typedef xcom::Vector<RegD const*> Reg2RegD; //Map Reg to the unique RegD.

class REGDIdx2RegD : public xcom::Vector<RegD*> {
    COPY_CONSTRUCTOR(REGDIdx2RegD);
    UINT m_count;
public:
    REGDIdx2RegD() { m_count = 0; }

    void remove(REGDIdx idx)
    {
        ASSERT0(idx != REGD_UNDEF); //can not be illegal rd.
        ASSERT0(get(idx) != nullptr);
        xcom::Vector<RegD*>::set(idx, nullptr);
        m_count--;
    }

    void set(REGDIdx idx, RegD * rd)
    {
        ASSERTN(xcom::Vector<RegD*>::get(idx) == nullptr, ("already mapped"));
        xcom::Vector<RegD*>::set(idx, rd);
        m_count++;
    }

    UINT get_elem_count() const { return m_count; }
    void dump(Region const* rg) const;
};


//RegD System.
//Manage the memory allocation and free of RegD, and
//the mapping between REGD_id and RegD.
//Manage the memory allocation and free of RegDTab, and
//the mapping between RegFile and RegDTab.
//NOTE: each Region Manager has a single RegDSystem.
class RegDSystem {
    COPY_CONSTRUCTOR(RegDSystem);
protected:
    typedef TMap<REGFILE, RegDTab*> REGFILE2RegDTab;
    typedef TMapIter<REGFILE, RegDTab*> REGFILE2RegDTabIter;
protected:
    UINT m_rd_count; //generate RegD index, used by registerRegD().
    SMemPool * m_pool;
    SMemPool * m_sc_rdptr_pool;
    RegionMgr const* m_rm;
    TargInfoMgr const* m_timgr;
    SList<RegD*> m_free_rd_list; //RegD allocated in pool.
    REGFILE2RegDTab m_rf2rdtab; //map RegFile to RegDTab.
    DefMiscBitSetMgr m_sbsmgr;
    RegDSetHashAllocator m_rds_hash_allocator;
    RegDSetHash m_rds_hash; //RegDSet hash table.
    SRegSetHashAllocator m_rs_hash_allocator;
    SRegSetHash m_rs_hash; //SRegSet hash table.
    REGDIdx2RegD m_id2regd; //Map RegD id to RegD.
    Reg2RegD m_reg2regd; //Map Reg to the unique RegD.

    //Map Reg to a RegDSet that record all overlapped RegD.
    Reg2RegDSet m_reg2overlap_rds;

    //Map Reg to a SRegSet that record all overlapped Registers.
    Reg2SRegSet m_reg2overlap_rs;
protected:
    RegD * allocRegD()
    {
        RegD * rd = m_free_rd_list.remove_head();
        if (rd == nullptr) {
            rd = (RegD*)smpoolMallocConstSize(sizeof(RegD), m_pool);
            rd->clean();
        }
        return rd;
    }

    //Allocated object should be recorded in list.
    RegDTab * allocRegDTab() { return new RegDTab(); }

    //The function initialize the base of RegD by TargInfoMgr.
    //Note some target machine partitions registers into different regfiles to
    //facilitate the linear-scan or instruction-scheduling to differentiate
    //the register groups. However, the partition may confuse the overlap
    //computation, because these physical-register actually belong to same
    //physical regfile, such as RF_S, RF_D, RF_Q are actually share the same
    //registers.
    virtual void computeBaseOfRegD(Reg r, MOD RegD & rd);

    //The function computes the bit-offset of register r in given regfile.
    //and returns the bit-offset of next register should be, which computed
    //by plusing bit-size that 'r' is actually occupied in given regfile to
    //current bit-offset.
    //current_bofst: current bit offset in given regfile.
    virtual TMWORD computeOffsetOfRegDAndRF(
        Reg r, TMWORD current_bofst, MOD RegD & rd);

    DefMiscBitSetMgr & getSBSMgr() { return m_sbsmgr; }

    void initRegDForAllReg();
    void initOverlapSetForAllReg();
public:
    RegDSystem(RegionMgr const* rm, TargInfoMgr const* tim);
    virtual ~RegDSystem() { destroy(); }

    void clean();

    //Compute all other RegD which are overlapped with 'rd', the output
    //will include 'rd' itself if there are overlapped REGDs.
    //e.g: given rd1, and rd1 overlapped with rd2, rd3,
    //then output set is {rd1, rd2, rd3}.
    //rd: input to compute the overlapped rd-set.
    //tabiter: for local use.
    //Note this function does NOT clean output, and will append result to
    //output.
    void computeOverlap(
        RegD const* rd, OUT RegDSet & output, ConstRegDIter & tabiter,
        DefMiscBitSetMgr & rdsmgr);

    //Compute all other RegD which are overlapped with RegD in set 'rds'.
    //e.g: rds contains {rd1}, and rd1 overlapped with rd2, rd3,
    //then output is {rd2, rd3}.
    //rds: it is readonly input.
    //output: output RegD set.
    //rditer: for local use.
    //Note output do not need to clean before invoke this function.
    void computeOverlap(
        RegDSet const& rds, OUT RegDSet & output, ConstRegDIter & rditer,
        DefMiscBitSetMgr & rdsmgr);

    //Compute all other RegD which are overlapped with RegD in set 'rds'.
    //e.g: rds contains {rd1}, and rd1 overlapped with rd2, rd3,
    //then output set 'rds' is {rd1, rd2, rd3}.
    //rds: it is not only input but also output buffer.
    //added: records the new RegD that added into 'rds'.
    //rditer: for local use.
    //memory.
    void computeOverlap(
        MOD RegDSet & rds, MOD xcom::Vector<RegD const*> & added,
        ConstRegDIter & rditer, DefMiscBitSetMgr & rbsmgr);

    //Dump all registered RegDs.
    void dump() const;
    void destroy();

    void freeRegD(RegD * rd)
    {
        if (rd == nullptr) { return; }
        m_id2regd.remove(REGD_id(rd));
        REGDIdx idx = REGD_id(rd);
        ::memset((void*)rd, 0, sizeof(RegD));
        REGD_id(rd) = idx;
        m_free_rd_list.append_head(rd);
    }

    //Get registered RegD through REGDIdx.
    //NOTICE: DO NOT free the return value, because it is the registered one.
    RegD const* getRegDByIdx(REGDIdx id) const
    {
        ASSERT0(id != REGD_UNDEF);
        RegD const* rd = m_id2regd.get(id);
        ASSERT0(rd == nullptr || REGD_id(rd) == id);
        return rd;
    }

    //Get registered RegD through Register.
    //NOTICE: DO NOT free the return value, because it is the registered one.
    RegD const* getRegDByReg(Reg reg) const
    {
        ASSERT0(reg != REG_UNDEF);
        RegD const* rd = m_reg2regd.get(reg);
        ASSERT0(rd == nullptr || rd->getReg() == reg);
        return rd;
    }

    //Get RegD TAB that described rds which under same base regfile.
    RegDTab * getRegDTab(REGFILE rf)
    {
        ASSERT0(rf != RF_UNDEF);
        return m_rf2rdtab.get(rf);
    }
    RegionMgr const* getRegionMgr() const { return m_rm; }
    TargInfoMgr const* getTIMgr() const { return m_timgr; }
    UINT getNumOfRegD() const { return m_id2regd.get_elem_count(); }
    REGDIdx2RegD const& getId2RegD() const { return m_id2regd; }

    //Get the overlapped Reg Description Set.
    Reg2RegDSet const& getReg2RegDSet() const { return m_reg2overlap_rds; }
    RegDSet const* getOverlapRegDSet(Reg reg) const
    { return m_reg2overlap_rds.get(reg); }

    //Get the overlapped Sparse RegSet.
    Reg2SRegSet const& getReg2SRegSet() const { return m_reg2overlap_rs; }
    SRegSet const* getOverlapSRegSet(Reg reg) const
    { return m_reg2overlap_rs.get(reg); }

    //The initialization of RegDSystem.
    void init(RegionMgr const* rm, TargInfoMgr const* tim);
    void initRegDByTargInfo();

    //Register RegD according to specific m. And return the generated RegD.
    RegD const* registerRegD(RegD const& m);

    //Remove all REGDs related to specific regfile 'rf'.
    void removeRegDForRegFile(REGFILE rf, IN ConstRegDIter & iter);
};

} //namespace xoc
#endif

