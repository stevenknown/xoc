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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START REGDID
//
void REGDIdx2RegD::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        RegD const* rd = Vector<RegD*>::get(i);
        if (rd == nullptr) { continue; }
        ASSERT0(REGD_id(rd) == (REGDIdx)i);
        prt(rg, "%u,", i);
    }
}
//END REGDID


//Return true if current rd exactly cover 'm', such as:
//CASE1:
//  current rd: |-------|
//  m:            |----|
//CASE2:
//  current rd: |---------|
//  m(range):     |--..--|
bool RegD::is_exact_cover(RegD const* m) const
{
    ASSERT0(m);
    //ASSERT0(this != m);
    if (get_base() != m->get_base()) {
        return false;
    }
    return ((REGD_ofst(this) <= REGD_ofst(m)) &&
           (REGD_ofst(this) + REGD_size(this) >= REGD_ofst(m) + REGD_size(m)));
}


//Return true if current rd intersect but may be not cover 'm', such as:
//current rd: |-------|
//m:            |-------|
bool RegD::is_overlap(RegD const* m) const
{
    ASSERT0(m && this != m);
    //NOTE: If one is going to compare m with Delegate, the best choice is to
    //use RegDSet::is_contain() rather than comparing RegD one by one.
    //
    //TBD: Does it necessary to judge if either current
    //RegD or input RegD is FULL_MEM?
    //As we observed, passes that utilize RegD relationship add
    //REGD2 to according IR's RegDSet, which can keep global variables
    //and REGD2 dependence.
    //e.g: g=10, #mustdef=REGD10, maydef={REGD2, REGD10}, g is global
    //           #variable that represented in Program Region.
    //     foo(); #maydef={REGD2, REGD10}
    //if (REGD_id(m) == REGD_FULL_MEM || REGD_id(this) == REGD_FULL_MEM) {
    //    return true;
    //}

    //It is also unnecessary to judge HEAP_REGD relationship if RegD
    //is FULL_MEM.
    //if (REGD_id(m) == REGD_HEAP_MEM && REGD_id(this) == REGD_FULL_MEM)
    //{ return true; }

    if (REGD_base(m) != REGD_base(this)) { return false; }
    return !(((REGD_ofst(m) + REGD_size(m)) <= REGD_ofst(this)) ||
             ((REGD_ofst(this) + REGD_size(this)) <= REGD_ofst(m)));
}


CHAR * RegD::dump(StrBuf & buf, RegDSystem const* sys) const
{
    TargInfoMgr const* tim = sys->getTIMgr();
    REGFILE rf = tim->getRegFile(REGD_reg(this));
    ASSERT0(rf != REG_UNDEF);
    buf.strcat("RegD%u:%s(%s),base:%s,",
               REGD_id(this), tim->getRegName(REGD_reg(this)),
               tim->getRegFileName(rf),
               tim->getRegFileName(REGD_base(this)));
    CHAR const* ofstfmt = getHostUIntFormat(false);
    TMWORD lofst = REGD_ofst(this);
    StrBuf fmt(16);
    fmt.strcat("ofst:%s,size:%s", ofstfmt, ofstfmt);
    buf.strcat(fmt.buf, (HOST_UINT)lofst, (HOST_UINT)REGD_size(this));
    return buf.buf;
}


void RegD::dump(RegDSystem const* sys) const
{
    RegionMgr const* rm = sys->getRegionMgr();
    ASSERT0(rm);
    if (!rm->isLogMgrInit()) { return; }
    StrBuf buf(64);
    note(rm, "\n%s", dump(buf, sys));
}
//END RegD


//
//START RegDSet
//
void RegDSet::bunion(REGDIdx rd, DefMiscBitSetMgr & rdsmgr)
{
    //TBD: Does it necessary to judge if either current
    //RegD or input RegD is FULL_MEM?
    //As we observed, passes that utilize RegD relationship add
    //REGD2 to according IR's RegDSet, which can keep global variables
    //and REGD2 dependence.
    //e.g: g=10, #mustdef=REGD10, maydef={REGD2, REGD10}, g is global
    //           #variable that represented in Program Region.
    //     foo(); #maydef={REGD2, REGD10}
    //if (rd == REGD_FULL_MEM) {
    //    clean(rdsmgr);
    //    DefSBitSetCore::bunion(REGD_FULL_MEM, rdsmgr);
    //    return;
    //}
    //if (DefSBitSetCore::is_contain(REGD_FULL_MEM)) {
    //    ASSERT0(DefSBitSetCore::get_elem_count() == 1);
    //    return;
    //}

    DefSBitSetCore::bunion(rd, rdsmgr);
}


//Return true current set is equivalent to rds, and every element
//in set is exact.
bool RegDSet::is_exact_equal(RegDSet const& rds, RegDSystem const* sys) const
{
    ASSERT0(sys);
    UINT count = 0;
    BSIdx rd1 = BS_UNDEF;
    RegDSetIter iter;
    for (BSIdx i = get_first(&iter); i != BS_UNDEF; i = get_next(i, &iter)) {
        rd1 = i;
        count++;
        if (count > 1) {
            //If the number of RegD more than one, the Alias analysis
            //might compute the consevative solution.
            return false;
        }
    }
    count = 0;
    BSIdx rd2 = BS_UNDEF;
    for (BSIdx i = rds.get_first(&iter);
         i != BS_UNDEF; i = get_next(i, &iter)) {
        rd2 = i;
        count++;
        if (count > 1) {
            //If the number of RegD more than one,
            //the Alias analysis might compute the consevative solution.
            return false;
        }
    }
    return rd1 == rd2;
}


bool RegDSet::is_contain(RegD const* rd) const
{
    return DefSBitSetCore::is_contain(REGD_id(rd));
}


//Return true if rd is overlap with the elements in set.
bool RegDSet::is_overlap(RegD const* rd) const
{
    return is_contain(rd);
}


//Return true if 'rd' overlapped with element in current RegDSet.
//Note this function will iterate elements in current RegDSet which is costly.
//Use it carefully.
bool RegDSet::is_overlap_ex(RegD const* rd, RegDSystem const* sys) const
{
    ASSERT0(rd && sys);
    if (RegDSet::is_overlap(rd)) { return true; }
    RegDSetIter iter = nullptr;
    for (BSIdx i = get_first(&iter);
         i != BS_UNDEF; i = get_next(i, &iter)) {
        RegD const* t = const_cast<RegDSystem*>(sys)->getRegDByIdx((REGDIdx)i);
        ASSERT0(t);
        if (t->is_overlap(rd)) { return true; }
    }
    return false;
}


void RegDSet::bunion(RegDSet const& rds, DefMiscBitSetMgr & rdsmgr)
{
    if (this == &rds) { return; }

    ASSERT0(!((DefSBitSetCore&)rds).is_contain(0));

    //TBD: Does it necessary to judge if either current
    //RegD or input RegD is FULL_MEM?
    //As we observed, passes that utilize RegD relationship add
    //REGD2 to according IR's RegDSet, which can keep global variables
    //and REGD2 dependence.
    //e.g: g=10, #mustdef=REGD10, maydef={REGD2, REGD10}, g is global
    //           #variable that represented in Program Region.
    //     foo(); #maydef={REGD2, REGD10}
    //if (DefSBitSetCore::is_contain(REGD_FULL_MEM)) {
    //    return;
    //}
    //if (((DefSBitSetCore const&)rds).is_contain(REGD_FULL_MEM)) {
    //    clean(rdsmgr);
    //    DefSBitSetCore::bunion(REGD_FULL_MEM, rdsmgr);
    //    return;
    //}

    DefSBitSetCore::bunion((DefSBitSetCore&)rds, rdsmgr);
}


//This function will walk through whole current RegDSet and differenciate
//overlapped elements.
//Note this function is very costly.
void RegDSet::diffAllOverlapped(
    REGDIdx id, DefMiscBitSetMgr & m, RegDSystem const* sys)
{
    RegDSetIter iter;
    RegD const* srcrd = const_cast<RegDSystem*>(sys)->getRegDByIdx(id);
    BSIdx next_i;
    for (BSIdx i = get_first(&iter); i != BS_UNDEF; i = next_i) {
        next_i = get_next(i, &iter);
        RegD const* tgtrd = const_cast<RegDSystem*>(sys)->getRegDByIdx(i);
        ASSERT0(tgtrd);
        if (srcrd->is_exact_cover(tgtrd)) {
            diff(i, m);
        }
    }
}


void RegDSet::dump(RegDSystem const* sys, bool detail) const
{
    ASSERT0(sys);
    if (!sys->getRegionMgr()->isLogMgrInit()) { return; }
    RegDSetIter iter;
    for (BSIdx i = get_first(&iter); i != BS_UNDEF;) {
        prt(sys->getRegionMgr(), "RegD%u", i);
        i = get_next(i, &iter);
        if (i != BS_UNDEF) {
            prt(sys->getRegionMgr(), ",");
        }
    }
    if (!detail) { return; }
    for (BSIdx i = get_first(&iter);
         i != BS_UNDEF; i = get_next(i, &iter)) {
        RegD const* rd = sys->getRegDByIdx((REGDIdx)i);
        ASSERT0(rd);
        rd->dump(sys);
    }
}


RegD const* RegDSet::get_unique_rd(RegDSystem const* sys) const
{
    RegDSetIter it = nullptr;
    BSIdx first = get_first(&it);
    if (first == BS_UNDEF) { return nullptr; }
    BSIdx second = get_next(first, &it);
    return second != BS_UNDEF ? nullptr : sys->getRegDByIdx((REGDIdx)first);
}
//END RegDSet


//
//START RegDSystem
//
RegDSystem::RegDSystem(RegionMgr const* rm, TargInfoMgr const* tim)
    : m_rds_hash_allocator(&m_sbsmgr), m_rds_hash(&m_rds_hash_allocator),
      m_rs_hash_allocator(&m_sbsmgr), m_rs_hash(&m_rs_hash_allocator)
{
    init(rm, tim);
}


//Register RegD and generating unique id for it, with the followed method:
//1. Generating RegD hash table for any unique Var.
//2. Entering 'rd' into RegD hash table, the hash-value comes
//    from an evaluating binary-Tree that the branch of
//    tree-node indicate determination data related with RegD fields.
//Return the registered element.
//
//NOTE:
//1. DO NOT free the registered element!
//2. If you want to register an new RegD, keep the id is 0.
RegD const* RegDSystem::registerRegD(RegD const& m)
{
    ASSERT0(REGD_base(&m));
    if (REGD_id(&m) > REGD_UNDEF) {
        //Find the entry in RegDTab according to m.
        RegDTab * rdtab = getRegDTab(REGD_base(&m));
        ASSERTN(rdtab != nullptr, ("rd has not been registered"));
        RegD const* entry = rdtab->find(&m);
        ASSERTN(entry, ("rd has not been registered"));
        return entry;
    }

    ASSERT0(REGD_base(&m) != RF_UNDEF);

    //Check if RegD has been registerd.
    RegDTab * rdtab = getRegDTab(REGD_base(&m));
    if (rdtab != nullptr) {
        //Var-base has been registered, then check rd by
        //offset in rd-table.
        RegD const* hash_entry = rdtab->find(&m);
        if (hash_entry != nullptr) {
            //find RegD via REGD_ofst.
            return hash_entry;
        }

        //TBD: Does it necessary to judge if either current
        //RegD or input RegD is FULL_MEM?
        //As we observed, passes that utilize RegD relationship add
        //REGD2 to according IR's RegDSet, which can keep global variables
        //and REGD2 dependence.
        //e.g:
        //  #mustdef=REGD10, maydef={REGD2, REGD10}, g is global
        //  #variable that represented in Program Region.
        //  g=10,
        //
        //  #maydef={REGD2, REGD10}
        //  foo();
        //if (REGD_base(&m) == m_all_mem) {
        //    return getRegDByIdx(REGD_FULL_MEM);
        //}

        //TODO: remove HEAP, STACK id. I consider they are useless.
        //if (REGD_base(rd) == g_heap_mem) {
        //    REGD_id(rd) = REGD_HEAP_MEM;
        //    return ::getRegDByIdx(REGD_HEAP_MEM);
        //}
        //if (REGD_base(rd) == g_stack_mem) {
        //    REGD_id(rd) = REGD_LOCAL_VAR;
        //    return ::getRegDByIdx(REGD_LOCAL_VAR);
        //}
    }

    //Generate a new RegD and record it in rd-table according to its id.
    RegD * entry = allocRegD();
    if (REGD_id(entry) == REGD_UNDEF) {
        REGD_id(entry) = m_rd_count++;
    }
    entry->copy(&m);
    if (rdtab == nullptr) {
        rdtab = allocRegDTab();
        m_rf2rdtab.set(REGD_base(entry), rdtab);
    }

    //Insert entry into RegDTab of Var.
    rdtab->append(entry);
    m_id2regd.set(REGD_id(entry), entry);
    return entry;
}


void RegDSystem::init(RegionMgr const* rm, TargInfoMgr const* tim)
{
    ASSERT0(rm && tim);
    m_pool = smpoolCreate(sizeof(RegD) * 5, MEM_CONST_SIZE);
    m_sc_rdptr_pool = smpoolCreate(
        sizeof(xcom::SC<RegD*>) * 10, MEM_CONST_SIZE);
    m_free_rd_list.set_pool(m_sc_rdptr_pool);
    m_rd_count = REGD_UNDEF + 1;
    m_rm = rm;
    m_timgr = tim;
}


void RegDSystem::destroy()
{
    REGFILE2RegDTabIter iter;
    RegDTab * rdtab;
    for (m_rf2rdtab.get_first(iter, &rdtab);
         rdtab != nullptr; m_rf2rdtab.get_next(iter, &rdtab)) {
        delete rdtab;
    }
    smpoolDelete(m_pool);
    smpoolDelete(m_sc_rdptr_pool);
}


void RegDSystem::computeBaseOfRegD(Reg r, MOD RegD & rd)
{
    REGFILE rf = getTIMgr()->getRegFile(r);
    ASSERT0(rf);
    REGD_base(&rd) = rf;
}


TMWORD RegDSystem::computeOffsetOfRegDAndRF(
    Reg r, TMWORD current_bofst, MOD RegD & rd)
{
    REGD_ofst(&rd) = current_bofst;
    ASSERT0(REGD_size(&rd) > 0);
    return current_bofst + REGD_size(&rd);
}


void RegDSystem::initRegDForAllReg()
{
    TargInfoMgr const* tim = getTIMgr();
    ASSERT0(m_reg2regd.get_elem_count() == 0);
    Vector<TMWORD> rf2bitoffset;
    for (Reg r = REG_UNDEF + 1; r < REG_NUM; r++) {
        RegD rd;
        REGFILE rf = tim->getRegFile(r);
        ASSERT0(rf);
        TMWORD cur_ofst = rf2bitoffset.get(rf);
        computeBaseOfRegD(r, rd);
        REGD_reg(&rd) = r;
        REGD_size(&rd) = tim->getBitSize(r);
        TMWORD new_ofst = computeOffsetOfRegDAndRF(r, cur_ofst, rd);
        rf2bitoffset.set(rf, new_ofst);
        RegD const* registed = registerRegD(rd);
        m_reg2regd.set(r, registed);
    }
}


static void extractRegSet(
    RegDSystem const* sys, MOD SRegSetHash & rshash, RegDSet const& rds,
    OUT SRegSet & rs)
{
    RegDSetIter rdsit;
    for (BSIdx i = rds.get_first(&rdsit);
         i != BS_UNDEF; i = rds.get_next(i, &rdsit)) {
        RegD const* rd = sys->getRegDByIdx((REGDIdx)i);
        ASSERT0(rd && rd->getReg() != REG_UNDEF);
        rs.bunion(rd->getReg(), *rshash.getBsMgr());
    }
}


void RegDSystem::initOverlapSetForAllReg()
{
    ASSERT0(m_reg2overlap_rds.get_elem_count() == 0);
    RegDSet rds;
    SRegSet rs;
    ConstRegDIter rdsit;
    DefMiscBitSetMgr rdsmgr;
    for (Reg r = REG_UNDEF + 1; r < REG_NUM; r++) {
        rds.clean(rdsmgr);
        rs.clean(*m_rs_hash.getBsMgr());
        RegD const* rd = getRegDByReg(r);
        ASSERT0(rd);
        computeOverlap(rd, rds, rdsit, rdsmgr);
        RegDSet const* hashed_rds = m_rds_hash.append(rds);
        m_reg2overlap_rds.set(r, hashed_rds);
        extractRegSet(this, m_rs_hash, rds, rs);
        SRegSet const* hashed_rs = m_rs_hash.append(rs);
        m_reg2overlap_rs.set(r, hashed_rs);
    }
    rds.clean(rdsmgr);
    rs.clean(rdsmgr);
}


void RegDSystem::initRegDByTargInfo()
{
    initRegDForAllReg();
    initOverlapSetForAllReg();
}


//Compute all other RegD which are overlapped with 'rd', the output
//will include 'rd' itself if there are overlapped REGDs.
//e.g: given rd1, and rd1 overlapped with rd2, rd3,
//then output set is {rd1, rd2, rd3}.
//rd: input to compute the overlapped rd-set.
//tabiter: for local use.
//strictly: set to true to compute if rd may be overlapped
//            with global variables or import variables.
//Note this function does NOT clean output, and will append result to output.
void RegDSystem::computeOverlap(
    RegD const* rd, OUT RegDSet & output, ConstRegDIter & tabiter,
    DefMiscBitSetMgr & rdsmgr)
{
    ASSERT0(rd);
    RegDTab * rdt = getRegDTab(REGD_base(rd));
    ASSERT0(rdt != nullptr);
    RDOffsetTab * ofsttab = rdt->get_ofst_tab();
    ASSERT0(ofsttab);
    if (ofsttab->get_elem_count() == 0) { return; }

    tabiter.clean();
    bool find_overlapped = false;
    for (RegD const* trd = ofsttab->get_first(tabiter, nullptr);
         trd != nullptr; trd = ofsttab->get_next(tabiter, nullptr)) {
        ASSERT0(REGD_base(rd) == REGD_base(trd));
        if (trd == rd) { continue; }
        if (rd->is_overlap(trd)) {
            output.bunion(trd, rdsmgr);
            find_overlapped = true;
        }
    }
    if (!find_overlapped) { return; }

    //Add rd itself into the output set because the function compute the
    //overlap-set according to 'rd', thus record the causality between
    //'rd' and its overlap-set.
    output.bunion(rd, rdsmgr);
}


//Compute all other RegD which are overlapped with RegD in set 'rds'.
//e.g: rds contains {rd1}, and rd1 overlapped with rd2, rd3,
//then output set 'rds' is {rd1, rd2, rd3}.
//rds: it is not only input but also output buffer.
//added: records the new RegD that added into 'rds'.
//rditer: for local use.
void RegDSystem::computeOverlap(
    MOD RegDSet & rds, MOD Vector<RegD const*> & added,
    ConstRegDIter & rditer, DefMiscBitSetMgr & rdsmgr)
{
    UINT count = 0;
    added.clean();
    RegDSetIter iter;
    for (BSIdx i = rds.get_first(&iter);
         i != BS_UNDEF; i = rds.get_next(i, &iter)) {
        RegD const* rd = getRegDByIdx((REGDIdx)i);
        ASSERT0(rd);
        RegDTab * rdt = getRegDTab(REGD_base(rd));
        ASSERT0(rdt != nullptr);
        RDOffsetTab * ofsttab = rdt->get_ofst_tab();
        ASSERT0(ofsttab);
        rditer.clean();
        for (RegD const* trd = ofsttab->get_first(rditer, nullptr);
             trd != nullptr; trd = ofsttab->get_next(rditer, nullptr)) {
            if (((DefSBitSetCore&)rds).is_contain(REGD_id(trd))) {
                continue;
            }
            ASSERT0(REGD_base(rd) == REGD_base(trd));
            if (rd->is_overlap(trd)) {
                added.set(count, trd);
                count++;
            }
        }
    }
    for (VecIdx i = 0; i <= added.get_last_idx(); i++) {
        RegD const* t = added.get(i);
        ASSERT0(t);
        rds.bunion(t, rdsmgr);
    }
}


//Compute all other RegD which are overlapped with RegD in set 'rds'.
//e.g: rds contains {rd1}, and rd1 overlapped with rd2, rd3,
//then output is {rd2, rd3}.
//rds: it is readonly input.
//output: output RegD set.
//rditer: for local use.
//Note 'output' do not need to clean before invoke this function.
void RegDSystem::computeOverlap(
    RegDSet const& rds, OUT RegDSet & output, ConstRegDIter & rditer,
    DefMiscBitSetMgr & rdsmgr)
{
    ASSERT0(&rds != &output);
    RegDSetIter iter;
    for (BSIdx i = rds.get_first(&iter);
         i != BS_UNDEF; i = rds.get_next(i, &iter)) {
        RegD const* rd = getRegDByIdx((REGDIdx)i);
        ASSERT0(rd);
        RegDTab * rdt = getRegDTab(REGD_base(rd));
        ASSERT0(rdt != nullptr);
        RDOffsetTab const* ofsttab = rdt->get_ofst_tab();
        ASSERT0(ofsttab);
        rditer.clean();
        for (RegD const* trd = ofsttab->get_first(rditer, nullptr);
             trd != nullptr; trd = ofsttab->get_next(rditer, nullptr)) {
            if (rds.is_contain_pure(REGD_id(trd))) {
                continue;
            }
            ASSERT0(REGD_base(rd) == REGD_base(trd));
            if (rd->is_overlap(trd)) {
                output.bunion_pure(REGD_id(trd), rdsmgr);
            }
        }
    }
}


void RegDSystem::clean()
{
    REGFILE2RegDTabIter iter;
    RegDTab * rdtab;
    for (REGFILE rf = m_rf2rdtab.get_first(iter, &rdtab);
         rf != RF_UNDEF; rf = m_rf2rdtab.get_next(iter, &rdtab)) {
        rdtab->clean();
    }
    for (VecIdx i = 0; i <= m_id2regd.get_last_idx(); i++) {
        RegD * rd = m_id2regd.get(i);
        if (rd == nullptr) { continue; }
        freeRegD(rd);
    }
    m_reg2regd.clean();
    m_reg2overlap_rds.clean();
    m_rds_hash.clean();
    m_rs_hash.clean();
    m_rd_count = REGD_UNDEF + 1;
}


static void dumpAllRegD(RegDSystem const* sys)
{
    RegionMgr const* rm = sys->getRegionMgr();
    if (!rm->isLogMgrInit()) { return; }
    note(rm, "\n---- DUMP ALL RegD ----");
    for (UINT i = REGD_UNDEF + 1; i < sys->getNumOfRegD(); i++) {
        RegD const* rd = sys->getRegDByIdx((REGDIdx)i);
        if (rd == nullptr) { continue; }
        ASSERT0(rd->id() == (REGDIdx)i);
        rd->dump(sys);
    }
}


static void dumpSRegSet2RegD(SRegSet const* rs, RegDSystem const* sys)
{
    SRegSetIter it;
    for (BSIdx i = rs->get_first(&it);
         i != BS_UNDEF; i = rs->get_next(i, &it)) {
        Reg r = (Reg)i;
        RegD const* rd = sys->getRegDByReg(r);
        ASSERT0(rd && rd->getReg() == r);
        rd->dump(sys);
    }
}


static void dumpAllRegD2OverlapSet(RegDSystem const* sys)
{
    RegionMgr const* rm = sys->getRegionMgr();
    if (!rm->isLogMgrInit()) { return; }
    note(rm, "\n---- DUMP Reg2OverlapSet ----");
    Reg2SRegSet const& reg2rset = sys->getReg2SRegSet();
    FILE * h = rm->getLogMgr()->getFileHandler();
    for (UINT i = 0; i < reg2rset.get_elem_count(); i++) {
        SRegSet const* overlap = reg2rset.get((Reg)i);
        if (overlap == nullptr) { continue; }
        RegD const* rd = sys->getRegDByReg((Reg)i);
        ASSERT0(rd);
        ASSERT0(rd->getReg() == (Reg)i);
        rd->dump(sys);
        prt(rm, " OVERLAPSET: ");
        overlap->dump(h);
        rm->getLogMgr()->incIndent(2);
        dumpSRegSet2RegD(overlap, sys);
        rm->getLogMgr()->decIndent(2);
    }
}


void RegDSystem::dump() const
{
    if (!getRegionMgr()->isLogMgrInit()) { return; }
    note(getRegionMgr(), "\n==---- DUMP RegDSystem ----==");
    dumpAllRegD(this);
    dumpAllRegD2OverlapSet(this);
}


void RegDSystem::removeRegDForRegFile(REGFILE rf, ConstRegDIter & iter)
{
    ASSERT0(rf != RF_UNDEF);
    RegDTab * rdtab = getRegDTab(rf);
    if (rdtab != nullptr) {
        RDOffsetTab * ofstab = rdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (RegD const* rd = ofstab->get_first(iter, nullptr);
                 rd != nullptr; rd = ofstab->get_next(iter, nullptr)) {
                RegD const* freeone = getRegDByIdx(rd->id());
                freeRegD(const_cast<RegD*>(freeone));
            }
        }
        delete rdtab;
    }
    m_rf2rdtab.remove(rf);
}
//END RegDSystem

} //namespace xoc
