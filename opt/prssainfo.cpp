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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START SSAInfo
//
static void dumpDefUseRef(VPR const* vpr, Region const* rg)
{
    IR * def = vpr->getDef();
    if (vpr->version() != PRSSA_INIT_VERSION) {
        //After renaming, version is meaningless, thus it is only visible
        //to VPR.
        //For convenient purpose, tolerate the pathological SSA form.
        //ASSERT0(def);
    }

    //PR SSAInfo
    PRNO defprno = PRNO_UNDEF;
    DUMMYUSE(defprno);
    if (def != nullptr) {
        prt(rg, "DEF:%s", IRNAME(def)); //def may be IR_UNDEF.
        if (def->isWritePR()) {
            defprno = def->getPrno();
            prt(rg, "(%s%d,id:%d)", PR_TYPE_CHAR, def->getPrno(), def->id());
        } else if (def->isCallStmt()) {
            if (def->hasReturnValue()) {
                defprno = def->getPrno();
                prt(rg, "(%s%d,id:%d)", PR_TYPE_CHAR, def->getPrno(),
                    def->id());
            } else {
                prt(rg, "NoRetVal??");
            }
        } else if (vpr->version() == PRSSA_INIT_VERSION) {
            //CASE: Zero verison VPR might be generated in constructing SSA
            //region, the zero version VPR will be renamed to another
            //non-zero version VPR. However, both the two VPRs reference same
            //PRNO, and both of them record the same DEF stmt. Thus when one
            //of VPR(SSAInfo) are updated during DU modification, the DEF stmt
            //is freed, the PRSSAMgr does not update the another DEF meanwhile.
            //e.g: $9 is renamed to $18, and immediately VPR8 is obsoleted.
            //Howevern both VPR8 and VPR15 have recorded the DEF stmt which
            //pointed to (stpr id:49), when the DEF in VPR15 freed in DCE,
            //the DEF of VPR15 is NULL, but the DEF of VPR8 became to IR_UNDEF.
            //VPR8:$9v0$9: DEF:stpr($18,id:49) USE:--
            //VPR15:$9v1$18: DEF:stpr($18, id:49) USE : --
        } else {
            ASSERTN(0, ("not def stmt of PR"));
        }
    } else {
        prt(rg, "DEF:--");
    }

    if (!vpr->hasUse()) {
        prt(rg, " USE:--");
        return;
    }
    prt(rg, " USE:");
    SSAUseIter it = nullptr;
    BSIdx nextu = 0;
    for (BSIdx u = SSA_uses(vpr).get_first(&it); it != nullptr; u = nextu) {
        nextu = SSA_uses(vpr).get_next(u, &it);
        IR * use = rg->getIR(u);
        ASSERTN(use, ("'%s' does not have No.%u IR", u));
        ASSERT0(use && use->is_pr());
        ASSERTN(defprno == PRNO_UNDEF || defprno == use->getPrno(),
                ("unmatched PR"));
        prt(rg, "id:%d", use->id());
        if (nextu != BS_UNDEF) {
            prt(rg, ",");
        }
    }
}


static void dumpDefChain(VPR const* vpr, Region const* rg)
{
    VPR const* prevdef = VPR_prev(vpr);
    if (prevdef != nullptr) {
        prt(rg, ",PrevDEF:VPR%u", prevdef->id());
        IR const* prevdef_ir = prevdef->getDef();
        if (prevdef_ir != nullptr) {
            xcom::DefFixedStrBuf buf;
            prt(rg, ":%s", xoc::dumpIRName(prevdef_ir, buf));
        }
    }
    VPRSet const* nextset = VPR_nextset(vpr);
    if (nextset == nullptr || nextset->is_empty()) { return; }
    prt(rg, ",NextDEF:");
    VPRSetIter nit = nullptr;
    bool first = true;
    for (BSIdx w = nextset->get_first(&nit);
        w != BS_UNDEF; w = nextset->get_next(w, &nit)) {
        if (first) {
            first = false;
        } else {
            prt(rg, ",");
        }
        prt(rg, "VPR%u", w);
    }
}


static void dumpVersion(VPR const* vpr, Region const* rg)
{
    if (vpr->orgprno() != PRNO_UNDEF) {
        prt(rg, "%s%d", PR_TYPE_CHAR, vpr->orgprno());
    } else {
        prt(rg, "--");
    }
    prt(rg, "v%d", vpr->version());
    if (vpr->newprno() != PRNO_UNDEF) {
        prt(rg, "%s%d", PR_TYPE_CHAR, vpr->newprno());
    } else {
        prt(rg, "--");
    }
}


void SSAInfo::dump(Region const* rg) const
{
    //VPR info is only usful during SSA construction.
    note(rg, "\nVPR%d:", id());
    VPR * vpr = ((VPR*)this);
    dumpVersion(vpr, rg);
    prt(rg, ": ");
    dumpDefUseRef(vpr, rg);
    dumpDefChain(vpr, rg);
}
//END SSAInfo


//
//START VPR
//
CHAR const* VPR::dumpBuf(OUT StrBuf & buf) const
{
    if (orgprno() != PRNO_UNDEF) {
        buf.sprint("%s%uv%u", PR_TYPE_CHAR, orgprno(), version());
    } else {
        buf.sprint("-v%u", version());
    }
    if (newprno() != PRNO_UNDEF) {
        buf.strcat("%s%u", PR_TYPE_CHAR, newprno());
    } else {
        buf.strcat("--");
    }
    return buf.buf;
}
//END VPRVec


//
//START VPRVec
//
//Find the VPR that have PR defined at given BB.
VPR * VPRVec::findVPR(UINT bbid) const
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        VPR * vpr = get(i);
        if (vpr == nullptr || vpr->getDef() == nullptr) { continue; }
        ASSERT0(vpr->getDef() && vpr->getDef()->getBB());
        if (vpr->getDef()->getBB()->id() == bbid) {
            return vpr;
        }
    }
    return nullptr;
}


//Find the initial version VPR.
VPR * VPRVec::getInitVersion() const
{
    return get(PRSSA_INIT_VERSION);
}
//END VPRVec


//
//START VPRSet
//
void VPRSet::append(VPR const* v, MOD VPRSetMgr & mgr)
{
    xcom::DefSBitSetCore::bunion(v->id(), mgr.getSBSMgr());
}


void VPRSet::remove(VPR const* v, MOD VPRSetMgr & mgr)
{
    ASSERT0(v);
    xcom::DefSBitSetCore::diff(v->id(), mgr.getSBSMgr());
}
//END VPRSet


//
//START VPRSetMgr
//
VPRSetMgr::VPRSetMgr()
{
    m_set_pool = nullptr;
    init();
}


VPRSetMgr::~VPRSetMgr()
{
    destroy();
}


void VPRSetMgr::init()
{
    if (m_set_pool != nullptr) { return; }
    m_set_pool = xcom::smpoolCreate(sizeof(VPRSet) * 2, MEM_CONST_SIZE);
    m_sbs_mgr.init();
}


void VPRSetMgr::destroy()
{
    if (m_set_pool == nullptr) { return; }
    for (VPRSet * set = m_set_list.get_head();
         set != nullptr; set = m_set_list.get_next()) {
        set->clean(getSBSMgr());
    }
    m_set_list.clean();
    xcom::smpoolDelete(m_set_pool);
    m_sbs_mgr.destroy();
    m_set_pool = nullptr;
}


VPRSet * VPRSetMgr::allocVPRSet()
{
    ASSERT0(m_set_pool);
    VPRSet * set = (VPRSet*)xcom::smpoolMallocConstSize(
        sizeof(VPRSet), m_set_pool);
    set->init();
    m_set_list.append_tail(set);
    return set;
}
//END VPRSetMgr

} //namespace xoc
