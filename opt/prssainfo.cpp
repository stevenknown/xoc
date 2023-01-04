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
void SSAInfo::dump(Region const* rg) const
{
    //VPR info is only usful during SSA construction.
    note(rg, "\nVPR%d:", id());

    VPR * vpr = ((VPR*)this);
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
    prt(rg, ": ");

    IR * def = getDef();
    if (vpr->version() != PRSSA_INIT_VERSION) {
        //After renaming, version is meaningless, thus it is only visible
        //to VPR.
        //For convenient purpose, tolerate the pathological SSA form.
        //ASSERT0(def);
    }

    //PR SSAInfo
    PRNO defprno = PRNO_UNDEF;
    if (def != nullptr) {
        ASSERT0(def->is_stmt());
        prt(rg, "DEF:%s", IRNAME(def));
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
        } else {
            ASSERTN(0, ("not def stmt of PR"));
        }
    } else {
        prt(rg, "DEF:--");
    }

    if (!hasUse()) {
        prt(rg, " USE:--");
        return;
    }
    prt(rg, " USE:");
    SSAUseIter vit = nullptr;
    BSIdx nexti = 0;
    for (BSIdx i2 = SSA_uses(this).get_first(&vit);
         vit != nullptr; i2 = nexti) {
        nexti = SSA_uses(this).get_next(i2, &vit);
        IR * use = rg->getIR(i2);
        ASSERT0(use->is_pr());
        ASSERTN(defprno == PRNO_UNDEF || defprno == use->getPrno(),
                ("unmatched PR"));
        prt(rg, "id:%d", use->id());
        if (nexti != BS_UNDEF) {
            prt(rg, ",");
        }
    }
}
//END SSAInfo


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

} //namespace xoc
