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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

bool RegionMgr::checkIRSwitchCaseInterface(IR_CODE c) const
{
    ASSERTN(0, ("miss entry")); //Target Dependent Code.
    return true;
}


bool RegionMgr::checkIRSwitchCaseEntry() const
{
    for (UINT i = IR_UNDEF + 1; i < IR_CODE_NUM; i++) {
        switch ((IR_CODE)i) {
        SWITCH_CASE_EXT_PLACEHOLDER:
            //Placeholder IRs do not participate any concrete IR operations,
            //there is no need to check them.
            continue;
        default:; //Nothing to do.
        }
        switch ((IR_CODE)i) {
        SWITCH_CASE_STMT:
        SWITCH_CASE_EXP:
            break;
        default: checkIRSwitchCaseInterface((IR_CODE)i);
        }
        if (IRDES_is_stmt(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_STMT:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        } else {
            switch ((IR_CODE)i) {
            SWITCH_CASE_EXP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_direct_mem_op(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_DIRECT_MEM_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_indirect_mem_op(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_INDIRECT_MEM_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_write_pr(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_WRITE_PR:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_array_op(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_ARRAY_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_conditional_br(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_CONDITIONAL_BRANCH_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_unconditional_br(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_UNCONDITIONAL_BRANCH_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_ter(i)) { checkIRSwitchCaseInterface((IR_CODE)i); }
        if (IRDES_is_bin(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_BIN:
            SWITCH_CASE_EXT_BIN:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_una(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_UNA:
            SWITCH_CASE_EXT_UNA:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_stmt_in_bb(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_STMT_IN_BB:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_non_pr_memref(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_MEM_NONPR_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_has_du(i)) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_HAS_DU:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
    }
    return true;
}

//
//START IRList
//
bool IRList::verifyUnique(IR const* ir) const
{
    IRListIter it;
    for (IR const* x = get_head(&it); x != nullptr; x = get_next(&it)) {
        ASSERTN(x != ir, ("ir is already on the list"));
    }
    return true;
}
//END IRList


//
//START ConstIRList
//
bool ConstIRList::verifyUnique(IR const* ir) const
{
    ConstIRListIter it;
    for (IR const* x = get_head(&it); x != nullptr; x = get_next(&it)) {
        ASSERTN(x != ir, ("ir is already on the list"));
    }
    return true;
}
//END ConstIRList


//
//START IRVec
//
bool IRVec::is_empty() const
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        if (get(i) != nullptr) { return false; }
    }
    return true;
}


void IRVec::dump(Region const* rg) const
{
    note(rg, "\n-- DUMP IRVec --");
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        IR const* ir = get(i);
        if (ir == nullptr) { continue; }
        dumpIR(ir, rg);
    }
}


void IRVec::copyContent(IRVec const& src, Region * rg)
{
    ASSERT0(is_empty());
    for (VecIdx i = 0; i <= src.get_last_idx(); i++) {
        IR const* ir = src.get(i);
        if (ir == nullptr) { continue; }
        set(i, rg->dupIRTree(ir));
    }
}


void IRVec::freeContent(Region * rg)
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        IR * ir = get(i);
        if (ir == nullptr) { continue; }
        rg->freeIRTree(ir);
    }
    clean();
}
//END IRVec

} //namespace xoc
