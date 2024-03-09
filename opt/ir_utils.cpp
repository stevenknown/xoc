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
        SWITCH_CASE_STMT:
        SWITCH_CASE_EXP:
            break;
        default: checkIRSwitchCaseInterface((IR_CODE)i);
        }
        if (IRDES_is_stmt(g_ir_desc[i])) {
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
        if (IRDES_is_direct_mem_op(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_DIRECT_MEM_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_indirect_mem_op(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_INDIRECT_MEM_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_write_pr(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_WRITE_PR:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_array_op(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_ARRAY_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_conditional_br(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_CONDITIONAL_BRANCH_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_unconditional_br(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_UNCONDITIONAL_BRANCH_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_bin(g_ir_desc[i])) {
            DUMMYUSE_LABEL(SWITCH_CASE_EXT_BIN);
            switch ((IR_CODE)i) {
            SWITCH_CASE_BIN:
            SWITCH_CASE_EXT_BIN:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_una(g_ir_desc[i])) {
            DUMMYUSE_LABEL(SWITCH_CASE_EXT_UNA);
            switch ((IR_CODE)i) {
            SWITCH_CASE_UNA:
            SWITCH_CASE_EXT_UNA:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_stmt_in_bb(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_STMT_IN_BB:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_is_non_pr_memref(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_MEM_NONPR_OP:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
        if (IRDES_has_du(g_ir_desc[i])) {
            switch ((IR_CODE)i) {
            SWITCH_CASE_HAS_DU:
                break;
            default: checkIRSwitchCaseInterface((IR_CODE)i);
            }
        }
    }
    return true;
}

} //namespace xoc
