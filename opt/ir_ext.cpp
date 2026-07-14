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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

bool IR::isVirtualOp() const
{
    switch (getCode()) {
    SWITCH_CASE_EXT_VSTMT: return true;
    default:; //Nothing to do.
    }
    return false;
}

//Return the 'base' of partial operations.
IR const* IR::getBaseOfPartialSetOp() const
{
    switch (getCode()) {
    case IR_SELECT_TO_RES: return SELECTTORES_op(this);
    case IR_DYNLEN_OP: return DYNLENOP_op(this);
    case IR_MASK_OP: return MASKOP_op(this);
    default: UNREACHABLE(); return nullptr;
    }
}

//Return the 'len' of partial operations.
IR const* IR::getLenOfPartialSetOp() const
{
    IR const* len = nullptr;
    IR const* rhs = this;
    if (is_select_to_res()) {
        rhs = getBaseOfPartialSetOp();
    }
    if (rhs->is_dynlenop()) {
        len = DYNLENOP_len(rhs);
    } else if (rhs->is_maskop()) {
        len = DYNLENOP_len(rhs->getBaseOfPartialSetOp());
    }
    ASSERT0(len);
    return len;
}

//Return the 'mask' of partial operations.
IR const* IR::getMaskOfPartialSetOp() const
{
    IR const* mask = nullptr;
    IR const* rhs = this;
    if (is_select_to_res()) {
        rhs = getBaseOfPartialSetOp();
    }
    if (rhs->is_maskop()) {
        mask = MASKOP_mask(rhs);
    }
    ASSERT0(mask);
    return mask;
}

} //namespace xoc
