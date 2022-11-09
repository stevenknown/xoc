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

bool verifyVST(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(d);
    ASSERTN(d->getDType()!= D_UNDEF, ("size of store value cannot be zero"));
    ASSERT0(VST_idinfo(ir));
    if (VST_rhs(ir) != nullptr) {
        ASSERT0(VST_rhs(ir)->is_exp());
        ASSERT0(VST_rhs(ir)->is_single());
    }
    if (d->is_vector()) {
        ASSERT0(d->getVectorElemType() != D_UNDEF);
        ASSERT0(tm->getDTypeByteSize(d->getVectorElemType()) >=
                tm->getByteSize(VST_rhs(ir)->getType()));
    }
    return true;
}


bool verifyVIST(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(d);
    if (!g_is_support_dynamic_type) {
        ASSERTN(VIST_base(ir)->is_ptr(), ("base must be pointer"));
        ASSERT0(tm->getPointerBaseByteSize(VIST_base(ir)->getType()) > 0);
    }
    ASSERT0(VIST_base(ir)->is_single());
    if (VIST_rhs(ir) != nullptr) {
        ASSERT0(VIST_rhs(ir)->is_exp());
        ASSERT0(VIST_rhs(ir)->is_single());
    }
    ASSERTN(ir->getDType() != D_UNDEF,
            ("size of virtual istore value cannot be zero"));
    return true;
}


bool verifyVSTPR(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(d);
    ASSERTN(ir->getDType() != D_UNDEF, ("size of store value cannot be zero"));
    ASSERT0(ir->getDType() != D_UNDEF);
    ASSERT0(VSTPR_no(ir) != PRNO_UNDEF);
    if (VSTPR_rhs(ir) != nullptr) {
        ASSERT0(VSTPR_rhs(ir)->is_exp());
        ASSERT0(VSTPR_rhs(ir)->is_single());
    }
    return true;
}


bool verifyBROADCAST(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(BROADCAST_src(ir) && BROADCAST_src(ir)->is_exp());
    ASSERT0(BROADCAST_src(ir)->is_single());
    return true;
}

} //namespace xoc
