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

IRMgrExt::IRMgrExt(Region * rg) : IRMgr(rg)
{
    ASSERT0(m_rg->getPassMgr());
}


IR * IRMgrExt::buildVIStore(IR * base, TMWORD ofst, IR * rhs, IR * dummyuse,
                            Type const* ty)
{
    ASSERT0(ty && base);
    IR * ir = allocIR(IR_VIST);
    VIST_base(ir) = base;
    VIST_ofst(ir) = ofst;
    VIST_rhs(ir) = rhs;
    IR_dt(ir) = ty;
    IR_parent(base) = ir;
    if (rhs != nullptr) {
        ASSERT0(rhs->is_single());
        IR_parent(rhs) = ir;
    }
    VIST_dummyuse(ir) = dummyuse;
    if (dummyuse != nullptr) {
        ASSERT0(dummyuse->is_dummyuse());
        ASSERT0(dummyuse->is_single());
        IR_parent(dummyuse) = ir;
    }
    return ir;
}


IR * IRMgrExt::buildVStore(Var * lhs, TMWORD ofst, IR * rhs, IR * dummyuse,
                         Type const* ty)
{
    ASSERT0(ty && lhs);
    IR * ir = allocIR(IR_VST);
    VST_idinfo(ir) = lhs;
    VST_ofst(ir) = ofst;
    VST_rhs(ir) = rhs;
    IR_dt(ir) = ty;
    if (rhs != nullptr) {
        ASSERT0(rhs->is_single());
        IR_parent(rhs) = ir;
    }
    VST_dummyuse(ir) = dummyuse;
    if (dummyuse != nullptr) {
        ASSERT0(dummyuse->is_dummyuse());
        ASSERT0(dummyuse->is_single());
        IR_parent(dummyuse) = ir;
    }
    return ir;
}


IR * IRMgrExt::buildVStorePR(PRNO resprno, IR * rhs, IR * dummyuse,
                             Type const* ty)
{
    ASSERT0(resprno != PRNO_UNDEF);
    IR * ir = allocIR(IR_VSTPR);
    VSTPR_no(ir) = resprno;
    IR_dt(ir) = ty;
    VSTPR_rhs(ir) = rhs;
    if (rhs != nullptr) {
        ASSERT0(rhs->is_single());
        IR_parent(VSTPR_rhs(ir)) = ir;
    }
    VSTPR_dummyuse(ir) = dummyuse;
    if (dummyuse != nullptr) {
        ASSERT0(dummyuse->is_dummyuse());
        ASSERT0(dummyuse->is_single());
        IR_parent(dummyuse) = ir;
    }
    return ir;
}


IR * IRMgrExt::buildBroadCast(IR * src, IR * res_list, Type const* ty)
{
    ASSERT0(src && res_list);
    IR * ir = allocIR(IR_BROADCAST);
    BROADCAST_src(ir) = src;
    IR_parent(src) = ir;
    BROADCAST_res_list(ir) = res_list;
    for (IR * res = res_list; res != nullptr; res = res->get_next()) {
        IR_parent(res) = ir;
    }
    IR_dt(ir) = ty;
    return ir;
}


bool IRMgrExt::hasMultiRes(IR * stmt) const
{
    switch (stmt->getCode()) {
    case IR_BROADCAST: return true;
    default:;
    }
    return false;
}


IR * IRMgrExt::getAlterResDescList(IR * stmt) const
{
    switch (stmt->getCode()) {
    case IR_BROADCAST:
        return BROADCAST_res_list(stmt);
    default: UNREACHABLE();
    }
    return nullptr;
}

} //namespace xoc
