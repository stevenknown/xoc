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
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {
//
//START Var2Offset
//
HOST_UINT Var2Offset::getOrAddVarOffset(xoc::Var const* v, bool * find_ptr)
{
    bool find = false;
    HOST_UINT off = get(v, &find);
    if (find_ptr != nullptr) { *find_ptr = find; }
    if (find) { return off; }

    HOST_UINT adjust_align =
        computeAlignForTwoAligns(v->get_align(), getAlign());
    m_cur_offset = (HOST_UINT)xcom::ceil_align(m_cur_offset, adjust_align);
    off = m_cur_offset;
    set(v, off);
    m_cur_offset += v->getByteSize(m_tm);
    return off;
}


void Var2Offset::dump(OUT StrBuf & buf) const
{
    Var2OffsetIter it;
    HOST_UINT off;
    xcom::StrBuf tmp(32);
    xoc::VarMgr const* vm = m_tm->getRegionMgr()->getVarMgr();
    for (xoc::Var const* v = get_first(it, &off); !it.end();
         v = get_next(it, &off)) {
        v->dump(tmp, vm);
        buf.strcat("%s:OFFSET:0x%x", tmp.buf, (UINT)off);
    }
}


void Var2Offset::dump(OUT FileObj & fo) const
{
    xcom::StrBuf buf(32);
    dump(buf);
    fo.prt("%s", buf.buf);
}


void Var2OffsetMgr::computeLocalVarOffsetFromFP(Var const* var)
{
    ASSERT0(var);
    ASSERT0(var != m_pelog->getRAVar() && var != m_pelog->getFPVar());
    bool find = false;
    HOST_INT offset = m_local_var2off->getOrAddVarOffset(var, &find);
    if (find) { return; }
    DynamicStack * m_dystack_impl = (DynamicStack*)m_rg->getPassMgr()->
        queryPass(PASS_DYNAMIC_STACK);
    if (m_dystack_impl == nullptr || !m_dystack_impl->hasAlloca()) {
        //offset = stacksize - var2off.getOrAddVarOffset(var) - argument size
        //NOTE: MaxArgSize is the argument space size, The offset of the local
        //var needs to accumulate the MaxArgSize, so the MaxArgSize needs to
        //satisfy the alignment requirements of local variables.
        offset = m_pelog->getStackSpaceSize() - offset -
            xcom::ceil_align(m_arg_passer->getMaxArgSize(),
            m_pelog->getMaxAlignment());
    } else {
        //Formula: offset = stacksize - var2off.getOrAddVarOffset(var)
        ASSERT0(m_dystack_impl->hasAlloca());
        offset = m_pelog->getStackSpaceSize() - offset;
    }
    m_local_var2off->setAlways(var, -offset);
}


void Var2OffsetMgr::computeLocalVarOffset(Var const* var)
{
    ASSERT0(var);
    ASSERT0(var != m_pelog->getRAVar() && var != m_pelog->getFPVar());
    if (m_pelog->isUsedFPAsSP()) {
        return computeLocalVarOffsetFromFP(var);
    }
    bool find = false;
    HOST_UINT off = m_local_var2off->getOrAddVarOffset(var, &find);
    if (find) { return; }

    //NOTE: MaxArgSize is the argument space size, The offset of the
    //local var needs to accumulate the MaxArgSize,
    //so the MaxArgSize needs to satisfy the alignment requirements.
    HOST_UINT offset = off +
        xcom::ceil_align(m_arg_passer->getMaxArgSize(),
        m_pelog->getMaxAlignment());
    m_local_var2off->setAlways(var, offset);
}


void Var2OffsetMgr::computeSpecialVarOffset(Var const* var)
{
    ASSERT0(var);
    ASSERT0(var == m_pelog->getRAVar() || var == m_pelog->getFPVar() ||
            var == m_pelog->getSSVar());
    bool find = false;
    HOST_UINT var_offset = m_special_var2off->getOrAddVarOffset(var, &find);
    if (find) { return; }
    var_offset = m_pelog->getStackSpaceSize() - var_offset -
        var->getByteSize(m_tm) - m_pelog->getSizeOfVaarg();
    m_special_var2off->setAlways(var, var_offset);
}


void Var2OffsetMgr::computeParamOffset()
{
    ParamList * param_list_stack = m_arg_passer->getListOfParamOnStack();
    ASSERT0(param_list_stack);
    bool find = false;
    for (xoc::Var const* v = param_list_stack->get_head(); v != nullptr;
         v = param_list_stack->get_next()) {
        HOST_INT offset = m_param_var2off->getOrAddVarOffset(v, &find);
        m_param_var2off->setAlways(v, offset);
        if (find) { continue; }
        if (m_pelog->isUsedFPAsSP() || m_pelog->isNeedStackRealignment()) {
            continue;
        }
        m_param_var2off->setAlways(v, offset + m_pelog->getStackSpaceSize());
    }
}


void Var2OffsetMgr::computeArgVarOffset()
{
    CallIR2ArgListMap const* callir2_arg_list = m_arg_passer->getStackArgMap();
    CallIR2ArgListIter iter;
    xcom::List<Var const*> * callir_param_list = nullptr;
    for (callir2_arg_list->get_first(iter, &callir_param_list); !iter.end();
         callir2_arg_list->get_next(iter, &callir_param_list)) {
        for (xoc::Var const* v = callir_param_list->get_head();
             v != nullptr; v = callir_param_list->get_next())
        { m_arg_var2off->getOrAddVarOffset(v); }

        //The argument space can be reused and reset when the function
        //call is finished.
        resetArgSpaceOffset();
    }
}


bool Var2OffsetMgr::isParameter(Var const* var) const
{
    ASSERT0(var);
    ASSERT0(m_arg_passer->getListOfParamOnStack());
    return m_arg_passer->getListOfParamOnStack()->find(var);
}


bool Var2OffsetMgr::isArgument(Var const* var) const
{
    ASSERT0(var);
    ASSERT0(m_arg_passer->getStackArgMap());
    return m_arg_passer->getStackArgMap()->findVar(var);
}


bool Var2OffsetMgr::isSpillVarInEntryBB(Var const* var) const
{
    ASSERT0(var);
    return var == m_pelog->getFPVar() || var == m_pelog->getRAVar() ||
        var == m_pelog->getSSVar();
}


void Var2OffsetMgr::resetArgSpaceOffset()
{
    m_arg_var2off->resetCurOffset();
}


void Var2OffsetMgr::reviseParamOffsetByPushPopSize(UINT offset)
{
    ParamList * param_list_stack = m_arg_passer->getListOfParamOnStack();
    ASSERT0(param_list_stack);
    bool find = false;
    for (xoc::Var const* v = param_list_stack->get_head(); v != nullptr;
         v = param_list_stack->get_next()) {
        HOST_INT offset_new = m_param_var2off->getOrAddVarOffset(v, &find) +
            offset;
        m_param_var2off->setAlways(v, offset_new);
    }
}


void Var2OffsetMgr::computeVarOffset(Var const* var)
{
    ASSERT0(var);
    ASSERT0(!isParameter(var) && !isArgument(var));
    if (isSpillVarInEntryBB(var)) {
        computeSpecialVarOffset(var);
    } else {
        computeLocalVarOffset(var);
    }
}


HOST_INT Var2OffsetMgr::getVarOffset(Var const* var) const
{
    ASSERT0(var);
    bool find = false;
    HOST_INT offset = 0;
    if (isParameter(var)) {
        offset = m_param_var2off->get(var, &find);
    } else if (isArgument(var)) {
        offset = m_arg_var2off->get(var, &find);
    } else if (isSpillVarInEntryBB(var)) {
        offset = m_special_var2off->get(var, &find);
    } else {
        offset = m_local_var2off->get(var, &find);
    }
    ASSERT0(find);
    return offset;
}


} // namespace xoc
