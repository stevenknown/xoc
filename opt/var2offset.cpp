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
HOST_UINT Var2Offset::getOrAddVarOffset(xoc::Var const* v)
{
    bool find = false;
    HOST_UINT off = get(v, &find);
    if (find) { return off; }

    m_cur_offset = (HOST_UINT)xcom::ceil_align(m_cur_offset,
        MAX(v->get_align(), getAlign()));
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


HOST_INT Var2OffsetMgr::computeLocalVarOffsetFromFP(Var const* var)
{
    ASSERT0(var);
    ASSERT0(var != m_pelog->getRAVar() && var != m_pelog->getFPVar());
    HOST_INT offset = m_local_var2off->getOrAddVarOffset(var);
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
        return -offset;
    }

    //Formula: offset = stacksize - var2off.getOrAddVarOffset(var)
    ASSERT0(m_dystack_impl->hasAlloca());
    offset = m_pelog->getStackSpaceSize() - offset;
    return -offset;
}


HOST_INT Var2OffsetMgr::computeLocalVarOffset(Var const* var)
{
    ASSERT0(var);
    ASSERT0(var != m_pelog->getRAVar() && var != m_pelog->getFPVar());
    if (m_pelog->isUsedFPAsSP()) {
        return computeLocalVarOffsetFromFP(var);
    }
    HOST_UINT off = m_local_var2off->getOrAddVarOffset(var);

    //NOTE: MaxArgSize is the argument space size, The offset of the
    //local var needs to accumulate the MaxArgSize,
    //so the MaxArgSize needs to satisfy the alignment requirements.
    HOST_UINT offset = off +
        xcom::ceil_align(m_arg_passer->getMaxArgSize(),
        m_pelog->getMaxAlignment());
    return offset;
}


HOST_UINT Var2OffsetMgr::computeSpecialVarOffset(Var const* var)
{
    ASSERT0(var);
    ASSERT0(var == m_pelog->getRAVar() || var == m_pelog->getFPVar() ||
            var == m_pelog->getSSVar());

    HOST_UINT var_offset = m_special_var2off->getOrAddVarOffset(var);

    var_offset = m_pelog->getStackSpaceSize() - var_offset -
        var->getByteSize(m_tm) - m_pelog->getSizeOfVaarg();
    return var_offset;
}


//Compute argument var offset.
HOST_UINT Var2OffsetMgr::computeArgVarOffset(Var const* var)
{
    ASSERT0(var && isArgument(var));
    return m_arg_var2off->getOrAddVarOffset(var);
}


HOST_UINT Var2OffsetMgr::getParamOffset(Var const* var) const
{
    ASSERT0(var);
    TMWORD offset = (TMWORD)m_param_var2off->get(var);
    if (m_pelog->isUsedFPAsSP() || m_pelog->isNeedStackRealignment()) {
        return offset;
    }
    return offset + m_pelog->getStackSpaceSize();
}


void Var2OffsetMgr::computeParamOffset()
{
    ParamList * param_list_stack = m_arg_passer->getListOfParamOnStack();
    ASSERT0(param_list_stack);
    for (xoc::Var const* v = param_list_stack->get_head(); v != nullptr;
         v = param_list_stack->get_next())
    { m_param_var2off->getOrAddVarOffset(v); }
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
        { computeArgVarOffset(v); }

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


HOST_INT Var2OffsetMgr::computeVarOffset(Var const* var)
{
    ASSERT0(var);
    if (isParameter(var)) {
        return getParamOffset(var);
    }
    if (isArgument(var)) {
        return computeArgVarOffset(var);
    }
    if (isSpillVarInEntryBB(var)) {
        return computeSpecialVarOffset(var);
    }
    return computeLocalVarOffset(var);
}


} // namespace xoc
