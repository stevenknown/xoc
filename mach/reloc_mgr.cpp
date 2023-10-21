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
#include "machinc.h"
#include "../elf/elfinc.h"
#include "../opt/comopt.h"

namespace mach {

static bool hasLocalVar(mach::MInst const* mi)
{
    return (mi->hasVar() && MI_var(mi) != nullptr &&
            MI_var(mi)->is_local());
}

//
//START Var2Offset
//
TMWORD Var2Offset::getOrAddVarOffset(xoc::Var const* v)
{
    bool find = false;
    TMWORD off = get(v, &find);
    if (find) { return off; }

    m_cur_offset = (TMWORD)xcom::ceil_align(m_cur_offset,
        MAX(v->get_align(), getAlign()));

    off = m_cur_offset;
    set(v, off);
    m_cur_offset += v->getByteSize(m_tm);
    return off;
}


// ---------------------------------
// | FP |var0|var1| ...       | SP |
//      ^    ^    ^                ^
//      A    B    C                D
// ---------------------------------
// sp_rel_offset = C - D
// address(var1) = address(SP) + sp_rel_offset
//
// Equals:
//
// fp_rel_offset = A - C
// address(var1) = address(FP) - fp_rel_offset
// address(var1) = address(FP) - (stack_size - sp_rel_offset)
TMWORD Var2Offset::getOrAddVarOffsetRelatedFP(xoc::Var const* v,
                                              UINT stack_size)
{
    bool find = false;
    TMWORD off = get(v, &find);
    if (find) { return off; }

    // Why minus v->get_align() ?
    // eg:
    // stack_size: 896
    // m_cur_offset: 16
    //
    // var1: bytesize(8), align(8)
    // var2: bytesize(64), align(64)
    //
    // CASE1: No minus v->get_align().
    // [var1]:
    // fp_rel_offset = 896 - 16 = 880
    // after align: fp_rel_offset = 880
    // m_cur_offset = 896 - 880 + 8 = 24
    // [var2]:
    // fp_rel_offset = 896 - 24 = 872
    // after align: fp_rel_offset = 896
    // Result is error!
    // fp_rel_offset of var2 shouldn't be greater than that of var1.

    // CASE2: With minus v->get_align().
    // [var1]:
    // fp_rel_offset = 896 - 8 - 16 = 872
    // after align: fp_rel_offset = 872
    // m_cur_offset = 896 - 872 + 16 = 32
    // [var2]:
    // fp_rel_offset = 896 - 64 - 32 = 800
    // after align: fp_rel_offset = 832
    // Result is correct!
    // 832 is less than 872 and meets the alignment requirements.

    // NOTE: calculating the offset of var1 also subtracts alignment,
    // which causing memory wasted.
    // Therefore, only when m_cur_offset is not aligned with v->get_align(),
    // fp_rel_offset = stack_size - v->get_align() - m_cur_offset;

    TMWORD fp_rel_offset;
    if ((m_cur_offset % v->get_align()) != 0) {
        fp_rel_offset = stack_size - v->get_align() - m_cur_offset;
    } else {
        fp_rel_offset = stack_size - m_cur_offset;
    }

    fp_rel_offset = (TMWORD)xcom::ceil_align(fp_rel_offset,
        MAX(v->get_align(), getAlign()));

    off = fp_rel_offset;
    set(v, off);

    m_cur_offset = stack_size - fp_rel_offset +
        v->getByteSize(m_tm);

    // m_cur_offset should also be aligned according to the STACK_ALIGNMENT.
    // eg:
    // var1: bytesize(12), align(8)
    // var2: bytesize(8), align(8)

    // CASE1: No align:
    // [var1]:
    // fp_rel_offset = 896 - 16 = 880
    // after align: fp_rel_offset = 880
    // m_cur_offset = 896 - 880 + 12 = 28
    // [var2]:
    // fp_rel_offset = 896 - 28 = 868
    // after align: fp_rel_offset = 872
    // --> Result is error!  880 - 872 < 12

    // CASE2: With align:
    // [var1]:
    // fp_rel_offset = 896 - 16 = 880
    // after align: fp_rel_offset = 880
    // m_cur_offset = 896 - 880 + 12 = 28
    // after align: m_cur_offset = 32
    // [var2]:
    // fp_rel_offset = 896 - 32 = 868
    // after align: fp_rel_offset = 864
    // --> Result is correct!  880 - 864 > 12

    m_cur_offset = (TMWORD)xcom::ceil_align(m_cur_offset, STACK_ALIGNMENT);
    return off;
}


void Var2Offset::dump(OUT StrBuf & buf) const
{
    Var2OffsetIter it;
    TMWORD off;
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
//END Var2Offset


//
//START RelocMgr
//
RelocMgr::RelocMgr(Region * rg, MInstMgr * imgr, TMWORD align) :
    m_rg(rg), m_mimgr(imgr), m_code_align(align), m_data_align(align)
{
    m_tm = m_rg->getTypeMgr();
    m_pelog = (PrologueEpilogueInserter *)m_rg->getPassMgr()->
        queryPass(PASS_PROLOGUE_EPILOGUE);
    m_ra = m_pelog->getLsra();
}


bool RelocMgr::isParameter(Var const* var) const
{
    bool is_param =
        m_ra->getArgPasser()->getStackParamList()->find((Var*)var);

    return is_param;
}


bool RelocMgr::isArgument(Var const* var) const
{
    bool is_arg =
        m_ra->getArgPasser()->getStackArgList()->find((Var*)var);

    return is_arg;
}


TMWORD RelocMgr::computeVarOffset(MInst * mi, OUT Var2Offset & var2off)
{
    Var const* var = MI_var(mi);
    TMWORD var_offset = 0;

    bool is_related_fp = (m_pelog->isUsedFP() &&
        isSpilledRegRelatedFP(mi)) ? true : false;

    if (is_related_fp) {

        //local_var_space_size =
        //    total_stack_size(sp - oldsp) - arg_space_size - maxalignment.
        //
        //  |    |------local var space-------|---arg space---|
        //  ^    ^                                            ^
        //oldsp  fp                                           sp
        //
        // The unavailable space between oldsp and fp,
        // is generated at stack realignment, the code is:
        //  srl     $FP, shift_mount, $FP
        //  sll     $FP, shift_mount, $FP
        UINT local_var_space_size = m_pelog->getStackSpaceSize() -
            m_ra->getArgPasser()->getMaxArgSize() -
            m_pelog->getMaxAlignment();

        var_offset = -(UINT64)var2off.getOrAddVarOffsetRelatedFP(var,
            local_var_space_size);
    } else {
        var_offset = var2off.getOrAddVarOffset(var) +
            m_ra->getArgPasser()->getMaxArgSize();
    }

    return var_offset;
}


TMWORD RelocMgr::computeArgVarOffset(MInst * mi, OUT Var2Offset & var2off)
{
    Var const* var = MI_var(mi);
    ASSERT0(isArgument(var));

    return var2off.getOrAddVarOffset(var);
}


void RelocMgr::resetArgSpaceOffset(MInst * mi, OUT Var2Offset & var2off)
{
    ASSERT0(isCall(mi));
    var2off.resetCurOffset();
}


TMWORD RelocMgr::getParamOffset(MInst * mi, Var2Offset & var2off) const
{
    Var const* var = MI_var(mi);
    TMWORD offset = var2off.get(var);

    return offset + m_pelog->getStackSpaceSize();
}


void RelocMgr::computeParamOffset(OUT Var2Offset & var2off)
{
    List<xoc::Var const*> param_list;
    m_rg->findFormalParam(param_list, true);

    for (xoc::Var const* v = param_list.get_head();
         v != nullptr; v = param_list.get_next()) {
        bool is_param =
            m_ra->getArgPasser()->getStackParamList()->find((Var*)v);

        if (!is_param) { continue; }

        var2off.getOrAddVarOffset(v);
    }
}


//A helper function to judge whether current machine instruction is aligned
//by word length of machine instruction of current architecture.
static bool isMIAlignedByWordLength(TMWORD offset, UINT length)
{
    return offset % length == 0;
}


//A helper function to compute offset of jump instruction based on PC value of
//target label and current instruction. The formula is as follows:
//   offset = (PC_target_label - PC_jump_instruction) / SIZE_instruction;
//Note that, PC values must be aligned by size of instructions.
static TMWORD computeJumpOff(Label2Offset const& lab2off, MInst const* mi)
{
    TMWORD label_pc = lab2off.get(MI_lab(mi));
    TMWORD inst_pc = MI_pc(mi);
    TMWORD inst_size = mi->getWordBufLen();

    ASSERT0(isMIAlignedByWordLength(label_pc, inst_size));
    ASSERT0(isMIAlignedByWordLength(inst_pc, inst_size));
    ASSERT0(isMIAlignedByWordLength(label_pc - inst_pc, inst_size));

    //Label is a custom label.
    if (lab2off.find(MI_lab(mi))) {
        return (label_pc - inst_pc) / inst_size - 1;
    }

    //Label is a internal used for relaxation.
    //If relaxation is performed, uncond-br "br" is only used to save current
    //PC value and cond-br such as "bne" is used to skip uncond-br.
    //For example:
    //    br   $31, label_0                   beq $1, label_1
    //    ......                              ......
    //          |                                    |
    //          V                                    V
    //    br   $28, 0      <--- This is 0.    bne $1, 1        <--- This is 1.
    //    ldih $63, 0($31)                    br  $31, label_1
    //    ......                              ......
    //    addl $63, $28, $28
    //    jmp  $31, $28, 0x1
    //    ......
    return 0;
}


// Stack Space:
// ---------------
// |    caller   |    |
// |             |  caller local var space
// |             |    |
// |             |-----
// |             |    |
// |             |  caller argument space:
// |             |    |  [param2off] = offset + stacksize of callee.
// --------------------   NOTE: This offset is calculated for callee.
// |    callee   |    |
// |             |  callee local var space:
// |             |    |  [var2off] = offset + maxArgumentSize.
// |             |-----
// |             |    |
// |             |  callee argument space:
// |             |    | [arg2off] = offset
// ---------------   NOTE:The capacity of argument space is maxArgumentSize.
//
//[var2off]: used to calculate the relative offset within local var space.
//
//[param2off]:
// used to calculate the relative offset within caller argument var space.
//
//[arg2off]:
// used to calculate the relative offset within callee argument var space.
void RelocMgr::computeDataOffset(MOD MIList & milst,
                                 OUT Var2Offset & var2off,
                                 Label2Offset const& lab2off)
{
    MIListIter it;
    TMWORD offset = 0;

    //var2off: local var space
    //param2off: parameter space
    //arg2off: argument space
    Var2Offset param2off(getDataAlign(), m_tm);
    Var2Offset arg2off(getDataAlign(), m_tm);

    computeParamOffset(param2off);
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it)) {
        if (mi->getCode() == MI_label) {
            continue;
        }
        if (mi->hasLab()) {
            ASSERT0(MI_lab(mi));
            setValueViaMICode(mi, computeJumpOff(lab2off, mi));
            continue;
        }

        if (isCall(mi)) {
            resetArgSpaceOffset(mi, arg2off);
        }

        if (!hasLocalVar(mi)) { continue; }

        Var const* var = MI_var(mi);
        TMWORD var_offset;
        if (isParameter(var)) {
            var_offset = getParamOffset(mi, param2off);
        } else if (isArgument(var)) {
            var_offset = computeArgVarOffset(mi, arg2off);
        } else {
            var_offset = computeVarOffset(mi, var2off);
        }
        setValueViaMICode(mi, var_offset);

        offset = (TMWORD)xcom::ceil_align(offset, getCodeAlign());
        MI_pc(mi) = offset;
        offset += mi->getWordBufLen();
    }
}


void RelocMgr::computeCodeOffset(MOD MIList & milst,
                                 OUT Label2Offset & lab2off)
{
    MIListIter it;
    UINT mic = 0;
    TMWORD offset = 0;
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it), mic++) {
        if (mi->getCode() == MI_label) {
            ASSERT0(MI_lab(mi));
            lab2off.set(MI_lab(mi), offset);
            continue;
        }
        setCodeAlign(mi->getWordBufLen());
        offset = (TMWORD)xcom::ceil_align(offset, getCodeAlign());
        MI_pc(mi) = offset;
        offset += mi->getWordBufLen();
    }
}


void RelocMgr::perform(MOD MIList & milst)
{
    Var2Offset var2off(getDataAlign(), m_tm);
    Label2Offset lab2off;
    computeCodeOffset(milst, lab2off);
    computeDataOffset(milst, var2off, lab2off);
}
//END RelocMgr

} //namespace
