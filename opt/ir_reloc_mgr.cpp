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


Var const* IRRelocMgr::getStackVar(IR * ir, OUT IR ** ir_has_var) const
{
    ASSERT0(ir);
    ConstIRIter irit;
    for (IR const* inner_ir = iterInitC(ir, irit); inner_ir != nullptr;
        inner_ir = iterNextC(irit)) {
        if (!inner_ir->is_lda() && !inner_ir->isMemRef())
        { continue; }

        if (!inner_ir->hasIdinfo())
        { continue; }

        Var const* var = inner_ir->getIdinfo();

        if (!var->is_local()) { continue; }
        if (var->is_unallocable() && var->getStorageSpace() != SS_STACK) {
            continue;
        }

        if (ir->is_call()) { continue; }

        *ir_has_var = const_cast<IR*>(inner_ir);
        return var;
    }
    return nullptr;
}


IR * IRRelocMgr::constructAddressIROfLocalVar(
    OUT IRList & irlist, Var const* var, HOST_INT offset)
{
    ASSERT0(var);

    Type const* tp_u64 = m_tm->getU64();
    Type const* tp_reg = m_tm->getTargMachRegisterType();

    PRNO base_prno;
    if (m_pelog->isUsedFPAsSP() && var != m_pelog->getRAVar() &&
        var != m_pelog->getFPVar() && var != m_pelog->getSSVar()) {
        base_prno = m_ra->buildPrnoAndSetReg(tp_u64, m_ra->getFP());
    } else {
        base_prno = m_ra->buildPrnoAndSetReg(tp_u64, m_ra->getSP());
    }
    PRNO tmpprno = m_ra->buildPrnoAndSetReg(
        tp_u64, m_ra->getTempScalar(tp_u64));

    //If the offset does not exceed the representable range of the load
    //immediate instruction, use the base pr with the offset directly to avoid
    //generating additional offset calculation instructions.
    if (isValidOffsetOfLoadImm(offset)) {
        IR * address = computeAddressUseFPAndOffsetDirectly(
            tmpprno, base_prno, offset);
        irlist.append_tail(address);
        return m_irmgr->buildPRdedicated(tmpprno, m_tm->getAny());
    }

    //1. Load the value of offset into the temp register.
    loadOffsetToRegister(tmpprno, (UINT64)offset, irlist);

    //2. Sign extented.
    IR * stpr0 = m_irmgr->buildStorePR(tmpprno, tp_reg,
        m_irmgr->buildBinaryOp(IR_ADD, tp_reg,
        m_irmgr->buildImmInt(0, tp_reg),
        m_irmgr->buildPRdedicated(tmpprno, tp_reg)));

    irlist.append_tail(stpr0);

    IR * temp_ir = m_irmgr->buildPRdedicated(tmpprno, tp_u64);
    IR * src_ir = m_irmgr->buildPRdedicated(base_prno, tp_u64);

    //3. Add the value of the base address register to the temp register.
    IR * stpr = m_irmgr->buildStorePR(tmpprno,
        m_tm->getTargMachRegisterType(),
        m_irmgr->buildBinaryOp(IR_ADD, tp_u64, src_ir, temp_ir));
    irlist.append_tail(stpr);

    return m_irmgr->buildPRdedicated(tmpprno, m_tm->getAny());
}


void IRRelocMgr::processStoreIR(OUT IRList & irlist, IR const* ir,
                                HOST_INT offset)
{
    ASSERT0(ir && ir->is_st());
    Type const* tp = ir->getType();
    ASSERT0(tp);
    IR const* rhs = ir->getRHS();
    ASSERT0(rhs);
    Var const* var = ir->getIdinfo();
    ASSERT0(var && var->is_local());

    //Calculate the address of local var.
    offset += ST_ofst(ir);
    IR * base = constructAddressIROfLocalVar(irlist, var, offset);
    ST_ofst(ir) = 0;
    IR * istore = m_irmgr->buildIStore(base, m_rg->dupIRTree(rhs),
        ST_ofst(ir), tp);
    istore->setAligned(ir->isAligned());
    irlist.append_tail(istore);
    if (xoc::g_debug) { copyDbx(istore, ir, m_rg); }
}


void IRRelocMgr::processLoadIR(OUT IRList & irlist, IR const* ir,
                               HOST_INT offset)
{
    ASSERT0(ir && ir->is_stpr() && ir->getRHS()->is_ld());
    ASSERT0(ir->getRHS()->hasIdinfo() && ir->getRHS()->getIdinfo()->is_local());

    offset += LD_ofst(ir->getRHS());
    IR * base = constructAddressIROfLocalVar(irlist,
        ir->getRHS()->getIdinfo(), offset);

    //Build IR_ILD.
    LD_ofst(ir->getRHS()) = 0;
    IR * ild = m_irmgr->buildILoad(base,
        LD_ofst(ir->getRHS()), ir->getType());

    ild->setAligned(ir->getRHS()->isAligned());

    IR * stpr1 = m_irmgr->buildStorePR(ir->getPrno(), ir->getType(), ild);

    irlist.append_tail(stpr1);
    if (xoc::g_debug) {
        copyDbx(stpr1, ir, m_rg);
    }
}


void IRRelocMgr::processLdaIR(OUT IRList & irlist, IR const* ir,
                              HOST_INT offset)
{
    ASSERT0(ir && ir->is_stpr() && ir->getRHS()->is_lda());
    ASSERT0(ir->getRHS()->hasIdinfo());

    IR * addr = constructAddressIROfLocalVar(irlist,
        ir->getRHS()->getIdinfo(), offset);

    Type const* ty = m_tm->getPointerType(1);

    IR * stpr = m_irmgr->buildMove(ir->getPrno(), addr->getPrno(), ty);

    irlist.append_tail(stpr);
    if (xoc::g_debug) {
        copyDbx(stpr, ir, m_rg);
    }
}


void IRRelocMgr::processIR(OUT IRList & irlist, IR * ir,
                           HOST_INT offset)
{
    ASSERT0(ir);
    //Generate irlist for irs when offset is outside the range of
    //the immediate number of the corresponding machine instruction
    //of ir.
    if (ir->is_st()) {
        return processStoreIR(irlist, ir, offset);
    }
    if (ir->is_stpr() && ir->getRHS()->is_ld()) {
        return processLoadIR(irlist, ir, offset);
    }
    if (ir->is_stpr() && ir->getRHS()->is_lda()) {
        return processLdaIR(irlist, ir, offset);
    }
    //TODO: If a new IR appears, can process it here.
    UNREACHABLE();
}


bool IRRelocMgr::isValidOffset(HOST_INT offset, Var const* var,
                               IR * ir, IR * inner_ir) const
{
    HOST_INT check_offset = (m_pelog->isUsedFPAsSP() &&
        !m_var2offset->isArgument(var) &&
        !m_var2offset->isParameter(var) &&
        !m_var2offset->isSpillVarInEntryBB(var)) ? -offset : offset;
    return !hasOffsetExceedImmRange(ir, inner_ir, check_offset);
}


bool IRRelocMgr::verifyLocalVarOffset() const
{
    BBList const* bblist = m_rg->getBBList();
    BBListIter bbit;

    //Traverse to find each local variable contained in each IR
    //after relocation. By now the ceil aligned stack offset of each local
    //variable should be in the immediate range of the MI corresponding to IR.
    for (IRBB * bb = bblist->get_head(&bbit); bb != nullptr;
         bb = bblist->get_next(&bbit)) {
        BBIRListIter bbirit;
        BBIRList & irlst = bb->getIRList();
        for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
             ir = irlst.get_next(&bbirit)) {
            IR * inner_ir = nullptr;
            Var const* var = getStackVar(ir, &inner_ir);
            if (var == nullptr || var->is_func()) {
                continue;
            }
            ASSERT0(inner_ir && inner_ir->getType());

            HOST_INT offset = xcom::ceil_align(
                m_var2offset->computeVarOffset(var),
                m_tm->getByteSize(inner_ir->getType()));

            //The offset should be valid, which means
            //it is in the immediate range of the MI corresponding to IR.
            ASSERT0(isValidOffset(offset, var, ir, inner_ir));
        }
    }
    return true;
}


void IRRelocMgr::performRelocForIR(IR * ir, OUT IRList & new_ir_list)
{
    ASSERT0(ir && ir->is_stmt());
    if (xoc::g_debug && ir->getCode() == IR_CFI_DEF_CFA) {
        xoc::MCDwarfMgr * dm = m_rg->getRegionMgr()->getDwarfMgr();
        IR const * rhs0 = CFI_CFA_KID(ir, 0);
        ASSERT0(rhs0->getCode() == IR_CONST);
        UINT reg_num = (UINT)((CConst*)rhs0)->getInt();
        IR const * rhs1 = CFI_CFA_KID(ir, 1);
        ASSERT0(rhs1->getCode() == IR_CONST);
        INT cfa_offset = (INT)((CConst*)rhs1)->getInt();

        //Update the current CFA.
        dm->updateCfa(reg_num, cfa_offset);
    }
    if (canResetArgSpace(ir)) {
        m_var2offset->resetArgSpaceOffset();
    }

    IR * inner_ir = nullptr;
    Var const* var = getStackVar(ir, &inner_ir);
    if (var == nullptr || var->is_func()) {
        new_ir_list.append_tail(ir);
        return;
    }

    //[BUG FIX] For operations that access stack variables, the offset
    //of the stack variable needs to be rounded up according to the
    //type of current operation.
    //[BUF FIX] Only use type of IR that directly operates the current
    //variable to prevent getting sizes for incorrect types.
    ASSERT0(inner_ir && inner_ir->getType());
    HOST_INT offset = xcom::ceil_align(
        m_var2offset->computeVarOffset(var),
        m_tm->getByteSize(inner_ir->getType()));

    //If debugging is enabled,
    //we need to record the current stack position of the variable
    //and encode it relative to the CFA.
    //Currently, we are using the fp encoding,
    //so it will definitely be a negative number.
    if (xoc::g_debug) {
        xoc::MCDwarfMgr * dm = m_rg->getRegionMgr()->getDwarfMgr();
        dm->setVarFinalValue(offset, var,
            (UINT)xgen::tmMapReg2TMWORD(m_var2offset->getRa()->getFP()),
            m_rg);
    }
    if (isValidOffset(offset, var, ir, inner_ir)) {
        new_ir_list.append_tail(ir);
        return;
    }

    //Use the stack variable offset directly, and the offset alignment
    //is completed by the mi_reloc stage.
    processIR(new_ir_list, ir, m_var2offset->computeVarOffset(var));
}


void IRRelocMgr::performRelocForBB(IRBB * bb)
{
    BBIRListIter bbirit;
    BBIRList & irlst = bb->getIRList();
    IRList new_ir_list;
    for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
         ir = irlst.get_next(&bbirit)) {
        performRelocForIR(ir, new_ir_list);
    }
    bb->getIRList().clean();
    for (IR * ir = new_ir_list.get_head(&bbirit); ir != nullptr;
        ir = new_ir_list.get_next(&bbirit)) {
        bb->getIRList().append_tail(ir);
    }
}


void IRRelocMgr::performReloc()
{
    BBList const* bblist = m_rg->getBBList();
    BBListIter bbit;

    //First, calculate the parameters and arguments. They should not be
    //calculated in the order in the IR LIST, because they are placed on
    //the stack in sequence in ArgPasser.
    //If the layout of arguments is computed in the IR lexicographic order,
    //while the IR is highly likely to be scheduled by the IR-Insched pass,
    //the result of the arguments offset on the stack might be incorrect.
    m_var2offset->computeParamOffset();
    m_var2offset->computeArgVarOffset();
    for (IRBB * bb = bblist->get_head(&bbit); bb != nullptr;
         bb = bblist->get_next(&bbit)) {
        performRelocForBB(bb);
    }
}


bool IRRelocMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(m_rg, "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    if (m_rg->getIRList() != nullptr) {
        dumpIRList(m_rg->getIRList(), m_rg);
    } else if (m_rg->getBBList() != nullptr) {
        dumpBBList(m_rg->getBBList(), m_rg);
    }
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool IRRelocMgr::perform(OptCtx & oc)
{
    performReloc();
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpIRReloc()) {
        dump();
    }

    ASSERT0(verifyLocalVarOffset());
    ASSERT0(m_rg->getCFG()->verifyRPO(oc));
    ASSERT0(m_rg->getCFG()->verifyLoopInfo(oc));
    ASSERT0(m_rg->getCFG()->verifyDomAndPdom(oc));

    return true;
}

} //namespace
