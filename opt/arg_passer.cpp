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

static bool isLoadFormalParam(IR * ir)
{
    return (ir->is_stpr() && ir->getRHS()->is_ld() &&
            ir->getRHS()->getIdinfo()->is_formal_param());
}


static bool isStoreFormalParam(IR * ir)
{
    return (ir->is_st() && ir->getIdinfo()->is_formal_param());
}


static bool isLoadFormalParamAddress(IR * ir)
{
    return ir->is_stpr() && ir->hasRHS() && ir->getRHS()->is_lda()
        && ir->getRHS()->getIdinfo()->is_formal_param();
}


//Update the param list of call with new param irs.
static void updateCallParamList(IR * ir, IR *& paramlist)
{
    CALL_param_list(ir) = paramlist;
    while (paramlist != nullptr) {
        IR_parent(paramlist) = ir;
        paramlist = IR_next(paramlist);
    }
}


ArgPasser::ArgPasser(Region * rg) : Pass(rg)
{
    m_avail_param_scalar.init();
    m_avail_param_vector.init();
    m_avail_ret_scalar.init();
    m_avail_ret_vector.init();

    m_irmgr = m_rg->getIRMgr();
    m_tm = m_rg->getTypeMgr();
    m_max_argument_size = 0;
    m_dystack_impl = nullptr;
    m_entry_param = nullptr;
}


void ArgPasser::passRetViaRegister(MOD IRBB * irbb, IR const* ir,
    MOD IRList & irlist, xgen::Reg reg)
{
    Type const* ty = ir->getType()->is_mc() ?
        m_tm->getTargMachRegisterType() : ir->getType();

    //Bind the return value register to a temporary prno to
    //avoid the return value register using in non-call irs.
    //e.g:
    //CODE:
    //  .reg $1;
    //  call test($0) -> $1;
    //  mov.u32 $1, 1;
    //If [m_lsra->getDedicatedMgr().add(ir->getPrno(), reg);]
    //  $2:u64 = call 'func'  id:8 attachinfo:Dbx
    //     $1:u64 param0 id:6
    //     $2:u64 param1 id:7
    //  stpr $2:u32 id:11 attachinfo:Dbx
    //     intconst:u32 1|0x1 id:10
    //The return value register is written by the 'mov'.
    //If [m_lsra->getDedicatedMgr().add(temp, reg);]
    //  stpr $6:u64 id:19
    //     $2:u64 id:18
    //  $7:u64 = call 'func'  id:8 attachinfo:Dbx
    //     $5:u64 param0 id:17
    //     $6:u64 param1 id:20
    //  stpr $2:u64 id:22
    //     $7:u64 id:21
    //  stpr $2:u32 id:11 attachinfo:Dbx
    //     intconst:u32 1|0x1 id:10
    //The return value register is not written by the 'mov'.
    xoc::PRNO tmpprno = m_irmgr->buildPrno(ty);
    m_lsra->getDedicatedMgr().add(tmpprno, reg);

    IR * stpr = m_irmgr->buildStorePR(ir->getPrno(), ty,
        m_irmgr->buildPRdedicated(tmpprno, ty));

    CALL_prno(ir) = tmpprno;

    stpr->setBB(irbb);
    irlist.append_tail(stpr);
}


void ArgPasser::passRetViaStack(IR const* ir)
{
    return;
}


void ArgPasser::buildLoadForGlobalOrSpmVar(OUT IRList & irlist, PRNO tgtprno,
                                           Var * var, Type const* type,
                                           TMWORD ofst)
{
    ASSERT0(tgtprno != PRNO_UNDEF && var && type);
    ASSERT0(var->getStorageSpace() == SS_GLOBAL ||
            var->getStorageSpace() == SS_SPM ||
            var->getStorageSpace() == SS_READONLY);

    PRNO lda_prno = m_irmgr->buildPrno(var->getType());
    IR * lda = m_irmgr->buildStorePR(lda_prno, m_tm->getTargMachRegisterType(),
                                     m_irmgr->buildLda(var));
    irlist.append_tail(lda);

    IR * ld = m_irmgr->buildILoad(m_irmgr->buildPRdedicated(lda_prno,
        m_tm->getAny()), ofst, type);
    ld->setAligned(true);
    IR * stpr = m_irmgr->buildStorePR(tgtprno, type, ld);
    irlist.append_tail(stpr);
}


void ArgPasser::passArgViaRegister(OUT IRList & irlist,
                                   OUT IR ** paramlist,
                                   IR const* param, xgen::Reg reg)
{
    IRMgr * irmgr = m_lsra->getRegion()->getIRMgr();
    xoc::PRNO tmpprno = irmgr->buildPrno(param->getType());
    m_lsra->getDedicatedMgr().add(tmpprno, reg); // bind register to param

    Type const* src_tp = param->getType();
    Type const* dst_tp = param->getType();

    xoc::PRNO prno;
    if (param->is_ld()) {
        //pass stack var.
        prno = irmgr->buildPrno(src_tp);
        ASSERT0(param->hasIdinfo());
        if (param->getIdinfo()->getStorageSpace() == SS_GLOBAL ||
            param->getIdinfo()->getStorageSpace() == SS_SPM ||
            param->getIdinfo()->getStorageSpace() == SS_READONLY) {
            buildLoadForGlobalOrSpmVar(irlist, prno, param->getIdinfo(),
                                       src_tp);
        } else {
            IR * newload = irmgr->buildLoad(param->getIdinfo(), src_tp);
            IR * ir = irmgr->buildStorePR(prno, src_tp, newload);
            irlist.append_tail(ir);
        }
    } else if (param->is_lda()) {
        //pass address
        prno = m_irmgr->buildPrno(m_tm->getTargMachRegisterType());
        IR * ir = m_irmgr->buildStorePR(prno,
            m_tm->getTargMachRegisterType(),
            irmgr->buildLda(param->getIdinfo()));
        irlist.append_tail(ir);
    } else {
        //pass reg.
        ASSERT0(param->is_pr());
        prno = param->getPrno();
        //Get the type of the parameter register used.
        dst_tp = getParamRegisterType(src_tp, reg);
    }

    IR * stpr = m_irmgr->buildStorePR(tmpprno,
        dst_tp, m_irmgr->buildPRdedicated(prno, src_tp));

    irlist.append_tail(stpr);

    //Generate new param ir and record it.
    xcom::add_next(paramlist, m_irmgr->buildPRdedicated(tmpprno, src_tp));
}


void ArgPasser::passArgViaStack(OUT IRList & irlist,
    OUT IR ** paramlist, IR const* ir, MOD UINT & arg_size,
    MOD UINT & alignment)
{
    PRNO prno;
    if (ir->is_ld()) {
        //Pass stack var.
        prno = m_irmgr->buildPrno(ir->getType());
        IR * newload = m_irmgr->buildLoad(ir->getIdinfo(), ir->getType());
        IR * newir = m_irmgr->buildStorePR(prno, ir->getType(), newload);
        irlist.append_tail(newir);
    } else if (ir->is_lda()) {
        //Pass address
        prno = m_irmgr->buildPrno(m_tm->getTargMachRegisterType());
        ASSERT0(ir->hasIdinfo());
        IR * newir = m_irmgr->buildStorePR(prno,
            m_tm->getTargMachRegisterType(),
            m_irmgr->buildLda(ir->getIdinfo()));
        irlist.append_tail(newir);
    } else {
        ASSERT0(ir->is_pr());
        //Pass register.
        //A new prno should be built here bacause when a register is passed
        //as two params, prno and spill var correspond one-to-one.
        //e.g: call func ($0, $0);
        prno = m_irmgr->buildPrno(ir->getType());
        IR * newir = m_irmgr->buildStorePR(prno,
            ir->getType(),
            m_irmgr->buildPRdedicated(ir->getPrno(), ir->getType()));
        irlist.append_tail(newir);
    }

    IR * spill = m_lsra->buildSpill(prno, ir->getType());

    ASSERT0(spill->hasIdinfo());
    VAR_align(spill->getIdinfo()) = m_tm->getByteSize(ir->getType()) >
        PARAM_ALIGNMENT ? m_tm->getByteSize(ir->getType()) : PARAM_ALIGNMENT;

    irlist.append_tail(spill);
    m_arg_stack_list.append_tail(spill->getIdinfo());

    //Update total arg_size and alignment for each function call.
    updateArgSizeAndAlignment(arg_size, alignment, spill->getIdinfo());

    //Generate new param ir and record it.
    xcom::add_next(paramlist,
        m_irmgr->buildLoad(spill->getIdinfo(), spill->getType()));

    return;
}


void ArgPasser::processRetValue(MOD IRBB * irbb,
    OUT IRList & irlist, IR const* ir)
{
    if (!ir->hasReturnValue()) { return; }

    xgen::Reg reg = pickRetReg(ir);

    return reg == REG_UNDEF ? passRetViaStack(ir) :
        passRetViaRegister(irbb, ir, irlist, reg);
}


Type const* ArgPasser::getCurrentDataType(UINT align)
{
    UINT trailing_zeros = countTrailingZero(align);
    //e.g: 0b10, align by 2 bytes.
    if (trailing_zeros == 1) {
        return m_tm->getU16();
    }
    //e.g: 0b100, align by 4 bytes.
    if (trailing_zeros == 2) {
        return m_tm->getU32();
    }
    //e.g: 0b1000/0b10000, align by 8/16 bytes.
    if (trailing_zeros == 3 || trailing_zeros == 4) {
        return m_tm->getU64();
    }
    //e.g: 0b100000, align by 32 bytes.
    if (trailing_zeros == 5) {
        return m_tm->getVectorType(ELEM_NUM_OF_16_ELEM_VECTOR_TYPE, D_F16);
    }
    //e.g: 0b1000000, align by 64 bytes.
    if (trailing_zeros == 6) {
        return m_tm->getVectorType(ELEM_NUM_OF_16_ELEM_VECTOR_TYPE, D_U32);
    }

    //e.g: 0b1, align by 1 byte.
    ASSERT0(trailing_zeros == 0);
    return m_tm->getU8();
}


IR * ArgPasser::copyMCWithMemcpy(OUT IRList & irlist, IR const* ir,
                                 OUT UINT & arg_size, MOD UINT & alignment)
{
    ASSERT0(ir && ir->is_mc() && ir->hasIdinfo());
    xcom::StrBuf name(64);
    Var * var = m_rg->getVarMgr()->registerVar(m_lsra->
        genFuncLevelNewVarName(name), ir->getType(), VAR_align(ir->getIdinfo()),
        VAR_LOCAL);
    m_arg_stack_list.append_tail(var);

    updateArgSizeAndAlignment(arg_size, alignment, var);

    buildMemcpy(irlist, ir->getIdinfo(), var, ir->getTypeSize(m_tm));

    return m_irmgr->buildLda(var);
}


IR * ArgPasser::copyMCAndReturnAddress(OUT IRList & irlist, IR const* ir,
                                       OUT UINT & arg_size,
                                       MOD UINT & alignment)
{
    ASSERT0(ir && ir->is_mc() && ir->hasIdinfo());

    if (ir->getTypeSize(m_tm) / VAR_align(ir->getIdinfo()) > getMaxCopyNum()) {
        return copyMCWithMemcpy(irlist, ir, arg_size, alignment);
    }

    UINT sz = ir->getTypeSize(m_tm);
    UINT offset = 0;
    IR * start_address = nullptr;
    //Move the argument from localspace to argument space.
    while (offset < sz)
    {
        //Get partital data type.
        Type const* type = getCurrentDataType(ir->getIdinfo()->get_align());
        PRNO prno = m_irmgr->buildPrno(type);
        UINT type_size = m_tm->getByteSize(type);

        if (ir->getIdinfo()->getStorageSpace() == SS_GLOBAL ||
            ir->getIdinfo()->getStorageSpace() == SS_SPM ||
            ir->getIdinfo()->getStorageSpace() == SS_READONLY) {
            //Load partital value from global/spm space.
            buildLoadForGlobalOrSpmVar(irlist, prno, ir->getIdinfo(),
                                       type, offset);
        } else {
            //Load partital value from localspace.
            IR * newload = m_irmgr->buildLoad(ir->getIdinfo(), offset, type);
            newload->setAligned(true);
            IR * newir = m_irmgr->buildStorePR(prno, type, newload);
            irlist.append_tail(newir);
        }

        offset += type_size;

        //Store partital value to argument space.
        IR * spill = m_lsra->buildSpill(prno, type);
        irlist.append_tail(spill);
        m_arg_stack_list.append_tail(spill->getIdinfo());

        //Update total arg_size and alignment for each function call.
        updateArgSizeAndAlignment(arg_size, alignment, spill->getIdinfo());

        //Get start address of this argument.
        if (start_address == nullptr) {
            start_address = m_irmgr->buildLda(spill->getIdinfo());
        }
    }

    ASSERT0(start_address != nullptr);
    return start_address;
}


IR * ArgPasser::processArgumentWithMCType(IR const* ir, IR * param,
    OUT IRList & ir_list, MOD UINT & arg_size, MOD UINT & alignment)
{
    ASSERT0(param->is_ld() && param->is_mc());
    //Param is argument.
    return copyMCAndReturnAddress(ir_list, param, arg_size, alignment);
}


void ArgPasser::processArgument(MOD IRBB * irbb,
                                OUT IRList & irlist, IR const* ir,
                                MOD UINT & arg_size, MOD UINT & alignment)
{
    IRList tmp_ir_list;
    IR * new_param_list = nullptr;

    for (IR * param = CALL_param_list(ir); param != nullptr;
         param = param->get_next()) {
        IR * curparam = param;
        if (curparam->is_mc()) {
            //Arguments with mc type need to be passed by memory, including
            //case that the size of argument is smaller than the register size.
            curparam = processArgumentWithMCType(ir, curparam,
                tmp_ir_list, arg_size, alignment);
        }

        xgen::Reg reg = pickParamReg(curparam);
        if (reg == REG_UNDEF) {
            passArgViaStack(tmp_ir_list, &new_param_list, curparam,
                            arg_size, alignment);
        } else {
            passArgViaRegister(tmp_ir_list, &new_param_list, curparam, reg);
        }
    }

    //Update the param list of call with new param irs.
    updateCallParamList((IR*)ir, new_param_list);

    //Update max argument size.
    m_max_argument_size = arg_size > m_max_argument_size ?
        arg_size : m_max_argument_size;

    for (IR * ir = tmp_ir_list.get_head(); ir != nullptr;
         ir = tmp_ir_list.get_next()) {
        ir->setBB(irbb);
        irlist.append_tail(ir);
    }
}


void ArgPasser::getFormalParamViaRegister(OUT IRList & irlist, IR * ir)
{
    ASSERT0(ir);
    //Convert
    //  stpr $0
    //    ld.param
    //To:
    //  stpr $0
    //    $param_reg
    Var const* v = ir->getRHS()->getIdinfo();
    ASSERT0(v && m_var2prno.find(v));
    PRNO prno = m_var2prno.get(v);
    ASSERT0(prno != PRNO_UNDEF);

    IR * pr = m_irmgr->buildPRdedicated(prno,
        ir->getRHS()->getIdinfo()->getType());
    ir->setRHS(pr);
    irlist.append_tail(ir);
}


void ArgPasser::processFormalParamWithMCType(OUT IRList & irlist, IR * ir)
{
    ASSERT0(ir->is_stpr() && ir->getRHS()->is_ld());

    Var const* v = ir->getRHS()->getIdinfo();

    ASSERT0(v->is_mc());

    //CASE1: The address of the mc type variable has been calculated.
    //The address of the mc type variable has been calculated in
    //appendIRToGetStartAddressOfMCInEntryFunc and stored in $address.
    //Replace:
    //load.param.u64 .aligned $1, [mystruct];
    //load.param.u64 .aligned $2, [mystruct + 8];
    //With:
    //load.param.u64 .aligned $1, [$address];
    //load.param.u64 .aligned $2, [$address + 8];
    if (m_var2prno.find(v)) {
        PRNO prno = m_var2prno.get(ir->getRHS()->getIdinfo());
        IR * ild = m_irmgr->buildILoad(m_irmgr->buildPRdedicated(prno,
            m_tm->getPointerType(1)), LD_ofst(ir->getRHS()),
            ir->getRHS()->getType());
        ild->setAligned(true);
        ir->setRHS(ild);
        irlist.append_tail(ir);
        return;
    }

    //CASE2: The address of the mc type variable is not calculated.
    //The address of mc should be calculated later in reloc_mgr,
    //these irs can be inserted directly into the irlist and cannot be replaced.
    //load.param.u64 .aligned $1, [mystruct];
    //load.param.u64 .aligned $2, [mystruct + 8];
    irlist.append_tail(ir);
    return;
}


void ArgPasser::processLoadFormalParam(MOD IRBB * irbb,
                                       OUT IRList & irlist, IR * ir)
{
    ASSERT0(ir && ir->is_stpr() && ir->getRHS()->is_ld());

    Var const* v = ir->getRHS()->getIdinfo();
    ASSERT0(v);

    if (v->is_mc()) {
        return processFormalParamWithMCType(irlist, ir);
    }

    if (m_var2prno.find(v)) {
        return getFormalParamViaRegister(irlist, ir);
    }
    irlist.append_tail(ir);
}


void ArgPasser::processFormalReturnParam(OUT IRList & irlist, IR * ir)
{
    xgen::Reg reg = pickRetReg(ir);

    ASSERT0(ir->is_st() && ir->getRHS()->is_pr());
    ASSERT0(ir->hasIdinfo());

    if (reg != REG_UNDEF && !ir->getIdinfo()->is_mc()) {
        xoc::PRNO prno = m_irmgr->buildPrno(ir->getType());
        m_lsra->getDedicatedMgr().add(prno, reg);
        IR * newir = m_irmgr->buildStorePR(prno, ir->getType(), ir->getRHS());
        irlist.append_tail(newir);
        return;
    }

    if (reg == REG_UNDEF && !ir->getIdinfo()->is_mc()) {
        irlist.append_tail(ir);
        return;
    }

    //When the type of retval is B<n>, there are two ways to pass it.
    //The first is: memcpy.param.stack [retval], [stackval], size;
    //The second is: store.param.u64 [retval], $0;
    //               store.param.u64 [retval + 8], $1;
    //The first case is handled in function processIRUsedFormalParam.
    //Here is to deal with the second case, get the address of the retval
    //and then construct an IR_IST.
    ASSERT0(ir->getIdinfo()->is_mc());
    Var const* v = ir->getIdinfo();
    ASSERT0(v);
    bool is_find = m_var2prno.find(v);
    if (is_find) {
        xoc::PRNO prno = m_var2prno.get(v);
        ASSERT0(prno != PRNO_UNDEF);
        ASSERT0(ir->hasRHS() && ir->getRHS()->is_pr() && ir->getType());
        IR * istore = m_irmgr->buildIStore(m_irmgr->buildPRdedicated(prno,
            m_tm->getPointerType(1)),
            m_irmgr->buildPRdedicated(ir->getRHS()->getPrno(),
            ir->getType()), ST_ofst(ir), ir->getType());
        irlist.append_tail(istore);
        return;
    }

    xoc::PRNO prno = m_irmgr->buildPrno(ir->getType());
    IR * ld = m_irmgr->buildLoad((Var*)v, ir->getType());
    IR * newir = m_irmgr->buildStorePR(prno, ir->getType(), ld);
    irlist.append_tail(newir);
    ASSERT0(ir->hasRHS() && ir->getRHS()->is_pr() && ir->getType());
    IR * istore = m_irmgr->buildIStore(m_irmgr->buildPRdedicated(prno,
        m_tm->getPointerType(1)),
        m_irmgr->buildPRdedicated(ir->getRHS()->getPrno(),
        ir->getType()), ST_ofst(ir), ir->getType());
    irlist.append_tail(istore);

    return;
}


IR * ArgPasser::processFormalParamAddress(OUT IRList & irlist, IR * ir)
{
    ASSERT0(ir && ir->is_stpr() && ir->getRHS()->is_lda());

    Var const* v = ir->getRHS()->getIdinfo();

    bool is_find = m_var2prno.find(v);

    if (is_find) {
        xoc::PRNO prno = m_var2prno.get(v);
        ir->setRHS(m_irmgr->buildPRdedicated(prno, ir->getType()));
    } else {
        IR * lda = m_irmgr->buildLda(ir->getRHS()->getIdinfo());
        ir->setRHS(lda);
    }
    return ir;
}


void ArgPasser::initRegBindInfo()
{
    m_avail_param_scalar.clean();
    m_avail_param_vector.clean();
    m_avail_ret_scalar.clean();
    m_avail_ret_vector.clean();
    xgen::RegSet const* rs = getParamRegSetScalar();
    if (rs != nullptr) {
        m_avail_param_scalar.copy(*rs);
    }
    rs = getParamRegSetVector();
    if (rs != nullptr) {
        m_avail_param_vector.copy(*rs);
    }
    rs = getRetRegSetScalar();
    if (rs != nullptr) {
        m_avail_ret_scalar.copy(*rs);
    }
    rs = getRetRegSetVector();
    if (rs != nullptr) {
        m_avail_ret_vector.copy(*rs);
    }
}


void ArgPasser::appendIRToGetStartAddressOfParamsInEntryFunc(OUT PRNO & prno)
{
    IRBB * bb = m_rg->getCFG()->getEntry();

    PRNO pr_dedicated = m_lsra->buildPrnoDedicated(
        m_tm->getTargMachRegisterType(), m_lsra->getParamScalarStart());

    prno = m_irmgr->buildPrno(m_tm->getTargMachRegisterType());
    IR * ir = m_irmgr->buildStorePR(prno, m_tm->getTargMachRegisterType(),
        m_irmgr->buildPRdedicated(pr_dedicated,
        m_tm->getTargMachRegisterType()));

    bb->getIRList().append_tail_ex(ir);
}


//Get the address of param of type mc.
void ArgPasser::appendIRToGetStartAddressOfMCInEntryFunc(IRBB * bb,
    PRNO start_address_prno, OUT UINT & offset, Var const* v)
{
    PRNO param_prno = m_irmgr->buildPrno(
        m_tm->getTargMachRegisterType());

    //$start_address: the start address of total parameters.
    //The address of param = $start_address + current_offset.
    //For example:
    // ------------------ current_offset = 0
    // |  param.s32 p0  |
    // ------------------ current_offset = 8
    // |  param.u64 p1  |
    // ------------------ current_offset = 16
    // |  param.B<24> p2| address(p2) = $start_address + current_offset.
    // ------------------
    IR * stpr = m_irmgr->buildStorePR(param_prno, m_tm->getU64(),
        m_irmgr->buildBinaryOp(IR_ADD, m_tm->getU64(),
        m_irmgr->buildPRdedicated(start_address_prno,
        m_tm->getTargMachRegisterType()),
        m_irmgr->buildImmInt(offset, m_tm->getU64())));

    bb->getIRList().append_tail_ex(stpr);

    m_var2prno.set(v, param_prno);
    offset += m_tm->getByteSize(v->getType());
}


//For entry function, host puts all parameters into memory and
//passes the first address to the slave through a register.
//For example:
//.entry doing0(.param.u32 p0, .param.b<16> p1)
//{
//    .reg $0;
//    load.param.u32 .aligned $0, [p0];
//    .reg $1;
//    load.param.u64 .aligned $1, [p1];
//    .reg $2;
//    load.param.u64 .aligned $2, [p1 + 8];
//}
//
// The steps to get parameters in entry function are:
// step1: Get the start address of parameters and stores in $start_address_prno.
// step2: Replace "load.param.u32 .aligned $0, [p0];" with:
//        (1) load.param.u32 .aligned $tmp0, [$start_address_prno + 0];
//        (2) mov.u32 $0, $tmp0;
// step3: Get the start address of param of type mc and
//        stores in $start_address_mc_prno.
// step4: Replace "load.param.u64 .aligned $1, [p1];" with:
//        load.param.u64 .aligned $1, [$start_address_mc_prno];
// step5: Replace "load.param.u64 .aligned $2, [p1 + 8];" with:
//        load.param.u64 .aligned $2 [$start_address_mc_prno + 8];
void ArgPasser::processEntryFunction(List<xoc::Var const*> & param_list)
{
    IRBB * bb = m_rg->getCFG()->getEntry();
    ASSERT0(bb);

    //Get the start address of parameters in kernel function.
    PRNO start_address_prno;
    appendIRToGetStartAddressOfParamsInEntryFunc(start_address_prno);
    ASSERT0(start_address_prno != PRNO_UNDEF);

    UINT offset = 0;

    for (xoc::Var const* v = param_list.get_head();
         v != nullptr; v = param_list.get_next()) {
        ASSERT0(!m_var2prno.find(v));
        offset = (UINT)xcom::ceil_align(offset, VAR_align(v));
        if (v->getType()->is_mc()) {
            // Get the start address of param of type mc
            // in kernel function.
            appendIRToGetStartAddressOfMCInEntryFunc(bb,
                start_address_prno, offset, v);
            continue;
        }

        Type const* type = v->getType();
        IR * ild = m_irmgr->buildILoad(m_irmgr->buildPRdedicated(
            start_address_prno, m_tm->getPointerType(1)), offset, type);
        ild->setAligned(true);

        PRNO param_prno = m_irmgr->buildPrno(type);
        IR * stpr = m_irmgr->buildStorePR(param_prno, type, ild);

        //We only need to record the first parameter, and subsequent spill irs
        //will be inserted before this parameter.
        if (m_entry_param == nullptr) { m_entry_param = stpr; }

        bb->getIRList().append_tail_ex(stpr);

        m_var2prno.set(v, param_prno);

        offset += m_tm->getByteSize(v->getType());
    }
}


void ArgPasser::preProcessFormalParam(OptCtx & oc)
{
    initRegBindInfo();

    List<xoc::Var const*> param_list;
    m_rg->findFormalParam(param_list, true);

    if (m_rg->getRegionVar() != nullptr &&
        m_rg->getRegionVar()->is_entry()) {
        return processEntryFunction(param_list);
    }

    IRBB * entry_bb = m_rg->getCFG()->getEntry();
    ASSERT0(entry_bb);
    IRBB * new_bb = m_rg->getCFG()->insertFallThroughBBAfter(entry_bb, &oc);
    ASSERT0(new_bb);

    for (xoc::Var const* v = param_list.get_head();
         v != nullptr; v = param_list.get_next()) {
        ASSERT0(!m_var2prno.find(v));
        Reg reg = pickParamReg(v);
        if (reg == REG_UNDEF) {
            m_param_stack_list.append_tail(v);
            continue;
        }
        if (v->is_mc()) {
            m_param_stack_list.append_tail(v);
        }

        //For variable of type mc, the address is
        //passed directly. Thus it's type is pointer.
        Type const* type = v->getType()->is_mc() ?
            m_tm->getPointerType(1) : v->getType();

        PRNO tgt_prno = m_irmgr->buildPrno(type);
        PRNO src_prno = m_lsra->buildPrnoDedicated(type, reg);

        //NOTE: The generated IR needs to be inserted after the entry bb,
        //otherwise it will cause the register to be used incorrectly.
        //For example, tgt_prno is assigned a callee save register, this
        //register will be saved in the entrybb, and if the IR is also
        //inserted into the entrybb, it may be inserted before the callee
        //save. Therefore, in order to ensure the correctness of the callee
        //save registers, these newly generated IRs are inserted into a new
        //bb, and this new bb is inserted after the entrybb.
        IR * ir = m_irmgr->buildStorePR(tgt_prno, type, m_irmgr->
            buildPRdedicated(src_prno, getParamRegisterType(type, reg)));

        ir->setBB(new_bb);
        new_bb->getIRList().append_tail(ir);

        m_var2prno.set(v, tgt_prno);
    }
}


void ArgPasser::processIRUsedFormalParam(OptCtx & oc)
{
    //Traverse each IR in each BB in BBList, bind the parameter and return
    //value of the call instruction to the register.
    BBList * bblist = m_rg->getBBList();
    ASSERT0(bblist);
    BBListIter bbit;

    if (isNeedPreprocessFormalParam()) {
        preProcessFormalParam(oc);
    }

    //Process formal parameter.
    for (IRBB * bb = bblist->get_head(&bbit); bb != nullptr;
         bb = bblist->get_next(&bbit)) {

        List<IR*> new_ir_list;

        for (IR * ir = bb->getIRList().get_head(); ir != nullptr;
             ir = bb->getIRList().get_next()) {
            //load.param.u32 $0, [param0];
            if (isLoadFormalParam(ir)) {
                processLoadFormalParam(bb, new_ir_list, ir);
                continue;
            }
            //store.param.u32 [param0], $0;
            if (isStoreFormalParam(ir)) {
                processFormalReturnParam(new_ir_list, ir);
                continue;
            }
            //memcpy.param.stack [param0], [stackval], n;
            if (isLoadFormalParamAddress(ir)) {
                ir = processFormalParamAddress(new_ir_list, ir);
            }
            new_ir_list.append_tail(ir);
        }
        BB_irlist(bb).copy(new_ir_list);
    }
}


void ArgPasser::buildTargetAddressForCall(OUT IRList & irlist, IR * ir)
{
    ASSERT0(ir->isCallStmt());
    if (ir->is_call()) {

        if (!isArgPasserExternalCallNeedLda()) { return; }

        PRNO call_prno = m_lsra->buildPrnoDedicated(
            m_tm->getTargMachRegisterType(), m_lsra->getTA());
        IR * call_addr = m_irmgr->buildStorePR(call_prno,
            m_tm->getTargMachRegisterType(),
            m_irmgr->buildLda(ir->getIdinfo()));

        irlist.append_tail(call_addr);

        return;
    }

    ASSERT0(ir->is_icall());
    PRNO call_prno = m_lsra->buildPrnoDedicated(
        m_tm->getTargMachRegisterType(), m_lsra->getTA());
    IR * stpr = m_irmgr->buildStorePR(call_prno,
        m_tm->getTargMachRegisterType(), m_irmgr->
        buildPRdedicated(ICALL_callee(ir)->getPrno(),
                        m_tm->getTargMachRegisterType()));
    irlist.append_tail(stpr);
}


void ArgPasser::processCallStmt()
{
    //Traverse each IR in each BB in BBList, bind the parameter and return
    //value of the call instruction to the register.
    BBList * bblist = m_rg->getBBList();
    BBListIter bbit;

    for (IRBB * bb = bblist->get_head(&bbit); bb != nullptr;
         bb = bblist->get_next(&bbit)) {

        List<IR*> new_ir_list;
        List<IR*> ir_list_each_call;
        for (IR * ir = bb->getIRList().get_head(); ir != nullptr;
             ir = bb->getIRList().get_next()) {

            if (!ir->isCallStmt() || ir->isIntrinsicOp()) {
                new_ir_list.append_tail(ir);
                continue;
            }

            initRegBindInfo();

            UINT arg_size = 0;
            UINT alignment = STACK_ALIGNMENT;
            processArgument(bb, ir_list_each_call, ir, arg_size, alignment);

            buildTargetAddressForCall(ir_list_each_call, ir);

            ir_list_each_call.append_tail(ir);

            processRetValue(bb, ir_list_each_call, ir);

            if (m_dystack_impl->hasAllocaIR() &&
                (arg_size > 0) && g_support_alloca) {
                //The size of the sp adjustment is arg_size and
                //should aligned to alignment.
                m_dystack_impl->dynamicAdjustSPForArgument(
                    ir_list_each_call, ir, arg_size, alignment);
            }

            new_ir_list.move_tail(ir_list_each_call);
            ir_list_each_call.clean();
        }

        BB_irlist(bb).copy(new_ir_list);
    }
}


bool ArgPasser::dump() const
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


bool ArgPasser::perform(OptCtx & oc)
{
    m_lsra = (LinearScanRA*)m_rg->getPassMgr()->registerPass(
        PASS_LINEAR_SCAN_RA);
    ASSERT0(m_lsra);

    if (g_support_alloca) {
        m_dystack_impl = (DynamicStack*)m_rg->getPassMgr()->
            queryPass(PASS_DYNAMIC_STACK);
        ASSERT0(m_dystack_impl);
    }
    processIRUsedFormalParam(oc);
    processCallStmt();
    oc.setInvalidDom(); //CFG has been modified.
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpArgPasser()) {
        dump();
    }
    return true;
}
