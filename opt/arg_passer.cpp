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
    return ir->is_stpr() && ir->getRHS()->is_lda() &&
        ir->getRHS()->getIdinfo()->is_formal_param();
}


//Update the arguments list of call with new arguments irs.
static void updateCallArgList(IR * ir, IR *& arglist)
{
    CALL_arg_list(ir) = arglist;
    while (arglist != nullptr) {
        IR_parent(arglist) = ir;
        arglist = IR_next(arglist);
    }
}


//
//Start ArgPasserResMgr.
//
xcom::List<Var const*> * ArgPasserResMgr::allocListVar()
{
    xcom::List<Var const*> * list_var = new xcom::List<Var const*>();
    m_list_var_mgr.append_tail(list_var);
    return list_var;
}


void ArgPasserResMgr::deleteListVar()
{
    for (xcom::List<Var const*> * r = m_list_var_mgr.get_head();
         r != nullptr; r = m_list_var_mgr.get_next()) {
        if (r != nullptr) { delete r; }
    }
    m_list_var_mgr.destroy();
}


ArgPasserResMgr::~ArgPasserResMgr()
{
    deleteListVar();
}
//End ArgPasserResMgr.


ArgPasser::ArgPasser(Region * rg) :
    Pass(rg),
    m_arg_pass_res_mgr(new ArgPasserResMgr()),
    m_call_arg_list(m_arg_pass_res_mgr)
{
    m_avail_param_scalar.init();
    m_avail_param_vector.init();
    m_avail_ret_scalar.init();
    m_avail_ret_vector.init();

    m_irmgr = m_rg->getIRMgr();
    m_rm = m_rg->getRegionMgr();
    m_tm = m_rg->getTypeMgr();
    m_max_argument_size = 0;
    m_max_argument_align = STACK_ALIGNMENT;
    m_dystack_impl = nullptr;
    m_entry_param = nullptr;
    ASSERT0(m_arg_pass_res_mgr);
}


ArgPasser::~ArgPasser()
{
    ASSERT0(m_arg_pass_res_mgr);
    delete m_arg_pass_res_mgr;
    m_arg_pass_res_mgr = nullptr;
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
    //If [m_lsra->getPreAssignedMgr().add(ir->getPrno(), reg);]
    //  $2:u64 = call 'func'  id:8 attachinfo:Dbx
    //     $1:u64 param0 id:6
    //     $2:u64 param1 id:7
    //  stpr $2:u32 id:11 attachinfo:Dbx
    //     intconst:u32 1|0x1 id:10
    //The return value register is written by the 'mov'.
    //If [m_lsra->getPreAssignedMgr().add(temp, reg);]
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
    PRNO tmpprno = m_lsra->buildPrnoAndSetReg(ty, reg);
    IR * stpr = m_irmgr->buildMove(ir->getPrno(), tmpprno, ty);
    CALL_prno(ir) = tmpprno;
    stpr->setBB(irbb);
    irlist.append_tail(stpr);
}


void ArgPasser::buildLoadForGlobalOrSpmVar(OUT IRList & irlist, PRNO tgtprno,
                                           Var * var, Type const* type,
                                           TMWORD ofst)
{
    ASSERT0(tgtprno != PRNO_UNDEF && var && type);

    StorageSpace ss = var->getStorageSpace();
    ASSERT0(ss == SS_GLOBAL || ss == SS_SPM || ss == SS_READONLY ||
            ss == SS_PARAM);

    PRNO src_prno = PRNO_UNDEF;
    if (ss == SS_PARAM) {
        //If the variable is the same as the formal parameter variable, there
        //is no need to perform additional address loading operations, just use
        //the register corresponding to the formal parameter.
        ASSERT0(m_var2prno.find(var));
        src_prno = m_var2prno.get(var);
    } else {
        IR * load_address = m_irmgr->buildLoadAddrOfVar(var);
        irlist.append_tail(load_address);
        src_prno = load_address->getPrno();
    }

    IR * load_value = m_irmgr->buildStorePR(tgtprno, type,
        m_irmgr->buildILoad(
        m_irmgr->buildPRdedicated(src_prno, m_tm->getAny()),
        ofst, type));
    load_value->getRHS()->setAlign(true);
    irlist.append_tail(load_value);
}


void ArgPasser::passArgViaRegister(OUT IRList & irlist, OUT IR ** paramlist,
                                   IR const* param, xgen::Reg reg)
{
    // bind register to param
    xoc::PRNO tmpprno = m_lsra->buildPrnoAndSetReg(param->getType(), reg);
    Type const* src_tp = param->getType();
    Type const* dst_tp = param->getType();
    xoc::PRNO prno = PRNO_UNDEF;
    if (param->is_ld()) {
        //Pass stack var.
        prno = m_irmgr->buildPrno(src_tp);
        ASSERT0(param->hasIdinfo());
        Var * var = param->getIdinfo();
        StorageSpace ss = var->getStorageSpace();
        if (ss == SS_GLOBAL || ss == SS_SPM || ss == SS_READONLY ||
            ss == SS_PARAM) {
            buildLoadForGlobalOrSpmVar(irlist, prno, var, src_tp);
        } else {
            IR * ir = m_irmgr->buildStorePR(prno, src_tp,
                m_irmgr->buildLoad(var, src_tp));
            irlist.append_tail(ir);

            //Get the type of the parameter register used.
            dst_tp = getParamRegisterType(src_tp, reg);
        }
    } else if (param->is_lda()) {
        //Pass address.
        Type const* tp_reg = m_tm->getTargMachRegisterType();
        prno = m_irmgr->buildPrno(tp_reg);
        IR * ir = m_irmgr->buildLoadAddrOfVar(prno, param->getIdinfo());
        irlist.append_tail(ir);
    } else if (param->is_const()) {
        IR * save_const = m_irmgr->buildStprFromConst(
            m_rg->dupIRTree(param), param->getType());
        irlist.append_tail(save_const);
        prno = save_const->getPrno();
        dst_tp = getParamRegisterType(src_tp, reg);
    } else {
        //Pass reg.
        ASSERT0(param->is_pr());
        prno = param->getPrno();
        //Get the type of the parameter register used.
        dst_tp = getParamRegisterType(src_tp, reg);
    }

    IR * stpr = m_irmgr->buildMove(tmpprno, prno, dst_tp, src_tp);
    irlist.append_tail(stpr);

    //Generate new param ir and record it.
    xcom::add_next(paramlist, m_irmgr->buildPRdedicated(tmpprno, src_tp));
}


void ArgPasser::passArgViaStack(OUT IRList & irlist,
    OUT IR ** paramlist, IR const* ir, MOD UINT & arg_size,
    MOD UINT & alignment, IR const* call_ir)
{
    ASSERT0(ir && ir->getType());

    Type const* tp = ir->getType();
    PRNO prno = PRNO_UNDEF;

    if (ir->is_ld()) {
        //Pass stack var.
        ASSERT0(ir->getIdinfo());
        prno = m_irmgr->buildPrno(tp);
        IR * newir = m_irmgr->buildStorePR(prno, tp,
            m_irmgr->buildLoad(ir->getIdinfo(), tp));
        irlist.append_tail(newir);
    } else if (ir->is_lda()) {
        //Pass address
        ASSERT0(ir->getIdinfo());
        Type const* tp_reg = m_tm->getTargMachRegisterType();
        prno = m_irmgr->buildPrno(tp_reg);
        IR * newir = m_irmgr->buildLoadAddrOfVar(prno, ir->getIdinfo());
        irlist.append_tail(newir);
    } else {
        PRNO prno_old = PRNO_UNDEF;
        if (ir->is_const()) {
            //GCOVR_EXCL_START
            IR * save_const = m_irmgr->buildStprFromConst(
                m_rg->dupIRTree(ir), ir->getType());
            irlist.append_tail(save_const);
            prno_old = save_const->getPrno();
            //GCOVR_EXCL_STOP
        } else {
            prno_old = ir->getPrno();
        }
        //Pass register.
        //A new prno should be built here bacause when a register is passed
        //as two params, prno and spill var correspond one-to-one.
        //e.g: call func ($0, $0);
        prno = m_irmgr->buildPrno(tp);
        IR * newir = m_irmgr->buildMove(prno, prno_old, tp);
        irlist.append_tail(newir);
    }

    IR * spill = m_lsra->buildSpill(prno, tp);

    ASSERT0(spill->hasIdinfo() && spill->getIdinfo());
    Var * spill_var = (Var*)spill->getIdinfo();
    VAR_align(spill_var) = m_tm->getByteSize(tp) > PARAM_ALIGNMENT ?
        m_tm->getByteSize(tp) : PARAM_ALIGNMENT;

    irlist.append_tail(spill);

    //Records the current call instruction and the new variables passed
    //through the stack.
    appendCallIRArgsToStack(call_ir, spill_var);

    //Update total arg_size and alignment for each function call.
    updateArgSizeAndAlignment(arg_size, alignment, spill_var);

    //Generate new param ir and record it.
    xcom::add_next(paramlist, m_irmgr->buildLoad(spill_var, tp));
    return;
}


void ArgPasser::processRetValue(MOD IRBB * irbb,
    OUT IRList & irlist, IR const* ir)
{
    if (!ir->hasReturnValue()) { return; }

    xgen::Reg reg = pickRetReg(ir);

    ASSERT0(reg != REG_UNDEF);
    return passRetViaRegister(irbb, ir, irlist, reg);
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


void ArgPasser::copyMCWithMemcpy(OUT IRList & irlst, MOD IR *& param,
                                 MOD UINT & size, MOD UINT & align,
                                 Var const* param_var,
                                 Var const* formal_param_var, IR const* call_ir)
{
    ASSERT0(param && param_var);

    Type const* tp = formal_param_var != nullptr ?
        formal_param_var->getType() : param_var->getType();
    UINT const param_align = formal_param_var != nullptr ?
        formal_param_var->get_align() : param_var->get_align();

    //Construct variable to save values.
    xcom::StrBuf name(64);
    Var * arg_var = m_rg->getVarMgr()->registerVar(
        m_lsra->genFuncLevelNewVarName(name), tp, param_align, VAR_LOCAL);

    //Records the current call instruction and the new variables passed
    //through the stack.
    appendCallIRArgsToStack(call_ir, arg_var);

    //Update argument size and alignment based parameter of current call.
    updateArgSizeAndAlignment(size, align, arg_var);

    //Use memcpy to copy values.
    buildMemcpy(irlst, param, arg_var, m_tm->getByteSize(tp));

    //Modify current parameter.
    param = m_irmgr->buildLda(arg_var);
}


void ArgPasser::copyMCWithLoadStore(OUT IRList & irlst, MOD IR *& param,
                                    MOD UINT & size, MOD UINT & align,
                                    Var const* param_var,
                                    Var const* formal_param_var,
                                    IR const* call_ir)
{
    ASSERT0(param && param_var);

    Type const* tp = formal_param_var != nullptr ?
        formal_param_var->getType() : param_var->getType();
    ASSERT0(tp);

    UINT sz = m_tm->getByteSize(tp);
    if (sz == 0) { sz = 1; } //Type may be b<0>, we must keep it un-zero.
    UINT offset = 0;
    Var * start_var = nullptr;

    //Move the argument from local space to argument space.
    while (offset < sz) {
        //Get partital data type.
        Type const* type = getCurrentDataType(param_var->get_align());
        PRNO prno = m_irmgr->buildPrno(type);

        if (param->is_pr()) {
            IR * load = m_irmgr->buildStorePR(prno, type,
                m_irmgr->buildILoad(
                m_irmgr->buildPRdedicated(param->getPrno(), m_tm->getAny()),
                offset, type));
            irlst.append_tail(load);
        } else {
            ASSERT0(param->is_ld() && param->is_mc());
            StorageSpace ss = param_var->getStorageSpace();
            Var * var = (Var*)param_var;

            if (ss == SS_GLOBAL || ss == SS_SPM || ss == SS_READONLY ||
                ss == SS_PARAM) {
                //Load partital value from global/spm/const/param space.
                buildLoadForGlobalOrSpmVar(irlst, prno, var, type, offset);
            } else {
                //Load partital value from local space.
                IR * load = m_irmgr->buildStorePR(prno, type,
                    m_irmgr->buildLoad(var, offset, type));
                load->getRHS()->setAligned(true);
                irlst.append_tail(load);
            }
        }

        ASSERT0(m_tm->getByteSize(type) != 0);
        offset += m_tm->getByteSize(type);

        //Store partital value to argument space.
        IR * spill = m_lsra->buildSpill(prno, type);
        irlst.append_tail(spill);

        //Records the current call instruction and the new variables passed
        //through the stack.
        appendCallIRArgsToStack(call_ir, spill->getIdinfo());

        //Update total argument size and alignment for each function call.
        updateArgSizeAndAlignment(size, align, spill->getIdinfo());

        //Get start address of this argument.
        if (start_var == nullptr) {
            start_var = spill->getIdinfo();
        }
    }

    ASSERT0(start_var != nullptr);
    param = m_irmgr->buildLda(start_var);
}


void ArgPasser::processArgOrParamWithMCType(MOD IR *& param,
    OUT IRList & irlst, MOD UINT & size, MOD UINT & align,
    Var const* param_var, Var const* formal_param_var, IR const* call_ir)
{
    ASSERT0(param && param_var);

    //Use size and alignment of formal parameter.
    UINT param_size = formal_param_var != nullptr ?
        m_tm->getByteSize(formal_param_var->getType()) :
        m_tm->getByteSize(param_var->getType());
    UINT param_align = formal_param_var != nullptr ?
        formal_param_var->get_align() : param_var->get_align();
    m_max_argument_align = MAX(m_max_argument_align, param_align);

    //If the size of parameter passed is very large, use memcpy to improve
    //performance.
    if ((UINT)(param_size / param_align) > getMaxCopyNum()) {
        return copyMCWithMemcpy(irlst, param, size, align, param_var,
                                formal_param_var, call_ir);
    }

    copyMCWithLoadStore(irlst, param, size, align, param_var,
                        formal_param_var, call_ir);
}


Var const* ArgPasser::getCalleeFormalParamVar(IR const* ir, UINT position)
{
    ASSERT0(ir && ir->isCallStmt());

    //[TODO] Currently, the function definition corresponding to ICALL cannot
    //       be obtained, and we need to wait for the support of llvm or
    //       some other ways.
    if (ir->is_icall()) { return nullptr; }

    Region * callee_region = m_rg->getRegionMgr()->getRegion(CALL_idinfo(ir));

    if (callee_region == nullptr) { return nullptr; }
    Var const* var = callee_region->findFormalParam(position);
    return var != nullptr && var->is_mc() ? var : nullptr;
}


void ArgPasser::processArgument(MOD IRBB * irbb,
                                OUT IRList & irlist, IR const* ir,
                                MOD UINT & arg_size, MOD UINT & alignment)
{
    ASSERT0(m_rg->getRegionMgr());

    IRList irlst;
    IR * new_arg_list = nullptr;

    //[TODO] We can also handle ir with no id info (IR_ICALL) after LLVM-PCX
    //       adds enough function pointer information on it. Temporarily we
    //       handle IR_CALL only.
    // ASSERT0(ir->is_call() && CALL_idinfo(ir));

    UINT position = 0; //Arguments index.
    for (IR * arg = CALL_arg_list(ir); arg != nullptr; arg = arg->get_next()) {
        IR * curarg = arg;
        Var const* arg_var = nullptr;
        Var const* formal_parameter_var = nullptr;

        //Whether argument or formal parameter has type MC.
        if (arg->is_ld() || arg->is_lda()) {
            if (arg->is_mc()) {
                arg_var = arg->getIdinfo();
                formal_parameter_var = getCalleeFormalParamVar(ir, position);
            }
        } else {
            arg_var = getCalleeFormalParamVar(ir, position);
        }

        //Process argument or parameter with type mc.
        if (arg_var != nullptr) {
            processArgOrParamWithMCType(curarg, irlst, arg_size, alignment,
                                        arg_var, formal_parameter_var, ir);
        }

        xgen::Reg reg = pickParamReg(curarg);
        if (reg == REG_UNDEF) {
            passArgViaStack(irlst, &new_arg_list, curarg,
                            arg_size, alignment, ir);
        } else {
            passArgViaRegister(irlst, &new_arg_list, curarg, reg);
        }

        position++;
    }

    //Update the arguments list of call with new arguments irs.
    updateCallArgList((IR*)ir, new_arg_list);

    //Update max argument size.
    m_max_argument_size = arg_size > m_max_argument_size ?
        arg_size : m_max_argument_size;

    for (IR * ir = irlst.get_head(); ir != nullptr;
         ir = irlst.get_next()) {
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
    ASSERT0(reg != REG_UNDEF);

    ASSERT0(ir->is_st() && ir->getRHS()->is_pr());
    ASSERT0(ir->hasIdinfo());

    if (!ir->getIdinfo()->is_mc()) {
        PRNO prno = m_lsra->buildPrnoAndSetReg(ir->getType(), reg);
        IR * newir = m_irmgr->buildStorePR(prno, ir->getType(), ir->getRHS());
        irlist.append_tail(newir);
        return;
    }

    //When the type of retval is B<n>, there are two ways to pass it.
    //The first is: memcpy.param.stack [retval], [stackval], size;
    //The second is: store.param.u64 [retval], $0;
    //               store.param.u64 [retval + 8], $1;
    //The first case is handled in function processIRUsedFormalParam.
    //Here is to deal with the second case, get the address of the retval
    //and then construct an IR_IST.
    ASSERT0(ir->getIdinfo() && ir->getIdinfo()->is_mc() && ir->getType());
    Var const* v = ir->getIdinfo();
    Type const* tp = ir->getType();

    bool is_find = m_var2prno.find(v);
    if (is_find) {
        xoc::PRNO prno = m_var2prno.get(v);
        ASSERT0(prno != PRNO_UNDEF);
        ASSERT0(ir->hasRHS() && ir->getRHS()->is_pr() && tp);
        IR * istore = m_irmgr->buildIStore(
            m_irmgr->buildPRdedicated(prno, m_tm->getPointerType(1)),
            m_irmgr->buildPRdedicated(ir->getRHS()->getPrno(), tp),
            ST_ofst(ir), tp);
        irlist.append_tail(istore);
        return;
    }

    //GCOVR_EXCL_START
    xoc::PRNO prno = m_irmgr->buildPrno(tp);
    IR * newir = m_irmgr->buildStorePR(prno, tp,
        m_irmgr->buildLoad((Var*)v, tp));
    irlist.append_tail(newir);

    ASSERT0(ir->hasRHS() && ir->getRHS()->is_pr() && tp);
    IR * istore = m_irmgr->buildIStore(
        m_irmgr->buildPRdedicated(prno, m_tm->getPointerType(1)),
        m_irmgr->buildPRdedicated(ir->getRHS()->getPrno(), tp),
        ST_ofst(ir), tp);
    irlist.append_tail(istore);
    //GCOVR_EXCL_STOP
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
    ASSERT0(m_rg->getCFG() && m_rg->getCFG()->getEntry());
    IRBB * bb = m_rg->getCFG()->getEntry();
    Type const* tp = m_tm->getTargMachRegisterType();

    PRNO prno_dedicated = m_lsra->buildPrnoAndSetReg(
        tp, m_lsra->getParamScalarStart());
    prno = m_irmgr->buildPrno(tp);
    IR * ir = m_irmgr->buildMove(prno, prno_dedicated, tp);

    bb->getIRList().append_tail(ir);
}


void ArgPasser::appendCallIRArgsToStack(IR const* call_ir, Var const* v)
{
    ASSERT0(call_ir && call_ir->isCallStmt() && v);
    bool find = false;
    xcom::List<Var const*> * list_var =
        m_call_arg_list.getAndGen(call_ir, &find);
    ASSERT0(list_var && m_call_arg_list.find(call_ir));
    list_var->append_tail(v);
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

    bb->getIRList().append_tail(stpr);

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
    IRBB * bb = getKernelAdjustBBForEntryFunc();
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

        bb->getIRList().append_tail(stpr);

        m_var2prno.set(v, param_prno);

        offset += m_tm->getByteSize(v->getType());
    }
    m_mod_bblist.append_tail(bb);
}


void ArgPasser::preProcessFormalParam(OptCtx & oc)
{
    initRegBindInfo();

    ConstVarList param_list;

    //If the function call has a return value and its type is not MC,
    //no preprocessing is needed.
    //e.g:
    //
    //CASE1: No return value.
    //  C Syntax:
    //      void foo(a);
    //  Call IR:
    //      call 'foo'  id:46     <- IR type is ANY.
    //          $1:u64 arg0 id:21
    //
    //CASE1: Return type is simplex.
    //  C Syntax:
    //      int foo(a);
    //  Call IR:
    //      $29:u64 = call 'foo'  id:46
    //          $1:u64 arg0 id:21
    //
    //CASE2: Return type is struct.
    //  C Syntax:
    //      struct S foo(a);
    //  Call IR:
    //      $15:mc<16> = call 'foo'  id:46
    //          $1:u64 arg0 id:21
    //          lda:*<16>:storage_space(stack) 'struct1' arg1
    //
    //When the return type is MC, the return value address needs to
    //be passed using a parameter register.
    findAndSetFormalParam(param_list);

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
        PRNO src_prno = m_lsra->buildPrnoAndSetReg(type, reg);

        //NOTE: The generated IR needs to be inserted after the entry bb,
        //otherwise it will cause the register to be used incorrectly.
        //For example, tgt_prno is assigned a callee save register, this
        //register will be saved in the entrybb, and if the IR is also
        //inserted into the entrybb, it may be inserted before the callee
        //save. Therefore, in order to ensure the correctness of the callee
        //save registers, these newly generated IRs are inserted into a new
        //bb, and this new bb is inserted after the entrybb.
        IR * ir = m_irmgr->buildMove(tgt_prno, src_prno, type,
                                     getParamRegisterType(type, reg));
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

    preProcessFormalParam(oc);

    //Process formal parameter.
    IRList new_ir_list;
    for (IRBB * bb = bblist->get_head(&bbit); bb != nullptr;
         bb = bblist->get_next(&bbit)) {
        new_ir_list.clean();
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
    Type const* tp = m_tm->getTargMachRegisterType();
    if (ir->is_call()) {
        if (!isArgPasserExternalCallNeedLda()) { return; }
        PRNO call_prno = m_lsra->buildPrnoAndSetReg(tp, m_lsra->getTA());
        IR * call_addr = m_irmgr->buildLoadAddrOfVar(call_prno,
                                                     ir->getIdinfo());
        irlist.append_tail(call_addr);
        return;
    }
    ASSERT0(ir->is_icall() && ICALL_callee(ir)->getPrno() != PRNO_UNDEF);
    PRNO call_prno = m_lsra->buildPrnoAndSetReg(tp, m_lsra->getTA());
    IR * stpr = m_irmgr->buildMove(call_prno, ICALL_callee(ir)->getPrno(), tp);
    irlist.append_tail(stpr);
}


void ArgPasser::processCallStmt()
{
    //Traverse each IR in each BB in BBList, bind the parameter and return
    //value of the call instruction to the register.
    BBList * bblist = m_rg->getBBList();
    BBListIter bbit;
    IRList new_ir_list;
    IRList ir_list_each_call;
    for (IRBB * bb = bblist->get_head(&bbit); bb != nullptr;
         bb = bblist->get_next(&bbit)) {
        new_ir_list.clean();
        ir_list_each_call.clean();
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

            if (m_dystack_impl->hasAlloca() && arg_size > 0) {
                //The size of the sp adjustment is arg_size and
                //should aligned to alignment.
                m_dystack_impl->dynamicAdjustSPForArgument(
                    ir_list_each_call, ir, arg_size, alignment);
            }

            new_ir_list.move_tail(ir_list_each_call);
            ir_list_each_call.clean();
            m_mod_bblist.append_tail(bb);
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


void ArgPasser::findAndSetFormalParam(OUT ConstVarList & paramlst)
{
    ConstVarList const* lst = m_rg->findAndRecordFormalParamList(true);
    ASSERT0(lst != nullptr);

    paramlst.copy(*const_cast<ConstVarList*>(lst));
    if (paramlst.get_elem_count() == 0) { return; }

    xcom::List<IRBB*> * bblist = m_rg->getCFG()->getExitList();
    xcom::List<IRBB*>::Iter bbit;
    for (IRBB * bb = bblist->get_tail(&bbit); bb != nullptr;
         bb = bblist->get_prev(&bbit)) {
        ASSERT0(bb->is_exit());
        for (IR * ir = bb->getIRList().get_tail(); ir != nullptr;
             ir = bb->getIRList().get_prev()) {
            if (isStoreFormalParam(ir) && !ir->getIdinfo()->is_mc()) {
                ASSERT0(ir->getIdinfo() == paramlst.get_tail());
                paramlst.remove_tail();
                return;
            }
        }
    }
}


void ArgPasser::splitBBIfNeeded(OptCtx & oc)
{
    //Split the BBs that are inserted with IRs after the call statement.
    for (IRBB * bb = m_mod_bblist.get_head();
         bb != nullptr; bb = m_mod_bblist.get_next()) {
        m_rg->getCFG()->splitBBIfNeeded(bb, oc);
    }
}


bool ArgPasser::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_lsra = (LinearScanRA*)m_rg->getPassMgr()->registerPass(
        PASS_LINEAR_SCAN_RA);
    ASSERT0(m_lsra);

    m_dystack_impl = (DynamicStack*)m_rg->getPassMgr()->
        queryPass(PASS_DYNAMIC_STACK);
    ASSERT0(m_dystack_impl);

    processIRUsedFormalParam(oc);
    processCallStmt();
    splitBBIfNeeded(oc);
    oc.setInvalidDom(); //CFG has been modified.
    oc.setInvalidLoopInfo();
    set_valid(true);
    END_TIMER(t, getPassName());
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpArgPasser()) {
        dump();
    }
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(m_rg->getCFG()->verifyRPO(oc));
    ASSERT0(m_rg->getCFG()->verifyLoopInfo(oc));
    ASSERT0(m_rg->getCFG()->verifyDomAndPdom(oc));
    return true;
}
