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
#ifndef _ARG_PASS_H_
#define _ARG_PASS_H_

namespace xoc {

typedef xcom::TMap<xoc::Var const*, Reg> ParamVar2Reg;
typedef xcom::TMap<xoc::Var const*, PRNO> ParamVar2Prno;

class LinearScanRA;
class Region;
class IRMgr;
class TypeMgr;
class DynamicStack;

class ArgPasser : public Pass {
    COPY_CONSTRUCTOR(ArgPasser);
protected:
    LinearScanRA * m_lsra;
    IRMgr * m_irmgr;
    TypeMgr * m_tm;
    DynamicStack * m_dystack_impl;

    //Record the IR that obtains the entry function parameter address and use
    //it as a marker so that a spill can be inserted before the callee register
    //during subsequent register allocation.
    IR * m_entry_param;

    // Record register binding info of scalar parameters.
    xgen::RegSet m_avail_param_scalar;
    // Record register binding info of vecotor parameters.
    xgen::RegSet m_avail_param_vector;
    // Record register binding info of scalar ret values.
    xgen::RegSet m_avail_ret_scalar;
    // Record register binding info of vector ret values.
    xgen::RegSet m_avail_ret_vector;

    //The maximum possible size of argument need to be passed on the stack.
    UINT m_max_argument_size;

    //The list of arguments passed on the stack.
    xcom::List<Var const*> m_arg_stack_list;

    //The list of formal parameters passed on the stack.
    xcom::List<Var const*> m_param_stack_list;

    //Record the prno corresponding to the parameters for entry function.
    ParamVar2Prno m_var2prno;
protected:
    //Get the start address of mc type in kernel function.
    void appendIRToGetStartAddressOfMCInEntryFunc(MOD IRBB * irbb,
        PRNO start_address_prno, OUT UINT & offset, Var const* v);

    //Get the start address of parameters in kernel function.
    virtual void appendIRToGetStartAddressOfParamsInEntryFunc(OUT PRNO & prno);

    //Use external function call to copy src_var to dst_var.
    //src_var: source varlable.
    //dst_var: target variable.
    //size: byte size of the copy.
    virtual void buildMemcpy(OUT xcom::List<IR*> & irlist, Var * src_var,
                             Var * dst_var, UINT const size)
    { ASSERTN(0, ("Target Dependent Code")); }

    //Get the destination address of the IR_CALL/IR_ICALL and put it into the
    //REG_TARGET_ADDRESS_REGISTER.
    //NOTE: This function needs to be called after processArgument, consider
    //the following scenario:
    //    .stack.b<192> .align<8> st0;
    //    call test(st0);
    //st0 needs to be passed to the function test, and since the size of st0
    //is 192 and the alignment is 8, for some architectures, st0 will be
    //copied into the parameter space by memcpy. Therefore if the
    //buildTargetAddressForCall is before the processArgument, REG_TARGET_
    //ADDRESS_REGISTER will be overwritten.
    //
    //The pseudocode comparison is as follows:
    //|--------------------------------------------------------------------|
    //|Wrong order:                    |Correct order:                     |
    //|--------------------------------|-----------------------------------|
    //|buildTargetAddressForCall(...); |processArgument(... ...);          |
    //|processArgument(... ...);       |buildTargetAddressForCallStmt(...);|
    //|--------------------------------|-----------------------------------|
    //|    lda $ra, test               |    lda $ra, memcpy                |
    //|    lda $ra, memcpy             |    call memcpy()                  |
    //|    call memcpy()               |    lda $ra, test                  |
    //|    call test()                 |    call test()                    |
    //----------------------------------------------------------------------
    void buildTargetAddressForCall(OUT IRList & irlist, IR * ir);

    //When copying an argument to the parameter space, if the argument size is
    //too large, many loads and stores will be generated, which will lead to
    //a long compilation time. Therefore, when the load and store number is
    //greater than the maximum number of copies defined by the architecture,
    //the memcpy function will be called to copy the variable.
    IR * copyMCWithMemcpy(OUT IRList & irlist, IR const* ir,
                          OUT UINT & arg_size, MOD UINT & alignment);

    //When passing an argument of type MC by value, use this function to
    //move the variable of type MC to the parameter space.
    //e.g:
    //.func entry()
    //{
    //    .stack.b<16> .align<8> stval;
    //    call test(st);
    //    ret;
    //}
    //In this case, stval is a stack variable and is passed to the function
    //test as a value, so stval needs to be copyed to the parameter space,
    //the way to move is:
    //    load.stack.u64 $0, [stval];
    //    store.param.u64 [param], $0;
    //    load.stack.u64 $0, [stval + 8];
    //    store.param.u64 [param + 8], $0;
    //
    //arg_size: the sum of the parameter size passed through the stack for each
    //          function call.
    //alignment: The maximum alignment of all parameters passed through the
    //           stack.
    //NOTE: The size of the data for each copy, calculated from the alignment
    //of the variables.
    virtual IR * copyMCAndReturnAddress(OUT IRList & irlist, IR const* ir,
        OUT UINT & arg_size, MOD UINT & alignment);

    //This function is used for passing paramater which is a memory variable and
    //is located in global or spm space. The variable will be loaded in the
    //tgtprno.
    void buildLoadForGlobalOrSpmVar(OUT IRList & irlist, PRNO tgtprno,
                                    Var * var, Type const* type,
                                    TMWORD ofst = 0);

    virtual bool dump() const;

    //Return the data type of each copy when copying the argument of type MC to
    //the parameter space, and the size of the data copied each time is equal to
    //the alignment of the variable.
    //e.g:
    //.stack.b<16> .align<8> st0;
    //The alignment of st0 is 8 and the size is 16, so st0 will be loaded twice.
    //-->
    //    load.stack.u64 $0, [st0];
    //    store.param.u64 [param0], $0;
    //    load.stack.u64 $0, [st0 + 8];
    //    store.param.u64 [param0 + 8], $0;
    //
    //.stack.b<16> .align<4> st1;
    //The alignment of st1 is 4 and the size is 16, so st1 will be loaded four
    //times.
    //-->
    //    load.stack.u32 $0, [st1];
    //    store.param.u32 [param1], $0;
    //    load.stack.u32 $0, [st1 + 4];
    //    store.param.u32 [param1 + 4], $0;
    //    load.stack.u32 $0, [st1 + 8];
    //    store.param.u32 [param1 + 8], $0;
    //    load.stack.u32 $0, [st1 + 12];
    //    store.param.u32 [param1 + 12], $0;
    Type const* getCurrentDataType(UINT align);

    void getFormalParamViaRegister(OUT IRList & irlist, IR * ir);

    //Determines the argument register type when passing parameters
    //via registers.
    virtual Type const* getParamRegisterType(Type const* ty, Reg r) const
    { return ty; }

    //Init registers binding info before allocated for each function.
    virtual void initRegBindInfo();

    //Whether the parameter passer generates LAD operations for function calls.
    virtual bool isArgPasserExternalCallNeedLda() //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Pick reg according to var.
    virtual xgen::Reg pickParamReg(Var const* v) //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    //Pick reg according to ir.
    virtual xgen::Reg pickParamReg(IR const* ir) //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    //Pick ret reg.
    virtual xgen::Reg pickRetReg(IR const* ir) //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //1. For parameters that are passed by registers.
    //(1) Assign parameter registers to each parameter.
    //The reason is that function PickParamReg() is to pick registers
    //sequential, but parameters may be accessed out of order, so need
    //to pickreg for each parameter before processing the irlist.
    //CASE 1: Formal parameters may be accessed out of order.
    //    .func test(.param.u32 param0, .param.u32 param1)
    //    {
    //        .reg $0;
    //        .reg $1;
    //        load.param.u32 $0, [param1];
    //        load.param.u32 $1, [param0];
    //    }
    //CASE 2: B<N>-type may be accessed multiple times.
    //    .func test(.param.b<16> mystruct, .param.u32 param1)
    //    {
    //        .reg $0;
    //        .reg $1;
    //        .reg $2;
    //        load.param.u64 $0, [mystruct];
    //        load.param.u64 $1, [mystruct + 8];
    //        load.param.u64 $2, [param1];
    //    }
    //(2) Mov parameter from the parameter register to a normal register.
    //For the following CASE 3, param0 and param1 use two parameter
    //registers, and the arguments of printf will use two parameter
    //registers too, so it is necessary to mov the parameters of the
    //function from the parameter register to the common register before
    //the function body to avoid conflicts with other function calls.
    //CASE 3:
    //    .func test(.param.u64 param0, .param.u64 param1)
    //    {
    //        ... ...
    //        call printf(str, $1);
    //        load.param.u64 $0, [param0];
    //        load.param.u64 $1, [param1];
    //        ret;
    //    }
    //2. For parameters that are passed by stack: append parameters passed
    //   by stack into the m_param_stack_list.
    //For the following case, assume there are 6 param registers, param6 and
    //param7 will be passed by stack, and param6 is not used in function test.
    //Therefore, param6 needs to be appended into the m_param_stack_list,
    //otherwise the offset of param7 will be incorrect.
    //CASE 4: Parameters passed through the stack are not used.
    //    .func test(.param.u32 param0, .param.u32 param1,
    //               .param.u32 param2, .param.u32 param3,
    //               .param.u32 param4, .param.u32 param5,
    //               .param.u32 param6, .param.u32 param7)
    //    {
    //        .reg $0;
    //        load.param.u64 $0, [param7];
    //        ret;
    //    }
    void preProcessFormalParam(OptCtx & oc);

    //Process irs that used formal parameters.
    //CASE1:
    //Replace load.param.u64 $0, [param0];
    //with mov.u64 $0, $dedicate_param_reg;
    //
    //CASE2:
    //Replace store.param.u64 [retval0], $0;
    //With mov.u64 $dedicate_param_reg, $0;
    //
    //CASE3: memcpy.param.stack [retval], [mystruct], 12;
    //Replace lda.param.u64 $0, [retval];
    //With mov.u64 $0, $dedicate_param_reg;
    void processIRUsedFormalParam(OptCtx & oc);

    //Process IR_CALL and IR_ICALL.
    //It consists of two parts:
    //PART1: Build IR for each argument and retval.
    //e.g: call test($0) -> $1;
    //The generated IR sequence is:
    // -> mov.u64 $dedidate_arg_reg $0;
    // -> call test($dedidate_arg_reg) -> $dedicate_ret_reg;
    // -> mov.u64 $1, $dedicate_ret_reg;
    //PART2: dynamic adjust sp if argument size > 0 and hasAllocaIR() is true.
    //e.g: call test($0, $1, $2, $3, $4, $5, $6) -> $7;
    //Assume: 1.The number of argument registers is 6, $6 will be pass by stack.
    //        2.The register size of $6 is 8B.
    //The generated IR sequence is:
    // -> sub.u64 $sp, $sp, 8;
    // -> store.stack.u64 [$sp], $6;
    // -> call test(... ...) -> $dedicate_ret_reg;
    // -> mov.u64 $7, $dedicate_ret_reg;
    // -> add.u64 $sp, $sp, 8;
    void processCallStmt();

    //Traversing paramlist, and recording the prnos used by formal params.
    void processEntryFunction(List<xoc::Var const*> & param_list);

    //Bind register to return value.
    void processRetValue(MOD IRBB * irbb, OUT IRList & irlist, IR const* ir);

    //Bind registers to parameters.
    //Different architectures provide some specific registers to store function
    //arguments. The registers are moved backwards according to the increase of
    //parameters.
    //However, the number of parameter registers is usually limited. When the
    //number of parameters exceeds this value, the parameters will be
    //temporarily stored on the stack. For example:
    //                       call func(i, j, k, l);
    //Arch A provides $1~$3 as parameter registers. Para i will bind $1, para j
    //will bind $2 and k will bind $3. Para l will be store in stack and then
    //load later for using.
    //
    //arg_size: the sum of the parameter size passed through the stack for each
    //          function call.
    //alignment: The maximum alignment of all parameters passed through the
    //           stack.
    //NOTE: arg_size and alignment are used to open a stack for arguments of
    //function calls when alloca exists.
    void processArgument(MOD IRBB * irbb, OUT IRList & irlist,
        IR const* ir, MOD UINT & arg_size, MOD UINT & alignment);

    //If the parameters are passed through the register.
    //Replace load.param.u64 $0, [param0];
    //with mov.u64 $0, $dedicate_param_reg;
    void processLoadFormalParam(MOD IRBB * irbb,
                                OUT IRList & irlist, IR * ir);

    //process formal param with mc type.
    //CASE1: The address of the mc type variable has been calculated.
    //The address of the mc type variable has been calculated in
    //appendIRToGetStartAddressOfMCInEntryFunc and stored in $address.
    //Replace:
    //load.param.u64 .aligned $1, [mystruct];
    //load.param.u64 .aligned $2, [mystruct + 8];
    //With:
    //load.param.u64 .aligned $1, [$address];
    //load.param.u64 .aligned $2, [$address + 8];
    //CASE2: The address of the mc type variable is not calculated.
    //The address of mc type variable should be calculated later in reloc_mgr,
    //these irs can be inserted directly into the irlist and cannot be replaced.
    //load.param.u64 .aligned $1, [mystruct];
    //load.param.u64 .aligned $2, [mystruct + 8];
    void processFormalParamWithMCType(OUT IRList & irlist, IR * ir);

    //If the parameters are passed through the register.
    //Replace store.param.u64 [retval], $0;
    //with mov.u64 $dedicate_param_reg, $0;
    void processFormalReturnParam(OUT IRList & irlist, IR * ir);

    //Pass parameters less than or equal to the number fo parameter registers
    //via register.
    void passArgViaRegister(OUT IRList & irlist, OUT IR ** paramlist,
                            IR const* ir, xgen::Reg reg);

    //Pass parameters more than the number fo parameter registers via stack.
    void passArgViaStack(OUT IRList & irlist, OUT IR ** paramlist,
                         IR const* ir, MOD UINT & arg_size,
                         MOD UINT & alignment);

    //Pass return values less than or equal to the number fo return value
    //registers via register.
    void passRetViaRegister(MOD IRBB * irbb, IR const* ir,
        MOD IRList & irlist, xgen::Reg reg);

    //Pass return values more than the number fo return value registers via
    //stack.
    void passRetViaStack(IR const* ir);

    //Process argument which size exceed the general register size.
    //CASE1: Pass the address of this argument.
    //e.g: call test() -> .param.B<N> retval;
    //In this case, the address of the retval needs to be passed
    //into the function test.
    //
    //CASE2: Pass the value of this augument.
    //e.g: call test(.stack.B<N> stack0);
    //In this case, the value of stack0 needs to be passed
    //into the function test.
    IR * processArgumentWithMCType(IR const* ir, IR * param,
        OUT IRList & ir_list, MOD UINT & arg_size, MOD UINT & alignment);

    IR * processFormalParamAddress(OUT IRList & irlist, IR * ir);

    //Update total arg_size and max alignment for each function call.
    void updateArgSizeAndAlignment(MOD UINT & arg_size, MOD UINT & alignment,
                                   Var const* var) {
        arg_size += m_tm->getByteSize(var->getType());
        alignment = (UINT)xcom::ceil_align(MAX(var->get_align(),
            alignment), STACK_ALIGNMENT);
        arg_size = (UINT)xcom::ceil_align(arg_size, alignment);
    }

public:
    ArgPasser(Region * rg);
    virtual ~ArgPasser() {}

    IR * getEntryParam() const { return m_entry_param; }

    UINT getMaxArgSize() const { return m_max_argument_size; }

    //Different architectures need to overwrite this interface to bind
    //different registers for sclar parameters.
    virtual xgen::RegSet const* getParamRegSetScalar() const //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Different architectures need to overwrite this interface to bind
    //different registers for vector parameters.
    virtual xgen::RegSet const* getParamRegSetVector() const //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Different architectures need to overwrite this interface to bind
    //different registers for scalar return values.
    virtual xgen::RegSet const* getRetRegSetScalar() const //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Different architectures need to overwrite this interface to bind
    //different registers for vector return values.
    virtual xgen::RegSet const* getRetRegSetVector() const //GCOVR_EXCL_LINE
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    xcom::List<Var const*> * getStackArgList() { return &m_arg_stack_list; }

    xcom::List<Var const*> * getStackParamList() { return &m_param_stack_list; }

    virtual CHAR const* getPassName() const { return "Arg Passer"; }

    PASS_TYPE getPassType() const { return PASS_ARGPASSER; }

    //Returns the maximum number of copies when moving a variable of type mc to
    //the parameter space. If the actual number of copies is greater than this
    //value, the parameter is moved by function call.
    virtual UINT getMaxCopyNum() const { return 8; }

    virtual bool perform(OptCtx & oc);
};

}

#endif
