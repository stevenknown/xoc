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
#ifndef _VAR2OFFSET_
#define _VAR2OFFSET_

namespace xoc {

class LinearScanRA;

class PrologueEpilogueInserter;

typedef xcom::TMapIter<Var const*, HOST_UINT> Var2OffsetIter;


//This class is used to calcalate the offset of variable.
class Var2Offset : public xcom::TMap<Var const*, HOST_UINT> {
    COPY_CONSTRUCTOR(Var2Offset);
protected:
    HOST_UINT m_cur_offset;
    HOST_UINT m_align;
    TypeMgr const* m_tm;
public:
    explicit Var2Offset(HOST_UINT align, TypeMgr const* tm) :
        m_cur_offset(0), m_align(align), m_tm(tm)
    {}

    void dump(OUT StrBuf & buf) const;
    void dump(OUT FileObj & fo) const;

    //The function tries to retrieve 'v' in the variable layout, return the
    //offset of 'v' if find. Otherwise, the function will compute the layout
    //of 'v' and add 'v' to current variable table.
    HOST_UINT getOrAddVarOffset(Var const* v);

    HOST_UINT getAlign() const { return m_align; }

    //Get total offset.
    HOST_UINT getTotalOffset() const { return m_cur_offset; }

    //m_cur_offset can only be reset when meeting function call,
    //and current m_cur_offset must represent the offset in argument space.
    void resetCurOffset() { m_cur_offset = 0; }

    //Set the alignment.
    //The byte offset of each Variable will be aligned in 'align'.
    void setAlign(HOST_UINT align) { m_align = align; }
};


//This class is used to calculate the offset of the variable in
//different stack memory partition.
//The variable layout in stack is divided into four parts:
//1. argument space: stores the arguments of current function.
//2. parameter space: stores the formal parameters of current function.
//3. local variable space: stores local variables, such as spilled var
//                         or stack var.
//4. special variable space: stores the spilled variable of $fp or $ra.
class Var2OffsetMgr {
    COPY_CONSTRUCTOR(Var2OffsetMgr);
protected:
    Region * m_rg;
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    LinearScanRA * m_ra;
    PrologueEpilogueInserter * m_pelog;
    ArgPasser * m_arg_passer;
    Var2Offset * m_arg_var2off;
    Var2Offset * m_param_var2off;
    Var2Offset * m_local_var2off;
    Var2Offset * m_special_var2off;
public:
    Var2OffsetMgr(Region * rg)
    {
        m_rg = rg;
        m_tm = m_rg->getTypeMgr();
        m_irmgr = m_rg->getIRMgr();
        m_pelog = (PrologueEpilogueInserter*)m_rg->getPassMgr()->
            registerPass(PASS_PROLOGUE_EPILOGUE);
        m_ra = m_pelog->getLsra();
        m_arg_passer = (ArgPasser*)rg->getPassMgr()->
            registerPass(PASS_ARGPASSER);
        ASSERT0(m_arg_passer);
        m_local_var2off = new Var2Offset(STACK_ALIGNMENT, m_tm);
        m_special_var2off = new Var2Offset(STACK_ALIGNMENT, m_tm);
        m_arg_var2off = new Var2Offset(STACK_ALIGNMENT, m_tm);
        m_param_var2off = new Var2Offset(PARAM_ALIGNMENT, m_tm);
    }

    virtual ~Var2OffsetMgr() {
        if (m_local_var2off != nullptr) {
            delete m_local_var2off;
            m_local_var2off = nullptr;
        }
        if (m_special_var2off != nullptr) {
            delete m_special_var2off;
            m_special_var2off = nullptr;
        }
        if (m_arg_var2off != nullptr) {
            delete m_arg_var2off;
            m_arg_var2off = nullptr;
        }
        if (m_param_var2off != nullptr) {
            delete m_param_var2off;
            m_param_var2off = nullptr;
        }
    }

public:
    //This function is used to calculate the offset of a local variable.
    //There are two steps to calculate the offset of a local variable:
    //
    //  1. Calculates the relative offset of the variable within all local
    //     variables, the formula is:
    //       offset = var2off.getOrAddVarOffset(var);
    //
    //  2. Calculate the relative offset of the variable relative to the SP.
    //       offset = var2off.getOrAddVarOffset(var) + argument size;
    //
    //     The reason for adding argument size is that the arguments must be
    //     stored next to the SP and can only be accessed by SP, while local
    //     variables can be accessed by FP, so the offset of arguments and local
    //     variables needs to be calculated separately.
    //     The relative offset of current variable within the local variables,
    //     add the total size of the argument size, is the offset of current
    //     variable relative to the SP.
    //
    //      ---------------- --> FP --------------
    //      |              |                     |
    //      |              |                     |
    //      |              |                     |
    //      |              |                     |
    //      |              |                     |
    //      |              |                     |
    //      |              |                     |
    //      |              |                     |
    //      |   local var  | <------------|      |
    //      |              |              |      |--> stacksize
    //      |              |              |      |
    //      |              |              |      |
    //      |--------------|--------------|------|----
    //      |              |              |      |   |
    //      |              |              |      |   |
    //      |              |              |      |   |
    //      |              |              |      |   | -->argument size
    //      |              |              |      |   |
    //      |              |              |      |   |
    //      |              |              |      |   |
    //      ---------------- --> SP ------------------
    HOST_INT computeLocalVarOffset(Var const* var);

    //This function is used to calculate the offset of a local variable when FP
    //is used as SP.
    //
    //There are three steps to calculate the offset of a local variable:
    //  1. Calculates the relative offset of the variable within all local
    //     variables, the formula is:
    //       offset = var2off.getOrAddVarOffset(var);
    //
    //  2. Calculate the relative offset of the variable relative to the FP,
    //     there are two scenarios to consider here.
    //     (1) The formula of scenario 1 is:
    //       offset = stacksize - var2off.getOrAddVarOffset(var)
    //       In this scenario, stacksize doesn't contains the size of arguments.
    //
    //     (2) The formula of scenario 2 is:
    //       offset = stacksize - var2off.getOrAddVarOffset(var) - argument size
    //       In this scenario, stacksize contains the size of arguments.
    //__________________________________________________________________________
    //  Scenario 1: has alloca ir   |    Scenario 2: no alloca ir
    //--------------------------------------------------------------------------
    //
    // ----------- -->FP            | ----------------- --> FP: high address
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         | --> stacksize    | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |--> stacksize          |
    // |         |                  | |         |     |                       |
    // |         |                  | |         |     |                       |
    // -----------                  | |---------|---  |                       |
    // |         |                  | |         |  |  |                       |
    // |         |                  | |         |  |  |                       |
    // |         |                  | |         |  |  |                       |
    // |         | --> argument size| |         |  |--|--> argument size      |
    // |         |                  | |         |  |  |                       |
    // |         |                  | |         |  |  |                       |
    // |         |                  | |         |  |  |                       V
    // ----------- --> SP           | ----------------- --> SP : low address
    //-------------------------------------------------------------------------
    //
    //  3. Calculate the negative of offset.
    //     Since the FP is at the bottom of the stack and the stack opening
    //     direction is from high to low, variables need to be accessed by FP in
    //     the opposite direction.
    //
    //     (1) The final formula of scenario 1 is:
    //        offset = -(stacksize - var2off.getOrAddVarOffset(var))
    //
    //     (2) The final formula of scenario 2 is:
    //        offset = -(stacksize - var2off.getOrAddVarOffset(var) -
    //                   argument size)
    HOST_INT computeLocalVarOffsetFromFP(Var const* var);

    //This function is used to calculate the offset of the spilled variable of
    //$fp or $ra.
    //NOTE: $fp and $ra are currently spilled to a fixed position on the stack,
    //      when $fp and $ra are involved in register allocation, the variable
    //      of $fp and $ra could be spilled as local variables and this function
    //      will be removed.
    //      -------------------- --> FP
    //      |                  | --> var of $fp
    //      |                  | --> var of $ra
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      |                  |
    //      -------------------- --> SP
    HOST_UINT computeSpecialVarOffset(Var const* var);

    //Compute the offset of var in argument space.
    HOST_UINT computeArgVarOffset(Var const* var);

    //This function is used to calculate the offset of a param variable.
    //
    //There are two steps to calculate the offset of a param variable:
    //  1. Calculates the relative offset of the variable within all param
    //     variables, the formula is:
    //       offset = var2off.getOrAddVarOffset(var);
    //
    //  2. Calculate the relative offset of the variable relative to the
    //     base address register.
    //     there are two scenarios to consider here:
    //
    //     (1) The formula of scenario 1 is:
    //       offset = var2off.getOrAddVarOffset(var) + stacksize.
    //       In this scenario, arguments are accessed by SP, SP points to the
    //       top of the stack.
    //
    //     (2) The formula of scenario 2 is:
    //       offset = var2off.getOrAddVarOffset(var);
    //       In this scenario, arguments are accessed by FP, FP points to the
    //       bottom of the stack.
    //
    //__________________________________________________________________________
    //  Scenario 1: not used FP as SP  |    Scenario 2: used FP as SP
    //--------------------------------------------------------------------------
    // -------------- -->FP            | -------------- --> FP: high address
    // |            |                  | |            |
    // |            |                  | |            |
    // |            |                  | |            |
    // |            |                  | |            |
    // |            |                  | |            |
    // |            | --> caller       | |            | --> caller
    // |            |                  | |            |
    // |            |                  | |            |
    // |            |                  | |            |
    // |            |                  | |            |
    // |            |<-------------|   | |            |<-----------|
    // |            |              |   | |            |            |
    // -------------- --> FP       |   | |------------| --> FP -----
    // |            |              |   | |            |
    // |            |              |   | |            |
    // |            |              |   | |            |
    // |            | --> callee   |   | |            | --> callee
    // |            |              |   | |            |
    // |            |              |   | |            |
    // |            |              |   | |            |
    // -------------- --> SP ------|   | -------------- --> SP : low address
    //--------------------------------------------------------------------------
    void computeParamOffset();

    //This function calculates the offset of an argument variable,
    //similar to the computeParamOffset() function.
    //When there are not enough argument registers to hold the arguments,
    //the caller pushes the arguments onto the stack.
    //
    //the compiler divides the stack into the following four regions:
    //
    // +-----------------------+
    // |   Special Region      |  <-- Return address and old FP
    // | (Return Address, Old FP)|
    // +-----------------------+
    // |   Local Region        |  <-- Local data (Local Variables)
    // | (Local Variables)     |
    // +-----------------------+
    // |   Alloca Region       |  <-- Dynamically allocated space
    // | (Dynamically Allocated)|
    // +-----------------------+
    // |   Param Region        |  <-- Parameter region (Function Arguments)
    // | (Function Arguments)  |
    // +-----------------------+
    void computeArgVarOffset();

    //True if the input var is an argument variable.
    bool isArgument(Var const* var) const;

    //True if the input var is the spill variable in entry bb.
    bool isSpillVarInEntryBB(Var const* var) const;

    //True if the input var is formal parameter variable.
    bool isParameter(Var const* var) const;

    //Compute the offset of param variable from m_param_var2off.
    HOST_UINT getParamOffset(Var const* var) const;

    HOST_INT computeVarOffset(Var const* var);

    //The argument space can be reused and reset when the function
    //call is finished.
    void resetArgSpaceOffset();

    //Get ra.
    LinearScanRA * getRa() const { return m_ra; }
};

} // namespace xoc

#endif
