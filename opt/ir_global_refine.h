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

author: Su Zhenyu
@*/
#ifndef _IR_GLOBAL_REFINE_H_
#define _IR_GLOBAL_REFINE_H_

namespace xoc {

//This class perform global refinement optimization.
//NOTE:global refinement does not maintain CFG related information.
class GlobalRefine : public Pass {
    COPY_CONSTRUCTOR(GlobalRefine);
protected:
    //Perform global refinement optimization to BB list.
    //BB list will be updated if optimization performed.
    //Return true if BB list changed.
    bool refineBBList(MOD BBList *, MOD OptCtx &)
    { return false; }

    //Refine the conditional branch operations for unsigned operands compared
    //with zero. Specifically (uxx: u8/u16/u32......):
    //
    //1. Refine the following IRs to NULL.
    //  truebr label label1               falsebr label label1
    //    lt:bool                   or      ge:bool
    //      $0:uxx                            $0:uxx
    //      intconst:uxx 0|0x0                intconst:uxx 0|0x0
    //To:
    //  NULL
    //
    //2. Refine the following IRs to unconditional branch operation.
    //  truebr label label1               falsebr label label1
    //    ge:bool                   or      lt:bool
    //      $0:uxx                            $0:uxx
    //      intconst:uxx 0|0x0                intconst:uxx 0|0x0
    //To:
    //  goto label label1
    //
    IR * refineBr(IR * ir, bool & change, MOD OptCtx & oc);
    IR * refineIR(IR * ir, bool & change, MOD OptCtx & oc);

    //Perform global refinement optimization to ir_list.
    //Return updated ir_list if optimization performed.
    IR * refineIRList(IR * ir_list, bool & change, MOD OptCtx & oc);
public:
    GlobalRefine(Region * rg) : Pass(rg) {}
    ~GlobalRefine() {}

    virtual CHAR const* getPassName() const { return "IR Global Refinement"; }
    virtual PASS_TYPE getPassType() const { return PASS_GLOBAL_REFINE; }

    virtual bool dump() const override;
    virtual bool perform(OptCtx & oc) override;
};

} //namespace xoc
#endif
