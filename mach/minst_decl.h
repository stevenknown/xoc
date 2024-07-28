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
#ifndef _MINST_DECL_H_
#define _MINST_DECL_H_

namespace mach {

#define MI_lab(mi) (((LabelMInst*)mi)->m_lab)
class LabelMInst : public MInst {
    COPY_CONSTRUCTOR(LabelMInst);
public:
    xoc::LabelInfo const* m_lab;
};


#define MI_var(mi) (((MemAccMInst*)mi)->m_var)
class MemAccMInst : public MInst {
    COPY_CONSTRUCTOR(MemAccMInst);
public:
    xoc::Var const* m_var;
};

//The following are MI instructions related to DWARF
//This class is a base class. It contains a union representing either
//an offset or register2. The "2" signifies that in the future,
//when derived classes inherit from it, they will have their own register,
//which may cause repetition. For example,
//when MCCFICOffsetIns inherits from MCCCFIInst,
//it will have its own m_register and m_offset.
class MCCCFIInst : public MInst {
    COPY_CONSTRUCTOR(MCCCFIInst);
public:
    union {
        INT m_offset;
        UINT m_register2;
    };
};

//.cfi_def_cfa defines a rule for computing CFA as: take address from
//Register and add Offset to it.
#define MI_cfi_def_cfa_offset(mi)   (((MCCCFIDefCfaIns*)(mi))->m_offset)
#define MI_cfi_def_cfa_register(mi) (((MCCCFIDefCfaIns*)(mi))->m_register)
class MCCCFIDefCfaIns : public MCCCFIInst {
    COPY_CONSTRUCTOR(MCCCFIDefCfaIns);
public:
    UINT m_register;
};

//.cfi_same_value Current value of Register is the same as in the
//previous frame. I.e., no restoration is needed.
#define MI_cfi_samevalue_register(mi) \
    (((MCCCFISameValueIns*)(mi))->m_register2)
class MCCCFISameValueIns : public MCCCFIInst {
    COPY_CONSTRUCTOR(MCCCFISameValueIns);
};

//.cfi_offset Previous value of Register is saved at offset Offset
//from CFA.
#define MI_cfi_offset_offset(mi)   (((MCCFICOffsetIns*)(mi))->m_offset)
#define MI_cfi_offset_register(mi) (((MCCFICOffsetIns*)(mi))->m_register)
class MCCFICOffsetIns : public MCCCFIInst {
    COPY_CONSTRUCTOR(MCCFICOffsetIns);
public:
    UINT m_register;
};

//.cfi_restore says that the rule for Register is now the same as it
//was at the beginning of the function, after all initial instructions added
//by .cfi_startproc were executed.
#define MI_cfi_restore_register(mi) \
    (((MCCCFIRestoreInst*)(mi))->m_register2)
class MCCCFIRestoreInst : public MCCCFIInst {
    COPY_CONSTRUCTOR(MCCCFIRestoreInst);
};

//.cfi_def_cfa_offset modifies a rule for computing CFA. Register
//remains the same, but offset is new. Note that it is the absolute offset
//that will be added to a defined register to the compute CFA address.
#define MI_cfi_def_cfa_offset_offset(mi) \
    (((MCCFIDefCfaOffsetInst*)(mi))->m_offset)
class MCCFIDefCfaOffsetInst : public MCCCFIInst {
    COPY_CONSTRUCTOR(MCCFIDefCfaOffsetInst);
};

} //namespace

#endif
