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
@*/
#ifndef _IR_DEBUG_CHECK_H_
#define _IR_DEBUG_CHECK_H_

namespace xoc {

class IR;

#define CASTCONST(ptr) (const_cast<IR*>(static_cast<IR const*>(ptr)))

#ifdef _DEBUG_
INT checkKidNumValid(IR const* ir, UINT n, CHAR const* file, INT lineno);
INT checkKidNumValidCall(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidArray(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidLoop(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidBranch(
    IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidTernary(
    IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidBinary(
    IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidUnary(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumIRCode(
    IR const* ir, UINT n, IR_CODE irc, CHAR const* filename, INT line);

//These functions check the consistency of the IR code and misellaneous
//attributes which corresponding the code.
IR * checkIRC(IR * ir, IR_CODE irc);
IR * checkIRCBranch(IR * ir);
IR * checkIRCCall(IR * ir);
IR * checkIRCArray(IR * ir);
IR * checkIRCOnlyCall(IR * ir);
IR * checkIRCOnlyICall(IR * ir);
UINT checkArrayDimension(IR const* ir, UINT n);
#endif

#ifdef _DEBUG_

//These macros define check-functions of IR in debug mode.
#define CK_KID_NUM(ir, n, f, l) (checkKidNumValid(ir, n, f, l))
#define CK_KID_NUM_IRC(ir, n, irc, f, l) \
    (checkKidNumIRCode(ir, n, irc, f, l))
#define CK_KID_NUM_UNA(ir, n, f, l) (checkKidNumValidUnary(ir, n, f, l))
#define CK_KID_NUM_BIN(ir, n, f, l) (checkKidNumValidBinary(ir, n, f, l))
#define CK_KID_NUM_TER(ir, n, f, l) (checkKidNumValidTernary(ir, n, f, l))
#define CK_KID_NUM_BR(ir, n, f, l) (checkKidNumValidBranch(ir, n, f, l))
#define CK_KID_NUM_LOOP(ir, n, f, l) (checkKidNumValidLoop(ir, n, f, l))
#define CK_KID_NUM_CALL(ir, n, f, l) (checkKidNumValidCall(ir, n, f, l))
#define CK_KID_NUM_ARR(ir, n, f, l) (checkKidNumValidArray(ir, n, f, l))
#define CK_IRC(ir, irc) (checkIRC(CASTCONST(ir), irc))
#define CK_IRC_BR(ir) (checkIRCBranch(CASTCONST(ir)))
#define CK_IRC_CALL(ir) (checkIRCCall(CASTCONST(ir)))
#define CK_IRC_ARR(ir) (checkIRCArray(CASTCONST(ir)))
#define CK_IRC_ONLY_CALL(ir) (checkIRCOnlyCall(CASTCONST(ir)))
#define CK_IRC_ONLY_ICALL(ir) (checkIRCOnlyICall(CASTCONST(ir)))
#define CK_ARRAY_DIM(ir, n) (checkArrayDimension(CASTCONST(ir), n))

#else

//These macros undefine check-functions of IR in release mode.
#define CK_KID_NUM(ir, n, f, l) (n)
#define CK_KID_NUM_IRC(ir, n, irc, f, l) (n)
#define CK_KID_NUM_UNA(ir, n, f, l) (n)
#define CK_KID_NUM_BIN(ir, n, f, l) (n)
#define CK_KID_NUM_TER(ir, n, f, l) (n)
#define CK_KID_NUM_BR(ir, n, f, l) (n)
#define CK_KID_NUM_LOOP(ir, n, f, l) (n)
#define CK_KID_NUM_CALL(ir, n, f, l) (n)
#define CK_KID_NUM_ARR(ir, n, f, l) (n)
#define CK_IRC(ir, irc) (CASTCONST(ir))
#define CK_IRC_BR(ir) (CASTCONST(ir))
#define CK_IRC_CALL(ir) (CASTCONST(ir))
#define CK_IRC_ARR(ir) (CASTCONST(ir))
#define CK_IRC_ONLY_CALL(ir) (CASTCONST(ir))
#define CK_IRC_ONLY_ICALL(ir) (CASTCONST(ir))
#define CK_ARRAY_DIM(ir, n) (n)

#endif

#define CK_KID_IRC(ir, irc, n) CK_KID_NUM_IRC(ir, n, irc, __FILE__, __LINE__)
#define CK_KID_BR(ir, n) CK_KID_NUM_BR(ir, n, __FILE__, __LINE__)
#define CK_KID_LOOP(ir, n) CK_KID_NUM_LOOP(ir, n, __FILE__, __LINE__)
#define CK_KID_UNA(ir, n) CK_KID_NUM_UNA(ir, n, __FILE__, __LINE__)
#define CK_KID_BIN(ir, n) CK_KID_NUM_BIN(ir, n, __FILE__, __LINE__)
#define CK_KID_TER(ir, n) CK_KID_NUM_TER(ir, n, __FILE__, __LINE__)
#define CK_KID_CALL(ir, n) CK_KID_NUM_CALL(ir, n, __FILE__, __LINE__)
#define CK_KID_ARR(ir, n) CK_KID_NUM_ARR(ir, n, __FILE__, __LINE__)

#define CK_KID(ir, CLASS, OPCODE, IDX) \
    ((static_cast<CLASS*>(CASTCONST(ir)))->opnd[CK_KID_IRC(ir, OPCODE, IDX)])

#define CK_CALL_KID(ir, CLASS, IDX) \
    ((static_cast<CLASS*>(CASTCONST(ir)))->opnd[CK_KID_CALL(ir, IDX)])

#define CK_FLD(ir, CLASS, OPCODE, FIELD) \
    ((static_cast<CLASS*> CK_IRT(ir, OPCODE))->FIELD)

#define CK_FLD_KIND(ir, CLASS, FUNC, FIELD) \
    ((static_cast<CLASS*>FUNC(ir))->FIELD)

} //namespace xoc

#endif
