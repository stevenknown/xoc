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

#ifdef _DEBUG_
class IR;
INT checkKidNumValid(IR const* ir, UINT n, CHAR const* file, INT lineno);
INT checkKidNumValidCall(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidArray(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidLoop(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidBranch(IR const* ir, UINT n, CHAR const* filename,
                           INT line);
INT checkKidNumValidBinary(IR const* ir, UINT n, CHAR const* filename,
                           INT line);
INT checkKidNumValidUnary(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumIRtype(IR const* ir, UINT n, IR_TYPE irty, CHAR const* filename,
                      INT line);
IR const* checkIRT(IR const* ir, IR_TYPE irt);
IR const* checkIRTBranch(IR const* ir);
IR const* checkIRTCall(IR const* ir);
IR const* checkIRTArray(IR const* ir);
IR const* checkIRTOnlyCall(IR const* ir);
IR const* checkIRTOnlyICall(IR const* ir);
UINT checkArrayDimension(IR const* ir, UINT n);
#endif

#ifdef _DEBUG_
#define CK_KID_NUM(ir, n, f, l) (checkKidNumValid(ir, n, f, l))
#define CK_KID_NUM_IRTY(ir, n, irty, f, l) \
    (checkKidNumIRtype(ir, n, irty, f, l))
#define CK_KID_NUM_UNA(ir, n, f, l) (checkKidNumValidUnary(ir, n, f, l))
#define CK_KID_NUM_BIN(ir, n, f, l) (checkKidNumValidBinary(ir, n, f, l))
#define CK_KID_NUM_BR(ir, n, f, l) (checkKidNumValidBranch(ir, n, f, l))
#define CK_KID_NUM_LOOP(ir, n, f, l) (checkKidNumValidLoop(ir, n, f, l))
#define CK_KID_NUM_CALL(ir, n, f, l) (checkKidNumValidCall(ir, n, f, l))
#define CK_KID_NUM_ARR(ir, n, f, l) (checkKidNumValidArray(ir, n, f, l))
#define CK_IRT(ir, irt) (checkIRT(ir, irt))
#define CK_IRT_BR(ir) (checkIRTBranch(ir))
#define CK_IRT_CALL(ir) (checkIRTCall(ir))
#define CK_IRT_ARR(ir) (checkIRTArray(ir))
#define CK_IRT_ONLY_CALL(ir) (checkIRTOnlyCall(ir))
#define CK_IRT_ONLY_ICALL(ir) (checkIRTOnlyICall(ir))
#define CK_ARRAY_DIM(ir, n) (checkArrayDimension(ir, n))
#else
#define CK_KID_NUM(ir, n, f, l) (n)
#define CK_KID_NUM_IRTY(ir, n, irty, f, l) (n)
#define CK_KID_NUM_UNA(ir, n, f, l) (n)
#define CK_KID_NUM_BIN(ir, n, f, l) (n)
#define CK_KID_NUM_BR(ir, n, f, l) (n)
#define CK_KID_NUM_LOOP(ir, n, f, l) (n)
#define CK_KID_NUM_CALL(ir, n, f, l) (n)
#define CK_KID_NUM_ARR(ir, n, f, l) (n)
#define CK_IRT(ir, irt) (ir)
#define CK_IRT_BR(ir) (ir)
#define CK_IRT_CALL(ir) (ir)
#define CK_IRT_ARR(ir) (ir)
#define CK_IRT_ONLY_CALL(ir) (ir)
#define CK_IRT_ONLY_ICALL(ir) (ir)
#define CK_ARRAY_DIM(ir, n) (n)
#endif

#define CKID_TY(ir, irty, n) CK_KID_NUM_IRTY(ir, n, irty, __FILE__, __LINE__)
#define CKID_BR(ir, n) CK_KID_NUM_BR(ir, n, __FILE__, __LINE__)
#define CKID_LOOP(ir, n) CK_KID_NUM_LOOP(ir, n, __FILE__, __LINE__)
#define CKID_UNA(ir, n) CK_KID_NUM_UNA(ir, n, __FILE__, __LINE__)
#define CKID_BIN(ir, n) CK_KID_NUM_BIN(ir, n, __FILE__, __LINE__)
#define CKID_CALL(ir, n) CK_KID_NUM_CALL(ir, n, __FILE__, __LINE__)
#define CKID_ARR(ir, n) CK_KID_NUM_ARR(ir, n, __FILE__, __LINE__)


