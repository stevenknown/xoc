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
#ifndef __IR_VERIFY_H__
#define __IR_VERIFY_H__

namespace xoc {

bool verifyConst(IR const* ir, Region const* rg);
bool verifyGeneral(IR const* ir, Region const* rg);
bool verifyNothing(IR const* ir, Region const* rg);
bool verifyLD(IR const* ir, Region const* rg);
bool verifyST(IR const* ir, Region const* rg);
bool verifySTPR(IR const* ir, Region const* rg);
bool verifyILD(IR const* ir, Region const* rg);
bool verifyIST(IR const* ir, Region const* rg);
bool verifySETELEM(IR const* ir, Region const* rg);
bool verifyGETELEM(IR const* ir, Region const* rg);
bool verifyLDA(IR const* ir, Region const* rg);
bool verifyCALL(IR const* ir, Region const* rg);
bool verifyICALL(IR const* ir, Region const* rg);
bool verifyShift(IR const* ir, Region const* rg);
bool verifyADD(IR const* ir, Region const* rg);
bool verifyCompare(IR const* ir, Region const* rg);
bool verifyBin(IR const* ir, Region const* rg);
bool verifyLNOT(IR const* ir, Region const* rg);
bool verifyUna(IR const* ir, Region const* rg);
bool verifyGOTO(IR const* ir, Region const* rg);
bool verifyIGOTO(IR const* ir, Region const* rg);
bool verifyLoopCFS(IR const* ir, Region const* rg);
bool verifyIF(IR const* ir, Region const* rg);
bool verifySWITCH(IR const* ir, Region const* rg);
bool verifyCASE(IR const* ir, Region const* rg);
bool verifyArrayOp(IR const* ir, Region const* rg);
bool verifyCVT(IR const* ir, Region const* rg);
bool verifyPR(IR const* ir, Region const* rg);
bool verifyBranch(IR const* ir, Region const* rg);
bool verifyRETURN(IR const* ir, Region const* rg);
bool verifySELECT(IR const* ir, Region const* rg);
bool verifyPHI(IR const* ir, Region const* rg);

} //namespace xoc
#endif

#include "ir_verify_ext.h"
