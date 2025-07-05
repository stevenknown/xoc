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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

class VFCompareEQ {
public:
    bool find;
    IR const* compared_ir;
public:
    VFCompareEQ() { find = false; }
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        if (ir == compared_ir) {
            //Visiting will terminated immedately.
            find = true;
            is_terminate = true;
            return false;
        }
        //Keep visiting the kid and sibling.
        return true;
    }
};


static bool isMultiRes(IR const* exp, IR const* reslist)
{
    class IterTree : public VisitIRTree<VFCompareEQ> {
    public:
        IterTree(VFCompareEQ & vf) : VisitIRTree(vf) {}
    };
    VFCompareEQ vf;
    vf.compared_ir = exp;
    IterTree it(vf);
    it.visit(reslist);
    return vf.find;
}


//
//Start CBroadCast.
//
bool CBroadCast::isResList(IR const* exp) const
{
    ASSERT0(exp->is_exp());
    return isMultiRes(exp, getResList());
}
//End CBroadCast.


//
//Start CAtomInc.
//
bool CAtomInc::isResList(IR const* exp) const
{
    ASSERT0(exp->is_exp());
    return isMultiRes(exp, getResList());
}
//End CAtomInc.


//
//Start CAtomCas.
//
bool CAtomCas::isResList(IR const* exp) const
{
    ASSERT0(exp->is_exp());
    return isMultiRes(exp, getResList());
}
//End CAtomCas.

} //namespace xoc
