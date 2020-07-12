/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
#ifndef _DU_HELPER_H_
#define _DU_HELPER_H_

namespace xoc {

//DU chain operation.
//Build DU chain : def->use.
void addDUChain(IR * def, IR * use, Region * rg);

//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//'newuse': indicate the target expression which change to.
//'olduse': indicate the source expression which will be changed.
//e.g: given DU chain DEF->'olduse', change to DEF->'newuse'.
void changeUse(IR * olduse, IR * newuse, Region * rg);

//DU chain operation.
//Change Def stmt from 'olddef' to 'newdef'.
//'olddef': original stmt.
//'newdef': new stmt.
//e.g: given 'olddef'->USE, change to 'newdef'->USE.
void changeDef(IR * olddef, IR * newdef, Region * rg);

//Check each USE of stmt, remove the expired one which is not reference
//the memory any more that stmt defined.
//Return true if DU changed.
bool removeExpiredDUForStmt(IR * stmt, Region * rg);

//Remove Use-Def chain.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means
//ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
//updated as well.
void removeUse(IR * ir, Region * rg);

//Coalesce DU chain of 'from' to 'to'.
//This function replace definition of USE of 'from' to defintion of 'to'.
//Just like copy-propagation.
//e.g: to_def =...
//     from = to
//     ...= from_use
//=> after coalescing
//     to_def = ...
//     ------ //removed
//     ... = to
void coalesceDUChain(IR * from, IR * to, Region * rg);

//DU chain and Memory Object reference operation.
//This function copy MustUse and MayUse mds from tree 'from' to tree 'to'
//and build new DU chain for 'to'.
//add_duchain: if true to add DU chain from tree 'from' to tree 'to'.
//    this operation will establish new DU chain between the DEF of 'from' and
//    'to'.
//'to': root expression of target tree.
//'from': root expression of source tree.
//NOTE: IR tree 'to' and 'from' must be identical structure.
//'to' and 'from' must be expression.
void copyRefAndAddDUChain(IR * to,
                          IR const* from,
                          Region * rg,
                          bool add_duchain);

} //namespace xoc
#endif
