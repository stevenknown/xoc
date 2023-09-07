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
#ifndef _DU_HELPER_H_
#define _DU_HELPER_H_

namespace xoc {

class VN;
class GVN;

//The function manipulates DU chain. It adds DU chain from tree 'from' to
//tree 'to',  and the function will establish new DU chain between DEF of
//'from' and expression 'to'.
//to: root expression of target tree.
//from: root expression of source tree.
//NOTE:IR tree 'to' and 'from' must be isomorphic structure.
//     The function will NOT iterate sibling IR if 'from' is a list.
//Both 'to' and 'from' must be expression.
void addUseForTree(IR * to, IR const* from, Region * rg);

//The function manipulates DU chain. It adds DU chain from 'from' to
//'to',  and the function will establish new DU chain between DEF of
//'from' and expression 'to'.
//to: target IR expression.
//from: source IR expression.
//Both 'to' and 'from' must be expression.
void addUse(IR * to, IR const* from, Region * rg);

//DU chain operation.
//The function builds DU chain from stmt 'def' to expression 'use'.
//def: stmt
//use: expression
void buildDUChain(IR * def, IR * use, Region * rg, OptCtx const& oc);

//DU chain operation.
//The function changes USE of some DU chain from 'olduse' to 'newuse'.
//newuse: indicates the target expression which is changed to.
//olduse: indicates the source expression which will be changed.
//e.g: given DU chain DEF->'olduse', the result is DEF->'newuse'.
//Note the function does NOT allow NonPR->PR or PR->NonPR.
void changeUse(IR * olduse, IR * newuse, Region * rg);

//DU chain operation.
//The function changes USE of some DU chain from 'olduse' to 'newuse'.
//newuse: indicates the target expression which is changed to.
//olduse: indicates the source expression which will be changed.
//defset: if the function meets CASE3, the defst will supply the Definition
//        Stmt Set that will be 'newuse'. The defset may NOT come
//        from 'olduse'.
//e.g: given DU chain DEF->'olduse', the result is DEF->'newuse'.
//Note the function allows NonPR->PR or PR->NonPR.
//The function is an extended version of 'changeUse'. The function will handle
//the combination of different category of Memory Reference, e.g: PR<->NonPR.
void changeUseEx(IR * olduse, IR * newuse, IRSet const* defset, Region * rg,
                 OptCtx const& oc);

//DU chain operation.
//The function changes DEF of some DU chain from 'olddef' to 'newdef'.
//olddef: original stmt.
//newdef: new stmt.
//e.g: given 'olddef'->USE, the result is 'newdef'->USE.
void changeDef(IR * olddef, IR * newdef, Region * rg);

//The function coalesces DU chain of 'from' to 'to'.
//The function replaces definition of USE of 'from' to defintion of 'to',
//just behaved like copy-propagation.
//from: indicates stmt operation, see example for detail.
//to: indicates expression operation, see example for detail.
//e.g: to_def =...
//     from = to //S1
//     ... = from_use
//=> after coalescing
//     to_def = ...
//     ------ //S1 removed
//     ... = to
void coalesceDUChain(IR * from, IR * to, Region * rg);

//The function collects all USE expressions into 'useset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain when doing collection.
void collectUseSet(IR const* def, MDSSAMgr const* mdssamgr, OUT IRSet * useset);

//The function copy MDSSAInfo from 'src' to 'tgt'. Then add 'tgt' as an USE of
//the new MDSSAInfo.
void copyAndAddMDSSAOcc(IR * tgt, IR const* src, Region * rg);

//The function collects all DEF expressions of 'use' into 'defset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain when doing collection.
//The function will keep iterating DEF of PHI operand.
void collectDefSet(IR const* use, MDSSAMgr const* mdssamgr, OUT IRSet * defset);

//The function finds the nearest dominated DEF stmt of 'exp'.
//Note RPO of BB must be available.
//exp: given expression.
//defset: DEF stmt set of 'exp'.
//omit_self: true if we do not consider the stmt of 'exp' itself.
IR * findNearestDomDef(IR const* exp, IRSet const& defset, Region const* rg,
                       bool omit_self);

//This function try to require VN of base of ir.
//Return the VN if found, and the indirect operation level.
//e.g: given ILD(ILD(p)), return p and indirect_level is 2.
//e.g2: given IST(ILD(q)), return q and indirect_level is 2.
VN const* getVNOfIndirectOp(IR const* ir, UINT * indirect_level,
                            GVN const* gvn);

//Return true if both ir1 and ir2 have same unique def.
//ir1: expression.
//ir2: expression.
bool hasSameUniqueMustDef(IR const* ir1, IR const* ir2, Region const* rg);

//Return true if both IR tree that start at ir1 and IR tree that start at ir2
//have same unique def.
//ir1: root expression.
//ir2: root expression.
bool hasSameUniqueMustDefForTree(IR const* ir1, IR const* ir2,
                                 Region const* rg);

//Return true if all kid of isomorphic IR tree that start at ir1 and ir2
//have same unique def.
//ir1: stmt or expression.
//ir2: stmt or expression.
bool hasSameUniqueMustDefForIsomoKidTree(IR const* ir1, IR const* ir2,
                                         Region const* rg);

//Return true if both ir1 and ir2 have region livein def.
//ir1: expression.
//ir2: expression.
bool hasSameRegionLiveIn(IR const* ir1, IR const* ir2, Region const* rg);

//Return true if def is killing-def of use.
//Note this function does not check if there is DU chain between def and use.
//gvn: if it is not NULL, the function will attempt to reason out the
//     relation between 'def' and 'use' through gvn info.
bool isKillingDef(IR const* def, IR const* use, GVN const* gvn);

//Return true if def is killing-def of usemd.
//Note this function does not check if there is DU chain between def and usemd.
bool isKillingDef(IR const* def, MD const* usemd);

//Return true if defmd is killing-def MD of usemd.
//Note this function does not check if there is DU chain between defmd
//and usemd.
bool isKillingDef(MD const* defmd, MD const* usemd);

//Return true if ir1 reference is may overlap to ir2.
//ir1: stmt or expression.
//ir2: stmt or expression.
//costly_analysis: set to true if caller expect to compute overlap for
//                 element in MayDef/MayUse, which will iterate elements
//                 in MDSet, and is costly.
bool isDependent(IR const* ir1, IR const* ir2, bool costly_analysis,
                 Region const* rg);

//Return true if ir1 reference is may overlap to whole IR tree that root at ir2.
//ir1: stmt or expression.
//ir2: the root stmt or expression of IR tree.
//costly_analysis: set to true if caller expect to compute overlap for
//                 element in MayDef/MayUse, which will iterate elements
//                 in MDSet, and is costly.
bool isDependentForTree(IR const* ir1, IR const* ir2, bool costly_analysis,
                        Region const* rg);

//Return true if ir1 is may overlap to phi.
bool isDependent(IR const* ir, MDPhi const* phi);

//Return true if both ir1 and ir2 are in loop 'li', and there is only
//loop-independent dependence between ir1 and ir2, not loop-carried dependence.
//ir1: stmt or expression.
//ir2: stmt or expression.
//costly_analysis: set to true if caller expect to compute overlap for
//                 element in MayDef/MayUse, which will iterate elements
//                 in MDSet, and is costly.
//li: loop info.
bool isLoopIndependent(IR const* ir1, IR const* ir2, bool costly_analysis,
                       LI<IRBB> const* li, Region const* rg);

//The function try to find the killing-def for 'use'.
//To find the killing-def, the function prefer use SSA info.
IR * findKillingDef(IR const* use, Region const* rg);

//The function try to find the unique must-def for 'use'.
//Note must-def is the DEF that overlapped with 'use', but may not be
//killing-def.
//To find the killing-def, the function prefer use SSA info.
IR * findUniqueMustDef(IR const* use, Region const* rg);

//Note DOM info must be available.
//exp: the expression that expected to set livein.
//startir: the start position in 'startbb', it can be NULL.
//         If it is NULL, the function first finding the Phi list of
//         'startbb', then keep finding its predecessors until the
//         CFG entry.
//startbb: the BB that begin to do searching.
void findAndSetLiveInDef(IR * root, IR * startir, IRBB * startbb, Region * rg,
                         OptCtx const& oc);

//The function checks each DEF|USE occurrence of ir, remove the expired
//expression which is not reference the memory any more that ir referenced.
//Return true if DU changed.
//e.g: stpr1 = ... //S1
//     ..... = pr2 //S2
//  If there is DU between S1 and S2, cutoff the DU chain.
//stmt: PR write operation.
//exp: PR read operation.
bool removeExpiredDU(IR const* ir, Region * rg);

//Remove Use-Def chain for all memory references in IR Tree that rooted by exp.
//exp: it is the root of IR tree that to be removed.
//e.g: ir = ...
//        = exp //S1
//If S1 will be deleted, exp should be removed from its UseSet in MDSSAInfo.
//NOTE: If exp is an IR tree, e.g: ild(x, ld(y)), remove ild(x) means
//ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
//updated as well.
void removeUseForTree(IR const* exp, Region * rg, OptCtx const& oc);

//The function remove classic DU chain for region's IRList or IRBBList.
//rmprdu: true to remove all PR operations classic DU chain.
//rmnonprdu: true to remove all NonPR operations classic DU chain.
void removeClassicDUChain(Region * rg, bool rmprdu, bool rmnonprdu);

//Remove Use-Def chain.
//exp: the expression to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: the function only process exp itself.
void removeUse(IR const* exp, Region * rg);

//Remove all DU info of 'stmt'.
//Note do NOT remove stmt from BBIRList before call the function.
void removeStmt(IR * stmt, Region * rg, OptCtx const& oc);

//The function moves IR_PHI and MDPhi from 'from' to 'to'.
//This function often be used in updating PHI when adding new dominater BB
//to 'to'.
void movePhi(IRBB * from, IRBB * to, Region * rg);

//The function try to destruct classic DU chain according to the
//options in 'oc'. Usually, PRSSA will clobber the classic DUSet information.
//User call the function to avoid the misuse of PRSSA and classic DU in the
//meanwhile.
void destructClassicDUChain(Region * rg, OptCtx const& oc);

} //namespace xoc
#endif
