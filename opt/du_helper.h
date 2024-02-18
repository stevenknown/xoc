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
class LoopDepInfo;

//The class computes the number of DEF of MustRef and MayRef for each IR
//in given loop.
class ComputeMD2DefCnt {
    COPY_CONSTRUCTOR(ComputeMD2DefCnt);
protected:
    SMemPool * m_pool;
    Region const* m_rg;
    IRCFG const* m_cfg;
    LI<IRBB> const* m_li;
    MDSystem const* m_md_sys;
    typedef xcom::TMap<MD const*, UINT*> MD2UINTPtr;
    typedef xcom::TMapIter<MD const*, UINT*> MD2UINTPtrIter;
    MD2UINTPtr m_md2defcnt;
protected:
    void applyMayDefEffect(ConstIRList const& only_maydef);
    void computeBB(IRBB const* bb, OUT ConstIRList & only_maydef);

    //must: can be nullptr if there is no MustRef.
    void updateMDSet2DefCnt(MDSet const* may, MD const* must);
    void updateMD2DefCnt(IR const* ir, OUT ConstIRList & only_maydef);
    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != nullptr);
        void * p = smpoolMallocConstSize(sizeof(UINT), m_pool);
        ASSERT0(p != nullptr);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    ComputeMD2DefCnt(Region const* rg, LI<IRBB> const* li) : m_rg(rg), m_li(li)
    {
        m_cfg = rg->getCFG();
        m_md_sys = rg->getMDSystem();
        m_pool = smpoolCreate(4 * sizeof(UINT), MEM_CONST_SIZE);
    }
    ComputeMD2DefCnt() { smpoolDelete(m_pool); }

    void compute();

    void dump() const;

    //Return the count of definition of MD reference of 'ir' in given loop.
    UINT getMustRefDefCnt(IR const* ir) const;

    //Return the count of definition of 'md' in given loop.
    UINT getMDDefCnt(MD const* md) const;

    //Return true if ir is the unqiue DEF stmt of MustRef in the loop.
    bool isUniqueDef(IR const* ir) const;

    //Return true if ir is the unqiue DEF stmt of MustRef in the loop.
    //The function is not only check MustRef's DEF count but also the MD in
    //MayDef. Return true if both MustRef and MayRef have unique DEF.
    bool isUniqueDefStrict(IR const* ir) const;
};

//The function manipulates DU chain.
//The function will establish new DU chain between DEF of 'from' and
//expression 'to'.
//e.g:stpr $1=... #S1
//    ...=$1 + 1 $S2
//    ...=$1 - 3 #S3
//By giving 'from' is $1 in $S2, 'to' is $1 in S3, the function will add $1 in
//#S3 to be USE of #S1.
//to: root expression of target tree.
//from: root expression of source tree.
//NOTE:IR tree 'to' and 'from' must be isomorphic structure.
//     The function will NOT iterate sibling IR if 'from' is a IR list.
//Both 'to' and 'from' must be expression.
void addUseForTree(IR * to, IR const* from, Region * rg);

//The function manipulates DU chain.
//The function will establish new DU chain between DEF of 'from' and
//expression 'to'.
//e.g:stpr $1=... #S1
//    ...=$1 + 1 $S2
//    ...=$1 - 3 #S3
//By giving 'from' is $1 in $S2, 'to' is $1 in S3, the function will add $1 in
//#S3 to be USE of #S1.
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

//DU chain operation.
//The function changes DEF of PRSSA DU chain from 'olddef' to 'newdef'.
//Note the function only changes the DU chain for the USE in
//'partial_useset', the rest of IRs in use-set of 'olddef' will unchanged.
//olddef: original stmt.
//newdef: new stmt.
//e.g: given 'olddef'->USE, the result is 'newdef'->USE.
void changeDefForPartialUseSet(IR * olddef, IR * newdef,
                               IRSet const& partial_useset, Region * rg);

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
void collectUseSet(IR const* def, Region const* rg, OUT IRSet * useset);

//Collect all USE expressions that inside loop of 'def' into 'useset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain in doing collection.
void collectUseSetInLoop(IR const* def, Region const* rg, LI<IRBB> const* li,
                         OUT IRSet * useset);

//The function copy MDSSAInfo from 'src' to 'tgt'. Then add 'tgt' as an USE of
//the new MDSSAInfo.
void copyAndAddMDSSAOcc(IR * tgt, IR const* src, Region * rg);

//The function collects all DEF expressions of 'use' into 'defset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain when doing collection.
//The function will keep iterating DEF of PHI operand.
void collectDefSet(IR const* use, Region const* rg, OUT IRSet * defset);

//The function finds the nearest dominated DEF stmt of 'exp'.
//Note RPO of BB must be available.
//exp: given expression.
//defset: DEF stmt set of 'exp'.
//omit_self: true if we do not consider the stmt of 'exp' itself.
IR * findNearestDomDef(IR const* exp, IRSet const& defset, Region const* rg);
IR * findNearestDomDef(IR const* exp, Region const* rg);

//The function try to find the killing-def for 'use'.
//To find the killing-def, the function prefer use SSA info.
IR * findKillingDef(IR const* use, Region const* rg);

//Find the unique DEF of 'exp' that is inside given loop.
//set: it is optional, if it is not NULL, the function will record all DEF
//     found into the set as a return result.
IR * findUniqueDefInLoopForMustRef(IR const* exp, LI<IRBB> const* li,
                                   Region const* rg, OUT IRSet * set = nullptr);

//The function try to find the unique must-def for 'use'.
//Note must-def is the DEF that overlapped with 'use', but may not be
//killing-def.
//To find the killing-def, the function prefer use SSA info.
IR * findUniqueMustDef(IR const* use, Region const* rg);

//Note DOM info must be available.
//exp: the expression that expected to set livein.
//startir: the start position in 'startbb', it can be NULL.
//         If it is NULL, the function first finding the Phi list of
//         'startbb', then keep finding its predecessors until meet CFG entry.
//startbb: the BB that begin to do searching.
void findAndSetLiveInDef(IR * root, IR * startir, IRBB * startbb, Region * rg,
                         OptCtx const& oc);

//This function try to require VN of base of ir.
//Return the VN if found, and the indirect operation level.
//e.g: given ILD(ILD(p)), return p and indirect_level is 2.
//e.g2: given IST(ILD(q)), return q and indirect_level is 2.
VN const* getVNOfIndirectOp(IR const* ir, OUT UINT * indirect_level,
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
bool hasSameUniqueMustDefForIsomoKidTree(
    IR const* ir1, IR const* ir2, Region const* rg);

//Return true if both ir1 and ir2 have region livein def.
//ir1: expression.
//ir2: expression.
bool hasSameRegionLiveIn(IR const* ir1, IR const* ir2, Region const* rg);

//Return true if there is loop-reduce dependence between ir and its DEF.
//ir: exp or stmt.
bool hasLoopReduceDep(
    IR const* ir, Region const* rg, LI<IRBB> const* li, OUT LoopDepInfo & info);

//Return true if there is loop-reduce dependence between each IR that rooted
//by 'ir' and elements in 'lst'.
//ir: the root of IR tree, it may be exp or stmt.
//lst: a list of IR stmt
bool hasLoopReduceDepForIRTree(
    IR const* ir, Region const* rg, LI<IRBB> const* li);

//Return true if there are multiple-definition of MD reference represeted
//by 'ir' inside given loop 'li'.
//set: record the DEF stmt set of reduction variable.
bool hasUniqueDefInLoopForMustRef(
    IR const* ir, Region const* rg, LI<IRBB> const* li);

//Return true if there are more than one definition of MD reference represeted
//by 'ir' inside given loop 'li'.
//defcnt: return the count of definition found.
//        Note it may be equal to 0 if there is not any DEF in loop.
bool hasMoreThanOneDefInLoopForMustRef(
    IR const* ir, Region const* rg, LI<IRBB> const* li, OUT UINT & defcnt);

//Return true if def is killing-def of use.
//Note this function does not check if there is DU chain between def and use.
//gvn: if it is not NULL, the function will attempt to reason out the
//     relation between 'def' and 'use' through gvn info.
bool isKillingDef(IR const* def, IR const* use, GVN const* gvn);

//Return true if ir1's MD reference exactly cover ir2's MD reference.
//Note the function does not check if there is DU chain between ir1 and ir2.
//gvn:if it is not NULL, the function will attempt to reason out the
//    relation between 'ir1' and 'ir2' through gvn info.
bool isCover(IR const* ir1, IR const* ir2, GVN const* gvn);

//Return true if ir1's MD reference exactly cover md2.
//Note the functin does not check if there is DU chain between ir1 and md2.
bool isCover(IR const* ir1, MD const* md2);

//Return true if md1 exactly cover md2.
//Note the functin does not check if there is DU chain between md1 and md2.
bool isCover(MD const* md1, MD const* md2);

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
                       LI<IRBB> const* li, Region const* rg, GVN const* gvn);

//Return true if both ir1 and ir2 are in loop 'li', and there is at least
//loop-carried dependence between ir1 and ir2.
//ir1: stmt or expression.
//ir2: stmt or expression.
//costly_analysis: set to true if caller expect to compute overlap for
//                 element in MayDef/MayUse, which will iterate elements
//                 in MDSet, and is costly.
//li: loop info.
//Note the function does not check PR operation.
bool isLoopCarried(
    IR const* ir1, IR const* ir2, bool costly_analysis, LI<IRBB> const* li,
    Region const* rg, GVN const* gvn, OUT LoopDepInfo & info);

//Return true if there is loop-carried dependence between ir and stmt in loop.
//ir: exp or stmt.
//include_itselfstmt: True if the function ignores stmt that is parent of 'ir'.
//Note the function will check the dependence between ir and every stmt in
//given loop.
//Note the function does not check PR operation.
bool isLoopCarried(
    IR const* ir, Region const* rg, bool is_aggressive, bool include_itselfstmt,
    LI<IRBB> const* li, GVN const* gvn, OUT LoopDepInfo & info);

//Return true if there is loop-carried dependence between ir and elements in
//'lst'.
//ir: exp or stmt.
//include_itselfstmt: True if the function ignores stmt that is parent of 'ir'.
//lst: a list of IR stmt
//Note the function does not check PR operation.
bool isLoopCarried(
    IR const* ir, Region const* rg, bool is_aggressive, bool include_itselfstmt,
    xcom::List<IR*> const& lst, LI<IRBB> const* li, GVN const* gvn,
    OUT LoopDepInfo & info);

//Return true if there is loop-carried dependence between each IR that rooted
//by 'ir' and elements in 'lst'.
//ir: the root of IR tree, it may be exp or stmt.
//include_itselfstmt: True if the function ignores stmt that is parent of 'ir'.
//lst: a list of IR stmt
//Note the function does not check PR operation.
bool isLoopCarriedForIRTree(
    IR const* ir, Region const* rg, bool is_aggressive, bool include_itselfstmt,
    xcom::List<IR*> const& lst, LI<IRBB> const* li, GVN const* gvn,
    OUT LoopDepInfo & info);

//Return true if ir is the unique DEF of its must-ref MD in given loop.
//The function also consider the MayRef of each stmt in loop.
//e.g: loop {
//        t1 = .. //t1 reference MD10
//        t2 = .. //t1 reference MD22
//        *p = .. //*p reference MD21, MD22
//     }
//     t1 is the unique def of MD10, and t2 is NOT the unique def of MD22.
bool isUniqueDefInLoopForMustRef(
    IR const* ir, LI<IRBB> const* li, Region const* rg,
    MOD DefMiscBitSetMgr & sbsmgr);

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
//If S1 will be deleted, exp should be removed from its UseSet in MDSSAInfo,
//SSAInfo and Classic DU info.
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
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo,
//SSAInfo and Classic DU info.
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
