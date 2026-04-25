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
#ifndef _IR_EVN_H_
#define _IR_EVN_H_

namespace xoc {

template <typename IntType>
class IntSet2VN : public IntSetMap<IntType, VN*> {
public:
    //The target dependent code to dump the content of user defined
    //MappedObject.
    virtual void dumpMappedObj(FILE * h, UINT indent, VN * const& mapped) const
    {
        if (mapped == nullptr) { return; }
        xcom::log(h, 0, ":VN%u", mapped->id());
    }
};


//The class generates an unique VN by registering a leading IR code and
//a list VN id followed.
//NOTE the first integer must be IR_CODE, and the following integer must be
//VN id.
//e.g: given IR_ADD, VN1, VN2, the class will generate VN3.
class IRCAndVNHash {
public:
    typedef HOST_UINT IntType;
    typedef xcom::List<IntType> IntList;
    typedef xcom::List<IntType>::Iter IntListIter;
    typedef xcom::List<VN const*> VNList;
    typedef xcom::List<VN const*>::Iter VNListIter;
protected:
    GVN * m_gvn;
    IntSet2VN<IntType> m_intset2vn;
protected:
    VN * registerVN(IntList const& ilst);
public:
    IRCAndVNHash(GVN * gvn) : m_gvn(gvn) {}
    ~IRCAndVNHash();
    void clean();
    void dump(Region const* rg, UINT indent) const;

    //The function register VN by given a list of VN id.
    //irt: the operation code.
    //vnnum: the number of VN id.
    //...: A variable number of VN id, each VN id's type should be VNHashInt.
    VN const* registerVN(IR_CODE irt, UINT vnnum, ...);
    VN const* registerVN(IR_CODE irt, MOD IntList & ilst);
};
typedef IRCAndVNHash::IntType VNHashInt;

class InferCtx {
protected:
    xcom::TTab<UINT> m_mdphi_tab; //temp usage.
    xcom::TTab<UINT> m_irtab; //temp usage.
    ActMgr * m_am;
    OptCtx const& m_oc;
public:
    InferCtx(OptCtx const& oc, ActMgr * am = nullptr) : m_am(am), m_oc(oc) {}

    //The function clean the temp variables for next inference.
    void clean()
    {
        m_mdphi_tab.clean();
        m_irtab.clean();
    }

    ActMgr * getActMgr() const { return m_am; }
    OptCtx const& getOptCtx() const { return m_oc; }

    //The function is used to avoid accessing MDPhi in a cycle.
    bool isVisited(MDPhi const* phi) const
    { return m_mdphi_tab.find(phi->id()); }

    //The function is used to avoid accessing IR in a cycle.
    bool isVisited(IR const* ir) const
    { return m_irtab.find(ir->id()); }

    void setVisited(MDPhi const* phi) { m_mdphi_tab.append(phi->id()); }
    void setVisited(IR const* ir) { m_irtab.append(ir->id()); }
};


//The class inferences Equivalent-VN via walk through DefUse chain.
//Equivalent-VN describes a kind of VN comparasion strategy when determining
//whether two given IR exps's value are equal.
//The passes can reason out that two IR expressions have same runtime value
//if their Equivalent-VN are the same one.
//ONE KEY NOTE: the Equivalent-VN is different to the normal VN that GVN
//computed in usually.
//If given two IR expressions's Equivalent-VN is different, we can NOT
//exactly conclude the IR expressions have different runtime value.
//On the contrary, GVN's VN comparison strategy could exactly say the IR
//expressions have different runtime value.
//e.g: given two IR expressions ld x and $y, if InferEVN reasoned out that
//ld x has EVN1, and $y has EVN2, we just tell user that we have no knowledge
//about ld x and $y's value.
//NOTE: the EVN's inference may utilize GVN's VN if exist to infer EVN.
class InferEVN {
    COPY_CONSTRUCTOR(InferEVN);
protected:
    GVN * m_gvn;
    Region * m_rg;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    IR2VN m_irid2vn;
    PRNO2VN m_prno2vn;
    VMD2VN m_vmd2vn;
    MDPhi2VN m_mdphi2vn;
    IRCAndVNHash m_ircvnhash;
protected:
    //Return true if the inference will try to infer Phi's EVN via inferring
    //each operands of Phi respectively.
    virtual bool allowCrossPhi() { return true; }

    //The function allocates a VN given stmt.
    VN const* allocVNForStmt(IR const* ir, InferCtx & ctx);
    VN const* allocVNForMDDef(MDDef const* mddef, InferCtx & ctx);
    VN const* allocVNForVMD(VMD const* vmd, InferCtx & ctx);
    VN const* allocVNForPRNO(PRNO prno);

    //The function allocates a VN given extended stmt.
    virtual VN const* allocVNForExtStmt(IR const* ir, InferCtx & ctx);

    //Return true if there exist aliases between memory-operations that are
    //composed of different IR structures.
    bool mayExistAlias() const
    {
        //CASE:exec/evn.c
        //...=ild($1) #S1
        //ist($2)=...
        //...=ild($1) #S2
        //VN of ild($1) in #S1 can NOT be inferred through $1 and ILD code
        //because ist($2) may alias with ild($1).
        return true;
    }

    VN const* getVN(IR const* ir) { return m_irid2vn.get(ir->id()); }
    VN const* getVN(PRNO prno) { return m_prno2vn.get(prno); }
    VN const* getVN(MDDef const* mddef)
    { return m_mdphi2vn.get(mddef->id()); }
    VN const* getVN(VMD const* vmd) { return m_vmd2vn.get(vmd->id()); }

    //Return true if ir has a valid corresponded VN.
    bool hasVN(IR const* ir) const
    { return const_cast<InferEVN*>(this)->getVN(ir) != nullptr; }

    VN const* inferIntConst(HOST_INT val);
    VN const* inferConst(IR const* ir, InferCtx & ctx);
    virtual VN const* inferExtStmt(IR const* ir, InferCtx & ctx);
    virtual VN const* inferExtExp(IR const* ir, InferCtx & ctx);
    VN const* inferDirectStmt(IR const* ir, InferCtx & ctx);
    VN const* inferIndirectStmt(IR const* ir, InferCtx & ctx);
    VN const* inferLiveinVMDForDirectExp(IR const* ir, InferCtx & ctx);
    VN const* inferVNByIterKid(IR const* ir, InferCtx & ctx);
    VN const* inferArrayKidOp(IR const* ir, InferCtx & ctx);
    VN const* inferWriteArray(IR const* ir, InferCtx & ctx);
    VN const* inferStmt(IR const* ir, InferCtx & ctx);
    VN const* inferMDPhi(MDPhi const* phi, InferCtx & ctx);
    VN const* inferVNViaBaseAndOfst(IR const* ir, InferCtx & ctx);
    VN const* inferVNViaArrayKidAndOfst(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExp(IR const* ir, InferCtx & ctx);
    VN const* inferArray(IR const* ir, InferCtx & ctx);
    VN const* inferIndirectMemExp(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExpViaMDPhi(
        IR const* ir, MDDef const* mdssadef, InferCtx & ctx);
    VN const* inferDirectExpViaMDSSA(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExpViaPRSSA(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExpViaSSA(IR const* ir, InferCtx & ctx);

    //The function try to infer VN for given killdef, which is
    //killing-definition of 'exp'. If there is not an available VN, the
    //function generates a dedicated VN for 'killdef'.
    //NOTE: User has to guarantee that 'killdef' must be the killing-def
    //of 'exp'.
    VN const* inferAndGenVNForKillingDef(
        IR const* exp, IR const* killdef, InferCtx & ctx);

    //This function infers the VN of cvt IR.
    VN const* inferCvt(IR const* cvt, InferCtx & ctx);

    //The function maps given mddef information into an unique IR_CODE.
    //The mapped ir-code is used to conform the hashing-rules when registers
    //a MDDef and a set of integers.
    IR_CODE mapMDDef2IRCode(MDDef const* mddef) const;

    //Register the VN for CVT via the input vn, and the type of src and the
    //target for the cvt operation.
    VN const* registerCvtVN(VN const* v0, Type const* srcty, Type const* tgtty);

    //Register the VN for cvt IR by the three input VNs.
    VN const* registerCvtVN(VN const* v0, VN const* v1, VN const* v2);

    //IR may be set an initial VN through DU chain, then the IR's vn
    //will be re-updated when the recursive processing of its kid IR finished.
    //e.g:
    //  BB2:
    //  phi $10 = $11, $12;
    //  falsebr BB3;
    //  BB3:
    //  stpr $12 = add $10, 1; #S1
    //  goto BB2;
    //At first, the VN of phi is set to VN6(OP) because it is kill-def
    //of $10 in #S1, then updated to VN8(OP) when all its two operands $11 and
    //$12's VN are set.
    void setVNAlways(IR const* ir, VN const* vn)
    {
        ASSERT0(vn == nullptr || !vn->is_unknown());

        //NOTE: each IR can be set only once, user has to check VN before
        //inoke the function.
        m_irid2vn.setAlways(ir->id(), vn);
    }
    void setVN(PRNO prno, VN const* vn)
    {
        ASSERT0(vn == nullptr || !vn->is_unknown());
        //NOTE: each IR can be set only once, user has to check VN before
        //inoke the function.
        ASSERT0(getVN(prno) == nullptr || getVN(prno) == vn);
        m_prno2vn.set(prno, vn);
    }

    //NOTE ir's VN could be set multiple times but should be unique.
    void setVN(IR const* ir, VN const* vn)
    {
        ASSERT0(vn && !vn->is_unknown());
        //NOTE: each IR can be set only unique VN, user has to check VN before
        //inoking the function.
        ASSERT0(getVN(ir) == nullptr || getVN(ir) == vn);
        m_irid2vn.setAlways(ir->id(), vn);
    }
    void setVN(MDDef const* mddef, VN const* vn)
    {
        ASSERT0(!vn->is_unknown());
        //NOTE: each MDDef can be set only unique VN, user has to check VN
        //before inoking the function.
        ASSERT0(getVN(mddef) == nullptr || getVN(mddef) == vn);
        m_mdphi2vn.setAlways(mddef->id(), vn);
    }
    void setVN(VMD const* vmd, VN const* vn)
    {
        ASSERT0(!vn->is_unknown());
        //NOTE: each MDDef can be set only unique VN, user has to check VN
        //before inoking the function.
        ASSERT0(getVN(vmd) == nullptr || getVN(vmd) == vn);
        m_vmd2vn.setAlways(vmd->id(), vn);
    }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    InferEVN(GVN * gvn);
    virtual ~InferEVN() {}

    //Clean ir related VN info.
    void cleanVN(IR const* ir) { setVNAlways(ir, nullptr); }
    void cleanVNIRTree(IR const* ir);
    void clean();

    //filename: dump BB list into given filename.
    void dumpBBListWithEVN(CHAR const* filename);
    void dumpBBListWithEVN() const;
    void dump() const;
    void dumpIR2VN() const;
    void dumpForTest() const;

    MDSSAMgr * getMDSSAMgr() const { return m_mdssamgr; }
    PRSSAMgr * getPRSSAMgr() const { return m_prssamgr; }
    Region * getRegion() const { return m_rg; }
    TypeMgr * getTypeMgr() const { return m_rg->getTypeMgr(); }

    VN const* inferExp(IR const* ir, InferCtx & ctx);
    bool is_valid() const;
};

} //namespace xoc
#endif
