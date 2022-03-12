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
#ifndef _LFTR_H_
#define _LFTR_H_

namespace xoc {

class IV;
class BIV;
class IVR;

//This class represents linear function test replacement optimization.

class LFTR : public Pass {
    COPY_CONSTRUCTOR(LFTR);
protected:

    class LFRInfo {
    public:
        IR * lf_exp; //record the linear-function expression.
        IV const* ivinfo; //record the IV that correspond to current LF.
        IR const* ivcoeff; //record the coefficient of IV in LF.

        //record the new DIV that used to displace LF. It may be PR or STPR.
        IR * new_div;

        //record the stmt that compute the initiail value of new DIV.
        IR * init_stmt;

        BIV const* getBIVInfo() const { return (BIV const*)ivinfo; }
        HOST_INT getIVCoeff() const { return CONST_int_val(ivcoeff); }
        bool is_init() const { return lf_exp != nullptr; }
    };

    SMemPool * m_pool;
    IVR * m_ivr;
    IRCFG * m_cfg;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    DUMgr * m_dumgr;
    IRIter m_iriter; //for local using
    IRList m_cand_list; //record the IR expression that to be displaced.
    TMap<MD const*, IV const*> m_ivmd2ivinfo; //cache IV for next use

    typedef TMapIter<IR const*, LFRInfo*> LFRInfoMapIter;
    typedef TMap<IR const*, LFRInfo*> LFRInfoMap;
    LFRInfoMap m_cand_occ2info;
protected:
    void analyzeBB(LI<IRBB> * li, IRBB * bb);
    void analysis(LI<IRBB> * li);
    LFRInfo * allocLFRInfo() { return (LFRInfo*)xmalloc(sizeof(LFRInfo)); }
    void addDUChainForRHSOfInitDef(IR * newrhs, IR const* oldrhs,
                                   LI<IRBB> const* li);

    void buildClassicDUForRed(IR * init, IR * red);

    void clean();

    //doReplacement candidate IRs to preheader BB.
    //This function will maintain RPO if new BB inserted.
    //Return true if BB or STMT changed.
    bool doReplacement(OUT IRBB * prehead, OUT LI<IRBB> * li,
                             OUT bool & insert_bb);
    //Return true if code changed.
    bool doLoopTree(LI<IRBB> * li, OUT bool & du_set_info_changed,
                    OUT bool & insert_bb, OptCtx & oc);

    //Generate IV reference.
    IR * genNewIVRef(LFRInfo const* info);
    void getLinearRepOfIV(IR const* lf_exp, IV const** iv, IR const** coeff);
    LFRInfo * genLFRInfo(IR const* lf_exp)
    {
        LFRInfo * info = m_cand_occ2info.get(lf_exp);
        if (info == nullptr) {
            info = allocLFRInfo();
            m_cand_occ2info.set(lf_exp, info);
        }
        return info;
    }

    //Insert PHI at head BB of loop for given reduction operation.
    //red: reduction that is inserted at loop.
    IR * insertPhiForRed(LI<IRBB> const* li, IR * init, IR * red);
    bool insertReductionCode(List<LFRInfo*> & lfrinfo_list);
    bool insertComputingInitValCode(IRBB * preheader,
                                    List<LFRInfo*> const& lfrinfo_list,
                                    LI<IRBB> const* li);

    void pickupProperCandidate(OUT List<LFRInfo*> & lfrinfo_list);
    bool replaceCandByIV();

    void * xmalloc(INT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit LFTR(Region * rg) :
        Pass(rg), m_pool(nullptr), m_ivr(nullptr), m_prssamgr(nullptr),
        m_mdssamgr(nullptr)
    {
        ASSERT0(rg != nullptr);
        m_cfg = rg->getCFG();
        m_pool = smpoolCreate(sizeof(LFRInfo) * 4, MEM_COMM);
    }
    virtual ~LFTR() { smpoolDelete(m_pool); }

    bool dump(LI<IRBB> const* li) const;

    virtual CHAR const* getPassName() const
    { return "Linear Function Test Replacement"; }
    PASS_TYPE getPassType() const { return PASS_LFTR; }
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
