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
class LFRInfo;

typedef TMapIter<IR const*, LFRInfo*> LFRInfoMapIter;
typedef TMap<IR const*, LFRInfo*> LFRInfoMap;

class LFRInfo {
public:
    IR * lf_exp; //record the linear-function expression.
    IV const* ivinfo; //record the IV that correspond to current LF.
    IR const* ivcoeff; //record the coefficient of IV in LF.
    //record the new DIV that used to displace LF. It may be PR or STPR.
    IR * new_div;

    //record the stmt that compute the initiail value of new DIV.
    IR * init_stmt;
public:
    BIV const* getBIVInfo() const { return (BIV const*)ivinfo; }
    HOST_INT getIVCoeff() const
    {
        ASSERT0(ivcoeff->is_const() && ivcoeff->is_int());
        return CONST_int_val(ivcoeff);
    }

    void init(IR * plfexp, IV const* pivinfo, IR const* pcoeff)
    { lf_exp = plfexp; ivinfo = pivinfo; ivcoeff = pcoeff; }
    bool is_init() const { return lf_exp != nullptr; }
};


class LFAnaCtx {
    LI<IRBB> const* m_li;
    SMemPool * m_pool;
    OptCtx const& m_oc;
    IRList m_cand_list; //record the IR expression that to be displaced.
    LFRInfoMap m_cand_occ2info;
    TMap<MD const*, IV const*> m_ivmd2ivinfo; //cache IV for next use
private:
    void * xmalloc(INT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }
    LFRInfo * allocLFRInfo();
public:
    LFAnaCtx(OptCtx const& oc, LI<IRBB> const* li);
    ~LFAnaCtx() { smpoolDelete(m_pool); }

    void addCandLF(IR * x) { m_cand_list.append_tail(x); }

    void dump(Region * rg) const;

    IV const* getIVInfo(MD const* md) const { return m_ivmd2ivinfo.get(md); }
    LFRInfo * genLFRInfo(IR const* lf_exp);
    IRList const& getCandList() const { return m_cand_list; }
    LFRInfoMap const& getLFRInfoMap() const { return m_cand_occ2info; }
    LFRInfo * getLFRInfo(IR const* exp) const
    { return m_cand_occ2info.get(exp); }
    OptCtx const& getOptCtx() const { return m_oc; }

    void setMapExpToLFRInfo(IR const* exp, LFRInfo * info)
    { m_cand_occ2info.set(exp, info); }
    void setMapMDAndIV(MD const* ivmd, IV const* iv);
};


//This class represents linear function test replacement optimization.
class LFTR : public Pass {
    COPY_CONSTRUCTOR(LFTR);
    SMemPool * m_pool;
    IVR * m_ivr;
    IRCFG * m_cfg;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    DUMgr * m_dumgr;
    IRMgr * m_irmgr;
    IRIter m_iriter; //for local using
protected:
    void analyzeBB(LI<IRBB> const* li, IRBB const* bb, MOD LFAnaCtx & ctx);
    void analysis(LI<IRBB> * li, MOD LFAnaCtx & ctx);
    void addDUChainForRHSOfInitDef(IR * newrhs, IR const* oldrhs,
                                   LI<IRBB> const* li);

    void buildClassicDUForRed(IR * init, IR * red);

    //doReplacement candidate IRs to preheader BB.
    //This function will maintain RPO if new BB inserted.
    //Return true if BB or STMT changed.
    bool doReplacement(OUT IRBB * prehead, OUT LI<IRBB> * li,
                       OUT bool & insert_bb, MOD LFAnaCtx & ctx);
    //Return true if code changed.
    bool doLoopTree(LI<IRBB> * li, OUT bool & du_set_info_changed,
                    OUT bool & insert_bb, OptCtx & oc);

    //Generate IV reference.
    IR * genNewIVRef(LFRInfo const* info);
    void getLinearRepOfIV(LFAnaCtx const& ctx, IR const* lf_exp,
                          IV const** iv, IR const** coeff);
    IRMgr * getIRMgr() const { return m_irmgr; }

    //Insert PHI at head BB of loop for given reduction operation.
    //init: initial value stmt.
    //red: reduction stmt that is inserted at loop.
    //Return the inserted PHI.
    IR * insertPhiForRed(LI<IRBB> const* li, IR * init, IR * red);
    bool insertReductionCode(List<LFRInfo*> const& lfrinfo_list);
    bool insertComputingInitValCode(IRBB * preheader,
                                    List<LFRInfo*> const& lfrinfo_list,
                                    LI<IRBB> const* li);

    void pickupProperCandidate(OUT List<LFRInfo*> & lfrinfo_list,
                               MOD LFAnaCtx & ctx);
    bool replaceCandByIV(LFAnaCtx const& ctx);

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
        m_dumgr = nullptr;
        m_irmgr = nullptr;
    }
    virtual ~LFTR() { smpoolDelete(m_pool); }

    bool dump(LI<IRBB> const* li, LFAnaCtx const& ctx) const;

    virtual CHAR const* getPassName() const
    { return "Linear Function Test Replacement"; }
    PASS_TYPE getPassType() const { return PASS_LFTR; }
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
