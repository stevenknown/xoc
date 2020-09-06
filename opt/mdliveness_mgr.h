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
#ifndef __MDLIVENESS_MGR_H__
#define __MDLIVENESS_MGR_H__

namespace xoc {

class MDLivenessMgr : public Pass {
    COPY_CONSTRUCTOR(MDLivenessMgr);
    Region * m_rg;
    xcom::DefMiscBitSetMgr m_sbs_mgr;
    MDSetMgr m_mds_mgr;
    DUMgr * m_dumgr;
    Vector<MDSet*> m_liveout_mds_vec;
    Vector<MDSet*> m_livein_mds_vec;
    Vector<MDSet*> m_def_mds_vec;
    Vector<MDSet*> m_use_mds_vec;

    //Note this function will not clean 'mayuse', thus the result will blend
    //with original elements.
    void collectMayUseMDS(IR const* ir, OUT MDSet * mayuse);
    void computeLocalLiveness(IRBB * bb);
    void computeGlobalLiveness();

    MDSet * getDefMDSet(IRBB * bb);
    MDSet * getUseMDSet(IRBB * bb);

public:
    MDLivenessMgr(Region * rg) : m_rg(rg), m_mds_mgr(rg, &m_sbs_mgr)
    { m_dumgr = rg->getDUMgr(); }
    ~MDLivenessMgr()
    {
        for (INT i = 0; i <= m_livein_mds_vec.get_last_idx(); i++) {
            MDSet * mds = m_livein_mds_vec.get(i);
            if (mds == NULL) { continue; }
            m_mds_mgr.free(mds);
        }

        for (INT i = 0; i <= m_liveout_mds_vec.get_last_idx(); i++) {
            MDSet * mds = m_liveout_mds_vec.get(i);
            if (mds == NULL) { continue; }
            m_mds_mgr.free(mds);
        }

        for (INT i = 0; i <= m_def_mds_vec.get_last_idx(); i++) {
            MDSet * mds = m_def_mds_vec.get(i);
            if (mds == NULL) { continue; }
            m_mds_mgr.free(mds);
        }

        for (INT i = 0; i <= m_use_mds_vec.get_last_idx(); i++) {
            MDSet * mds = m_use_mds_vec.get(i);
            if (mds == NULL) { continue; }
            m_mds_mgr.free(mds);
        }
    }

    void dump(bool with_name = false) const;

    MDSetMgr * getMdsMgr() { return &m_mds_mgr; }
    Region * getRegion() const { return m_rg; }
    xcom::DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }
    MDSet * getLiveInMDSet(IRBB * bb);
    MDSet * getLiveOutMDSet(IRBB * bb);    
    virtual CHAR const* getPassName() const { return "MDLivenessMgr"; }
    PASS_TYPE getPassType() const { return PASS_MDLIVENESS_MGR; }

    virtual bool perform(OptCtx & oc);
};   

} //namespace xoc
#endif
