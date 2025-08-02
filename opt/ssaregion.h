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
#ifndef _SSA_REGION_H_
#define _SSA_REGION_H_

namespace xoc {

class ActMgr;

//
//START SSARegion
//
//The class represents a region that is consist of BB in bbset, which
//is expected to perform SSA construction for the region.
class SSARegion {
    COPY_CONSTRUCTOR(SSARegion);
protected:
    bool verifyRootDom() const;
public:
    Region const* m_rg;
    IRBB * m_root;
    OptCtx * m_oc;
    IRCFG const* m_cfg;
    ActMgr * m_am;
    DomTree const& m_domtree;
    xcom::TTab<UINT> m_iridtab; //for local used.
    IRList m_irlist;
    BBSet m_bbset;
public:
    SSARegion(xcom::DefMiscBitSetMgr * sbs, DomTree const& dt,
              Region const* rg, OptCtx * oc, ActMgr * am);

    //The function will find PR that assigned 'prno' into current
    //SSA construction region.
    //start: stmt or expression start to find.
    void add(PRNO prno, IR * start);

    //Add ir to current SSA construction region that expected to transform
    //to SSA mode.
    //Note ir must be PR op.
    void add(IR * ir);

    //Add BB into current SSA construction region.
    //Note even if there is no occurrence of PR operation in 'bb', the BB that
    //belong to the SSARegion also should be add into bbset.
    void add(IRBB const* bb) { ASSERT0(bb); add(bb->id()); }
    void add(UINT bbid)
    { ASSERT0(bbid != BBID_UNDEF); getBBSet().bunion(bbid); }

    //Add a set of BB into current SSA construction region.
    //The construction will not exceed these BBs.
    void add(BitSet const& bbset) { getBBSet().bunion(bbset); }

    //Walk through each predecessors from 'start' to guarantee all vertexs in
    //path from root to start have been added.
    void addPredBBTillRoot(IRBB const* start);

    //Walk through DomTree start from root and add BB into SSARegion.
    //Note root must be set first.
    void addAllBBUnderRoot();

    //Return true if bb can be regarded as the root of SSA region.
    bool canBeRoot(IRBB const* bb) const;

    void dump() const;

    //The function attempt to find properly root BB of SSA region.
    //A properly root BB either does not have any PHI operation, or all
    //predecessors of root BBs are located within current SSA region.
    IRBB * findRootBB(IRBB * start);

    //Get the bbset of region.
    BBSet & getBBSet() { return m_bbset; }

    //Get the irlist of current SSA construction region.
    IRList & getIRList() { return m_irlist; }

    //Get the root BB of current SSA construction region.
    IRBB * getRootBB() const { return m_root; }
    OptCtx * getOptCtx() const { return m_oc; }
    DomTree const& getDomTree() const { return m_domtree; }
    ActMgr * getActMgr() const { return m_am; }

    //Return true if BB id is in the SSA region.
    //id: the BB id.
    bool isInRegion(UINT id) const { return m_bbset.is_contain(id); }

    //Return true if all predecessors of 'bb' are located in SSA region.
    bool isAllPredInRegion(IRBB const* bb) const;

    //Infer and add those BBs that should be also handled in PRSSA
    //construction.
    void inferAndAddRelatedBB();

    //Set the root BB of current SSA construction region.
    //root: root BB for CFG region that is consisted of BB which is
    //in SSA construction region.
    void setRootBB(IRBB * root) { add(root); m_root = root; }

    //Verify whether the SSA region is legal enough to construct.
    bool verify() const;
};
//END SSARegion

} //namespace xoc
#endif
