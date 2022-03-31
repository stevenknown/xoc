/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#ifndef _CDG_H_
#define _CDG_H_

namespace xoc {

class Region;

//Control Dependence Graph
class CDG : public Pass, public xcom::Graph {
    COPY_CONSTRUCTOR(CDG);
    //Set to true if CDG presents control-edge to cyclic control flow.
    //e.g: given loop, the control BB of loop-header includes itself.
    bool m_allow_cycle;
private:
    bool is_control(Vertex const* a, Vertex const* b) const;
    void rebuild(MOD OptCtx & oc, xcom::DGraph & cfg);
public:
    CDG(Region * rg) : Pass(rg), m_allow_cycle(false) {}
    void build(MOD OptCtx & oc, xcom::DGraph & cfg);

    void dumpDOT(CHAR const* name = nullptr) const;
    void dumpVCG(CHAR const* name = nullptr) const;
    virtual bool dump() const;

    void get_cd_preds(UINT id, OUT List<xcom::Vertex*> & lst);
    void get_cd_succs(UINT id, OUT List<xcom::Vertex*> & lst);
    virtual CHAR const* getPassName() const { return "CDG"; }
    virtual PASS_TYPE getPassType() const { return PASS_CDG; }

    //Return true if vertex is only control itself.
    bool is_only_control_self(UINT vid) const;

    //Return true if vertex b is control dependent on vertex a.
    //e.g:a
    //    |\
    //    | b
    //    |/
    //    end
    // a controls b.
    bool is_control(UINT a, UINT b) const
    { return is_control(getVertex(a), getVertex(b)); }
 
    virtual bool perform(OptCtx & oc);

    void set_allow_cycle(bool doit) { m_allow_cycle = doit; }
};

} //namespace xoc
#endif
