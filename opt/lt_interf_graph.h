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
#ifndef _LT_INTERF_GRAPH_H_
#define _LT_INTERF_GRAPH_H_

namespace xoc {

class LifeTime;
class LifeTimeMgr;
class LTList;

//This class represents lifetime interference graph.
class LTIG : public xcom::Graph {
    COPY_CONSTRUCTOR(LTIG);
    Region * m_rg;
    LifeTimeMgr & m_lt_mgr;
private:
    LifeTimeMgr const& getLTMgr() const { return m_lt_mgr; }
    virtual void dumpVertexDesc(
        xcom::Vertex const* v, xcom::DefFixedStrBuf & buf) const override;
public:
    LTIG(Region * rg, LifeTimeMgr & mgr) : m_rg(rg), m_lt_mgr(mgr)
    { set_direction(false); set_dense(true); }
    void dumpVCG(CHAR const* name) const;
    bool is_interferred(LifeTime const* lt1, LifeTime const* lt2) const;

    //Build interference graph for lifetime in LifeTimeMgr.
    void build();

    //Build interference graph for lifetime in given lst.
    void build(LTList const& ltlst);
};

} //namespace xoc
#endif
