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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __RPO_H__
#define __RPO_H__

namespace xcom {

class Graph;
class Vertex;

//Increase or decrease RPO by 1.
//#define UPDATE_RPO_JUST_BY_SINGLE_STEP
#define RPO_INTERVAL 10
#define RPO_INIT_VAL 0
#define RPO_UNDEF ((RPOVal)-1)

typedef INT RPOVal;
typedef UINT RPOUVal;
typedef C<Vertex const*> * RPOVexListIter;
class RPOVexList : public List<Vertex const*> {
};


class RPOMgr {
    COPY_CONSTRUCTOR(RPOMgr);
    TMap<RPOUVal, RPOUVal> m_used_rpo;
private:
    void add(RPOVal rpo) { m_used_rpo.set(rpo, rpo); }
public:
    RPOMgr() {}

    static inline RPOVal computeNearestLessUnUsableRPO(RPOVal rpo)
    { ASSERT0(rpo > RPO_UNDEF); return rpo / RPO_INTERVAL * RPO_INTERVAL; }
    static inline RPOVal computeNearestGreaterUnUsableRPO(RPOVal rpo)
    {
        ASSERT0(rpo > RPO_UNDEF);
        return (rpo / RPO_INTERVAL + ((rpo % RPO_INTERVAL) != 0)) *
               RPO_INTERVAL;
    }

    //Sort vertice by RPO order, and update rpo of vertex.
    //Record sorted vertex into vlst in incremental order of RPO.
    //NOTE: rpo start at RPO_INIT_VAL.
    void computeRPO(Graph const& g, MOD Vertex * root, OUT RPOVexList & vlst);

    //Free RPO for next allocation.
    void freeRPO(RPOVal rpo)
    {
        ASSERT0(rpo != RPO_UNDEF);
        m_used_rpo.remove(rpo);
    }

    //Return true if rpo is available to assign to a new vertex.
    //And the rpo will not repeat with other vertex.
    bool isUsableRPO(RPOVal rpo) const
    {
        return rpo != RPO_UNDEF && ((rpo % RPO_INTERVAL) != 0) &&
               !m_used_rpo.find(rpo);
    }

    //Return true if find an order of RPO for 'v' that less than order of 'ref'.
    bool tryFindLessRPO(Vertex * v, Vertex const* ref);

    //Try to find an usable RPO that is between 'begin' and 'end'.
    //Note the algorithm has assigned positive integers as RPO to each vertex
    //by every RPO_INTERVAL numbers. These assigned integers are regarded as
    //unusable integer.
    //'begin' and 'end' have to be within same INTERVAL.
    RPOVal tryFindUsableRPO(RPOVal begin, RPOVal end);

    //Try to update RPO of newvex accroding to RPO of marker.
    //newvex_prior_marker: true if newvex's lexicographical order is prior to
    //marker. Return true if this function find a properly RPO for 'newvex',
    //otherwise return false.
    bool tryUpdateRPO(MOD Vertex * newvex, Vertex const* marker,
                      bool newvex_prior_marker);
};

} //namespace xcom
#endif //END __RPO_H__
