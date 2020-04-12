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
#ifndef _CALLG_H_
#define _CALLG_H_

namespace xoc {

//CALL NODE
#define CN_id(c) ((c)->id)
#define CN_sym(c) ((c)->ru_name)
#define CN_ru(c) ((c)->rg)
#define CN_is_used(c) ((c)->u1.s1.is_used)
#define CN_unknown_callee(c) ((c)->u1.s1.has_unknown_callee)
class CallNode {
    COPY_CONSTRUCTOR(CallNode);
public:
    UINT id;
    SYM const* ru_name; //record the Region name.
    Region * rg; //record the Region that callnode corresponds to.
    union {
        struct {
            //It is marked by attribute used, which usually means
            //that it is called in inline assembly code.
            BYTE is_used:1;

            //It may be unable to determine some targets
            //of a callsite. In this case, the flag will return true.
            BYTE has_unknown_callee:1;
        } s1;
        BYTE u1b1;
    } u1;

public:
    CallNode() { ::memset(this, 0, sizeof(CallNode)); }
};


//Mapping from SYM to CallNode.
typedef TMap<SYM const*, CallNode*> SYM2CN;
typedef TMap<Region*, SYM2CN*> Region2SYM2CN;
typedef TMapIter<Region*, SYM2CN*> Region2SYM2CNIter;


//Call xcom::Graph
//The call graph is not precise. That is, a callsite may indicate it can
//call a function when in fact it does not do so in the running program.
#define CALLG_DUMP_IR            1
#define CALLG_DUMP_SRC_LINE      2
#define CALLG_DUMP_INNER_REGION  4
class CallGraph : public xcom::DGraph {
    COPY_CONSTRUCTOR(CallGraph);
private:
    RegionMgr * m_ru_mgr;
    TypeMgr * m_tm;
    SMemPool * m_cn_pool; //pool for call node.
    UINT m_cn_count;
    Vector<CallNode*> m_cnid2cn;
    Vector<CallNode*> m_ruid2cn;
    Region2SYM2CN m_ru2sym2cn;

private:
    CallNode * allocCallNode()
    {
        ASSERT0(m_cn_pool);
        CallNode * p = (CallNode*)smpoolMallocConstSize(
                            sizeof(CallNode), m_cn_pool);
        ASSERT0(p);
        ::memset(p, 0, sizeof(CallNode));
        return p;
    }

    //Generate map between Symbol and CallNode for current region.
    SYM2CN * genSYM2CN(Region * rg)
    {
        ASSERT0(rg);
        SYM2CN * sym2cn = m_ru2sym2cn.get(rg);
        if (sym2cn == NULL) {
            sym2cn = new SYM2CN();
            m_ru2sym2cn.set(rg, sym2cn);
        }
        return sym2cn;
    }

    //Read map between Symbol and CallNode for current region if exist.
    SYM2CN * getSYM2CN(Region * rg) const { return m_ru2sym2cn.get(rg); }

    CallNode * newCallNode(IR const* ir, Region * rg);
    CallNode * newCallNode(Region * rg);

public:
    CallGraph(UINT edge_hash,
              UINT vex_hash,
              RegionMgr * rumgr): xcom::DGraph(edge_hash, vex_hash)
    {
        ASSERT0(edge_hash > 0 && vex_hash > 0);
        m_ru_mgr = rumgr;
        m_tm = rumgr->getTypeMgr();
        m_cn_count = 1;
        m_cn_pool = smpoolCreate(sizeof(CallNode) * 2, MEM_CONST_SIZE);
    }
    virtual ~CallGraph()
    {
        SYM2CN * sym2cn = NULL;
        Region2SYM2CNIter iter;
        for (Region * rg = m_ru2sym2cn.get_first(iter, &sym2cn);
             rg != NULL; rg = m_ru2sym2cn.get_next(iter, &sym2cn)) {
            delete sym2cn;
        }
        smpoolDelete(m_cn_pool);
    }

    void add_node(CallNode * cn)
    {
        m_cnid2cn.set(CN_id(cn), cn);
        addVertex(CN_id(cn));
    }
    bool build(RegionMgr * rumgr);
    void computeEntryList(List<CallNode*> & elst);
    void computeExitList(List<CallNode*> & elst);

    //name: file name if you want to dump VCG to specified file.
    //flag: default is 0xFFFFffff(-1) means doing dumping
    //        with completely information.
    void dumpVCG(CHAR const* name = NULL, INT flag = -1);

    CallNode * mapId2CallNode(UINT id) const { return m_cnid2cn.get(id); }
    CallNode * mapVertex2CallNode(xcom::Vertex const* v) const
    { return m_cnid2cn.get(VERTEX_id(v)); }

    CallNode * mapRegion2CallNode(Region const* rg) const
    { return m_ruid2cn.get(REGION_id(rg)); }

    CallNode * mapSym2CallNode(SYM const* sym, Region * start) const
    {
        for (; start != NULL; start = start->getParent()) {
            SYM2CN * sym2cn = getSYM2CN(start);
            if (sym2cn == NULL) { continue; }
            CallNode * cn = sym2cn->get(sym);
            if (cn != NULL) { return cn; }
        }
        return NULL;
    }

    //Map a call/icall to its target Region.
    //rg: the region that ir resident in.
    Region * mapCall2Region(IR const* ir, Region * rg)
    {
        if (ir->is_call()) {
            CallNode * cn = mapSym2CallNode(CALL_idinfo(ir)->get_name(), rg);
            if (cn != NULL) { return CN_ru(cn); }
            return NULL;
        }
        ASSERT0(ir->is_icall());
        return NULL; //TODO: implement icall analysis.
    }

    void erase()
    {
        m_cnid2cn.clean();
        xcom::Graph::erase();
    }

    //This is an interface.
    //Return true if an edge is needed bewteen the caller and the ir.
    //Note ir must be a function call.
    virtual bool shouldAddEdge(IR const* ir) const
    {
        ASSERT0(ir->isCallStmt());
        DUMMYUSE(ir);
        return true;
    }
};

} //namespace xoc
#endif
