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
#define CN_id(c)         ((c)->id)
#define CN_sym(c)        ((c)->ru_name)
#define CN_ru(c)         ((c)->ru)
#define CN_is_used(c)    ((c)->u1.s1.is_used)
class CallNode {
public:
    UINT id;
    SYM const* ru_name; //record the Region name.
    Region * ru; //record the Region that callnode corresponds to.
    union {
        struct {
            //It is marked by attribute used, which usually means
            //that it is called in inline assembly code.
            BYTE is_used:1;
        } s1;
        BYTE u1b1;
    } u1;
};


//Mapping from SYM to CallNode.
typedef TMap<SYM const*, CallNode*> SYM2CN;


//Call Graph
#define CALLG_DUMP_IR    1
class CallGraph : public DGraph {
    RegionMgr * m_ru_mgr;
    TypeMgr * m_dm;
    SMemPool * m_cn_pool; //pool for call node.
    UINT m_cn_count;
    Vector<CallNode*> m_cnid2cn;
    Vector<CallNode*> m_ruid2cn;
    SYM2CN m_sym2cn_map;

    CallNode * newCallNode()
    {
        ASSERT0(m_cn_pool);
        CallNode * p = (CallNode*)smpoolMallocConstSize(
                            sizeof(CallNode), m_cn_pool);
        ASSERT0(p);
        memset(p, 0, sizeof(CallNode));
        return p;
    }
public:
    CallGraph(UINT edge_hash, UINT vex_hash, RegionMgr * rumgr):
          DGraph(edge_hash, vex_hash)
    {
        ASSERT0(edge_hash > 0 && vex_hash > 0);
        m_ru_mgr = rumgr;
        m_dm = rumgr->get_type_mgr();
        m_cn_count = 1;
        m_cn_pool = smpoolCreate(sizeof(CallNode) * 2, MEM_CONST_SIZE);
    }
    COPY_CONSTRUCTOR(CallGraph);
    virtual ~CallGraph() { smpoolDelete(m_cn_pool); }

    void add_node(CallNode * cn)
    {
        m_cnid2cn.set(CN_id(cn), cn);
        addVertex(CN_id(cn));
    }
    void build(Region * top);
    void computeEntryList(List<CallNode*> & elst);
    void computeExitList(List<CallNode*> & elst);

    //name: file name if you want to dump VCG to specified file.
    //flag: default is 0xFFFFffff(-1) means doing dumping
    //        with completely information.
    void dump_vcg(CHAR const* name = NULL, INT flag = -1);

    CallNode * map_id2cn(UINT id) const { return m_cnid2cn.get(id); }
    CallNode * map_vex2cn(Vertex const* v) const
    { return m_cnid2cn.get(VERTEX_id(v)); }

    CallNode * map_ru2cn(Region const* ru) const
    { return m_ruid2cn.get(REGION_id(ru)); }

    CallNode * map_sym2cn(SYM const* name) const
    { return m_sym2cn_map.get(name); }

    Region * map_ir2ru(IR const* ir)
    {
        ASSERT0(ir->is_calls_stmt());
        SYM * name = VAR_name(CALL_idinfo(ir));
        CallNode * cn = map_sym2cn(name);
        if (cn == NULL) return NULL;
        return CN_ru(cn);
    }

    CallNode * newCallNode(IR * ir);
    CallNode * newCallNode(Region * ru);

    void erase()
    {
        m_cnid2cn.clean();
        Graph::erase();
    }

    //This is an interface.
    //Return true if an edge is needed bewteen the caller and the ir.
    //Note ir must be a function call.
    virtual bool shouldAddEdge(IR const* ir) const
    {
        ASSERT0(ir->is_calls_stmt());
        return true;
    }
};

} //namespace xoc
#endif
