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
#ifndef _DEX_CALL_GRAPH_H_
#define _DEX_CALL_GRAPH_H_

class DexCallGraph : public CallGraph {
protected:
    //Record very unimportant functions that do not
    //need to add an edge on call graph.
    TTab<Sym const*> m_unimportant_symtab;

public:
    DexCallGraph(UINT edge_hash, UINT vex_hash, RegionMgr * rumgr);
    virtual ~DexCallGraph() {}

    bool is_unimportant(Sym const* sym) const;

    virtual bool shouldAddEdge(IR const* ir) const
    {
        ASSERT0(ir->isCallStmt());
        Sym const* name = VAR_name(CALL_idinfo(ir));
        if (is_unimportant(name)) { return false; }

        CHAR const* q = "Ljava/lang/";
        CHAR const* p = SYM_name(name);
        while ((*p++ == *q++) && *q != 0);
        return *q != 0;
    }
};

#endif
