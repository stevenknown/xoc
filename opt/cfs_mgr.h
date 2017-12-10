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
#ifndef __CFS_MGR_H__
#define __CFS_MGR_H__

namespace xoc {

//Abstract Tree Type to describe high level control flow struct.
typedef enum _ABS_TYPE {
    ABS_LOOP = 0,
    ABS_IF,
    ABS_BB,
} ABS_TYPE;


/*
SCOP
  |-SCOP_NODE:LOOP
     |-SCOP_NODE:BB
     |-SCOP_NODE:IF
        |-SCOP_NODE:BB
        |-SCOP_NODE:BB
     |-SCOP_NODE:LOOP
     |-...
  |-SCOP_NODE:IF
     |-SCOP_NODE:LOOP
        |-SCOP_NODE:BB
        |-SCOP_NODE:BB
     |-SCOP_NODE:BB
     |-...
   ...

*/
#define ABS_NODE_prev(sn)            ((sn)->prev)
#define ABS_NODE_next(sn)            ((sn)->next)
#define ABS_NODE_parent(sn)          ((sn)->parent)
#define ABS_NODE_loop_body(sn)       ((sn)->u2.loop_body)
#define ABS_NODE_true_body(sn)       ((sn)->u2.s1.true_body)
#define ABS_NODE_false_body(sn)      ((sn)->u2.s1.false_body)
#define ABS_NODE_type(sn)            ((sn)->ty)
#define ABS_NODE_if_head(sn)         ((sn)->u1.if_head)
#define ABS_NODE_loop_head(sn)       ((sn)->u1.loop_head)
#define ABS_NODE_bb(sn)              ((sn)->u1.bb)
class AbsNode {
public:
    AbsNode * prev;
    AbsNode * next;
    AbsNode * parent;
    ABS_TYPE ty; //Type of SCOP NODE
    union {
        AbsNode * loop_body;
        struct {
            AbsNode * true_body;
            AbsNode * false_body;
        } s1;
    } u2;

    union {
        IRBB * if_head;
        IRBB * loop_head;
        IRBB * bb;
    } u1;
};


//
//CFS_INFO
//
#define CFS_INFO_cfs_type(ci)            ((ci)->cfs_type)
#define CFS_INFO_ir(ci)                  ((ci)->u2.ir)
#define CFS_INFO_head(ci)                ((ci)->u2.head)
#define CFS_INFO_true_body(ci)           ((ci)->u1.if_info.true_body_ir_set)
#define CFS_INFO_false_body(ci)          ((ci)->u1.if_info.false_body_ir_set)
#define CFS_INFO_loop_body(ci)           ((ci)->u1.loop_info.loop_body_ir_set)
class CFS_INFO {
public:
    IR_TYPE cfs_type;
    union {
        struct {
            BitSet * true_body_ir_set; //TRUE BODY
            BitSet * false_body_ir_set; //FALSE BODY
        } if_info;
        struct {
            BitSet * loop_body_ir_set; //LOOP BODY
        } loop_info;
    } u1;
    union {
        IR * ir;
        IRBB * head;
    } u2;
};



//
//CfsMgr, record and rebuild the control flow structure information.
//
class CfsMgr : public Pass {
protected:
    BitSetMgr m_bs_mgr; //BitSet manager.
    SMemPool * m_pool;
    Vector<CFS_INFO*> m_map_ir2cfsinfo;
    Region * m_ru;
    Vector<AbsNode*> m_map_bb2abs;

protected:
    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }
public:
    CfsMgr(Region * rg)
    {
        m_ru = rg;
        m_pool = smpoolCreate(64, MEM_COMM);
    }

    ~CfsMgr() { smpoolDelete(m_pool); }

    AbsNode * constructAbsLoop(
            IN IRBB * entry,
            IN AbsNode * parent,
            IN BitSet * cur_region,
            IN Graph & cur_graph,
            IN OUT BitSet & visited);
    AbsNode * constructAbsIf(
            IN IRBB * entry,
            IN AbsNode * parent,
            IN Graph & cur_graph,
            IN OUT BitSet & visited);
    AbsNode * constructAbsBB(IN IRBB * bb, IN AbsNode * parent);
    AbsNode * constructAbsTree(
            IN IRBB * entry,
            IN AbsNode * parent,
            IN BitSet * cur_region,
            IN Graph & cur_graph,
            IN OUT BitSet & visited);
    AbsNode * constructAbstractControlFlowStruct();

    void dump_indent(UINT indent);
    void dump_abs_tree(AbsNode * an);
    void dump_abs_tree(AbsNode * an, UINT indent);

    CFS_INFO * map_ir2cfsinfo(IR * ir);
    AbsNode * map_bb2abs(IRBB const* bb);

    CFS_INFO * new_cfs_info(IR_TYPE irtype);
    AbsNode * new_abs_node(ABS_TYPE ty);

    void set_map_ir2cfsinfo(IR * ir, CFS_INFO * ci);
    void set_map_bb2abs(IRBB const* bb, AbsNode * abs);

    void recordStmt(IR * ir, BitSet & irset);

    virtual CHAR const* getPassName() const
    { return "Control Flow Structure MGR"; }

    virtual PASS_TYPE getPassType() const { return PASS_CFS_MGR; }
};

} //namespace xoc
#endif
