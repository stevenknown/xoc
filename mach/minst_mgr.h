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

author: Su Zhenyu
@*/
#ifndef _MINST_MGR_H_
#define _MINST_MGR_H_

namespace mach {

typedef List<MInst*>::Iter MIListIter;
class MIList : public List<MInst*> {
    COPY_CONSTRUCTOR(MIList);
public:
    MIList() {}
    ~MIList() {}

    void append_tail(MInst * mi);
    //Move elements in 'ors' to tail of current list.
    void move_tail(MOD MIList & ors);

    void copyDbx(IR const* ir)
    {
        ASSERT0(ir);
        if (IR_ai(ir) == nullptr) { return; }
        DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
        if (da == nullptr) { return; }
        for (MInst * mi = get_head(); mi != nullptr; mi = get_next()) {
            MI_dbx(mi).copy(da->dbx);
        }
    }
    void copyDbx(Dbx const* dbx)
    {
        if (dbx == nullptr) { return; }
        for (MInst * mi = get_head(); mi != nullptr; mi = get_next()) {
            MI_dbx(mi).copy(*dbx);
        }
    }

    void dump(LogMgr * lm, MInstMgr const& mgr) const;
};


//
//START RecycMIList
//
//This class defined recyclable MIList.
//The object will be recycled when it destructed.
class RecycMIList {
protected:
    COPY_CONSTRUCTOR(RecycMIList);
    MIList * m_entity;
    RecycMIListMgr * m_mgr;
protected:
    void init(RecycMIListMgr * mgr);
public:
    RecycMIList(RecycMIListMgr * mgr) { init(mgr); }
    ~RecycMIList();

    void append_tail(MInst * mi) { m_entity->append_tail(mi); }

    //Move elements in 'ors' to tail of current list.
    void move_tail(MIList & ors) { m_entity->move_tail(ors); }
    void move_tail(RecycMIList & ors) { m_entity->move_tail(ors.getList()); }

    void copyDbx(IR const* ir) { m_entity->copyDbx(ir); }
    void copyDbx(Dbx const* dbx) { m_entity->copyDbx(dbx); }
    void clean() { m_entity->clean(); }

    void dump(LogMgr * lm, MInstMgr const& mgr) { m_entity->dump(lm, mgr); }

    MIList & getList() const { return *m_entity; }
};


class RecycMIListMgr {
    COPY_CONSTRUCTOR(RecycMIListMgr);
    List<MIList*> m_free_list;
public:
    RecycMIListMgr() {}
    ~RecycMIListMgr();
    MIList * getFree() { return m_free_list.remove_head(); }
    void addFree(MIList * e) { m_free_list.append_head(e); }
};
//END RecycMIList


class MInstMgr {
    COPY_CONSTRUCTOR(MInstMgr);
    MFieldMgr const& m_field_mgr;
    LabelInstDesc m_label_instdesc;
    MemAccInstDesc m_memacc_instdesc;
    SMemPool * m_pool;
protected:
    template<class T>
    MInst * allocMInst(UINT fieldnum)
    {
        MInst * mi = (MInst*)xmalloc(sizeof(T));
        //CASE:MI_label does not have any field.
        //ASSERTN(fieldnum > 0, ("instruction can not be empty"));
        if (fieldnum > 0) {
            MI_field_vec(mi) = (MField*)xmalloc(sizeof(MField) * fieldnum);
        }
        return mi;
    }

    LabelInstDesc & getLabelInstDesc() { return m_label_instdesc; }
    MemAccInstDesc & getMemAccInstDesc() { return m_memacc_instdesc; }

    void * xmalloc(UINT size);
    MInstMgr * self() { return this; }
public:
    MInstMgr(MFieldMgr const& fm) : m_field_mgr(fm),
        m_label_instdesc(*self()),
        m_memacc_instdesc(*self())
    { m_pool = smpoolCreate(64, MEM_COMM); }
    virtual ~MInstMgr() { smpoolDelete(m_pool); }

    //Build a machine instruction.
    //c: machine instruction code.
    //d: machine instruction description.
    template<class T>
    MInst * buildMInst(MI_CODE c, MInstDesc const* d)
    {
        ASSERT0(d && c != MI_UNDEF);
        MInst * mi = allocMInst<T>(d->getFieldNum());
        MI_code(mi) = c;
        MI_desc(mi) = d;
        return mi;
    }

    //Build a dummy machine instruction that indicates a label.
    virtual MInst * buildLabel()
    {
        MInst * mi = buildMInst<LabelMInst>(MI_label, &m_label_instdesc);
        mi->setFlag(MI_FLAG_HAS_LABEL);
        return mi;
     }

    //Build a dummy machine instruction that indicates a memory access.
    virtual MInst * buildMemAcc(MI_CODE c)
    {
        MInst * mi = buildMInst<MemAccMInst>(c, &m_memacc_instdesc);
        mi->setFlag(MI_FLAG_HAS_VAR);
        return mi;
    }

    //Return the name of given field type.
    CHAR const* getFieldName(FIELD_TYPE ft) const
    { return m_field_mgr.getFieldName(ft); }

    //Return the bit width of given field type.
    UINT getFieldSize(FIELD_TYPE ft) const
    { return getFieldMgr().getFieldSize(ft); }

    //Return the start bit position of given field type.
    UINT getFieldStart(FIELD_TYPE ft) const
    { return getFieldMgr().getFieldStart(ft); }

    //Return the end bit position of given field type.
    UINT getFieldEnd(FIELD_TYPE ft) const
    { return getFieldMgr().getFieldEnd(ft); }

    MFieldMgr const& getFieldMgr() const { return m_field_mgr; }

    //Return the name of machine instruction.
    //Target Dependent Code.
    virtual CHAR const* getMInstName(MInst const* mi) const { return "NONAME"; }
};

} //namespace

#endif
