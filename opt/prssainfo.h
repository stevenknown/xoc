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
#ifndef _PRSSAINFO_H_
#define _PRSSAINFO_H_

namespace xoc {

class IRSet : public DefSBitSet {
public:
    IRSet(DefSegMgr * sm) : DefSBitSet(sm) {}
    COPY_CONSTRUCTOR(IRSet);

    void append(IR const* v) { DefSBitSet::bunion(IR_id(v)); }

    bool find(IR const* v) const
    {
        ASSERT0(v);
        return DefSBitSet::is_contain(IR_id(v));
    }

    void remove(IR const* v)
    {
        ASSERT0(v);
        DefSBitSet::diff(IR_id(v));
    }
};


//Verisoned Presentation.
//For each version of each prno, VP is unique.
typedef SEGIter * SSAUseIter;

#define SSA_id(ssainfo)         ((ssainfo)->id)
#define SSA_def(ssainfo)        ((ssainfo)->def_stmt)
#define SSA_uses(ssainfo)       ((ssainfo)->use_exp_set)
class SSAInfo {
protected:
    void cleanMember()
    {
        id = 0;
        def_stmt = NULL;
    }
public:
    UINT id;
    IR * def_stmt;
    IRSet use_exp_set;

public:
    SSAInfo(DefSegMgr * sm) : use_exp_set(sm) { cleanMember(); }

    inline void cleanDU()
    {
        SSA_def(this) = NULL;
        SSA_uses(this).clean();
    }

    inline void init(DefSegMgr * sm)
    {
        cleanMember();
        use_exp_set.init(sm);
    }

    void initNoClean(DefSegMgr * sm) { use_exp_set.init(sm); }

    void destroy() { use_exp_set.destroy(); }

    IR const* get_def() const { return def_stmt; }
    IRSet const& get_uses() const { return use_exp_set; }
};


//Version PR.
#define VP_prno(v)           ((v)->prno)
#define VP_ver(v)            ((v)->version)
class VP : public SSAInfo {
public:
    UINT version;
    UINT prno;

    VP(DefSegMgr * sm) : SSAInfo(sm) { cleanMember(); }

    inline void cleanMember()
    {
        SSAInfo::cleanMember();
        prno = 0;
        version = 0;
    }

    void init(DefSegMgr * sm)
    {
        cleanMember();
        SSAInfo::init(sm);
    }
};

//Mapping from PRNO to vector of VP.
typedef Vector<Vector<VP*>*> UINT2VPVec;

//Mapping from PRNO id to Stack of VP.
typedef Vector<Stack<VP*>*> UINT2VPStack;

} //namespace xoc
#endif
