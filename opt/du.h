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
#ifndef __DU_H__
#define __DU_H__

namespace xoc {

class IR_DU_MGR;

typedef SEGIter * DUIter;

class DUSet : public DefSBitSetCore {
protected:
    friend class IR_DU_MGR;
public:
    DUSet() {}
    ~DUSet()
    {
        //Do not free ref here. They are allocated in mempool,
        //and the memory is freed when the pool destructed.
    }

    void add(UINT irid, DefMiscBitSetMgr & m) { bunion(irid, m); }
    void addDef(IR const* stmt, DefMiscBitSetMgr & m);
    void addUse(IR const* exp, DefMiscBitSetMgr & m);

    void remove(UINT irid, DefMiscBitSetMgr & m) { diff(irid, m); }
    void removeUse(IR const* exp, DefMiscBitSetMgr & m);
    void removeDef(IR const* stmt, DefMiscBitSetMgr & m);
};

//Record DU info for ir.
#define DU_md(du)           ((du)->md)
#define DU_mds(du)          ((du)->mds)
#define DU_duset(du)        ((du)->duset)
class DU {
public:
    MD const* md; //indicate the Must MD reference.
    MDSet const* mds; //indicate May MDSet reference.
    DUSet * duset; //indicate Def/Use of stmt/expr set.

    void clean()
    {
        md = NULL;
        mds = NULL;
        duset = NULL;
    }

    bool has_clean() const
    { return md == NULL && mds == NULL && duset == NULL; }
};

} //namespace xoc
#endif
