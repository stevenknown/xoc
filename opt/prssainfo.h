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

//Initial version is an abstract description to indicate the imported DEF of
//each PR. Especially for dedicated PR, parameter.
//PR should have explicitly definition that version must not
//be initial version.
#define PRSSA_INIT_VERSION 0

//Verisoned Presentation.
//For each version of each prno, VPR is unique.
typedef SEGIter * SSAUseIter;

#define SSA_id(ssainfo) ((ssainfo)->uid)
#define SSA_def(ssainfo) ((ssainfo)->def_stmt)
#define SSA_uses(ssainfo) ((ssainfo)->use_exp_set)
class SSAInfo {
    COPY_CONSTRUCTOR(SSAInfo);
protected:
    void cleanMember()
    {
        uid = 0;
        def_stmt = nullptr;
    }
public:
    UINT uid;
    IR * def_stmt;
    IRSet use_exp_set;

public:
    SSAInfo(DefSegMgr * sm) : use_exp_set(sm) { cleanMember(); }

    //Add an USE reference.
    //This function build DU chain between DEF and 'use'.
    void addUse(IR const* use)
    {
        ASSERT0(use && use->isReadPR());
        ASSERT0(use->getSSAInfo() == this);
        SSA_uses(this).append(use);
    }

    //Add an USE reference.
    //This function build DU chain between DEF and 'use'.
    void addUseAndSSAInfo(IR * use)
    {
        ASSERT0(use && use->isReadPR());
        if (use->getSSAInfo() == nullptr) {
            use->setSSAInfo(this);
        } else {
            ASSERT0(use->getSSAInfo() == this);
        }
        SSA_uses(this).append(use);
    }

    //This function guarantee all memory resource recycled.
    void cleanDU()
    {
        SSA_def(this) = nullptr;
        SSA_uses(this).clean();
    }
    void cleanUseSet() { SSA_uses(this).clean(); }
    void copyUseSet(SSAInfo const& src) { SSA_uses(this).copy(src.getUses()); }

    //Get SSAInfo id.
    UINT id() const { return uid; }
    void init(DefSegMgr * sm)
    {
        cleanMember();
        SSA_uses(this).init(sm);
    }
    void initNoClean(DefSegMgr * sm) { SSA_uses(this).init(sm); }

    void dump(Region const* rg) const;
    void destroy() { SSA_uses(this).destroy(); }

    //Return true if 'ir' is an USE reference.
    bool findUse(IR const* ir) const
    {
        ASSERT0(ir && ir->isReadPR());
        return SSA_uses(this).find(ir);
    }

    //Get the DEF stmt.
    IR * getDef() const { return SSA_def(this); }
    //Get the USE set of expressions.
    IRSet const& getUses() const { return SSA_uses(this); }

    bool hasUse() const { return !getUses().is_empty(); }

    //Remove 'ir' from USE set.
    //This function cut off DU chain between DEF and 'ir'.
    void removeUse(IR const* ir)
    {
        ASSERT0(ir && ir->isReadPR());
        SSA_uses(this).remove(ir);
    }
};


//Version PR.
//Record original PRNO before SSA construction.
#define VPR_orgprno(v) (((VPR*)v)->m_orgprno)

//Record original PRNO data type.
#define VPR_orgpr_type(v) (((VPR*)v)->m_orgpr_type)

//Record renamed PRNO after SSA construction.
#define VPR_newprno(v) (((VPR*)v)->m_newprno)

//Record Version related to original PRNO.
#define VPR_version(v) (((VPR*)v)->m_version)

class VPR : public SSAInfo {
    COPY_CONSTRUCTOR(VPR);
public:
    UINT m_version; //record Version related to original PRNO.
    PRNO m_newprno; //record renamed PRNO after SSA construction.
    PRNO m_orgprno; //record original PRNO before SSA construction.
    Type const* m_orgpr_type; //record original PRNO data type.
public:
    VPR(DefSegMgr * sm) : SSAInfo(sm) { cleanMember(); }

    inline void cleanMember()
    {
        SSAInfo::cleanMember();
        m_newprno = PRNO_UNDEF;
        m_orgprno = PRNO_UNDEF;
        m_version = PRSSA_INIT_VERSION;
        m_orgpr_type = nullptr;
    }

    void init(DefSegMgr * sm)
    {
        cleanMember();
        SSAInfo::init(sm);
    }
    bool is_init_version() const { return version() == PRSSA_INIT_VERSION; }

    PRNO newprno() const { return VPR_newprno(this); }
    PRNO orgprno() const { return VPR_orgprno(this); }
    Type const* orgpr_type() const { return VPR_orgpr_type(this); }

    UINT version() const { return VPR_version(this); }
};


class VPRVec : public Vector<VPR*> {
    COPY_CONSTRUCTOR(VPRVec);
public:
    VPRVec() {}

    //Find the VPR that have PR defined at given BB.
    VPR * findVPR(UINT bbid) const;

    //Find the initial version VPR.
    VPR * getInitVersion() const;
};


//Mapping from PRNO to vector of VPR.
typedef Vector<VPRVec*> PRNO2VPRVec;

//Mapping from PRNO to Stack of VPR.
typedef Vector<Stack<VPR*>*> PRNO2VPRStack;

} //namespace xoc
#endif
