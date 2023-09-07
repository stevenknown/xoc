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
#ifndef __AI_H__
#define __AI_H__

namespace xoc {

class MDSSAInfo;

//Usage of AIContainer:
//1.Allocate AIContainer.
//2.Construct AI data structure to be attached.
//3.Set the AIContainer type and the data structure.
//e.g: construct DBX AI.
//  IR * ir = ...; //Given IR.
//  IR_ai(ir) = region->allocAIContainer();
//  Dbx * dbx = getDbx();
//  IR_ai(ir)->set(AI_DBX, (BaseAttachInfo*)dbx);
//Note that you do not need to free/delete AI structure,
//which will be freed in destructor of region.
//And region is not reponsible for allocation or free of
//AI data structure.

//Attach Info Type.
typedef enum _AI_TYPE {
    AI_UNDEF = 0,
    AI_DBX,       //Debug Info
    AI_PROF,      //Profile Info
    AI_TBAA,      //Type Based AA
    AI_EH_LABEL,  //Record a list of Labels.
    AI_USER_DEF,  //User Defined
    AI_MD_SSA,    //MD SSA info
    AI_LAST,      //The number of ai type.
} AI_TYPE;

#define AI_type(ai) ((ai)->m_type)

class BaseAttachInfo {
    COPY_CONSTRUCTOR(BaseAttachInfo);
protected:
    AI_TYPE m_type;

public:
    explicit BaseAttachInfo(AI_TYPE t) { init(t); }
    ~BaseAttachInfo() {}
    void init(AI_TYPE t) { m_type = t; }
    AI_TYPE getType() const { return m_type; }
};


typedef xcom::SimpleVector<BaseAttachInfo*, 1, 64> AICont;

#define AICT_cont(ai) ((ai)->cont)

//This class represents container of miscellaneous AttachInfo.
//The whole object including its field 'cont' are allocated in AttachInfoMgr.
//Note the content which recorded in container should managed by user themself.
class AIContainer {
    friend class AttachInfoMgr;
    COPY_CONSTRUCTOR(AIContainer);
protected:
    AICont cont;
public:
    AIContainer() { init(); }
    ~AIContainer() {}

    void copy(AIContainer const* ai, Region * rg);
    void cleanContainer() { cont.clean(); }
    void clean(AI_TYPE type)
    {
        if (!is_init()) { return; }
        ASSERT0(type > AI_UNDEF && type < AI_LAST);
        for (UINT i = 0; i < cont.get_capacity(); i++) {
            BaseAttachInfo * ac = cont.get(i);
            if (ac != nullptr && ac->getType() == type) {
                cont[i] = nullptr;
                return;
            }
        }
    }

    void destroy() { cont.destroy(); }

    void init() { cont.init(); }
    void init(UINT size, SMemPool * pool) { cont.init(size, pool); }
    INT is_init() const { return cont.is_init(); }

    //The function formats string name for XOC recognized attach-info.
    CHAR const* getAIName(AI_TYPE type) const;
    BaseAttachInfo * get(AI_TYPE type) const
    {
        if (!is_init()) {
            //To faciliate the use of getAI(), disable the assertion of init.
            return nullptr;
        }
        for (UINT i = 0; i < cont.get_capacity(); i++) {
            BaseAttachInfo * ac = cont.get(i);
            if (ac != nullptr && ac->getType() == type) {
                return ac;
            }
        }
        return nullptr;
    }

    AICont const* getContainer() const { return &cont; }

    void set(BaseAttachInfo * c, Region * rg);
};


//Exception Handler Labels.
class EHLabelAttachInfo : public BaseAttachInfo {
    COPY_CONSTRUCTOR(EHLabelAttachInfo);
public:
    xcom::SList<LabelInfo*> labels; //record a list of Labels.

public:
    EHLabelAttachInfo(SMemPool * pool = nullptr) : BaseAttachInfo(AI_EH_LABEL)
    { init(pool); }

    //This function must be invoked during construction.
    void init(SMemPool * pool)
    {
        BaseAttachInfo::init(AI_EH_LABEL);
        labels.init(pool);
    }

    SList<LabelInfo*> const& read_labels() const { return labels; }
    SList<LabelInfo*> & get_labels() { return labels; }
};


class DbxAttachInfo : public BaseAttachInfo {
    COPY_CONSTRUCTOR(DbxAttachInfo);
public:
    Dbx dbx; //record debug info.
    DbxAttachInfo() : BaseAttachInfo(AI_DBX) { init(); }
    void init()
    {
        BaseAttachInfo::init(AI_DBX);
        dbx.clean();
    }
};


class ProfileAttachInfo : public BaseAttachInfo {
    COPY_CONSTRUCTOR(ProfileAttachInfo);
public:
    Sym const* tag;
    INT * data; //truebr freq, falsebr freq.

public:
    ProfileAttachInfo() : BaseAttachInfo(AI_DBX) { init(); }
    void init()
    {
        BaseAttachInfo::init(AI_PROF);
        tag = nullptr;
        data = nullptr;
    }
};


class TbaaAttachInfo : public BaseAttachInfo {
    COPY_CONSTRUCTOR(TbaaAttachInfo);
public:
    Type const* type;
public:
    TbaaAttachInfo() : BaseAttachInfo(AI_TBAA) { type = nullptr; }
};


class MDSSAInfoAttachInfo : public BaseAttachInfo {
    COPY_CONSTRUCTOR(MDSSAInfoAttachInfo);
public:
    MDSSAInfoAttachInfo() : BaseAttachInfo(AI_MD_SSA) {}
};

} //namespace xoc
#endif
