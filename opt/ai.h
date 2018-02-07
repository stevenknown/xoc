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


class BaseAttachInfo {
public:
    AI_TYPE type;

public:
    explicit BaseAttachInfo(AI_TYPE t) { init(t); }
    COPY_CONSTRUCTOR(BaseAttachInfo);
    ~BaseAttachInfo() {}

    void init(AI_TYPE t) { type = t; }
};


//This class represents container of miscellaneous AttachInfo.
typedef SimpleVec<BaseAttachInfo*, 1> AICont;
class AIContainer {
protected:
    AICont cont;

public:
    AIContainer() { init(); }
    COPY_CONSTRUCTOR(AIContainer);
    ~AIContainer() {}

    void init()
    {
        if (cont.is_init()) { return; }
        cont.init();
    }

    INT is_init() const { return cont.is_init(); }

    void destroy() { cont.destroy(); }
    void destroy_vec() { cont.destroy_vec(); }

    void copy(AIContainer const* ai)
    {
        ASSERT0(ai);
        if (!ai->is_init()) { return; }
        cont.copy(ai->cont);
    }

    void clean(AI_TYPE type)
    {
        if (!cont.is_init()) { return; }
        ASSERT0(type > AI_UNDEF && type < AI_LAST);
        for (UINT i = 0; i < cont.get_capacity(); i++) {
            BaseAttachInfo * ac = cont.get(i);
            if (ac != NULL && ac->type == type) {
                cont.set(i, NULL);
                return;
            }
        }
    }

    void set(BaseAttachInfo * c)
    {
        ASSERT(c, ("Can not set empty AI"));

        INT emptyslot = -1;
        if (!cont.is_init()) { cont.init(); }

        AI_TYPE type = c->type;
        ASSERT0(type > AI_UNDEF && type < AI_LAST);

        UINT i;
        for (i = 0; i < cont.get_capacity(); i++) {
            BaseAttachInfo * ac = cont.get(i);
            if (ac == NULL) {
                emptyslot = (INT)i;
            } else if (ac->type != type) {
                continue;
            }

            //Note c will override the prior AIContainer that has same type.
            cont.set(i, c);
            return;
        }

        if (emptyslot != -1) {
            cont.set((UINT)emptyslot, c);
        } else {
            //AIContainer buffer will grow bigger.
            cont.set(i, c);
        }
    }

    BaseAttachInfo * get(AI_TYPE type) const
    {
        if (!cont.is_init()) { return NULL; }
        for (UINT i = 0; i < cont.get_capacity(); i++) {
            BaseAttachInfo * ac = cont.get(i);
            if (ac != NULL && ac->type == type) {
                return ac;
            }
        }
        return NULL;
    }

    AICont const& read_cont() const { return cont; }

    CHAR const* get_ai_name(AI_TYPE type) const
    {
        switch (type) {
        case AI_UNDEF: return "Undef";
        case AI_DBX: return "Dbx";
        case AI_PROF: return "Prof";
        case AI_TBAA: return "Tbaa";
        case AI_EH_LABEL: return "EH";
        case AI_USER_DEF: return "UserDef";
        case AI_MD_SSA: return "MDSSA";
        case AI_LAST:;
        default: UNREACH();
        }
        return NULL;
    }
};


//Exception Handler Labels.
class EHLabelAttachInfo : public BaseAttachInfo {
public:
    SList<LabelInfo*> labels; //record a list of Labels.

public:
    EHLabelAttachInfo(SMemPool * pool = NULL) : BaseAttachInfo(AI_EH_LABEL)
    { init(pool); }
    COPY_CONSTRUCTOR(EHLabelAttachInfo);

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
public:
    Dbx dbx; //record debug info.

    DbxAttachInfo() : BaseAttachInfo(AI_DBX) { init(); }
    COPY_CONSTRUCTOR(DbxAttachInfo);

    void init()
    {
        BaseAttachInfo::init(AI_DBX);
        dbx.clean();
    }
};


class ProfileAttachInfo : public BaseAttachInfo {
public:
    SYM const* tag;

    //truebr freq, falsebr freq.
    INT * data;

    ProfileAttachInfo() : BaseAttachInfo(AI_DBX) { init(); }
    COPY_CONSTRUCTOR(ProfileAttachInfo);

    void init()
    {
        BaseAttachInfo::init(AI_PROF);
        tag = NULL;
        data = NULL;
    }
};


class TbaaAttachInfo : public BaseAttachInfo {
public:
    Type const* type;

    TbaaAttachInfo() : BaseAttachInfo(AI_TBAA) { type = NULL; }
    COPY_CONSTRUCTOR(TbaaAttachInfo);
};


class MDSSAInfoAttachInfo : public BaseAttachInfo {
public:
    MDSSAInfoAttachInfo() : BaseAttachInfo(AI_MD_SSA) {}
    COPY_CONSTRUCTOR(MDSSAInfoAttachInfo);
};

} //namespace xoc
#endif
