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
#ifndef _DEX_AA_H_
#define _DEX_AA_H_

//Mapping from TYID to MD.
//tyid start from 1.
typedef TMap<Type const*, MD const*> Type2MD;

class DEX_AA : public IR_AA {
    Type2MD m_type2md;
public:
    DEX_AA(Region * rg) : IR_AA(rg) {}

    //Attemp to compute POINT-TO set via data type.
    virtual MD const* computePointToViaType(IR const* ir)
    {
        AIContainer * ai = IR_ai(ir);
        ASSERT0(ir && ai);

        TbaaAttachInfo * ty = (TbaaAttachInfo*)ai->get(AI_TBAA);
        if (ty == NULL) { return NULL; }

        ASSERTN(ty->type, ("Type should not be given if "
                          "you intend to annotate it as TBAA."));
        MD const* md = m_type2md.get(ty->type);
        if (md != NULL) {
            return md;
        }

        CHAR buf[64];
        sprintf(buf, "dummy%d", (UINT)(size_t)ty->type);
        VAR * dummy = m_var_mgr->registerVar(
                        buf, ty->type, 1, VAR_GLOBAL);
        VAR_is_addr_taken(dummy) = true;
        VAR_is_unallocable(dummy) = true;
        m_ru->addToVarTab(dummy);

        MD dummy_md;
        MD_base(&dummy_md) = dummy;
        MD_size(&dummy_md) = 0;
        MD_ty(&dummy_md) = MD_UNBOUND;
        MD const* entry = m_md_sys->registerMD(dummy_md);
        m_type2md.set(ty->type, entry);
        return entry;
    }

    void handle_ld(IR * ld, MD2MDSet * mx)
    {
        ASSERT0(ld->is_ld() && mx);
        AIContainer * ai = IR_ai(ld);
        if (ai == NULL) { return; }

        TbaaAttachInfo * ty = (TbaaAttachInfo*)ai->get(AI_TBAA);
        if (ty == NULL) { return; }

        MD const* md = m_type2md.get(ty->type);
        if (md != NULL) {
            MD const* t = allocLoadMD(ld);
            setPointToMDSetByAddMD(MD_id(t), *mx, md);
            return;
        }

        CHAR buf[64];
        sprintf(buf, "dummy%d", (UINT)(size_t)ty->type);
        VAR * dummy = m_var_mgr->registerVar(
                    buf, m_tm->getMCType(0), 1, VAR_GLOBAL);
        VAR_is_addr_taken(dummy) = true;
        VAR_is_unallocable(dummy) = true;
        m_ru->addToVarTab(dummy);

        MD dummy_md;
        MD_base(&dummy_md) = dummy;
        MD_size(&dummy_md) = 0;
        MD_ty(&dummy_md) = MD_UNBOUND;
        MD const* entry = m_md_sys->registerMD(dummy_md);
        m_type2md.set(ty->type, entry);

        MD const* t = allocLoadMD(ld);
        setPointToMDSetByAddMD(MD_id(t), *mx, entry);
    }
};

#endif
