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
#include "cominc.h"

namespace xoc {

VarFlagDesc const g_varflag_desc[] = {
    { VAR_UNDEF, "undef", }, //idx0
    { VAR_GLOBAL, "global", }, //idx1
    { VAR_LOCAL, "local", }, //idx2
    { VAR_PRIVATE, "private", }, //idx3
    { VAR_READONLY, "readonly", }, //idx4
    { VAR_VOLATILE, "volatile", }, //idx5
    { VAR_HAS_INIT_VAL, "hasInitVal", }, //idx6
    { VAR_FAKE, "fake", }, //idx7
    { VAR_IS_LABEL, "label", }, //idx8
    { VAR_IS_FUNC, "func", }, //idx9
    { VAR_IS_ARRAY, "array", }, //idx10
    { VAR_IS_FORMAL_PARAM, "formal_param", }, //idx11
    { VAR_IS_SPILL, "spill", }, //idx12
    { VAR_ADDR_TAKEN, "addr_taken", }, //idx13
    { VAR_IS_PR, "$", }, //idx14
    { VAR_IS_RESTRICT, "restrict", }, //idx15
    { VAR_IS_UNALLOCABLE, "unallocable", }, //idx16
    { VAR_IS_DECL, "decl", }, //idx17
    { VAR_IS_REGION, "region", }, //idx18
    { VAR_IS_ENTRY, "entry", }, //idx19
};
static UINT g_varflag_num = sizeof(g_varflag_desc) / sizeof(g_varflag_desc[0]);

//Represents flags that used in GR file.
static VAR_FLAG g_grmode_flag[] = {
    VAR_IS_DECL,
    VAR_IS_UNALLOCABLE,
    VAR_IS_FUNC,
    VAR_PRIVATE,
    VAR_READONLY,
    VAR_VOLATILE,
    VAR_IS_RESTRICT,
    VAR_FAKE,
    VAR_IS_LABEL,
    VAR_IS_ARRAY,
};
static UINT g_grmode_flag_num = sizeof(g_grmode_flag) /
                                sizeof(g_grmode_flag[0]);

//
//START VarFlag
//
bool VarFlag::verify() const
{
    VarFlag::Iter it;
    for (VAR_FLAG v = (VAR_FLAG)get_first_flag(it); !end(it);
         v = (VAR_FLAG)get_next_flag(it)) {
        ASSERT0_DUMMYUSE(v > VAR_UNDEF &&
                         VarFlagDesc::getDescIdx(v) < g_varflag_num);
    }
    return true;
}
//END VarFlag


//
//START VarFlagDesc
//
CHAR const* VarFlagDesc::getName(VAR_FLAG flag)
{
    if (flag == VAR_UNDEF) { return g_varflag_desc[0].name; }
    VarFlag v(flag);
    xcom::ROBitSet bs((BYTE const*)&v.getFlagSet(), v.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_varflag_num);
    return g_varflag_desc[idx].name;
}


UINT VarFlagDesc::getDescIdx(VAR_FLAG flag)
{
    if (flag == VAR_UNDEF) { return 0; }
    VarFlag v(flag);
    xcom::ROBitSet bs((BYTE const*)&v.getFlagSet(), v.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_varflag_num);
    return idx;
}


VAR_FLAG VarFlagDesc::getFlag(UINT idx)
{
    ASSERT0(idx < g_varflag_num);
    return g_varflag_desc[idx].flag;
}
//END VarFlagDesc


VarLinkAttrDesc const g_var_link_attr_desc[] = {
    { VAR_LINK_ATTR_UNDEF, "undef", }, //idx0
    { VAR_LINK_ATTR_WEAK, "weak", }, //idx1
    { VAR_LINK_ATTR_VISIBLE, "visible", }, //idx2
    { VAR_LINK_ATTR_EXTERN, "extern", }, //idx3
};

static UINT g_var_link_attr_num = sizeof(g_var_link_attr_desc) /
                                  sizeof(g_var_link_attr_desc[0]);


//
//START VarLinkAttr
//
bool VarLinkAttr::verify() const
{
    VarLinkAttr::Iter it;
    for (VAR_LINK_ATTR attr = (VAR_LINK_ATTR)get_first_flag(it); !end(it);
         attr = (VAR_LINK_ATTR)get_next_flag(it)) {
        ASSERT0_DUMMYUSE(attr > VAR_LINK_ATTR_UNDEF &&
                         VarLinkAttrDesc::getDescIdx(attr) <
                         g_var_link_attr_num);
    }
    return true;
}
//END VarLinkAttr


//
//START VarLinkAttrDesc
//
CHAR const* VarLinkAttrDesc::getName(VAR_LINK_ATTR link_attr)
{
    if (link_attr == VAR_LINK_ATTR_UNDEF) {
        return g_var_link_attr_desc[0].name;
    }
    VarLinkAttr attr(link_attr);
    xcom::ROBitSet bs((BYTE const*)&attr.getFlagSet(), attr.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_var_link_attr_num);
    return g_var_link_attr_desc[idx].name;
}


UINT VarLinkAttrDesc::getDescIdx(VAR_LINK_ATTR link_attr)
{
    if (link_attr == VAR_LINK_ATTR_UNDEF) { return 0; }
    VarLinkAttr attr(link_attr);
    xcom::ROBitSet bs((BYTE const*)&attr.getFlagSet(), attr.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_var_link_attr_num);
    return idx;
}


VAR_LINK_ATTR VarLinkAttrDesc::getAttr(UINT idx)
{
    ASSERT0(idx < g_var_link_attr_num);
    return g_var_link_attr_desc[idx].attr;
}
//END VarLinkAttrDesc


void VarTab::dump(VarMgr const* vm) const
{
    if (!vm->getRegionMgr()->isLogMgrInit()) { return; }
    ASSERT0(vm);
    VarTabIter iter;
    for (Var * v = get_first(iter); v != nullptr; v = get_next(iter)) {
        v->dump(vm);
    }
}


void ConstVarTab::dump(VarMgr const* vm) const
{
    if (!vm->getRegionMgr()->isLogMgrInit()) { return; }
    ASSERT0(vm);
    ConstVarTabIter iter;
    for (Var const* v = get_first(iter); v != nullptr; v = get_next(iter)) {
        v->dump(vm);
    }
}


VarMgr::VarMgr(RegionMgr * rm)
{
    ASSERT0(rm);
    m_var_count = VAR_ID_UNDEF + 1; //for enjoying bitset.
    m_str_count = 1;
    m_rm = rm;
    m_tm = rm->getTypeMgr();
}


//
//START Var
//
Var::Var() : varflag(VAR_UNDEF), var_link_attr(VAR_LINK_ATTR_UNDEF)
{
    VAR_id(this) = 0; //unique id;
    VAR_align(this) = 0;
    VAR_formal_param_pos(this) = 0;
    VAR_prno(this) = PRNO_UNDEF;
    VAR_type(this) = UNDEF_TYID;
    VAR_name(this) = nullptr;
    VAR_storage_space(this) = SS_UNDEF;
    VAR_string(this) = nullptr;
    VAR_flag(this).clean(); //Record various properties of variable.
}


static void dumpIndent(LogMgr * lm)
{
    UINT indent = lm->getIndent();
    for (; indent > 0; indent--) {
        prt(lm, "%c", lm->getIndentChar());
    }
}


void Var::dump(VarMgr const* vm) const
{
    if (!vm->getRegionMgr()->isLogMgrInit()) { return; }
    note(vm->getRegionMgr(), "\n");
    dumpIndent(vm->getRegionMgr()->getLogMgr());

    StrBuf buf(64);
    prt(vm->getRegionMgr(), "%s", dump(buf, vm));
}


void Var::dumpFlag(xcom::StrBuf & buf, bool grmode, bool & first) const
{
    BitSet grmode_flag_idx_set;
    for (UINT i = 0; i < g_grmode_flag_num; i++) {
        grmode_flag_idx_set.bunion(VarFlagDesc::getDescIdx(g_grmode_flag[i]));
    }
    VarFlag::Iter it;
    for (VAR_FLAG v = (VAR_FLAG)VAR_flag(this).get_first_flag(it);
         !VAR_flag(this).end(it);
         v = (VAR_FLAG)VAR_flag(this).get_next_flag(it)) {
        UINT idx = VarFlagDesc::getDescIdx(v);
        if (grmode && !grmode_flag_idx_set.is_contain(idx)) { continue; }
        if (!first) { buf.strcat(","); }
        first = false;
        CHAR const* name = VarFlagDesc::getName(v);
        buf.strcat(name);
    }
}


void Var::dumpInitVal(bool first, xcom::StrBuf & buf, bool grmode) const
{
    if (hasInitString()) {
        ASSERT0(is_string());
        if (!first) {
            buf.strcat(",");
        }
        first = false;

        //Add back-slash to translate '"' and '\\'.
        CHAR const* local_string = getString()->getStr();
        UINT quote_num = 0;
        UINT len = 0;
        for (CHAR const* p = local_string; *p != 0; p++, len++) {
            if (*p == '"' || *p == '\\') {
                quote_num++;
            }
        }
        CHAR * local_buf = nullptr;
        if (quote_num != 0) {
            UINT i = 0;
            len += quote_num;
            if (len < HOST_STACK_MAX_USABLE_MEMORY_BYTE_SIZE) {
                local_buf = (CHAR*)ALLOCA(len + 1);
            } else {
                local_buf = (CHAR*)::malloc(len + 1);
            }
            for (CHAR const* q = local_string; *q != 0; q++, i++) {
                if (*q == '"' || *q == '\\') {
                    local_buf[i] = '\\';
                    i++;
                }
                local_buf[i] = *q;
            }
            local_buf[len] = 0;
            local_string = local_buf;
        }
        buf.strcat("string(\"%s\")", local_string);
        if (local_buf != nullptr &&
            len >= HOST_STACK_MAX_USABLE_MEMORY_BYTE_SIZE) {
            ::free(local_buf);
        }
        return;
    }
    if (hasInitVal()) {
        ASSERTN(!is_string(),
                ("initial string value should be recorded in VAR_string"));
        ASSERT0(getByteValue());
        //Initial value can NOT be nullptr.
        if (!first) {
            buf.strcat(",");
        }
        first = false;
        buf.strcat("byte(");
        BYTE const* p = getByteValue()->getBuffer();
        UINT size = getByteValue()->getSize();
        ASSERT0(p);
        buf.strcat("0x%x", (BYTE)*p);
        UINT i = 1;
        for (p++; i < size; i++, p++) {
            buf.strcat(",0x%x", (BYTE)*p);
        }
        buf.strcat(")");
    }
}


void Var::dumpProp(xcom::StrBuf & buf, bool grmode) const
{
    bool first = true;
    dumpFlag(buf, grmode, first);
    if (get_align() > 1) {
        if (!first) {
            buf.strcat(",");
        }
        first = false;
        buf.strcat("align(%d)", get_align());
    }
    dumpInitVal(first, buf, grmode);
}


CHAR const* Var::dumpGR(StrBuf & buf, TypeMgr * dm) const
{
    xcom::StrBuf buf2(32);
    xcom::StrBuf buf3(32);
    buf.strcat("var %s:%s", GRDump::compositeName(VAR_name(this), buf3),
        dm->dump_type(getType(), buf2));
    if (hasGRFlag() || get_align() > 1) {
        buf.strcat(":(");
        dumpProp(buf, true);
        buf.strcat(")");
    }
    return buf.buf;
}


//You must make sure this function will not change any field of Var.
CHAR const* Var::dump(OUT StrBuf & buf, VarMgr const* vm) const
{
    CHAR * lname = SYM_name(VAR_name(this));
    CHAR tt[43];
    if (xcom::xstrlen(lname) > 43) {
        ::memcpy(tt, lname, 43);
        tt[39] = '.';
        tt[40] = '.';
        tt[41] = '.';
        tt[42] = 0;
        lname = tt;
    }
    buf.strcat("Var%d(%s):", VAR_id(this), lname);
    dumpProp(buf, false);

    Type const* ltype = VAR_type(this);
    ASSERT0(ltype);
    if (is_string()) {
        buf.strcat(",str");
    }
    if (is_pointer()) {
        buf.strcat(",pointer,pt_base_sz:%d", TY_ptr_base_size(ltype));
    }

    buf.strcat(",%s", TypeMgr::getDTypeName(TY_dtype(ltype)));
    if (TY_dtype(ltype) > D_F128) {
        if (ltype->is_any()) {
            buf.strcat(",mem_size:ANY");
        } else {
            buf.strcat(",mem_size:%d",
                getByteSize(vm->getRegionMgr()->getTypeMgr()));
        }
    }

    if (getStorageSpace() != SS_UNDEF) {
        buf.strcat(",storage_space:%s",
                   StorageSpaceDesc::getName(getStorageSpace()));
    }

    buf.strcat(",decl:'");
    dumpVARDecl(buf, vm);
    buf.strcat("'");
    ASSERT0(VAR_flag(this).verify());
    ASSERT0(VAR_link_attr(this).verify());
    return buf.buf;
}
//END Var


//
//START VarMgr
//
//Free Var memory.
void VarMgr::destroyVar(Var * v)
{
    ASSERT0(v->id() != VAR_ID_UNDEF);
    m_freelist_of_varid.bunion(v->id(), *m_rm->getSBSMgr());
    m_var_vec.set(v->id(), nullptr);
    if (v->is_string() && v->hasInitString()) {
        //User may declare a empty string.
        //e.g: var gc:str:(fake,align(4));
        m_str_tab.remove(v->getString());
    }
    delete v;
}


void VarMgr::destroy()
{
    for (VecIdx i = 0; i <= m_var_vec.get_last_idx(); i++) {
        Var * v = m_var_vec.get((UINT)i);
        if (v == nullptr) { continue; }
        delete v;
    }

    m_freelist_of_varid.clean(*m_rm->getSBSMgr());
}


bool VarMgr::isDedicatedStringVar(CHAR const* name) const
{
    return ::strcmp(name, DEDICATED_STRING_VAR_NAME) == 0;
}


void VarMgr::assignVarId(Var * v)
{
    DefSBitSetIter iter = nullptr;
    BSIdx id = m_freelist_of_varid.get_first(&iter);
    ASSERT0(id != VAR_ID_UNDEF);
    if (IS_BSUNDEF(id)) {
        VAR_id(v) = (UINT)m_var_count++;
    } else {
        m_freelist_of_varid.diff(id, *m_rm->getSBSMgr());
        VAR_id(v) = id;
    }
    ASSERTN(VAR_id(v) < VAR_ID_MAX, ("too many variables"));
    ASSERT0(m_var_vec.get(VAR_id(v)) == nullptr);
    m_var_vec.set(VAR_id(v), v);
}


//Find variable by 'name'.
//Note there may be multiple variable with same name, this function return
//the first.
Var * VarMgr::findVarByName(Sym const* name)
{
    for (VecIdx i = 0; i <= m_var_vec.get_last_idx(); i++) {
        Var * v = m_var_vec.get(i);
        if (v == nullptr) { continue; }
        if (v->get_name() == name) {
            return v;
        }
    }
    return nullptr;
}


Var * VarMgr::registerVar(CHAR const* varname, Type const* type, UINT align,
                          VarFlag const& flag)
{
    ASSERT0(varname);
    Sym const* sym = m_rm->addToSymbolTab(varname);
    return registerVar(sym, type, align, flag);
}


Var * VarMgr::registerVar(Sym const* var_name, Type const* type, UINT align,
                          VarFlag const& flag)
{
    ASSERT0(type);
    ASSERTN(var_name, ("variable need a name"));

    //Var is string type, but not const string.
    //ASSERTN(!type->is_string(), ("use registerStringVar instead of"));

    Var * v = allocVAR();
    VAR_type(v) = type;
    VAR_name(v) = var_name;
    VAR_align(v) = align;
    VAR_flag(v) = flag;
    assignVarId(v);
    return v;
}


Var * VarMgr::registerStringVar(CHAR const* var_name, Sym const* s, UINT align)
{
    ASSERT0(s);
    Var * v;
    if ((v = m_str_tab.get(s)) != nullptr) {
        return v;
    }
    v = allocVAR();
    if (var_name == nullptr) {
        StrBuf buf(64);
        buf.sprint("_const_string_%lu", (ULONG)m_str_count++);
        VAR_name(v) = m_rm->addToSymbolTab(buf.buf);
    } else {
        VAR_name(v) = m_rm->addToSymbolTab(var_name);
    }
    VAR_string(v) = s;
    VAR_type(v) = m_tm->getString();
    VAR_align(v) = align;
    v->setToGlobal(true);
    assignVarId(v);
    m_str_tab.set(s, v);
    return v;
}


void VarMgr::dumpFreeIDList() const
{
    if (m_freelist_of_varid.is_empty()) { return; }

    RegionMgr * rm = m_tm->getRegionMgr();
    prt(rm, "\nVarMgr: FREE VAR ID:");
    DefSBitSetIter iter = nullptr;
    bool first = true;
    for (BSIdx id = m_freelist_of_varid.get_first(&iter);
         id != BS_UNDEF; id = m_freelist_of_varid.get_next(id, &iter)) {
        if (!first) { prt(rm, ","); }
        prt(rm, "%d", id);
        first = false;
    }
}


//Dump all variables registered.
void VarMgr::dump() const
{
    RegionMgr * rm = m_tm->getRegionMgr();
    prt(rm, "\n\nVAR to Decl Mapping:");
    StrBuf buf(64);
    for (VecIdx i = 0; i <= m_var_vec.get_last_idx(); i++) {
        Var const* v = m_var_vec.get(i);
        if (v == nullptr) { continue; }
        buf.clean();
        prt(rm, "\n%s", v->dump(buf, this));
    }
    prt(rm, "\n");
    dumpFreeIDList();
}


bool VarMgr::verifyVar(Var const* v) const
{
    ASSERTN(v->is_global() ^ v->is_local(), ("Var flag is conflict"));
    return true;
}


bool VarMgr::verifyAllVar() const
{
    for (VecIdx i = 0; i <= m_var_vec.get_last_idx(); i++) {
        Var const* v = m_var_vec.get(i);
        if (v == nullptr) { continue; }
        verifyVar(v);
    }
    return true;
}
//END VarMgr

} //namespace xoc
