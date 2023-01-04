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
    { VAR_HAS_INIT_VAL, "has_init_val", }, //idx6
    { VAR_FAKE, "fake", }, //idx7
    { VAR_IS_LABEL, "label", }, //idx8
    { VAR_IS_FUNC, "func", }, //idx9
    { VAR_IS_ARRAY, "array", }, //idx10
    { VAR_IS_FORMAL_PARAM, "formal_param", }, //idx11
    { VAR_IS_SPILL, "spill", }, //idx12
    { VAR_ADDR_TAKEN, "addr_taken", }, //idx13
    { VAR_IS_PR, "pr", }, //idx14
    { VAR_IS_RESTRICT, "restrict", }, //idx15
    { VAR_IS_UNALLOCABLE, "unallocable", }, //idx16
    { VAR_IS_DECL, "decl", }, //idx17
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
    ROBitSet bs((BYTE const*)&v.getFlagSet(), v.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_varflag_num);
    return g_varflag_desc[idx].name;
}


UINT VarFlagDesc::getDescIdx(VAR_FLAG flag)
{
    if (flag == VAR_UNDEF) { return 0; }
    VarFlag v(flag);
    ROBitSet bs((BYTE const*)&v.getFlagSet(), v.getFlagSetSize());
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


void VarTab::dump(TypeMgr const* tm) const
{
    if (!tm->getRegionMgr()->isLogMgrInit()) { return; }
    ASSERT0(tm);
    VarTabIter iter;
    for (Var * v = get_first(iter); v != nullptr; v = get_next(iter)) {
        v->dump(tm);
    }
}


void ConstVarTab::dump(TypeMgr * tm)
{
    if (!tm->getRegionMgr()->isLogMgrInit()) { return; }
    ASSERT0(tm);
    ConstVarTabIter iter;
    for (Var const* v = get_first(iter); v != nullptr; v = get_next(iter)) {
        v->dump(tm);
    }
}


VarMgr::VarMgr(RegionMgr * rm)
{
    ASSERT0(rm);
    m_var_count = VAR_ID_UNDEF + 1; //for enjoying bitset.
    m_str_count = 1;
    m_ru_mgr = rm;
    m_tm = rm->getTypeMgr();
}


//
//START Var
//
Var::Var() : varflag(VAR_UNDEF)
{
    VAR_id(this) = 0; //unique id;
    VAR_type(this) = UNDEF_TYID;
    VAR_name(this) = nullptr;
    VAR_string(this) = nullptr;
    VAR_flag(this).clean(); //Record various properties of variable.
    align = 0;
}


static void dumpIndent(LogMgr * lm)
{
    UINT indent = lm->getIndent();
    for (; indent > 0; indent--) {
        prt(lm, "%c", lm->getIndentChar());
    }
}


void Var::dump(TypeMgr const* tm) const
{
    if (!tm->getRegionMgr()->isLogMgrInit()) { return; }
    note(tm->getRegionMgr(), "\n");
    dumpIndent(tm->getRegionMgr()->getLogMgr());

    StrBuf buf(64);
    prt(tm->getRegionMgr(), "%s", dump(buf, tm));
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
    if (is_string() && getString() != nullptr) {
        if (!first) {
            buf.strcat(",");
        }
        first = false;

        //Add back-slash to translate '"' and '\\'.
        CHAR const* local_string = SYM_name(getString());
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
    } else if (has_init_val()) {
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
CHAR const* Var::dump(OUT StrBuf & buf, TypeMgr const* dm) const
{
    CHAR * lname = SYM_name(VAR_name(this));
    CHAR tt[43];
    if (xstrlen(lname) > 43) {
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
            buf.strcat(",mem_size:%d", getByteSize(dm));
        }
    }

    buf.strcat(",decl:'");
    dumpVARDecl(buf);
    buf.strcat("'");
    ASSERT0(VAR_flag(this).verify());
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
    m_freelist_of_varid.bunion(v->id(), *m_ru_mgr->get_sbs_mgr());
    m_var_vec.set(v->id(), nullptr);
    if (v->is_string() && v->getString() != nullptr) {
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

    m_freelist_of_varid.clean(*m_ru_mgr->get_sbs_mgr());
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
        m_freelist_of_varid.diff(id, *m_ru_mgr->get_sbs_mgr());
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


//Add Var into VarTab.
//Note you should call this function cafefully, and make sure
//the Var is unique. This function does not keep the uniqueness
//related to properties.
//'var_name': name of the variable, it is optional.
Var * VarMgr::registerVar(CHAR const* varname, Type const* type, UINT align,
                          UINT flag)
{
    ASSERT0(varname);
    Sym const* sym = m_ru_mgr->addToSymbolTab(varname);
    return registerVar(sym, type, align, flag);
}


//Add Var into VarTab.
//Note you should call this function cafefully, and make sure
//the Var is unique. This function does not keep the uniqueness
//related to properties.
//'var_name': name of the variable, it is optional.
Var * VarMgr::registerVar(Sym const* var_name, Type const* type, UINT align,
                          UINT flag)
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


//Register Var for const string.
//Return Var if there is already related to 's',
//otherwise create a new Var.
//'var_name': name of the variable, it is optional.
//'s': string's content.
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
        VAR_name(v) = m_ru_mgr->addToSymbolTab(buf.buf);
    } else {
        VAR_name(v) = m_ru_mgr->addToSymbolTab(var_name);
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
        Var * v = m_var_vec.get(i);
        if (v == nullptr) { continue; }

        buf.clean();
        prt(rm, "\n%s", v->dump(buf, m_tm));
    }
    prt(rm, "\n");
    dumpFreeIDList();
}
//END VarMgr

} //namespace xoc
