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
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexOpcodes.h"
#include "liropcode.h"
#include "lir.h"
#include "cominc.h"
#include "comopt.h"
#include "dx_mgr.h"
#include "dex.h"
#include "trycatch_info.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"
#include "dex2ir.h"

inline static bool is_array_type(CHAR const* type_name)
{
    ASSERT0(type_name);
    return *type_name == '[';
}


inline static bool is_obj_type(CHAR const* type_name)
{
    ASSERT0(type_name);
    return *type_name == 'L';
}

Dex2IR::Dex2IR(IN Region * rg,
               IN DexFile * df,
               IN LIRCode * fu,
               DbxVec const& dbxvec) : m_dbxvec(dbxvec)
{
    ASSERT0(rg && df && fu);
    m_ru = (DexRegion*)rg;
    m_ru_mgr = (DexRegionMgr*)rg->getRegionMgr();
    m_tm = rg->getTypeMgr();
    m_vm = rg->getVarMgr();
    m_df = df;
    m_var2fieldid = m_ru->getVAR2Fieldid();
    m_lircode = fu;
    m_tr = ((DexRegion*)rg)->getTypeIndexRep();
    m_ti = NULL;
    m_pool = smpoolCreate(16, MEM_COMM);
    m_pr2v.init(MAX(4, getNearestPowerOf2(fu->maxVars)));
    m_ptr_addend = m_tm->getSimplexType(D_U32);
    m_ofst_addend = m_tm->get_dtype_bytesize(D_I64);
    m_pr2v.maxreg = fu->maxVars - 1;
    m_pr2v.paramnum = fu->numArgs;
    m_current_catch_list = NULL;
}


//Add a variable of CLASS.
VAR * Dex2IR::addVarByName(CHAR const* name, Type const* ty)
{
    SYM * sym = m_ru_mgr->addToSymbolTab(name);
    VAR * v = m_str2var.get(sym);
    if (v != NULL) { return v; }
    UINT flag = 0;
    SET_FLAG(flag, VAR_GLOBAL);
    v = m_vm->registerVar(sym, ty, 0, flag);

    //Record global class,method symbol at RegionMgr.
    //Record global class,method variable at program region.
    ASSERT0(m_ru_mgr->getProgramRegion());
    if (m_ru_mgr->getSym2Var().get(sym) == NULL) {
        m_ru_mgr->getProgramRegion()->addToVarTab(v);
        m_ru_mgr->getSym2Var().set(sym, v);
    }

    m_str2var.set(sym, v);
    return v;
}


VAR * Dex2IR::addStringVar(CHAR const* string)
{
    CHAR const* local_string = string;
    //UINT quote_num = 0;
    //UINT len = 0;
    //for (CHAR const* p = string; *p != 0; p++, len++) {
    //    if (*p == '"' || *p == '\\') {
    //        quote_num++;
    //    }
    //}
    //if (quote_num != 0) {
    //    UINT i = 0;
    //    CHAR * buf = (CHAR*)ALLOCA(len + quote_num + 1);
    //    for (CHAR const* q = string; *q != 0; q++, i++) {
    //        if (*q == '"' || *q == '\\') {
    //            buf[i] = '\\';
    //            i++;
    //        }
    //        buf[i] = *q;
    //    }
    //    local_string = buf;
    //}
    SYM * sym = m_ru_mgr->addToSymbolTab(local_string);
    VAR * v = m_str2var.get(sym);
    if (v != NULL) { return v; }
    v = m_vm->registerStringVar(NULL, sym, 0);

    //Record string at RegionMgr.
    ASSERT0(m_ru_mgr->getProgramRegion());
    if (m_ru_mgr->getSym2Var().get(sym) == NULL) {
        m_ru_mgr->getProgramRegion()->addToVarTab(v);
        m_ru_mgr->getSym2Var().set(sym, v);
    }

    m_str2var.set(sym, v);
    return v;
}


//Add a variable of CLASS.
VAR * Dex2IR::addClassVar(UINT class_id, LIR * lir)
{
    DexClassDef const* class_info = dexGetClassDef(m_df, class_id);
    ASSERT0(class_info);
    CHAR const* class_name = dexStringByTypeIdx(m_df, class_info->classIdx);
    ASSERT0(class_name);
    return addVarByName(class_name, getType(lir));
}


/*
Add a variable of CLASS + FIELD.

'field_id': the global index in class-defining-tab of dexfile.
e.g: Given field_id is 6, the class-defining-tab is:
    class LSwitch; {
         0th, field_1, Type:I
         1th, field_2, Type:S
         2th, field_3, Type:C
         3th, field_4, Type:D
    }
    class LTT; {
         4th, tt_f1, Type:I
         5th, tt_f2, Type:I
    }
    class LUI; {
         6th, u1, Type:S
         7th, u2, Type:J
    }

    Field LUI;::u1 is the target.
*/
VAR * Dex2IR::addFieldVar(UINT field_id, Type const* ty)
{
    DexFieldId const* fid = dexGetFieldId(m_df, field_id);
    ASSERT0(fid);
    CHAR const* class_name = dexStringByTypeIdx(m_df, fid->classIdx);
    CHAR const* field_name = get_field_name(m_df, field_id);
    ASSERT0(class_name && field_name);
    UINT cls_pos_id = computeClassPosid(fid->classIdx);
    DexClassDef const* class_info = dexGetClassDef(m_df, cls_pos_id);
    ASSERT0(class_info);
    CHAR buf[256];
    CHAR * pbuf = buf;
    UINT l = strlen(class_name) + strlen(field_name) + 10;
    if (l >= 256) {
        pbuf = (CHAR*)ALLOCA(l);
        ASSERT0(pbuf);
    }
    sprintf(pbuf, "%s::%s", class_name, field_name);
    return addVarByName(pbuf, ty);
}


CHAR const* Dex2IR::get_var_type_name(UINT field_id)
{
    DexFieldId const* fid = dexGetFieldId(m_df, field_id);
    ASSERT0(fid);
    return dexStringByTypeIdx(m_df, fid->typeIdx);
}


VAR * Dex2IR::addStaticVar(UINT field_id, Type const* ty)
{
    DexFieldId const* fid = dexGetFieldId(m_df, field_id);
    ASSERT0(fid);
    CHAR const* type_name = dexStringByTypeIdx(m_df, fid->typeIdx);
    CHAR const* class_name = dexStringByTypeIdx(m_df, fid->classIdx);
    CHAR const* field_name = get_field_name(m_df, field_id);
    ASSERT0(class_name && field_name && type_name);
    CHAR buf[256];
    CHAR * pbuf = buf;
    UINT l = strlen(class_name) + strlen(field_name) + strlen(type_name) + 10;
    if (l >= 256) {
        pbuf = (CHAR*)ALLOCA(l);
    }
    sprintf(pbuf, "%s::%s:%s", class_name, field_name, type_name);
    return addVarByName(pbuf, ty);
}


static CHAR const* g_readonly_method [] = {
    "cos",
    "sin",
    "asin",
    "acos",
    "tan",
    "atan",
    "println",
};
UINT const g_readonly_method_num =
    sizeof(g_readonly_method) / sizeof(g_readonly_method[0]);


bool Dex2IR::is_readonly(CHAR const* method_name) const
{
    for (UINT i = 0; i < g_readonly_method_num; i++) {
        if (strcmp(method_name, g_readonly_method[i]) == 0) {
            return true;
        }
    }
    return false;
}


//'method_id': the global index in method-defining-tab of dexfile.
VAR * Dex2IR::addFuncVar(UINT method_id, Type const* ty)
{
    DexMethodId const* mid = dexGetMethodId(m_df, method_id);
    ASSERT0(mid);
    CHAR const* class_name = dexStringByTypeIdx(m_df, mid->classIdx);
    CHAR const* func_name = dexStringById(m_df, mid->nameIdx);
    CHAR const* func_param_type = get_func_type(m_df, mid);
    ASSERT0(class_name && func_name && func_param_type);
    CHAR buf[256];
    CHAR * pbuf = buf;
    UINT l = strlen(class_name) +
             strlen(func_name) +
             strlen(func_param_type) + 10;
    if (l >= 256) {
        pbuf = (CHAR*)ALLOCA(l);
        ASSERT0(pbuf);
    }

    //Function string is consist of these.
    pbuf = assemblyUniqueName(pbuf, class_name, func_param_type, func_name);
    VAR * v = addVarByName(pbuf, ty);
    VAR_is_readonly(v) = is_readonly(func_name);
    VAR_is_func_decl(v) = true;
    return v;
}


/*
Return type-id.
Type                    Size (bytes)
java.lang.Object        8
java.lang.Float            16
java.lang.Double        16
java.lang.Integer        16
java.lang.Long            16
java.math.BigInteger    56 (*)
java.lang.BigDecimal    72 (*)
java.lang.String        2*(Length) + 38(+/-)2
empty java.util.Vector    80
object reference        4
float array                4*(Length) + 14(+/-)2

For common 32-bit JVMs specifications a plain java.lang.
Object takes up 8 bytes, and the basic data types are
usually of the least physical size that can accommodate
the language requirements (except boolean takes up a whole byte):
java.lang.Object shell size in bytes:
public static final int OBJECT_SHELL_SIZE   = 8;
public static final int OBJREF_SIZE         = 4;
public static final int LONG_FIELD_SIZE     = 8;
public static final int INT_FIELD_SIZE      = 4;
public static final int SHORT_FIELD_SIZE    = 2;
public static final int CHAR_FIELD_SIZE     = 2;
public static final int BYTE_FIELD_SIZE     = 1;
public static final int BOOLEAN_FIELD_SIZE  = 1;
public static final int DOUBLE_FIELD_SIZE   = 8;
public static final int FLOAT_FIELD_SIZE    = 4;
*/
Type const* Dex2IR::getType(LIR * ir)
{
    switch (LIR_dt(ir)) {
    case LIR_JDT_unknown:
        UNREACHABLE();
    case LIR_JDT_void    :
        return m_tr->i32;
    case LIR_JDT_int    :
        //a Java int is 32 bits in all JVMs and on all platforms,
        //but this is only a language specification requirement for the
        //programmer-perceivable width of this data type.
        return m_tr->i32;
    case LIR_JDT_object    : return m_tr->ptr;
    case LIR_JDT_boolean: return m_tr->b;
    case LIR_JDT_byte    : return m_tr->u8;
    case LIR_JDT_char    : return m_tr->i8;
    case LIR_JDT_short    : return m_tr->i16;
    case LIR_JDT_float    : return m_tr->f32;
    case LIR_JDT_none    : return m_tr->u32;
    case LIR_wide: return m_tr->u64;
    case LIR_JDT_wide: return m_tr->u64;
    case LIR_JDT_long: return m_tr->i64;
    case LIR_JDT_double: return m_tr->f64;
    default: UNREACHABLE();
    }
    return 0;
}


void Dex2IR::set_map_v2ofst(VAR * v, UINT ofst)
{
    bool find;
    UINT k = m_var2fieldid->get(v, &find);
    if (!find) {
        m_var2fieldid->set(v, ofst);
    } else {
        ASSERT0(k == ofst);
    }
}


//AABBBB
IR * Dex2IR::convertSget(IN LIR * lir)
{
    //vA(res) <- field_id(op0)",
    Type const* ty = getType(lir);
    IR * res = genMappedPR(LIR_res(lir), ty);

    /*
    Identical PR may be reused by designating different typeid.
    e.g: PR1(D_PTR:24)
         ...
         PR1(D_U32)
         ...
    TODO: Generate new PR instead of change the ty.
    */
    //VAR * v = addStaticVar(LIR_op0(lir), m_tr->obj_lda_base_type);
    VAR * v = addStaticVar(LIR_op0(lir), getType(lir));
    set_map_v2ofst(v, LIR_op0(lir));
    IR * rhs = NULL;
    /*
    if (LIR_dt(lir) == LIR_JDT_object) {
        IR * id = m_ru->buildId(v);
        rhs = m_ru->buildLda(id);
    } else {
        rhs = m_ru->buildLoad(v);
    }
    */
    rhs = m_ru->buildLoad(v);
    AIContainer * ai = m_ru->allocAIContainer();

    TbaaAttachInfo * tbaa = (TbaaAttachInfo*)xmalloc(
                    sizeof(TbaaAttachInfo), m_ru->get_pool());
    tbaa->init(AI_TBAA);
    tbaa->type = mapFieldType2Type(LIR_op0(lir));
    ASSERT0(tbaa->type);
    ai->set(tbaa);
    ASSERT0(rhs->getAI() == NULL);
    IR_ai(rhs) = ai;

    IR * c = m_ru->buildStorePR(PR_no(res), res->getType(), rhs);
    IR_may_throw(c) = true;
    if (m_has_catch) {
        IR * lab = m_ru->buildLabel(m_ru->genIlabel());
        add_next(&c, lab);
        attachCatchInfo(c);
    }
    return c;
}


Type const* Dex2IR::mapDexType2XocType(CHAR charty)
{
    Type const* ty = NULL;
    switch (charty) {
    case 'V': ty = m_tm->getSimplexTypeEx(D_VOID); break;
    case 'Z': ty = m_tm->getSimplexTypeEx(D_B); break;
    case 'B': ty = m_tm->getSimplexTypeEx(D_U8); break;
    case 'S': ty = m_tm->getSimplexTypeEx(D_I16); break;
    case 'C': ty = m_tm->getSimplexTypeEx(D_I8); break;
    case 'I': ty = m_tm->getSimplexTypeEx(D_I32); break;
    case 'J': ty = m_tm->getSimplexTypeEx(D_I64); break;
    case 'F': ty = m_tm->getSimplexTypeEx(D_F32); break;
    case 'D': ty = m_tm->getSimplexTypeEx(D_F64); break;
    case 'L': ty = m_tr->ptr; break;
    case '[': ty = m_tr->array; break;
    default: ASSERTN(0, ("TODO: not yet support")); break;
    }
    ASSERT0(ty);
    return ty;
}


Type const* Dex2IR::mapFieldType2Type(UINT field_id)
{
    CHAR const* type_name = get_var_type_name(field_id);
    ASSERT0(type_name);
    Type const* ty = m_typename2type.get(type_name);
    if (ty != NULL) { return ty; }
    m_typename2type.set(type_name, ty = mapDexType2XocType(*type_name));
    ASSERT0(ty);
    return ty;
}


//AABBBB
IR * Dex2IR::convertSput(IN LIR * lir)
{
    //vA(res) -> field_id(op0)"
    IR * res = genMappedPR(LIR_res(lir), getType(lir));

    /*
    Identical PR may be reused by designating different typeid.
    e.g: PR1(D_PTR:24)
         ...
         PR1(D_U32)
         ...
    TODO: Generate new PR instead of change the ty.
    */
    IR_dt(res) = getType(lir);
    ASSERT0(res->getType());
    VAR * v = addStaticVar(LIR_op0(lir), getType(lir));
    set_map_v2ofst(v, LIR_op0(lir));
    IR * c = m_ru->buildStore(v, res);
    IR_may_throw(c) = true;
    if (m_has_catch) {
        IR * lab = m_ru->buildLabel(m_ru->genIlabel());
        add_next(&c, lab);
        attachCatchInfo(c);
    }
    return c;
}


//AABBCC
//aput, OBJ, v0 -> (array_base_ptr)v1, (array_elem)v2
IR * Dex2IR::convertAput(IN LIR * lir)
{
    Type const* ty = getType(lir);
    //st-val
    IR * src = genMappedPR(LIR_res(lir), ty);
    //array
    IR * base = genMappedPR(LIR_op0(lir), m_tr->ptr);
    IR * ofst = genMappedPR(LIR_op1(lir), m_tr->u32);
    TMWORD enbuf = 0;

    //base type info.
    AIContainer * ai = m_ru->allocAIContainer();
    TbaaAttachInfo * tbaa = (TbaaAttachInfo*)xmalloc(
                    sizeof(TbaaAttachInfo), m_ru->get_pool());
    tbaa->init(AI_TBAA);
    tbaa->type = m_tr->array;
    ASSERT0(tbaa->type);
    ai->set(tbaa);
    ASSERT0(base->getAI() == NULL);
    IR_ai(base) = ai;

    IR * c = m_ru->buildStoreArray(base, ofst, ty, ty, 1, &enbuf, src);
    IR_may_throw(c) = true;
    if (m_has_catch) {
        IR * lab = m_ru->buildLabel(m_ru->genIlabel());
        add_next(&c, lab);
        attachCatchInfo(c);
    }
    return c;
}


//AABBCC
//aget, OBJ, v0 <- (array_base_ptr)v1, (array_elem)v2
IR * Dex2IR::convertAget(IN LIR * lir)
{
    Type const* ty = getType(lir);
    //st-val
    IR * res = genMappedPR(LIR_res(lir), ty);

    //array
    IR * base = genMappedPR(LIR_op0(lir), m_tr->ptr);
    IR * ofst = genMappedPR(LIR_op1(lir), m_tr->u32);
    TMWORD enbuf = 0;
    IR * array = m_ru->buildArray(base, ofst, ty, ty, 1, &enbuf);

    //Even if array operation may throw exception in DEX, we still
    //set the may-throw property at its stmt but is not itself.
    //IR_may_throw(array) = true;

    //base type info.
    AIContainer * ai = m_ru->allocAIContainer();
    TbaaAttachInfo * tbaa = (TbaaAttachInfo*)xmalloc(
                    sizeof(TbaaAttachInfo), m_ru->get_pool());
    tbaa->init(AI_TBAA);
    tbaa->type = m_tr->array;
    ASSERT0(tbaa->type);
    ai->set(tbaa);
    ASSERT0(base->getAI() == NULL);
    IR_ai(base) = ai;

    IR * c = m_ru->buildStorePR(PR_no(res), res->getType(), array);
    IR_may_throw(c) = true;
    if (m_has_catch) {
        IR * lab = m_ru->buildLabel(m_ru->genIlabel());
        add_next(&c, lab);
        attachCatchInfo(c);
    }
    return c;
}


//ABCCCC
IR * Dex2IR::convertIput(IN LIR * lir)
{
    /*
    class UI {
        public UI(short x) { u1=x; }
        short u1;
        long u2;
    }
    iput-short v1, v0, LUI;.u1:S // field@0006

    v%d(res) -> v%d(op0, thiptr), field_id(op1)",
    */
    IR * stv = genMappedPR(LIR_res(lir), getType(lir));

    /*
    Identical PR may be reused by designating different typeid.
    e.g: PR1(D_PTR:24)
         ...
         PR1(D_U32)
         ...
    TODO: Generate new PR instead of change the type.
    */
    IR_dt(stv) = getType(lir);
    ASSERT0(IR_dt(stv));

    //Get 'this' pointer
    IR * addr = genMappedPR(LIR_op0(lir), m_tr->ptr);

    /*
    UINT ofst = computeFieldOffset(LIR_op1(lir));
    IR * addr = m_ru->buildBinaryOp(IR_ADD,
                                thisptr->getType(),
                                thisptr,
                                m_ru->buildImmInt(ofst, m_ptr_addend));
    */
    IR * c = m_ru->buildIStore(addr, stv, stv->getType());
    IST_ofst(c) = LIR_op1(lir) * m_ofst_addend;

    IR_may_throw(c) = true;
    if (m_has_catch) {
        IR * lab = m_ru->buildLabel(m_ru->genIlabel());
        add_next(&c, lab);
        attachCatchInfo(c);
    }
    return c;
}


/*
AABBCC
cmpkind vAA <- vBB, vCC

DEX will be converted to:
    IR_CALL
        param0: cmp-kind.
        param1: vBB
        param2: vCC
        res: vAA
*/
IR * Dex2IR::convertCmp(IN LIR * lir)
{
    //see d2d_l2d.c
    CMP_KIND ck = CMP_UNDEF;
    switch (LIR_opcode(lir)) {
    case LOP_CMPL:
        if (LIR_dt(lir) == LIR_CMP_float) {
            ck = CMPL_FLOAT;
        } else {
            ASSERT0(LIR_dt(lir) == LIR_CMP_double);
            ck = CMPL_DOUBLE;
        }
        break;
    case LOP_CMP_LONG:
        ck = CMP_LONG;
        break;
    case LOP_CMPG:
        if (LIR_dt(lir) == LIR_CMP_float) {
            ck = CMPG_FLOAT;
        } else {
            ASSERT0(LIR_dt(lir) == LIR_CMP_double);
            ck = CMPG_DOUBLE;
        }
        break;
    default: UNREACHABLE();
    }

    Type const* ty = 0;
    switch (ck) {
    case CMPL_FLOAT: ty = m_tr->f32; break;
    case CMPG_FLOAT: ty = m_tr->f32; break;
    case CMPL_DOUBLE: ty = m_tr->f64; break;
    case CMPG_DOUBLE: ty = m_tr->f64; break;
    case CMP_LONG: ty = m_tr->i64; break;
    default: UNREACHABLE();
    }

    //Generate callee.
    VAR * v = getBuiltinVar(BLTIN_CMP_BIAS, m_ru_mgr);
    ASSERT0(v);
    IR * last = NULL;
    IR * p = NULL;

    //The first parameter is used to record cmp-kind.
    IR * kind = m_ru->buildImmInt(ck, m_tr->u32);
    add_next(&p, &last, kind);

    //The second parameter is opnd0 reigster.
    IR * r = genMappedPR(LIR_op0(lir), ty);
    add_next(&p, &last, r);

    //The third parameter is opnd1 reigster.
    r = genMappedPR(LIR_op1(lir), ty);
    add_next(&p, &last, r);

    IR * c = m_ru->buildCall(v, p,
                             genMappedPrno(LIR_res(lir), m_tr->i32),
                             m_tr->i32);
    CALL_is_readonly(c) = true;
    IR_has_sideeffect(c) = false;
    CALL_is_intrinsic(c) = true;
    //Not throw exception.
    return c;
}


//cls_id_in_tytab: typeIdx into type-table for defining class.
UINT Dex2IR::computeClassPosid(UINT cls_id_in_tytab)
{
    bool find = false;
    UINT posid = m_typeidx2posidx.get(cls_id_in_tytab, &find);
    if (find) {
        return posid;
    }
    for (UINT i = 0; i < m_df->pHeader->classDefsSize; i++) {
        //Show section header.
        //dumpClassDef(pDexFile, i);

        //Dump class name.
        DexClassDef const* class_info = dexGetClassDef(m_df, i);
        ASSERT0(class_info);
        if (class_info->classIdx == cls_id_in_tytab) {
            m_typeidx2posidx.set(cls_id_in_tytab, i);
            return i;
        }
    }
    CHAR const* class_name = dexStringByTypeIdx(m_df, cls_id_in_tytab);
    ASSERT0(class_name);
    ASSERTN(0, ("Not find!"));
    return 0;
}


/* 'field_id': the global index in class-defining-tab of dexfile.
e.g: Given field_id is 6, the class-defining-tab is:
    class LSwitch; {
         0th, field_1, Type:INT
         1th, field_2, Type:SHORT
         2th, field_3, Type:CHAR
         3th, field_4, Type:DOUBLE
    }
    class LTT; {
         4th, tt_f1, Type:INT
         5th, tt_f2, Type:LONG
    }
    class LUI; {
         6th, u1, Type:SHORT
         7th, u2, Type:OBJ
    }

    Field LUI;::u1 is the target.
*/
UINT Dex2IR::computeFieldOffset(UINT field_id)
{
    DexFieldId const* fid = dexGetFieldId(m_df, field_id);
    UINT cls_pos_id = computeClassPosid(fid->classIdx);
    DexClassDef const* class_info = dexGetClassDef(m_df, cls_pos_id);
    ASSERT0(class_info);
    CHAR const* class_name = dexStringByTypeIdx(m_df, fid->classIdx);
    CHAR const* field_name = get_field_name(m_df, field_id);

    //Get class-definition.
    BYTE const* encoded_data = dexGetClassData(m_df, class_info);
    DexClassData * class_data = dexReadAndVerifyClassData(&encoded_data, NULL);
    for (UINT i = 0; i < class_data->header.instanceFieldsSize; i++) {
        DexField * finfo = &class_data->instanceFields[i];
        if (finfo->fieldIdx == field_id) {
            return i * m_tm->get_dtype_bytesize(D_I64);
        }
        //dumpf_field(df, finfo, 4);
    }
    UNREACHABLE();
    return 0;
}


IR * Dex2IR::convertIget(IN LIR * lir)
{
    //ABCCCC
    //v%d(res) <- v%d(op0), field_id(op1)",
    Type const* ty = getType(lir);
    IR * res = genMappedPR(LIR_res(lir), ty);

    //Get object pointer
    IR * obj = genMappedPR(LIR_op0(lir), m_tr->ptr);

    /* How to describe the field offset of object-ptr?
    Since the field-idx(in LIR_op1(lir)) is unique in whole
    dex file, we can use it as offset value. Although it
    looks like a little bit tricky approach, but it works.
    UINT ofst = computeFieldOffset(LIR_op1(lir)); */
    UINT objofst = LIR_op1(lir) * m_ofst_addend;

    /*
    IR * addr = m_ru->buildBinaryOp(
                            IR_ADD,
                            IR_dt(obj),
                            obj,
                            m_ru->buildImmInt(objofst, m_ptr_addend));
    IR * ild = m_ru->buildILoad(addr, getType(lir));
    */

    IR * ild = m_ru->buildILoad(obj, ty);
    ILD_ofst(ild) = objofst; //ILD(MEM_ADDR+ofst)
    IR_may_throw(ild) = true;
    IR * c = m_ru->buildStorePR(PR_no(res), res->getType(), ild);

    IR_may_throw(c) = true;

    AIContainer * ai = m_ru->allocAIContainer();
    TbaaAttachInfo * tbaa = (TbaaAttachInfo*)xmalloc(
                    sizeof(TbaaAttachInfo), m_ru->get_pool());
    tbaa->init(AI_TBAA);
    tbaa->type = m_tr->ptr;
    ASSERT0(tbaa->type);
    ai->set(tbaa);
    ASSERT0(obj->getAI() == NULL);
    IR_ai(obj) = ai;

    if (m_has_catch) {
        IR * lab = m_ru->buildLabel(m_ru->genIlabel());
        add_next(&c, lab);
        attachCatchInfo(c);
    }

    CHAR const* type_name = get_var_type_name(LIR_op1(lir));
    if (is_array_type(type_name)) {
        //The type of result value of ild is pointer to array type.
        AIContainer * ai = m_ru->allocAIContainer();
        TbaaAttachInfo * tbaa = (TbaaAttachInfo*)xmalloc(
                    sizeof(TbaaAttachInfo), m_ru->get_pool());
        tbaa->init(AI_TBAA);
        tbaa->type = m_tr->array;
        ASSERT0(tbaa->type);
        ai->set(tbaa);
        ASSERT0(ild->getAI() == NULL);
        IR_ai(ild) = ai;
    } else if (is_obj_type(type_name)) {
        //The type of result value of ild is pointer to object type.
        AIContainer * ai = m_ru->allocAIContainer();
        TbaaAttachInfo * tbaa = (TbaaAttachInfo*)xmalloc(
                    sizeof(TbaaAttachInfo), m_ru->get_pool());
        tbaa->init(AI_TBAA);
        tbaa->type = m_tr->ptr;
        ASSERT0(tbaa->type);
        ai->set(tbaa);
        ASSERT0(ild->getAI() == NULL);
        IR_ai(ild) = ai;
    }

    return c;
}


//AABBCC
IR * Dex2IR::convertBinaryOpAssign(IN LIR * lir)
{
    Type const* ty = getType(lir);
    Type const* ty2 = ty;
    IR_TYPE ir_ty = IR_UNDEF;
    switch (LIR_opcode(lir)) {
    case LOP_ADD_ASSIGN : ir_ty = IR_ADD; break;
    case LOP_SUB_ASSIGN : ir_ty = IR_SUB; break;
    case LOP_MUL_ASSIGN : ir_ty = IR_MUL; break;
    case LOP_DIV_ASSIGN : ir_ty = IR_DIV; break;
    case LOP_REM_ASSIGN : ir_ty = IR_REM; break;
    case LOP_AND_ASSIGN : ir_ty = IR_BAND; break;
    case LOP_OR_ASSIGN  : ir_ty = IR_BOR; break;
    case LOP_XOR_ASSIGN : ir_ty = IR_XOR; break;
    case LOP_SHL_ASSIGN : ir_ty = IR_LSL;
        ty2 = m_tm->getSimplexTypeEx(D_U32); break;
    case LOP_SHR_ASSIGN : ir_ty = IR_ASR;
        ty2 = m_tm->getSimplexTypeEx(D_U32); break;
    case LOP_USHR_ASSIGN: ir_ty = IR_LSR;
        ty2 = m_tm->getSimplexTypeEx(D_U32); break;
    default: UNREACHABLE();
    }
    IR * res = genMappedPR(LIR_res(lir), ty);
    IR * op0 = genMappedPR(LIR_op0(lir), ty2);
    IR * x = m_ru->buildBinaryOp(ir_ty, ty, res, op0);
    IR * c = m_ru->buildStorePR(PR_no(res), res->getType(), x);
    if (ir_ty == IR_DIV || ir_ty == IR_REM) {
        #ifdef DIV_REM_MAY_THROW
        IR_may_throw(x) = true;
        IR_may_throw(c) = true;
        #endif
        if (m_has_catch) {
            IR * lab = m_ru->buildLabel(m_ru->genIlabel());
            add_next(&c, lab);
            attachCatchInfo(c);
        }
    }
    return c;
}


//AABBCC
IR * Dex2IR::convertBinaryOp(IN LIR * lir)
{
    Type const* ty = getType(lir);
    IR_TYPE ir_ty = IR_UNDEF;
    switch (LIR_opcode(lir)) {
    case LOP_ADD : ir_ty = IR_ADD; break;
    case LOP_SUB : ir_ty = IR_SUB; break;
    case LOP_MUL : ir_ty = IR_MUL; break;
    case LOP_DIV : ir_ty = IR_DIV; break;
    case LOP_REM : ir_ty = IR_REM; break;
    case LOP_AND : ir_ty = IR_BAND; break;
    case LOP_OR  : ir_ty = IR_BOR; break;
    case LOP_XOR : ir_ty = IR_XOR; break;
    case LOP_SHL : ir_ty = IR_LSL; break;
    case LOP_SHR : ir_ty = IR_ASR; break;
    case LOP_USHR: ir_ty = IR_LSR; break;
    default: UNREACHABLE();
    }
    IR * res = genMappedPR(LIR_res(lir), ty);
    IR * op0 = genMappedPR(LIR_op0(lir), ty);
    IR * op1 = genMappedPR(LIR_op1(lir), ty);
    IR * x = m_ru->buildBinaryOp(ir_ty, ty, op0, op1);
    IR * c = m_ru->buildStorePR(PR_no(res), res->getType(), x);
    if (ir_ty == IR_DIV || ir_ty == IR_REM) {
        #ifdef DIV_REM_MAY_THROW
        IR_may_throw(x) = true;
        IR_may_throw(c) = true;
        #endif
        if (m_has_catch) {
            IR * lab = m_ru->buildLabel(m_ru->genIlabel());
            add_next(&c, lab);
            attachCatchInfo(c);
        }
    }
    return c;
}


IR * Dex2IR::convertBinaryOpLit(IN LIR * lir)
{
    //LIRABCOp * p = (LIRABCOp*)ir;
    //if (is_s8(LIR_op1(ir))) {
        //8bit imm
    //} else {
        //16bit imm
    //}
    Type const* ty = getType(lir);
    IR_TYPE ir_ty = IR_UNDEF;
    switch (LIR_opcode(lir)) {
    case LOP_ADD_LIT : ir_ty = IR_ADD; break;
    case LOP_SUB_LIT : ir_ty = IR_SUB; break;
    case LOP_MUL_LIT : ir_ty = IR_MUL; break;
    case LOP_DIV_LIT : ir_ty = IR_DIV; break;
    case LOP_REM_LIT : ir_ty = IR_REM; break;
    case LOP_AND_LIT : ir_ty = IR_BAND; break;
    case LOP_OR_LIT  : ir_ty = IR_BOR; break;
    case LOP_XOR_LIT : ir_ty = IR_XOR; break;
    case LOP_SHL_LIT : ir_ty = IR_LSL; break;
    case LOP_SHR_LIT : ir_ty = IR_ASR; break;
    case LOP_USHR_LIT: ir_ty = IR_LSR; break;
    default: UNREACHABLE();
    }

    IR * res = genMappedPR(LIR_res(lir), ty);
    IR * op0 = genMappedPR(LIR_op0(lir), ty);
    IR * lit = m_ru->buildImmInt(LIR_op1(lir), ty);
    IR * x = m_ru->buildBinaryOp(ir_ty, ty, op0, lit);
    IR * c = m_ru->buildStorePR(PR_no(res), res->getType(), x);
    if ((ir_ty == IR_DIV || ir_ty == IR_REM) && CONST_int_val(lit) == 0) {
        IR_may_throw(x) = true;
        IR_may_throw(c) = true;
        if (m_has_catch) {
            IR * lab = m_ru->buildLabel(m_ru->genIlabel());
            add_next(&c, lab);
            attachCatchInfo(c);
        }
    }
    return c;
}


UINT Dex2IR::get_dexopcode(UINT flag)
{
    UINT flag1 = flag & 0x0f;
    UINT flag2 = flag & 0xf0;
    UINT dexopcode = 0; //defined in DexOpcodes.h
    switch (flag1) {
    case LIR_invoke_unknown:
        UNREACHABLE();
        switch (flag2) {
        case 0: dexopcode = OP_FILLED_NEW_ARRAY; break;
        case LIR_Range: dexopcode = OP_FILLED_NEW_ARRAY_RANGE; break;
        default: UNREACHABLE();
        }
        break;
    case LIR_invoke_virtual:
        switch (flag2) {
        case 0: dexopcode = OP_INVOKE_VIRTUAL; break;
        case LIR_Range: dexopcode = OP_INVOKE_VIRTUAL_RANGE; break;
        default: UNREACHABLE();
        }
        break;
    case LIR_invoke_super:
        switch(flag2) {
        case 0: dexopcode = OP_INVOKE_SUPER; break;
        case LIR_Range: dexopcode = OP_INVOKE_SUPER_RANGE; break;
        default: UNREACHABLE();
        }
        break;
    case LIR_invoke_direct:
        switch (flag2) {
        case 0: dexopcode = OP_INVOKE_DIRECT; break;
        case LIR_Range: dexopcode = OP_INVOKE_DIRECT_RANGE; break;
        default: UNREACHABLE();
        }
        break;
    case LIR_invoke_static:
        switch (flag2) {
        case 0: dexopcode = OP_INVOKE_STATIC; break;
        case LIR_Range: dexopcode = OP_INVOKE_STATIC_RANGE; break;
        default: UNREACHABLE();
        }
        break;
    case LIR_invoke_interface:
        switch (flag2) {
        case 0: dexopcode = OP_INVOKE_INTERFACE; break;
        case LIR_Range: dexopcode = OP_INVOKE_INTERFACE_RANGE; break;
        default: UNREACHABLE();
        }
        break;
    default:
        UNREACHABLE();
    }
    return dexopcode;
}


/*
invoke-direct {v0}, Ljava/lang/Object;.<init>:()V // method@0006
invoke-virtual
invoke-static
*/
IR * Dex2IR::convertInvoke(IN LIR * lir)
{
    UINT dexopcode = get_dexopcode(LIR_dt(lir));
    Type const* ret_ty = m_tr->i32; //TODO: ensure the return-value type.
    LIRInvokeOp * r = (LIRInvokeOp*)lir;

    //Generate callee.
    VAR * v = addFuncVar(r->ref, ret_ty);

    //Only for Debug
    DexMethodId const* method_id = dexGetMethodId(m_df, r->ref);
    CHAR const* method_name = dexStringById(m_df, method_id->nameIdx);
    CHAR const* class_name = dexStringByTypeIdx(m_df, method_id->classIdx);
    DexProtoId const* proto_id = dexGetProtoId(m_df, method_id->protoIdx);
    CHAR const* paramty = dexStringById(m_df, proto_id->shortyIdx);
    ASSERT0(method_name && paramty);

    //Construct param list.
    IR * last = NULL;
    IR * param_list = NULL;

    //The first parameter is used to record invoke-kind.
    UINT k = LIR_dt(lir);
    bool is_range = HAVE_FLAG((k & 0xf0), LIR_Range);
    INVOKE_KIND ik = INVOKE_UNDEF;
    bool has_this = true;
    if (is_range) {
        switch (k & 0x0f) {
        case LIR_invoke_unknown: UNREACHABLE(); break;
        case LIR_invoke_virtual: ik = INVOKE_VIRTUAL_RANGE; break;
        case LIR_invoke_direct: ik = INVOKE_DIRECT_RANGE; break;
        case LIR_invoke_super: ik = INVOKE_SUPER_RANGE; break;
        case LIR_invoke_interface: ik = INVOKE_INTERFACE_RANGE; break;
        case LIR_invoke_static:
            ik = INVOKE_STATIC_RANGE;
            has_this = false;
            break;
        default: UNREACHABLE();
        }
    } else {
        switch (k & 0x0f) {
        case LIR_invoke_unknown: UNREACHABLE(); break;
        case LIR_invoke_virtual: ik = INVOKE_VIRTUAL; break;
        case LIR_invoke_direct: ik = INVOKE_DIRECT; break;
        case LIR_invoke_super: ik = INVOKE_SUPER; break;
        case LIR_invoke_interface: ik = INVOKE_INTERFACE; break;
        case LIR_invoke_static: ik = INVOKE_STATIC; has_this = false; break;
        default: UNREACHABLE();
        }
    }
    IR * kind = m_ru->buildImmInt(ik, m_tr->u32);
    add_next(&param_list, &last, kind);

    //The second parameter is used to record method-id.
    IR * midv = m_ru->buildImmInt(r->ref, m_tr->u32);
    add_next(&param_list, &last, midv);

    //Record invoke-parameters.
    paramty++;
    for (USHORT i = 0; i < r->argc; i++) {
        IR * param = NULL;
        if (has_this && i == 0) {
            param = genMappedPR(r->args[i], m_tr->ptr);
        } else {
            ASSERTN(*paramty != '[', ("should be convert to L"));
            param = genMappedPR(r->args[i], mapDexType2XocType(*paramty));
            switch (*paramty) {
            case 'J':
            case 'D':
                i++; break;
            default:;
            }
            paramty++;
        }
        add_next(&param_list, &last, param);
    }
    ASSERT0(*paramty == 0);
    IR * c = m_ru->buildCall(v, param_list, 0, m_tm->getVoid());
    IR_may_throw(c) = true;
    CALL_is_readonly(c) = VAR_is_readonly(v);
    IR_has_sideeffect(c) = true;
    attachCatchInfo(c);
    return c;
}


//AABBBB
//new-instance vA <- CLASS-NAME
IR * Dex2IR::convertNewInstance(IN LIR * lir)
{
    //Region::buld_binary_op will detect the POINTER type, IR_ADD
    //computes the POINTER addend size automatically.
    //Here we hacked it by change POINTER base type size to 1 byte.
    Type const* ty = m_tr->ptr; //return value is referrence type.

    //AABBBB
    //new-instance v%d <- CLASS-NAME
    VAR * v = getBuiltinVar(BLTIN_NEW, m_ru_mgr);

    //return_value = new(CLASS-NAME)
    IR * class_type_id = m_ru->buildImmInt(LIR_op0(lir), m_tr->u32);
    IR * c = m_ru->buildCall(v, class_type_id,
                             genMappedPrno(LIR_res(lir), ty), ty);
    IR_may_throw(c) = true;
    CALL_is_alloc_heap(c) = true;
    CALL_is_readonly(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = true;
    attachCatchInfo(c);
    return c;
}


//ABCCCC
//new-array vA(res) <- vB(op0), LCAnimal(op1)
IR * Dex2IR::convertNewArray(IN LIR * lir)
{
    Type const* ty = m_tr->ptr; //return value is obj-ptr.
    VAR * v = getBuiltinVar(BLTIN_NEW_ARRAY, m_ru_mgr);

    //The first parameter is the number of array element.
    IR * param_list = genMappedPR(LIR_op0(lir), ty);

    //The class_name id.
    CHAR const* class_name = dexStringByTypeIdx(m_df, LIR_op1(lir));
    ASSERT0(class_name);

    //The second parameter is class-type-id.
    add_next(&param_list, m_ru->buildImmInt(LIR_op1(lir), m_tr->u32));

    //Call new_array(num_of_elem, class_name_id)
    IR * c =  m_ru->buildCall(v, param_list,
                               genMappedPrno(LIR_res(lir), ty), ty);
    IR_may_throw(c) = true;
    CALL_is_alloc_heap(c) = true;
    CALL_is_readonly(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = true;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertArrayLength(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_ARRAY_LENGTH, m_ru_mgr);
    IR * c = m_ru->buildCall(v,
                             genMappedPR(LIR_op0(lir), m_tr->ptr),
                             genMappedPrno(LIR_res(lir), m_tr->i32),
                             m_tr->i32);

    IR_may_throw(c) = true;
    CALL_is_readonly(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = false;
    attachCatchInfo(c);
    return c;
}


void Dex2IR::attachCatchInfo(IR * ir, AIContainer * ai)
{
    if (m_current_catch_list != NULL) {
        ASSERT0(ai);
        EHLabelAttachInfo * ehai = (EHLabelAttachInfo*)xmalloc(
                    sizeof(EHLabelAttachInfo), m_ru->get_pool());
        ehai->init(m_ru->get_sc_pool());
        ai->set(ehai);
        for (CatchInfo * ci = m_current_catch_list;
             ci != NULL; ci = ci->next) {
            ehai->get_labels().append_head(ci->catch_start);
        }
    }
}


void Dex2IR::attachCatchInfo(IR * ir)
{
    if (m_current_catch_list != NULL) {
        AIContainer * ai = m_ru->allocAIContainer();
        ASSERT0(ir->getAI() == NULL);
        IR_ai(ir) = ai;

        EHLabelAttachInfo * ehai = (EHLabelAttachInfo*)xmalloc(
                    sizeof(EHLabelAttachInfo), m_ru->get_pool());
        ehai->init(m_ru->get_sc_pool());
        ai->set(ehai);
        for (CatchInfo * ci = m_current_catch_list;
             ci != NULL; ci = ci->next) {
            ehai->get_labels().append_head(ci->catch_start);
        }
    }
}


//Releases the monitor of the object referenced by vA.
IR * Dex2IR::convertMonitorExit(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_MONITOR_EXIT, m_ru_mgr);
    IR * c = m_ru->buildCall(v, genMappedPR(LIR_res(lir), m_tr->i32),
                             0, m_tm->getVoid());
    IR_may_throw(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = true;
    CALL_is_readonly(c) = true;
    attachCatchInfo(c);
    return c;
}


//Obtains the monitor of the object referenced by vA.
IR * Dex2IR::convertMonitorEnter(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_MONITOR_ENTER, m_ru_mgr);
    IR * c = m_ru->buildCall(v, genMappedPR(LIR_res(lir), m_tr->i32),
                             0, m_tm->getVoid());
    IR_may_throw(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = true;
    CALL_is_readonly(c) = true;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertPackedSwitch(IN LIR * lir)
{
    LIRSwitchOp * p = (LIRSwitchOp*)lir;
    ASSERT0(LIR_dt(p) == 0x1);

    USHORT * pdata = p->data;

    UINT f = LIR_switch_kind(p);
    ASSERT0(f == 0x100);

    USHORT num_of_case = LIR_case_num(p);

    INT base_val = LIR_packed_switch_base_value(p);

    IR * case_list = NULL;
    IR * last = NULL;
    if (num_of_case > 0) {
        Type const* ty = m_tr->i32;
        UINT * pcase_entry = LIR_packed_switch_case_entry(p);
        for (USHORT i = 0; i < num_of_case; i++, base_val++) {
            UINT idx_of_insn = pcase_entry[i];
            LIR * tgt = LIRC_op(m_lircode, idx_of_insn);
            ASSERT0(tgt);
            List<LabelInfo*> * labs = m_lir2labs.get(tgt);

            //There may be many labs over there.
            //ASSERT0(labs && labs->get_elem_count() == 1);

            LabelInfo * lab = labs->get_head();
            add_next(&case_list, &last,
                m_ru->buildCase(m_ru->buildImmInt(base_val, ty), lab));
        }
    }

    LabelInfo * defaultlab = m_ru->genIlabel();
    IR * vexp = genMappedPR(p->value, m_tr->i32);
    IR * ret_list = m_ru->buildSwitch(vexp, case_list, NULL, defaultlab);
    add_next(&ret_list, m_ru->buildLabel(defaultlab));
    return ret_list;
}


IR * Dex2IR::convertFillArrayData(IN LIR * lir)
{
    LIRSwitchOp * l = (LIRSwitchOp*)lir;
    USHORT const* pdata = l->data;

    #ifdef _DEBUG_
    //Check the legality of content at lir data.
    //pdata[0]: the magic number of code
    //0x100 PACKED_SWITCH, 0x200 SPARSE_SWITCH, 0x300 FILL_ARRAY_DATA
    ASSERT0(pdata[0] == 0x300);
    //pdata[1]: size of each element.
    //pdata[2]: the number of element.
    UINT size_of_elem = pdata[1];
    UINT num_of_elem = pdata[2];
    UINT data_size = num_of_elem * size_of_elem;
    #endif

    VAR * v = getBuiltinVar(BLTIN_FILL_ARRAY_DATA, m_ru_mgr);
    IR * call = m_ru->buildCall(v, NULL, 0, m_tm->getVoid());

    //The first parameter is array obj-ptr.
    IR * p = genMappedPR(l->value, m_tr->ptr);

    //The second parameter points to filling data.
    add_next(&p, m_ru->buildImmInt((HOST_INT)pdata, m_tr->u32));

    CALL_param_list(call) = p;
    call->setParentPointer(false);
    IR_may_throw(call) = true;
    CALL_is_intrinsic(call) = true;
    IR_has_sideeffect(call) = true;
    CALL_is_readonly(call) = true;
    attachCatchInfo(call);
    return call;
}


IR * Dex2IR::convertSparseSwitch(IN LIR * lir)
{
    LIRSwitchOp * p = (LIRSwitchOp*)lir;
    ASSERT0(LIR_dt(p) == 0x2);

    USHORT * pdata = p->data;
    UINT f = LIR_switch_kind(p);
    ASSERT0(f == 0x200);

    UINT num_of_case = LIR_case_num(p);
    IR * case_list = NULL;
    IR * last = NULL;
    if (num_of_case > 0) {
        Type const* ty = m_tr->i32;
        UINT * pcase_value =LIR_sparse_switch_case_value(p);

        UINT * pcase_entry = LIR_sparse_switch_case_entry(p);
        for (UINT i = 0; i < num_of_case; i++) {
            LIR * tgt = LIRC_op(m_lircode, pcase_entry[i]);
            ASSERT0(tgt);
            List<LabelInfo*> * labs = m_lir2labs.get(tgt);

            //There may be many labs over there.
            //ASSERT0(labs && labs->get_elem_count() == 1);

            LabelInfo * lab = labs->get_head();
            add_next(&case_list, &last,
                m_ru->buildCase(m_ru->buildImmInt(pcase_value[i], ty), lab));
        }
    }

    LabelInfo * defaultlab = m_ru->genIlabel();
    IR * vexp = genMappedPR(p->value, m_tr->i32);
    IR * ret_list = m_ru->buildSwitch(vexp, case_list, NULL, defaultlab);
    add_next(&ret_list, m_ru->buildLabel(defaultlab));
    return ret_list;
}


IR * Dex2IR::convertInstanceOf(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_INSTANCE_OF, m_ru_mgr);

    //first parameter
    IR * p = genMappedPR(LIR_op0(lir), m_tr->ptr);

    //second one.
    add_next(&p, m_ru->buildImmInt(LIR_op1(lir), m_tr->i32));

    IR * c = m_ru->buildCall(v, p,
                             genMappedPrno(LIR_res(lir), m_tr->i32),
                             m_tr->i32);
    IR_may_throw(c) = true;
    CALL_is_readonly(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = false;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertCheckCast(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_CHECK_CAST, m_ru_mgr);
    IR * p = genMappedPR(LIR_res(lir), m_tr->i32);
    add_next(&p, m_ru->buildImmInt(LIR_op0(lir), m_tr->i32));
    IR * c = m_ru->buildCall(v, p, 0, m_tm->getVoid());
    IR_may_throw(c) = true;
    CALL_is_intrinsic(c) = true;
    CALL_is_readonly(c) = true;

    //It throw a ClassCastException if the reference in the given register
    //cannot be cast to the indicated.
    IR_has_sideeffect(c) = true;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertFilledNewArray(IN LIR * lir)
{
    //AABBBBCCCC or ABCCCCDEFG
    //e.g: A(argc), B,D,E,F,G(parampters), CCCC(class_type)

    LIRInvokeOp * r = (LIRInvokeOp*)lir;

    VAR * v = getBuiltinVar(BLTIN_FILLED_NEW_ARRAY, m_ru_mgr);

    #ifdef _DEBUG_
    CHAR const* class_name = dexStringByTypeIdx(m_df, r->ref);
    ASSERT0(class_name);
    #endif
    //first parameter is invoke-kind.
    IR * p = m_ru->buildImmInt(LIR_dt(lir), m_tr->u32);

    //second one is class-id.
    IR * cid = m_ru->buildImmInt(r->ref, m_tr->i32);
    IR * last = p;
    add_next(&p, &last, cid);

    //and else parameters.
    for (UINT i = 0; i < r->argc; i++) {
        add_next(&p, &last, genMappedPR(r->args[i], m_tr->i32));
    }
    IR * c = m_ru->buildCall(v, p, 0, m_tm->getVoid());
    IR_may_throw(c) = true;
    CALL_is_intrinsic(c) = true;
    CALL_is_readonly(c) = true;
    IR_has_sideeffect(c) = true;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertThrow(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_THROW, m_ru_mgr);
    IR * c = m_ru->buildCall(v, genMappedPR(LIR_res(lir), m_tr->i32),
                             0, m_tm->getVoid());
    IR_may_throw(c) = true;
    CALL_is_readonly(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = true;
    IR_is_terminate(c) = true;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertConstClass(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_CONST_CLASS, m_ru_mgr);
    IR * c = m_ru->buildCall(v,
                             m_ru->buildImmInt(LIR_op0(lir), m_tr->i32),
                             genMappedPrno(LIR_res(lir), m_tr->i32),
                             m_tr->i32);
    IR_may_throw(c) = true;
    CALL_is_readonly(c) = true;
    CALL_is_intrinsic(c) = true;
    IR_has_sideeffect(c) = false;
    attachCatchInfo(c);
    return c;
}


IR * Dex2IR::convertMoveException(IN LIR * lir)
{
    VAR * v = getBuiltinVar(BLTIN_MOVE_EXP, m_ru_mgr);
    IR * ir = m_ru->buildCall(v, NULL,
                              genMappedPrno(LIR_res(lir), m_tr->i32),
                              m_tr->i32);
    CALL_is_readonly(ir) = true;
    CALL_is_intrinsic(ir) = true;
    IR_has_sideeffect(ir) = true;
    return ir;
    //Not throw exception.
}


IR * Dex2IR::convertMoveResult(IN LIR * lir)
{
    Type const* resty = NULL;
    switch (LIR_dt(lir)) {
    case LIR_JDT_unknown: resty = m_tr->i32; break;
    case LIR_JDT_object: resty = m_tr->ptr; break;
    case LIR_JDT_wide: resty = m_tr->i64; break;
    default: UNREACHABLE();
    }

    VAR * v = getBuiltinVar(BLTIN_MOVE_RES, m_ru_mgr);
    VAR_is_readonly(v) = true;
    IR * x = m_ru->buildCall(v, NULL,
                             genMappedPrno(LIR_res(lir), resty),
                             resty);
    CALL_is_readonly(x) = true;
    IR_has_sideeffect(x) = true;
    CALL_is_intrinsic(x) = true;
    return x;
}


IR * Dex2IR::convertMove(IN LIR * lir)
{
    Type const* ty = NULL;
    switch (LIR_dt(lir)) {
    case LIR_JDT_unknown: //return pr.
        ty = m_tr->i32;
        break;
    case LIR_JDT_object: //return object.
        ty = m_tr->ptr;
        break;
    case LIR_JDT_wide: //return 64bits result
        ty = m_tr->i64; //TODO: need pair?
        break;
    default: UNREACHABLE();
    }
    IR * tgt = genMappedPR(LIR_res(lir), ty);
    IR * src = genMappedPR(LIR_op0(lir), ty);
    return m_ru->buildStorePR(PR_no(tgt), tgt->getType(), src);
}


//Map Vreg to PR, and return PR.
//vid: Vreg id.
IR * Dex2IR::genMappedPR(UINT vid, Type const* ty)
{
    IR * vx = m_v2pr.get(vid);
    if (vx == NULL) {
        vx = m_ru->buildPR(ty);
        m_v2pr.set(vid, vx);
        m_pr2v.set(PR_no(vx), vid);
    }
    vx = m_ru->dupIR(vx);
    IR_dt(vx) = ty;
    return vx;
}


//Map Vreg to PR, then return the PR no.
//vid: Vreg id.
UINT Dex2IR::genMappedPrno(UINT vid, Type const* ty)
{
    IR * vx = m_v2pr.get(vid);
    if (vx == NULL) {
        vx = m_ru->buildPR(ty);
        m_v2pr.set(vid, vx);
        m_pr2v.set(PR_no(vx), vid);
    }
    return PR_no(vx);
}


IR * Dex2IR::convertRet(IN LIR * lir)
{
    IR * exp = NULL;
    switch (LIR_dt(lir)) {
    case LIR_JDT_unknown: //return pr. vAA
        exp = genMappedPR(LIR_res(lir), m_tr->i32);
        break;
    case LIR_JDT_void:
        exp = NULL;
        break;
    case LIR_JDT_object: //return object.
        exp = genMappedPR(LIR_res(lir), m_tr->ptr);
        break;
    case LIR_JDT_wide: //return 64bits result
        exp = genMappedPR(LIR_res(lir), m_tr->i64);
        break;
    default: UNREACHABLE();
    }
    return m_ru->buildReturn(exp);
}


IR * Dex2IR::convertUnaryOp(IN LIR * lir)
{
    Type const* ty = getType(lir);
    IR_TYPE ir_ty = IR_UNDEF;
    switch (LIR_opcode(lir)) {
    case LOP_NEG: ir_ty = IR_NEG; break;
    case LOP_NOT: ir_ty = IR_BNOT; break;
    default: UNREACHABLE();
    }
    IR * res = genMappedPR(LIR_res(lir), ty);
    IR * op0 = genMappedPR(LIR_op0(lir), ty);
    IR * x = m_ru->buildUnaryOp(ir_ty, ty, op0);
    return m_ru->buildStorePR(PR_no(res), res->getType(), x);
    //Not throw exception.
}


IR * Dex2IR::convertLoadStringAddr(IN LIR * lir)
{
    CHAR const* string = dexStringById(m_df, LIR_op0(lir));
    ASSERT0(string);
    VAR * v = addStringVar(string);
    set_map_v2ofst(v, LIR_op0(lir));
    IR * lda = m_ru->buildLda(v);
    IR * res = genMappedPR(LIR_res(lir), m_tr->ptr);
    return m_ru->buildStorePR(PR_no(res), res->getType(), lda);
}


IR * Dex2IR::convertLoadConst(IN LIR * lir)
{
    Type const* ty = 0;
    switch (LIR_dt(lir)) {
    case LIR_JDT_unknown:
        ty = m_tr->i32;
        break;
    case LIR_JDT_void   :
    case LIR_JDT_int    :
    case LIR_JDT_object :
    case LIR_JDT_boolean:
    case LIR_JDT_byte   :
    case LIR_JDT_char   :
    case LIR_JDT_short  :
    case LIR_JDT_float  :
    case LIR_JDT_none   :
    case LIR_wide       :
        UNREACHABLE();
        break;
    case LIR_JDT_wide:
        ty = m_tr->i64;
        break;
    case LIR_JDT_long:
    case LIR_JDT_double :
    default: UNREACHABLE();
    }

    IR * res = genMappedPR(LIR_res(lir), ty);
    return m_ru->buildStorePR(PR_no(res), res->getType(),
                              m_ru->buildImmInt(LIR_int_imm(lir), ty));
}


IR * Dex2IR::convertGoto(IN LIR * lir)
{
    LIR * tgt = LIRC_op(m_lircode, ((LIRGOTOOp*)lir)->target);
    ASSERT0(tgt);
    List<LabelInfo*> * labs = m_lir2labs.get(tgt);
    ASSERT0(labs);
    return m_ru->buildGoto(labs->get_head());
}


IR * Dex2IR::convertCondBr(IN LIR * lir)
{
    IR_TYPE rt = IR_UNDEF;
    switch (LIR_dt(lir)) {
    case LIR_cond_EQ: rt = IR_EQ; break;
    case LIR_cond_LT: rt = IR_LT; break;
    case LIR_cond_GT: rt = IR_GT; break;
    case LIR_cond_LE: rt = IR_LE; break;
    case LIR_cond_GE: rt = IR_GE; break;
    case LIR_cond_NE: rt = IR_NE; break;
    }

    //Det
    Type const* ty = m_tr->i32;
    IR * op0 = NULL;
    IR * op1 = NULL;
    UINT liridx = 0;
    if (LIR_opcode(lir) == LOP_IF) {
        op0 = genMappedPR(LIR_res(lir), ty);
        op1 = genMappedPR(LIR_op0(lir), ty);
        liridx = LIR_op1(lir);
    } else if (LIR_opcode(lir) == LOP_IFZ) {
        op0 = genMappedPR(LIR_res(lir), ty);
        op1 = m_ru->buildImmInt(0, ty);
        liridx = LIR_op0(lir);
    } else {
        UNREACHABLE();
    }
    IR * det = m_ru->buildBinaryOp(rt, m_tr->b, op0, op1);
    //Target label
    LIR * tgt = LIRC_op(m_lircode, liridx);
    ASSERT0(tgt);
    List<LabelInfo*> * labs = m_lir2labs.get(tgt);
    ASSERT0(labs);
    return m_ru->buildBranch(true, det, labs->get_head());
}


IR * Dex2IR::convertCvt(IN LIR * lir)
{
    Type const* tgt = NULL;
    Type const* src = NULL;
    switch (LIR_dt(lir)) {
    case LIR_convert_i2l: tgt = m_tr->i64; src = m_tr->i32; break;
    case LIR_convert_i2f: tgt = m_tr->f32; src = m_tr->i32; break;
    case LIR_convert_i2d: tgt = m_tr->f64; src = m_tr->i32; break;

    case LIR_convert_l2i: tgt = m_tr->i32; src = m_tr->i64; break;
    case LIR_convert_l2f: tgt = m_tr->f32; src = m_tr->i64; break;
    case LIR_convert_l2d: tgt = m_tr->f64; src = m_tr->i64; break;

    case LIR_convert_f2i: tgt = m_tr->i32; src = m_tr->f32; break;
    case LIR_convert_f2l: tgt = m_tr->i64; src = m_tr->f32; break;
    case LIR_convert_f2d: tgt = m_tr->f64; src = m_tr->f32; break;

    case LIR_convert_d2i: tgt = m_tr->i32; src = m_tr->f64; break;
    case LIR_convert_d2l: tgt = m_tr->i64; src = m_tr->f64; break;
    case LIR_convert_d2f: tgt = m_tr->f32; src = m_tr->f64; break;

    case LIR_convert_i2b: tgt = m_tr->b; src = m_tr->i32; break;
    case LIR_convert_i2c: tgt = m_tr->i8; src = m_tr->i32; break;
    case LIR_convert_i2s: tgt = m_tr->i16; src = m_tr->i32; break;
    default: UNREACHABLE();
    }

    IR * res = genMappedPR(LIR_res(lir), tgt);
    IR * exp = genMappedPR(LIR_op0(lir), src);
    IR * cvt = m_ru->buildCvt(exp, tgt);
    return m_ru->buildStorePR(PR_no(res), res->getType(), cvt);
}


IR * Dex2IR::convert(IN LIR * lir)
{
    switch (LIR_opcode(lir)) {
    case LOP_NOP:
        break;
    case LOP_CONST:
        return convertLoadConst(lir);
    case LOP_RETURN:
        return convertRet(lir);
    case LOP_THROW:
        return convertThrow(lir);
    case LOP_MONITOR_ENTER  :
        return convertMonitorEnter(lir);
    case LOP_MONITOR_EXIT   :
        return convertMonitorExit(lir);
    case LOP_MOVE_RESULT    :
        return convertMoveResult(lir);
    case LOP_MOVE_EXCEPTION :
        return convertMoveException(lir);
    case LOP_GOTO            :
        return convertGoto(lir);
    case LOP_MOVE        :
        return convertMove(lir);
    case LOP_NEG        :
    case LOP_NOT        :
        return convertUnaryOp(lir);
    case LOP_CONVERT    :
        return convertCvt(lir);
    case LOP_ADD_ASSIGN :
    case LOP_SUB_ASSIGN :
    case LOP_MUL_ASSIGN :
    case LOP_DIV_ASSIGN :
    case LOP_REM_ASSIGN :
    case LOP_AND_ASSIGN :
    case LOP_OR_ASSIGN  :
    case LOP_XOR_ASSIGN :
    case LOP_SHL_ASSIGN :
    case LOP_SHR_ASSIGN :
    case LOP_USHR_ASSIGN:
        return convertBinaryOpAssign(lir);
    case LOP_ARRAY_LENGTH:
        return convertArrayLength(lir);
    case LOP_IFZ         :
        return convertCondBr(lir);
    case LOP_NEW_INSTANCE:
        return convertNewInstance(lir);
    case LOP_CONST_STRING:
        return convertLoadStringAddr(lir);
    case LOP_CONST_CLASS :
        return convertConstClass(lir);
    case LOP_SGET        :
        return convertSget(lir);
    case LOP_CHECK_CAST  :
        return convertCheckCast(lir);
    case LOP_SPUT        :
        return convertSput(lir);
    case LOP_APUT        :
        return convertAput(lir);
    case LOP_AGET      :
        return convertAget(lir);
    case LOP_CMPL      :
    case LOP_CMP_LONG  :
    case LOP_CMPG:
        return convertCmp(lir);
    case LOP_ADD       :
    case LOP_SUB       :
    case LOP_MUL       :
    case LOP_DIV       :
    case LOP_REM       :
    case LOP_AND       :
    case LOP_OR        :
    case LOP_XOR       :
    case LOP_SHL       :
    case LOP_SHR       :
    case LOP_USHR      :
        return convertBinaryOp(lir);
    case LOP_IF        :
        return convertCondBr(lir);
    case LOP_ADD_LIT   :
    case LOP_SUB_LIT   :
    case LOP_MUL_LIT   :
    case LOP_DIV_LIT   :
    case LOP_REM_LIT   :
    case LOP_AND_LIT   :
    case LOP_OR_LIT    :
    case LOP_XOR_LIT   :
    case LOP_SHL_LIT   :
    case LOP_SHR_LIT   :
    case LOP_USHR_LIT   :
        return convertBinaryOpLit(lir);
    case LOP_IPUT       :
        return convertIput(lir);
    case LOP_IGET       :
        return convertIget(lir);
    case LOP_NEW_ARRAY  :
        return convertNewArray(lir);
    case LOP_INSTANCE_OF:
        return convertInstanceOf(lir);
    case LOP_TABLE_SWITCH:
        return convertPackedSwitch(lir);
    case LOP_LOOKUP_SWITCH:
        return convertSparseSwitch(lir);
    case LOP_FILL_ARRAY_DATA:
        return convertFillArrayData(lir);
    case LOP_INVOKE         :
        return convertInvoke(lir);
    case LOP_FILLED_NEW_ARRAY:
        return convertFilledNewArray(lir);
    case LOP_PHI:
        UNREACHABLE();
        break;
    default: UNREACHABLE();
    } //end switch

    if (g_tfile != NULL) {
        fflush(g_tfile);
    }
    return NULL;
}


void Dex2IR::markLIRLabel()
{
    for (INT i = 0; i < LIRC_num_of_op(m_lircode); i++) {
        LIR * lir = LIRC_op(m_lircode, i);
        ASSERT0(lir);
        if (LIR_opcode(lir) == LOP_IF) {
            LIR * tgt = LIRC_op(m_lircode, LIR_op1(lir));
            ASSERT0(tgt);
            List<LabelInfo*> * lst = m_lir2labs.get(tgt);
            if (lst == NULL) {
                lst = new List<LabelInfo*>();
                m_lir2labs.set(tgt, lst);
                lst->append_tail(m_ru->genIlabel());
            }
        } else if (LIR_opcode(lir) == LOP_IFZ) {
            LIR * tgt = LIRC_op(m_lircode, LIR_op0(lir));
            ASSERT0(tgt);
            List<LabelInfo*> * lst = m_lir2labs.get(tgt);
            if (lst == NULL) {
                lst = new List<LabelInfo*>();
                m_lir2labs.set(tgt, lst);
                lst->append_tail(m_ru->genIlabel());
            }
        } else if (LIR_opcode(lir) == LOP_GOTO) {
            LIR * tgt = LIRC_op(m_lircode, ((LIRGOTOOp*)lir)->target);
            ASSERT0(tgt);
            List<LabelInfo*> * lst = m_lir2labs.get(tgt);
            if (lst == NULL) {
                lst = new List<LabelInfo*>();
                m_lir2labs.set(tgt, lst);
                lst->append_tail(m_ru->genIlabel());
            }
        } else if (LIR_opcode(lir) == LOP_TABLE_SWITCH) {
            LIRSwitchOp * p = (LIRSwitchOp*)lir;
            ASSERT0(LIR_dt(p) == 0x1);
            USHORT * pdata = p->data;
            USHORT num_of_case = pdata[1];

            if (num_of_case > 0) {
                UINT * pcase_entry = (UINT*)&pdata[4];
                for (USHORT i = 0; i < num_of_case; i++) {
                    LIR * tgt = LIRC_op(m_lircode, pcase_entry[i]);
                    ASSERT0(tgt);
                    List<LabelInfo*> * lst = m_lir2labs.get(tgt);
                    if (lst == NULL) {
                        lst = new List<LabelInfo*>();
                        m_lir2labs.set(tgt, lst);
                        lst->append_tail(m_ru->genIlabel());
                    }
                }
            }
        } else if (LIR_opcode(lir) == LOP_LOOKUP_SWITCH) {
            LIRSwitchOp * p = (LIRSwitchOp*)lir;
            ASSERT0(LIR_dt(p) == 0x2);
            USHORT * pdata = p->data;
            //pdata[1]: the number of CASE entry.
            UINT num_of_case = pdata[1];
            if (num_of_case > 0) {
                BYTE * tp = (BYTE*)pdata;
                UINT * pcase_entry = (UINT*)&tp[4 + num_of_case * 4];
                for (UINT i = 0; i < num_of_case; i++) {
                    LIR * tgt = LIRC_op(m_lircode, pcase_entry[i]);
                    ASSERT0(tgt);
                    List<LabelInfo*> * lst = m_lir2labs.get(tgt);
                    if (lst == NULL) {
                        lst = new List<LabelInfo*>();
                        m_lir2labs.set(tgt, lst);
                        lst->append_tail(m_ru->genIlabel());
                    }
                }
            }
        } else if (LIR_opcode(lir) == LOP_MOVE_EXCEPTION) {
            /* Do not generate catch start label here, and it will
             be made up during handle try-block.
            List<LabelInfo*> * lst = m_lir2labs.get(lir);
            if (lst == NULL) {
                lst = new List<LabelInfo*>();
                m_lir2labs.set(lir, lst);
                LabelInfo * lab = m_ru->genIlabel();
                LABEL_INFO_is_catch_start(lab) = true;
                lst->append_tail(lab);
            }
            */
        }
    }
}


void Dex2IR::markTryLabel()
{
    if (m_lircode->triesSize != 0) {
        TryInfo * tihead = NULL, * lasti = NULL;
        for (UINT i = 0; i < m_lircode->triesSize; i++) {
            LIROpcodeTry * each_try = m_lircode->trys + i;
            TryInfo * ti = (TryInfo*)xmalloc(sizeof(TryInfo));
            add_next(&tihead, &lasti, ti);

            INT pos = each_try->start_pc;
            ASSERT0(pos >= 0 && pos < LIRC_num_of_op(m_lircode));
            LIR * lir = LIRC_op(m_lircode, pos);
            List<LabelInfo*> * lst = m_lir2labs.get(lir);
            if (lst == NULL) {
                lst = new List<LabelInfo*>();
                m_lir2labs.set(lir, lst);
            }
            LabelInfo * lab = m_ru->genIlabel();
            LABEL_INFO_is_try_start(lab) = true;
            lst->append_tail(lab);
            ti->try_start = lab;
            ti->try_start_pos = pos;

            pos = each_try->end_pc;
            ASSERT0(pos >= 0 && pos <= LIRC_num_of_op(m_lircode));
            lab = m_ru->genIlabel();
            LABEL_INFO_is_try_end(lab) = true;
            if (pos < LIRC_num_of_op(m_lircode)) {
                lir = LIRC_op(m_lircode, pos);
                if (LIR_opcode(lir) == LOP_NOP &&

                    //So far, in our experience, not more than one NOP
                    //may be put at the end of LIR list in order to
                    //do alignment.
                    pos + 1 == LIRC_num_of_op(m_lircode)) {
                    //NOP will be deleted when after CFG finished.
                    //So, in actually, the try_end label will be the last.
                    m_last_try_end_lab_list.append_tail(lab);
                } else {
                    lst = m_lir2labs.get(lir);
                    if (lst == NULL) {
                        lst = new List<LabelInfo*>();
                        m_lir2labs.set(lir, lst);
                    }
                    lst->append_tail(lab);
                }
            } else {
                /* Record the Label which be placed at the end of LIR.
                e.g:
                    call '#monitor_exit'  id:168 throw sideeffect ai:EH,
                        $3:i32 param0 id:167

                    ilabel(_L12)(try_start ) id:170  <=== Try Start

                    call '#throw'  id:172 throw terminate sideeffect ai:EH,
                        $1:i32 param0 id:171

                    ilabel(_L13)(try_end ) id:173 <=== Try End, the last label

                Note the last Label can not be deleted by CFG optimizations
                because it will be used during updateLIRCode(). */
                m_last_try_end_lab_list.append_tail(lab);
            }
            ti->try_end = lab;
            ti->try_end_pos = pos;

            CatchInfo * last = NULL;
            for (UINT j = 0; j < each_try->catchSize; j++) {
                LIROpcodeCatch * each_catch = each_try->catches + j;
                CatchInfo * ci = (CatchInfo*)xmalloc(sizeof(CatchInfo));

                ASSERT0(each_catch->handler_pc < (UInt32)LIRC_num_of_op(m_lircode));
                LIR * x = LIRC_op(m_lircode, each_catch->handler_pc);
                ASSERT0(x);
                //CASE: it is not always true. Apk may be messed up.
                //ASSERTN(LIR_opcode(x) == LOP_MOVE_EXCEPTION,
                //         ("LIR at catch start must be move_exception"));

                LabelInfo * lab = NULL;
                lst = m_lir2labs.get(x);
                if (lst == NULL) {
                    lst = new List<LabelInfo*>();
                    m_lir2labs.set(x, lst);
                } else {
                    for (lab = lst->get_head();
                         lab != NULL; lab = lst->get_next()) {
                        if (LABEL_INFO_is_catch_start(lab)) { break; }
                    }

                    //Multiple label may correspond to same LIR.
                    //ASSERT0(lst->get_elem_count() == 1);
                }

                if (lab == NULL) {
                    lab = m_ru->genIlabel();
                    LABEL_INFO_is_catch_start(lab) = true;
                    lst->append_tail(lab);
                }

                ci->catch_start = lab;
                ci->kind = each_catch->class_type;
                ci->kindname = ci->kind == kDexNoIndex ?
                               "<any>" : dexStringByTypeIdx(m_df, ci->kind);
                //In DEX, one catch-block may be shared by multiple try-block.
                //We only record one of them.
                m_label2catchinfo.setAlways(lab, ci);
                add_next(&ti->catch_list, &last, ci);
                m_has_catch = true;
            }
        }
        m_ti = tihead;
    }
}


void Dex2IR::dump_lir2lab()
{
    if (g_tfile != NULL) {
        INT c;
        note("\n==-- DUMP LIR->LABEL --== ");

        TMapIter<LIR*, List<LabelInfo*>*> iter;
        List<LabelInfo*> * lst;
        for (LIR * lir = m_lir2labs.get_first(iter, &lst);
             lir != NULL; lir =    m_lir2labs.get_next(iter, &lst)) {
            ASSERT0(lst);
            note("\n");
            dump_lir2(lir, m_df, -1);
            for (LabelInfo * lab = lst->get_head();
                 lab != NULL; lab = lst->get_next()) {
                dumpLabel(lab);
            }
        }
    }
}


extern bool g_dd;
//'succ': return true if convertion is successful.
IR * Dex2IR::convert(bool * succ)
{
    m_has_catch = false;
    markLIRLabel();
    markTryLabel();
    IR * ir_list = NULL;
    IR * last = NULL;

    bool dump = g_dump_dex2ir && g_tfile != NULL;
    if (dump) {
        note("\n\n==== DEX->IR CONVERT %s =====",
                m_ru->getRegionName());
    }

    FILE * log = NULL;
    if (g_dd) {
        log = fopen("dex2ir.log", "a+");
        fprintf(log, "\n== %s ==", m_ru->getRegionName());
    }

    if (dump) { dump_lir2lab(); }

    INT start = -1;
    INT end = -1;
    TryInfo * ti = m_ti;
    m_current_catch_list  = NULL;
    if (ti != NULL) {
        start = ti->try_start_pos;
        end = ti->try_end_pos - 1;
    }

    for (INT i = 0; i < LIRC_num_of_op(m_lircode); i++) {
        LIR * lir = LIRC_op(m_lircode, i);

        FILE * t = NULL;
        if (g_dd) {
            t = g_tfile;
            g_tfile = log;
        }

        //Do not use j as idx, since it will mess up
        //the diff with other log.
        if (dump) { dump_lir(lir, m_df, -1); }

        if (g_dd) {
            g_tfile = t;
        }

        List<LabelInfo*> * labs = m_lir2labs.get(lir);
        if (labs != NULL) {
            for (LabelInfo * l = labs->get_head();
                 l != NULL; l = labs->get_next()) {
                add_next(&ir_list, &last, m_ru->buildLabel(l));
            }
        }

        if (i > end && ti != NULL) {
            ti = ti->next;
            if (ti != NULL) {
                start = ti->try_start_pos;
                end = ti->try_end_pos - 1;
            }
        }

        if (i >= start && i <= end) {
            m_current_catch_list = ti->catch_list;
            ASSERT0(m_current_catch_list);
        } else {
            m_current_catch_list = NULL;
        }

        IR * newir = convert(lir);

        if (dump) {
            dump_lir(lir, m_df, i);
            dumpIRList(newir, m_tm);
        }

        DexDbx const* dbx = m_dbxvec.get(i);
        if (dbx != NULL) {
            //Record Debug info.
            for (IR * tmp = newir; tmp != NULL; tmp = IR_next(tmp)) {
                AIContainer * ai = IR_ai(tmp);
                if (ai == NULL) {
                    ai = m_ru->allocAIContainer();
                    IR_ai(tmp) = ai;
                }
                ai->set((BaseAttachInfo*)dbx);
            }
        }

        add_next(&ir_list, &last, newir);
    }

    for (LabelInfo * li = m_last_try_end_lab_list.get_head();
         li != NULL; li = m_last_try_end_lab_list.get_next()) {
        add_next(&ir_list, &last, m_ru->buildLabel(li));
    }

    if (g_dd) {
        fclose(log);
    }
    *succ = true;
    return ir_list;
}
