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
#ifndef _DEX_UTIL_H_
#define _DEX_UTIL_H_

//Dex File Structure:
//    header!
//    string id!
//    type id!
//    proto id!
//    field id!
//    method id!
//    class def id!
//
//    annotation set ref list
//    annotation set item list !
//
//    code item !
//
//    annotations directory item!
//    type list info!
//    string data item list !
//    debug info!
//    annotation item list !
//    encodearray item list !
//
//    class Data item !
//    map list!

#define OBJ_MC_SIZE       16
#define ARRAY_MC_SIZE     32
#define NUM_OF_PART       3
#define CLASSPATH_PART    0
#define TYPE_PART         1
#define FUNCNAME_PART     2

class DexRegionMgr;

//Export Functions.
void dump_lir(LIR * lir, DexFile * df, INT pos);
void dump_lir2(LIR * lir, DexFile * df, INT pos);
void dump_all_lir(LIRCode * fu, DexFile * df, DexMethod const* dm);
void dump_all_class_and_field(DexFile * df);

//Generate unique name by assembling Class Name, Function Parameter Type,
//and Function Name.
//1th part: class path
//2th part: type string, include (parameters string list) return-value-type
//3th part: function name.
CHAR * assemblyUniqueName(
        OUT CHAR buf[],
        CHAR const* class_name,
        CHAR const* func_type,
        CHAR const* func_name);

//Extract the function parameter and return value name
//from function type string.
void extractFunctionTypeFromFunctionTypeString(
        CHAR const* functype,
        OUT CHAR * param_type,
        OUT CHAR * return_type);

//Extract the function parameter and return value name
//from entirely name format, and return the function name.
//Note outputbuf should big enough to hold the string.
void extractFunctionTypeFromRegionName(
        CHAR const* runame,
        OUT CHAR * classpath,
        OUT CHAR * functionname,
        OUT CHAR * param_type,
        OUT CHAR * return_type);

//Seperate param_type_string into a set of individual type string.
//Return the number of individual type strings.
//If params is nullptr, this function only compute the number of type string.
//Note the outputbuf recorded in params should big enough to
//hold each type string.
UINT extractSeparateParamterType(
        CHAR const* param_type_string,
        OUT List<CHAR*> * params,
        UINT len = 0);

//Print human readable type info according to type_string.
//Note this function check the length of buffer needed.
UINT printType2Buffer(CHAR const* type_string, OUT CHAR * buf);

//Return the Var related to given builtin.
inline Var * getBuiltinVar(BLTIN_TYPE blt, DexRegionMgr const* rumgr)
{
    ASSERT0(rumgr);
    bool find = false;
    Var * v = rumgr->getBuiltin2VarC().get((UINT)blt, &find);
    ASSERT0(find);
    return v;
}

inline BLTIN_TYPE getBuiltinType(IR const* ir, DexRegionMgr const* rumgr)
{
    ASSERT0(ir && ir->is_call() && rumgr);
    BLTIN_TYPE bt = (BLTIN_TYPE)rumgr->getVar2BuiltinC().
                                get_mapped(CALL_idinfo(ir));
    ASSERT0(bt > BLTIN_UNDEF && BLTIN_LAST);
    return bt;
}

CHAR const* getClassSourceFilePath(
        DexFile const* df,
        DexClassDef const* class_info);

CHAR const* get_class_name(DexFile const* df, DexMethod const* dm);
CHAR const* get_class_name(DexFile const* df, UINT cls_type_idx);
CHAR const* get_class_name(DexFile const* df, DexClassDef const* class_info);
CHAR const* get_func_name(DexFile const* df, DexMethod const* dm);
CHAR const* get_func_type(DexFile const* df, DexMethod const* dm);
CHAR const* get_func_type(DexFile const* df, DexMethodId const* dmid);
CHAR const* get_field_name(DexFile const* df, UINT field_id);
CHAR const* get_field_name(DexFile const* df, DexField * finfo);
CHAR const* get_field_type_name(DexFile const* df, UINT field_id);
CHAR const* get_field_type_name(DexFile const* df, DexField * finfo);
CHAR const* get_field_class_name(DexFile const* df, UINT field_id);
CHAR const* get_field_class_name(DexFile const* df, DexField * finfo);
CHAR const* get_dt_name(LIR * ir);

bool is_builtin(IR const* ir, BLTIN_TYPE bt, DexRegionMgr const* rumgr);

inline bool is_builtin(IR const* ir)
{
    //The first charactor of builtin function is #.
    ASSERT0(ir && ir->is_call());
    return ((CCall*)ir)->getCalleeNameString()[0] == '#';
}

inline bool is_wide(LIR * lir)
{
    return LIR_dt(lir) == LIR_JDT_wide ||
           LIR_dt(lir) == LIR_JDT_double ||
           LIR_dt(lir) == LIR_JDT_long;
}
#endif
