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
#include "libdex/DexProto.h"
#include "liropcode.h"
#include "lir.h"
#include "drAlloc.h"
#include "d2d_comm.h"
#include "cominc.h"
#include "comopt.h"
#include "dex.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"

CHAR const* get_dt_name(LIR * ir)
{
    switch (LIR_dt(ir)) {
    case LIR_JDT_unknown:
        return "UNDEF";
    case LIR_JDT_void    :
        return "VOID";
    case LIR_JDT_int    :
        return "INT";
    case LIR_JDT_object    :
        return "OBJ";
    case LIR_JDT_boolean:
        return "BOOL";
    case LIR_JDT_byte    :
        return "BYTE";
    case LIR_JDT_char    :
        return "CHAR";
    case LIR_JDT_short    :
        return "SHORT";
    case LIR_JDT_float    :
        return "FLOAT";
    case LIR_JDT_none    :
        return "NONE";
    case LIR_wide:
        return "WIDE";
    case LIR_JDT_wide:
        return "WIDE";
    case LIR_JDT_long:
        return "LONG";
    case LIR_JDT_double:
        return "DOUBLE";
    }
    return nullptr;
}


//Return true if ir is specified builtin function.
bool is_builtin(IR const* ir, BLTIN_TYPE bt, DexRegionMgr const* rumgr)
{
    if (!is_builtin(ir)) { return false; }
    BLTIN_TYPE tbt = getBuiltinType(ir, rumgr);
    if (tbt != bt) { return false; }

#ifdef _DEBUG_
    //Check builtin function's legality.
    switch (bt) {
    case BLTIN_NEW                :
        {
            /*CALL (r:PTR:4 ptbase:1) id:4
                ID ('#new' r:PTR:4 ptbase:1) callee id:1
                PARAM param0 id:3
                    INTCONST r:U32:4 (1 0x1) id:2
                PR, retv0 */
            ASSERT0(CALL_param_list(ir) && CALL_param_list(ir)->is_const());
            ASSERT0(IR_next(CALL_param_list(ir)) == nullptr);
            ASSERT0(ir->hasReturnValue());
        }
        break;
    case BLTIN_NEW_ARRAY        :
        {
            /* (new_array, res), UNDEF, v5 <-
                (num_of_elem, op0)v8, (elem_type, op1)'[LCAnimal;'

            CALL (r:PTR:4 ptbase:1) id:66
                ID ('#new_array' r:PTR:4 ptbase:1) callee id:61
                PARAM param0 id:64
                    PR2 (r:PTR:4 ptbase:1) id:62
                PARAM param1 id:65
                    INTCONST r:U32:4 (14 0xe) id:64
                PR7 (r:PTR:4 ptbase:1) retv id:69 */
            //The first is a pr that record the number of array elements.
            IR * p = CALL_param_list(ir);
            ASSERT0(p && p->is_pr());
            p = p->get_next();

            //The second is array element type id.
            ASSERT0(p && p->is_const());
            UINT elem_type_id = CONST_int_val(p);
            ASSERT0(p->get_next() == nullptr);
        }
        break;
    case BLTIN_MOVE_EXP         :
        //The res is a pr that record the exception handler.
        ASSERT0(ir->hasReturnValue());
        break;
    case BLTIN_MOVE_RES         :
        ASSERT0(ir->hasReturnValue());
        break;
    case BLTIN_THROW            :
        {
            //The first is a pr that is the reference of
            //the exception object.
            IR * p = CALL_param_list(ir);
            ASSERT0(p->is_pr());
            ASSERT0(p->get_next() == nullptr);
        }
        break;
    case BLTIN_CHECK_CAST       :
        {
            /*
            CALL (r:PTR:4 ptbase:1) id:113
                ID ('#check_cast' r:PTR:4 ptbase:1) callee id:108
                PARAM param0 id:110
                    PR8 (r:I32:4) id:109
                PARAM param1 id:112
                    INTCONST r:I32:4 (16 0x10) id:111
            */
            //The first is a pr that record the object-ptr
            IR * p = CALL_param_list(ir);
            ASSERT0(p && p->is_pr());
            p = p->get_next();

            //The second is class type id.
            ASSERT0(p && p->is_const());
            UINT type_id = CONST_int_val(p);
            ASSERT0(p->get_next() == nullptr);
        }
        break;
    case BLTIN_FILLED_NEW_ARRAY    :
        {
            /*
            CALL (r:PTR:4 ptbase:1) id:82
                ID ('#filled_new_array' r:PTR:4 ptbase:1) callee id:75
                PARAM param0 id:77
                    INTCONST r:I32:4 (0 0x0) id:76
                PARAM param1 id:77
                    INTCONST r:I32:4 (13 0xd) id:76
                PARAM param2 id:79
                    PR2 (r:I32:4) id:78
                PARAM param3 id:81
                    PR8 (r:I32:4) id:80
            */
            IR * p = CALL_param_list(ir);

            //The first record invoke flag.
            ASSERT0(p->is_const());
            p = p->get_next();

            //The second record class type id.
            ASSERT0(p && p->is_const());
            p = p->get_next();
            while (p != nullptr) {
                ASSERT0(p->is_pr());
                p = p->get_next();
            }
        }
        break;
    case BLTIN_FILL_ARRAY_DATA:
        {
            /*
            CALL (r:PTR:4 ptbase:1) id:82
                ID ('#fill_array_data' r:PTR:4 ptbase:1) callee id:75
                PARAM param0 id:77
                    PR2 r:I32:4 id:76
                PARAM param1 id:77
                    INTCONST r:U32:4 (13 0xd) id:76
            */
            IR * p = CALL_param_list(ir);

            //The first record array obj-ptr.
            ASSERT0(p->is_pr());
            p = p->get_next();

            //The second record binary data.
            ASSERT0(p && p->is_const());
            ASSERT0(p->get_next() == nullptr);
        }
        break;
    case BLTIN_CONST_CLASS      :
        {
            /*
            CALL (r:PTR:4 ptbase:1) id:94
                ID ('#const_class' r:PTR:4 ptbase:1) callee id:88
                PARAM param0 id:91
                    PR9 (r:I32:4) id:90
                PARAM param1 id:93
                    INTCONST r:I32:4 (2 0x2) id:92
            */
            ASSERT0(ir->hasReturnValue());
            ASSERT0(CALL_param_list(ir) && CALL_param_list(ir)->is_const());
            ASSERT0(IR_next(CALL_param_list(ir)) == nullptr);
        }
        break;
    case BLTIN_ARRAY_LENGTH     :
        ASSERT0(ir->hasReturnValue());
        ASSERT0(CALL_param_list(ir) &&
                 CALL_param_list(ir)->is_pr());
        ASSERT0(IR_next(CALL_param_list(ir)) == nullptr);
        break;
    case BLTIN_MONITOR_ENTER    :
        ASSERT0(CALL_param_list(ir) &&
                 CALL_param_list(ir)->is_pr());
        ASSERT0(IR_next(CALL_param_list(ir)) == nullptr);
        break;
    case BLTIN_MONITOR_EXIT     :
        ASSERT0(CALL_param_list(ir) &&
                 CALL_param_list(ir)->is_pr());
        ASSERT0(IR_next(CALL_param_list(ir)) == nullptr);
        break;
    case BLTIN_INSTANCE_OF      :
        {
            /*
            CALL (r:PTR:4 ptbase:1) id:82
                ID ('#instance_of' r:PTR:4 ptbase:1) callee id:75
                PARAM param0 id:77
                    PR r:I32:4 (0 0x0) id:76
                PARAM param1 id:77
                    PR r:I32:4 (13 0xd) id:76
                PARAM param2 id:79
                    INTCONST
            */
            IR * p = CALL_param_list(ir);
            //The first is object-ptr reg.
            ASSERT0(p && p->is_pr());
            p = p->get_next();

            //The second is type-id.
            ASSERT0(p->is_const());
            ASSERT0(p->get_next() == nullptr);

            //The first is result reg..
            ASSERT0(ir->hasReturnValue());
        }
        break;
    case BLTIN_CMP_BIAS:
        {
            /*
            AABBCC
            cmpkind vAA <- vBB, vCC
            IR will be:
                IR_CALL
                    param0: cmp-kind.
                    param1: vBB
                    param2: vCC
                    res: vAA
            */
            IR * p = CALL_param_list(ir);
            //The first is object-ptr reg.
            RegionMgr * trumgr =
                   const_cast<RegionMgr*>((RegionMgr const*)rumgr);
            ASSERT0(p && p->is_int());
            p = p->get_next();

            ASSERT0(p && p->is_pr());
            p = p->get_next();

            ASSERT0(p && p->is_pr());
            ASSERT0(p->get_next() == nullptr);
            ASSERT0(ir->hasReturnValue());
        }
        break;
    default: UNREACHABLE();
    }
#endif
    return true;
}


//Generate unique name by assembling Class Name, Function Parameter Type,
//and Function Name.
CHAR * assemblyUniqueName(
        OUT CHAR buf[],
        CHAR const* class_name,
        CHAR const* func_type,
        CHAR const* func_name)
{
    sprintf(buf, "%s:%s:%s", class_name, func_type, func_name);
    return buf;
}


//Extract the function parameter and return value name
//from function type string.
//Note outputbuf should big enough to hold the string.
void extractFunctionTypeFromFunctionTypeString(
        CHAR const* functype,
        OUT CHAR * param_type,
        OUT CHAR * return_type)
{
    ASSERT0(functype && param_type && return_type);

    UINT l = strlen(functype);
    ASSERTN(l > 0, ("unsupport string pattern"));

    CHAR const* start = functype;
    CHAR const* end = functype + l - 1;

    //The format of type string is: (Ljava/lang/String;)Ljava/lang/String;
    CHAR const* p = start;
    CHAR const* start2 = nullptr;
    CHAR const* end2 = nullptr;
    for (; l > 0; l--, p++) {
        if (*p == '(') {
            ASSERTN(start2 == nullptr, ("unsupport string pattern"));
            start2 = p;
        } else if (*p == ')') {
            end2 = p;
            UINT s = end2 - start2 - 1;
            ::memcpy(param_type, start2 + 1, s);
            param_type[s] = 0;
            break;
        }
    }

    ASSERTN(end2, ("unsupport string pattern"));

    UINT s = end - end2;
    ::memcpy(return_type, end2 + 1, s);
    return_type[s] = 0;
}


//Extract the function parameter and return value name
//from entirely name format, and return the function name.
//Note outputbuf should big enough to hold the string.
void extractFunctionTypeFromRegionName(
        CHAR const* runame,
        OUT CHAR * classpath,
        OUT CHAR * functionname,
        OUT CHAR * param_type,
        OUT CHAR * return_type)
{
    ASSERT0(runame);
    Vector<CHAR*> ret;
    UINT num = xsplit(runame, ret, ":");
    ASSERTN(num == NUM_OF_PART, ("You may be input builtin function name"));

    if (classpath != nullptr) {
        UINT l = strlen(ret[CLASSPATH_PART]);
        ::memcpy(classpath, ret[CLASSPATH_PART], l + 1);
        classpath[l] = 0;
    }

    if (functionname != nullptr) {
        UINT l = strlen(ret[FUNCNAME_PART]);
        ::memcpy(functionname, ret[FUNCNAME_PART], l + 1);
        functionname[l] = 0;
    }

    //The format of type string is: (Ljava/lang/String;)Ljava/lang/String;
    CHAR const* typestr = ret[TYPE_PART];
    CHAR const* start = nullptr;
    CHAR const* end = nullptr;

    //Parse parameter type string.
    UINT len = 0;
    for (CHAR const* p = typestr; *p != 0; p++, len++) {
        if (*p == '(') {
            ASSERTN(start == nullptr, ("unsupport string pattern"));
            start = p;
        } else if (*p == ')') {
            end = p;
            UINT s = end - start - 1;
            if (param_type != nullptr) {
                ::memcpy(param_type, start + 1, s);
                param_type[s] = 0;
            }
            len++;
            break;
        }
    }

    ASSERTN(end, ("unsupport string pattern"));

    UINT s = ::strlen(typestr) - len;
    if (return_type != nullptr) {
        ::memcpy(return_type, typestr + len, s);
        return_type[s] = 0;
    }

    for (UINT i = 0; i < num; i++) {
        ::free(ret[i]);
    }
}


//Print human readable type info according to type_string.
//Return the length of string.
//Note the length does not include the ending '\0'.
UINT printType2Buffer(CHAR const* type_string, OUT CHAR * buf)
{
    ASSERT0(type_string);

    //buf may be nullptr. If it is the case, user want to compute the
    //actual length of the buffer.

    CHAR const* name = nullptr;
    UINT len = 0;
    CHAR const* start = type_string;
    switch (*type_string) {
    case 'V': name = "void"; len = strlen(name); start++; break;
    case 'Z': name = "boolean"; len = strlen(name); start++; break;
    case 'B': name = "unsigned char"; len = strlen(name); start++; break;
    case 'S': name = "short"; len = strlen(name); start++; break;
    case 'C': name = "char"; len = strlen(name); start++; break;
    case 'I': name = "int"; len = strlen(name); start++; break;
    case 'J': name = "long long"; len = strlen(name); start++; break;
    case 'F': name = "float"; len = strlen(name); start++; break;
    case 'D': name = "double"; len = strlen(name); start++; break;
    case 'L': //object pointer
        name = type_string + 1;
        CHAR const* p;
        for (p = type_string; *p != 0 && *p != ';'; p++, len++) ;
        ASSERT0(len > 0);

        start += len;

        len--; //omit L
        break;
    case '[': break;
    default: ASSERTN(0, ("unknown dex type"));
    }

    if (buf != nullptr) {
        for (UINT i = 0; i < len; i++) {
            buf[i] = name[i];
        }
        buf += len;
    }

    for (; *start == '['; start++) {
        len += 2; // print [ ]
        if (buf != nullptr) {
            *buf++ = '[';
            *buf++ = ']';
        }
    }

    if (buf != nullptr) {
        *buf = 0;
    }
    return len; //the length does not include the ending '\0'.
}


//Note outputbuf should big enough to hold the string.
UINT extractSeparateParamterType(
        CHAR const* param_type_string,
        OUT List<CHAR*> * params,
        UINT len)
{
    //The format of type string is: a;b;c;
    ASSERT0(param_type_string);
    if (len == 0) {
        len = strlen(param_type_string);
    }
    CHAR const* start = param_type_string;
    CHAR const* end = param_type_string;

    //Note outputbuf should big enough to hold the string.
    CHAR * outputbuf = nullptr;
    if (params != nullptr) {
        outputbuf = params->get_head();
    }

    UINT num_of_param = 0;
    bool is_obj = false;
    for (; end != 0 && len != 0;) {
        //Determine if parameter is primitive type.
        if (is_obj) {
            //Parse object type.
            if (*end != ';') {
                len--;
                end++;
                continue;
            }

            UINT s = end - start;
            if (outputbuf != nullptr) {
                ::memcpy(outputbuf, start, s);
                outputbuf[s] = 0;
                ASSERT0(params);
                outputbuf = params->get_next();
            }
            num_of_param++;
            start = end + 1;
            is_obj = false;
        } else {
            switch (*end) {
            case 'V': //void
            case 'Z': //boolean
            case 'B': //u8
            case 'S': //i16
            case 'C': //i8
            case 'I': //i32
            case 'J': //i64
            case 'F': //f32
            case 'D': //f64
                if (start == end) {
                    //Parse primitive type.
                    if (outputbuf != nullptr) {
                        outputbuf[0] = *end;
                        outputbuf[1] = 0;
                        ASSERT0(outputbuf);
                        outputbuf = params->get_next();
                    }

                    num_of_param++;
                    start = end + 1;
                }
                break;
            case 'L': //object pointer
                is_obj = true;
                break;
            case '[': //array
                //Parse array type.
                while (*end == '[' && end != 0) {
                    len--;
                    end++;
                }
                ASSERT0(end != 0);
                continue;
            default:
                ASSERTN(is_obj,
                ("Not match any case, it is also not an object"));
                break;
            }
        }
        len--;
        end++;
   }

    ASSERT0(len == 0);
    //ASSERT0(*end == 0); //User may ask to parse part of string.
    return num_of_param;
}


//cls_type_idx: typeIdx in string-type-table,
//    not the idx in class-def-item table.
CHAR const* get_class_name(DexFile const* df, UINT cls_type_idx)
{
    return dexStringByTypeIdx(df, cls_type_idx);
}


//Get the source file path of the class.
CHAR const* getClassSourceFilePath(
        DexFile const* df,
        DexClassDef const* class_info)
{
    return dexStringById(df, class_info->sourceFileIdx);
}


CHAR const* get_class_name(DexFile const* df, DexClassDef const* class_info)
{
    ASSERT0(class_info);
    CHAR const* class_name = dexStringByTypeIdx(df, class_info->classIdx);
    ASSERT0(class_name);
    return class_name;
}


CHAR const* get_class_name(DexFile const* df, DexMethod const* dm)
{
    ASSERT0(df && dm);
    DexMethodId const* pMethodId = dexGetMethodId(df, dm->methodIdx);
    return dexStringByTypeIdx(df, pMethodId->classIdx);
}


CHAR const* get_func_name(DexFile const* df, DexMethod const* dm)
{
    ASSERT0(df && dm);
    DexMethodId const* pMethodId = dexGetMethodId(df, dm->methodIdx);
    return dexStringById(df, pMethodId->nameIdx);
}


//Return function parameter and return type.
CHAR const* get_func_type(DexFile const* df, DexMethod const* dm)
{
    ASSERT0(df && dm);
    DexMethodId const* pMethodId = dexGetMethodId(df, dm->methodIdx);
    return dexCopyDescriptorFromMethodId(df, pMethodId);
}


//Return function parameter and return type.
CHAR const* get_func_type(DexFile const* df, DexMethodId const* dmid)
{
    ASSERT0(df && dmid);
    return dexCopyDescriptorFromMethodId(df, dmid);
}


void dump_lir(LIR * lir, DexFile * df, INT pos)
{
    if (g_tfile == nullptr || lir == nullptr) return;
    note("\n");
    dump_lir2(lir, df, pos);
}


//Dump LIR stmts stored in fu->lirList array.
void dump_lir2(LIR * lir, DexFile * df, INT pos)
{
    if (g_tfile == nullptr || lir == nullptr) return;
    if (pos < 0) {
        prt("%s", LIR_name(lir));
    } else {
        prt("(%dth)%s", pos, LIR_name(lir));
    }
    switch (LIR_opcode(lir)) {
    case LOP_NOP:
        break;
    case LOP_CONST:
        switch (LIR_dt(lir)) {
        case LIR_JDT_unknown:
            prt(", INT");
            if (is_s4(LIR_int_imm(lir)) && LIR_res(lir) < 16) {
                //AB
                prt(", v%d <- %d",
                        LIR_res(lir), (INT)LIR_int_imm(lir));
            } else if (is_s16(LIR_int_imm(lir))) {
                //AABBBB
                prt(", v%d <- %d",
                        LIR_res(lir), (INT)LIR_int_imm(lir));
            } else {
                //AABBBBBBBB
                prt(", v%d <- %d",
                        LIR_res(lir), (INT)LIR_int_imm(lir));
            }
            break;
        case LIR_JDT_wide:
            prt(", %s", get_dt_name(lir));
            if (is_swide16(LIR_int_imm(lir))) {
                //AABBBB
                prt(", (v%d,v%d) <- %d",
                        LIR_res(lir), LIR_res(lir) + 1, (INT)LIR_int_imm(lir));
            } else if (is_swide32(LIR_int_imm(lir))) {
                //AABBBBBBBB
                prt(", (v%d,v%d) <- %d",
                        LIR_res(lir), LIR_res(lir) + 1, (INT)LIR_int_imm(lir));
            } else {
                //AABBBBBBBBBBBBBBBB
                prt(", (v%d,v%d) <- %lld",
                        LIR_res(lir), LIR_res(lir) + 1, LIR_int_imm(lir));
            }
            break;
        default:
            //It seems dex does not distinguish
            //float and integer const. And regard float as
            //32bit integer, double will be 64bit integer.
            UNREACHABLE();
        }
        break;
    case LOP_RETURN:
        {
            switch (LIR_dt(lir)) {
            case LIR_JDT_unknown: //return preg.
                prt(", INT");
                prt(", v%d", LIR_res(lir));
                break;
            case LIR_JDT_void: //No return value.
                break;
            case LIR_JDT_object: //return object.
                prt(", obj_ptr:v%d", LIR_res(lir));
                break;
            case LIR_JDT_wide:
                prt(", %s", get_dt_name(lir));
                prt(", (v%d,v%d)", LIR_res(lir), LIR_res(lir) + 1);
                break;
            default: UNREACHABLE();
            }
        }
        break;
    case LOP_THROW: //AA
        //Throws an exception object.
        //The reference of the exception object is in vx.
        prt(", v%d", LIR_res(lir));
        break;
    case LOP_MONITOR_ENTER  :
        prt(", v%d", LIR_res(lir));
        break;
    case LOP_MONITOR_EXIT   :
        break;
    case LOP_MOVE_RESULT    :
        {
            //Move function return value to regisiter.
            //AA
            LIRAOp * p = (LIRAOp*)lir;
            switch (LIR_dt(lir)) {
            case LIR_JDT_unknown: //lexOpcode = lc_mov_result32; break;
                prt(", INT");
                prt(", retval -> v%d", LIR_res(lir));
                break;
            case LIR_JDT_wide: //lexOpcode = lc_mov_result64; break;
                prt(", %s", get_dt_name(lir));
                prt(", retval -> (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1);
                break;
            case LIR_JDT_object: //lexOpcode = lc_mov_result_object; break;
                prt(", obj-ptr");
                prt(", retval -> v%d", LIR_res(lir));
                break;
            }
        }
        break;
    case LOP_MOVE_EXCEPTION : //AA
        prt(", v%d", LIR_res(lir));
        break;
    case LOP_GOTO            : //AABBBBBBBB
        {
            LIRGOTOOp * p = (LIRGOTOOp*)lir;
            prt(", (lirIdx)%dth", p->target);
        }
        break;
    case LOP_MOVE        :
        switch (LIR_dt(lir)) {
        case LIR_JDT_unknown:
            prt(", INT");
            if ((LIR_op0(lir) | LIR_res(lir)) < 16) {
                //AB
                prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else if (LIR_res(lir) < 256) {
                //AABBBB
                prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else {
                //AAAABBBB
                prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            }
            break;
        case LIR_JDT_wide:
            prt(", %s", get_dt_name(lir));
            if ((LIR_op0(lir) | LIR_res(lir)) < 16) {
                //AB
                prt(", (v%d,v%d) <- (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1,
                        LIR_op0(lir), LIR_op0(lir) + 1);
            } else if (LIR_res(lir) < 256) {
                //AABBBB
                prt(", (v%d,v%d) <- (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1,
                        LIR_op0(lir), LIR_op0(lir) + 1);
            } else {
                //AAAABBBB
                prt(", (v%d,v%d) <- (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1,
                        LIR_op0(lir), LIR_op0(lir) + 1);
            }
            break;
        case LIR_JDT_object:
            prt(", obj-ptr");
            if ((LIR_op0(lir) | LIR_res(lir)) < 16) {
                //AB
                prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else if (LIR_res(lir) < 256) {
                //AABBBB
                prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else {
                //AAAABBBB
                prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            }
            break;
        }
        break;
    case LOP_NEG        : //AB
    case LOP_NOT        : //AB
        prt(", %s", get_dt_name(lir));
        if (is_wide(lir)) {
            prt(", (v%d,v%d) <- (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir), LIR_op0(lir)+1);
        } else {
            prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
        }
        break;
    case LOP_CONVERT    : //AB
        switch (LIR_dt(lir)) {
        case LIR_convert_i2l:
            prt(", INT->LONG");
            prt(", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_i2f: prt(", INT->FLOAT");  break;
        case LIR_convert_i2d:
            prt(", INT->DOUBLE");
            prt(", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_l2i:
            prt(", LONG->INT");
            prt(", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_l2f:
            prt(", LONG->FLOAT");
            prt(", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_l2d:
            prt(", LONG->DOUBLE");
            prt(", (v%d,v%d) <- (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_f2i: prt(", FLOAT->INT");  break;
        case LIR_convert_f2l:
            prt(", FLOAT->LONG");
            prt(", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_f2d:
            prt(", FLOAT->DOUBLE");
            prt(", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_d2i:
            prt(", DOUBLE->INT");
            prt(", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_d2l:
            prt(", DOUBLE->LONG");
            prt(", (v%d,v%d) <- (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_d2f:
            prt(", DOUBLE->FLOAT");
            prt(", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_i2b:
            prt(", INT->BOOL");
            prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            break;
        case LIR_convert_i2c:
            prt(", INT->CHAR");
            prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            break;
        case LIR_convert_i2s:
            prt(", INT->SHORT");
            prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            break;
        default: UNREACHABLE();
        }
        break;
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
        prt(", %s", get_dt_name(lir));
        if (is_wide(lir)) {
            prt(", (v%d,v%d) <- (v%d,v%d), (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), LIR_op0(lir)+1);
        } else {
            prt(", v%d <- v%d, v%d",
                    LIR_res(lir), LIR_res(lir), LIR_op0(lir));
        }
        break;
    case LOP_ARRAY_LENGTH: //AABBBB
        //Calculates the number of elements of the array referenced by vy
        //and puts the length value into vx.
        prt(", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
        break;
    case LOP_IFZ         :
        //AABBBB
        switch (LIR_dt(lir)) {
        case LIR_cond_EQ: prt(", =="); break;
        case LIR_cond_NE: prt(", !="); break;
        case LIR_cond_LT: prt(", <"); break;
        case LIR_cond_GE: prt(", >="); break;
        case LIR_cond_GT: prt(", >"); break;
        case LIR_cond_LE: prt(", <="); break;
        }
        if (is_wide(lir)) {
            prt(", (v%d,v%d), 0, (lirIdx)%dth",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
        } else {
            prt(", v%d, 0, (lirIdx)%dth",
                    LIR_res(lir), LIR_op0(lir));
        }
        break;
    case LOP_NEW_INSTANCE:
        //AABBBB
        //LIR_op0(lir) is class-type-id, not class-declaration-id.
        ASSERT0(df);
        prt(", (obj_ptr)v%d <- (clsname<%d>)%s",
                LIR_res(lir),
                LIR_op0(lir),
                get_class_name(df, LIR_op0(lir)));
        break;
    case LOP_CONST_STRING:
        //AABBBB or AABBBBBBBB
        ASSERT0(df);
        prt(", v%d <- (strofst<%d>)\"%s\"",
                LIR_res(lir),
                LIR_op0(lir),
                dexStringById(df, LIR_op0(lir)));
        break;
    case LOP_CONST_CLASS :
        //AABBBB
        //const-class vx,type_id
        //Moves the class object of a class identified by
        //type_id (e.g. Object.class) into vx.
        prt(", v%d <- (clsname<%d>)%s",
                LIR_res(lir),
                LIR_op0(lir),
                dexStringByTypeIdx(df, LIR_op0(lir)));
        break;
    case LOP_SGET        :
        //AABBBB
        prt(", %s", get_dt_name(lir));
        ASSERT0(df);
        prt(", v%d <- (ofst<%d>)%s::%s",
                LIR_res(lir),
                LIR_op0(lir),
                get_field_class_name(df, LIR_op0(lir)),
                get_field_name(df, LIR_op0(lir)));
        break;
    case LOP_CHECK_CAST  :
        //AABBBB
        ASSERT0(df);
        prt(", v%d '%s'",
                LIR_res(lir),
                dexStringByTypeIdx(df, LIR_op0(lir)));
        break;
    case LOP_SPUT        :
        {
            //AABBBB
            LIRABOp * p = (LIRABOp*)lir;
            prt(", %s", get_dt_name(lir));
            ASSERT0(df);
            if (is_wide(lir)) {
                prt(", (v%d,v%d) -> %s::%s",
                        LIR_res(lir), LIR_res(lir)+1,
                        get_field_class_name(df, LIR_op0(lir)),
                        get_field_name(df, LIR_op0(lir)));
            } else {
                prt(", v%d -> %s::%s",
                        LIR_res(lir),
                        get_field_class_name(df, LIR_op0(lir)),
                        get_field_name(df, LIR_op0(lir)));
            }
        }
        break;
    case LOP_APUT        :
        //AABBCC
        prt(", %s", get_dt_name(lir));
        ASSERT0(df);
        if (is_wide(lir)) {
            prt(", (v%d,v%d) -> (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), (UINT)LIR_op1(lir));
        } else {
            prt(", v%d -> (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
        }
        break;
    case LOP_AGET      :
        //AABBCC
        prt(", %s", get_dt_name(lir));
        ASSERT0(df);
        if (is_wide(lir)) {
            prt(", (v%d,v%d) <- (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), (UINT)LIR_op1(lir));
        } else {
            prt(", v%d <- (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
        }
        break;
    case LOP_CMPL      :
    case LOP_CMP_LONG  :
        //AABBCC
        ASSERT0(df);
        switch (LIR_dt(lir)) {
        case LIR_CMP_float:
            prt(", FLOAT");
            prt(", v%d, v%d, %d",
                    LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
            break;
        case LIR_CMP_double:
            prt(", DOUBLE");
            prt(", (v%d,v%d), (v%d,v%d), %d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), LIR_op0(lir)+1,
                    (UINT)LIR_op1(lir));
            break;
        default: UNREACHABLE();
        }
        break;
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
        {
            prt(", %s", get_dt_name(lir));
            LIRABCOp * p = (LIRABCOp*)lir;
            if (is_wide(lir)) {
                prt(", (v%d,v%d) <- (v%d,v%d), (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir)+1,
                        LIR_op0(lir), LIR_op0(lir)+1,
                        (UINT)LIR_op1(lir), (UINT)LIR_op1(lir)+1);
            } else {
                prt(", v%d <- v%d, v%d",
                        LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
            }
        }
        break;
    case LOP_IF        :
        //ABCCCC
        switch (LIR_dt(lir)) {
        case LIR_cond_EQ: prt(", =="); break;
        case LIR_cond_NE: prt(", !="); break;
        case LIR_cond_LT: prt(", <"); break;
        case LIR_cond_GE: prt(", >="); break;
        case LIR_cond_GT: prt(", >"); break;
        case LIR_cond_LE: prt(", <="); break;
        }
        prt(", v%d, v%d, (lirIdx)%dth",
                LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
        break;
    case LOP_ADD_LIT   : //AABBCC, AABBCCCC
    case LOP_SUB_LIT   : //AABBCC, AABBCCCC
    case LOP_MUL_LIT   : //AABBCC, AABBCCCC
    case LOP_DIV_LIT   : //AABBCC, AABBCCCC
    case LOP_REM_LIT   : //AABBCC, AABBCCCC
    case LOP_AND_LIT   : //AABBCC, AABBCCCC
    case LOP_OR_LIT    : //AABBCC, AABBCCCC
    case LOP_XOR_LIT   : //AABBCC, AABBCCCC
    case LOP_SHL_LIT   : //AABBCC
    case LOP_SHR_LIT   : //AABBCC
    case LOP_USHR_LIT   : //AABBCC
        {
            prt(", %s", get_dt_name(lir));
            LIRABCOp * p = (LIRABCOp*)lir;
            if (is_wide(lir)) {
                prt(", (v%d,v%d) <- (v%d,v%d),",
                        LIR_res(lir), LIR_res(lir)+1,
                        LIR_op0(lir), LIR_op0(lir)+1);
            } else {
                prt(", v%d <- v%d,",
                        LIR_res(lir), LIR_op0(lir));
            }

            if (is_s8((INT)LIR_op1(lir))) {
                //8bit imm
                prt("(lit8)%d", (INT)LIR_op1(lir));
            } else if (is_s16((INT)LIR_op1(lir))) {
                //16bit imm
                prt("(lit16)%d", (INT)LIR_op1(lir));
            } else {
                UNREACHABLE();
            }
        }
        break;
    case LOP_IPUT       :
        prt(", %s", get_dt_name(lir));
        //ABCCCC
        ASSERT0(df);
        if (is_wide(lir)) {
            prt(", (v%d,v%d) -> (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        } else {
            prt(", v%d -> (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir),
                    LIR_op0(lir),
                    (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        }
        break;
    case LOP_IGET       :
        prt(", %s", get_dt_name(lir));
        //ABCCCC
        ASSERT0(df);
        if (is_wide(lir)) {
            prt(", (v%d,v%d) <- (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir),
                    (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        } else {
            prt(", v%d <- (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir),
                    LIR_op0(lir),
                    (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        }
        break;
    case LOP_INSTANCE_OF:
        prt(", (pred)v%d <- v%d, (clsname<%d>)'%s'",
                LIR_res(lir),
                LIR_op0(lir),
                (UINT)LIR_op1(lir),
                dexStringByTypeIdx(df, (UINT)LIR_op1(lir)));
        break;
    case LOP_NEW_ARRAY  :
        //ABCCCC
        //new-array v%d(res) <- v%d(op0), LCAnimal(op1)
        prt(", %s", get_dt_name(lir));
        //ABCCCC
        ASSERT0(df);
        prt(", v%d <- (num_of_elem)v%d, (elem_type<%d>)'%s'",
                LIR_res(lir),
                LIR_op0(lir),
                (UINT)LIR_op1(lir),
                dexStringByTypeIdx(df, (UINT)LIR_op1(lir)));
        break;
    case LOP_TABLE_SWITCH:
        {
            LIRSwitchOp * p = (LIRSwitchOp*)lir;
            ASSERT0(LIR_dt(p) == 0x1);
            prt(", v%d", p->value);
            USHORT * pdata = p->data;

            //data[0]: flag to indicate switch-table type:
            //    0x1 TABLE_SWITCH, 0x2 LOOKUP_SWITCH
            USHORT f = pdata[0];
            ASSERT0(f == 0x100);
            //data[1]: the number of CASE entry.
            USHORT num_of_case = pdata[1];

            //data[2..3]: the base value of case-table
            UINT base_val = *((UINT*)(&pdata[2]));
            prt(", basev:%d", base_val);

            //((BYTE*)data)[4..num_of_case*4]:
            //    the position of the index table is at current instruction.
            if (num_of_case > 0) {
                UINT * pcase_entry = (UINT*)&pdata[4];
                prt(" tgtidx:");
                for (USHORT i = 0; i < num_of_case; i++) {
                    UINT idx_of_insn = pcase_entry[i];
                    prt("%d", idx_of_insn);
                    if (i != num_of_case - 1) {
                        prt(",");
                    }
                }
            }
        }
        break;
    case LOP_LOOKUP_SWITCH:
        {
            LIRSwitchOp * p = (LIRSwitchOp*)lir;
            ASSERT0(LIR_dt(p) == 0x2);
            prt(", v%d", p->value);
            USHORT * pdata = p->data;

            //pdata[0]: flag to indicate switch-table type:
            //    0x1 TABLE_SWITCH, 0x2 LOOKUP_SWITCH
            UINT f = pdata[0];
            ASSERT0(f == 0x200);

            //pdata[1]: the number of CASE entry.
            UINT num_of_case = pdata[1];
            if (num_of_case > 0) {
                BYTE * tp = (BYTE*)pdata;
                //((BYTE*)pdata)[4..4+num_of_case*4-1]: the case-value buffer.
                UINT * pcase_value = (UINT*)&tp[4];

                //((BYTE*)pdata)[4+num_of_case*4, 4+num_of_case*8-1]:
                //    the position of the index table is at current instruction.
                UINT * pcase_entry = (UINT*)&tp[4 + num_of_case * 4];
                prt(" val2idx(");
                for (UINT i = 0; i < num_of_case; i++) {
                    UINT idx_of_insn = pcase_entry[i];
                    prt("%d:%d", pcase_value[i], idx_of_insn);
                    if (i != num_of_case - 1) {
                        prt(",");
                    }
                }
                prt(")");
            }
        }
        break;
    case LOP_FILL_ARRAY_DATA:
        {
            prt(", %s", get_dt_name(lir));
            //AABBBBBBBB
            //pdata[0]: the magic number of code
            //0x100 PACKED_SWITCH, 0x200 SPARSE_SWITCH, 0x300 FILL_ARRAY_DATA
            LIRSwitchOp * r = (LIRSwitchOp*)lir;
            UInt16 const* pdata = (UInt16 const*)r->data;
            ASSERT0(pdata[0] == 0x300);
            //pdata[1]: size of each element.
            //pdata[2]: the number of element.
            UINT size_of_elem = pdata[1];
            UINT num_of_elem = pdata[2];
            UINT data_size = num_of_elem * size_of_elem;
            //prt(", (elem_sz<%d>), (elem_num<%d>), (data_ptr)0x%x",
            prt(", (elem_sz<%d>), (elem_num<%d>)",
                    size_of_elem, num_of_elem);
        }
        break;
    case LOP_INVOKE:
        {
            /*
            ABCCCCDDDD the layout clarifies:
                A(p4), B(argc), CCCC(method_id), DDDD(p0, p1, p2, p3)
                where p0,p1,p2,p3,p4 are five parameters.

            AABBBBCCCC the layout clarifies:
                AA(argc), BBBB(method_id), CCCC(p0,p1,...p(argc-1))
            */
            LIRInvokeOp * r = (LIRInvokeOp*)lir;
            UINT flags = LIR_dt(lir);
            UINT flag1 = flags & 0x0F;
            UINT flag2 = flags & 0xF0;
            ASSERTN(flag1 != 0, ("means LIR is LOP_FILLED_NEW_ARRAY"));
            DexMethodId const* method_id = dexGetMethodId(df, r->ref);
            ASSERT0(method_id);
            CHAR const* method_name = dexStringById(df, method_id->nameIdx);
            CHAR const* class_name =
                dexStringByTypeIdx(df, method_id->classIdx);
            ASSERT0(method_name);
            DexProtoId const* proto_id =
                dexGetProtoId(df, method_id->protoIdx);
            CHAR const* shorty_name = dexStringById(df, proto_id->shortyIdx);
            prt(", %s::%s", class_name, method_name);

            UINT k = LIR_dt(lir);
            bool is_range = HAVE_FLAG((k & 0xf0), LIR_Range);
            if (is_range) {
                switch (k & 0x0f) {
                case LIR_invoke_unknown: UNREACHABLE(); break;
                case LIR_invoke_virtual:
                    prt(", virtual-range"); break;
                case LIR_invoke_direct:
                    prt(", direct-range"); break;
                case LIR_invoke_super:
                    prt(", super-range"); break;
                case LIR_invoke_interface:
                    prt(", interface-range"); break;
                case LIR_invoke_static:
                    prt(", static-range"); break;
                default: UNREACHABLE();
                }
            } else {
                switch (k & 0x0f) {
                case LIR_invoke_unknown: UNREACHABLE(); break;
                case LIR_invoke_virtual:
                    prt(", virtual"); break;
                case LIR_invoke_direct:
                    prt(", direct"); break;
                case LIR_invoke_super:
                    prt(", super"); break;
                case LIR_invoke_interface:
                    prt(", interface"); break;
                case LIR_invoke_static:
                    prt(", static"); break;
                default: UNREACHABLE();
                }
            }

            if (r->argc != 0) {
                prt(", arg(");
                for (USHORT i = 0; i < r->argc; i++) {
                    prt("v%d", r->args[i]);
                    if (i != r->argc-1) {
                        prt(",");
                    }
                }
                prt(")");
            }
        }
        break;
    case LOP_FILLED_NEW_ARRAY:
        {
            /*
            AABBBBCCCC or ABCCCCDEFG
            e.g:
                A(argc), B,D,E,F,G(parampters), CCCC(class_tyid)
            */
            LIRInvokeOp * r = (LIRInvokeOp*)lir;
            UINT flags = LIR_dt(lir);
            CHAR const* class_name = dexStringByTypeIdx(df, r->ref);
            ASSERT0(class_name);
            prt(", %s", class_name);
            if (r->argc != 0) {
                prt(", arg(");
                for (USHORT i = 0; i < r->argc; i++) {
                    prt("v%d", r->args[i]);
                    if (i != r->argc-1) {
                        prt(",");
                    }
                }
                prt(")");
            }
        }
        break;
    case LOP_CMPG:
        //AABBCC
        ASSERT0(df);
        switch (LIR_dt(lir)) {
        case LIR_CMP_float:
            prt(", FLOAT");
            prt(", v%d, v%d, %d",
                    LIR_res(lir),
                    LIR_op0(lir),
                    (INT)LIR_op1(lir));
            break;
        case LIR_CMP_double:
            prt(", DOUBLE");
            prt(", (v%d,v%d), v%d, %d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir),
                    (INT)LIR_op1(lir));
            break;
        default: UNREACHABLE();
        }
        break;
    case LOP_PHI:
        UNREACHABLE();
        break;
    default: UNREACHABLE();
    } //end switch
    fflush(g_tfile);
}


//Dump LIR stmts stored in fu->lirList array.
void dump_all_lir(LIRCode * fu, DexFile * df, DexMethod const* dm)
{
    if (g_tfile == nullptr) { return; }
    ASSERT0(fu && df && dm);
    CHAR const* class_name = get_class_name(df, dm);
    CHAR const* func_name = get_func_name(df, dm);
    ASSERT0(class_name && func_name);
    note("\n==---- DUMP LIR of %s::%s ----== maxreg:%d ",
            class_name, func_name, fu->maxVars - 1);
    if (fu->maxVars > 0) {
        prt("(");
        for (INT i = fu->maxVars - fu->numArgs; i < (INT)fu->maxVars; i++) {
            ASSERT0(i >= 0);
            prt("v%d,", i);
        }
        prt(")");
    }
    prt(" ====");
    if (fu->triesSize != 0) {
        for (UINT i = 0; i < fu->triesSize; i++) {
            note("\ntry%d, ", i);
            LIROpcodeTry * each_try = fu->trys + i;
            prt("start_idx=%dth, end_idx=%dth",
                    each_try->start_pc, each_try->end_pc);
            for (UINT j = 0; j < each_try->catchSize; j++) {
                LIROpcodeCatch * each_catch = each_try->catches + j;
                note("\n    catch%d, kind=%d, start_idx=%dth", j,
                        each_catch->class_type,
                        each_catch->handler_pc);
            }
        }
    }
    for (INT i = 0; i < LIRC_num_of_op(fu); i++) {
        LIR * lir = LIRC_op(fu, i);
        ASSERT0(lir);
        note("\n");
        dump_lir2(lir, df, i);
    }
    fflush(g_tfile);
}


//'finfo': field info.
static void dumpf_field(DexFile * df, DexField * finfo, INT indent)
{
    if (g_tfile == nullptr) { return; }
    CHAR const* fname = get_field_name(df, finfo);
    CHAR const* tname = get_field_type_name(df, finfo);
    ASSERT0(fname && tname);
    while (indent-- >= 0) { prt(" "); }
    prt("%dth, %s, Type:%s",
            finfo->fieldIdx, fname, tname);
    fflush(g_tfile);
}


//finfo: field-info in class.
CHAR const* get_field_name(DexFile const* df, DexField * finfo)
{
    //Get field's name via nameIdx in symbol table.
    return get_field_name(df, finfo->fieldIdx);
}


//finfo: field-info in class.
CHAR const* get_field_type_name(DexFile const* df, DexField * finfo)
{
    return get_field_type_name(df, finfo->fieldIdx);
}


//finfo: field-info in class.
CHAR const* get_field_class_name(DexFile const* df, DexField * finfo)
{
    return get_field_class_name(df, finfo->fieldIdx);
}


//field_id: field number in dexfile.
CHAR const* get_field_type_name(DexFile const* df, UINT field_id)
{
    DexFieldId const* fid = dexGetFieldId(df, field_id);
    ASSERT0(fid);

    //Get field's type name.
    return dexStringByTypeIdx(df, fid->typeIdx);
}


//Return the class name which 'field_id' was placed in.
//field_id: field number in dexfile.
CHAR const* get_field_class_name(DexFile const* df, UINT field_id)
{
    DexFieldId const* fid = dexGetFieldId(df, field_id);
    ASSERT0(fid);
    return dexStringByTypeIdx(df, fid->classIdx);
}


//field_id: field number in dexfile.
CHAR const* get_field_name(DexFile const* df, UINT field_id)
{
    DexFieldId const* fid = dexGetFieldId(df, field_id);
    ASSERT0(fid);

    //Get field's name via nameIdx in symbol table.
    return dexStringById(df, fid->nameIdx);
}


//Dump all classes declared in DEX file 'df'.
void dump_all_class_and_field(DexFile * df)
{
    if (g_tfile == nullptr) { return; }
    note("\n==---- DUMP ALL CLASS and FIELD ----==");
    //Walk through each class declaration via class-index in file.
    ASSERT0(df && df->pHeader);
    for (UINT i = 0; i < df->pHeader->classDefsSize; i++) {
        note("\n");
        //Show section header.
        //dumpClassDef(pDexFile, i);

        //Dump class name.
        DexClassDef const* class_info = dexGetClassDef(df, i);
        //ASSERTN(i == class_info->classIdx, ("they might be equal"));
        //CHAR const* class_name = dexStringByTypeIdx(df, class_info->classIdx);
        //ASSERT0(class_name);
        prt("source file:%s", getClassSourceFilePath(df, class_info));
        note("\nclass %s {", get_class_name(df, class_info));

        //Dump class fields.
        BYTE const* encoded_data = dexGetClassData(df, class_info);
        if (encoded_data != nullptr) {
            DexClassData * class_data =
                dexReadAndVerifyClassData(&encoded_data, nullptr);
            if (class_data != nullptr) {
                for (UINT i = 0; i < class_data->header.instanceFieldsSize; i++) {
                    note("\n");
                    DexField * finfo = &class_data->instanceFields[i];
                    ASSERT0(finfo);
                    dumpf_field(df, finfo, 4);
                    fflush(g_tfile);
                }
            } else {
                note("\n\t--");
            }
        } else {
            note("\n\t--");
        }
        note("\n");
        prt("}");
        fflush(g_tfile);
    }
    fflush(g_tfile);
}
