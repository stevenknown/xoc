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

#include "cominc.h"
#include "comopt.h"

#include "drAlloc.h"
#include "d2d_comm.h"

#include "dex.h"
#include "gra.h"

#include "dx_mgr.h"
#include "aoc_dx_mgr.h"
#include "dex_util.h"
#include "dex2ir.h"
#include "ir2dex.h"

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
    return NULL;
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
            ASSERT0(IR_next(CALL_param_list(ir)) == NULL);
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
            p = IR_next(p);

            //The second is array element type id.
            ASSERT0(p && p->is_const());
            UINT elem_type_id = CONST_int_val(p);
            ASSERT0(IR_next(p) == NULL);
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
            ASSERT0(IR_next(p) == NULL);
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
            p = IR_next(p);

            //The second is class type id.
            ASSERT0(p && p->is_const());
            UINT type_id = CONST_int_val(p);
            ASSERT0(IR_next(p) == NULL);
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
            p = IR_next(p);

            //The second record class type id.
            ASSERT0(p && p->is_const());
            p = IR_next(p);
            while (p != NULL) {
                ASSERT0(p->is_pr());
                p = IR_next(p);
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
            p = IR_next(p);

            //The second record binary data.
            ASSERT0(p && p->is_const());
            ASSERT0(IR_next(p) == NULL);
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
            ASSERT0(IR_next(CALL_param_list(ir)) == NULL);
        }
        break;
    case BLTIN_ARRAY_LENGTH     :
        ASSERT0(ir->hasReturnValue());
        ASSERT0(CALL_param_list(ir) &&
                 CALL_param_list(ir)->is_pr());
        ASSERT0(IR_next(CALL_param_list(ir)) == NULL);
        break;
    case BLTIN_MONITOR_ENTER    :
        ASSERT0(CALL_param_list(ir) &&
                 CALL_param_list(ir)->is_pr());
        ASSERT0(IR_next(CALL_param_list(ir)) == NULL);
        break;
    case BLTIN_MONITOR_EXIT     :
        ASSERT0(CALL_param_list(ir) &&
                 CALL_param_list(ir)->is_pr());
        ASSERT0(IR_next(CALL_param_list(ir)) == NULL);
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
            p = IR_next(p);

            //The second is type-id.
            ASSERT0(p->is_const());
            ASSERT0(IR_next(p) == NULL);

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
            ASSERT0(p && p->is_int(trumgr->get_type_mgr()));
            p = IR_next(p);

            ASSERT0(p && p->is_pr());
            p = IR_next(p);

            ASSERT0(p && p->is_pr());
            ASSERT0(IR_next(p) == NULL);
            ASSERT0(ir->hasReturnValue());
        }
        break;
    default: ASSERT0(0);
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
    ASSERT(l > 0, ("unsupport string pattern"));

    CHAR const* start = functype;
    CHAR const* end = functype + l - 1;

    //The format of type string is: (Ljava/lang/String;)Ljava/lang/String;
    CHAR const* p = start;
    CHAR const* start2 = NULL;
    CHAR const* end2 = NULL;
    for (; l > 0; l--, p++) {
        if (*p == '(') {
            ASSERT(start2 == NULL, ("unsupport string pattern"));
            start2 = p;
        } else if (*p == ')') {
            end2 = p;
            UINT s = end2 - start2 - 1;
            memcpy(param_type, start2 + 1, s);
            param_type[s] = 0;
            break;
        }
    }

    ASSERT(end2, ("unsupport string pattern"));

    UINT s = end - end2;
    memcpy(return_type, end2 + 1, s);
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
    ASSERT(num == NUM_OF_PART, ("You may be input builtin function name"));

    if (classpath != NULL) {
        UINT l = strlen(ret[CLASSPATH_PART]);
        memcpy(classpath, ret[CLASSPATH_PART], l + 1);
        classpath[l] = 0;
    }

    if (functionname != NULL) {
        UINT l = strlen(ret[FUNCNAME_PART]);
        memcpy(functionname, ret[FUNCNAME_PART], l + 1);
        functionname[l] = 0;
    }

    //The format of type string is: (Ljava/lang/String;)Ljava/lang/String;
    CHAR const* typestr = ret[TYPE_PART];
    CHAR const* start = NULL;
    CHAR const* end = NULL;

    //Parse parameter type string.
    UINT len = 0;
    for (CHAR const* p = typestr; *p != 0; p++, len++) {
        if (*p == '(') {
            ASSERT(start == NULL, ("unsupport string pattern"));
            start = p;
        } else if (*p == ')') {
            end = p;
            UINT s = end - start - 1;
            if (param_type != NULL) {
                memcpy(param_type, start + 1, s);
                param_type[s] = 0;
            }
            len++;
            break;
        }
    }

    ASSERT(end, ("unsupport string pattern"));

    UINT s = strlen(typestr) - len;
    if (return_type != NULL) {
        memcpy(return_type, typestr + len, s);
        return_type[s] = 0;
    }

    for (UINT i = 0; i < num; i++) {
        free(ret[i]);
    }
}


//Print human readable type info according to type_string.
//Return the length of string.
//Note the length does not include the ending '\0'.
UINT printType2Buffer(CHAR const* type_string, OUT CHAR * buf)
{
    ASSERT0(type_string);

    //buf may be NULL. If it is the case, user want to compute the
    //actual length of the buffer.

    CHAR const* name = NULL;
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
    default: ASSERT(0, ("unknown dex type"));
    }

    if (buf != NULL) {
        for (UINT i = 0; i < len; i++) {
            buf[i] = name[i];
        }
        buf += len;
    }

    for (; *start == '['; start++) {
        len += 2; // print [ ]
        if (buf != NULL) {
            *buf++ = '[';
            *buf++ = ']';
        }
    }

    if (buf != NULL) {
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
    CHAR * outputbuf = NULL;
    if (params != NULL) {
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
            if (outputbuf != NULL) {
                memcpy(outputbuf, start, s);
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
                    if (outputbuf != NULL) {
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
                ASSERT(is_obj,
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
    if (g_tfile == NULL || lir == NULL) return;
    fprintf(g_tfile, "\n");
    dump_lir2(lir, df, pos);
}


//Dump LIR stmts stored in fu->lirList array.
void dump_lir2(LIR * lir, DexFile * df, INT pos)
{
    if (g_tfile == NULL || lir == NULL) return;
    if (pos < 0) {
        fprintf(g_tfile, "%s", LIR_name(lir));
    } else {
        fprintf(g_tfile, "(%dth)%s", pos, LIR_name(lir));
    }
    switch (LIR_opcode(lir)) {
    case LOP_NOP:
        break;
    case LOP_CONST:
        switch (LIR_dt(lir)) {
        case LIR_JDT_unknown:
            fprintf(g_tfile, ", INT");
            if (is_s4(LIR_int_imm(lir)) && LIR_res(lir) < 16) {
                //AB
                fprintf(g_tfile, ", v%d <- %d",
                        LIR_res(lir), (INT)LIR_int_imm(lir));
            } else if (is_s16(LIR_int_imm(lir))) {
                //AABBBB
                fprintf(g_tfile, ", v%d <- %d",
                        LIR_res(lir), (INT)LIR_int_imm(lir));
            } else {
                //AABBBBBBBB
                fprintf(g_tfile, ", v%d <- %d",
                        LIR_res(lir), (INT)LIR_int_imm(lir));
            }
            break;
        case LIR_JDT_wide:
            fprintf(g_tfile, ", %s", get_dt_name(lir));
            if (is_swide16(LIR_int_imm(lir))) {
                //AABBBB
                fprintf(g_tfile, ", (v%d,v%d) <- %d",
                        LIR_res(lir), LIR_res(lir) + 1, (INT)LIR_int_imm(lir));
            } else if (is_swide32(LIR_int_imm(lir))) {
                //AABBBBBBBB
                fprintf(g_tfile, ", (v%d,v%d) <- %d",
                        LIR_res(lir), LIR_res(lir) + 1, (INT)LIR_int_imm(lir));
            } else {
                //AABBBBBBBBBBBBBBBB
                fprintf(g_tfile, ", (v%d,v%d) <- %lld",
                        LIR_res(lir), LIR_res(lir) + 1, LIR_int_imm(lir));
            }
            break;
        default:
            /* It seems dex does not distinguish
            float and integer const. And regard float as
            32bit integer, double will be 64bit integer. */
            ASSERT0(0);
        }
        break;
    case LOP_RETURN:
        {
            switch (LIR_dt(lir)) {
            case LIR_JDT_unknown: //return preg.
                fprintf(g_tfile, ", INT");
                fprintf(g_tfile, ", v%d", LIR_res(lir));
                break;
            case LIR_JDT_void: //No return value.
                break;
            case LIR_JDT_object: //return object.
                fprintf(g_tfile, ", obj_ptr:v%d", LIR_res(lir));
                break;
            case LIR_JDT_wide:
                fprintf(g_tfile, ", %s", get_dt_name(lir));
                fprintf(g_tfile, ", (v%d,v%d)", LIR_res(lir), LIR_res(lir) + 1);
                break;
            default: ASSERT0(0);
            }
        }
        break;
    case LOP_THROW: //AA
        //Throws an exception object.
        //The reference of the exception object is in vx.
        fprintf(g_tfile, ", v%d", LIR_res(lir));
        break;
    case LOP_MONITOR_ENTER  :
        fprintf(g_tfile, ", v%d", LIR_res(lir));
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
                fprintf(g_tfile, ", INT");
                fprintf(g_tfile, ", retval -> v%d", LIR_res(lir));
                break;
            case LIR_JDT_wide: //lexOpcode = lc_mov_result64; break;
                fprintf(g_tfile, ", %s", get_dt_name(lir));
                fprintf(g_tfile, ", retval -> (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1);
                break;
            case LIR_JDT_object: //lexOpcode = lc_mov_result_object; break;
                fprintf(g_tfile, ", obj-ptr");
                fprintf(g_tfile, ", retval -> v%d", LIR_res(lir));
                break;
            }
        }
        break;
    case LOP_MOVE_EXCEPTION : //AA
        fprintf(g_tfile, ", v%d", LIR_res(lir));
        break;
    case LOP_GOTO            : //AABBBBBBBB
        {
            LIRGOTOOp * p = (LIRGOTOOp*)lir;
            fprintf(g_tfile, ", (lirIdx)%dth", p->target);
        }
        break;
    case LOP_MOVE        :
        switch (LIR_dt(lir)) {
        case LIR_JDT_unknown:
            fprintf(g_tfile, ", INT");
            if ((LIR_op0(lir) | LIR_res(lir)) < 16) {
                //AB
                fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else if (LIR_res(lir) < 256) {
                //AABBBB
                fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else {
                //AAAABBBB
                fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            }
            break;
        case LIR_JDT_wide:
            fprintf(g_tfile, ", %s", get_dt_name(lir));
            if ((LIR_op0(lir) | LIR_res(lir)) < 16) {
                //AB
                fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1,
                        LIR_op0(lir), LIR_op0(lir) + 1);
            } else if (LIR_res(lir) < 256) {
                //AABBBB
                fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1,
                        LIR_op0(lir), LIR_op0(lir) + 1);
            } else {
                //AAAABBBB
                fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir) + 1,
                        LIR_op0(lir), LIR_op0(lir) + 1);
            }
            break;
        case LIR_JDT_object:
            fprintf(g_tfile, ", obj-ptr");
            if ((LIR_op0(lir) | LIR_res(lir)) < 16) {
                //AB
                fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else if (LIR_res(lir) < 256) {
                //AABBBB
                fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            } else {
                //AAAABBBB
                fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            }
            break;
        }
        break;
    case LOP_NEG        : //AB
    case LOP_NOT        : //AB
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir), LIR_op0(lir)+1);
        } else {
            fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
        }
        break;
    case LOP_CONVERT    : //AB
        switch (LIR_dt(lir)) {
        case LIR_convert_i2l:
            fprintf(g_tfile, ", INT->LONG");
            fprintf(g_tfile, ", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_i2f: fprintf(g_tfile, ", INT->FLOAT");  break;
        case LIR_convert_i2d:
            fprintf(g_tfile, ", INT->DOUBLE");
            fprintf(g_tfile, ", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_l2i:
            fprintf(g_tfile, ", LONG->INT");
            fprintf(g_tfile, ", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_l2f:
            fprintf(g_tfile, ", LONG->FLOAT");
            fprintf(g_tfile, ", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_l2d:
            fprintf(g_tfile, ", LONG->DOUBLE");
            fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_f2i: fprintf(g_tfile, ", FLOAT->INT");  break;
        case LIR_convert_f2l:
            fprintf(g_tfile, ", FLOAT->LONG");
            fprintf(g_tfile, ", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_f2d:
            fprintf(g_tfile, ", FLOAT->DOUBLE");
            fprintf(g_tfile, ", (v%d,v%d) <- v%d",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
            break;
        case LIR_convert_d2i:
            fprintf(g_tfile, ", DOUBLE->INT");
            fprintf(g_tfile, ", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_d2l:
            fprintf(g_tfile, ", DOUBLE->LONG");
            fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_d2f:
            fprintf(g_tfile, ", DOUBLE->FLOAT");
            fprintf(g_tfile, ", v%d <- (v%d,v%d)",
                    LIR_res(lir), LIR_op0(lir), LIR_op0(lir)+1);
            break;
        case LIR_convert_i2b:
            fprintf(g_tfile, ", INT->BOOL");
            fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            break;
        case LIR_convert_i2c:
            fprintf(g_tfile, ", INT->CHAR");
            fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            break;
        case LIR_convert_i2s:
            fprintf(g_tfile, ", INT->SHORT");
            fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
            break;
        default:
            ASSERT0(0);
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
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d), (v%d,v%d)",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), LIR_op0(lir)+1);
        } else {
            fprintf(g_tfile, ", v%d <- v%d, v%d",
                    LIR_res(lir), LIR_res(lir), LIR_op0(lir));
        }
        break;
    case LOP_ARRAY_LENGTH: //AABBBB
        //Calculates the number of elements of the array referenced by vy
        //and puts the length value into vx.
        fprintf(g_tfile, ", v%d <- v%d", LIR_res(lir), LIR_op0(lir));
        break;
    case LOP_IFZ         :
        //AABBBB
        switch (LIR_dt(lir)) {
        case LIR_cond_EQ: fprintf(g_tfile, ", =="); break;
        case LIR_cond_NE: fprintf(g_tfile, ", !="); break;
        case LIR_cond_LT: fprintf(g_tfile, ", <"); break;
        case LIR_cond_GE: fprintf(g_tfile, ", >="); break;
        case LIR_cond_GT: fprintf(g_tfile, ", >"); break;
        case LIR_cond_LE: fprintf(g_tfile, ", <="); break;
        }
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d), 0, (lirIdx)%dth",
                    LIR_res(lir), LIR_res(lir)+1, LIR_op0(lir));
        } else {
            fprintf(g_tfile, ", v%d, 0, (lirIdx)%dth",
                    LIR_res(lir), LIR_op0(lir));
        }
        break;
    case LOP_NEW_INSTANCE:
        //AABBBB
        //LIR_op0(lir) is class-type-id, not class-declaration-id.
        ASSERT0(df);
        fprintf(g_tfile, ", (obj_ptr)v%d <- (clsname<%d>)%s",
                LIR_res(lir),
                LIR_op0(lir),
                get_class_name(df, LIR_op0(lir)));
        break;
    case LOP_CONST_STRING:
        //AABBBB or AABBBBBBBB
        ASSERT0(df);
        fprintf(g_tfile, ", v%d <- (strofst<%d>)\"%s\"",
                LIR_res(lir),
                LIR_op0(lir),
                dexStringById(df, LIR_op0(lir)));
        break;
    case LOP_CONST_CLASS :
        //AABBBB
        //const-class vx,type_id
        //Moves the class object of a class identified by
        //type_id (e.g. Object.class) into vx.
        fprintf(g_tfile, ", v%d <- (clsname<%d>)%s",
                LIR_res(lir),
                LIR_op0(lir),
                dexStringByTypeIdx(df, LIR_op0(lir)));
        break;
    case LOP_SGET        :
        //AABBBB
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        ASSERT0(df);
        fprintf(g_tfile, ", v%d <- (ofst<%d>)%s::%s",
                LIR_res(lir),
                LIR_op0(lir),
                get_field_class_name(df, LIR_op0(lir)),
                get_field_name(df, LIR_op0(lir)));
        break;
    case LOP_CHECK_CAST  :
        //AABBBB
        ASSERT0(df);
        fprintf(g_tfile, ", v%d '%s'",
                LIR_res(lir),
                dexStringByTypeIdx(df, LIR_op0(lir)));
        break;
    case LOP_SPUT        :
        {
            //AABBBB
            LIRABOp * p = (LIRABOp*)lir;
            fprintf(g_tfile, ", %s", get_dt_name(lir));
            ASSERT0(df);
            if (is_wide(lir)) {
                fprintf(g_tfile, ", (v%d,v%d) -> %s::%s",
                        LIR_res(lir), LIR_res(lir)+1,
                        get_field_class_name(df, LIR_op0(lir)),
                        get_field_name(df, LIR_op0(lir)));
            } else {
                fprintf(g_tfile, ", v%d -> %s::%s",
                        LIR_res(lir),
                        get_field_class_name(df, LIR_op0(lir)),
                        get_field_name(df, LIR_op0(lir)));
            }
        }
        break;
    case LOP_APUT        :
        //AABBCC
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        ASSERT0(df);
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d) -> (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), (UINT)LIR_op1(lir));
        } else {
            fprintf(g_tfile, ", v%d -> (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
        }
        break;
    case LOP_AGET      :
        //AABBCC
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        ASSERT0(df);
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d) <- (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), (UINT)LIR_op1(lir));
        } else {
            fprintf(g_tfile, ", v%d <- (array_base_ptr)v%d, (array_elem)v%d",
                    LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
        }
        break;
    case LOP_CMPL      :
    case LOP_CMP_LONG  :
        //AABBCC
        ASSERT0(df);
        switch (LIR_dt(lir)) {
        case LIR_CMP_float:
            fprintf(g_tfile, ", FLOAT");
            fprintf(g_tfile, ", v%d, v%d, %d",
                    LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
            break;
        case LIR_CMP_double:
            fprintf(g_tfile, ", DOUBLE");
            fprintf(g_tfile, ", (v%d,v%d), (v%d,v%d), %d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), LIR_op0(lir)+1,
                    (UINT)LIR_op1(lir));
            break;
        default: ASSERT0(0);
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
            fprintf(g_tfile, ", %s", get_dt_name(lir));
            LIRABCOp * p = (LIRABCOp*)lir;
            if (is_wide(lir)) {
                fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d), (v%d,v%d)",
                        LIR_res(lir), LIR_res(lir)+1,
                        LIR_op0(lir), LIR_op0(lir)+1,
                        (UINT)LIR_op1(lir), (UINT)LIR_op1(lir)+1);
            } else {
                fprintf(g_tfile, ", v%d <- v%d, v%d",
                        LIR_res(lir), LIR_op0(lir), (UINT)LIR_op1(lir));
            }
        }
        break;
    case LOP_IF        :
        //ABCCCC
        switch (LIR_dt(lir)) {
        case LIR_cond_EQ: fprintf(g_tfile, ", =="); break;
        case LIR_cond_NE: fprintf(g_tfile, ", !="); break;
        case LIR_cond_LT: fprintf(g_tfile, ", <"); break;
        case LIR_cond_GE: fprintf(g_tfile, ", >="); break;
        case LIR_cond_GT: fprintf(g_tfile, ", >"); break;
        case LIR_cond_LE: fprintf(g_tfile, ", <="); break;
        }
        fprintf(g_tfile, ", v%d, v%d, (lirIdx)%dth",
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
            fprintf(g_tfile, ", %s", get_dt_name(lir));
            LIRABCOp * p = (LIRABCOp*)lir;
            if (is_wide(lir)) {
                fprintf(g_tfile, ", (v%d,v%d) <- (v%d,v%d),",
                        LIR_res(lir), LIR_res(lir)+1,
                        LIR_op0(lir), LIR_op0(lir)+1);
            } else {
                fprintf(g_tfile, ", v%d <- v%d,",
                        LIR_res(lir), LIR_op0(lir));
            }

            if (is_s8((INT)LIR_op1(lir))) {
                //8bit imm
                fprintf(g_tfile, "(lit8)%d", (INT)LIR_op1(lir));
            } else if (is_s16((INT)LIR_op1(lir))) {
                //16bit imm
                fprintf(g_tfile, "(lit16)%d", (INT)LIR_op1(lir));
            } else {
                ASSERT0(0);
            }
        }
        break;
    case LOP_IPUT       :
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        //ABCCCC
        ASSERT0(df);
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d) -> (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir), (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        } else {
            fprintf(g_tfile, ", v%d -> (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir),
                    LIR_op0(lir),
                    (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        }
        break;
    case LOP_IGET       :
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        //ABCCCC
        ASSERT0(df);
        if (is_wide(lir)) {
            fprintf(g_tfile, ", (v%d,v%d) <- (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir),
                    (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        } else {
            fprintf(g_tfile, ", v%d <- (obj_ptr)v%d, (ofst<%d>)%s::%s",
                    LIR_res(lir),
                    LIR_op0(lir),
                    (UINT)LIR_op1(lir),
                    get_field_class_name(df, (UINT)LIR_op1(lir)),
                    get_field_name(df, (UINT)LIR_op1(lir)));
        }
        break;
    case LOP_INSTANCE_OF:
        fprintf(g_tfile, ", (pred)v%d <- v%d, (clsname<%d>)'%s'",
                LIR_res(lir),
                LIR_op0(lir),
                (UINT)LIR_op1(lir),
                dexStringByTypeIdx(df, (UINT)LIR_op1(lir)));
        break;
    case LOP_NEW_ARRAY  :
        //ABCCCC
        //new-array v%d(res) <- v%d(op0), LCAnimal(op1)
        fprintf(g_tfile, ", %s", get_dt_name(lir));
        //ABCCCC
        ASSERT0(df);
        fprintf(g_tfile, ", v%d <- (num_of_elem)v%d, (elem_type<%d>)'%s'",
                LIR_res(lir),
                LIR_op0(lir),
                (UINT)LIR_op1(lir),
                dexStringByTypeIdx(df, (UINT)LIR_op1(lir)));
        break;
    case LOP_TABLE_SWITCH:
        {
            LIRSwitchOp * p = (LIRSwitchOp*)lir;
            ASSERT0(LIR_dt(p) == 0x1);
            fprintf(g_tfile, ", v%d", p->value);
            USHORT * pdata = p->data;

            //data[0]: flag to indicate switch-table type:
            //    0x1 TABLE_SWITCH, 0x2 LOOKUP_SWITCH
            USHORT f = pdata[0];
            ASSERT0(f == 0x100);
            //data[1]: the number of CASE entry.
            USHORT num_of_case = pdata[1];

            //data[2..3]: the base value of case-table
            UINT base_val = *((UINT*)(&pdata[2]));
            fprintf(g_tfile, ", basev:%d", base_val);

            //((BYTE*)data)[4..num_of_case*4]:
            //    the position of the index table is at current instruction.
            if (num_of_case > 0) {
                UINT * pcase_entry = (UINT*)&pdata[4];
                fprintf(g_tfile, " tgtidx:");
                for (USHORT i = 0; i < num_of_case; i++) {
                    UINT idx_of_insn = pcase_entry[i];
                    fprintf(g_tfile, "%d", idx_of_insn);
                    if (i != num_of_case - 1) {
                        fprintf(g_tfile, ",");
                    }
                }
            }
        }
        break;
    case LOP_LOOKUP_SWITCH:
        {
            LIRSwitchOp * p = (LIRSwitchOp*)lir;
            ASSERT0(LIR_dt(p) == 0x2);
            fprintf(g_tfile, ", v%d", p->value);
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
                fprintf(g_tfile, " val2idx(");
                for (UINT i = 0; i < num_of_case; i++) {
                    UINT idx_of_insn = pcase_entry[i];
                    fprintf(g_tfile, "%d:%d", pcase_value[i], idx_of_insn);
                    if (i != num_of_case - 1) {
                        fprintf(g_tfile, ",");
                    }
                }
                fprintf(g_tfile, ")");
            }
        }
        break;
    case LOP_FILL_ARRAY_DATA:
        {
            fprintf(g_tfile, ", %s", get_dt_name(lir));
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
            //fprintf(g_tfile, ", (elem_sz<%d>), (elem_num<%d>), (data_ptr)0x%x",
            fprintf(g_tfile, ", (elem_sz<%d>), (elem_num<%d>)",
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
            ASSERT(flag1 != 0, ("means LIR is LOP_FILLED_NEW_ARRAY"));
            DexMethodId const* method_id = dexGetMethodId(df, r->ref);
            ASSERT0(method_id);
            CHAR const* method_name = dexStringById(df, method_id->nameIdx);
            CHAR const* class_name =
                dexStringByTypeIdx(df, method_id->classIdx);
            ASSERT0(method_name);
            DexProtoId const* proto_id =
                dexGetProtoId(df, method_id->protoIdx);
            CHAR const* shorty_name = dexStringById(df, proto_id->shortyIdx);
            fprintf(g_tfile, ", %s::%s", class_name, method_name);

            UINT k = LIR_dt(lir);
            bool is_range = HAVE_FLAG((k & 0xf0), LIR_Range);
            if (is_range) {
                switch (k & 0x0f) {
                case LIR_invoke_unknown: ASSERT0(0); break;
                case LIR_invoke_virtual:
                    fprintf(g_tfile, ", virtual-range"); break;
                case LIR_invoke_direct:
                    fprintf(g_tfile, ", direct-range"); break;
                case LIR_invoke_super:
                    fprintf(g_tfile, ", super-range"); break;
                case LIR_invoke_interface:
                    fprintf(g_tfile, ", interface-range"); break;
                case LIR_invoke_static:
                    fprintf(g_tfile, ", static-range"); break;
                default: ASSERT0(0);
                }
            } else {
                switch (k & 0x0f) {
                case LIR_invoke_unknown: ASSERT0(0); break;
                case LIR_invoke_virtual:
                    fprintf(g_tfile, ", virtual"); break;
                case LIR_invoke_direct:
                    fprintf(g_tfile, ", direct"); break;
                case LIR_invoke_super:
                    fprintf(g_tfile, ", super"); break;
                case LIR_invoke_interface:
                    fprintf(g_tfile, ", interface"); break;
                case LIR_invoke_static:
                    fprintf(g_tfile, ", static"); break;
                default: ASSERT0(0);
                }
            }

            if (r->argc != 0) {
                fprintf(g_tfile, ", arg(");
                for (USHORT i = 0; i < r->argc; i++) {
                    fprintf(g_tfile, "v%d", r->args[i]);
                    if (i != r->argc-1) {
                        fprintf(g_tfile, ",");
                    }
                }
                fprintf(g_tfile, ")");
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
            fprintf(g_tfile, ", %s", class_name);
            if (r->argc != 0) {
                fprintf(g_tfile, ", arg(");
                for (USHORT i = 0; i < r->argc; i++) {
                    fprintf(g_tfile, "v%d", r->args[i]);
                    if (i != r->argc-1) {
                        fprintf(g_tfile, ",");
                    }
                }
                fprintf(g_tfile, ")");
            }
        }
        break;
    case LOP_CMPG:
        //AABBCC
        ASSERT0(df);
        switch (LIR_dt(lir)) {
        case LIR_CMP_float:
            fprintf(g_tfile, ", FLOAT");
            fprintf(g_tfile, ", v%d, v%d, %d",
                    LIR_res(lir),
                    LIR_op0(lir),
                    (INT)LIR_op1(lir));
            break;
        case LIR_CMP_double:
            fprintf(g_tfile, ", DOUBLE");
            fprintf(g_tfile, ", (v%d,v%d), v%d, %d",
                    LIR_res(lir), LIR_res(lir)+1,
                    LIR_op0(lir),
                    (INT)LIR_op1(lir));
            break;
        default: ASSERT0(0);
        }
        break;
    case LOP_PHI:
        ASSERT0(0);
        break;
    default: ASSERT0(0);
    } //end switch
    fflush(g_tfile);
}


//Dump LIR stmts stored in fu->lirList array.
void dump_all_lir(LIRCode * fu, DexFile * df, DexMethod const* dm)
{
    if (g_tfile == NULL) return;
    ASSERT0(fu && df && dm);
    CHAR const* class_name = get_class_name(df, dm);
    CHAR const* func_name = get_func_name(df, dm);
    ASSERT0(class_name && func_name);
    fprintf(g_tfile, "\n==---- DUMP LIR of %s::%s ----== maxreg:%d ",
            class_name, func_name, fu->maxVars - 1);
    if (fu->maxVars > 0) {
        fprintf(g_tfile, "(");
        for (INT i = fu->maxVars - fu->numArgs; i < (INT)fu->maxVars; i++) {
            ASSERT0(i >= 0);
            fprintf(g_tfile, "v%d,", i);
        }
        fprintf(g_tfile, ")");
    }
    fprintf(g_tfile, " ====");
    if (fu->triesSize != 0) {
        for (UINT i = 0; i < fu->triesSize; i++) {
            fprintf(g_tfile, "\ntry%d, ", i);
            LIROpcodeTry * each_try = fu->trys + i;
            fprintf(g_tfile, "start_idx=%dth, end_idx=%dth",
                    each_try->start_pc, each_try->end_pc);
            for (UINT j = 0; j < each_try->catchSize; j++) {
                LIROpcodeCatch * each_catch = each_try->catches + j;
                fprintf(g_tfile, "\n    catch%d, kind=%d, start_idx=%dth", j,
                        each_catch->class_type,
                        each_catch->handler_pc);
            }
        }
    }
    for (INT i = 0; i < LIRC_num_of_op(fu); i++) {
        LIR * lir = LIRC_op(fu, i);
        ASSERT0(lir);
        fprintf(g_tfile, "\n");
        dump_lir2(lir, df, i);
    }
    fflush(g_tfile);
}


//'finfo': field info.
static void dumpf_field(DexFile * df, DexField * finfo, INT indent)
{
    if (g_tfile == NULL) { return; }
    CHAR const* fname = get_field_name(df, finfo);
    CHAR const* tname = get_field_type_name(df, finfo);
    ASSERT0(fname && tname);
    while (indent-- >= 0) { fprintf(g_tfile, " "); }
    fprintf(g_tfile, "%dth, %s, Type:%s",
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


//Return the class name which 'field_id' was place in.
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
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==---- DUMP ALL CLASS and FIELD ----==");
    //Walk through each class declaration via class-index in file.
    ASSERT0(df && df->pHeader);
    for (UINT i = 0; i < df->pHeader->classDefsSize; i++) {
        fprintf(g_tfile, "\n");
        //Show section header.
        //dumpClassDef(pDexFile, i);

        //Dump class name.
        DexClassDef const* class_info = dexGetClassDef(df, i);
        //ASSERT(i == class_info->classIdx, ("they might be equal"));
        //CHAR const* class_name = dexStringByTypeIdx(df, class_info->classIdx);
        //ASSERT0(class_name);
        fprintf(g_tfile, "source file:%s", getClassSourceFilePath(df, class_info));
        fprintf(g_tfile, "\nclass %s {", get_class_name(df, class_info));

        //Dump class fields.
        BYTE const* encoded_data = dexGetClassData(df, class_info);
        if (encoded_data != NULL) {
            DexClassData * class_data =
                dexReadAndVerifyClassData(&encoded_data, NULL);
            if (class_data != NULL) {
                for (UINT i = 0; i < class_data->header.instanceFieldsSize; i++) {
                    fprintf(g_tfile, "\n");
                    DexField * finfo = &class_data->instanceFields[i];
                    ASSERT0(finfo);
                    dumpf_field(df, finfo, 4);
                    fflush(g_tfile);
                }
            } else {
                fprintf(g_tfile, "\n\t--");
            }
        } else {
            fprintf(g_tfile, "\n\t--");
        }
        fprintf(g_tfile, "\n");
        fprintf(g_tfile, "}");
        fflush(g_tfile);
    }
    fflush(g_tfile);
}


/*
---dex file map-------
    header!
    string id!
    type id!
    proto id!
    field id!
    method id!
    class def id!

    annotation set ref list
    annotation set item list !

    code item !

    annotations directory item!
    type list info!
    string data item list !
    debug info!
    annotation item list !
    encodearray item list !

    class Data item !
    map list!
*/

//
//START DEX_CP
//
class DEX_CP : public IR_CP {
public:
    DEX_CP(Region * ru) : IR_CP(ru) {}
    virtual ~DEX_CP() {}

    //Check if ir is appropriate for propagation.
    virtual bool canBeCandidate(IR const* ir) const
    {
        //Prop const imm may generate code which is not legal dex format.
        //TODO: Perform more code normalization before ir2dex.
        //return ir->is_const() || ir->is_pr();

        return ir->is_pr();
    }
};
//END DEX_CP


class DEX_RP : public IR_RP {
    bool m_has_insert_stuff;
public:
    DEX_RP(Region * ru, IR_GVN * gvn) : IR_RP(ru, gvn)
    { m_has_insert_stuff = false; }
    virtual ~DEX_RP() {}

    /*
    virtual bool is_promotable(IR const* ir) const
    {
        if (ir->is_array()) {
            IR * sub = ARR_sub_list(ir);
            ASSERT0(sub);
            if (cnt_list(sub) == 1) {
                if (sub->is_pr() && PR_no(sub) == 2) {
                    return false;
                }
            }
        }
        return IR_RP::is_promotable(ir);
    }

    void insert_stuff_code(IR const* ref, Region * ru, IR_GVN * gvn)
    {
        ASSERT0(IR_type(ref) == IR_ARRAY);

        IR * stmt = ref->get_stmt();
        ASSERT0(stmt);
        IRBB * stmt_bb = stmt->get_bb();
        ASSERT0(stmt_bb);

        IR_DU_MGR * dumgr = ru->get_du_mgr();

        C<IR*> * ct = NULL;
        BB_irlist(stmt_bb).find(stmt, &ct);
        ASSERT0(ct != NULL);

        //Insert stuff code as you need. It will slow down the benchmark.
        UINT num_want_to_insert = 30;
        for (UINT i = 0; i < num_want_to_insert; i++) {
            IR * newref = ru->dupIRTree(ref);
            dumgr->copyIRTreeDU(newref, ref, true);
            IR * stpr = ru->buildStorePR(ru->buildPrno(IR_dt(newref)),
                                            IR_dt(newref), newref);
            ru->allocRefForPR(stpr);
            IR_may_throw(stpr) = true;

            //New IR has same VN with original one.
            gvn->set_mapIR2VN(stpr, gvn->mapIR2VN(ref));

            BB_irlist(stmt_bb).insert_before(stpr, ct);
        }
    }

    virtual void handleAccessInBody(IR * ref, IR * delegate, IR * delegate_pr,
                                    TMap<IR*, SList<IR*>*> &
                                            delegate2has_outside_uses_ir_list,
                                    TTab<IR*> & restore2mem,
                                    List<IR*> & fixup_list,
                                    TMap<IR*, IR*> & delegate2stpr,
                                    LI<IRBB> const* li,
                                    IRIter & ii)
    {
        if (!m_has_insert_stuff &&
            IR_type(ref) == IR_ARRAY &&
            (m_ru->isRegionName(
                "Lsoftweg/hw/performance/CPUTest;::arrayElementsDouble") ||
             m_ru->isRegionName(
                 "Lsoftweg/hw/performance/CPUTest;::arrayElementsSingle"))) {
            m_has_insert_stuff = true;
            insert_stuff_code(ref, m_ru, m_gvn);
        }
        IR_RP::handleAccessInBody(ref, delegate, delegate_pr,
                                delegate2has_outside_uses_ir_list,
                                restore2mem, fixup_list,
                                delegate2stpr, li, ii);
    }
    */
};

//
//START AocDxMgr
//
CHAR const* AocDxMgr::get_string(UINT str_idx)
{
    return dexStringById(m_df, str_idx);
}


CHAR const* AocDxMgr::get_type_name(UINT idx)
{
    return dexStringByTypeIdx(m_df, idx);
}


//field_id: field number in dexfile.
CHAR const* AocDxMgr::get_field_name(UINT field_idx)
{
    DexFieldId const* fid = dexGetFieldId(m_df, field_idx);
    ASSERT0(fid);

    //Get field's name via nameIdx in symbol table.
    return dexStringById(m_df, fid->nameIdx);
}


CHAR const* AocDxMgr::get_method_name(UINT method_idx)
{
    DexMethodId const* method_id = dexGetMethodId(m_df, method_idx);
    ASSERT0(method_id);
    CHAR const* method_name = dexStringById(m_df, method_id->nameIdx);
    ASSERT0(method_name);
    return method_name;
}


CHAR const* AocDxMgr::get_class_name(UINT class_type_idx)
{
    return dexStringByTypeIdx(m_df, class_type_idx);
}


CHAR const* AocDxMgr::get_class_name_by_method_id(UINT method_idx)
{
    DexMethodId const* method_id = dexGetMethodId(m_df, method_idx);
    ASSERT0(method_id);
    CHAR const* class_name = dexStringByTypeIdx(m_df, method_id->classIdx);
    return class_name;
}


CHAR const* AocDxMgr::get_class_name_by_field_id(UINT field_idx)
{
    DexFieldId const* fid = dexGetFieldId(m_df, field_idx);
    ASSERT0(fid);
    return dexStringByTypeIdx(m_df, fid->classIdx);
}


CHAR const* AocDxMgr::get_class_name_by_declaration_id(UINT cls_def_idx)
{
    DexClassDef const* class_info = dexGetClassDef(m_df, cls_def_idx);
    CHAR const* class_name = dexStringByTypeIdx(m_df, class_info->classIdx);
    ASSERT0(class_name);
    return class_name;
}
//END AocDxMgr


//
//START DexPassMgr
//
Pass * DexPassMgr::allocDCE()
{
    IR_DCE * pass = new IR_DCE(m_ru);
    pass->set_elim_cfs(true);
    return pass;
}


Pass * DexPassMgr::allocCopyProp()
{
    Pass * pass = new DEX_CP(m_ru);
    SimpCTX simp;
    pass->set_simp_cont(&simp);
    return pass;
}


Pass * DexPassMgr::allocRP()
{
    Pass * pass = new DEX_RP(m_ru, (IR_GVN*)registerPass(PASS_GVN));
    return pass;
}


int xtime = 1;
void DexPassMgr::performScalarOpt(OptCTX & oc)
{
    List<Pass*> passlist; //A list of optimization.
    IR_SSA_MGR * ssamgr = (IR_SSA_MGR*)registerPass(PASS_SSA_MGR);
    bool is_ssa_avail = false;
    if (ssamgr != NULL) {
        is_ssa_avail = ssamgr->is_ssa_constructed();
    }

    passlist.append_tail(registerPass(PASS_CP));
    passlist.append_tail(registerPass(PASS_DCE));
    passlist.append_tail(registerPass(PASS_RP));
    passlist.append_tail(registerPass(PASS_CP));
    passlist.append_tail(registerPass(PASS_DCE));
    passlist.append_tail(registerPass(PASS_RP));
    passlist.append_tail(registerPass(PASS_CP));
    passlist.append_tail(registerPass(PASS_DCE));
    passlist.append_tail(registerPass(PASS_LOOP_CVT));
    passlist.append_tail(registerPass(PASS_LICM));
    passlist.append_tail(registerPass(PASS_GCSE));

    ((IR_DCE*)registerPass(PASS_DCE))->set_elim_cfs(false);

    if (passlist.get_elem_count() != 0) {
        LOG("\tScalar optimizations for '%s'", m_ru->get_ru_name());
    }

    bool change;
    UINT count = 0;
    //do {
        change = false;
        for (Pass * pass = passlist.get_head();
             pass != NULL; pass = passlist.get_next()) {
            CHAR const* passname = pass->get_pass_name();
            LOG("\t\tpass %s", passname);
            ASSERT0(verifyIRandBB(m_ru->get_bb_list(), m_ru));
            ULONGLONG t = getusec();

            //dumpBBList(m_ru->get_bb_list(), m_ru, "before");
            //m_ru->get_cfg()->dump_vcg("before.vcg");

            bool doit = pass->perform(oc);

            //dumpBBList(m_ru->get_bb_list(), m_ru, "after");
            //m_ru->get_cfg()->dump_vcg("after.vcg");

            appendTimeInfo(pass->get_pass_name(), getusec() - t);
            if (doit) {
                LOG("\t\t\tchanged");
                change = true;
                ASSERT0(verifyIRandBB(m_ru->get_bb_list(), m_ru));
                ASSERT0(m_ru->get_cfg()->verify());
            }
        }
        count++;
    //} while (change && count < 20);
    //ASSERT0(!change);
}
//END DexPassMgr


//
//START DEX_AA
//
//Mapping from TYID to MD.
//tyid start from 1.
typedef TMap<Type const*, MD const*> Type2MD;

class DEX_AA : public IR_AA {
    Type2MD m_type2md;
public:
    DEX_AA(Region * ru) : IR_AA(ru) {}

    //Attemp to compute POINT-TO set via data type.
    virtual MD const* computePointToViaType(IR const* ir)
    {
        AttachInfo * ai = IR_ai(ir);
        ASSERT0(ir && ai);

        TbaaAttachInfo * ty = (TbaaAttachInfo*)ai->get(AI_TBAA);
        if (ty == NULL) { return NULL; }

        ASSERT(ty->type, ("Type should not be given if "
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
        VAR_allocable(dummy) = false;
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
        AttachInfo * ai = IR_ai(ld);
        if (ai == NULL) { return; }

        TbaaAttachInfo * ty = (TbaaAttachInfo*)ai->get(AI_TBAA);
        if (ty == NULL) { return; }

        MD const* md = m_type2md.get(ty->type);
        if (md != NULL) {
            MD const* t = allocLoadMD(ld);
            setPointToMDSetByAddMD(t, *mx, md);
            return;
        }

        CHAR buf[64];
        sprintf(buf, "dummy%d", (UINT)(size_t)ty->type);
        VAR * dummy = m_var_mgr->registerVar(
                    buf, m_dm->getMCType(0), 1, VAR_GLOBAL);
        VAR_is_addr_taken(dummy) = true;
        VAR_allocable(dummy) = false;
        m_ru->addToVarTab(dummy);

        MD dummy_md;
        MD_base(&dummy_md) = dummy;
        MD_size(&dummy_md) = 0;
        MD_ty(&dummy_md) = MD_UNBOUND;
        MD const* entry = m_md_sys->registerMD(dummy_md);
        m_type2md.set(ty->type, entry);

        MD const* t = allocLoadMD(ld);
        setPointToMDSetByAddMD(t, *mx, entry);
    }
};
//END DEX_AA


//
static CHAR const* g_unimportant_func[] = {
    "Ljava/lang/StringBuilder;:()V:<init>",
};
static UINT g_unimportant_func_num =
    sizeof(g_unimportant_func) / sizeof(g_unimportant_func[0]);


DexCallGraph::DexCallGraph(UINT edge_hash, UINT vex_hash, RegionMgr * rumgr) :
        CallGraph(edge_hash, vex_hash, rumgr)
{
    ASSERT0(rumgr);
    SymTab * symtab = rumgr->get_sym_tab();
    for (UINT i = 0; i < g_unimportant_func_num; i++) {
        SYM const* sym = symtab->add(g_unimportant_func[i]);
        m_unimportant_symtab.append(sym);
    }
}


bool DexCallGraph::is_unimportant(SYM const* sym) const
{
    return m_unimportant_symtab.find(sym);
}
//END DexCallGraph


//
//START DexRegionMgr
//

//Add a variable of CLASS.
VAR * DexRegionMgr::addVarForBuiltin(CHAR const* name)
{
    SYM * sym = addToSymbolTab(name);
    UINT flag = 0;
    SET_FLAG(flag, VAR_GLOBAL);
    return get_var_mgr()->registerVar(sym, get_type_mgr()->getVoid(), 0, flag);
}


void DexRegionMgr::initBuiltin()
{
    for (UINT i = BLTIN_UNDEF + 1; i < BLTIN_LAST; i++) {
        VAR * v = addVarForBuiltin(BLTIN_name((BLTIN_TYPE)i));
        m_var2blt.set(v, i);
        m_blt2var.set(i, v);
    }
}


void DexRegionMgr::processProgramRegion(Region * program)
{
    ASSERT0(program && program->is_program());

    //Function region has been handled. And call list should be available.
    CallGraph * callg = initCallGraph(program, false);
    //callg->dump_vcg();
}
//START DexRegion
//
PassMgr * DexRegion::allocPassMgr()
{
    return new DexPassMgr(this);
}


//Initialize alias analysis.
IR_AA * DexRegion::allocAliasAnalysis()
{
    return new DEX_AA(this);
}


bool DexRegion::MiddleProcess(OptCTX & oc)
{
    return Region::MiddleProcess(oc);
}


void DexRegion::HighProcessImpl(OptCTX & oc)
{
    PassMgr * passmgr = get_pass_mgr();
    ASSERT0(passmgr);

    if (g_do_cfg) {
        ASSERT0(g_cst_bb_list);
        IR_CFG * cfg = (IR_CFG*)passmgr->registerPass(PASS_CFG);
        ASSERT0(cfg);
        cfg->initCfg(oc);
        if (g_do_loop_ana) {
            ASSERT0(g_do_cfg_dom);
            cfg->LoopAnalysis(oc);
        }
    }

    if (g_do_ssa) {
        IR_SSA_MGR * ssamgr = (IR_SSA_MGR*)passmgr->registerPass(PASS_SSA_MGR);
        ASSERT0(ssamgr);
        ssamgr->construction(oc);
    }

    if (g_do_aa) {
        ASSERT0(g_cst_bb_list && OC_is_cfg_valid(oc));
        IR_AA * aa = (IR_AA*)passmgr->registerPass(PASS_AA);
        ASSERT0(aa);
        aa->initAliasAnalysis();
        aa->perform(oc);
    }

    if (g_do_du_ana) {
        ASSERT0(g_cst_bb_list && OC_is_cfg_valid(oc) && OC_is_aa_valid(oc));
        IR_DU_MGR * dumgr = (IR_DU_MGR*)passmgr->registerPass(PASS_DU_MGR);
        ASSERT0(dumgr);

        UINT f = SOL_REACH_DEF|SOL_REF;
        //f |= SOL_AVAIL_REACH_DEF|SOL_AVAIL_EXPR|SOL_RU_REF;

        if (g_do_ivr) {
            f |= SOL_AVAIL_REACH_DEF|SOL_AVAIL_EXPR;
        }

        if (g_do_compute_available_exp) {
            f |= SOL_AVAIL_EXPR;
        }

        dumgr->perform(oc, f);
        dumgr->computeMDDUChain(oc);
}
}


bool DexRegion::HighProcess(OptCTX & oc)
{
    CHAR const* ru_name = get_ru_name();
    g_indent = 0;
    SimpCTX simp;
    SIMP_if(&simp) = 1;
    SIMP_do_loop(&simp) = 1;
    SIMP_do_while(&simp) = 1;
    SIMP_while_do(&simp) = 1;
    SIMP_switch(&simp) = 0;
    SIMP_break(&simp) = 1;
    SIMP_continue(&simp) = 1;

    REGION_analysis_instrument(this)->m_ir_list =
                    simplifyStmtList(get_ir_list(), &simp);

    ASSERT0(verify_simp(get_ir_list(), simp));
    ASSERT0(verify_irs(get_ir_list(), NULL, this));

    constructIRBBlist();

    ASSERT0(verifyIRandBB(get_bb_list(), this));

    //All IRs have been moved to each IRBB.
    REGION_analysis_instrument(this)->m_ir_list = NULL;

    HighProcessImpl(oc);
    return true;
}


//All global prs must be mapped.
bool DexRegion::verifyRAresult(RA & ra, Prno2Vreg & prno2v)
{
    GltMgr * gltm = ra.get_gltm();
    Vector<GLT*> * gltv = gltm->get_gltvec();
    for (UINT i = 0; i < gltm->get_num_of_glt(); i++) {
        GLT * g = gltv->get(i);
        if (g == NULL) { continue; }
        ASSERT0(g->has_allocated());
        if (GLT_bbs(g) == NULL) {
            //parameter may be have no occ.
            continue;
        }
        bool find;
        prno2v.get(GLT_prno(g), &find);
        ASSERT0(find);
    }

    BBList * bbl = get_bb_list();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        LTMgr * ltm = gltm->map_bb2ltm(bb);
        if (ltm == NULL) { continue; }
        Vector<LT*> * lvec = ltm->get_lt_vec();
        for (INT i = 0; i <= lvec->get_last_idx(); i++) {
            LT * l = lvec->get(i);
            if (l == NULL) { continue; }
            ASSERT0(l->has_allocated());
            bool find;
            prno2v.get(LT_prno(l), &find);
            ASSERT0(find);
        }
    }
    return true;
}


void DexRegion::updateRAresult(IN RA & ra, OUT Prno2Vreg & prno2v)
{
    prno2v.maxreg = ra.get_maxreg();
    prno2v.paramnum = ra.get_paramnum();
    GltMgr * gltm = ra.get_gltm();
    BBList * bbl = get_bb_list();
    prno2v.clean();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        LTMgr * ltm = gltm->map_bb2ltm(bb);
        if (ltm == NULL) { continue; }
        Vector<LT*> * lvec = ltm->get_lt_vec();
        for (INT i = 0; i <= lvec->get_last_idx(); i++) {
            LT * l = lvec->get(i);
            if (l == NULL) { continue; }
            ASSERT0(l->has_allocated());
            bool find;
            UINT v = prno2v.get(LT_prno(l), &find);
            if (find) {
                //each prno is corresponding to a unqiue vreg.
                ASSERT0(v == LT_phy(l));
            } else {
                prno2v.set(LT_prno(l), LT_phy(l));
            }
        }
    }
    //prno2v.dump();
    ASSERT0(verifyRAresult(ra, prno2v));
}


void DexRegion::processSimply()
{
    LOG("DexRegion::processSimply %s", get_ru_name());
    if (get_ir_list() == NULL) { return ; }
    OptCTX oc;
    OC_show_comp_time(oc) = g_show_comp_time;

    CHAR const* ru_name = get_ru_name();

    constructIRBBlist();

    ASSERT0(verifyIRandBB(get_bb_list(), this));

    REGION_analysis_instrument(this)->m_ir_list = NULL; //All IRs have been moved to each IRBB.

    PassMgr * passmgr = initPassMgr();
    ASSERT0(passmgr);

    ASSERT0(g_cst_bb_list);
    IR_CFG * cfg = (IR_CFG*)passmgr->registerPass(PASS_CFG);
    ASSERT0(cfg);
    cfg->initCfg(oc);
    ASSERT0(g_do_cfg_dom);
    cfg->LoopAnalysis(oc);

    destroyPassMgr();

    //Do not allocate register.
    getPrno2Vreg()->clean();
    getPrno2Vreg()->copy(*getDex2IR()->getPR2Vreg());
    return;
}


//This function outputs Prno2Vreg after Dex register allocation.
void DexRegion::process()
{
    if (get_ir_list() == NULL) { return; }
    OptCTX oc;
    OC_show_comp_time(oc) = g_show_comp_time;

    g_indent = 0;
    LOG("DexRegion process %s", get_ru_name());
    //note("\n==---- REGION_NAME:%s ----==", get_ru_name());
    prescan(get_ir_list());

    REGION_is_pr_unique_for_same_number(this) = true;

    PassMgr * passmgr = initPassMgr();

    HighProcess(oc);
    MiddleProcess(oc);

    ASSERT0(get_pass_mgr());
    IR_SSA_MGR * ssamgr = (IR_SSA_MGR*)passmgr->query_opt(PASS_SSA_MGR);
    if (ssamgr != NULL && ssamgr->is_ssa_constructed()) {
        ssamgr->destructionInBBListOrder();
    }

    //Destroy PassMgr.
    destroyPassMgr();

    if (!is_function()) { return; }

    BBList * bbl = get_bb_list();
    if (bbl->get_elem_count() == 0) { return; }

    ASSERT0(verifyIRandBB(bbl, this));

    RefineCTX rf;
    RC_insert_cvt(rf) = false; //Do not insert cvt for DEX code.
    refineBBlist(bbl, rf);
    ASSERT0(verifyIRandBB(bbl, this));

    if (g_do_dex_ra) {
        Prno2Vreg * original_prno2vreg = getDex2IR()->getPR2Vreg();
        RA ra(this,
              getTypeIndexRep(),
              getParamNum(),
              getOrgVregNum(),
              getDex2IR()->getVreg2PR(),
              original_prno2vreg,
              &m_var2pr);
        LOG("\t\tdo DEX Register Allcation for '%s'", get_ru_name());
        ra.perform(oc);
        updateRAresult(ra, *getPrno2Vreg());
    } else {
        //Do not allocate register.
        getPrno2Vreg()->clean();
        getPrno2Vreg()->copy(*getDex2IR()->getPR2Vreg());
    }
}
//END DexRegion
