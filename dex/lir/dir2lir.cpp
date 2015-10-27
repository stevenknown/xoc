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

author: GongKai, JinYue
@*/
#include "liropcode.h"
#include "dir.h"

//GENERATED AUTOMATICALLY BY lir-gen
#define DECLARE_DIR2LIR\
    DIR2LIR(OP_NOP                        ,LOP_NOP                       ,0                             )\
    DIR2LIR(OP_MOVE                       ,LOP_MOVE                      ,0                             )\
    DIR2LIR(OP_MOVE_FROM16                ,LOP_MOVE                      ,0                             )\
    DIR2LIR(OP_MOVE_16                    ,LOP_MOVE                      ,0                             )\
    DIR2LIR(OP_MOVE_WIDE                  ,LOP_MOVE                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_MOVE_WIDE_FROM16           ,LOP_MOVE                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_MOVE_WIDE_16               ,LOP_MOVE                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_MOVE_OBJECT                ,LOP_MOVE                      ,LIR_JDT_object                )\
    DIR2LIR(OP_MOVE_OBJECT_FROM16         ,LOP_MOVE                      ,LIR_JDT_object                )\
    DIR2LIR(OP_MOVE_OBJECT_16             ,LOP_MOVE                      ,LIR_JDT_object                )\
    DIR2LIR(OP_MOVE_RESULT                ,LOP_MOVE_RESULT               ,0                             )\
    DIR2LIR(OP_MOVE_RESULT_WIDE           ,LOP_MOVE_RESULT               ,LIR_JDT_wide                  )\
    DIR2LIR(OP_MOVE_RESULT_OBJECT         ,LOP_MOVE_RESULT               ,LIR_JDT_object                )\
    DIR2LIR(OP_MOVE_EXCEPTION             ,LOP_MOVE_EXCEPTION            ,LIR_JDT_object                )\
    DIR2LIR(OP_RETURN_VOID                ,LOP_RETURN                    ,LIR_JDT_void                  )\
    DIR2LIR(OP_RETURN                     ,LOP_RETURN                    ,0                             )\
    DIR2LIR(OP_RETURN_WIDE                ,LOP_RETURN                    ,LIR_JDT_wide                  )\
    DIR2LIR(OP_RETURN_OBJECT              ,LOP_RETURN                    ,LIR_JDT_object                )\
    DIR2LIR(OP_CONST_4                    ,LOP_CONST                     ,0                             )\
    DIR2LIR(OP_CONST_16                   ,LOP_CONST                     ,0                             )\
    DIR2LIR(OP_CONST                      ,LOP_CONST                     ,0                             )\
    DIR2LIR(OP_CONST_HIGH16               ,LOP_CONST                     ,0                              )\
    DIR2LIR(OP_CONST_WIDE_16              ,LOP_CONST                     ,LIR_JDT_wide                  )\
    DIR2LIR(OP_CONST_WIDE_32              ,LOP_CONST                     ,LIR_JDT_wide                  )\
    DIR2LIR(OP_CONST_WIDE                 ,LOP_CONST                     ,LIR_JDT_wide                  )\
    DIR2LIR(OP_CONST_WIDE_HIGH16          ,LOP_CONST                     ,LIR_JDT_wide                  )\
    DIR2LIR(OP_CONST_STRING               ,LOP_CONST_STRING              ,0                             )\
    DIR2LIR(OP_CONST_STRING_JUMBO         ,LOP_CONST_STRING              ,0                             )\
    DIR2LIR(OP_CONST_CLASS                ,LOP_CONST_CLASS               ,0                             )\
    DIR2LIR(OP_MONITOR_ENTER              ,LOP_MONITOR_ENTER             ,0                             )\
    DIR2LIR(OP_MONITOR_EXIT               ,LOP_MONITOR_EXIT              ,0                             )\
    DIR2LIR(OP_CHECK_CAST                 ,LOP_CHECK_CAST                ,0                             )\
    DIR2LIR(OP_INSTANCE_OF                ,LOP_INSTANCE_OF               ,0                             )\
    DIR2LIR(OP_ARRAY_LENGTH               ,LOP_ARRAY_LENGTH              ,0                             )\
    DIR2LIR(OP_NEW_INSTANCE               ,LOP_NEW_INSTANCE              ,0                             )\
    DIR2LIR(OP_NEW_ARRAY                  ,LOP_NEW_ARRAY                 ,0                             )\
    DIR2LIR(OP_FILLED_NEW_ARRAY           ,LOP_FILLED_NEW_ARRAY          ,0                             )\
    DIR2LIR(OP_FILLED_NEW_ARRAY_RANGE     ,LOP_FILLED_NEW_ARRAY          ,LIR_Range                     )\
    DIR2LIR(OP_FILL_ARRAY_DATA            ,LOP_FILL_ARRAY_DATA           ,0                             )\
    DIR2LIR(OP_THROW                      ,LOP_THROW                     ,0                             )\
    DIR2LIR(OP_GOTO                       ,LOP_GOTO                      ,0                             )\
    DIR2LIR(OP_GOTO_16                    ,LOP_GOTO                      ,0                             )\
    DIR2LIR(OP_GOTO_32                    ,LOP_GOTO                      ,0                             )\
    DIR2LIR(OP_PACKED_SWITCH              ,LOP_TABLE_SWITCH              ,LIR_switch_table              )\
    DIR2LIR(OP_SPARSE_SWITCH              ,LOP_LOOKUP_SWITCH             ,LIR_switch_lookup             )\
    DIR2LIR(OP_CMPL_FLOAT                 ,LOP_CMPL                      ,LIR_CMP_float                 )\
    DIR2LIR(OP_CMPG_FLOAT                 ,LOP_CMPG                      ,LIR_CMP_float                 )\
    DIR2LIR(OP_CMPL_DOUBLE                ,LOP_CMPL                      ,LIR_CMP_double                )\
    DIR2LIR(OP_CMPG_DOUBLE                ,LOP_CMPG                      ,LIR_CMP_double                )\
    DIR2LIR(OP_CMP_LONG                   ,LOP_CMP_LONG                  ,0                             )\
    DIR2LIR(OP_IF_EQ                      ,LOP_IF                        ,LIR_cond_EQ                   )\
    DIR2LIR(OP_IF_NE                      ,LOP_IF                        ,LIR_cond_NE                   )\
    DIR2LIR(OP_IF_LT                      ,LOP_IF                        ,LIR_cond_LT                   )\
    DIR2LIR(OP_IF_GE                      ,LOP_IF                        ,LIR_cond_GE                   )\
    DIR2LIR(OP_IF_GT                      ,LOP_IF                        ,LIR_cond_GT                   )\
    DIR2LIR(OP_IF_LE                      ,LOP_IF                        ,LIR_cond_LE                   )\
    DIR2LIR(OP_IF_EQZ                     ,LOP_IFZ                       ,LIR_cond_EQ                   )\
    DIR2LIR(OP_IF_NEZ                     ,LOP_IFZ                       ,LIR_cond_NE                   )\
    DIR2LIR(OP_IF_LTZ                     ,LOP_IFZ                       ,LIR_cond_LT                   )\
    DIR2LIR(OP_IF_GEZ                     ,LOP_IFZ                       ,LIR_cond_GE                   )\
    DIR2LIR(OP_IF_GTZ                     ,LOP_IFZ                       ,LIR_cond_GT                   )\
    DIR2LIR(OP_IF_LEZ                     ,LOP_IFZ                       ,LIR_cond_LE                   )\
    DIR2LIR(OP_UNUSED_3E                  ,0                             ,0                             )\
    DIR2LIR(OP_UNUSED_3F                  ,0                             ,0                             )\
    DIR2LIR(OP_UNUSED_4                   ,0                             ,0                             )\
    DIR2LIR(OP_UNUSED_41                  ,0                             ,0                             )\
    DIR2LIR(OP_UNUSED_42                  ,0                             ,0                             )\
    DIR2LIR(OP_UNUSED_43                  ,0                             ,0                             )\
    DIR2LIR(OP_AGET                       ,LOP_AGET                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_AGET_WIDE                  ,LOP_AGET                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_AGET_OBJECT                ,LOP_AGET                      ,LIR_JDT_object                )\
    DIR2LIR(OP_AGET_BOOLEAN               ,LOP_AGET                      ,LIR_JDT_boolean               )\
    DIR2LIR(OP_AGET_BYTE                  ,LOP_AGET                      ,LIR_JDT_byte                  )\
    DIR2LIR(OP_AGET_CHAR                  ,LOP_AGET                      ,LIR_JDT_char                  )\
    DIR2LIR(OP_AGET_SHORT                 ,LOP_AGET                      ,LIR_JDT_short                 )\
    DIR2LIR(OP_APUT                       ,LOP_APUT                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_APUT_WIDE                  ,LOP_APUT                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_APUT_OBJECT                ,LOP_APUT                      ,LIR_JDT_object                )\
    DIR2LIR(OP_APUT_BOOLEAN               ,LOP_APUT                      ,LIR_JDT_boolean               )\
    DIR2LIR(OP_APUT_BYTE                  ,LOP_APUT                      ,LIR_JDT_byte                  )\
    DIR2LIR(OP_APUT_CHAR                  ,LOP_APUT                      ,LIR_JDT_char                  )\
    DIR2LIR(OP_APUT_SHORT                 ,LOP_APUT                      ,LIR_JDT_short                 )\
    DIR2LIR(OP_IGET                       ,LOP_IGET                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_IGET_WIDE                  ,LOP_IGET                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_IGET_OBJECT                ,LOP_IGET                      ,LIR_JDT_object                )\
    DIR2LIR(OP_IGET_BOOLEAN               ,LOP_IGET                      ,LIR_JDT_boolean               )\
    DIR2LIR(OP_IGET_BYTE                  ,LOP_IGET                      ,LIR_JDT_byte                  )\
    DIR2LIR(OP_IGET_CHAR                  ,LOP_IGET                      ,LIR_JDT_char                  )\
    DIR2LIR(OP_IGET_SHORT                 ,LOP_IGET                      ,LIR_JDT_short                 )\
    DIR2LIR(OP_IPUT                       ,LOP_IPUT                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_IPUT_WIDE                  ,LOP_IPUT                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_IPUT_OBJECT                ,LOP_IPUT                      ,LIR_JDT_object                )\
    DIR2LIR(OP_IPUT_BOOLEAN               ,LOP_IPUT                      ,LIR_JDT_boolean               )\
    DIR2LIR(OP_IPUT_BYTE                  ,LOP_IPUT                      ,LIR_JDT_byte                  )\
    DIR2LIR(OP_IPUT_CHAR                  ,LOP_IPUT                      ,LIR_JDT_char                  )\
    DIR2LIR(OP_IPUT_SHORT                 ,LOP_IPUT                      ,LIR_JDT_short                 )\
    DIR2LIR(OP_SGET                       ,LOP_SGET                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_SGET_WIDE                  ,LOP_SGET                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_SGET_OBJECT                ,LOP_SGET                      ,LIR_JDT_object                )\
    DIR2LIR(OP_SGET_BOOLEAN               ,LOP_SGET                      ,LIR_JDT_boolean               )\
    DIR2LIR(OP_SGET_BYTE                  ,LOP_SGET                      ,LIR_JDT_byte                  )\
    DIR2LIR(OP_SGET_CHAR                  ,LOP_SGET                      ,LIR_JDT_char                  )\
    DIR2LIR(OP_SGET_SHORT                 ,LOP_SGET                      ,LIR_JDT_short                 )\
    DIR2LIR(OP_SPUT                       ,LOP_SPUT                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_SPUT_WIDE                  ,LOP_SPUT                      ,LIR_JDT_wide                  )\
    DIR2LIR(OP_SPUT_OBJECT                ,LOP_SPUT                      ,LIR_JDT_object                )\
    DIR2LIR(OP_SPUT_BOOLEAN               ,LOP_SPUT                      ,LIR_JDT_boolean               )\
    DIR2LIR(OP_SPUT_BYTE                  ,LOP_SPUT                      ,LIR_JDT_byte                  )\
    DIR2LIR(OP_SPUT_CHAR                  ,LOP_SPUT                      ,LIR_JDT_char                  )\
    DIR2LIR(OP_SPUT_SHORT                 ,LOP_SPUT                      ,LIR_JDT_short                 )\
    DIR2LIR(OP_INVOKE_VIRTUAL             ,LOP_INVOKE                    ,LIR_invoke_virtual            )\
    DIR2LIR(OP_INVOKE_SUPER               ,LOP_INVOKE                    ,LIR_invoke_super              )\
    DIR2LIR(OP_INVOKE_DIRECT              ,LOP_INVOKE                    ,LIR_invoke_direct             )\
    DIR2LIR(OP_INVOKE_STATIC              ,LOP_INVOKE                    ,LIR_invoke_static             )\
    DIR2LIR(OP_INVOKE_INTERFACE           ,LOP_INVOKE                    ,LIR_invoke_interface          )\
    DIR2LIR(OP_UNUSED_73                  ,0                             ,0                             )\
    DIR2LIR(OP_INVOKE_VIRTUAL_RANGE       ,LOP_INVOKE                    ,LIR_invoke_virtual|LIR_Range  )\
    DIR2LIR(OP_INVOKE_SUPER_RANGE         ,LOP_INVOKE                    ,LIR_invoke_super|LIR_Range    )\
    DIR2LIR(OP_INVOKE_DIRECT_RANGE        ,LOP_INVOKE                    ,LIR_invoke_direct|LIR_Range   )\
    DIR2LIR(OP_INVOKE_STATIC_RANGE        ,LOP_INVOKE                    ,LIR_invoke_static|LIR_Range   )\
    DIR2LIR(OP_INVOKE_INTERFACE_RANGE     ,LOP_INVOKE                    ,LIR_invoke_interface|LIR_Range)\
    DIR2LIR(OP_UNUSED_79                  ,0                             ,0                             )\
    DIR2LIR(OP_UNUSED_7A                  ,0                             ,0                             )\
    DIR2LIR(OP_NEG_INT                    ,LOP_NEG                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_NOT_INT                    ,LOP_NOT                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_NEG_LONG                   ,LOP_NEG                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_NOT_LONG                   ,LOP_NOT                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_NEG_FLOAT                  ,LOP_NEG                       ,LIR_JDT_float                 )\
    DIR2LIR(OP_NEG_DOUBLE                 ,LOP_NEG                       ,LIR_JDT_double                )\
    DIR2LIR(OP_INT_TO_LONG                ,LOP_CONVERT                   ,LIR_convert_i2l               )\
    DIR2LIR(OP_INT_TO_FLOAT               ,LOP_CONVERT                   ,LIR_convert_i2f               )\
    DIR2LIR(OP_INT_TO_DOUBLE              ,LOP_CONVERT                   ,LIR_convert_i2d               )\
    DIR2LIR(OP_LONG_TO_INT                ,LOP_CONVERT                   ,LIR_convert_l2i               )\
    DIR2LIR(OP_LONG_TO_FLOAT              ,LOP_CONVERT                   ,LIR_convert_l2f               )\
    DIR2LIR(OP_LONG_TO_DOUBLE             ,LOP_CONVERT                   ,LIR_convert_l2d               )\
    DIR2LIR(OP_FLOAT_TO_INT               ,LOP_CONVERT                   ,LIR_convert_f2i               )\
    DIR2LIR(OP_FLOAT_TO_LONG              ,LOP_CONVERT                   ,LIR_convert_f2l               )\
    DIR2LIR(OP_FLOAT_TO_DOUBLE            ,LOP_CONVERT                   ,LIR_convert_f2d               )\
    DIR2LIR(OP_DOUBLE_TO_INT              ,LOP_CONVERT                   ,LIR_convert_d2i               )\
    DIR2LIR(OP_DOUBLE_TO_LONG             ,LOP_CONVERT                   ,LIR_convert_d2l               )\
    DIR2LIR(OP_DOUBLE_TO_FLOAT            ,LOP_CONVERT                   ,LIR_convert_d2f               )\
    DIR2LIR(OP_INT_TO_BYTE                ,LOP_CONVERT                   ,LIR_convert_i2b               )\
    DIR2LIR(OP_INT_TO_CHAR                ,LOP_CONVERT                   ,LIR_convert_i2c               )\
    DIR2LIR(OP_INT_TO_SHORT               ,LOP_CONVERT                   ,LIR_convert_i2s               )\
    DIR2LIR(OP_ADD_INT                    ,LOP_ADD                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_SUB_INT                    ,LOP_SUB                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_MUL_INT                    ,LOP_MUL                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_DIV_INT                    ,LOP_DIV                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_REM_INT                    ,LOP_REM                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_AND_INT                    ,LOP_AND                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_OR_INT                     ,LOP_OR                        ,LIR_JDT_int                   )\
    DIR2LIR(OP_XOR_INT                    ,LOP_XOR                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_SHL_INT                    ,LOP_SHL                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_SHR_INT                    ,LOP_SHR                       ,LIR_JDT_int                   )\
    DIR2LIR(OP_USHR_INT                   ,LOP_USHR                      ,LIR_JDT_int                   )\
    DIR2LIR(OP_ADD_LONG                   ,LOP_ADD                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_SUB_LONG                   ,LOP_SUB                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_MUL_LONG                   ,LOP_MUL                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_DIV_LONG                   ,LOP_DIV                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_REM_LONG                   ,LOP_REM                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_AND_LONG                   ,LOP_AND                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_OR_LONG                    ,LOP_OR                        ,LIR_JDT_long                  )\
    DIR2LIR(OP_XOR_LONG                   ,LOP_XOR                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_SHL_LONG                   ,LOP_SHL                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_SHR_LONG                   ,LOP_SHR                       ,LIR_JDT_long                  )\
    DIR2LIR(OP_USHR_LONG                  ,LOP_USHR                      ,LIR_JDT_long                  )\
    DIR2LIR(OP_ADD_FLOAT                  ,LOP_ADD                       ,LIR_JDT_float                 )\
    DIR2LIR(OP_SUB_FLOAT                  ,LOP_SUB                       ,LIR_JDT_float                 )\
    DIR2LIR(OP_MUL_FLOAT                  ,LOP_MUL                       ,LIR_JDT_float                 )\
    DIR2LIR(OP_DIV_FLOAT                  ,LOP_DIV                       ,LIR_JDT_float                 )\
    DIR2LIR(OP_REM_FLOAT                  ,LOP_REM                       ,LIR_JDT_float                 )\
    DIR2LIR(OP_ADD_DOUBLE                 ,LOP_ADD                       ,LIR_JDT_double                )\
    DIR2LIR(OP_SUB_DOUBLE                 ,LOP_SUB                       ,LIR_JDT_double                )\
    DIR2LIR(OP_MUL_DOUBLE                 ,LOP_MUL                       ,LIR_JDT_double                )\
    DIR2LIR(OP_DIV_DOUBLE                 ,LOP_DIV                       ,LIR_JDT_double                )\
    DIR2LIR(OP_REM_DOUBLE                 ,LOP_REM                       ,LIR_JDT_double                )\
    DIR2LIR(OP_ADD_INT_2ADDR              ,LOP_ADD_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_SUB_INT_2ADDR              ,LOP_SUB_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_MUL_INT_2ADDR              ,LOP_MUL_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_DIV_INT_2ADDR              ,LOP_DIV_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_REM_INT_2ADDR              ,LOP_REM_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_AND_INT_2ADDR              ,LOP_AND_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_OR_INT_2ADDR               ,LOP_OR_ASSIGN                 ,LIR_JDT_int                   )\
    DIR2LIR(OP_XOR_INT_2ADDR              ,LOP_XOR_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_SHL_INT_2ADDR              ,LOP_SHL_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_SHR_INT_2ADDR              ,LOP_SHR_ASSIGN                ,LIR_JDT_int                   )\
    DIR2LIR(OP_USHR_INT_2ADDR             ,LOP_USHR_ASSIGN               ,LIR_JDT_int                   )\
    DIR2LIR(OP_ADD_LONG_2ADDR             ,LOP_ADD_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_SUB_LONG_2ADDR             ,LOP_SUB_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_MUL_LONG_2ADDR             ,LOP_MUL_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_DIV_LONG_2ADDR             ,LOP_DIV_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_REM_LONG_2ADDR             ,LOP_REM_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_AND_LONG_2ADDR             ,LOP_AND_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_OR_LONG_2ADDR              ,LOP_OR_ASSIGN                 ,LIR_JDT_long                  )\
    DIR2LIR(OP_XOR_LONG_2ADDR             ,LOP_XOR_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_SHL_LONG_2ADDR             ,LOP_SHL_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_SHR_LONG_2ADDR             ,LOP_SHR_ASSIGN                ,LIR_JDT_long                  )\
    DIR2LIR(OP_USHR_LONG_2ADDR            ,LOP_USHR_ASSIGN               ,LIR_JDT_long                  )\
    DIR2LIR(OP_ADD_FLOAT_2ADDR            ,LOP_ADD_ASSIGN                ,LIR_JDT_float                 )\
    DIR2LIR(OP_SUB_FLOAT_2ADDR            ,LOP_SUB_ASSIGN                ,LIR_JDT_float                 )\
    DIR2LIR(OP_MUL_FLOAT_2ADDR            ,LOP_MUL_ASSIGN                ,LIR_JDT_float                 )\
    DIR2LIR(OP_DIV_FLOAT_2ADDR            ,LOP_DIV_ASSIGN                ,LIR_JDT_float                 )\
    DIR2LIR(OP_REM_FLOAT_2ADDR            ,LOP_REM_ASSIGN                ,LIR_JDT_float                 )\
    DIR2LIR(OP_ADD_DOUBLE_2ADDR           ,LOP_ADD_ASSIGN                ,LIR_JDT_double                )\
    DIR2LIR(OP_SUB_DOUBLE_2ADDR           ,LOP_SUB_ASSIGN                ,LIR_JDT_double                )\
    DIR2LIR(OP_MUL_DOUBLE_2ADDR           ,LOP_MUL_ASSIGN                ,LIR_JDT_double                )\
    DIR2LIR(OP_DIV_DOUBLE_2ADDR           ,LOP_DIV_ASSIGN                ,LIR_JDT_double                )\
    DIR2LIR(OP_REM_DOUBLE_2ADDR           ,LOP_REM_ASSIGN                ,LIR_JDT_double                )\
    DIR2LIR(OP_ADD_INT_LIT16              ,LOP_ADD_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_RSUB_INT                   ,LOP_SUB_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_MUL_INT_LIT16              ,LOP_MUL_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_DIV_INT_LIT16              ,LOP_DIV_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_REM_INT_LIT16              ,LOP_REM_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_AND_INT_LIT16              ,LOP_AND_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_OR_INT_LIT16               ,LOP_OR_LIT                    ,LIR_JDT_int                   )\
    DIR2LIR(OP_XOR_INT_LIT16              ,LOP_XOR_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_ADD_INT_LIT8               ,LOP_ADD_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_RSUB_INT_LIT8              ,LOP_SUB_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_MUL_INT_LIT8               ,LOP_MUL_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_DIV_INT_LIT8               ,LOP_DIV_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_REM_INT_LIT8               ,LOP_REM_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_AND_INT_LIT8               ,LOP_AND_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_OR_INT_LIT8                ,LOP_OR_LIT                    ,LIR_JDT_int                   )\
    DIR2LIR(OP_XOR_INT_LIT8               ,LOP_XOR_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_SHL_INT_LIT8               ,LOP_SHL_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_SHR_INT_LIT8               ,LOP_SHR_LIT                   ,LIR_JDT_int                   )\
    DIR2LIR(OP_USHR_INT_LIT8              ,LOP_USHR_LIT                  ,LIR_JDT_int                   )\
    DIR2LIR(OP_PACKED_SWITCH_PAYLOAD      ,LOP_PACKED_SWITCH_PAYLOAD     ,0                             )\
    DIR2LIR(OP_SPARSE_SWITCH_PAYLOAD      ,LOP_SPARSE_SWITCH_PAYLOAD     ,0                             )\


#define kNumOpcodes 229


#define DIR2LIR(dop,lop,flags) lop,
static UInt8 gDIR2LIROpcode[kNumOpcodes] = {
    DECLARE_DIR2LIR
};
#undef DIR2LIR

#define DIR2LIR(dop,lop,flags) flags,
static UInt8 gDIR2LIRFlags[kNumOpcodes] = {
    DECLARE_DIR2LIR
};
#undef DIR2LIR


DIR2LIRInfoTables gDIR2LIRInfo = {
    gDIR2LIROpcode,
    gDIR2LIRFlags,
};
