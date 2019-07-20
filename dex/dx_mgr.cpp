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
#include "ltype.h"
#include "dx_mgr.h"

DXC_INFO const g_dx_code_info[] = {
    {0,        DX_NOP,                            1, DX_ATTR_CONT, "nop", FMT10x},
    {1,        DX_MOVE,                           1, DX_ATTR_CONT, "move", FMT12x},
    {2,        DX_MOVE_FROM16,                    2, DX_ATTR_CONT, "move/from16", FMT22x},
    {3,        DX_MOVE_16,                        3, DX_ATTR_CONT, "move/16", FMT32x},
    {4,        DX_MOVE_WIDE,                      1, DX_ATTR_CONT, "move-wide", FMT12x},
    {5,        DX_MOVE_WIDE_FROM16,               2, DX_ATTR_CONT, "move-wide/from16", FMT22x},
    {6,        DX_MOVE_WIDE_16,                   3, DX_ATTR_CONT, "move-wide/16", FMT32x},
    {7,        DX_MOVE_OBJECT,                    1, DX_ATTR_CONT, "move-object", FMT12x},
    {8,        DX_MOVE_OBJECT_FROM16,             2, DX_ATTR_CONT, "move-object/from16", FMT22x},
    {9,        DX_MOVE_OBJECT_16,                 3, DX_ATTR_CONT, "move-object/16", FMT32x},

    //11x
    {10,     DX_MOVE_RESULT,                    1, DX_ATTR_CONT, "move-result", FMT11x},
    {11,     DX_MOVE_RESULT_WIDE,               1, DX_ATTR_CONT, "move-result-wide", FMT11x},
    {12,     DX_MOVE_RESULT_OBJECT,             1, DX_ATTR_CONT, "move-result-object", FMT11x},
    {13,     DX_MOVE_EXCEPTION,                 1, DX_ATTR_CONT, "move-exception", FMT11x},

    //10x
    {14,     DX_RETURN_VOID,                    1, DX_ATTR_RET, "return-void", FMT10x},

    //11x
    {15,     DX_RETURN,                         1, DX_ATTR_RET, "return", FMT11x},
    {16,     DX_RETURN_WIDE,                    1, DX_ATTR_RET, "return-wide", FMT11x},
    {17,     DX_RETURN_OBJECT,                  1, DX_ATTR_RET, "return-object", FMT11x},

    //
    {18,     DX_CONST_4,                        1, DX_ATTR_CONT, "const/4", FMT11n},
    {19,     DX_CONST_16,                       2, DX_ATTR_CONT, "const/16", FMT21s},
    {20,     DX_CONST,                          3, DX_ATTR_CONT, "const", FMT31i},

    //21h
    {21,     DX_CONST_HIGH16,                   2, DX_ATTR_CONT, "const/high16", FMT21h},
    {22,     DX_CONST_WIDE_16,                  2, DX_ATTR_CONT, "const-wide/16", FMT21s},
    {23,     DX_CONST_WIDE_32,                  3, DX_ATTR_CONT, "const-wide/32", FMT31i},
    {24,     DX_CONST_WIDE,                     5, DX_ATTR_CONT, "const-wide", FMT51l},
    {25,     DX_CONST_WIDE_HIGH16,              2, DX_ATTR_CONT, "const-wide/high16", FMT21h},
    {26,     DX_CONST_STRING,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "const-string", FMT21c},
    {27,     DX_CONST_STRING_JUMBO,             3, DX_ATTR_CONT|DX_ATTR_THROW, "const-string/jumbo", FMT31c},
    {28,     DX_CONST_CLASS,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "const-class", FMT21c},

    //11x
    {29,     DX_MONITOR_ENTER,                  1, DX_ATTR_CONT|DX_ATTR_THROW, "monitor-enter", FMT11x},
    {30,     DX_MONITOR_EXIT,                   1, DX_ATTR_CONT|DX_ATTR_THROW, "monitor-exit", FMT11x},

    //21c
    {31,     DX_CHECK_CAST,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "check-cast", FMT21c},

    //22c
    {32,     DX_INSTANCE_OF,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "instance-of", FMT22c},

    //12x
    {33,     DX_ARRAY_LENGTH,                   1, DX_ATTR_CONT|DX_ATTR_THROW, "array-length", FMT12x},

    //21c
    {34,     DX_NEW_INSTANCE,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "new-instance", FMT21c},

    //22c
    {35,     DX_NEW_ARRAY,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "new-array", FMT22c},

    //35c
    {36,     DX_FILLED_NEW_ARRAY,               3, DX_ATTR_CONT|DX_ATTR_THROW, "filled-new-array", FMT35c},

    //3rc
    {37,     DX_FILLED_NEW_ARRAY_RANGE,         3, DX_ATTR_CONT|DX_ATTR_THROW, "filled-new-array/range", FMT3rc},

    //31t
    {38,     DX_FILL_ARRAY_DATA,                3, DX_ATTR_CONT|DX_ATTR_THROW, "fill-array-data", FMT31t},

    //11x
    {39,     DX_THROW,                          1, DX_ATTR_THROW, "throw", FMT11x},

    //10t
    {40,     DX_GOTO,                           1, DX_ATTR_BR|DX_ATTR_GOTO, "goto", FMT10t},

    //20t
    {41,     DX_GOTO_16,                        2, DX_ATTR_BR|DX_ATTR_GOTO, "goto/16", FMT20t},

    //30t
    {42,     DX_GOTO_32,                        3, DX_ATTR_BR|DX_ATTR_GOTO, "goto/32", FMT30t},

    //31t
    {43,     DX_PACKED_SWITCH,                  3, DX_ATTR_CONT|DX_ATTR_SWITCH, "packed-switch", FMT31t},
    {44,     DX_SPARSE_SWITCH,                  3, DX_ATTR_CONT|DX_ATTR_SWITCH, "sparse-switch", FMT31t},

    //23x
    {45,     DX_CMPL_FLOAT,                     2, DX_ATTR_CONT, "cmpl-float", FMT23x},
    {46,     DX_CMPG_FLOAT,                     2, DX_ATTR_CONT, "cmpg-float", FMT23x},
    {47,     DX_CMPL_DOUBLE,                    2, DX_ATTR_CONT, "cmpl-double", FMT23x},
    {48,     DX_CMPG_DOUBLE,                    2, DX_ATTR_CONT, "cmpg-double", FMT23x},
    {49,     DX_CMP_LONG,                       2, DX_ATTR_CONT, "cmp-long", FMT23x},

    //22t
    {50,     DX_IF_EQ,                          2, DX_ATTR_BR|DX_ATTR_CONT, "if-eq", FMT22t},
    {51,     DX_IF_NE,                          2, DX_ATTR_BR|DX_ATTR_CONT, "if-ne", FMT22t},
    {52,     DX_IF_LT,                          2, DX_ATTR_BR|DX_ATTR_CONT, "if-lt", FMT22t},
    {53,     DX_IF_GE,                          2, DX_ATTR_BR|DX_ATTR_CONT, "if-ge", FMT22t},
    {54,     DX_IF_GT,                          2, DX_ATTR_BR|DX_ATTR_CONT, "if-gt", FMT22t},
    {55,     DX_IF_LE,                          2, DX_ATTR_BR|DX_ATTR_CONT, "if-le", FMT22t},

    //21t
    {56,     DX_IF_EQZ,                         2, DX_ATTR_BR|DX_ATTR_CONT, "if-eqz", FMT21t},
    {57,     DX_IF_NEZ,                         2, DX_ATTR_BR|DX_ATTR_CONT, "if-nez", FMT21t},
    {58,     DX_IF_LTZ,                         2, DX_ATTR_BR|DX_ATTR_CONT, "if-ltz", FMT21t},
    {59,     DX_IF_GEZ,                         2, DX_ATTR_BR|DX_ATTR_CONT, "if-gez", FMT21t},
    {60,     DX_IF_GTZ,                         2, DX_ATTR_BR|DX_ATTR_CONT, "if-gtz", FMT21t},
    {61,     DX_IF_LEZ,                         2, DX_ATTR_BR|DX_ATTR_CONT, "if-lez", FMT21t},

    {62,     DX_UNUSED_3E,                      1, DX_ATTR_UNDEF, "unused-3e", FMT10x},
    {63,     DX_UNUSED_3F,                      1, DX_ATTR_UNDEF, "unused-3f", FMT10x},
    {64,     DX_UNUSED_40,                      1, DX_ATTR_UNDEF, "unused-40", FMT10x},
    {65,     DX_UNUSED_41,                      1, DX_ATTR_UNDEF, "unused-41", FMT10x},
    {66,     DX_UNUSED_42,                      1, DX_ATTR_UNDEF, "unused-42", FMT10x},
    {67,     DX_UNUSED_43,                      1, DX_ATTR_UNDEF, "unused-43", FMT10x},

    //23x
    {68,     DX_AGET,                           2, DX_ATTR_CONT|DX_ATTR_THROW, "aget", FMT23x},
    {69,     DX_AGET_WIDE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "aget-wide", FMT23x},
    {70,     DX_AGET_OBJECT,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "aget-object", FMT23x},
    {71,     DX_AGET_BOOLEAN,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "aget-boolean", FMT23x},
    {72,     DX_AGET_BYTE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "aget-byte", FMT23x},
    {73,     DX_AGET_CHAR,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "aget-char", FMT23x},
    {74,     DX_AGET_SHORT,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "aget-short", FMT23x},
    {75,     DX_APUT,                           2, DX_ATTR_CONT|DX_ATTR_THROW, "aput", FMT23x},
    {76,     DX_APUT_WIDE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "aput-wide", FMT23x},
    {77,     DX_APUT_OBJECT,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "aput-object", FMT23x},
    {78,     DX_APUT_BOOLEAN,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "aput-boolean", FMT23x},
    {79,     DX_APUT_BYTE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "aput-byte", FMT23x},
    {80,     DX_APUT_CHAR,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "aput-char", FMT23x},
    {81,     DX_APUT_SHORT,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "aput-short", FMT23x},

    //22c
    {82,     DX_IGET,                           2, DX_ATTR_CONT|DX_ATTR_THROW, "iget", FMT22c},
    {83,     DX_IGET_WIDE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-wide", FMT22c},
    {84,     DX_IGET_OBJECT,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-object", FMT22c},
    {85,     DX_IGET_BOOLEAN,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-boolean", FMT22c},
    {86,     DX_IGET_BYTE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-byte", FMT22c},
    {87,     DX_IGET_CHAR,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-char", FMT22c},
    {88,     DX_IGET_SHORT,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-short", FMT22c},
    {89,     DX_IPUT,                           2, DX_ATTR_CONT|DX_ATTR_THROW, "iput", FMT22c},
    {90,     DX_IPUT_WIDE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-wide", FMT22c},
    {91,     DX_IPUT_OBJECT,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-object", FMT22c},
    {92,     DX_IPUT_BOOLEAN,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-boolean", FMT22c},
    {93,     DX_IPUT_BYTE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-byte", FMT22c},
    {94,     DX_IPUT_CHAR,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-char", FMT22c},
    {95,     DX_IPUT_SHORT,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-short", FMT22c},

    //21c
    {96,     DX_SGET,                           2, DX_ATTR_CONT|DX_ATTR_THROW, "sget", FMT21c},
    {97,     DX_SGET_WIDE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "sget-wide", FMT21c},
    {98,     DX_SGET_OBJECT,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "sget-object", FMT21c},
    {99,     DX_SGET_BOOLEAN,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "sget-boolean", FMT21c},
    {100,    DX_SGET_BYTE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "sget-byte", FMT21c},
    {101,    DX_SGET_CHAR,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "sget-char", FMT21c},
    {102,    DX_SGET_SHORT,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "sget-short", FMT21c},
    {103,    DX_SPUT,                           2, DX_ATTR_CONT|DX_ATTR_THROW, "sput", FMT21c},
    {104,    DX_SPUT_WIDE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "sput-wide", FMT21c},
    {105,    DX_SPUT_OBJECT,                    2, DX_ATTR_CONT|DX_ATTR_THROW, "sput-object", FMT21c},
    {106,    DX_SPUT_BOOLEAN,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "sput-boolean", FMT21c},
    {107,    DX_SPUT_BYTE,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "sput-byte", FMT21c},
    {108,    DX_SPUT_CHAR,                      2, DX_ATTR_CONT|DX_ATTR_THROW, "sput-char", FMT21c},
    {109,    DX_SPUT_SHORT,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "sput-short", FMT21c},

    //35c
    {110,    DX_INVOKE_VIRTUAL,                 3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-virtual", FMT35c},
    {111,    DX_INVOKE_SUPER,                   3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-super", FMT35c},
    {112,    DX_INVOKE_DIRECT,                  3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-direct", FMT35c},
    {113,    DX_INVOKE_STATIC,                  3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-static", FMT35c},
    {114,    DX_INVOKE_INTERFACE,               3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-interface", FMT35c},

    //10x
    {115,    DX_RETURN_VOID_BARRIER,         1, DX_ATTR_RET, "return-void-barrier", FMT10x},

    //3rc
    {116,    DX_INVOKE_VIRTUAL_RANGE,        3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-virtual/range", FMT3rc},
    {117,    DX_INVOKE_SUPER_RANGE,             3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-super/range", FMT3rc},
    {118,    DX_INVOKE_DIRECT_RANGE,         3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-direct/range", FMT3rc},
    {119,    DX_INVOKE_STATIC_RANGE,         3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-static/range", FMT3rc},
    {120,     DX_INVOKE_INTERFACE_RANGE,         3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-interface/range", FMT3rc},
    {121,     DX_UNUSED_79,                      1, DX_ATTR_UNDEF, "unused-79", FMT10x},
    {122,     DX_UNUSED_7A,                      1, DX_ATTR_UNDEF, "unused-7a", FMT10x},

    //12x
    {123,     DX_NEG_INT,                        1, DX_ATTR_CONT, "neg-int", FMT12x},
    {124,     DX_NOT_INT,                        1, DX_ATTR_CONT, "not-int", FMT12x},
    {125,     DX_NEG_LONG,                       1, DX_ATTR_CONT, "neg-long", FMT12x},
    {126,     DX_NOT_LONG,                       1, DX_ATTR_CONT, "not-long", FMT12x},
    {127,     DX_NEG_FLOAT,                      1, DX_ATTR_CONT, "neg-float", FMT12x},
    {128,     DX_NEG_DOUBLE,                     1, DX_ATTR_CONT, "neg-double", FMT12x},
    {129,     DX_INT_TO_LONG,                    1, DX_ATTR_CONT, "int-to-long", FMT12x},
    {130,     DX_INT_TO_FLOAT,                   1, DX_ATTR_CONT, "int-to-float", FMT12x},
    {131,     DX_INT_TO_DOUBLE,                  1, DX_ATTR_CONT, "int-to-double", FMT12x},
    {132,     DX_LONG_TO_INT,                    1, DX_ATTR_CONT, "long-to-int", FMT12x},
    {133,     DX_LONG_TO_FLOAT,                  1, DX_ATTR_CONT, "long-to-float", FMT12x},
    {134,     DX_LONG_TO_DOUBLE,                 1, DX_ATTR_CONT, "long-to-double", FMT12x},
    {135,     DX_FLOAT_TO_INT,                   1, DX_ATTR_CONT, "float-to-int", FMT12x},
    {136,     DX_FLOAT_TO_LONG,                  1, DX_ATTR_CONT, "float-to-long", FMT12x},
    {137,     DX_FLOAT_TO_DOUBLE,                1, DX_ATTR_CONT, "float-to-double", FMT12x},
    {138,     DX_DOUBLE_TO_INT,                  1, DX_ATTR_CONT, "double-to-int", FMT12x},
    {139,     DX_DOUBLE_TO_LONG,                 1, DX_ATTR_CONT, "double-to-long", FMT12x},
    {140,     DX_DOUBLE_TO_FLOAT,                1, DX_ATTR_CONT, "double-to-float", FMT12x},
    {141,     DX_INT_TO_BYTE,                    1, DX_ATTR_CONT, "int-to-byte", FMT12x},
    {142,     DX_INT_TO_CHAR,                    1, DX_ATTR_CONT, "int-to-char", FMT12x},
    {143,     DX_INT_TO_SHORT,                   1, DX_ATTR_CONT, "int-to-short", FMT12x},

    //23x
    {144,     DX_ADD_INT,                        2, DX_ATTR_CONT, "add-int", FMT23x},
    {145,     DX_SUB_INT,                        2, DX_ATTR_CONT, "sub-int", FMT23x},
    {146,     DX_MUL_INT,                        2, DX_ATTR_CONT, "mul-int", FMT23x},
    {147,     DX_DIV_INT,                        2, DX_ATTR_CONT|DX_ATTR_THROW, "div-int", FMT23x},
    {148,     DX_REM_INT,                        2, DX_ATTR_CONT|DX_ATTR_THROW, "rem-int", FMT23x},
    {149,     DX_AND_INT,                        2, DX_ATTR_CONT, "and-int", FMT23x},
    {150,     DX_OR_INT,                         2, DX_ATTR_CONT, "or-int", FMT23x},
    {151,     DX_XOR_INT,                        2, DX_ATTR_CONT, "xor-int", FMT23x},
    {152,     DX_SHL_INT,                        2, DX_ATTR_CONT, "shl-int", FMT23x},
    {153,     DX_SHR_INT,                        2, DX_ATTR_CONT, "shr-int", FMT23x},
    {154,     DX_USHR_INT,                       2, DX_ATTR_CONT, "ushr-int", FMT23x},
    {155,     DX_ADD_LONG,                       2, DX_ATTR_CONT, "add-long", FMT23x},
    {156,     DX_SUB_LONG,                       2, DX_ATTR_CONT, "sub-long", FMT23x},
    {157,     DX_MUL_LONG,                       2, DX_ATTR_CONT, "mul-long", FMT23x},
    {158,     DX_DIV_LONG,                       2, DX_ATTR_CONT|DX_ATTR_THROW, "div-long", FMT23x},
    {159,     DX_REM_LONG,                       2, DX_ATTR_CONT|DX_ATTR_THROW, "rem-long", FMT23x},
    {160,     DX_AND_LONG,                       2, DX_ATTR_CONT, "and-long", FMT23x},
    {161,     DX_OR_LONG,                        2, DX_ATTR_CONT, "or-long", FMT23x},
    {162,     DX_XOR_LONG,                       2, DX_ATTR_CONT, "xor-long", FMT23x},
    {163,     DX_SHL_LONG,                       2, DX_ATTR_CONT, "shl-long", FMT23x},
    {164,     DX_SHR_LONG,                       2, DX_ATTR_CONT, "shr-long", FMT23x},
    {165,     DX_USHR_LONG,                      2, DX_ATTR_CONT, "ushr-long", FMT23x},
    {166,     DX_ADD_FLOAT,                      2, DX_ATTR_CONT, "add-float", FMT23x},
    {167,     DX_SUB_FLOAT,                      2, DX_ATTR_CONT, "sub-float", FMT23x},
    {168,     DX_MUL_FLOAT,                      2, DX_ATTR_CONT, "mul-float", FMT23x},
    {169,     DX_DIV_FLOAT,                      2, DX_ATTR_CONT, "div-float", FMT23x},
    {170,     DX_REM_FLOAT,                      2, DX_ATTR_CONT, "rem-float", FMT23x},
    {171,     DX_ADD_DOUBLE,                     2, DX_ATTR_CONT, "add-double", FMT23x},
    {172,     DX_SUB_DOUBLE,                     2, DX_ATTR_CONT, "sub-double", FMT23x},
    {173,     DX_MUL_DOUBLE,                     2, DX_ATTR_CONT, "mul-double", FMT23x},
    {174,     DX_DIV_DOUBLE,                     2, DX_ATTR_CONT, "div-double", FMT23x},
    {175,     DX_REM_DOUBLE,                     2, DX_ATTR_CONT, "rem-double", FMT23x},

    //12x
    {176,     DX_ADD_INT_2ADDR,                  1, DX_ATTR_CONT, "add-int/2addr", FMT12x},
    {177,     DX_SUB_INT_2ADDR,                  1, DX_ATTR_CONT, "sub-int/2addr", FMT12x},
    {178,     DX_MUL_INT_2ADDR,                  1, DX_ATTR_CONT, "mul-int/2addr", FMT12x},
    {179,     DX_DIV_INT_2ADDR,                  1, DX_ATTR_CONT|DX_ATTR_THROW, "div-int/2addr", FMT12x},
    {180,     DX_REM_INT_2ADDR,                  1, DX_ATTR_CONT|DX_ATTR_THROW, "rem-int/2addr", FMT12x},
    {181,     DX_AND_INT_2ADDR,                  1, DX_ATTR_CONT, "and-int/2addr", FMT12x},
    {182,     DX_OR_INT_2ADDR,                   1, DX_ATTR_CONT, "or-int/2addr", FMT12x},
    {183,     DX_XOR_INT_2ADDR,                  1, DX_ATTR_CONT, "xor-int/2addr", FMT12x},
    {184,     DX_SHL_INT_2ADDR,                  1, DX_ATTR_CONT, "shl-int/2addr", FMT12x},
    {185,     DX_SHR_INT_2ADDR,                  1, DX_ATTR_CONT, "shr-int/2addr", FMT12x},
    {186,     DX_USHR_INT_2ADDR,                 1, DX_ATTR_CONT, "ushr-int/2addr", FMT12x},
    {187,     DX_ADD_LONG_2ADDR,                 1, DX_ATTR_CONT, "add-long/2addr", FMT12x},
    {188,     DX_SUB_LONG_2ADDR,                 1, DX_ATTR_CONT, "sub-long/2addr", FMT12x},
    {189,     DX_MUL_LONG_2ADDR,                 1, DX_ATTR_CONT, "mul-long/2addr", FMT12x},
    {190,     DX_DIV_LONG_2ADDR,                 1, DX_ATTR_CONT|DX_ATTR_THROW, "div-long/2addr", FMT12x},
    {191,     DX_REM_LONG_2ADDR,                 1, DX_ATTR_CONT|DX_ATTR_THROW, "rem-long/2addr", FMT12x},
    {192,     DX_AND_LONG_2ADDR,                 1, DX_ATTR_CONT, "and-long/2addr", FMT12x},
    {193,     DX_OR_LONG_2ADDR,                  1, DX_ATTR_CONT, "or-long/2addr", FMT12x},
    {194,     DX_XOR_LONG_2ADDR,                 1, DX_ATTR_CONT, "xor-long/2addr", FMT12x},
    {195,     DX_SHL_LONG_2ADDR,                 1, DX_ATTR_CONT, "shl-long/2addr", FMT12x},
    {196,     DX_SHR_LONG_2ADDR,                 1, DX_ATTR_CONT, "shr-long/2addr", FMT12x},
    {197,     DX_USHR_LONG_2ADDR,                1, DX_ATTR_CONT, "ushr-long/2addr", FMT12x},
    {198,     DX_ADD_FLOAT_2ADDR,                1, DX_ATTR_CONT, "add-float/2addr", FMT12x},
    {199,     DX_SUB_FLOAT_2ADDR,                1, DX_ATTR_CONT, "sub-float/2addr", FMT12x},
    {200,     DX_MUL_FLOAT_2ADDR,                1, DX_ATTR_CONT, "mul-float/2addr", FMT12x},
    {201,     DX_DIV_FLOAT_2ADDR,                1, DX_ATTR_CONT, "div-float/2addr", FMT12x},
    {202,     DX_REM_FLOAT_2ADDR,                1, DX_ATTR_CONT, "rem-float/2addr", FMT12x},
    {203,     DX_ADD_DOUBLE_2ADDR,               1, DX_ATTR_CONT, "add-double/2addr", FMT12x},
    {204,     DX_SUB_DOUBLE_2ADDR,               1, DX_ATTR_CONT, "sub-double/2addr", FMT12x},
    {205,     DX_MUL_DOUBLE_2ADDR,               1, DX_ATTR_CONT, "mul-double/2addr", FMT12x},
    {206,     DX_DIV_DOUBLE_2ADDR,               1, DX_ATTR_CONT, "div-double/2addr", FMT12x},
    {207,     DX_REM_DOUBLE_2ADDR,               1, DX_ATTR_CONT, "rem-double/2addr", FMT12x},

    //22s
    {208,     DX_ADD_INT_LIT16,                  2, DX_ATTR_CONT, "add-int/lit16", FMT22s},
    {209,     DX_RSUB_INT,                       2, DX_ATTR_CONT, "rsub-int", FMT22s},
    {210,     DX_MUL_INT_LIT16,                  2, DX_ATTR_CONT, "mul-int/lit16", FMT22s},
    {211,     DX_DIV_INT_LIT16,                  2, DX_ATTR_CONT|DX_ATTR_THROW, "div-int/lit16", FMT22s},
    {212,     DX_REM_INT_LIT16,                  2, DX_ATTR_CONT|DX_ATTR_THROW, "rem-int/lit16", FMT22s},
    {213,     DX_AND_INT_LIT16,                  2, DX_ATTR_CONT, "and-int/lit16", FMT22s},
    {214,     DX_OR_INT_LIT16,                   2, DX_ATTR_CONT, "or-int/lit16", FMT22s},
    {215,     DX_XOR_INT_LIT16,                  2, DX_ATTR_CONT, "xor-int/lit16", FMT22s},

    //22b
    {216,     DX_ADD_INT_LIT8,                   2, DX_ATTR_CONT, "add-int/lit8", FMT22b},
    {217,     DX_RSUB_INT_LIT8,                  2, DX_ATTR_CONT, "rsub-int/lit8", FMT22b},
    {218,     DX_MUL_INT_LIT8,                   2, DX_ATTR_CONT, "mul-int/lit8", FMT22b},
    {219,     DX_DIV_INT_LIT8,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "div-int/lit8", FMT22b},
    {220,     DX_REM_INT_LIT8,                   2, DX_ATTR_CONT|DX_ATTR_THROW, "rem-int/lit8", FMT22b},
    {221,     DX_AND_INT_LIT8,                   2, DX_ATTR_CONT, "and-int/lit8", FMT22b},
    {222,     DX_OR_INT_LIT8,                    2, DX_ATTR_CONT, "or-int/lit8", FMT22b},
    {223,     DX_XOR_INT_LIT8,                   2, DX_ATTR_CONT, "xor-int/lit8", FMT22b},
    {224,     DX_SHL_INT_LIT8,                   2, DX_ATTR_CONT, "shl-int/lit8", FMT22b},
    {225,     DX_SHR_INT_LIT8,                   2, DX_ATTR_CONT, "shr-int/lit8", FMT22b},
    {226,     DX_USHR_INT_LIT8,                  2, DX_ATTR_CONT, "ushr-int/lit8", FMT22b},

    //
    {227,     DX_IGET_QUICK,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-quick", FMT22c},
    {228,     DX_IGET_WIDE_QUICK,                2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-wide-quick", FMT22c},
    {229,     DX_IGET_OBJECT_QUICK,              2, DX_ATTR_CONT|DX_ATTR_THROW, "iget-object-quick", FMT22c},
    {230,     DX_IPUT_QUICK,                     2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-quick", FMT22c},
    {231,     DX_IPUT_WIDE_QUICK,                2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-wide-quick", FMT22c},
    {232,     DX_IPUT_OBJECT_QUICK,              2, DX_ATTR_CONT|DX_ATTR_THROW, "iput-object-quick", FMT22c},
    {233,     DX_INVOKE_VIRTUAL_QUICK,          3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-virtual-quick", FMT35c},
    {234,     DX_INVOKE_VIRTUAL_RANGE_QUICK,    3, DX_ATTR_CONT|DX_ATTR_THROW|DX_ATTR_CALL, "invoke-virtual/range-quick", FMT3rc},
    {235,     DX_UNUSED_EB,                     1, DX_ATTR_UNDEF, "unused-eb", FMT10x},
    {236,     DX_UNUSED_EC,                     1, DX_ATTR_UNDEF, "unused-ec", FMT10x},
    {237,     DX_UNUSED_ED,                     1, DX_ATTR_UNDEF, "unused-ed", FMT10x},
    {238,     DX_UNUSED_EE,                     1, DX_ATTR_UNDEF, "unused-ee", FMT10x},
    {239,     DX_UNUSED_EF,                     1, DX_ATTR_UNDEF, "unused-ef", FMT10x},
    {240,    DX_UNUSED_F0,                     1, DX_ATTR_UNDEF, "unused-f0", FMT10x},
    {241,    DX_UNUSED_F1,                     1, DX_ATTR_UNDEF, "unused-f1", FMT10x},
    {242,    DX_UNUSED_F2,                     1, DX_ATTR_UNDEF, "unused-f2", FMT10x},
    {243,    DX_UNUSED_F3,                     1, DX_ATTR_UNDEF, "unused-f3", FMT10x},
    {244,    DX_UNUSED_F4,                     1, DX_ATTR_UNDEF, "unused-f4", FMT10x},
    {245,    DX_UNUSED_F5,                     1, DX_ATTR_UNDEF, "unused-f5", FMT10x},
    {246,    DX_UNUSED_F6,                     1, DX_ATTR_UNDEF, "unused-f6", FMT10x},
    {247,    DX_UNUSED_F7,                     1, DX_ATTR_UNDEF, "unused-f7", FMT10x},
    {248,    DX_UNUSED_F8,                     1, DX_ATTR_UNDEF, "unused-f8", FMT10x},
    {249,    DX_UNUSED_F9,                     1, DX_ATTR_UNDEF, "unused-f9", FMT10x},
    {250,    DX_UNUSED_FA,                     1, DX_ATTR_UNDEF, "unused-fa", FMT10x},
    {251,    DX_UNUSED_FB,                     1, DX_ATTR_UNDEF, "unused-fb", FMT10x},
    {252,    DX_UNUSED_FC,                     1, DX_ATTR_UNDEF, "unused-fc", FMT10x},
    {253,    DX_UNUSED_FD,                     1, DX_ATTR_UNDEF, "unused-fd", FMT10x},
    {254,    DX_UNUSED_FE,                     1, DX_ATTR_UNDEF, "unused-fe", FMT10x},
    {255,    DX_UNUSED_FF,                     1, DX_ATTR_UNDEF, "unused-ff", FMT10x},
    {256,     DX_PACKED_SWITCH_PAYLOAD,        1, DX_ATTR_CONT,  "packed-switch-payload", FMT00x},
    {257,     DX_SPARSE_SWITCH_PAYLOAD,        1, DX_ATTR_CONT,  "sparse-switch-payload", FMT00x},
};


//Handy macros for helping decode instructions.
#define FETCH(offset)            (cptr[(offset)])
#define FETCH_uint32(offset)    (fetch_uint32_impl((offset), cptr))
#define INST_A(insn)            (((USHORT)(insn) >> 8) & 0x0f)
#define INST_B(insn)            ((USHORT)(insn) >> 12)
#define INST_AA(insn)            ((insn) >> 8)

static inline UINT fetch_uint32_impl(UINT offset, USHORT const* cptr)
{
    return cptr[offset] | ((UINT)cptr[offset+1] << 16);
}


//Dump all insns of method.
void DX_MGR::dump_method(IN DX_INFO const& dxinfo, IN FILE * h)
{
    if (h == NULL) return;
    fprintf(h, "\n=== DX_MGR DUMP : COMPILE %s::%s, has %d insn ===",
                     DXI_class_name(&dxinfo),
                     DXI_method_name(&dxinfo),
                     DXI_num_of_op(&dxinfo));
    USHORT const* cptr = DXI_start_cptr(&dxinfo);
    USHORT const* endptr = DXI_end_cptr(&dxinfo);
    ASSERT0(cptr && endptr);
    fprintf(h, "\n");
    INT ofst = 0; //offset to dexcode-insn-buffer.
    if (cptr == NULL) return;
    UINT i = 0;
    while (cptr < endptr) {
        if (is_inline_data(cptr)) {
            break;
        }
        DXC dc;
        decode_dx(cptr, dc);
        fprintf(h, "\n(%dth)(%d):", i, ofst);
        dump_dx(dc, h, ofst);
        cptr += DX_sz(dc.opc);
        ofst += DX_sz(dc.opc);
        i++;
    }
    fprintf(h, "\n=====\n");
    fflush(h);
}


void DX_MGR::dump_dx(DXC const& dc, FILE * h, INT ofst)
{
    if (h == NULL) { return; }
    ASSERT0(dc.opc <= 255);
    fprintf(h, "%s ", DX_name(dc.opc));
    fflush(h);
    switch (DX_fmt(dc.opc)) {
    case FMT10x:       // op
        fprintf(h, "%d", dc.vA);
        break;
    case FMT12x:       // op vA, vB
        fprintf(h, "v%u, v%u", dc.vA, dc.vB);
        break;
    case FMT11n:       // op vA, #+B
        fprintf(h, "v%u, %d", dc.vA, dc.vB);
        break;
    case FMT11x:       // op vAA
        fprintf(h, "v%u", dc.vA);
        break;
    case FMT10t:       // op +AA
        fprintf(h, "%d(%d)", dc.vA, dc.vA + ofst);
        break;
    case FMT20t:       // op +AAAA
        fprintf(h, "%d(%d)", dc.vA, dc.vA + ofst);
        break;
    case FMT21c:       // op vAA, thing@BBBB
        if (dc.opc == DX_NEW_INSTANCE) {
            fprintf(h, "v%u, %s", dc.vA, get_type_name(dc.vB));
        } else if (dc.opc == DX_CONST_STRING) {
            fprintf(h, "v%u, \"%s\"", dc.vA, get_string(dc.vB));
        } else if (dc.opc == DX_CONST_CLASS ||
                   dc.opc == DX_CHECK_CAST) {
            fprintf(h, "v%u, %s", dc.vA, get_type_name(dc.vB));
        } else {
            //sget|sput
            fprintf(h, "v%u, %s::%s(%d)", dc.vA,
                        get_class_name_by_field_id(dc.vB),
                        get_field_name(dc.vB),
                        dc.vB);
        }
        break;
    case FMT22x:       // op vAA, vBBBB
        fprintf(h, "v%u, v%u", dc.vA, dc.vB);
        break;
    case FMT21s:       // op vAA, #+BBBB
        fprintf(h, "v%u, %d", dc.vA, dc.vB);
        break;
    case FMT21t:       // op vAA, +BBBB,
        //Branch to the given destination if the given register's
        //value compares with 0 as specified.
        fprintf(h, "v%u, %d(%d)", dc.vA, dc.vB, dc.vB + ofst);
        break;
    case FMT21h:       // op vAA, #+BBBB0000[00000000]
        /*
        The value should be treated as right-zero-extended, but we don't
        actually do that here. Among other things, we don't know if it's
        the top bits of a 32- or 64-bit value.
        */
        fprintf(h, "v%u, 0x%x", dc.vA, dc.vB);
        break;
    case FMT23x:       // op vAA, vBB, vCC
        fprintf(h, "v%u, v%u, v%u", dc.vA, dc.vB, dc.vC);
        break;
    case FMT22b:       // op vAA, vBB, #+CC
        fprintf(h, "v%u, v%u, %d", dc.vA, dc.vB, dc.vC);
        break;
    case FMT22s:       // op vA, vB, #+CCCC
        fprintf(h, "v%u, v%u, %d", dc.vA, dc.vB, dc.vC);
        break;
    case FMT22t:       // op vA, vB, +CCCC
        fprintf(h, "v%u, v%u, %d(%d)", dc.vA, dc.vB, dc.vC, dc.vC + ofst);
        break;
    case FMT22c:       // op vA, vB, thing@CCCC
        if (dc.opc == DX_INSTANCE_OF ||
            dc.opc == DX_NEW_ARRAY) {
            fprintf(h, "v%u, v%u, %s(%d)", dc.vA, dc.vB,
                                        get_type_name(dc.vC),
                                        dc.vC);
        } else {
            //iput|iget
            fprintf(h, "v%u, v%u, %s::%s(%d)", dc.vA, dc.vB,
                                get_class_name_by_field_id(dc.vC),
                                get_field_name(dc.vC),
                                dc.vC);
        }
        break;
    case FMT30t:       // op +AAAAAAAA
        fprintf(h, "%d(%d)", dc.vA, dc.vA + ofst);
        break;
    case FMT31t:       // op vAA, +BBBBBBBB
        fprintf(h, "v%u, %d(relofst)(%d)", dc.vA, dc.vB, dc.vB + ofst);
        break;
    case FMT31c:       // op vAA, string@BBBBBBBB
        fprintf(h, "v%u, %s(%d)", dc.vA, get_string(dc.vB), dc.vB);
        break;
    case FMT32x:       // op vAAAA, vBBBB
        fprintf(h, "v%u, v%u", dc.vA, dc.vB);
        break;
    case FMT31i:       // op vAA, #+BBBBBBBB
        fprintf(h, "v%u, %d", dc.vA, dc.vB);
        break;
    case FMT35c:       // op {vC, vD, vE, vF, vG}, thing@BBBB
        {
            if (dc.opc == DX_FILL_ARRAY_DATA) {
                fprintf(h, "%s, {", get_type_name(dc.vB));
            } else {
                //Invoke.
                fprintf(h, "%s::%s, {",
                            get_class_name_by_method_id(dc.vB),
                            get_method_name(dc.vB));
            }
            for (INT i = 0; i < dc.vA; i++) {
                fprintf(h, "v%u", dc.arg[i]);
                if (i != dc.vA - 1) {
                    fprintf(h, ",");
                }
            }
            fprintf(h, "}");
        }
        break;
    case FMT3rc:       // op {vCCCC .. v(CCCC+AA-1)}, meth@BBBB
        //The argument count is always in vA, and the
        //method idx is always in vB, and the first vReg in vC.
        fprintf(h, "%s::%s, {",
                    get_class_name_by_method_id(dc.vB),
                    get_method_name(dc.vB));
        for (INT i = 0; i < dc.vA; i++) {
            fprintf(h, "v%u", dc.vC + i);
            if (i != dc.vA - 1) {
                fprintf(h, ",");
            }
        }
        fprintf(h, "}");
        break;
    case FMT51l:       // op vAA, #+BBBBBBBBBBBBBBBB
        fprintf(h, "v%u, %llu", dc.vA, dc.vB_wide);
        break;
    default:
        ASSERTN(0, ("Unknown dex format"));
        return;
    } //end switch
    fflush(h);
}


//Disassemble DEX instruction.
void DX_MGR::decode_dx(USHORT const* cptr, IN OUT DXC & dc)
{
    USHORT insn = *cptr;
    dc.opc = (DX_OPC)(insn & 0xFF);
    switch (DX_fmt(dc.opc)) {
    case FMT10x:       // op
        /* nothing to do; copy the AA bits out for the verifier */
        dc.vA = INST_AA(insn);
        break;
    case FMT12x:       // op vA, vB
        dc.vA = INST_A(insn);
        dc.vB = INST_B(insn);
        break;
    case FMT11n:       // op vA, #+B
        dc.vA = INST_A(insn);
        dc.vB = (INT)(INST_B(insn) << 28) >> 28;  // sign extend 4-bit value
        break;
    case FMT11x:       // op vAA
        dc.vA = INST_AA(insn);
        break;
    case FMT10t:       // op +AA, GOTO
        dc.vA = (CHAR)INST_AA(insn); // sign-extend 8-bit value
        break;
    case FMT20t:       // op +AAAA
        dc.vA = (SHORT)FETCH(1); // sign-extend 16-bit value
        break;
    case FMT21c:       // op vAA, thing@BBBB
    case FMT22x:       // op vAA, vBBBB
        dc.vA = INST_AA(insn);
        dc.vB = FETCH(1);
        break;
    case FMT21s:       // op vAA, #+BBBB
    case FMT21t:       // op vAA, +BBBB
        dc.vA = INST_AA(insn);
        dc.vB = (SHORT)FETCH(1); // sign-extend 16-bit value
        break;
    case FMT21h:       // op vAA, #+BBBB0000[00000000]
        dc.vA = INST_AA(insn);
        /*
        The value should be treated as right-zero-extended, but we don't
        actually do that here. Among other things, we don't know if it's
        the top bits of a 32- or 64-bit value.
        */
        dc.vB = FETCH(1);
        dc.vB_wide = dc.vB;
        if (dc.opc == DX_CONST_WIDE_HIGH16) {
            // op vAA, #+BBBB000000000000
            dc.vB = 0;
            dc.vB_wide <<= 48;
        } else {
            // op vAA, #+BBBB0000
            dc.vB <<= 16;
            dc.vB_wide <<= 16;
        }
        break;
    case FMT23x:       // op vAA, vBB, vCC
        dc.vA = INST_AA(insn);
        dc.vB = FETCH(1) & 0xff;
        dc.vC = FETCH(1) >> 8;
        break;
    case FMT22b:       // op vAA, vBB, #+CC
        dc.vA = INST_AA(insn);
        dc.vB = FETCH(1) & 0xff;
        dc.vC = (CHAR)(FETCH(1) >> 8);            // sign-extend 8-bit value
        break;
    case FMT22s:       // op vA, vB, #+CCCC
    case FMT22t:       // op vA, vB, +CCCC
        dc.vA = INST_A(insn);
        dc.vB = INST_B(insn);
        dc.vC = (SHORT)FETCH(1);                   // sign-extend 16-bit value
        break;
    case FMT22c:       // op vA, vB, thing@CCCC
        dc.vA = INST_A(insn);
        dc.vB = INST_B(insn);
        dc.vC = FETCH(1);
        break;
    case FMT30t:       // op +AAAAAAAA
        dc.vA = FETCH_uint32(1);                     // signed 32-bit value
        break;
    case FMT31t:       // op vAA, +BBBBBBBB packed-switch/sparse-switch
    case FMT31c:       // op vAA, string@BBBBBBBB
        dc.vA = INST_AA(insn);
        dc.vB = FETCH_uint32(1);                     // 32-bit value
        break;
    case FMT32x:       // op vAAAA, vBBBB
        dc.vA = FETCH(1);
        dc.vB = FETCH(2);
        break;
    case FMT31i:       // op vAA, #+BBBBBBBB
        dc.vA = INST_AA(insn);
        dc.vB = FETCH_uint32(1);                     // signed 32-bit value
        break;
    case FMT35c:       // op {vC, vD, vE, vF, vG}, thing@BBBB
        {
            /*
            Note that the fields mentioned in the spec don't appear in
            their "usual" positions here compared to most formats.
            This was done so that the field names for the argument count and
            reference index match between this format and the corresponding
            range formats (3rc etc).

            Bottom line: The argument count is always in vA, and the
            method constant (or equivalent) is always in vB.
            */
            USHORT reglist;
            INT count;

            dc.vA = INST_B(insn); //vA is argument count.
            dc.vB = FETCH(1); //vB is method idx.
            reglist = FETCH(2);

            /*
            Copy the argument registers into the arg[] array, and
            also copy the first argument (if any) into vC.
            */
            ASSERT0(dc.vA <= DX_PCOUNT);
            switch (dc.vA) {
            case 5: dc.arg[4] = INST_A(insn);
            case 4: dc.arg[3] = (reglist >> 12) & 0x0f;
            case 3: dc.arg[2] = (reglist >> 8) & 0x0f;
            case 2: dc.arg[1] = (reglist >> 4) & 0x0f;
            case 1: dc.vC = dc.arg[0] = reglist & 0x0f; break;
            case 0: break;
            default: ASSERTN(0, ("Invalid arg count"));
            }
        }
        break;
    case FMT3rc:       // op {vCCCC .. v(CCCC+AA-1)}, meth@BBBB
        //INVOKE_XXX_RANGE
        dc.vA = INST_AA(insn); //vA is argument count.
        dc.vB = FETCH(1); //vB is method idx.
        dc.vC = FETCH(2); //vC is the first parameter.
        if (dc.vA >= DX_PCOUNT) {
            //Only record the first parameter in arg[0].
            dc.arg[0] = dc.vC;
            for (USHORT i = 1; i < DX_PCOUNT; i++) {
                dc.arg[i] = -1;
            }
        } else {
            for (USHORT i = 0; i < DX_PCOUNT; i++) {
                dc.arg[i] = dc.vC + i;
            }
        }
        break;
    case FMT51l:       // op vAA, #+BBBBBBBBBBBBBBBB
        dc.vA = INST_AA(insn);
        dc.vB_wide = FETCH_uint32(1) | ((ULONGLONG)FETCH_uint32(3) << 32);
        break;
    default:
        ASSERTN(0, ("Unknown dex format"));
    } //end switch
}


/*
Extract the dex insn info and class and method
name from code_item.
'cptr': code buffer pointer.
'cs': code size. It records the number of USHORT.
*/
void DX_MGR::extract_dxinfo(OUT DX_INFO & dxinfo, USHORT const* cptr, UINT cs,
                            UINT const* class_def_idx, UINT const* method_idx)
{
    //Record start address of code-section.
    DXI_start_cptr(&dxinfo) = cptr;

    //Record end address of code-section.
    DXI_end_cptr(&dxinfo) = cptr + cs;

    //Walk through DEX instructions of 'code_item'.
    USHORT const* sptr = DXI_start_cptr(&dxinfo);
    USHORT const* eptr = DXI_end_cptr(&dxinfo);

    //Count up insns.
    UINT i = 0;
    DXC dc;
    while (sptr < eptr) {
        if (is_inline_data(sptr)) {
            break;
        }
        decode_dx(sptr, dc);
        sptr += DX_sz(dc.opc);
        i++;
    }
    DXI_num_of_op(&dxinfo) = i;

    //class definition.
    if (class_def_idx == NULL) {
        DXI_class_name(&dxinfo) = "NO Class Declaration Name";
    } else {
        DXI_class_name(&dxinfo) =
            get_class_name_by_declaration_id(*class_def_idx);
    }

    //method definition.
    if (method_idx == NULL) {
        DXI_method_name(&dxinfo) = "NO Method Name";
    } else {
        DXI_method_name(&dxinfo) = get_method_name(*method_idx);
        if (class_def_idx == NULL) {
            DXI_class_name(&dxinfo) = get_class_name_by_method_id(*method_idx);
        }
    }
}


