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
#include "../opt/cominc.h"
#include "dex.h"

BuiltInInfo g_builtin_info[] = {
    {BLTIN_UNDEF,               ""                  },
    {BLTIN_NEW,                 "#new"              },
    {BLTIN_NEW_ARRAY,           "#new_array"        },
    {BLTIN_MOVE_EXP,            "#move_exception"   },
    {BLTIN_MOVE_RES,            "#move_result"      },
    {BLTIN_THROW,               "#throw"            },
    {BLTIN_CHECK_CAST,          "#check_cast"       },
    {BLTIN_FILLED_NEW_ARRAY,    "#filled_new_array" },
    {BLTIN_FILL_ARRAY_DATA,     "#fill_array_data"  },
    {BLTIN_CONST_CLASS,         "#const_class"      },
    {BLTIN_ARRAY_LENGTH,        "#array_length"     },
    {BLTIN_MONITOR_ENTER,       "#monitor_enter"    },
    {BLTIN_MONITOR_EXIT,        "#monitor_exit"     },
    {BLTIN_INSTANCE_OF,         "#instance_of"      },
    {BLTIN_CMP_BIAS,            "#cmp_bias"         },
    {BLTIN_LAST,                ""                  },
};
UINT g_builtin_num = sizeof(g_builtin_info) / sizeof(g_builtin_info[0]);


//Perform Dex register allocation.
bool g_do_dex_ra = false;

//Set true to collect debug info.
bool g_collect_debuginfo = false;
bool g_dump_ir2dex = false;
bool g_dump_dex2ir = false;
bool g_dump_classdefs = false;
bool g_dump_lirs = false;
bool g_is_pretty_print_method_name = true;
bool g_dump_dex_file_path = false;
bool g_record_region_for_classs = false;
