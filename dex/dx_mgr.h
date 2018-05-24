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
#ifndef _DX_UTIL_H_
#define _DX_UTIL_H_

typedef enum _DX_OPC {
    DX_NOP = 0,
    DX_MOVE,
    DX_MOVE_FROM16,
    DX_MOVE_16,
    DX_MOVE_WIDE,
    DX_MOVE_WIDE_FROM16,
    DX_MOVE_WIDE_16,
    DX_MOVE_OBJECT,
    DX_MOVE_OBJECT_FROM16,
    DX_MOVE_OBJECT_16,
    DX_MOVE_RESULT,
    DX_MOVE_RESULT_WIDE,
    DX_MOVE_RESULT_OBJECT,
    DX_MOVE_EXCEPTION,
    DX_RETURN_VOID,
    DX_RETURN,
    DX_RETURN_WIDE,
    DX_RETURN_OBJECT,
    DX_CONST_4,
    DX_CONST_16,
    DX_CONST,
    DX_CONST_HIGH16,
    DX_CONST_WIDE_16,
    DX_CONST_WIDE_32,
    DX_CONST_WIDE,
    DX_CONST_WIDE_HIGH16,
    DX_CONST_STRING,
    DX_CONST_STRING_JUMBO,
    DX_CONST_CLASS,
    DX_MONITOR_ENTER,
    DX_MONITOR_EXIT,
    DX_CHECK_CAST,
    DX_INSTANCE_OF,
    DX_ARRAY_LENGTH,
    DX_NEW_INSTANCE,
    DX_NEW_ARRAY,
    DX_FILLED_NEW_ARRAY,
    DX_FILLED_NEW_ARRAY_RANGE,
    DX_FILL_ARRAY_DATA,
    DX_THROW,
    DX_GOTO,
    DX_GOTO_16,
    DX_GOTO_32,
    DX_PACKED_SWITCH,
    DX_SPARSE_SWITCH,
    DX_CMPL_FLOAT,
    DX_CMPG_FLOAT,
    DX_CMPL_DOUBLE,
    DX_CMPG_DOUBLE,
    DX_CMP_LONG,
    DX_IF_EQ,
    DX_IF_NE,
    DX_IF_LT,
    DX_IF_GE,
    DX_IF_GT,
    DX_IF_LE,
    DX_IF_EQZ,
    DX_IF_NEZ,
    DX_IF_LTZ,
    DX_IF_GEZ,
    DX_IF_GTZ,
    DX_IF_LEZ,
    DX_UNUSED_3E,
    DX_UNUSED_3F,
    DX_UNUSED_40,
    DX_UNUSED_41,
    DX_UNUSED_42,
    DX_UNUSED_43,
    DX_AGET,
    DX_AGET_WIDE,
    DX_AGET_OBJECT,
    DX_AGET_BOOLEAN,
    DX_AGET_BYTE,
    DX_AGET_CHAR,
    DX_AGET_SHORT,
    DX_APUT,
    DX_APUT_WIDE,
    DX_APUT_OBJECT,
    DX_APUT_BOOLEAN,
    DX_APUT_BYTE,
    DX_APUT_CHAR,
    DX_APUT_SHORT,
    DX_IGET,
    DX_IGET_WIDE,
    DX_IGET_OBJECT,
    DX_IGET_BOOLEAN,
    DX_IGET_BYTE,
    DX_IGET_CHAR,
    DX_IGET_SHORT,
    DX_IPUT,
    DX_IPUT_WIDE,
    DX_IPUT_OBJECT,
    DX_IPUT_BOOLEAN,
    DX_IPUT_BYTE,
    DX_IPUT_CHAR,
    DX_IPUT_SHORT,
    DX_SGET,
    DX_SGET_WIDE,
    DX_SGET_OBJECT,
    DX_SGET_BOOLEAN,
    DX_SGET_BYTE,
    DX_SGET_CHAR,
    DX_SGET_SHORT,
    DX_SPUT,
    DX_SPUT_WIDE,
    DX_SPUT_OBJECT,
    DX_SPUT_BOOLEAN,
    DX_SPUT_BYTE,
    DX_SPUT_CHAR,
    DX_SPUT_SHORT,
    DX_INVOKE_VIRTUAL,
    DX_INVOKE_SUPER,
    DX_INVOKE_DIRECT,
    DX_INVOKE_STATIC,
    DX_INVOKE_INTERFACE,
    DX_RETURN_VOID_BARRIER,
    DX_INVOKE_VIRTUAL_RANGE,
    DX_INVOKE_SUPER_RANGE,
    DX_INVOKE_DIRECT_RANGE,
    DX_INVOKE_STATIC_RANGE,
    DX_INVOKE_INTERFACE_RANGE,
    DX_UNUSED_79,
    DX_UNUSED_7A,
    DX_NEG_INT,
    DX_NOT_INT,
    DX_NEG_LONG,
    DX_NOT_LONG,
    DX_NEG_FLOAT,
    DX_NEG_DOUBLE,
    DX_INT_TO_LONG,
    DX_INT_TO_FLOAT,
    DX_INT_TO_DOUBLE,
    DX_LONG_TO_INT,
    DX_LONG_TO_FLOAT,
    DX_LONG_TO_DOUBLE,
    DX_FLOAT_TO_INT,
    DX_FLOAT_TO_LONG,
    DX_FLOAT_TO_DOUBLE,
    DX_DOUBLE_TO_INT,
    DX_DOUBLE_TO_LONG,
    DX_DOUBLE_TO_FLOAT,
    DX_INT_TO_BYTE,
    DX_INT_TO_CHAR,
    DX_INT_TO_SHORT,
    DX_ADD_INT,
    DX_SUB_INT,
    DX_MUL_INT,
    DX_DIV_INT,
    DX_REM_INT,
    DX_AND_INT,
    DX_OR_INT,
    DX_XOR_INT,
    DX_SHL_INT,
    DX_SHR_INT,
    DX_USHR_INT,
    DX_ADD_LONG,
    DX_SUB_LONG,
    DX_MUL_LONG,
    DX_DIV_LONG,
    DX_REM_LONG,
    DX_AND_LONG,
    DX_OR_LONG,
    DX_XOR_LONG,
    DX_SHL_LONG,
    DX_SHR_LONG,
    DX_USHR_LONG,
    DX_ADD_FLOAT,
    DX_SUB_FLOAT,
    DX_MUL_FLOAT,
    DX_DIV_FLOAT,
    DX_REM_FLOAT,
    DX_ADD_DOUBLE,
    DX_SUB_DOUBLE,
    DX_MUL_DOUBLE,
    DX_DIV_DOUBLE,
    DX_REM_DOUBLE,
    DX_ADD_INT_2ADDR,
    DX_SUB_INT_2ADDR,
    DX_MUL_INT_2ADDR,
    DX_DIV_INT_2ADDR,
    DX_REM_INT_2ADDR,
    DX_AND_INT_2ADDR,
    DX_OR_INT_2ADDR,
    DX_XOR_INT_2ADDR,
    DX_SHL_INT_2ADDR,
    DX_SHR_INT_2ADDR,
    DX_USHR_INT_2ADDR,
    DX_ADD_LONG_2ADDR,
    DX_SUB_LONG_2ADDR,
    DX_MUL_LONG_2ADDR,
    DX_DIV_LONG_2ADDR,
    DX_REM_LONG_2ADDR,
    DX_AND_LONG_2ADDR,
    DX_OR_LONG_2ADDR,
    DX_XOR_LONG_2ADDR,
    DX_SHL_LONG_2ADDR,
    DX_SHR_LONG_2ADDR,
    DX_USHR_LONG_2ADDR,
    DX_ADD_FLOAT_2ADDR,
    DX_SUB_FLOAT_2ADDR,
    DX_MUL_FLOAT_2ADDR,
    DX_DIV_FLOAT_2ADDR,
    DX_REM_FLOAT_2ADDR,
    DX_ADD_DOUBLE_2ADDR,
    DX_SUB_DOUBLE_2ADDR,
    DX_MUL_DOUBLE_2ADDR,
    DX_DIV_DOUBLE_2ADDR,
    DX_REM_DOUBLE_2ADDR,
    DX_ADD_INT_LIT16,
    DX_RSUB_INT,
    DX_MUL_INT_LIT16,
    DX_DIV_INT_LIT16,
    DX_REM_INT_LIT16,
    DX_AND_INT_LIT16,
    DX_OR_INT_LIT16,
    DX_XOR_INT_LIT16,
    DX_ADD_INT_LIT8,
    DX_RSUB_INT_LIT8,
    DX_MUL_INT_LIT8,
    DX_DIV_INT_LIT8,
    DX_REM_INT_LIT8,
    DX_AND_INT_LIT8,
    DX_OR_INT_LIT8,
    DX_XOR_INT_LIT8,
    DX_SHL_INT_LIT8,
    DX_SHR_INT_LIT8,
    DX_USHR_INT_LIT8,
    DX_IGET_QUICK,
    DX_IGET_WIDE_QUICK,
    DX_IGET_OBJECT_QUICK,
    DX_IPUT_QUICK,
    DX_IPUT_WIDE_QUICK,
    DX_IPUT_OBJECT_QUICK,
    DX_INVOKE_VIRTUAL_QUICK,
    DX_INVOKE_VIRTUAL_RANGE_QUICK,
    DX_UNUSED_EB,
    DX_UNUSED_EC,
    DX_UNUSED_ED,
    DX_UNUSED_EE,
    DX_UNUSED_EF,
    DX_UNUSED_F0,
    DX_UNUSED_F1,
    DX_UNUSED_F2,
    DX_UNUSED_F3,
    DX_UNUSED_F4,
    DX_UNUSED_F5,
    DX_UNUSED_F6,
    DX_UNUSED_F7,
    DX_UNUSED_F8,
    DX_UNUSED_F9,
    DX_UNUSED_FA,
    DX_UNUSED_FB,
    DX_UNUSED_FC,
    DX_UNUSED_FD,
    DX_UNUSED_FE,
    DX_UNUSED_FF,
    DX_PACKED_SWITCH_PAYLOAD,
    DX_SPARSE_SWITCH_PAYLOAD,
} DX_OPC;


typedef enum _DX_OP_FMT {
    FMT10x = 0,  // op
    FMT12x,  // op vA, vB
    FMT11n,  // op vA, #+B
    FMT11x,  // op vAA
    FMT10t,  // op +AA
    FMT20t,  // op +AAAA
    FMT22x,  // op vAA, vBBBB
    FMT21t,  // op vAA, +BBBB
    FMT21s,  // op vAA, #+BBBB
    FMT21h,  // op vAA, #+BBBB00000[00000000]
    FMT21c,  // op vAA, thing@BBBB
    FMT23x,  // op vAA, vBB, vCC
    FMT22b,  // op vAA, vBB, #+CC
    FMT22t,  // op vA, vB, +CCCC
    FMT22s,  // op vA, vB, #+CCCC
    FMT22c,  // op vA, vB, thing@CCCC
    FMT32x,  // op vAAAA, vBBBB
    FMT30t,  // op +AAAAAAAA
    FMT31t,  // op vAA, +BBBBBBBB
    FMT31i,  // op vAA, #+BBBBBBBB
    FMT31c,  // op vAA, thing@BBBBBBBB
    FMT35c,  // op {vC, vD, vE, vF, vG}, thing@BBBB (B: count, A: vG)
    FMT3rc,  // op {vCCCC .. v(CCCC+AA-1)}, meth@BBBB
    FMT51l,  // op vAA, #+BBBBBBBBBBBBBBBB
    FMT00x,
} DX_OP_FMT;


#define DX_ATTR_UNDEF        0x00 //conditional or unconditional branch
#define DX_ATTR_BR            0x01 //conditional or unconditional branch
#define DX_ATTR_CONT        0x02 //flow can continue to next statement
#define DX_ATTR_SWITCH        0x04 //switch statement
#define DX_ATTR_THROW        0x08 //could cause an exception to be thrown
#define DX_ATTR_RET            0x10 //returns, no additional statements
#define DX_ATTR_CALL        0x20 //invoke
#define DX_ATTR_GOTO        0x40 //unconditional branch

#define DX_opc(d)            (g_dx_code_info[(d)].op_code_decimal)
#define DX_sz(d)            (g_dx_code_info[(d)].op_hword_size)
#define DX_name(d)            (g_dx_code_info[(d)].op_name)
#define DX_fmt(d)            (g_dx_code_info[(d)].fmt)
class DXC_INFO {
public:
    USHORT op_code_decimal;
    DX_OPC op_code;
    CHAR op_hword_size; //DEX instruction size in half-word.
    BYTE op_attr; //DEX instruction attribute.
    CHAR const* op_name;
    DX_OP_FMT fmt;
};


//Dex file info.
#define DXI_num_of_op(d)        ((d)->num_of_op)
#define DXI_num_of_class(d)        ((d)->num_of_class)
#define DXI_start_cptr(d)        ((d)->start_code_ptr)
#define DXI_end_cptr(d)            ((d)->end_code_ptr)
#define DXI_class_name(d)        ((d)->class_name)
#define DXI_method_name(d)        ((d)->method_name)
class DX_INFO {
public:
    DX_INFO()
    {
        num_of_op = 0;
        num_of_class = 0;
        start_code_ptr = NULL;
        end_code_ptr = NULL;
    }

    UINT num_of_op;
    UINT num_of_class;
    USHORT const* start_code_ptr;
    USHORT const* end_code_ptr;
    CHAR const* class_name;
    CHAR const* method_name;

    inline UINT get_num_of_op() const { return num_of_op; }
    inline UINT get_num_of_class() const { return num_of_class; }
};


#define DX_PCOUNT 5  //Parameter count
class DXC {
public:
    DXC() { ::memset(this, 0, sizeof(DXC)); }

    DX_OPC opc;
    INT vA;
    INT vB;
    LONGLONG vB_wide;
    INT vC;
    INT arg[DX_PCOUNT];
};


//Exported Functions
#define OBJ_MC_SIZE            16
#define PTR_ADDEND_TYPE        D_U32

//Exported Variable.
extern DXC_INFO const g_dx_code_info[];


/*
Since the low 8-bit in metadata may look like DX_NOP, we
need to check both the low and whole half-word
to determine whether it is code or data.

If instruction's low byte is NOP(0x0), and high byte is not zero,
it is the inline data.
*/
inline bool is_inline_data(USHORT const* ptr)
{
    USHORT data = *ptr;
    DX_OPC opcode = (DX_OPC)(data & 0x00ff);
    USHORT highbyte = data & 0xff00;
    if (opcode == DX_NOP && highbyte != 0) {
        //There are three kinds of inline data,
        //PACKED_SWITCH_DATA(0x100), SPARSE_SWITCH_DATA(0x200),
        //FILLED_ARRAY_DATA(0x300).
        return true;
    }
    return false;
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
class DX_MGR  {
public:
    virtual ~DX_MGR() {}
    void decode_dx(USHORT const* cptr, IN OUT DXC & dc);
    void dump_dx(DXC const& dc, FILE * h, INT ofst);
    void dump_method(IN DX_INFO const& dxinfo, IN FILE * h);
    virtual CHAR const* get_string(UINT str_idx) { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_type_name(UINT idx) { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_field_name(UINT field_idx)
    { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_method_name(UINT method_idx)
    { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_class_name(UINT class_type_idx)
    { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_class_name_by_method_id(UINT method_idx)
    { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_class_name_by_field_id(UINT field_idx)
    { UNREACHABLE(); return NULL; }
    virtual CHAR const* get_class_name_by_declaration_id(UINT cls_def_idx)
    { UNREACHABLE(); return NULL; }

    void extract_dxinfo(OUT DX_INFO & dxinfo, USHORT const* cptr, UINT cs,
                        UINT const* class_def_idx, UINT const* method_idx);
};
#endif

