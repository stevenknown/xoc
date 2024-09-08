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

#ifndef _MC_DWARF_H_
#define _MC_DWARF_H_

namespace xoc {

//**********************************************************
//File Name: dwarf.h
//
//File Description:
//This header file contains various classes and functions
//used for handling DWARF debugging information.
//DWARF is a format used for debugging data, stored in
//the binary files of programs. This debugging information
//includes details such as variables, type definitions,
//functions, and source code line numbers.
//DWARF information is critical for debugging tools to
//provide meaningful insights during the software development process.
//
//Background Knowledge:
//- DWARF (Debug With Arbitrary Record Format):
//  DWARF is a standardized debugging information format
//  widely used in many compilers and debuggers. It allows
//  the debugger to understand the structure of a program,
//  the locations of variables, type information, and
//  source code line numbers. This provides detailed debugging
//  information during the debugging process, enabling developers
//  to diagnose and fix issues in their code effectively.
//  The DWARF format is highly extensible and supports complex
//  programming constructs such as inlined functions and
//  optimized code paths.
//
//- ELF (Executable and Linkable Format):
//  ELF is a common file format for executable files, object
//  code, shared libraries, and core dumps. DWARF debugging
//  information is usually embedded within ELF files to provide
//  debugging support. ELF files are divided into sections, each
//  containing different types of information needed for execution
//  or debugging. For example, the `.text` section typically contains
//  the executable code, while the `.debug_info` section contains
//  DWARF debugging information.
//
//Terminology Explanation:
//- MC (Machine Code):
//  MC stands for "Machine Code" and refers to the intermediate
//  layer in the compiler related to generating and handling
//  machine code. The MC layer is responsible for translating
//  high-level language code into low-level machine code instructions.
//  This layer handles tasks such as instruction selection,
//  register allocation, and instruction scheduling.
//
//- MCSymbol:
//  MCSymbol is a class in the MC layer representing a symbol
//  in the program (e.g., variable names or function names).
//  During machine code generation, these symbols need to be
//  resolved and processed to generate correct machine code instructions.
//  Symbols are essential for linking, as they allow different parts
//  of a program to reference each other.
//
//- MCFixup:
//  MCFixup is a class in the MC layer representing a fixup,
//  an adjustment needed during machine code generation.
//  For example, the operands of certain instructions may need
//  to be fixed up during the linking stage. Fixups are used to
//  handle references to symbols whose addresses are not known
//  until link time.
//
//- MCFixupKind:
//  MCFixupKind represents the kind of fixup operation, usually
//  including various data sizes such as 1 byte, 2 bytes, 4 bytes, etc.
//  Different kinds of fixups are needed to handle different types
//  of relocations, such as absolute addresses, relative addresses,
//  and offsets.
//
//- MCSection:
//  MCSection represents a section in a binary file, such as
//  the code section, data section, etc. Different sections
//  contain different types of data, and the compiler and linker
//  place code and data into the appropriate sections. For example,
//  the `.data` section typically contains initialized global variables,
//  while the `.bss` section contains uninitialized global variables.
//
//- DW_LNE:
//  DW_LNE stands for "DWARF Line Number Extended," indicating
//  extended line number program opcodes. It is used in the
//  line number program to perform extended operations, such as
//  setting the current address. These opcodes extend the capabilities
//  of the standard line number program, allowing for more complex
//  debugging information to be encoded.
//
//- DW_LNE_set_address (0x02):
//  This is a specific `DW_LNE` opcode with a value of `0x02`.
//  It is used to set the current address (usually the program
//  counter) in the line number program, allowing the debugger
//  to map source code line numbers to the correct machine code address.
//  This opcode is essential for accurately associating source code
//  lines with their corresponding machine code instructions.
//
//- CFA (Canonical Frame Address):
//  CFA stands for "Canonical Frame Address," representing the base
//  address of a stack frame. It is a reference point used to
//  calculate the locations of other local variables and saved
//  registers within the stack frame. The CFA is crucial for
//  unwinding the stack during exceptions and for providing
//  backtrace information during debugging.
//
//- CFI (Call Frame Information):
//  CFI stands for "Call Frame Information," representing the
//  information about a call stack frame. It includes details
//  needed to restore the program's execution state, such as
//  the locations of saved registers and local variables.
//  CFI is crucial during exception handling and debugging to
//  reconstruct the call stack. It ensures that a debugger can
//  accurately trace the flow of a program, even across function calls
//  and through complex control flow structures.
//
//Main Contents of This File:
//- Defines various classes and functions needed to handle DWARF
//  debugging information. These include classes for representing
//  debugging symbols, line number information, and frame descriptions.
//- Includes definitions and methods for symbols, fixups, sections, etc.
//- Provides utility functions for generating and parsing DWARF information.
//  These utilities handle the encoding and decoding of DWARF data,
//  making it easier to generate and interpret debugging information.
//
//Notes:
//- The classes and functions in this file are designed for internal
//  use by compilers and linkers. They provide the infrastructure needed
//  to generate and process DWARF debugging information, ensuring that
//  the debugging tools can provide accurate and detailed insights
//  into the program's execution.
//- Using these classes and functions requires a certain level of
//  knowledge about compiler implementation and machine code generation.
//  It is important to understand how high-level language constructs
//  are translated into machine code and how debugging information
//  is generated and used.
//
//Intended Audience:
//- This file is intended for developers with a certain level of
//  understanding of compiler and linker implementation. It is
//  particularly useful for those working on developing or maintaining
//  compilers, linkers, and debugging tools.
//- If you are a beginner in compiler implementation, it is recommended
//  to first study the basic principles of compilers and the fundamentals
//  of machine code generation. Understanding the overall architecture
//  of a compiler, including the front-end, middle-end, and back-end,
//  will provide a solid foundation for working with the classes and
//  functions defined in this file.
//**********************************************************/
class DwarfResMgr;

//This is the alignment value of debug section, aligned to 1.
#define DEBUG_SECTION_ALIGN_ONE 1

//This is the alignment value of debug section, aligned to 8.
#define DEBUG_SECTION_ALIGN_EIGHT 8

//This is the offset size of the variable type
//mcsymbol on the stack. We fix it to be 8 bytes.
#define MCSYMBOL_VAR_STACK_OFF_SIZE 8
#define DWARF2_LINE_DEFAULT_IS_STMT 1
#define DWARF2_FLAG_IS_STMT (1 << 0)
#define DWARF2_FLAG_BASIC_BLOCK (1 << 1)
#define DWARF2_FLAG_PROLOGUE_END (1 << 2)
#define DWARF2_FLAG_EPILOGUE_BEGIN (1 << 3)

#define MCSYMBOL_END_FLAG "end"
#define MCSYMBOL_START_FLAG "begin"

//This represents the maximum value for a 64-bit signed integer (INT64_MAX).
#define DW_LNE_END_SEQUENCE_FLAG 0x7FFFFFFFFFFFFFFFLL
#define UNIQUE_VAR_COUNT 1
#define DWARF_MAX_OPCODE 255
#define DWARF_OPCODE_OVERFLOW_THRESHOLD 256

//Number of initial CFI (Call Frame Information) instructions:
//- DW_CFA_def_cfa: r30 ofs 0
//- DW_CFA_same_value: r15
//- DW_CFA_same_value: r26
#define INITIAL_CFI_INSTRUCTION_COUNT 3

//Fixups are symbols that are undetermined
//at the beginning of parsing and need to be further resolved
//until the end when they can be determined. We fill them in at the end,
//and if they are still undetermined by then,
//it means we need to generate final relocation entries.
//The following are the types of relocations.
//Currently, there are only a few scenarios, and more will be added later.
typedef enum tagMCFixupKind {
    FK_NONE = 0, //A no-op fixup.
    FK_DATA_1,   //A one-byte fixup.
    FK_DATA_2,   //A two-byte fixup.
    FK_DATA_4,   //A four-byte fixup.
    FK_DATA_8,   //A eight-byte fixup.
} MCFixupKind;

//This is a symbol used to record frontend and
//debugging-related information,
//which will be passed as context in the future.
#define MCSYMBOL_is_registered(sym)  ((sym)->m_is_registered)
#define MCSYMBOL_region(sym)         ((sym)->m_region)
#define MCSYMBOL_region_offset(sym)  ((sym)->u2.m_region_offset)
#define MCSYMBOL_value(sym)          ((sym)->u2.m_value)
#define MCSYMBOL_label(sym)          ((sym)->u1.m_label)
#define MCSYMBOL_var(sym)            ((sym)->u1.m_var)
#define MCSYMBOL_type(sym)           ((sym)->m_type)
class MCSymbol {
    COPY_CONSTRUCTOR(MCSymbol);
public:
    typedef enum tagSymbolType {
        NONE,

        //The label representing a function cannot be fully registered
        //at the frontend syntax level.
        //it needs to follow the IR to the MI level to be registered
        //completely.
        FUNC_LABEL,

        //This label is a label within a section
        //and it can be fully registered directly.
        SECTION_LABEL,

        //This represents a variable within a function similar to FUNC_LABEL.
        //It needs to descend with the IR until reaching
        //the MI level to register this symbol properly.
        FUNC_VAR
    } SymbolType;
    SymbolType m_type;
    bool m_is_registered;

    //Who owns this symbol.
    Region const* m_region;

    //For attributes FUNC_LABEL and SECTION_LABEL,
    //they both correspond to m_label, while for attribute FUNC_VAR,
    //it is m_var.
    union {
        LabelInfo const* m_label;
        Var * m_var;
    } u1;
    union {
        UINT m_region_offset;
        CHAR m_value[MCSYMBOL_VAR_STACK_OFF_SIZE];
    } u2;
public:
    MCSymbol() {}
    MCSymbol(bool is_registered, Region const* region, UINT region_offset,
             LabelInfo const* label) :
        m_is_registered(is_registered), m_region(region) {
        u1.m_label = label;
        u2.m_region_offset = region_offset;
    }
};

typedef TMap<Region const*, MCSymbol*> Region2MCSymbol;
typedef xcom::Vector<BYTE> BYTEVec;
typedef xcom::TMap<Sym const*, MCSymbol*> Sym2MCSymbol;
typedef xcom::TMap<Sym const*, Region2MCSymbol*> Sym2Region2MCSymbol;
typedef xcom::TMapIter<Region const*, MCSymbol*> Region2MCSymbolIter;

//The base class for symbol expressions,
//which fixups will use, currently only
//has three types: Binary, SymbolRef, and CONSTANT.
#define MCEXPR_kind(exp)  ((exp)->m_kind)
class MCExpr {
public:
    enum ExprKind : UINT8 {
        BINARY, //Binary expressions.
        SYMBOLREF, //References to labels and assigned expressions.
        CONSTANT, //CONSTANT expressions.
    };
    ExprKind m_kind;
    static INT64 evaluateAsAbsolute(MCExpr const* expr);
protected:
    explicit MCExpr(ExprKind kind) : m_kind(kind) {
        ASSERTN(m_kind == BINARY || m_kind == SYMBOLREF || m_kind == CONSTANT,
                ("m_kind must be either Binary or SYMBOLREF \
                  or CONSTANT"));
    }
};


//This is a reference expression used
//for referencing a specific location in a section.
#define MCSYMBOLREFEXPR_mc_symbol(symbol_ref) \
    (((MCSymbolRefExpr*)symbol_ref)->m_mc_symbol)
class MCSymbolRefExpr : public MCExpr {
public:
    MCSymbol const* m_mc_symbol;
public:
    explicit MCSymbolRefExpr(MCSymbol const* mc_symbol):
        MCExpr(MCExpr::SYMBOLREF), m_mc_symbol(mc_symbol) {
        ASSERT0(m_mc_symbol);
    }
};


//Represent a constant integer expression.
#define MCCONSTANTEXPR_value(e) ((e)->m_value)
class MCConstantExpr : public MCExpr {
public:
    INT64 m_value;
public:
    explicit MCConstantExpr(INT64 value):
        MCExpr(MCExpr::CONSTANT), m_value(value) {}
};


//This is an operation between two references within a section.
#define MCBINARYEXPR_lhs(e)     ((e)->m_lhs)
#define MCBINARYEXPR_rhs(e)     ((e)->m_rhs)
#define MCBINARYEXPR_opcode(e)  ((e)->m_opcode_type)
class MCBinaryExpr : public MCExpr {
public:
    enum Opcode {
        ADD,  //Addition.
        AND,  //Bitwise and.
        DIV,  //Signed division.
        EQ,   //Equality comparison.

        //Signed greater than comparison
        //(result is either 0 or some target-specific non-zero value).
        GT,

        //Signed greater than or equal comparison
        //(result is either 0 or some target-specific non-zero value).
        GTE,
        LAND, //Logical and.
        LOR,  //Logical or.

        //Signed less than comparison
        //(result is either 0 or some target-specific non-zero value).
        LT,

        //Signed less than or equal comparison
        //(result is either 0 or some target-specific non-zero value).
        LTE,
        MUL,  //Multiplication.
        NE,   //Inequality comparison.
        OR,   //Bitwise or.
        SHL,  //Shift left.
        ASHR, //Arithmetic shift right.
        LSHR, //Logical shift right.
        SUB,  //Subtraction.
        XOR   //Bitwise exclusive or.
    };
    MCExpr const* m_lhs;
    MCExpr const* m_rhs;
    Opcode m_opcode_type;
public:
    explicit MCBinaryExpr(Opcode op, MCExpr const* lhs, MCExpr const* rhs):
        MCExpr(MCExpr::BINARY), m_lhs(lhs), m_rhs(rhs), m_opcode_type(op) {
        ASSERT0(m_lhs && m_rhs);
    }
};


//Fixups are symbols that are undetermined
//at the beginning of parsing and need to be further resolved
//until the end when they can be determined. We fill them in at the end,
//and if they are still undetermined by then,
//it means we need to generate final relocation entries.
//The following are the types of relocations.
//Currently, there are only a few scenarios, and more will be added later.
#define MCFIXUP_offset(e)  ((e)->m_offset)
#define MCFIXUP_value(e)   ((e)->m_value)
#define MCFIXUP_kind(e)    ((e)->m_kind)
class MCFixup {
    friend class MCDwarfMgr;
public:
    //The target of the reference is the offset located in the source's
    //section.
    UINT m_offset;

    //The value of the reference target will need to be modified in the future.
    MCExpr const* m_value;
    MCFixupKind m_kind;
public:
    MCFixup(UINT offset, MCExpr const* value, MCFixupKind kind):
        m_offset(offset), m_value(value), m_kind(kind) {
        ASSERT0(m_value);
    }
};


typedef xcom::Vector<MCFixup*> FixupVec;

//For debug line
//MCDwarfFile is a data structure related to the debug_line section,
//used to store file information.
struct MCDwarfFile {
public:
    //The base name of the file without its directory path.
    StrBuf const* m_name;

    //The index into the list of directory names for this file name.
    UINT m_dir_index;
public:
    MCDwarfFile() : m_name(nullptr), m_dir_index(0) {}
};


//For debug line
#define DWARF_LINE_OPCODE_BASE 13
#define DWARF_LINE_BASE -5
#define DWARF_LINE_RANGE 14

struct MCDwarfLineTableParams {
public:
    //First special line opcode - leave room for the standard opcodes.
    //Note: If you want to change this, you'll have to update the
    //"StandardOpcodeLengths" table that is emitted in
    //Emit(), DWARF_LINE_OPCODE_BASE is a fixed value for line_op_base.
    UINT8 m_dwarf2_line_opcode_base;

    //Minimum line offset in a special line info. opcode.
    //DWARF_LINE_BASE was chosen to give a reasonable range of values.
    INT8 m_dwarf2_line_base;

    //Range of line offsets in a special line info. opcode.
    //DWARF_LINE_RANGE is a fixed value for line_range.
    UINT8 m_dwarf2_line_range;
public:
    MCDwarfLineTableParams() :
        m_dwarf2_line_opcode_base(DWARF_LINE_OPCODE_BASE),
        m_dwarf2_line_base(DWARF_LINE_BASE),
        m_dwarf2_line_range(DWARF_LINE_RANGE) {}
};


//This is to record debug_line-related location information,
//which is the frontend user's location information,
//for example, 1.c, 1.cpp.
#define MCDWARFLOC_file_index(e)  ((e)->m_file_index)
#define MCDWARFLOC_line(e)        ((e)->m_line)
#define MCDWARFLOC_column(e)      ((e)->m_column)
#define MCDWARFLOC_flags(e)       ((e)->m_flags)
class MCDwarfLoc {
public:
    UINT32 m_file_index;
    UINT32 m_line;
    UINT16 m_column;
    UINT8 m_flags;
public:
    MCDwarfLoc() : m_file_index(0), m_line(0), m_column(0), m_flags(0) {}
    MCDwarfLoc(MCDwarfLoc const& other):
        m_file_index(other.m_file_index), m_line(other.m_line),
        m_column(other.m_column) {}
};


//This class will additionally record a label,
//through which the corresponding relationship
//between location information and underlying binary can be obtained.
#define MCDWARFLINEENTRY_label(e)  ((e)->m_label)
class MCDwarfLineEntry : public MCDwarfLoc {
public:
    MCSymbol const* m_label;
public:
    MCDwarfLineEntry(MCSymbol const* label) : m_label(label) {}
};


//Call frame instruction encodings.
#define DW_CFA_nop 0x00
#define DW_CFA_advance_loc 0x40
#define DW_CFA_offset 0x80
#define DW_CFA_restore 0xc0
#define DW_CFA_set_loc 0x01
#define DW_CFA_advance_loc1 0x02
#define DW_CFA_advance_loc2 0x03
#define DW_CFA_advance_loc4 0x04
#define DW_CFA_offset_extended 0x05
#define DW_CFA_restore_extended 0x06
#define DW_CFA_undefined 0x07
#define DW_CFA_same_value 0x08
#define DW_CFA_register 0x09
#define DW_CFA_remember_state 0x0a
#define DW_CFA_restore_state 0x0b
#define DW_CFA_def_cfa 0x0c
#define DW_CFA_def_cfa_register 0x0d
#define DW_CFA_def_cfa_offset 0x0e

#define DW_LNS_const_add_pc 0x08
#define DW_LNS_advance_pc 0x02
#define DW_LNS_extended_op 0x00
#define DW_LNE_end_sequence 0x01
#define DW_LNS_advance_line 0x03
#define DW_LNS_copy 0x01
#define DW_LNS_set_file 0x04
#define DW_LNS_set_column 0x05

#define DW_LNE_set_discriminator 0x04
#define DW_LNS_set_isa 0x0c
#define DW_LNS_negate_stmt 0x06
#define DW_LNS_prologue_end 0x0a
#define DW_LNS_set_basic_block 0x07
#define DW_LNS_set_prologue_end 0x0a
#define DW_LNS_set_epilogue_begin 0x0b
#define DW_LNE_set_address 0x02

//The following is related to the debug_frame.
#define MCCFIINSTRUCTION_operation(e)  ((e)->m_operation)
#define MCCFIINSTRUCTION_label(e)      ((e)->m_label)
#define MCCFIINSTRUCTION_register(e)   ((e)->m_register)
#define MCCFIINSTRUCTION_offset(e)     ((e)->m_offset)
#define MCCFIINSTRUCTION_register2(e)  ((e)->m_register2)
class MCCFIInstruction {
public:
    enum OpType {
        OPSAMEVALUE,
        OPREMEMBERSTATE ,
        OPRESTORESTATE,
        OPOFFSET,
        OPDEFCFAREGISTER,
        OPDEFCFAOFFSET,
        OPDEFCFA,
        OPRELOFFSET,
        OPADJUSTCFAOFFSET,
        OPESCAPE,
        OPRESTORE,
        OPUNDEFINED,
        OPREGISTER,
        OPWINDOWSAVE,
        OPNEGATERASTATE,
        OPGNUARGSSIZE
    };

    OpType m_operation;
    MCSymbol const* m_label;
    UINT m_register;
    union {
        INT m_offset;
        UINT m_register2;
    };
public:
    MCCFIInstruction(OpType op, MCSymbol const* l, UINT r, INT o):
        m_operation(op), m_label(l), m_register(r), m_offset(o) {
        ASSERT0(op != OPREGISTER);
    }

    MCCFIInstruction(OpType op, MCSymbol const* l, UINT r1, UINT r2):
        m_operation(op), m_label(l), m_register(r1), m_register2(r2) {
        ASSERT0(op == OPREGISTER);
    }

    OpType getOperation() const { return m_operation; }
    MCSymbol const* getLabel() const { return m_label; }

    UINT getRegister() const {
        ASSERT0(m_operation == OPDEFCFA || m_operation == OPOFFSET ||
                m_operation == OPRESTORE || m_operation == OPUNDEFINED ||
                m_operation == OPSAMEVALUE ||
                m_operation == OPDEFCFAREGISTER ||
                m_operation == OPRELOFFSET || m_operation == OPREGISTER);
        return m_register;
    }

    INT getOffset() const {
        ASSERT0(m_operation == OPDEFCFA || m_operation == OPOFFSET ||
                m_operation == OPRELOFFSET ||
                m_operation == OPDEFCFAOFFSET ||
                m_operation == OPADJUSTCFAOFFSET ||
                m_operation == OPGNUARGSSIZE);
        return m_offset;
    }
};

typedef Vector<MCCFIInstruction*> MCCFIInstructionVec;

//This contains various CFI (Call Frame Information)
//information corresponding to each function.
//In the future, it will be used to generate stack information
//for each function. For example,
//through these, I can know the information about
//the SP (Stack Pointer) of this stack at the moment it was running.
struct MCDwarfFrameRegionInfo {
public:
    MCSymbol const* m_begin;
    MCSymbol const* m_end;
    MCCFIInstructionVec * m_instructions;
    UINT m_ra_reg;
    UINT m_fp_reg;
    UINT m_sp_reg;
public:
    //Constructor with parameters for initialization.
    MCDwarfFrameRegionInfo(MCSymbol const* begin = nullptr,
        MCSymbol const* end = nullptr,
        MCCFIInstructionVec * instructions = nullptr, UINT ra_reg = 0,
        UINT fp_reg = 0, UINT sp_reg = 0) :
        m_begin(begin), m_end(end), m_instructions(instructions),
        m_ra_reg(ra_reg), m_fp_reg(fp_reg), m_sp_reg(sp_reg) {}
};

typedef xcom::Vector<MCDwarfLineEntry*> MCDwarfLineEntryVec;
typedef xcom::TMap<Region*, MCDwarfLineEntryVec*> Region2MCDwarfLineEntryVec;
typedef xcom::TMapIter<Region*, MCDwarfLineEntryVec*>
    Region2MCDwarfLineEntryVecIter;
typedef xcom::TMap<Region const*, MCDwarfFrameRegionInfo*>
    Region2MCDwarfFrameRegionInfo;
typedef TMap<Region*, Vector<LabelInfo const*>*> Region2LabelInfoVec;

//START DwarfResMgr
//Responsible for managing resources related to DWARF.
class DwarfResMgr {
    COPY_CONSTRUCTOR(DwarfResMgr);
    SMemPool * m_pool;
public:
    List<StrBuf*> m_str_mgr;
    List<Region2MCSymbol*> m_map_region_mc_symbol_mgr;
    List<MCDwarfFrameRegionInfo*> m_region_frame_info_mgr;
    List<MCCFIInstructionVec*> m_cfi_info_vector_mgr;
    List<MCDwarfLineEntryVec*> m_line_entry_mgr;
    List<Vector<LabelInfo const*>*> m_ret_hint_map_region_info_mgr;
public:
    DwarfResMgr() { m_pool = nullptr; init(); }
    ~DwarfResMgr() { destroy(); }
    MCExpr const* allocMCBinaryExpr(MCBinaryExpr::Opcode op, MCExpr const* lhs,
                                    MCExpr const* rhs);
    MCExpr const* allocMCSymbolRefExpr(MCSymbol const* mc_symbol);
    MCExpr const* allocMCConstantExpr(INT64 value);

    MCFixup * allocFixup(UINT offset, MCExpr const* value,
                         MCFixupKind kind);
    MCSymbol * allocMCSymbol();

    //Allocate memory for CFI (Call Frame Information).
    //.cfi_def_cfa defines a rule for computing CFA as: take address from
    //Register and add Offset to it.
    MCCFIInstruction * allocCFIDefCfa(MCSymbol const* l, UINT r,
                                      INT offset);

    //.cfi_same_value Current value of Register is the same as in the
    //previous frame. I.e., no restoration is needed.
    MCCFIInstruction * allocSameValue(MCSymbol const* l, UINT r);

    //.cfi_offset Previous value of Register is saved at offset from CFA.
    MCCFIInstruction * allocOffset(MCSymbol const* l, UINT r, INT o);

    //.cfi_restore says that the rule for Register is now the same as it
    //was at the beginning of the function,
    //after all initial instructions added
    //by .cfi_startproc were executed.
    MCCFIInstruction * allocRestore(MCSymbol const* l, UINT r);

    //.cfi_def_cfa_offset modifies a rule for computing CFA. Register
    //remains the same, but offset is new.
    //Note that it is the absolute offset
    //that will be added to a defined register to the compute CFA address.
    MCCFIInstruction * allocDefCfaOffset(MCSymbol const* l, INT o);

    //Allocate memory for each line_entry in debug_line.
    MCDwarfLineEntry * allocLineEntry(MCSymbol const* l);

    //Allocate dwarf directory.
    xcom::StrBuf * allocStr(UINT init_size);

    //Allocate var vector.
    Region2MCSymbol * allocMapRegionVar();

    //Allocate cfi info vector.
    MCCFIInstructionVec * allocCFIInfoVector();

    //Allocate frame region info.
    MCDwarfFrameRegionInfo * allocFrameRegionInfo();

    //Allocate line entry vector.
    MCDwarfLineEntryVec * allocLineEntryVector();

    //Allocate label vector.
    Vector<LabelInfo const*> * allocLabelVector();
    void init();
    void * xmalloc(size_t size);
    void destroy();
};


#define MCDWARFMGR_debug_abbrev_code(e)    ((e)->m_debug_abbrev_code)
#define MCDWARFMGR_debug_info_code(e)      ((e)->m_debug_info_code)
#define MCDWARFMGR_debug_info_fixups(e)    ((e)->m_debug_info_fixups)
#define MCDWARFMGR_debug_ranges_code(e)    ((e)->m_debug_ranges_code)
#define MCDWARFMGR_debug_ranges_fixups(e)  ((e)->m_debug_rangse_fixups)
#define MCDWARFMGR_debug_str_code(e)       ((e)->m_debug_str_code)
#define MCDWARFMGR_region_frame_info(e)    ((e)->m_region_frame_info)
#define MCDWARFMGR_debug_frame_code(e)     ((e)->m_debug_frame_code)
#define MCDWARFMGR_debug_loc_code(e)       ((e)->m_debug_loc_code)
#define MCDWARFMGR_debug_frame_fixups(e) \
    ((e)->m_debug_frame_fixups)
#define MCDWARFMGR_region_line_info(e)     ((e)->m_region_line_info)
#define MCDWARFMGR_debug_line_code(e)      ((e)->m_debug_line_code)
#define MCDWARFMGR_debug_line_fixups(e)    ((e)->m_debug_line_fixups)
#define MCDWARFMGR_mc_dwarf_dirs(e)        ((e)->m_mc_dwarf_dirs)
#define MCDWARFMGR_mc_dwarf_files(e)       ((e)->m_mc_dwarf_files)
#define MCDWARFMGR_dwarf_res_mgr(e)        ((e)->m_dwarf_res_mgr)
#define MCDWARFMGR_char_out_stream(e)      ((e)->m_output_char_buffer)
#define MCDWARFMGR_byte_out_stream(e)      ((e)->m_output_byte_buffer)
#define MCDWARFMGR_ret_after_hint_map_region(e) \
    ((e)->m_ret_hint_map_region)
class MCDwarfMgr {
protected:
    //For var relocation.
    UINT m_cur_cfa;
    INT m_cur_cfa_offset;

    //The minimum alignment size of stack slots for callee-saved
    //registers in bytes.When generating the .debug_frame,
    //it is calculated differently based on the architecture.
    INT m_stack_slot_alignment;

    //Temporary buffer for encoding data with CHAR elements.
    //Used to mitigate the overhead of constantly using temporary variables.
    Vector<CHAR> m_output_char_buffer;

    //Temporary buffer for encoding data with BYTE (UINT8) elements.
    //Used to mitigate the overhead of constantly using temporary variables.
    Vector<BYTE> m_output_byte_buffer;
public:
    //For debug_line
    Vector<StrBuf const*> m_mc_dwarf_dirs;
    Vector<MCDwarfFile> m_mc_dwarf_files;
    MCDwarfLineTableParams m_dwarf_line_para;

    //Future MCSymbol records for types FUNC_LABEL
    //and SECTION_LABEL will be stored here.
    Sym2MCSymbol m_map_symbol;

    //Future MCSymbol records for types FUNC_VAR will be stored here.
    Sym2Region2MCSymbol m_map_symbol_var;

    //debug_abbrev
    BYTEVec m_debug_abbrev_code;

    //debug_info
    BYTEVec m_debug_info_code;

    FixupVec m_debug_info_fixups;

    //debug_ranges
    BYTEVec m_debug_ranges_code;
    FixupVec m_debug_rangse_fixups;

    //debug_str
    BYTEVec m_debug_str_code;

    //debug_loc
    BYTEVec m_debug_loc_code;

    //debug_frame
    //The code for all regions in the current scope.
    BYTEVec m_debug_frame_code;
    Region2MCDwarfFrameRegionInfo m_region_frame_info;
    FixupVec m_debug_frame_fixups;

    //debug_line
    //The code for all regions in the current scope.
    Region2MCDwarfLineEntryVec m_region_line_info;
    BYTEVec m_debug_line_code;
    FixupVec m_debug_line_fixups;

    //Allocate memory for some objects of the Dwarf class,
    //keeping in mind that it has no copy constructor.
    DwarfResMgr m_dwarf_res_mgr;

    //Used to record the hint following the final in each region,
    //e.g:
    // .func .visible foo39PERSON_stPi()
    // {
    //     call memcpy ($_reg_rd0)
    //     ret;
    //     .hint Ltmp1:
    //     .hint Lfunc_end0:
    // }
    //It can be seen that the hint appears at the end of this region,
    //which is used to record the PC value after the 'ret' instruction.
    //However, during the compilation process,
    //these two hints may be optimized away.
    //Since we need to use them, we will record them during lexical parsing,
    //and then add them back in the Machine Instruction (MI) layer.
    Region2LabelInfoVec m_ret_hint_map_region;
public:
    MCDwarfMgr();
    virtual ~MCDwarfMgr();
    void destroy();

    //Encode the value according to its name
    //and place it into the encoding container for the corresponding section.
    //This interface only handles debug sections that will be relocated.
    //The encoding follows little-endian rules.
    //Parameter Explanation:
    //value: The value we want to encode.
    //name: The section where we want to place the value.
    //count: The number of bytes to encode.
    void appendBytes(UINT64 value, CHAR const* name, UINT count);

    //Assistive appendBytes function.
    //Parameter Explanation:
    //vec: The destination vector.
    //value: The value we want to encode.
    //count: The number of bytes to encode.
    inline static void appendBytesFromValue(
        BYTEVec & vec, UINT64 value, UINT count)
    {
        ASSERT0(count <= sizeof(value));
        vec.append((BYTE*)(&value), count);
    }

    //Create an unregistered label for type of func_label
    //Because many pieces of information about the
    //function's label are unknown at the frontend syntax level
    //it needs to be passed to the backend at the end of the pass.
    void createMCSymbol(Region const* region, LabelInfo * label);

    //Create a registered label for vector
    //Currently, symbols are preserved in three ways:
    //1 one is directly stored in a vector,
    //2 another is stored in a TMap<Sym const*, MCSymbol*>,
    //3 the third is stored in a TMap<Sym const*, Vector<MCSymbol*>*>.

    //Parameter description:
    //region: The region to which it belongs.
    //region_offset: Its offset within this region.
    //label: Additional information accompanying this mcsynbo,
    //such as its name.
    MCSymbol const* createVectorMCSymbol(Region const* region,
                                         UINT region_offset,
                                         LabelInfo const* label);

    //Create a unregistered label for vector, no return value needed.
    //Parameter description:
    //region: The region to which it belongs.
    //label: Additional information accompanying this mcsynbo,
    //such as its name.
    MCSymbol * createVectorMCSymbol(Region const* region,
                                    LabelInfo const* label);

    //Create a registered label for SECTION_LABEL.
    //Parameter description:
    //region: The region to which it belongs.
    //label: Additional information accompanying this mcsynbo,
    //such as its name.
    void createMCSymbol(Region const* region, UINT region_offset,
                        LabelInfo const* label);

    //Create a registered label when updating label information.
    //Parameter description:
    //region: The region to which it belongs.
    //label: Additional information accompanying this mcsynbo,
    //such as its name.
    //is_registered: Has registration been completed?
    //all symbol information has been transparently provided.
    void createMCSymbol(Region const* region, LabelInfo const* label,
                        bool is_registered = true);

    //Create a single reference relocation.
    //In other words, when a MCSymbol exists within
    //a particular region or section,
    //and it's referenced within the same region or another section,
    //we need to create a single reference relocation to record this process.
    //For example, let's say we have a MCSymbol "Lfunc_begin0"
    //within the function foo39PERSON_stPi,
    //and it's referenced in the debug_info section.
    //Parameter explanation:
    //name: The symbol to be referenced.
    //region_name: The region where the single reference will be generated.
    //kind: The type of single reference.
    void createSingleRefRel(Sym const* name, CHAR const* region_name,
                            MCFixupKind kind);

    //Create a binary expression reference.
    //Similar to a single reference
    //For instance, in the debug_info section,
    //there are two MCSymbols: "Ldebug_info_end0" and "Ldebug_info_start0".
    //When referenced in debug_info,
    //such as "Ldebug_info_end0 - Ldebug_info_start0",
    //we compute the difference between the two symbols.
    //Parameter explanation:
    //name0: The first MCSymbol.
    //name1: The second MCSymbol.
    //region_name: The region to which they belong.
    //op_type: The type of operation.
    //kind: The type of dual reference.
    void createBinaryExprRef(Sym const* name0, Sym const* name1,
                             CHAR const* region_name,
                             MCBinaryExpr::Opcode op_type, MCFixupKind kind);

    //Create a reference to the variable "var".
    //For local variables,
    //we reference the variable in the debug_info
    //and record its belonging region,
    //type, and offset within debug_info.
    //When parsing at the frontend, it remains a variable,
    //and we only know it's on the stack,
    //but not its position relative to CFA (Canonical Frame Address).
    //Hence, here we record it, and in the backend MI phase,
    //we'll fill in its offset relative to CFA.
    //Parameter explanation:
    //func_region: The region where the variable resides.
    //var_name: The name of the variable.
    //region_name: The name of the region where the variable reference
    //is to be created, for example, if the variable belongs to funcA()
    //and we reference it in the debug_section,
    //the region_name would be passed as debug_section.
    //kind: The type of variable reference. Currently,
    //all variables are of 8-byte type.
    void createVarRefRel(Region const* func_region, Sym const* var_name,
                         CHAR const* region_name, MCFixupKind kind);

    //Create or update a var symbol
    //The "var" symbol currently only records global and local variables,
    //In the future, their locations will be recorded at the backend.
    void createVarOrUpdateSymbol(Region const* region, Var * var);

    //Please refer to the detailed explanation of the
    //function createVarOrUpdateSymbol().
    void createNewVarSymbol(Region const* region, Var * var, Sym const* name);

    //This function is for encoding the debug_line section.
    //Encode based on the line delta and address (PC)
    //of each location information generated by the frontend,
    //and finally put the corresponding binary encoding in the OS.
    //For more detailed operations, please refer to the function details,
    //where there are detailed comments.
    //Parameter explanation:
    //line_delta: The difference between two lines,
    //for example, the difference between line 6 and line 3 is 6-3=3.
    //addr_delta: The difference in addresses corresponding to two loc_symbols.
    //For example, if line 6 corresponds to address 10 and line 3 corresponds
    //to address 5, then their difference is 10-5=5.
    //os: The binary value after encoding.
    void encode(INT64 line_delta, UINT64 addr_delta, OUT Vector<CHAR> & os);

    //This function is an auxiliary function for genAdvanceLoc,
    //for a more comprehensive explanation, please refer to genAdvanceLoc.
    //used to encode address variations for the debug_frame section.
    //Parameter Explanation:
    //os: The binary code returned by the encoding.
    //addr_delta: The difference inPC values
    //corresponding to two CFI instructions.
    void encodeAdvanceLoc(OUT Vector<BYTE> & os, UINT64 addr_delta);

    //Retrieves the region object corresponding to
    //the specified symbol name from the region manager.
    Region const* findRegionByName(Sym const* name,
                                   xoc::RegionMgr * region_mgr);

    //Get the MCSymbol for this region from the vector in the map.
    MCSymbol * getVarSymbolInRegion(Sym const* name,
                                    Region const* region) const;

    //Get the starting MCSymbol for this function.
    MCSymbol const* getMCSymbolRegionStart(Region const* region) const;

    //Get the ending MCSymbol for this function.
    MCSymbol const* getMCSymbolRegionEnd(Region const* region) const;

    //This is to obtain the offset multiplier factor from the
    //CFI instruction's offset. This value will be passed to DWARF
    //for future use.
    INT getCFIInstOffsetOffByFactor(const MCCFIInstruction * ins) const;

    //Generate code for debug_frame.
    //The process mainly consists of the following two steps:
    //1 Generate a Common Information Entry (CIE) as the header for
    //the debug_frame section of the current file.
    //This CIE is generated only once per file,
    //even if there are multiple functions within the file.
    //Detailed explanations will be provided
    //in the genFrameCIE() function below.
    //2 Generate the Frame Description Entry (FDE) for each function
    //in this file. Detailed explanations for the FDE
    //are provided in the genFrameFDE() function.
    void genFrameBinary();

    //Generate CIE (Common Information Entry) for debug_frame.
    //The CIE specifies the version and
    //the current CFA (Canonical Frame Address) for the frame stack
    //of each function.
    //e.g:
    //Version:               1
    //Augmentation:          ""
    //Code alignment factor: 1
    //Data alignment factor: -8
    //Return address column: 26
    //DW_CFA_def_cfa: r30 ofs 0
    //DW_CFA_same_value: r26
    //DW_CFA_same_value: r15
    MCSymbol const* genFrameCIE(Region * region_ptr);

    //Generate code for each FDE (Frame Description Entry).
    //FDE describes the stack changes for each function in the file,
    //such as the current CFA (Canonical Frame Address),
    //old FP (Frame Pointer), RA (Return Address), etc.
    //e.g:
    //00000018 0000000000000044 00000000 FDE
    //cie=00000000 pc=0000000000000000..0000000000000b58
    //DW_CFA_def_cfa: r30 ofs 0
    //DW_CFA_same_value: r15
    //DW_CFA_same_value: r26
    //DW_CFA_advance_loc: 12 to 000000000000000c
    //DW_CFA_def_cfa: r30 ofs 4400
    //DW_CFA_advance_loc: 24 to 0000000000000024
    //DW_CFA_offset: r15 at cfa-8
    //DW_CFA_advance_loc: 28 to 0000000000000040
    //DW_CFA_def_cfa: r15 ofs 0
    //DW_CFA_advance_loc: 40 to 0000000000000068
    //DW_CFA_offset: r26 at cfa-16
    //DW_CFA_advance_loc2: 2924 to 0000000000000bd4
    //DW_CFA_def_cfa: r30 ofs 4400
    //DW_CFA_advance_loc2: 2936 to 000000000000174c
    //DW_CFA_restore: r15
    //DW_CFA_advance_loc2: 2948 to 00000000000022d0
    //DW_CFA_restore: r26
    //DW_CFA_advance_loc2: 2952 to 0000000000002e58
    //DW_CFA_def_cfa_offset: 0
    void genFrameFDE(MCSymbol const* cie_mc_symbol,
                     MCDwarfFrameRegionInfo * region_frame_info,
                     Region * region);

    //Encode various CFI (Call Frame Information) instructions.
    void genCFIInstructions(MCDwarfFrameRegionInfo * region_frame_info);

    //Encode various CFI (Call Frame Information) instruction.
    void genCFIInstruction(MCCFIInstruction * ins);

    //This function is for encoding the difference in PC
    //values between two CFI instructions for the debug_frame section.
    //In the future, dwarf instructions will be generated based on addr_delta,
    //such as DW_CFA_advance 128.
    //This indicates that in the future, GDB will know that the
    //program counter (PC) has advanced by 128.
    //Parameter Explanation:
    //addr_delta: This represents the difference in PC values
    //between two CFI instructions.
    //For example, if cfi0.pc = 100 and cfi1.pc = 50,
    //then their difference is 100 - 50 = 50.
    void genAdvanceLoc(UINT64 addr_delta);

    //Encode the value using ULEB128 encoding,
    //then place the encoded binary into dst.
    void genULEB128AndToV(INT32 value, OUT Vector<BYTE> & dst);

    //Generate code for the debug_line section.
    //The process mainly consists of the following two steps:
    //1 Generate the header for the debug_line section.
    //For detailed explanations, refer to the genLineHeader() function.
    //2 Generate the line table for each function in every file.
    //For detailed explanations, please refer to the
    //genDwarfLineTable() function.
    void genLineBinary();

    //This is the header for the debug_line section,
    //which primarily records the following information.
    //This header is generated only once.
    //1 The size of the debug_line section in bytes,
    //including the size of the debug_line header.
    //2 The version of the DWARF.
    //3 The directory of the current file and the paths
    //to the header files it includes.
    //e.g: .debug_line contents:
    //debug_line[0x00000000]
    //Line table prologue:
    //total_length: 0x000002d7
    //format: DWARF32
    //version: 2
    //prologue_length: 0x00000272
    //include_directories[  1] = "/usr/include"
    //include_directories[  2] = "/opt/rh/devtoolset-7/root/7/bits"
    //include_directories[  3] = "/usr/include/bits"
    //file_names[  1]:
    //      name: "stdlib.h"
    //      dir_index: 1
    //      mod_time: 0x00000000
    //      length: 0x00000000
    //file_names[  2]:
    //      name: "std_abs.h"
    //      dir_index: 2
    //      mod_time: 0x00000000
    //      length: 0x00000000
    //file_names[  3]:
    //      name: "mathcalls.h"
    //      dir_index: 3
    //      mod_time: 0x00000000
    //      length: 0x00000000
    MCSymbol * genLineHeader(Region * region_ptr);

    //Generate the file path for the current scope for debug_line.
    //Detailed explanations can be found in genLineHeader().
    void genV2FileDirTables();

    //Generate final code for each loc information
    //to connect frontend and backend loc information
    //e.g: Below, you can see how we will obtain
    //the correspondence between skipping a few lines
    //in the high-level file and stepping through assembly code.
    //Line Number Statements:
    //[0x0000027c]  Extended opcode 2: set Address to 0x0
    //[0x00000287]  Advance Line by 28 to 29
    //[0x00000289]  Copy
    //[0x0000028a]  Set column to 7
    //[0x0000028c]  Advance PC by 128 to 0x80
    //[0x0000028f]  Special opcode 6: advance Address by 0
    //to 0x80 and Line by 1 to 30
    void genDwarfLineTable(MCDwarfLineEntryVec * line_entry_v);

    //This is for encoding the line table
    //(mapping between line numbers and assembly PC) in the debug_line section.
    //e.g: here is a sample func_line_table:
    //    Address            Line   Column File   ISA Discriminator Flags
    //------------------ ------ ------ ------ --- ------------- -------------
    //0x0000000000000000      5      0     10   0             0  is_stmt
    //0x0000000000000024      6      7     10   0             0  is_stmt
    //0x0000000000000028      6      9     10   0             0
    //0x000000000000002c      6      5     10   0             0
    //0x0000000000000030      7      7     10   0             0  is_stmt
    //0x0000000000000034      7      9     10   0             0
    //0x0000000000000040      7      5     10   0             0
    //0x0000000000000044      8      4     10   0             0  is_stmt
    //0x0000000000000050      9      4     10   0             0  is_stmt
    //0x0000000000000070     10      1     10   0             0  is_stmt
    //0x0000000000000074     10      1     10   0             0  is_stmt
    //end_sequence

    //Parameter Explanation:
    //line_delta: The difference between two line numbers.
    //last_label: The MCSymbol corresponding to the previous location.
    //Label: The MCSymbol corresponding to the current location.
    //pointer_size: The size of the pointer.
    void genDwarfAdvanceLineAddr(INT64 line_delta, MCSymbol const* last_label,
                                 MCSymbol const* label, UINT pointer_size);

    //This is for encoding the first line of the line table,
    //such as the first line of the func_line_table above.
    void genDwarfSetLineAddr(INT64 line_delta, MCSymbol const* label,
                             UINT pointer_size);

    //Return the size of various types of fixups.
    //Currently, only sizes 1, 2, 4, and 8 are supported.
    inline static UINT getSizeForFixupKind(MCFixupKind kind)
    {
        switch (kind) {
        case FK_DATA_1:
            return 1;
        case FK_DATA_2:
            return 2;
        case FK_DATA_4:
            return 4;
        case FK_DATA_8:
            return 8;
        default: UNREACHABLE(); return 1;
        }
    }

    //Returns the region object that corresponds to the specified
    //name from the region manager. If no region with the given name exists,
    //the function may return nullptr.
    static Region const* getRegionByName(RegionMgr * rm, CHAR const* name);

    //Please refer to the detailed explanation
    //in the function createSingleRefRel().
    //Consider only global variables, not stack variables.
    //Stack variables are handled in the handle4Or8ByteArrow() function.
    //One important thing to note is that global variables can belong
    //to any region, while stack variables must belong to a specific region.
    void handleGlobalVarSymSingleRefRel(Sym const* name,
                                        CHAR const* region_name,
                                        MCFixupKind kind);

    //Please refer to the detailed explanation
    //in the function createSingleRefRel().
    void handleFuncAndSectionLabelSingleRefRel(Sym const* name,
        CHAR const* region_name, MCFixupKind kind);

    //Represents that there is no function region in the current file.
    bool isNullRegion();

    //Assistive appendBytes function.
    //Parameter Explanation:
    //vec: The destination vector.
    //value: The value we want to encode.
    //count: The number of bytes to encode.
    //offset: Offset of the destination vector.
    static void overwriteBytesAtOffset(BYTEVec & vec, UINT64 value,
                                       UINT count, UINT offset);

    //Create or retrieve a MCSymbol based on its name.
    MCSymbol * getOrCreateSymbol(Sym const* name1);

    //The following are functions related to debugging lines.
    //Record the filename and file number passed from the frontend
    //e.g: frontend .file 3 "/usr/include/stdlib.h"
    //then the number 3 and the path "/usr/include/stdlib.h" are separated.
    void recordFileInfo(UINT32 file_num, xoc::Sym const* all_file_name);

    //This symbol of a certain variable type must exist,
    //meaning it needs to be present in m_map_symbol_var
    //and match with the corresponding region.
    //This is a general interface.
    bool isVarSymbolFound(Sym const* name, Region const* region);

    //This symbol of a certain variable type must exist,
    //meaning it needs to be present in m_map_symbol_var
    //and match with the corresponding region.
    bool isVarSymbolInVector(Sym const* name, Region const* region);

    //This is an interface where the symbol
    //needs to be present in m_map_symbol_var,
    //regardless of whether the same symbol exists in different regions.
    bool isVarSymbolPresent(Sym const* name);

    //For find symbol only for FUNC_LABEL,SECTION_LABEL,FUNC_VAR.
    inline bool isSymbolfind(Sym const* name)
    {
        return m_map_symbol.find(name);
    }

    //This is for encoding the difference in line numbers and
    //PC values between every two locations.
    //Detailed explanations and examples can
    //be found in genDwarfAdvanceLineAddr().
    void genLineAddr(INT64 line_delta, UINT64 addr_delta);

    //After the stack variable has been allocated an address,
    //we will encode it. In other words,
    //based on the offset of a certain variable relative to the CFAs,
    //where our current CFA is the address of
    //the frame pointer after stack allocation,
    //the encoded value will definitely be a negative number.
    void setVarFinalValue(INT64 fp_offset, Var const* var, UINT fp,
                          Region const* region);

    //Set function label offset
    //We need to know the final PC for some function labels.
    //Parameter Explanation:
    //offset: Offset of the function label's program counter.
    //name: Name of the function labe
    void setFuncLabelOff(UINT offset, Sym const* name);

    //Update the offset of symbols in the ELF stage.
    //The symbols at this stage include global variables and static variables,
    //but not stack variables.
    void setStageElfSymbol(Sym const* name, UINT offset);

    //Set a special starting tag for the label of function.
    //Such as the starting label and ending label of the region.
    void setSpecialTagLabel(LabelInfo * label, MCSymbol * mc_symbol_ptr);

    //The minimum alignment size of stack slots for callee-saved
    //registers in bytes.When generating the .debug_frame,
    //it is calculated differently based on the architecture.
    void setStackSlotAlignment(Region const* region);

    //Update the current CFA position to track changes in the current stack.
    //it is related to the debug_frame.
    void updateCfa(UINT cur_cfa, INT cur_cfa_offset);

    //Please refer to the detailed explanation of the
    //function createVarOrUpdateSymbol().
    void updateExistingVarSymbol(Region const* region,
                                 Var * var, Sym const* name);
};

} // namespace xoc

#endif // MC_DWARF_H
