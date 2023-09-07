/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#ifndef _IR2MINST_H_
#define _IR2MINST_H_

namespace mach {

//This structure records common information of symbols
//for different architectures, which is then used to generate
//relocation information and write elf files.
typedef struct {
    CHAR const* name; //Symbol name.
    UCHAR type:4;     //Symbol type(1:variable, 2:function. .eg).
    UINT64 index;     //Top-down index of current symbol in all.
    UINT64 size;      //Size(byte) of current symbol.
    UINT64 offset;    //Location(byte) of current symbol in generated mi list.
} SYMBOL;
//This structure saved all symbol info defined or used in current function
//region. Note that the first element of this vector always be the function
//symbol defined by current function region because of the top-down symbol
//info collection. For example:
//            void test0() {
//                global_var = 1; // global_var is a global variable
//                test1();        // test1() is a function
//            }
//SymVec will save three SYMBOL(test0, global_var and test1) in order, in
//which the test0 is defined by current function region, global_var and
//test1 is used by current function region.
typedef xcom::Vector<SYMBOL> SymVec;

//
//START IMCtx
//
//Defined IR to MInst convertion Context.
//Record information during convertion in between IR and MInst.
#define IMCTX_is_inverted(cont) ((cont)->u2.s1.is_inverted)
#define IMCTX_pred(cont) ((cont)->pred)
#define IMCTX_micode(cont) ((cont)->micode)
#define IMCTX_sr_vec(cont) ((cont)->reg_vec)
#define IMCTX_param_size(cont) ((cont)->u1.param_size)
#define IMCTX_mem_byte_size(cont) ((cont)->u1.mem_byte_size)
#define IMCTX_int_imm(cont) ((cont)->u1.int_imm)
#define IMCTX_symbol_vec(cont) ((cont)->sym_vec)
#define IMCTX_func_name(cont) ((cont)->func_name)
class IMCtx {
public:
    //Propagate info bottom up.
    //Used as a result, and record MI_CODE of result if exist.
    MI_CODE micode;
    union {
        //Propagate info top down.
        //Used as input parameter, record total size of real
        //parameters before a function call.
        UINT param_size;

        //Propagate info top down.
        //used as input parameter, record memory byte-size for operation.
        UINT mem_byte_size;

        //Propagate info top down.
        //used as input parameter, record integer literal.
        LONGLONG int_imm;
    } u1;
    union {
        struct {
            //Propagate info top down.
            //Used as output result, set by convertRelationOp(),
            //true if the relation operation inverted.
            UINT is_inverted:1;
        } s1;
        UINT u2val;
    } u2;

    //Propagate info top down.
    //Used to save info of symbols.
    //It's set by TECOIR2MInst::extractVarSymRelInfo() and
    //TECOIR2MInst::extractFuncSymRelInfo().
    //It's used by TECOMIGen class to construct info of symbols
    //writen to elf file.
    SymVec sym_vec;

    //Propagate info top down.
    //Used to save name of current function region.
    //It's set by TECOIR2MInst::adjustGlobalPointer() and used by
    //TECOMIGen class to construct section header name of elf file.
    CHAR const * func_name;
public:
    IMCtx()
    {
        ::memset(&u1, 0, sizeof(u1));;
        ::memset(&u2, 0, sizeof(u2));;
        micode = MI_UNDEF;

        //Init symbol vector.
        sym_vec.init();

        //Init function region name.
        func_name = nullptr;
    }
    IMCtx(IMCtx const& src) { clean(); copy_topdown(src); }
    IMCtx const& operator = (IMCtx const&);
    virtual ~IMCtx() {}

    virtual void clean()
    {
        ::memset(&u1, 0, sizeof(u1));;
        ::memset(&u2, 0, sizeof(u2));;
        micode = MI_UNDEF;

        //Init symbol vector.
        sym_vec.init();

        //Init function region name.
        func_name = nullptr;
    }
    virtual void copy_topdown(IMCtx const& src)
    {
        u1 = src.u1;
        u2 = src.u2;
    }
    virtual void copy_bottomup(IMCtx const& src)
    {
        set_micode(src.get_micode());
    }
    void clean_bottomup()
    {
        set_micode(MI_UNDEF);
    }

    UINT getMemByteSize() const { return IMCTX_mem_byte_size(this); }
    MI_CODE get_micode() const { return micode; }

    virtual void set_micode(MI_CODE ort) { micode = ort; }
public:
    void setFuncRegionName(CHAR const* name) { IMCTX_func_name(this) = name; }

    SYMBOL getSymbol(UINT index) { return IMCTX_symbol_vec(this)[index]; }

    CHAR const* getSymbolName(UINT index) { return getSymbol(index).name; }

    //The first element of sym_vec always be the function symbol defined
    //by current function region since the symbol info collection is top-down
    //and saving is in order.
    SYMBOL getDefinedSymbol() { return getSymbol(0); }

    void setDefinedSymbolSize(UINT64 val) {
        SYMBOL symbol = getDefinedSymbol();
        symbol.size = val;
        IMCTX_symbol_vec(this).set(0, symbol);
    }
};
//END IMCtx


class IR2MInst {
    COPY_CONSTRUCTOR(IR2MInst);
protected:
    Region * m_rg; //Current region.
    TypeMgr * m_tm; //Data manager.
    MInstMgr * m_mimgr;
    RecycMIListMgr m_recyc_orlist_mgr;
protected:
    void convertIRListToMIList(OUT RecycMIList & milst, MOD IMCtx * cont);
    void convertIRBBListToMIList(OUT RecycMIList & milst, MOD IMCtx * cont);

    //Load constant float value into register.
    void convertLoadConstFP(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont);

    //Load constant integer value into register.
    void convertLoadConstInt(HOST_INT constval, UINT constbytesize,
                             bool is_signed, Dbx const* dbx,
                             OUT RecycMIList & mis, MOD IMCtx * cont);

    //Load constant integer value into register.
    void convertLoadConstInt(IR const* ir, OUT RecycMIList & mis,
                             MOD IMCtx * cont);

    //Load constant boolean value into register.
    void convertLoadConstBool(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont);

    //Load constant string address into register.
    void convertLoadConstStr(IR const* ir, OUT RecycMIList & mis,
                             MOD IMCtx * cont);

    //Load constant value into register.
    void convertLoadConst(IR const* ir, OUT RecycMIList & mis,
                          MOD IMCtx * cont);

    //The function try extend loaded value to larger size when the loaded
    //value is passed through registers.
    void tryExtendLoadValByMemSize(bool is_signed, Dbx const* dbx,
                                   OUT RecycMIList & mis, MOD IMCtx * cont);
public:
    IR2MInst(Region * rg, MInstMgr * mgr);
    virtual ~IR2MInst() {}

    virtual void adjustGlobalPointer(OUT RecycMIList & milst,
                                     MOD IMCtx * cont) = 0;
    virtual void convertLabel(IR const* ir, OUT RecycMIList & mis);
    virtual void convertBBLabel(IRBB const* bb, OUT RecycMIList & mis);
    virtual void convertStorePR(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont) = 0;
    virtual void convertStoreVar(IR const* ir, OUT RecycMIList & mis,
                                 MOD IMCtx * cont) = 0;
    virtual void convertUnaryOp(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont);
    virtual void convertBinaryOp(IR const* ir, OUT RecycMIList & mis,
                                 MOD IMCtx * cont);
    ///Generate compare operations and return the comparation result registers.
    //The output registers in IMCtx are ResultSR,
    //TruePredicatedSR, FalsePredicatedSR.
    //The ResultSR record the boolean value of comparison of relation operation.
    virtual void convertRelationOp(IR const* ir, OUT RecycMIList & mis,
                                   MOD IMCtx * cont);
    virtual void convertGoto(IR const* ir, OUT RecycMIList & mis,
                             MOD IMCtx * cont) = 0;
    virtual void convertTruebr(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont) = 0;
    virtual void convertFalsebr(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont);
    virtual void convertReturn(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertCall(IR const* ir, OUT RecycMIList & mis,
                             MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertAdd(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_add(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertSub(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_sub(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertDiv(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_div(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertMul(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_mul(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertRem(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_rem(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertMod(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_mod(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }

    //Logical AND
    virtual void convertLogicalAnd(IR const* ir, OUT RecycMIList & mis,
                                   MOD IMCtx * cont)
    {
        ASSERTN(ir->is_land(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }

    //Logical OR
    virtual void convertLogicalOr(IR const* ir, OUT RecycMIList & mis,
                                  MOD IMCtx * cont)
    {
        ASSERTN(ir->is_lor(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }

    //Bitwise AND
    virtual void convertBitAnd(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont)
    {
        ASSERTN(ir->is_band(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }

    //Bitwise OR
    virtual void convertBitOr(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont)
    {
        ASSERTN(ir->is_bor(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertXOR(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_xor(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }

    //Bitwise NOT.
    //e.g BNOT(0x0001) = 0xFFFE
    virtual void convertBitNot(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont)
    {
        ASSERTN(ir->is_bnot(), ("illegal ir"));
        convertUnaryOp(ir, mis, cont);
    }

    //Boolean logical not.
    //e.g LNOT(non-zero) = 0, LNOT(0) = 1
    virtual void convertLogicalNot(IR const* ir, OUT RecycMIList & mis,
                                   MOD IMCtx * cont)
    {
        ASSERTN(ir->is_lnot(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertNeg(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
    {
        ASSERTN(ir->is_neg(), ("illegal ir"));
        convertBinaryOp(ir, mis, cont);
    }
    virtual void convertLT(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont)
    {
        ASSERTN(ir->is_lt(), ("illegal ir"));
        convertRelationOp(ir, mis, cont);
    }
    virtual void convertLE(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont)
    {
        ASSERTN(ir->is_le(), ("illegal ir"));
        convertRelationOp(ir, mis, cont);
    }
    virtual void convertGT(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont)
    {
        ASSERTN(ir->is_gt(), ("illegal ir"));
        convertRelationOp(ir, mis, cont);
    }
    virtual void convertGE(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont)
    {
        ASSERTN(ir->is_ge(), ("illegal ir"));
        convertRelationOp(ir, mis, cont);
    }
    virtual void convertEQ(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont)
    {
        ASSERTN(ir->is_eq(), ("illegal ir"));
        convertRelationOp(ir, mis, cont);
    }

    virtual void convertNE(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont)
    {
        ASSERTN(ir && ir->is_ne(), ("illegal ir"));
        convertRelationOp(ir, mis, cont);
    }
    //virtual void convertRegion(IR const* ir, OUT RecycMIList & mis,
    //                           MOD IMCtx * cont);
    virtual void convertSetElem(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont)
    {
        ASSERTN(0, ("Target Dependent Code"));
    }
    virtual void convertGetElem(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont)
    {
        ASSERTN(0, ("Target Dependent Code"));
    }
    virtual void convertExtStmt(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont)
    {
        ASSERTN(0, ("Target Dependent Code"));
    }
    void copyDbx(MInst * mi, IR const* ir)
    {
        Dbx * d = ::getDbx(ir);
        if (d != nullptr) { MI_dbx(mi).copy(*d); }
    }

    //Translate IR in IRBB to a list of MInst.
    void convertToMIList(OUT RecycMIList & milst, MOD IMCtx * cont);
    virtual void convert(IR const* ir, OUT RecycMIList & mis,
                         MOD IMCtx * cont);

    MInstMgr * getMIMgr() const { return m_mimgr; }
    TypeMgr const* getTypeMgr() const { return m_tm; }
    RecycMIListMgr * getRecycMIListMgr() { return &m_recyc_orlist_mgr; }

    //Extract the constant value from 'val' that the size is conform to given
    //field type.
    //Note the function will truncate the val according the bit size of 'ft'.
    TMWORD extractImm(HOST_INT val, FIELD_TYPE ft);

    //Return target-machine-word for given register.
    //The tmword is always used in assembly or machine code generation.
    TMWORD mapReg2TMCode(xgen::Reg r);

    //Register local variable that will be allocated in memory.
    Var * registerLocalVar(IR const* pr);

    void processRealParams(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont);
};

} //namespace

#endif