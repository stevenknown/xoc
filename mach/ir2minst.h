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

namespace elf {
class ELFMgr;
}

namespace mach {

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
#define IMCTX_label_num(cont) ((cont)->m_label_num)
#define IMCTX_cfi_num(cont) ((cont)->m_cfi_num)
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

    //Record total number of MI_label instructions generated by
    //convertBBLabel() interface. This number will be used to adjust relocation
    //offset by addRelocation() interface since MI_label instructions will not
    //be added in the final assemble instructions.
    UINT m_label_num;

    //Record the number of CFI instructions in the debug_frame section.
    UINT m_cfi_num;
public:
    IMCtx()
    {
        ::memset((void*)&u1, 0, sizeof(u1));;
        ::memset((void*)&u2, 0, sizeof(u2));;
        micode = MI_UNDEF;
        m_label_num = 0;
        m_cfi_num = 0;
    }
    IMCtx(IMCtx const& src) { clean(); copy_topdown(src); }
    IMCtx const& operator = (IMCtx const&);
    virtual ~IMCtx() {}

    virtual void clean()
    {
        ::memset((void*)&u1, 0, sizeof(u1));;
        ::memset((void*)&u2, 0, sizeof(u2));;
        micode = MI_UNDEF;
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
};
//END IMCtx


class IR2MInst {
    COPY_CONSTRUCTOR(IR2MInst);
protected:
    Region * m_rg; //Current region.
    TypeMgr * m_tm; //Data manager.
    MInstMgr * m_mimgr;
    elf::ELFMgr * m_em;
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
    IR2MInst(Region * rg, MInstMgr * mgr, elf::ELFMgr * em);
    virtual ~IR2MInst() {}

    virtual void convertLabel(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont);
    virtual void convertBBLabel(IRBB const* bb, OUT RecycMIList & mis,
                                MOD IMCtx * cont);
    virtual void convertStorePR(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertStoreVar(IR const* ir, OUT RecycMIList & mis,
                                 MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertIStoreVar(IR const* ir, OUT RecycMIList & mis,
                                  MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
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
                             MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertIgoto(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertTruebr(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertFalsebr(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont);
    virtual void convertReturn(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertCall(IR const* ir, OUT RecycMIList & mis,
                             MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertICall(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont)
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void convertExtract(IR const* ir, OUT RecycMIList & mis,
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
    virtual void convertXor(IR const* ir, OUT RecycMIList & mis,
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

    //The following are operations for converting
    //DWARF-related instructions to MI.

    //The basic format for both IR and MI is .cfi_def_cfa $reg,offset,
    //with the difference being the need to obtain
    //the PC at the MI layer, requiring knowledge of the current CFI's PC.
    virtual void convertCFIDefCfa(IR const* ir, OUT RecycMIList & mis,
                                  MOD IMCtx * cont);

    //Similar to the above, its format is .cfi_same_value $reg.
    virtual void convertCFISameValue(IR const* ir, OUT RecycMIList & mis,
                                     MOD IMCtx * cont);

    //Similar to the above, its format is .cfi_offset $reg ,offset.
    virtual void convertCFIOffset(IR const* ir, OUT RecycMIList & mis,
                                  MOD IMCtx * cont);

    //Similar to the above, its format is .cfi_restore $reg.
    virtual void convertCFIRestore(IR const* ir, OUT RecycMIList & mis,
                                   MOD IMCtx * cont);

    //Similar to the above, its format is .cfi_def_cfa_offset num.
    virtual void convertCFICfaOffset(IR const* ir, OUT RecycMIList & mis,
                                     MOD IMCtx * cont);

    void copyDbx(MInst * mi, IR const* ir, DbxMgr * dbx_mgr)
    {
        Dbx * d = ::getDbx(ir);
        if (d != nullptr) { MI_dbx(mi).copy(*d, dbx_mgr); }
    }

    //Translate IR in IRBB to a list of MInst.
    void convertToMIList(OUT RecycMIList & milst, MOD IMCtx * cont);
    virtual void convert(IR const* ir, OUT RecycMIList & mis,
                         MOD IMCtx * cont);

    MInstMgr * getMIMgr() const { return m_mimgr; }
    TypeMgr const* getTypeMgr() const { return m_tm; }
    RecycMIListMgr * getRecycMIListMgr() { return &m_recyc_orlist_mgr; }
    Region * getRegion() { return m_rg; }
    DbxMgr * getDbxMgr()
    {
        DbxMgr * dbx_mgr = getRegion()->getDbxMgr();
        ASSERT0(dbx_mgr);
        return dbx_mgr;
    }

    //Extract the constant value from 'val' that the size is conform to given
    //field type.
    //Note the function will truncate the val according the bit size of 'ft'.
    TMWORD extractImm(HOST_INT val, FIELD_TYPE ft);

    //Return target-machine-word for given register.
    //The tmword is always used in assembly or machine code generation.
    virtual TMWORD mapReg2TMCode(xgen::Reg r);

    //Register local variable that will be allocated in memory.
    Var * registerLocalVar(IR const* pr);

    void processRealParams(IR const* ir, OUT RecycMIList & mis,
                           MOD IMCtx * cont);

    //Process hint of after ret.
    void processHintOfAfterRet(OUT RecycMIList & mis, MOD IMCtx * cont);
};

} //namespace

#endif
