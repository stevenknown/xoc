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
#ifndef __DEX_TO_IR_H__
#define __DEX_TO_IR_H__

//Map from LIR to LABEL.
typedef TMap<LIR*, List<LabelInfo*>*> LIR2LabelInfo;

class DexRegion;
class DexRegionMgr;

typedef enum _CMP_KIND {
    CMP_UNDEF = 0,
    CMPL_FLOAT,
    CMPG_FLOAT,
    CMPL_DOUBLE,
    CMPG_DOUBLE,
    CMP_LONG,
} CMP_KIND;


//#define DIV_REM_MAY_THROW

class CmpStr {
public:
    bool is_less(CHAR const* t1, CHAR const* t2) const
    { return strcmp(t1, t2) < 0; }

    bool is_equ(CHAR const* t1, CHAR const* t2) const
    { return strcmp(t1, t2) == 0; }
};

typedef TMap<CHAR const*, Type const*, CmpStr> Str2Type;
typedef TMap<LabelInfo const*, CatchInfo const*> Label2CatchInfo;

//In Actually, it does work to convert ANA IR to IR, but is not DEX.
//To wit, the class declared in class LIR2IR, that will be better.
class Dex2IR {
protected:
    DexRegion * m_ru;
    DexRegionMgr * m_ru_mgr;
    TypeMgr * m_tm;
    DexFile * m_df;
    VarMgr * m_vm;
    LIRCode * m_lircode;
    TypeIndexRep const* m_tr;
    SMemPool * m_pool;

    LIR2LabelInfo m_lir2labs;
    Str2Type m_typename2type;
    Label2CatchInfo m_label2catchinfo;
    DbxVec const& m_dbxvec;
    Type const* m_ptr_addend;
    UINT m_ofst_addend;
    CatchInfo * m_current_catch_list;

    //Map from typeIdx of type-table to
    //positionIdx in file-class-def-table.
    FieldTypeIdx2PosIdx m_typeidx2posidx;
    ConstSym2Var m_str2var;
    TryInfo * m_ti;
    bool m_has_catch; //Set to true if region has catch block.
    List<LabelInfo*> m_last_try_end_lab_list;
    Var2UINT * m_var2fieldid;

    void attachCatchInfo(IR * ir);
    void attachCatchInfo(IR * ir, AIContainer * ai);

    void markLIRLabel();
    void markTryLabel();

    void * xmalloc(UINT size, SMemPool * pool)
    {
        ASSERT(size > 0, ("xmalloc: size less zero!"));
        ASSERT(pool, ("need pool!!"));
        void * p = smpoolMalloc(size, pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }

    void * xmalloc(INT size)
    {
        ASSERT(size > 0, ("xmalloc: size less zero!"));
        ASSERT(m_pool != NULL, ("need pool!!"));
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }
public:
    Vreg2PR m_v2pr; //map from dex register v to IR_PR node.
    Prno2Vreg m_pr2v; //map from dex register v to IR_PR node.

    Dex2IR(Region * rg, DexFile * df, LIRCode * fu, DbxVec const& dbxvec);
    ~Dex2IR()
    {
        TMapIter<LIR*, List<LabelInfo*>*> iter;
        List<LabelInfo*> * l;
        for (m_lir2labs.get_first(iter, &l);
             l != NULL; m_lir2labs.get_next(iter, &l)) {
            delete l;
        }

        smpoolDelete(m_pool);
    }

    VAR * addVarByName(CHAR const* name, Type const* ty);
    VAR * addClassVar(UINT class_id, LIR * lir);
    VAR * addStringVar(CHAR const* string);
    VAR * addFieldVar(UINT field_id, Type const* ty);
    VAR * addStaticVar(UINT field_id, Type const* ty);
    VAR * addFuncVar(UINT method_id, Type const* ty);

    UINT computeClassPosid(UINT cls_id_in_tytab);
    UINT computeFieldOffset(UINT field_id);
    IR * convertArrayLength(IN LIR * lir);
    IR * convertLoadStringAddr(IN LIR * lir);
    IR * convertCvt(IN LIR * lir);
    IR * convertCheckCast(IN LIR * lir);
    IR * convertCondBr(IN LIR * lir);
    IR * convertConstClass(IN LIR * lir);
    IR * convertFilledNewArray(IN LIR * lir);
    IR * convertFillArrayData(IN LIR * lir);
    IR * convertGoto(IN LIR * lir);
    IR * convertLoadConst(IN LIR * lir);
    IR * convertMoveResult(IN LIR * lir);
    IR * convertMove(IN LIR * lir);
    IR * convertThrow(IN LIR * lir);
    IR * convertMoveException(IN LIR * lir);
    IR * convertMonitorExit(IN LIR * lir);
    IR * convertMonitorEnter(IN LIR * lir);
    IR * convertInvoke(IN LIR * lir);
    IR * convertInstanceOf(IN LIR * lir);
    IR * convertNewInstance(IN LIR * lir);
    IR * convertPackedSwitch(IN LIR * lir);
    IR * convertSparseSwitch(IN LIR * lir);
    IR * convertAput(IN LIR * lir);
    IR * convertAget(IN LIR * lir);
    IR * convertSput(IN LIR * lir);
    IR * convertSget(IN LIR * lir);
    IR * convertIput(IN LIR * lir);
    IR * convertIget(IN LIR * lir);
    IR * convertCmp(IN LIR * lir);
    IR * convertUnaryOp(IN LIR * lir);
    IR * convertBinaryOpAssign(IN LIR * lir);
    IR * convertBinaryOp(IN LIR * lir);
    IR * convertBinaryOpLit(IN LIR * lir);
    IR * convertRet(IN LIR * lir);
    IR * convertNewArray(IN LIR * lir);
    IR * convert(IN LIR * lir);
    IR * convert(bool * succ);

    void dump_lir2lab();

    IR * genMappedPR(UINT vid, Type const* ty);
    UINT genMappedPrno(UINT vid, Type const* ty);
    UINT get_ofst_addend() const
    {
        return 8;
        //return m_ofst_addend;
    }
    CHAR const* get_var_type_name(UINT field_id);
    UINT get_dexopcode(UINT flag);
    Type const* getType(LIR * ir);
    Prno2Vreg * getPR2Vreg() { return &m_pr2v; }
    Vreg2PR * getVreg2PR() { return &m_v2pr; }
    TryInfo * getTryInfo() { return m_ti; }
    Label2CatchInfo const& getLabel2CatchInfo() const
    { return m_label2catchinfo; }

    List<LabelInfo*> & getLastTryEndLabelList()
    { return m_last_try_end_lab_list; }

    bool is_readonly(CHAR const* method_name) const;

    //Return true if ir is built-in function.
    bool hasCatch() const { return m_has_catch; }

    Type const* mapDexType2XocType(CHAR charty);
    Type const* mapFieldType2Type(UINT field_id);

    void set_map_v2ofst(VAR * v, UINT ofst);
};

#endif
