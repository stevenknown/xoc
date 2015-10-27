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
#ifndef _DEX_UTIL_H_
#define _DEX_UTIL_H_

class Dex2IR;

class DexPassMgr : public PassMgr {
public:
    DexPassMgr(Region * ru) : PassMgr(ru) {}
    virtual ~DexPassMgr() {}

    virtual Pass * allocDCE();
    virtual Pass * allocCopyProp();
    virtual Pass * allocRP();

    virtual void performScalarOpt(OptCTX & oc);
};


//
//START DexRegion
//
#define DR_functype(dr)     ((dr)->m_functype)
#define DR_funcname(dr)     ((dr)->m_funcname)
#define DR_classname(dr)    ((dr)->m_classname)
class DexRegion : public Region {
protected:
    VAR2PR m_var2pr; //map from var id to prno.
    Dex2IR * m_dex2ir;
    Prno2Vreg * m_prno2v;
    TypeIndexRep * m_type_index_rep;
    UINT m_paramnum;
    UINT m_org_vregnum;
    CHAR const* m_src_path; //record source file path of class.
    DexFile const* m_dexfile;
    DexMethod const* m_dexmethod;
    SMemPool * m_sc_pool; //a pool to hold the SC<LabelInfo*>

protected:
    bool is_64bit(IR const* ir)
    { return get_type_mgr()->get_bytesize(IR_dt(ir))== 8; }
    void HighProcessImpl(OptCTX & oc);

public:
    CHAR const* m_functype;
    CHAR const* m_funcname;
    CHAR const* m_classname;

public:
    DexRegion(REGION_TYPE rt, RegionMgr * rm) : Region(rt, rm)
    {
        m_dex2ir = NULL;
        m_prno2v = NULL;
        m_type_index_rep = NULL;
        m_paramnum = 0;
        m_org_vregnum = 0;
        m_src_path;
        m_dexfile = NULL;
        m_dexmethod = NULL;
        m_classname = NULL;
        m_funcname = NULL;
        m_functype = NULL;
        m_sc_pool = smpoolCreate(sizeof(SC<LabelInfo*>), MEM_CONST_SIZE);
    }
    virtual ~DexRegion() { smpoolDelete(m_sc_pool); }

    IR * gen_and_add_sib(IR * ir, UINT prno);
    DexPassMgr * getDexPassMgr() { return (DexPassMgr*)get_pass_mgr(); }

    virtual bool HighProcess(OptCTX & oc);
    virtual bool MiddleProcess(OptCTX & oc);

    virtual PassMgr * allocPassMgr();
    virtual IR_AA * allocAliasAnalysis();

    SMemPool * get_sc_pool() const { return m_sc_pool; }
    void setDex2IR(Dex2IR * dex2ir) { m_dex2ir = dex2ir; }
    Dex2IR * getDex2IR() { return m_dex2ir; }

    void setPrno2Vreg(Prno2Vreg * p2v) { m_prno2v = p2v; }
    Prno2Vreg * getPrno2Vreg() { return m_prno2v; }

    void setTypeIndexRep(TypeIndexRep * tr) { m_type_index_rep = tr; }
    TypeIndexRep * getTypeIndexRep() { return m_type_index_rep; }

    void setParamNum(UINT num) { m_paramnum = num; }
    UINT getParamNum() const { return m_paramnum; }

    void setOrgVregNum(UINT num) { m_org_vregnum = num; }
    UINT getOrgVregNum() const { return m_org_vregnum; }

    void setClassSourceFilePath(CHAR const* path) { m_src_path = path; }
    CHAR const* getClassSourceFilePath() const { return m_src_path; }

    void setDexFile(DexFile const* df) { m_dexfile = df; }
    DexFile const* getDexFile() const { return m_dexfile; }

    void setDexMethod(DexMethod const* dm) { m_dexmethod = dm; }
    DexMethod const* getDexMethod() const { return m_dexmethod; }

    void updateRAresult(RA & ra, Prno2Vreg & prno2v);

    bool verifyRAresult(RA & ra, Prno2Vreg & prno2v);

    void process_group_bb(IRBB * bb, List<IR*> & lst);
    void process_group();
    void processSimply();
    virtual void process();
};
//END DexRegion


class DexCallGraph : public CallGraph {
protected:
    //Record very unimportant functions that do not
    //need to add an edge on call graph.
    TTab<SYM const*> m_unimportant_symtab;

public:
    DexCallGraph(UINT edge_hash, UINT vex_hash, RegionMgr * rumgr);
    virtual ~DexCallGraph() {}

    bool is_unimportant(SYM const* sym) const;

    virtual bool shouldAddEdge(IR const* ir) const
    {
        ASSERT0(ir->is_calls_stmt());
        SYM const* name = VAR_name(CALL_idinfo(ir));
        if (is_unimportant(name)) { return false; }

        CHAR const* q = "Ljava/lang/";
        CHAR const* p = SYM_name(name);
        while ((*p++ == *q++) && *q != 0);
        return *q != 0;
    }
};

//
//START DexRegionMgr
//
class DexRegionMgr : public RegionMgr {
protected:
    SMemPool * m_pool;
    Var2UINT m_var2blt;
    UINT2Var m_blt2var;

protected:
    VAR * addVarForBuiltin(CHAR const* name);
    void initBuiltin();

public:
    DexRegionMgr()
    { m_pool = smpoolCreate(128, MEM_COMM); }

    COPY_CONSTRUCTOR(DexRegionMgr);
    virtual ~DexRegionMgr() { smpoolDelete(m_pool); }

    virtual Region * allocRegion(REGION_TYPE rt)
    { return new DexRegion(rt, this); }
    virtual CallGraph * allocCallGraph(UINT edgenum, UINT vexnum)
    { return new DexCallGraph(edgenum, vexnum, this); }

    Region * getProgramRegion()
    {
        //The first region should be program-region.
        return get_region(1);
    }
    SMemPool * get_pool() { return m_pool; }
    Var2UINT & getVar2Builtin() { return m_var2blt; }
    UINT2Var & getBuiltin2Var() { return m_blt2var; }
    Var2UINT const& getVar2BuiltinC() const { return m_var2blt; }
    UINT2Var const& getBuiltin2VarC() const { return m_blt2var; }

    //Do some initialization.
    void init() { initBuiltin(); }

    DexRegionMgr * self() { return this; }

    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }

    virtual void processProgramRegion(Region * program);
};
//END DexRegionMgr

#define OBJ_MC_SIZE            16
#define ARRAY_MC_SIZE        32

#define LIRC_num_of_op(l)    ((l)->instrCount)
#define LIRC_op(l, i)        ((l)->lirList[i])

#define LIR_opcode(ir)        ((ir)->opcode)
#define LIR_name(ir)        (gLIROpcodeInfo.opNames[LIR_opcode(ir)])
#define LIR_dt(ir)            ((ir)->flags)

//For CONST
#define LIR_int_imm(ir)        (((LIRConstOp*)ir)->vB)

//For SPUT
#define LIR_sym_ptr(ir)        (((LIRABOp*)ir)->ptr)
#define LIR_val(ir)            (((LIRABOp*)ir)->vA) //store value

//For ADD,SUB,MUL,DIV,REM,AND,OR,SHL,SHR,USHR,
//XOR,CONST,NEW_INSTANCE.
#define LIR_res(ir)                (((LIRABCOp*)ir)->vA)

//For ADD,SUB,MUL,DIV,REM,AND,OR,SHL,SHR,USHR,
//XOR,NEW_INSTANCE.
#define LIR_op0(ir)                (((LIRABCOp*)ir)->vB)

//For ADD,SUB,MUL,DIV,REM,AND,OR,SHL,SHR,USHR,XOR
#define LIR_op1(ir)                (((LIRABCOp*)ir)->vC)


#define LIR_fill_array_data_mark(ir) \
        (((USHORT*)(((LIRSwitchOp*)ir)->data))[0])
#define LIR_switch_kind(ir) \
        (((USHORT*)(((LIRSwitchOp*)ir)->data))[0])
#define LIR_case_num(ir) \
        (((USHORT*)(((LIRSwitchOp*)ir)->data))[1])
#define LIR_packed_switch_base_value(ir) \
        (*((INT*)(&(((LIRSwitchOp*)ir)->data[2]))))
#define LIR_packed_switch_case_entry(ir) \
        ((UINT*)(((USHORT*)((LIRSwitchOp*)ir)->data) + 4))
#define LIR_sparse_switch_case_value(ir) \
        ((UINT*)(((USHORT*)(((LIRSwitchOp*)ir)->data)) + 2))
#define LIR_sparse_switch_case_entry(ir) \
        (UINT*)&(((BYTE*)(((LIRSwitchOp*)ir)->data))[4 + LIR_case_num(ir) * 4])

typedef LIRBaseOp LIR;

//Backfill data.
class BackFillData {
public:
    IR * ir;
    LIR * lir;
};

//Export Functions.
void dump_lir(LIR * lir, DexFile * df, INT pos);
void dump_lir2(LIR * lir, DexFile * df, INT pos);
void dump_all_lir(LIRCode * fu, DexFile * df, DexMethod const* dm);
void dump_all_class_and_field(DexFile * df);
inline bool is_wide(LIR * lir)
{
    return LIR_dt(lir) == LIR_JDT_wide ||
            LIR_dt(lir) == LIR_JDT_double ||
            LIR_dt(lir) == LIR_JDT_long;
}


#define NUM_OF_PART       3
#define CLASSPATH_PART    0
#define TYPE_PART         1
#define FUNCNAME_PART     2

//Generate unique name by assembling Class Name, Function Parameter Type,
//and Function Name.
//1th part: class path
//2th part: type string, include (parameters string list) return-value-type
//3th part: function name.
CHAR * assemblyUniqueName(
        OUT CHAR buf[],
        CHAR const* class_name,
        CHAR const* func_type,
        CHAR const* func_name);

//Extract the function parameter and return value name
//from function type string.
void extractFunctionTypeFromFunctionTypeString(
        CHAR const* functype,
        OUT CHAR * param_type,
        OUT CHAR * return_type);

//Extract the function parameter and return value name
//from entirely name format, and return the function name.
//Note outputbuf should big enough to hold the string.
void extractFunctionTypeFromRegionName(
        CHAR const* runame,
        OUT CHAR * classpath,
        OUT CHAR * functionname,
        OUT CHAR * param_type,
        OUT CHAR * return_type);

//Seperate param_type_string into a set of individual type string.
//Return the number of individual type strings.
//If params is NULL, this function only compute the number of type string.
//Note the outputbuf recorded in params should big enough to
//hold each type string.
UINT extractSeparateParamterType(
        CHAR const* param_type_string,
        OUT List<CHAR*> * params,
        UINT len = 0);

//Print human readable type info according to type_string.
//Note this function check the length of buffer needed.
UINT printType2Buffer(CHAR const* type_string, OUT CHAR * buf);

//Return the VAR related to given builtin.
inline VAR * getBuiltinVar(BLTIN_TYPE blt, DexRegionMgr const* rumgr)
{
    ASSERT0(rumgr);
    bool find = false;
    VAR * v = rumgr->getBuiltin2VarC().get((UINT)blt, &find);
    ASSERT0(find);
    return v;
}

inline BLTIN_TYPE getBuiltinType(IR const* ir, DexRegionMgr const* rumgr)
{
    ASSERT0(ir && ir->is_call() && rumgr);
    BLTIN_TYPE bt = (BLTIN_TYPE)rumgr->getVar2BuiltinC().mget(CALL_idinfo(ir));
    ASSERT0(bt > BLTIN_UNDEF && BLTIN_LAST);
    return bt;
}

CHAR const* getClassSourceFilePath(
        DexFile const* df,
        DexClassDef const* class_info);

CHAR const* get_class_name(DexFile const* df, DexMethod const* dm);
CHAR const* get_class_name(DexFile const* df, UINT cls_type_idx);
CHAR const* get_class_name(DexFile const* df, DexClassDef const* class_info);
CHAR const* get_func_name(DexFile const* df, DexMethod const* dm);
CHAR const* get_func_type(DexFile const* df, DexMethod const* dm);
CHAR const* get_func_type(DexFile const* df, DexMethodId const* dmid);
CHAR const* get_field_name(DexFile const* df, UINT field_id);
CHAR const* get_field_name(DexFile const* df, DexField * finfo);
CHAR const* get_field_type_name(DexFile const* df, UINT field_id);
CHAR const* get_field_type_name(DexFile const* df, DexField * finfo);
CHAR const* get_field_class_name(DexFile const* df, UINT field_id);
CHAR const* get_field_class_name(DexFile const* df, DexField * finfo);
CHAR const* get_dt_name(LIR * ir);

bool is_builtin(IR const* ir, BLTIN_TYPE bt, DexRegionMgr const* rumgr);
inline bool is_builtin(IR const* ir)
{
    //The first charactor of builtin function is #.
    ASSERT0(ir && ir->is_call());
    return ((CCall*)ir)->get_callee_name()[0] == '#';
}
#endif

