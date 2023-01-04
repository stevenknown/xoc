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
#ifndef __VAR_H__
#define __VAR_H__

namespace xoc {

#define VAR_ID_UNDEF 0
#define VAR_ID_MAX 5000000

class RegionMgr;

//
//START Var
//

//The size of Var is unnecessary to be recorded, because the memory size
//processed is to be changed dynamically, so the size is related with
//the corresponding IR's type that referred the Var.

////////////////////////////////////////////////////////////////////////////////
//NOTE: Do *NOT* forget modify the bit-field in Var if you remove/add flag here.
////////////////////////////////////////////////////////////////////////////////
#define DEDICATED_STRING_VAR_NAME "#DedicatedStringVar"

enum VAR_FLAG {
    VAR_UNDEF = 0x0,
    
    //Variable is global.
    //The attribute describes the scope of variable.
    //A variable can be seen by all regions if it is GLOBAL.
    //The global attribute is conform to definition of MD_GLOBAL_VAR of MD.
    VAR_GLOBAL = 0x1,
    
    //Variable is local.
    //The attribute defined the scope of variable.
    //A variable can ONLY be seen by current and inner region.
    //It always be allocated in stack or thread local storage(TLS).
    VAR_LOCAL = 0x2,
    
    //The attribute describes the scope of variable.
    //A private variable which is GLOBAL can ONLY be seen in
    //current and inner region.
    VAR_PRIVATE = 0x4,
    
    //Variable is readonly.
    //The attribute describes the access authority of variable.
    //A readonly variable guarantees that no operation can modify the memory.
    VAR_READONLY = 0x8, //var is readonly
    VAR_VOLATILE = 0x10, //var is volatile
    VAR_HAS_INIT_VAL = 0x20, //var with initialied value.

    //Variable is aritifical or spurious that used to
    //facilitate optimizations and analysis.
    //Note a fake variable can not be taken address, it is always used to
    //represent some relations between STMT/EXP, or be regarded as a placeholder.
    VAR_FAKE = 0x40,

    //Variable is label.
    VAR_IS_LABEL = 0x80,

    //Variable represents a function region.
    VAR_IS_FUNC = 0x100,

    //Variable describes a memory that arranged in the manner of array.
    VAR_IS_ARRAY = 0x200,

    //Variable is parameter of this region.
    VAR_IS_FORMAL_PARAM = 0x400,

    //Variable is spill location.
    VAR_IS_SPILL = 0x800,

    //Variable has been taken address.
    VAR_ADDR_TAKEN = 0x1000,

    //Variable is PR.
    VAR_IS_PR = 0x2000,

    //Variable is marked restrict, means the variable will not alias
    //to anyother variables.
    VAR_IS_RESTRICT = 0x4000,

    //Variable is unallocable in memory.
    //Variable should NOT be allocated in memory and
    //it is only being a placeholder in essence.
    VAR_IS_UNALLOCABLE = 0x8000,

    //Variable is declaration.
    VAR_IS_DECL = 0x10000,
};

class VarFlag : public UFlag {
public:
    VarFlag(UINT v) : UFlag(v) {}
    bool verify() const;
};


class VarFlagDesc {
public:
    ////////////////////////////////////////////////////////////////////
    //NOTE: DO NOT CHANGE THE LAYOUT OF CLASS MEMBERS BECAUSE THEY ARE//
    //CORRESPONDING TO THE SPECIAL INITIALIZING VALUE.                //
    ////////////////////////////////////////////////////////////////////
    VAR_FLAG flag;
    CHAR const* name;
public:
    //Get flag's name.
    static CHAR const* getName(VAR_FLAG flag);
   
    //Compute the index of 'flag' in the Desc table.
    static UINT getDescIdx(VAR_FLAG flag);

    //Get the flag by given index in enum definition.
    static VAR_FLAG getFlag(UINT idx);
};


////////////////////////////////////////////////////////
//NOTE: DO *NOT* forget modify the bit-field in Var if//
//you remove/add flag here.                           //
////////////////////////////////////////////////////////
#define BYTEBUF_size(b) ((b)->m_byte_size)
#define BYTEBUF_buffer(b) ((b)->m_byte_buffer)
class ByteBuf {
    COPY_CONSTRUCTOR(ByteBuf);
public:
    UINT m_byte_size;
    BYTE * m_byte_buffer;
public:
    BYTE * getBuffer() const { return m_byte_buffer; }
    UINT getSize() const { return m_byte_size; }
};

//Variable unique id.
#define VAR_id(v) ((v)->uid)

//Variable type.
#define VAR_type(v) ((v)->type)

#define VAR_name(v) ((v)->name)

//Various flag.
#define VAR_flag(v) ((v)->varflag)

//Record string content if variable is string.
#define VAR_string(v) ((v)->u1.string)

//Record LabelInfo if variable is label.
#define VAR_labinfo(v) ((v)->u1.labinfo)

//Record the initial valud index.
#define VAR_byte_val(v) ((v)->u1.byte_val)

//Record prno if variable represent a PR.
#define VAR_prno(v) ((v)->prno)

//Variable has initial value.
#define VAR_has_init_val(v) ((v)->u2.s1.has_init_val)

//Record the parameter position.
#define VAR_formal_param_pos(v) ((v)->formal_parameter_pos)

//Record the alignment.
#define VAR_align(v) ((v)->align)

class Var {
    COPY_CONSTRUCTOR(Var);
public:
    UINT uid; //unique id.
    Type const* type; //data type.
    UINT align; //memory alignment of Var.
    Sym const* name; //record name of Var.

    //Record the formal parameter position if Var is parameter.
    //The position start from 0.
    UINT formal_parameter_pos;

    //Record prno if Var indicates a PR location.
    PRNO prno;

    union {
        //Record string contents if Var is a constant string.
        Sym const* string;

        //Record byte code if Var has constant initial value.
        ByteBuf * byte_val;

        //Record label related info if Var indicates a label.
        LabelInfo * labinfo;
    } u1; //record the properties that are exclusive.
    VarFlag varflag;
public:
    Var();
    virtual ~Var() {}

    UINT id() const { return VAR_id(this); }
    bool is_local() const { return varflag.have(VAR_LOCAL); }
    bool is_global() const { return varflag.have(VAR_GLOBAL); }
    bool is_fake() const { return varflag.have(VAR_FAKE); }
    bool is_label() const { return varflag.have(VAR_IS_LABEL); }
    bool is_unallocable() const { return varflag.have(VAR_IS_UNALLOCABLE); }
    bool is_decl() const { return varflag.have(VAR_IS_DECL); }
    bool is_array() const { return varflag.have(VAR_IS_ARRAY); }
    bool is_formal_param() const { return varflag.have(VAR_IS_FORMAL_PARAM); }
    bool is_private() const { return varflag.have(VAR_PRIVATE); }
    bool is_readonly() const { return varflag.have(VAR_READONLY); }
    bool is_func() const { return varflag.have(VAR_IS_FUNC); }
    bool is_volatile() const { return varflag.have(VAR_VOLATILE); }
    bool is_spill() const { return varflag.have(VAR_IS_SPILL); }
    bool is_taken_addr() const { return varflag.have(VAR_ADDR_TAKEN); }
    bool is_pr() const { return varflag.have(VAR_IS_PR); }
    bool is_restrict() const { return varflag.have(VAR_IS_RESTRICT); }
    bool is_any() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_any();
    }

    //Return true if variable is pointer data type.
    bool is_pointer() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_pointer();
    }

    //Return true if variable is regarded as a pointer.
    bool isPointer() const { return is_any() || is_pointer(); }

    //Return true if variable type is memory chunk data type.
    bool is_mc() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_mc();
    }

    //Return true if variable type is string data type.
    bool is_string() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_string();
    }

    //Return true if variable type is vector data type.
    bool is_vector() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_vector();
    }

    //Return true if variable has flags to output to GR.
    bool hasGRFlag() const
    {
        return (is_unallocable() ||
                is_func() ||
                is_decl() ||
                is_private() ||
                is_readonly() ||
                is_volatile() ||
                is_restrict() ||
                is_fake() ||
                is_label() ||
                is_array() ||
                (is_string() && getString() != nullptr) ||
                getByteValue() != nullptr);
    }

    //Return true if variable has initial value.
    bool has_init_val() const { return VAR_flag(this).have(VAR_HAS_INIT_VAL); }

    //Get byte alignment.
    UINT get_align() const { return VAR_align(this); }
    Sym const* get_name() const { return VAR_name(this); }
    Type const* getType() const { return VAR_type(this); }
    DATA_TYPE getDType() const { return TY_dtype(getType()); }
    UINT getFormalParamPos() const { return VAR_formal_param_pos(this); }

    //Get byte length if variable records string contents.
    UINT getStringLength() const
    {
        ASSERT0(getType()->is_string());
        return getString() == nullptr ?
               0 : xcom::xstrlen(getString()->getStr());
    }

    //Get string contents.
    Sym const* getString() const { return VAR_string(this); }

    //Get initial value.
    ByteBuf const* getByteValue() const { return VAR_byte_val(this); }

    //Return the byte size of variable accroding type.
    UINT getByteSize(TypeMgr const* dm) const
    {
        //Length of string var should include '\0'.
        return is_string() ? getStringLength() + 1:
                             dm->getByteSize(VAR_type(this));
    }

    //The interface to dump declaration information when current
    //variable dumpped. This is target dependent code.
    virtual CHAR const* dumpVARDecl(StrBuf &) const { return nullptr; }
    virtual void dump(TypeMgr const* tm) const;

    //You must make sure this function will not change any field of Var.
    //The information will be dumpped into 'buf'.
    virtual CHAR const* dump(OUT StrBuf & buf, TypeMgr const* dm) const;
    void dumpFlag(xcom::StrBuf & buf, bool grmode, bool & first) const;
    void dumpProp(OUT StrBuf & buf, bool grmode) const;
    CHAR const* dumpGR(OUT StrBuf & buf, TypeMgr * dm) const;

    //Set variable is global scope.
    //The global attribute is conform to definition of MD_GLOBAL_VAR of MD.
    void setToGlobal(bool is_global)
    {
        is_global ? setflag(VAR_GLOBAL), removeflag(VAR_LOCAL) :
                    removeflag(VAR_GLOBAL), setflag(VAR_LOCAL);
    }
    void setToFormalParam() { setflag(VAR_IS_FORMAL_PARAM); }
    void setflag(VAR_FLAG f) { VAR_flag(this).set(f); }

    void removeflag(VAR_FLAG f) {VAR_flag(this).remove(f); }
};
//END Var

typedef TTabIter<Var*> VarTabIter;
typedef TTabIter<Var const*> ConstVarTabIter;

class CompareVar {
public:
    bool is_less(Var * t1, Var * t2) const { return t1->id() < t2->id(); }
    bool is_equ(Var * t1, Var * t2) const { return t1 == t2; }
    Var * createKey(Var * t) { return t; }
};


class CompareConstVar {
public:
    bool is_less(Var const* t1, Var const* t2) const
    { return t1->id() < t2->id(); }
    bool is_equ(Var const* t1, Var const* t2) const { return t1 == t2; }
    Var const* createKey(Var const* t) { return t; }
};


class VarTab : public TTab<Var*, CompareVar> {
    COPY_CONSTRUCTOR(VarTab);
public:
    VarTab() {}
    void dump(TypeMgr const* tm) const;
};


class ConstVarTab : public TTab<Var const*, CompareConstVar> {
public:
    void dump(TypeMgr * tm);
};


//Map from const Sym to Var.
typedef TMap<Sym const*, Var*> ConstSym2Var;

//Map from Var id to Var.
typedef Vector<Var*> VarVec;

//This class is responsible for allocation and deallocation of variable.
//User can only create variable via VarMgr, as well as delete
//it in the same way.
class VarMgr {
    COPY_CONSTRUCTOR(VarMgr);
    size_t m_var_count;
    VarVec m_var_vec;
    ConstSym2Var m_str_tab;
    size_t m_str_count;
    DefSBitSetCore m_freelist_of_varid;
    RegionMgr * m_ru_mgr;
    TypeMgr * m_tm;

    //Assign an unique ID to given variable.
    void assignVarId(Var * v);
    void dumpFreeIDList() const;
public:
    explicit VarMgr(RegionMgr * rm);
    virtual ~VarMgr() { destroy(); }

    //Destroy holistic variable manager.
    void destroy();
    //Destroy specific variable and recycle its ID for next allocation.
    void destroyVar(Var * v); //Free Var memory.
    void dump() const;

    TypeMgr * getTypeMgr() const { return m_tm; }
    //Returnt the vector that holds all variables registered.
    VarVec * getVarVec() { return &m_var_vec; }
    //Get variable via var-id.
    Var * get_var(size_t id) const { return m_var_vec.get((UINT)id); }

    //Find string-variable via specific string content.
    Var * findStringVar(Sym const* str) { return m_str_tab.get(str); }
    //Find variable by variable's name.
    //Note there may be multiple variable with same name, this function return
    //the first one.
    Var * findVarByName(Sym const* name);
    //Return true if the 'name' is the name of recorded unique
    //dedicated string variable.
    bool isDedicatedStringVar(CHAR const* name) const;

    //Interface to target machine.
    //Customer could specify additional attributions for specific purpose.
    virtual Var * allocVAR() { return new Var(); }

    //Create variable by string name.
    Var * registerVar(CHAR const* varname, Type const* type, UINT align,
                      UINT flag);
    //Create variable by symbol name.
    Var * registerVar(Sym const* var_name, Type const* type, UINT align,
                      UINT flag);
    //Create string variable by name and string-content.
    Var * registerStringVar(CHAR const* var_name, Sym const* s, UINT align);
};

} //namespace xoc
#endif
