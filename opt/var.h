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

class RegionMgr;

//
//START Var
//

//The size of Var does not be recorded, because the memory size processed is
//to be changed dynamically, so the size is related with the corresponding
//IR's type that referred the Var.

///////////////////////////////////////////////////////
//NOTE: Do *NOT* forget modify the bit-field in Var if
//you remove/add flag here.
///////////////////////////////////////////////////////
#define DEDICATED_STRING_VAR_NAME "#DedicatedStringVar"
#define VAR_UNDEF 0x0

//This attribute describe the scope of variable.
//A variable can be seen by all regions if it is GLOBAL.
#define VAR_GLOBAL 0x1

//This attribute defined the scope of variable.
//A variable can ONLY be seen by current and inner region.
//It always be allocated in stack or thread local storage(TLS).
#define VAR_LOCAL 0x2

//This attribute describe the scope of variable.
//A private variable which is GLOBAL can ONLY be seen in
//current and inner region.
#define VAR_PRIVATE 0x4

//This attribute describe the access authority of variable.
//A readonly variable guarantees that no operation can modify the memory.
#define VAR_READONLY 0x8 //var is readonly
#define VAR_VOLATILE 0x10 //var is volatile
#define VAR_HAS_INIT_VAL 0x20 //var with initialied value.
#define VAR_FAKE 0x40 //var is fake
#define VAR_IS_LABEL 0x80 //var is label.
#define VAR_FUNC_DECL 0x100 //var is function region declaration.
#define VAR_IS_ARRAY 0x200 //var is array.
#define VAR_IS_FORMAL_PARAM 0x400 //var is formal parameter.
#define VAR_IS_SPILL 0x800 //var is spill location.
#define VAR_ADDR_TAKEN 0x1000 //var's address has been taken.
#define VAR_IS_PR 0x2000 //var is pr.
#define VAR_IS_RESTRICT 0x4000 //var is restrict.
#define VAR_IS_UNALLOCABLE 0x8000 //var is unallocable in memory.

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
#define VAR_flag(v) ((v)->u2.flag)

//Record string content if variable is string.
#define VAR_string(v) ((v)->u1.string)

//Record LabelInfo if variable is label.
#define VAR_labinfo(v) ((v)->u1.labinfo)

//Variable is label.
#define VAR_is_label(v) ((v)->u2.s1.is_label)

//Variable is global.
//This attribute describe the scope of variable.
//A variable can be seen by all regions if it is GLOBAL.
#define VAR_is_global(v) ((v)->u2.s1.is_global)

//Variable is local.
//This attribute defined the scope of variable.
//A variable can ONLY be seen by current and inner region.
//It always be allocated in stack or thread local storage(TLS).
#define VAR_is_local(v) ((v)->u2.s1.is_local)

//This attribute describe the scope of variable.
//A private variable which is GLOBAL can ONLY be seen in
//current and inner region.
#define VAR_is_private(v) ((v)->u2.s1.is_private)

//Variable is readonly.
//This attribute describe the access authority of variable.
//A readonly variable guarantees that no operation can modify the memory.
#define VAR_is_readonly(v) ((v)->u2.s1.is_readonly)

//Record the initial valud index.
#define VAR_byte_val(v) ((v)->u1.byte_val)

//Record prno if variable represent a PR.
#define VAR_prno(v) ((v)->u1.prno)

//Variable has initial value.
#define VAR_has_init_val(v) ((v)->u2.s1.has_init_val)

//Variable is function region.
#define VAR_is_func_decl(v) ((v)->u2.s1.is_func_decl)

//Variable is aritifical or spurious that used to
//facilitate optimizations and analysis.
#define VAR_is_fake(v) ((v)->u2.s1.is_fake)

//Variable is volatile.
#define VAR_is_volatile(v) ((v)->u2.s1.is_volatile)

//Variable describes a memory that arranged in the manner of array.
#define VAR_is_array(v) ((v)->u2.s1.is_array)

//Variable is parameter of this region.
#define VAR_is_formal_param(v) ((v)->u2.s1.is_formal_param)

//Record the parameter position.
#define VAR_formal_param_pos(v) ((v)->formal_parameter_pos)

//Variable is spill location.
#define VAR_is_spill(v) ((v)->u2.s1.is_spill)

//Variable has been taken address.
#define VAR_is_addr_taken(v) ((v)->u2.s1.is_addr_taken)

//Variable is PR.
#define VAR_is_pr(v) ((v)->u2.s1.is_pr)

//Variable is marked "restrict", and it always be parameter.
#define VAR_is_restrict(v) ((v)->u2.s1.is_restrict)

//Variable is not concrete, and will NOT occupy any memory.
#define VAR_is_unallocable(v) ((v)->u2.s1.is_unallocable)

//Record the alignment.
#define VAR_align(v) ((v)->align)

class Var {
    COPY_CONSTRUCTOR(Var);
public:
    UINT uid; //unique id;
    Type const* type; //Data type.
    UINT align; //memory alignment of var.
    Sym const* name;

    //Record the formal parameter position if Var is parameter.
    //Start from 0.
    UINT formal_parameter_pos;

    union {
        //Record string contents if Var is const string.
        Sym const* string;

        //Record byte code if Var has constant initial value.
        ByteBuf * byte_val;

        //Record labelinfo if Var is label.
        LabelInfo * labinfo;

        //Record Prno if Var is pr.
        UINT prno;
    } u1;

    union {
        UINT flag; //Record variant properties of Var.
        struct {
            UINT is_global:1; //Var can be seen all program.
            UINT is_local:1; //Var only can be seen in region.
            UINT is_private:1; //Var only can be seen in file.
            UINT is_readonly:1; //Var is readonly.
            UINT is_volatile:1; //Var is volatile.
            UINT has_init_val:1; //Var has initial value.
            UINT is_fake:1; //Var is fake.
            UINT is_label:1; //Var is label.
            UINT is_func_decl:1; //Var is function unit declaration.
            UINT is_array:1; //Var is array
            UINT is_formal_param:1; //Var is formal parameter.
            UINT is_spill:1; //Var is spill location in function.
            UINT is_addr_taken:1; //Var has been taken address.
            UINT is_pr:1; //Var is pr.
            UINT is_restrict:1; //Var is restrict.

            //True if variable should NOT be allocated in memory and
            //it is only being a placeholder in essence.
            UINT is_unallocable:1;
        } s1;
    } u2;
public:
    Var();
    virtual ~Var() {}

    UINT id() const { return VAR_id(this); }
    bool is_local() const { return VAR_is_local(this); }
    bool is_global() const { return VAR_is_global(this); }
    bool is_fake() const { return VAR_is_fake(this); }
    bool is_label() const { return VAR_is_label(this); }
    bool is_unallocable() const { return VAR_is_unallocable(this); }
    bool is_array() const { return VAR_is_array(this); }
    bool is_formal_param() const { return VAR_is_formal_param(this); }
    bool is_private() const { return VAR_is_private(this); }
    bool is_readonly() const { return VAR_is_readonly(this); }
    bool is_func_decl() const { return VAR_is_func_decl(this); }
    bool is_volatile() const { return VAR_is_volatile(this); }
    bool is_spill() const { return VAR_is_spill(this); }
    bool is_addr_taken() const { return VAR_is_addr_taken(this); }
    bool is_pr() const { return VAR_is_pr(this); }
    bool is_restrict() const { return VAR_is_restrict(this); }
    bool is_any() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_any();
    }

    bool is_pointer() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_pointer();
    }

    //Return true if variable type is memory chunk.
    bool is_mc() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_mc();
    }

    bool is_string() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_string();
    }

    bool is_vector() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_vector();
    }

    //Return true if variable has flags to output to GR.
    bool hasGRFlag() const
    {
        return (is_unallocable() ||
                is_func_decl() ||
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
    bool has_init_val() const { return VAR_has_init_val(this); }

    UINT get_align() const { return VAR_align(this); }
    Sym const* get_name() const { return VAR_name(this); }
    Type const* getType() const { return VAR_type(this); }
    DATA_TYPE getDType() const { return TY_dtype(getType()); }
    UINT getFormalParamPos() const { return VAR_formal_param_pos(this); }
    UINT getStringLength() const
    {
        ASSERT0(VAR_type(this)->is_string());
        return VAR_string(this) == nullptr ?
               0 : xstrlen(SYM_name(VAR_string(this)));
    }
    Sym const* getString() const { return VAR_string(this); }
    ByteBuf const* getByteValue() const { return VAR_byte_val(this); }

    //Return the byte size of variable accroding type.
    UINT getByteSize(TypeMgr const* dm) const
    {
        //Length of string var should include '\0'.
        return is_string() ?
            getStringLength() + 1:
            dm->getByteSize(VAR_type(this));
    }

    virtual CHAR const* dumpVARDecl(StrBuf &) const { return nullptr; }
    virtual void dump(TypeMgr const* tm) const;

    //You must make sure this function will not change any field of Var.
    virtual CHAR const* dump(StrBuf & buf, TypeMgr const* dm) const;
    void dumpProp(xcom::StrBuf & buf, bool grmode) const;
    CHAR const* dumpGR(StrBuf & buf, TypeMgr * dm) const;

    void setToGlobal(bool is_global)
    {
        VAR_is_global(this) = (UINT)is_global;
        VAR_is_local(this) = (UINT)!is_global;
    }
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
    Var * registerVar(CHAR const* varname,
                      Type const* type,
                      UINT align,
                      UINT flag);
    //Create variable by symbol name.
    Var * registerVar(Sym const* var_name,
                      Type const* type,
                      UINT align,
                      UINT flag);
    //Create string variable by name and string-content.
    Var * registerStringVar(CHAR const* var_name, Sym const* s, UINT align);
};

} //namespace xoc
#endif
