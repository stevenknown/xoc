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
//START VAR
//

//The size of VAR does not be recorded, because the memory size processed is
//to be changed dynamically, so the size is related with the corresponding
//IR's type that referred the VAR.

///////////////////////////////////////////////////////
//NOTE: Do *NOT* forget modify the bit-field in VAR if
//you remove/add flag here.
///////////////////////////////////////////////////////
#define DEDICATED_STRING_VAR_NAME "#DedicatedStringVar"
#define VAR_UNDEF                0x0
#define VAR_GLOBAL               0x1    //can be seen by all functions.

//This kind of variable only can be seen by current function or thread.
//It always be allocated in stack or thread local storage(TLS).
#define VAR_LOCAL                0x2

//This kind of variable can be seen in same file.
#define VAR_PRIVATE              0x4
#define VAR_READONLY             0x8    //var is readonly
#define VAR_VOLATILE             0x10   //var is volatile
#define VAR_HAS_INIT_VAL         0x20   //var with initialied value.
#define VAR_FAKE                 0x40   //var is fake
#define VAR_IS_LABEL             0x80   //var is label.
#define VAR_FUNC_DECL            0x100  //var is function declaration.
#define VAR_IS_ARRAY             0x200  //var is array.
#define VAR_IS_FORMAL_PARAM      0x400  //var is formal parameter.
#define VAR_IS_SPILL             0x800  //var is spill location.
#define VAR_ADDR_TAKEN           0x1000 //var's address has been taken.
#define VAR_IS_PR                0x2000 //var is pr.
#define VAR_IS_RESTRICT          0x4000 //var is restrict.
#define VAR_IS_UNALLOCABLE       0x8000 //var is unallocable in memory.

///////////////////////////////////////////////////////
//NOTE: Do *NOT* forget modify the bit-field in VAR if
//you remove/add flag here.
///////////////////////////////////////////////////////
#define BYTEBUF_size(b)    ((b)->m_byte_size)
#define BYTEBUF_buffer(b)  ((b)->m_byte_buffer)
class ByteBuf {
public:
    UINT m_byte_size;
    BYTE * m_byte_buffer;

    BYTE * getBuffer() const { return m_byte_buffer; }
    UINT getSize() const { return m_byte_size; }
};

//Variable unique id.
#define VAR_id(v)                ((v)->uid)

//Variable type.
#define VAR_type(v)              ((v)->type)

#define VAR_name(v)              ((v)->name)

//Various flag.
#define VAR_flag(v)              ((v)->u2.flag)

//Record string content if variable is string.
#define VAR_string(v)            ((v)->u1.string)

//Record LabelInfo if variable is label.
#define VAR_labinfo(v)           ((v)->u1.labinfo)

//Variable is label.
#define VAR_is_label(v)          ((v)->u2.s1.is_label)

//Variable is global.
#define VAR_is_global(v)         ((v)->u2.s1.is_global)

//Variable is local.
#define VAR_is_local(v)          ((v)->u2.s1.is_local)

//Global Variables which is private cannot be
//referenced outside current file region.
#define VAR_is_private(v)        ((v)->u2.s1.is_private)

//Variable is readonly.
#define VAR_is_readonly(v)       ((v)->u2.s1.is_readonly)

//Record the initial valud index.
#define VAR_byte_val(v)          ((v)->u1.byte_val)

//Variable has initial value.
#define VAR_has_init_val(v)      (VAR_byte_val(v) != NULL)

//Variable is region.
#define VAR_is_func_decl(v)      ((v)->u2.s1.is_func_decl)

//Variable is aritifical or spurious that used to
//faciliate optimizations and analysis.
#define VAR_is_fake(v)           ((v)->u2.s1.is_fake)

//Variable is volatile.
#define VAR_is_volatile(v)       ((v)->u2.s1.is_volatile)

//Variable is an array.
#define VAR_is_array(v)          ((v)->u2.s1.is_array)

//Variable is parameter of this region.
#define VAR_is_formal_param(v)   ((v)->u2.s1.is_formal_param)

//Record the parameter position.
#define VAR_formal_param_pos(v)  ((v)->formal_parameter_pos)

//Variable is spill location.
#define VAR_is_spill(v)          ((v)->u2.s1.is_spill)

//Variable has been taken address.
#define VAR_is_addr_taken(v)     ((v)->u2.s1.is_addr_taken)

//Variable is PR.
#define VAR_is_pr(v)             ((v)->u2.s1.is_pr)

//Variable is marked "restrict", and it always be parameter.
#define VAR_is_restrict(v)       ((v)->u2.s1.is_restrict)

//Variable is concrete, and will be output to Code Generator.
#define VAR_is_unallocable(v)    ((v)->u2.s1.is_unallocable)

//Record the alignment.
#define VAR_align(v)             ((v)->align)
class VAR {
public:
    UINT uid; //unique id;
    Type const* type; //Data type.
    UINT align; //memory alignment of var.
    SYM const* name;

	//Record the formal parameter position if VAR is parameter.
	//Start from 0.
	UINT formal_parameter_pos;

    union {
		//Record string contents if VAR is const string.
		SYM const* string;

        //Record byte code if VAR has constant initial value.
        ByteBuf * byte_val;

		//Record labelinfo if VAR is label.
		LabelInfo * labinfo;
    } u1;

    union {
        UINT flag; //Record variant properties of VAR.
        struct {
            UINT is_global:1;       //VAR can be seen all program.
            UINT is_local:1;        //VAR only can be seen in region.
            UINT is_private:1;      //VAR only can be seen in file.
            UINT is_readonly:1;     //VAR is readonly.
            UINT is_volatile:1;     //VAR is volatile.
            UINT has_init_val:1;    //VAR has initial value.
            UINT is_fake:1;         //VAR is fake.
            UINT is_label:1;        //VAR is label.
            UINT is_func_decl:1;    //VAR is function unit declaration.
            UINT is_array:1;        //VAR is array
            UINT is_formal_param:1; //VAR is formal parameter.
            UINT is_spill:1;        //VAR is spill location in function.
            UINT is_addr_taken:1;   //VAR has been taken address.
            UINT is_pr:1;           //VAR is pr.
            UINT is_restrict:1;     //VAR is restrict.

            //True if variable should NOT be allocated in memory and
            //it is only being a placeholder in essence.
            UINT is_unallocable:1;
        } s1;
    } u2;
public:
    VAR();
    virtual ~VAR() {}

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
    bool is_void() const
    {
        ASSERT0(VAR_type(this));
        return VAR_type(this)->is_void();
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
            (is_string() && getString() != NULL) ||
            getByteValue() != NULL);
    }

    UINT get_align() const { return VAR_align(this); }
    SYM const* get_name() const { return VAR_name(this); }
    Type const* getType() const { return VAR_type(this); }
    DATA_TYPE getDType() const { return TY_dtype(getType()); }
    UINT getFormalParamPos() const { return VAR_formal_param_pos(this); }
    UINT getStringLength() const
    {
        ASSERT0(VAR_type(this)->is_string());
        return VAR_string(this) == NULL ? 0 : xstrlen(SYM_name(VAR_string(this)));
    }
    SYM const* getString() const { return VAR_string(this); }
    ByteBuf const* getByteValue() const { return VAR_byte_val(this); }

    //Return the byte size of variable accroding type.
    UINT getByteSize(TypeMgr const* dm) const
    {
        //Length of string var should include '\0'.
        return is_string() ?
            getStringLength() + 1:
            dm->get_bytesize(VAR_type(this));
    }

    virtual CHAR const* dumpVARDecl(StrBuf &) const { return NULL; }
    virtual void dump(FILE * h, TypeMgr const* dm) const;

    //You must make sure this function will not change any field of VAR.
    virtual CHAR const* dump(StrBuf & buf, TypeMgr const* dm) const;
    void dumpProp(xcom::StrBuf & buf, bool grmode) const;
    CHAR const* dumpGR(StrBuf & buf, TypeMgr * dm) const;

    void setToGlobal(bool is_global)
    {
        VAR_is_global(this) = (UINT)is_global;
        VAR_is_local(this) = (UINT)!is_global;
    }
};

//END VAR

typedef TabIter<VAR*> VarTabIter;
typedef TabIter<VAR const*> ConstVarTabIter;

class CompareVar {
public:
    bool is_less(VAR * t1, VAR * t2) const { return t1 < t2; }
    bool is_equ(VAR * t1, VAR * t2) const { return t1 == t2; }
    VAR * createKey(VAR * t) { return t; }
};


class CompareConstVar {
public:
    bool is_less(VAR const* t1, VAR const* t2) const { return t1 < t2; }
    bool is_equ(VAR const* t1, VAR const* t2) const { return t1 == t2; }
    VAR const* createKey(VAR const* t) { return t; }
};


class VarTab : public TTab<VAR*, CompareVar> {
public:
    void dump(TypeMgr * dm)
    {
        if (g_tfile == NULL) { return; }

        ASSERT0(dm);
        VarTabIter iter;
        for (VAR * v = get_first(iter); v != NULL; v = get_next(iter)) {
            v->dump(g_tfile, dm);
        }
    }
};


class ConstVarTab : public TTab<VAR const*, CompareConstVar> {
public:
    void dump(TypeMgr * dm)
    {
        if (g_tfile == NULL) { return; }

        ASSERT0(dm);
        ConstVarTabIter iter;
        for (VAR const* v = get_first(iter); v != NULL; v = get_next(iter)) {
            v->dump(g_tfile, dm);
        }
    }
};


//Map from const SYM to VAR.
typedef TMap<SYM const*, VAR*> ConstSym2Var;

//Map from VAR id to VAR.
typedef Vector<VAR*> VarVec;

//This class is responsible for allocation and deallocation of VAR.
//User can only create VAR via VarMgr, as well as delete it in the same way.
class VarMgr {
protected:
    size_t m_var_count;
    VarVec m_var_vec;
    ConstSym2Var m_str_tab;
    size_t m_str_count;
    DefSBitSetCore m_freelist_of_varid;
    RegionMgr * m_ru_mgr;
    TypeMgr * m_tm;

protected:
    inline void assignVarId(VAR * v);

public:
    explicit VarMgr(RegionMgr * rm);
    COPY_CONSTRUCTOR(VarMgr);
    virtual ~VarMgr() { destroy(); }

    void destroy();
    void destroyVar(VAR * v); //Free VAR memory.
    void dump(IN OUT CHAR * name = NULL);

    TypeMgr * getTypeMgr() const { return m_tm; }
    VarVec * get_var_vec() { return &m_var_vec; }
    VAR * get_var(size_t id) const { return m_var_vec.get((UINT)id); }

    VAR * findStringVar(SYM const* str) { return m_str_tab.get(str); }
    VAR * findVarByName(SYM const* name);

    bool isDedicatedStringVar(CHAR const* name) const;

    //Interface to target machine.
    //Customer could specify additional attributions for specific purpose.
    virtual VAR * allocVAR() { return new VAR(); }

    //Create a VAR.
    VAR * registerVar(
            CHAR const* varname,
            Type const* type,
            UINT align,
            UINT flag);
    VAR * registerVar(
            SYM const* var_name,
            Type const* type,
            UINT align,
            UINT flag);

    //Create a String VAR.
    VAR * registerStringVar(CHAR const* var_name, SYM const* s, UINT align);
};

} //namespace xoc
#endif
