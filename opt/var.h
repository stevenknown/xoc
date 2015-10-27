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

//************************************************
//NOTE: Do *NOT* forget modify the bit-field in VAR if
//you remove/add flag here.
//************************************************

#define VAR_UNDEF                0x0
#define VAR_GLOBAL               0x1    //can be seen by all functions.

//This kind of variable only can be seen by current function or thread.
//It always be allocated in stack or thread local storage(TLS).
#define VAR_LOCAL                0x2

//This kind of variable can be seen by all functions in same file.
#define VAR_STATIC               0x4
#define VAR_READONLY             0x8 //var is readonly
#define VAR_VOLATILE             0x10 //var is volatile
#define VAR_HAS_INIT_VAL         0x20 //var with initialied value.
#define VAR_FAKE                 0x40 //var is fake
#define VAR_FUNC_UNIT            0x80 //var is function unit.
#define VAR_FUNC_DECL            0x100 //var is function declaration.
#define VAR_IS_ARRAY             0x200 //var is array.
#define VAR_IS_FORMAL_PARAM      0x400 //var is formal parameter.
#define VAR_IS_SPILL             0x800 //var is spill location.
#define VAR_ADDR_TAKEN           0x1000 //var's address has been taken.
#define VAR_IS_PR                0x2000 //var is pr.
#define VAR_IS_RESTRICT          0x4000 //var is restrict.
#define VAR_IS_ALLOCABLE         0x8000 //var is allocable on memory.
//************************************************
//NOTE: Do *NOT* forget modify the bit-field in VAR if
//you remove/add flag here.
//************************************************

//Variable unique id.
#define VAR_id(v)                ((v)->id)

//Variable type.
#define VAR_type(v)              ((v)->type)

#define VAR_name(v)              ((v)->name)

//Various flag.
#define VAR_flag(v)              ((v)->u2.flag)

//Record string content if variable is string.
#define VAR_str(v)               ((v)->u1.string)

//Variable is global.
#define VAR_is_global(v)         ((v)->u2.u2s1.is_global)

//Variable is local.
#define VAR_is_local(v)          ((v)->u2.u2s1.is_local)

//Global Variables which is static cannot be referenced outside this region.
#define VAR_is_static(v)         ((v)->u2.u2s1.is_static)

//Variable is readonly.
#define VAR_is_readonly(v)       ((v)->u2.u2s1.is_readonly)

//Record the initial valud index.
#define VAR_init_val_id(v)       ((v)->u1.init_val_id)

//Variable has initial value.
#define VAR_has_init_val(v)      (VAR_init_val_id(v) != 0)

//Variable is region.
#define VAR_is_func_unit(v)      ((v)->u2.u2s1.is_func_unit)

//Variable is region.
#define VAR_is_func_decl(v)      ((v)->u2.u2s1.is_func_decl)

//Variable is aritifical or spurious that used to
//faciliate optimizations and analysis.
#define VAR_is_fake(v)           ((v)->u2.u2s1.is_fake)

//Variable is volatile.
#define VAR_is_volatile(v)       ((v)->u2.u2s1.is_volatile)

//Variable is an array.
#define VAR_is_array(v)          ((v)->u2.u2s1.is_array)

//Variable is parameter of this region.
#define VAR_is_formal_param(v)   ((v)->u2.u2s1.is_formal_param)

//Variable is spill location.
#define VAR_is_spill(v)          ((v)->u2.u2s1.is_spill)

//Variable has been taken address.
#define VAR_is_addr_taken(v)     ((v)->u2.u2s1.is_addr_taken)

//Variable is PR.
#define VAR_is_pr(v)             ((v)->u2.u2s1.is_pr)

//Variable is marked "restrict", and it always be parameter.
#define VAR_is_restrict(v)       ((v)->u2.u2s1.is_restrict)

//Variable is concrete, and will be output to Code Generator.
#define VAR_allocable(v)         ((v)->u2.u2s1.is_allocable)

//Record the alignment.
#define VAR_align(v)             ((v)->align)
class VAR {
public:
    UINT id; //unique id;
    Type const* type; //Data type.
    UINT align; //memory alignment of var.
    SYM * name;

    union {
        //Record string contents if VAR is string.
        SYM * string;

        //Index to constant value table.
        //This value is 0 if no initial value.
        //Record constant content if VAR has constant initial value.
        UINT init_val_id;
    } u1;
    union {
        UINT flag; //Record variant properties of VAR.
        struct {
            UINT is_global:1;         //VAR can be seen all program.
            UINT is_local:1;         //VAR only can be seen in region.
            UINT is_static:1;         //VAR only can be seen in file.
            UINT is_readonly:1;         //VAR is readonly.
            UINT is_volatile:1;     //VAR is volatile.
            UINT has_init_val:1;     //VAR has initial value.
            UINT is_fake:1;         //VAR is fake.
            UINT is_func_unit:1;     //VAR is function unit with body defined.
            UINT is_func_decl:1;    //VAR is function unit declaration.
            UINT is_array:1;         //VAR is array
            UINT is_formal_param:1; //VAR is formal parameter.
            UINT is_spill:1;         //VAR is spill location in function.
            UINT is_addr_taken:1;     //VAR has been taken address.
            UINT is_pr:1;             //VAR is pr.
            UINT is_restrict:1;        //VAR is restrict.

            //True if var should be allocate on memory or
            //false indicate it is only being a placeholder and do not
            //allocate in essence.
            UINT is_allocable:1;
        } u2s1;
    } u2;

public:
    VAR();
    virtual ~VAR() {}

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

    UINT getStringLength() const
    {
        ASSERT0(VAR_type(this)->is_string());
        return xstrlen(SYM_name(VAR_str(this)));
    }

    //Return the byte size of variable accroding type.
    UINT getByteSize(TypeMgr const* dm) const
    {
        //Length of string var should include '\0'.
        return is_string() ?
                getStringLength() + 1:
                dm->get_bytesize(VAR_type(this));
    }

    virtual CHAR * dumpVARDecl(CHAR*, UINT) { return NULL; }
    virtual void dump(FILE * h, TypeMgr const* dm);
    virtual CHAR * dump(CHAR * buf, TypeMgr const* dm);
};
//END VAR

typedef TabIter<VAR*> VarTabIter;

class CompareVar {
public:
    bool is_less(VAR * t1, VAR * t2) const { return t1 < t2; }
    bool is_equ(VAR * t1, VAR * t2) const { return t1 == t2; }
};


class CompareConstVar {
public:
    bool is_less(VAR const* t1, VAR const* t2) const { return t1 < t2; }
    bool is_equ(VAR const* t1, VAR const* t2) const { return t1 == t2; }
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


//Map from SYM to VAR.
typedef TMap<SYM*, VAR*> Sym2Var;

//Map from const SYM to VAR.
typedef TMap<SYM const*, VAR*> ConstSym2Var;

//Map from VAR id to VAR.
//typedef TMap<UINT, VAR*> ID2VAR;
typedef Vector<VAR*> ID2VAR;

//This class is responsible for allocation and deallocation of VAR.
//User can only create VAR via VarMgr, as well as delete it in the same way.
class VarMgr {
protected:
    size_t m_var_count;
    ID2VAR m_var_vec;
    Sym2Var m_str_tab;
    size_t m_str_count;
    List<UINT> m_freelist_of_varid;
    RegionMgr * m_ru_mgr;
    TypeMgr * m_dm;

protected:
    inline void assignVarId(VAR * v);

public:
    explicit VarMgr(RegionMgr * rm);
    COPY_CONSTRUCTOR(VarMgr);
    virtual ~VarMgr() { destroy(); }

    void destroy()
    {
        for (INT i = 0; i <= m_var_vec.get_last_idx(); i++) {
            VAR * v = m_var_vec.get((UINT)i);
            if (v == NULL) { continue; }
            delete v;
        }
    }

    void dump(IN OUT CHAR * name = NULL);

    TypeMgr * get_type_mgr() const { return m_dm; }
    ID2VAR * get_var_vec() { return &m_var_vec; }

    VAR * findStringVar(SYM * str) { return m_str_tab.get(str); }

    //Interface to target machine.
    //Customer could specify additional attributions for specific purpose.
    virtual VAR * newVar()    { return new VAR(); }

    //Free VAR memory.
    inline void destroyVar(VAR * v)
    {
        ASSERT0(VAR_id(v) != 0);
        m_freelist_of_varid.append_head(VAR_id(v));
        m_var_vec.set(VAR_id(v), NULL);
        delete v;
    }

    //Create a VAR.
    VAR * registerVar(
            CHAR const* varname, Type const* type,
            UINT align, UINT flag);
    VAR * registerVar(SYM * var_name, Type const* type, UINT align, UINT flag);

    //Create a String VAR.
    VAR * registerStringVar(CHAR const* var_name, SYM * s, UINT align);
};

} //namespace xoc
#endif
