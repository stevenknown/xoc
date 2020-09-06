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
#ifndef _DEX_H_
#define _DEX_H_

#ifdef _ENABLE_LOG_
#define LOG ALOGI
#else
#define LOG
#endif

typedef enum _INVOKE_KIND {
    INVOKE_UNDEF = 0,
    INVOKE_VIRTUAL,
    INVOKE_SUPER,
    INVOKE_DIRECT,
    INVOKE_STATIC,
    INVOKE_INTERFACE,
    INVOKE_VIRTUAL_RANGE,
    INVOKE_SUPER_RANGE,
    INVOKE_DIRECT_RANGE,
    INVOKE_STATIC_RANGE,
    INVOKE_INTERFACE_RANGE,
} INVOKE_KIND;

typedef enum _BLTIN_TYPE {
    BLTIN_UNDEF = 0,
    BLTIN_NEW,
    BLTIN_NEW_ARRAY,
    BLTIN_MOVE_EXP,
    BLTIN_MOVE_RES,
    BLTIN_THROW,
    BLTIN_CHECK_CAST,
    BLTIN_FILLED_NEW_ARRAY,
    BLTIN_FILL_ARRAY_DATA,
    BLTIN_CONST_CLASS,
    BLTIN_ARRAY_LENGTH,
    BLTIN_MONITOR_ENTER,
    BLTIN_MONITOR_EXIT,
    BLTIN_INSTANCE_OF,
    BLTIN_CMP_BIAS,
    BLTIN_LAST,
} BLTIN_TYPE;


#define BLTIN_type(t)            (g_builtin_info[(t)].ty)
#define BLTIN_name(t)            (g_builtin_info[(t)].name)
class BuiltInInfo {
public:
    BLTIN_TYPE ty;
    CHAR const* name;
};


extern UINT g_builtin_num;
extern BuiltInInfo g_builtin_info[];


class Var2UINT : public TMap<Var const*, UINT> {
public:
    Var2UINT() {}
    virtual ~Var2UINT() {}

    UINT get_mapped(Var const* v) const
    {
        bool find;
        UINT i = TMap<Var const*, UINT>::get(v, &find);
        ASSERT0(find);
        return i;
    }
};


class UINT2Var : public TMap<UINT, Var*> {
public:
    UINT2Var() {}
    virtual ~UINT2Var() {}

    Var * get_mapped(UINT u)
    {
        ASSERT0(u != 0);
        bool find;
        Var * v = TMap<UINT, Var*>::get(u, &find);
        ASSERT0(find);
        return v;
    }
};


class Prno2Vreg : public HMap<UINT, UINT, HashFuncBase2<UINT> > {
public:
    INT maxreg; //record the max vreg used.
    UINT paramnum; //record the number of parameter.

    Prno2Vreg(UINT sz = 0) : HMap<UINT, UINT, HashFuncBase2<UINT> >(sz)
    {
        maxreg = -1;
        paramnum = 0;
    }
    virtual ~Prno2Vreg() {}

    UINT get(UINT prno)
    {
        bool find = false;
        UINT x = HMap<UINT, UINT, HashFuncBase2<UINT> >::get(prno, &find);
        ASSERT0(find);
        return x;
    }

    UINT get_mapped_vreg(UINT prno)
    {
        UINT mapped = 0;
        bool f = HMap<UINT, UINT, HashFuncBase2<UINT> >::find(prno, &mapped);
        DUMMYUSE(f);
        ASSERTN(f, ("prno should be mapped with vreg in dex2ir"));
        //return HMap<UINT, UINT, HashFuncBase2<UINT> >::get(prno);
        return mapped;
    }

    UINT get(UINT prno, bool * find)
    { return HMap<UINT, UINT, HashFuncBase2<UINT> >::get(prno, find); }

    void set(UINT prno, UINT v)
    {
        ASSERT0(prno > 0);
        HMap<UINT, UINT, HashFuncBase2<UINT> >::set(prno, v);
    }

    void copy(Prno2Vreg & src)
    {
        INT cur;
        for (UINT prno = src.get_first(cur); cur >= 0; prno = src.get_next(cur)) {
            UINT v = src.get(prno);
            set(prno, v);
        }
        maxreg = src.maxreg;
        paramnum = src.paramnum;
    }

    void dump(Region const* rg)
    {
        if (!rg->isLogMgrInit()) { return; }
        note(rg, "\n==---- DUMP Prno2Vreg ----==");
        rg->getLogMgr()->incIndent(4);

        if (maxreg < 0) {
            note(rg, "\n==------ PR to Vreg is unordered ------==");
            INT cur;
            for (UINT prno = get_first(cur); cur >= 0; prno = get_next(cur)) {
                UINT v = get(prno);
                note(rg, "\nPR%d->v%d", prno, v);
            }
        } else {
            INT cur;
            for (INT i = 0; i <= maxreg; i++) {
                bool find = false;
                for (UINT prno = get_first(cur); cur >= 0; prno = get_next(cur)) {
                    UINT v = get(prno);
                    if (v == (UINT)i) {
                        note(rg, "\nPR%d -> v%d", prno, v);
                        find = true;
                        break;
                    }
                }

                if (!find) {
                    note(rg, "\n-- -> v%d", i);
                }
            }
        }
        rg->getLogMgr()->decIndent(4);
    }
};


class Vreg2PR : public Vector<IR*> {
public:
    void dump(Region const* rg)
    {
        if (!rg->isLogMgrInit()) { return; }

        note(rg, "\n==---- DUMP Prno2Vreg ----==");

        for (INT i = 0; i <= get_last_idx(); i++) {
            IR * pr = get(i);
            if (pr == NULL) {
                note(rg, "\nv%d -> --", i);
            }
            note(rg, "\nv%d -> PR%u", i, pr->getPrno());
        }
    }
};


class Str2BuiltinType : public HMap<CHAR const*, BLTIN_TYPE, HashFuncString> {
public:
    Str2BuiltinType(UINT sz = 13) : HMap<CHAR const*, BLTIN_TYPE, HashFuncString>(sz)
    {
        for (UINT i = BLTIN_UNDEF + 1; i < g_builtin_num; i++) {
            set(BLTIN_name((BLTIN_TYPE)i), (BLTIN_TYPE)i);
        }
    }
    virtual ~Str2BuiltinType() {}
};


//Map from typeIdx of type-table to positionIdx in file-class-def-table.
class FieldTypeIdx2PosIdx : public TMap<UINT, UINT> {
public:
};


class DexDbx : public BaseAttachInfo {
public:
    DexDbx(AI_TYPE t = AI_DBX) : BaseAttachInfo(t) {}
    UINT linenum;
    CHAR const* filename;
};


class OffsetVec : public Vector<UINT, 16> {
public:
};


class DbxVec : public Vector<DexDbx*> {
public:
    DbxVec(UINT size) : Vector<DexDbx*>(size) {}
};


inline bool is_us4(UINT value)
{ return value == (value & 0xf); }

inline bool is_s4(INT value)
{ return (value >= -8) && (value <= 7); }

inline bool is_us8(UINT value)
{ return value == (value & 0xff); }

inline bool is_s8(INT value)
{ return (CHAR)value == value; }

inline bool is_us16(UINT value)
{ return value == (value & 0xffff); }

inline bool is_s16(INT value)
{ return (SHORT) value == value; }

inline bool is_swide16(LONGLONG value)
{ return (LONGLONG)((SHORT)value) == value; }

inline bool is_swide32(LONGLONG value)
{ return (LONGLONG)((INT)value) == value; }

//Record type to facilitate type comparation.
class TypeIndexRep {
public:
    Type const* i8;
    Type const* u8;
    Type const* i16;
    Type const* u16;
    Type const* i32;
    Type const* u32;
    Type const* i64;
    Type const* u64;
    Type const* f32;
    Type const* f64;
    Type const* b;
    Type const* ptr;
    Type const* array;
    Type const* obj_lda_base_type;
    Type const* uint16x4;
    Type const* int16x4;
    Type const* uint32x4;
    Type const* int32x4;
    Type const* uint64x2;
    Type const* int64x2;

public:
    TypeIndexRep() { ::memset(this, 0, sizeof(TypeIndexRep)); }
};

//Perform Dex register allocation.
extern bool g_do_dex_ra;

//Set true to collect debug info.
extern bool g_collect_debuginfo;
extern bool g_dump_ir2dex;
extern bool g_dump_dex2ir;
extern bool g_dump_classdefs;
extern bool g_dump_lirs;
extern bool g_is_pretty_print_method_name;
extern bool g_dump_dex_file_path;
extern bool g_record_region_for_classs;
#endif
