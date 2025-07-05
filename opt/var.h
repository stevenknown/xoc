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
#define VAR_ID_MAX 10000000

class RegionMgr;
class VarMgr;

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
    //represent some relations between STMT/EXP,
    //or be regarded as a placeholder.
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

    //Variable is region.
    VAR_IS_REGION = 0x20000,

    //Variable is entry function.
    VAR_IS_ENTRY = 0x40000,

    //Variable is section.
    VAR_IS_SECTION = 0x80000,
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

//Link-related attributes of variable.
enum VAR_LINK_ATTR {
    //Undefined.
    VAR_LINK_ATTR_UNDEF   = 0x0,
    //".weak" modifier.
    VAR_LINK_ATTR_WEAK    = 0x1,
    //".visible" modifier.
    VAR_LINK_ATTR_VISIBLE = 0x2,
    //".extern" modifier.
    VAR_LINK_ATTR_EXTERN  = 0x4,
};


class VarLinkAttr : public UFlag {
public:
    VarLinkAttr(UINT v) : UFlag(v) {}
    bool verify() const;
};


class VarLinkAttrDesc {
public:
    ////////////////////////////////////////////////////////////////////
    //NOTE: DO NOT CHANGE THE LAYOUT OF CLASS MEMBERS BECAUSE THEY ARE//
    //CORRESPONDING TO THE SPECIAL INITIALIZING VALUE.                //
    ////////////////////////////////////////////////////////////////////
    VAR_LINK_ATTR attr;
    CHAR const* name;
public:
    //Get the attribute by given index in enum definition.
    static VAR_LINK_ATTR getAttr(UINT idx);

    //Compute the index of 'attr' in the Desc table.
    static UINT getDescIdx(VAR_LINK_ATTR attr);

    //Get link attribute's name.
    static CHAR const* getName(VAR_LINK_ATTR attr);
};


////////////////////////////////////////////////////////
//NOTE: DO *NOT* forget modify the bit-field in Var if//
//you remove/add flag here.                           //
////////////////////////////////////////////////////////
//Variable unique id.
#define VAR_id(v) ((v)->uid)

//Variable type.
#define VAR_type(v) ((v)->type)

#define VAR_name(v) ((v)->name)

//Various flag.
#define VAR_flag(v) ((v)->varflag)

//Record string content if variable is string.
#define VAR_string(v) ((v)->u1.string)

//Record the initial valud index.
#define VAR_byte_val(v) ((v)->u1.byte_val)

//Record prno if variable represent a PR.
#define VAR_prno(v) ((v)->prno)

//Variable has initial value.
#define VAR_has_init_val(v) ((v)->u2.s1.hasInitVal)

//Record the parameter position.
#define VAR_formal_param_pos(v) ((v)->formal_parameter_pos)

//Record the alignment.
#define VAR_align(v) ((v)->align)

//Record the storage space.
#define VAR_storage_space(v) ((v)->storage_space)

//Record the link attributes.
#define VAR_link_attr(v) ((v)->var_link_attr)

class Var {
    COPY_CONSTRUCTOR(Var);
public:
    UINT uid; //unique id.
    UINT align; //memory alignment of Var.
    //Record the formal parameter position if Var is parameter.
    //The position start from 0.
    UINT formal_parameter_pos;

    //Record prno if Var indicates a PR location.
    PRNO prno;
    Type const* type; //data type.
    Sym const* name; //record name of Var.

    //Record the storage space if var is memory object.
    StorageSpace storage_space;
    union {
        //Record string contents if Var is a constant string.
        Sym const* string;

        //Record byte code if Var has constant initial value.
        ByteBuf * byte_val;
    } u1; //record the properties that are exclusive.
    VarFlag varflag;
    //Record the attributes of the current variable related to the link.
    VarLinkAttr var_link_attr;
public:
    Var();
    virtual ~Var() {}

    //The interface to dump declaration information when current
    //variable dumpped. This is target dependent code.
    virtual CHAR const* dumpVARDecl(
        OUT xcom::DefFixedStrBuf &, VarMgr const*) const
    { return nullptr; }

    virtual CHAR const* dumpVARDecl(
        OUT xcom::StrBuf & strbuf, VarMgr const* vm) const
    {
        xcom::DefFixedStrBuf fixstrbuf;
        fixstrbuf.bind(&strbuf);
        dumpVARDecl(fixstrbuf, vm);
        fixstrbuf.unbind();
        return strbuf.getBuf();
    }
    virtual void dump(VarMgr const* vm) const;

    //You must make sure this function will not change any field of Var.
    //The information will be dumpped into 'buf'.
    virtual CHAR const* dump(OUT StrBuf & buf, VarMgr const* vm) const;
    void dumpFlag(xcom::StrBuf & buf, bool grmode, bool & first) const;
    void dumpInitVal(bool first, xcom::StrBuf & buf, bool grmode) const;
    void dumpProp(OUT StrBuf & buf, bool grmode) const;
    CHAR const* dumpGR(OUT StrBuf & buf, TypeMgr * dm) const;

    //Get byte alignment.
    UINT get_align() const { return VAR_align(this); }
    Sym const* get_name() const { return VAR_name(this); }
    Type const* getType() const { return VAR_type(this); }
    DATA_TYPE getDType() const { return TY_dtype(getType()); }
    UINT getFormalParamPos() const { return VAR_formal_param_pos(this); }
    VarFlag const& getFlag() const { return VAR_flag(this); }

    //Get byte length if variable records string contents.
    UINT getStringLength() const
    {
        ASSERT0(getType()->is_string());
        return getString() == nullptr ?
               0 : xcom::xstrlen(getString()->getStr());
    }

    //Get string contents.
    Sym const* getString() const
    {
        ASSERT0(is_string());
        return VAR_string(this);
    }

    //Get PRNO if Var is PR.
    PRNO getPrno() const
    {
        ASSERT0(is_pr());
        return VAR_prno(this);
    }

    //Get initial value.
    ByteBuf const* getByteValue() const
    {
        ASSERT0(hasInitVal());
        return VAR_byte_val(this);
    }

    //Get the storage space.
    StorageSpace getStorageSpace() const { return VAR_storage_space(this); }

    //Return the byte size of variable according type.
    UINT getByteSize(TypeMgr const* dm) const
    {
        //Length of string var should include '\0'.
        return is_string() && hasInitVal() ?
            getStringLength() + 1 : dm->getByteSize(VAR_type(this));
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
                (hasInitVal() && getByteValue() != nullptr));
    }

    //Return true if variable has initial byte value that stored in ByteBuf.
    bool hasInitVal() const { return VAR_flag(this).have(VAR_HAS_INIT_VAL); }

    //Return true if variable has initial string value that stored
    //in VAR_string.
    //Note if the variable is string type and it has initial string value, then
    //the initial string value should be recorded in VAR_string, not in
    //VAR_byte_val.
    bool hasInitString() const
    { return hasInitVal() && is_string() && getString() != nullptr; }

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
    bool is_region() const { return varflag.have(VAR_IS_REGION); }
    bool is_section() const { return varflag.have(VAR_IS_SECTION); }
    bool is_restrict() const { return varflag.have(VAR_IS_RESTRICT); }
    bool is_entry() const { return varflag.have(VAR_IS_ENTRY); }
    bool is_weak() const { return var_link_attr.have(VAR_LINK_ATTR_WEAK); }
    bool is_extern() const { return var_link_attr.have(VAR_LINK_ATTR_EXTERN); }
    bool is_visible() const
    { return var_link_attr.have(VAR_LINK_ATTR_VISIBLE); }
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

    //Set variable is global scope.
    //The global attribute is conform to definition of MD_GLOBAL_VAR of MD.
    void setToGlobal(bool is_global)
    {
        is_global ? (setFlag(VAR_GLOBAL), removeFlag(VAR_LOCAL)) :
                    (removeFlag(VAR_GLOBAL), setFlag(VAR_LOCAL));
    }
    void setToFormalParam() { setFlag(VAR_IS_FORMAL_PARAM); }
    void setFlag(VAR_FLAG f) { VAR_flag(this).set(f); }
    void setLinkAttr(VAR_LINK_ATTR attr) { VAR_link_attr(this).set(attr); }

    void removeFlag(VAR_FLAG f) {VAR_flag(this).remove(f); }
    void removeLinkAttr(VAR_LINK_ATTR attr)
    {
        VAR_link_attr(this).remove(attr);
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
    void dump(VarMgr const* vm) const;
};


class ConstVarTab : public TTab<Var const*, CompareConstVar> {
public:
    void dump(VarMgr const* vm) const;
};


//
//Start class LabelRefill.
//
//This class is used to record the use of labels to initialize variables.
//Because some variables are initialized in the following form:
//
//e.g.
//    u32 var0 = func.label0 - func.label1
//    u64 var1 = { 123, func.label0 - func.label1 }
//
//Note that only "-" is supported.
//
//Where "label0" and "label1" are labels in a function and "var0" and "var1"
//are global variables defined outside this function.
//
//For the first case, "var0" is initialized using the difference between the PC
//values of "label0" and "label1" defined in a function.
//
//For the second case, the second element of "var1" is initialized using the
//difference of the PC values of the "label0" and "label1" defined in the same
//function.
//
//Since the difference of PC values of two labels defined in the same function
//can be determined during compilation, the information stored in this class
//will be used to calculate specific values and refill the initial value of
//variables.
#define LABELREFILL_function(lr) ((lr)->m_function)
#define LABELREFILL_label_0(lr)  ((lr)->m_label_0)
#define LABELREFILL_label_1(lr)  ((lr)->m_label_1)
#define LABELREFILL_offset(lr)   ((lr)->m_offset)
#define LABELREFILL_type(lr)     ((lr)->m_type)
#define LABELREFILL_var(lr)      ((lr)->m_var)
class LabelRefill {
    COPY_CONSTRUCTOR(LabelRefill);
public:
    //The location of the variable that needs to be refilled.
    UINT m_offset;
    //Variables that need to be refilled with initial values.
    Var const* m_var;
    //Type of the initial value for refilling which must be scalar type.
    Type const* m_type;
    //Which function labels are defined.
    Sym const* m_function;
    //Two labels used to calculate initial value.
    LabelInfo const* m_label_0;
    LabelInfo const* m_label_1;
public:
    LabelRefill() {}
    ~LabelRefill() {}

    void setFunction(Sym const* func) { ASSERT0(func); m_function = func; }
    void setLabel0(LabelInfo const* li) { ASSERT0(li); m_label_0 = li; }
    void setLabel1(LabelInfo const* li) { ASSERT0(li); m_label_1 = li; }
    void setOffset(UINT offset) { m_offset = offset; }
    void setType(Type const* tp) { ASSERT0(tp); m_type = tp; }
    void setVar(Var const* var) { ASSERT0(var); m_var = var; }
};
//End class LabelRefill.


//
//Start class LabelReloc.
//
//This class records all cases where single label is used to initialize global
//variables.
//For example:
//
//    u32 var0 = func.label0;
//    u64 var1 = { 1, func.label1 };
//
//Since the PC values represented by "label0" and "label1" can only be
//obtained at linking time, we first add the relocation of the variables to
//point to the location of the code segment represented by label.
//
//For the first case, "var0" points to the location of "label0" in the "func"
//code segment.
//
//For the second case, the second element of "var1" points to the location of
//"label1" in the "func" code segment.
#define LABELRELOC_offset(lr)     ((lr)->m_offset)
#define LABELRELOC_current(lr)    ((lr)->m_current)
#define LABELRELOC_other(lr)      ((lr)->m_other)
#define LABELRELOC_label(lr)      ((lr)->m_label)
class LabelReloc {
    COPY_CONSTRUCTOR(LabelReloc);
public:
    //The position of the relocated symbol.
    UINT m_offset;
    //Which symbol needs to be relocated.
    Sym const* m_current;
    //Which symbol will be relocated to.
    Sym const* m_other;
    //The label defined in the function which is used to calculate the location
    //of the code segment pointed to by the relocation.
    LabelInfo const* m_label;
public:
    LabelReloc() {}
    ~LabelReloc() {}

    void setCurrent(Sym const* sym) { ASSERT0(sym); m_current = sym; }
    void setLabel(LabelInfo const* li) { ASSERT0(li); m_label = li; }
    void setOther(Sym const* sym) { ASSERT0(sym); m_other = sym; }
    void setOffset(UINT offset) { m_offset = offset; }
};
//End class LabelReloc.


//
//Start class VarLabelRelationMgr.
//
class VarLabelRelationMgr {
    COPY_CONSTRUCTOR(VarLabelRelationMgr);
protected:
    //Stored all entries that use the label difference to initialize global
    //variables.
    //Note that RegionMgr will save all information from program region and
    //each function region will extract related information and process it.
    xcom::List<LabelRefill*> m_var_label_refill;
    //Stored all entries that use the single label to initialize global
    //variables.
    xcom::List<LabelReloc*> m_var_label_reloc;
protected:
    //Allocate LabelRefill.
    LabelRefill * allocLabelRefill();

    //Allocate LabelReloc.
    LabelReloc * allocLabelReloc();
public:
    VarLabelRelationMgr() {}
    virtual ~VarLabelRelationMgr();

    //This function will add an entry of using label difference to initialize
    //a global variable(scalar) or a location of a global variable(vector).
    //
    //For example:
    //
    //  u32 var0 = { func0.label_b - func0.label_a,
    //               func1.label_d - func1.label_c }
    //
    //  function func0()       function func1()
    //  {                      {
    //      ......                 ......
    //      label_a                label_c
    //      ......                 ......
    //      label_b                label_d
    //      ......                 ......
    //  }                      }
    //
    //Where the first element of "var0" is initialized using the difference
    //between "label_b" and "label_a" defined in "func0", and the second
    //element is initialized using the difference between "label_d" and
    //"label_c" defined in "func1".
    //
    //When processing the first element, the parameters are:
    //  var: var0; offset: 0(Byte); tp: u32; function: func0;
    //  label0: label_b; label1: label_a;
    //
    //When processing the second element, the parameters are:
    //  var: var0; offset: 4(Byte); tp: u32; function: func1;
    //  label0: label_d; label1: label_c;
    //
    void addVarLabelRefill(Var const* var, UINT offset, Type const* tp,
        Sym const* function, LabelInfo const* label0, LabelInfo const* label1);

    //Add relocation items using label for symbols.
    //
    //parameters:
    //    current: Symbol initialized with label.
    //    other:   Symbol that label defined in.
    //    offset:  Location where current symbol occurs.
    //    li:      Label used for initialization.
    //
    //e.g.
    //    u32 arr0 = { 1, foo.label0 };
    //    function foo()
    //    {
    //        ......
    //        .label label0
    //        ......
    //    }
    //
    //    current is "arr0", other is "foo", offset is 4(B), li is "label0".
    void addVarLabelRelocation(Sym const* current, Sym const* other,
        UINT offset, LabelInfo const* li);

    xcom::List<LabelRefill*> * getVarLabelRefill()
    { return &m_var_label_refill; }
    xcom::List<LabelReloc*> * getVarLabelReloc()
    { return &m_var_label_reloc; }
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
protected:
    size_t m_var_count;
    VarVec m_var_vec;
    ConstSym2Var m_str_tab;
    size_t m_str_count;
    DefSBitSetCore m_freelist_of_varid;
    RegionMgr * m_rm;
    TypeMgr * m_tm;
protected:
    //Assign an unique ID to given variable.
    void assignVarId(Var * v);
    void dumpFreeIDList() const;
public:
    explicit VarMgr(RegionMgr * rm);
    virtual ~VarMgr() { destroy(); }

    //Destroy holistic variable manager.
    void destroy();

    //Destroy specific variable and recycle its ID for next allocation.
    void destroyVar(Var * v);
    void dump() const;

    TypeMgr * getTypeMgr() const { return m_tm; }
    RegionMgr * getRegionMgr() const { return m_rm; }

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
    Var * findVarByName(CHAR const* name);

    //Return true if the 'name' is the name of recorded unique
    //dedicated string variable.
    bool isDedicatedStringVar(CHAR const* name) const;

    //Interface to target machine.
    //Customer could specify additional attributions for specific purpose.
    virtual Var * allocVAR() { return new Var(); }

    //Create variable by string name.
    //Add Var into VarTab.
    //Note you should call this function cafefully, and make sure
    //the Var is unique. This function does not keep the uniqueness
    //related to properties.
    //var_name: name of the variable, it is optional.
    Var * registerVar(CHAR const* varname, Type const* type, UINT align,
                      VarFlag const& flag);

    //Create variable by symbol name.
    //Add Var into VarTab.
    //Note you should call this function cafefully, and make sure
    //the Var is unique. This function does not keep the uniqueness
    //related to properties.
    //var_name: name of the variable, it is optional.
    Var * registerVar(Sym const* var_name, Type const* type, UINT align,
                      VarFlag const& flag);

    //Create string variable by name and string-content.
    //Register Var for const string.
    //Return Var if there is already related to 's',
    //otherwise create a new Var.
    //var_name: name of the variable, it is optional.
    //s: string's content.
    Var * registerStringVar(CHAR const* var_name, Sym const* s, UINT align);

    //The function verify that the given variable information is sane.
    bool verifyVar(Var const* v) const;

    //The function verify all variables generated to guarantee they are sane.
    bool verifyAllVar() const;
};

} //namespace xoc
#endif
