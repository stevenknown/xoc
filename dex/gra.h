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
#ifndef __GRA_H__
#define __GRA_H__

#define LT_FIRST_POS    0
#define RG_PAIR_SZ         2
#define FIRST_PHY_REG    0

class GLT;
class LTMgr;
class LTG;
class RA;
class GltMgr;
class RSC;
class RG;

//Local Life Time.
#define LT_uid(lt)              ((lt)->id)
#define LT_range(lt)            ((lt)->range)
#define LT_occ(lt)              ((lt)->occ)
#define LT_usable(lt)           ((lt)->usable)
#define LT_prno(lt)             ((lt)->prno)
#define LT_priority(lt)         ((lt)->priority)
#define LT_phy(lt)              ((lt)->phy)
#define LT_prefer_reg(lt)       ((lt)->prefer_reg)
#define LT_is_global(lt)        ((lt)->is_global_pr)
#define LT_ltg(lt)              ((lt)->lt_group)
#define LT_ltg_pos(lt)          ((lt)->lt_group_pos)
#define LT_rg(lt)               ((lt)->reg_group)
#define LT_rg_sz(lt)            ((lt)->reg_group_size)
class LT {
public:
    UINT prno;
    UINT id;
    BitSet * range;
    BitSet * occ;
    BitSet const* usable;
    LTG * lt_group;
    RG * reg_group;    //register group
    float priority;
    USHORT phy;    //physical register.
    USHORT prefer_reg;
    USHORT reg_group_size; //the size of register group.
    BYTE lt_group_pos:5;
    BYTE is_global_pr:1; //true if pr is global.

    void clean();
    inline bool is_intersect(LT const* lt) const
    {
        ASSERT0(range);
        return LT_range(this)->is_intersect(*LT_range(lt));
    }
    bool is_def(UINT pos) const { return (pos & 1) == 0; }
    bool is_reg_equal(LT const* l) const
    { return prno == LT_prno(l) || phy == LT_phy(l); }

    //Get forward searching occurrence corresponds to 'pos',
    //or return -1 if nothing find.
    //e.g:
    //    lowest_pos....pos...forward_occ...highest_pos
    inline INT getForwardOcc(INT pos, OUT bool * is_def, INT firstpos)
    {
        DUMMYUSE(firstpos);
        ASSERTN(pos >= firstpos, ("Illegal position"));
        ASSERT0(occ);
        pos = occ->get_next(pos);
        *is_def = this->is_def(pos);
        return pos;

        //for (INT i = range->get_next(pos);
        //     i >= 0; i = range->get_next(i)) {
        //    IR const* p = range->get(i);
        //    if (p != NULL) {
        //        *is_def = this->is_def(i);
        //        return i;
        //    }
        //}
        //*is_def = false;
        //return -1;
    }

    //Get forward searching def corresponds to 'pos',
    //or return -1 if nothing find.
    //e.g:
    //    lowest_pos....pos...forward_def...highest_pos
    inline INT getForwardOccForDEF(INT pos, INT firstpos)
    {
        DUMMYUSE(firstpos);
        ASSERTN(pos >= firstpos, ("Illegal position"));
        ASSERT0(occ);
        for (pos = occ->get_next(pos); pos >= 0; pos = occ->get_next(pos)) {
            if (is_def(pos)) {
                return pos;
            }
        }
        return -1;
    }

    //Get backward occurrences of 'pos'
    //e.g:
    //    Lowest_Pos...Backward_Occ....Pos.....Highest_Pos
    inline INT getBackwardOcc(INT pos, OUT bool * is_d, INT firstpos)
    {
        ASSERTN(pos >= firstpos, ("Illegal position"));
        if (pos == firstpos && (occ == NULL || occ->is_empty())) { return -1; }

        INT backwpos = -1;
        INT start = LT_range(this)->get_first();
        for (INT i = pos - 1; i >= start; i--) {
            if (!occ->is_contain(i)) { continue; }
            backwpos = i;
            if (is_def(i)) {
                *is_d = true;
            } else {
                *is_d = false;
            }
            break;
        }
        return backwpos;
    }

    inline INT getBackwardOccForDEF(INT pos, INT firstpos)
    {
        ASSERTN(pos >= firstpos, ("Illegal position"));
        if (pos == firstpos && (occ == NULL || occ->is_empty())) { return -1; }

        INT start = LT_range(this)->get_first();
        for (INT i = pos - 1; i >= start; i--) {
            if (!occ->is_contain(i)) { continue; }
            if (is_def(i)) {
                return i;
            }
        }
        return -1;
    }

    bool has_allocated() const { return LT_phy(this) != REG_UNDEF; }
    bool has_branch(LTMgr * ltm) const;

    GLT * set_global(GltMgr & gltm);
};


class PR2LT : public HMap<UINT, LT*> {
public:
    PR2LT(UINT bsize = 0) : HMap<UINT, LT*>(bsize) {}
};


typedef enum _LTG_TYPE {
    LTG_UNDEF = 0,
    LTG_RANGE_PARAM,
    LTG_REG_PAIR,
} LTG_TYPE;

//Lifetime Group.
class LTG : public Vector<LT*> {
public:
    LTG_TYPE ty;

public:
    LTG() { ty = LTG_UNDEF; }
    COPY_CONSTRUCTOR(LTG);

    //Return the number of lt in group.
    UINT getLiftTimeCount() const { return get_last_idx() + 1; }

    //Return register group size.
    UINT get_rg_sz() const
    {
        UINT rgsz = 0;
        for (INT i = 0; i <= get_last_idx(); i++) {
            LT const* l = get(i);
            ASSERT0(l);
            rgsz += LT_rg_sz(l);
        }
        return rgsz;
    }

    //Return true if the lt that corresponded to 'prno' is in group.
    bool is_member(UINT prno)
    {
        for (INT i = 0; i <= get_last_idx(); i++) {
            LT * l = get(i);
            ASSERT0(l);
            if (LT_prno(l) == prno) {
                return true;
            }
        }
        return false;
    }
};


//Register Group.
class RG {
public:
    BYTE rnum; //the number of group member
    USHORT * rvec; //buffer to hold group member.

public:
    void set(UINT i, UINT phy)
    {
        ASSERT0(i < rnum && rvec && phy != REG_UNDEF);
        rvec[i] = (USHORT)phy;
    }

    UINT get(UINT i)
    {
        ASSERT0(i < rnum && rvec);
        return (UINT)rvec[i];
    }
};


//Register Group Mgr
class RGMgr {
    SMemPool * m_pool;
public:
    RGMgr() { m_pool = smpoolCreate(0, MEM_COMM); }
    COPY_CONSTRUCTOR(RGMgr);
    ~RGMgr() { smpoolDelete(m_pool); }

    RG * create(UINT n)
    {
        RG * rg = (RG*)smpoolMalloc(sizeof(RG), m_pool);
        rg->rvec = (USHORT*)smpoolMalloc(sizeof(USHORT) * n, m_pool);
        rg->rnum = (BYTE)n;
        return rg;
    }
};


//Map from IR to INT.
typedef TMap<IR*, INT> IR2INT;
typedef TMapIter<IR*, INT> IR2INTIter;


//Lifetime Group Mgr
class LTGMgr : public TMap<UINT, LTG*> {
    List<LTG*> m_ltgs;
public:
    virtual ~LTGMgr()
    {
        for (LTG * ltg = m_ltgs.get_head();
             ltg != NULL; ltg = m_ltgs.get_next()) {
            delete ltg;
        }
    }

    LTG * map_ir2ltg(UINT irid)
    { return get(irid); }

    void set_map_ir2ltg(UINT irid, LTG * ltg)
    { set(irid, ltg); }

    LTG * create()
    {
        LTG * ltg = new LTG();
        m_ltgs.append_tail(ltg);
        return ltg;
    }
};


class IG : public Graph {
    LTMgr * m_ltm;
public:
    IG() {}
    void set_ltm(LTMgr * ltm) { ASSERT0(ltm); m_ltm = ltm; }
    bool is_interf(LT const* lt1, LT const* lt2) const;
    void build();
    void dumpVCG(CHAR const* name = NULL);
    void get_neighbor(OUT List<LT*> & nis, LT * lt) const;
};


//Local Life Time Manager.
//TODO: update lt incrementally.
class LTMgr {
protected:
    friend class GltMgr;
    friend class BBRA;
    friend class RSC;
    friend class RA;
    IRBB * m_bb;
    //PR2LT m_prno2lt_map;
    TMap<UINT, LT*> m_prno2lt;
    Vector<LT*> m_lt_vec;
    Vector<IR*> m_pos2ir;
    IG m_ig;
    SMemPool * m_pool;
    LivenessMgr * m_liveness_mgr;
    Prno2Vreg * m_pr2v;
    Vreg2PR * m_v2pr;
    GltMgr * m_gltm;
    TypeMgr * m_tm;
    Region * m_rg;
    RA * m_ra;
    UINT m_lt_count; //local lt count.
    UINT m_max_lt_len;

protected:
    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p != NULL);
        ::memset(p, 0, size);
        return p;
    }

    void reviseLTCase1(LT * lt);
    void revise_special_lt(List<LT*> * lts);
    void process_rg(LT * lt);
    void processLiveout(IN OUT BitSet & lived_lt, UINT pos,
                         bool always_consider_glt);
    void processLivein(IN OUT BitSet & lived_lt, UINT pos,
                        bool always_consider_glt);
    void processExitBB(IN OUT List<LT*> * liveout_exitbb,
                              IN OUT BitSet & lived_lt,
                              BitSet const& retval_regset, UINT pos);
    void processUse(IN IR * ir, ConstIRIter & cii, INT pos,
                     OUT BitSet & lived_lt, bool group_part);
    void processResult(IN IR * ir, INT pos, OUT BitSet & lived_lt,
                     bool group_part);
    LT * processUsePR(IR const* ir, UINT pos, OUT BitSet & lived_lt);
    LT * processResultPR(UINT prno, UINT pos, OUT BitSet & lived_lt);
    void processUseGroupPart(IR const* ir, UINT pos, OUT BitSet & lived_lt);
    void processResultGroupPart(IR const* ir, UINT pos, OUT BitSet & lived_lt);

    void genRangeCallGroup(IR const* ir);
    IR * genDedicatePR(UINT phyrid);
    void recordPhyRegOcc(LT * lt, UINT pos, IN BitSet & lived_lt);
    void renameUse(IR * ir, LT * l, IR ** newpr);
    void dump_allocated(FILE * h, BitSet & visited);
    void dump_unallocated(FILE * h, BitSet & visited);
public:
    LTMgr(IRBB * bb, LivenessMgr * prdf, GltMgr * gltm, SMemPool * pool);
    COPY_CONSTRUCTOR(LTMgr);
    ~LTMgr() {}

    void build(bool consider_glt, List<LT*> * liveout_exitbb_lts,
               ConstIRIter & iter);
    void buildGroup(ConstIRIter & cii);

    void clean();


    IRBB * getBB() { return m_bb; }
    LT * getLifeTime(UINT ltid) { return m_lt_vec.get(ltid); }
    Vector<LT*> * get_lt_vec() { return &m_lt_vec; }
    UINT get_first_pos() const { return LT_FIRST_POS; }
    UINT get_last_pos() const { return m_max_lt_len - 1; }
    IR * getIR(UINT pos) { return m_pos2ir.get(pos); }
    IG * get_ig() { return &m_ig; }

    IR * genMappedPR(UINT vid, Type const* ty);
    void genGroup(LT * first, LT * second);
    IR * genSpill(LT * lt, INT pos);
    IR * genSpill(UINT prno, Type const* type, IR * marker, IR * spill_loc);
    IR * genSpillSwap(IR * stmt, UINT prno, Type const* prty, IR * spill_loc);
    IR * genReload(LT * lt, INT pos, IR * spill_loc);
    IR * genReload(IR * newpr, IR * marker, IR * spill_loc);
    IR * genReloadSwap(IR * newpr, IR * marker);

    bool has_pair_res(IR * ir);

    inline bool is_pair(IR const* ir) const
    { return m_tm->getByteSize(IR_dt(ir))== 8; }

    inline bool is_livein(UINT prno) const
    { return m_liveness_mgr->get_livein(m_bb->id())->is_contain(prno); }
    inline bool is_livein(LT const* l) const
    {
        ASSERT0(LT_range(l));
        return LT_range(l)->is_contain(get_first_pos());
    }

    inline bool is_liveout(UINT prno) const
    { return m_liveness_mgr->get_liveout(m_bb->id())->is_contain(prno); }
    inline bool is_liveout(LT const* l) const
    {
        ASSERT0(LT_range(l));
        return LT_range(l)->is_contain(get_last_pos());
    }

    inline LT * map_pr2lt(UINT prno) const
    {
        if (m_prno2lt.get_elem_count() == 0) {
            return NULL;
        }
        return m_prno2lt.get(prno);
    }

    LT * newLT(UINT prno);

    void removeLifeTime(LT * lt);
    void renameLT(LT * l, IR ** newpr);
    void rename(TMap<UINT, LT*> & prno2lt, BitSet & met);

    void dump();
};


#define GLT_id(g) ((g)->id)
#define GLT_prno(g) ((g)->prno)
#define GLT_phy(g) ((g)->phy)
#define GLT_prefer_reg(g) ((g)->prefer_reg)
#define GLT_bbs(g) ((g)->livebbs)
#define GLT_prio(g) ((g)->prio)
#define GLT_freq(g) ((g)->freq)
#define GLT_usable(g) ((g)->usable)
#define GLT_rg(g) ((g)->reg_group)
#define GLT_rg_sz(g) ((g)->reg_group_size)
#define GLT_is_param(g) ((g)->is_param)
#define GLT_param_pos(g) ((g)->param_pos)
class GLT {
public:
    UINT id;
    UINT prno;
    float prio;
    float freq;
    DefDBitSetCore * livebbs;
    RG * reg_group;
    BitSet const* usable;
    USHORT phy;
    USHORT prefer_reg;
    USHORT reg_group_size; //the size of register group.
    BYTE param_pos;
    BYTE is_param:1;

    UINT computeNumOfOcc(GltMgr & gltm);
    bool has_allocated() const { return GLT_phy(this) != REG_UNDEF; }
    void set_local(GltMgr & gltm);
    void set_local_usable(GltMgr & gltm);
};


class GLT2REG : public HMap<GLT*, UINT> {
public:
    GLT2REG() : HMap<GLT*, UINT>(0) {}

    void set(GLT * g, UINT v)
    {
        ASSERT0(v != 0);
        HMap<GLT*, UINT>::set(g, v);
    }
};


//Format of DEX instructions.
//c: constant.
//v: value, can be offset or idx.
//h: high part value.
typedef enum _FMT {
    FUNDEF = 0,
    F0,             //op
    FAB,            // op vA, vB
    FABcv,          // op vA, #+B
    FAA,            // op vAA
    FAAv,           // op +AA
    FAAAAv,         // op +AAAA
    FAABBBB,        // op vAA, vBBBB

    FAABBBBv,       // op vAA, +BBBB
                    // op vAA, thing@BBBB
    FAABBBBcv,      // op vAA, #+BBBB
    FAABBBBcvh,     // op vAA, #+BBBB00000[00000000]
    FAABBCC,        // op vAA, vBB, vCC
    FAABBCCcv,      // op vAA, vBB, #+CC
    FABCCCCv,       // op vA, vB, +CCCC
    FABCCCCcv,      // op vA, vB, #+CCCC
                    // op vA, vB, thing@CCCC
    FAAAABBBB,      // op vAAAA, vBBBB
    FAAAAAAAAv,     // op +AAAAAAAA
    FAABBBBBBBBv,   // op vAA, +BBBBBBBB
                    // op vAA, thing@BBBBBBBB
    FAABBBBBBBBcv,  // op vAA, #+BBBBBBBB
    FACDEFGBBBBv,   // op {vC, vD, vE, vF, vG}, thing@BBBB (B: count, A: vG)
    FAACCCCBBBBv,   // op {vCCCC .. v(CCCC+AA-1)}, meth@BBBB
    FAABBBBBBBBBBBBBBBBcv,  // op vAA, #+BBBBBBBBBBBBBBBB
    FNUM,
} FMT;


//GLT Manager
class GltMgr {
    friend class LTMgr;
    friend class RA;
    friend class GIG;
protected:
    Vector<BitSet*> m_glt2usable_regs; //Map GLT to its usable registers.
    Vector<LTMgr*> m_bb2ltmgr; //Map from BB id to LTMgr.
    Vector<GLT*> m_gltid2glt_map; //Map from id to GLT.
    DefMiscBitSetMgr m_sbs_mgr;
    RGMgr m_rg_mgr;
    BitSetMgr m_bs_mgr;
    LTGMgr m_ltgmgr;
    Vector<GLT*> m_pr2glt;
    Vector<UINT> m_params;
    ConstIRIter m_cii; //const IR iter.
    Region * m_rg;
    RA * m_ra;
    RSC * m_rsc;
    SMemPool * m_pool;
    LivenessMgr * m_liveness_mgr;
    TypeMgr * m_tm;
    UINT m_glt_count;
    bool m_is_consider_local_interf;

    void comp_st_usage(IR const* ir);
    void comp_ir_usage(IR const* ir);
    void * xmalloc(UINT size)
    {
        ASSERTN(m_pool != NULL,("need graph pool!!"));
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p != NULL);
        ::memset(p, 0, size);
        return p;
    }
    bool verify();
public:
    GltMgr(Region * rg, LivenessMgr * prdf, RA * ra);
    COPY_CONSTRUCTOR(GltMgr);
    ~GltMgr()
    {
        for (INT i = 0; i <= m_bb2ltmgr.get_last_idx(); i++) {
            LTMgr * l = m_bb2ltmgr.get(i);
            if (l != NULL) {
                delete l;
            }
        }
        smpoolDelete(m_pool);
    }

    void build(bool build_group_part);
    GLT * buildGltLike(IR * pr, GLT * cand);
    void dump();
    void dumpg();
    void dumpl(UINT bbid);

    //Get LTMgr via BB's id.
    LTMgr * get_ltm(UINT bbid) { return m_bb2ltmgr.get(bbid); }
    Region * getRegion() { return m_rg; }
    BitSetMgr * getBitSetMgr() { return &m_bs_mgr; }
    Vector<GLT*> * get_pr2glt_map() { return &m_pr2glt; }
    UINT get_num_of_glt() const { return m_glt_count - 1; }
    Vector<GLT*> * get_gltvec() { return &m_gltid2glt_map; }
    BitSet * getUsableRegSet(GLT const* g, bool alloc)
    {
        BitSet * rs = m_glt2usable_regs.get(GLT_id(g));
        if (rs == NULL && alloc) {
            rs = m_bs_mgr.create();
            m_glt2usable_regs.set(GLT_id(g), rs);
        }
        return rs;
    }
    GLT * get_glt(UINT gltid) { return m_gltid2glt_map.get(gltid); }

    //Free all DefDBitSetCore allocated.
    void freeGLTBitset()
    {
        Vector<GLT*> * gltv = get_gltvec();
        for (INT i = 0; i <= gltv->get_last_idx(); i++) {
            GLT * g = gltv->get(i);
            if (g == NULL) { continue; }
            m_sbs_mgr.freeDBitSetCore(GLT_bbs(g));
        }
    }

    RG * new_rg(UINT rnum) { return m_rg_mgr.create(rnum); }
    GLT * new_glt(UINT prno);

    LTMgr * map_bb2ltm(IRBB * bb);

    GLT * map_pr2glt(UINT prno)
    { return m_pr2glt.get(prno); }

    void renameUse(IR const* ir, LT * l, IR ** newpr);
    void renameGLT(GLT * g);
    void renameLocal();
    void rename();
    void set_consider_local_interf(bool doit)
    { m_is_consider_local_interf = doit; }

    void localize(GLT * g);
};



//Global Interference Graph.
#define GIG_ru(g)            ((g)->m_rg)
#define GIG_glt_mgr(g)        ((g)->m_glt_mgr)
class GIG : public Graph {
protected:
public:
    Region * m_rg;
    IRCFG * m_cfg;
    GltMgr * m_gltm;

    //consider local life time interference during global allocation.
    bool m_is_consider_local_interf;

public:
    GIG(Region * rg, GltMgr * glt_mgr)
    {
        ASSERT0(rg && glt_mgr);
        m_gltm = glt_mgr;
        m_rg = rg;
        m_cfg = m_rg->getCFG();
        m_is_consider_local_interf = false;
        set_direction(false);
    }

    void add_glt(GLT * g) { addVertex(GLT_id(g)); }
    void dumpVCG(CHAR const* name = NULL);
    void remove_glt(GLT * g) { removeVertex(GLT_id(g)); }
    bool is_interf(IN GLT * glt1, IN GLT * glt2);
    bool is_interf_with_neighbour(GLT * g, DefSBitSet & nis, UINT phy);

    void set_consider_local_interf(bool doit)
    { m_is_consider_local_interf = doit; }
    void set_interf_with(UINT gltid, List<UINT> & lst);

    void rebuild()
    {
        erase();
        build();
    }
    void build();
};


//Compute the Resource Constraints
class RSC {
    friend class RA;
protected:
    BitSet * m_4;
    BitSet * m_8;
    BitSet * m_16;
    Region * m_rg;
    GltMgr * m_gltm;
    TypeMgr * m_tm;
    BitSetMgr * m_bsm;
    Vector<FMT> m_ir2fmt;
    Str2BuiltinType m_str2builtin;
    BitSet * m_usable[FNUM][2]; //1:def, 0:use

    void init_usable();
public:
    RSC(GltMgr * gltm)
    {
        ASSERT0(gltm);
        m_gltm = gltm;
        m_rg = gltm->getRegion();
        m_tm = m_rg->getTypeMgr();
        m_bsm = gltm->getBitSetMgr();
        m_4 = NULL;
        m_8 = NULL;
        m_16 = NULL;
        init_usable();
    }
    ~RSC()
    {
        m_bsm->free(m_4);
        m_bsm->free(m_8);
        m_bsm->free(m_16);
    }

    //Return the usable regs for 'fmt'.
    BitSet * get_usable(FMT fmt, bool is_def)
    {
        ASSERT0(FUNDEF < fmt && fmt < FNUM);
        return m_usable[fmt][is_def];
    }

    BitSet * get_4();
    BitSet * get_8();
    BitSet * get_16();
    void comp_st_fmt(IR const* ir);
    void comp_ist_fmt(IR const* ir);
    void comp_starray_fmt(IR const* ir);
    void comp_call_fmt(IR const* ir);
    void comp_ir_fmt(IR const* ir);
    void comp_usable_regs(LT * lt, LTMgr * ltm);
    void comp_ir_constrain();
    void comp_lt_usable(LT * lt, LTMgr * ltm);
    void comp_local_usage(LTMgr * ltm, bool only_local, bool omit_constrain);

    void dump_ir_fmt();
    void dump_glt_usable();
    void dump_bb(UINT bbid);
    void dump();
    FMT get_fmt(IR const* ir) const { return m_ir2fmt.get(ir->id()); }

    GLT * mapLT2GLT(LT * lt) { return m_gltm->map_pr2glt(LT_prno(lt)); }
    bool verify_fmt();
    void perform(bool omit_constrain);
};


class BBRA {
protected:
    IRBB * m_bb;
    RA * m_ra;
    GltMgr * m_gltm;
    LTMgr * m_ltm;
    Region * m_rg;
    RSC * m_rsc;
    IG * m_ig;

    //true if omit constraint when compute lt usable-regs set.
    BYTE m_omit_constrain:1;

    List<LT*> * m_tmp_lts; //for local tmp use.
    List<LT*> * m_tmp_lts2; //for local tmp use.
    List<UINT> * m_tmp_uints; //for local tmp use.
    ConstIRIter * m_tmp_cii; //for local tmp use.

protected:
    LT * computeSplitCand(
            LT * lt,
            bool & has_hole,
            List<LT*> * tmp,
            List<LT*> * tmp2);
    void computeLTResideInHole(OUT List<LT*> & reside_in, LT const* lt);
    bool canBeSplit(LT const* lt) const;
    void collectUnalloc(List<LT*> & unalloc);

    void dump_prio(List<LT*> & prios);
    bool getMaxHole(OUT INT * startpos, OUT INT * endpos, LT const* lt);

    bool find_hole(OUT INT & startpos, OUT INT & endpos,
                   LT const* owner, LT const* inner);

    bool is_live_through(LT const* l) const;
    bool isOpndSameWithResult(IR * ir);
    bool isSatisfiedConstrain(LT * lt, LT * cand);

    void selectReasonableSplitPos(
            OUT INT & pos1,
            OUT INT & pos2,
            OUT bool & is_pos1_spill,
            OUT bool & is_pos2_spill,
            LT * lt);
    void splitLTAt(INT start, INT end, bool is_start_spill,
                     bool is_end_spill, LT * lt);
    bool split(LT * lt);

    void renameResult(IR * ir, UINT old_prno, IR * newpr);
    void renameOpnd(IR * ir, UINT old_prno, IR * newpr);
    void renameOpndInRange(LT * lt, IR * newpr, INT start, INT end);
public:
    BBRA(IRBB * bb, RA * ra);
    ~BBRA() {}
    void buildPrioList(List<LT*> const& lts, OUT List<LT*> & prios);
    float computePrio(LT const* lt);
    void rename();
    bool assignRegister(LT * l, List<UINT> & nis);
    void allocPrioList(OUT List<LT*> & prios, List<UINT> & nis);

    void set_omit_constrain(bool omit) { m_omit_constrain = omit; }
    void set_tmp_lts(List<LT*> * tmp) { ASSERT0(tmp); m_tmp_lts = tmp; }
    void set_tmp_lts2(List<LT*> * tmp) { ASSERT0(tmp); m_tmp_lts2 = tmp; }
    void set_tmp_uints(List<UINT> * tmp) { ASSERT0(tmp); m_tmp_uints = tmp; }
    void set_tmp_cii(ConstIRIter * tmp) { ASSERT0(tmp); m_tmp_cii = tmp; }
    bool solve(List<LT*> & prios);

    bool perform(List<LT*> & prios);
};


//RA
class RA {
protected:
    friend class BBRA;
    friend class LTMgr;
    friend class GltMgr;

    LivenessMgr m_liveness_mgr;
    GltMgr m_gltm;
    GIG m_ig;
    RSC m_rsc;
    Region * m_rg;
    IRCFG * m_cfg;
    TypeMgr * m_tm;
    TypeIndexRep * m_tr;
    Vreg2PR * m_v2pr;
    Prno2Vreg * m_pr2v;
    VAR2PR * m_var2pr;
    UINT m_param_num; //record the number of param.
    UINT m_param_reg_start;
    UINT m_vregnum; //record the number of original vreg.
    UINT m_maxreg; //record the max vreg used. note it is not the number of vreg.
    IRIter m_ii; //for tmp used.
    ConstIRIter m_cii; //for tmp used.

protected:
    void assignLTG(LTG * ltg, IR * ir);
    bool assignRegister(GLT * g, List<UINT> & nis, List<UINT> & nis2);
    void allocParameter();
    void allocGroup();
    void allocPrioList(OUT List<GLT*> & prios, OUT List<GLT*> & unalloc,
                          List<UINT> & nis, List<UINT> & nis2);
    void allocGlobal(List<UINT> & nis, List<UINT> & nis2);
    void allocLocal(List<UINT> & nis, bool omit_constrain);
    void allocLocalSpec(List<UINT> & nis);

    void buildLocalIG();
    void buildPrioList(OUT List<GLT*> & prios);

    inline bool checkIfNeedSpill(UINT prno, FMT fmt, LTMgr const* ltm);
    UINT computeReserveRegister(IRIter & ii, List<IR*> & resolve_list);
    UINT computeNumRegister(
            UINT rangestart,
            UINT rangeend,
            LTG const* ltg,
            BitSet const& occupied,
            BitSet const& assigned,
            BitSet const& liveout_phy);
    UINT computeSatisfiedNumRegister(
            UINT rangestart,
            LTG const* ltg,
            UINT rgsz,
            BitSet const& occupied,
            BitSet const& assigned,
            BitSet const& liveout_phy);
    float computePrio(GLT * g);

    void dump_ltg();
    void diffLocalNeighbourUsed(GLT * g, List<UINT> & nis, BitSet * unusable);
    void freeGLTBitset();

    IR * insertMoveBefore(IR * stmt, IR * src);
    bool is_cross_param(UINT reg_start, UINT rg_sz) const
    { return reg_start < m_param_num && (reg_start + rg_sz) > m_param_num; }

    bool is_cross_liveout_phy(UINT reg_start, UINT rgsz,
                              BitSet const& liveout_phy);

    void shiftReg(UINT ofst);
    IR * split(GLT * g);
    void solveConflict(OUT List<GLT*> & unalloc, List<UINT> & nis);
    RA * self() { return this; }

    INT tryReuseAppeared(
            LTG const* ltg,
            BitSet const& occupied,
            BitSet const& assigned,
            BitSet const& liveout_phy);
    INT tryExtend(LTG const* ltg,
            BitSet const& occupied,
            BitSet const& liveout_phy,
            BitSet const& assigned);

    void remedyLTG(
            LTG * ltg,
            IR * ir,
            LTMgr * ltm,
            DefSBitSet & nis,
            BitSet & visited,
            UINT rangestart);
    void reviseRSC();
    void rotateReg();
    void reviseParam();
    bool overlapParam(LT const* l) const;
public:
    RA(Region * rg,
       TypeIndexRep * tr,
       UINT param_num,
       UINT vregnum,
       Vreg2PR * v2pr,
       Prno2Vreg * pr2v,
       VAR2PR * var2pr) :
        m_liveness_mgr(rg), m_gltm(rg, &m_liveness_mgr, self()), m_ig(rg, &m_gltm),
        m_rsc(&m_gltm)
    {
        ASSERT0(rg && tr);
        m_rg = rg;
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_tr = tr;
        m_param_num = param_num;
        m_vregnum = vregnum;
        m_maxreg = 0;
        m_v2pr = v2pr;
        m_pr2v = pr2v;
        m_var2pr = var2pr;
    }
    COPY_CONSTRUCTOR(RA);
    ~RA(){}

    LT * getLifeTime(UINT bbid, UINT prno)
    {
        LTMgr * ltm = m_gltm.get_ltm(bbid);
        if (ltm == NULL) { return NULL; }
        return ltm->map_pr2lt(prno);
    }
    GLT * get_glt(UINT prno) { return m_gltm.map_pr2glt(prno); }
    GltMgr * get_gltm() { return &m_gltm; }
    RSC * get_rsc() { return &m_rsc; }
    UINT get_maxreg() const { return m_maxreg; }
    UINT get_paramnum() const { return m_param_num; }

    void updateLocal();
    void updateMaxReg(UINT r) { m_maxreg = MAX(m_maxreg, r); }
    void updateGltMaxReg(GLT * g)
    {
        if (GLT_rg_sz(g) > 1) {
            ASSERT0(GLT_rg_sz(g) == RG_PAIR_SZ);
            updateMaxReg(GLT_phy(g) + 1);
        } else {
            updateMaxReg(GLT_phy(g));
        }
    }
    void updateLTMaxReg(LT * l)
    {
        if (!l->has_allocated()) { return; }
        if (LT_rg_sz(l) > 1) {
            ASSERT0(LT_rg_sz(l) == RG_PAIR_SZ);
            updateMaxReg(LT_phy(l) + 1);
        } else {
            updateMaxReg(LT_phy(l));
        }
    }

    bool verify_interf();
    bool verify_lt_occ();
    bool verify_usable();
    bool verify_rsc();
    bool verify_ltg();
    bool verify_reg(bool check_usable, bool check_alloc);
    bool verify_glt(bool check_alloc);
    bool perform(OptCtx & oc);
};
#endif
