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
@*/
#ifndef _IF_CONVERSION_H_
#define _IF_CONVERSION_H_

namespace xoc {

class IfConversion;
class IfCvsCtx;

//The class represents a diamond region in control-flow.
//e.g:  __top__
//     |       |
//  left       right
//     |__   __|
//        | |
//        v v
//        bottom
//or:
//      __top__
//     |       |
//  left|right |
//     |__   __|
//        | |
//        v v
//        bottom
class DiamondRegion {
public:
    IRBB * top;
    IRBB * right;
    IRBB * left;
    IRBB * bottom;
public:
    DiamondRegion() { ::memset(this, 0, sizeof(DiamondRegion)); }
    void dump(Region const* rg) const;
    CHAR const* dumpBuf(
        Region const* rg, IfCvsCtx const& ctx, OUT xcom::StrBuf & buf) const;

    //Return true if the diamond region is triangle.
    //e.g.  __top__
    //     |       |
    //  left|right |
    //     |__   __|
    //        | |
    //        v v
    //        bottom
    bool isTri() const { ASSERT0(right && left); return right == left; }
    bool verify(IRCFG const* cfg) const;
};


class IfCvsCtx : public PassCtx {
    //THE CLASS ALLOWS PARTIAL COPY-CONSTRUCTOR.
    COPY_CONSTRUCTOR_ASSIGN(IfCvsCtx);
public:
    typedef xcom::EList<IR*, IR2Holder>::Iter GenedListIter;
    class GenedList : public xcom::EList<IR*, IR2Holder> {
    public:
        void append(IRList const& irlst);
        void append(IR * ir);
    };
protected:
    //The list records all generated SELECT operations that are converted.
    GenedList m_truepart_gened_list;
    GenedList m_falsepart_gened_list;
    GenedList m_bottom_gened_list;
    GenedList m_top_gened_list;
public:
    LI<IRBB> const* m_li;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    MDMgr * m_mdmgr;
    IfConversion const* m_ifcvs;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
public:
    IfCvsCtx(
        MOD OptCtx & oc, LI<IRBB> const* li, IfConversion const* ifcvs,
        ActMgr * am);
    IfCvsCtx(IfCvsCtx const& src) : PassCtx(src) { copyTopDownInfo(src); }
    ~IfCvsCtx();

    void copyTopDownInfo(IfCvsCtx const& src);
    void clean();
    void dump() const;

    LI<IRBB> const* getLI() const { return m_li; }
    IfConversion const* getIfCvs() const { return m_ifcvs; }
    IRCFG * getCFG() const { return m_cfg; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    MDMgr * getMDMgr() const { return m_mdmgr; }
    MDSSAMgr * getMDSSAMgr() const { return m_mdssamgr; }
    PRSSAMgr * getPRSSAMgr() const { return m_prssamgr; }
    GenedList & getTruePartGenedList() { return m_truepart_gened_list; }
    GenedList & getFalsePartGenedList() { return m_falsepart_gened_list; }
    GenedList & getBottomGenedList() { return m_bottom_gened_list; }
    GenedList & getTopGenedList() { return m_top_gened_list; }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
    void unionBottomUpInfo(IfCvsCtx const& src);

    bool verify() const;
};


//This class represents if-conversion.
class IfConversion : public Pass {
    COPY_CONSTRUCTOR(IfConversion);
    bool m_is_try_loop_tree;
    ActMgr m_am;
protected:
    bool doLoopTree(MOD LI<IRBB> * li, MOD OptCtx & oc);

    //Return true if the pass only try to scan loop tree to perform
    //optimization.
    bool isTryLoopTree() const { return m_is_try_loop_tree; }
    bool initDepPass(MOD OptCtx & oc);

    void reset();

    bool tryLoopImpl(MOD IfCvsCtx & ctx);
    bool tryLoop(MOD LI<IRBB> * li, MOD OptCtx & oc);
    bool tryBBListImpl(MOD IfCvsCtx & ctx);
    bool tryBBList(MOD OptCtx & oc);
public:
    explicit IfConversion(Region * rg);
    virtual ~IfConversion();

    bool canBeConverted(IR const* comp, IfCvsCtx const& ctx) const;

    //The function is the reverse operation of if-conversion.
    //It converts SELECT to CONDITIONAL-BRANCH stmt.
    //Return true if new stmts generated, and the new stmts will be recorded
    //in ctx.
    //e.g: given $x = select $m, 1, 2;
    //the new generated stmts are:
    //  truebr eq $m, 0, TRUE;
    //  $x = 2;
    //  goto END;
    //  label TRUE;
    //  $x = 1;
    //  label END;
    static IR * convertSelectToBranch(IR const* ir, MOD IfCvsCtx & ctx);

    virtual bool dump() const;

    //Return true if the function finds a diamond region and record the
    //region in 'dr' as return result.
    static bool findDiamondRegion(
        IR const* ir, IfCvsCtx const& ctx, OUT DiamondRegion & dr);

    //Return true if the function finds a diamond region and record the
    //region in 'dr' as return result.
    static bool findDiamondRegion(
        IRBB const* bb, IfCvsCtx const& ctx, OUT DiamondRegion & dr);

    virtual CHAR const* getPassName() const { return "If Conversion"; }
    PASS_TYPE getPassType() const { return PASS_IF_CONVERSION; }
    ActMgr & getActMgr() { return m_am; }

    virtual bool perform(OptCtx & oc);

    //Tell the pass that perform if-conversion only on loop-tree.
    void setToTryLoopTree(bool try_loop) { m_is_try_loop_tree = try_loop; }

    //All generated IR stmt will be appended to the generated-stmt-list
    //of 'ctx'.
    static bool tryConvertDiamondRegion(
        DiamondRegion const& dr, MOD IfCvsCtx & ctx);
};

} //namespace xoc

#endif
