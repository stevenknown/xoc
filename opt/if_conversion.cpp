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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START DUMP
//
static void dumpReasonAndIR(
    IR const* ir, MOD IfCvsCtx & ctx, CHAR const* format, ...)
{
    Region const* rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }

    //Dump formatted string to buffer.
    xcom::StrBuf reason(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        reason.vstrcat(format, args);
        va_end(args);
    }
    //Dump IR info to buffer.
    xcom::StrBuf irbuf(64);
    ASSERT0(ir);
    xoc::dumpIRToBuf(ir, rg, irbuf);

    //Dump to action.
    ctx.getActMgr()->dump("%s:%s",
        reason.getBuf(), irbuf.getBuf());
}


static void dumpGenedIRList(
    IfCvsCtx::GenedList const& converted_stmt_list, MOD IfCvsCtx & ctx,
    CHAR const* format, ...)
{
    Region const* rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }

    //Dump formatted string to buffer.
    xcom::StrBuf tmpbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }

    xcom::StrBuf irbuf(64);
    IRListIter it;
    UINT const incdn = 4;
    for (IR * c = converted_stmt_list.get_head(&it);
         c != nullptr; c = converted_stmt_list.get_next(&it)) {
        //Dump IR info to buffer.
        ASSERT0(c);
        xoc::dumpIRToBuf(c, rg, irbuf, DumpFlag(IR_DUMP_COMBINE), incdn);
    }

    //Dump to action.
    ctx.getActMgr()->dump("%s, \nwhere generated IR:%s",
        tmpbuf.getBuf(), irbuf.getBuf());
}


static void dumpDR(
    DiamondRegion const& dr, MOD IfCvsCtx & ctx,
    CHAR const* format, ...)
{
    Region const* rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }

    //Dump formatted string to buffer.
    xcom::StrBuf tmpbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    //Dump diamond-region info to buffer.
    xcom::StrBuf drbuf(64);
    dr.dumpBuf(rg, ctx, drbuf);

    //Dump to action.
    ctx.getActMgr()->dump("%s:%s",
        tmpbuf.getBuf(), drbuf.getBuf());
}


static void dumpReverseIfCvs(
    IR const* selop, IR const* newstmtlist, IfCvsCtx const& ctx)
{
    Region const* rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }

    //Dump IR info to buffer.
    xcom::StrBuf buf1(64);
    ASSERT0(selop);
    xoc::dumpIRToBuf(selop, rg, buf1);

    xcom::StrBuf buf2(64);
    for (IR const* t = newstmtlist; t != nullptr; t = t->get_next()) {
        xcom::StrBuf buf3(64);
        xoc::dumpIRToBuf(t, rg, buf3);
        buf2.strcat(buf3);
    }

    //Dump to action.
    ctx.getActMgr()->dump("REVERSE IF CVS, CONVERT %s TO %s",
        buf1.getBuf(), buf2.getBuf());
}
//END DUMP


//
//START DiamondRegion
//
bool DiamondRegion::verify(IRCFG const* cfg) const
{
    ASSERT0(cfg);
    ASSERT0(top && bottom && left && right);
    ASSERT0(top->getNumOfSucc() == 2);
    IRBB const* succ0 = cfg->getNthSucc(top, 0);
    IRBB const* succ1 = cfg->getNthSucc(top, 1);
    if (left == right) {
        ASSERT0(left == succ0 || left == succ1);
    } else {
        if (left != succ0 && right != succ0) {
            ASSERT0(0);
        }
        if (left != succ1 && right != succ1) {
            ASSERT0(0);
        }
    }
    ASSERT0(cfg->isUniqueSucc(left, bottom));
    ASSERT0(cfg->isUniqueSucc(right, bottom));
    return true;
}


CHAR const* DiamondRegion::dumpBuf(
    Region const* rg, IfCvsCtx const& ctx, OUT xcom::StrBuf & buf) const
{
    if (!rg->isLogMgrInit()) { return nullptr; }
    class Dump : public xoc::DumpToBuf {
    public:
        DiamondRegion const* dr;
        Dump(Region const* rg, xcom::StrBuf & buf) : DumpToBuf(rg, buf, 2) {}
        virtual void dumpUserInfo() const override { dr->dump(getRegion()); }
    };
    Dump d(rg, buf);
    d.dr = this;
    d.dump();
    return buf.getBuf();
}


void DiamondRegion::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    if (top != nullptr) {
        note(rg, "\ntop:BB%u", top->id());
    } else {
        note(rg, "\ntop:--");
    }
    if (left != nullptr) {
        note(rg, "\nleft:BB%u", left->id());
    } else {
        note(rg, "\nleft:--");
    }
    if (right != nullptr) {
        note(rg, "\nrigh:BB%u", right->id());
    } else {
        note(rg, "\nright:--");
    }
    if (bottom != nullptr) {
        note(rg, "\nbottom:BB%u", bottom->id());
    } else {
        note(rg, "\nbottom:--");
    }
}
//END DiamondRegion


//
//START IfCvsCtx
//
IfCvsCtx::IfCvsCtx(
    MOD OptCtx & oc, LI<IRBB> const* li, IfConversion const* ifcvs, ActMgr * am)
    : PassCtx(&oc, am)
{
    ASSERT0(ifcvs);
    m_li = li;
    m_ifcvs = ifcvs;
    m_cfg = getRegion()->getCFG();
    m_irmgr = getRegion()->getIRMgr();
    m_mdmgr = getRegion()->getMDMgr();
    m_mdssamgr = getRegion()->getMDSSAMgr();
    m_prssamgr = getRegion()->getPRSSAMgr();
}


IfCvsCtx::~IfCvsCtx()
{
}


void IfCvsCtx::unionBottomUpInfo(IfCvsCtx const& src)
{
    m_truepart_gened_list.append_tail(
        const_cast<IfCvsCtx&>(src).getTruePartGenedList());
    m_falsepart_gened_list.append_tail(
        const_cast<IfCvsCtx&>(src).getFalsePartGenedList());
}


void IfCvsCtx::clean()
{
    m_truepart_gened_list.clean();
    m_falsepart_gened_list.clean();
    m_bottom_gened_list.clean();
    m_top_gened_list.clean();
}


void IfCvsCtx::copyTopDownInfo(IfCvsCtx const& src)
{
    //DO NOT COPY genedlist because it is not the top-down info.
    m_li = src.m_li;
    m_cfg = src.m_cfg;
    m_irmgr = src.m_irmgr;
    m_mdmgr = src.m_mdmgr;
    m_ifcvs = src.m_ifcvs;
    m_mdssamgr = src.m_mdssamgr;
    m_prssamgr = src.m_prssamgr;
}


void IfCvsCtx::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    IfCvsCtx * pthis = const_cast<IfCvsCtx*>(this);
    note(m_rg, "\n==-- DUMP IfCvsCtx --==");
    if (getActMgr() != nullptr) {
        getActMgr()->dump();
    }
    DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE);

    //Set the flag to true if user expect to dump whole
    //IR tree in one line.
    //e.g:stpr $4:bool id:55 gt:bool id:3 ld:i32 'A' id:1 ld:i32 'B' id:2
    bool dump_ir_in_one_line = false;
    if (dump_ir_in_one_line) {
        dumpflag.set(IR_DUMP_NO_NEWLINE);
    }

    note(m_rg, "\n-- TopGenedList --");
    for (IR * g = pthis->getTopGenedList().get_head();
         g != nullptr; g = pthis->getTopGenedList().get_next()) {
        if (dump_ir_in_one_line) {
            xoc::note(getRegion(), "\n");
        }
        xoc::dumpIR(g, getRegion(), dumpflag);
    }

    note(m_rg, "\n-- TruePartGenedList --");
    for (IR * g = pthis->getTruePartGenedList().get_head();
         g != nullptr; g = pthis->getTruePartGenedList().get_next()) {
        if (dump_ir_in_one_line) {
            xoc::note(getRegion(), "\n");
        }
        xoc::dumpIR(g, getRegion(), dumpflag);
    }

    note(m_rg, "\n-- FalsePartGenedList --");
    for (IR * g = pthis->getFalsePartGenedList().get_head();
         g != nullptr; g = pthis->getFalsePartGenedList().get_next()) {
        if (dump_ir_in_one_line) {
            xoc::note(getRegion(), "\n");
        }
        xoc::dumpIR(g, getRegion(), dumpflag);
    }

    note(m_rg, "\n-- BottomGenedList --");
    for (IR * g = pthis->getBottomGenedList().get_head();
         g != nullptr; g = pthis->getBottomGenedList().get_next()) {
        if (dump_ir_in_one_line) {
            xoc::note(getRegion(), "\n");
        }
        xoc::dumpIR(g, getRegion(), dumpflag);
    }
}


void IfCvsCtx::GenedList::append(IRList const& irlst)
{
    IRListIter it;
    for (IR * ir = irlst.get_head(&it);
         ir != nullptr; ir = irlst.get_next(&it)) {
        if (find(ir)) { continue; }
        xcom::EList<IR*, IR2Holder>::append_tail(ir);
    }
}


void IfCvsCtx::GenedList::append(IR * ir)
{
    if (find(ir)) { return; }
    xcom::EList<IR*, IR2Holder>::append_tail(ir);
}


bool IfCvsCtx::verify() const
{
    IfCvsCtx * pthis = const_cast<IfCvsCtx*>(this);
    for (IR * ir = pthis->getTruePartGenedList().get_head();
         ir != nullptr; ir = pthis->getTruePartGenedList().get_next()) {
        ASSERT0(ir->is_stmt());

        //NOTE:Some stmts, such as stpr, do not need to be conditionally
        //executed.
        //ASSERT0(IRMgr::isCondExec(ir));
    }
    for (IR * ir = pthis->getFalsePartGenedList().get_head();
         ir != nullptr; ir = pthis->getFalsePartGenedList().get_next()) {
        ASSERT0(ir->is_stmt());

        //NOTE:Some stmts, such as stpr, do not need to be conditionally
        //executed.
        //ASSERT0(IRMgr::isCondExec(ir));
    }
    return true;
}
//END IfCvsCtx


//
//START IfConversion
//
static bool isMultiCondBranchDiamondShape(
    IR const* ir, IfCvsCtx const& ctx, OUT DiamondRegion & dr)
{
    ASSERT0(ir->isMultiConditionalBr());
    ASSERT0(0); //TODO:Determine if ir leads the diamond region.
    return false;
}


static bool isDiamondShape(
    IR const* ir, IRBB const* fallthroughbb, IRBB const* tgtbb,
    IfCvsCtx const& ctx, OUT DiamondRegion & dr)
{
    //CASE2:
    //   ---BR
    //  |   |
    //  |   FALLTHROUGH --
    //  |                  |
    //  |                  |
    //   -> TGTBB          |
    //      |              |
    //      |              |
    //      v              |
    //      END <----------
    ASSERTN(fallthroughbb != tgtbb, ("redundant branch"));
    IRCFG const* cfg = ctx.getCFG();

    //Find END BB.
    IRBB const* endbb = cfg->getUniqueSucc(tgtbb);
    if (endbb == nullptr) {
        //Unnormal control-flow structure, can not find END BB.
        return false;
    }
    if (cfg->getPredsNum(endbb) != 2) { return false; }
    if (fallthroughbb->getNumOfPred() != 1 || tgtbb->getNumOfPred() != 1) {
        //e.g: BB1-
        //     |   |
        //     v   |
        //    BB2  |
        //    |  | |
        //    v  v v
        //  BB4  BB6
        //    |  |
        //    v  v
        //    BB7
        //DR(BB2,4,6,7) is not diamond-region because BB6 has two preds.
        return false;
    }
    if (!cfg->isUniqueSucc(fallthroughbb, endbb)) {
        //TODO:there might be a path that starts at FALLTHROUGH and end at END.
        //e.g:FALLTHROUGH->x->y->...->END.
        return false;
    }
    dr.top = ir->getBB();
    dr.left = const_cast<IRBB*>(fallthroughbb);
    dr.right = const_cast<IRBB*>(tgtbb);
    dr.bottom = const_cast<IRBB*>(endbb);
    return true;
}


static bool isTriangleShape(
    IR const* ir, IRBB const* fallthroughbb, IRBB const* tgtbb,
    IfCvsCtx const& ctx, OUT DiamondRegion & dr)
{
    //CASE1:
    //  BR -----------
    //  |            |
    //  FALL-THROUGH |
    //  |            |
    //  TGT <--------
    //
    //CASE1.1:
    //  BR ------------------
    //  |                    |
    //  FALL-THROUGH ----    |
    //                   |   |
    //                   v   |
    //  TGT <----------------
    ASSERTN(fallthroughbb != tgtbb, ("redundant branch"));
    IRCFG const* cfg = ctx.getCFG();
    if (!cfg->isUniqueSucc(fallthroughbb, tgtbb)) { return false; }
    if (cfg->getPredsNum(tgtbb) != 2) { return false; }
    if (fallthroughbb->getNumOfPred() != 1) {
        //e.g: BB1-
        //     |   |
        //     v   |
        //    BB2  |
        //    |  | |
        //    v  v v
        //  BB4  BB6
        //    |  |
        //    v  v
        //    BB7
        //DR(BB2,4,6,7) is not diamond-region because BB6 has two preds.
        return false;
    }
    dr.top = ir->getBB();
    dr.left = const_cast<IRBB*>(fallthroughbb);
    dr.right = const_cast<IRBB*>(fallthroughbb);
    dr.bottom = const_cast<IRBB*>(tgtbb);
    return true;
}


static bool isDiamondShape(
    IR const* ir, IfCvsCtx const& ctx, OUT DiamondRegion & dr)
{
    ASSERT0(ir->isConditionalBr());
    if (!ir->is_truebr() && !ir->is_falsebr()) {
        return false;
    }
    IRCFG const* cfg = ctx.getCFG();
    IRBB const* fallthroughbb = cfg->getFallThroughBB(ir->getBB());
    ASSERT0(fallthroughbb);
    IRBB const* tgtbb = cfg->getTargetBB(ir);
    ASSERT0(tgtbb);
    if (isTriangleShape(ir, fallthroughbb, tgtbb, ctx, dr)) {
        return true;
    }
    if (isDiamondShape(ir, fallthroughbb, tgtbb, ctx, dr)) {
        return true;
    }
    return false;
}


IfConversion::IfConversion(Region * rg) : Pass(rg), m_am(rg)
{
    ASSERT0(rg != nullptr);
    m_is_try_loop_tree = false;
}


IfConversion::~IfConversion()
{
}


bool IfConversion::findDiamondRegion(
    IRBB const* bb, IfCvsCtx const& ctx, OUT DiamondRegion & dr)
{
    IR const* ir = const_cast<IRBB*>(bb)->getLastIR();
    if (ir == nullptr) { return false; }
    return findDiamondRegion(ir, ctx, dr);
}


bool IfConversion::findDiamondRegion(
    IR const* ir, IfCvsCtx const& ctx, OUT DiamondRegion & dr)
{
    ASSERT0(ir);
    if (ir->isConditionalBr()) {
        return isDiamondShape(ir, ctx, dr);
    }
    if (ir->isMultiConditionalBr()) {
        return isMultiCondBranchDiamondShape(ir, ctx, dr);
    }
    return false;
}


void IfConversion::reset()
{
    m_am.clean();
}


bool IfConversion::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    if (!g_dump_opt.isDumpPass(PASS_IF_CONVERSION)) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    m_am.dump();
    Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool IfConversion::initDepPass(MOD OptCtx & oc)
{
    return true;
}


static bool verifyTreeForStmtList(
    Region const* rg, IfCvsCtx::GenedList const& converted_stmt_list)
{
    IfCvsCtx::GenedListIter it;
    for (IR * c = converted_stmt_list.get_head(&it);
         c != nullptr; c = converted_stmt_list.get_next(&it)) {
        ASSERT0(c->verifyTree(rg));
    }
    return true;
}


static void convertStmtToSelectOp(
    bool is_true_part, OUT IfCvsCtx::GenedList & converted_stmt_list,
    MOD IR * compdet, MOD IR * converted, IfCvsCtx const& ctx)
{
    ASSERT0(converted->is_stmt() && !converted->isBranch());
    ASSERT0(compdet && compdet->is_stmt() && compdet->is_stpr());
    Region * rg = ctx.getRegion();

    //Generate true and false part.
    IR * truepart = nullptr;
    IR * falsepart = nullptr;
    Type const* parttype = nullptr;
    if (is_true_part) {
        falsepart = nullptr;

        //NOTE: we move the expresions directly to aviod building and adding
        //DU chain for the expressions.
        truepart = converted->getPureRHS();
        parttype = truepart->getType();
        converted->setRHS(nullptr);
    } else {
        truepart = nullptr;

        //NOTE: we move the expresions directly to aviod building and adding
        //DU chain for the expressions.
        falsepart = converted->getPureRHS();
        parttype = falsepart->getType();
        converted->setRHS(nullptr);
    }
    ASSERT0(parttype);

    //Generate true and false part.
    //NOTE: we move the expresions directly to aviod building and adding
    //DU chain for the expressions.
    IR * det = rg->dupIsomoExpTree(compdet);
    det->setType(rg->getTypeMgr()->getBool());
    det->copyRef(compdet, rg);

    //Generate the select.
    IR * select = rg->getIRMgr()->buildSelect(
        det, truepart, falsepart, parttype);
    IR * rhs = select;
    if (CSelect::isPartialSelect(select)) {
        rhs = ((IRMgrExt*)rg->getIRMgr())->
            buildSelectToRes(select, nullptr, parttype);
    }

    //Update the original stmt.
    converted->setRHS(rhs);
    converted_stmt_list.append_tail(converted);

    //There is no need to maintain DU chain for true-or-false part because
    //the DU chain of them is unchanged.
    xoc::buildDUChain(compdet, det, rg, *ctx.getOptCtx());
}


//Return false if if-conversion may be illegal.
//The reason is given bb may contain stmts that prevent if-conversion.
static bool checkCondExecBBCanBeConvertToSelectOp(
    IRBB const* bb, MOD IfCvsCtx & ctx, bool is_true_part)
{
    ASSERT0(bb);
    BBIRListIter it;
    for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         ir != nullptr; ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (ir->isBranch()) {
            //Branch op will be eliminated.
            continue;
        }
        if (!ctx.getIfCvs()->canBeConverted(ir, ctx)) {
            dumpReasonAndIR(ir, ctx, "stmt can not be if-converted");
            return false;
        }
    }
    return true;
}


//Return true if there exist IR in BB that cannot be converted.
static void convertBBToSelectOp(
    OUT IfCvsCtx::GenedList & converted_stmt_list, IRBB const* bb,
    MOD IR * compdet, MOD IfCvsCtx & ctx, bool is_true_part)
{
    ASSERT0(bb && compdet);
    BBIRListIter it;
    for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         ir != nullptr; ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (ir->isBranch()) {
            //Branch op will be eliminated.
            continue;
        }
        ASSERT0(ctx.getIfCvs()->canBeConverted(ir, ctx));
        convertStmtToSelectOp(
            is_true_part, converted_stmt_list, compdet, ir, ctx);
    }
}


static bool isTruePartSelectOp(IR const* stmt, IR ** trueexp = nullptr)
{
    if (!stmt->isPartialStoreStmt()) { return false; }
    ASSERT0(stmt->hasRHS());
    IR const* purerhs = stmt->getPureRHS();
    ASSERT0(purerhs && !purerhs->is_select_to_res());
    if (!purerhs->is_select()) { return false; }
    CSelect const* sel = (CSelect const*)purerhs;
    if (sel->getTrueExp() != nullptr && sel->getFalseExp() == nullptr) {
        if (trueexp != nullptr) {
            *trueexp = sel->getTrueExp();
        }
        return true;
    }
    return false;
}


static bool isFalsePartSelectOp(IR const* stmt, IR ** falseexp = nullptr)
{
    if (!stmt->isPartialStoreStmt()) { return false; }
    ASSERT0(stmt->hasRHS());
    IR const* purerhs = stmt->getPureRHS();
    ASSERT0(purerhs && !purerhs->is_select_to_res());
    if (!purerhs->is_select()) { return false; }
    CSelect const* sel = (CSelect const*)purerhs;
    if (sel->getTrueExp() == nullptr && sel->getFalseExp() != nullptr) {
        if (falseexp != nullptr) {
            *falseexp = sel->getFalseExp();
        }
        return true;
    }
    return false;
}


static bool allBeTruePartSelectOp(IfCvsCtx::GenedList const& stmtlist)
{
    IRListIter it;
    for (IR const* stmt = stmtlist.get_head(&it);
         stmt != nullptr; stmt = stmtlist.get_next(&it)) {
        ASSERT0(isTruePartSelectOp(stmt));
    }
    return true;
}


static bool allBeFalsePartSelectOp(IfCvsCtx::GenedList const& stmtlist)
{
    IRListIter it;
    for (IR const* stmt = stmtlist.get_head(&it);
         stmt != nullptr; stmt = stmtlist.get_next(&it)) {
        ASSERT0(isFalsePartSelectOp(stmt));
    }
    return true;
}


static bool checkCanBeConvertToSelectOp(
    IRBB * truepart, IRBB * falsepart, DiamondRegion const& dr,
    MOD IfCvsCtx & ctx)
{
    ASSERT0(truepart || falsepart);
    ASSERT0(truepart != falsepart);
    IRBB * top = dr.top;
    IR * condbr = top->getLastIR();
    ASSERT0(condbr->isConditionalBr());
    if (truepart != nullptr &&
        !checkCondExecBBCanBeConvertToSelectOp(truepart, ctx, true)) {
        return false;
    }
    if (falsepart != nullptr &&
        !checkCondExecBBCanBeConvertToSelectOp(falsepart, ctx, false)) {
        return false;
    }
    return true;
}


//Return false if there exist IR in given BB that cannot be converted.
static bool genTrueAndFalsePartWithSelectOp(
    OUT IR ** compdet, IRBB * truepart, IRBB * falsepart,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    ASSERT0(truepart || falsepart);
    ASSERT0(truepart != falsepart);
    IRBB * top = dr.top;
    IR * condbr = top->getLastIR();
    ASSERT0(condbr->isConditionalBr());

    //Generate determinator of select-op.
    //NOTE: we move the expresions directly to aviod building and adding
    //DU chain for the expressions.
    Region const* rg = ctx.getRegion();
    IRMgr * irmgr = ctx.getIRMgr();

    //Use original det-expression directly to avoid update DU info.
    IR * detpart_org = BR_det(condbr);
    ASSERT0(detpart_org);
    BR_det(condbr) = nullptr;
    *compdet = irmgr->buildStorePR(detpart_org);
    ctx.getMDMgr()->allocRef(*compdet);
    if (truepart != nullptr) {
        convertBBToSelectOp(
            ctx.getTruePartGenedList(), truepart, *compdet, ctx, true);
    }
    if (falsepart != nullptr) {
        convertBBToSelectOp(
            ctx.getFalsePartGenedList(), falsepart, *compdet, ctx, false);
    }
    //Append stmt to generated-stmt-list.
    ASSERT0(verifyTreeForStmtList(rg, ctx.getTruePartGenedList()));
    ASSERT0(verifyTreeForStmtList(rg, ctx.getFalsePartGenedList()));
    dumpGenedIRList(
        ctx.getTruePartGenedList(), ctx,
        "convert truepart of control-flow to cond-exec-op");
    dumpGenedIRList(
        ctx.getFalsePartGenedList(), ctx,
        "convert falsepart of control-flow to cond-exec-op");
    return true;
}


//The function extracts TRUE and FALSE BB of diamond region, and record them
//in the ctx.
static void extractTrueAndFalsePartForDiamond(
    OUT IRBB ** truepart, OUT IRBB ** falsepart, DiamondRegion const& dr,
    MOD IfCvsCtx & ctx)
{
    ASSERT0(truepart && falsepart);
    IRBB * left = dr.left;
    IRBB * right = dr.right;
    IRBB * top = dr.top;
    IR * condbr = top->getLastIR();
    ASSERT0(condbr->isConditionalBr());
    IRBB * fallthrough = ctx.getCFG()->getFallThroughBB(top);
    ASSERT0(left == fallthrough || right == fallthrough);
    IRBB * taken = nullptr;
    if (dr.isTri()) {
        taken = nullptr;
    } else {
        taken = left == fallthrough ? right : left;
    }
    if (condbr->is_falsebr()) {
        *falsepart = taken;
        *truepart = fallthrough;
    } else {
        ASSERT0(condbr->is_truebr());
        *truepart = taken;
        *falsepart = fallthrough;
    }
}


//The function will maintain DU.
static bool removeCondBrInTop(DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    IRBB * top = dr.top;
    ASSERT0(top);
    IR * condbr = top->getLastIR();
    ASSERT0(condbr->isConditionalBr());
    Region const* rg = ctx.getRegion();
    xoc::removeStmt(condbr, rg, *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(condbr);
    top->getIRList().remove(condbr);
    rg->freeIRTree(condbr);
    return true;
}


//CASE:the complete select-op should have both true and false expression.
//e.g: $9 = select $8, 10 #true_exp : 20 #false_exp;
static bool isCompleteSelectOp(
    IR * ir, OUT IR ** trueexp, OUT IR ** falseexp)
{
    if (!ir->isStoreStmt()) { return false; }
    ASSERT0(ir->hasRHS());
    IR * rhs = ir->getRHS();
    if (!rhs->is_select()) { return false; }
    ASSERT0(trueexp && falseexp);
    *trueexp = SELECT_trueexp(rhs);
    *falseexp = SELECT_falseexp(rhs);
    ASSERT0(*trueexp && *falseexp);
    return true;
}


static bool mergeTruePartSelectOpIntoOne(
    IR * complete_select_op, IR * trueexp,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    if (!trueexp->isMemOpnd()) { return false; }
    Region * rg = ctx.getRegion();
    OptCtx const* oc = ctx.getOptCtx();
    IR const* trueexp_def = xoc::findDomAvailDef(trueexp, rg);
    if (trueexp_def == nullptr) { return false; }
    IR * partial_trueexp = nullptr;
    if (!isTruePartSelectOp(trueexp_def, &partial_trueexp)) { return false; }
    ASSERT0(partial_trueexp);
    IR * new_trueexp = ctx.getRegion()->dupIRTree(partial_trueexp);
    complete_select_op->replaceKid(trueexp, new_trueexp, true);
    xoc::addUseForTree(new_trueexp, partial_trueexp, rg);
    xoc::removeUseForTree(trueexp, rg, *oc);
    return true;
}


//Return true if IR changed.
static bool mergeFalsePartSelectOpIntoOne(
    IR * complete_select_op, IR * falseexp,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    if (!falseexp->isMemOpnd()) { return false; }
    Region * rg = ctx.getRegion();
    OptCtx const* oc = ctx.getOptCtx();
    IR const* falseexp_def = xoc::findDomAvailDef(falseexp, rg);
    if (falseexp_def == nullptr) { return false; }
    IR * partial_falseexp = nullptr;
    if (!isFalsePartSelectOp(falseexp_def, &partial_falseexp)) { return false; }
    ASSERT0(partial_falseexp);
    IR * new_falseexp = ctx.getRegion()->dupIRTree(partial_falseexp);
    complete_select_op->replaceKid(falseexp, new_falseexp, true);
    xoc::addUseForTree(new_falseexp, partial_falseexp, rg);
    xoc::removeUseForTree(falseexp, rg, *oc);
    return true;
}


//Return true if IR changed.
static bool tryMergePartialSelectOpIntoCompleteOp(
    MOD IRBB * truepart, MOD IRBB * falsepart,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //CASE:merge the first two select-op into the third-one.
    //  $5 = select_to_res (select $8, 10 #true_exp : NULL #false_exp)
    //  $6 = select_to_res (select $8, NULL #true_exp : 20 #false_exp)
    //  $9 = select $8, $5 #true_exp : $6 #false_exp
    //Into:
    //  $9 = select $8, 10 #true_exp : 20 #false_exp
    IRBB * top = dr.top;
    ASSERT0(top);
    bool changed = false;
    BBIRListIter it;
    for (top->getIRList().get_head(&it);
         it != nullptr; top->getIRList().get_next(&it)) {
        IR * ir = it->val();
        IR * trueexp;
        IR * falseexp;
        if (!isCompleteSelectOp(ir, &trueexp, &falseexp)) { continue; }
        ASSERT0(trueexp && falseexp);
        changed |= mergeTruePartSelectOpIntoOne(ir, trueexp, dr, ctx);
        changed |= mergeFalsePartSelectOpIntoOne(ir, falseexp, dr, ctx);
    }
    return changed;
}


static void removeOrgStmtFromTrueAndFalsePart(
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    IRBB * left = dr.left;
    IRBB * right = dr.right;
    IRBB * top = dr.top;
    ASSERT0(left && right && top);
    Region * rg = ctx.getRegion();

    //First of all, remove if-converted storestmts from original IRBB.
    IfCvsCtx::GenedListIter it;
    IfCvsCtx::GenedListIter nextit;
    IfCvsCtx::GenedList & tplst = ctx.getTruePartGenedList();
    IfCvsCtx::GenedList & fplst = ctx.getFalsePartGenedList();
    for (left->getIRList().get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        left->getIRList().get_next(&nextit);
        IR * c = it->val();
        if (tplst.find(c) || fplst.find(c)) {
            //Generated Stmt will be move to top BB.
            left->getIRList().remove(it);
            continue;
        }
        xoc::removeStmt(c, rg, *ctx.getOptCtx());
        ctx.tryInvalidInfoBeforeFreeIR(c);
        left->getIRList().remove(it);
        rg->freeIRTree(c);
    }
    for (right->getIRList().get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        right->getIRList().get_next(&nextit);
        IR * c = it->val();
        if (tplst.find(c) || fplst.find(c)) {
            //Generated Stmt will be move to top BB.
            right->getIRList().remove(it);
            continue;
        }
        xoc::removeStmt(c, rg, *ctx.getOptCtx());
        ctx.tryInvalidInfoBeforeFreeIR(c);
        right->getIRList().remove(it);
        rg->freeIRTree(c);
    }
}


//The function will maintain DU.
static bool moveGenedStmtToTop(
    IR * compdet, DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    IRBB * left = dr.left;
    IRBB * right = dr.right;
    IRBB * top = dr.top;
    ASSERT0(left && right && top);

    //First of all, remove if-converted storestmts from original IRBB.
    removeOrgStmtFromTrueAndFalsePart(dr, ctx);

    ASSERT0(ctx.getTopGenedList().get_elem_count() == 0);
    BBIRList & topirlst = top->getIRList();

    //First, add determinate-computation OP.
    topirlst.append_tail(compdet);
    ctx.getTopGenedList().append_tail(compdet);

    //Sencond, the truepart partial-storestmt-op.
    for (IR * t = ctx.getTruePartGenedList().get_head();
         t != nullptr; t = ctx.getTruePartGenedList().get_next()) {
        topirlst.append_tail(t);
    }

    //Third, the falsepart partial-storestmt-op.
    for (IR * t = ctx.getFalsePartGenedList().get_head();
         t != nullptr; t = ctx.getFalsePartGenedList().get_next()) {
        topirlst.append_tail(t);
    }
    return true;
}


static bool removeEmptyTrueAndFalsePart(
    MOD IRBB * truepart, MOD IRBB * falsepart,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //NOTE: remove true and false BB does NOT change DOM and PDOM info.
    IRBB * top = dr.top;
    IRBB * bottom = dr.bottom;
    ASSERT0(truepart == nullptr || truepart->getNumOfIR() == 0);
    ASSERT0(falsepart == nullptr || falsepart->getNumOfIR() == 0);
    Region * rg = ctx.getRegion();
    IRCFG * cfg = ctx.getCFG();
    IRCfgOptCtx cfgoptctx(ctx.getOptCtx());

    //Note the Dom Info is unchange if removing true and false part.
    //However, the Phi needs to be maintained.
    CFGOPTCTX_need_update_dominfo(&cfgoptctx) = false;
    if (truepart != nullptr) {
        //In triangle-case, one of part may be NULL.
        cfg->removeBB(truepart, cfgoptctx);
    }
    if (falsepart != nullptr) {
        //In triangle-case, one of part may be NULL.
        cfg->removeBB(falsepart, cfgoptctx);
    }
    cfg->addEdge(top, bottom, cfgoptctx);
    ctx.getOptCtx()->setInvalidIfCFGChangedExcept(
        ctx.getOptCtx(), PASS_RPO, PASS_DOM, PASS_PDOM, PASS_UNDEF);
    ASSERT0(verifyIRandBB(rg->getBBList(), rg));
    ASSERT0(cfg->verifyRPO(*ctx.getOptCtx()));
    ASSERT0(cfg->verifyLoopInfo(*ctx.getOptCtx()));
    ASSERT0(cfg->verifyDomAndPdom(*ctx.getOptCtx()));
    return true;
}


static bool isDefOfMDPhiOpndInNoneOfPart(
    MDPhi const* phi, DiamondRegion const& dr, IfCvsCtx const& ctx)
{
    ASSERT0(ctx.useMDSSADU());
    ASSERT0(ctx.getMDSSAMgr());
    ASSERT0(phi->is_phi());
    ASSERT0(phi->getOpndNum() == 2);
    IRBB * left = dr.left;
    IRBB * right = dr.right;
    ASSERT0(left == right);
    IRBB * mid = left;
    VMD const* vmd0 = phi->getVMDOfNthOpnd(0, ctx.getMDSSAMgr());
    VMD const* vmd1 = phi->getVMDOfNthOpnd(1, ctx.getMDSSAMgr());
    ASSERT0(vmd0 && vmd1);
    if (vmd0->isLiveIn()) {
        if (vmd1->isLiveIn()) {
            //There is at least one virtual-DEF in mid-BB.
            return false;
        }
        IR * def1 = vmd1->getOcc();
        if (def1 == nullptr) {
            //There is no DEF.
            return true;
        }
        if (def1->getBB() == mid) {
            //There is at least one DEF in mid-BB.
            return false;
        }
        return true;
    }
    if (vmd1->isLiveIn()) {
        if (vmd0->isLiveIn()) {
            //There is at least one virtual-DEF in mid-BB.
            return false;
        }
        IR * def0 = vmd0->getOcc();
        if (def0 == nullptr) {
            //There is no DEF.
            return true;
        }
        if (def0->getBB() == mid) {
            //There is at least one DEF in mid-BB.
            return false;
        }
        return true;
    }
    ASSERT0(vmd0->getDef() && vmd1->getDef());
    IRBB const* defbb0 = vmd0->getDef()->getBB();
    IRBB const* defbb1 = vmd1->getDef()->getBB();
    ASSERT0(defbb0 && defbb1);
    if (defbb0 == mid || defbb1 == mid) {
        //There is at least one DEF in mid-BB.
        return false;
    }
    return true;
}


static bool isDefOfMDPhiOpndInOneOfPart(
    MDPhi const* phi, DiamondRegion const& dr, IfCvsCtx const& ctx)
{
    ASSERT0(ctx.useMDSSADU());
    ASSERT0(ctx.getMDSSAMgr());
    ASSERT0(phi->is_phi());
    ASSERT0(phi->getOpndNum() == 2);
    IRBB * left = dr.left;
    IRBB * right = dr.right;
    ASSERT0(left == right);
    IRBB * mid = left;
    VMD const* vmd0 = phi->getVMDOfNthOpnd(0, ctx.getMDSSAMgr());
    VMD const* vmd1 = phi->getVMDOfNthOpnd(1, ctx.getMDSSAMgr());
    if (vmd0->isLiveIn()) {
        if (vmd1->isLiveIn()) {
            //There is at least one virtual-DEF in mid-BB.
            return false;
        }
        IR * def1 = vmd1->getOcc();
        if (def1 == nullptr) { return false; }
        if (def1->getBB() != mid) {
            //There is at least one DEF in mid-BB.
            return false;
        }
        return true;
    }
    if (vmd1->isLiveIn()) {
        if (vmd0->isLiveIn()) {
            //There is at least one virtual-DEF in mid-BB.
            return false;
        }
        IR * def0 = vmd0->getOcc();
        if (def0 == nullptr) { return false; }
        if (def0->getBB() != mid) {
            //There is at least one DEF in mid-BB.
            return false;
        }
        return true;
    }
    ASSERT0(vmd0->getDef() && vmd1->getDef());
    IRBB const* defbb0 = vmd0->getDef()->getBB();
    IRBB const* defbb1 = vmd1->getDef()->getBB();
    ASSERT0(defbb0 && defbb1);
    if (defbb0 != mid && defbb1 != mid) {
        //There is at least one DEF in mid-BB.
        return false;
    }
    if (!((defbb0 == mid) ^ (defbb1 == mid))) {
        //Def stmt of opnd must be in different BB.
        return false;
    }
    return true;
}


static bool isDefOfMDPhiOpndInDistinctBB(
    MDPhi const* phi, DiamondRegion const& dr, IfCvsCtx const& ctx)
{
    ASSERT0(ctx.useMDSSADU());
    ASSERT0(ctx.getMDSSAMgr());
    ASSERT0(phi->is_phi());
    ASSERT0(phi->getOpndNum() == 2);
    IRBB * left = dr.left;
    IRBB * right = dr.right;
    VMD const* vopnd0 = phi->getVMDOfNthOpnd(0, ctx.getMDSSAMgr());
    VMD const* vopnd1 = phi->getVMDOfNthOpnd(1, ctx.getMDSSAMgr());
    ASSERTN(vopnd0, ("ID's VOpnd should be unique"));
    ASSERTN(vopnd0->is_md(), ("invalid ID's VOpnd"));
    ASSERTN(vopnd1, ("ID's VOpnd should be unique"));
    ASSERTN(vopnd1->is_md(), ("invalid ID's VOpnd"));
    IRBB * defbb0 = nullptr;
    IRBB * defbb1 = nullptr;
    if (vopnd0->isLiveIn()) { defbb0 = nullptr; }
    else { defbb0 = vopnd0->getDef()->getBB(); }
    if (vopnd1->isLiveIn()) { defbb1 = nullptr; }
    else { defbb1 = vopnd1->getDef()->getBB(); }
    if (defbb0 != nullptr && defbb1 == nullptr) {
        if (defbb0 != left && defbb0 != right) {
            return false;
        }
        return true;
    }
    if (defbb0 == nullptr && defbb1 != nullptr) {
        if (defbb1 != left && defbb1 != right) {
            return false;
        }
        return true;
    }
    if (defbb0 != left && defbb0 != right) {
        //CASE:compile/if_cvs_mdphi.c
        //One of defbb0 and defbb1 are out side dimanond-region.
        //In the case, at least one of them must be at right or left BB.
        //    BB_outside_region(defbb0)
        //    |
        //    v
        //    top
        //    |  |
        //    v  |
        //  left v
        //    |  right(defbb1)
        //    |  |
        //    v  v
        //    bottom
        if (defbb1 == left || defbb1 == right) { return true; }
        return false;
    }
    if (defbb1 != left && defbb1 != right) {
        //CASE:compile/if_cvs_mdphi.c
        //One of defbb0 and defbb1 are out side dimanond-region.
        //In the case, at least one of them must be at right or left BB.
        //    BB_outside_region(defbb0)
        //    |
        //    v
        //    top
        //    |  |
        //    v  |
        //  left v
        //    |  right(defbb1)
        //    |  |
        //    v  v
        //    bottom
        if (defbb0 == left || defbb0 == right) { return true; }
        return false;
    }
    if (!((defbb0 == left) ^ (defbb1 == left))) {
        //Def stmt of opnd must be in different BB.
        return false;
    }
    return true;
}


static bool isDefOfMDPhiOpndInOneOfPartForPhiList(
    DiamondRegion const& dr, IfCvsCtx const& ctx)
{
    ASSERT0(ctx.useMDSSADU());
    ASSERT0(ctx.getMDSSAMgr());
    IRBB const* bottom = dr.bottom;
    ASSERT0(bottom);
    MDPhiList const* philist = ctx.getMDSSAMgr()->getPhiList(bottom);
    if (philist == nullptr) { return true; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());

        //CASE:vect_if_cvs.c
        //Using isDefOfMDPhiOpndInNoneOfPart:
        //  BB2
        //  |
        //  v
        //  BB3
        //  |  |
        //  |  v
        //  |  BB4
        //  |  |
        //  v  v
        //  BB5
        //  MDPhi2:MD16V3 <- (id MD16V1 BB3), (id MD16V1 BB4)
        //In the case, since some previous optimizations have transform the
        //MDSSA info, both MDDef of two operands of MDPhi2 have been moved to
        //BB2. In the situation, none of MDDef should be in mid-BB.
        ASSERTN(isDefOfMDPhiOpndInOneOfPart(phi, dr, ctx) ||
                isDefOfMDPhiOpndInNoneOfPart(phi, dr, ctx),
                ("redundant or illegal phi"));
    }
    return true;
}


static bool isDefOfMDPhiOpndInDistinctPartForPhiList(
    DiamondRegion const& dr, IfCvsCtx const& ctx)
{
    ASSERT0(ctx.useMDSSADU());
    ASSERT0(ctx.getMDSSAMgr());
    IRBB const* bottom = dr.bottom;
    ASSERT0(bottom);
    MDPhiList const* philist = ctx.getMDSSAMgr()->getPhiList(bottom);
    if (philist == nullptr) { return true; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        ASSERTN(isDefOfMDPhiOpndInDistinctBB(phi, dr, ctx),
                ("redundant or illegal phi"));
    }
    return true;
}


static IR * replacePhiWithSelectOp(
    MOD IR * compdet, IR const* ir, IRBB const* truepart, IRBB const* falsepart,
    IfCvsCtx const& ctx)
{
    //NOTE: Either truepart or falsepart could be NULL.
    ASSERT0(ir->is_phi());
    CPhi * phi = (CPhi*)ir;
    ASSERT0(phi->getOpndNum() == 2);
    ASSERT0(phi->getBB());
    bool is_pred = false;
    UINT pred_pos = -1;
    IRBB const* in0 = nullptr; //record the first incoming BB of phi's BB.
    if (truepart != nullptr) {
        pred_pos = ctx.getCFG()->WhichPred(truepart, phi->getBB(), is_pred);
        ASSERT0(is_pred);
        if (pred_pos == 0) {
            in0 = truepart;
        } else {
            ASSERT0(pred_pos == 1);
            in0 = falsepart;
        }
    } else {
        ASSERT0(falsepart);
        pred_pos = ctx.getCFG()->WhichPred(falsepart, phi->getBB(), is_pred);
        ASSERT0(is_pred);
        if (pred_pos == 0) {
            in0 = falsepart;
        } else {
            ASSERT0(pred_pos == 1);
            in0 = truepart;
        }
    }
    //NOTE: in0 may be NULL if one of truepart or falsepart is NULL.
    IR const* opnd0 = phi->getOpnd(0);
    IR const* opnd1 = phi->getOpnd(1);
    ASSERT0(opnd0 && opnd1);
    if (in0 == falsepart) {
        xcom::swap(opnd0, opnd1);
    }
    Region * rg = ctx.getRegion();
    OptCtx * oc = ctx.getOptCtx();
    IR * trueexp = nullptr;
    IR * falseexp = nullptr;
    if (opnd0->isMemRef()) {
        trueexp = rg->dupIsomoExpTree(opnd0);
    } else {
        ASSERT0(opnd0->isPhiOpnd());
        trueexp = rg->dupIRTree(opnd0);
    }
    if (opnd1->isMemRef()) {
        falseexp = rg->dupIsomoExpTree(opnd1);
    } else {
        ASSERT0(opnd1->isPhiOpnd());
        falseexp = rg->dupIRTree(opnd1);
    }
    if (opnd0->isPROp()) {
        ASSERT0(opnd0->isReadPR() && opnd0->getSSAInfo());

        //For the new generated expression, make it to the new USE of
        //original opnd. The original opnd's DU info will be discarded when
        //the IR is freed.
        xoc::addUseForTree(trueexp, opnd0, rg);
    }
    if (opnd1->isPROp()) {
        ASSERT0(opnd1->isReadPR() && opnd1->getSSAInfo());

        //For the new generated expression, make it to the new USE of
        //original opnd. The original opnd's DU info will be discarded when
        //the IR is freed.
        xoc::addUseForTree(falseexp, opnd1, rg);
    }
    IR * det = rg->dupIsomoExpTree(compdet);
    det->setType(rg->getTypeMgr()->getBool());
    det->copyRef(compdet, rg);
    xoc::buildDUChain(compdet, det, rg, *oc);
    ASSERT0(trueexp && falseexp);
    Type const* parttype = trueexp->getType();

    //Generate the select.
    IR * select = rg->getIRMgr()->buildSelect(
        det, trueexp, falseexp, parttype);
    IR * rhs = select;
    if (CSelect::isPartialSelect(select)) {
        rhs = ((IRMgrExt*)rg->getIRMgr())->
            buildSelectToRes(select, nullptr, parttype);
    }
    IR * stpr = rg->getIRMgr()->buildStorePR(phi->getType(), rhs);
    ctx.getMDMgr()->allocRef(stpr);
    return stpr;
}


static bool replacePhiListWithSelectOp(
    MOD IR * compdet, IRBB const* truepart, IRBB const* falsepart,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //dr can be both Tri and Diamond.
    IRBB * top = dr.top;
    IRBB * bottom = dr.bottom;
    ASSERT0(top && bottom);
    BBIRListIter it;
    BBIRListIter nextit;
    BBIRList & irlst = bottom->getIRList();
    Region * rg = ctx.getRegion();
    OptCtx * oc = ctx.getOptCtx();
    for (irlst.get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        irlst.get_next(&nextit);
        IR * t = it->val();
        if (!t->is_phi()) { break; }
        IR * stpr = replacePhiWithSelectOp(
            compdet, t, truepart, falsepart, ctx);
        ASSERT0(stpr && stpr->is_stpr());
        top->getIRList().append_tail(stpr);
        ctx.getBottomGenedList().append(stpr);
        xoc::changeDef(t, stpr, rg);
        xoc::removeStmt(t, rg, *oc);
        ctx.tryInvalidInfoBeforeFreeIR(t);
        irlst.remove(it);
        rg->freeIRTree(t);
    }
    return true;
}


static bool reconstructPhiInBottom(
    MOD IR * compdet, IRBB const* truepart, IRBB const* falsepart,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //NOTE:There is no need to replace MDPhi.
    if (!ctx.usePRSSADU()) { return true; }
    replacePhiListWithSelectOp(compdet, truepart, falsepart, dr, ctx);
    return true;
}


static void updateMDSSADUForDefDefChainInTop(
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    if (!ctx.useMDSSADU()) { return; }

    //Since the DEF in the true-or-false part might affect the version of MD at
    //the subsequent BBs right after diamond region, we have to recompute the
    //MDSSA. Therefore, the DOM info is necessary.
    ctx.getRegion()->getPassMgr()->checkValidAndRecompute(
        ctx.getOptCtx(), PASS_DOM, PASS_UNDEF);
    IRBB const* top = dr.bottom;
    ASSERT0(top);

    //Build DomTree because CFG changed.
    xcom::DomTree domtree;
    ctx.getCFG()->genDomTree(domtree);
    MDSSAMgr * mgr = ctx.getMDSSAMgr();
    OptCtx const* oc = ctx.getOptCtx();
    ActMgr * am = ctx.getActMgr();
    for (IR * ir = ctx.getTopGenedList().get_head();
         ir != nullptr; ir = ctx.getTopGenedList().get_next()) {
        ASSERT0(ir->is_stmt());
        if (!MDSSAMgr::hasMDSSAInfo(ir)) { continue; }

        //Update the stmt's MDSSAInfo which include DefDef chain and
        //DefUse chain.
        xoc::recomputeDefUseChainForAllExp(ir, domtree, mgr, *oc, am);
        xoc::recomputeDefDefAndDefUseChain(ir, domtree, mgr, *oc, am);
    }
    for (IR * ir = ctx.getTruePartGenedList().get_head();
         ir != nullptr; ir = ctx.getTruePartGenedList().get_next()) {
        ASSERT0(ir->is_stmt());
        if (!MDSSAMgr::hasMDSSAInfo(ir)) { continue; }

        //Update the stmt's MDSSAInfo which include DefDef chain and
        //DefUse chain.
        xoc::recomputeDefUseChainForAllExp(ir, domtree, mgr, *oc, am);
        xoc::recomputeDefDefAndDefUseChain(ir, domtree, mgr, *oc, am);
    }
    for (IR * ir = ctx.getFalsePartGenedList().get_head();
         ir != nullptr; ir = ctx.getFalsePartGenedList().get_next()) {
        ASSERT0(ir->is_stmt());
        if (!MDSSAMgr::hasMDSSAInfo(ir)) { continue; }

        //Update the stmt's MDSSAInfo which include DefDef chain and
        //DefUse chain.
        xoc::recomputeDefUseChainForAllExp(ir, domtree, mgr, *oc, am);
        xoc::recomputeDefDefAndDefUseChain(ir, domtree, mgr, *oc, am);
    }
    for (IR * ir = ctx.getBottomGenedList().get_head();
         ir != nullptr; ir = ctx.getBottomGenedList().get_next()) {
        ASSERT0(ir->is_stmt());
        if (!MDSSAMgr::hasMDSSAInfo(ir)) { continue; }

        //Update the stmt's MDSSAInfo which include DefDef chain and
        //DefUse chain.
        xoc::recomputeDefUseChainForAllExp(ir, domtree, mgr, *oc, am);
        xoc::recomputeDefDefAndDefUseChain(ir, domtree, mgr, *oc, am);
    }
}


static void updateDU(DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //DU chains of PROP are not changed.
    updateMDSSADUForDefDefChainInTop(dr, ctx);
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(ctx.getRegion(), *ctx.getOptCtx()));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(ctx.getRegion(), *ctx.getOptCtx()));
}


static bool reconstructDiamondRegionCase(
    MOD IR * compdet, MOD IRBB * truepart, MOD IRBB * falsepart,
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    if (dr.isTri()) {
        ASSERT0(!ctx.useMDSSADU() ||
                isDefOfMDPhiOpndInOneOfPartForPhiList(dr, ctx));
    } else {
        ASSERT0(!ctx.useMDSSADU() ||
                isDefOfMDPhiOpndInDistinctPartForPhiList(dr, ctx));
    }
    //TODO:tryMergePartialSelectOpIntoCompleteOp(truepart, falsepart, dr, ctx);
    removeCondBrInTop(dr, ctx);
    moveGenedStmtToTop(compdet, dr, ctx);
    reconstructPhiInBottom(compdet, truepart, falsepart, dr, ctx);
    removeEmptyTrueAndFalsePart(truepart, falsepart, dr, ctx);
    updateDU(dr, ctx);
    return true;
}


//Return true if if-conversion has been performed and IR changed.
static bool tryConvertDiamondRegionCase(
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //Diamond Region.
    //   ---BR
    //  |   |
    //  |   FALLTHROUGH --
    //  |                  |
    //  |                  |
    //   -> TGTBB          |
    //      |              |
    //      |              |
    //      v              |
    //      END <----------
    IRBB * truepart = nullptr;
    IRBB * falsepart = nullptr;
    IR * compdet = nullptr;
    dumpDR(dr, ctx, "find diamond region");
    extractTrueAndFalsePartForDiamond(&truepart, &falsepart, dr, ctx);
    ASSERTN(truepart && falsepart, ("both true and false part be avail"));
    if (!checkCanBeConvertToSelectOp(truepart, falsepart, dr, ctx)) {
        return false;
    }
    if (!genTrueAndFalsePartWithSelectOp(
            &compdet, truepart, falsepart, dr, ctx)) {
        return false;
    }
    if (!reconstructDiamondRegionCase(compdet, truepart, falsepart, dr, ctx)) {
        return false;
    }
    return true;
}


bool IfConversion::canBeConverted(IR const* comp, IfCvsCtx const& ctx) const
{
    if (comp->isCallStmt()) { return false; }
    if (comp->isVirtualOp()) { return false; }
    if (comp->hasSideEffect(true)) { return false; }
    if (!comp->hasRHS()) { return false; }
    ASSERT0(!comp->isBranch());
    return true;
}


//Return true if if-conversion has been performed and IR changed.
static bool tryConvertTriangleRegionCase(
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    //Triangle Region:
    //  BR(top)------------------
    //  |                        |
    //  FALL-THROUGH(left|right) |
    //  |                        |
    //  TGT <--------------------
    //AND:
    //  BR(top)-----------------------
    //  |                             |
    //  FALL-THROUGH(left|right)---   |
    //                             |  |
    //                             v  |
    //  TGT <-------------------------
    IRBB * truepart = nullptr;
    IRBB * falsepart = nullptr;
    IR * compdet = nullptr;
    dumpDR(dr, ctx, "find triangle region");
    extractTrueAndFalsePartForDiamond(&truepart, &falsepart, dr, ctx);
    ASSERTN(truepart == nullptr || falsepart == nullptr,
            ("at least one part is NULL"));
    if (!checkCanBeConvertToSelectOp(truepart, falsepart, dr, ctx)) {
        return false;
    }
    if (!genTrueAndFalsePartWithSelectOp(
            &compdet, truepart, falsepart, dr, ctx)) {
        return false;
    }
    if (!reconstructDiamondRegionCase(compdet, truepart, falsepart, dr, ctx)) {
        return false;
    }
    return true;
}


//Return true if if-conversion has been performed and IR changed.
static bool tryConvertBB(IRBB const* bb, MOD IfCvsCtx & ctx)
{
    ASSERT0(bb);
    DiamondRegion dr;
    if (!IfConversion::findDiamondRegion(bb, ctx, dr)) { return false; }
    if (dr.isTri()) {
        return tryConvertTriangleRegionCase(dr, ctx);
    }
    return tryConvertDiamondRegionCase(dr, ctx);
}


bool IfConversion::tryConvertDiamondRegion(
    DiamondRegion const& dr, MOD IfCvsCtx & ctx)
{
    if (dr.isTri()) {
        return tryConvertTriangleRegionCase(dr, ctx);
    }
    return tryConvertDiamondRegionCase(dr, ctx);
}


IR * IfConversion::convertSelectToBranch(IR const* ir, MOD IfCvsCtx & ctx)
{
    ASSERT0(ir->is_stmt() && ir->hasRHS());
    IR * rhs = ir->getRHS();
    ASSERT0(rhs->is_select());
    Region * rg = ctx.getRegion();
    IRMgr * irmgr = rg->getIRMgr();

    //Build IF det expression.
    IR * det = rg->dupIRTree(SELECT_det(rhs));
    if (!det->is_judge()) {
        //det-expression of IR stmt must be judgement.
        det = irmgr->buildJudge(det);
    }

    //Build true body.
    IR * truestmt = nullptr;
    if (SELECT_trueexp(rhs) != nullptr) {
        truestmt = rg->dupIsomoStmt(ir, rg->dupIRTree(SELECT_trueexp(rhs)));
    }

    //Build false body.
    IR * falsestmt = nullptr;
    if (SELECT_falseexp(rhs) != nullptr) {
        falsestmt = rg->dupIsomoStmt(ir, rg->dupIRTree(SELECT_falseexp(rhs)));
    }
    //Build IF cfs.
    IR * ifop = irmgr->buildIf(det, truestmt, falsestmt);
    IRSimp * simp = (IRSimp*)rg->getPassMgr()->registerPass(PASS_IRSIMP);
    ASSERT0(simp && simp->is_valid());

    //Simplify to stmts list.
    SimpCtx simpctx(ctx.getOptCtx());
    simpctx.setSimpCFS();
    IR * stmtlist = simp->simplifyStmt(ifop, &simpctx);
    dumpReverseIfCvs(ifop, stmtlist, ctx);
    return stmtlist;
}


bool IfConversion::tryBBListImpl(MOD IfCvsCtx & ctx)
{
    bool changed = false;
    BBList * bbl = ctx.getRegion()->getBBList();
    BBListIter it;
    for (IRBB * bb = bbl->get_head(&it); bb != nullptr;) {
        IfCvsCtx tctx(ctx);
        bool lchanged = tryConvertBB(bb, tctx);
        changed |= lchanged;
        if (lchanged) {
            //Rescan BB list to find more convesion opportunities.
            bb = bbl->get_head(&it);
            ctx.unionBottomUpInfo(tctx);
            continue;
        }
        bb = bbl->get_next(&it);
    }
    ASSERT0(ctx.verify());
    return changed;
}


bool IfConversion::tryBBList(MOD OptCtx & oc)
{
    IfCvsCtx ctx(oc, nullptr, this, &getActMgr());
    if (!tryBBListImpl(ctx)) { return false; }
    if (g_dump_opt.isDumpAfterPass() &&
        g_dump_opt.isDumpPass(PASS_IF_CONVERSION)) {
        ctx.dump();
    }
    return true;
}


bool IfConversion::tryLoopImpl(MOD IfCvsCtx & ctx)
{
    LI<IRBB> const* li = ctx.getLI();
    ASSERT0(li->getLoopHead());
    bool changed = false;
    IRCFG * cfg = ctx.getCFG();
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = cfg->getBB(i);
        ASSERT0(bb);
        changed |= tryConvertBB(bb, ctx);
        if (changed) {
            //Rescan loop body BBSet to find more convesion opportunities.
            i = li->getBodyBBSet()->get_first();
        }
    }
    return changed;
}


bool IfConversion::tryLoop(MOD LI<IRBB> * li, MOD OptCtx & oc)
{
    IfCvsCtx ctx(oc, li, this, &getActMgr());
    if (!tryLoopImpl(ctx)) {
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() &&
        g_dump_opt.isDumpPass(PASS_IF_CONVERSION)) {
        ctx.dump();
    }
    return true;
}


bool IfConversion::doLoopTree(MOD LI<IRBB> * li, MOD OptCtx & oc)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != nullptr; tli = tli->get_next()) {
        bool lchanged_inner = doLoopTree(tli->getInnerList(), oc);
        changed |= lchanged_inner;
        if (lchanged_inner) {
            //Inner Loop may have been destroyed, reperform doLoopTree().
            return changed;
        }
        bool lchanged_cur_loop = tryLoop(tli, oc);
        changed |= lchanged_cur_loop;
        if (lchanged_cur_loop) {
            return changed;
        }
    }
    return changed;
}


bool IfConversion::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    START_TIMER(t, getPassName());
    if (!initDepPass(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    reset();

    //DumpBufferSwitch buff(m_rg->getLogMgr());
    //if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    bool changed = false;
    bool lchanged = false;
    IRCFG * cfg = getRegion()->getCFG();
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(getRegion(), oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(getRegion(), oc));
    if (isTryLoopTree()) {
        do {
            lchanged = doLoopTree(cfg->getLoopInfo(), oc);
            changed |= lchanged;
        } while (lchanged);
    } else {
        changed = tryBBList(oc);
    }
    if (!changed) {
        //m_rg->getLogMgr()->cleanBuffer();
        END_TIMER(t, getPassName());
        return false;
    }
    dump();

    //The pass does not devastate IVR information. However, new IV might be
    //inserted.
    //DU chain and DU reference should be maintained.
    DUMgr * dumgr = getRegion()->getDUMgr();
    ASSERT0(dumgr);
    ASSERT0(dumgr->verifyMDRef() && verifyClassicDUChain(getRegion(), oc));
    oc.setInvalidIfDUMgrLiveChanged();
    oc.setInvalidIfCFGChangedExcept(
        &oc, PASS_RPO, PASS_LOOP_INFO, PASS_DOM, PASS_PDOM, PASS_UNDEF);
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(getRegion(), oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(getRegion(), oc));
    END_TIMER(t, getPassName());
    return true;
}
//END IfConversion

} //namespace xoc
