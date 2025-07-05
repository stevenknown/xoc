/*@Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

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

#define MAX_PHY_REG_NUM 128

//If this flag is true, the verify function of RegisterVerify will be executed.
//Only used during the debugging phase.
static bool g_enable_lsra_over_strict = false;

static void dumpInsertMove(LSRAImpl & lsra, IRBB const* mvbb, IR const* mv,
                           CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf buf(64);
    va_list args;
    va_start(args, format);
    buf.vstrcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "INSERT_MV:insert move ir id:%u at BB%u, reason:%s",
        mv->id(), mvbb->id(), buf.buf);
}


static void dumpInsertBB(LSRAImpl & lsra, IRBB const* from, IRBB const* to,
                         IRBB const* newbb, CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf buf(64);
    va_list args;
    va_start(args, format);
    buf.vstrcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "INSERT_BB:insert BB%u between BB%u and BB%u, reason:%s",
        newbb->id(), from->id(), to->id(), buf.buf);
}


static void dumpSplitTwo(LSRAImpl & lsra, LifeTime const* lt,
                         LifeTime const* newlt,
                         Pos lt_end_pos, Pos newlt_start_pos)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    lsra.getActMgr().dump(
        "SPLIT:$%u into $%u and $%u, $%u end at pos:%u, $%u start from pos:%u",
        lt->getPrno(), lt->getPrno(), newlt->getPrno(),
        lt->getPrno(),  lt_end_pos,
        newlt->getPrno(), newlt_start_pos);
}


static void dumpSpill(LSRAImpl & lsra, IR const* spill, Reg reg,
                      IRBB const* bb, CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::DefFixedStrBuf tmpbuf;
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    ActHandler acth = lsra.getActMgr().dump(
        "SPILL:insert spill ir id:%u at BB%u to spill %s",
        spill->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
    if (format != nullptr) {
        ASSERT0(acth.info);
        acth.info->strcat(", reason:%s", tmpbuf.getBuf());
    }
}


static void dumpSpill(LSRAImpl & lsra, IR const* spill, LifeTime const* lt,
                      IR const* marker, bool before)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    lsra.getActMgr().dump(
        "SPILL:insert spill ir id:%u %s ir id:%u while splitting $%u",
        spill->id(), before ? "before" : "after",
        marker->id(), lt->getPrno());
}


static void dumpReload(LSRAImpl & lsra, IR const* reload, Reg reg,
                       IRBB const* bb, CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::DefFixedStrBuf tmpbuf;
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    ActHandler acth = lsra.getActMgr().dump(
        "RELOAD:insert reload ir id:%u at BB%u to remat %s",
        reload->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
    if (format != nullptr) {
        ASSERT0(acth.info);
        acth.info->strcat(", reason:%s", tmpbuf.getBuf());
    }
}


static void dumpReload(LSRAImpl & lsra, IR const* reload, LifeTime const* lt,
                       LifeTime const* newlt, IR const* marker)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    lsra.getActMgr().dump(
        "RELOAD:insert reload ir id:%u before ir id:%u "
        "while splitting $%u, $%u rename to $%u",
        reload->id(), marker->id(), lt->getPrno(), lt->getPrno(),
        newlt->getPrno());
}


static void dumpRemat(LSRAImpl & lsra, IR const* remat, Reg reg,
                      IRBB const* bb, CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::DefFixedStrBuf tmpbuf;
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    ActHandler acth = lsra.getActMgr().dump(
        "RELOAD:insert remat ir id:%u at BB%u to reload %s",
        remat->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
    if (format != nullptr) {
        ASSERT0(acth.info);
        acth.info->strcat(", reason:%s", tmpbuf.getBuf());
    }
}


static void dumpRemat(LSRAImpl & lsra, IR const* remat, LifeTime const* lt,
                      LifeTime const* newlt, IR const* marker)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    lsra.getActMgr().dump(
        "RELOAD:insert remat ir id:%u before ir id:%u "
        "while splitting $%u, $%u rename to $%u",
        remat->id(), marker->id(), lt->getPrno(), lt->getPrno(),
        newlt->getPrno());
}


static void dumpSelectSplitCand(LSRAImpl & lsra, LifeTime const* lt,
                                Pos split_pos,
                                bool canbe, CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::DefFixedStrBuf tmpbuf;
    if (canbe) {
        if (format != nullptr) {
            va_list args;
            va_start(args, format);
            tmpbuf.vstrcat(format, args);
            va_end(args);
        }
        ActHandler acth = lsra.getActMgr().dump(
            "SELECT_SPLIT_CAND:$%u is split-candidate at pos:%u",
            lt->getPrno(), split_pos);
        if (format != nullptr) {
            ASSERT0(acth.info);
            acth.info->strcat(", reason:%s", tmpbuf.getBuf());
        }
        return;
    }
    //Reason is necessary.
    ASSERT0(format);
    va_list args;
    va_start(args, format);
    tmpbuf.strcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "SELECT_SPLIT_CAND:$%u can NOT be splitted at pos:%u, reason:%s",
        lt->getPrno(), split_pos, tmpbuf.getBuf());
}


static LifeTime * pickFromSet(PRNO prno, MOD LTSet & set)
{
    LTSetIter it;
    LTSetIter nit;
    for (set.get_head(&it), nit = it; it != nullptr; it = nit) {
        set.get_next(&nit);
        LifeTime * lt = it->val();
        if (lt->getPrno() == prno) {
            set.remove(it);
            return lt;
        }
    }
    return nullptr;
}


static void genInconsistPairPR2PR(OUT InConsistPair * pair, UINT from, UINT to,
    LifeTime const* from_lt, LifeTime const* to_lt)
{
    ASSERT0(pair && from_lt && to_lt);
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    pair->from_vex_id = from;
    pair->to_vex_id = to;
    pair->from_lt = from_lt;
    pair->to_lt = to_lt;
    pair->type = INCONSIST_PR2PR;
}


static void genInconsistPairPR2MEM(OUT InConsistPair * pair, UINT from, UINT to,
    LifeTime const* from_lt, Var const* spill_loc)
{
    ASSERT0(pair && from_lt && spill_loc);
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    pair->from_vex_id = from;
    pair->to_vex_id = to;
    pair->from_lt = from_lt;
    pair->mem_var = spill_loc;
    pair->type = INCONSIST_PR2MEM;
}


static void genInconsistPairMEM2PR(OUT InConsistPair * pair, UINT from, UINT to,
    LifeTime const* to_lt, Var const* spill_loc)
{
    ASSERT0(pair && to_lt && spill_loc);
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    pair->from_vex_id = from;
    pair->to_vex_id = to;
    pair->to_lt = to_lt;
    pair->mem_var = spill_loc;
    pair->type = INCONSIST_MEM2PR;
}


static void genInconsistPairRemat(OUT InConsistPair * pair, UINT from, UINT to,
    IR const* exp, LifeTime const* to_lt)
{
    ASSERT0(pair && to_lt && exp);
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    pair->from_vex_id = from;
    pair->to_vex_id = to;
    pair->remat_exp = exp;
    pair->to_lt = to_lt;
    pair->type = INCONSIST_REMAT;
}


//
//START LatchBBReorder
//
//This class shall reorder the MOV IRs inserted in the latch BB due to
//the USE dependencies. Normally, the data in the physical-register should
//be used first before the data in physical-register is modified.
//
//There are two kinds of USE dependency problems:
//1. Normal dependency problem.
//2. Cyclic dependency problem.
//
//For example:
//  The group of MOV IRs are in the order below:
//  r10 <- mov r8    #S1
//   r7 <- mov r10   #S2
//   r3 <- mov r5    #S3
//   r4 <- mov r3    #S4
//   r5 <- mov r4    #S5
//  1. r10 in #S1 and #S2 are normal dependency problem.
//     r10 will be wrote to r7 in #S2, but before the original data is used,
//     r10 is changed to the data of r8 in #S1, this is not what we expected,
//     the correct order for #S1 and #S2 should ensure #S2 before #S1, which
//     means the data of r10 is moved to r7 before it is wroten by r8.
//
//  2. r3, r4 and r5 in #S3, #S4 and #S5 are cyclic dependency problem.
//     Firstly, we want to move r5 to r3 in #S3, but before change the r3, we
//     have to move r3 to r4 in #S4, this is a basic rule, and then apply the
//     same rule for r4, we find it needs to move r4 to r5 before #S4 is
//     executed, so now, we can find these dependencies look like a "cycle",
//     they dependents each other.
//
//This class will implement a reorder algorithm to resolve the two kinds
//of USE dependency problem. First, distinguish the normal dependency MOV IRs
//and reorder these MOV IRs. and then process the second cyclic dependency
//problem for the remaining MOV IRs.
//
//The reorder algorithm should include two parts:
//Part 1. Process normal dependency problem.
//  we consider the USE count of destination registers, if the USE count
//  is zero, that means this MOV IR has no dependency with other MOV IRs, we
//  can reorder these MOV IRs at the beginning of the group of IRs. At the same
//  time the USE count of the source registers should be decremented by 1.
//  Algo steps:
//   Go through all the dst-physical-register by the index, assume 'r' is the
//   current index:
//     1. If the USE count of dst-physical-register is more than 1, try the
//        next dst-physical-register.
//     2. If the USE count of dst-physical-register is zero:
//        2.1. place the MOV IR which assigned to this dst-physical-register
//             in the new IR list.
//        2.2. Decrement the use count of src-physical-register by 1 placed in
//             the step 2.1.
//        2.3 If the idex of src-physical-register is less than 'r', and the
//            use count of src-physical-register is zero, and we update 'r'
//            to the index of src-physical-register, since the use count of
//            src-physical-register is zero, there is no MOV IRs in the group
//            are dependenct with it, so we have to go back to try to check
//            the condition in step 1 for the possible new chance for other
//            MOV IRs.
//  e.g.:
//  r1 <- mov r6                   r1 <- mov r6
//  r6 <- mov r3   after reorder   r5 <- mov r2
//  r5 <- mov r2   ------------->  r2 <- mov r7
//  r2 <- mov r7                   r6 <- mov r3
//  r7 <- mov r9                   r7 <- mov r9
//
//Part 2. Process cyclic dependency problem.
//  After the processing of part 1, we only have the MOVs IR with
//  "cycles" left, that means we have to resolve these permute MOV
//  instructions.
//  We can use the example mentioned above, there are three MOV IRs no handled
//  at this point:
//      r3 <- mov r5
//      r4 <- mov r3
//      r5 <- mov r4
//    Assume the original data in the registers before the moving operations
//    are as below:
//        r3 = 'a'
//        r4 = 'b'
//        r5 = 'c'
//    The expected final data in the registers after the moving operations
//    are as below:
//        r3 = 'c'
//        r4 = 'a'
//        r5 = 'b'
//
//  Algorithm for reordering a cyclic MOV group:
//    1. Use any MOV IR in the cyclic MOV group, and let's assume register 'r'
//       is occurened in the LHS, and 'r1' is  occurened in the RHS.
//    2. Place a copy IR first, which would copy any register 'r1' used in
//       the RHS of MOV IR to a temp location.
//    3. Put the MOV IR which defined the register 'r1' right after the
//       copy IR in step 2.
//    4. Get the regsiter 'r2' occurenced in the RHS of the MOV IR in step 2,
//       and place the MOV IR which defined the register 'r2' right after the
//       MOV IR inserted in step 3.
//    5. Repeat step 3 and step 4 until the 'r2' is equal to 'r'.
//    6. Place a copy IR at last, which  which would copy the data from the
//       temp location to the the register 'r'.
//
//    e.g.: For the three cyclic MOV IRs in the above example, there should
//          be 4 steps to complete the reorder operation:
//          Step 1: tmp <- mov r5 //tmp = 'c'
//          Step 2: r5  <- mov r4 //r5  = 'b'
//          Step 3: r4  <- mov r3 //r4  = 'a'
//          Step 4: r3  <- tmp    //r3  = 'c'
//    [Note]: tmp stands for the temp register reserved on some specific arch.
//            If there is no temp register available on the arch, the memory
//            slot on the stack need to be used to finish the swap operation.
class LatchBBReorder {
    COPY_CONSTRUCTOR(LatchBBReorder);

    //The max number of physical register.
    UINT m_max_reg_num;
    Region * m_rg;
    IRMgr * m_irmgr;

    //Map the the dst-physical-register to src-physical-register for the MOV
    //IRs. The index is the ID of dst-physcial-register, the responding value
    //is the src-physcial-register.
    //e.g.:
    //  $3(r1) <- mov $5(r3)
    //  $4(r5) <- mov $3(r1)
    //  The data of m_phyreg_dst2src will be:
    //  m_phyreg_dst2src[r1] = r3;
    //  m_phyreg_dst2src[r5] = r1;
    UINT * m_phyreg_dst2src;

    //Map the src-physical-register to it's using count in MOV IRs. The
    //index is the ID of src-physical-register.
    //e.g.:
    //  $3(r1) <- mov $5(r3)
    //  $4(r5) <- mov $3(r1)
    //  The data of m_srcreg_to_usecnt will be:
    //  m_srcreg_to_usecnt[r1] = 1;
    //  m_srcreg_to_usecnt[r3] = 1;
    //  m_srcreg_to_usecnt[r5] = 0;
    UINT * m_srcreg_to_usecnt;

    //Map the physical-register to the result of a MOV IR that has been
    //assigned to the physical-register, the index is the ID of
    //physical-resgiter.
    IR ** m_phyreg2defop;

    //The marker IR which indicates where the IR will be inserted in BB.
    IR * m_marker;

    //The Map of latch BB.
    LatchMap const& m_latch_map;
    LSRAImpl & m_impl;

    //Map the TMP register to a fixed prno.
    Reg2PRNO m_reg2prno;
public:
    LatchBBReorder(LatchMap const& latch_map, LSRAImpl & impl) :
        m_phyreg_dst2src(nullptr), m_srcreg_to_usecnt(nullptr),
        m_phyreg2defop(nullptr), m_marker(nullptr), m_latch_map(latch_map),
        m_impl(impl)
    {
        m_rg = m_impl.getRegion();
        m_irmgr = m_rg->getIRMgr();
        m_max_reg_num = m_impl.getRegSetImpl().getTotalRegNum();
        ASSERTN(m_max_reg_num <= MAX_PHY_REG_NUM,
            ("Physical register number too big"));
    }

    //Build a copy IR from prno to the temp location.
    IR * buildCopyFromPRtoTmp(PRNO prno, Type const* ty);

    //Build a copy IR from the temp location to prno.
    IR * buildCopyFromTmptoPR(PRNO prno, Type const* ty);

    //Implemented the Part 1 above for the normal MOV IRs in BB.
    //bb: the specified latch BB.
    void doNormalMoveReorder(MOD IRBB * bb);

    //Implemented the Part 2 above for the cyclic MOV groups.
    //bb: the specified latch BB.
    void doCyclicMoveReorder(MOD IRBB * bb);

    void dumpBBAfterReorder(IRBB const* bb) const;

    //Get a fixed temp prno per the input data type.
    PRNO getTmpPrno(Type const* ty);

    //This function will check the reorder for the MOV IRs is required or not
    //in the specified BB, and also collect some infomations used in the coming
    //reorder phase.
    //bb: the specified latch BB.
    //reg_use_cnt: the register use count for the registers involved in MOV IRs.
    //move_info: contains the source and destination registers for the MOV IRs.
    //def_irs: contains the original MOV IR responding to the defined register.
    //marker: the marker IR indicates where the adjusted IR should be inserted.
    //return value: true if it is needed to do the reorder; false means don't
    //              need to do reorder.
    //For example:
    //  The group of MOV IRs are in the order below:
    //    r10 <- MOV r8    #S1
    //    r7  <- MOV r10   #S2
    //
    //  Because r10 is used in #S2, but before it is used, it will be
    //  overwrote by r8 in #S1, so this two MOV IRs need to be reordered,
    //  The expected sequence should be:
    //    r7  <- MOV r10   #S2
    //    r10 <- MOV r8    #S1
    bool isBBReorderRequired(MOD IRBB * bb);

    //The entry function for the reorder.
    void perform();

    //This function will do the reorder for the MOV IRs in the latch BB.
    //bb: the specified latch BB.
    void reorderMoveIRForBB(MOD IRBB * bb);

    //This function will verify the reorder result.
    bool verifyReorderResult() const;
};


void LatchBBReorder::dumpBBAfterReorder(IRBB const* bb) const
{
    note(m_rg, "\n-- DUMP After Reorder MOV IRs for Latch BB%u in LSRA --",
         bb->id());
    m_rg->getLogMgr()->incIndent(2);
    bb->dump(m_rg, true);
    m_rg->getLogMgr()->decIndent(2);
}


bool LatchBBReorder::verifyReorderResult() const
{
    ASSERT0(m_phyreg_dst2src);
    for (Reg r = 0; r < m_max_reg_num; r++) {
        ASSERT0(m_phyreg_dst2src[r] == r);
    }
    return true;
}


bool LatchBBReorder::isBBReorderRequired(MOD IRBB * bb)
{
    ASSERT0(bb && m_srcreg_to_usecnt && m_phyreg_dst2src && m_phyreg2defop);
    BBIRList const& irlst = bb->getIRList();
    BBIRListIter bbirit;
    UINT move_cnt = 0;
    UINT const MIN_PRCOESS_NUM = 2;
    m_marker = nullptr;

    //Collect the informations first.
    for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
         ir = irlst.get_next(&bbirit)) {
        if (m_impl.getRA().isSpillOp(ir)) {
            m_marker = ir;
            continue;
        }
        //Since the MOV IRs are in the middle of latch BB, so we can exit the
        //loop when a reload IR or a remat IR is encountered.
        if (m_impl.getRA().isReloadOp(ir) || m_impl.getRA().isRematOp(ir))
        { break; }

        ASSERT0(m_impl.getRA().isMoveOp(ir));
        ASSERT0(ir->isWritePR());
        ASSERT0(ir->getRHS()->isReadPR());

        PRNO use_prno = ir->getRHS()->getPrno();
        PRNO def_prno = ir->getPrno();
        ASSERT0(use_prno != PRNO_UNDEF);
        ASSERT0(def_prno != PRNO_UNDEF);

        Reg use_reg = m_impl.getRA().getReg(use_prno);
        Reg def_reg = m_impl.getRA().getReg(def_prno);
        ASSERT0(use_reg != REG_UNDEF);
        ASSERT0(def_reg != REG_UNDEF);
        m_phyreg_dst2src[def_reg] = use_reg;
        m_phyreg2defop[def_reg] = ir;
        m_srcreg_to_usecnt[use_reg]++;
        move_cnt++;
    }

    //If the number of MOV IR is single or zero, just return false.
    if (move_cnt < MIN_PRCOESS_NUM) { return false; }

    //Check the USE count of the defined registers, if all the defined
    //registers are not used in these MOV IRs, then return false.
    for (UINT i = 0; i < m_max_reg_num; i++) {
        if (m_srcreg_to_usecnt[i] > 0) { return true; }
    }
    return false;
}


PRNO LatchBBReorder::getTmpPrno(Type const* ty)
{
    ASSERT0(ty);
    if (!m_impl.getRA().isTmpRegAvailable(ty)) { return PRNO_UNDEF; }
    Reg tmp_rg = m_impl.getRA().getTempReg(ty);
    bool find;
    PRNO pr = m_reg2prno.get(tmp_rg, &find);
    if (find) { return pr; }
    pr = m_impl.getRA().buildPrnoAndSetReg(ty, tmp_rg);
    m_reg2prno.set(tmp_rg, pr);
    return pr;
}


IR * LatchBBReorder::buildCopyFromPRtoTmp(PRNO prno, Type const* ty)
{
    ASSERT0(ty);
    ASSERT0(prno != PRNO_UNDEF);
    PRNO tmpprno = getTmpPrno(ty);
    IR * copy = nullptr;
    if (tmpprno != PRNO_UNDEF) {
        copy = m_irmgr->buildMove(tmpprno, prno, ty);
        m_impl.getRA().setMove(copy);
        return copy;
    }
    Var * spill_loc = m_impl.getRA().getTempVar(ty);
    copy = m_impl.getRA().buildSpillByLoc(prno, spill_loc, ty);
    m_impl.getRA().setSpill(copy);
    return copy;
}


IR * LatchBBReorder::buildCopyFromTmptoPR(PRNO prno, Type const* ty)
{
    ASSERT0(ty);
    ASSERT0(prno != PRNO_UNDEF);
    PRNO tmpprno = getTmpPrno(ty);
    IR * copy = nullptr;
    if (tmpprno != PRNO_UNDEF) {
        copy = m_irmgr->buildMove(prno, tmpprno, ty);
        m_impl.getRA().setMove(copy);
        return copy;
    }
    Var * spill_loc = m_impl.getRA().getTempVar(ty);
    copy = m_impl.getRA().buildReload(prno, spill_loc, ty);
    m_impl.getRA().setReload(copy);
    return copy;
}


void LatchBBReorder::doNormalMoveReorder(MOD IRBB * bb)
{
    ASSERT0(bb && m_srcreg_to_usecnt && m_phyreg_dst2src && m_phyreg2defop);

    //Implemented to Part 1 above.
    for (Reg r = 0; r < m_max_reg_num;) {
        UINT old_src = m_phyreg_dst2src[r];
        if (old_src == r || m_srcreg_to_usecnt[r] > 0) {
            //Since the data is already in the dst-pthysial-register, or it is
            //used by other MOV IR, so we cannot put the MOV IR right now.
            r++;
            continue;
        }
        ASSERT0(m_phyreg2defop[r]);

        //Remove the ir first.
        bb->getIRList().remove(m_phyreg2defop[r]);
        if (m_marker == nullptr) {
            bb->getIRList().append_head(m_phyreg2defop[r]);
        } else {
            bb->getIRList().insert_after(m_phyreg2defop[r], m_marker);
        }
        m_marker = m_phyreg2defop[r];

        //Se the flag that dst-pthysial-register has the correct data.
        m_phyreg_dst2src[r] = r;
        ASSERT0(m_srcreg_to_usecnt[old_src] > 0);

        //Decrement the use count of src-pthysial-register.
        m_srcreg_to_usecnt[old_src]--;

        //If the idex of src-physical-register is less than 'r', and the
        //use count of src-physical-register is zero, and we update 'r'
        //to the index of src-physical-register, since the use count of
        //src-physical-register is zero, there is no MOV IRs in the group
        //are dependenct with it, so we have to go back to try to check
        //the condition in step 1 for the possible new chance for other
        //MOV IRs.
        if (old_src < r && m_srcreg_to_usecnt[old_src] == 0) {
            //CASE: When iter physical-register 'r2', the m_phyreg2defop['r2']
            //      is #S2, but the m_srcreg_to_usecnt['r2'] > 0, so this
            //      #S2 is ignored. But when iter the physical-register 'r5',
            //      #S3 will be ordered after #S1, and m_srcreg_to_usecnt['r2']
            //      is updated to 0, and at this time, the 'r' is updated to
            //      old_src 'r2', so we will have a chance to iter
            //      physical-register 'r2' again to order m_phyreg2defop['r2]
            //      #S2.
            //      r1 <- mov r6  #S1                  r1 <- mov r6  #S1
            //      r2 <- mov r7  #S2  after reorder   r5 <- mov r2  #S3
            //      r5 <- mov r2  #S3  ------------->  r2 <- mov r7  #S2
            //      r7 <- mov r9  #S4                  r7 <- mov r9  #S4

            r = old_src;
            continue;
        }
        r++;
    }
}


void LatchBBReorder::doCyclicMoveReorder(MOD IRBB * bb)
{
    ASSERT0(bb && m_srcreg_to_usecnt && m_phyreg_dst2src && m_phyreg2defop);
    bool is_new_group = true;
    for (Reg r = 0; r < m_max_reg_num;) {
        UINT r1 = m_phyreg_dst2src[r];
        if (r1 == r) { ++r; continue; }
        ASSERT0(m_srcreg_to_usecnt[r1] == 1);

        //Exchange 'r1' and 'r2', after that 'r1' is a fixed point, which means
        //'r1' hold the correct final data.
        Reg r2 = m_phyreg_dst2src[r1];
        ASSERT0(m_phyreg2defop[r1]);
        ASSERT0(m_phyreg2defop[r2]);
        ASSERT0(m_impl.getRA().isMoveOp(m_phyreg2defop[r1]));
        ASSERT0(m_impl.getRA().isMoveOp(m_phyreg2defop[r2]));
        ASSERT0(m_impl.getRA().isMoveOp(m_phyreg2defop[r]));

        if (is_new_group) {
            //Process the start of new group, we have to move the data of 'r1'
            //into temp location.
            PRNO src_prno_with_r1 = m_phyreg2defop[r]->getRHS()->getPrno();
            Type const* ty = m_phyreg2defop[r]->getType();

            //Build the new copy IR, copy data from the register 'r1' to the
            //temp location, insert after the marker, and update the marker
            IR * copy = buildCopyFromPRtoTmp(src_prno_with_r1, ty);
            if (m_marker != nullptr) {
                bb->getIRList().insert_after(copy, m_marker);
            } else {
                bb->getIRList().append_head(copy);
            }
            m_marker = copy;
            is_new_group = false;
        }
        //Insert the m_phyreg2defop[r1] after the marker
        ASSERT0(m_marker && m_marker->is_stmt());
        bb->getIRList().remove(m_phyreg2defop[r1]);
        bb->getIRList().insert_after(m_phyreg2defop[r1], m_marker);
        m_marker = m_phyreg2defop[r1];

        //Process the last IR in the cyclic group.
        if (r == r2) {
            Type const* ty = m_phyreg2defop[r]->getType();
            PRNO dst_prno_with_r = m_phyreg2defop[r]->getPrno();

            //Build the copy IR, copy date form the temp location to the
            //register r, and update the marker.
            IR * copy = buildCopyFromTmptoPR(dst_prno_with_r, ty);
            bb->getIRList().insert_after(copy, m_marker);
            m_marker = copy;

            //Remove the def IR of 'r'.
            bb->getIRList().remove(m_phyreg2defop[r]);
            m_impl.getRA().removeMoveOp(m_phyreg2defop[r]);

            //Since the current group has been processed, so set the flag to
            //indicate a new group will be started.
            is_new_group = true;
        }

        //Value of register 'r2' is now in the correct register 'r1'.
        m_phyreg_dst2src[r1] = r1;

        //The source of 'r' changed to 'r2'.
        m_phyreg_dst2src[r] = r2;
    }
}


void LatchBBReorder::reorderMoveIRForBB(MOD IRBB * bb)
{
    ASSERT0(bb && m_srcreg_to_usecnt && m_phyreg_dst2src && m_phyreg2defop);

    //Do the reorder for normal MOV IRs.
    doNormalMoveReorder(bb);

    //Do the reorder MOV IRs with cyclic.
    doCyclicMoveReorder(bb);

    //When comes here, all MOV IRs need to be handled, there must be some
    //exception if there is any move unprocessed.
    ASSERT0(verifyReorderResult());

    if (g_dump_opt.isDumpLSRAReorderMovInLatchBB()) {
        dumpBBAfterReorder(bb);
    }
}


void LatchBBReorder::perform()
{
    UINT reg_array_size = m_max_reg_num * sizeof(UINT);
    UINT ir_array_size = m_max_reg_num * sizeof(IR*);

    m_srcreg_to_usecnt = (UINT*)ALLOCA(reg_array_size);
    m_phyreg_dst2src = (UINT*)ALLOCA(reg_array_size);
    m_phyreg2defop = (IR**)ALLOCA(ir_array_size);
    LatchMapIter it;
    IRBB * bb = nullptr;
    for (m_latch_map.get_first(it, &bb); !it.end();
         m_latch_map.get_next(it, &bb)) {
        //Init the data structures.
        ::memset(m_srcreg_to_usecnt, 0,  reg_array_size);
        ::memset(m_phyreg2defop, 0, ir_array_size);
        for (UINT i = 0; i < m_max_reg_num; i++) { m_phyreg_dst2src[i] = i; }

        //Check the reorder is necessary to be performed or not.
        if (!isBBReorderRequired(bb)) { continue; }

        //Do the reorder for the MOV IRs in the latch BB.
        reorderMoveIRForBB(bb);
    }
}
//END LatchBBReorder


//
//START LTConsistencyMgr
//
LTConsistencyMgr::LTConsistencyMgr(LSRAImpl & impl) : m_impl(impl)
{
    m_rg = impl.getRegion();
    m_bb_list = impl.getBBList();
    m_cfg = impl.getCFG();
    m_oc = impl.getOptCtx();
    m_is_insert_bb = false;
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->
        queryPass(PASS_PRLIVENESS_MGR);
    ASSERT0(m_live_mgr && m_live_mgr->is_valid());
    UINT const mem_pool_init_size = 64;
    m_pool = smpoolCreate(mem_pool_init_size, MEM_COMM);
    ASSERT0(m_pool);
}


void LTConsistencyMgr::dump() const
{
    Region const* rg = m_rg;
    BBList const* bblst = m_bb_list;
    note(rg, "\n==-- DUMP %s --==", "LTConsistencyMgr");
    UINT ind = 2;
    rg->getLogMgr()->incIndent(ind);
    BBListIter cb;
    for (IRBB * bb = bblst->get_head(&cb); bb != nullptr;
         bb = bblst->get_next(&cb)) {
        UINT bbid = bb->id();
        note(rg, "\n\n-- BB%d --", bbid);
        PR2LT const* intab = getInPR2Lt(bbid);
        PR2LTIter it;
        LifeTime const* lt = nullptr;
        note(rg, "\nPR2Lt-IN:");
        if (intab != nullptr) {
            rg->getLogMgr()->incIndent(ind);
            for (PRNO prno = intab->get_first(it, &lt);
                 prno != PRNO_UNDEF; prno = intab->get_next(it, &lt)) {
                if (lt == nullptr) {
                    note(rg, "\n$%u: null", prno);
                    continue;
                }
                note(rg, "\n$%u:%u", prno, lt->getPrno());
            }
            rg->getLogMgr()->decIndent(ind);
        }

        PR2LT const* outtab = getInPR2Lt(bbid);
        note(rg, "\nPR2Lt-OUT:");
        if (outtab != nullptr) {
            rg->getLogMgr()->incIndent(ind);
            for (PRNO prno = outtab->get_first(it, &lt);
                 prno != PRNO_UNDEF; prno = outtab->get_next(it, &lt)) {
                if (lt == nullptr) {
                    note(rg, "\n$%u: null", prno);
                    continue;
                }
                note(rg, "\n$%u:%u", prno, lt->getPrno());
            }
            rg->getLogMgr()->decIndent(ind);
        }
    }
    rg->getLogMgr()->decIndent(ind);
}


LifeTime const* LTConsistencyMgr::getLifetimeChild(LTList const& lt_list,
                                                   Pos pos)
{
    LTListIter it;
    for (LifeTime const* lt = lt_list.get_head(&it);
         lt != nullptr; lt = lt_list.get_next(&it)) {
        if (lt->is_cover(pos)) { return lt; }
    }
    return nullptr;
}


Occ LTConsistencyMgr::getFirstRealOccOfFakeUseAtLexFirstBBInLoop(LifeTime * lt)
{
    //Since the first occ of lt is the fake-use IR, the first real occ of
    //this lifetime is the occ right after the fake-use occ in Lexicographical
    //order.
    //e.g:
    // lifetime $1: <5-17><34-67>
    //    |     ------------                ----------------------------------
    //    |     u          u                d  u               u             u
    // POS:     5          17               34 37  41          53            67
    //          ^          ^
    //          |          |
    //  fake-use occ first real-use occ
    ASSERT0(lt);
    LTList const& child_list = lt->getChild();
    OccListIter it = nullptr;
    OccList & ol = lt->getOccList();
    Occ real_occ = ol.get_head(&it);

    //If the occ list has more than 1 occ, the second occ must be the real occ.
    if (ol.get_elem_count() > 1) {
        ASSERT0(it != ol.end());
        real_occ = ol.get_next(&it);
        ASSERT0(real_occ.pos() != POS_UNDEF && real_occ.getIR() != nullptr);
        return real_occ;
    }

    //Get the real occ from the split child.
    LifeTime * first_child = const_cast<LTList&>(child_list).get_head();
    ASSERT0(first_child);
    it = nullptr;
    real_occ = first_child->getOccList().get_head(&it);
    ASSERT0(real_occ.pos() != POS_UNDEF && real_occ.getIR() != nullptr);
    return real_occ;
}


//This function selects the proper lifetime inserted with fake-use IR at
//the first BB of loop in lexicographical order when the input position is
//between the fake-use IR and the real-use IR. If the ancestor lifetime
//is split at the fake-use IR, the data of this lifetime is in the memory,
//or else it is in the register.
//
//E.g.1:
//   $1 is split into $1 and $5 at the fake-use IR.
//      BB1          -----             BB1            -----
//   [x]<-spill $1     ^            [x]<-spill $1       ^
//      <-fake-use $1  |               <-fake-use $1  IN MEM
//      |              |               ...              V
//      V            IN MEM          $5<-reload [x]   -----
//      BB2            |               <-real-use $5
//      ...            V
//    $5<-reload [x] -----
//      <-real-use $5
//           (a)                           (b)
//  Note: 1. (a) shows the fake-use and real use are not in the the same BB.
//        2. (b) shows the fake-use and real use are in the the same BB.
//
//E.g.2:
//   $1 is not split at the fake-use IR.
//      BB1          -----             BB1            -----
//      <-fake-use $1  ^               <-fake-use $1  IN REG
//      |              |               ...            -----
//      V            IN REG            <-real-use $1
//      BB2            |
//      ...            V
//      <-real-use $1
//          (a)                            (b)
//  Note: 1. (a) shows the fake-use and real use are not in the the same BB.
//        2. (b) shows the fake-use and real use are in the the same BB.
bool LTConsistencyMgr::selectLTAtPosForFakeUseAtLexFirstBBOfLoop(
    LifeTime * anct, Pos pos, OUT LifeTime const*& lt)
{
    ASSERT0(anct);
    ASSERT0(pos != POS_UNDEF);
    Occ fake_occ = anct->getFirstOcc();
    Occ real_occ = getFirstRealOccOfFakeUseAtLexFirstBBInLoop(anct);
    ASSERT0(fake_occ.pos() < real_occ.pos());
    ASSERT0(pos < real_occ.pos());
    ASSERT0(pos >= anct->getFirstPos());

    if (isLTSplitBetweenLeadingFakeUseAndRealUse(anct)) {
        //If the anct prno is spilt due to the fake-use IR, we can know this
        //through the number of occ in anct lifetime when it has only one occ
        //in its occ list (the other occs are moved to the child lifetimes),
        //the content of the anct lifetime should be in the memory, and the
        //related spill IR will be removed afterwards in function
        //LTConsistencyMgr::removeSpillIRForFakeUseAtLexFirstBBOfLoop().
        LinearScanRA & lsra = m_impl.getRA();
        PRNO anct_prno = anct->getPrno();
        ASSERT0(anct_prno != PRNO_UNDEF);
        lsra.genSpillLoc(anct_prno, lsra.getSpillType(anct_prno));
        return false;
    }

    //if the anct prno is not spilled, that means the content of this anct
    //lifetime can be kept in the registers, so the anct will be selected.
    lt = anct;
    return true;
}


bool LTConsistencyMgr::selectLifetimeAtPos(LifeTime * anct, Pos pos,
                                           OUT LifeTime const*& lt)
{
    ASSERT0(anct);
    ASSERT0(pos != POS_UNDEF);
    LTList const& lt_list = anct->getChild();

    //This function is used to select a responding lifetime at various position,
    //because there are multiple child lifetimes are generated during split.
    //
    //e.g:
    // Original lifetime $1: <5-17><34-37>
    //    |     ------------                ----
    //    |     u          u                d  u
    // POS: 2   5          17               34 37

    //Lifetimes after split:
    // Anct Lifetime $1: <5-17>
    //    |     ------------
    //    |     u          u
    // POS: 2   5          17
    //
    // Child Lifetime $10: <34-37>
    //    |                                 ----
    //    |                                 d  u
    // POS:                                 34 37
    //
    //For the above lifetimes generated during split, there are some standards
    //used to slelect a proper lifetime:
    //  1. If the pos is between [0, 5), anct will be selected.
    //
    //  2. If the anct lifetime is used in the fake-use IR, and the pos is
    //     less then first real occ of the lifetime, then refer to the select
    //     logic in function selectLTAtPosForFakeUseAtLexFirstBBOfLoop.
    //
    //  3. If the spill memory is used:
    //     3.1 If the pos is less then 17, anct will be selected.
    //     3.2 If the pos is between [34, 37], child will be selected.
    //     3.3 If the pos is between (17, 34), return false.
    //     3.4 If the pos > 34:
    //         3.4.1 If the last child is spilled only, return false.
    //         3.4.2 If the last child is not spilled only, last child
    //               will be selected.
    //
    //  4. If the spill memory is not used:
    //     4.1 If the pos is less then 17, anct will be selected.
    //     4.2 If the pos is between [34, 37], child will be selected.
    //     4.3 If the pos is between (17, 34), that means the position is
    //         located in the hole of lifetime:
    //         4.3.1 If it is rematerialized, return false directly to
    //               represent it is rematerialized althrough it is not in
    //               memory.
    //         4.3.2 If the spill memory can be avoided, the parent lifetime
    //               will be selected.
    //         4.3.3 If the spill memory cannot be avoided, that means the
    //               spill memory needs to be generated to solve the
    //               inconsistency, return false.
    //     4.4 If the pos > 34, child will be used.

    //Since anct indicates the original lifetime, if the pos is before the
    //start position of this lifetime, anct should be selected as the correct
    //lifetime.
    if (pos < anct->getFirstPos()) {
        //Implemented 1 above.
        lt = anct;
        return true;
    }

    if (m_impl.getRA().isLTWithFakeUseAtLexFirstBBInLoop(anct)) {
         Occ real_occ = getFirstRealOccOfFakeUseAtLexFirstBBInLoop(anct);
         if (pos < real_occ.pos()) {
             //Implemented 2 above.
             return selectLTAtPosForFakeUseAtLexFirstBBOfLoop(anct, pos, lt);
         }
    }

    //Since anct indicates the original lifetime, if the pos is before the
    //end position of this lifetime, anct should be selected as the correct
    //lifetime.
    if (pos <= anct->getLastPos()) {
        //Implemented 3.1 and 4.1 above.
        lt = anct;
        return true;
    }

    //Since child_last indicates the last child generated during split,
    //if the pos is after the end position of this lifetime, child_last
    //should be selected as the correct lifetime unless it is spilled ony.
    LifeTime * child_last = lt_list.get_elem_count() == 0 ?
        anct : const_cast<LTList&>(lt_list).get_tail();
    if (pos > child_last->getLastPos()) {
        //Implemented 3.4 and 4.4 above.
        if (child_last->isSpillOnly()) { return false; }
        lt = child_last;
        return true;
    }

    //Traverse the child list to find the correct lifetime.
    LTListIter it = nullptr;
    LinearScanRA & lsra = m_impl.getRA();
    PRNO anct_prno = anct->getPrno();
    Var const* spill_loc = lsra.getSpillLoc(anct_prno);
    bool is_remat = anct->isRematerialized();
    for (LifeTime * child = lt_list.get_head(&it);
         child != nullptr; child = lt_list.get_next(&it)) {
        //Since the child lifetimes are generated by the split of ancestor
        //lifetime, and there is no overlap between any two child lifetimes, so
        //for a specified postion, if it is covered by a child lifetime, which
        //indicates that the child lifetime is used at the position.
        if (child->is_cover(pos)) {
            //Implemented 3.2 and 4.2 above.
            lt = child;
            return true;
        }

        if (spill_loc == nullptr && pos < child->getFirstPos()) {
            if (is_remat) {
                //Implemented 4.3.1 above.
                //If the lifime is rematerialized, return false directly.
                //Since this lifetime is rematerialized, the value of the
                //the lifetime cannot responding to any descendant lifetime,
                //so we use the return value 'false' to represent it is
                //rematerialized althrough it is not in memory.
                return false;
            }

            //Implemented 4.3.3 above.
            //CASE: The spill memory of 'anct' is not created so far, becasue
            //the split position of 'anct' is at an exit BB in a branch of
            //CFG, and the spill memory is not created after the calling of
            //isUsedBySuccessors (due to no successors of exit BB), but the
            //spill memory still be required since there is a backward-edge
            //jumping from the BB after the split position to the exit BB, so
            //the data of the lifetime must be spill into the memory, and then
            //reloaded from memory to register before going to the exit BB.
            //
            //e.g: $1 has been split to $1 and $10. BB4 is in the lifetime
            //     hole of $1, and $1 has been assigned to r1, and $10 has
            //     assigned to r4, and $2 has been assgined to r1 in BB4.
            //     When the BB7 jumps back to BB3 through BB4, the real value
            //     of $1 is stored in r4 in BB7, but the value is used from r1
            //     in BB3 so this is incorrect. The value of $1 in BB3 should
            //     be reloaded from memory before going into BB3, and spilled
            //     into memory before jumping to BB4 from BB7, the spill and
            //     reload operation will be inserted if the data of $1 is in
            //     memory for BB4 wherein the lifetime hole.
            //            --BB2
            //           |  |
            //           |  V
            //           |  BB3<--------
            //           |  x<-$1(r1)   |
            //           |  ret      $1<-[mem]
            //           |              |
            //           |              |  -
            //            --            |  |
            //              |           |  |lifetime hole of $1
            //              V           |  |
            //     -------->BB4         |  |
            //    |         $2(r1)<-y   |  |
            //    |         truebr BB3--   |
            //    |         |              |
            //    |         V              |
            //    |         BB7            -
            //    |         $10(r4)<-z
            // [mem]<-$10   goto BB4
            //    |         |
            //     ---------
            lsra.genSpillLoc(anct_prno, lsra.getSpillType(anct_prno));
            return false;
        }
    }
    //Implemented 3.3 above.
    return false;
}


void LTConsistencyMgr::computePR2LtInfoForBB(UINT bbid, bool is_input)
{
    ASSERT0(bbid != BBID_UNDEF);
    Pos boundary_pos = POS_UNDEF;
    PRLiveSet const* live_set = nullptr;
    PRLiveSetIter * iter = nullptr;
    if (is_input) {
        boundary_pos = m_impl.getLTMgr().getBBStartPos(bbid);
        live_set = m_live_mgr->get_livein(bbid);
    } else {
        boundary_pos = m_impl.getLTMgr().getBBEndPos(bbid);
        live_set = m_live_mgr->get_liveout(bbid);
    }
    ASSERT0(boundary_pos != POS_UNDEF);
    ASSERT0(live_set);
    for (PRNO pr = (PRNO)live_set->get_first(&iter);
         pr != BS_UNDEF; pr = (PRNO)live_set->get_next(pr, &iter)) {
        LifeTime * lt = m_impl.getRA().getLTMgr().getLifeTime(pr);
        if (canIngoreInconsistCheck(lt)) { continue; }
        LifeTime const* child = nullptr;
        if (!selectLifetimeAtPos(lt, boundary_pos, child)) { continue; }
        ASSERT0(child);
        if (is_input) { addInPR2Lt(pr, child, bbid); continue; }
        addOutPR2Lt(pr, child, bbid);
    }
}


void LTConsistencyMgr::computePR2LtInfo()
{
    BBListIter bbit;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
        bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        computePR2LtInfoForBB(bb->id(), true);
        computePR2LtInfoForBB(bb->id(), false);
    }
}


void LTConsistencyMgr::computeEdgeConsistency(
    OUT InConsistPairList & inconsist_lst)
{
    xcom::EdgeIter it;
    for (xcom::Edge * e = m_cfg->get_first_edge(it); e != nullptr;
         e = m_cfg->get_next_edge(it)) {
        computeEdgeConsistencyImpl(e, inconsist_lst);
    }
}


void LTConsistencyMgr::computeInconsistency(
    OUT InConsistPairList & inconsist_lst, UINT from, UINT to,
    LifeTime const* from_lt, LifeTime const* to_lt, Var const* spill_loc)
{
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    if (from_lt != nullptr && to_lt != nullptr) {
        //If the from_lt and to_lt both exist, consider to revise the type
        //INCONSIST_PR2PR.
        InConsistPair * pair = allocInconsistPair();
        genInconsistPairPR2PR(pair, from, to, from_lt, to_lt);
        inconsist_lst.append_tail(pair);
        addInconsitVexpairTab(VexPair(from, to));
        return;
    }

    bool is_remat = to_lt != nullptr && to_lt->isRematerialized();

    //If there is no spill memory used during the split or or it is
    //rematerialized, return directly.
    if (spill_loc == nullptr && !is_remat) { return; }

    //If the spill memory is used during the split, then consider
    //the inconsistency type for INCONSIST_MEM2PR or INCONSIST_PR2MEM.
    if (from_lt == nullptr) {
        //Process the remat case.
        if (is_remat) {
            ASSERT0(to_lt->canBeRemat());
            InConsistPair * pair = allocInconsistPair();
            genInconsistPairRemat(pair, from, to, to_lt->getRematExp(), to_lt);
            inconsist_lst.append_tail(pair);
            addInconsitVexpairTab(VexPair(from, to));
            return;
        }
        //If the 'from' lifetime exists and 'to' lifetime does not
        //exist, then should do the revise type INCONSIST_MEM2PR.
        InConsistPair * pair = allocInconsistPair();
        genInconsistPairMEM2PR(pair, from, to, to_lt, spill_loc);
        inconsist_lst.append_tail(pair);
        addInconsitVexpairTab(VexPair(from, to));
        return;
    }
    if (to_lt == nullptr) {
        //If the 'from' lifetime does not exist and 'from' lifetime
        //exists, then should do the revise type INCONSIST_PR2MEM.
        InConsistPair * pair = allocInconsistPair();
        genInconsistPairPR2MEM(pair, from, to, from_lt, spill_loc);
        inconsist_lst.append_tail(pair);
        addInconsitVexpairTab(VexPair(from, to));
    }
}


void LTConsistencyMgr::computeEdgeConsistencyImpl(xcom::Edge const* e,
    OUT InConsistPairList & inconsist_lst)
{
    ASSERT0(e);
    xcom::VexIdx from = e->from()->id();
    xcom::VexIdx to = e->to()->id();
    PRLiveSet const* live_in_to = m_live_mgr->get_livein(to);
    PRLiveSet const* live_out_from = m_live_mgr->get_liveout(from);
    ASSERT0(live_in_to && live_out_from);

    PRLiveSetIter * iter = nullptr;
    for (PRNO pr = (PRNO)live_in_to->get_first(&iter);
         pr != BS_UNDEF; pr = (PRNO)live_in_to->get_next(pr, &iter)) {
        if (!live_out_from->is_contain(pr)) { continue; }

        LifeTime * lt = getRA().getLTMgr().getLifeTime(pr);
        ASSERT0(lt);

        //If this lifetime is never defined or can ignore the inconsist check,
        //don't consider the inconsistency.
        if (!lt->isOccHasDef() || canIngoreInconsistCheck(lt)) { continue; }

        LifeTime const* from_lt = getOutLt(pr, from);
        LifeTime const* to_lt = getInLt(pr, to);
        if (from_lt == to_lt) { continue; }

        //Compute the inconsistency between from_lt and to_lt on the current
        //edge.
        Var const* spill_loc = getRA().getSpillLoc(lt->getPrno());
        computeInconsistency(inconsist_lst, from, to, from_lt, to_lt,
                             spill_loc);
    }
}


LinearScanRA & LTConsistencyMgr::getRA() const
{
    return m_impl.getRA();
}


void LTConsistencyMgr::tryUseTrampBBAsLatchBB(
    IRBB const* tramp, MOD LatchMap & latch_map)
{
    ASSERT0(tramp);
    Vertex const* tramp_v = tramp->getVex();
    ASSERT0(tramp_v->getInDegree() == 1);
    ASSERT0(tramp_v->getOutDegree() == 1);

    AdjVertexIter ito;
    Vertex const* in = Graph::get_first_in_vertex(tramp_v, ito);
    Vertex const* out = Graph::get_first_out_vertex(tramp_v, ito);
    VexPair pair(in->id(), out->id());
    if (!isLatchBBRequired(pair)) { return; }
    latch_map.set(pair, const_cast<IRBB*>(tramp));
}


IRBB * LTConsistencyMgr::insertLatch(IRBB const* from, MOD IRBB * to,
                                     MOD LatchMap & latch_map)
{
    IRBB * newbb = m_rg->allocBB();
    BBListIter fromit;
    BBListIter toit;
    m_bb_list->find(from, &fromit);
    m_bb_list->find(to, &toit);
    ASSERT0(fromit && toit);

    if (from->rpo() < to->rpo()) {
        //The newbb_prior_marker is set to true means the newbb is prior
        //to the marker BB 'to' in lexicographical order.
        //e.g:
        //  CFG:
        //    ... --> from --> newbb --> to(marker) -> ...
        //
        //  Lexicographical order:
        //    ... --> from --> newbb --> to(marker) -> ...
        //
        m_impl.tryUpdateRPO(newbb, to, true);
    } else {
        //The newbb_prior_marker is set to false means the newbb is behind
        //the marker BB 'from' in lexicographical order.
        //e.g:
        //  CFG:
        //    ... -> to --> ... --> from(marker) -> ...
        //           ^                |
        //           |                |
        //           '---- newbb <----'
        //
        //  Lexicographical order:
        //    ... -> to --> ... --> from(marker) -> newbb -> ...
        //
        m_impl.tryUpdateRPO(newbb, from, false);
    }

    //Insert newbb that must be fallthrough BB prior to occbb.
    IRBB * tramp = m_cfg->insertBBBetween(from, fromit, to, toit, newbb, m_oc);

    if (tramp != nullptr) {
        //Try to use the tramp BB as a latch BB, which would save the
        //cost to insert latch BB if it is really required between the
        //the precedessor BB and the successor BB of the tramp BB.
        tryUseTrampBBAsLatchBB(tramp, latch_map);
    }

    m_impl.tryUpdateDom(from, newbb, to);
    m_impl.addLivenessForEmptyLatchBB(newbb, from);
    dumpInsertBB(m_impl, from, to, newbb, "fix lifetime consistency");
    m_is_insert_bb = true;
    return newbb;
}


IRBB * LTConsistencyMgr::genLatchBB(MOD LatchMap & latch_map,
                                    InConsistPair const* pair)
{
    ASSERT0(pair);
    VexPair vp;
    vp.fromid = pair->from_vex_id;
    vp.toid = pair->to_vex_id;
    IRBB * latch = latch_map.get(vp);
    if (latch == nullptr) {
        IRBB * frombb = m_cfg->getBB(pair->from_vex_id);
        IRBB * tobb = m_cfg->getBB(pair->to_vex_id);
        latch = insertLatch(frombb, tobb, latch_map);
        latch_map.set(vp, latch);
    }
    return latch;
}


void LTConsistencyMgr::reviseTypePR2PR(MOD LatchMap & latch_map,
                                       InConsistPair const* pair)
{
    ASSERT0(pair);
    IRBB * latch = genLatchBB(latch_map, pair);
    Type const* fromty = pair->from_lt->getFirstOccType();
    Type const* toty = pair->to_lt->getFirstOccType();
    ASSERT0(fromty && toty);
    IR * mv = m_impl.insertMove(pair->from_lt->getPrno(),
        pair->to_lt->getPrno(), fromty, toty, latch);
    dumpInsertMove(m_impl, latch, mv,
                   "fix lifetime consistency pr to pr $%u->$%u",
                   pair->from_lt->getPrno(), pair->to_lt->getPrno());
}


void LTConsistencyMgr::reviseTypeMEM2PR(MOD LatchMap & latch_map,
                                        InConsistPair const* pair)
{
    ASSERT0(pair);
    ASSERT0(pair->mem_var && pair->mem_var->getType());
    IRBB * latch = genLatchBB(latch_map, pair);
    IR * reload = m_impl.insertReload(
        pair->to_lt->getPrno(), const_cast<Var*>(pair->mem_var),
        pair->mem_var->getType(), latch);
    Reg r = m_impl.getRA().getReg(pair->to_lt->getPrno());
    ASSERT0(r != REG_UNDEF);
    dumpReload(m_impl, reload, r, latch,
               "fix lifetime consistency memory to pr %s->$%u",
               pair->mem_var->get_name()->getStr(), pair->to_lt->getPrno());
}


void LTConsistencyMgr::reviseTypeRemat(MOD LatchMap & latch_map,
                                       InConsistPair const* pair)
{
    ASSERT0(pair && pair->to_lt);
    IRBB * latch = genLatchBB(latch_map, pair);
    Type const* toty = pair->remat_exp->getType();
    IR * remat = m_impl.insertRemat(pair->to_lt->getPrno(),
                                    pair->remat_exp, toty, latch);
    Reg r = m_impl.getRA().getReg(pair->to_lt->getPrno());
    ASSERT0(r != REG_UNDEF);
    dumpRemat(m_impl, remat, r, latch,
              "fix lifetime consistency remat exp to pr %u->$%u",
              pair->remat_exp->id(), pair->to_lt->getPrno());
}


void LTConsistencyMgr::reviseTypePR2MEM(MOD LatchMap & latch_map,
                                        InConsistPair const* pair)
{
    ASSERT0(pair);
    ASSERT0(pair->mem_var && pair->mem_var->getType());
    IRBB * latch = genLatchBB(latch_map, pair);
    IR * spill = m_impl.insertSpillAtBBEnd(
        pair->from_lt->getPrno(), const_cast<Var*>(pair->mem_var),
        pair->mem_var->getType(), latch);
    Reg r = m_impl.getRA().getReg(pair->from_lt->getPrno());
    ASSERT0(r != REG_UNDEF);
    dumpSpill(m_impl, spill, r, latch,
              "fix lifetime consistency pr to memory $%u->%s",
              pair->from_lt->getPrno(), pair->mem_var->get_name()->getStr());
    SplitCtx ctx(POS_UNDEF);
    m_impl.getSpillReloadEliminateMgr().recordSpill(
        pair->from_lt, spill, ctx, false);
}


bool LTConsistencyMgr::verifyLatchBB(IRBB * bb, MOD Vector<BYTE> & use_reg_cnt,
                                     MOD Vector<BYTE> & def_reg_cnt) const
{
    ASSERT0(bb);
    BBIRList const& irlst = bb->getIRList();
    BBIRListIter bbirit;

    for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
         ir = irlst.get_next(&bbirit)) {
        if (m_impl.getRA().isSpillOp(ir)) {
            PRNO use_prno = ir->getRHS()->getPrno();
            ASSERT0(use_prno != PRNO_UNDEF);
            Reg use_reg = m_impl.getRA().getReg(use_prno);
            ASSERT0(use_reg != REG_UNDEF);
            use_reg_cnt[use_reg]++;
            if (use_reg_cnt[use_reg] > 1) {
                ASSERTN(0, ("The count of use reg is more than 1"));
            }
            continue;
        }
        //Ignore the the IR if it is a jump.
        if (ir->is_goto()) { continue; }
        ASSERT0(m_impl.getRA().isReloadOp(ir) ||
                m_impl.getRA().isRematOp(ir) ||
                m_impl.getRA().isMoveOp(ir));

        ASSERT0(ir->isWritePR());

        PRNO def_prno = ir->getPrno();
        ASSERT0(def_prno != PRNO_UNDEF);

        Reg def_reg = m_impl.getRA().getReg(def_prno);
        ASSERT0(def_reg != REG_UNDEF);
        def_reg_cnt[def_reg]++;
        if (def_reg_cnt[def_reg] > 1) {
            ASSERTN(0, ("The count of def reg is more than 1"));
        }
    }
    return true;
}


bool LTConsistencyMgr::verifyLatchBBs(LatchMap const& latch_map) const
{
    UINT const max_reg_num = m_impl.getRegSetImpl().getTotalRegNum();
    Vector<BYTE> use_reg_cnt(max_reg_num);
    Vector<BYTE> def_reg_cnt(max_reg_num);
    LatchMapIter it;
    IRBB * bb = nullptr;
    for (latch_map.get_first(it, &bb); !it.end(); latch_map.get_next(it, &bb)) {
        //Init the data structures.
        use_reg_cnt.clean();
        def_reg_cnt.clean();
        verifyLatchBB(bb, use_reg_cnt, def_reg_cnt);
    }
    return true;
}


void LTConsistencyMgr::reviseEdgeConsistency(
    InConsistPairList const& inconsist_lst)
{
    LatchMap inserted_latch;
    InConsistPairListIter it;
    UINT i = 0;

    //There are three kinds of IRs (Spill/Mov/Reload) need to be added in the
    //latch BB, because it is necessary to adjust the order of MOV IRs due to
    //the issue, so each kind of IR should be grouped in the latch
    //BB to facilitate the adjustion afterwards. The groups should be organized
    //in the following order:
    //  1. Group of Spill IRs: Responding to the INCONSIST_PR2MEM type.
    //  2. Group of MOV IRs: Responding to the INCONSIST_PR2PR type.
    //  3. Group of Reload IRs: Responding to the INCONSIST_MEM2PR type.
    //  4. Group of Remat IRs: Responding to the INCONSIST_REMAT type.


    //Step 1: Process the inconsist type PR2MEM first to ensure the data in the
    //is used first before it is overwriten.
    for (InConsistPair * pair = inconsist_lst.get_head(&it);
         i < inconsist_lst.get_elem_count();
         pair = inconsist_lst.get_next(&it), i++) {
        if (pair->type != INCONSIST_PR2MEM) { continue; }
        reviseTypePR2MEM(inserted_latch, pair);
    }

    //Step 2: Process the inconsist type PR2PR to ensure the MOV IRs are in
    //the middle of latch BB, so we can reorder the MOV IRs for the
    //correctness of use-def dependencies if required.
    i = 0;
    for (InConsistPair * pair = inconsist_lst.get_head(&it);
         i < inconsist_lst.get_elem_count();
         pair = inconsist_lst.get_next(&it), i++) {
        if (pair->type != INCONSIST_PR2PR) { continue; }
        reviseTypePR2PR(inserted_latch, pair);
    }

    //Step 3: Process the inconsist type MEM2PR then because there is no IRs
    //left in the latch BB use the data in the register, just overwrite
    //directly.
    i = 0;
    for (InConsistPair * pair = inconsist_lst.get_head(&it);
         i < inconsist_lst.get_elem_count();
         pair = inconsist_lst.get_next(&it), i++) {
        if (pair->type != INCONSIST_MEM2PR) { continue; }
        reviseTypeMEM2PR(inserted_latch, pair);
    }

    //Step 4: Process the inconsist type REMAT last because there is no IRs
    //left in the latch BB use the data in the register, just overwrite
    //directly.
    i = 0;
    for (InConsistPair * pair = inconsist_lst.get_head(&it);
         i < inconsist_lst.get_elem_count();
         pair = inconsist_lst.get_next(&it), i++) {
        if (pair->type != INCONSIST_REMAT) { continue; }
        reviseTypeRemat(inserted_latch, pair);
    }
    ASSERT0L1(verifyLatchBBs(inserted_latch));

    //Reorder the MOV IR after all IRs are inserted in the latch BB.
    LatchBBReorder reorder(inserted_latch, m_impl);
    reorder.perform();

    //Record latch BBs for subsequent optimization and analysis.
    m_impl.getSpillReloadEliminateMgr().recordLatchBBTab(inserted_latch);
}


void LTConsistencyMgr::perform()
{
    computePR2LtInfo();
    InConsistPairList inconsist_lst;
    computeEdgeConsistency(inconsist_lst);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLSRA()) {
        inconsist_lst.dump(m_rg);
    }
    reviseEdgeConsistency(inconsist_lst);
}
//END LTConsistencyMgr


//
//Start SpillReloadEliminateMgr
//
SpillReloadEliminateMgr::SpillReloadEliminateMgr(LSRAImpl & impl)
    : m_impl(impl), m_ra(impl.getRA())
{
    m_lt2spill = nullptr;
    m_spill_reload_pairs = nullptr;
    m_rg = impl.getRegion();
    ASSERT0(m_rg);
}


SpillReloadEliminateMgr::~SpillReloadEliminateMgr()
{
    freeRemovedIRs();
    if (m_lt2spill != nullptr) {
        LT2IRTabIter it;
        IRTab * irtab = nullptr;
        for (m_lt2spill->get_first(it, &irtab); !it.end();
             m_lt2spill->get_next(it, &irtab)) {
            ASSERT0(irtab);
            delete irtab;
        }
        delete m_lt2spill;
        m_lt2spill = nullptr;
    }
    if (m_spill_reload_pairs != nullptr) {
        for (IRPair * pair = m_spill_reload_pairs->get_head();
             pair != nullptr; pair = m_spill_reload_pairs->get_next()) {
            delete pair;
        }
        delete m_spill_reload_pairs;
        m_spill_reload_pairs = nullptr;
    }
}


void SpillReloadEliminateMgr::freeRemovedIRs()
{
    RemovedIRTabIter it;
    for (IR const* ir = m_removed_ir_tab.get_first(it); ir != nullptr;
         ir = m_removed_ir_tab.get_next(it)) {
        //Removed the IRs recorded before in LSRA.
        m_ra.removeOpInPosGapRecord(ir);

        //Free the resources of IRs.
        m_rg->freeIRTree(const_cast<IR*>(ir));
    }
}


void SpillReloadEliminateMgr::recordSpill(
    LifeTime const* lt, IR * spill, SplitCtx const& ctx, bool is_def)
{
    ASSERT0(lt && spill);

    //Record the spill for one-def lifetime.
    recordInfoUsedByOneDefElim(lt, spill);

    //Record the spill for the lifetime with fake-use IR.
    recordInfoUsedByFakeUseSpillElim(lt, spill, ctx.split_pos_ir, is_def);
}


void SpillReloadEliminateMgr::recordLatchBBTab(LatchMap const& latch_map)
{
    LatchMapIter itt;
    IRBB * bb = nullptr;
    for (latch_map.get_first(itt, &bb); !itt.end();
         latch_map.get_next(itt, &bb)) {
        m_ra.setLatchBB(bb);
    }
}


void SpillReloadEliminateMgr::recordInfoUsedByOneDefElim(
    LifeTime const* lt, IR * spill)
{
    ASSERT0(lt && spill);
    if (!lt->isOneDefOnly()) { return; }
    ASSERT0(lt->getAncestor());
    record(lt->getAncestor(), spill);
}


void SpillReloadEliminateMgr::recordInfoUsedByFakeUseSpillElim(
    LifeTime const* lt, IR const* spill, IR const* split_pos_ir, bool is_def)
{
    if (split_pos_ir == nullptr) { return; }
    ASSERT0(lt && spill);
    //If the spill at a def IR, return directly since the fake-use
    //IR is a use.
    if (is_def) { return; }

    //If the lifetime is not related to the fake-use IR, do nothing.
    if (!m_ra.isLTWithFakeUseAtLexFirstBBInLoop(lt)) { return; }

    //Check the split pos IR is the fake-use IR at the first BB of loop in
    //lexicographical order or not.
    IR const* split_stmt = split_pos_ir->is_stmt() ?
        split_pos_ir : split_pos_ir->getStmt();
    IR const* fake_use = const_cast<LifeTime*>(lt)->getFirstOccStmt();
    if (fake_use != split_stmt) { return; }

    recordSpillAtFirstFakeUse(lt->getPrno(), spill);
}


void SpillReloadEliminateMgr::record(LifeTime const* lt, IR * ir)
{
    ASSERT0(lt && ir);
    if (m_lt2spill == nullptr) { m_lt2spill = allocLT2IRTab(); }
    bool find = false;
    IRTab * irtab = m_lt2spill->getAndGen(lt, &find);
    ASSERT0(irtab);
    irtab->getAndGen(ir, &find);
}


void SpillReloadEliminateMgr::record(IR * spill, IR * reload)
{
    if (spill == nullptr || reload == nullptr) { return; }
    ASSERT0(spill->getBB() && reload->getBB());
    if (spill->getBB() != reload->getBB()) { return; }
    if (m_spill_reload_pairs == nullptr)
    { m_spill_reload_pairs = allocIRPairLst(); }
    IRPair * pair = allocIRPair();
    pair->spill = spill;
    pair->reload = reload;
    m_spill_reload_pairs->append_tail(pair);
}
//End SpillReloadEliminateMgr


//
//START SplitMgr
//
SplitMgr::SplitMgr(LSRAImpl & impl) : m_impl(impl), m_ra(impl.getRA())
{
    m_rg = m_impl.getRegion();
    m_irmgr = m_rg->getIRMgr();
    m_cfg = m_ra.getCFG();
    m_oc = m_impl.getOptCtx();
    m_live_mgr = impl.getLiveMgr();
}


void SplitMgr::selectLTImpl(OUT Occ & next_occ, OUT LifeTime *& selected_lt,
                            IN LifeTime *const cur_lt, SplitCtx const& ctx)
{
    if (selected_lt == nullptr) {
        next_occ = ctx.reload_occ;
        selected_lt = cur_lt;
        return;
    }

    //Select the lifetime with lower loop nesting level.
    if (cur_lt->getPriority() - selected_lt->getPriority() > EPSILON) {
        return;
    }

    //Select the lifetime with the next occurance from given position is the
    //furthest. For example, given pos is 10, and two lifetime lt1 and lt2:
    //  lt1:  <5-40>, next-occ is in 20
    //  lt2:  <5-25>, next-occ is in 25
    //the lifetime lt2 will be selected.
    if (next_occ.pos() > ctx.reload_pos) { return; }

    //[PENDING] Experiments show that this is not a reasonable optimization
    //          point and will not be adopted.
    //Select the lifetime with higher length.
    //if (cur_lt->getLength() < selected_lt->getLength()) { return; }

    //Select the lifetime with lower activity level (i.e., with lower
    //occurances).
    if (cur_lt->getOccList().get_elem_count() >
        selected_lt->getOccList().get_elem_count()) {
        return;
    }

    //Consider the cost of spillage. Normally the spillage of vector
    //operation is more larger than scalar operation. We will not select
    //the lifetime with vector operation to be spilled.
    ASSERT0(next_occ.getIR() && next_occ.getIR()->getType());
    ASSERT0(ctx.reload_occ.getIR() && ctx.reload_occ.getIR()->getType());
    if (!next_occ.getIR()->is_vec() && ctx.reload_occ.getIR()->is_vec()) {
        return;
    }

    next_occ = ctx.reload_occ;
    selected_lt = cur_lt;
}


LifeTime * SplitMgr::selectLTBaseMultiStrategies(LTSet const& lst,
    Vector<SplitCtx> const& ctxvec, MOD SplitCtx & cur_ctx)
{
    VecIdx cnt = 0;
    ASSERT0(ctxvec.get_elem_count() == lst.get_elem_count());
    Occ next_occ;
    LifeTime * selected_lt = nullptr;
    LTSetIter it;
    for (LifeTime * cur_lt = lst.get_head(&it);
         cur_lt != nullptr; cur_lt = lst.get_next(&it), cnt++) {
        SplitCtx ctx = ctxvec.get(cnt);
        selectLTImpl(next_occ, selected_lt, cur_lt, ctx);
    }
    if (selected_lt == nullptr) { return nullptr; }
    cur_ctx.reload_occ = next_occ;
    dumpSelectSplitCand(m_impl, selected_lt, cur_ctx.split_pos, true,
                        "$%u has furthest reload-occ", selected_lt->getPrno());
    return selected_lt;
}


LifeTime * SplitMgr::selectLTByFurthestNextRange(
    LTSet const& lst, Pos pos, OUT Occ & reload_occ)
{
    LTSetIter it;
    Range furthest_range(POS_UNDEF);
    LifeTime * cand = nullptr;
    VecIdx cnt = 0;
    for (LifeTime * t = lst.get_head(&it);
         t != nullptr; t = lst.get_next(&it), cnt++) {
        VecIdx ridx, less, great;
        Range r1(POS_UNDEF);
        bool find = t->findRange(pos, r1, ridx, &less, &great);
        if (find) {
            ASSERT0(ridx != VEC_UNDEF);
            VecIdx nextrange = ridx + 1;
            if (nextrange > t->getLastRangeIdx()) {
                //No remaining range.
                continue;
            }
            Range r3 = t->getRange(nextrange);
            if (furthest_range.start() == POS_UNDEF ||
                r3.start() > furthest_range.start()) {
                furthest_range = r3;
                cand = t;
            }
            continue;
        }
        if (great == VEC_UNDEF) {
            //There is not range after given 'pos'.
            continue;
        }
        Range r2 = t->getRange(great);
        ASSERT0(r2.start() != POS_UNDEF && r2.end() != POS_UNDEF);
        if (furthest_range.start() == POS_UNDEF ||
            r2.start() > furthest_range.start()) {
            furthest_range = r2;
            cand = t;
        }
    }
    if (cand == nullptr) { return nullptr; }
    OccListIter it2;
    bool succ = cand->findOcc(furthest_range.start(), it2);
    ASSERT0_DUMMYUSE(succ);
    reload_occ = it2->val();
    dumpSelectSplitCand(m_impl, cand, pos, true,
                        "$%u has furthest next range", cand->getPrno());
    return cand;
}


void SplitMgr::selectSplitCandFromSet(LTSet const& set, SplitCtx const& ctx,
                                      OUT LTSet & candlst,
                                      OUT Vector<SplitCtx> & candctxvec)
{
    LTSetIter it;
    LTSetIter nit;
    for (set.get_head(&it), nit = it; it != nullptr; it = nit) {
        set.get_next(&nit);
        LifeTime * t = it->val();
        ASSERTN(m_ra.hasReg(t), ("it should not be in InActiveSet"));

        //If the current lifetime's PR exists in the constraint set of the PR
        //to be allocated, it is ignored.
        if (isConflictedWithTargetLT(t, ctx.split_lt->getPrno())) { continue; }

        Type const* target_ty = m_ra.getVarTypeOfPRNO(
            m_ra.getAnctPrno(ctx.split_lt->getPrno()));
        ASSERT0(target_ty);
        Reg r = m_ra.getReg(t->getPrno());

        //The target type of the lifetime must match the type of
        //the assigned register.
        if (!m_impl.getRegSetImpl().isRegTypeMatch(target_ty, r)) { continue; }

        SplitCtx lctx(ctx);
        bool canbe = checkIfCanBeSplitCand(t, ctx.split_pos, lctx.reload_pos,
                                           lctx.reload_occ);
        if (!canbe) { continue; }
        candlst.append_tail(t);
        candctxvec.append(lctx);
    }
}


LifeTime * SplitMgr::selectSplitCandByDensity(LTSet & set, LifeTime * lt,
                                              bool tryself, OUT SplitCtx & ctx)
{
    //TODO:select split-candidate by choosing the least occurrence-density.
    //lifetime's occurrence-density = the number of occ /
    //                                the length of lifetime.
    return nullptr;
}


LifeTime * SplitMgr::selectSplitCandImpl(LTSet & set, LifeTime * lt,
                                         bool tryself, OUT SplitCtx & ctx)
{
    LTSet candlst;
    Vector<SplitCtx> candctxvec;
    selectSplitCandFromSet(set, ctx, candlst, candctxvec);
    if (tryself) {
        OccListIter it;
        bool succ = lt->findOccAfter(ctx.split_pos, it);
        SplitCtx lctx(ctx);
        if (succ) {
            lctx.reload_pos = it->val().pos();
            lctx.reload_occ = it->val();
        } else {
            //There is no reload-occ of lt.
            lctx.reload_pos = POS_UNDEF;
        }
        candlst.append_tail(lt);
        candctxvec.append(lctx);
    }
    if (candlst.get_elem_count() == 0) {
        return nullptr;
    }
    //Attempt to select a lifetime with least priority and the biggest hole to
    //contain the entire given 'lt'.
    LifeTime * cand = selectLTBaseMultiStrategies(candlst, candctxvec, ctx);
    ASSERT0(cand);
    ctx.reload_pos = ctx.reload_occ.pos();
    //candidate may not have reload_pos.
    //ASSERT0(ctx.reload_pos != POS_UNDEF);
    set.remove(cand);
    return cand;
}


LifeTime * SplitMgr::selectSplitCandFromInActive(LifeTime * lt, bool tryself,
                                                 OUT SplitCtx & ctx)
{
    return selectSplitCandImpl(m_ra.getInActive(), lt, tryself, ctx);
}


LifeTime * SplitMgr::selectSplitCandFromActive(LifeTime * lt, bool tryself,
                                               OUT SplitCtx & ctx)

{
    LifeTime * cand = selectSplitCandByDensity(m_ra.getActive(), lt,
                                               tryself, ctx);
    if (cand != nullptr) { return cand; }
    return selectSplitCandImpl(m_ra.getActive(), lt, tryself, ctx);
}


LifeTime * SplitMgr::selectSplitCand(LifeTime * lt, bool tryself,
                                     OUT SplitCtx & ctx)
{
    //lt is active lifetime, thus do not mix it up with inactive lifetimes.
    bool mixup_with_inactive = false;
    LifeTime * t = selectSplitCandFromInActive(lt, mixup_with_inactive, ctx);
    if (t != nullptr) {
        return t;
    }
    return selectSplitCandFromActive(lt, tryself, ctx);
}


bool SplitMgr::isConflictedWithTargetLT(LifeTime const* target_lt, PRNO cur_pr)
{
    return target_lt->getLTConstraints() != nullptr &&
           target_lt->getLTConstraints()->isConflictPR(cur_pr);
}


bool SplitMgr::checkIfCanBeSplitCand(LifeTime const* lt, Pos split_pos,
                                     OUT Pos & reload_pos, OUT Occ & reload_occ)
{
    OccList & occlst = const_cast<LifeTime*>(lt)->getOccList();
    OccListIter it = nullptr;
    for (Occ occ = occlst.get_head(&it); it != occlst.end();
         occ = occlst.get_next(&it)) {
        if (occ.pos() == split_pos) {
            //lt can not be split at the given position because lt also
            //has an occurrence at the position right there.
            dumpSelectSplitCand(m_impl, lt, split_pos, false,
                                "lt interferred at pos:%u", split_pos);
            return false;
        }
        if (occ.pos() > split_pos) {
            ASSERT0(occ.getIR());
            reload_occ = occ;
            reload_pos = occ.pos();
            dumpSelectSplitCand(m_impl, lt, split_pos, true, nullptr);
            return true;
        }
    }
    return false;
}


//If there is no real-occurrence at split_pos, the lifetime 'lt'
//should shrink to the nearest USE, namely either the RHS of spill or RHS at
//split_pos.
void SplitMgr::shrinkLTToSplitPos(LifeTime * lt, Pos split_pos,
                                  IR const* split_pos_ir)
{
    //When spill operation inserted before split_pos, lt will be terminated
    //at the previous USE position, even if IR at split_pos may not be the
    //occurrence of lt.
    if (UpdatePos::isDef(split_pos)) {
        //CASE:[10] x <- y [9]
        //  If 'split_pos' is 10, spill operation will be inserted before the
        //  stmt at split_pos.
        //  lt's lifetime will be terminated at RHS of previous stmt.
        UpdatePos::decToLastUse(split_pos);
    }
    if (lt->isUseOcc(split_pos_ir)) {
        //CASE:[10] x <- $1 [9]
        //  If 'split_pos' is 9, spill operation will be inserted before the
        //  stmt at split_pos.
        //  lt's lifetime will be terminated at split_pos.
        UpdatePos::incToNextDef(split_pos);
    }

    if (split_pos == lt->getFirstRange().start()) {
        //This would avoid the lifetime may be updated to start from zero
        //after shrinked.
        UpdatePos::inc(split_pos);
    }
    lt->cleanRangeFrom(split_pos);
    ASSERT0(lt->getFirstRange().start() > POS_UNDEF);
}


void SplitMgr::cutoffLTFromSpillPos(LifeTime * lt, Pos split_pos)
{
    //When spill operation inserted after 'split_pos', lt will be terminated
    //at split_pos.
    //e.g:lt termiated at position 10.
    //    [10] $1 <- 1
    //         ... <- $1 //spill operation of $1
    Pos nextpos = split_pos;
    UpdatePos::inc(nextpos);
    lt->cleanRangeFrom(nextpos);
}


bool SplitMgr::isDefLT(IR const* stmt, LifeTime const* lt) const
{
    ASSERT0(stmt->is_stmt());
    IR const* pr = const_cast<IR*>(stmt)->getResultPR();
    return pr != nullptr && pr->getPrno() == lt->getPrno() ? true : false;
}


IR * SplitMgr::doSpillAfterSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    //There is no need to insert spill code in some cases:
    //1. split_pos is in a hole of lt.
    //Even if split_pos is DEF position, spill code is also needed, e.g:
    //spill caller-saved register at call-stmt, the position indicates
    //the call-stmt which is DEF position.
    IR * spill = nullptr;
    cutoffLTFromSpillPos(lt, ctx.split_pos);

    //Prepare the correct register type for spill
    Type const* reg_type = m_ra.getSpillType(lt->getPrno());
    ASSERT0(reg_type);

    spill = m_impl.insertSpillAfter(lt->getPrno(), reg_type, ctx.split_pos_ir);
    ASSERT0(spill);
    dumpSpill(m_impl, spill, lt, ctx.split_pos_ir, false);
    m_impl.getSpillReloadEliminateMgr().recordSpill(lt, spill, ctx, true);
    return spill;
}


IR * SplitMgr::doSpillBeforeSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    IR * spill = nullptr;
    //Prepare the correct register type for spill
    Type const* reg_type = m_ra.getSpillType(lt->getPrno());
    ASSERT0(reg_type);

    shrinkLTToSplitPos(lt, ctx.split_pos, ctx.split_pos_ir);
    spill = m_impl.insertSpillBefore(lt->getPrno(), reg_type, ctx.split_pos_ir);
    ASSERT0(spill);
    dumpSpill(m_impl, spill, lt, ctx.split_pos_ir, true);
    m_impl.getSpillReloadEliminateMgr().recordSpill(lt, spill, ctx, false);
    return spill;
}


IR * SplitMgr::insertSpillAroundSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    //CASE: Because the original lifetime is shrinked forward to the last occ,
    //the split position may not be included in the shrinked lifetime.
    //ASSERT0(lt->is_contain(ctx.split_pos));
    ASSERT0(ctx.split_pos_ir);

    if (UpdatePos::isDef(ctx.split_pos) && lt->isDefOcc(ctx.split_pos_ir)) {
        return doSpillAfterSplitPos(lt, ctx);
    }

    return doSpillBeforeSplitPos(lt, ctx);
}


bool SplitMgr::isUsedBySuccessors(PRNO prno, SplitCtx const& ctx)
{
    ASSERT0(ctx.split_pos_ir);

    //CASE: The reload position can be POS_UNDEF if there is no occ
    //after current split position.
    //ASSERT0(ctx.reload_pos != POS_UNDEF);

    //If the reload position is a USE point, return true directly.
    if (ctx.reload_pos != POS_UNDEF && UpdatePos::isUse(ctx.reload_pos))
    { return true; }
    IRBB * bb =
        ctx.split_pos_ir->is_stmt() ? ctx.split_pos_ir->getBB() :
        ctx.split_pos_ir->getStmt()->getBB();
    ASSERT0(bb);
    AdjVertexIter ito;
    PRNO root_prno = m_ra.getAnctPrno(prno);
    for (Vertex const* out = Graph::get_first_out_vertex(bb->getVex(), ito);
         out != nullptr; out = Graph::get_next_out_vertex(ito)) {
        PRLiveSet const* live_in = m_live_mgr->get_livein(out->id());
        if (live_in->is_contain(root_prno)) { return true; }
    }
    return false;
}


void SplitMgr::insertSpillDuringSplitting(LifeTime * lt, SplitCtx const& ctx,
                                          bool canberemat,
                                          RematCtx const& rematctx,
                                          OUT IR *& spill)
{
    if (canberemat) { return; }
    if (!isUsedBySuccessors(lt->getPrno(), ctx)) { return; }
    spill = insertSpillAroundSplitPos(lt, ctx);
}


void SplitMgr::insertReloadDuringSplitting(LifeTime * lt, LifeTime * newlt,
    SplitCtx const& ctx, bool canberemat, RematCtx const& rematctx,
    IR * spill, OUT IR*& reload)
{
    if (!UpdatePos::isUse(ctx.reload_pos)) { return; }
    if (canberemat) {
        IR * remat = m_impl.insertRematBefore(newlt->getPrno(), rematctx,
            rematctx.material_exp->getType(), ctx.reload_occ.getIR());
        newlt->setRematerialized();
        dumpRemat(m_impl, remat, lt, newlt, ctx.reload_occ.getIR());
        return;
    }

    //There is no need to insert reload newlt's register in some cases:
    //1. reload_pos is already a DEF operation.
    ASSERT0(ctx.reload_occ.getIR());
    ASSERTN(spill, ("illegal splitting strategy"));
    Var * spill_loc = m_impl.findSpillLoc(spill);
    ASSERT0(spill_loc);
    reload = m_impl.insertReloadBefore(newlt->getPrno(), spill_loc,
        spill_loc->getType(), ctx.reload_occ.getIR());
    dumpReload(m_impl, reload, lt, newlt, ctx.reload_occ.getIR());
}


LifeTime * SplitMgr::splitAt(LifeTime * lt, MOD SplitCtx & ctx)
{
    LifeTime * newlt = splitIntoTwoLT(lt, ctx);

    //Update the split position info because the original lifetime may be
    //shrinked forward, the spill IR should be inserted at the last occ
    //position of the original lifetime after split.
    Occ occ = lt->getOccList().get_tail();
    ctx.split_pos = occ.pos();
    ctx.split_pos_ir = occ.getIR();

    RematCtx rematctx;
    bool canberemat = m_ra.checkLTCanBeRematerialized(lt, rematctx);
    IR * spill = nullptr;
    IR * reload = nullptr;
    insertSpillDuringSplitting(lt, ctx, canberemat, rematctx, spill);
    insertReloadDuringSplitting(lt, newlt, ctx, canberemat, rematctx, spill,
                                reload);
    m_impl.recordSplittedNewLT(newlt);
    m_impl.getSpillReloadEliminateMgr().record(spill, reload);
    return newlt;
}


void SplitMgr::shrinkSplitPosForSpillOnly(LifeTime * lt, MOD SplitCtx & ctx)
{
    OccListIter it = nullptr;
    bool find = lt->findOccBefore(ctx.split_pos, it);
    if (!find) { return; }
    Occ occ = it->val();
    ctx.split_pos = occ.pos();
    ctx.split_pos_ir = occ.getIR();
    m_ra.getActMgr().dump(
        "SPILL_ONLY: $%u is shrinked to split pos %u at split pos ir:id%u",
        lt->getPrno(), occ.pos(), occ.getIR()->id());
}


void SplitMgr::shrinkLTForSpillOnly(LifeTime * lt, Pos split_pos,
                                    IR const* split_pos_ir)
{
    //We need two steps to finish the lifetime shrink.
    //  1. Shrink the lifetime forward to the split position.
    //  2. Shrink the lifetime forward to the last occ if there are some gap
    //     position between the last postion of lifetime and the last occ.
    //E.g:
    //
    //Original lifetime:
    // lifetime $1: <2-17><34-53>
    //    | ----------------                --------------------
    //    |                u                d  u               u
    // POS: 2   5          17               34 37    41        53
    //                                               ^
    //                                               |
    //                                           split_pos
    //Modified lifetime after step 1:
    // lifetime $1: <2-17><34-41>
    //    | ----------------                ---------
    //    |                u                d  u
    // POS: 2   5          17               34 37    41
    //                                               ^
    //                                               |
    //                                           split_pos
    //Final lifetime after step 2:
    // lifetime $1: <2-17><34-37>
    //    | ----------------                ----
    //    |                u                d  u
    // POS: 2   5          17               34 37

    ASSERT0(lt && split_pos_ir);
    ASSERT0(split_pos != POS_UNDEF);
    //Implemente the step 1.
    shrinkLTToSplitPos(lt, split_pos, split_pos_ir);

    //Implement the step 2, shrink the lifetime to the last pcc before the
    //split position if possible.
    OccListIter it = nullptr;
    bool find = lt->findOccBefore(split_pos, it);
    if (!find) { return; }
    ASSERT0(find);
    Occ occ = it->val();
    Range r = lt->getLastRange();
    RG_end(r) = occ.pos();
    lt->setLastRange(r);
}


void SplitMgr::spillOnly(LifeTime * lt, MOD SplitCtx & ctx)
{
    ASSERT0(lt);
    if (!lt->canBeRemat()) {
        shrinkSplitPosForSpillOnly(lt, ctx);
        IR * spill = insertSpillAroundSplitPos(lt, ctx);
        m_ra.getActMgr().dump("SPILL_ONLY: $%u is spilled only with ir:id%u",
            lt->getPrno(), spill->id());
    } else {
        shrinkLTForSpillOnly(lt, ctx.split_pos, ctx.split_pos_ir);
        lt->setRematerialized();
        m_ra.getActMgr().dump("SPILL_ONLY: $%u is spilled only but can remat",
            lt->getPrno());
    }
    lt->setSpillOnly();
}


LifeTime * SplitMgr::splitIntoTwoLT(LifeTime * lt, SplitCtx const& ctx)
{
    ASSERT0(ctx.reload_occ.getIR());
    PRNO newprno = m_irmgr->buildPrno(ctx.reload_occ.getIR()->getType());
    //Note newlt will start from the reload_pos, therefore there is a free hole
    //between split_pos and reload_pos of lt.
    LifeTime * newlt = m_impl.getLTMgr().genLifeTime(newprno);
    newlt->setParent(lt);
    newlt->setAncestor(lt->getAncestor());
    const_cast<LifeTime*>(lt->getAncestor())->addChild(newlt);
    newlt->moveFrom(lt, ctx.reload_pos);

    //Shrink the original lifetime forward to the last occ position. Because
    //the split point can be at any position, when this split position is
    //between the two USE positions, the original lifetime after split contains
    //the useless range after the first USE position.
    //For example:
    //   lifetime: <2-25>
    //    | ------------------------
    //    |                u       u
    // POS: 2              17      25
    //                         ^
    //                         |
    //                   split_pos = 20
    //
    //  original lifetime after split:
    //   lifetime: <2-20>
    //    | -------------------
    //    |                u
    // POS: 2              17  20
    //
    //  original lifetime after split and shrink:
    //   lifetime: <2-17>
    //    | ----------------
    //    |                u
    // POS: 2              17
    lt->shrinkForwardToLastOccPos();
    newlt->inheritAttrFlag(lt);
    m_ra.setReg(newlt->getPrno(), REG_UNDEF);
    if (newlt->isPreAssigned()) {
        Reg antireg = m_ra.getPreAssignedReg(lt->getPrno());
        ASSERT0(antireg != REG_UNDEF);
        //Both lt and newlt expect the same pre-assigned register.
        m_ra.setPreAssignedReg(newlt->getPrno(), antireg);
    }
    m_impl.getLTMgr().renameLifeTimeOcc(newlt, newprno);
    dumpSplitTwo(m_impl, lt, newlt, ctx.split_pos, ctx.reload_pos);
    return newlt;
}
//END SplitMgr


//
//Start LSRAImpl.
//
LSRAImpl::LSRAImpl(LinearScanRA & ra, RegSetImpl & rsimpl, bool use_expose)
    : m_is_dominfo_valid(true), m_ra(ra), m_rsimpl(rsimpl),
      //NOTE:m_rg has to be initialized before m_spill_reload_eliminate_mgr.
      m_rg(ra.getRegion()),
      m_spill_reload_eliminate_mgr(*this)
{
    m_is_insert_bb = false;
    m_use_expose = use_expose;
    m_rg = ra.getRegion();
    m_tm = m_rg->getTypeMgr();
    m_irmgr = m_rg->getIRMgr();
    m_cfg = ra.getCFG();
    m_bb_list = ra.getBBList();
    m_live_mgr = nullptr;
    m_oc = nullptr;
    m_argpasser = nullptr;
}


LSRAImpl::~LSRAImpl()
{}


void LSRAImpl::dumpAssign(
    LSRAImpl & lsra, LifeTime const* lt, CHAR const* format, ...)
{
    Reg r = lsra.getReg(lt);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        DefFixedStrBuf buf;
        buf.vstrcat(format, args);
        va_end(args);
        lsra.getActMgr().dump("ASSIGN:$%u with %s, reason:%s",
            lt->getPrno(), lsra.getRegName(r), buf.getBuf());
    } else {
        lsra.getActMgr().dump("ASSIGN:$%u with %s",
            lt->getPrno(), lsra.getRegName(r));
    }
}


void LSRAImpl::dumpBBList() const
{
    xoc::dumpBBList(m_bb_list, m_rg);
}


void LSRAImpl::forceAssignRegister(LifeTime const* lt, Reg reg)
{
    m_rsimpl.pickRegFromAllocable(reg);
    if (m_rsimpl.isCallee(reg)) {
        m_rsimpl.recordUsedCallee(reg);
    }
    if (m_rsimpl.isCaller(reg)) {
        m_rsimpl.recordUsedCaller(reg);
    }
    m_ra.setReg(lt->getPrno(), reg);
    dumpAssign(*this, lt, "assign pre-assigned register");
}


bool LSRAImpl::tryAssignCallee(IR const* ir, LifeTime const* lt)
{
    if (!m_ra.isCalleePermitted(lt)) { return false; }

    Reg r = m_rsimpl.pickCallee(ir, lt->getLTConstraints());
    if (r != REG_UNDEF) {
        ASSERT0(m_rsimpl.isAvailAllocable(r));
        m_rsimpl.pickRegisterFromCalleeAliasSet(r);
        ASSERT0(m_rsimpl.isCallee(r));
        m_ra.setReg(lt->getPrno(), r);
        m_rsimpl.recordUsedCallee(r);
        dumpAssign(*this, lt, nullptr);
        return true;
    }
    return false;
}


bool LSRAImpl::tryAssignCaller(IR const* ir, LifeTime const* lt)
{
    Reg r = m_rsimpl.pickCaller(ir, lt->getLTConstraints());
    if (r != REG_UNDEF) {
        ASSERT0(m_rsimpl.isAvailAllocable(r));
        ASSERT0(m_rsimpl.isCaller(r));
        m_rsimpl.pickRegisterFromCallerAliasSet(r);
        m_ra.setReg(lt->getPrno(), r);
        m_rsimpl.recordUsedCaller(r);
        dumpAssign(*this, lt, nullptr);
        return true;
    }
    return false;
}


bool LSRAImpl::tryAssignRegisterByPrefer(IR const* ir, LifeTime const* lt)
{
    ASSERT0(!lt->isPreAssigned());
    ASSERT0(getLTPrefer(lt) != PREFER_UNDEF);

    if (getLTPrefer(lt) == PREFER_CALLEE) {
        if (tryAssignCallee(ir, lt)) { return true; }
        if (tryAssignCaller(ir, lt)) { return true; }
    }

    if (getLTPrefer(lt) == PREFER_CALLER) {
        return tryAssignRegisterDefault(ir, lt);
    }

    getActMgr().dump("ASSIGN:can NOT find register for $%u",
                     lt->getPrno());
    return false;
}


bool LSRAImpl::tryAssignRegisterDefault(IR const* ir, LifeTime const* lt)
{
    ASSERT0(!lt->isPreAssigned());
    if (tryAssignCaller(ir, lt)) { return true; }
    if (tryAssignCallee(ir, lt)) { return true; }
    getActMgr().dump("ASSIGN:can NOT find register for $%u", lt->getPrno());
    return false;
}


bool LSRAImpl::tryAssignRegister(IR const* ir, LifeTime const* lt)
{
    ASSERT0(!lt->isPreAssigned());
    return (getLTPrefer(lt) == PREFER_UNDEF) ?
        tryAssignRegisterDefault(ir, lt) : tryAssignRegisterByPrefer(ir, lt);
}


LifeTime * LSRAImpl::selectAssignDefCand(Pos curpos, IR const* curstmt)
{
    IR const* res = const_cast<IR*>(curstmt)->getResultPR();
    if (res == nullptr) { return nullptr; }
    return pickFromSet(res->getPrno(), m_ra.getUnhandled());
}


LifeTime * LSRAImpl::selectAssignUseCand(
    Pos curpos, IR const* curstmt, OUT IR const** curir)
{
    ASSERT0(curstmt && curir);
    if (m_ra.isFakeUseAtLexLastBBInLoop(curstmt)) {
        //If the IR is a fake-use IR located at the last BB of loop by
        //lexicographical order, ignore this IR due to it is just a fake-use,
        //or else this would introduce lots of spill/reload operation
        //when assgin the lifetime used in the fake-use IR with a register
        //if there is a conflict.
        return nullptr;
    }

    IR const* cand = nullptr;
    ConstIRIter it;
    for (IR const* e = xoc::iterExpInitC(curstmt, it);
         e != nullptr; e = xoc::iterExpNextC(it)) {
        if (!e->isPROp()) { continue; }
        if (m_ra.hasReg(e->getPrno())) { continue; }
        cand = e;
        break;
    }
    if (cand == nullptr) { return nullptr; }
    LifeTime * candlt = pickFromSet(cand->getPrno(), m_ra.getUnhandled());
    ASSERT0(candlt);
    *curir = cand;
    return candlt;
}


//Spill LT that assigned referred register in given LTSet.
void LSRAImpl::splitAllLTWithReg(
    Pos curpos, IR const* ir, Reg r, MOD LTSet & set)
{
    ASSERT0(r != REG_UNDEF);
    LTSetIter it;
    LTSetIter nit;
    for (set.get_head(&it), nit = it; it != nullptr; it = nit) {
        set.get_next(&nit);
        LifeTime * t = it->val();
        if (!m_rsimpl.isAlias(getReg(t), r)) { continue; }
        SplitMgr spltmgr(*this);
        SplitCtx ctx(curpos);
        ctx.split_pos = curpos;
        ctx.split_pos_ir = ir;
        ctx.split_lt = t;
        bool canbe = spltmgr.checkIfCanBeSplitCand(
            t, ctx.split_pos, ctx.reload_pos, ctx.reload_occ);
        ASSERT0_DUMMYUSE(canbe);
        dumpSelectSplitCand(
            *this, t, curpos, true, "split $%u that assigned %s",
            t->getPrno(), m_ra.getRegName(r));
        splitOrSpillOnly(t, curpos, ctx, spltmgr);
        set.remove(it);
        m_ra.getHandled().append_tail(t);
        m_rsimpl.freeReg(t);
    }
}


void LSRAImpl::splitLinkLT(Pos curpos, IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    Reg l = getTIMgr().getLink();
    if (ir->isIntrinsicOp() || !m_rsimpl.isAvailAllocable(l)) { return; }
    splitActiveLTWithReg(curpos, ir, l);
}


void LSRAImpl::splitInActiveLTWithReg(Pos curpos, IR const* ir, Reg r)
{
    splitAllLTWithReg(curpos, ir, r, m_ra.getInActive());
}


void LSRAImpl::splitActiveLTWithReg(Pos curpos, IR const* ir, Reg r)
{
    splitAllLTWithReg(curpos, ir, r, m_ra.getActive());
}


void LSRAImpl::splitCallerSavedLT(Pos curpos, IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    if (ir->isIntrinsicOp()) { return; }
    RegSet const& used = m_rsimpl.getUsedCaller();
    for (BSIdx i = used.get_first(); i != BS_UNDEF; i = used.get_next(i)) {
        ASSERT0(i != REG_UNDEF);
        //Since the LifeTime is 2D, which includes live-in and live-out
        //information, there is no need to split lifetimes that resided in
        //a 2D hole, because these in-hole 2D lifetimes absolutely do not
        //intersect with the lifetime of 'ir'.
        splitAllLTWithReg(curpos, ir, (Reg)i, m_ra.getActive());
    }
}


void LSRAImpl::saveCallee()
{
    RegSet const& used_callee = m_rsimpl.getUsedCallee();
    for (BSIdx i = used_callee.get_first();
         i != BS_UNDEF; i = used_callee.get_next(i)) {
        ASSERT0(m_rsimpl.isCallee(i));
        IR * spill = insertSpillCalleeAtEntry((Reg)i);
        insertReloadCalleeAtExit(i, findSpillLoc(spill));
    }
}


//Pre-assigned register must be satefied in the highest priority.
void LSRAImpl::assignPreAssignedLT(Pos curpos, IR const* ir, LifeTime * lt)
{
    ASSERT0(lt->isPreAssigned());
    Reg antireg = m_ra.getPreAssignedReg(lt);
    ASSERT0(antireg != REG_UNDEF);

    //Dedicated prnos should not be involved in the lifetime splitting.
    if (!m_ra.isDedicatedReg(antireg) ||
        (antireg == m_ra.getFP() && m_ra.isFPAllocable())) {
        splitActiveLTWithReg(curpos, ir, antireg);
        splitInActiveLTWithReg(curpos, ir, antireg);
        m_ra.addActive(lt);
    }
    forceAssignRegister(lt, antireg);
    ASSERT0(getReg(lt) == antireg);
    ASSERT0L3(m_ra.verify4List());
}


//Try assign register for given ir which at 'pos'.
//ir: may be expression or stmt.
//lt: lifetime that corresponding to 'ir'.
void LSRAImpl::tryAssignRegForIR(Pos pos, IR const* ir, LifeTime * lt)
{
    ASSERT0(ir && (ir->is_stmt() || ir->is_exp()));
    if (lt->isPreAssigned()) {
        assignPreAssignedLT(pos, ir, lt);
        return;
    }
    //Normal lifetime.
    if (tryAssignRegister(ir, lt)) {
        Reg r = getReg(lt);
        ASSERT0(r != REG_UNDEF);
        splitInActiveLTWithReg(pos, ir, r);
        m_ra.addActive(lt);
        ASSERT0L3(m_ra.verify4List());
        return;
    }
    ASSERT0(ir);
    solveConflict(lt, pos, ir);
}


void LSRAImpl::transferInActive(Pos curpos)
{
    LTSetIter it;
    LTSetIter nit;
    LTSet & act = m_ra.getActive();
    LTSet & handled = m_ra.getHandled();
    LTSet & inact = m_ra.getInActive();
    for (inact.get_head(&it), nit = it; it != nullptr; it = nit) {
        inact.get_next(&nit);
        LifeTime * lt = it->val();
        if (!lt->is_cover(curpos)) {
            //lt even not conver 'curpos', it has been handled.
            //Transfer lt to handled and free targ-machine resource.
            inact.remove(it);
            handled.append_tail(lt);
            m_rsimpl.freeReg(lt);
            continue;
        }
        if (lt->is_contain(curpos)) {
            //lt is not only conver 'curpos' but also in a range.
            //Transfer lt to active.
            inact.remove(it);
            act.append_tail(lt);
            continue;
        }
    }
}


void LSRAImpl::transferActive(Pos curpos)
{
    LTSetIter it;
    LTSetIter nit;
    LTSet & act = m_ra.getActive();
    LTSet & handled = m_ra.getHandled();
    LTSet & inact = m_ra.getInActive();
    for (act.get_head(&it), nit = it; it != nullptr; it = nit) {
        act.get_next(&nit);
        LifeTime * lt = it->val();
        if (!lt->is_cover(curpos)) {
            //lt even not conver 'curpos', it has been handled.
            //Transfer lt to handled and free targ-machine resource.
            act.remove(it);
            handled.append_tail(lt);
            m_rsimpl.freeReg(lt);
            continue;
        }
        if (!lt->is_contain(curpos)) {
            //lt convers 'curpos' but in a hole.
            //Transfer lt to inactive.
            act.remove(it);
            inact.append_tail(lt);
            continue;
        }
    }
}


//The function check each CFG edge to fixup the lifetime conflict while the
//linearization allocation flattening the CFG.
void LSRAImpl::reviseLTConsistency()
{
    START_TIMER(t, "reviseLTConsistency");
    LTConsistencyMgr mgr(*this);
    mgr.perform();
    END_TIMER(t, "reviseLTConsistency");
}


bool LSRAImpl::isSpillLikeOp(IR const* ir)
{
    if (!ir->is_st()) { return false; }
    if (!ir->getRHS()->is_pr()) { return false; }
    if (!ir->getIdinfo()->is_local()) { return false; }
    return true;
}


bool LSRAImpl::isRematLikeOp(IR const* ir) const
{
    return m_ra.isRematLikeOp(ir);
}


bool LSRAImpl::isReloadLikeOp(IR const* ir)
{
    if (!ir->is_stpr()) { return false; }
    if (!ir->getRHS()->is_ld()) { return false; }
    if (!ir->getRHS()->getIdinfo()->is_local()) { return false; }
    return true;
}


Var * LSRAImpl::findSpillLoc(IR const* ir)
{
    if (isSpillLikeOp(ir)) {
        return ir->getIdinfo();
    }
    ASSERT0(isReloadLikeOp(ir));
    return ir->getRHS()->getIdinfo();
}


IR * LSRAImpl::insertSpillCalleeAtEntry(Reg r)
{
    ASSERT0(r != REG_UNDEF);
    ASSERT0(m_rsimpl.isCallee(r));
    IRBB * bb = m_ra.getCalleeSpilledBB();
    Type const* ty = m_rsimpl.getCalleeRegisterType(r, m_tm);
    PRNO prno = m_irmgr->buildPrno(ty);
    m_ra.setReg(prno, r);
    IR * spill = insertSpillAtBBEnd(prno, ty, bb);
    dumpSpill(*this, spill, r, bb, "spill callee-saved at entry");
    return spill;
}


void LSRAImpl::insertReloadCalleeAtExit(Reg r, Var * spill_loc)
{
    ASSERT0(r != REG_UNDEF && spill_loc);
    ASSERT0(m_rsimpl.isCallee(r));
    List<IRBB*>::Iter it;
    Type const* ty = m_rsimpl.getCalleeRegisterType(r, m_tm);
    PRNO prno = m_irmgr->buildPrno(ty);
    m_ra.setReg(prno, r);
    for (IRBB * bb = m_cfg->getExitList()->get_head(&it);
         bb != nullptr; bb = m_cfg->getExitList()->get_next(&it)) {
        IR * reload = insertReloadAtBB(prno, spill_loc, ty, bb, false);
        dumpReload(*this, reload, r, bb, "reload callee-saved at exit");
    }
}


void LSRAImpl::insertSpillAtHead(IR * spill, MOD IRBB * bb)
{
    ASSERT0(!bb->hasPhi(m_cfg));
    bb->getIRList().append_head(spill);
}


void LSRAImpl::insertSpillAfter(IR * spill, IR const* marker)
{
    ASSERT0(isSpillLikeOp(spill) && marker);
    m_ra.setSpill(spill);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    if (stmt->isCallStmt()) {
        IRBB * followed_bb = m_cfg->insertFallThroughBBAfter(
            stmt->getBB(), m_oc);
        ASSERT0(followed_bb);

        //Increamentally maintain new BB's liveness via marker's BB.
        addLivenessForEmptyLatchBB(followed_bb, stmt->getBB());
        insertSpillAtHead(spill, followed_bb);
        return;
    }
    ASSERTN(!IRBB::isLowerBoundary(stmt), ("need insert new BB"));
    stmt->getBB()->getIRList().insert_after(spill, stmt);
}


IR * LSRAImpl::insertRematBefore(PRNO newres, RematCtx const& rematctx,
                                 Type const* loadvalty, IR const* marker)
{
    IR * remat = m_ra.buildRemat(newres, rematctx, loadvalty);
    insertRematBefore(remat, marker);
    return remat;
}


void LSRAImpl::insertRematBefore(IR * remat, IR const* marker)
{
    ASSERT0(isRematLikeOp(remat) && marker);
    m_ra.setRemat(remat);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    ASSERTN(!stmt->is_phi(), ("LSRA does not support SSA mode"));
    stmt->getBB()->getIRList().insert_before(remat, stmt);
}


void LSRAImpl::insertReloadBefore(IR * reload, IR const* marker)
{
    ASSERT0(isReloadLikeOp(reload) && marker);
    m_ra.setReload(reload);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    ASSERTN(!stmt->is_phi(), ("LSRA does not support SSA mode"));
    stmt->getBB()->getIRList().insert_before(reload, stmt);
}


IR * LSRAImpl::insertMove(PRNO from, PRNO to, Type const* fromty,
                          Type const* toty, IRBB * bb)
{
    IR * mv = m_irmgr->buildMove(to, from, toty, fromty);
    ASSERT0(m_rg->getMDMgr());
    m_rg->getMDMgr()->allocRef(mv->getRHS());
    m_rg->getMDMgr()->allocRef(mv);
    m_ra.setMove(mv);
    bb->getIRList().append_tail_ex(mv);
    return mv;
}


void LSRAImpl::recordSplittedNewLT(LifeTime const* newlt)
{
    m_splitted_newlt_lst.append_tail(newlt);
}


void LSRAImpl::tryUpdateDom(IRBB const* from, IRBB const* newbb,
                            IRBB const* to)
{
    //Update DomInfo incrementally.
    m_rg->getCFG()->addDomToNewSingleInOutBB(from->getVex(),
        newbb->getVex(), to->getVex());
    setDomInfoValid(false);
}


void LSRAImpl::addLivenessForEmptyLatchBB(
    IRBB const* latch_bb, IRBB const* from)
{
    ASSERT0(latch_bb && from);
    ASSERT0(latch_bb->is_empty());
    ASSERT0(m_cfg->is_pred(latch_bb->getVex(), from->getVex()));
    ASSERT0(latch_bb->getVex()->getInDegree() == 1);
    ASSERT0(latch_bb->getVex()->getOutDegree() == 1);
    m_live_mgr->setLivenessForEmptyBB(latch_bb, from);
}


void LSRAImpl::tryUpdateRPO(OUT IRBB * newbb, IRBB const* marker,
                            bool newbb_prior_marker)
{
    m_cfg->tryUpdateRPOBeforeCFGChanged(newbb, marker, newbb_prior_marker,
                                        m_oc);
}


IR * LSRAImpl::insertSpillAfter(PRNO prno, Type const* ty, IR const* marker)
{
    IR * spill = m_ra.buildSpill(prno, ty);
    insertSpillAfter(spill, marker);
    return spill;
}


IR * LSRAImpl::insertSpillBefore(PRNO prno, Type const* ty,
                                 IR const* marker)
{
    IR * spill = m_ra.buildSpill(prno, ty);
    insertSpillBefore(spill, marker);
    return spill;
}


IR * LSRAImpl::insertReloadAtBB(PRNO prno, Var * spill_loc,
                                Type const* ty, IRBB * bb, bool start)
{
    IR * reload = m_ra.buildReload(prno, spill_loc, ty);
    insertReloadAtBB(reload, bb, start);
    return reload;
}


IR * LSRAImpl::insertSpillAtBBEnd(PRNO prno, Type const* ty, IRBB * bb)
{
    IR * spill = m_ra.buildSpill(prno, ty);
    insertSpillAtBBEnd(spill, bb);
    return spill;
}


IR * LSRAImpl::insertSpillAtBBEnd(PRNO prno, Var * var, Type const* ty,
                                  IRBB * bb)
{
    ASSERT0(var && ty && bb);
    IR * spill = m_ra.buildSpillByLoc(prno, var, ty);
    insertSpillAtBBEnd(spill, bb);
    return spill;
}


ArgPasser * LSRAImpl::getArgPasser()
{
    if (m_argpasser != nullptr) { return m_argpasser; }
    m_argpasser = (ArgPasser*)m_rg->getPassMgr()->registerPass(
        PASS_ARGPASSER);
    return m_argpasser;
}


IRListIter LSRAImpl::insertSpillAtBBEnd(IR * spill, IRBB * bb)
{
    ASSERT0(spill && isSpillLikeOp(spill) && bb && m_rg &&
            m_rg->getRegionVar());
    m_ra.setSpill(spill);
    ArgPasser * ap = getArgPasser();
    ASSERT0(ap);
    IR * marker = ap->getEntryParam();
    if (m_rg->getRegionVar()->is_entry() && bb->is_entry() &&
        marker != nullptr) {
        return bb->getIRList().insert_before(spill, ap->getEntryParam());
    }

    return bb->getIRList().append_tail_ex(spill);
}


IRListIter LSRAImpl::insertReloadAtBB(IR * reload, IRBB * bb, bool start)
{
    ASSERT0(isReloadLikeOp(reload) && bb);
    m_ra.setReload(reload);
    if (start) {
        return bb->getIRList().append_head_ex(reload);
    }
    return bb->getIRList().append_tail_ex(reload);
}


void LSRAImpl::insertSpillBefore(IR * spill, IR const* marker)
{
    ASSERT0(isSpillLikeOp(spill) && marker);
    m_ra.setSpill(spill);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERTN(!stmt->is_phi(), ("LSRA does not support SSA mode"));
    BBIRList & irlst = stmt->getBB()->getIRList();
    BBIRListIter it = nullptr;
    irlst.find(const_cast<IR*>(stmt), &it);
    ASSERT0(it);

    //Find the proper position to place the spill IR.
    bool is_after_reload = false;
    for (; it->get_prev() != nullptr; it = irlst.get_prev(it)) {
        IR * ir = it->get_prev()->val();
        if (!m_ra.isReloadOp(ir) && !m_ra.isSpillOp(ir) &&
            !m_ra.isRematOp(ir)) {
            break;
        }

        if (m_ra.isReloadOp(ir)) {
            PRNO def_prno = ir->getPrno();
            PRNO use_prno = spill->getRHS()->getPrno();
            ASSERT0(def_prno != PRNO_UNDEF);
            ASSERT0(use_prno != PRNO_UNDEF);

            //CASE: If a prno is already reloaded before this marker, and
            //the same prno is also needed to be spilled before this marker,
            //the sequence of this reload IR should be keeped to before the
            //spill IR to ensure the correctness of the spilled data.
            if (def_prno == use_prno) { is_after_reload = true; break; }
        }
    }
    if (is_after_reload) {
        irlst.insert_after(spill, it->get_prev());
        return;
    }
    irlst.insert_before(spill, it);
}


IR * LSRAImpl::insertReloadBefore(PRNO newres, Var * spill_loc,
                                  Type const* ty, IR const* marker)
{
    IR * reload = m_ra.buildReload(newres, spill_loc,
        ty->is_any() ? m_tm->getTargMachRegisterType() : ty);
    insertReloadBefore(reload, marker);
    return reload;
}


IR * LSRAImpl::insertReload(PRNO to, Var * v, Type const* ty, IRBB * bb)
{
    IR * reload = m_ra.buildReload(to, v, ty);
    m_ra.setReload(reload);
    bb->getIRList().append_tail_ex(reload);
    return reload;
}


IR * LSRAImpl::insertRemat(PRNO to, IR const* exp, Type const* ty, IRBB * bb)
{
    ASSERT0(exp && ty && bb);
    ASSERT0(to != PRNO_UNDEF);
    RematCtx rematctx;
    rematctx.material_exp = exp;
    IR * remat = m_ra.buildRemat(to, rematctx, ty);
    m_ra.setRemat(remat);
    bb->getIRList().append_tail_ex(remat);
    return remat;
}


bool LSRAImpl::isLtSplitBeforeFakeUseAtLexLastBBInLoop(LifeTime const* lt,
                                                       Pos curpos) const
{
    ASSERT0(lt);
    ASSERT0(curpos != POS_UNDEF);
    OccList & lt_occ_list = const_cast<LifeTime*>(lt)->getOccList();
    IR * lt_tail = lt_occ_list.get_tail().getIR();
    IR * lt_stmt = lt_tail->is_stmt() ? lt_tail : lt_tail->getStmt();
    if (!getRA().isFakeUseAtLexLastBBInLoop(lt_stmt)) { return false; }
    OccListIter it = nullptr;
    bool find = lt->findOccAfter(curpos, it);
    if (!find) { return false; }
    if (it->val().getIR() == lt_tail) { return true; }
    return false;
}


void LSRAImpl::splitPosOpt(LifeTime * lt, MOD SplitCtx & ctx)
{
    moveSplitPosOutsideLoop(lt, ctx);
}


void LSRAImpl::moveSplitPosOutsideLoop(LifeTime * lt, MOD SplitCtx & ctx) const
{
    LI<IRBB> const* liroot = m_cfg->getLoopInfo();
    if (liroot == nullptr) { return; } //No loop info.

    Occ final_occ; //Use as the final split position.
    UINT min_nestlevel = 0xFFFFFFFF; //Maximum number of nest level in loop.

    OccList const& occlst = lt->getOccList();
    OccListIter it = nullptr;
    for (Occ occ = occlst.get_tail(&it); it != nullptr;
         occ = occlst.get_prev(&it)) {

        //Don't care about the occurrences after the split position.
        if (occ.pos() >= ctx.split_pos) { continue; }

        //Spilling cannot cross the definition occurrence.
        if (occ.is_def()) { break; }

        //Whether the occurrence is within the loop.
        ASSERT0(occ.getBB());
        UINT nestlevel = 0;
        liroot->isInsideLoopTree(occ.getBB()->id(), nestlevel, true);

        //Select the occurrence with the smallest nest level. Note that we only
        //select the occurrence that is closest to the original split position
        //and has the smallest nesting level.
        if (nestlevel >= min_nestlevel) { continue; }
        min_nestlevel = nestlevel;
        final_occ = occ;
    }

    //Move the split position out of loop.
    ctx.split_pos = final_occ.pos();
    ctx.split_pos_ir = final_occ.getIR();
}


void LSRAImpl::splitOrSpillOnly(LifeTime * lt, Pos split_pos,
                                MOD SplitCtx & ctx, SplitMgr & spltmgr)
{
    if (isLtSplitBeforeFakeUseAtLexLastBBInLoop(lt, split_pos)) {
        spltmgr.spillOnly(lt, ctx);
        return;
    }

    //Lifetime split position optimization.
    splitPosOpt(lt, ctx);

    LifeTime * newlt = spltmgr.splitAt(lt, ctx);
    m_ra.addUnhandled(newlt);
    computeLTPrefer(newlt);
}


void LSRAImpl::solveConflict(LifeTime * lt, Pos curpos, IR const* curir)
{
    ASSERT0(m_ra.getLTMgr().verifyPos(curir, curpos));
    bool succ = false;
    bool tryself = true;
    UINT count = 0;
    SplitMgr spltmgr(*this);
    do {
        SplitCtx ctx(curpos, curir, lt);
        LifeTime * cand = spltmgr.selectSplitCand(lt, tryself, ctx);
        //CASE: The candidate cannot be found under if all the lifetimes
        //in the active list do not have the occurence after the split
        //position.
        // ASSERTN(cand, ("no enough resource to remedy splitting"));
        if (cand == lt) {
            //Only try itself once.
            tryself = false;
            //The lifetime used in fake-use Op can not cut itself.
            //CASE: The lifetime used in fake-use Op will be changed to
            //contain a single position only, so the register assigned to
            //lifetime will be freed after the current position, and this
            //register may be assigned to another lifetime. This will lead
            //to the inconsistency problem if the two lifetimes are both
            //live-in of this BB.
            if (isLTUsedInFakeOp(lt)) { continue; }
        }

        //If the candidate does not have a reload_pos, splitAt() can NOT
        //cutoff lifetime into two or do the force-spill operation.
        ASSERT0(ctx.reload_pos != POS_UNDEF);
        splitOrSpillOnly(cand, curpos, ctx, spltmgr);

        //CASE:lsra_split.gr, cand may not have reload-occ.
        //ASSERT0(ctx.reload_pos != POS_UNDEF);
        if (m_ra.hasReg(cand)) {
            m_ra.addHandled(cand);
            m_rsimpl.freeReg(cand);
        } else {
            //CASE:lt may be assigning-candidate, whereas lt is selected as the
            //splitting-candidate meanwhile. Thus after the function return, lt
            //will be assigned a register and newlt will be waiting for
            //assigning.
            //ASSERT0(m_ra.getUnhandled().find(lt));

            //Update the lifetime prefer only if it is not handled.
            computeLTPrefer(cand);
        }
        succ = tryAssignRegister(curir, lt);
        count++;
    } while (!succ && count < 20);
    ASSERT0(succ);
    dumpAssign(*this, lt, nullptr);
    m_ra.addActive(lt);
    splitInActiveLTWithReg(curpos, curir, getReg(lt));
    ASSERT0L3(m_ra.verify4List());
}


void LSRAImpl::dump() const
{
    m_rsimpl.dumpAvailRegSet();
}


void LSRAImpl::computeRAPrefer()
{
    PRNO2LT const& prno2lt = getRA().getLTMgr().getPrno2LT();

    for (VecIdx i = 0; i <= prno2lt.get_last_idx(); i++) {
        if (prno2lt[i] == nullptr) { continue; }
        computeLTPrefer(prno2lt[i]);
    }
}


void LSRAImpl::computeLTPrefer(LifeTime const* lt)
{
    REG_PREFER prefer = PREFER_UNDEF;

    if (lt->getCallCrossedNum() >= CROSS_CALL_NUM_THRESHOLD) {
        prefer = PREFER_CALLEE;
    }
    if (lt->getCallCrossedNum() == 1) {
        prefer = PREFER_CALLER;
    }
    m_lt2prefer.setAlways(lt, prefer);
}


bool LSRAImpl::eliminateSingleIR(IR const* ir)
{
    ASSERT0(ir);

    //If the IR is already removed, return;
    if (getSpillReloadEliminateMgr().isIRRemoved(ir)) { return false; }

    IRBB * bb = ir->getBB();

    //If the basic block of IR is nullptr, it means that this IR has already
    //been eliminated by some optimizations.
    //Note that we donot free this IR here. Since multiple optimizations may
    //eliminate same IR, We record it instead of free it to prevent multiple
    //free. The IR will be uniformly freed in the destructor's freeRemovedIRs()
    //after all optimizations are completed.
    if (bb != nullptr) { bb->getIRList().remove(const_cast<IR*>(ir)); }

    //Record all the eliminated IRs.
    getSpillReloadEliminateMgr().addToRemovedIRTab(ir);
    return true;
}


void LSRAImpl::removeSpillIRForFakeUseAtLexFirstBBOfLoop()
{
    PR2FakeUseSpill const& pr2fakeuse_spill =
        getSpillReloadEliminateMgr().getPR2FakeUseSpill();

    PR2FakeUseSpillIter it;
    IR const* spill = nullptr;
    for(PRNO pr = pr2fakeuse_spill.get_first(it, &spill);
        pr != PRNO_UNDEF; pr = pr2fakeuse_spill.get_next(it, &spill)) {
       ASSERT0(getRA().isSpillOp(spill));
       ASSERT0(pr == spill->getRHS()->getPrno());
       eliminateSingleIR(spill);
    }
}


bool LSRAImpl::eliminateIRsInTab(IRTab & irtab)
{
    if (irtab.get_elem_count() == 0) { return false; }
    bool changed = false;
    IRTabIter it;
    for (IR * ir = irtab.get_first(it); ir != nullptr;
         ir = irtab.get_next(it)) {
        changed |= eliminateSingleIR(ir);
    }
    return changed;
}


bool LSRAImpl::eliminateSpillOfOneDefLifeTime()
{
    bool changed = false;

    LT2IRTab * lt2irtab = m_spill_reload_eliminate_mgr.getLT2Spill();
    if (lt2irtab == nullptr) { return false; }

    LTList const& ltlst = getLTMgr().getLTList();
    LTListIter it;
    for (LifeTime const* lt = ltlst.get_head(&it); lt != nullptr;
         lt = ltlst.get_next(&it)) {
        //We only process one-def lifetimes carried spilling IRs.
        if (!lt->isOneDefOnly() || !lt2irtab->find(lt)) { continue; }

        //Eliminate all spilling IRs of current one-def lifetime.
        eliminateIRsInTab(*(lt2irtab->get(lt)));

        //Insert a new spilling IR after the definition of one-def lifetime.
        ASSERT0(lt->getAncestor());
        Occ def = lt->getAncestor()->getOnlyDefOcc();
        ASSERT0(def.pos() != POS_UNDEF && def.getIR() != nullptr);
        IR const* def_ir = def.getIR();
        ASSERT0(def_ir->is_stmt() && def_ir->getPrno() != PRNO_UNDEF);
        PRNO def_prno = def_ir->getPrno();
        Type const* tp = getRA().getSpillType(def_prno);
        insertSpillAfter(def_prno, tp, def_ir);

        changed = true;
    }
    return changed;
}


bool LSRAImpl::eliminateRedundantMov()
{
    BBListIter bbit;
    IRTab remove_tab;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        BBIRList const& irlst = bb->getIRList();
        BBIRListIter bbirit;
        for (IR * ir = irlst.get_head(&bbirit);
             ir != nullptr; ir = irlst.get_next(&bbirit)) {
            //Process the mov Op only.
            if (!m_ra.isMoveOp(ir)) { continue; }

            PRNO dst_prno = ir->getPrno();
            PRNO src_prno = ir->getRHS()->getPrno();
            ASSERT0(dst_prno != PRNO_UNDEF);
            ASSERT0(src_prno != PRNO_UNDEF);
            ASSERT0(m_ra.getReg(dst_prno) != REG_UNDEF);
            ASSERT0(m_ra.getReg(src_prno) != REG_UNDEF);

            //Ignore the dst register and src register are not same.
            if (m_ra.getReg(dst_prno) != m_ra.getReg(src_prno)) { continue; }

            //Record redundant ir.
            remove_tab.append(ir);
        }
    }

    return eliminateIRsInTab(remove_tab);
}


bool LSRAImpl::eliminateSameSlotSpillAfterReload()
{
    BBListIter bbit;
    IRTab remove_tab;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        BBIRList const& irlst = bb->getIRList();
        BBIRListIter bbirit;
        IR * prev_ir = nullptr;

        //Match and record redundant ir.
        for (IR * cur_ir = irlst.get_head(&bbirit); cur_ir != nullptr;
             prev_ir = cur_ir, cur_ir = irlst.get_next(&bbirit)) {

            //Process only when current is spillOp and previous is reloadOp.
            if (prev_ir == nullptr || !m_ra.isSpillOp(cur_ir) ||
                !m_ra.isReloadOp(prev_ir)) {
                continue;
            }

            PRNO ld_prno = prev_ir->getPrno();
            PRNO st_prno = cur_ir->getRHS()->getPrno();
            Var const* ld_var = LD_idinfo(prev_ir->getRHS());
            Var const* st_var = ST_idinfo(cur_ir);
            ASSERT0(ld_prno != PRNO_UNDEF && st_prno != PRNO_UNDEF);
            ASSERT0(ld_var != nullptr && st_var != nullptr);

            Reg ld_reg = m_ra.getReg(ld_prno);
            Reg st_reg = m_ra.getReg(st_prno);
            ASSERT0(ld_reg != REG_UNDEF && st_reg != REG_UNDEF);

            //Ignore if registers or var IDs don't match.
            if (ld_reg != st_reg || ld_var != st_var) { continue; }

            //Record redundant ir.
            remove_tab.append(cur_ir);
        }
    }

    return eliminateIRsInTab(remove_tab);
}


bool LSRAImpl::optimizeCrossBBStoreLoadToMov()
{
    BBListIter bbit;
    bool changed = false;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {

        //Only handle latch BBs.
        if (!getRA().isLatchBB(bb) ||
            bb->getNumOfSucc() != 1) {
            continue;
        }

        IRBB * succ_bb = m_cfg->get_first_succ(bb);
        if (!isUniquePredSuccPair(bb, succ_bb)) { continue; }

        //If the successor is a tramp BB, consider its successor instead.
        if (isTrampBB(succ_bb)) {
            IRBB * succ_succ_bb = m_cfg->get_first_succ(bb);
            if (!isUniquePredSuccPair(succ_bb, succ_succ_bb)) { continue; }
            succ_bb = succ_succ_bb;
        }

        if (bb->is_empty() || succ_bb->is_empty()) { continue; }

        IR * lst_ir = bb->getLastIR();
        IR * fst_ir = succ_bb->getFirstIR();
        if (!m_ra.isSpillOp(lst_ir) || !m_ra.isReloadOp(fst_ir) ||
            ST_idinfo(lst_ir) != LD_idinfo(fst_ir->getRHS())) {
            continue;
        }

        PRNO st_prno = lst_ir->getRHS()->getPrno();
        PRNO ld_prno = fst_ir->getPrno();
        ASSERT0(st_prno != PRNO_UNDEF && ld_prno != PRNO_UNDEF);

        //Convert to mov if assigned physical registers are different.
        if (m_ra.getReg(ld_prno) != m_ra.getReg(st_prno)) {
            IR * mv = m_irmgr->buildMove(ld_prno, st_prno,
                fst_ir->getRHS()->getType(), lst_ir->getType());
            m_ra.setMove(mv);
            succ_bb->getIRList().append_head(mv);
        }
        changed |= eliminateSingleIR(fst_ir);
    }
    return changed;
}


bool LSRAImpl::isDifferentIROp(IR const* ir1, IR const* ir2) const
{
    if (ir1 == nullptr || ir2 == nullptr) { return true; }

    if (m_ra.isRematOp(ir1) && m_ra.isRematOp(ir2)) {
        //e.g.:
        //    stpr $661:i32 (r42) attachinfo:Dbx
        //        ldi:i32
        //            $210:i32 (r13)
        //            intconst:i32 255|0xff
        PRNO ir1_prno = ir1->getPrno();
        PRNO ir2_prno = ir2->getPrno();
        ASSERT0(ir1_prno != PRNO_UNDEF && ir2_prno != PRNO_UNDEF);
        if (ir1_prno == ir2_prno &&
            m_ra.getReg(ir1_prno) == m_ra.getReg(ir2_prno) &&
            m_irmgr->isIRIsomorphicExtOp(ir1->getRHS(), ir2->getRHS(),
            true, IsomoFlag(ISOMO_CK_ALL))) {
            return false;
        }
        return true;
    }
    if (m_ra.isReloadOp(ir1) && m_ra.isReloadOp(ir2)) {
        //e.g.:
        //    stpr $246:u64 (r8)
        //        ld:u64:storage_space(stack) 'func_level_var_40'
        PRNO ir1_prno = ir1->getPrno();
        PRNO ir2_prno = ir2->getPrno();
        ASSERT0(ir1_prno != PRNO_UNDEF && ir2_prno != PRNO_UNDEF);
        if (ir1_prno == ir2_prno &&
            m_ra.getReg(ir1_prno) == m_ra.getReg(ir2_prno) &&
            LD_idinfo(ir1->getRHS()) == LD_idinfo(ir2->getRHS())) {
            return false;
        }
        return true;
    }
    if (m_ra.isMoveOp(ir1) && m_ra.isMoveOp(ir2)) {
        //e.g.:
        //    stpr $6125:i32 (r6)
        //        $6197:i32 (r4)
        PRNO ir1_prno = ir1->getPrno();
        PRNO ir2_prno = ir2->getPrno();
        PRNO ir1_rhs_prno = ir1->getRHS()->getPrno();
        PRNO ir2_rhs_prno = ir2->getRHS()->getPrno();
        ASSERT0(ir1_prno != PRNO_UNDEF && ir2_prno != PRNO_UNDEF &&
                ir1_rhs_prno != PRNO_UNDEF && ir2_rhs_prno != PRNO_UNDEF);
        if (ir1_prno == ir2_prno && ir1_rhs_prno == ir2_rhs_prno &&
            m_ra.getReg(ir1_prno) == m_ra.getReg(ir2_prno) &&
            m_ra.getReg(ir1_rhs_prno) == m_ra.getReg(ir2_rhs_prno)) {
            return false;
        }
        return true;
    }
    if (m_ra.isSpillOp(ir1) && m_ra.isSpillOp(ir2)) {
        //e.g.:
        //    st:u64:storage_space(stack) 'func_level_var_35'
        //        $240:u64 (r8)
        PRNO ir1_prno = ir1->getRHS()->getPrno();
        PRNO ir2_prno = ir2->getRHS()->getPrno();
        ASSERT0(ir1_prno != PRNO_UNDEF && ir2_prno != PRNO_UNDEF);
        if (ir1_prno == ir2_prno && ST_idinfo(ir1) == ST_idinfo(ir2) &&
            m_ra.getReg(ir1_prno) == m_ra.getReg(ir2_prno)) {
            return false;
        }
    }
    return true;
}


bool LSRAImpl::mergeCommonIRAtBlockEnd(Vector<IRBB*> & latch_vec,
                                       MOD IRBB * base_bb, UINT com_ir_num)
{
    ASSERT0(base_bb != nullptr);

    bool changed = false;
    UINT latchbb_nums = latch_vec.get_elem_count();

    if (com_ir_num == 0 || latchbb_nums == 0) { return false; }

    //1.Select the IR sequence to be merged.
    //If any BB in the set fallthrough to 'base_bb', use its identical IRs as
    //the merged IRs to avoid generating redundant branch instructions.
    IRBB * ft_bb = m_cfg->getFallThroughPrevBB(base_bb);
    IRBB * target_bb = latch_vec[latchbb_nums - 1];
    ASSERT0(ft_bb != nullptr && target_bb != nullptr);

    for (VecIdx i = 0; i < (VecIdx)latchbb_nums; i++) {
        if (latch_vec[i] != ft_bb) { continue; }
        target_bb = ft_bb;
        break;
    }

    //2.Remove IRs to be merged from the other BBs.
    for (VecIdx i = 0; i < (VecIdx)latchbb_nums; i++) {
        if (latch_vec[i] == target_bb) { continue; }
        BBIRList & irlst = latch_vec[i]->getIRList();
        ASSERT0(irlst.get_elem_count() >= com_ir_num);

        for (int j = com_ir_num; j > 0; --j) {
            //Remove the IR and clear its record in LSRA.
            IR * remove_ir = irlst.remove_tail();
            changed |= eliminateSingleIR(remove_ir);
        }
    }

    //3.Generate the merged BB.
    IRBB * new_bb = nullptr;
    BBIRListIter bbirit;
    IRCfgOptCtx coctx(getOptCtx());

    //Extract common IRs from the target BB and move them into a new BB.
    if (target_bb->getIRList().get_elem_count() > com_ir_num) {
        //Finding the Spill Point.
        BBIRList & irlst = target_bb->getIRList();
        UINT i = 0;
        for (irlst.get_tail(&bbirit); i < com_ir_num;
             ++i, irlst.get_prev(&bbirit)) {}

        //Invalid DomInfo to inform CFG related API to stop update.
        m_oc->setInvalidDom();
        m_oc->setInvalidPDom();

        new_bb = m_cfg->splitBB(target_bb, bbirit, *m_oc);
    } else {
        //If the target BB contains only common IRs, use the target
        //BB as the merged BB.
        new_bb = target_bb;
    }

    LabelInfo const* li = m_rg->genILabel();
    m_cfg->addLabel(new_bb, li);

    //4.Update the CFG to make latch BBs point to the newly merged BB.
    List<IRBB*> preds;
    for (VecIdx i = 0; i < (VecIdx)latchbb_nums; i++) {
        if (latch_vec[i] == target_bb) { continue; }

        IRBB * goto_bb = m_cfg->getFallThroughBB(latch_vec[i]);
        ASSERT0(goto_bb != nullptr || isTrampBB(goto_bb));

        if (latch_vec[i]->getIRList().get_elem_count() != 0) {
            //Case1:If the BB is not empty after removing common IRs,
            //update its goto BB to point to the new BB.
            IR * goto_ir = goto_bb->getLastIR();
            GOTO_lab(goto_ir) = li;
            m_cfg->addEdge(goto_bb, new_bb, coctx);
            m_cfg->removeEdge(goto_bb, base_bb, coctx);
        } else {
            //Case2:If the BB is empty after removing common IRs,
            //update its predecessor BBs to point to the new BB,
            //remove the current BB and its goto BB.
            m_cfg->moveLabels(latch_vec[i], new_bb);

            preds.clean();
            m_cfg->get_preds(preds, latch_vec[i]);
            for (IRBB * pred = preds.get_head();
                 pred != nullptr; pred = preds.get_next()) {
                //The case where there is a fallthrough predecessor BB
                //is not considered.
                ASSERT0(pred->getLastIR()->isBranch());
                m_cfg->addEdge(pred, new_bb, coctx);
                m_cfg->removeEdge(pred, latch_vec[i], coctx);
            }

            m_cfg->removeEdge(goto_bb, base_bb, coctx);
            m_cfg->removeEdge(latch_vec[i], goto_bb, coctx);
            m_cfg->removeBB(goto_bb, coctx);
            m_cfg->removeBB(latch_vec[i], coctx);
        }
    }
    return changed;
}


UINT LSRAImpl::getPredLatchBB(IRBB const* bb, MOD Vector<IRBB*> & latch_vec)
{
    ASSERT0(bb != nullptr);

    latch_vec.clean();
    for (UINT i = 0; i < bb->getNumOfPred(); ++i) {
         IRBB * pred_bb = m_cfg->getNthPred(bb, i);

        if (isTrampBB(pred_bb)) {
            //If the predecessor BB is a tramp BB, the BB that fallthrough
            //to the tramp BB is considered the actual latch BB.
            pred_bb = m_cfg->getFallThroughPrevBB(pred_bb);
        }

        if (!getRA().isLatchBB(pred_bb)) { continue; }

        latch_vec.append(pred_bb);
    }
    return latch_vec.get_elem_count();
}


UINT LSRAImpl::getCommonIRNumsSuffix(Vector<IRBB*> const& bbvec)
{
    UINT com_ir_cnt = 0;
    UINT vec_nums = bbvec.get_elem_count();
    Vector<BBIRList*> irlsts(vec_nums);
    Vector<BBIRListIter> irits(vec_nums);

    for (UINT i = 0; i < vec_nums; ++i) {
        irlsts[i] = &bbvec[i]->getIRList();
        irlsts[i]->get_tail(&irits[i]);
    }

    for (; irits[0] != nullptr; irits[0] = irlsts[0]->get_prev(irits[0])) {
        IR * ir = irits[0]->val();
        bool is_diff = false;

        //Traverse all BBs and check whether the IR at the current
        //position is identical.
        for (UINT i = 1; i < vec_nums; i++) {
            if (irits[i] == nullptr || irits[i]->val() == nullptr ||
                isDifferentIROp(ir, irits[i]->val())) {
                is_diff = true;
                break;
            }
            irlsts[i]->get_prev(&irits[i]);
        }

        //Return immediately when the first different IR is encountered.
        if (is_diff) { break; }
        com_ir_cnt++;
    }

    return com_ir_cnt;
}


bool LSRAImpl::optimizeCommonIRAtLatchBBEnd()
{
    bool changed = false;
    BBListIter bbit;
    Vector<IRBB *> latch_vec;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {

        //When predecessor is less than 2, optimization cannot be applied.
        if (bb->getNumOfPred() < 2) { continue; }

        //Find and record the predecessor latch BBs of the current BB.
        //If there are fewer than 2 predecessor latch BBs,
        //optimization cannot be applied.
        if (getPredLatchBB(bb, latch_vec) < 2) { continue; }

        //Calculate the number of identical IRs at the end of latch BBs.
        UINT com_ir_cnt = getCommonIRNumsSuffix(latch_vec);

        if (com_ir_cnt == 0) { continue; }

        //Merge identical IRs at the end of BBs in the vector.
        changed |= mergeCommonIRAtBlockEnd(latch_vec, bb, com_ir_cnt);
    }
    return changed;
}


bool LSRAImpl::optimizeCommonIRAtBlockBegin()
{
    bool changed = false;
    BBListIter bbit;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {

        //Optimization applies only when the number of successors equals 2.
        if (bb->getNumOfSucc() != 2) { continue; }

        ASSERT0(bb->getLastIR()->isBranch());

        IRBB * succ_bb1 = m_cfg->getNthSucc(bb, 0);
        IRBB * succ_bb2 = m_cfg->getNthSucc(bb, 1);

        if (!getRA().isLatchBB(succ_bb1) || !getRA().isLatchBB(succ_bb2) ||
            succ_bb1->getNumOfPred() != 1 || succ_bb2->getNumOfPred() != 1) {
            continue;
        }

        BBIRList & irlst1 = succ_bb1->getIRList();
        BBIRList & irlst2 = succ_bb2->getIRList();
        BBIRListIter bbirit1;
        BBIRListIter bbirit2;
        IR * ir1 = irlst1.get_head(&bbirit1);
        IR * ir2 = irlst2.get_head(&bbirit2);
        UINT com_ir_cnt = 0;

        //Calculate the number of common IRs at the begin of BBs.
        for (; ir1 != nullptr && ir2 != nullptr;
             ir1 = irlst1.get_next(&bbirit1), ir2 = irlst2.get_next(&bbirit2)) {
            if (isDifferentIROp(ir1, ir2)) { break; }
            com_ir_cnt++;
        }

        while(com_ir_cnt--) {
            //Move ir1 into the bb.
            ir1 = irlst1.remove_head();
            bb->getIRList().append_tail_ex(ir1);

            //Delete ir2 from the irlist2.
            ir2 = irlst2.remove_head();
            changed |= eliminateSingleIR(ir2);
        }
    }
    return changed;
}


bool LSRAImpl::optimizeCommonIRAtBBBoundaries()
{
    bool changed = false;

    //Optimize common IRs in predecessor latch BBs.
    changed |= optimizeCommonIRAtLatchBBEnd();

    //Optimize common IRs in successor BBs.
    changed |= optimizeCommonIRAtBlockBegin();

    return changed;
}


bool LSRAImpl::replaceSpillAndReloadToMov()
{
    IRPairLst * pair_lst = m_spill_reload_eliminate_mgr.getSpillReloadPairs();
    if (pair_lst == nullptr) { return false; }

    //Try to get available register to replace spill and reload IRs into mov
    //operations. Exit if it not exists.
    Reg reg_replace = m_ra.getAvaiRegForLsraOpt();
    if (reg_replace == REG_UNDEF) { return false; }

    bool changed = false;
    TypeMgr * tm = m_rg->getTypeMgr();
    ASSERT0(tm);
    xcom::List<IRBB*> bblst;
    for (IRPair * pair = pair_lst->get_head(); pair != nullptr;
         pair = pair_lst->get_next()) {
        IR * spill = pair->spill;
        IR * reload = pair->reload;
        ASSERT0(spill && reload && spill->getType() && reload->getType());
        ASSERT0(spill->getIdinfo() == reload->getRHS()->getIdinfo());
        ASSERT0(spill->getType() == reload->getType());
        ASSERT0(spill->getBB() && reload->getBB() &&
                spill->getBB() == reload->getBB());
        IRBB * bb = spill->getBB();
        ASSERT0(bb);

        //Current basic block has used this register and cannot use it again.
        if (bblst.find(bb)) { continue; }
        bblst.append_tail(bb);

        Type const* tp = spill->getType();
        ASSERT0(tp);
        PRNO prno = m_ra.buildPrnoAndSetReg(tp, reg_replace);
        IR * replace_spill = m_irmgr->buildStorePR(prno, tp,
            m_rg->dupIRTree(spill->getRHS()));
        IR * replace_reload = m_irmgr->buildStorePR(reload->getPrno(), tp,
            m_irmgr->buildPRdedicated(prno, tp));
        bb->getIRList().insert_before(replace_spill, spill);
        bb->getIRList().insert_before(replace_reload, reload);
        bb->getIRList().remove(spill);
        bb->getIRList().remove(reload);
        changed |= true;
    }
    return changed;
}


bool LSRAImpl::hasRegConflict(IR const* ir, Reg reg)
{
    ASSERT0(ir && reg != REG_UNDEF);
    if (!ir->isWritePR()) { return false; }
    PRNO prno = ir->getPrno();
    ASSERT0(prno != PRNO_UNDEF);
    Reg curreg = getRA().getReg(prno);
    ASSERT0(curreg != REG_UNDEF);
    return curreg == reg;
}


bool LSRAImpl::hasStackSlotConflict(IR const* ir, Var const* var,
                                    MOD ConstIRIter & inneririt)
{
    ASSERT0(ir && var);
    inneririt.clean();
    for (IR const* inner_ir = iterInitC(ir, inneririt); inner_ir != nullptr;
         inner_ir = iterNextC(inneririt)) {
        if (!inner_ir->hasIdinfo()) { continue; }
        Var const* curvar = inner_ir->getIdinfo();
        ASSERT0(curvar);
        if (curvar == var) { return true; }
    }
    return false;
}


bool LSRAImpl::hasRegOrStackSlotConflictOrCallInLoopBB(
    IR const* ir, IRBB * bb, MOD ConstIRIter & inneririt)
{
    ASSERT0(bb && ir);
    ASSERT0(getRA().isSpillOp(ir) || getRA().isReloadOp(ir));
    IR const* rhs = ir->getRHS();
    ASSERT0(rhs);
    PRNO prno = getRA().isReloadOp(ir) ? ir->getPrno() : rhs->getPrno();
    ASSERT0(prno != PRNO_UNDEF);
    Reg reg = getRA().getReg(prno);
    ASSERT0(reg != REG_UNDEF);
    Var const* var = getRA().isReloadOp(ir) ? LD_idinfo(rhs) : ST_idinfo(ir);
    ASSERT0(var);
    IRListIter outeririt;
    BBIRList & irlst = bb->getIRList();
    for (IR * cur_ir = irlst.get_head(&outeririt); cur_ir != nullptr;
         cur_ir = irlst.get_next(&outeririt)) {
        if (cur_ir == ir) { continue; }

        //Call stmt may change the register value.
        if (cur_ir->isCallStmt() && m_rsimpl.isCaller(reg)) { return true; }

        //There exists a register conflict in the loop.
        if (hasRegConflict(cur_ir, reg)) { return true; }

        //There exists a stack slot conflict in the loop.
        if (hasStackSlotConflict(cur_ir, var, inneririt)) { return true; }
    }
    return false;
}


bool LSRAImpl::hasRegOrStackSlotConflictOrCallInLoop(
    IR const* ir, LI<IRBB> const* li, MOD ConstIRIter & inneririt)
{
    ASSERT0(ir && li);
    IRCFG * cfg = m_rg->getCFG();
    ASSERT0(cfg);
    xcom::BitSet * bbset = LI_bb_set(li);
    ASSERT0(bbset);
    for (BSIdx idx = bbset->get_first(); idx != BS_UNDEF;
         idx = bbset->get_next(idx)) {
        IRBB * bb = cfg->getBB(idx);
        ASSERT0(bb);
        if (hasRegOrStackSlotConflictOrCallInLoopBB(ir, bb, inneririt)) {
            return true;
        }
    }
    return false;
}


bool LSRAImpl::canBeHoisted(
    IR const* ir, LI<IRBB> const* li, MOD ConstIRIter & inneririt)
{
    ASSERT0(ir);
    if (li == nullptr) { return false; }
    IRBB * loophead = li->getLoopHead();
    ASSERT0(loophead);
    IRBB * curbb = ir->getBB();
    ASSERT0(curbb);

    //IRs in loop head should not be hoisted.
    if (curbb == loophead) { return false; }
    return !hasRegOrStackSlotConflictOrCallInLoop(ir, li, inneririt);
}


LI<IRBB> const* LSRAImpl::findHoistOuterMostLoop(
    IR const* ir, LI<IRBB> const* liroot, MOD ConstIRIter & inneririt)
{
    ASSERT0(ir);
    IRBB * bb = ir->getBB();
    ASSERT0(bb);
    LI<IRBB> const* li = liroot->getInnerMostLoopIncludeBB(bb->id());

    //If there exists register conflict, stack slot conflict or call
    //operation in the loop, do not hoist the IR outside the loop.
    LI<IRBB> const* res = nullptr;
    while (canBeHoisted(ir, li, inneririt)) {
        res = li;
        li = li->getOuter();
    }
    return res;
}


IRBB * LSRAImpl::genBBToHoistSpillReloadOutsideLoop(LI<IRBB> const* li,
    MOD BB2BB & bb2insert, MOD OptCtx & oc)
{
    IRCFG * cfg = m_rg->getCFG();
    ASSERT0(cfg);
    IRBB * header_bb = li->getLoopHead();
    ASSERT0(header_bb);
    if (bb2insert.find(header_bb)) {
        return bb2insert.get(header_bb);
    }
    IRBB * header_prev_bb = cfg->get_idom(header_bb);
    ASSERT0(header_prev_bb);
    IRBB * newbb = m_rg->allocBB();
    BBListIter fromit;
    BBListIter toit;
    cfg->getBBList()->find(header_prev_bb, &fromit);
    cfg->getBBList()->find(header_bb, &toit);
    ASSERT0(fromit && toit);
    BBListIter tmpit = fromit;
    if (!cfg->tryUpdateRPOBeforeCFGChanged(newbb, header_bb, true, &oc)) {
        oc.setInvalidRPO();
    }
    if (cfg->getBBList()->get_next(&tmpit) == header_bb) {
        cfg->insertBBBetween(header_prev_bb, fromit, header_bb, toit, newbb,
                             &oc);
    } else {
        cfg->insertBBBefore(header_bb, newbb);
    }
    bool add_pdom_failed = false;
    cfg->addDomInfoToNewIDom(header_bb->id(), newbb->id(), add_pdom_failed);
    if (add_pdom_failed) {
        oc.setInvalidPDom();
    }
    bb2insert.set(header_bb, newbb);
    getRA().setHoistBB(newbb);
    return newbb;
}


bool LSRAImpl::hoistSpillReloadOutsideLoopImpl(OptCtx & oc, IRTab const& tab)
{
    bool changed = false;
    LI<IRBB> const* liroot = m_cfg->getLoopInfo();
    if (liroot == nullptr) { return changed; } //No loop info.
    BB2BB bb2insert;
    ConstIRIter inneririt;
    IRTabIter irit;
    for (IR const* ir = tab.get_first(irit); ir != nullptr;
         ir = tab.get_next(irit)) {
        IRBB * bb = ir->getBB();
        if (bb == nullptr) { continue; }
        LI<IRBB> const* li = findHoistOuterMostLoop(ir, liroot, inneririt);
        if (li == nullptr) { continue; }

        //Generate a new basic block for hoisting the spill and reload IRs
        //outside the loop.
        IRBB * new_bb = genBBToHoistSpillReloadOutsideLoop(li, bb2insert, oc);
        bb->getIRList().remove(const_cast<IR*>(ir));
        new_bb->getIRList().append_tail_ex(const_cast<IR*>(ir));
        changed = true;
    }

    return changed;
}


bool LSRAImpl::removeRedundantPosGapIR(OptCtx & oc)
{
    PosGapIROpt pos_gap_opt(*this);
    return pos_gap_opt.perform(oc);
}


bool LSRAImpl::hoistSpillReloadOutsideLoop(OptCtx & oc)
{
    ASSERT0L3(m_rg->getCFG()->verifyLoopInfo(oc));
    ASSERT0(oc.is_loopinfo_valid());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_LOOP_INFO,
                                               PASS_UNDEF);
    bool changed = false;
    changed |= hoistSpillReloadOutsideLoopImpl(oc, getRA().getSpillTab());
    changed |= hoistSpillReloadOutsideLoopImpl(oc, getRA().getReloadTab());
    if (changed) { oc.setInvalidLoopInfo(); }
    return changed;
}


bool LSRAImpl::promoteSpillReload(OptCtx & oc)
{
    m_ra.generateRegLifeTime(oc);
    return m_ra.promoteSpillReload(oc, m_rsimpl);
}


bool LSRAImpl::postProcess(OptCtx & oc)
{
    START_TIMER(t, "postProcess");
    bool changed = false;

    //Replace spill and reload IRs to mov operation using available register.
    changed |= replaceSpillAndReloadToMov();

    //Eliminate extra spilling IRs for one-def lifetimes.
    changed |= eliminateSpillOfOneDefLifeTime();

    //Eliminate the redundant spilling IRs.
    changed |= eliminateSameSlotSpillAfterReload();

    //Optimize common IR sequences at basic block boundaries.
    changed |= optimizeCommonIRAtBBBoundaries();

    //Remove the redundant pos gap IR.
    changed |= removeRedundantPosGapIR(oc);

    //Promote the spill/reload by the available registers.
    changed |= promoteSpillReload(oc);

    //Optimize load following a store across BBs into a mov instruction.
    changed |= optimizeCrossBBStoreLoadToMov();

    //Eliminate the redundant Mov IRs.
    changed |= eliminateRedundantMov();

    //Hoist spill and reload IRs outside the loop if possible.
    // changed |= hoistSpillReloadOutsideLoop(oc);

    END_TIMER(t, "postProcess");
    return changed;
}


bool LSRAImpl::perform(OptCtx & oc)
{
    bool changed = false;
    m_oc = &oc;
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_DOM, PASS_PRLIVENESS_MGR, PASS_UNDEF);
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->
        queryPass(PASS_PRLIVENESS_MGR);
    ASSERT0(m_live_mgr && m_live_mgr->is_valid());

    //Compute the lifetime prefer for Register Allocation.
    computeRAPrefer();

    ScanInPosOrder scan(*this);
    scan.perform();

    //Revise lifetime consistency.
    reviseLTConsistency();

    //Remove the unnecessary spill IR insterted due to the fake-use IR at
    //the loop head.
    removeSpillIRForFakeUseAtLexFirstBBOfLoop();

    //Remove the fake-use IR with no code gen attribute after register
    //allocation.
    m_ra.removeFakeUseIR();

    ASSERT0L3(m_ra.verifyLSRAByInterfGraph(oc));
    ASSERT0L3(verifyLSRAOverStrict(oc));

    //TODO: Investigate and fix incorrect dom information caused
    //by pre-optimization.
    m_oc->setInvalidPDom();
    m_oc->setInvalidDom();
    m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_DOM, PASS_UNDEF);

    //Post process after basic allocation.
    bool removed = false;
    removed |= postProcess(oc);

    //Save callee saved registers.
    saveCallee();

    if (!isDomInfoValid()) {
        //Recompute the DOM/PDOM if necessary.
        m_oc->setInvalidPDom();
        m_oc->setInvalidDom();
        m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_DOM, PASS_UNDEF);
    }
    if (m_is_insert_bb) {
        m_live_mgr->set_valid(false);
    }
    changed |= m_is_insert_bb;
    changed |= m_ra.isInsertOp();
    return changed;
}


bool LSRAImpl::verifyLSRAOverStrict(OptCtx & oc) const
{
    RegisterVerify register_verify(m_ra, m_rsimpl, m_rg, m_rg->getCFG());
    ASSERT0(register_verify.verify());
    return true;
}
//END LSRAImpl.


//
//START PosGapIROpt
//
void PosGapIROpt::genDUInfo(OptCtx & oc)
{
    ASSERT0(m_rg->getCFG()->verifyRPO(oc));
    ASSERT0(m_rg->getCFG()->verifyDomAndPdom(oc));
    ASSERT0(oc.is_rpo_valid());

    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_DOM,
        PASS_UNDEF);

    //FIXME START
    //Need the previous PASS to revise the following PASS_LOOP_INFO, PASS_AA,
    //PASS_MD_REF information.
    bool do_aa = g_do_aa;
    bool do_md_du_analysis = g_do_md_du_analysis;
    bool compute_pr_du_chain = g_compute_pr_du_chain;
    bool compute_nonpr_du_chain = g_compute_nonpr_du_chain;
    bool compute_pr_du_chain_by_prssa = g_compute_pr_du_chain_by_prssa;

    g_do_aa = true;
    g_do_md_du_analysis = true;
    g_compute_pr_du_chain = true;
    g_compute_nonpr_du_chain = false;
    g_compute_pr_du_chain_by_prssa = true;
    oc.setInvalidPass(PASS_PRLIVENESS_MGR);
    oc.setInvalidPass(PASS_LOOP_INFO);
    oc.setInvalidPass(PASS_AA);
    oc.setInvalidPass(PASS_MD_REF);
    oc.setInvalidPRDU();
    m_rg->doAA(oc);
    m_rg->doMDRefAndClassicDU(oc);
    g_do_aa = do_aa;
    g_do_md_du_analysis = do_md_du_analysis;
    g_compute_pr_du_chain = compute_pr_du_chain;
    g_compute_nonpr_du_chain = compute_nonpr_du_chain;
    g_compute_pr_du_chain_by_prssa = compute_pr_du_chain_by_prssa;
    //FIXME END
}


static bool isSpillDefByReloadOnly(IR const* ir,
    PosGapIROpt const* pos_gap_opt, MOD IR2BOOL & def_by_reload_only_map)
{
    ASSERT0(ir && ir->is_exp());

    //Check this IR is defined by reload only from the cached map first. This
    //cached map 'def_by_reload_only_map' is used to record the spill IRs if
    //the data of the spilling register is defined by the reload IR only,
    //which means this spilling operation is useless since the data in the
    //spilling register never changed, and it is consistent with the data in
    //the memory. This cached map can be used to avoid the traversing of the
    //whole DU set everytime if it is cached before.
    bool find = false;
    bool rst = def_by_reload_only_map.get(ir, &find);
    if (find) { return rst; }

    //If it is not cached, do the check from the DU set of current IR.
    DUSet const* set = ir->readDUSet();
    ASSERT0(set);
    Region * rg = pos_gap_opt->getRegion();
    ASSERT0(rg);

    DUSetIter di = nullptr;
    rst = true;
    for (BSIdx i = set->get_first(&di); i != BS_UNDEF;
         i = set->get_next(i, &di)) {
        ASSERT0(rg->getIR(i)->is_stmt());
        if (!pos_gap_opt->getRA().isReloadOp(rg->getIR(i))) {
            rst = false;
            break;
        }
    }
    def_by_reload_only_map.set(ir, rst);
    return rst;
}


void PosGapIROpt::collectDeadSpillIR()
{
    //The cached map 'def_by_reload_only_map' is used to record the spill IRs
    //if the data of the spilling register is defined by the reload IR only,
    //which means this spilling operation is useless since the data in the
    //spilling register never changed, and it is consistent with the data in
    //the memory. This cached map can be used to avoid the traversing of the
    //whole DU set everytime if it is cached before.
    IR2BOOL def_by_reload_only_map;
    for (IRBB * bb = m_bb_list->get_head(); bb != nullptr;
         bb = m_bb_list->get_next()) {
        for (IR * ir = BB_irlist(bb).get_head();
             ir != nullptr; ir = BB_irlist(bb).get_next()) {
            if (!m_impl.getRA().isReloadOp(ir)) { continue; }

            //Check the use-set of the pr defined by this reload, if the USE
            //IR is spill IR, and the pr this spill IR is defined by reload
            //only, so this spill IR can be reconginzed as a dead spill IR.
            DUSet const* set = ir->readDUSet();
            if (set == nullptr) { continue; }
            DUSetIter di = nullptr;
            for (BSIdx i = set->get_first(&di); i != BS_UNDEF;
                 i = set->get_next(i, &di)) {
                ASSERT0(m_rg->getIR(i)->is_exp());
                IR const* use_stmt = m_rg->getIR(i)->getStmt();
                if (!m_impl.getRA().isSpillOp(use_stmt)) { continue; }
                if (!isSpillDefByReloadOnly(m_rg->getIR(i), this,
                    def_by_reload_only_map)) {
                    continue;
                }
                addDeadSpillIR(use_stmt);
            }
        }
    }
}


void PosGapIROpt::collectDeadMiscIR()
{
    LinearScanRA const& lsra = m_impl.getRA();
    for (IRBB * bb = m_bb_list->get_head(); bb != nullptr;
         bb = m_bb_list->get_next()) {
        for (IR * ir = BB_irlist(bb).get_head();
             ir != nullptr; ir = BB_irlist(bb).get_next()) {
            if (!lsra.isMoveOp(ir) && !lsra.isRematOp(ir)
                && !lsra.isReloadOp(ir)) { continue; }
            DUSet const* set = ir->readDUSet();
            if (set == nullptr || set->is_empty()) {
                //If the use-set is empty, it is a dead reload/remat/move.
                addDeadMiscIR(ir);
                continue;
            }
            //Do the further check for the reload is dead or not.
            if (!lsra.isReloadOp(ir)) { continue; }
            bool is_dead_reload = true;
            DUSetIter di = nullptr;
            for (BSIdx i = set->get_first(&di); i != BS_UNDEF;
                 i = set->get_next(i, &di)) {
                ASSERT0(m_rg->getIR(i)->is_exp());
                IR const* use_stmt = m_rg->getIR(i)->getStmt();
                if (!lsra.isSpillOp(use_stmt)) {
                    //If there is an use IR which is not spill, it is not
                    //a dead reload.
                    is_dead_reload = false;
                    break;
                }
                if (!m_dead_spill_tab.find(use_stmt)) {
                    //If the use IR is a spill IR, but it is not a dead spill
                    //IR, so this reload IR is not dead either.
                    is_dead_reload = false;
                    break;
                }
            }
            //Record the dead reload IR.
            if (is_dead_reload) { addDeadMiscIR(ir); }
         }
    }
}


void PosGapIROpt::removeDeadIR()
{
    ConstIRTabIter it;
    for (IR const* ir = m_dead_misc_tab.get_first(it);
         ir != nullptr; ir = m_dead_misc_tab.get_next(it)) {
        m_impl.eliminateSingleIR(ir);
    }
    for (IR const* ir = m_dead_spill_tab.get_first(it);
         ir != nullptr; ir = m_dead_spill_tab.get_next(it)) {
        m_impl.eliminateSingleIR(ir);
    }
}


bool PosGapIROpt::perform(OptCtx & oc)
{
    //1.Generate the Def-Use information.
    genDUInfo(oc);

    //2.Collect the deal spill IRs.
    collectDeadSpillIR();

    //3.Collect the dead reload/remat/move IRs.
    collectDeadMiscIR();

    //4.Remove the recorded IRs in step 2 and step 3.
    removeDeadIR();

    ASSERT0L3(m_rg->getCFG()->verifyLoopInfo(oc));
    ASSERT0(oc.is_loopinfo_valid());

    return m_dead_misc_tab.get_elem_count() > 0 ||
           m_dead_spill_tab.get_elem_count() > 0;
}
//END PosGapIROpt


//
//START RegisterVerify
//
bool RegisterVerify::checkState(PhyReg2VirReg const* input_state,
    Reg reg, PRNO prno) const
{
    ASSERT0(reg != REG_UNDEF && prno != PRNO_UNDEF);
    if (isPhysicalRegisterNotCheck(reg)) { return false; };
    if (input_state->get(reg) == prno) { return false; }

    //May be the virtual register prno has not been initialized.
    if (input_state->get(reg) == PRNO_UNDEF) {
        note(m_rg, "!! Warning in register allocation: prno(%u) has not"
             " been initilized.\n", prno);
        return false;
    }

    //The calculation of lastuse did not take into account the
    //control flow, so in the comparison of input_state, the same
    //physical register reg can be assigned to two pseudo registers
    //prnos whose lifetimes do not conflict, but this is correct,
    //so this situation needs to be addressed.
    //For example:
    //  After processing BB0 in the following figure, r1 is assigned to $a, and
    //  when processing BB2, r1 is assigned to $c. This is correct because the
    //  lifetimes of $a and $c do not conflict, but during verification,
    //  it will be directly transferred from BB0 to BB2. At this time, the
    //  state of BB0 is passed to BB2. When verifying the variable $c in BB2,
    //  an error occurs, which is a false alarm and needs to be ignored.
    //     $a $c          BB0
    //     +     ----------$a<-1 assume assign physical register r1 to $a
    //     |     |         |
    //     |     |         |
    //     |     |         |
    //     |     |         V
    //     |     |        BB1
    //     -     |         store $a
    //        +  |         $c<-2 assume assign physical register r1 to $c
    //        |  |         |
    //        |  |         |
    //        |  |         |
    //        |  |         V
    //        |  |        BB2
    //        -  +-------->$d<-$c * 2 assume assign physical register r1 to $c
    //This code is not used because it may be inaccurate due to
    //lifetime updates.
    //LifeTime * old_lt = m_lsra.getLT(input_state->get(reg));
    //LifeTime * now_lt = m_lsra.getLT(prno);
    //ASSERT0(old_lt != nullptr && now_lt != nullptr);
    //if (!old_lt->is_intersect(now_lt)) {
    //    return false;
    //}
    note(m_rg, "!! Error in register allocation: The physical"
         " register reg(%u) r1 cannot be assigned to two pseudo"
         " registers prno(%u) and prno(%u) with conflicting"
         " lifetimes simultaneously\n", reg, input_state->get(reg), prno);
    return true;
}


void RegisterVerify::computeLastOcc()
{
    BBList const* bblst = m_cfg->getBBList();
    BBListIter bbit;

    //Calculate the last occurence position of a prno in lexical order.
    for (IRBB const* bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        BBIRList & irlst = const_cast<IRBB*>(bb)->getIRList();
        BBIRListIter bbirit;
        for (IR const* ir = irlst.get_head(&bbirit);
             ir != nullptr; ir = irlst.get_next(&bbirit)) {
            //No need to handle spill/reload/remat/move, the occ IR did not
            //encoded with a position and therefore no resided in any lifetime.
            if (m_lsra.isOpInPosGap(ir)) { continue; }
            genLastOccRHS(ir);
            genLastOccLHS(ir);
        }
    }
}


void RegisterVerify::dumpInitInfo() const
{
    note(m_rg, "\n\n==---- DUMP Register Verify Informations ----==");
    note(m_rg, "\nOutput basic informations:");
    note(m_rg, "\n==================================");
    note(m_rg, "\nNote that both physical and virtual register numbers"
         " start from 1, with 0 indicating undefined");
    note(m_rg, "\nBasic block's num is %u", m_cfg->getNumOfBB());
    note(m_rg, "\nphysical register's num is %u", m_physical_reg_num);
    note(m_rg, "\n==================================");
}


void RegisterVerify::dumpPhyReg2VirReg(PhyReg2VirReg const* input_state) const
{
    note(m_rg, "\ninput_state is: [");

    //The physical register number starts from 1.
    for (UINT i = 1; i <= getPhysicalRegNum(); i++) {
        note(m_rg, "{reg(%u) -> prno(%u)}", i, input_state->get(i));
        if (i == getPhysicalRegNum()) {
            note(m_rg, "]");
        } else {
            note(m_rg, ", ");
        }
    }
}


void RegisterVerify::genLastOccRHS(IR const* ir)
{
    ASSERT0(ir && ir->is_stmt());
    IRIter irit;
    for (IR const* e = xoc::iterExpInit(ir, irit); e != nullptr;
         e = xoc::iterExpNext(irit)) {
        if (e->isReadPR()) {
            m_pr2lastocc.set(e->getPrno(), ir);
        }
    }
}


void RegisterVerify::genLastOccLHS(IR const* ir)
{
    ASSERT0(ir && ir->is_stmt());
    IR const* res = const_cast<IR*>(ir)->getResultPR();
    if (res == nullptr) { return; }
    PRNO prno = res->getPrno();
    m_pr2lastocc.set(prno, ir);
}


RegisterVerify::PhyReg2VirReg * RegisterVerify::getAndGenPhyReg2VirReg(
    PhyReg2VirReg const* input_state)
{
    if (input_state == nullptr) { return nullptr; }
    PhyReg2VirReg * new_state = allocPhyReg2VirReg();
    new_state->copy(*input_state);
    return new_state;
}


RegisterVerify::PhyReg2VirReg * RegisterVerify::createPhyReg2VirReg()
{
    //The physical register number 0 indicates undefined,
    //therefore it is ignored. The array index starts from 1.
    PhyReg2VirReg * new_state =
        allocPhyReg2VirReg(getPhysicalRegNum() + 1);
    for (UINT i = 0; i <= getPhysicalRegNum(); i++) {
        new_state->set(i, PRNO_UNDEF);
    }
    return new_state;
}


void RegisterVerify::setPhyReg2VirReg(MOD PhyReg2VirReg * input_state,
    Reg reg, PRNO prno) const
{
    ASSERT0(input_state != nullptr && reg != REG_UNDEF);
    input_state->set(reg, prno);
}


void RegisterVerify::destroyPhyReg2VirReg(PhyReg2VirReg const* input_state)
    const
{
    ASSERT0(input_state != nullptr);
    delete input_state;
}


bool RegisterVerify::isLastOcc(IR const* ir, IR const* input) const
{
    ASSERT0(ir != nullptr && input != nullptr);
    return m_pr2lastocc.get(input->getPrno()) == ir;
}


bool RegisterVerify::isPhysicalRegisterNotCheck(Reg reg) const
{
    ASSERT0(reg != REG_UNDEF);
    if (m_lsra.getFP() == reg || m_lsra.getPC() == reg ||
        m_lsra.getRA() == reg || m_lsra.getSP() == reg ||
        m_lsra.getTA() == reg || m_lsra.getZeroScalar() == reg ||
        m_lsra.getZeroVector() == reg) {
        note(m_rg, "\nreg(%u) does not require verification.", reg);
        return true;
    }
    return false;
}


void RegisterVerify::initPhysicalRegCaller()
{
    ASSERT0(m_physical_reg_num != 0);
    for (UINT i = 0; i <= m_physical_reg_num; i++) {
        if (m_reg_set.isCaller(i)) {
            m_caller_regs.append(i);
        }
    }
}


void RegisterVerify::initPhysicalRegNum()
{
    m_physical_reg_num = m_reg_set.getTotalRegNum();
}


UINT RegisterVerify::getPhysicalRegNum() const
{
    return m_physical_reg_num;
}


void RegisterVerify::processBlock(IRBB const* bb)
{
    ASSERT0(bb != nullptr);

    //Must copy state because it is modified.
    PhyReg2VirReg * input_state = getAndGenPhyReg2VirReg(getPhyReg2VirReg(bb));
    ASSERT0(input_state != nullptr);

    note(m_rg, "\nprocess BB(%u), predecessors is: [", bb->id());
    Vertex const* vex = bb->getVex();
    AdjVertexIter in_iti;
    for (Vertex const* in = Graph::get_first_in_vertex(vex, in_iti);
         in != nullptr; in = Graph::get_next_in_vertex(in_iti)) {
        note(m_rg, " %u ", in->id());
    }
    note(m_rg, "]");
    dumpPhyReg2VirReg(input_state);

    //Process all operations of the block.
    processOperations(bb, input_state);

    //Iterate bb's all successors.
    AdjVertexIter out_iti;
    for (Vertex const* out = Graph::get_first_out_vertex(vex, out_iti);
         out != nullptr; out = Graph::get_next_out_vertex(out_iti)) {
        processSuccessor(m_cfg->getBB(out->id()), input_state);
    }
}


void RegisterVerify::processOperations(IRBB const* bb,
    MOD PhyReg2VirReg * input_state)
{
    bool has_error = false;

    //Visit all IRs of the block.
    ASSERT0(bb != nullptr);
    BBIRList const& irlist = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    for (IR const* ir = irlist.get_head(&irit); ir != nullptr;
         ir = irlist.get_next(&irit)) {
        has_error |= processOperation(ir, input_state);
    }
    if(!has_error) {
        note(m_rg, "\nRegister allocation check passed.");
    }
}


bool RegisterVerify::processOperation(IR const* ir,
    MOD PhyReg2VirReg * input_state)
{
    //Check if input operands are correct.
    IRIter irit;
    bool has_error = false;
    for (IR * input = xoc::iterExpInit(ir, irit); input != nullptr;
         input = xoc::iterExpNext(irit)) {
        if (!input->isReadPR()) { continue; }
        PRNO prno = input->getPrno();
        ASSERT0(prno != PRNO_UNDEF);
        Reg reg = m_lsra.getReg(prno);
        ASSERT0(reg != REG_UNDEF);
        if (m_lsra.getLT(prno) == nullptr) { continue; }
        has_error |= checkState(input_state, reg, prno);
        if (!m_lsra.getLT(prno)->isOccHasDef()) {
            note(m_rg, "\nWARN IR:%u found prno $%u --> r%u due to no"
                 " def", ir->id(), prno, reg);
        }

        ASSERTN(!has_error, ("\nIR:%u found prno $%u --> r%u error",
                ir->id(), prno, reg));

        //If it is the last use, set the status of the physical register
        //to invalid. Because some variables are uninitialized,
        //if this step is not taken, it will be judged as a register
        //allocation failure. After completing this step,
        //it can be determined whether the variable is uninitialized.
        //For example:
        //   (1) mov.u64 $0, 100;
        //   (2) add.u64 $0, $0, 10;
        //   (3) sub.u64 $2, $1, 8;
        //In (3) input_state is {r6 -> $0}, assume r6 -> $1
        //(There is no lift time conflict between $0 and $1,
        //so the same physical register can be allocated), then
        //  a.If the last use has not been calculated:
        //    the lifetime of $0
        //    will continue until the end of the basic block.
        //    At this point, it is determined that $0 and
        //    $1 have been allocated to the same physical register,
        //    resulting in a misjudgment.
        //  b.If the last use has been calculated:
        //    input_state will be modified to {r6 -> PRNO_UNDEF},
        //    and r6 -> $1, At this point,
        //    it is believed that $1 is uninitialized.
        if (!isLastOcc(ir, input)) { continue; }
        note(m_rg, "\nprno(%u)[id(%u)] is last use.",
             input->getPrno(), input->id());
        setPhyReg2VirReg(input_state, reg, PRNO_UNDEF);
    }

    if (ir->isCallStmt()) {
        //Invalidate all caller save registers at calls.
        setAllCallerInvalid(input_state);
    }

    //Process output operands.
    IR const* output = const_cast<IR*>(ir)->getResultPR();
    if (output == nullptr || !output->isPROp()) { return has_error; }
    PRNO prno = output->getPrno();
    ASSERT0(prno != PRNO_UNDEF);
    Reg reg = m_lsra.getReg(prno);
    ASSERT0(reg != REG_UNDEF);
    setPhyReg2VirReg(input_state, reg, prno);
    if (isLastOcc(ir, output)) {
        note(m_rg, "\nprno(%u)[id(%u)] is last def use.",
             output->getPrno(), output->id());
        setPhyReg2VirReg(input_state, reg, PRNO_UNDEF);
    }

    return has_error;
}


void RegisterVerify::processSuccessor(IRBB const* bb,
    PhyReg2VirReg const* input_state)
{
    ASSERT0(bb != nullptr && input_state != nullptr);
    PhyReg2VirReg * saved_state =
        const_cast<PhyReg2VirReg*>(getPhyReg2VirReg(bb));

    if (saved_state == nullptr) {
        //The basic block corresponding to bbid has not been traversed,
        //so set initial input_state.
        setPhyReg2VirRegs(bb, getAndGenPhyReg2VirReg(input_state));
        pushWorkList(bb);
        return;
    }

    //The basic block corresponding to bbid has been traversed.
    //Check if new input_state is consistent with saved_state.
    bool saved_state_correct = true;
    for (UINT i = 1; i <= getPhysicalRegNum(); i++) {
        //Current input_state and previous saved_state assume a
        //different interval in this register -> assume that
        //this register is invalid.
        if (saved_state->get(i) != input_state->get(i) &&
            saved_state->get(i) != PRNO_UNDEF) {
            saved_state->set(i, PRNO_UNDEF);
            saved_state_correct = false;
            note(m_rg, "\nprocess_successor B(%d):"
                 " invalidating slot/reg(%d).", bb->id(), i);
        }
    }

    if (saved_state_correct) {
        //Already processed block with correct input_state.
        note(m_rg, "\nprocess_successor B(%d): "
             "previous visit already correct.", bb->id());
        return;
    }

    //Must re-visit this block.
    note(m_rg, "\nprocess_successor B(%d): must re-visit"
         " because input state changed.", bb->id());
    pushWorkList(bb);
}


void RegisterVerify::destroyPhyReg2VirRegs() const
{
    xcom::List<PhyReg2VirReg const*>::Iter irit;
    PhyReg2VirReg const* pv = nullptr;
    for (pv = m_input_states.get_head(&irit); pv != nullptr;
         pv = m_input_states.get_next(&irit)) {
        destroyPhyReg2VirReg(pv);
    }
}


RegisterVerify::PhyReg2VirReg const* RegisterVerify::getPhyReg2VirReg(
    IRBB const* bb) const
{
    ASSERT0(bb != nullptr);
    return m_saved_states.get(bb);
}


void RegisterVerify::setAllCallerInvalid(MOD PhyReg2VirReg * input_state) const
{
    ASSERT0(input_state != nullptr);
    //Invalidate all caller save registers at calls.
    for (UINT i = 0; i < m_caller_regs.get_elem_count(); i++) {
        setPhyReg2VirReg(input_state, m_caller_regs.get(i), PRNO_UNDEF);
    }
}


void RegisterVerify::setPhyReg2VirRegs(IRBB const* bb,
    IN PhyReg2VirReg * saved_state)
{
    ASSERT0(bb != nullptr);
    m_saved_states.setAlways(bb, saved_state);
}


void RegisterVerify::setupInputStateForEntryBB(MOD PhyReg2VirReg * input_state)
    const
{
    //Temporarily not verifying parameter registers.
    //TODO: Setup input registers (method arguments) for first block.
}


void RegisterVerify::verify(IRBB const* start)
{
    ASSERT0(start != nullptr);
    PhyReg2VirReg * input_state = createPhyReg2VirReg();
    ASSERT0(input_state != nullptr);
    computeLastOcc();

    //Setup input registers (method arguments) for first block.
    setupInputStateForEntryBB(input_state);
    setPhyReg2VirRegs(start, input_state);
    pushWorkList(start);

    //Traverse CFG for verification.
    do {
        IRBB const* bb = popWorkList();
        processBlock(bb);
    } while (!isEmptyWorkList());
}


IRBB const* RegisterVerify::popWorkList()
{
    ASSERT0(m_work_list.get_elem_count() > 0);
    return m_work_list.remove_head();
}


void RegisterVerify::pushWorkList(IRBB const* bb)
{
    ASSERT0(bb != nullptr);
    if (!m_work_list.find(bb)) { m_work_list.append_tail(bb); }
}


bool RegisterVerify::isEmptyWorkList() const
{
    return m_work_list.get_elem_count() == 0;
}


bool RegisterVerify::verify()
{
    if (!g_enable_lsra_over_strict) { return true; }
    dumpInitInfo();
    IRBB const* start = m_cfg->getEntry();
    ASSERT0(start != nullptr);
    verify(start);
    return true;
}
//End RegisterVerify

} //namespace xoc
