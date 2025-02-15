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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//ir: stmt or expression.
static IR * dupIsomoVDef(
    IR const* ir, IR * realuse, IR * dummyuse, Region * rg)
{
    IRMgrExt * irmgrext = (IRMgrExt*)rg->getIRMgr();
    ASSERT0(ir);
    Type const* ty = ir->getType();
    if (realuse != nullptr && !realuse->is_dummyuse()) {
        realuse = irmgrext->buildDummyUse(realuse);
    }
    if (dummyuse != nullptr) {
        dummyuse = irmgrext->buildDummyUse(dummyuse);
    }
    if (ir->isPROp()) {
        return irmgrext->buildVStorePR(ir->getPrno(), realuse, dummyuse, ty);
    }
    if (ir->isDirectMemOp()) {
        return irmgrext->buildVStore(
            ir->getIdinfo(), ir->getOffset(), realuse, dummyuse, ty);
    }
    ASSERT0(ir->isIndirectMemOp());
    //This base IR may be used in other operations.
    return irmgrext->buildVIStore(rg->dupIRTree(ir->getBase()),
        ir->getOffset(), realuse, dummyuse, ty);
}


static IR * genPreVDefByRemainRes(
    IR const* rhs, IR const* remain_res, Region * rg)
{
    ASSERT0(rhs && rhs->is_exp() && remain_res);
    IR * realuse_of_stmt = rg->dupIRTree(rhs);
    if (rhs->isUseIsomoExp(remain_res, rg->getIRMgr())) {
        xcom::add_next(&realuse_of_stmt, rg->dupIsomoExpTree(remain_res));
    }
    return dupIsomoVDef(remain_res, realuse_of_stmt, nullptr, rg);
}


IR * MultiResConvert::convertMultiResMemMem(IR * stmt, IR * res1)
{
    ASSERT0(res1->is_stpr() && stmt->isMemRefNonPR());
    IR * retlst = nullptr;
    IR * last = nullptr;
    //CASE1:
    //  res1, res2 <- OP
    //=>
    //  res1 vst<- NULL              #S1
    //  res2 <- OP, dummy_use(res1)  #S2
    //  res1 vst<- res1, res2        #S3 (optional)
    //
    //CASE2:
    //  res1, res2 <- OP, res1
    //=>
    //  res1 vst<- res1              #S1
    //  res2 <- OP, res1             #S2
    //  res1 vst<- res1, res2        #S3 (optional)
    //
    //CASE3:
    //  res1, res2 <- OP, src, res1
    //=>
    //  res1 vst<- src, res1         #S1
    //  res2 <- OP, src, res1        #S2
    //  res1 vst<- res1, res2        #S3 (optional)

    //Build #S1.
    //  res1 vst<- NULL|res1
    ASSERT0(stmt->getRHS());
    IR * dummy_def1 = genPreVDefByRemainRes(stmt->getRHS(), res1, m_rg);
    xcom::add_next(&retlst, &last, dummy_def1);

    //Build #S2.
    //  res2 <- OP, dummy_use(res1)
    xcom::add_next(&retlst, &last, stmt);

    //Build #S3.
    //  res1 vst<- res1, res2
    IR * rhs = m_rg->dupIsomoExpTree(res1);
    xcom::add_next(&rhs, m_rg->dupIsomoExpTree(stmt));
    IR * dummy_def2 = m_rg->dupIsomoStmt(res1, rhs);
    xcom::add_next(&retlst, &last, dummy_def2);

    return retlst;
}


IR * MultiResConvert::genPreDefStmt(
    IR * stmt, IR * res_isomo_list, OUT IR ** predeflst)
{
    IR * last = nullptr;
    IR * last2 = nullptr;
    IR * new_res_isomo_list = nullptr;
    IRMgr * irmgr = getIRMgr();
    ASSERT0(stmt->hasRHS());
    IR const* rhs = stmt->getRHS();
    ASSERT0(rhs);
    for (IR * res = xcom::removehead(&res_isomo_list); res != nullptr;
         res = xcom::removehead(&res_isomo_list)) {
        if (res->isIsomoTo(stmt, irmgr, false, ISOMO_CK_MEMREF_NAME)) {
            continue;
        }
        IR * vdef = genPreVDefByRemainRes(rhs, res, m_rg);
        xcom::add_next(predeflst, &last, vdef);
        xcom::add_next(&new_res_isomo_list, &last2, res);
    }
    return new_res_isomo_list;
}


IR * MultiResConvert::genExtractStmtList(IR * stmt, IR * res_isomo_list)
{
    ASSERT0(stmt && stmt->is_stmt());
    IR * last = nullptr;
    IR * postdeflst = nullptr;
    UINT i = 0;
    Type const* u32 = m_tm->getU32();
    for (IR * res = res_isomo_list; res != nullptr;
         res = res->get_next(), i++) {
        IR * dummyrhs = m_rg->dupIsomoExpTree(stmt);
        xcom::add_next(&dummyrhs, m_irmgr->buildImmInt(i, u32));
        IR * vdef = dupIsomoVDef(res, dummyrhs, nullptr, m_rg);
        xcom::add_next(&postdeflst, &last, vdef);
    }
    return postdeflst;
}


IR * MultiResConvert::genPostDefStmt(IR * stmt, IR * res_isomo_list)
{
    ASSERT0(stmt && stmt->is_stmt());
    IR * last = nullptr;
    IR * postdeflst = nullptr;
    IR * prev_res = stmt;
    for (IR * res = res_isomo_list; res != nullptr;
         res = res->get_next()) {
        if (res->isIsomoTo(stmt, m_irmgr, false, ISOMO_CK_MEMREF_NAME)) {
            continue;
        }
        IR * dummyuse = m_rg->dupIsomoExpTree(prev_res);
        IR * vdef = dupIsomoVDef(res, nullptr, dummyuse, m_rg);
        xcom::add_next(&postdeflst, &last, vdef);
        prev_res = res;
    }
    return postdeflst;
}


IR * MultiResConvert::convertMultiResByExtractRes(IR * stmt)
{
    ASSERT0(stmt && stmt->is_stmt() && stmt->hasRHS());
    ASSERT0(stmt->getRHS()->hasResList());
    IR * res_list = stmt->getRHS()->getResList();
    if (res_list == nullptr) { return stmt; }
    ASSERT0(xcom::cnt_list(res_list) <= getMaxMultiResDescNum());
    IR * postdeflst = genExtractStmtList(stmt, res_list);
    xcom::add_next(&stmt, postdeflst);
    return stmt;
}


IR * MultiResConvert::convertMultiResBySplitRes(IR * stmt, bool genpostdef)
{
    ASSERT0(stmt && stmt->is_stmt() && stmt->hasRHS());
    ASSERT0(stmt->getRHS()->hasResList());
    IR * res_list = stmt->getRHS()->getResList();
    if (res_list == nullptr) { return stmt; }
    stmt->getRHS()->setResList(nullptr);
    ASSERT0(xcom::cnt_list(res_list) <= getMaxMultiResDescNum());
    IR * predeflst = nullptr;
    IR * new_res_list = genPreDefStmt(stmt, res_list, &predeflst);

    //Restore ResDescList to indicates dummy_use of elements in list.
    stmt->getRHS()->setResList(new_res_list);
    xcom::add_next(&predeflst, stmt);
    if (genpostdef) {
        IR * postdeflst = genPostDefStmt(stmt, new_res_list);
        xcom::add_next(&predeflst, postdeflst);
    }
    return predeflst;
}


IR * MultiResConvert::convertMultiResRegMem(IR * stmt, IR * res1)
{
    ASSERT0(res1->is_stpr() && stmt->isMemRefNonPR());
    //  res1, res2 <- OP
    //=>
    //  res1 vstpr<- res1           #S1
    //  res2 <- OP, dummy_use(res1) #S2
    //  res1 vstpr<- res1, res2     #S3
    //NOTE: after PRSSA renaming #S1 and #S3 may be:
    //  resX vstpr<- res1           #S1
    //  res1 vstpr<- resX           #S3
    //Then after register allocation, if res1's physical register is not
    //same with resX's, there need to insert a physical register MOVE to
    //keep syntax legality.
    IR * retlst = nullptr;
    IR * last = nullptr;

    //Build #S1.
    //  res1 vstpr<- NULL|res1
    PRNO defpruse = PRNO_UNDEF;
    if (stmt->isRHSUsePR(res1->getPrno())) {
        defpruse = res1->getPrno();
    }
    Type const* res1ty = res1->getType();
    IR * dummy_def1 = getIRMgr()->buildVStorePR(
        res1->getPrno(), getIRMgr()->buildPRdedicated(defpruse, res1ty),
        nullptr, res1ty);
    xcom::add_next(&retlst, &last, dummy_def1);

    //Build #S2.
    //  res2 <- OP, dummy_use(res1)
    xcom::add_next(&retlst, &last, stmt);

    //Build #S3.
    //  res1 vstpr<- res1, res2
    IR * dummy_def2 = getIRMgr()->buildVStorePR(
        res1->getPrno(), m_rg->dupIsomoExpTree(res1),
        m_rg->dupIsomoExpTree(stmt), res1->getType());
    xcom::add_next(&retlst, &last, dummy_def2);

    return retlst;
}


IR * MultiResConvert::convertMultiResRegReg(IR * stmt, IR * res1)
{
    //  res1, res2 <- ...
    //=>
    //  res1 vstpr<- NULL|res1
    //  res2 <- OP, dummy_use(res1)
    //  res1 <- res1, res2
    ASSERT0(res1->is_stpr() && stmt->is_stpr());
    IR * retlst = nullptr;
    IR * last = nullptr;

    //Build #S1.
    //  res1 vstpr<- NULL|res1
    PRNO src = PRNO_UNDEF;
    if (stmt->isRHSUsePR(res1->getPrno())) {
        src = res1->getPrno();
    }
    Type const* res1ty = res1->getType();
    IR * dummy_def1 = getIRMgr()->buildVStorePR(
        res1->getPrno(), getIRMgr()->buildPRdedicated(src, res1ty),
        nullptr, res1ty);
    xcom::add_next(&retlst, &last, dummy_def1);

    //Build #S2.
    //  res2 <- OP, dummy_use(res1)
    xcom::add_next(&retlst, &last, stmt);

    //Build #S3.
    //  res1 vstpr<- res1, res2
    IR * dummy_def2 = getIRMgr()->buildVStorePR(
        res1->getPrno(), m_rg->dupIsomoExpTree(res1),
        m_rg->dupIsomoExpTree(stmt), res1ty);
    xcom::add_next(&retlst, &last, dummy_def2);

    return retlst;
}


IR * MultiResConvert::buildStorePRWithMultiResAndConvertBySplit(
    PRNO prno, Type const* type, IR * rhs)
{
    IR * stmt = m_irmgr->buildStorePR(prno, type, rhs);
    ASSERT0(rhs->getResList());
    return convertMultiResBySplitRes(stmt, m_is_enable_post_vdef);
}


IR * MultiResConvert::buildStorePRWithMultiResAndConvertBySplit(
    Type const* type, IR * rhs)
{
    IR * stmt = m_irmgr->buildStorePR(type, rhs);
    ASSERT0(rhs->getResList());
    return convertMultiResBySplitRes(stmt, m_is_enable_post_vdef);
}


IR * MultiResConvert::buildStoreWithMultiResAndConvertBySplit(
    Var * lhs, IR * rhs)
{
    IR * stmt = m_irmgr->buildStore(lhs, rhs);
    ASSERT0(rhs->getResList());
    return convertMultiResBySplitRes(stmt, m_is_enable_post_vdef);
}


IR * MultiResConvert::buildStoreWithMultiResAndConvertByExtract(
    Var * lhs, IR * rhs)
{
    IR * stmt = m_irmgr->buildStore(lhs, rhs);
    ASSERT0(rhs->getResList());
    return convertMultiResByExtractRes(stmt);
}


IR * MultiResConvert::buildStoreWithMultiResAndConvertBySplit(
    Var * lhs, Type const* type, IR * rhs)
{
    IR * stmt = m_irmgr->buildStore(lhs, type, rhs);
    ASSERT0(rhs->getResList());
    return convertMultiResBySplitRes(stmt, m_is_enable_post_vdef);
}


bool MultiResConvert::dump() const
{
    m_act_mgr.dump();
    return true;
}

} //namespace xoc
