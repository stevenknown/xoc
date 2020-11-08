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
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexProto.h"
#include "liropcode.h"
#include "lir.h"
#include "d2d_comm.h"
#include "drAlloc.h"
#include "cominc.h"
#include "comopt.h"
#include "dex.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"

Pass * DexPassMgr::allocCFG()
{
    BBList * bbl = m_rg->getBBList();
    UINT n = MAX(8, xcom::getNearestPowerOf2(bbl->get_elem_count()));
    return new DEX_CFG(C_SEME, bbl, m_rg, n, n);
}


Pass * DexPassMgr::allocDCE()
{
    DeadCodeElim * pass = new DeadCodeElim(m_rg);
    pass->set_elim_cfs(true);
    return pass;
}


Pass * DexPassMgr::allocCopyProp()
{
    return new DEX_CP(m_rg);
}


Pass * DexPassMgr::allocRP()
{
    Pass * pass = new DEX_RP(m_rg, (GVN*)registerPass(PASS_GVN));
    return pass;
}


int xtime = 1;
void DexPassMgr::performScalarOpt(OptCtx & oc)
{
    List<Pass*> passlist; //A list of optimization.
    PRSSAMgr * ssamgr = (PRSSAMgr*)registerPass(PASS_PR_SSA_MGR);
    bool is_ssa_avail = false;
    if (ssamgr != nullptr) {
        is_ssa_avail = ssamgr->is_valid();
    }

    passlist.append_tail(registerPass(PASS_CP));
    passlist.append_tail(registerPass(PASS_DCE));
    passlist.append_tail(registerPass(PASS_RP));
    passlist.append_tail(registerPass(PASS_CP));
    passlist.append_tail(registerPass(PASS_DCE));
    passlist.append_tail(registerPass(PASS_RP));
    passlist.append_tail(registerPass(PASS_CP));
    passlist.append_tail(registerPass(PASS_DCE));
    passlist.append_tail(registerPass(PASS_LOOP_CVT));
    passlist.append_tail(registerPass(PASS_LICM));
    passlist.append_tail(registerPass(PASS_GCSE));

    ((DeadCodeElim*)registerPass(PASS_DCE))->set_elim_cfs(false);

    if (passlist.get_elem_count() != 0) {
        LOG("\tScalar optimizations for '%s'", m_rg->getRegionName());
    }

    bool change;
    UINT count = 0;
    //do {
        change = false;
        for (Pass * pass = passlist.get_head();
             pass != nullptr; pass = passlist.get_next()) {
            CHAR const* passname = pass->getPassName();
            LOG("\t\tpass %s", passname);
            ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
            ULONGLONG t = getusec();

            //dumpBBList(m_rg->getBBList(), m_rg, "before");
            //m_rg->getCFG()->dumpVCG("before.vcg");

            bool doit = pass->perform(oc);

            //dumpBBList(m_rg->getBBList(), m_rg, "after");
            //m_rg->getCFG()->dumpVCG("after.vcg");

            appendTimeInfo(pass->getPassName(), getusec() - t);
            if (doit) {
                LOG("\t\t\tchanged");
                change = true;
                ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
                ASSERT0(m_rg->getCFG()->verify());
            }
        }
        count++;
    //} while (change && count < 20);
    //ASSERT0(!change);
}
