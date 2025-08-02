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
#include "../../opt/cominc.h"
#include "ex1.h"

static void generate_region(RegionMgr * rm)
{
    //Generate region for whole program.
    Region * toprg = rm->newRegion(REGION_PROGRAM);
    toprg->initPassMgr();
    toprg->initIRMgr();
    toprg->initIRBBMgr();
    rm->addToRegionTab(toprg);
    toprg->setRegionVar(rm->getVarMgr()->registerVar(
        "program", rm->getTypeMgr()->getMCType(0), 0, VAR_GLOBAL|VAR_FAKE,
        SS_UNDEF));

    //Generate region for function.
    Region * func = rm->allocRegion(REGION_FUNC);
    func->initPassMgr();
    func->initIRMgr();
    func->initDbxMgr();
    func->initIRBBMgr();
    func->setRegionVar(rm->getVarMgr()->registerVar(
        ".function", rm->getTypeMgr()->getMCType(0), 0, VAR_GLOBAL|VAR_FAKE,
        SS_UNDEF));

    IR * ir = toprg->getIRMgr()->buildRegion(func);
    toprg->addToIRList(ir);

    //----------
    //Generate a list IRs for demonstration.
    //e.g:
    //  i32 g;
    //  i32 q;
    //  q = g;
    //  if (q >= 20) {
    //      g = g + 1;
    //  } else {
    //      g = g - 1;
    //      q = 30;
    //  }
    //  return g:u32;
    Var * g = rm->getVarMgr()->registerVar("g",
        rm->getTypeMgr()->getI32(), 0, VAR_GLOBAL, SS_UNDEF);
    Var * q = rm->getVarMgr()->registerVar("q",
            rm->getTypeMgr()->getI32(), 0, VAR_GLOBAL, SS_UNDEF);
    Type const* i32ty = rm->getTypeMgr()->getI32();
    Type const* u32ty = rm->getTypeMgr()->getU32();
    IRMgr * irmgr = func->getIRMgr();

    //Load g with i32 type
    IR * ld_exp = irmgr->buildLoad(g, i32ty);

    //Store q with i32 type
    IR * st_stmt = irmgr->buildStore(q, ld_exp);

    //Record IR stmt in an IR-list of region
    func->addToIRList(st_stmt);

    //Build g = g + 1
    IR * true_stmt = irmgr->buildStore(g,
        irmgr->buildBinaryOp(IR_ADD,
            i32ty,
            irmgr->buildLoad(g),
            irmgr->buildImmInt(1, i32ty)));

    //Build g = g - 1
    IR * false_stmt = irmgr->buildStore(g,
        irmgr->buildBinaryOp(IR_SUB,
            i32ty,
            irmgr->buildLoad(g),
            irmgr->buildImmInt(1, i32ty)));

    //Build q = 30
    IR * false_stmt_2 = irmgr->buildStore(q,
        irmgr->buildImmInt(30, i32ty));

    //Chain false_stmt and false_stmt_2 into a list.
    add_next(&false_stmt, false_stmt_2);

    //Build q >= 20
    IR * det_exp = irmgr->buildCmp(IR_GE,
        irmgr->buildLoad(q),
        irmgr->buildImmInt(20, i32ty));

    //Build IF stmt
    IR * ifstmt = irmgr->buildIf(det_exp, true_stmt, false_stmt);

    //Record IR stmt in an IR-list of region
    func->addToIRList(ifstmt);

    //Build return stmt
    //Note the type of current g becomes u32
    IR * ret = REGION_ru(ir)->getIRMgr()->buildReturn(
        irmgr->buildLoad(g, u32ty));

    //Record IR stmt in an IR-list of region
    func->addToIRList(ret);
}


static void dumpGR(Region * rg)
{
    xcom::StrBuf b(64);
    b.strcat("tmp.gr");
    FILE * gr = fopen(b.buf, "a");
    rg->getLogMgr()->push(gr, b.buf);
    rg->dumpGR(true);
    rg->getLogMgr()->pop();
    fclose(gr);
}


int main(int argc, char * argv[])
{
    RegionMgr * rm = new RegionMgr();
    rm->getLogMgr()->init("ex.tmp", true);
    rm->initVarMgr();

    printf("\nGENERATE REGION");

    //Generate region.
    generate_region(rm);

    //Dump All regions to tmp.log
    rm->dump(true);

    printf("\nPROCESS REGION");

    //Compile region.
    OptCtx oc(rm->getRegion(1));
    bool s = rm->processProgramRegion(rm->getRegion(1), &oc);
    ASSERT0(s);

    //Dump GR file.
    dumpGR(rm->getRegion(1));

    delete rm;
    printf("\nFINISH\n");

    return 0;
}

