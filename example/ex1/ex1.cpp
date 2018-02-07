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
    Region * topru = rm->newRegion(REGION_PROGRAM);
    rm->addToRegionTab(topru);
    topru->setRegionVar(rm->getVarMgr()->registerVar(
        "program", rm->getTypeMgr()->getMCType(0), 0, VAR_GLOBAL|VAR_FAKE));

    //Generate region for function.
    Region * func_ru = rm->allocRegion(REGION_FUNC);
    func_ru->setRegionVar(rm->getVarMgr()->registerVar(
        ".function", rm->getTypeMgr()->getMCType(0), 0, VAR_GLOBAL|VAR_FAKE));

    IR * ir = topru->buildRegion(func_ru);
    topru->addToIRList(ir);

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
    VAR * g = rm->getVarMgr()->registerVar("g",
        rm->getTypeMgr()->getI32(),0,VAR_GLOBAL);
    VAR * q = rm->getVarMgr()->registerVar("q",
            rm->getTypeMgr()->getI32(),0,VAR_GLOBAL);
    Type const* i32ty = rm->getTypeMgr()->getI32();
    Type const* u32ty = rm->getTypeMgr()->getU32();

    //Load g with i32 type
    IR * ld_exp = func_ru->buildLoad(g, i32ty);

    //Store q with i32 type
    IR * st_stmt = func_ru->buildStore(q, ld_exp);

    //Record IR stmt in an IR-list of region
    func_ru->addToIRList(st_stmt);

    //Build g = g + 1
    IR * true_stmt = func_ru->buildStore(g,
        func_ru->buildBinaryOp(IR_ADD,
            i32ty,
            func_ru->buildLoad(g),
            func_ru->buildImmInt(1, i32ty)));

    //Build g = g - 1
    IR * false_stmt = func_ru->buildStore(g,
        func_ru->buildBinaryOp(IR_SUB,
            i32ty,
            func_ru->buildLoad(g),
            func_ru->buildImmInt(1, i32ty)));

    //Build q = 30
    IR * false_stmt_2 = func_ru->buildStore(q,
        func_ru->buildImmInt(30, i32ty));

    //Chain false_stmt and false_stmt_2 into a list.
    add_next(&false_stmt, false_stmt_2);

    //Build q >= 20
    IR * det_exp = func_ru->buildCmp(IR_GE,
        func_ru->buildLoad(q),
        func_ru->buildImmInt(20, i32ty));

    //Build IF stmt
    IR * ifstmt = func_ru->buildIf(det_exp, true_stmt, false_stmt);

    //Record IR stmt in an IR-list of region
    func_ru->addToIRList(ifstmt);

    //Build return stmt
    //Note the type of current g becomes u32
    IR * ret = REGION_ru(ir)->buildReturn(func_ru->buildLoad(g, u32ty));

    //Record IR stmt in an IR-list of region
    func_ru->addToIRList(ret);
}


static void dumpGR(Region * rg)
{
    g_indent = 0;
    xcom::StrBuf b(64);
    b.strcat("tmp.gr");
    FILE * gr = fopen(b.buf, "a");
    FILE * oldvalue = g_tfile;
    g_tfile = gr;
    rg->dumpGR(true);
    fclose(gr);
    g_tfile = oldvalue;
}


int main(int argc, char * argv[])
{
    g_dbx_mgr = new DbxMgr();
    RegionMgr * rm = new RegionMgr();
    rm->initVarMgr();

    printf("\nGenerate region");

    initdump("tmp.log", true);

    //Generate region.
    generate_region(rm);

    //Dump All regions to tmp.log
    rm->dump(true);

    printf("\nProcess region");

    //Compile region.
    OptCtx oc;
    bool s = rm->processProgramRegion(rm->getRegion(1), &oc);
    ASSERT0(s);

    //Dump GR file.
    dumpGR(rm->getRegion(1));

    delete rm;
    delete g_dbx_mgr;
    g_dbx_mgr = NULL;
    printf("\nFinish\n");

    finidump();

    return 0;
}

