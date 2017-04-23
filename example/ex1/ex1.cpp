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
    Region * topru = rm->newRegion(RU_PROGRAM);
    rm->addToRegionTab(topru);
    topru->set_ru_var(rm->getVarMgr()->registerVar(
        ".program", rm->getTypeMgr()->getMCType(0), 0, VAR_GLOBAL|VAR_FAKE));

    //Generate region for function.
    Region * func_ru = rm->allocRegion(RU_FUNC);
    func_ru->set_ru_var(rm->getVarMgr()->registerVar(
        ".function", rm->getTypeMgr()->getMCType(0), 0, VAR_GLOBAL|VAR_FAKE));

    IR * ir = topru->buildRegion(func_ru);
    topru->addToIRList(ir);

    //----------
    //Generate a list IRs for demonstration.
    //e.g:
    //  i32 g;
    //  i32 q;
    //  q = g; 
    //  return g:u32;
    IR * ld = func_ru->buildLoad( //Load value with I32 type.
        rm->getVarMgr()->registerVar("g",
            rm->getTypeMgr()->getI32(),0,VAR_GLOBAL),
        rm->getTypeMgr()->getI32());

    IR * st = func_ru->buildStore(
        rm->getVarMgr()->registerVar("q",
            rm->getTypeMgr()->getI32(),0,VAR_GLOBAL),
        ld);

    func_ru->addToIRList(st);

    IR * ret = REGION_ru(ir)->buildReturn(
        func_ru->buildLoad(LD_idinfo(ld), 
            rm->getTypeMgr()->getU32())); //Return value with U32 type.

    func_ru->addToIRList(ret);

    //Dump All regions to tmp.log
    rm->dump(true);
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

    printf("\nProcess region");

    //Compile region.
    OptCtx oc;
    bool s = rm->processProgramRegion(rm->get_region(1), &oc);
    ASSERT0(s);

    delete rm;
    delete g_dbx_mgr;
    g_dbx_mgr = NULL;
    printf("\nFinish\n");
    
    finidump();

    return 0;
}

