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
#include "../../opt/comopt.h"
#include "../../reader/grreader.h"

static bool readGR(CHAR * gr_file_name)
{
    ASSERT0(gr_file_name);
    xoc::RegionMgr * rm = new xoc::RegionMgr();
    rm->initVarMgr();
    rm->initIRDescFlagSet();
    rm->getLogMgr()->init("tmp.log", true);
    bool succ = xoc::readGRAndConstructRegion(rm, gr_file_name);
    if (!succ) {
        xoc::prt2C("\nread gr file %s failed\n", gr_file_name);
        return false;
    }

    xoc::LogMgr * lm = rm->getLogMgr();
    //Normalizing and dump new GR file.
    for (UINT i = 0; i < rm->getNumOfRegion(); i++) {
        xoc::Region * rg = rm->getRegion(i);
        if (rg == NULL) { continue; }
        if (rg->is_program() && succ) {
            //rg->dump(true);
            xcom::StrBuf b(64);
            b.strcat(gr_file_name);
            b.strcat(".gr");
            UNLINK(b.buf);
            FILE * gr = ::fopen(b.buf, "w");
            lm->push(gr, b.buf);
            rg->dumpGR(true);
            lm->pop();
            ::fclose(gr);
            xoc::prt2C("\noutput is %s\n", b.buf);
        }
        if (rg->getPassMgr() != NULL) {
            xoc::PRSSAMgr * ssamgr = (PRSSAMgr*)rg->getPassMgr()->queryPass(
                                         PASS_PRSSA_MGR);
            if (ssamgr != NULL && ssamgr->is_valid()) {
                OptCtx * loc = rm->getAndGenOptCtx(rg);
                ssamgr->destruction(*loc);
            }
        }
    }

    delete rm;
    return true;
}


void usage()
{
    printf("\nInput a GR file, output a new GR file with normalized format.");
    printf("\nreader <input-grfile-name>");
    printf("\n");
}


int main(int argc, char * argv[])
{
    if (argc != 2) {
        usage();
        return 1;
    }
    if (!readGR(argv[1])) {
        return 2;
    }
    return 0;
}

