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
#include "../elfinc.h"

static void usage()
{
    ::fprintf(stdout,
              "\nUsage: readelf.exe file [options]"
              "\noptions: "
              "\n <dumpfile>      output file name"
              "\n");
}


int main(int argc, char const* argv[])
{
    if (argc < 2) { usage(); return 0; }
    CHAR const* elffile = argv[1];
    CHAR const* dumpfile = nullptr;
    if (argc >= 3) {
        dumpfile = argv[2];
    }
    elf::MiscELFMgr em;
    if (dumpfile != nullptr) {
        em.initdumpfile(dumpfile, true);
    } else {
        em.initdumpscr();
    }
    if (!FileObj::isFileExist(elffile)) {
        printf("\nerror:%s not exist\n", elffile);
        return 1;
    }
    if (em.readELF(elffile, true) != elf::EM_SUCC) {
        printf("\nerror:%s read failed\n", elffile);
        return 2;
    }
    bool demo_how_to_write_elf = false;
    if (demo_how_to_write_elf) {
        //Demo write elf.
        xcom::StrBuf buf(32);
        buf.strcat("%s.new.elf", elffile);
        em.writeELF(buf.buf);

        elf::MiscELFMgr em2;
        if (dumpfile != nullptr) {
            em2.initdumpfile("a.elf.dump2", true);
        }
        em2.readELF(buf.buf);
    }
    return 0;
}
