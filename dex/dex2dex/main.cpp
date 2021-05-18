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

author: GongKai, JinYue
@*/
#include <stdio.h>
#include <malloc.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "trace/ctrace.h"
#include "d2d_main.h"

#include "cominc.h"
#include "cmdline.h"
#include "dex.h"

//#define DEBUG_D2D
#ifdef DEBUG_D2D
int main(int argcc, char * argvc[])
{
    DUMMYUSE(argcc);
    DUMMYUSE(argvc);

    CHAR * argv[] = {
        "d2d.exe",
        //"test.apk",
        "-o", "output.dex",
    };
    int argc = sizeof(argv)/sizeof(argv[0]);
#else
int main(int argc, char const* argv[])
{
#endif
    int locerrno = 0; //0 indicates no error.
    long filelen;
    if (!processCommandLine(argc, argv)) {
        locerrno = -1;
        goto FIN;
    }

    if (g_logfile_name != nullptr && g_dump_dex_file_path) {
        note("\n==---- %s ----==\n", g_dex_file_path);
    }

    if (d2dEntry(g_source_file_handler, g_output_file_handler,
                 &filelen, false, g_dex_file_path) != 0) {
        locerrno = errno;
        LOGE("error: perform dexpro failed.\n");
    }

FIN:
    if (g_source_file_handler >= 0) {
        close(g_source_file_handler);
    }

    if (g_output_file_handler >= 0) {
        close(g_output_file_handler);
    }
    return locerrno; //success.
}

