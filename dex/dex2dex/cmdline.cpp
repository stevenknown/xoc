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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "trace/ctrace.h"

#include "cominc.h"
#include "cmdline.h"
#include "dex.h"
#include <errno.h>

INT g_source_file_handler = 0;
CHAR const* g_dex_file_path = nullptr;
INT g_output_file_handler = 0;
CHAR const* g_logfile_name = nullptr;

//Set true to disable all auxiliary informations which will output to stdout.
bool g_silence = false;
static CHAR const* g_version = "0.10.0";

static void usage()
{
    fprintf(stdout,
            "\ndexpro Version %s"
            "\nUsage: dexpro [options] your.dex"
            "\nOptions: "
            "\n  -o <file>       refer to output dex file path"
            "\n  -dump <file>    refer to dump file path"
            "\n  -silence        if it is set, dexpro will not display any auxiliary informations to screen"
            "\n", g_version);
}


static bool is_dex_source_file(CHAR const* fn)
{
    UINT len = strlen(fn) + 1;
    CHAR * buf = (CHAR*)ALLOCA(len);
    buf = xcom::getfilesuffix(fn, buf, len);

    if (buf == nullptr) { return false; }

    if (strcmp(xcom::upper(buf), "DEX") == 0 ||
        strcmp(xcom::upper(buf), "ODEX") == 0) {
        return true;
    }
    return false;
}


static bool create_dump_file(UINT argc, CHAR const* argv[], MOD UINT & i)
{
    CHAR const* dumpfile = nullptr;
    if (i + 1 < argc && argv[i + 1] != nullptr) {
        dumpfile = argv[i + 1];
    }
    i += 2;
    if (dumpfile == nullptr) { return false; }

    g_logfile_name = dumpfile;
    return true;
}


//Return true if command line is valid, otherwise return false.
static bool process_prefix_dump(UINT argc, CHAR const* argv[], MOD UINT & i)
{
    if (strcmp(argv[i], "-dump") == 0) {
        return create_dump_file(argc, argv, i);
    }

    return false;
}


//Return true if command line is valid, otherwise return false.
static bool process_silence(UINT argc, CHAR const* argv[], MOD UINT & i)
{
    if (strcmp(argv[i], "-silence") == 0) {
        g_silence = true;
        i += 1;
        return true;
    }

    return false;
}


static bool process_o(UINT argc, CHAR const* argv[], MOD UINT & i)
{
    CHAR const* output = nullptr;
    if (i + 1 < argc && argv[i + 1] != nullptr) {
        output = argv[i + 1];
    }
    i += 2;
    g_output_file_handler = open(output, O_RDWR|O_CREAT, 0666);
    if (g_output_file_handler < 0) {
        ASSERTN(0, ("can not open %s, errno:%d, errstring is %s",
                   output, errno, strerror(errno)));
        return false;
    }
    return true;
}


bool processCommandLine(UINT argc, CHAR const* argv[])
{
    if (argc <= 1) { usage(); return false; }

    g_source_file_handler = -1;
    g_output_file_handler = -1;

    for (UINT i = 1; i < argc;) {
        if (argv[i][0] == '-') {
            CHAR const* cmdstr = &argv[i][1];
            if (strcmp(cmdstr, "o") == 0) {
                if (!process_o(argc, argv, i)) {
                    usage();
                    return false;
                }
            } else if (strcmp(cmdstr, "silence") == 0) {
                if (!process_silence(argc, argv, i)) {
                    usage();
                    return false;
                }
            } else if (strncmp(cmdstr, "dump", 4) == 0) {
                if (!process_prefix_dump(argc, argv, i)) {
                    usage();
                    return false;
                }
            } else {
                usage();
                return false;
            }
        } else if (is_dex_source_file(argv[i])) {
            g_dex_file_path = argv[i];
            g_source_file_handler = open(g_dex_file_path, O_RDWR);
            if (g_source_file_handler < 0) {
                fprintf(stdout,
                        "dexpro: cannot open %s, error information is %s\n",
                        g_dex_file_path, strerror(errno));
                usage();
                return false;
            }
            i++;
        } else {
            usage();
            return false;
        }
    }

    if (g_source_file_handler < 0) {
        fprintf(stdout, "dexpro: no input file\n");
        usage();
        return false;
    }

    //output file is not indispensable.
    //if (g_output_file_handler < 0) {
    //    fprintf(stdout, "dexpro: no given output file path\n");
    //    usage();
    //    return false;
    //}

    return true;
}
