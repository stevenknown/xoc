/*@
Copyright (c) 2026-present, Su Zhenyu steven.known@gmail.com

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
#include "xinfer_cominc.h"

//#define DEBUG_XINFER
#ifdef DEBUG_XINFER
INT main(INT argcc, CHAR const* argvc[])
{
    DUMMYUSE(argcc);
    DUMMYUSE(argvc);
    CHAR const* argv[] = {
        "xinfer.exe",
        "-time",
        "-dump","tmp.log",
        "-dump-all",
        "test.safetensors",
    };
    INT argc = sizeof(argv)/sizeof(argv[0]);
#else
INT main(INT argc, CHAR const* argv[])
{
#endif
    if (!xinfer::processCmdLine(argc, argv)) {
        return 1;
    }
    if (xinfer::g_safetensors_file_list.get_elem_count() == 0 &&
        xinfer::g_gguf_file_list.get_elem_count() == 0) {
        fprintf(stderr, "\nerror:no input files\n");
        fflush(stderr);
        return 4;
    }
    xinfer::Driver driver;
    bool succ = driver.perform();
    if (!succ) {
        fprintf(stderr,
            "\nerror:inference process failed, please check the dump file\n");
        fflush(stderr);
    }
    return succ ? 0 : 3;
}
