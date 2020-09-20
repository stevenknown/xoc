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
#include "ltype.h"

INT m518087(CHAR const* info, ...)
{
    //CHAR * ptr = (CHAR*)&info;
    //ptr += sizeof(CHAR*); //being sizeof(info)
    va_list ptr;
    va_start(ptr, info);

    //PROTOTYPE: int __cdecl vprintf(char const*, char *)
    //vprintf(info, ptr);

    //PROTOTYPE: int vfprintf (FILE * stream, const char * format, va_list ap)
    vfprintf(stderr, info, ptr);
    fflush(stderr);

    //abort() has type 'void' and is not a throw-expression.
    //And it is unable to be used in clause : a ? 1 : abort().
    va_end(ptr);
    abort();
    return 0;
}


INT m522138(CHAR const* filename, INT line)
{
     fprintf(stderr, "\nASSERTION in %s(%d): ", filename, line);
     return 0;
}
