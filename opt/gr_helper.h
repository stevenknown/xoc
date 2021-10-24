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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __GR_HELPER_H__
#define __GR_HELPER_H__

namespace xoc {

//The class represents options during GR dump.
class DumpGRCtx {
public:
    //Propagate info top down.
    //Set to true to dump string literal and ignore the \n.
    BYTE dump_string_in_one_line:1;

    //Propagate info top down.
    //Set to true to dump inner region.
    BYTE dump_inner_region:1;

    //Propagate info top down.
    //Supply CFG when dumpping PHI.
    IRCFG * cfg;
public:
    DumpGRCtx() { ::memset(this, 0, sizeof(DumpGRCtx)); }
};


CHAR const* compositeName(Sym const* n, xcom::StrBuf & buf);

//ctx: it can be NULL if user is not going to control the dumpping.
//     But it must be given if 'ir' is PHI because dump PHI will using CFG.
void dumpGR(IR const* ir, TypeMgr * tm, DumpGRCtx * ctx);
void dumpGRInBBList(List<IRBB*> * bblist, TypeMgr * tm, DumpGRCtx * ctx);
void dumpGRList(IR * irlist, TypeMgr * tm, DumpGRCtx * ctx);

} //namespace xoc
#endif
