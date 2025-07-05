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
#include "cominc.h"
#include "comopt.h"
#include "range.h"

namespace xoc {

//
//START Range
//
void Range::dumpG(Pos init_pos, Region const* rg) const
{
    ASSERT0(rg);
    StrBuf buf(64);
    for (Pos pos = init_pos; pos <= end(); pos++) {
        if (pos < start()) {
            //prt(rg, " ");
            buf.strcat(" ");
            continue;
        }
        //prt(rg, "-");
        buf.strcat("-");
    }
    prt(rg, "%s", buf.buf);
}


void Range::dumpGWithPos(Pos init_pos, Region const* rg,
                         Pos in_start, Pos in_end) const
{
    ASSERT0(rg);
    StrBuf buf(64);
    for (Pos pos = init_pos; pos <= end(); pos++) {
        if (!(pos >= in_start && pos <= in_end)) { continue; }
        if (pos < start()) {
            buf.strcat(" ");
            continue;
        }
        buf.strcat("-");
    }
    prt(rg, "%s", buf.buf);
}


void Range::dump(Region const* rg) const
{
    ASSERT0(rg);
    prt(rg, "<");
    if (start() == POS_UNDEF) {
        prt(rg, "UNDEF");
    } else {
        prt(rg, "%u", start());
    }
    if (start() != end()) {
        prt(rg, "-");
        if (end() == POS_UNDEF) {
            prt(rg, "UNDEF");
        } else {
            prt(rg, "%u", end());
        }
    }
    prt(rg, ">");
}
//END Range

} //namespace xoc
