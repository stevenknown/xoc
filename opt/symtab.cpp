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
#include "errno.h"
#include "commoninc.h"
#include "region_deps.h"

namespace xoc {

//
//START Sym
//
void Sym::dump(MOD LogMgr * lm) const
{
    ASSERT0(lm);
    xoc::prt(lm, "%s", getStr());
}
//END Sym


//
//START ESym
//
void ESym::dump(MOD LogMgr * lm) const
{
    ASSERT0(lm);
    CHAR const* str = getStr();
    for (UINT i = 0; i < getLen(); i++) {
        prt(lm, "%c", str[i]);
    }
    prt(lm, " slen:%u", getLen());
}
//END ESym


//
//START SymTab
//
void SymTab::dump(MOD LogMgr * lm) const
{
    SymTabIter it;
    for (Sym const* s = get_first(it); !it.end(); s = get_next(it)) {
        note(lm, "\n");
        s->dump(lm);
    }
    note(lm, "\n");
}
//END SymTab


//
//START ESymTab
//
void ESymTab::dump(MOD LogMgr * lm) const
{
    ESymTabIter it;
    for (ESym const* s = get_first(it); !it.end(); s = get_next(it)) {
        note(lm, "\n");
        s->dump(lm);
    }
    note(lm, "\n");
}
//END ESymTab

} //namespace xoc
