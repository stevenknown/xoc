/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
#include "../com/xcominc.h"
#include "symtab.h"

namespace xoc {

//Add const string into symbol table.
Sym const* SymTab::add(CHAR const* s)
{
    Sym * sym = m_free_one;
    if (sym == nullptr) {
        sym = (Sym*)smpoolMalloc(sizeof(Sym), m_pool);
    }
    SYM_name(sym) = const_cast<CHAR*>(s);
    Sym const* appended_one = TTab<Sym*, CompareSymTab>::append(sym);
    ASSERT0(m_free_one == nullptr || m_free_one == sym);
    if (appended_one != sym) {
        //'s' has already been appended.
        SYM_name(sym) = nullptr;
        m_free_one = sym;
    } else {
        //m_free_one has been inserted into table.
        //'s' is a new string so far.
        m_free_one = nullptr;
    }
    return appended_one;
}

} //namespace xoc
