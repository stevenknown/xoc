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
#include "stdio.h"

#ifdef RUN_STL
#include <list>
using std::list;
int main()
{
    printf("\ntest std list\n");
    int x = 1;
    std::list<int> mylist;
    for (int j = 0; j < 1000; j++) {
       for (int i = 0; i < 10000; i++) {
            int v = x++;
            mylist.push_back(v);
        }
    }

    int sum = 0;
    for (std::list<int>::iterator it = mylist.begin();
         it != mylist.end(); it++) {
        sum += *it;
    }
    for (; !mylist.empty();) {
        mylist.pop_front();
    }
    for (int j = 0; j < 1000; j++) {
       for (int i = 0; i < 10000; i++) {
            int v = x++;
            mylist.push_back(v);
        }
    }
    return 0;
}

#else

#include "../xcominc.h"
int main()
{
    printf("\ntest xoc sstl List\n");
    int x = 1;
    xcom::List<int> mylist;
    for (int j = 0;j < 1000; j++) {
        for (int i = 0; i < 10000; i++) {
            int v = x++;
            mylist.append_tail(v);
        }
    }

    int sum = 0;
    for (int it = mylist.get_head(); it != 0; it = mylist.get_next()) {
        sum+=it;
    }
    for (; mylist.get_elem_count() != 0;) {
        mylist.remove_head();
    }
    for (int j = 0;j < 1000; j++) {
        for (int i = 0; i < 10000; i++) {
            int v = x++;
            mylist.append_tail(v);
        }
    }
    return 0;
}
#endif
