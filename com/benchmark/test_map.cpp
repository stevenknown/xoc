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
#include "stdio.h"
#define NUM 1000000
#ifdef RUN_STL
#include <map>
int main()
{
    int x = 1;
    std::map<int, int> mymap;
    for (int i = 0; i < NUM; i++) {
        int v = x++;
        mymap.insert(std::pair<int, int>(v, v));
    }

    int sum = 0;
    for (std::map<int, int>::iterator it = mymap.begin();
         it != mymap.end(); it++) {
        sum += (*it).second;
    }

    int x2 = 1;
    for (; !mymap.empty();) {
        int v = x2++;
        mymap.erase(v);
    }

    for (int j = 0; j < NUM; j++) {
        int v = x++;
        mymap.insert(std::pair<int, int>(v, v));
    }
    return 0;
}

#else  //RUN XCOM

#include "../xcominc.h"
int main()
{
    int x = 1;
    xcom::TMap<int, int> mymap;
    for (int i = 0; i < NUM; i++) {
        int v = x++;
        mymap.set(v, v);
    }

    int sum = 0;
    xcom::TMapIter<int, int> iter;
    int tgt;
    for (int src = mymap.get_first(iter, &tgt);
         src != 0; src = mymap.get_next(iter, &tgt)) {
        sum += tgt;
    }

    int x2 = 1;
    for (; mymap.get_elem_count() != 0;) {
        int v = x2++;
        mymap.remove(v);
    }

    for (int j = 0; j < NUM; j++) {
        int v = x++;
        mymap.set(v, v);
    }
    return 0;
}
#endif
