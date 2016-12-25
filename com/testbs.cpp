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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "xcominc.h"
#include "../opt/util.h"
using namespace xoc;

#ifdef _DEBUG_
void bs_test()
{
    SegMgr<BITS_PER_SEG> sm;

    DBitSet<BITS_PER_SEG> a(&sm);
    DBitSet<BITS_PER_SEG> b(&sm);
    a.set_sparse(false);
    b.set_sparse(false);
    a.bunion(5);
    a.bunion(511);
    a.bunion(515);
    b.bunion(21);
    a.dump(g_tfile);
    //int u = a.count_mem();
    b.dump(g_tfile);
    a.bunion(b);
    a.dump(g_tfile);


    DBitSet<BITS_PER_SEG> x(&sm, 20);
    x.set_sparse(false);
    x.bunion(1999);
    //x.bunion(1);
    x.bunion(2000);
    //x.bunion(2007);
    //x.bunion(2016);
    x.bunion(2014);
    //x.bunion(2025);
    //x.bunion(2033);
    x.bunion(2048);
    //x.bunion(1997);
    //x.bunion(1991);
    x.bunion(1983);
    x.bunion(2112);
    x.bunion(2113);
    x.diff(2048);
    x.bunion(2048);
    x.bunion(13555);
    x.dump(g_tfile);
    int n = x.get_elem_count();
    SC<SEG<BITS_PER_SEG>*>  * ct = NULL;
    n = x.get_first(&ct);
    n = x.get_next(n, &ct);
    n = x.get_next(n, &ct);
    n = x.get_next(n, &ct);
    n = x.get_next(n, &ct);
    n = x.get_next(n, &ct);
    n = x.get_next(n, &ct);
    n = x.get_last(&ct);

    DBitSet<BITS_PER_SEG> y(&sm, 20);
    y.set_sparse(false);
    y.bunion(23);
    y.bunion(1990);
    y.bunion(2113);
    y.bunion(12345);
    y.bunion(10000);
    y.bunion(14330);
    y.bunion(15330);
    y.bunion(16330);

    fprintf(g_tfile, "\n");
    x.dump(g_tfile);
    fprintf(g_tfile, "\n");
    y.dump(g_tfile);
    fprintf(g_tfile, "\n");

    y.bunion(x);
    fprintf(g_tfile, "\n=====\n");
    y.dump(g_tfile);

    //y.intersect(x);
    x.bunion(0);
    x.bunion(100000000);
    fprintf(g_tfile, "\ny:"); y.dump(g_tfile);
    fprintf(g_tfile, "\nx:"); x.dump(g_tfile);
    x.diff(y);
    fprintf(g_tfile, "\n=====\n");
    x.dump(g_tfile);
}


void bs_test2()
{
    SegMgr<BITS_PER_SEG> sm;
    SBitSet<BITS_PER_SEG> a(&sm),b(&sm);
    a.bunion(8);
    a.bunion(512);
    a.dump2(g_tfile);
    a.intersect(b);
    a.dump2(g_tfile);

    SBitSet<BITS_PER_SEG> c(&sm),d(&sm);
    c.bunion(8);
    c.bunion(512);
    c.dump2(g_tfile);
    d.diff(c);
    d.dump2(g_tfile);


    BitSet e,f;
    e.bunion(64);
    //int i = e.get_first();


    SBitSet<BITS_PER_SEG> g(&sm),h(&sm);
    g.bunion(1);
    g.bunion(100);
    g.bunion(600);
    g.bunion(1500);
    g.dump2(g_tfile);
    h.bunion(1501);
    h.bunion(2500);
    h.bunion(3500);
    g.is_intersect(h);

}


void bs_test3()
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n===");
    MiscBitSetMgr<33> mbsm;
    SBitSet<33> x1(mbsm.getSegMgr());
    for (int i = 0; i < 600; i+=3) {
        x1.bunion(i);
    }
    x1.dump(g_tfile);
    fprintf(g_tfile, "\n===");

    MiscBitSetMgr<123> mbsm2;
    SBitSet<123> x2(mbsm2.getSegMgr());
    for (int i = 0; i < 600; i+=3) {
        x2.bunion(i);
    }
    x2.dump(g_tfile);
    fprintf(g_tfile, "\n===");
    fflush(g_tfile);
}


#ifdef DEBUG_SEG
template <UINT BitsPerSeg>
void dump_segmgr(SegMgr<BitsPerSeg> & m)
{
    if (g_tfile == NULL) { return; }
    SC<SEG<BitsPerSeg>*> * st = NULL;
    fprintf(g_tfile, "\n====start %d:%d===\n",
            m.get_free_list()->get_elem_count(),
            m.get_seg_count());

    BitSet x;
    SList<SEG<BitsPerSeg>*> const* flst = m.get_free_list();
    for (flst->get_head(&st); st != flst.end(); st = flst->get_next(st)) {
        SEG<BitsPerSeg> const* s = st->val();
        fprintf(g_tfile, "%d,", s->id);
        x.bunion(s->id);
    }
    fflush(g_tfile);
    x.dump(g_tfile);
}
#endif
#endif
