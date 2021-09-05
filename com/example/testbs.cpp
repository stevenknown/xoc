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

void bs_test(FILE * h)
{
    if (h == nullptr) { return; }
    SegMgr<BITS_PER_SEG> sm;
    DBitSet<BITS_PER_SEG> a(&sm);
    DBitSet<BITS_PER_SEG> b(&sm);
    a.set_sparse(false);
    b.set_sparse(false);
    a.bunion(5);
    a.bunion(511);
    a.bunion(515);
    b.bunion(21);
    a.dump(h);
    b.dump(h);
    a.bunion(b);
    a.dump(h);

    DBitSet<BITS_PER_SEG> x(&sm, 20);
    x.set_sparse(false);
    x.bunion(1999);
    x.bunion(2000);
    x.bunion(2014);
    x.bunion(2048);
    x.bunion(1983);
    x.bunion(2112);
    x.bunion(2113);
    x.diff(2048);
    x.bunion(2048);
    x.bunion(13555);
    x.dump(h);

    int n = x.get_elem_count();
    SC<SEG<BITS_PER_SEG>*>  * ct = nullptr;
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

    fprintf(h, "\n");
    x.dump(h);
    fprintf(h, "\n");
    y.dump(h);
    fprintf(h, "\n");

    y.bunion(x);
    fprintf(h, "\n=====\n");
    y.dump(h);

    //y.intersect(x);
    x.bunion(0);
    x.bunion(100000000);
    fprintf(h, "\ny:"); y.dump(h);
    fprintf(h, "\nx:"); x.dump(h);
    x.diff(y);
    fprintf(h, "\n=====\n");
    x.dump(h);
}


void bs_test2(FILE * h)
{
    if (h == nullptr) { return; }
    SegMgr<BITS_PER_SEG> sm;
    SBitSet<BITS_PER_SEG> a(&sm),b(&sm);
    a.bunion(8);
    a.bunion(512);
    a.dump2(h);
    a.intersect(b);
    a.dump2(h);

    SBitSet<BITS_PER_SEG> c(&sm),d(&sm);
    c.bunion(8);
    c.bunion(512);
    c.dump2(h);
    d.diff(c);
    d.dump2(h);

    BitSet e,f;
    e.bunion(64);

    SBitSet<BITS_PER_SEG> g(&sm),m(&sm);
    g.bunion(1);
    g.bunion(100);
    g.bunion(600);
    g.bunion(1500);
    g.dump2(h);
    m.bunion(1501);
    m.bunion(2500);
    m.bunion(3500);
    g.is_intersect(m);
}


void bs_test3(FILE * h)
{
    if (h == nullptr) { return; }
    fprintf(h, "\n===");
    MiscBitSetMgr<33> mbsm;
    SBitSet<33> x1(mbsm.getSegMgr());
    for (int i = 0; i < 600; i+=3) {
        x1.bunion(i);
    }
    x1.dump(h);
    fprintf(h, "\n===");

    MiscBitSetMgr<123> mbsm2;
    SBitSet<123> x2(mbsm2.getSegMgr());
    for (int i = 0; i < 600; i+=3) {
        x2.bunion(i);
    }
    x2.dump(h);
    fprintf(h, "\n===");
    fflush(h);
}


template <UINT BitsPerSeg>
void dumpLocSegMgr(SegMgr<BitsPerSeg> & m, FILE * h)
{
    if (h == nullptr) { return; }
    fprintf(h, "\n====start %d:%d===\n",
            m.get_free_list()->get_elem_count(),
            m.get_seg_count());
    BitSet x;
    SList<SEG<BitsPerSeg>*> const* flst = m.get_free_list();
    for (SC<SEG<BitsPerSeg>*> * st =flst->get_head();
         st != flst->end(); st = flst->get_next(st)) {
        SEG<BitsPerSeg> const* s = st->val();
        fprintf(h, "%d,", s->id);
        x.bunion(s->id);
    }
    fflush(h);
    x.dump(h);
}

int main()
{
    FILE * h = fopen("bs.dump", "a+");
    if (h == nullptr) { return 0; }
    bs_test(h);
    bs_test2(h);
    bs_test3(h);
    DefMiscBitSetMgr mgr;
    dumpLocSegMgr(*mgr.getSegMgr(), h);
    fclose(h);
    return 0;
}
