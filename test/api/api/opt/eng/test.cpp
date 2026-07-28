#include "../../header_for_xgen.h"
#include "../../enable_opt.h"
#include "../../header_for_xpoly.h"

using namespace xpoly;
void test_27();
void test28();
void test29();
void test30();
void test31();
void test32();
void test33();
void test34();
void test35();
void test36();
void test37();
void test38();
void test39();
void test40();
void test41();
void test42();
void test43();
void test46();
void test47();
void test48();
void test49();
void test50();
void test51();
void test52();
void test53();
void test54();
void test55();
void test56();
void test57();
void test58();
void test59();
void test60();
void test61();
void test61_1();
void test61_1();
void test61_2();
void test61_3();
void test62();
void test63();
void test63_2();
void test63_2_2();
void test63_3();
void test63_4();
void test63_5();
void test63_6();
void test69();
void test70();
void test70_1();
void test70_1_2();
void test70_2();
void test70_3();
void test70_4();
void testCombineTranNoParameter();
void test73();
bool test74();
void test75();
bool test77();
void test78();
void test_poly();
void test_sort();
void testCombineTran();
void testFMelim2();
void test_poly3();
void test_feautrier();
void test_cost_model();
void test_birational();
void testUnimodularTran();
void test_six_eng();
void test_vector_product();
void testChernikova();
void testTransitiveEdge();
void testMatMul();
void testDepPoly();
void testGlobalParameter();
void testMIP();
void testLoopReverse();
void testLoopInterchange();
void testLoopInterchange2();
void testFMelim();
void test_graph();
void testLifeTime();
void testAssembleBin();
void testPolyhedral();
void testELFMgr();
void testSolveSystemEquation();
void testExtractBitValue();

void test_1() {
  FMatMgr fm;
  RMatMgr rm;
  printf("\ntest_1()\n");
  IMat r1(2, 2);
  FMat i2(1, 1);
  i2.setg(0, 0, 1);
  i2.setg(0, 1, 0);
  i2.setg(0, 2, 0);
  i2.setg(0, 3, 0);
  i2.setg(1, 0, -2);
  i2.setg(1, 1, 1);
  i2.setg(1, 2, 0);
  i2.setg(1, 3, 0);
  i2.setg(2, 0, 1);
  i2.setg(2, 1, -3);
  i2.setg(2, 2, 1);
  i2.setg(2, 3, 0);
  i2.setg(3, 0, -3);
  i2.setg(3, 1, 4);
  i2.setg(3, 2, 2);
  i2.setg(3, 3, 1);
  i2.dumps();
  FMat i2_rank(4, 4);
  printf("\ni2,det=%f,rank=%d\n", i2.det(fm).val(), i2.rank(fm, &i2_rank));
  i2_rank.dumps();
  FMat i2_inv(4, 4);
  i2.inv(i2_inv, fm);
  i2_inv.dumps();
  printf("\ni2_inv,det=%f,rank=%d\n",
         i2_inv.det(fm).val(), i2_inv.rank(fm, &i2_rank));
  i2_rank.dumps();
  i2_rank.dumpf(0, 0);

  FMat pro(4, 4);
  // pro must be unity
  FMat::mul(i2, i2_inv, pro);
  pro.dumps();
}

void test_2() {
  FMatMgr fm;
  printf("\ntest_2()\n");
  FMat i2(4, 5);
  i2.setg(0, 0, 2);
  i2.setg(0, 1, 4);
  i2.setg(0, 2, -1);
  i2.setg(0, 3, 5);
  i2.setg(0, 4, -2);
  i2.setg(1, 0, -4);
  i2.setg(1, 1, -5);
  i2.setg(1, 2, 3);
  i2.setg(1, 3, -8);
  i2.setg(1, 4, 1);
  i2.setg(2, 0, 2);
  i2.setg(2, 1, -5);
  i2.setg(2, 2, -4);
  i2.setg(2, 3, 1);
  i2.setg(2, 4, 8);
  i2.setg(3, 0, -6);
  i2.setg(3, 1, 0);
  i2.setg(3, 2, 7);
  i2.setg(3, 3, -3);
  i2.setg(3, 4, 1);
  i2.dumps();
  FMat i2_rank(4, 4);
  printf("\ni2,rank=%d\n", i2.rank(fm, &i2_rank));
  i2_rank.dumps();

  FMat c(4, 4), c1(4, 4), c2(4, 4), c3(4, 4), c4(4, 4);
  c.setg(0, 0, 1);
  c.setg(1, 0, 2);
  c.setg(1, 1, 1);
  c.setg(2, 0, -1);
  c.setg(2, 2, 1);
  c.setg(3, 0, 3);
  c.setg(3, 3, 1);
  c1 = c;

  FMat pro(4, 5), pro2(4, 5);
  // pro must be unity
  FMat::mul(c, i2, pro);
  pro.dumps();

  c.zero();
  c.setg(0, 0, 1);
  c.setg(1, 1, 1);
  c.setg(2, 1, 3);
  c.setg(2, 2, 1);
  c.setg(3, 1, -4);
  c.setg(3, 3, 1);
  c2 = c;

  // pro must be unity
  FMat::mul(c, pro, pro, fm);
  pro.dumps();

  c.zero();
  c.setg(0, 0, 1);
  c.setg(1, 1, 1);
  c.setg(2, 2, 1);
  c.setg(3, 2, -2);
  c.setg(3, 3, 1);
  c3 = c;

  printf("\nU is:\n");
  // pro must be unity
  FMat::mul(c, pro, pro, fm);
  pro.dumps();

  printf("\nc4 is:\n");
  FMatWrap t1(fm);
  FMat & t10 = (FMat&)FMat::mul(c3, c2, t1.m());
  FMat::mul(t10, c1, c4);

  c4.dumps();
  printf("\nc4 det:%f,rank:%d\n", c4.det(fm).val(), c4.rank(fm, nullptr));
  c4.inv(c4, fm);
  printf("\nc4 inv is:\n");
  c4.dumps();
  printf("\nc4 inv det:%f,rank:%d\n", c4.det(fm).val(), c4.rank(fm, nullptr));

  printf("\nc4 inv * U is:\n");
  FMat::mul(c4, pro, pro2);
  pro2.dumps();
}

void test_3() {
  printf("\ntest_3()\n");
  RMatMgr rm;
  RMat i2(4, 5);
  i2.setg(0, 0, 2);
  i2.setg(0, 1, 4);
  i2.setg(0, 2, -1);
  i2.setg(0, 3, 5);
  i2.setg(0, 4, -2);
  i2.setg(1, 0, -4);
  i2.setg(1, 1, -5);
  i2.setg(1, 2, 3);
  i2.setg(1, 3, -8);
  i2.setg(1, 4, 1);
  i2.setg(2, 0, 2);
  i2.setg(2, 1, -5);
  i2.setg(2, 2, -4);
  i2.setg(2, 3, 1);
  i2.setg(2, 4, 8);
  i2.setg(3, 0, -6);
  i2.setg(3, 1, 0);
  i2.setg(3, 2, 7);
  i2.setg(3, 3, -3);
  i2.setg(3, 4, 1);
  i2.dumps();

  RMat p(1, 1), l(10, 2), u(1, 1);
  i2.plu(p, l, u, rm);
  printf("\np:\n");
  p.dumps();
  printf("\nl:\n");
  l.dumps();
  printf("\nu:\n");
  u.dumps();

  RMat A(4, 5);
  RMat::mul(l, u, A);
  A.dumps();
  p.trans();
  RMat::mul(p, A, A, rm);
  A.dumps();

  //////
  RMat a(3, 3), al, au;
  a.sete(9, 1, 2, 3, 4, 5, 6, 7, 8, 0);
  a.dumps();
  ASSERTN(a.plu(p, al, au, rm), ("lu failed!!"));
  p.dumps();
  al.dumps();
  au.dumps();
}

void test_4() {
  printf("\ntest_4()\n");
  RMatMgr rm;
  RMat i2(5, 4);
  i2.setg(0, 0, 2);
  i2.setg(0, 1, -4);
  i2.setg(0, 2, -2);
  i2.setg(0, 3, 3);
  i2.setg(1, 0, 6);
  i2.setg(1, 1, -9);
  i2.setg(1, 2, -5);
  i2.setg(1, 3, 8);
  i2.setg(2, 0, 2);
  i2.setg(2, 1, -7);
  i2.setg(2, 2, -3);
  i2.setg(2, 3, 9);
  i2.setg(3, 0, 4);
  i2.setg(3, 1, -2);
  i2.setg(3, 2, -2);
  i2.setg(3, 3, -1);
  i2.setg(4, 0, -6);
  i2.setg(4, 1, 3);
  i2.setg(4, 2, 3);
  i2.setg(4, 3, 4);
  i2.dumps();

  RMat p(1, 1), l(10, 2), u(1, 1);
  i2.plu(p, l, u, rm);
  // i2.lu(l,u);
  printf("\np:\n");
  p.dumps();
  printf("\nl:\n");
  l.dumps();
  printf("\nu:\n");
  u.dumps();

  if (l == u) {
    printf("eq\n");
  } else if (l != u) {
    printf("uneq\n");
  }

  RMat A(5, 4);
  RMat::mul(l, u, A);
  A.dumps();
  p.trans();
  RMat::mul(p, A, A, rm);
  A.dumps();
}

void test_5() {
  RMatMgr rm;
  /*
  A = [ 1    2    3
    4    5    6
    7    8    0 ];

L =

  1.0000         0         0
  0.1429    1.0000         0
  0.5714    0.5000    1.0000

U =
  7.0000    8.0000         0
       0    0.8571    3.0000
       0         0    4.5000

P =
  0    0    1
  1    0    0
  0    1    0
    */
  printf("\ntest_5()\n");
  RMat i1(3, 3);
  i1.set(0, 0, 1);
  i1.set(0, 1, 2);
  i1.set(0, 2, 3);
  i1.set(1, 0, 4);
  i1.set(1, 1, 5);
  i1.set(1, 2, 6);
  i1.set(2, 0, 7);
  i1.set(2, 1, 8);
  i1.set(2, 2, 0);
  i1.dumps();

  RMat p(3, 3), l(3, 3), u(3, 3);
  i1.plu(p, l, u, rm);
  printf("\np:\n");
  p.dumps();
  printf("\nl:\n");
  l.dumps();
  printf("\nu:\n");
  u.dumps();

  if (l == u) {
    printf("eq\n");
  } else if (l != u) {
    printf("uneq\n");
  }

  RMat A(3, 3);
  RMat::mul(l, u, A);
  A.dumps();
  p.trans();
  RMat::mul(p, A, A, rm);
  A.dumps();
}

void test_6() {
  printf("\ntest_6()\n");

  RMatMgr rm;
  FMatMgr fm;
  RMat i1, l, u, p;
  i1.setg(0, 0, 0);
  i1.setg(0, 1, 1);
  i1.setg(1, 0, 1);
  i1.setg(1, 1, 0);
  i1.dumps();
  i1.lu(l, u);
  i1.plu(p, l, u, rm);
  l.dumps();
  u.dumps();

  FMat i2, l2, u2, p2, x;
  i2.setg(0, 0, 0.001);
  i2.setg(0, 1, 1.0);
  i2.setg(1, 0, 1.0);
  i2.setg(1, 1, 2.0);
  i2.dumps();
  if (i2.lu(l2, u2)) {
    l2.dumps();
    u2.dumps();
  }
  if (i2.plu(p2, l2, u2, fm)) {
    l2.dumps();
    u2.dumps();
  }
  FMat b;
  b.setg(0, 0, 1);
  b.setg(1, 0, 3);
  b.dumps();
  i2.sse(x, b, fm);
  x.dumps();

  i2.reinit(4, 4);
  i2.sete(16, 0.78, -0.02, -0.12, -0.14, -0.02, 0.86, -0.04, 0.06, -0.12, -0.04,
          0.72, -0.08, -0.14, 0.06, -0.08, 0.74);
  b.setg(0, 0, 0.76);
  b.setg(1, 0, 0.08);
  b.setg(2, 0, 1.12);
  b.setg(3, 0, 0.68);
  i2.dumps();
  b.dumps();
  i2.sse(x, b, fm);
  /*x is   1.534965
   *       0.122010
   *       1.975156
   *       1.412955
   * */
  x.dumps();
}

void test_7() {
  RMatMgr rm;
  FMatMgr fm;
  /*
   a=[3  -3  1;-3  5  -2;1  -2  1];
   b=[14  13  5; 5  1  12;6  14  5];
   d1=det(a)
   x1=inv(a)
   d2=det(b)
   x2=inv(b)
   d1 =
        1
   x1 =
        1.0000    1.0000    1.0000
        1.0000    2.0000    3.0000
        1.0000    3.0000    6.0000

   d2 =    -1351
   x2 =
       0.1207   -0.0037   -0.1118
       -0.0348   -0.0296    0.1058
       -0.0474    0.0873    0.0377
    */
  printf("\ntest_7()\n");
  RMat a(3, 3);
  a.sete(9, 3, -3, 1, -3, 5, -2, 1, -2, 1);
  a.dumps();

  FMat b(3, 3);
  b.sete(9, 14.0, 13.0, 5.0, 5.0, 1.0, 12.0, 6.0, 14.0, 5.0);
  b.dumps();

  a.inv(a, rm);
  a.dumps();

  printf("\ndeta:%d,detb:%f\n", a.det(rm).num(), b.det(fm).val());

  b.inv(b, fm);
  b.dumps();

  // norm
  FMat b2;
  b.reinit(2, 3);
  b.sete(6, 3.0, 6.0, 0.0, 0.0, 0.0, 2.0);
  b2 = b;
  b2.trans();
  b2.dumps();
  FMat::mul(b, b2, b2, fm);
  b2.dumps();

  b.dumps();
  b.nml();
  b.dumps();
  b2 = b;
  b2.trans();
  b2.dumps();
  FMat::mul(b, b2, b2, fm);
  /*
  b.norm:
       0.447214           0.894427           0.000000
      0.000000           0.000000           2.000000

  b2:
        1.000000           0.000000
        0.000000           1.000000
  */
  b2.dumps();

  /*A=[22  46  20  20; 30  36  46  44;39  8  45  2];
   *
   * [q,r]=qr(A)
   *
   * q =
         0.377297           0.094660           0.585981
         0.788893           -0.553475           -0.153184
         0.342997           0.612547           0.397221
         0.342997           0.556319           -0.689476
   *
   * r =
         58.309519           70.588818           37.146594
         0.000000           35.569915           27.941194
         0.000000           -0.000000           38.123749

  */
  FMat c(3, 4), bs;
  c.sete(12, 22.0, 46.0, 20.0, 20.0, 30.0, 36.0, 46.0, 44.0, 39.0, 8.0, 45.0,
         2.0);
  c.dumps();
  printf("\nrankc:%d\n", c.rank(fm, nullptr));
  c.basis(bs, fm);
  bs.dumps();

  FMat q, r, w, qt;
  c.qr(q, r, fm);
  printf("\n\n\n");
  q.dumps();
  r.dumps();
  FMat::mul(q, r, w, fm);
  w.dumps();
  qt = q;
  qt.trans();
  FMat::mul(qt, q, qt, fm);
  /*
       1.000000           0.000000           0.000000
       0.000000           1.000000           -0.000000
       0.000000           -0.000000           1.000000
  */
  qt.dumps(); // qt is unitary

  ////
  FMat e(4, 3), eq, er;
  e.sete(12, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0);
  /*
  matrix is nonfull rank.
  basis is
   1.000000           0.000000           -1.000000
   0.000000           1.000000           2.000000

  Q =
      0.707107           0.577350
      0.000000           0.577350
      -0.707107           0.577350
  R =
       1.414214           -1.414214
       0.000000           1.732051
  */
  e.dumpf();
  printf("\nrank is %d\n", e.rank(fm));
  q = e;
  q.dumpf();
  q.qr(eq, er, fm, true);
  eq.dumpf();
  er.dumpf();
  // w is only basis of e.
  FMat::mul(eq, er, w);
  w.dumpf();

  /*
  after orthogonalized
q:
   0.500000           -0.866025           0.000000
   0.500000           0.288675           -0.816497
   0.500000           0.288675           0.408248
   0.500000           0.288675           0.408248
r:
      2.000000           1.500000           1.000000
      0.000000           0.866025           0.577350
      0.000000           0.000000           0.816497

  */
  e.reinit(4, 3);
  e.sete(12, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0);
  printf("\ne1:\n");
  e.dumps();
  e.trans();
  printf("\ne2:\n");
  e.dumps();
  e.qr(eq, er, fm);
  eq.dumps();
  er.dumps();
  FMat::mul(eq, er, w, fm);
  w.dumps();

  /*
  q:
        0.833333           -0.166667
        0.166667           0.833333
        -0.500000           0.166667
        0.166667           0.500000
  r:
     6.000000           12.000000
     0.000000           6.000000
      */
  FMat f(2, 4), fq, fr, u;
  f.sete(8, 5.0, 1.0, -3.0, 1.0, 9.0, 7.0, -5.0, 5.0);
  f.dumps();
  f.qr(fq, fr, fm);
  fq.dumps();
  fr.dumps();
  FMat::mul(fq, fr, u, fm);
  u.dumps();

  /*
  q:
      0.500000           0.500000           0.500000
      0.500000           -0.500000           -0.500000
      0.500000           -0.500000           0.500000
      0.500000           0.500000           -0.500000
   r:
      2.000000           4.000000           5.000000
      0.000000           2.000000           3.000000
      0.000000           0.000000           2.000000
      */
  f.reinit(4, 3);
  f.sete(12, 1.0, 3.0, 5.0, 1.0, 1.0, 0.0, 1.0, 1.0, 2.0, 1.0, 3.0, 3.0);
  f.trans();
  f.dumps();
  f.qr(fq, fr, fm);
  fq.dumps();
  fr.dumps();
  FMat::mul(fq, fr, u, fm);
  u.dumps();
}

void test_inv() {
  RMatMgr rm;
  printf("\ntest_inv()\n");
  bool is_s = false;
  RMat rat4(3, 3);
  // non-singular test
  rat4.setr(0, 0, 1);
  rat4.setr(0, 1, 2);
  rat4.setr(0, 2, 3);
  rat4.setr(1, 0, 2);
  rat4.setr(1, 1, 1);
  rat4.setr(1, 2, 2);
  rat4.setr(2, 0, 1);
  rat4.setr(2, 1, 3);
  rat4.setr(2, 2, 3);

  is_s = rat4.inv(rat4, rm);
  rat4.dumps();
  printf("\n %d/%d\n", rat4.tr().num(), rat4.tr().den());
  if (!is_s) {
    printf("Singular!!");
  }
  rat4.dumps();

  // singular test
  rat4.setr(0, 0, 1);
  rat4.setr(0, 1, 0);
  rat4.setr(0, 2, 2);
  rat4.setr(1, 0, 2);
  rat4.setr(1, 1, 0);
  rat4.setr(1, 2, 4);
  rat4.setr(2, 0, 1);
  rat4.setr(2, 1, 2);
  rat4.setr(2, 2, 3);
  Rational det = rat4.det(rm);

  RMat rr = rat4;
  INT rank = rat4.rank(rm, &rr);
  printf("\nrank=%d\n", rank);
  rr.dumps();
  is_s = rat4.inv(rat4, rm);
  if (!is_s) {
    printf("Singular!!");
  }
  rat4.dumpf();
  rat4.dumps();

  RMat rat2(4, 4);
  rat2.setr(0, 0, 0);
  rat2.setr(0, 1, 0);
  rat2.setr(0, 2, 0);
  rat2.setr(0, 3, 1);
  rat2.setr(1, 0, 0);
  rat2.setr(1, 1, 0);
  rat2.setr(1, 2, -1);
  rat2.setr(1, 3, 2);
  rat2.setr(2, 0, 0);
  rat2.setr(2, 1, 3);
  rat2.setr(2, 2, -1);
  rat2.setr(2, 3, -3);
  rat2.setr(3, 0, 3);
  rat2.setr(3, 1, 2);
  rat2.setr(3, 2, 3);
  rat2.setr(3, 3, 4);

  rr = rat2;
  rank = rat2.rank(rm, &rr);
  printf("\nrank=%d\n", rank);
  rr.dumps();
  det = rat2.det(rm);
  rat2.dumpf();
  if (rat2.isLowTriangular()) {
    printf("\nlow tri");
  }
  if (rat2.isAntiLowTriangular()) {
    printf("\nanti low tri");
  }
  if (rat2.isUpTriangular()) {
    printf("\nup tri");
  }
  if (rat2.isAntiUpTriangular()) {
    printf("\nanti up tri");
  }
  is_s = rat2.inv(rat2, rm);
  if (!is_s) {
    printf("Singular!!");
  }
  /////////test det
  rat2.setr(0, 0, 8);
  rat2.setr(0, 1, 0);
  rat2.setr(0, 2, 0);
  rat2.setr(0, 3, 0);
  rat2.setr(1, 0, 5);
  rat2.setr(1, 1, -1);
  rat2.setr(1, 2, 0);
  rat2.setr(1, 3, 0);
  rat2.setr(2, 0, 2);
  rat2.setr(2, 1, 3);
  rat2.setr(2, 2, -1);
  rat2.setr(2, 3, 0);
  rat2.setr(3, 0, 3);
  rat2.setr(3, 1, 2);
  rat2.setr(3, 2, 3);
  rat2.setr(3, 3, 4);
  det = rat2.det(rm);
  rat2.dumpf();
  printf("\ndet:%d\n", det.num());
  if (rat2.isLowTriangular()) {
    printf("\nlow tri");
  }
  if (rat2.isAntiLowTriangular()) {
    printf("\nanti low tri");
  }
  if (rat2.isUpTriangular()) {
    printf("\nup tri");
  }
  if (rat2.isAntiUpTriangular()) {
    printf("\nanti up tri");
  }

  printf("\nafter identical:1\n");
  rat2.eche(rm);
  rat2.dumps();

  is_s = rat2.inv(rat2, rm);
  if (!is_s) {
    printf("Singular!!");
  }

  /// test rank
  rat2.setr(0, 0, 8);
  rat2.setr(0, 1, 0);
  rat2.setr(0, 2, 3);
  rat2.setr(0, 3, -19);
  rat2.setr(1, 0, 5);
  rat2.setr(1, 1, -1);
  rat2.setr(1, 2, 2);
  rat2.setr(1, 3, 7);
  rat2.setr(2, 0, 0);
  rat2.setr(2, 1, 0);
  rat2.setr(2, 2, 2);
  rat2.setr(2, 3, 7);
  rat2.setr(3, 0, 0);
  rat2.setr(3, 1, 0);
  rat2.setr(3, 2, 4);
  rat2.setr(3, 3, 14);
  rat2.dumps();
  det = rat2.det(rm);
  rat2.dumpf();
  printf("\ndet4:%d\n", det.num());

  rr = rat2;
  rank = rat2.rank(rm, &rr);
  printf("\nrank:%d\n", rank);
  rr.dumps();
  is_s = rat2.inv(rat2, rm);
  if (!is_s) {
    printf("Singular!!\n");
  } else {
    rat2.dumps();
    printf("\nafter identical:2\n");
    rat2.eche(rm);
    rat2.dumps();
  }
}

// for nullspace space
void test_8() {
  printf("\ntest_8()\n");
  RMatMgr rm;
  FMatMgr fm;
  RMat a(3, 5), b(3, 1);
  a.sete(15, -3, 6, -1, 1, -7, 1, -2, 2, 3, -1, 2, -4, 5, 8, -4);
  RMat ns;
  a.nullspace(ns, rm);
  printf("\na:");
  a.dumps();
  printf("\nnullspace of a:");
  /*
  0          2          0          1         -3
  0          1          0          0          0
  0          0          0         -2          2
  0          0          0          1          0
  0          0          0          0          1
  */
  ns.dumps();
  // a.adj(ns); printf("\nadj of a:"); ns.dumps();
  RMat p, l, u;
  if (a.plu(p, l, u, rm)) {
    printf("\n\tp:\n");
    p.dumps();
    printf("\n\tl:\n");
    l.dumps();
    printf("\n\tu:\n");
    u.dumps();

    RMat A;
    RMat::mul(l, u, A);
    A.dumps();
    p.trans();
    RMat::mul(p, A, A, rm);
    A.dumps();
  }

  a.reinit(3, 3);
  a.sete(9, -1, -2, 2, -2, -4, 4, 2, 4, -4);
  printf("\na:");
  a.dumps();
  a.nullspace(ns, rm);
  printf("\nnullspace of a:");
  /*         0                   -2                    2
   *         0                    1                    0
   *         0                    0                    1
   * */
  ns.dumps();

  a.reinit(3, 3);
  a.sete(9, 8, -2, 2, -2, 5, 4, 2, 4, 5);
  printf("\na:");
  a.dumps();
  a.nullspace(ns, rm);
  printf("\nnullspace of a:");
  /* 0                    0                   -1/2
   * 0                    0                   -1
   * 0                    0                    1
   */
  ns.dumps();

  a.reinit(3, 3);
  a.sete(9, 1, 2, 3, 2, 1, 2, 1, 3, 3);
  printf("\na:");
  a.dumps();
  /*-3         -3          1
   * 4          0         -4
   * 5          1         -3
   */
  a.adj(ns, rm);
  printf("\nadj of a:");
  ns.dumps();
  a.sete(9, -3, 2, -5, -1, 0, -2, 3, -4, 1);
  printf("\na:");
  a.dumps();
  /*
  -8         18         -4
  -5         12         -1
   4         -6          2
   */
  a.adj(ns, rm);
  printf("\nadj of a2:");
  ns.dumps();

  /*

  */
  FMat fa, nfa;
  fa.reinit(3, 3);
  fa.sete(9, 2.0, -1.0, 6.0, 2.0, -1.0, 6.0, 2.0, -1.0, 6.0);
  fa.dumpf();
  fa.nullspace(nfa, fm);
  printf("\na:");
  fa.dumpf();
  printf("\nnullspace of fa:");
  nfa.dumpf();
}

// for nullspace space
void test_9() {
  printf("\ntest_9()\n");
  FMatMgr fm;
  RMatMgr rm;
  FMat a(3, 3), b, bt;
  a.sete(9, 1.0, 1.0, 1.0, 0.0, 1.0, 2.0, 2.0, 0.0, 3.0);
  a.dumps();
  a.orthn(b, fm);
  /*
      0.577350    0.577350    0.577350
      -0.707107    0                    0.707107
      0.408248    -0.816497    0.408248
  */
  b.dumps();

  FMat::mul(b, b, bt, fm);
  printf("\n%f\n", bt.det(fm).val());
  bt.dumps();
  bt = b;
  bt.trans();
  bt.dumps();
  FMat::mul(bt, b, bt, fm);
  bt.dumps();

  /*
  orth:
          1.000000           1.000000           1.000000           1.000000
          -0.750000           0.250000           0.250000           0.250000
          0.000000           -0.666667           0.333333           0.333333
  product:
      4.000000           0.000000           0.000000
      0.000000           0.750000           0.000000
      0.000000           0.000000           0.666667
  */
  RMat c(4, 3), co, ct;
  c.sete(12, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1);
  c.trans();
  c.orth(co, rm);
  co.dumps();
  ct = co;
  ct.trans();
  RMat::mul(co, ct, ct, rm);
  ct.dumps();

  a.sete(9, 1.0, 2.0, -1.0, -1.0, 3.0, 1.0, 4.0, -1.0, 0.0);
  a.dumps();
  a.orthn(b, fm);
  /*
         0.408248           0.816497           -0.408248
         -0.577350           0.577350           0.577350
         0.707107           0.000000           0.707107
  */
  b.dumps();

  RMat cf(2, 3), cfo;
  cf.sete(6, 3, 6, 0, 1, 2, 2);
  cf.dumps();
  cf.orth(cfo, rm);
  /*
    3          6          0
    0          0          2
      */
  cfo.dumps();

  cf.reinit(3, 4);
  cf.sete(12, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1);
  cf.orth(cfo, rm);
  /*
      1          1          1          1
     -3/4        1/4        1/4        1/4
      0         -2/3        1/3        1/3
  */
  cfo.dumps();
}

// for least squares
void test_10() {
  printf("\ntest_10()\n");

  FMatMgr rm;
  /*
  x is :
      0.000000           0.000000           0.000000           -1.000000
  -3.000000 0.000000           0.000000
  0.000000           1.000000           5.000000 0.000000           0.000000
  0.000000           1.000000           2.000000 0.000000           0.000000
  0.000000           0.000000           -0.000000
  */
  FMat a(4, 6), x, b(6, 1);
  a.sete(24, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0);
  a.dumpf();
  a.trans();
  b.sete(6, -3.0, -1.0, 0.0, 2.0, 5.0, 1.0);
  a.dumpf();
  b.dumpf();
  a.mls(x, b, rm);
  x.dumpf();
}

void test_11() {
  FMatMgr fm;
  printf("\ntest_11()\n");
  /*
  EIG    Eigenvalues and eigenvectors.
  E = EIG(X) is a vector containing the eigenvalues of a square matrix X.
  [V,D] = EIG(X) produces a diagonal matrix D of eigenvalues and a full matrix V
  whose columns are the corresponding eigenvectors so that X*V = V*D. [V,D] =
  EIG(X,'nobalance') performs the computation with balancing disabled, which
  sometimes gives more accurate results for certain problems with unusual
  scaling. If X is symmetric, EIG(X,'nobalance') is ignored since X is already
  balanced.
  */
  FMat b(4, 4), eigv;
  b.sete(16, 2.9766, 0.3945, 0.4198, 1.1159, 0.3945, 2.7328, -0.3097, 0.1129,
         0.4198, -0.3097, 2.5675, 0.6079, 1.1159, 0.1129, 0.6079, 1.7231);
  b.dumps();
  b.trans();
  b.eig(eigv, fm);
  eigv.dumps();

  /*
  a=[34  25  15; 18  35  9; 41  21  9]
  e=eig(a)
  [v,d]=eig(a)
  a =
      34    25    15
      18    35     9
      41    21     9
  e =
     68.5066
     15.5122
     -6.0187
  v =   -0.6227   -0.4409   -0.3105
     -0.4969    0.6786   -0.0717
     -0.6044   -0.5875    0.9479
  d =
     68.5066         0         0
           0   15.5122         0
           0         0   -6.0187
    */
  FMat t(3, 3), nul;
  t.sete(9, 3.0, 3.0, 3.0, -3.0, -3.0, -3.0, 3.0, 3.0, 3.0);
  // t.trans();
  int i = t.rank(fm, &nul);
  nul.dumps();
  t.nullspace(nul, fm);
  nul.dumps();

  FMat a(3, 3), eigx, x, y;
  a.sete(9, 34.0, 25.0, 15.0, 18.0, 35.0, 9.0, 41.0, 21.0, 9.0);
  a.dumps();
  printf("det(a):%f", a.det(fm).val());
  a.eig(eigv, fm);
  //eigv: 68.506553           15.512164           -6.018717
  eigv.dumps();

  a.sete(9, 1.0, 3.0, 3.0, -3.0, -5.0, -3.0, 3.0, 3.0, 1.0);
  a.dumps();
  a.setSigDigitDesc(2);
  a.eig(eigv, eigx, fm);
  /*
      eigv:
           -2.000000           0.000000           0.000000
           0.000000           -2.000000           0.000000
           0.000000           0.000000           1.000000

      eigx:
          -1.000000           -1.000000           1.000000
         1.000000           0.000000           -1.000000
         0.000000           1.000000           1.000000
      */
  eigv.dumps();
  eigx.dumps();

  // for verify
  FMat::mul(a, eigx, x, fm);
  FMat::mul(eigx, eigv, y, fm);
  x.dumps();
  y.dumps();
  ASSERTN(x == y, ("Ax=x*D"));

  a.sete(9, 2.0, 4.0, 3.0, -4.0, -6.0, -3.0, 3.0, 3.0, 1.0);
  a.setSigDigitDesc(2);
  a.eig(eigv, eigx, fm);
  eigv.dumps();
  eigx.dumps();
  return;
}

void test_12() {
  IMatMgr im;
  RMatMgr rm;
  FMatMgr fm;
  printf("\ntest_12()\n");
  /*
  ÆæÒìÖµ·Ö½â.( Singular value decomposition).
  Èç´æÔÚÁ½¸öÊ¸Á¿u,v¼°Ò»³£Êýc,Ê¹µÃ¾ØÕóAÂú×ã£ºAv=cu,  A¡¯u=cv
  ³ÆcÎªÆæÒìÖµ£¬³Æu,vÎªÆæÒìÊ¸Á¿¡£
  ½«ÆæÒìÖµÐ´³É¶Ô½Ç·½Õó¡Æ£¬¶øÏà¶ÔÓ¦µÄÆæÒìÊ¸Á¿×÷ÎªÁÐÊ¸Á¿Ôò¿ÉÐ´³ÉÁ½¸ö
  Õý½»¾ØÕóU£¬V£¬Ê¹µÃ£º
  AV=U¡Æ£¬ A¡®U=V¡Æ  ÒòÎªU£¬VÕý½»£¬ËùÒÔ¿ÉµÃÆæÒìÖµ±í´ïÊ½£º A=U¡ÆV¡¯¡£
  Ò»¸ömÐÐnÁÐµÄ¾ØÕóA¾­ÆæÒìÖµ·Ö½â£¬¿ÉÇóµÃmÐÐmÁÐµÄU,
  mÐÐnÁÐµÄ¾ØÕó¡ÆºÍnÐÐnÁÐµÄ¾ØÕóV.¡£ ÆæÒìÖµ·Ö½âÓÃsvdº¯ÊýÊµÏÖ£¬µ÷ÓÃ¸ñÊ½Îª£»
  [u,s,v]=svd(a)
  SVD    Singular value decomposition.
  [U,S,V] = SVD(X) produces a diagonal matrix S, of the same dimension as X
  and with nonnegative diagonal elements in decreasing order, and unitary
  matrices U and V so that X = U*S*V'.
  S = SVD(X) returns a vector containing the singular values.
  [U,S,V] = SVD(X,0) produces the "economy size" decomposition.
  If X is m-by-n with m > n, then only the first n columns of U are
  computed and S is n-by-n.

  Àý£º ÆæÒìÖµ·Ö½â¡£
  a=[8  5; 7  3;4  6];
  [u,s,v]=svd(a)             % sÎªÆæÒìÖµ¶Ô½Ç·½Õó
  u =
     -0.6841   -0.1826   -0.7061
     -0.5407   -0.5228    0.6591
     -0.4895    0.8327    0.2589

  s =
     13.7649         0
           0    3.0865
           0         0

  v =
     -0.8148   -0.5797
     -0.5797    0.8148
      */
  {
    // Failed!!
    FMat f(5, 4), u, w, v;
    f.sete(20, 1.0, 2.0, 3.0, 4.0, 6.0, 7.0, 8.0, 9.0, 1.0, 2.0, 13.0, 0.0,
           16.0, 17.0, 8.0, 9.0, 2.0, 4.0, 3.0, 4.0);
    f.dumps();
    f.svd(u, w, v, fm);
    u.dumpf();
    w.dumpf();
    v.dumpf();
  }

  FMat w(3, 3), wn;
  ///////////////
  w.sete(9, -280.0, 100.000000, 40.000000, 100.000000, -190.0, 140.000000,
         40.000000, 140.000000, -160.00);
  w.nullspace(wn, fm);
  wn.dumps();
  ///////////////

  FMat a(3, 2), u, s, v;
  /////////////////////////////////////

  a.sete(6, 1.0, -1.0, -2.0, 2.0, 2.0, -2.0);
  // a.setSigDigitDesc(1);
  a.dumps();
  a.setSigDigitDesc(2);
  a.svd(u, s, v, fm);
  u.dumps();
  s.dumps();
  v.dumps();
  a.dumps();
  FMat & t10 = (FMat &)FMat::mul(u, s, FMatWrap(fm).m());
  FMat::mul(t10, v, a);
  a.dumps();

  /////////////////////////////////////
  a.reinit(2, 3);
  a.sete(6, 4.0, 11.0, 14.0, 8.0, 7.0, -2.0);
  // a.setSigDigitDesc(1);
  a.dumps();
  a.svd(u, s, v, fm);
  u.dumps();
  s.dumps();
  v.dumps();
  a.dumps();
  t10 = (FMat &)FMat::mul(u, s, FMatWrap(fm).m());
  FMat::mul(t10, v, a);

  a.setSigDigitDesc(2);
  a.adjust();
  a.dumps();

  a = u;
  a.trans();
  FMat::mul(a, u, a, fm);
  a.dumps();

  a = v;
  a.trans();
  FMat::mul(v, a, a, fm);
  a.dumps();

  /////////////////////////////////////
  a.reinit(2, 2);
  a.setSigDigitDesc(6);
  a.sete(4, 0.96, 1.72, 2.28, 0.96);
  a.svd(u, s, v, fm);
  u.dumps();
  s.dumps();
  v.dumps();
  t10 = (FMat &)FMat::mul(u, s, FMatWrap(fm).m());
  FMat::mul(t10, v, a);
  a.adjust();
  a.dumps();
  /////////////////////////////////////
  IMat x(2, 2);
  x.setPartialElem(4, 7, 10, 5, 7);
  INT i;
  x.cond(i, im);
  printf("\n%d\n", i);
  //////////////////////////////////////
  FMat xx(2, 2);
  xx.sete(4, -1.0, 2.0, 3.0, 7.0);

  // ss = 7.690416
  double ss = xx.sprad(fm).val();
  printf("\n%f\n", ss);

  // ss = 7.758372
  ss = xx.norm(NORM_2, fm).val();
  printf("\n%f\n", ss);
  /////////////////////////////////////

  /*
  ¸ø³ö¾ØÕóºÍ2·¶ÊýµÄ½á¹û?
      10½×µ¥Î»Õó£¬2-·¶ÊýÊÇ1¡£¡£¡£

      ÆäÊµ¾ÍÊÇ×î´óµÄÆæÒìÖµ¶øÒÑ£¬»òÕßA^T*AµÄ×î´óÌØÕ÷Öµ¿ª¸ùºÅ¡£

      ¸øÄã¸ö¼òµ¥µÄÀý×Ó
      A=
      0 1 0 0 0 0 0 0 0 0
      1 0 1 0 0 0 0 0 0 0
      0 1 0 1 0 0 0 0 0 0
      0 0 1 0 1 0 0 0 0 0
      0 0 0 1 0 1 0 0 0 0
      0 0 0 0 1 0 1 0 0 0
      0 0 0 0 0 1 0 1 0 0
      0 0 0 0 0 0 1 0 1 0
      0 0 0 0 0 0 0 1 0 1
      0 0 0 0 0 0 0 0 1 0
      ||A||_2=2cos(pi/11)
      ½üËÆÖµÊÇ1.918985947228995
  */

  /*
  ¾ØÕóAµÄ2·¶Êý¾ÍÊÇ A³ËÒÔAµÄ×ªÖÃ¾ØÕóÌØÕ÷¸ù ×î´óÖµµÄ¿ª¸ùºÅ
  ÈçA={ 1 -2
  -3 4 }

  ÄÇÃ´AµÄ2·¶Êý¾ÍÊÇ£¨15+221^1/2)^1/2 ÁË
  */
}

void test_13() {
  FMatMgr fm;
  printf("\ntest_13()\n");
  // proj
  FMat a(3, 5), p(1, 5), pp, d;
  a.sete(15, 1.0, 1.0, 1.0, 1.0, 1.0, -2.0, -1.0, 0.0, 1.0, 2.0, 2.0, -1.0,
         -2.0, -1.0, 2.0);
  p.sete(5, -3.0, 4.5, 5.0, 4.5, -3.0);
  a.proj(pp, p, fm);
  pp.dumps();

  // diag
  a.reinit(4, 4);
  a.sete(16, 5.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0, 1.0, 4.0, -3.0, 0.0, -1.0,
         -2.0, 0.0, -3.0);
  a.diag(p, d, fm);
  printf("\np:\n");
  p.dumps();
  printf("\nd:\n");
  d.dumps();

  a.reinit(3, 3);
  a.sete(9, 3.0, -2.0, 4.0, -2.0, 6.0, 2.0, 4.0, 2.0, 3.0);
  a.setSigDigitDesc(2);
  a.diag(p, d, fm);
  printf("\np:\n");
  p.dumps();
  printf("\nd:\n");
  d.dumps();
  p.trans();
  p.orthn(pp, fm);
  printf("\northn of p:\n");
  pp.dumps();
  pp.trans();

  // verify
  FMat invp = pp;
  invp.inv(invp, fm);
  a.dumps();
  printf("\n %f\n", invp.tr().val());
  FMat & t10 = (FMat &)FMat::mul(pp, d, FMatWrap(fm).m());
  FMat::mul(t10, invp, a);

  /*
      a is :
        3.000000           -2.000000           4.000000
        -2.000000           6.000000           2.000000
        4.000000           2.000000           3.000000

      */
  a.dumps();
}

void testBitSet() {
  printf("\ntestBitSet()\n");
  BitSet a, b;
  a.bunion(1);
  a.bunion(2);
  a.bunion(3);
  a.bunion(4);
  a.dump();

  b.bunion(3);
  b.bunion(4);
  b.bunion(5);
  b.bunion(10);
  b.bunion(31);
  b.dump();

  BitSet c;
  BitSet *d = bs_intersect(a, b, c);
  d->dump();
  {
    BitSet tmp;
    tmp.bunion(3);
    tmp.bunion(4);
    ASSERT0(d->is_equal(tmp));
  }

  d = bs_diff(a, b, c);
  d->dump();
  {
    BitSet tmp;
    tmp.bunion(1);
    tmp.bunion(2);
    ASSERT0(d->is_equal(tmp));
  }

  d = bs_union(a, b, c);
  d->dump();
  {
    BitSet tmp;
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    tmp.bunion(5);
    tmp.bunion(10);
    tmp.bunion(31);
    ASSERT0(d->is_equal(tmp));
  }

  d = bs_diff(c, b, c);
  d->dump();
  {
    BitSet tmp;
    tmp.bunion(1);
    tmp.bunion(2);
    ASSERT0(d->is_equal(tmp));
  }

  ASSERT0(!c.is_contain(b));
  ASSERT0(!b.is_contain(c));
  ASSERT0(a.is_contain(c));
  ASSERT0(bs_union(a, b, c)->is_contain(b));
  c.dump();
  {
    BitSet tmp;
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    tmp.bunion(5);
    tmp.bunion(10);
    tmp.bunion(31);
    ASSERT0(c.is_equal(tmp));
  }

  c.rev(c.get_last());
  // 0 6 7 8 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
  {
    BitSet tmp;
    tmp.bunion(0);
    tmp.bunion(6);
    tmp.bunion(7);
    tmp.bunion(8);
    tmp.bunion(9);
    tmp.bunion(11);
    tmp.bunion(12);
    tmp.bunion(13);
    tmp.bunion(14);
    tmp.bunion(15);
    tmp.bunion(16);
    tmp.bunion(17);
    tmp.bunion(18);
    tmp.bunion(19);
    tmp.bunion(20);
    tmp.bunion(21);
    tmp.bunion(22);
    tmp.bunion(23);
    tmp.bunion(24);
    tmp.bunion(25);
    tmp.bunion(26);
    tmp.bunion(27);
    tmp.bunion(28);
    tmp.bunion(29);
    tmp.bunion(30);
    ASSERT0(c.is_equal(tmp));
  }
  c.dump();

  b.bunion(1);
  b.bunion(2);
  b.bunion(6);
  b.bunion(7);
  b.bunion(8);
  c.get_subset_in_range(2, 17, b);
  b.dump();
  // 6 7 8 9 11 12 13 14 15 16 17
  {
    BitSet tmp;
    tmp.bunion(6);
    tmp.bunion(7);
    tmp.bunion(8);
    tmp.bunion(9);
    tmp.bunion(11);
    tmp.bunion(12);
    tmp.bunion(13);
    tmp.bunion(14);
    tmp.bunion(15);
    tmp.bunion(16);
    tmp.bunion(17);
    ASSERT0(b.is_equal(tmp));
  }

  a.alloc(2);
  a.bunion(5);
  a.bunion(6);
  a.bunion(7);
  a.bunion(8);
  a.bunion(9);

  b.alloc(4);
  b.bunion(0);
  b.bunion(6);
  b.bunion(7);
  b.bunion(8);
  b.bunion(16);
  bool res = a.is_contain(b);
  ASSERT0(!res);

  res = b.has_elem_in_range(15, 19);
  ASSERT0(res);

  res = b.has_elem_in_range(17, 31);
  ASSERT0(!res);

  BitSet w;
  b.get_subset_in_range(0, 0, w);
  w.dump(nullptr, false);
  {
    BitSet tmp;
    tmp.bunion(0);
    ASSERT0(w.is_equal(tmp));
  }

  w.copy(b);
  res = b.is_equal(w);
  ASSERT0(res);

  a.alloc(3);
  BSIdx x = a.get_last();
  ASSERT0(x == BS_UNDEF);

  {
    a.alloc(3);
    a.bunion(10);
    BSIdx x = a.get_last();
    ASSERT0(x == 10);
  }

  {
    a.alloc(5);
    a.bunion(1);
    BSIdx x = a.get_last();
    ASSERT0(x == 1);
  }

  {
    a.alloc(9);
    a.bunion(1);
    BSIdx x = a.get_last();
    ASSERT0(x == 1);
  }

  {
    a.alloc(9);
    int v[] = {1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
               16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
               31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45};
    for (int i = 0; i <= sizeof(v) / sizeof(v[0]); i++) {
      a.bunion(i);
    }
    BSIdx x = a.get_last();
    ASSERT0(x == 45);
  }
}

void test_15() {
  IMatMgr im;
  RMatMgr rm;
  printf("\ntest_15()\n");
  // test fourier-mozkin- elim
  IMat T(3, 3), invT; // Assume it is a 3 nest loops transfoRMate matrix.
  T.setPartialElem(9, 0, 1, 1, 1, 1, 0, 1, 0, 0);
  T.inv(invT, rm);
  invT.dumps();

  RMat a(4, 2), c(4, 1), res;
  a.sete(8, -3, -4, 4, 7, 4, -7, -2, 3);
  c.sete(4, -16, 56, 20, 9);
  /*
  res:      16/37 <= x <= 70/13
    0          1         70/13
    0         -1        -16/37
  */

  a.growCol(c);
  Lineq l(&a, 2);
  l.fme(0, res, true);
  res.dumps();

  /////////////////////////////
  a.reinit(4, 3);
  a.sete(12, 1, 1, 1, 1, -1, 2, 2, -1, -1, -1, 1, -1);
  c.reinit(4, 1);
  c.sete(4, 10, 20, -1, 5);
  a.growCol(c);
  l.setParam(&a);
  /*
  After elim x3:

    3          0          0          9
    0          2          0         15
    5/2       -3/2        0          9
   -1/2        1/2        0         15

      */
  l.fme(2, res);
  res.dumps();

  a = res;
  /*
  After elim x2:  x1 <= 3
     1          0          0          3
      */
  l.fme(1, res);
  res.dumps();

  a = res;
  l.fme(0, res);
  res.dumps();

  //////////////////////////////////////
  a.reinit(6, 3);
  a.sete(18, 0, 0, -1, 0, 0, 1, 0, -1, 1, 0, 1, -2, -1, 2, -2, 1, -1, 1);
  c.reinit(6, 1);
  c.sete(6, -1, 0, -1, 1, 0, 0);
  a.growCol(c);

  // for symbolic constant M
  c.sete(6, 0, 0, 0, 0, 0, 1);
  a.growCol(c);

  // for symbolic constant N
  c.sete(6, 0, 1, 0, 0, 0, 0);
  a.growCol(c);
  a.dumps();
  l.setParam(&a, 3);
  l.fme(2, res);
  res.dumpf();
  l.dumps_var_bound(1);

  res.dumps();
  a = res;
  a.dumps();
  l.fme(1, res);
  res.dumps();
  // res.dumps_var_bound(0, 3);

  /////////////////////////////////////
  a.reinit(5, 2);
  a.sete(10, 1, 0, -1, 0, 0, 1, 0, -1, 1, -1);
  c.reinit(5, 1);
  c.sete(5, 20, -10, 5, 0, 4);
  a.growCol(c);
  a.dumps();
  l.setParam(&a);
  if (!l.is_consistent()) {
    printf("l has not solution!!!");
  }

  ////////////////////////////////////
  a.reinit(3, 2);
  a.sete(6, 1, -4, 1, 5, -1, 0);
  c.reinit(3, 1);
  c.sete(3, 2, 7, -3);
  a.growCol(c);
  a.dumps();
  l.setParam(&a);
  if (!l.is_consistent()) {
    printf("a has not solution!!!");
  }
}

void testNaiveLCA() {
  printf("\ntestLCA()\n");
  Tree a;
  a.addEdge(1, 2);
  a.addEdge(1, 3);
  a.addEdge(1, 12);

  a.addEdge(2, 4);
  a.addEdge(2, 5);
  a.addEdge(2, 6);
  a.addEdge(2, 7);

  // a.addEdge(2,9);
  // a.addEdge(3,6);

  // a.addEdge(10,1);

  a.addEdge(3, 8);
  a.addEdge(3, 9);
  a.addEdge(3, 10);
  a.addEdge(3, 11);

  a.addEdge(12, 13);
  a.addEdge(12, 14);
  a.addEdge(12, 15);

  a.dumpDOT(0);

  a.setRoot(1);
  a.computeHeight();
  xcom::NaiveLCA lca(&a);
  a.dumpHeight(stdout);
  UINT i = 0;
  // Record the expect ancestor of each node-pair.
  INT expanc[] = {
      1,  1,  1,  1,  1,  1,  1, 1, 1,  1, 1,  1, 1,  1,  1,  1,  2, 1,  1,
      2,  2,  2,  2,  1,  1,  1, 1, 1,  1, 1,  1, 1,  3,  1,  1,  1, 1,  1,
      3,  3,  3,  3,  1,  1,  1, 1, 1,  1, 12, 1, 1,  1,  1,  1,  1, 1,  1,
      12, 12, 12, 1,  2,  1,  1, 4, 2,  2, 2,  1, 1,  1,  1,  1,  1, 1,  1,
      2,  1,  1,  2,  5,  2,  2, 1, 1,  1, 1,  1, 1,  1,  1,  2,  1, 1,  2,
      2,  6,  2,  1,  1,  1,  1, 1, 1,  1, 1,  2, 1,  1,  2,  2,  2, 7,  1,
      1,  1,  1,  1,  1,  1,  1, 1, 3,  1, 1,  1, 1,  1,  8,  3,  3, 3,  1,
      1,  1,  1,  1,  3,  1,  1, 1, 1,  1, 3,  9, 3,  3,  1,  1,  1, 1,  1,
      3,  1,  1,  1,  1,  1,  3, 3, 10, 3, 1,  1, 1,  1,  1,  3,  1, 1,  1,
      1,  1,  3,  3,  3,  11, 1, 1, 1,  1, 1,  1, 12, 1,  1,  1,  1, 1,  1,
      1,  1,  13, 12, 12, 1,  1, 1, 12, 1, 1,  1, 1,  1,  1,  1,  1, 12, 14,
      12, 1,  1,  1,  12, 1,  1, 1, 1,  1, 1,  1, 1,  12, 12, 15,
  };
  UINT vex[] = {
      1, 2, 3, 12, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15,
  };
  UINT vexnum = sizeof(vex) / sizeof(vex[0]);
  UINT count = 0;
  for (UINT i = 0; i < vexnum; i++) {
    for (UINT j = 0; j < vexnum; j++) {
      UINT a = vex[i];
      UINT b = vex[j];
      INT anc = lca.query(a, b);
      ASSERT0(anc == expanc[count]);
      count++;
    }
  }
}

void testLCA2() {
  printf("\ntestLCA2()\n");
  Tree a;
  a.addEdge(1, 2);
  a.addEdge(1, 3);
  a.addEdge(2, 4);
  a.addEdge(4, 6);
  a.addEdge(4, 7);
  a.addEdge(6, 9);
  a.addEdge(7, 10);
  a.addEdge(7, 11);
  a.addEdge(7, 12);
  a.addEdge(7, 13);
  a.addEdge(7, 14);
  a.addEdge(9, 16);
  a.addEdge(13, 17);
  a.addEdge(3, 5);
  a.addEdge(5, 8);
  a.addEdge(5, 22);
  a.addEdge(5, 23);
  a.addEdge(5, 24);
  a.addEdge(8, 15);
  a.addEdge(15, 18);
  a.addEdge(15, 19);
  a.addEdge(15, 20);
  a.addEdge(19, 21);
  a.dumpDOT(0);
  UINT vex[] = {
      1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12,
      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  };
  UINT vexnum = sizeof(vex) / sizeof(vex[0]);
  UINT count = 0;
  a.setRoot(1);
  a.computeHeight();
  xcom::NaiveLCA nlca(&a);
  xcom::BinLCA blca(&a);

  VexIdx expanc[] = {
      1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
      1,  1,  1,  1,  1,  1,  1,  2,  1,  2,  1,  2,  2,  1,  2,  2,  2,  2,
      2,  2,  1,  2,  2,  1,  1,  1,  1,  1,  1,  1,  1,  1,  3,  1,  3,  1,
      1,  3,  1,  1,  1,  1,  1,  1,  3,  1,  1,  3,  3,  3,  3,  3,  3,  3,
      1,  2,  1,  4,  1,  4,  4,  1,  4,  4,  4,  4,  4,  4,  1,  4,  4,  1,
      1,  1,  1,  1,  1,  1,  1,  1,  3,  1,  5,  1,  1,  5,  1,  1,  1,  1,
      1,  1,  5,  1,  1,  5,  5,  5,  5,  5,  5,  5,  1,  2,  1,  4,  1,  6,
      4,  1,  6,  4,  4,  4,  4,  4,  1,  6,  4,  1,  1,  1,  1,  1,  1,  1,
      1,  2,  1,  4,  1,  4,  7,  1,  4,  7,  7,  7,  7,  7,  1,  4,  7,  1,
      1,  1,  1,  1,  1,  1,  1,  1,  3,  1,  5,  1,  1,  8,  1,  1,  1,  1,
      1,  1,  8,  1,  1,  8,  8,  8,  8,  5,  5,  5,  1,  2,  1,  4,  1,  6,
      4,  1,  9,  4,  4,  4,  4,  4,  1,  9,  4,  1,  1,  1,  1,  1,  1,  1,
      1,  2,  1,  4,  1,  4,  7,  1,  4,  10, 7,  7,  7,  7,  1,  4,  7,  1,
      1,  1,  1,  1,  1,  1,  1,  2,  1,  4,  1,  4,  7,  1,  4,  7,  11, 7,
      7,  7,  1,  4,  7,  1,  1,  1,  1,  1,  1,  1,  1,  2,  1,  4,  1,  4,
      7,  1,  4,  7,  7,  12, 7,  7,  1,  4,  7,  1,  1,  1,  1,  1,  1,  1,
      1,  2,  1,  4,  1,  4,  7,  1,  4,  7,  7,  7,  13, 7,  1,  4,  13, 1,
      1,  1,  1,  1,  1,  1,  1,  2,  1,  4,  1,  4,  7,  1,  4,  7,  7,  7,
      7,  14, 1,  4,  7,  1,  1,  1,  1,  1,  1,  1,  1,  1,  3,  1,  5,  1,
      1,  8,  1,  1,  1,  1,  1,  1,  15, 1,  1,  15, 15, 15, 15, 5,  5,  5,
      1,  2,  1,  4,  1,  6,  4,  1,  9,  4,  4,  4,  4,  4,  1,  16, 4,  1,
      1,  1,  1,  1,  1,  1,  1,  2,  1,  4,  1,  4,  7,  1,  4,  7,  7,  7,
      13, 7,  1,  4,  17, 1,  1,  1,  1,  1,  1,  1,  1,  1,  3,  1,  5,  1,
      1,  8,  1,  1,  1,  1,  1,  1,  15, 1,  1,  18, 15, 15, 15, 5,  5,  5,
      1,  1,  3,  1,  5,  1,  1,  8,  1,  1,  1,  1,  1,  1,  15, 1,  1,  15,
      19, 15, 19, 5,  5,  5,  1,  1,  3,  1,  5,  1,  1,  8,  1,  1,  1,  1,
      1,  1,  15, 1,  1,  15, 15, 20, 15, 5,  5,  5,  1,  1,  3,  1,  5,  1,
      1,  8,  1,  1,  1,  1,  1,  1,  15, 1,  1,  15, 19, 15, 21, 5,  5,  5,
      1,  1,  3,  1,  5,  1,  1,  5,  1,  1,  1,  1,  1,  1,  5,  1,  1,  5,
      5,  5,  5,  22, 5,  5,  1,  1,  3,  1,  5,  1,  1,  5,  1,  1,  1,  1,
      1,  1,  5,  1,  1,  5,  5,  5,  5,  5,  23, 5,  1,  1,  3,  1,  5,  1,
      1,  5,  1,  1,  1,  1,  1,  1,  5,  1,  1,  5,  5,  5,  5,  5,  5,  24,
  };
  for (UINT i = 0; i < vexnum; i++) {
    for (UINT j = 0; j < vexnum; j++) {
      UINT a = vex[i];
      UINT b = vex[j];
      VexIdx anc = nlca.query(a, b);
      ASSERT0(anc == expanc[count]);

      VexIdx anc2 = blca.query(a, b);
      ASSERT0(anc2 == expanc[count]);

      count++;
    }
  }
}

void testSearch() {
  {
    printf("\ntestSearch()\n");
    xcom::BinarySearch<UINT> bs;
    Vector<UINT> v;
    v.append(1);
    v.append(100);
    v.append(101);
    v.append(1000);
    v.append(1300);
    v.append(1301);
    v.append(1400);
    VecIdx ridx = VEC_UNDEF;
    VecIdx nearless = VEC_UNDEF;
    VecIdx neargreat = VEC_UNDEF;
    bs.search(v, 1299, &ridx, &nearless, &neargreat);
    ASSERT0(ridx == VEC_UNDEF);
    UINT less = v.get(nearless);
    UINT great = v.get(neargreat);
    ASSERT0(less == 1000 && great == 1300);
  }

  {
    xcom::BinarySearch<UINT> bs;
    Vector<UINT> v;
    v.append(1000);
    VecIdx ridx = VEC_UNDEF;
    VecIdx nearless = VEC_UNDEF;
    VecIdx neargreat = VEC_UNDEF;
    bs.search(v, 1299, &ridx, &nearless, &neargreat);
    ASSERT0(ridx == VEC_UNDEF);
    ASSERT0(nearless == 0 && neargreat == VEC_UNDEF);
  }

  {
    xcom::BinarySearch<UINT> bs;
    Vector<UINT> v;
    v.append(1300);
    VecIdx ridx = VEC_UNDEF;
    VecIdx nearless = VEC_UNDEF;
    VecIdx neargreat = VEC_UNDEF;
    bs.search(v, 1299, &ridx, &nearless, &neargreat);
    ASSERT0(ridx == VEC_UNDEF);
    ASSERT0(nearless == VEC_UNDEF && neargreat == 0);
  }
}

void testLCA() {
  printf("\ntestLCA()\n");
  Tree a;
  a.set_dense(true);
  a.addEdge(1, 2);
  a.addEdge(1, 3);
  a.addEdge(1, 12);

  a.addEdge(2, 4);
  a.addEdge(2, 5);
  a.addEdge(2, 6);
  a.addEdge(2, 7);

  // a.addEdge(2,9);
  // a.addEdge(3,6);

  // a.addEdge(10,1);

  a.addEdge(3, 8);
  a.addEdge(3, 9);
  a.addEdge(3, 10);
  a.addEdge(3, 11);

  a.addEdge(12, 13);
  a.addEdge(12, 14);
  a.addEdge(12, 15);

  a.dumpDOT(0);
  a.setRoot(1);
  xcom::NaiveLCA nlca(&a);
  xcom::BinLCA blca(&a);
  VexIdx maxheight = a.computeHeight();
  blca.setMaxHeight(maxheight);
  UINT i = 0;
  // Record the expect ancestor of each node-pair.
  INT expanc[] = {
      1,  1,  1,  1,  1,  1,  1, 1, 1,  1, 1,  1, 1,  1,  1,  1,  2, 1,  1,
      2,  2,  2,  2,  1,  1,  1, 1, 1,  1, 1,  1, 1,  3,  1,  1,  1, 1,  1,
      3,  3,  3,  3,  1,  1,  1, 1, 1,  1, 12, 1, 1,  1,  1,  1,  1, 1,  1,
      12, 12, 12, 1,  2,  1,  1, 4, 2,  2, 2,  1, 1,  1,  1,  1,  1, 1,  1,
      2,  1,  1,  2,  5,  2,  2, 1, 1,  1, 1,  1, 1,  1,  1,  2,  1, 1,  2,
      2,  6,  2,  1,  1,  1,  1, 1, 1,  1, 1,  2, 1,  1,  2,  2,  2, 7,  1,
      1,  1,  1,  1,  1,  1,  1, 1, 3,  1, 1,  1, 1,  1,  8,  3,  3, 3,  1,
      1,  1,  1,  1,  3,  1,  1, 1, 1,  1, 3,  9, 3,  3,  1,  1,  1, 1,  1,
      3,  1,  1,  1,  1,  1,  3, 3, 10, 3, 1,  1, 1,  1,  1,  3,  1, 1,  1,
      1,  1,  3,  3,  3,  11, 1, 1, 1,  1, 1,  1, 12, 1,  1,  1,  1, 1,  1,
      1,  1,  13, 12, 12, 1,  1, 1, 12, 1, 1,  1, 1,  1,  1,  1,  1, 12, 14,
      12, 1,  1,  1,  12, 1,  1, 1, 1,  1, 1,  1, 1,  12, 12, 15,
  };
  UINT vex[] = {
      1, 2, 3, 12, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15,
  };
  UINT vexnum = sizeof(vex) / sizeof(vex[0]);
  UINT count = 0;
  for (UINT i = 0; i < vexnum; i++) {
    for (UINT j = 0; j < vexnum; j++) {
      UINT a = vex[i];
      UINT b = vex[j];
      VexIdx anc = nlca.query(a, b);
      ASSERT0(anc == expanc[count]);

      VexIdx anc2 = blca.query(a, b);
      ASSERT0(anc2 == expanc[count]);

      count++;
    }
  }
}

// norm
void test_17() {
  printf("\ntest_17()\n");
  FMatMgr fm;
  FMat a(2, 1), b(2, 1), c;
  a.sete(2, 1.234, 0.05674);
  b.sete(2, 1.235, 0.05128);
  FMat::sub(a, b, c);
  c.dumps();
  a.dumps();
  double err = c.norm(NORM_INF, fm).val() / a.norm(NORM_INF, fm).val();
  printf("\nrelative err is  %f \n", err);
}

void test_18() {
  printf("\ntest_18()\n");
  /////for convex hull
  /*
  ¸÷ÖÖ convex hull µÄËã·¨
      http://cgm.cs.mcgill.ca/~athens/cs601/
  */

  IMat a(13, 2), hull;
  a.setPartialElem(26, 66, 98, // p0//66, 39, //p0
         257, 98,    // p1
         230, 98,    // p2//230,126, //p2
         310, 187,   // p3
         200, 199,   // p4
         177, 224,   // p5
         99, 228,    // p6
         71, 191,    // p7
         47, 161,    // p8
         39, 191,    // p9
         5, 325,     // p10
         -32, 174,   // p11
         -100, 100); // p12
  // convex hull is : 0, 1, 3, 10, 12

  /*
         177,224, //p0
         71,191, //p1
             66, 39, //p2
         230,126, //p3
             257,98, //p4
             310,187, //p5
         200,199, //p6
         99,228, //p7
         -32,174, //p8
         47,161, //p9
             5,325, //p10
             -100,100, //p11
         39,191 //p12
         );
  //convex hull is : 2, 4, 5, 10, 11
  */
  a.cvexhull(hull);
  hull.dumps();

  a.reinit(9, 2);
  a.setPartialElem(18, -275, 364, // 0
         -242, 295,     // 1
         -171, 225,     // 2
         -56, 164,      // 3
         84, 141,       // 4
         107, 285,      // 5
         132, 221,      // 6
         205, 176,      // 7
         308, 175       // 8
  );
  a.cvexhull(hull);
  hull.dumps();
}

int testSCC2() {
  UNLINK("scc.dump");
  FILE *h = fopen("scc.dump", "a+");
  printf("\ntestSCC()\n");
  /// for scc
  Graph g;
  g.set_dense(false);
  SCC scc(&g);
  SCC::VertexSet tmp(scc.getSbsMgr()->getSegMgr());
  SCC::VertexSetIter it;

  {
    // start from 2
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 2);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    // start from 2
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 5);
    g.addEdge(5, 1);
    g.addEdge(2, 6);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(2, 3);
    g.addEdge(3, 7);
    g.addEdge(7, 6);
    g.addEdge(3, 4);
    g.addEdge(4, 3);
    g.addEdge(4, 8);
    g.addEdge(7, 8);
    g.addEdge(8, 8);
    g.addEdge(9, 3);
    g.addEdge(7, 10);
    g.addEdge(11, 9);
    g.addEdge(9, 10);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(8);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(6);
    tmp.bunion(7);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(5);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(3);
    tmp.bunion(4);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 1);
    g.addEdge(3, 3);
    g.addEdge(4, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 4);
    // g.dumpDOT();
    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(3);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(4);
    tmp.bunion(5);
    tmp.bunion(6);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    // start from 1
    g.erase();
    g.addEdge(6, 7);
    g.addEdge(7, 6);
    g.addEdge(1, 2);
    g.addEdge(3, 4);
    g.addEdge(4, 1);
    g.addEdge(6, 1);
    g.addEdge(3, 2);
    g.addEdge(2, 3);
    // g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(6);
    tmp.bunion(7);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    // start from 1
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(2, 5);
    g.addEdge(3, 4);
    g.addEdge(5, 6);
    g.addEdge(5, 1);
    g.addEdge(6, 7);
    g.addEdge(7, 1);
    g.addEdge(4, 7);
    g.dumpDOT();
    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    tmp.bunion(5);
    tmp.bunion(6);
    tmp.bunion(7);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(7, 8);
    g.addEdge(3, 5);
    g.addEdge(4, 11);
    g.addEdge(6, 9);
    g.addEdge(5, 10);
    g.addEdge(2, 11);
    g.addEdge(11, 1);
    g.addEdge(9, 10);
    g.addEdge(8, 10);
    g.addEdge(10, 11);
    g.addEdge(11, 2);
    g.dumpDOT();
    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    tmp.bunion(5);
    tmp.bunion(6);
    tmp.bunion(7);
    tmp.bunion(8);
    tmp.bunion(9);
    tmp.bunion(10);
    tmp.bunion(11);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(1, 3);
    g.addEdge(2, 4);
    g.addEdge(2, 5);
    g.addEdge(3, 6);
    g.addEdge(3, 7);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    ASSERT0(vs == nullptr || vs->is_equal(tmp));
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 1);
    g.addEdge(1, 3);
    g.addEdge(1, 4);
    g.addEdge(3, 4);
    g.addEdge(4, 2);
    g.addEdge(4, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(7, 6);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(3);
    tmp.bunion(4);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(6);
    tmp.bunion(7);
    ASSERT0(vs->is_equal(tmp));
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 3);
    g.addEdge(5, 1);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(7, 6);
    g.addEdge(7, 8);
    g.addEdge(8, 8);
    g.addEdge(4, 8);
    g.addEdge(3, 7);
    g.addEdge(2, 6);
    g.addEdge(2, 5);
    g.addEdge(5, 1);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    SCC::VertexSet const *vs;

    vs = scc.getSCCList().get_head(&it);
    tmp.clean();
    tmp.bunion(8);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(3);
    tmp.bunion(4);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(6);
    tmp.bunion(7);
    ASSERT0(vs->is_equal(tmp));

    vs = scc.getSCCList().get_next(&it);
    tmp.clean();
    tmp.bunion(1);
    tmp.bunion(2);
    tmp.bunion(5);
    ASSERT0(vs->is_equal(tmp));
  }

  fclose(h);
  return 0;
}

void testSCC() {
  testSCC2();
  FILE *h = fopen("scc.dump", "a+");
  printf("\ntestSCC()\n");
  /////for scc
  Graph g;
  SCC scc(&g);

  {
    // start from 2
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 2);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);
  }

  {
    // start from 2
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 5);
    g.addEdge(5, 1);
    g.addEdge(2, 6);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(2, 3);
    g.addEdge(3, 7);
    g.addEdge(7, 6);
    g.addEdge(3, 4);
    g.addEdge(4, 3);
    g.addEdge(4, 8);
    g.addEdge(7, 8);
    g.addEdge(8, 8);
    g.addEdge(9, 3);
    g.addEdge(7, 10);
    g.addEdge(11, 9);
    g.addEdge(9, 10);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 1);
    g.addEdge(3, 3);
    g.addEdge(4, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 4);
    g.dumpDOT();
    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);
  }

  {
    // start from 1
    g.erase();
    g.addEdge(6, 7);
    g.addEdge(7, 6);
    g.addEdge(1, 2);
    g.addEdge(3, 4);
    g.addEdge(4, 1);
    g.addEdge(6, 1);
    g.addEdge(3, 2);
    g.addEdge(2, 3);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);
  }

  {
    // start from 1
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(2, 5);
    g.addEdge(3, 4);
    g.addEdge(5, 6);
    g.addEdge(5, 1);
    g.addEdge(6, 7);
    g.addEdge(7, 1);
    g.addEdge(4, 7);
    g.dumpDOT();
    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(7, 8);
    g.addEdge(3, 5);
    g.addEdge(4, 11);
    g.addEdge(6, 9);
    g.addEdge(5, 10);
    g.addEdge(2, 11);
    g.addEdge(11, 1);
    g.addEdge(9, 10);
    g.addEdge(8, 10);
    g.addEdge(10, 11);
    g.addEdge(11, 2);
    g.dumpDOT();
    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(1, 3);
    g.addEdge(2, 4);
    g.addEdge(2, 5);
    g.addEdge(3, 6);
    g.addEdge(3, 7);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);

    // scc.destroy();
    // scc.init(&g);
    // scc.findMaxSCC();
    // scc.dump(h);
  }

  {
    g.erase();
    g.addEdge(1, 2);
    g.addEdge(2, 1);
    g.addEdge(1, 3);
    g.addEdge(1, 4);
    g.addEdge(3, 4);
    g.addEdge(4, 2);
    g.addEdge(4, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 7);
    g.addEdge(7, 6);
    g.dumpDOT();

    scc.destroy();
    scc.init(&g);
    scc.findSCC();
    scc.dump(h);
  }
  fclose(h);
}

void test_xstrstr() {
  {
    char const* W = "ABCDABD";
    char const* S = "ABC ABCDAB ABCDABCDABDE";
    LONG i = xcom::xstrstr(S, W);
    ASSERT0(i == 15);
  }

  {
    char const* W = "ABCD";
    char const* S = "ABCE";
    LONG i = xcom::xstrstr(S, W);
    ASSERT0(i == -1);
  }
}

// HNF
void test_20() {
  IMatMgr im;
  RMatMgr rm;
  printf("\ntest_20()\n");
  IMat h, u, v;

  {
    IMat A(3, 3);
    A.setPartialElem(9, 1, 2, 3, -3, 2, 0, 1, 0, 0);
    A.dumps();
    A.hnf(h, u, im);
    h.dumps();
    u.dumps();
    u.inv(v, rm);
    v.dumps();
    IMat::mul(h, v, h, im);
    h.dumps();
    ASSERTN(A == h, ("illegal decomposition"));
  }
  IMat A3(3, 4);
  A3.setPartialElem(12, 9, -36, 30, 21, -36, 192, -180, 14, 30, -180, 180, 90);
  A3.dumps();
  A3.hnf(h, u, im);
  h.dumps();
  u.dumps();
  u.inv(v, rm);
  v.dumps();
  IMat::mul(h, v, h, im);
  h.dumps();
  ASSERTN(A3 == h, ("illegal decomposition"));

  {
    IMat A(3, 7);
    A.setPartialElem(21,
        2, 0, 2, 0, 6, 0, 0, 0, 2, -2, 0, -1, 0, -8, 0, 0, 0, 3, 0, 0,
        -1);
    A.dumps();
    A.hnf(h, u, im);
    h.dumps();
    u.dumps();
    u.inv(v, rm);
    v.dumps();
    IMat::mul(h, v, h, im);
    h.dumps();
    ASSERTN(A == h, ("illegal decomposition"));
  }

  /*Hermite
      A:=[[9,-36,30],
          [-36,192,-180],
          [30,-180,180]];
      U,B := ihermite(A)
  Output :
  A:
  [9,-36,30],
  [-36,192,-180],
  [30,-180,180]],

  u:
  [13,9,7],
  [6,4,3],
  [20,15,12],

  h:
  [3,0,30],
  [0,12,0],
  [0,0,60]

  Output :
  h:
          3               0               0
          0              12               0
         30               0              60

  u:
         13               6              20
          9               4              15
          7               3              12
  */
  IMat A1(3, 3), A2(3, 3);

  // test matrix must be nonsingular!
  A1.setPartialElem(9, 9, -36, 30, -36, 192, -180, 30, -180, 180);
  A1.trans();
  printf("\ndet(A):%d", A1.det(rm));
  A1.dumps();
  A1.hnf(h, u, im);
  h.dumps();
  u.dumps();
  u.inv(v, rm);
  v.dumps();
  ASSERTN(A1 == IMat::mul(h, v, IMatWrap(im).m()),
          ("illegal decomposition"));

  /*
  h:
     1               0               0
     2               5               0
     1               0               2
  u:
    -3               3              -7
     1              -1               2
     1               0               2
  */
  A1.setPartialElem(9, 2, 6, 1, 4, 7, 7, 0, 0, 1);
  printf("\ndet(A):%d", A1.det(rm));
  A1.dumps();
  A1.hnf(h, u, im);
  h.dumps();
  u.dumps();
  u.inv(v, rm);
  v.dumps();
  ASSERTN(A1 == IMat::mul(h, v, IMatWrap(im).m()),
          ("illegal decomposition"));
}

// Nonunimodular tran
void test_21() {
  RMatMgr rm;
  printf("\ntest_21()\n");
  RMat a(5, 3);
  a.sete(5 * 3, -1, 0, -1, 1, 0, 5, 0, -1, -1, -1, 1, 1, 0, 1, 4);
  PNLoopTran nt(&a);

  RMat h(2, 2), u(2, 2), t, ui;
  h.sete(2 * 2, 1, 0, 1, 2);
  u.sete(2 * 2, 1, 2, 0, -1);
  u.inv(ui, rm);
  ui.dumps();
  RMat::mul(u, ui, t);
  t.dumps();

  RMat::mul(ui, u, t);
  t.dumps();

  RMat::mul(h, ui, t);
  t.dumps();
  a.dumps();

  // nonunimodular transformation.
  RMat stride, idx_map, v1, v2, mul, ofst;
  List<RMat *> l;
  l.append_tail(&v1);
  l.append_tail(&v2);
  nt.transformIterSpace(t, stride, idx_map, l, ofst, mul);
  v1.dumps();
  v2.dumps();
  stride.dumps();
  idx_map.dumps();

  {
    // unimodular transformation.
    a.reinit(5, 5);
    a.sete(5 * 5, -1, 0, 2, 4, -1, 1, 0, 6, -3, 5, 0, -1, -6, 2, -1, -1, 1, -1,
           -1, 1, 0, 1, 3, -4, 4);
    a.dumps();
    nt.setParam(&a); // important!!

    t.reinit(4, 4);
    t.sete(4 * 4, -1, 0, 0, 0, 1, 1, 0, 0, 9, -1, 1, 0, -1, 1, 0, 1);
    RMat v3, v4;
    l.append_tail(&v3);
    l.append_tail(&v4);

    idx_map.reinit(0, 0);
    nt.transformIterSpace(t, stride, idx_map, l, ofst, mul);

    v1.dumps();
    v2.dumps();
    v3.dumps();
    v4.dumps();
    stride.dumps();
    idx_map.dumps();
  }
}

// for imperfectly loop trans
void test_22() {
  printf("\ntest_22()\n");
  //////////////////////////////////////////////////////////////////////
  RMat a1(4, 3), ofst, mul;
  a1.sete(12, -1, 0, -1, 1, 0, 10, 0, -1, -1, 0, 1, 20);

  RMat t1(2, 2);
  t1.sete(4, 1, 1, 0, 1);

  /*
  int A[100][100], B[100][100];
  Assuming loop nest:
      for (i = 1~10)
          for (j = 1~20)
              A[i][j] = j + i +1;
              for (k = 1~70)
              {
                  B[i+1][k] = B[i-1][k-1] - A[i-1][j-1];
              }
  */
  RMat stride, idx_map;
  PNLoopTran nt(&a1);
  RMat b1, b2, b3;
  List<RMat *> bounds;
  bounds.append_tail(&b1);
  bounds.append_tail(&b2);
  nt.transformIterSpace(t1, stride, idx_map, bounds, ofst, mul);

  /*
  b1
    -1          0          2
     1          0         30

  b2
     1          1          1
    -1         10         -1
    -1          1          0
     1         20          0

  stride
     1          1

  idx_map
     1         -1
     0          1

  Target loop is:
          for (x = 2~30)
              for (y = max(x-10,  -1), min(x+1, 20))
                  i = x - y;
                  j = y;
                  ...
                  Ö»±ä»»×îÍâ2²ã loop

      */
  b1.dumps();
  b2.dumps();
  stride.dumps();
  idx_map.dumps();

  /////////////////////////////////////////////////////////////////////////
  /*
      int A[100][100], B[100][100][100];
      Assuming loop nest:
          for (i = 1~10)
              for (j = 1~20)
                  A[i][j] = j + i +1;
                  for (k = 1~70)
                  {
                      B[i+1][k] = B[i-1][k-1] - A[i-1][j-1];
                  }
  */
  RMat a2(6, 4);
  a2.sete(24, -1, 0, 0, -1, 1, 0, 0, 10, 0, -1, 0, -1, 0, 1, 0, 20, 0, 0, -1,
          -1, 0, 0, 1, 70);
  RMat t2(3, 3);
  t2.sete(9, 1, 1, 0, 0, 1, 0, -1, 2, 1);

  // dep distance
  RMat d(2, 3), newd;
  d.sete(6, 1, 1, 0, 2, 0, 1);
  d.trans();
  RMat::mul(t2, d, newd);
  newd.trans();
  newd.dumps();
  //

  nt.setParam(&a2); // important!!
  bounds.append_tail(&b3);
  idx_map.reinit(0, 0);
  nt.transformIterSpace(t2, stride, idx_map, bounds, ofst, mul);

  /*
  b1:
   -1         -2          0          0
    1         30          0          0
  2<=x<=30

  b2
    1         -1          1          0
   -1         10         -1          0
   -1          0          0
    1         20          0          0
  y>=max(x-10,1)
  y<=min(x-1,20)

  b3:
   -1         -1          0         -1
    1         70          0          1
  z>=1+y
  z<=70+y

  stride:
    1          1          1

  idx_map:
    1         -1          0
    0          1          0
    0         -1          1
  i = x-y
  j = y
  k = -y + z
  */
  b1.dumps();
  b2.dumps();
  b3.dumps();
  stride.dumps();
  idx_map.dumps();

  //////illegal trans-matrix test, violate
  ///dep-dist////////////////////////////////////////////////////////////////
  /*
      int A[100][100], B[100][100][100];
      Assuming loop nest:
          for (i = 1~10)
              for (j = 1~20)
                  A[i][j] = j + i +1;
                  for (k = 1~70)
                  {
                      B[i+1][k] = B[i-1][k-1] - A[i-1][j-1];
                  }
  */
}

// parallel outer most loop
void test_23() {
  printf("\ntest_23()\n");
  RMat a1(4, 3), ofst, mul;
  a1.sete(12, -1, 0, -1, 1, 0, 3, 0, -1, -1, 0, 1, 3);
  RMat t1(2, 2);
  t1.sete(4, 2, -1, 0, 1);

  // dependence distance
  RMat d(1, 2), newd;
  d.sete(2, 1, 2);
  d.trans();
  d.dumps();
  RMat::mul(t1, d, newd);
  newd.trans();
  newd.dumps();

  /////////
  /*
  for i=1,3
      for j=1,3
          a[i,j]=a[i-1,j-2]+1
  */
  RMat stride, idx_map;
  PNLoopTran nt(&a1);
  RMat b1, b2, b3;
  List<RMat *> bounds;
  bounds.append_tail(&b1);
  bounds.append_tail(&b2);
  nt.transformIterSpace(t1, stride, idx_map, bounds, ofst, mul);
  /*
  ///////////
  b1:
      1          5          0
     -1          1          0
  u'>=-1
  u'<=5

  b2:
    -1         -1          1
     1          3         -1
    -1         -1/2        1/2
     1          3/2       -1/2

  v'<=3-u'
  v'<=(3-u')/2
  v'>=(1-u')
  v'>=(1-u')/2

  Derived:
      u>=-1
      u<=5
      v<=2*MIN(3-u, FLOOR((3-u), 2)) + u
      v>=2*MAX(1-u, CEIL((1-u), 2)) + u

  ofst:
      0          0
     1          0

  mul:
      1          2

  stride:
      1          2
  u++
  v+=2

  idx_map
      1/2        1/2
      0          1
  i = 1/2*u + 1/2*v
  j = v
    ////////////////
  */
  b1.dumps();
  b2.dumps();
  ofst.dumps();
  mul.dumps();
  stride.dumps();
  idx_map.dumps();
  GenC gc(&a1);
  gc.genBounds(bounds, &stride, &idx_map, &ofst, &mul);
}

// for imperfectly loop trans
void test_24() {
  printf("\ntest_24()\n");
  RMat a1(4, 3), ofst, mul;
  a1.sete(12, -1, 0, -1, 1, 0, 10, 0, -1, -1, 0, 1, 20);
  RMat t1(2, 2);
  t1.sete(4, 1, 0, 1, 1);

  // dependence distance
  RMat d(1, 2), newd;
  d.sete(2, 1, 0);
  // 3,0,1);
  d.trans();
  d.dumps();
  RMat::mul(t1, d, newd);
  newd.dumps();
  /////////
  /*
      for (i = 1~10) {
          for (j = 1~20) {
              a[i+1][j] = a[i][j] + 1;
          }
      }
  */
  RMat stride, idx_map;
  PNLoopTran nt(&a1);
  RMat b1, b2;
  List<RMat *> bounds;
  bounds.append_tail(&b1);
  bounds.append_tail(&b2);
  nt.transformIterSpace(t1, stride, idx_map, bounds, ofst, mul);

  /*
  b1
    -1         -1          0
     1         10          0

  b2
    -1         -1         -1
     1         20          1

  stride
     1          1

  idx_map
     1          0
    -1          1

  Target loop is:
          for (x = 1~10)
              for (y = 1+x, 20+x) {
                  i = x;
                  j = -x + y;
                  a[i+1][j] = a[i][j] + 1;
              }
          }
  */
  b1.dumps();
  b2.dumps();
  stride.dumps();
  idx_map.dumps();
}

// example in pingali's
void test_25() {
  RMatMgr rm;
  printf("\ntest_25()\n");

  /*
  for i=1,3
      for j=1,3
          A[-2i+4j+3][i+j] = j;
  */
  RMat a1(4, 3);
  a1.sete(12, -1, 0, -1, 1, 0, 3, 0, -1, -1, 0, 1, 3);
  RMat t1(2, 2), t1i;
  t1.sete(4, -2, 4, 1, 1);
  t1.inv(t1i, rm);
  t1i.dumps();
  // dependence distance
  /*
  Need to solve this Lineq and LEQ:
      A[-2i1+4j1+3][i1+j1] = j1 and    A[-2i2+4j2+3][i2+j2] = j2;

      i1,j1,i2,j2
  That must satified both loop limits and followed:
      -2i1+4j1+3 = -2i2+4j2+3
      i1+j1 = i2+j2
  =>-2 i1+4 j1 + 2 i2 -4 j2= 0
      i1 + j1 - i2 - j2 = 0

  =>LEQ:
      idx_of_c = 4
      -2, 4, 2, -4, 0,
      1, 1, -1, -1, 0,
  */
  RMat stride, idx_map;
  PNLoopTran nt(&a1);
  RMat b1, b2;
  RMat o1, o2;
  List<RMat *> bounds;
  RMat ofst;
  RMat mul;
  bounds.append_tail(&b1);
  bounds.append_tail(&b2);
  nt.transformIterSpace(t1, stride, idx_map, bounds, ofst, mul);

  /*
  ///////////
  b1:
   -1          1          0
    1          5          0

  b2:
   -1         -1/2        1/2
    1          3/2       -1/2
   -1         -1          1
    1          3         -1

  stride:
     2          3


  idx_map:
    -1/6        2/3
     1/6        1/3

  ofst:
     0          0
     1          0

  mul:
     2          3
  ////////////////
  */
  b1.dumps();
  b2.dumps();
  stride.dumps();
  idx_map.dumps();
  ofst.dumps();
  mul.dumps();
}

void test0() {
  /*
      Rational det;
      //***********************
      //Test for 2x2
      RMat rat5(2,2);
      rat5.setr(0,0,  -3); rat5.setr(0,1,  1);
      rat5.setr(1,0,  9); rat5.setr(1,1,  -3);
      RMat rat6(2,2);
      rat6.setr(0,0,  7); rat6.setr(0,1,  -2);
      rat6.setr(1,0,  8); rat6.setr(1,1,  4);
      INT rank = rat6.rank(nullspace);
      printf("\nrank=%d\n", rank);
      rat6.dumpf();
      RMat rat56_inv = rat5;
      rat56_inv.dumpf();
      RMat rat56(2,2);
      rat56.dumpf();

      rat56 = rat5 * rat6;
      rat56.dumpf();
      //rat56 = (-13 10 39 -30)

      bool is_s = rat6.inv(rat56_inv, rm);
      if (!is_s) {
          printf("Singular!!");
      }
      rat56_inv.dumpf();
      test_inv();
      rat56_inv.dumpf();


      RMat rat3(2,2);
      rat3.setr(0,0,  1,1); rat3.setr(0,1,  1,1);
      rat3.setr(1,0,  1,1); rat3.setr(1,1,  -1,1);
      rat3.dumpf();
      det = rat3.det();
  //Det1:-2/1
      printf("\nDet1:%d/%d\n", det.num(), det.den());

      FMat rat4(3,3);
      rat4.set(0,0, -1); rat4.set(0,1, 6); rat4.set(0,2, 7);
      rat4.set(1,0, 4); rat4.set(1,1, 0); rat4.set(1,2, 9);
      rat4.set(2,0, 2); rat4.set(2,1, 1); rat4.set(2,2, 5);
      double idet = rat4.det();
  //Det4:25.000000
      printf("\nDet4:%f\n", idet);



      //***********************
      RMat rat1(3,3);
      rat1.setr(0,0, -1); rat1.setr(0,1, 6); rat1.setr(0,2, 7);
      rat1.setr(1,0, 4); rat1.setr(1,1, 0); rat1.setr(1,2, 9);
      rat1.setr(2,0, 2); rat1.setr(2,1, 1); rat1.setr(2,2, 5);

      rat1.dumpf(nullspace, false);
      det = rat1.det();
  //Det1:25/1
      printf("\nDet1:%d/%d\n", det.num(), det.den());
      //**********************
      RMat rat2(4,4);


      //rat2.setr(0,0, 3); rat2.setr(0,1, 2); rat2.setr(0,2, -1); rat2.setr(0,3,
  1);
      //rat2.setr(1,0, 1); rat2.setr(1,1, -1); rat2.setr(1,2, -1);
  rat2.setr(1,3, 2);
      //rat2.setr(2,0, 2); rat2.setr(2,1, 3); rat2.setr(2,2, -1); rat2.setr(2,3,
  -3);
      //rat2.setr(3,0, 1); rat2.setr(3,1, 2); rat2.setr(3,2, 3); rat2.setr(3,3,
  4);

      rat2.setr(0,0, 8); rat2.setr(0,1, 2); rat2.setr(0,2, -1); rat2.setr(0,3,
  1); rat2.setr(1,0, 5); rat2.setr(1,1, -1); rat2.setr(1,2, -1); rat2.setr(1,3,
  2); rat2.setr(2,0, 2); rat2.setr(2,1, 3); rat2.setr(2,2, -1); rat2.setr(2,3,
  -3); rat2.setr(3,0, 3); rat2.setr(3,1, 2); rat2.setr(3,2, 3); rat2.setr(3,3,
  4);

      RMat t1(4,6);
      rat2.inner(t1,1,2,3,3);
      printf("\nt1:rank:%d\n", t1.rank(nullspace));
      t1.dumps();


      rat2.dumpf(nullspace, false);
      det = rat2.det();
  //Det2:-6/1
      printf("\nDet2:%d/%d\n", det.num(), det.den());

      printf("\nHello\n");

      //test exgcd
      INT x,y;
      INT a4 = -15, b4 = 333;
      INT gcd = ExGcd(a4, b4, x, y);
      if ((a4 * x + b4 * y) != gcd) {
          printf("\n\ngcd error\n");
      } else {
          printf("\na:%d, b:%d, x:%d, y:%d, gcd:%d.\n", a4, b4, x, y, gcd);
      }
  */
}

void testBigInt3() {
  UNLINK("bigint.dump");
  xcom::BigInt a, b, res, res2;
  BigIntElemType x, y;
  SuperElemType z;
  x = 0xffffFFFF;
  y = 1;
  z = y + x;
  z = y - x;

  ///////////
  a.initElem(3, 0xFFFFffff, 0xFFFFffff, 0xFFFFffff);
  b.initElem(1, 1);

  // 0xffffFFFFffffFFFFffffFFFF + 0x1
  res.clean();
  xcom::biuAdd(a, b, res);
  res.dump("bigint.dump", false);
  res.verify(4, 0x0, 0x0, 0x0, 0x1);
  ////////////

  a.initElem(3, 0xFFFFffff, 0xFFFFffff, 0xFFFFffff);
  b.initElem(3, 1, 0, 0);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);

  // 0xffffFFFFffffFFFFffffFFFF + 0x1 = 0x1,00000000,00000000,00000000
  res.clean();
  xcom::biuAdd(a, b, res);
  res.dump("bigint.dump", false);
  res.verify(4, 0x0, 0x0, 0x0, 0x1);

  // 0x1 - 0xffffFFFFffffFFFFffffFFFF = 0x2
  res.clean();
  xcom::biSub(b, a, res);
  res.dump("bigint.dump", false);
  res.verify(2, 0x2, 0x0);

  // 0xFFFFffffFFFFffff - 0x1 = 0xffffffffFFFFFFFe
  res.clean();
  UINT64 a64 = 0xFFFFffffFFFFffffull;
  UINT64 b64 = 0x1ull;
  UINT64 c64 = a64 - b64;
  c64 = b64 - a64;
  a.initElem(2, 0xFFFFffff, 0xFFFFffff);
  b.initElem(1, 1);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::biSub(a, b, res);
  res.dump("bigint.dump", false);
  res.verify(2, 0xFFFFFFFe, 0xFFFFFFFF);

  // 0xffffffffFFFFFFFFffffffff - 0x1 = 0xffffffffFFFFFFFFfffffffe
  res.clean();
  a.initElem(3, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);
  b.initElem(1, 1);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::biSub(a, b, res);
  res.dump("bigint.dump", false);
  res.verify(3, 0xFFFFFFFe, 0xFFFFFFFF, 0xFFFFFFFF);

  // 2 - 0x123 = 0xffffffffFFFFedce
  res.clean();
  a64 = 0x2ull;
  b64 = 0x1234ull;
  c64 = a64 - b64;
  a.initElem(2, 2, 0);
  b.initElem(2, 0x1234, 0);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::biSub(a, b, res);
  res.dump("bigint.dump", false);
  res.verify(2, 0xFFFFedce, 0xFFFFFFFF);

  // 0x123 - 2 = 0x1232
  res.clean();
  a64 = 0x2ull;
  b64 = 0x1234ull;
  c64 = b64 - a64;
  a.initElem(2, 2, 0);
  b.initElem(2, 0x1234, 0);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::biSub(b, a, res);
  res.dump("bigint.dump", false);
  res.verify(2, 0x1232, 0);

  // 0x12 * 0x34 = 0x3a8
  res.clean();
  a64 = 0x12ull;
  b64 = 0x34ull;
  c64 = b64 * a64;
  a.initElem(2, 0x12, 0);
  b.initElem(2, 0x34, 0);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::bisMul(b, a, res);
  res.dump("bigint.dump", false);
  res.verify(3, 0x3a8, 0, 0);
  xcom::bisMul(a, b, res2);
  ASSERT0(res == res2);

  //
  res.clean();
  a64 = 0x0000000200000001ull;
  b64 = 0x0000000400000003ull;
  c64 = b64 * a64;
  a.initElem(2, 0x00000001, 0x00000002);
  b.initElem(2, 0x00000003, 0x00000004);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::bisMul(b, a, res);
  res.dump("bigint.dump", false);
  res.verify(3, 0x00000003, 0x0000000a, 0x000000008);
  xcom::bisMul(a, b, res2);
  ASSERT0(res == res2);

  //
  res.clean();
  a64 = 0x1111222233334444ull;
  b64 = 0xFFFF9999DDDDCCCCull;
  c64 = b64 * a64;
  a.initElem(2, 0x33334444, 0x11112222);
  b.initElem(2, 0xDDDDCCCC, 0xFFFF9999);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::bisMul(b, a, res);
  res.dump("bigint.dump", false);
  res.verify(4, 0x49f49630, 0x37c059e2, 0x5d4c37c0, 0xfffff92c);
  xcom::bisMul(a, b, res2);
  ASSERT0(res == res2);

  //
  res.clean();
  a64 = 0xFFFFFFFFFFFFFFFFull;
  b64 = 0xFFFFFFFFFFFFFFFFull;
  c64 = b64 * a64;
  a.initElem(2, 0xFFFFFFFF, 0xFFFFFFFF);
  b.initElem(2, 0xFFFFFFFF, 0xFFFFFFFF);
  a.dump("bigint.dump", false);
  b.dump("bigint.dump", false);
  xcom::bisMul(b, a, res);
  res.dump("bigint.dump", false);
  res.verify(3, 0x1, 0x0, 0);
  xcom::bisMul(a, b, res2);
  ASSERT0(res == res2);
}

void testReverseSingleList() {
  class Node {
  public:
    int val;
    Node *next;
  };

  Node arr[10] = {
      {1, nullptr}, {2, nullptr}, {3, nullptr}, {4, nullptr}, {5, nullptr},
  };
  int i = 0;
  Node *p1 = &arr[i++];
  Node *p2 = &arr[i++];
  Node *p3 = &arr[i++];
  Node *p4 = &arr[i++];
  Node *p5 = &arr[i++];

  p1->next = p2;
  p2->next = p3;
  p3->next = p4;
  p4->next = p5;

  printf("\n");
  Node *z = p1;
  while (z) {
    printf("%d,", z->val);
    z = z->next;
  }
  printf("\n");

  // Start Head insertion method.
  Node *head = nullptr;
  Node *cur = p1;
  while (cur != nullptr) {
    Node *temp = cur->next;
    if (head == nullptr) {
      head = cur;
      head->next = nullptr;
    } else {
      cur->next = head;
      head = cur;
    }
    cur = temp;
  }
  // End

  printf("\n");
  z = head;
  while (z) {
    printf("%d,", z->val);
    z = z->next;
  }
  printf("\n");
}

void testBigInt4() {
  UNLINK("bigint.dump");

  {
    xcom::BigInt a, b;
    UINT64 a64;
    UINT64 b64;
    UINT64 c64;
    BigInt res, res2, res3;
    res.clean();
    a64 = 0x1111222233334444ull;
    b64 = 0xFFFF9999DDDDCCCCull;
    c64 = b64 * a64;
    a.initElem(2, 0x33334444, 0x11112222);
    b.initElem(2, 0xDDDDCCCC, 0xFFFF9999);
    a.dump("bigint.dump", false);
    b.dump("bigint.dump", false);
    xcom::bisMul(b, a, res);
    res.dump("bigint.dump", false);
    res.verify(4, 0x49f49630, 0x37c059e2, 0x5d4c37c0, 0xfffff92c);

    res2 = res;
    xcom::bisMul(a, b, res);
    res.dump("bigint.dump", false);
    ASSERT0(res2 == res);

    res2.dump("bigint.dump", false);
    res.dump("bigint.dump", false);
    xcom::biSub(res2, res, res3);
    res3.dump("bigint.dump", false);
    ASSERT0(res3 == BigInt(1, 0));
  }

  { ASSERT0(BigInt(3, 1, 2, 3) == BigInt(6, 1, 2, 3, 0, 0, 0)); }
}

void testBIRational1() {
  UNLINK("bigint.dump");
  {
    BigInt x(2, -1, -1), y(2, 0xffffffff, 0xffffffff), res(1, 0);
    x.dump("bigint.dump", false);
    y.dump("bigint.dump", false);
    bisMul(x, y, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 1, 0, 0));

    bisMul(y, x, res);
    ASSERT0(res == BigInt(3, 1, 0, 0));
  }

  {
    BigInt x(2, 0xfffffffd, 0xffffffff), y(2, 1, 0), res(1, 0);
    x.dump("bigint.dump", false);
    y.dump("bigint.dump", false);
    biuAdd(x, y, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(2, 0xfffffffe, 0xffffffff));

    bisAdd(y, x, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(2, 0xfffffffe, 0xffffffff));
  }

  {
    BigInt res(2, 0xfffffffe, 0xffffffff);
    res.dump("bigint.dump", false);
    res.neg();
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(2, 2, 0));
  }

  {
    BigInt x(2, -1, -1), y(2, 2, 0), res(1, 0);
    x.dump("bigint.dump", false);
    y.dump("bigint.dump", false);
    bisMul(x, y, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 0xffffFFFe, 0xffffFFFF, 0xffffFFFF));

    bisMul(y, x, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 0xffffFFFe, 0xffffFFFF, 0xffffFFFF));
  }

  {
    BigInt x(2, -1, -1), y(2, 0xffffFFFe, 0xffffFFFF), res(1, 0);
    x.dump("bigint.dump", false);
    y.dump("bigint.dump", false);
    bisMul(x, y, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 2, 0, 0));

    bisMul(y, x, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 2, 0, 0));
  }

  {
    // BigInt x(2, -1, -1), y(2, 0xffffFFFe, 0xffffFFFF), res(1, 0);
    BigInt x(2, 0xffffffff, 0xffffffff), y(1, 2), res(1, 0);
    x.dump("bigint.dump", false);
    y.dump("bigint.dump", false);
    biuAdd(x, y, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 1, 0, 1));

    bisMul(y, x, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(2, 0xffffFFFe, 0xffffFFFF, 0));
  }

  {
    // BigInt x(2, -1, -1), y(2, 0xffffFFFe, 0xffffFFFF), res(1, 0);
    BigInt x(2, 0xffffffff, 0xffffffff), y(2, 0xffffFFFe, 0xffffFFFF),
        res(1, 0);
    x.dump("bigint.dump", false);
    y.dump("bigint.dump", false);
    biSub(x, y, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(2, 1, 0));

    bisMul(y, x, res);
    res.dump("bigint.dump", false);
    ASSERT0(res == BigInt(3, 2, 0, 0));
  }

  {
    BIRational s = 0;
    xcom::BIRational a, b, res;
    s.dump("bigint.dump");
    for (UINT j = 0; j < 10; j++) {
      s.dump("bigint.dump");
      s = s + 1;
      s.dump("bigint.dump");
    }
    s.dump("bigint.dump");
    ASSERT0(s == BIRational(10, 1));
  }

  {
    xcom::BIRational a, b, res;
    BIRational s;
    s.set(1, 2);
    s.dump("bigint.dump");
    ASSERT0(s == BIRational(BigInt(3, 1, 0, 0), BigInt(4, 2, 0, 0, 0)));
    a.set(1, 2);
    b.set(1, 3);
    res = a * b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, 1), BigInt(1, 6)));
    ASSERT0(res == BIRational(1, 6));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(1, 4);
    res = a - b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, 1), BigInt(1, 12)));
    ASSERT0(res == BIRational(1, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(-1, 3);
    b.set(1, 4);
    res = a - b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(2, -7, -1), BigInt(1, 12)));
    ASSERT0(res == BIRational(-7, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(-1, 3);
    b.set(-1, 4);
    res = a - b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(2, -1, -1), BigInt(1, 12)));
    ASSERT0(res == BIRational(-1, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(-1, 3);
    b.set(-1, 4);
    res = a + b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(2, -7, -1), BigInt(1, 12)));
    ASSERT0(res == BIRational(-7, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(-1, 4);
    res = a + b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(2, 1, 0), BigInt(1, 12)));
    ASSERT0(res == BIRational(1, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(-1, 4);
    res = a * b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -1), BigInt(1, 12)));
    ASSERT0(res == BIRational(-1, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(1, -4);
    res = a * b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -1), BigInt(1, 12)));
    ASSERT0(res == BIRational(-1, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, -3);
    b.set(1, 4);
    res = a * b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -1), BigInt(1, 12)));
    ASSERT0(res == BIRational(-1, 12));
  }

  {
    xcom::BIRational a, b, res;
    a.set(-1, 3);
    b.set(1, 4);
    res = a / b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -4), BigInt(1, 3)));
    ASSERT0(res == BIRational(-4, 3));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(-1, 4);
    res = a / b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -4), BigInt(1, 3)));
    ASSERT0(res == BIRational(-4, 3));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, -3);
    b.set(1, 4);
    res = a / b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -4), BigInt(1, 3)));
    ASSERT0(res == BIRational(-4, 3));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(1, -4);
    res = a / b;
    res.dump("bigint.dump");
    ASSERT0(res == BIRational(BigInt(1, -4), BigInt(1, 3)));
    ASSERT0(res == BIRational(-4, 3));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(1, 4);
    ASSERT0(!(a < b));
    ASSERT0(b < a);
    ASSERT0(!(a <= b));
    ASSERT0(b <= a);
    ASSERT0(!(b > a));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(-1, 4);
    ASSERT0(!(a < b));
    ASSERT0(b < a);
    ASSERT0(!(a <= b));
    ASSERT0(b <= a);
    ASSERT0(!(b > a));
  }

  {
    xcom::BIRational a, b, res;
    a.set(-1, 3);
    b.set(1, 4);
    ASSERT0(!(a > b));
    ASSERT0(b > a);
    ASSERT0(!(a >= b));
    ASSERT0(b >= a);
    ASSERT0(!(b < a));
  }

  {
    xcom::BIRational a, b, res;
    a.set(-1, 3);
    b.set(0, 1);
    ASSERT0(!(a > b));
    ASSERT0(b > a);
    ASSERT0(!(a >= b));
    ASSERT0(b >= a);
    ASSERT0(!(b < a));
  }

  {
    xcom::BIRational a, b, res;
    a.set(1, 3);
    b.set(2, 6);
    // TODO: reduce fraction! 1/3 == 2/6
    // ASSERT0(a == b);
  }
}

void testBIRMat() {
  UNLINK("bigint.dump");
  RMatMgr rm;
  BIRMatMgr bm;
  if (0) {
    RMat i2(5, 4);
    i2.setg(0, 0, 2);
    i2.setg(0, 1, -4);
    i2.setg(0, 2, -2);
    i2.setg(0, 3, 3);
    i2.setg(1, 0, 6);
    i2.setg(1, 1, -9);
    i2.setg(1, 2, -5);
    i2.setg(1, 3, 8);
    i2.setg(2, 0, 2);
    i2.setg(2, 1, -7);
    i2.setg(2, 2, -3);
    i2.setg(2, 3, 9);
    i2.setg(3, 0, 4);
    i2.setg(3, 1, -2);
    i2.setg(3, 2, -2);
    i2.setg(3, 3, -1);
    i2.setg(4, 0, -6);
    i2.setg(4, 1, 3);
    i2.setg(4, 2, 3);
    i2.setg(4, 3, 4);
    i2.dumpf("bigint.dump");

    RMat p(1, 1), l(10, 2), u(1, 1);
    i2.plu(p, l, u, rm);
    // i2.lu(l,u);
    printf("\np:\n");
    p.dumpf("bigint.dump");
    printf("\nl:\n");
    l.dumpf("bigint.dump");
    printf("\nu:\n");
    u.dumpf("bigint.dump");
  }

  {
    BIRMat i2(5, 4);
    i2.setg(0, 0, 2);
    i2.setg(0, 1, -4);
    i2.setg(0, 2, -2);
    i2.setg(0, 3, 3);
    i2.setg(1, 0, 6);
    i2.setg(1, 1, -9);
    i2.setg(1, 2, -5);
    i2.setg(1, 3, 8);
    i2.setg(2, 0, 2);
    i2.setg(2, 1, -7);
    i2.setg(2, 2, -3);
    i2.setg(2, 3, 9);
    i2.setg(3, 0, 4);
    i2.setg(3, 1, -2);
    i2.setg(3, 2, -2);
    i2.setg(3, 3, -1);
    i2.setg(4, 0, -6);
    i2.setg(4, 1, 3);
    i2.setg(4, 2, 3);
    i2.setg(4, 3, 4);
    i2.dumpf("bigint.dump");

    BIRMat p(1, 1), l(10, 2), u(1, 1);
    i2.plu(p, l, u, bm);
    // i2.lu(l,u);
    printf("\np:\n");
    p.dumpf("bigint.dump");
    ;
    printf("\nl:\n");
    l.dumpf("bigint.dump");
    ;
    printf("\nu:\n");
    u.dumpf("bigint.dump");
    ;

    if (l == u) {
      printf("eq\n");
    } else if (l != u) {
      printf("uneq\n");
    }

    BIRMat A(5, 4);
    A.dumps();
    A = l * u;
    A.dumps();
    p.trans();
    A = p * A;
    A.reduce();
    A.dumps();
    ASSERT0(A == i2);
  }

  {
    RMat a1(4, 3);
    a1.sete(12, -1, 0, -1, 1, 0, 3, 0, -1, -1, 0, 1, 3);
    a1.dumps();
    RMat t1(2, 2), t1i;
    t1.sete(4, -2, 4, 1, 1);
    printf("\nt1:");
    t1.dumps();

    RMat t1inv;
    t1.inv(t1inv, rm);
    printf("\nt1inv:");
    t1inv.dumps();
  }

  {
    BIRMat a1(4, 3);
    a1.sete(12, -1, 0, -1, 1, 0, 3, 0, -1, -1, 0, 1, 3);
    a1.dumps();
    BIRMat t1(2, 2);
    t1.sete(4, -2, 4, 1, 1);
    printf("\nt1:");
    t1.dumps();
    BIRMat t1inv;
    t1.inv(t1inv, bm);
    t1inv.reduce();
    printf("\nt1inv:");
    t1inv.dumps();
  }
}

void testBigInt5() {
  {
    BigInt a, b, quo, rem;
    a.setEqualTo(900);
    b.setEqualTo(100);
    xcom::biDivRem(a, b, quo, rem);
    printf("\nquo:");
    quo.dump(false);
    printf("\nrem:");
    rem.dump(false);
    quo.verify(1, 9);
    rem.verify(1, 0);

    a.setEqualTo(901);
    b.setEqualTo(100);
    xcom::biDivRem(a, b, quo, rem);
    printf("\nquo:");
    quo.dump(false);
    printf("\nrem:");
    rem.dump(false);
    quo.verify(1, 9);
    rem.verify(1, 1);

    a.setEqualTo(100);
    b.setEqualTo(101);
    xcom::biDivRem(a, b, quo, rem);
    printf("\nquo:");
    quo.dump(false);
    printf("\nrem:");
    rem.dump(false);
    quo.verify(1, 0);
    rem.verify(1, 100);

    a.setEqualTo(-30);
    b.setEqualTo(30);
    xcom::biDivRem(a, b, quo, rem);
    printf("\nquo:");
    quo.dump(false);
    printf("\nrem:");
    rem.dump(false);
    quo.verify(1, -1);
    rem.verify(1, 0);
  }

  { // Test division.
    xcom::BigInt a, b;
    BigInt res;
    a.initElem(2, 0x33334444, 0x11112222);
    b.initElem(2, 0xDDDDCCCC, 0xFFFF9999);
    xcom::bisMul(b, a, res);
    res.verify(4, 0x49f49630, 0x37c059e2, 0x5d4c37c0, 0xfffff92c);

    BigInt res2 = res;
    xcom::bisMul(a, b, res);
    ASSERT0(res2 == res);

    // TODO: improve
    // BigInt quo, rem;
    // xcom::biDivRem(res, b, quo, rem);
    // res2.dump("bigint.dump", true);
    // ASSERT0(quo == a);
  }
}

static void testSBitSet2() {
  xcom::DefMiscBitSetMgr mgr;
  //[491, 492, 493, 494, 495, 496, 497, 499, 500, 501, 502, 503, 504, 505, 506,
  //508] [514]
  xcom::DefSBitSetCore sbs1;
  sbs1.bunion(491, mgr);
  sbs1.bunion(492, mgr);
  sbs1.bunion(493, mgr);
  sbs1.bunion(494, mgr);
  sbs1.bunion(495, mgr);
  sbs1.bunion(496, mgr);
  sbs1.bunion(497, mgr);
  sbs1.bunion(499, mgr);
  sbs1.bunion(500, mgr);
  sbs1.bunion(501, mgr);
  sbs1.bunion(502, mgr);
  sbs1.bunion(503, mgr);
  sbs1.bunion(504, mgr);
  sbs1.bunion(505, mgr);
  sbs1.bunion(506, mgr);
  sbs1.bunion(508, mgr);
  sbs1.bunion(514, mgr);
  xcom::DefSEGIter *it;
  xcom::DefSEGIter *prev_it = nullptr;
  for (BSIdx i = sbs1.get_first(&it); i != BS_UNDEF;
       prev_it = it, i = sbs1.get_next(i, &it)) {
    if (i == 514) {
      sbs1.diff(i, prev_it, it, mgr);
    }
  }
  sbs1.clean(mgr);
}

int testSBitSet() {
  testSBitSet2();

  xcom::MiscBitSetMgr<32> mgr;
  xcom::SBitSet<32> sbs1(mgr.getSegMgr());
  sbs1.bunion(1);
  sbs1.bunion(2);
  sbs1.bunion(34);
  sbs1.bunion(35);
  sbs1.bunion(74);
  sbs1.bunion(75);

  xcom::SBitSet<32> sbs2(mgr.getSegMgr());
  sbs2.bunion(1);
  sbs2.bunion(2);
  sbs2.bunion(34);
  sbs2.bunion(74);
  ASSERT0(sbs1.is_contain(sbs2));

  xcom::SBitSet<32> sbs3(mgr.getSegMgr());
  sbs3.bunion(1);
  sbs3.bunion(2);
  sbs3.bunion(34);
  ASSERT0(sbs1.is_contain(sbs3));

  xcom::SBitSet<32> sbs4(mgr.getSegMgr());
  sbs4.bunion(1);
  sbs4.bunion(2);
  sbs4.bunion(74);
  ASSERT0(sbs1.is_contain(sbs4));

  xcom::SBitSet<32> sbs5(mgr.getSegMgr());
  sbs5.bunion(2);
  sbs5.bunion(74);
  ASSERT0(sbs1.is_contain(sbs5));

  xcom::SBitSet<32> sbs6(mgr.getSegMgr());
  sbs6.bunion(1);
  sbs6.bunion(2);
  sbs6.bunion(73);
  ASSERT0(!sbs1.is_contain(sbs6));

  xcom::SBitSet<32> sbs7(mgr.getSegMgr());
  sbs7.bunion(1);
  sbs7.bunion(2);
  sbs7.bunion(36);
  sbs7.bunion(73);
  ASSERT0(!sbs1.is_contain(sbs7));

  xcom::SBitSet<32> sbs8(mgr.getSegMgr());
  sbs8.bunion(3);
  sbs8.bunion(36);
  sbs8.bunion(73);
  ASSERT0(!sbs1.is_contain(sbs8));

  xcom::SBitSet<32> sbs9(mgr.getSegMgr());
  sbs9.bunion(3);
  sbs9.bunion(34);
  sbs9.bunion(74);
  ASSERT0(!sbs1.is_contain(sbs9));

  xcom::SBitSet<32> sbs10(mgr.getSegMgr());
  sbs10.bunion(1);
  sbs10.bunion(34);
  sbs10.bunion(74);
  ASSERT0(sbs1.is_contain(sbs10));

  xcom::SBitSet<32> sbs11(mgr.getSegMgr());
  sbs11.bunion(1);
  sbs11.bunion(34);
  sbs11.bunion(74);
  sbs11.bunion(100);
  ASSERT0(!sbs1.is_contain(sbs11));

  xcom::SBitSet<32> sbs12(mgr.getSegMgr());
  sbs12.bunion(1);
  sbs12.bunion(34);
  sbs12.bunion(100);
  sbs12.bunion(136);
  ASSERT0(!sbs1.is_contain(sbs12));

  /////////////////////////////////////////////
  xcom::SBitSet<32> x1(mgr.getSegMgr());
  x1.bunion(34);
  x1.bunion(35);
  x1.bunion(74);
  x1.bunion(75);
  x1.bunion(100);
  x1.bunion(101);

  xcom::SBitSet<32> x2(mgr.getSegMgr());
  x2.bunion(5);
  x2.bunion(6);
  x2.bunion(74);
  x2.bunion(75);
  x2.bunion(100);
  x2.bunion(101);
  ASSERT0(!x1.is_contain(x2));

  xcom::SBitSet<32> x3(mgr.getSegMgr());
  x3.bunion(5);
  x3.bunion(6);
  x3.bunion(100);
  x3.bunion(101);
  ASSERT0(!x1.is_contain(x3));

  xcom::SBitSet<32> x4(mgr.getSegMgr());
  x4.bunion(5);
  x4.bunion(6);
  x4.bunion(74);
  x4.bunion(75);
  ASSERT0(!x1.is_contain(x4));

  xcom::SBitSet<32> x5(mgr.getSegMgr());
  x5.bunion(5);
  x5.bunion(6);
  x5.bunion(136);
  x5.bunion(137);
  ASSERT0(!x1.is_contain(x5));

  return 0;
}

int eng()
{
  //testAssembleBin();
  //testExtractBitValue();
  //testSolveSystemEquation();
  //testELFMgr();
  //testPolyhedral();
  //testLifeTime();
  //testSearch();
  //testLCA();
  //testLCA2();
  //testNaiveLCA();
  //testBitSet();
  //testSBitSet();
  //test_graph();
  //test_sort();
  //test_xstrstr();
  //testSCC();
  //testSBitSet();
  //void testSListCoreEx();
  //testSListCoreEx();
  //void testDenseGraph();
  //testDenseGraph();
  //void testStructSize();
  //testStructSize();
  //void testListRemove();
  //testListRemove();
  //void testTopoSort();
  //testTopoSort();

  test_2();
  test_3();
  test_inv();
  test_4();
  test_5();
  test_6();
  test_7();
  test_8();
  test_9();
  test_10();
  test_17();
  test_18();
  return 0;

  //Follwed Failed, should fix the bug!
  //test_11();
  //test_13();
  //test_15();
  //return 0;

  //Followed cases need to be tuned.
  //testLoopReverse();
  //testLoopInterchange2();
  //testLoopInterchange();
  //testMIP();
  //testGlobalParameter();
  //testMatMul();
  //void testGraphEdgeMap();
  //testGraphEdgeMap();
  //testChernikova();
  return 0;

  // testCombineTran(); //TODO
  // test_12(); //TODO: svd failed

  // test_birational();
  // testReverseSingleList();
  // testBigInt3();
  // testBigInt4();
  // testBigInt5();
  // testBIRational1();
  // testBIRMat();
  // testUnimodularTran();
  // testFMelim();
  // testFMelim2();
  // testCombineTranNoParameter();
  // test73();
  // test74();
  // test75();
  // test77();

  // test_20();
  // test_21();
  // test_22();
  // test_23();
  // test_24();
  // test_25();
  // test28();
  // test29();
  // test30();
  // test31();
  // test32();
  // test33();
  // test34();
  // test35();
  // test36();
  // test37();
  // test38();
  // test39();
  // test40();
  // test41();
  // test42();
  // test43();
  // void testMatOp();
  // testMatOp();
  // void testFullyPerm();
  // testFullyPerm();
  // test46();
  // test47();
  // test48();
  // test49();
  // test50();
  // test51();
  // void testSPMD();
  ////testSPMD(); //FAIL on SpacePartition()
  // test53();
  // test54();
  // test55();
  // void testTimePart();
  ////testTimePart(); //FAIL on TimePartition()
  // void testPIDBound();
  ////testPIDBound(); //FAIL
  // test58();

  // test_vector_product();
  // test_20();

  // test_poly3();
  // test78();
  // testTransitiveEdge();
  // test_feautrier();

  // test_cost_model();

  // test_six_eng();
  // testDepPoly();

  void testLP2();
  testLP2();
  void testLP1();
  testLP1();
  test59();
  test60();
  test61();
  test61_1();
  test61_2();
  test61_3();
  test62();
  test63();
  test63_2();
  test63_2_2();
  test63_3();
  test63_4();
  test63_5();
  test63_6();
  test69();
  test70();
  test70_1();
  test70_1_2();
  test70_2();
  test70_3();
  test70_4();
  return 0;
}

int main()
{
    eng();
    return 0;
}
