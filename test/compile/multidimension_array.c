/*
Here some examples Tobias Grosser committed in r163619:

multidim_only_ivs_2d.ll:
; void foo(long n, long m, double A[n][m]) {
;   for (long i = 0; i < n; i++)
;     for (long j = 0; j < m; j++)
;       A[i][j] = 1.0;
; }
; Access function: {{0,+,%m}<%for.i>,+,1}<nw><%for.j>

multidim_only_ivs_3d.ll:
; void foo(long n, long m, long o, double A[n][m][o]) {
;   for (long i = 0; i < n; i++)
;     for (long j = 0; j < m; j++)
;       for (long k = 0; k < o; k++)
;         A[i][j][k] = 1.0;
; }
; Access function: {{{0,+,(%m * %o)}<%for.i>,+,%o}<%for.j>,+,1}<nw><%for.k>

multidim_ivs_and_integer_offsets_3d.ll:
; void foo(long n, long m, long o, double A[n][m][o]) {
;   for (long i = 0; i < n; i++)
;     for (long j = 0; j < m; j++)
;       for (long k = 0; k < o; k++)
;         A[i+3][j-4][k+7] = 1.0;
; }
;
; Access function:
;   {{{(56 + (8 * (-4 + (3 * %m)) * %o) + %A),+,(8 * %m * %o)}<%for.i>,+,
;      (8 * %o)}<%for.j>,+,8}<%for.k>

multidim_only_ivs_3d_cast.ll:
; void foo(int n, int m, int o, double A[n][m][o]) {
;   for (int i = 0; i < n; i++)
;     for (int j = 0; j < m; j++)
;       for (int k = 0; k < o; k++)
;         A[i][j][k] = 1.0;
; }
;
; Access function:
;   {{{%A,+,(8 * (zext i32 %m to i64) * (zext i32 %o to i64))}<%for.i>,+,
;    (8 * (zext i32 %o to i64))}<%for.j>,+,8}<%for.k>

multidim_ivs_and_parameteric_offsets_3d.ll:
; void foo(long n, long m, long o, double A[n][m][o], long p, long q, long r) {
;
;   for (long i = 0; i < n; i++)
;     for (long j = 0; j < m; j++)
;       for (long k = 0; k < o; k++)
;         A[i+p][j+q][k+r] = 1.0;
; }
;
; Access function:
;    {{{((8 * ((((%m * %p) + %q) * %o) + %r)) + %A),+,(8 * %m * %o)}<%for.i>,+,
;        (8 * %o)}<%for.j>,+,8}<%for.k>
*/
long n; long m; long o;
void multidim_only_ivs_2d(double A[10][20])
{
    //Access function: {{0,+,%m}<%for.i>,+,1}<nw><%for.j>
    for (long i = 0; i < n; i++)
      for (long j = 0; j < m; j++)
        A[i][j] = 1.0;
}

void multidim_only_ivs_3d(double A[10][20][30])
{
    //Access function: {{{0,+,(%m * %o)}<%for.i>,+,%o}<%for.j>,+,1}<nw><%for.k>
    for (long i = 0; i < n; i++)
      for (long j = 0; j < m; j++)
        for (long k = 0; k < o; k++)
          A[i][j][k] = 1.0;
}

void multidim_ivs_and_integer_offsets_3d(long n, long m, long o, double A[10][20][30])
{
   // Access function:
   //   {{{(56 + (8 * (-4 + (3 * %m)) * %o) + %A),+,(8 * %m * %o)}<%for.i>,+,
   //      (8 * %o)}<%for.j>,+,8}<%for.k>
   for (long i = 0; i < n; i++)
     for (long j = 0; j < m; j++)
       for (long k = 0; k < o; k++)
         A[i+3][j-4][k+7] = 1.0;
}

void multidim_only_ivs_3d_cast(int n, int m, int o, double A[10][20][30])
{
    // Access function:
    //   {{{%A,+,(8 * (zext i32 %m to i64) * (zext i32 %o to i64))}<%for.i>,+,
    //    (8 * (zext i32 %o to i64))}<%for.j>,+,8}<%for.k>
    for (int i = 0; i < n; i++)
      for (int j = 0; j < m; j++)
        for (int k = 0; k < o; k++)
          A[i][j][k] = 1.0;
}

void multidim_ivs_and_parameteric_offsets_3d(long n, long m, long o, double A[10][20][30], long p, long q, long r)
{
   //Access function:
   //   {{{((8 * ((((%m * %p) + %q) * %o) + %r)) + %A),+,(8 * %m * %o)}<%for.i>,+,
   //       (8 * %o)}<%for.j>,+,8}<%for.k>
   for (long i = 0; i < n; i++)
     for (long j = 0; j < m; j++)
       for (long k = 0; k < o; k++)
         A[i+p][j+q][k+r] = 1.0;
}

void entry()
{
    n = 10;
    m = 20;
    o = 30;
}
