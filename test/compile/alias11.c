extern double G1[100];
extern double G2[100];

void foo(double *A, double *B);
void swap() {
  double *A = G1;
  double *B = G2;
  A = G1;
  B = G2;
  int i,j;
  for (i = 0; i < 90; i++) {
    for (j = 0; j < 100; j++) {
      B[j] = A[j] + 10; //Actually, B and A are pointer.
      //B and A are alias for conservative purpose because they are
      //being swapped in
      //following code snippet.
    }

    //A and B are being swapped in the outer loop
    //and they access same locations in alternate iterations of the i loop.
    double *tmp;
    tmp = A;
    A = B;
    B = tmp;
  }
  foo(A, B);
}

