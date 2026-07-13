int A[100];
int B[100];
int C[100];
int foo(int n)
{
   for (int i=0;i<n;i++) {
      if (A[i] > 0) {
          B[i] = C[i];
      }
   }
   return 0;
}

