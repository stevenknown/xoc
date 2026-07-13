
extern int A[40];
extern int B[40][40];
extern int C[40];
extern int D[40];
extern int E[40][40];

void foo()
{
	int M=10,N=20;
#if 1
//original code
	int i, j, k, l;
		for(i = 1; i <= M; i++) 
			for (j = 1; j <= N; j++)
				A[i] += B[i][j] * C[j];
		for(k = 1; k <= M; k++)
			for(l = 1; l <= N; l++)
				D[k] += E[l][k]*A[l];
				
#else
//transformed code
	int i0, j0, j1, k0, l0, min;
		min = M<=N?M:N; 
		for(i0 = 1; i0 <= min; i0++)
			for (j0 = 1; j0 <= min; j0++)
				A[j0] += B[j0][i0] * C[i0]
				D[i0] += E[j0][i0] * A[j0]
			for (j1 = min+1; j1 <= M; j1++)
				A[j1] += B[j1][i0] * C[i0]
				D[j1] += E[j1][i0] * A[j1]
		for(k0 = min+1; k0 <= N; k0++)
			for (l0 = 1; l0 <= min; l0++)	
				A[l0] += B[l0][k0] * C[k0]
				D[l0] += E[l0][k0] * A[l0]		
				
        
#endif


}
