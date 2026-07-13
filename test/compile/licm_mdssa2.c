//Test LICM hoisited DEF stmt.
int A[6][4][4];
int B[4][6][4][4];
void foo()
{
  int i,j;
  int m[4];
  for (j=0;j<4;j++)
  {
    m[2]=1;
    B[i][j][0][0]=m[i];
  }
  for (i=0;i<4;i++)
    B[i][j][0][0]= A[i][0][0];
}
