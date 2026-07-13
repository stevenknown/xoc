//Test MDSSA update in LICM hoisited DEF stmt.
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
}
