void itrans()
{
  int i,j,j1;
  int m5[4];
  int m6[4];
  for (i=0;i<4;i++)
  {
    for (j=0;j<4;j++)
      m5[j]=m5[j];
    for (j=0;j<2;j++)
    {
      m5[j] =m5[j] ? 0 : m6[j];
      m5[j1]=m5[j1] ? 0 : m6[j1];
    }
  }
}
