void matmul(float data[100][100], float weight[100][100], float temp[100][100]) {
  int i,j,k;
  for (int i=0;i<4;i++)
    for (int j=0;j<5;j++)
      temp[i][j]=0;
      for (int k=0;k<3;k++)
        temp[i][j]+=(data[i][k]*weight[k][j]);
}
