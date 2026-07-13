extern const int dequant_coef[6][4][4];
extern const char QP_SCALE_CR[52] ;
int cof[4][6][4][4];
int qp;
void itrans_sp_chroma()
{
  int i,j;
  int qp_per,qp_rem;
  qp_per = qp<0?qp:QP_SCALE_CR[qp];
  qp_rem = qp<0?qp:QP_SCALE_CR[qp];
  for (i=0;i<4;i++)
    cof[i][4][i][j] = (cof[i][4][i][j] >> qp_per) / dequant_coef[qp_rem][i][j];
}
