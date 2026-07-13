/*!
************************************************************************
* \brief
*    caculate the mv of H.264
************************************************************************
*/
typedef unsigned char UINT8;
typedef short INT16;
INT16 min(INT16, INT16);
INT16 max(INT16, INT16);
UINT8 Partition_8x16;
UINT8 Partition_16x8;

/*!
************************************************************************
* \brief
*    caculate median of H.264
************************************************************************
*/
void h264_MotionPred_MeanMv(INT16* in_p_MvA, INT16* in_p_MvB, INT16* in_p_MvC, INT16* out_p_MvE)
{
  INT16 min_mv, max_mv;

  min_mv = min(in_p_MvA[0], min(in_p_MvB[0], in_p_MvC[0]));
  max_mv = max(in_p_MvA[0], max(in_p_MvB[0], in_p_MvC[0]));
  out_p_MvE[0] = in_p_MvA[0] + in_p_MvB[0] + in_p_MvC[0] - max_mv - min_mv;

  min_mv = min(in_p_MvA[1], min(in_p_MvB[1], in_p_MvC[1]));
  max_mv = max(in_p_MvA[1], max(in_p_MvB[1], in_p_MvC[1]));
  out_p_MvE[1] = in_p_MvA[1] + in_p_MvB[1] + in_p_MvC[1] - max_mv - min_mv;

}

void h264_MotionPred_MotionVector(
                                  UINT8 in_Mb_Partition,
                                  UINT8 in_subMb,
                                  UINT8 in_E_Mb_refidx,

                                  //left A
                                  UINT8 in_A_BlockAvailable,
                                  UINT8 in_A_Mb_refidx,
                                  INT16 *in_p_A_Mv,           //[2]

                                  //top B
                                  UINT8 in_B_BlockAvailable,
                                  UINT8 in_B_Mb_refidx,
                                  INT16 *in_p_B_Mv,           //[2]

                                  //top right C
                                  UINT8 in_C_BlockAvailable,
                                  UINT8 in_C_Mb_refidx,
                                  INT16 *in_p_C_Mv,           //[2]

                                  INT16 *out_p_Mb_MVPred     //[2]
                                  )
{
  INT16  * p_Mb_MVPred;
  p_Mb_MVPred = out_p_Mb_MVPred;

  if(!(in_B_BlockAvailable || in_C_BlockAvailable) && in_A_BlockAvailable)            //p8
  {
    p_Mb_MVPred[0] = in_p_A_Mv[0];
    p_Mb_MVPred[1] = in_p_A_Mv[1];
  }
  else if((in_E_Mb_refidx == in_A_Mb_refidx) && (in_E_Mb_refidx != in_B_Mb_refidx) && (in_E_Mb_refidx != in_C_Mb_refidx)) //p1&p4&p6->p9
  {
    p_Mb_MVPred[0] = in_p_A_Mv[0];
    p_Mb_MVPred[1] = in_p_A_Mv[1];
  }
  else if((in_E_Mb_refidx != in_A_Mb_refidx) && (in_E_Mb_refidx == in_B_Mb_refidx) && (in_E_Mb_refidx != in_C_Mb_refidx)) //p2&p3&p6->p10
  {
    p_Mb_MVPred[0] = in_p_B_Mv[0];
    p_Mb_MVPred[1] = in_p_B_Mv[1];
  }
  else if((in_E_Mb_refidx != in_A_Mb_refidx) && (in_E_Mb_refidx != in_B_Mb_refidx) && (in_E_Mb_refidx == in_C_Mb_refidx)) //p2&p4&p5->p11
  {
    p_Mb_MVPred[0] = in_p_C_Mv[0];
    p_Mb_MVPred[1] = in_p_C_Mv[1];
  }
  else
  {
    if((in_Mb_Partition == Partition_8x16)&&(in_subMb == 0)&&(in_A_Mb_refidx == in_E_Mb_refidx)) //p13&p2&p1->p3
    {
      p_Mb_MVPred[0] = in_p_A_Mv[0];
      p_Mb_MVPred[1] = in_p_A_Mv[1];
    }
    else if((in_Mb_Partition == Partition_8x16)&&(in_subMb == 1)&&(in_C_Mb_refidx == in_E_Mb_refidx)) //p13&p4&p5->p5
    {
      p_Mb_MVPred[0] = in_p_C_Mv[0];
      p_Mb_MVPred[1] = in_p_C_Mv[1];
    }
    else if((in_Mb_Partition == Partition_16x8)&&(in_subMb == 0)&&(in_B_Mb_refidx == in_E_Mb_refidx)) //p12&p2&p3->p6
    {
      p_Mb_MVPred[0] = in_p_B_Mv[0];
      p_Mb_MVPred[1] = in_p_B_Mv[1];
    }
    else if((in_Mb_Partition == Partition_16x8)&&(in_subMb == 1)&&(in_A_Mb_refidx == in_E_Mb_refidx)) //p12&p4&p1->p7
    {
      p_Mb_MVPred[0] = in_p_A_Mv[0];
      p_Mb_MVPred[1] = in_p_A_Mv[1];
    }
    else
      h264_MotionPred_MeanMv(in_p_A_Mv, in_p_B_Mv, in_p_C_Mv, p_Mb_MVPred);
  }
}
