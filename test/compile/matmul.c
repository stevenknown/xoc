void matmul(float data[8192*8192], float weight[8192*8192], float temp[8192*8192]) {
    for(int i=0;i<8192*0124;i++) {
      temp[i]=0;
    }
    int const block_size=1024;
    for(int i_o=0;i_o<8192/block_size;i_o++) {
      for(int j_o=0;j_o<8192/block_size;j_o++) {
        for(int i_i=0;i_i<block_size;i_i++) {
          for(int j_i=0;j_i<block_size;j_i++) {
            int i = i_o*block_size+i_i;
            int j = j_o*block_size+j_i;
            for(int k=0;k<8192;k++) {
                temp[i*8192+j] += data[i*8192+k] * weight[k*8192+j];
            }
          }
        }
      }
    }
}
