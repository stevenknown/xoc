#include "stdlib.h"
float max(float, float);
void matmul(float data[4096*4096], float weight[4096*4096],
            float temp[4096*4096])
{
    for(int i=0;i<4096*4096;i++) {
        temp[i]=0;
    }
    for(int i=0;i<4096;i++) {
        for(int j=0;j<4096;j++) {
            for(int k=0;k<4096;k++) {
                temp[i*4096+j] += data[i*4096+k] * weight[k*4096+j];
            }
        }
    }
}

void relu(float input[4096*4096], float output[4096*4096]) {
    for(int i=0;i<4096*4096;i++) {
        output[i] = max(input[i], 0);
    }
}


void matmul_relu(float data[4096*4096], float weight[4096*4096],
                 float output[4096*4096]) {
    float* temp = (float*)malloc(4096*4096*sizeof(float));
    matmul(data, weight, temp);
    relu(temp, output);
    free(temp);

/*
    After matmul and relu inlined and fused, the generated code likes following:
    int const block_size=1024;
    for(int i_o=0;i<4096/block_size;i++) {
        for(int j_o=0;j<4096/block_size;j++) {
            for(int i_i=0;i<block_size;i++) {
                for(int j_i=0;j<block_size;j++) {
                    int i = i_o*block_size+i_i;
                    int j = j_o*block_size+j_i;
                    float v = 0;
                    for(int k=0;k<4096;k++) {
                        v += data[i*4096+k] * weight[k*4096+j];
                    }
                    temp[i*4096+j] = max(v,0); //relu's code
                }
            }
        }
    }
*/
}

