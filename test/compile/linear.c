#include "stdlib.h"
#define max(a,b) ((a)>=(b)?(a):(b))
void matmul(float data[8192*8192], float weight[8192*8192], float temp[8192*8192]) {
    for(int i=0;i<8192*8192;i++) {
        temp[i]=0;
    }
    for(int i=0;i<8192;i++) {
        for(int j=0;j<8192;j++) {
            for(int k=0;k<8192;k++) {
                temp[i*8192+j] += data[i*8192+k] * weight[k*8192+j];
            }
        }
    }
}

void relu(float input[8192*8192], float output[8192*8192]) {
    for(int i=0;i<8192*8192;i++) {
        output[i] = max(input[i], 0);
    }
}


void linear_and_relu(float data[8192*8192], float weight[8192*8192], float output[8192*8192]) {
    float* temp = (float*)malloc(8192*8192*sizeof(float));
    matmul(data, weight, temp);
    relu(temp, output);
    free(temp);
}
