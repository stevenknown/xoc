#include "ops.h"

void trans(float * out, float const* in, unsigned int n_row, unsigned int n_col)
{
    for (unsigned int i = 0; i < n_row; i++) {
        size_t row_base = i * n_col;
        for (unsigned int j = 0; j < n_col; j++) {
            size_t elem_off = row_base + j;
            float val = *(in + elem_off);
            *(out + j*n_row + i) = val;
        }
    }
}
