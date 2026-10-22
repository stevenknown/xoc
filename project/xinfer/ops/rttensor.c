#include "ops.h"

unsigned int getRTTensorDTypeByteSize(RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32: return 4;
    case RTTENSOR_DTYPE_F16: return 2;
    case RTTENSOR_DTYPE_BF16: return 2;
    case RTTENSOR_DTYPE_I8: return 1;
    case RTTENSOR_DTYPE_U8: return 1;
    case RTTENSOR_DTYPE_I16: return 2;
    case RTTENSOR_DTYPE_I32: return 2;
    case RTTENSOR_DTYPE_I64: return 8;
    case RTTENSOR_DTYPE_BOOL: return 1;
    case RTTENSOR_DTYPE_F8_E4M3: return 1;
    case RTTENSOR_DTYPE_F8_E5M2: return 1;
    default: XASSERT(0); //TODO:
    }
    return 0;
}
