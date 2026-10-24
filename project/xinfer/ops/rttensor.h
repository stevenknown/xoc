/*@
Copyright (c) 2026-present, Su Zhenyu steven.known@gmail.com

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _RTTENSOR_H_
#define _RTTENSOR_H_

//This file defineds runtime data structures.
//For maximum compatibility, these data structures are compiled in
//C linkage specification.

#define MAX_TENSOR_DIM_NUM 8

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    RTTENSOR_DTYPE_UNDEF = 0,
    RTTENSOR_DTYPE_F32,
    RTTENSOR_DTYPE_F16,
    RTTENSOR_DTYPE_BF16,
    RTTENSOR_DTYPE_I8,
    RTTENSOR_DTYPE_U8,
    RTTENSOR_DTYPE_I16,
    RTTENSOR_DTYPE_I32,
    RTTENSOR_DTYPE_I64,
    RTTENSOR_DTYPE_BOOL,
    RTTENSOR_DTYPE_F8_E4M3,
    RTTENSOR_DTYPE_F8_E5M2,
} RTTENSOR_DTYPE;

//RTTensor: lightweight view for a tensor, holds metadata and data pointer

#define RTTENSOR_vocab_size(rtt) ((rtt)->shape[0])
#define RTTENSOR_max_seq_len(rtt) ((rtt)->shape[0])
#define RTTENSOR_nemb(rtt) ((rtt)->shape[1])

typedef struct {
    #ifdef _DEBUG_
    unsigned int id;
    #endif

    //data:raw pointer to tensor buffer, no memory owned by this struct
    //     data = mmap_base + tensor file offset
    void * data;

    //dtype:element data type, refer to RTTENSOR_DTYPE enum
    //      dtype = parsed dtype from safetensors json
    RTTENSOR_DTYPE dtype;

    //ndim:number of dimensions
    //     ndim = dimension count
    unsigned int ndim;

    //the total number of elements
    int64_t elem_num;

    //shape:dimension sizes, max MAX_TENSOR_DIM_NUM dimensions supported
    //      shape[] = each dimension length
    int64_t shape[MAX_TENSOR_DIM_NUM];
} RTTensor;

unsigned int getRTTensorDTypeByteSize(RTTENSOR_DTYPE dtype);

#ifdef __cplusplus
}
#endif

#endif //END _RTTENSOR_H_
