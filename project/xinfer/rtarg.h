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
#ifndef _RTARG_H_
#define _RTARG_H_

//256KB
#define MAX_VOCAB_SIZE (256*1024)

extern "C" typedef struct tagRTCtx RTCtx;

namespace xinfer {

class RTArg {
    COPY_CONSTRUCTOR(RTArg);
public:
    TOKID newline_token_id;
    RTTENSOR_DTYPE tensor_dtype;
    UINT n_embd;
    UINT n_layer;
    UINT n_head;
    UINT n_kvhead;
    UINT n_positions;
    UINT intermediate_size;
    UINT64 current_pos;
    UINT64 eos_token_id;
    UINT64 vocab_size;
    double rope_theta;
    double layernorm_epsilon;
    double rmsnorm_epsilon;
    xoc::BYTEVec hidden;

    //The tmp buffer of attention,mlp,layernorm.
    xoc::BYTEVec tmp1;
    xoc::BYTEVec tmp2;
    xoc::BYTEVec tmp3;

    //The tmp buffer of qkv and mlp:fc.
    xoc::BYTEVec tmp_qkv_and_fc;
    xoc::BYTEVec kcache;
    xoc::BYTEVec vcache;
    xoc::BYTEVec logits;
    TokenVec gened_tokens;
public:
    RTArg();
    void addGenedToken(TokenArray const* tokarr);
    void addGenedToken(TOKID tokid);

    void clean()
    {
        newline_token_id = 0;
        n_embd = 0;
        n_layer = 0;
        n_head = 0;
        n_kvhead = 0;
        n_positions = 0;
        intermediate_size = 0;
        current_pos = 0;
        eos_token_id = 0;
        vocab_size = 0;
        rope_theta = 0;
        layernorm_epsilon = 0;
        rmsnorm_epsilon = 0;

        hidden.clean();
        tmp1.clean();
        tmp2.clean();
        tmp3.clean();
        tmp_qkv_and_fc.clean();
        kcache.clean();
        vcache.clean();
        logits.clean();
        gened_tokens.clean();
    }

    void dump(LLMInfer & infer, xoc::LogMgr * lm) const;
    void dumpGenedTokens(LLMInfer & infer, xoc::LogMgr * lm) const;

    UINT getMaxSeqLen() const { return n_positions; }

    BYTE * getHiddenBuf() { return hidden.get_vec(); }
    UINT getHiddenBufLen() { return hidden.get_capacity(); }

    BYTE * getTmp1Buf() { return tmp1.get_vec(); }
    UINT getTmp1BufLen() { return tmp1.get_capacity(); }

    BYTE * getTmp2Buf() { return tmp2.get_vec(); }
    UINT getTmp2BufLen() { return tmp2.get_capacity(); }

    BYTE * getTmp3Buf() { return tmp3.get_vec(); }
    UINT getTmp3BufLen() { return tmp3.get_capacity(); }

    BYTE * getTmpFCBuf() { return tmp_qkv_and_fc.get_vec(); }
    UINT getTmpFCBufLen() { return tmp_qkv_and_fc.get_capacity(); }

    BYTE * getTmpAttnOutBuf()
    {
        ASSERT0(tensor_dtype != RTTENSOR_DTYPE_UNDEF);
        UINT elemsz = getRTTensorDTypeByteSize(tensor_dtype);
        return getTmpQKVBuf() + n_embd * elemsz;
    }
    UINT getTmpAttnOutBufLen()
    {
        ASSERT0(tensor_dtype != RTTENSOR_DTYPE_UNDEF);
        UINT elemsz = getRTTensorDTypeByteSize(tensor_dtype);
        return n_embd * elemsz;
    }

    BYTE * getTmpQKVBuf() { return tmp_qkv_and_fc.get_vec(); }
    UINT getTmpQKVBufLen()
    {
        ASSERT0(tensor_dtype != RTTENSOR_DTYPE_UNDEF);
        UINT elemsz = getRTTensorDTypeByteSize(tensor_dtype);
        //Maximum size of Q+K+V is three times the hidden-size.
        return 3 * elemsz * n_embd;
    }

    BYTE * getKCacheBuf() { return kcache.get_vec(); }
    UINT getKCacheBufLen() { return kcache.get_capacity(); }

    BYTE * getVCacheBuf() { return vcache.get_vec(); }
    UINT getVCacheBufLen() { return vcache.get_capacity(); }

    BYTE * getLogitsBuf() { return logits.get_vec(); }
    UINT getLogitsBufLen() { return logits.get_capacity(); }
};

} //namespace xinfer

#endif
