#ifndef _KVCACHE_H_
#define _KVCACHE_H_

#ifdef __cplusplus
extern "C" {
#endif

//tmp_seq_qkv:the token-major tmp-QKV for seqences after projection.
//  Assum n_embd=768, then tmp_seq_qkv is:
//  pos0: | Q[768] | K[768] | V[768] |
//  pos1: | Q[768] | K[768] | V[768] |
//  pos2: | Q[768] | K[768] | V[768] |
//  pos3: | Q[768] | K[768] | V[768] |
//layer:Current transformer layer index, zerobased, used to index
//      corresponding section in KVcache.
void storeKVCacheFromQKV(
    BYTE const* tmp_seq_qkv, unsigned int layer, unsigned int elemsz,
    int64_t startpos, int64_t endpos, RTCtx const* ctx);

//tmp_k:the token-major tmp-K for seqences after projection.
//tmp_v:the token-major tmp-V for seqences after projection.
//  Assum n_embd=768, then tmp_k is:
//  pos0: | K[768] |
//  pos1: | K[768] |
//  pos2: | K[768] |
//  pos3: | K[768] |
//layer:Current transformer layer index, zerobased, used to index
//      corresponding section in K-cache.
void storeKCacheAndVCache(
    BYTE const* tmp_k, BYTE const* tmp_v,
    unsigned int layer, unsigned int elemsz,
    int64_t startpos, int64_t endpos, RTCtx const* ctx);

bool dumpKVCacheLayer(
    unsigned int layer, int64_t startpos, int64_t endpos, RTCtx const* ctx);

#ifdef __cplusplus
}
#endif

#endif
