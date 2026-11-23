#include "ops.h"

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
    int64_t startpos, int64_t endpos, RTCtx const* ctx)
{
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    XASSERT(endpos <= ctx->max_pos);

    //Each token occupies kvdim = n_kvhead * head_dim floats.
    //## K-cache start ########################################################
    //  |layer-0 start(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //
    //  |layer-1 start(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------

    //## V-cache start ########################################################
    //  |layer-0 start(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //
    //  |layer-1 start(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------

    unsigned int qheaddim = ctx->n_head * ctx->head_dim;
    unsigned int kvheaddim = ctx->n_kvhead * ctx->head_dim;

    //Write KV cache at position pos.
    //KV cache layout:[max_pos*n_embd]
    int64_t elem_num_per_layer_kv = ctx->max_pos * kvheaddim;
    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);
    BYTE * layer_kcache_start = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_vcache_start = vcache + layer * elem_num_per_layer_kv * elemsz;

    //Store k,v of layer to cache.
    int64_t q_sz = qheaddim * elemsz;
    int64_t kv_sz = kvheaddim * elemsz;
    int64_t k_off = q_sz;
    int64_t v_off = k_off + kv_sz;
    int64_t qkvsz = v_off + kv_sz;
    int64_t ind_of_seq_qkv = 0;
    for (int64_t i = startpos; i <= endpos; i++, ind_of_seq_qkv++) {
        int64_t pos_offset = i * kv_sz;
        BYTE const* tmp_qkv_of_pos = tmp_seq_qkv + ind_of_seq_qkv * qkvsz;
        memcpy(layer_kcache_start + pos_offset, tmp_qkv_of_pos + k_off, kv_sz);
        memcpy(layer_vcache_start + pos_offset, tmp_qkv_of_pos + v_off, kv_sz);
    }
    unlock_kcache(ctx);
    unlock_vcache(ctx);
}


//tmp_k:the token-major tmp-K for seqences after projection.
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
    int64_t startpos, int64_t endpos, RTCtx const* ctx)
{
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    XASSERT(endpos <= ctx->max_pos);

    //KV cache layout:[max_pos, n_kvhead*head_dim]=[max_pos*kvdim]
    //Each token occupies kvdim = n_kvhead * head_dim floats.
    //## K-cache start ########################################################
    //  |layer-0 start(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //
    //  |layer-1 start(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------

    //## V-cache start ########################################################
    //  |layer-0 start(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //
    //  |layer-1 start(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    unsigned int kvheaddim = ctx->n_kvhead * ctx->head_dim;

    //Write KV cache at position pos.
    //KV cache layout:[max_pos*n_embd]
    int64_t elem_num_per_layer_kv = ctx->max_pos * kvheaddim;
    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);
    BYTE * layer_kcache_start = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_vcache_start = vcache + layer * elem_num_per_layer_kv * elemsz;

    //Store k,v of layer to cache.
    int64_t seqlen = endpos - startpos + 1;
    int64_t kvsz = kvheaddim * elemsz;
    int64_t pos_offset = startpos * kvsz;
    memcpy(layer_kcache_start + pos_offset, tmp_k, seqlen * kvsz);
    memcpy(layer_vcache_start + pos_offset, tmp_v, seqlen * kvsz);
    unlock_kcache(ctx);
    unlock_vcache(ctx);
}


bool dumpKVCacheLayer(
    unsigned int layer, int64_t startpos, int64_t endpos, RTCtx const* ctx)
{
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    XASSERT(endpos <= ctx->max_pos);

    //KV cache layout:[max_pos, n_kvhead*head_dim]=[max_pos*kvdim]
    //Each token occupies kvdim = n_kvhead * head_dim floats.
    //## K-cache start ########################################################
    //  |layer-0 start(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //
    //  |layer-1 start(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  k_head0[0..127] ... k_head7[0..127]
    //  |  ------------------------------------

    //## V-cache start ########################################################
    //  |layer-0 start(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //
    //  |layer-1 start(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //  |that n_kvhead=8,head_dim=128
    //  |  ------------------------------------
    //  |  pos0
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  pos1
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    //  |  ...
    //  |  ------------------------------------
    //  |  pos new token
    //  |  v_head0[0..127] ... v_head7[0..127]
    //  |  ------------------------------------
    unsigned int kvheaddim = ctx->n_kvhead * ctx->head_dim;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);

    //Write KV cache at position pos.
    //KV cache layout:[max_pos*n_embd]
    int64_t elem_num_per_layer_kv = ctx->max_pos * kvheaddim;
    int64_t seqlen = endpos - startpos + 1;
    int64_t kvsz = kvheaddim * elemsz;
    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);
    BYTE * layer_kcache_start = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_vcache_start = vcache + layer * elem_num_per_layer_kv * elemsz;

    dump("KCACHE of layer%u:", layer);
    BYTE const* pos_kcache_start = layer_kcache_start + startpos * kvsz;
    for (int64_t pos = startpos; pos <= endpos;
         pos++, pos_kcache_start += kvsz) {
        dump_vec(pos_kcache_start, kvheaddim, dtype, NULL);
    }

    dump("VCACHE of layer%u:", layer);
    BYTE const* pos_vcache_start = layer_vcache_start + startpos * kvsz;
    for (int64_t pos = startpos; pos <= endpos;
         pos++, pos_vcache_start += kvsz) {
        dump_vec(pos_vcache_start, kvheaddim, dtype, NULL);
    }

    unlock_kcache(ctx);
    unlock_vcache(ctx);
    return true;
}
