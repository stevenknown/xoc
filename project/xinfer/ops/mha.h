#ifdef __cplusplus
extern "C" {
#endif

//Performs multi-head self-attention for one incoming token in decode
//generation mode.
//This function executes QKV projection, writes current-step K/V into KVCache,
//performs scaled dot attention against all cached historical keys and values,
//applies causal mask, softmax, output projection, and produces attention
//output vector.
//
//KVCache stores keys and values of all previous tokens; new K/V of current pos
//will be appended into cache inside this function.
//Depends on scratch buffers inside RTCtx; do NOT allocate/free heap memory
//inside this function.
//Does NOT perform layernorm or residual add; those are handled by
//upper-level block logic.
//out:Output buffer, shape [n_embd]. Receives final attention result.
//in:Input feature vector, shape [n_embd]. Must be normalized hidden
//   state after ln_1.
//c_attn_w:QKV concatenated projection weight tensor, shape [n_embd, 3*n_embd].
//c_attn_b:QKV concatenated projection bias tensor, shape [3*n_embd].
//c_proj_w:Output projection weight tensor, shape [n_embd, n_embd].
//c_proj_b:Output projection bias tensor, shape [n_embd].
//layer:Current transformer layer index, zerobased, used to index
//      corresponding section in KVcache.
//pos:Sequence position index of current token, used to write new K/V entry
//    into KVcache.
//n_embd:Embedding dimension size of model.
//n_head:Number of attention heads. head_dim = n_embd / n_head.
//ctx:Runtime context RTCtx*, provides temporary scratch buffers and
//    KVcache storage.
//If it is successful, valid result written to 'out', KVcache updated.
void mha(void * out, void const* in,
         RTTensor* c_attn_w, RTTensor* c_attn_b,
         RTTensor* c_proj_w, RTTensor* c_proj_b,
         unsigned int layer, int64_t pos, unsigned int n_embd,
         unsigned int n_head, RTCtx* ctx);

//MHA with RoPE and GQA.
//out:[n_embd] attention output AFTER o_proj(ready for residual add).
//h:[n_embd] normalized input hidden state.
//ctx:context, kcache|vcache|scratch buffers.
//pos:current token position, 0-based. KV cache written at 'pos'.
//layer: transformer layer index (which weights to use).
void mha_rope(
    void * out, void const* in,
    RTTensor * q_proj_w, RTTensor * q_proj_b,
    RTTensor * k_proj_w, RTTensor * k_proj_b,
    RTTensor * v_proj_w, RTTensor * v_proj_b,
    RTTensor * o_proj_w,
    unsigned int layer, int64_t pos, unsigned int n_embd,
    unsigned int n_head, RTCtx * ctx);

#ifdef __cplusplus
}
#endif


