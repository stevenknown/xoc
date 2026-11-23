#ifdef __cplusplus
extern "C" {
#endif

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
    unsigned int layer, int64_t startpos, int64_t endpos, unsigned int n_embd,
    unsigned int n_head, RTCtx * ctx);

#ifdef __cplusplus
}
#endif


