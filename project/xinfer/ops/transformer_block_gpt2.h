#ifdef __cplusplus
extern "C" {
#endif

//This function implements the core computation body of the GPT model.
//It takes the embedding-fused feature vector as input, iteratively runs
//every transformer block, and finally applies
//the final layernorm.
//It updates hidden in-place to become the final output hiddenstate for the
//current token.
//Execute all transformer block layers and the final layernorm.
//hidden:Input and output buffer: input is hidden state after embedding;
//       output is hidden state processed by all transformer layers
//tmp1:Temporary scratch buffer
//tmp2:Temporary scratch buffer
//pos:Current token position, used by MHA for reading and writing KVcache
//n_layer:Number of transformer layers
//n_embd:Embedding dimension size
//n_head:Number of attention heads
//arg RTArg runtime context, contains kcache, vcache and safetensor manager etc.
void transformer_blocks_gpt2(int64_t pos, RTCtx * ctx);

#ifdef __cplusplus
}
#endif
