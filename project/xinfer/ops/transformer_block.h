#ifndef _TRANSFORMER_BLOCK_H_
#define _TRANSFORMER_BLOCK_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef struct tagRTCtx RTCtx;

typedef enum tagPOS_ENCODING_TYPE {
    POS_UNDEF = 0,
    POS_WPE,
    POS_ROPE,
    POS_SINUSOID,
} POS_ENCODING_TYPE;

//This function executes all transformer block layers.
//hidden:Input and output buffer: input is hidden state after embedding;
//       output is hidden state processed by all transformer layers
//tmp1:Temporary scratch buffer
//tmp2:Temporary scratch buffer
//pos:Current token position, used by MHA for reading and writing KVcache
//n_layer:Number of transformer layers
//n_embd:Embedding dimension size
//n_head:Number of attention heads
//arg RTArg runtime context, contains kcache, vcache and safetensor manager etc.
void transformer_blocks(int64_t pos, RTCtx * ctx);
void transformer_blocks_seq(int64_t startpos, int64_t endpos, RTCtx * ctx);

#ifdef __cplusplus
}
#endif

#endif
