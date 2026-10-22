#ifdef __cplusplus
extern "C" {
#endif

//Prompt prefill:process all input prompt tokens and build KV-Cache
//Prefill Phase Responsibilities:
//1. Iterate over every token in the prompt and run transformer forward
//   pass sequentially.
//2. For each token, compute key / value tensors and write them into
//   KV-Cache buffers.
//3. After all prompt tokens are processed, KV-Cache holds keys and values
//   for the full prompt history.
//4. Return the sequence position after the last prompt token. This position
//   becomes the starting point
//   for the subsequent decode autoregressive loop.
//5. Once prefill finishes, no further prompt processing is needed. The
//   decode loop generates one
//   new token at a time and reuses the pre-built KV-Cache.
//
//tokarr Encoded prompt token array storing all prompt token ids
//       record all tokens of promot.
//promot Raw prompt string, used only for debug printing, not part of
//       inference computation
//first_next_token Reserved parameter, represents the first token fed into
//            decode after prefill completes
//ctx Inference context RTCtx, holds wte/wpe embedding tables, layer weights,
//    kcache/vcache, and scratch temporary buffers
//Return Valid position (>=0) on success; returns -1 on error.

//Returned pos semantics: if prompt contains N tokens, final pos equals N-1.
//In decode loop: feed first_token, start from this returned pos,
//increment pos by one for each generated token.

//ctx->kcache and ctx->vcache must be allocated in advance during initRTArg.
//kcache/vcache memory must be fully allocated before entering prefill.
//Prefill does not perform any malloc.
//All intermediate computations reuse scratch buffers from
//RTCtx(tmp1, tmp2, tmp_qkv etc).
//Total prompt token count must not exceed ctx->max_pos.

//Position embedding wpe:
//wpe is position embedding table with shape [max_pos, n_embd].
//Position vector for sequence position pos is fetched from wpe[pos].
//Block input hidden state = wte[token_id] + wpe[pos].

//TODO:batched prefill. For performance-critical scenarios, implement
//scaled_dot_attn_prefill.
int64_t prefill(
    int64_t * first_next_token, TokenArray const* tokarr, RTCtx * ctx);

#ifdef __cplusplus
}
#endif
