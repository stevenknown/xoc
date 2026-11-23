#ifdef __cplusplus
extern "C" {
#endif

void forward_token(int64_t token_id, int64_t pos, RTCtx * ctx);

void forward_token_seq(
    TokenArray const* tokarr, int64_t seqlen, RTCtx * ctx);

#ifdef __cplusplus
}
#endif
