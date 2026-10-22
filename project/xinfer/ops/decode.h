#ifdef __cplusplus
extern "C" {
#endif

void forward_token(int64_t token_id, int64_t pos, RTCtx * ctx);
int64_t decode(int64_t token_id, int64_t pos, RTCtx * ctx);

#ifdef __cplusplus
}
#endif
