#ifdef __cplusplus
extern "C" {
#endif

void * lock_hidden(RTCtx * ctx);
void unlock_hidden(RTCtx * ctx);

void * lock_logits(RTCtx * ctx);
void unlock_logits(RTCtx * ctx);

void * lock_tmp1(RTCtx * ctx);
void unlock_tmp1(RTCtx * ctx);

void * lock_tmp2(RTCtx * ctx);
void unlock_tmp2(RTCtx * ctx);

void * lock_tmp3(RTCtx * ctx);
void unlock_tmp3(RTCtx * ctx);

void * lock_tmp_qkv(RTCtx * ctx);
void unlock_tmp_qkv(RTCtx * ctx);

void * lock_tmp_fc(RTCtx * ctx);
void unlock_tmp_fc(RTCtx * ctx);

void * lock_tmp_attn_out(RTCtx * ctx);
void unlock_tmp_attn_out(RTCtx * ctx);

void * lock_kcache(RTCtx * ctx);
void unlock_kcache(RTCtx * ctx);

void * lock_vcache(RTCtx * ctx);
void unlock_vcache(RTCtx * ctx);

bool all_locked(RTCtx const* ctx);
bool all_unlocked(RTCtx const* ctx);

#ifdef __cplusplus
}
#endif
