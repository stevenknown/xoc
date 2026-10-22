#include "ops.h"

void * lock_hidden(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_hidden);
    XASSERT(dump("lock hidden"));
    ctx->lock_hidden = true;
    #endif
    return ctx->hidden;
}


void unlock_hidden(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_hidden = false;
    XASSERT(dump("unlock hidden"));
    #endif
}


void * lock_logits(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_logits);
    XASSERT(dump("lock logits"));
    ctx->lock_logits = true;
    #endif
    return ctx->logits;
}


void unlock_logits(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_logits = false;
    XASSERT(dump("unlock logits"));
    #endif
}


void * lock_tmp1(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_tmp1);
    ctx->lock_tmp1 = true;
    XASSERT(dump("lock tmp1"));
    #endif
    return ctx->tmp1;
}


void unlock_tmp1(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_tmp1 = false;
    XASSERT(dump("unlock tmp1"));
    #endif
}


void * lock_tmp2(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_tmp2);
    ctx->lock_tmp2 = true;
    XASSERT(dump("lock tmp2"));
    #endif
    return ctx->tmp2;
}


void unlock_tmp2(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_tmp2 = false;
    XASSERT(dump("unlock tmp2"));
    #endif
}


void * lock_tmp3(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_tmp3);
    ctx->lock_tmp3 = true;
    XASSERT(dump("lock tmp3"));
    #endif
    return ctx->tmp3;
}


void unlock_tmp3(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_tmp3 = false;
    XASSERT(dump("unlock tmp3"));
    #endif
}


void * lock_tmp_qkv(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_tmp_fc);
    XASSERT(!ctx->lock_tmp_qkv);

    //tmp_fc overlaps with tmp_qkv.
    ctx->lock_tmp_fc = true;
    ctx->lock_tmp_qkv = true;
    XASSERT(dump("lock tmp_qkv"));
    #endif
    return ctx->tmp_qkv;
}


void unlock_tmp_qkv(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_tmp_fc = false;
    ctx->lock_tmp_qkv = false;
    XASSERT(dump("unlock tmp_qkv"));
    #endif
}


void * lock_tmp_fc(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_tmp_fc);
    XASSERT(!ctx->lock_tmp_qkv);

    //tmp_fc overlaps with tmp_qkv.
    ctx->lock_tmp_fc = true;
    ctx->lock_tmp_qkv = true;
    XASSERT(dump("lock tmp_fc"));
    #endif
    return ctx->tmp_fc;
}


void unlock_tmp_fc(RTCtx * ctx)
{
    #ifdef _DEBUG_
    //tmp_fc overlaps with tmp_qkv.
    ctx->lock_tmp_fc = false;
    ctx->lock_tmp_qkv = false;
    XASSERT(dump("unlock tmp_fc"));
    #endif
}


void * lock_tmp_attn_out(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_tmp_attn_out);
    ctx->lock_tmp_attn_out = true;
    XASSERT(dump("lock tmp_attn_out"));
    #endif
    return ctx->tmp_attn_out;
}


void unlock_tmp_attn_out(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_tmp_attn_out = false;
    XASSERT(dump("unlock tmp_attn_out"));
    #endif
}


void * lock_kcache(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_kcache);
    ctx->lock_kcache = true;
    XASSERT(dump("lock kcache"));
    #endif
    return ctx->kcache;
}


void unlock_kcache(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_kcache = false;
    XASSERT(dump("unlock kcache"));
    #endif
}


void * lock_vcache(RTCtx * ctx)
{
    #ifdef _DEBUG_
    XASSERT(!ctx->lock_vcache);
    ctx->lock_vcache = true;
    XASSERT(dump("lock vcache"));
    #endif
    return ctx->vcache;
}


void unlock_vcache(RTCtx * ctx)
{
    #ifdef _DEBUG_
    ctx->lock_vcache = false;
    XASSERT(dump("unlock vcache"));
    #endif
}


bool all_locked(RTCtx const* ctx)
{
    #ifdef _DEBUG_
    if (!ctx->lock_tmp1) { return false; }
    if (!ctx->lock_tmp2) { return false; }
    if (!ctx->lock_tmp_qkv) { return false; }
    if (!ctx->lock_tmp_fc) { return false; }
    if (!ctx->lock_tmp_attn_out) { return false; }
    if (!ctx->lock_hidden) { return false; }
    if (!ctx->lock_logits) { return false; }
    if (!ctx->lock_kcache) { return false; }
    if (!ctx->lock_vcache) { return false; }
    #endif
    return true;
}


bool all_unlocked(RTCtx const* ctx)
{
    #ifdef _DEBUG_
    if (ctx->lock_tmp1) { return false; }
    if (ctx->lock_tmp2) { return false; }
    if (ctx->lock_tmp_qkv) { return false; }
    if (ctx->lock_tmp_fc) { return false; }
    if (ctx->lock_tmp_attn_out) { return false; }
    if (ctx->lock_hidden) { return false; }
    if (ctx->lock_logits) { return false; }
    if (ctx->lock_kcache) { return false; }
    if (ctx->lock_vcache) { return false; }
    #endif
    return true;
}

