#include "ops.h"

void transformer_blocks_seq(int64_t startpos, int64_t endpos, RTCtx * ctx)
{
    switch (ctx->pos_encoding_type) {
    case POS_WPE:
        transformer_blocks_gpt2_seq(startpos, endpos, ctx);
        return;
    case POS_ROPE:
        transformer_blocks_rope_seq(startpos, endpos, ctx);
        return;
    case POS_SINUSOID:
    default:
        XASSERT(0); //TODO:
    }
}
