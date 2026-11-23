#ifndef _MLP_SEQ_H_
#define _MLP_SEQ_H_

#ifdef __cplusplus
extern "C" {
#endif

//Forward MLP: fullyconnected -> GELU activation -> projection
//Standard MLP flow:
//1.c_fc projection: n_embd -> 4 * n_embd (hidden inner dimension)
//2.GELU nonlinear activation (GPT approximate GELU)
//3.c_proj projection: 4 * n_embd->n_embd, produce final MLP output.
//
//Input should be normalized hidden state after ln_2.
//Does NOT handle layernorm or residual addition; residual logic belongs to
//upper transformer block.
//Uses scratch buffer from RTCtx.
//
//out:Output buffer, shape [seqlen*n_embd], receives final MLP result.
//in:Input feature vector, shape [seqlen*n_embd], normalized input from
//   ln_2, readonly.
//c_fc_w:First fullyconnected weight tensor, shape [n_embd, 4*n_embd].
//c_fc_b:First fullyconnected bias tensor, shape [4*n_embd].
//c_proj_w:Output projection weight tensor, shape [4*n_embd, n_embd].
//c_proj_b:Output projection bias tensor, shape [n_embd].
//ctx:Runtime context RTCtx*, provides scratch temporary buffer for fc
//    intermediate result.
void mlp_seq(
    void * out, void const* in,
    RTTensor const* c_fc_w, RTTensor const* c_fc_b,
    RTTensor const* c_proj_w, RTTensor const* c_proj_b,
    int64_t seqlen, RTCtx * ctx);

#ifdef __cplusplus
}
#endif

#endif
