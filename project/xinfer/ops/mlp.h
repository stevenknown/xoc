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
//out:Output buffer, shape [n_embd], receives final MLP result.
//in:Input feature vector, shape [n_embd], normalized input from
//   ln_2, readonly.
//c_fc_w:First fullyconnected weight tensor, shape [n_embd, 4*n_embd].
//c_fc_b:First fullyconnected bias tensor, shape [4*n_embd].
//c_proj_w:Output projection weight tensor, shape [4*n_embd, n_embd].
//c_proj_b:Output projection bias tensor, shape [n_embd].
//ctx:Runtime context RTCtx*, provides scratch temporary buffer for fc
//    intermediate result.
void mlp(void * out, void const* in,
         RTTensor const* c_fc_w, RTTensor const* c_fc_b,
         RTTensor const* c_proj_w, RTTensor const* c_proj_b,
         RTCtx * ctx);

//out = silu(in*gate_proj_w) * (in*up_proj_w)*down_proj_w
//out:output=[1*n_embd]
//in:input hidden=[1*n_embd]
//gate_proj_w:weight gate=[n_embd*inner_dim]
//up_proj_w:weight up=[n_embd*inner_dim]
//down_proj_w:weight down=[inner_dim*n_embd]
//ctx:provide two scratch buffer of size inner_dim=4*n_embd
void mlp_swigu(
    void * out, void const* in,
    RTTensor const* gate_proj_w, RTTensor const* up_proj_w,
    RTTensor const* down_proj_w, RTCtx * ctx);

#ifdef __cplusplus
}
#endif
