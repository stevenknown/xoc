#ifndef _RTCTX_H_
#define _RTCTX_H_

#ifdef __cplusplus
extern "C" {
#endif

extern float const g_float_neg_infinity;

typedef struct tagRTCtx {
    RTTENSOR_DTYPE tensor_dtype;
    unsigned int n_layer;
    unsigned int n_embd;
    unsigned int n_head;
    unsigned int n_kvhead;
    unsigned int n_topk;
    unsigned int intermediate_size;
    int64_t vocab_size;
    int64_t max_pos; //max context window
    float rope_theta;
    float layernorm_epsilon;
    float rmsnorm_epsilon;
    float softmax_epsilon;

    //0:greedy, 1:original distribute, >1:more random distribute.
    float temperature;

    void * tmp1;
    uint64_t tmp1_len; //byte length

    void * tmp2;
    uint64_t tmp2_len; //byte length

    void * tmp3;
    uint64_t tmp3_len; //byte length

    //size is 3*n_embd, it is used in concatenating qkv.
    void * tmp_qkv;
    uint64_t tmp_qkv_len; //byte length

    //size is 4*n_embd, it is used in mlp.
    void * tmp_fc;
    uint64_t tmp_fc_len; //byte length

    //size is n_embd.
    void * tmp_attn_out;
    uint64_t tmp_attn_out_len; //byte length

    void * hidden; //hidden state
    uint64_t hidden_len; //byte length
    
    void * logits;
    uint64_t logits_len; //byte length

    void * kcache;
    uint64_t kcache_len; //byte length

    void * vcache;
    uint64_t vcache_len; //byte length

    RTTensor * wte;
    RTTensor * wpe;
    RTTensor * ln_f_w;
    RTTensor * ln_f_b;
    RTTensor ** ln_1_weight;
    RTTensor ** ln_1_bias;
    RTTensor ** ln_2_weight;
    RTTensor ** ln_2_bias;
    RTTensor ** c_attn_weight; //concatenated attention
    RTTensor ** c_attn_bias;
    RTTensor ** o_proj_weight;
    RTTensor ** o_proj_bias;
    RTTensor ** mlp_c_fc_weight;
    RTTensor ** mlp_c_fc_bias;
    RTTensor ** mlp_down_proj_weight;
    RTTensor ** mlp_down_proj_bias;

    //QWEN specific
    RTTensor ** q_proj_weight;
    RTTensor ** q_proj_bias;
    RTTensor ** k_proj_weight;
    RTTensor ** k_proj_bias;
    RTTensor ** v_proj_weight;
    RTTensor ** v_proj_bias;
    RTTensor ** mlp_gate_proj_weight;
    RTTensor ** mlp_up_proj_weight;

    #ifdef _DEBUG_
    bool dump_l2norm; //dump l2normalization.
    bool dump_vec; //dump vector.
    bool dump; //dump normal informations.
    bool lock_tmp1;
    bool lock_tmp2;
    bool lock_tmp3;
    bool lock_tmp_qkv;
    bool lock_tmp_fc;
    bool lock_tmp_attn_out;
    bool lock_hidden;
    bool lock_logits;
    bool lock_kcache;
    bool lock_vcache;
    #endif
    #ifdef _DUMP_BY_LOGMGR_
    void * lm;
    #endif
} RTCtx;

#ifdef __cplusplus
}
#endif

#endif
