#ifdef __cplusplus
extern "C" {
#endif

//Sample next token from last hidden state.
//logits:output logits.
//hidden:[1*n_embd], transformer output hidden vector
//wte:embedding weight tensor, shared lm_head
//arg:runtime argument, contains n_embd, vocab_size
//temperature:1.0 normal; <1.0 more deterministic; >1.0 more random
//n_topk:topk sampling, e.g:40
//Return sampled tokenid, INVALID_TOKEN for error.
int64_t compute_logits_and_sample(
    void * logits, void const* hidden, RTTensor const* wte,
    int64_t n_embd, int64_t vocab_size, float temperature, unsigned int n_topk,
    float eps, RTTENSOR_DTYPE dtype);
#ifdef __cplusplus
}
#endif
