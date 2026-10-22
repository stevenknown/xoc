#ifdef __cplusplus
extern "C" {
#endif

//Topk filter, inplace modify probability array
//probs:softmax output probability array
//vocab_size:vocabulary size
//k:keep topk highest prob tokens, others set zero
void topkF32(float * probs, int64_t vocab_size, int64_t k, float eps);
void topkF16(uint16_t * probs, int64_t vocab_size, int64_t k, float eps);
void topkBF16(uint16_t * probs, int64_t vocab_size, int64_t k, float eps);

#ifdef __cplusplus
}
#endif
