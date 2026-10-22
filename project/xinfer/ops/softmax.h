#ifdef __cplusplus
extern "C" {
#endif

//Softmax, inplace transform logits -> probability
//logits: input and output. logits array, record the probability.
void softmax_inplace_f32(float * logits, int64_t elemnum, float eps);
void softmax_inplace_f16(uint16_t * logits, int64_t elemnum, float eps);
void softmax_inplace_bf16(uint16_t * logits, int64_t elemnum, float eps);

#ifdef __cplusplus
}
#endif
