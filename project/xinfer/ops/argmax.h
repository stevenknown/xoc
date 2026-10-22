#ifdef __cplusplus
extern "C" {
#endif

//Return the index of the maximum probability in 'probs'.
int64_t argmaxF32(float const* probs, int64_t probslen);
int64_t argmaxF16(uint16_t const* probs, int64_t probslen);
int64_t argmaxBF16(uint16_t const* probs, int64_t probslen);

#ifdef __cplusplus
}
#endif
