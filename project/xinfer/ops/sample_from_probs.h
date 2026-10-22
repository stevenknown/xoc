#ifdef __cplusplus
extern "C" {
#endif

//Roulette0-wheel sample from probability array.
//probs:probability array, sum is approximate to 1.0.
//      or zero for filtered-out tokens.
//vocab_size:length of probs array.
//eps:epislon
//Return sampled token id; return INVALID_TOKEN on error.
int64_t sample_from_probs_f32(
    float const* probs, int64_t vocab_size, float eps);
int64_t sample_from_probs_f16(
    uint16_t const* probs, int64_t vocab_size, float eps);
int64_t sample_from_probs_bf16(
    uint16_t const* probs, int64_t vocab_size, float eps);

#ifdef __cplusplus
}
#endif
