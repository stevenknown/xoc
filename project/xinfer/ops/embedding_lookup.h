#ifdef __cplusplus
extern "C" {
#endif

//Perform embedding lookup, fetch one token embedding row and
//convert to float32 output buffer.
//emb_weight:Input embedding tensor, must be 2D layout [vocab_size, hidden_dim].
//token_id:Target token index to look up.
//out:Output float32 buffer, buffer length must equal hidden_dim.
//Supported input dtype: RTTENSOR_DTYPE_F32, RTTENSOR_DTYPE_F16,
//RTTENSOR_DTYPE_BF16
//Output data type is always float32.
void embedding_lookup(
    void * out, RTTensor const* emb_weight, int64_t token_id);

#ifdef __cplusplus
}
#endif
