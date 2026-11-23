#ifdef __cplusplus
extern "C" {
#endif

//Perform embedding lookup, fetch one token embedding row and
//convert to output buffer.
//emb_weight:Input embedding tensor, must be 2D layout [vocab_size, hidden_dim].
//token_id:Target token index to look up.
//out:Output buffer, buffer length must equal hidden_dim.
//Supported input dtype: RTTENSOR_DTYPE_F32, RTTENSOR_DTYPE_F16,
//RTTENSOR_DTYPE_BF16
void embedding_lookup(
    void * out, RTTensor const* emb_weight, int64_t token_id);

//Perform embedding lookup, fetch the tokens sequence embedding row and
//convert to output buffer.
//emb_weight:Input embedding tensor, must be 2D layout [vocab_size, hidden_dim].
//tokarr:a list of target token index to look up.
//out:Output buffer, buffer length must equal hidden_dim * seqlen.
//Supported input dtype: all kinds of dtype.
void embedding_lookup_seq(
    void * out, RTTensor const* emb_weight, TokenArray const* tokarr,
    int64_t seqlen);

#ifdef __cplusplus
}
#endif
