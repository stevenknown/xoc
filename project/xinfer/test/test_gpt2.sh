../xinfer.exe -model_name = "gpt2" -dump-safetensors -dump-act -temperature = 0.7 -topk = 20 -time -max_seq_len = 30 -prompt = "Please tell me about yourself. I am" -cont -max_tensor_data_dum_num = 30 -dump-tensor_data -dump-rt_norm -dump-rt_vec -dump-rt -dump-rtctx ~/model/gpt2/vocab.json ~/model/gpt2/merges.txt ~/model/gpt2/tokenizer_config.json ~/model/gpt2/tokenizer.json ~/model/gpt2/config.json ~/model/gpt2/model.safetensors -dump test_gpt2.log
../../../test/check_rule.pl -conf test_gpt2.conf -input test_gpt2.log

ret=$?

#Check exit code
if [[ $ret -ne 0 ]]; then
    echo "== CHECK FAILED! return code=$ret ==" >&2
    exit "${ret}"
fi

echo "TEST PASSED!!"
exit 0
