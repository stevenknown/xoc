../xinfer.exe -dump test_qwen.log -dump-rt -dump-rtctx -dump-rtarg -time -no-prefill_seq -model_name = "qwen2.5_1.5B" -temperature = 0.7 -topk = 20 -max_seq_len = 50 -prompt = "Please tell me about yourself. I am" -cont -max_tensor_data_dump_num = 0 ~/model/Qwen2.5-1.5B-Instruct/vocab.json ~/model/Qwen2.5-1.5B-Instruct/merges.txt ~/model/Qwen2.5-1.5B-Instruct/tokenizer_config.json ~/model/Qwen2.5-1.5B-Instruct/config.json ~/model/Qwen2.5-1.5B-Instruct/model.safetensors
../../../test/check_rule.pl -conf test_qwen2.5.conf -input test_qwen.log
ret=$?

#Check exit code
if [[ $ret -ne 0 ]]; then
    echo "== CHECK FAILED! return code=$ret ==" >&2
    exit "${ret}"
fi

################################################################################

../xinfer.exe -dump test_qwen.log -dump-rt -dump-rtctx -dump-rtarg -time -prefill_seq -model_name = "qwen2.5_1.5B" -temperature = 0.7 -topk = 20 -max_seq_len = 50 -prompt = "Please tell me about yourself. I am" -cont -max_tensor_data_dump_num = 0 ~/model/Qwen2.5-1.5B-Instruct/vocab.json ~/model/Qwen2.5-1.5B-Instruct/merges.txt ~/model/Qwen2.5-1.5B-Instruct/tokenizer_config.json ~/model/Qwen2.5-1.5B-Instruct/config.json ~/model/Qwen2.5-1.5B-Instruct/model.safetensors
../../../test/check_rule.pl -conf test_qwen2.5.conf -input test_qwen.log
ret=$?

#Check exit code
if [[ $ret -ne 0 ]]; then
    echo "== CHECK FAILED! return code=$ret ==" >&2
    exit "${ret}"
fi

################################################################################

echo "TEST PASSED!!"
exit 0
