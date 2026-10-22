../xinfer.exe -model_name = "distilgpt2" -temperature = 0.7 -topk = 1 -time -max_seq_len = 30 -prompt = "Please tell me about yourself. I am" -cont -max_tensor_data_dum_num = 30 -dump-tensor_data -dump-rt_norm -dump-rt_vec -dump-rt -dump-rtctx /home/zhenyu/model/distilgpt2/vocab.json /home/zhenyu/model/distilgpt2/merges.txt /home/zhenyu/model/distilgpt2/tokenizer_config.json /home/zhenyu/model/distilgpt2/tokenizer.json /home/zhenyu/model/distilgpt2/config.json /home/zhenyu/model/distilgpt2/model.safetensors -dump test_distilgpt2.log 
../../../test/check_rule.pl -conf test_distilgpt2.conf -input test_distilgpt2.log

ret=$?

#Check exit code
if [[ $ret -ne 0 ]]; then
    echo "== CHECK FAILED! return code=$ret ==" >&2
    exit "${ret}"
fi

echo "TEST PASSED!!"
exit 0
