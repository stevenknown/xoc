./test_distilgpt2.sh
ret=$?
if [[ $ret -ne 0 ]]; then
    echo "== CHECK FAILED! return code=$ret ==" >&2
    exit "${ret}"
fi

./test_gpt2.sh
ret=$?
if [[ $ret -ne 0 ]]; then
    echo "== CHECK FAILED! return code=$ret ==" >&2
    exit "${ret}"
fi


echo "TEST PASSED!!"
exit 0
