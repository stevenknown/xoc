#!/bin/bash
SRC="../ops"
set -x

##
gcc "test_matmul.c" \
    "${SRC}/matmul.c" \
    "${SRC}/fpcvt.c" \
    "${SRC}/trans.c" \
    "${SRC}/dump.c" \
    "${SRC}/rttensor.c" \
    -O0 -g2 \
    -o a.out -I../ops -lm -D_DEBUG_
./a.out
ret=$?
if [ $ret -eq 0 ]; then
    echo "SUCCESS, RETURN CODE = 0"
else
    echo "FAILED, RETURN CODE = $ret"
    exit 1
fi


echo "TEST PASSED!!"
exit 0
