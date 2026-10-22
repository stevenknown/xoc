#!/bin/bash
gcc test_matmul.c matmul.c fpcvt.c trans.c -o a.out -D_DEBUG_
./a.out
if [ $? -eq 0 ]; then
    echo "success, return code = 0"
else
    echo "failed, return code = $?"
    exit 1
fi

gcc test_layernorm.c layernorm.c  fpcvt.c -o a.out -D_DEBUG_ -lm
./a.out
if [ $? -eq 0 ]; then
    echo "success, return code = 0"
else
    echo "failed, return code = $?"
    exit 1
fi
