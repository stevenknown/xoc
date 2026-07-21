#!/bin/bash
./run.sh

#Get the return-result.
ret=$?

if [ $ret -ne 0 ]; then
    echo "run.sh FAILED!!"
    exit $ret
fi

./run_exec.sh

#Get the return-result.
ret=$?

if [ $ret -ne 0 ]; then
    echo "run_exec.sh FAILED!!"
    exit $ret
fi

exit 0
