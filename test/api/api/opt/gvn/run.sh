#!/bin/bash
CHECK_RULE=../../../../check_rule.pl
bash ./clean.sh
bash ./build.sh
./test.exe &> test.log

#Get the return-result.
ret=$?
if [ $ret -ne 0 ]; then
    echo "EXEC:test.exe FAILED!!"
    exit $ret
fi
if [ ! -f "test.conf" ]; then
    echo "NOTE: test.conf NOT FOUND."
    exit 0
fi

perl $CHECK_RULE -conf test.conf -input test.log

#Get the return-result.
ret=$?

if [ $ret -eq 0 ]; then
    echo "perl $CHECK_RULE debug SUCCESS!!"
    exit 0
else
    echo "perl $CHECK_RULE debug FAILED!!"
    exit $ret
fi
exit 0
~
~
~

