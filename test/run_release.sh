perl run_compile_on_linux.pl release

#Get the return-result.
ret=$?

if [ $ret -eq 0 ]; then
    echo "perl run_compile_on_linux.pl release SUCCESS!!"
    exit 0
else
    echo "perl run_compile_on_linux.pl release FAILED!!"
    exit $ret
fi
exit 0
