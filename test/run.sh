perl run_compile_on_linux.pl debug

#Get the return-result.
ret=$?

if [ $ret -eq 0 ]; then
    echo "perl run_compile_on_linux.pl debug SUCCESS!!"
    exit 0
else
    echo "perl run_compile_on_linux.pl debug FAILED!!"
    exit $ret
fi
exit 0
