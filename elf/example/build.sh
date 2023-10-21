if [ $1 == "--coverage" ]
then
    OPEN_COVERAGE=true
else
    OPEN_COVERAGE=false
fi

#make -f Makefile.readelf FOR_ARM=true FOR_X86=true FOR_X64=true FOR_SCORE=true FOR_TECO=true DEBUG=false
make readelf.exe -f Makefile.readelf DEBUG=true FOR_ARM=true FOR_X86=true FOR_X64=true FOR_SCORE=true FOR_TECO=true
