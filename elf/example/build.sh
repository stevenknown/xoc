if [ $1 == "--coverage" ]
then
    OPEN_COVERAGE=true
else
    OPEN_COVERAGE=false
fi

make readelf.exe -f Makefile.readelf DEBUG=true FOR_ARM=false FOR_X86=false \
  FOR_X64=false FOR_SCORE=false FOR_DEX=false FOR_JS=false FOR_TECO=false \
  TARG=FOR_DEX OPEN_COVERAGE=$OPEN_COVERAGE
