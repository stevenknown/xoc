IS_DEBUG=true
OPEN_COVERAGE=false
while [ "$#" -gt 0 ]
do
    if [ $1 == "--release" ]
    then
        IS_DEBUG=false
    elif [ $1 == "--coverage" ]
    then
        OPEN_COVERAGE=true
    fi
    shift
done

make readelf.exe -f Makefile.readelf DEBUG=$IS_DEBUG FOR_ARM=false FOR_X86=false \
  FOR_X64=false FOR_SCORE=false FOR_DEX=false FOR_JS=false FOR_TECO=true \
  TARG=FOR_TECO TARG_DIR=../../teco OPEN_COVERAGE=$OPEN_COVERAGE
