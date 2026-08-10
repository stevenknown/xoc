#!/bin/bash

if [[ -z "${XOC_HOME}" ]]; then
    echo "ERROR: XOC_HOME ENVIRONMENT VARIABLE IS NOT SET!"
    exit 1
fi

if [[ ! -d "${XOC_HOME}" ]]; then
    echo "ERROR: XOC_HOME PATH [${XOC_HOME}] DOES NOT EXIST!"
    exit 2
fi

xocc_path="$XOC_HOME/xocc/xocc.exe"

if [[ ! -f "${xocc_path}" ]]; then
    echo "ERROR: ${xocc_path} NOT FOUND"
    exit 1
fi

#== TEST SCRIPT OPTIONS ==
#NotQuitEarly

### BUILD ALL CASES ###
#builtin/divmod.c.i.o builtin/_umoddi3.c.i.o builtin/_udivdi3.c.i.o builtin/sqrtf.c.i.o builtin/fabsf.c.i.o
#LinkerFlag = " builtin/_clz32.c.i.o builtin/_clz64.c.i.o builtin/_divdi3.c.i.o builtin/_moddi3.c.i.o builtin/udivmodsi4.c.i.o  "

AsPath=arm-linux-gnueabihf-as
LinkerPath=arm-linux-gnueabihf-gcc 
Simulator=qemu-arm
Targ=arm
XoccFlag=" -no-vect -no-if_cvs "
LinkerFlag=" -static "

SKIPPED_XOCCFLAG="-no-vect -no-if_cvs -no-lsra "

FLAGARR=(
  "${SKIPPED_XOCCFLAG}"
  "-O3 ${SKIPPED_XOCCFLAG}"
  "-O3 -lowest_height -prmode ${SKIPPED_XOCCFLAG} "
  "-O3 -mdssa -prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -lowest_height -mdssa -prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -lowest_height -prmode -mdssa -prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -nonprdu -prdu -mdssa -prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -lowest_height -prmode -nonprdu -prdu -mdssa -prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG} "
  "-O3 -lowest_height -prmode -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG} "
)

for item in "${FLAGARR[@]}"; do
  echo "FLAGARR=$item"
  perl run.pl Targ = $Targ Simulator = $Simulator AsPath = $AsPath \
    LinkerPath = $LinkerPath XoccPath = $xocc_path XoccFlag = "${item}" \
    LinkerFlag = "$LinkerFlag" ShowOutput CompareResultIfExist \
    CompareDumpIfExist

  if [ $? -ne 0 ]; then
    echo "EXECUTE PERL FAILED, ERROR CODE = $?"
    exit 1
  fi
done

#============================
echo "EXECUTE PERL SUCCESS!!"
exit 0
