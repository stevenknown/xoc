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
XoccFlag="-O3 -no-vect -no-lsra -no-if_cvs "
LinkerFlag=" -static "
perl run.pl Targ = $Targ Simulator = $Simulator AsPath = $AsPath \
  LinkerPath = $LinkerPath XoccPath = $xocc_path XoccFlag = "$XoccFlag" \
  LinkerFlag = "$LinkerFlag" ShowOutput CompareResultIfExist CompareDumpIfExist

if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
