#!/bin/bash
set -e
xocc_path="../../src/xocc/xocc.exe"
#XoccFlag = "-O3 -cp -gvn -rce -no-vect -no-lsra -include_region = "test_qsort1" " \
### OPTIONS
#NotQuitEarly
### BUILD ALL CASES ###
#builtin/divmod.c.i.o builtin/_umoddi3.c.i.o builtin/_udivdi3.c.i.o builtin/sqrtf.c.i.o builtin/fabsf.c.i.o
#NotQuitEarly
#LinkerFlag = " builtin/_clz32.c.i.o builtin/_clz64.c.i.o builtin/_divdi3.c.i.o builtin/_moddi3.c.i.o builtin/udivmodsi4.c.i.o  "
#perl run.pl Targ = armhf NotQuitEarly XoccPath = $xocc_path XoccFlag = "-O0 -no-vect -no-lsra" CompareDumpIfExist CompareResultIfExist

# RUN SINGLE CASE.
#perl run.pl Targ = arm Case = $1 Simulator = qemu-arm-static XoccPath = $xocc_path XoccFlag = "-O3 -no-vect -no-lsra"  LinkerFlag = "-static "
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi

# RUN ALL CASES.
#perl run.pl Targ = arm Simulator = qemu-arm-static XoccPath = $xocc_path XoccFlag = "-O3 -no-vect -no-lsra -no-if_cvs "  LinkerFlag = "-static " CompareDumpIfExist CompareResultIfExist

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
