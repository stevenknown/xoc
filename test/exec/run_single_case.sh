#!/bin/bash
set -e
if [ -z "$1" ]; then
  echo "MISS INPUT FILE"
  echo "USAGE: ./run_single_case.sh YourFileName"
  exit 1
fi
xocc_path="../../xocc/xocc.exe"
#CreateBaseResult

AsPath=arm-linux-gnueabihf-as
LinkerPath=arm-linux-gnueabihf-gcc 
Simulator=qemu-arm
Targ=arm
#XoccFlag=" "
XoccFlag="-O3 -no-vect -no-if_cvs -dump a.log -dump-all "
LinkerFlag=" -static "

perl run.pl Case = $1 Targ = $Targ Simulator = $Simulator AsPath = $AsPath \
  LinkerPath = $LinkerPath XoccPath = $xocc_path XoccFlag = "$XoccFlag" \
  LinkerFlag = "$LinkerFlag" ShowOutput CompareResultIfExist CompareDumpIfExist

if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
