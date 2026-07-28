#!/bin/bash
set -e
xocc_path="../../src/xocc/xocc.exe"
perl run.pl Targ = arm AsPath = arm-linux-gnueabihf-as LinkerPath = arm-linux-gnueabihf-gcc NoLink NoRun XoccPath = $xocc_path XoccFlag = "-O0 -no-vect -no-lsra" LinkerFlag = " -static "
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
