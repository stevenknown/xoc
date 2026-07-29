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

perl run.pl Targ = arm AsPath = arm-linux-gnueabihf-as LinkerPath = arm-linux-gnueabihf-gcc NoLink NoRun XoccPath = $xocc_path XoccFlag = "-O0 -no-vect -no-lsra" LinkerFlag = " -static "
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
