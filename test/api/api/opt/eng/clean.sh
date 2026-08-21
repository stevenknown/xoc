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

make clean -f Makefile TARG=FOR_ARM TARG_DIR=${XOC_HOME}/arm ROOT_DIR=${XOC_HOME}

