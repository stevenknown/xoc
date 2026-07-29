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
#CompareDump
#CompareDumpIfExist
#NotQuitEarly
#CreateBaseResult
#Recur
#CompareDumpIfExist
#CompareDumpIfExist 
#MovePassed
#Recur
#NotQuitEarly
#Dir = slow Case = array_and_compute_sensitive_code.array_mode.gr
#Dir = passed 

SKIPPED_XOCCFLAG="-no-vect -no-if_cvs -no-lsra -no-cg "
SKIPPED_XOCCFLAG_2="-no-if_cvs -no-lsra -no-cg "

#== CASE1 ==
FLAGARR=(
 " ${SKIPPED_XOCCFLAG} "
 "-O3 -no-migen ${SKIPPED_XOCCFLAG} "
 "-O3  ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -prmode ${SKIPPED_XOCCFLAG} "
 "-O3 -nonprdu -prdu -mdssa -prssa  ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa  ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode  ${SKIPPED_XOCCFLAG} "

 "${SKIPPED_XOCCFLAG_2}  "
 "-O3 -no-migen ${SKIPPED_XOCCFLAG_2} "
 "-O3 ${SKIPPED_XOCCFLAG_2}  "
 "-O3 -lowest_height ${SKIPPED_XOCCFLAG_2}  "
 "-O3 -lowest_height -prmode ${SKIPPED_XOCCFLAG_2}  "
 "-O3 -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG_2}  "
 "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG_2}  "
 "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa -prmode ${SKIPPED_XOCCFLAG_2}  "
)

for item in "${FLAGARR[@]}"; do
  echo "FLAGARR=$item"
  perl run.pl NoAsm NoLink NoRun Targ = arm XoccPath = $xocc_path XoccFlag = "${FLAGARR}"
  if [ $? -ne 0 ]; then
    echo "EXECUTE PERL FAILED, ERROR CODE = $?"
    exit 1
  fi
done

#== CASE2 ==
DIR=no_classic_prdu

FLAGARR=(
 " ${SKIPPED_XOCCFLAG} "
 "-O3 -no-migen ${SKIPPED_XOCCFLAG} "
 "-O3 ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -prmode ${SKIPPED_XOCCFLAG} "
)

for item in "${FLAGARR[@]}"; do
  echo "FLAGARR=$item"
  perl run.pl Dir = ${DIR} NoAsm NoLink NoRun Targ = arm XoccPath = $xocc_path XoccFlag = "${FLAGARR}"
  if [ $? -ne 0 ]; then
    echo "EXECUTE PERL FAILED, ERROR CODE = $?"
    exit 1
  fi
done

#== CASE3 ==
DIR=nocg

FLAGARR=(
 "${SKIPPED_XOCCFLAG}"
 "-O3 ${SKIPPED_XOCCFLAG} "
 "-O3 -nonprdu -prdu -mdssa -prssa ${SKIPPED_XOCCFLAG} "
 "-O3 -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -prmode ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa ${SKIPPED_XOCCFLAG} "
 "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa -prmode ${SKIPPED_XOCCFLAG} "
)

for item in "${FLAGARR[@]}"; do
  echo "FLAGARR=$item"
  perl run.pl Dir = ${DIR} NoAsm NoLink NoRun Targ = arm XoccPath = $xocc_path XoccFlag = "${FLAGARR}"
  if [ $? -ne 0 ]; then
    echo "EXECUTE PERL FAILED, ERROR CODE = $?"
    exit 1
  fi
done

#============================ 
echo "EXECUTE PERL SUCCESS!!"
exit 0
