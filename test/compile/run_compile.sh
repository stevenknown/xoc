xocc_path="../../src/xocc/xocc.exe"

#== TEST SCRIPT OPTIONS ==
#CompareDumpIfExist
#Dir = passed 
#CompareDump 
#CreateBaseResult
#MovePassed
#NotQuitEarly
#CompareDump

SKIPPED_XOCCFLAG="-no-vect -no-if_cvs -no-lsra -no-cg "
SKIPPED_XOCCFLAG_2="-no-if_cvs -no-lsra -no-cg "

FLAGARR=(
 "${SKIPPED_XOCCFLAG_2} "
 "-O3 ${SKIPPED_XOCCFLAG_2} "
 "-O3 -lowest_height ${SKIPPED_XOCCFLAG_2} "
 "-O3 -lowest_height -prmode ${SKIPPED_XOCCFLAG_2} "
 "-O3 -nonprdu ${SKIPPED_XOCCFLAG_2} "
 "-O3 -nonprdu -prdu ${SKIPPED_XOCCFLAG_2} "
 "-O3 -nonprdu -prdu -mdssa ${SKIPPED_XOCCFLAG_2} "
 "-O3 -nonprdu -prdu -mdssa -prssa ${SKIPPED_XOCCFLAG_2} "
 "-O3 -nonprdu -prdu -mdssa -prssa -lowest_height ${SKIPPED_XOCCFLAG_2} "
 "-O3 -nonprdu -prdu -mdssa -prssa -lowest_height -prmode ${SKIPPED_XOCCFLAG_2} "
)

for item in "${FLAGARR[@]}"; do
  echo "FLAGARR=$item"
  perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "${FLAGARR}"
  if [ $? -ne 0 ]; then
    echo "EXECUTE PERL FAILED, ERROR CODE = $?"
    exit 1
  fi
done

#============================
echo "EXECUTE PERL SUCCESS!!"
exit 0
