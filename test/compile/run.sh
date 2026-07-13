xocc_path="../../src/xocc/xocc.exe"

### OPTIONS
#CompareDumpIfExist
#Dir = passed 
#CompareDump 
#CreateBaseResult
#MovePassed
#NotQuitEarly
#CompareDump

perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -prmode -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -mdssa -prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -mdssa -prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -prmode -mdssa -prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -prmode -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -nonprdu -prdu -no-mdssa -no-prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl NoAsm Targ = arm XoccPath = $xocc_path XoccFlag = "-O3 -lowest_height -prmode -nonprdu -prdu -no-mdssa -no-prssa -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi

echo "EXECUTE PERL SUCCESS!!"
exit 0
