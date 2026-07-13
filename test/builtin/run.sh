xocc_path="../../src/xocc/xocc.exe"

### ONLY COMPILE ALL CASE ###
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -lowest_height -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -nonprdu -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -lowest_height -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -lowest_height -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi

### BUILD ALL CASES ###
#perl run.pl Targ = armhf  NoRun NoLink XoccPath = $xocc_path XoccFlag = "-O3 -no-vect -no-lsra" CompareDumpIfExist CompareResultIfExist LinkerFlag = " "
