xocc_path="../xocc/xocc.exe"

### ONLY COMPILE ALL CASES ###
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode -no-vect -no-lsra"
if [ $? -ne 0 ]; then
  echo "EXECUTE PERL FAILED, ERROR CODE = $?"
  exit 1
fi


### BUILD ALL CASES ###
perl run.pl Targ = armhf XoccPath = $xocc_path XoccFlag = "-O3 -no-vect -no-lsra" CompareDumpIfExist CompareResultIfExist LinkerFlag = " "
