xocc_path="../../src/xocc/xocc.exe"

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
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case = array_and_compute_sensitive_code.set_key.gr
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case =  array_and_compute_sensitive_code.array_mode.gr
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time" Dir = slow Case = array_and_compute_sensitive_code.encrypt.gr
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case = array_and_compute_sensitive_code.pr_mode.gr
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case = array_and_compute_sensitive_code.decrypt.gr

#### FOLLOW TEST ENABLES ALL OPTIMIZATIONS ##########################################
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = ""
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-no-cg"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi
#perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg"
#if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
#fi


#### FOLLOW TEST IS DISABLE BOTH VECT AND LSRA ##########################################
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-migen -no-lsra -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-vect -prmode -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -prmode -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-no-cg -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -no-vect -prmode -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -no-vect -no-lsra"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -no-vect -prmode -no-lsra"
if [ $? -ne 0 ]; then  echo "EXECUTE PERL FAILED, ERROR CODE = $?"  exit 1
fi

##### THE FOLLOWED CASE DO NOT PERFORM CLASSIC_DU #####
perl run.pl Targ = arm Dir = no_classic_prdu XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-migen -no-lsra -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = no_classic_prdu XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = no_classic_prdu XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = no_classic_prdu XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-vect -no-lsra -no-cg"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
perl run.pl Targ = arm Dir = no_classic_prdu XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-vect -prmode -no-lsra -no-cg"
if [ $? -ne 0 ]; then echo "EXECUTE PERL FAILED, ERROR CODE = $?" exit 1
fi
