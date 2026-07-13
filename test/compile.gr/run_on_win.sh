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

perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = ""
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3"
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height"
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa"
perl run.pl Targ = arm XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa"

perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-no-cg"
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg"
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg"
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg"
perl run.pl Targ = arm Dir = nocg XoccPath = $xocc_path NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg"

