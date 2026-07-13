xocc_path="../../src/xocc/xocc.exe"
#XoccFlag = "-O3 -only-rp -rp -gvn -no-vect -no-lsra -dump-all -dump tmp.log -include_region = "sort""
#perl run.pl Case = select_exp_list.c Targ = armhf XoccPath = $xocc_path XoccFlag = "-O0 -no-vect -no-lsra" LinkerFlag = "builtin/sqrt.c.i.o builtin/fabs.c.i.o " CompareDumpIfExist CompareResultIfExist

### BUILD ALL CASES ###
perl run.pl Targ = armhf XoccPath = $xocc_path XoccFlag = "-O3 -no-vect -no-lsra" CompareDumpIfExist CompareResultIfExist LinkerFlag = " " NoRun NoLink
