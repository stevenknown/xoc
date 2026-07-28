xocc_path="../../src/xocc/xocc.exe"
#XoccFlag = "-O3 -cp -gvn -rce -no-vect -no-lsra -include_region = "test_qsort1" " \

### OPTIONS
#NotQuitEarly

### BUILD ALL CASES ###
#builtin/divmod.c.i.o builtin/_umoddi3.c.i.o builtin/_udivdi3.c.i.o builtin/sqrtf.c.i.o builtin/fabsf.c.i.o
#NotQuitEarly
#LinkerFlag = " builtin/_clz32.c.i.o builtin/_clz64.c.i.o builtin/_divdi3.c.i.o builtin/_moddi3.c.i.o builtin/udivmodsi4.c.i.o  "
perl run.pl Targ = armhf NotQuitEarly XoccPath = $xocc_path XoccFlag = "-O0 -no-vect -no-lsra" CompareDumpIfExist CompareResultIfExist

