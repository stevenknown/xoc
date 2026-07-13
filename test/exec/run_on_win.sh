xocc_path=../xocc/xocc.exe
### OPTIONS
#NotQuitEarly 

### RUN SINGLE CASE ###
#perl run.pl Case = rem.c Targ = armhf XoccPath = $xocc_path \
#  XoccFlag = "-O3 -no-vect -no-lsra -only-dce -cp -gvn -cfgopt -dce_aggr -dump-all -dump tmp.log " \
#  LinkerFlag = " builtin/_clz32.c.i.o builtin/_clz64.c.i.o \
#    builtin/_divdi3.c.i.o builtin/_moddi3.c.i.o builtin/udivmodsi4.c.i.o \
#    builtin/sqrtf.c.i.o builtin/fabsf.c.i.o " CompareDumpIfExist

### BUILD ALL CASES ###
perl run.pl Targ = armhf XoccPath = $xocc_path \
  XoccFlag = "-O3 -no-vect -no-lsra" CompareDumpIfExist CompareResultIfExist\
  LinkerFlag = " "
