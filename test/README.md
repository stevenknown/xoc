# Introduction
testsuite of XOCC

# How to run testcase
## e.g:show help of run.pl
   cd test/exec && perl ./run.pl 

## e.g:run testcase under 'exec' and 'compile'.
   cd test/exec && perl ./run.pl arm OverrideXoccPath = ~/yourname/arm/build/xocc.exe OverrideXoccFlag = "-O3 -prssa -mdssa -licm -dce -rp"
   cd test/compile && perl ./run.pl arm  

## e.g:run single testcase under 'exec'.
   cd test/exec && perl ./run.pl arm Case = hello.c OverrideXoccPath = ~/yourname/arm/build/xocc.exe

## e.g:run single testcase under 'exec' without target dependent code generation.
   cd test/exec && perl ./run.pl arm Case = hello.c OverrideXoccPath = ~/yourname/arm/build/xocc.exe NoCG

## e.g:run single testcase under 'exec' without target dependent code generation, and compare the log file to the baseline log file to verify the correctness of compiler.
   cd test/exec && perl ./run.pl arm Case = hello.c OverrideXoccPath = ~/yourname/arm/build/xocc.exe NoCG CompareDump
