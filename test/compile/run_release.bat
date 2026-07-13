@rem SET XOCC.EXE PATH HERE.
set xocc_path=../xocc/xocc.exe

perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -prmode -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -mdssa -prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -mdssa -prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -prmode -mdssa -prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -prmode -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -nonprdu -prdu -no-mdssa -no-prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -nonprdu -prdu -no-mdssa -no-prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% XoccFlag = "-O3 -lowest_height -prmode -nonprdu -prdu -no-mdssa -no-prssa -no-vect -no-lsra -no-cg"
@if %errorlevel% neq 0 (@exit /b 2)
@exit /b 0
