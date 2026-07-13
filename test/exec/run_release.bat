@rem SET XOCC.EXE PATH HERE.
set xocc_path=../xocc/xocc.exe

 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -lowest_height -prmode -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -nonprdu -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -lowest_height -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
 perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -lowest_height -prmode -no-vect -no-lsra"
 @if %errorlevel% neq 0 (@exit /b 2)
@exit /b 0
