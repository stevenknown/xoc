@rem set xocc_path=d:\X\src\xocc.for.arm\x64\Debug\xocc.for.arm.2.exe
set xocc_path=d:\X\src\xocc.for.arm\x64\Release\xocc.for.arm.2.exe
@rem set xocc_path=d:\X\src\xocc.for.arm\x64\Debug\xocc.for.arm.exe
@REM set xocc_path=d:\X\src\xocc.for.arm\x64\Release\xocc.for.arm.exe

@REM CompareDump
@REM CompareDumpIfExist
@REM CreateBaseResult
@REM Recur
@rem MovePassed
@rem NotQuitEarly
@rem Dir = slow Case = array_and_compute_sensitive_code.array_mode.gr
@rem Dir = passed
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case = array_and_compute_sensitive_code.set_key.gr
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case =  array_and_compute_sensitive_code.array_mode.gr
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time" Dir = slow Case = array_and_compute_sensitive_code.encrypt.gr
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case = array_and_compute_sensitive_code.pr_mode.gr
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -time"  Dir = slow Case = array_and_compute_sensitive_code.decrypt.gr
@rem perl run.pl Case = alias.loop.gr Targ = arm XoccPath = %xocc_path%  NoAsm NoLink NoRun
@rem perl run.pl Case = gvn3.gr Targ = arm XoccPath = %xocc_path%  NoAsm NoLink NoRun CompareDump
@rem perl run.pl Case = gvn4.gr Targ = arm XoccPath = %xocc_path%  NoAsm NoLink NoRun CompareDump

@rem #### FOLLOW TEST IS DISABLE BOTH VECT AND LSRA ##########################################
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-migen -no-lsra -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-vect -prmode -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -prmode -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-no-cg -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -no-vect -prmode -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -no-vect -prmode -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
@exit /b 0

@rem @rem #### FOLLOW TEST IS DISABLE LSRA ##########################################
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = " -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -prmode -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-no-cg -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -prmode -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -prmode -no-lsra"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem @exit /b 0
@rem @rem ###### DELETE END #######################################

@rem ##########################################################################
@rem ###### FOLLOWING IS ORIGINAL VERSION ####################################################################
@rem ##########################################################################
@rem ##########################################################################
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-migen"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = ""
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem @exit /b 0

@rem ##########################################################################
@rem ##########################################################################
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = ""
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -no-cg -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem perl run.pl Targ = arm Dir = nocg XoccPath = %xocc_path% NoAsm NoLink NoRun XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-cg -prmode"
@rem @if %errorlevel% neq 0 (@exit /b 2)
@rem @exit /b 0

