@rem SET XOCC.EXE PATH HERE.
set xocc_path=../xocc/xocc.exe

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-prmode -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -prmode -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -prmode -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -nonprdu -prdu -mdssa -prssa -prmode -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)

perl run.pl Targ = arm XoccPath = %xocc_path% CompareDumpIfExist NotQuitEarly NoAsm XoccFlag = "-O3 -lowest_height -nonprdu -prdu -mdssa -prssa -prmode -no-vect -no-lsra"
@if %errorlevel% neq 0 (@exit /b 2)
