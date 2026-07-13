#!/usr/bin/perl -w
use Cwd;
use File::Find;
use File::Copy;
use File::Path;
use File::Compare;
use strict;
#############################################################
# These are imported variables.
our $g_is_create_base_result;
our $g_is_move_passed_case;
our $g_is_test_gr;
our $g_is_input_gr;
our $g_is_invoke_cpp;
our $g_is_invoke_assembler;
our $g_is_invoke_linker;
our $g_is_invoke_simulator;
our $g_is_recur;
our $g_target;
our $g_xocc;
our $g_is_quit_early; #halt test if error occurred.
our $g_osname;
our $g_xoc_root_path;
our $g_single_testcase; #record the single testcase
our $g_find_testcase; #record the single testcase
our $g_is_compare_result;
our $g_is_baseresultfile_must_exist;
our $g_as;
our $g_ld;
our $g_ld_flag;
our $g_simulator;
our $g_cflags;
our $g_ldflags;
our $g_error_count;
our $g_single_directory; #record the single directory
our $g_succ;
our $g_fail;
our $g_is_compare_dump;
our $g_is_basedumpfile_must_exist;
require "../util.pl";
prolog();
main();
#############################################################
sub main
{
    $g_is_invoke_cpp = 0;
    $g_is_input_gr = 1;
    #Set $g_is_test_gr to true to generate GR and compile GR to asm, then
    #compare the latest output with the base result.
    tryCompile();
    if ($g_error_count != 0) {
        print "\nThere are $g_error_count error occurred!\n";
        abort(); #always quit immediately.
    }
    print "\nTEST FINISH!\n";
    return $g_succ;
}


sub tryCompile
{
    my $curdir = getcwd;
    if ($g_single_directory ne "") {
        $curdir .= "/".$g_single_directory;
    }
    #Collect files that need to test.
    my @f;
    if ($g_single_testcase ne "") {
        if ($g_is_recur) {
            @f = findFileRecursively($curdir, $g_single_testcase);
        } else {
            @f = findFileCurrent($curdir, $g_single_testcase);
        }
    } elsif ($g_is_recur) {
        @f = findRecursively($curdir, 'gr');
    } else {
        @f = findCurrent($curdir, 'gr');
    }
    compileFileList($curdir, \@f);
}


sub compileFileList
{
    my $curdir = $_[0];
    my @filelist = @{$_[1]};
    foreach (@filelist) {
        $g_error_count = 0; #initialize error counter.
        chomp;
        my $filename = getFileNameFromPath($_);
        my $fullpath = $curdir."/".$filename;
        print "\n-------------------------------------------";
        compileAssembleLinkRunFile($fullpath, $curdir);
        if ($g_single_testcase eq "" && $g_is_move_passed_case == 1) {
            #Do NOT move to passed if there is just a singlecase.
            moveToPassed($fullpath);
        }
    }
}


sub compileAssembleLinkRunFile
{
    my $fullpath = $_[0];
    my $curdir = $_[1];
    my $base_output_path = getBaseOutputFilePath($fullpath);
    my $fullexepath = compileFile($fullpath);
    if ($g_is_test_gr == 1) {
        generateGRandCompile($fullexepath);
    }
    if ($g_is_invoke_simulator) {
        runSimAndCompareResult($curdir, $fullexepath, $base_output_path);
    }
}


sub compileFile
{
    my $fullpath = $_[0];

    #Save original flags.
    my $org_cflags = $g_cflags;
    my $org_ldflags = $g_ldflags;
    
    #Extract customized CFLAG from configure file.
    extractAndSetCflag($fullpath);

    #Extract customized LDFLAG from configure file.
    extractAndSetLDflag($fullpath);

    #Try add dump file path to g_cflags.
    if ($g_is_create_base_result == 1 or $g_is_compare_dump == 1) {
        #Add the dump file path to flags of xocc.exe.
        my $dump_file = getDumpFilePath($fullpath);
        $g_cflags = $g_cflags." -dump $dump_file ";
        unlink($dump_file);
    }

    #Running CPP.
    my $fullpathaftercpp = tryRunCpp($fullpath);

    #Running XOCC.
    my $ret_exename = "";
    runXOCC($fullpathaftercpp, $g_is_invoke_assembler,
            $g_is_invoke_linker, $g_is_input_gr, \$ret_exename);
    print "\nRETURNED_EXENAME:'$ret_exename'\n";

    #Restore original flags.
    $g_cflags = $org_cflags;
    $g_ldflags = $org_ldflags;
    return $ret_exename;
    #return $fullpathaftercpp;
}


sub runSimAndCompareResult
{
    my $curdir = $_[0];
    my $fullexepath = $_[1];
    my $base_output_path = $_[2];

    #The new result output file.
    my $xocc_output = getOutputFilePath($fullexepath);
    my $rundir = computeDirFromFilePath($fullexepath);
    my $res = invokeSimulator($fullexepath, $curdir, $xocc_output, $rundir);
    tryShowOutput($xocc_output);
    if ($res != $g_succ) {
        abortex("EXECUTE:invokeSimulator FAILED!!");
    }
    if (!$g_is_compare_result) { return; }

    #The baseline result output file.
    if ($g_target eq "apc") {
        #Remove the first 2 redundant lines dumped by apc simulator.
        removeLine($g_xoc_root_path, $xocc_output, 2);
    }
    if ($rundir ne $curdir) {
        chdir $curdir;
    }
    if (!-e $base_output_path) {
        #Baseline output file does not exist.
        if ($g_is_baseresultfile_must_exist) {
            #Baseline output file is necessary.
            print "\nBASE OUTPUT OF $fullexepath NOT EXIST!\n";
            abortex();
        }
        return;
    }

    #Compare baseline output file and testcase binary's output file.
    print("\nCMD>>compare $base_output_path $xocc_output\n");
    if (compare($base_output_path, $xocc_output) == 0) {
        #New result is euqal to baseline result.
        #New result is correct.
        print "\nPASS!\n";
    } else {
        #Not equal
        #New result is incorrect!
        print "\nCOMPARE RESULT OF $fullexepath FAILED! NOT EQUAL TO BASE RESULT!\n";
        abortex();
    }
}
