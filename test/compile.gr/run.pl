#!/usr/bin/perl -w

use Cwd;
use File::Find;
use File::Copy;
use File::Path;
use File::Compare;
use strict;
#############################################################
our $g_is_create_base_result;
our $g_is_move_passed_case;
our $g_is_test_gr;
our $g_is_invoke_assembler;
our $g_is_invoke_linker;
our $g_is_invoke_simulator;
our $g_is_recur;
our $g_target;
our $g_is_quit_early; #finish test if error occurred.
our $g_osname;
our $g_xoc_root_path;
our $g_single_testcase; #record the single testcase
our $g_as;
our $g_ld;
our $g_ld_flag;
our $g_simulator;
our $g_cflags;
our $g_is_compare_dump;
our $g_error_count;
our $g_single_directory; #record the single directory
our $g_succ;
our $g_fail;
require "../util.pl";
prolog();
main();
#############################################################
sub main 
{
    # mkpath(["log"]);
    # clean();
    tryCompileAsmLinkRunCompare($g_is_test_gr);
    if ($g_error_count != 0) {
        print "\nTHERE ARE $g_error_count ERROR OCCURRED!\n";
        abort(); #always quit immediately.
    }
    print "\nTEST FINISH!\n";
    return $g_succ;
}

sub tryCompileAsmLinkRunCompare
{   
    #Set $is_test_gr to true to generate GR and compile GR to asm, then compare
    #the latest output with the base result.
    my $is_test_gr = $_[0];
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
    } elsif ($g_single_directory ne "") {
    	if ($g_is_recur) {
        	@f = findRecursively($g_single_directory, 'gr');
        } else {
        	@f = findCurrent($g_single_directory, 'gr');
        }
    } elsif ($g_is_recur) {
        @f = findRecursively($curdir, 'gr'); 
    } else {
        @f = findCurrent($curdir, 'gr'); 
    }
    if ($g_is_create_base_result == 1) {
        #Generate base-output log.
        generateBaseOutput($curdir, \@f);
    }
    compileAsmLinkRunCompareMove($curdir, $is_test_gr, \@f);
}

sub generateBaseOutput
{
    my $curdir = $_[0];
    my @filelist = @{$_[1]};
    foreach (@filelist) {
        chomp;
        my $fullpath = $_; 
        #The baseline result file.
        my $base_output = $fullpath.".base_output.txt";
        if (!-e $base_output) {
            #Generate baseline output result if it does not exist.
            if ($g_target eq "arm") {
                runBaseccToolChainToComputeBaseResult($fullpath,
                                                      $base_output,
                                                      $curdir);
            } else {
                print "\nUNKNOWN TARGET: $g_target!\n";
                abortex();
            }
        }
    }
}

sub compileAsmLinkRunCompareMove
{
    my $curdir = $_[0];
    my $is_test_gr = $_[1];
    my @filelist = @{$_[2]};
    foreach (@filelist) {
        $g_error_count = 0; #initialize error counter.
        chomp;
        my $filename = getFileNameFromPath($_);
        my $fullpath = $curdir."/".$filename; 
        print "\n-------------------------------------------";
        my $org_cflags = $g_cflags;

        #Apply *.conf file.
        extractAndSetCflag($fullpath);

        #The new dump file.
        my $xocc_dump_file = $fullpath.".xocc_dump.txt";
        unlink($xocc_dump_file);

        if ($g_is_compare_dump == 1) {
            #Add the dump file path to flags of xocc.exe.
            $g_cflags = $g_cflags." -dump $xocc_dump_file ";
        }

        #Running XOCC.
        runXOCC($fullpath, $g_is_invoke_assembler, $g_is_invoke_linker); 
        if ($g_error_count > 0) { next; }

        #Restore original flags.
        $g_cflags = $org_cflags;

        #Compare the new dump file.
        if ($g_is_compare_dump == 1) {
            compareDumpFile($fullpath, $xocc_dump_file);
            if ($g_error_count > 0) { next; }
        }

        if ($is_test_gr == 1) {
            generateGRandCompile($fullpath);
            if ($g_error_count > 0) { next; }
        }

        if ($g_is_invoke_simulator) {
            run($curdir, $fullpath);
        }
 
        if ($g_error_count > 0) { next; }

        if ($g_is_move_passed_case == 1) {
            #Move file to passed only success processed.
            moveToPassed($fullpath);
        }
    }
}

sub run
{
    my $curdir = $_[0];
    my $fullpath = $_[1];

    #The new result file.
    my $xocc_output = $fullpath.".xocc_output.txt";
    my $rundir = computeDirFromFilePath($fullpath);

    invokeSimulator($fullpath, $curdir, $xocc_output, $rundir);

    #The baseline result file.
    my $base_output = $fullpath.".base_output.txt";

    if ($g_target eq "apc") {
        #Remove the first 2 redundant lines dumped by apc simulator.
        removeLine($g_xoc_root_path, $xocc_output, 2);
    }

    if ($rundir ne $curdir) {
        chdir $curdir;
    }
  
    if (!-e $base_output) { 
        #Baseline output result does not exist.
        print "\nBASE OUTPUT OF $fullpath NOT EXIST!\n";
        abortex();
    }
  
    #Compare baseline output and xocc.exe's output.
    print("\nCMD>>compare $base_output $xocc_output\n");
    if (compare($base_output, $xocc_output) == 0) {
        #New result is euqal to baseline result.
        #New result is correct.
    } else {
        #Not equal
        #New result is incorrect!
        print "\nCOMPARE RESULT OF $fullpath FAILED! NOT EQUAL TO BASE RESULT!\n";
        abortex();
    }
}
