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
our $g_true;
our $g_false;
require "../util.pl";
prolog();
main();
#############################################################
sub main
{
    tryCompile($g_is_test_gr);
    if ($g_error_count != 0) {
        print "\nTHERE ARE $g_error_count ERROR OCCURRED!\n";
        abort(); #always quit immediately.
    }
    print "\nTEST FINISH!\n";
    return $g_succ;
}

sub tryCompile
{
    #Set $is_test_gr to true to generate GR and compile GR to asm, then compare
    #the latest output with the base result.
    my $is_test_gr = $_[0];
    my $curdir = getcwd;
    if ($g_single_directory ne "") {
        $curdir .= "/".$g_single_directory;
    }
    #Collect files that need to test.
    my @f=();
    collectTestCase($curdir, \@f);
    compileTestCase($curdir, $is_test_gr, \@f);
}

sub collectTestCase
{
    my $curdir = $_[0];
    my $f = $_[1];
    if ($g_single_testcase ne "") {
        if ($g_is_recur) {
            @$f = findFileRecursively($curdir, $g_single_testcase);
        } else {
            @$f = findFileCurrent($curdir, $g_single_testcase);
        }
    } elsif ($g_single_directory ne "") {
    	if ($g_is_recur) {
        	@$f = findRecursively($g_single_directory, 'gr');
        } else {
        	@$f = findCurrent($g_single_directory, 'gr');
        }
    } elsif ($g_is_recur) {
        @$f = findRecursively($curdir, 'gr');
    } else {
        @$f = findCurrent($curdir, 'gr');
    }
}

sub compileTestCase
{
    my $curdir = $_[0];
    my $is_test_gr = $_[1];
    my $filelist = $_[2];
    foreach (@$filelist) {
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
            #compareDumpFile($fullpath, $xocc_dump_file, $g_false);
            checkRuleOfDumpFile($fullpath, $xocc_dump_file, $g_false);
            if ($g_error_count > 0) { next; }
        }
        if ($is_test_gr == 1) {
            generateGRandCompile($fullpath);
            if ($g_error_count > 0) { next; }
        }
        if ($g_error_count > 0) { next; }
        if ($g_single_testcase eq "" && $g_is_move_passed_case == 1) {
            #Move file to passed only success processed.
            #Do NOT move to passed if there is just a singlecase.
            moveToPassed($fullpath);
        }
    }
}
