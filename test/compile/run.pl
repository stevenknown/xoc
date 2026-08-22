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
our $g_single_directory; #record the single directory
our $g_as;
our $g_ld;
our $g_ld_flag;
our $g_simulator;
our $g_cflags;
our $g_is_compare_dump;
our $g_error_count;
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
        print "\nThere are $g_error_count error occurred!\n";
        abort(); #always quit immediately.
    }
    print "\nTEST FINISH!\n";
    return $g_succ;
}

sub tryCompile
{
    #Set $g_is_test_gr to true to generate GR and compile GR to asm, then
    #compare the latest output with the base result.
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
        	@$f = findRecursively($g_single_directory, 'c');
        } else {
        	@$f = findCurrent($g_single_directory, 'c');
        }
    } elsif ($g_is_recur) {
        @$f = findRecursively($curdir, 'c');
    } else {
        @$f = findCurrent($curdir, 'c');
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
        compileFile($fullpath);
        if ($g_is_compare_dump == 1) {
            my $xocc_dump_file = getDumpFilePath($fullpath);

            #compareDumpFile($fullpath, $xocc_dump_file,
            #                $g_is_basedumpfile_must_exist);
            checkRuleOfDumpFile($fullpath, $xocc_dump_file, $g_false);
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

sub compileFile
{
    my $fullpath = $_[0];

    #Save original flags.
    my $org_cflags = $g_cflags;
    
    #Extract CFLAG from *.conf and append it to g_cflags.
    extractAndSetCflag($fullpath);
    
    if ($g_is_compare_dump == 1) {
        #Add the dump file path to flags of xocc.exe.
        #Compose the path of the new dump file.
        my $dump_file = getDumpFilePath($fullpath);
        $g_cflags = $g_cflags." -dump $dump_file ";
        unlink($dump_file);
    }
    
    #Running CPP.
    my $fullpathaftercpp = runCPP($fullpath);

    #Running XOCC.
    runXOCC($fullpathaftercpp, 0, 0, 0);

    #Restore original flags.
    $g_cflags = $org_cflags;
}
