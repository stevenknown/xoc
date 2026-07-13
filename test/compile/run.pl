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
our $g_is_input_gr;
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
our $g_is_basedumpfile_must_exist;
our $g_error_count;
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
    } elsif ($g_is_create_base_result == 1) {
        #Add the dump file path to flags of xocc.exe.
        my $base_dump_file = getBaseResultDumpFilePath($fullpath);
        if (!-e $base_dump_file) {
            #Compose the path of the new dump file.
            my $dump_file = getDumpFilePath($fullpath);
            $g_cflags = $g_cflags." -dump $dump_file ";
            unlink($dump_file);
        }
    }
    
    #Running CPP.
    my $fullpathaftercpp = runCPP($fullpath);

    #Running XOCC.
    runXOCC($fullpathaftercpp, 0, 0, 0);

    #Restore original flags.
    $g_cflags = $org_cflags;
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
        compileFile($fullpath);
        if ($g_is_create_base_result == 1) {
            #Copy current dumpfile to be the base-result dumpfile.
            my $base_dump_file = getBaseResultDumpFilePath($fullpath);
            if (!-e $base_dump_file) {
                #Copy file if not exist.
                my $dump_file = getDumpFilePath($fullpath);
                print("\nCMD>>copy $dump_file, $base_dump_file\n");
                copy($dump_file, $base_dump_file) or
                    abortex("COPY FILE FAILED!");
            }
        }

        if ($g_is_compare_dump == 1) {
            my $dump_file = getDumpFilePath($fullpath);
            compareDumpFile($fullpath, $dump_file,
                            $g_is_basedumpfile_must_exist);
        }

        if ($g_single_testcase eq "" && $g_is_move_passed_case == 1) {
            #Do NOT move to passed if there is just a singlecase.
            moveToPassed($fullpath);
        }
    }
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
    } elsif ($g_single_directory ne "") {
    	if ($g_is_recur) {
        	@f = findRecursively($g_single_directory, 'c');
        } else {
        	@f = findCurrent($g_single_directory, 'c');
        }
    } elsif ($g_is_recur) {
        @f = findRecursively($curdir, 'c');
    } else {
        @f = findCurrent($curdir, 'c');
    }
    compileFileList($curdir, \@f);
}
