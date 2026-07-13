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
our $g_true;
our $g_false;
require "./util.pl";
main();
#############################################################
my $g_is_debug;
sub main
{
    my $debug = $ARGV[0];
    $g_is_debug = $g_true;
    if ($debug and $debug eq "release") {
        $g_is_debug = $g_false;
    }
    $g_is_quit_early = 0;
    my @subdirlist = (
        '8cc-master',
        '9cc',
        'exec',
        'exec.gr',
        'compile.gr',
        'compile',
        'chibicc-main',
        'builtin',
        #'compile.gr.pcx',
        #'api',
        #'xcom',
        #'tutorial_input',
        #'llm.c-master',
    );
    foreach my $subdir (@subdirlist) {
        print "\nENTER DIRECTORY>>$subdir\n";
        chdir $subdir;
        if (execPerl() ne $g_succ) {
            return 1;    
        }
        chdir ".."; #back to parent directory
	}
    print "\n\n\nALL TESTCASES FINISH!!!\n";
    return 0;
}

sub execPerl
{
    my $curdir = getCurDir();
    #my $cmdline = "sh run.sh";
    my $cmdline = "sh run_no_cg.sh";
    if (!$g_is_debug) {
        $cmdline = "sh run_release.sh";
    }
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCURDIR>>", $curdir, "\n");
        print("\nCMD>>", $cmdline, "\n");
        print "\nFAILED! -- EXECUTE CMD FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }
    return $g_succ;
}
