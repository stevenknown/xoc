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
sub main
{
    $g_is_quit_early = 0;
    my @subdirlist = (
        'compile',
        'compile.gr',
        'exec',
        'exec.gr',
        'api',
        'xcom',
        'tutorial_input',
        'chibicc-main',
        '9cc',
        'builtin',
        '8cc-master',
    );
    foreach my $subdir (@subdirlist) {
        print "\nENTER DIRECTORY>>$subdir\n";
        chdir $subdir;
        execPerl();
        chdir ".."; #back to parent directory
	}
    return 0;
}

sub execPerl
{
    my $cmdline = "perl clean.pl";
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nFAILED! -- EXECUTE CMD FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }
    return $g_succ;
}
