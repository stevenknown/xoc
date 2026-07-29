#!/usr/bin/perl -w
use Cwd;
use File::Find;
use File::Copy;
use File::Path;
use File::Compare;
use strict;
our $g_fail;
our $g_succ;
################################################################################
require "../util.pl";
my $curdir = getcwd;
my @srclist = ();
my @filelist = ();
my $suffix = "";
main();
################################################################################
sub main
{
    removeFileRecur();
    removeDirectoryRecur();
    removeDesignatedDir();
    removeDesignatedFile("api");
    print "\nCLEAN FINISH\n";
} 

sub removeDesignatedFile
{
    my $dir = $_[0];
    if (!is_exist($dir)) {
        abortex("\n$dir IS NOT EXIST\n");
        return $g_fail; #No need execute the following code.
    }
    @srclist = ();
    sub designated_dir
    {
        push(@srclist, $File::Find::name) if ($_ =~ m/.*\.(tmp|dump|pdf|dot|swp|swo|asm|png|dot|vcg|log|VC.db|suo|tmp|TMP|dump|filters|vcxproj.user)$/);
    }
    &find(\&designated_dir, $dir);

    ## Begin removing.
    foreach (@srclist) {
        chomp;
        print "=========\nREMOVE FILE: ", $_, "\n";
        my $retval = unlink $_;
        if ($retval != 0) {
            print("\nFailed:retval=$retval");
    	}
    }
}

## Remove directory.
sub removeDirectoryRecur
{
    my $project_dir = $curdir;
    sub removeDirectoryHelper
    {
        push(@srclist, $File::Find::name) if ($_ =~ m/^(Debug|Release|\.vs|ipch|tmp)$/);
    }
    &find(\&removeDirectoryHelper, $project_dir);
    foreach (@srclist){
        chomp;
        print "=========\nREMOVE DIR: ", $_, "\n";
    	my $retval = removeDir($_);
        if ($retval != $g_succ) {
            print("\nFailed:retval=$retval");
    	}
    }
}

sub removeFileRecur
{
    my @f = ();
    push(@f, findRecursivelyInApiDir($curdir, 'LOGLOG$'));
    push(@f, findRecursivelyInApiDir($curdir, 'succ$'));
    push(@f, findRecursivelyInApiDir($curdir, 'fail$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.asm$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.tmp$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.log$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.vcg$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.exe$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.d$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.B$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.i$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.s$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.s$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.t$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.spin$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*\.o$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*xocc_output\.txt$'));
    push(@f, findRecursivelyInApiDir($curdir, '.*xocc_dump\.txt$'));
    foreach (@f) {
    	chomp;
    	print "\nunlink:$_";
    	my $retval = unlink($_);
        if ($retval != 1) {
            print("\nFailed:retval=$retval");
    	}
    }
}

sub removeDesignatedDir
{
    my @f2 = (
    	"$curdir\\tmp",
    );
    foreach (@f2) {
    	chomp;
    	print "\nremoveDir:$_";
    	my $retval = removeDir($_);
        if ($retval != $g_succ) {
            print("\nFailed:retval=$retval");
    	}
    }
}

sub findRecursivelyInApiDir {
    my $dir = $_[0];
    $suffix = $_[1];
    @filelist = ();
    &find(\&findCoreInApiDir, $dir);
    return @filelist;
}

sub findCoreInApiDir {
    my $pattern = $suffix;
    push(@filelist, $File::Find::name) if ($_ =~ m/$pattern/);
}

