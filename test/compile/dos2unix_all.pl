#!/usr/bin/perl -w

use File::Find;
use Data::Dump qw(dump);
use strict;

my $project_dir = ".";

#my @srclist = `find $project_dir -type f | grep -E '\\.(h|cpp|c|txt|cc)\$'`;
my @srclist = ();
sub wanted{
    push(@srclist, $File::Find::name) if ($_ =~ m/.*\.(c|cpp|h|txt|cc|conf|sh|pl)$/);
    push(@srclist, $File::Find::name) if ($_ =~ m/Makefile.*/);
}
&find(\&wanted, $project_dir);

foreach (@srclist){
    chomp;
    print "\nFile: ", $_, "\n";
    my $cmdline = "dos2unix \"$_\"";
	print $cmdline;
	my $retval = system($cmdline);
	if ($retval != 0) {
		exit($retval);
	}
}

my $files = @srclist;
printf "SUCCESS!! Total checked file: %d\n", $files;
