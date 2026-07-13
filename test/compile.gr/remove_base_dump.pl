#!/usr/bin/perl -w

use Cwd;
use File::Find;
use File::Copy;
use File::Path;
use File::Compare;
use strict;
#############################################################
require "../util.pl";
main();
#############################################################
sub main
{
    if (!$ARGV[0]) {
        print("\nUSAGE: perl remove_base_dump.pl YourFileName\n");
        exit(1);
    }
    if (!is_exist($ARGV[0])) {
        print("\n$ARGV[0] DOES NOT EXIST.\n");
        exit(1);
    }
    my $base_dump = getBaseResultDumpFilePath($ARGV[0]);
    unlink($base_dump);
    print("\nCMD>>REMOVE $base_dump\n");
    return 0;
}
