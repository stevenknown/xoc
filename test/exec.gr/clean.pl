#!/usr/bin/perl -w
use Cwd;
use File::Find;
use File::Copy;
use File::Path;
#use Data::Dump;
###########################################
## Remove tmp and create com/  dex/ opt/ directory.
# clean tmp directory
my $curdir = getcwd;

main();

sub main
{
    removeFile();
    removeDir();
    print "\nCLEAN FINISH\n";
} 


sub removeFile
{
    my @f;
    push(@f, findRecursively($curdir, 'LOGLOG$'));
    push(@f, findRecursively($curdir, 'succ$'));
    push(@f, findRecursively($curdir, 'fail$'));
    push(@f, findRecursively($curdir, '.*\.asm$'));
    push(@f, findRecursively($curdir, '.*\.tmp$'));
    push(@f, findRecursively($curdir, '.*\.log$'));
    push(@f, findRecursively($curdir, '.*\.out$'));
    push(@f, findRecursively($curdir, '.*\.swp$'));
    push(@f, findRecursively($curdir, '.*\.swo$'));
    push(@f, findRecursively($curdir, '.*\.vcg$'));
    push(@f, findRecursively($curdir, '.*\.dot$'));
    push(@f, findRecursively($curdir, '.*\.B$'));
    push(@f, findRecursively($curdir, '.*\.i$'));
    push(@f, findRecursively($curdir, '.*\.s$'));
    push(@f, findRecursively($curdir, '.*\.s$'));
    push(@f, findRecursively($curdir, '.*\.t$'));
    push(@f, findRecursively($curdir, '.*\.spin$'));
    push(@f, findRecursively($curdir, '.*\.o$'));
    push(@f, findRecursively($curdir, '.*xocc_output\.txt$'));
    push(@f, findRecursively($curdir, '.*xocc_dump\.txt$'));

    #push(@f, `find -maxdepth 1 -name "LOGLOG"`);
    #push(@f, `find -maxdepth 1 -name "succ"`);
    #push(@f, `find -maxdepth 1 -name "fail"`);
    #push(@f, `find -maxdepth 1 -name "*\.asm"`);
    #push(@f, `find -maxdepth 1 -name "*\.tmp"`);
    #push(@f, `find -maxdepth 1 -name "*\.log"`);
    #push(@f, `find -maxdepth 1 -name "*\.vcg"`);
    #push(@f, `find -maxdepth 1 -name "*\.B"`);
    #push(@f, `find -maxdepth 1 -name "*\.i"`);
    #push(@f, `find -maxdepth 1 -name "*\.s"`);
    #push(@f, `find -maxdepth 1 -name "*\*.out"`);
    #push(@f, `find -maxdepth 1 -name "*\.t"`);
    #push(@f, `find -maxdepth 1 -name "*\.spin"`);
    #push(@f, `find -maxdepth 1 -name "*\.o"`);
    #push(@f, `find -maxdepth 1 -name "*\.gr"`);
    #push(@f, `find -maxdepth 1 -name "*xocc_output.txt"`);
    foreach (@f) {
    	chomp;
    	print "\nunlink:$_";
    	my $retval = unlink($_);
        if ($retval != 1) {
            print("\nFailed:retval=$retval");
    	}
    }
}


sub removeDir
{
    my @f2 = (
    	"$curdir\\tmp",
    );
    foreach (@f2) {
    	chomp;
    	print "\nrmtree:$_";
    	my $retval = rmtree($_);
        if ($retval != 0) {
            print("\nFailed:retval=$retval");
    	}
    }
}


my @filelist = ();
my $suffix = "";
sub findRecursively {
    my $dir = $_[0];
    $suffix = $_[1];
    @filelist = ();
    &find(\&findCore, $dir);
    return @filelist;
}
sub findCore {
    my $pattern = $suffix;
    push(@filelist, $File::Find::name) if ($_ =~ m/$pattern/);
}

