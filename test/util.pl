#!/usr/bin/perl -w
use strict;
use Cwd;
use File::Find;
use File::Copy;
use File::Path;
use File::Compare;
#use File::Copy::Recursive; #ActivePerl report error: Can't locate File/Copy/Recursive.pm in @INC
#use File::Copy::Recursive qw(fcopy rcopy dircopy fmove rmove dirmove);
use File::Copy::Recursive qw(fcopy dircopy);
use Data::Dump qw(dump);

# These functions are exported.
our @EXPORT_OK = qw(
    abort
    abortex
    copyDir
    copyFile
    computeDirFromFilePath
    computeExeName
    computeSourceFileNameByExeName
    compileGR
    computeRelatedPathToXocRootDir
    computeAbsolutePathToXocRootDir
    compareDumpFile
    clean
    extractPostfixName
    getCurDir
    getDumpFilePath
    getBaseResultDumpFilePath
    getOutputFilePath
    getBaseOutputFilePath
    getFileNameFromPath
    generateGR
    findCurrent
    findFfileCurrent
    findRecursively
    findAnyFileRecursively
    findFileRecursively
    findDirRecursively
    is_exist
    isExistInEnvPath
    isInEnv
    isCPPPostfixName
    moveToPassed
    invokeSimulator
    peelPostfixName
    peelFileName
    pauseAndInput
    removeFile
    removeDir
    runSimulator
    runHostExe
    runBaseccToolChainToComputeBaseResult
    runBaseCC
    runCPP
    runXOCC
    tryCreateDir
    tryShowOutput
    tryRunCpp
    systemx);

# These variables are exported.
our $g_basecc = "";
our $g_basecc_cflags = "";
our $g_xocc = "";
our $g_cpp = "cpp";
our $g_cflags = "-O0";
our $g_simulator = "";
our $g_simulator_flags = "";
our $g_as = "";
our $g_ld = "";
our $g_ldflags = "";
our $g_is_quit_early = 1; #finish test if error occurred.
our $g_target; #indicate target machine.
our $g_is_create_base_result = 0;
our $g_is_move_passed_case = 0;
our $g_is_test_gr = 0;
our $g_is_show_output = 0;
our $g_is_invoke_cpp = 1;
our $g_is_invoke_assembler = 1;
our $g_is_invoke_linker = 1;
our $g_is_invoke_simulator = 1;
#1 if user intend to run testcase recursively, otherwise only
#test current directory.
our $g_is_recur = 0;
our $g_osname = $^O;
our $g_xoc_root_path = "";
our $g_single_testcase = ""; #record the single testcase
our $g_single_directory = ""; #record the single directory
our $g_config_file_path = "";
our $g_is_compare_dump = 0;
our $g_is_compare_result = 0;
our $g_is_nocg = 0;
our $g_is_basedumpfile_must_exist = 0;
our $g_is_baseresultfile_must_exist = 0;
our $g_error_count = 0;
our $g_fail = -1;
our $g_succ = 0;
our $g_true = 1;
our $g_false = 0;

#Local variables that are used in current file scope.
my $g_override_simulator = "";
my $g_override_xocc_path = "";
my $g_override_cpp_path = "";
my $g_override_as_path = "";
my $g_override_ld_path = "";
my $g_override_basecc_path = "";
my $g_override_xocc_flag = "";
my $g_override_as_flag = "";
my $g_override_ld_flag = "";
my $g_override_basecc_flag = "";

# These functions are imported.
## selectTargetFromConfigFile

#Find file in current directory.
sub findFileCurrent {
    my $dir = $_[0];
    my $filename = $_[1];
    my @filelist = ();
    # create a list of all files in
    # the current directory
    opendir(DIR, $dir);
    #@srclist = grep(/\.c$/, readdir(DIR));
    while (my $file = readdir(DIR)) {
        if ($file =~ m/$filename$/) {} #compare the filename.
        else { next; }
        push(@filelist, $file);
    }
    closedir(DIR);
    #print @filelist;
    return @filelist;
}

#Find file with given suffix in current directory.
sub findCurrent {
    #my @filelist = `find -maxdepth 1 -name "*.c"`; #ONLY AVAILABLE ON LINUX
    my $dir = $_[0];
    my $suffix = $_[1];
    my @filelist = ();
    # create a list of all files in
    # the current directory
    opendir(DIR, $dir);
    #@srclist = grep(/\.c$/, readdir(DIR));
    while (my $file = readdir(DIR)) {
        if ($file =~ m/\.$suffix$/) {} #compare the suffix of file.
        else { next; }
        push(@filelist, $dir."/".$file);
    }
    closedir(DIR);
    #print @filelist;
    return @filelist;
}

#dir: the directory that you are going to find.
#suffix: the file suffix that you are going to find.
my $suffix; #used inside this file scope.
my @g_filelist = (); #used inside this file scope.
sub findRecursively {
    my $dir = $_[0];
    $suffix = $_[1];
    @g_filelist = ();
    &find(\&findCore, $dir);
    return @g_filelist;
}

sub findAnyFileRecursively {
    my $dir = $_[0];
    @g_filelist = ();
    &find(\&findCore3, $dir);
    return @g_filelist;
}

sub findCore {
    push(@g_filelist, $File::Find::name) if ($_ =~ m/\.$suffix$/);
}

#Find file in current directory and sub-directory.
my $g_testcase; #used inside this file scope.
sub findFileRecursively {
    my $dir = $_[0];
    $g_testcase = $_[1];
    @g_filelist = ();
    &find(\&findCore2, $dir);
    return @g_filelist;
}

sub findCore2 {
    push(@g_filelist, $File::Find::name) if ($_ =~ m/$g_testcase$/);
}

sub findCore3 {
    push(@g_filelist, $File::Find::name);
}

# Remove one line from start of the given file.
sub removeLine
{
    my $cmdline;
    my $retval;
    my $xoc_root_path = $_[0];
    my $inputfile = $_[1];
    my $num_of_line = $_[2];
    my $removeline_script_path = $xoc_root_path."test/remove_line.py";
    $cmdline = "$removeline_script_path $inputfile $num_of_line";
    print("\nCMD>>", $cmdline, "\n");
    $retval = systemx($cmdline);
    if ($retval != 0) {
        return $g_fail;
    }
    return $g_succ;
}

#Use basecc to compile, assembly and link, and running.
sub runBaseccToolChainToComputeBaseResult
{
    my $file = $_[0]; #fullpath of source file, NOT the executable file.

    #redirect stdout to the file.
    #Note the redirected file is different to dump
    #file that generated by compiler. The redirected
    #file is used to record the OUTPUT of compiler
    #that print to console.
    my $base_outputfile = $_[1];

    my $curdir = $_[2]; #the directory where current perl invoked.
    my $exefile = computeExeName($file); #the executable binary file name.

    #Invoke compiler and toolchains to compile, assembly, link to generete
    #executable file.
    my $cmdline = "$g_basecc $file $g_basecc_cflags $g_override_basecc_flag -o $exefile";
    print("\nCMD>>", $cmdline, "\n");
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE $g_ld FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail; #No need execute the following code.
    }

    #The new result file.
    my $rundir = computeDirFromFilePath($file);
    return invokeSimulator($file, $curdir, $base_outputfile, $rundir);
}

sub runArmExe
{
    my $cmdline;
    my $retval;
    my $binfile = $_[0];
    my $outputfile = $_[1];
    $cmdline = "./$binfile";
    print("\nCMD>>", $cmdline, " >& ", $outputfile, "\n");

    open (my $OLDVALUE, '>&', STDOUT); #save stdout to oldvalue
    open (STDOUT, '>>', $outputfile); #redirect stdout to file
    $retval = systemx($cmdline);
    open (STDOUT, '>&', $OLDVALUE); #reload stdout from oldvalue

    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE $cmdline FAILED!! RES:$retval\n";
        #die($retval);
        return $g_fail;
    }

    return $g_succ;
}

sub runHostExe
{
    my $cmdline;
    my $retval;
    my $binfile = $_[0];
    my $outputfile = $_[1];
    $cmdline = "./$binfile";
    print("\nCMD>>", $cmdline, " >& ", $outputfile, "\n");

    open (my $OLDVALUE, '>&', STDOUT); #save stdout to oldvalue
    open (STDOUT, '>>', $outputfile); #redirect stdout to file
    $retval = systemx($cmdline);
    open (STDOUT, '>&', $OLDVALUE); #reload stdout from oldvalue

    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE $cmdline FAILED!! RES:$retval\n";
        #die($retval);
        return $g_fail;
    }

    return $g_succ;
}

sub runSimulator
{
    my $exefile = $_[0]; #the binary file to execute
    my $outputfile = $_[1]; #redirect stdout to the file
    my $cmdline;
    my $retval;

    if (!-e $exefile) {
        #Not equal
        print "\n$exefile DOES NOT EXIST!\n";
        #abortex();
        return $g_fail;
    }

    $cmdline = "$g_simulator $g_simulator_flags $exefile";
    print("\nCMD>>", $cmdline,"\n");
    print("\n>>OUTPUT FILE:$outputfile", "\n");

    open(my $OLDVALUE, '>&', STDOUT); #save stdout to oldvalue
    open(STDOUT, '>>', $outputfile); #redirect stdout to file
    $retval = systemx($cmdline);
    open(STDOUT, '>&', $OLDVALUE); #reload stdout from oldvalue
    if ($retval != 0) {
        #Base compiler might also failed, thus we just compare
        #the result of base compiler even if it is failed.
        #abortex("\nEXECUTE $cmdline FAILED!! RES:$retval\n");
        print("\nEXECUTE $cmdline FAILED!! RES:$retval\n");
        return $g_fail;
    }

    return $g_succ;
}

#Generate GR from C file.
sub generateGR
{
    my $cmdline;
    my $retval;
    my $fullpath = $_;
    my $grname = $fullpath.".gr";
    my $asmname = $grname.".asm";

    #generate GR
    unlink($grname);
    $cmdline = "$g_xocc $g_cflags $fullpath -dumpgr";
    print("\nCMD>>", $cmdline, "\n");
    $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE XOCC FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }

    if (!-e $grname) {
        #Not equal
        print "\n$grname DOES NOT EXIST!\n";
        abortex();
        return $g_fail;
    }

    return $g_succ;
}

#compile GR to asm
sub compileGR
{
    my $cmdline;
    my $retval;
    my $fullpath = $_;
    my $grname = $fullpath.".gr";
    my $asmname = $grname.".asm";
    if (!-e $grname) {
        #Not equal
        print "\n$grname DOES NOT EXIST!\n";
        abortex();
        return $g_fail;
    }

    #compile GR to asm
    $cmdline = "$g_xocc $g_cflags $grname -o $asmname";
    print("\nCMD>>", $cmdline, "\n");
    $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE XOCC FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }
    return $g_succ;
}

sub runAssembler
{
    my $asmname = $_[0];
    my $outputfilename = $_[1];
    my $cmdline = "$g_as $asmname -o $outputfilename";
    print("\nCMD>>", $cmdline, "\n");
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE $g_as FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }
    return $g_succ;
}

sub runLinker
{
    my $output = $_[0];
    my $inputasmname = $_[1];
    my $cmdline = "$g_ld $inputasmname -o $output $g_ldflags";

    print("\nCMD>>", $cmdline, "\n");
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE $g_ld FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }
    return $g_succ;
}

#Use base-cc to compile, assembly and link.
sub runBaseCC
{
    my $file = $_[0];
    my $outputfilename = $_[1];
    my $cmdline = "$g_basecc $file $g_basecc_cflags $g_override_basecc_flag -o $outputfilename";
    print("\nCMD>>", $cmdline, "\n");
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nEXECUTE $g_ld FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail;
    }
    return $g_succ;
}

#Use cpp to preprocess C file.
#Output preprocessed file that postfix with *.i.
sub runCPP
{
    my $cmdline;
    my $src_fullpath = $_[0];
    my $preprocessed_name = $src_fullpath.".i";
    my $exename = computeExeName($src_fullpath);
    my $objname = $src_fullpath.".o";

    #preprcessing
    unlink($preprocessed_name);
    $cmdline = "$g_cpp $src_fullpath -o $preprocessed_name -C -E -P -Iinc";
    print("\nCMD>>", $cmdline, "\n");
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nFAILED! -- EXECUTE CPP FAILED!! RES:$retval\n";
        abortex($retval);
        return "";
    }
    return $preprocessed_name;
}

#Use xocc to compile, assembly and link.
sub runXOCC
{
    my ($src_fullpath,
        $is_invoke_assembler,
        $is_invoke_linker,
        $is_input_gr,
        $ret_exename) = @_;
    my $postname = extractPostfixName($src_fullpath);

    #Record normalized src file path. e.g: remove the postfix 'i'.
    my $src_fullpath_normed = $src_fullpath;
    if (isCPPPostfixName($postname)) {
        $src_fullpath_normed = peelPostfixName($src_fullpath);
    }
    my $asmname = $src_fullpath_normed.".asm";
    my $exename = computeExeName($src_fullpath_normed);
    my $objname = $src_fullpath_normed.".o";
    my $cmdline;
    ${$ret_exename} = $exename;
    if (!is_exist($g_xocc)) {
        abortex("\n$g_xocc DOES NOT EXIST\n");
        return $g_fail; #No need execute the following code.
    }
    if (!is_exist($src_fullpath)) {
        abortex("\n$src_fullpath DOES NOT EXIST\n");
        return $g_fail; #No need execute the following code.
    }

    #compile
    unlink($asmname);
    $cmdline = "$g_xocc $g_cflags -o $asmname $g_override_xocc_flag";
    $cmdline = "$cmdline $src_fullpath";
    print("\nCMD>>$cmdline\n");
    my $retval = systemx($cmdline);
    if ($retval != 0) {
        print("\nCMD>>", $cmdline, "\n");
        print "\nFAILED! -- EXECUTE XOCC FAILED!! RES:$retval\n";
        abortex($retval);
        return $g_fail; #No need execute the following code.
    }
    if ($is_invoke_assembler) {
        if (runAssembler($asmname, $objname) == $g_fail) {
            return $g_fail; #No need execute the following code.
        }
    }
    if ($is_invoke_linker) {
        return runLinker($exename, $objname);
    }
    return $g_succ;
}

sub abortex
{
    $g_error_count += 1;
    my $msg = $_[0];
    if ($msg) {
        print "\n$msg\n";
    }
    if (!$g_is_quit_early) {
        return;
    }
    exit(1); #exit value set to 1 will affect the env-value.
}

sub abort
{
    my $msg = $_[0];
    if ($msg) {
        print "\n$msg\n";
    }
    exit(1); #exit value set to 1 will affect the env-value.
}

#Compute the executable binary file name.
sub computeExeName
{
    my $fullpath = $_[0];
    return $fullpath.".out";
}

sub computeSourceFileNameByExeName
{
    my $filepath = $_[0];
    my @segs = split(/\./, $filepath);
    my $seg;
    my $n = 0; #the number of seg
    while (defined($seg = $segs[$n])) { $n++; }

    #Drop the last seg off.
    $n--;
    my $path = "";
    my $i = 0;
    while (defined($seg = $segs[$i])) {
        if ($i == 0) {
            $path = $seg;
        } else {
            $path = "$path.$seg";
        }
        $i++;
        if ($i >= $n) {
          last;
        }
    }
    return $path;
}

#The function will pause the Perl interpreter to wait user input, and return
#the online input.
sub pauseAndInput
{
    print "ENTER ANY KEY TO CONTINUE:";
    my $anykey = <STDIN>;
    return $anykey;
}

#Extract path prefix from 'fullpath'.
#e.g: $fullpath is /a/b/c.cpp, then $path is /a/b/
#Note if $fullpath is c.cpp, return empty.
sub peelFileName
{
    my $fullpath = $_[0];
    my $path = substr($fullpath, 0, rindex($fullpath, "/") + 1);
    return $path;
}


#Given file path, extract the filename, drop the prefix path.
#e.g: given a/b.out, return b.out
sub getFileNameFromPath
{
    my $filepath = $_[0];
    my @segs = split(/\//, $filepath);
    my $seg;
    my $n = 0; #the number of seg
    my $prev_seg = $seg;
    while (defined($seg = $segs[$n])) {
        $n++;
        $prev_seg = $seg;
    }
    return $prev_seg;
}

sub isCPPPostfixName
{
    my $postname = $_[0];
    if ($postname eq "i") {
        return $g_true;
    }
    return $g_false;
}

#Given file path, extract the last postfix name.
#e.g: given a/b.out, return "out"
sub extractPostfixName
{
    my $filepath = $_[0];
    my @segs = split(/\./, $filepath);
    my $seg;
    my $n = 0; #the number of seg
    while (defined($seg = $segs[$n])) { $n++; }
    if ($n <= 0) { return ""; }
    return $segs[$n-1];
}

#Given file path, drop the last postfix name.
#e.g: given a/b.out, return a/b
sub peelPostfixName
{
    my $filepath = $_[0];
    my @segs = split(/\./, $filepath);
    my $seg;
    my $n = 0; #the number of seg
    while (defined($seg = $segs[$n])) { $n++; }

    #Drop the last seg off.
    $n--;
    my $path = "";
    my $i = 0;
    while (defined($seg = $segs[$i])) {
        if ($i == 0) {
            $path = $seg;
        } else {
            $path = "$path.$seg";
        }
        $i++;
        if ($i >= $n) {
          last;
        }
    }
    return $path;
}

#First param must be file path, not the directory.
#e.g: given /a/b/c.txt, return /a/b/
sub computeDirFromFilePath
{
    my $filepath = $_[0];
    my @segs = split(/\//, $filepath);
    my $seg;
    my $n = 0; #directory level
    my $dir;
    while (defined($seg = $segs[$n])) { $n++; }

    #Drop the last seg off.
    $n--;
    $dir = "";
    my $i = 0;
    while (defined($seg = $segs[$i])) {
        $dir = "$dir/$seg";
        $i++;
        if ($i >= $n) {
          last;
        }
    }
    return $dir;
}

sub readConfigFile
{
    if (!-e $g_config_file_path) {
        return;
    }
    require $g_config_file_path;
}

sub prolog
{
    computeRelatedPathToXocRootDir();
    #computeAbsolutePathToXocRootDir();
    parseCmdLine();
    readConfigFile();
    selectTarget();
    overrideOptions();
    printEnvVar();
    checkEnvValid();
}

sub overrideOptions
{
    if ($g_is_nocg) {
        $g_cflags = $g_cflags." -no-cg ";
    }
    if ($g_override_cpp_path ne "") {
        $g_cpp = $g_override_cpp_path;
    }
    if ($g_override_xocc_path ne "") {
        $g_xocc = $g_override_xocc_path;
    }
    if ($g_override_as_path ne "") {
        $g_as = $g_override_as_path;
    }
    if ($g_override_ld_path ne "") {
        $g_ld = $g_override_ld_path;
    }
    if ($g_override_basecc_path ne "") {
        $g_basecc = $g_override_basecc_path;
    }
    if ($g_override_simulator ne "") {
        $g_simulator = $g_override_simulator;
        $g_simulator_flags = "";
    }
}

sub checkEnvValid
{
    if (!$g_xocc) {
        usage();
        print "\nNOT DESIGNATE xocc.exe FOR $g_target!\n";
        abort();
    }
    if (!is_exist($g_xocc)) {
        abortex("\n$g_xocc DOES NOT EXIST\n");
        return $g_fail;
    }
    my @filelist = ();
    push(@filelist, $g_xocc);

    #NOTE: tools may be in environment path.
    if ($g_is_invoke_cpp) {
        push(@filelist, $g_cpp);
    }
    if ($g_is_invoke_assembler) {
        push(@filelist, $g_as);
    }
    if ($g_is_invoke_linker) {
        push(@filelist, $g_ld);
    }
    if ($g_is_invoke_simulator) {
        push(@filelist, $g_simulator);
    }
    foreach (@filelist) {
        if (!is_exist($_) && !isExistInEnvPath($_)) {
            print "\n$_ DOES NOT EXIST!\n";
            if ($g_is_quit_early) {
                abort();
            }
        }
    }
}

sub isInEnv
{
    my $cmd = $_[0];
    my $path = $ENV{'PATH'} || '';
    #Check whether software is in the environment.
    if ($path =~ m/(?=.*\/?$cmd)/) {
        return $g_true;
    }
    return $g_false;
}

sub isExistInEnvPath
{
    my $software = $_[0];
    my @path_dirs = split /:/, $ENV{'PATH'};
    foreach my $dir (@path_dirs) {
        #my $exec_path = File::Spec->catfile($dir, $software);
        my $exec_path = "$dir/$software";
        if (is_exist($exec_path)) {
            return $g_true;
        }
    }
    return $g_false;
}

sub is_exist
{
    my $filepath = $_[0];
    if (!-e $filepath) {
        return $g_false;
    }
    return $g_true;
}

sub parseCmdLine
{
    #Skip ARGV[0], it should describe target machine.
    for (my $i = 0; $ARGV[$i]; $i++) {
        my $curarg = $ARGV[$i];
        if ($ARGV[$i] eq "CreateBaseResult") {
            $g_is_create_base_result = 1;
            next;
        } elsif ($ARGV[$i] eq "CompareNewResult") {
            #nothing to do
        } elsif ($ARGV[$i] eq "MovePassed") {
            $g_is_move_passed_case = 1;
        } elsif ($ARGV[$i] eq "TestGr") {
            $g_is_test_gr = 1;
        } elsif ($ARGV[$i] eq "ShowOutput") {
            $g_is_show_output = 1;
        } elsif ($ARGV[$i] eq "ShowTime") {
            $g_cflags .= " -time";
        } elsif ($ARGV[$i] eq "Recur") {
            $g_is_recur = 1;
        } elsif ($ARGV[$i] eq "NotQuitEarly") {
            $g_is_quit_early = 0;
        } elsif ($ARGV[$i] eq "Targ") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_target = $ARGV[$i];
        } elsif ($ARGV[$i] eq "Case") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_single_testcase = $ARGV[$i];
        } elsif ($ARGV[$i] eq "Dir") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_single_directory = $ARGV[$i];
        } elsif ($ARGV[$i] eq "NoCG") {
            $g_is_invoke_assembler = 0;
            $g_is_invoke_linker = 0;
            $g_is_invoke_simulator = 0;
            $g_is_nocg = 1;
        } elsif ($ARGV[$i] eq "NoAsm") {
            $g_is_invoke_assembler = 0;
            $g_is_invoke_linker = 0;
            $g_is_invoke_simulator = 0;
        } elsif ($ARGV[$i] eq "NoLink") {
            $g_is_invoke_linker = 0;
            $g_is_invoke_simulator = 0;
        } elsif ($ARGV[$i] eq "NoRun") {
            $g_is_invoke_simulator = 0;
        } elsif ($ARGV[$i] eq "CompareDump") {
            #Compare dump file always.
            $g_is_compare_dump = 1;
            $g_is_basedumpfile_must_exist = 1;
        } elsif ($ARGV[$i] eq "CompareDumpIfExist") {
            $g_is_compare_dump = 1;
            $g_is_basedumpfile_must_exist = 0;
        } elsif ($ARGV[$i] eq "CompareResult") {
            #Compare result file always.
            $g_is_compare_result = 1;
            $g_is_baseresultfile_must_exist = 1;
         } elsif ($ARGV[$i] eq "CompareResultIfExist") {
            $g_is_compare_result = 1;
            $g_is_baseresultfile_must_exist = 0;
        } elsif ($ARGV[$i] eq "XoccPath") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_override_xocc_path = $ARGV[$i];
        } elsif ($ARGV[$i] eq "AsPath") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_override_as_path = $ARGV[$i];
        } elsif ($ARGV[$i] eq "LinkerPath") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_override_ld_path = $ARGV[$i];
        } elsif ($ARGV[$i] eq "CppPath") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_override_cpp_path = $ARGV[$i];
        } elsif ($ARGV[$i] eq "BaseccPath") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_override_basecc_path = $ARGV[$i];
        } elsif ($ARGV[$i] eq "ConfigFilePath") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                usage();
                abort();
            }
            $g_config_file_path = $ARGV[$i];
        } elsif ($ARGV[$i] eq "Simulator") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                #Note ""(empty string) and undef are both in the case.
                usage();
                abortex();
            }
            $g_override_simulator = $ARGV[$i];
        } elsif ($ARGV[$i] eq "BaseccFlag") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                #Note ""(empty string) and undef are both in the case.
                usage();
                abortex();
            }
            $g_override_basecc_flag = $ARGV[$i];
        } elsif ($ARGV[$i] eq "XoccFlag") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if ($ARGV[$i] eq "") {
                print "\nMISS ARG: $curarg\n";
                #Nothing to do.
            } elsif (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                #Note ""(empty string) and UNDEF are both in the case.
                usage();
                abortex();
            }

            #Add a blank to make sure there is a separator at least.
            $g_override_xocc_flag = $ARGV[$i]." ";
        } elsif ($ARGV[$i] eq "LinkerFlag") {
            $i++;
            if (!$ARGV[$i] or ($ARGV[$i] ne "=")) {
                print "\nUNKNOWN ARG: $ARGV[$i]\n";
                usage();
                abort();
            }
            $i++;
            if (!$ARGV[$i]) {
                print "\nMISS ARG: $curarg\n";
                #Note ""(empty string) and undef are both in the case.
                usage();
                abortex();
            }
            $g_ldflags = $ARGV[$i];
        } else {
            abort("UNSUPPORT COMMAND LINE:'$ARGV[$i]'");
        }
    }
}

sub printEnvVar
{
    print "\n==---- ENVIRONMENT VARIABLES ----==\n";
    print "\ng_target = $g_target";
    print "\ng_osname = $g_osname";
    print "\ng_simulator = $g_simulator";
    print "\ng_simulator_flags = $g_simulator_flags";
    print "\ng_basecc = $g_basecc";
    print "\ng_xocc = $g_xocc";
    print "\ng_cflags = $g_cflags";
    print "\ng_cpp = $g_cpp";
    print "\ng_as = $g_as";
    print "\ng_ld = $g_ld";
    print "\ng_ldflags = $g_ldflags";
    print "\ng_is_test_gr = $g_is_test_gr";
    print "\ng_is_show_output = $g_is_show_output";
    print "\ng_xoc_root_path = $g_xoc_root_path";
    print "\ng_is_invoke_cpp = $g_is_invoke_cpp";
    print "\ng_is_invoke_assembler = $g_is_invoke_assembler";
    print "\ng_is_invoke_linker = $g_is_invoke_linker";
    print "\ng_is_invoke_simulator = $g_is_invoke_simulator";
    if ($g_single_testcase ne "") {
        print "\ng_single_testcase = $g_single_testcase";
    }
    if ($g_single_directory ne "") {
        print "\ng_single_directory = $g_single_directory";
    }
    if ($g_override_cpp_path ne "") {
        print "\ng_override_cpp_path = $g_override_cpp_path";
    }
    if ($g_override_xocc_path ne "") {
        print "\ng_override_xocc_path = $g_override_xocc_path";
    }
    if ($g_override_xocc_flag ne "") {
        print "\ng_override_xocc_flag = $g_override_xocc_flag";
    }
    if ($g_override_as_path ne "") {
        print "\ng_override_as_path = $g_override_as_path";
    }
    if ($g_override_as_flag ne "") {
        print "\ng_override_as_flag = $g_override_as_flag";
    }
    if ($g_override_ld_path ne "") {
        print "\ng_override_ld_path = $g_override_ld_path";
    }
    if ($g_override_ld_flag ne "") {
        print "\ng_override_ld_flag = $g_override_ld_flag";
    }
    if ($g_override_basecc_flag ne "") {
        print "\ng_override_basecc_flag = $g_override_basecc_flag";
    }
    print "\n";
}


sub selectTarget
{
    if (!$g_target) {
        print "\nMISS TARG\n";
        usage();
        print "\nNOT SPECIFY A TARGET!\n";
        abort();
    }
    if ($g_osname eq 'MSWin32') {
        if ($g_target eq "arm") {
            $g_xocc = "$g_xoc_root_path/src/arm/xocc.exe";
            $g_basecc = "arm-linux-gnueabi-gcc";
            $g_as = "arm-linux-gnueabi-as";
            $g_ld = "arm-linux-gnueabi-gcc";
            $g_basecc_cflags = "-std=c99 -O0";
            $g_simulator = "qemu-arm";
            $g_simulator_flags = "-L /usr/arm-linux-gnueabihf";
        } elsif ($g_target eq "armhf") {
            $g_xocc = "$g_xoc_root_path/src/arm/xocc.exe";
            $g_basecc = "arm-linux-gnueabihf-gcc";
            $g_as = "arm-linux-gnueabihf-as";
            $g_ld = "arm-linux-gnueabihf-gcc";
            $g_basecc_cflags = "-std=c99 -O0";
            $g_simulator = "qemu-arm";
            $g_simulator_flags = "-L /usr/arm-linux-gnueabihf";
        } elsif ($g_target eq "pac") {
            $g_xocc = "$g_xoc_root_path/src/pac/xocc.exe";
            $g_basecc = "pacc.exe";
            $g_as = "pac-linux-gnueabi-as";
            $g_ld = "pac-linux-gnueabi-gcc";
            $g_basecc_cflags = "-std=c99 -O0";
            $g_simulator = "pacsim";
            $g_simulator_flags = "";
        } elsif ($g_target eq "x86") {
            $g_xocc = "$g_xoc_root_path/src/x86/xocc.exe";
            $g_basecc = "gcc";
            $g_as = "as";
            $g_ld = "gcc";
            $g_basecc_cflags = "-std=c99 -O0";
            $g_simulator = "qemu-x86";
            $g_simulator_flags = "";
        } elsif ($g_config_file_path ne "" &&
                 selectTargetFromConfigFile() == 1) {
            ; ## Has already selected target info from config file.
        } else {
            print "\nUNSUPPORT TARGET! PLEASE IMPORT TARGET FROM CONFIG FILE\n";
            abort();
        }
    } else {
        if ($g_target eq "arm") {
            $g_xocc = "$g_xoc_root_path/src/arm/xocc.exe";
            $g_basecc = "arm-linux-gnueabi-gcc";
            $g_as = "arm-linux-gnueabi-as";
            $g_ld = "arm-linux-gnueabi-gcc";
            $g_simulator = "qemu-arm";
            $g_simulator_flags = "-L /usr/arm-linux-gnueabihf";
        } elsif ($g_target eq "armhf") {
            $g_xocc = "$g_xoc_root_path/src/arm/xocc.exe";
            $g_basecc = "arm-linux-gnueabihf-gcc";
            $g_as = "arm-linux-gnueabihf-as";
            $g_ld = "arm-linux-gnueabihf-gcc";
            $g_simulator = "qemu-arm";
            $g_simulator_flags = "-L /usr/arm-linux-gnueabihf";
        } elsif ($g_target eq "pac") {
           $g_xocc = "$g_xoc_root_path/src/pac/xocc.exe";
           $g_basecc = "pacc";
           $g_as = "pac-linux-gnueabihf-as";
           $g_ld = "pac-linux-gnueabihf-gcc";
           $g_basecc_cflags = "-std=c99 -O0";
           $g_simulator = "pacsim";
           $g_simulator_flags = "";
        } elsif ($g_target eq "x86") {
            $g_xocc = "$g_xoc_root_path/src/x86/xocc.exe";
        } elsif ($g_config_file_path ne "" &&
                 selectTargetFromConfigFile() == 1) {
            ; ## Has already selected target info from config file.
        } else {
            print "\nUNSUPPORT TARGET! PLEASE IMPORT TARGET FROM CONFIG FILE\n";
            abort();
        }
    }
}


sub usage
{
    print "\n==-- NOTE: YOU HAVE TO ENSURE THE FILE NAME OF TESTCASE IS UNQIUE. --==",
          "\n\nUSAGE: ./run.pl Targ = x64|arm|armhf [Option List ...]",
          "\n       e.g: ./run.pl arm Case = hello.c XoccFlag = \"-O3\" NotQuitEarly",
          "\n\nOption List can be the following:",
          "\nMovePassed         move passed testcase to 'passed' directory",
          "\n                   NOTE: do not delete testcase in 'passed' directory",
          "\nCreateBaseResult   generate result if there is no one",
          "\n                   NOTE: deleting the exist one will regenerate base result",
          "\nTestGr             generate GR for related C file and test GR file",
          "\nShowTime           show compiling time for each compiler pass",
          "\nShowOutput         show output file content",
          "\nRecur              find testcases and perform testing recursively",
          "\nNotQuitEarly       perform test always even if there is failure",
          "\nCase = ...         run single case, e.g: Case = your_test_file_name",
          "\nDir = ...          run cases under the given Directory, e.g: Dir = your_specific_directory",
          "\nCompareDump        only compile and compare the dump file",
          "\nCompareDumpIfExist only compile and compare the dump file if the base-dump-file exist",
          "\nBaseccPath = ...   refer to base cc.exe path, e.g: BaseccPath = gcc",
          "\nBaseccFlag = ...   base cc.exe command line option, e.g: BaseccFlag = \"-std=c++0x\"",
          "\nCppPath = ...      refer to cpp.exe path, e.g: CppPath = your_cpp_file_path",
          "\nXoccPath = ...     refer to xocc.exe path, e.g: XoccPath = your_xocc_file_path",
          "\nXoccFlag = ...     xocc.exe command line option, e.g: XoccFlag = \"-O3 -time\"",
          "\nConfigFilePath = ...",
          "\n                   refer to imported config file path if exist",
          "\nNoCG               do not run Code Generation of xocc",
          "\nNoAsm              do not generate assembly and linking",
          "\nNoLink             do not perform linking",
          "\nNoRun              do not run execuable binary file",
          "\nCompareResultIfExist",
          "\n                   compile result-dump-file of execuable binary file if base-result-dump-file exist",
          "\nNotQuitEarly       do not quit even if any errors occur",
          "\n";
}

sub clean
{
    my $cmdline = "find -name \"*.asm\" | xargs rm -f";
    systemx($cmdline);
    $cmdline = "rm -rf log";
    systemx($cmdline);
}

#This function infer absolute root path for XOC project directory.
sub computeAbsolutePathToXocRootDir
{
    my $curdir = getcwd;
    my @segs = split(/\//, $curdir);
    my $seg;

    #Compute absoluately path.
    my $i = 0; #loop index
    $g_xoc_root_path = "/";
    while (defined($seg = $segs[$i])) {
        if ($seg eq "test") {
            last;
        } elsif ($seg ne "") {
            $g_xoc_root_path = $g_xoc_root_path.$seg."/";
        }
        $i++;
        if ($i > 30) {
            #too deep directory
            last;
        }
    }
}

#This function infer related root path for XOC project directory.
sub computeRelatedPathToXocRootDir
{
    my $curdir = getcwd;
    my @segs = split(/\//, $curdir);
    my $seg;

    #Compute related path.
    my $i = 0; #loop index
    while (defined($seg = $segs[$i])) { $i++; }
    $i--;
    $g_xoc_root_path = "../";
    while (defined($seg = $segs[$i])) {
        if ($seg ne "test") {
            $g_xoc_root_path = "$g_xoc_root_path../";
        } else {
            last;
        }
        $i--;
        if ($i > 30) {
            #too deep directory
            last;
        }
    }
}

#The function encapsulates runSimulator and do some preparatory works
#for running.
sub invokeSimulator
{
    my $fullpath = $_[0]; #fullpath of source file, NOT the executable file.
    my $curdir = $_[1]; #the directory where current perl invoked.

    #redirect stdout to the file.
    #Note the redirected file is different to dump
    #file that generated by compiler. The redirected
    #file is used to record the OUTPUT of compiler
    #that print to console.
    my $redirected_output = $_[2];

    my $rundir = $_[3]; #the directory where simulator should be run.

    #Remove the last redirected_output file.
    print("\nCMD>>unlink $redirected_output\n");
    unlink($redirected_output);

    #my $exefile = computeExeName($fullpath);
    my $exefile = $fullpath;
    
    #Some testcase need input file to run, the default location of
    #input file is same with testcase.
    if ($rundir ne $curdir) {
        chdir $rundir;
    }
    return runSimulator($exefile, $redirected_output);
}

sub generateGRandCompile
{
    my $fullpath = $_;
    my $grname = $fullpath.".gr";
    my $asmname = $grname.".asm";
    my $objname = $asmname.".o";

    generateGR($fullpath);
    compileGR($fullpath);
    if ($g_is_invoke_assembler) {
        runAssembler($asmname, $objname);
    }
    if ($g_is_invoke_linker) {
        my $exefile = computeExeName($fullpath);
        runLinker($exefile);
    }
}

#Extract LinkerFLAG from *.ldconf if exist and append it to g_ldflags.
sub extractAndSetLDflag
{
    #Record the configure file.
    my $configure_file_path = $_[0].".ldconf";
    if (!-e $configure_file_path) {
        return;
    }
    my $pattern = qr/^#/;
    # read file content
    open my $file, '<', $configure_file_path or
        abortex("FAILED! -- ERROR OPENNING FILE: $!\n");
    while (defined(my $line = <$file>)) {
        chomp $line;
        #Match the pattern with the content in each line of file
        if ($line =~ /$pattern/) {
           next;
        }
        $g_ldflags = $g_ldflags." ".$line;
    }
    close ($file);
}

#Extract CFLAG from *.conf if exist and append it to g_cflags.
sub extractAndSetCflag
{
    #Record the configure file.
    my $configure_file_path = $_[0].".conf";
    if (!-e $configure_file_path) {
        return;
    }
    my $pattern = qr/^#/;
    # read file content
    open my $file, '<', $configure_file_path or
        abortex("FAILED! -- ERROR OPENNING FILE: $!\n");
    while (defined(my $line = <$file>)) {
        chomp $line;
        #Match the pattern with the content in each line of file
        if ($line =~ /$pattern/) {
           next;
        }
        $g_cflags = $g_cflags." ".$line;
    }
    close ($file);
}

#This function compose and return new file path to output file.
sub getOutputFilePath
{
    my $fullpath = $_[0]; #path to src file.
    my $path = $fullpath.".xocc_output.txt";
    return $path;
}

#This function compose and return new file path to base output file.
sub getBaseOutputFilePath
{
    my $fullpath = $_[0]; #path to src file.
    my $path = $fullpath.".base_output.txt";
    return $path;
}

#Return current directory.
sub getCurDir
{
    my $curdir = getcwd;
    return $curdir;
}

#This function compose and return new file path to dump file.
sub getDumpFilePath
{
    my $fullpath = $_[0]; #path to src file.
    my $dumpfilepath = $fullpath.".xocc_dump.txt";
    return $dumpfilepath;
}

#This function compose and return new file path to base result dump file.
sub getBaseResultDumpFilePath
{
    my $fullpath = $_[0]; #path to src file.
    my $dumpfilepath = $fullpath.".base_dump.txt";
    return $dumpfilepath;
}

#This function compare the dump file and base file.
sub compareDumpFile
{
    my $fullpath = $_[0]; #path to src file.
    my $dump_file = $_[1]; #new dump file to be compared.
    my $is_basedumpfile_must_exist = $_[2];

    #Compare baseline dump and latest dump.
    #The baseline result file.
    my $base_dump_file = getBaseResultDumpFilePath($fullpath);
    if (!-e $base_dump_file) {
        if ($is_basedumpfile_must_exist) {
            #Baseline dump file does not exist.
            abortex("Base dump file '$base_dump_file' not exist.");
            return $g_fail; #No need execute the following code.
        } else {
            print "\nPASS! NOTE:BASE DUMP FILE '$base_dump_file' NOT EXIST.\n";
            return $g_succ; #No need execute the following code.
        }
    }

    print("\nCMD>>compare dump-file $base_dump_file $dump_file\n");
    if (compare($base_dump_file, $dump_file) == 0) {
        #New result is euqal to baseline result.
        #New result is correct.
        print "\nPASS!\n";
    } else {
        #Not equal
        #New result is incorrect!
        print "\nFAILED! -- COMPARE DUMP OF $fullpath FAILED! NOT EQUAL TO BASE DUMP!\n";
        abortex();
        return $g_fail; #No need execute the following code.
    }
    return $g_succ;
}


sub tryRunCpp
{
    my $fullpath = $_[0];
    my $fullpathaftercpp;
    if ($g_is_invoke_cpp == 1)  {
        $fullpathaftercpp = runCPP($fullpath);
    } else {
        $fullpathaftercpp = $fullpath;
    }
    return $fullpathaftercpp;
}


sub tryShowOutput
{
    my $output_path = $_[0]; #path to output files.
    if ($g_is_show_output != 1) { return; }
    open(my $fh, '<', $output_path) or do {
        warn "Error: Cannot open file '$output_path': $!\n";
        return 0;
    };
    # Read and Print file content.
    print("\n>>OUTPUT START:\n");
    while (my $line = <$fh>) {
        print $line;
    }
    print("\n>>OUTPUT END:\n");
    # Close file.
    close($fh) or warn "Warning: Could not close file '$output_path': $!\n";
}

sub tryCreateDir
{
    my $path = $_[0]; #path to directory.
    if (-e $path) { return $g_succ; }
    if (mkdir($path) != 1) {
        #mkdir return 0 on failure and 1 on success.
        abortex("CAN NOT CREATE DIRECTORY '$path'");
        return $g_fail;
    }
    return $g_succ;
}

sub moveToPassed
{
    my $fullpath = $_[0]; #path to src file.
    my $path = peelFileName($fullpath);
    if (!$path) {
        abortex("\nMOVE FAILED! PICK FILE PATH OF $fullpath FAILED! NEED COMPLETE PATH\n");
        return $g_fail;
    }
    my $passedpath = "$path\/passed\/";

    #Create passed directory.
    if (tryCreateDir($passedpath) != $g_succ) {
        return $g_fail;
    }

    #Move passed C/GR file to directory $fullpath/passed.
    #NOTE: Do NOT delete testcase file in 'passed' directory.
    print("\nCMD>>move $fullpath, $passedpath\n");
    move($fullpath, $passedpath) or abortex();
    return $g_succ;
}

sub systemx
{
    #Perl does not return multiplied exit values. So it returns a 16 bit
    #value, with the exit code in the higher 8 bits. It's often the same,
    #but not always.
    my $cmdline = $_[0];
    my $retval = system($cmdline);
    $retval = $retval/256;
    return $retval;
}

sub perlSyntax
{
    my $param0 = $_[0]; #pass scalar arg
    my @param1 = @{$_[1]}; #pass array arg
    perlSyntax($param0, \@param1);
}

sub removeFile
{
    my $filename = $_[0];
    if (!-e $filename) {
        print "\n$filename DOES NOT EXIST! NOTHING TO DO!\n";
        return $g_succ;
    }
    #unlink($filename) or abortex();
    my $retval = unlink($filename);
    if ($retval != $g_succ) { return $retval; }
    return $g_succ;
}

sub isDir
{
    my $dirname = $_[0];
    if (!-e $dirname) {
        return $g_false;
    }
    if (-d $dirname) {
        return $g_true;
    }
    return $g_false;
}

sub findDirRecursively {
    my $dir = $_[0];
    my @filelist = findAnyFileRecursively($dir);
    @g_filelist = ();
    foreach (@filelist) {
        chomp;
        if (isDir($_)) {
            push(@g_filelist, $_);
        }
    }
    return @g_filelist;
}

sub removeDir
{
    my $dirname = $_[0];
    if (!-e $dirname) {
        print "\n$dirname DOES NOT EXIST! NOTHING TO DO!\n";
        return $g_succ;
    }
    my $retval = rmtree($dirname);
    return $g_succ;
}

#Return succ if files copied.
sub copyFile
{
    my $old = $_[0]; #old file path
    my $new = $_[1]; #new file path
    my $num_of_files = fcopy($old, $new) or abortex();
    return $g_succ;
}

#Return the number of (files, dirs, depth) copied.
sub copyDir
{
    $File::Copy::Recursive::CPRFComp = 1;
    my $old = $_[0]; #old directory
    my $new = $_[1]; #new directory
    my($num_of_files_and_dirs, $num_of_dirs, $depth_traversed) =
        dircopy($old, $new)
        or abortex();
    return ($num_of_files_and_dirs, $num_of_dirs, $depth_traversed);
}

1;
