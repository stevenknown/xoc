#!/usr/bin/perl
use strict;
use warnings;
our $test_file;
our $input_file;
our @conf_lines;
our @input_lines;
our $input_content;
our @check_rule;
our $g_succ = 0;
our $g_fail = -1;
our $g_true = 1;
our $g_false = 0;

main();

sub main
{
    parseCmdLine();
    readCheckFileAndInputFile();
    parseCheckDirective();
    my $all_passed = checkInputFile();
    if ($all_passed == $g_true) {
        print "CHECK FINISH!\n";
        exit 0;
    }
    print "\nTHERE ARE ERRORS OCCURRED!\n";
    exit 1;
}

#Command line option parser
#parses --conf and --input arguments
#$test_file_ref - scalar reference to store conf file path
#$input_file_ref - scalar reference to store input file path
sub getPureOptions
{
    # Get references to the variables that will store the parsed values
    my ($test_file_ref, $input_file_ref) = @_;
    my $err = 0;
    # Iterate over all command line arguments in @ARGV
    for (my $i = 0; $i < scalar @ARGV; $i++) {
        my $current_arg = $ARGV[$i];

        # Parse -conf option
        if ($current_arg eq '-conf') {
            # Check if a value is provided after -conf
            if ($i + 1 >= scalar @ARGV) {
                print "ERROR: -CONF REQUIRES A FILE PATH\n";
                return 0;
            }
            # Assign the next argument as the conf file path
            $$test_file_ref = $ARGV[$i + 1];
            $i++;  # Skip the next index since it was consumed as a value
        }
        # Parse -input option
        elsif ($current_arg eq '-input') {
            # Check if a value is provided after -input
            if ($i + 1 >= scalar @ARGV) {
                print "ERROR: -INPUT REQUIRES A FILE PATH\n";
                return 0;
            }
            # Assign the next argument as the input file path
            $$input_file_ref = $ARGV[$i + 1];
            $i++;  # Skip the next index since it was consumed as a value
        }
        # Handle unknown options
        else {
            print "ERROR: UNKNOWN COMMAND LINE OPTION: $current_arg\n";
            return 0;
        }
    }
    if ($err > 0) {
        print "ERROR: ERROR OCCURRED.\n";
        return 0;
    }

    # Validate both required options are provided
    if (!$$test_file_ref || !$$input_file_ref) {
        print "ERROR: BOTH -CONF AND -INPUT OPTIONS ARE MANDATORY\n";
        return 0;
    }
    return 1;
}

# Print script usage information
sub usage
{
    print "USAGE: $0 -conf <configure_script_file> -input <input_file_to_verify>\n";
    print "CHECK DIRECTIVE SYNTAX:\n";
    print "  CHECK-EXIST: fuzzle matched content_to_verify_existence\n";
    print "  CHECK-NOT: fuzzle matched content_to_verify_existence\n";
    print "  CHECK-HEAD: fuzzle matched headline_content\n";
    print "  CHECK-HEAD-EXACT: exact matched headline_content\n";
    print "  CHECK-NEXT: fuzzle matched required_content_on_next_line\n";
}

# Pure Perl implementation to read file lines, no external modules
# Parameters: file path, chomp flag (1 to remove newlines)
# Returns: array of file lines
sub read_file
{
    my ($file_path, $do_chomp) = @_;
    my @lines;
    
    open(my $fh, '<', $file_path) or die "CANNOT OPEN FILE $file_path: $!";
    while (my $line = <$fh>) {
        chomp $line if $do_chomp;
        push @lines, $line;
    }
    close($fh);
    
    return @lines;
}

# Parse command line options
sub parseCmdLine
{
    if (getPureOptions(\$test_file, \$input_file) == 0){
        usage();
        exit(2);
    }

    # Validate input files exist
    if (!$test_file) {
        print "ERROR: INVALID PARAMETER OF TEST_FILE.\n";
        usage();
        exit(3);
    }
    if (!-f $test_file) {
        print "ERROR: $test_file DOES NOT EXIST.\n";
        exit(4);
    }
    if (!$input_file) {
        print "ERROR: INVALID PARAMETER OF INPUT_FILE.\n";
        exit(5);
    }
    if (!-f $input_file) {
        print "ERROR: $input_file DOES NOT EXIST.\n";
        exit(6);
    }
}

sub readCheckFileAndInputFile
{
    # Read input files using custom function
    @conf_lines = read_file($test_file, 1);
    @input_lines = read_file($input_file, 1);

    # Merge all input line into a big string buffer to facilitate CHECK-EXIST.
    $input_content = join("\n", @input_lines);
}

sub parseCheckDirective
{
    # Parse all check directives from test file
    my $last_line_num_of_conf_file = $#conf_lines;
    foreach my $line_num (0 .. $last_line_num_of_conf_file) {
        my $line = $conf_lines[$line_num];
        my $conf_line_num = $line_num + 1;
    
        # Parse CHECK-EXIST directive
        if ($line =~ /^\s*CHECK-EXIST:\s*(.*)$/) {
            push @check_rule, {
                type    => 'EXIST',
                content => $1,
                line    => $conf_line_num,
            };
        }

        #Fuzzle-match at line for CHECK-NOT directive
        elsif ($line =~ /^\s*CHECK-NOT:\s*(.*)$/) {
            push @check_rule, {
                type    => 'NOT',
                content => $1,
                line    => $conf_line_num,
            };
        }

        #Fuzzle-match at line for CHECK-HEAD directive
        elsif ($line =~ /^\s*CHECK-HEAD:\s*(.*)$/) {
            push @check_rule, {
                type    => 'HEAD',
                content => $1,
                line    => $conf_line_num,
            };
        }

        #Exact-match at line for CHECK-HEAD directive
        elsif ($line =~ /^\s*CHECK-HEAD-EXACT:(.*)$/) {
            push @check_rule, {
                type    => 'HEAD-EXACT',
                content => $1,
                line    => $conf_line_num,
            };
        }

        #Fuzzle-match at line for CHECK-NEXT directive
        elsif ($line =~ /^\s*CHECK-NEXT:\s*(.*)$/) {
            push @check_rule, {
                type    => 'NEXT',
                content => $1,
                line    => $conf_line_num,
            };
        }

        #Fuzzle-match at line for CHECK-NEXT directive
        elsif ($line =~ /^\s*CHECK-NEXT-EXACT:\s*(.*)$/) {
            push @check_rule, {
                type    => 'NEXT-EXACT',
                content => $1,
                line    => $conf_line_num,
            };
        } elsif ($line =~ /^\s*CHECK-.*:\s*(.*)$/) {
            print "WARNING($conf_line_num): UNKNOWN RULE:$line\n";
        }
    }
}

#Check if content does not exists anywhere in input file
sub checkNotExist
{
    my ($rule) = @_;
    my $type = $rule->{type};
    my $content = $rule->{content};
    my $conf_line_num = $rule->{line};
    if ($input_content =~ /\Q$content\E/) {
        print "CHECK-NOT line($conf_line_num) FAILED: PATTERN EXISTS IN $input_file\n";
        return $g_fail;
    }
    print "CHECK-NOT line($conf_line_num) PASSED: '$content'\n";
    return $g_succ
}

#Check if content exists anywhere in input file
sub checkExist
{
    my ($rule) = @_;
    my $type = $rule->{type};
    my $content = $rule->{content};
    my $conf_line_num = $rule->{line};
    chomp $content;
    $content =~ s/^[\t ]+|[\t ]+$//g;
    if ($input_content =~ /\Q$content\E/) {
        print "CHECK-EXIST line($conf_line_num) PASSED: '$content'\n";
        return $g_succ
    }
    print "CHECK-EXIST line($conf_line_num) FAILED: PATTERN DOES NOT EXIST IN $input_file\n";
    return $g_fail;
}

sub assert
{
    my ($det, $msg) = @_;
    if ($det != $g_true) {
        print "ASSERTION:$msg";
        abort();
    }
}

sub checkHead
{
    my ($input_line_num, $check_head_start) = @_;
    my $rule = $check_rule[$check_head_start];
    my $type = $rule->{type};
    my $content = $rule->{content};
    my $conf_line_num = $rule->{line};
    assert(isHead($type), "NEED CHECK-HEAD");
    my $input_line = $input_lines[$input_line_num];

    #Must striping the newline char at first, otherwise it will disturb the
    #regexp-matching.
    chomp $input_line;
    chomp $content;
    $content =~ s/^[\t ]+|[\t ]+$//g;
    $input_line =~ s/^[\t ]+|[\t ]+$//g;
    if (isFuzzleHead($type)) {
        #Allow any number of blank and tab at the start of the line.
        if ($input_line =~ /^.*\Q$content\E/) {
            print "\nCHECK-$type($conf_line_num) PASSED: '$content' IN INPUT FILE AT LINE(" . ($input_line_num+1) . ")\n";
            return $g_succ;
        }
    } elsif (isExactHead($type)) {
        if ($input_line =~ /^\Q$content\E/) {
            print "\nCHECK-$type($conf_line_num) PASSED: '$content' IN INPUT FILE AT LINE(" . ($input_line_num+1) . ")\n";
            return $g_succ;
        }
    }
    return $g_fail;
}

sub checkNext
{
    my ($input_line_num, $check_next_start, $check_next_end) = @_;
    my $tmp_input_line_num = $input_line_num;
    for my $idx ($check_next_start .. $check_next_end) {
        my $rule = $check_rule[$idx];
        my $type = $rule->{type};
        my $content = $rule->{content};
        my $conf_line_num = $rule->{line};
        if (!isNext($type)) {
            print "ASSERTION:NEED CHECK-NEXT";
            abort();
        }

        chomp $content;
        $content =~ s/^[\t ]+|[\t ]+$//g;
        my $next_line_content = $input_lines[$tmp_input_line_num];
        chomp $next_line_content;
        $next_line_content =~ s/^[\t ]+|[\t ]+$//g;
        if ($next_line_content =~ /^.*\Q$content\E/) {
            print "CHECK-NEXT($conf_line_num) PASSED: '$content' IN INPUT FILE AT LINE(" . ($tmp_input_line_num+1) . ")\n";
            $tmp_input_line_num++; #ready to read next line in input file.
            next;
        }
        print "CHECK-NEXT($conf_line_num) NOT MATCH, TRY NEXT: EXPECTED '$content' AT LINE(" . ($tmp_input_line_num+1) . "), GOT '$next_line_content'\n";
        return $g_fail;
    }
    return $g_succ;
}

#Match the HEAD line of the CHECK-HEAD pattern and record line number
sub checkHeadAndNext
{
    my ($check_head_start, $check_head_end) = @_;

    #Record the last line number of input file.
    my $last_line_num_of_input_file = $#input_lines;
    foreach my $input_line_num (0 .. $last_line_num_of_input_file) {
        my $res = checkHead($input_line_num, $check_head_start);
        if ($res != $g_succ) {
            next;
        }
        my $res2 = checkNext($input_line_num + 1, $check_head_start + 1, $check_head_end);
        if ($res2 == $g_succ) {
            return $g_succ;
        }
    }
    my $rule = $check_rule[$check_head_start];
    my $type = $rule->{type};
    my $content = $rule->{content};
    my $conf_line_num = $rule->{line};
    print "\nCHECK-HEAD($conf_line_num) FAILED: PATTERN DOES NOT EXIST IN $input_file THAT LEAD BY $check_head_start\n";
    return $g_fail; #No rules matched.
}

sub dumpCheckRule
{
    foreach my $rule (@check_rule) {
        my $type = $rule->{type};
        my $content = $rule->{content};
        my $conf_line_num = $rule->{line};
        print "\nDUMP RULE\n";
        print "\nTYPE:$type";
        print "\nCONTENT:$content";
        print "\nCONF_LINE_NUM:$conf_line_num";
    }
}

sub checkInputFile
{
    # Execute all validation checks
    my $all_passed = $g_true;
    my $check_head_start = -1;
    my $check_head_end = -1;
    my $rule_idx = 0;
    #dumpCheckRule();
    foreach my $rule (@check_rule) {
        my $type = $rule->{type};
        my $cur_rule_idx = $rule_idx;
        $rule_idx++;

        # Check if content exists anywhere in input file
        if ($type eq 'EXIST') {
            my $res = checkExist($rule);
            if ($res != $g_succ) {
                $all_passed = $g_false;
            }
            next;
        }
        if ($type eq 'NOT') {
            my $res = checkNotExist($rule);
            if ($res != $g_succ) {
                $all_passed = $g_false;
            }
            next;
        }
        if (isHead($type)) {
            my $check_rule_last = $#check_rule;
            $check_head_start = $cur_rule_idx;
            $check_head_end = $cur_rule_idx;
            for my $tmpidx ($check_head_start + 1 .. $check_rule_last) {
                my $tmprule = $check_rule[$tmpidx];
                my $tmpruletype = $tmprule->{type};
                if (!isFuzzleNext($tmpruletype)) {
                    last;
                }
                $check_head_end++;
            }
            my $res = checkHeadAndNext($check_head_start, $check_head_end);
            if ($res != $g_succ) {
                $all_passed = $g_false;
                last;
            }
        }
    }
    return $all_passed;
}

sub isFuzzleNext
{
    my ($type) = @_;
    if ($type eq 'NEXT') {
        return $g_true;
    }
    return $g_false;

}

sub isFuzzleHead
{
    my ($type) = @_;
    if ($type eq 'HEAD') {
        return $g_true;
    }
    return $g_false;

}

sub isExactHead
{
    my ($type) = @_;
    if ($type eq 'HEAD-EXACT') {
        return $g_true;
    }
    return $g_false;

}

sub isNext
{
    my ($type) = @_;
    if ($type eq 'NEXT' || $type eq 'NEXT-EXACT') {
        return $g_true;
    }
    return $g_false;
}

sub isHead
{
    my ($type) = @_;
    if ($type eq 'HEAD' || $type eq 'HEAD-EXACT') {
        return $g_true;
    }
    return $g_false;
}
