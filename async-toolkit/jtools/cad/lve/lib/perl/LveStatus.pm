
package LveStatus;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &get_lines &get_results &parse_raw_line &get_status_priority
        &combine_status &get_status
    );
}

# extract fields of a raw line

sub get_lines {
    my ($file) = @_;
    
    my @lines = ();
    open RAW, "<$file" or die "Can't read $file\n";
    while (my $line = <RAW>) { push(@lines,$line); }
    close RAW;

    return @lines;
}


# parse list of key=value pairs into hash table
sub get_results {
    my @args = @_;
    my %results = ();
    foreach my $i (@args) {
        my ($key,$value) = split("=",$i);
        $results{$key} = $value;
    }
    return %results;
}

sub parse_raw_line {
    my ($line) = @_;
    my @args   = split(" ",$line);
    my $status = shift(@args);
    my $task   = shift(@args);
    my $cell   = shift(@args);
    my $path   = shift(@args);
    my %results = get_results(@args);
    return ($status, $task, $cell, $path, %results);
}

%status_priority = ();
$status_priority{PASS} = 0;
$status_priority{NA} =   1;
$status_priority{SIGNOFF} = 2;
$status_priority{INCOMPLETE} = 3;
$status_priority{NOT_TESTED} = 4;
$status_priority{FAIL} = 5;
$status_priority{""} = 6;

sub get_status_priority {
    my ($status) = @_;
    return $status?$status_priority{$status}:0;
}

# combine two status to get a summary status
sub combine_status {
    
    # status priority table; this will control how every thing detected is 
    # perculated to the top
    
    my ($a, $b) = @_;
    my $c;
    if    (! defined $a) { $c = $b; }
    elsif (! defined $b) { $c = $a; }    
    elsif ($status_priority{$a} >= $status_priority{$b}) { $c = $a; }
    else { $c = $b; }
    return $c;
}

# aggregate status over a raw list
sub get_status {
    my @raw = @_;
    my $overall_status;
    foreach my $line (@raw) {
        my ($status, $task, $cell, $path, %results) = parse_raw_line($line);
        $overall_status = combine_status($overall_status,$status);
    }
    if (!defined $overall_status) { $overall_status = "NOT_TESTED"; }
    return $overall_status;
}

1;
