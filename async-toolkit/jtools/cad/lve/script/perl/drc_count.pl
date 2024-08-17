#!/usr/intel/bin/perl -w

my %count;
my %rules;
my $rule;
while (my $line = <STDIN> ){
    if ($line =~ /^ (\S+):/) { $rule = $1; }
    if ($line =~ /(\d+) violation[s]* found.$/) {
        my $c=$1;
        my $class;
        if ($rule =~ /NODATA/) { $class="nodata"; }
        elsif ($rule =~ /^bm/ || $rule =~ /^m/ || $rule =~ /^bv/ || $rule =~ /^v/) { $class="metvia"; }
        else { $class="other"; }
        if (defined($count{$class})) { $count{$class}+=$c; $rules{$class} .= "\t$rule\t$c\n"; }
        else                         { $count{$class} =$c; $rules{$class}  = "\t$rule\t$c\n"; }
    }
}

foreach my $key ("nodata", "other", "metvia") {
    defined($count{$key}) or next;
    print "$key violations: $count{$key}\n";
    print "$rules{$key}";
    print "\n";
}
