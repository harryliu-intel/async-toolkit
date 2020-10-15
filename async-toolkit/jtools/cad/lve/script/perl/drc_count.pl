#!/usr/intel/bin/perl -w

my %count;
my $rule;
while (my $line = <STDIN> ){
    if ($line =~ /^ (\S+):/) { $rule = $1; }
    if ($line =~ /(\d+) violation[s]* found.$/) {
        my $c=$1;
        print "$rule\t$c\n";
        if ($rule =~ /^M/ || $rule =~ /^V/) { $class="metvia"; }
        else                                { $class="other"; }
        if (defined($count{$class})) { $count{$class}+=$c; }
        else                         { $count{$class} =$c; }
    }
}

print "\n";
foreach my $key (keys %count) {
    print "$key violations: $count{$key}\n";
}
