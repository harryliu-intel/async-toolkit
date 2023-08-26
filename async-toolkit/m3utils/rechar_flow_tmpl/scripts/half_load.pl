#!/usr/intel/bin/perl

my $load = 0;
my $factor = $ARGV[0] // 0.5;
while(<STDIN>) {
    if ($load && /^(\s*{\s*)(.*)(\s*}.*)/) {
        my @loads = split(/ /, $2);
        for (my $i = 0; $i <= $#loads; $i++) {
            $loads[$i] *= $factor;
        }
        $_ = $1 . join(' ', @loads) . "$3\n";
    }
    $load = 0;
    if (/explicit_points_load/) {
        $load = 1;
    }
    print;
}
