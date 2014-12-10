package LveDelay;

use strict;
use LveUtil;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(&write_delay_directives);
}

#
# Given a set of characterized delays (as returned by
# LveAspice::characterize_delay), writes up to two files containing CAST
# measured_delay directives:
#
#   directives.0   - Model type 0 measured_delay directives for
#                    the cell (interpolation points).
#   directives.1   - Model type 1 measured_delay directives for
#                    the cell (2nd order polynomial coefficients.)
#                    This file is generated if the delay data set 
#                    includes exactly three input slew points.
#
sub write_delay_directives {
    my ($directory, $data_ref, $cap) = @_;
    # CONFIGURATION: Interpolation & polynomial should agree within 10% 
    # or else the data is probably questionable:
    my $MODEL_DIFF_THRESHOLD = 0.10;
    my @directives = ( [], [] );
    my $do_polynomial_fit = 0;                  # Skip polynomial fit
    my @charge_directives = ();
    my $docharge=0;
    foreach my $half_op (keys %$data_ref) {
        my @in_nodes = keys %{$data_ref->{$half_op}};
        foreach my $in (@in_nodes) {
            my @slew_points = sort {$a<=>$b} 
                              keys %{$data_ref->{$half_op}->{$in}};
            my @D = ( [], [], [], [] );
            my @CH = ( [], [], [], [] );
            # sometimes aspice or aplot leaves behind a : in the name
            my $onode=$half_op;
            my $inode=$in;
            $onode =~ s/:.*//;
            $inode =~ s/:.*//;
            push @{$directives[0]}, "measured_delay({$onode,$inode}) = {\n";
            push @charge_directives,
                "measured_charge({$onode,$inode}) = {\n";
            push @{$directives[0]}, "  { 0 },\n";
            push @charge_directives, "  { 0 },\n";
            my $dstr = "  { ";
            foreach my $s (@slew_points) {
                $dstr .= round_float($s,-2);
                $dstr .= ", " if ($s != $slew_points[$#slew_points]);
            }
            $dstr .= " },\n";
            push @{$directives[0]}, $dstr;
            push @charge_directives, $dstr;
            foreach my $s (@slew_points) {
                my @points = @{$data_ref->{$half_op}->{$in}->{$s}};
                for my $i (0..3) {
                    push @{$D[$i]}, $points[$i];
                    if (defined($points[$i+4])) {
                        push @{$CH[$i]}, $points[$i+4];
                        $docharge=1;
                    }
                }
            }
            # Max delay/slew (set 0), Min delay/slew (set 1)
            for my $maxmin (0..1) {
                for my $i (0..1) {
                    $dstr = "  { ";
                    for my $j (0..$#{$D[2*$maxmin+$i]}) {
                        $dstr .= round_float(${$D[2*$maxmin+$i]}[$j],-2);
                        if ($j != $#{$D[2*$maxmin+$i]}) { $dstr .= ", "; }
                        else { $dstr .= " }"; }
                    }
                    if ($maxmin==0 || $i==0) { $dstr .= ",\n"; }
                    else { $dstr .= "};\n"; }
                    push @{$directives[0]}, $dstr;
                }
            }
            for my $gv (0..1) {
                for my $cw (0..1) {
                    $dstr = "  { ";
                    for my $j (0..$#{$CH[2*$gv+$cw]}) {
                        if ($cw == 0) {
                            $dstr .= round_float(${$CH[2*$gv+$cw]}[$j]*1e6,-2);
                        }
                        else {
                            $dstr .= round_float(${$CH[2*$gv+$cw]}[$j]*1e3,-2);
                        }
                        if ($j != $#{$CH[2*$gv+$cw]}) { $dstr .= ", "; }
                        else { $dstr .= " }"; }
                    }
                    if ($gv==0 || $cw==0) { $dstr .= ",\n"; }
                    else { $dstr .= "};\n"; }
                    push @charge_directives, $dstr;
                }
            }
            if ($do_polynomial_fit) {
                my @C = ( [], [], [], [] );
                for my $i (0..3) {
                    @{$C[$i]} = polynomial_solve(\@slew_points,$D[$i]);
                }
                push @{$directives[1]}, "measured_delay({$onode,$inode}) = {\n";
                push @{$directives[1]}, "  { 1 },\n";
                for my $maxmin (0..1) {
                    for my $i (0..1) {
                        $dstr = "  { ";
                        for my $j (0..2) {
                            $dstr .= sprintf("%.5g",${$C[2*$maxmin+$i]}[$j]);
                            if ($j != 2) { $dstr .= ", "; }
                            else { $dstr .= " }"; }
                        }
                        if ($maxmin==0 || $i==0) { $dstr .= ",\n"; }
                        else { $dstr .= "};\n"; }
                        push @{$directives[1]}, $dstr;
                    }
                }
                
                # Write graph of polynomial fit for debugging
                open POLYDELAY, ">$directory/$half_op.delay.fit" || 
                    die "Couldn't write to $directory/$half_op.delay.fit.\n";
                open POLYSLEW, ">$directory/$half_op.slew.fit" || 
                    die "Couldn't write to $directory/$half_op.slew.fit.\n";
                print POLYDELAY "# Input $in\n";
                print POLYSLEW  "# Input $in\n";
                for my $i (0..40) {
                    print POLYDELAY sprintf("%.5g %.5g %.5g\n", 5*$i,
                        polynomial_eval($C[0],5*$i),
                        polynomial_eval($C[0],5*$i)-
                        polynomial_eval($C[2],5*$i));
                    print POLYSLEW  sprintf("%.5g %.5g %.5g\n", 5*$i,
                        polynomial_eval($C[1],5*$i), 
                        polynomial_eval($C[1],5*$i)-
                        polynomial_eval($C[3],5*$i));
                }
                print POLYDELAY "\n\n";
                print POLYSLEW  "\n\n";
                close POLYDELAY;
                close POLYSLEW;

                #
                # Evaluate error between linearly interpolated values
                # and polynomial values at a handful of points to make
                # sure the polynomial fit is reasonable.
                #
                my @test_slews = (10, @slew_points, 100);
                foreach my $i (1..$#slew_points) { 
                    $test_slews[$i] += ($slew_points[$i]-$slew_points[$i-1])/2;
                }
                my @data_set = ( "slow delay", "slow slew", 
                                 "fast delay", "fast slew" );
                foreach my $i (0..3) {
                    my $large_error = 0.0;
                    foreach my $slew (@test_slews) {
                        my $ival = interpolate($slew,\@slew_points,$D[$i]);
                        my $error = abs(polynomial_eval($C[$i],$slew) - $ival) 
                                    / $ival;
                        if ($error > $MODEL_DIFF_THRESHOLD && 
                            $error > $large_error) {
                            $large_error = $error 
                        }
                    }
                    if ($large_error != 0.0) {
                        warn "Warning: Large discrepancy of " . 
                             sprintf("%.3g", $large_error*100) . 
                             "% observed between interpolation\n" .
                             "         and polynomial fit of " .  
                             $data_set[$i] .  " data for $in -> $half_op.\n";
                    }
                }
            }
        }
    }
    my $suffix="0";
    $suffix = "${cap}f" if defined ($cap) and $cap ne "0";
    my $leading="      ";
    open (DIRECTIVES, ">$directory/directives.$suffix") ||
        die "Couldn't write to $directory/directives.$suffix.\n";
    print DIRECTIVES "$leading". join("$leading", @{$directives[0]});
    print DIRECTIVES "$leading". join("$leading", @charge_directives)
        if $docharge;
    close DIRECTIVES;
    if (@{$directives[1]}) {
        open (DIRECTIVES, ">$directory/directives.1") ||
            die "Couldn't write to $directory/directives.1.\n";
        print DIRECTIVES "$leading". join("$leading", @{$directives[1]});
        close DIRECTIVES;
    }
}


sub polynomial_solve {
    my ($xref,$yref) = @_;
    my @x = @{$xref}; my @y = @{$yref};
    my @c;
    $c[0] = $y[0] - $x[0]/($x[1]-$x[0])*($y[1]-$y[0]) +
            $x[0]*$x[1]/($x[2]-$x[1]) *
            (($y[2]-$y[0])/($x[2]-$x[0])-($y[1]-$y[0])/($x[1]-$x[0]));
    $c[1] = ($x[1]+$x[0])/($x[1]-$x[0])*(($y[1]-$y[0])/($x[1]+$x[0])-
             (($x[1]-$x[0])*($y[2]-$y[0])-($x[2]-$x[0])*($y[1]-$y[0]))/
              (($x[2]-$x[0])*($x[2]-$x[1])));
    $c[2] = 1/($x[2]-$x[1]) *
            (($y[2]-$y[0])/($x[2]-$x[0])-($y[1]-$y[0])/($x[1]-$x[0]));
    return @c;
}


sub polynomial_eval {
    my ($cref,$x) = @_;
    my $y = 0;
    my $xpow = 1;
    for my $i (0..$#{$cref}) {
        $y += $$cref[$i]*$xpow;
        $xpow *= $x;
    }
    return $y;
}

sub interpolate {
    my ($x, $xpoints_ref, $ypoints_ref) = @_;
    my @s = (-1,-1);

    my $i;
    for $i (0..$#$xpoints_ref) {
        $s[0] = $i if ($$xpoints_ref[$i] < $x);
        $s[1] = $i if ($$xpoints_ref[$i] >= $x && $s[1] == -1);
    }
    if ($s[0] == -1) {
        $s[0] = 0; $s[1] = 1;
    }
    elsif ($s[1] == -1) {
        $s[0] = $#$xpoints_ref-1; $s[1] = $#$xpoints_ref;
    }
    return $$ypoints_ref[$s[0]] + 
           ($$ypoints_ref[$s[1]] - $$ypoints_ref[$s[0]]) / 
           ($$xpoints_ref[$s[1]] - $$xpoints_ref[$s[0]]) *
           ($x - $$xpoints_ref[$s[0]]);
}

1;
