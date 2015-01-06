package LveAlint;

use POSIX;
use LveUtil;
use LveStatus;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &is_leakage_result
        &is_delay_result
        &is_slew_result
        &is_skew_result
        &is_bump_result 
        &is_inv_bump_result 
        &is_new_bump_result 
        &new_bump_measurement_successful
        &get_leakage_result
        &get_delay_result
        &get_slew_result
        &get_skew_result
        &get_bump_result 
        &get_inv_bump_result 
        &get_new_bump_result 
    );
}
use strict;
our @cleanMe=();

sub is_leakage_result {
    my ($keys) = @_;
    if (defined($keys->{"leak_dn"}) && 
        defined($keys->{"leak_up"}) && 
        defined($keys->{"leak_bound"}) &&
        defined($keys->{"node"})) {
        return 1;
    } else {
        return 0;
    }
}

sub is_delay_result {
    my ($keys) = @_;
    if (defined($keys->{"slow_delay_up"}) &&
        defined($keys->{"slow_delay_dn"}) &&
        defined($keys->{"fast_delay_up"}) &&
        defined($keys->{"fast_delay_dn"}) &&
        defined($keys->{"delay_bound_up"}) && 
        defined($keys->{"delay_bound_dn"}) && 
        defined($keys->{"node"}) &&
        defined($keys->{"tau"})) {
        return 1;
    } else {
        return 0;
    }
}

sub is_slew_result {
    my ($keys) = @_;
    if (defined($keys->{"slow_slew_up"}) &&
        defined($keys->{"slow_slew_dn"}) &&
        defined($keys->{"fast_slew_up"}) &&
        defined($keys->{"fast_slew_dn"}) &&
        defined($keys->{"slew_bound"}) &&
        defined($keys->{"node"}) &&
        defined($keys->{"tau"})) {
        return 1;
    } else {
        return 0;
    }
}

sub is_skew_result {
    my ($keys) = @_;
    if (defined($keys->{"skew_up"}) &&
        defined($keys->{"skew_dn"}) &&
        defined($keys->{"skew_bound"}) &&
        defined($keys->{"node"}) &&
        defined($keys->{"tau"})) {
        return 1;
    } else {
        return 0;
    }
}

sub is_bump_result {
    my ($keys, $cc) = @_;
    if (defined($keys->{"bump_dn"}) && 
        defined($keys->{"bump_up"}) && 
        defined($keys->{"inv_bump_dn"}) && 
        defined($keys->{"inv_bump_up"}) && 
        defined($keys->{"inv_bump_dn_bound"}) && 
        defined($keys->{"inv_bump_up_bound"}) && 
        defined($keys->{"bump_dn_bound"}) && 
        defined($keys->{"bump_up_bound"}) && 
        defined($keys->{"cc"}) &&
        defined($keys->{"node"}) &&
        ($keys->{"cc"} == $cc)) {
        return 1;
    } else {
        return 0;
    }
}

sub is_inv_bump_result {
    my ($keys, $cc) = @_;
    if (is_bump_result($keys,$cc) &&
        (!($keys->{"inv_bump_up_bound"} eq "NA") ||
         !($keys->{"inv_bump_dn_bound"} eq "NA"))) {
        return 1;
    } else {
        return 0;
    }
}

sub is_new_bump_result {
    my ($keys, $threshCC, $cc) = @_;
    if ((defined($keys->{"fanin"}) ||
         defined($keys->{"fanout"})) &&
        (defined($keys->{"threshCC"}) || 
         defined($keys->{"fail_type"}))&& 
        defined($keys->{"cc"})) {
        return (($keys->{"cc"} == $cc) && 
                ( defined($keys->{"fail_type"}) || 
                  ($keys->{"threshCC"} == $threshCC))) ? 1:0;
    } else {
        return 0;
    }
}

sub new_bump_measurement_successful {
    my ($keys) = @_;
    if (defined($keys->{"fail_type"})) {
        my $fail_type = $keys->{"fail_type"};
        if (($fail_type eq "bump_width") ||
            ($fail_type eq "thresh_resp_bound") ||
            ($fail_type eq "max_thresh_percent")) {
            return 1;
        }
        return 0;
    }
    return 1;
}

sub meas_le_bnd {
    my ($val, $spec) = @_;
    return (($val eq "NA")|| ($val <= $spec))? 1:0;
}

sub get_measurement {
    my ($raw_str) = @_;
    my @vals = split("@", $raw_str);
    return $vals[0];
}

sub valid_measurement {
    my ($val) = @_;
    return ((is_numeric($val) == 1) || ($val eq "NA")) ? 1:0;
}
       
sub get_leakage_result {
    my ($keys) = @_;
    my $node = $keys->{"node"};
    my $measured = 0;
    my $pass   = 0;
    my $bound   = $keys->{"leak_bound"};
    my $leak_up = get_measurement($keys->{"leak_up"});
    my $leak_dn = get_measurement($keys->{"leak_dn"});
    $measured = (valid_measurement($leak_up) && 
                 valid_measurement($leak_dn) &&
                 valid_measurement($bound)) ? 1 : 0;
    $pass = ((($measured == 1) && 
              meas_le_bnd($leak_up, $bound) && 
              meas_le_bnd($leak_dn, $bound)) ||
             $measured == 0) ? 1:0;
    return ($node, $measured, $pass);
}

sub get_new_bump_result {
    my ($keys, $cell) = @_;
    my $node;
    if (defined($keys->{"fanin"})) {
        $node = $keys->{"fanin"};
    } else {
        $node = $keys->{"fanout"};
    }
    my $measured = 1;
    my $pass   = 1;
    if (defined($keys->{"fail_type"})) {
        if (($keys->{"fail_type"} eq "bump_width") ||
            ($keys->{"fail_type"} eq "thresh_resp_bound") ||         
            ($keys->{"fail_type"} eq "max_thresh_percent")) {
            $pass     = 0;
        } else {
            $measured = 0;
            $pass     = 1;
        }
    }
    unless ($node =~ /\//) {
        $node="$cell/$node";
    }
    return ($node, $measured, $pass);
}

sub get_bump_result {
    my ($keys) = @_;
    my $node = $keys->{"node"};
    my $measured = 0;
    my $pass   = 0;
    my $bound_up = $keys->{"bump_up_bound"};
    my $bound_dn = $keys->{"bump_dn_bound"};
    my $bump_up  = get_measurement($keys->{"bump_up"});
    my $bump_dn  = get_measurement($keys->{"bump_dn"});
    $measured = ((valid_measurement($bump_up) == 1)&& 
                 (valid_measurement($bump_dn) == 1)) ? 1 : 0;
    if ($measured == 0) { $pass = 1; } 
    else { $pass = (
               meas_le_bnd($bump_up, $bound_up) && 
               meas_le_bnd($bump_dn, $bound_dn)) ? 1:0;
         }
    return ($node, $measured, $pass);
}

sub get_delay_result {
    my ($keys) = @_;
    my $node = $keys->{"node"};
    my $measured = 0;
    my $pass   = 0;
    my $bound_up = $keys->{"delay_bound_up"};
    my $bound_dn = $keys->{"delay_bound_dn"};
    my $slow_up  = get_measurement($keys->{"slow_delay_up"});
    my $slow_dn  = get_measurement($keys->{"slow_delay_dn"});
    my $fast_up  = get_measurement($keys->{"fast_delay_up"});
    my $fast_dn  = get_measurement($keys->{"fast_delay_dn"});
    $measured = (valid_measurement($slow_up) && 
                 valid_measurement($slow_dn) &&
                 valid_measurement($fast_up) && 
                 valid_measurement($fast_dn) &&
                 valid_measurement($bound_up) &&
                 valid_measurement($bound_dn)) ? 1 : 0;
    if ($measured == 1 && $keys->{"tau"} == 8) {
        my $pass_up = 1;
        if (is_numeric($bound_up)) {
            $pass_up = (meas_le_bnd($slow_up, $bound_up) && 
                        meas_le_bnd($fast_up, $bound_up)) ? 1:0;
        }
        my $pass_dn = 1;
        if (is_numeric($bound_dn)) {
            $pass_dn = (meas_le_bnd($slow_dn, $bound_dn) && 
                        meas_le_bnd($fast_dn, $bound_dn)) ? 1:0;
        }
        $pass = ($pass_up == 1 && $pass_dn == 1) ? 1:0;
    } else {
        $pass = 1;
    }
    return ($node, $measured, $pass);
}

sub get_slew_result {
    my ($keys) = @_;
    my $node = $keys->{"node"};
    my $measured = 0;
    my $pass   = 0;
    my $bound  = $keys->{"slew_bound"};
    my $slow_up  = get_measurement($keys->{"slow_slew_up"});
    my $slow_dn  = get_measurement($keys->{"slow_slew_dn"});
    my $fast_up  = get_measurement($keys->{"fast_slew_up"});
    my $fast_dn  = get_measurement($keys->{"fast_slew_dn"});
    $measured = (valid_measurement($slow_up) && 
                 valid_measurement($slow_dn) &&
                 valid_measurement($fast_up) && 
                 valid_measurement($fast_dn) &&
                 valid_measurement($bound)) ? 1 : 0;
    $pass = (($measured == 1 && 
              $keys->{"tau"} == 8 && 
              meas_le_bnd($slow_up, $bound) && 
              meas_le_bnd($fast_up, $bound) && 
              meas_le_bnd($slow_dn, $bound) && 
              meas_le_bnd($fast_dn, $bound)) ||
             $measured == 0 ||
             $keys->{"tau"} != 8) ? 1:0;
    return ($node, $measured, $pass);
}

sub get_skew_result {
    my ($keys) = @_;
    my $node = $keys->{"node"};
    my $measured = 0;
    my $pass   = 0;
    my $bound  = $keys->{"skew_bound"};
    my $up  = get_measurement($keys->{"skew_up"});
    my $dn  = get_measurement($keys->{"skew_dn"});
    $measured = (valid_measurement($up) && 
                 valid_measurement($dn) && 
                 valid_measurement($bound)) ? 1 : 0;
    $pass = ((($measured == 1 && $keys->{"tau"} == 8) && 
              meas_le_bnd($up, $bound) && 
              meas_le_bnd($dn, $bound)) ||
             $measured == 0 ||
             $keys->{"tau"} != 8) ? 1:0;
    return ($node, $measured, $pass);
}

sub get_inv_bump_result {
    my ($keys) = @_;
    my $node = $keys->{"node"};
    my $measured = 0;
    my $pass   = 0;
    my $bound_up = $keys->{"inv_bump_up_bound"};
    my $bound_dn = $keys->{"inv_bump_dn_bound"};
    my $bump_up  = get_measurement($keys->{"inv_bump_up"});
    my $bump_dn  = get_measurement($keys->{"inv_bump_dn"});
    $measured = ((valid_measurement($bump_up) == 1) &&
                 (valid_measurement($bump_dn) == 1)) ? 1 : 0;
    $pass = (($measured == 0) || 
             ( meas_le_bnd($bump_up, $bound_up) && 
               meas_le_bnd($bump_dn, $bound_dn))) ? 1:0;

    return ($node, $measured, $pass);
}

1;
