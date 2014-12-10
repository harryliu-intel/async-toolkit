#
# Misc
#
# Miscellaneous commands that don't really fit anywhere else.  Eventually
# some of these may be moved into other modules.
#
#

package Supersize::Misc;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Term::ANSIColor;

sub get_module_data {
  return { 
    NAME => "Misc",
    DESC => "Miscellaneous commands.",
    COMMANDS => {
    "timestamp" => {
        SUBREF => \&timestamp,
        USAGE  => "time [" . underline("text") . "]",
        DESC   =>
          "Prints the current time.  Useful for timestamping lines in " .
          "sizing scripts.  Any passed text will be printed immediately " .
          "after the time string." },
    "basetype" => {
        SUBREF => \&base_or_subtype,
        USAGE  => "basetype " . underline("fqcn") . " | " . underline("list") .
                         " | [--keys|values] " . underline("map"),
        DESC   => 
          "Converts all fully-qualified subtype names to base type names.  ".
          "For scalar and list arguments, the behavior is straightforward ".
          "and what you expect.  (For example, \"basetype " .
          "lib.buffer.half.BUF_1of2.400\" returns lib.buffer.half.BUF_1of2".
          ".)\n\n" .
          "For maps, this command will substitute ".
          "any fully-qualified subtype name appearing as either a key or ".
          "value.  If two keys map to the same base type, their value ".
          "[lists] will be concatenated without any filtering of redundant " .
          "entries.  Subtype-to-basetype compression can be restricted to ".
          "only the keys or the values with the --keys and --values " .
          "arguments.\n\n" .
          "Nothing is printed to standard output when the argument is " .
          "a map.  Instead, the return variable " . underline("map_result") .
          " is set to be the transformed map.",
        RV => {
          "map_result" => {
            TYPE => $TYPE_MAP,
            DESC => "Basetype-transformed return map." } } },
    "subtype_number" => {
        SUBREF => \&base_or_subtype,
        USAGE  => "subtype " . underline("fqcn") . " | " . underline("list") .
                         " | [--keys|values] " . underline("map"),
        DESC   => 
          "Converts all fully-qualified subtype names to subtype numbers.  ".
          "For scalar and list arguments, the behavior is straightforward ".
          "and what you expect.  (For example, \"subtype_number " .
          "lib.buffer.half.BUF_1of2.400\" returns 400.)\n\n" .
          "For maps, this command will substitute ".
          "any fully-qualified subtype name appearing as either a key or ".
          "value.  If two keys map to the same subtype number, their value " .
          "[lists] will be concatenated without any filtering of redundant " .
          "entries.  FQCN-to-subtype compression can be restricted to ".
          "only the keys or the values with the --keys and --values " .
          "arguments.\n\n" .
          "Nothing is printed to standard output when the argument is " .
          "a map.  Instead, the return variable " . underline("map_result") .
          " is set to be the transformed map.",
        RV => {
          "map_result" => {
            TYPE => $TYPE_MAP,
            DESC => "Basetype-transformed return map." } } },
    }
  };
}

#
# timestamp
#
sub timestamp {
    my $SS_r = shift;

    
    my $str = color('bold') . color('yellow') . localtime() . color('reset');

    # Append text from arguments
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        $str .= $arg . " ";
    }
    
    print $str . "\n";
}

#
# basetype , subtype_number
#
sub base_or_subtype {
    my $SS_r = shift;
    my $i = (get_active_command($SS_r))[1] eq "basetype" ? 0 : 1;
    my %map;
    my $case = -1;
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR && $val =~ /^--(.*)$/) {
            if ($1 eq "keys") {
                $case = 0;
            }
            elsif ($1 eq "values") {
                $case = 1;
            }
            else {
                command_die($SS_r, "Uncrecognized argument '$val'.");
            }
        }
        elsif ($type == $TYPE_SCALAR) {
            command_die($SS_r, "A map must accompany --keys|values.")
                if ($case != -1);
            print "" . (split_possible_fqcn($val))[$i] . "\n";
        }
        elsif ($type == $TYPE_LIST) {
            command_die($SS_r, "A map must accompany --keys|values.")
                if ($case != -1);
            print "< ";
            foreach my $fqcn (@{$val}) {
                print "" . (split_possible_fqcn($fqcn))[$i] . " ";
            }
            print ">\n";
        }
        elsif ($type == $TYPE_MAP) {
            foreach my $k (keys %{$val}) {
                my $bk = (split_possible_fqcn($k))[$i];
                my $key = $case==1 ? $k : $bk;
                if (!exists $map{$key}) {
                    $map{$key} = [ $TYPE_LIST, [] ];
                }
                if ($val->{$k}[0] == $TYPE_SCALAR) {
                    if ($case==0) {
                        push @{$map{$key}->[1]}, $val->{$k}[1];
                    }
                    else {
                        push @{$map{$key}->[1]}, 
                            (split_possible_fqcn($val->{$k}[1]))[$i];
                    }
                }
                elsif ($val->{$k}[0] == $TYPE_LIST) {
                    if ($case==0) {
                        push @{$map{$key}->[1]}, @{$val->{$k}[1]};
                    }
                    else {
                        foreach my $v (@{$val->{$k}[1]}) {
                            push @{$map{$key}->[1]},
                                (split_possible_fqcn($v))[$i];
                        }
                    }
                }
            }
        }
    }
    set_cmd_return_variable($SS_r, "map_result", [ $TYPE_MAP, \%map ]);
}

# Determines if the specified string 'str' is a fully-qualified subtype name.
# If so, returns (basetype, subtype_number).  If not, returns (str, str).
sub split_possible_fqcn {
    my $str = shift;
    return ($str, $str) if ($str =~ /[^\w\.\d\(\)\{\},-]/);
    my @comps = split /\./, $str;
    return ($str, $str) if (@comps < 4);
    my $invalid = 0;
    for my $i (0..$#comps-2) {
        $invalid = 1 if ($comps[$i] =~ /[\(\)\{\},-]/);
    }
    return ($str, $str) if ($invalid);
    return ($str, $str) if ($comps[$#comps] =~ /[\(\)\{\},-]/);
    return (substr($str, 0, rindex($str, ".")), , $comps[$#comps]);
}

1;
