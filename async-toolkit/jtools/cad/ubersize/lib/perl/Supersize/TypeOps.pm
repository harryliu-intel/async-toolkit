#
# TypeOps
#
# Commands that implement various Supersize Type manipulation operations.
# 

package Supersize::TypeOps;

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
    NAME => "TypeOps",
    DESC => "Type Manipulation Operations.",
    COMMANDS => {
    "count" => {
        SUBREF => \&count,
        USAGE  => "count [--keys|values] " . underline("map") . " | " .
                         underline("list"),
        DESC   => 
          "Counts the number of elements in a map or list.  If the type is ".
          "a map, then the --keys or --values option may be specified, " .
          "indicating whether the number of keys or total number of values ". 
          "should be counted.  The default is to give both." },
    "keys" => {
        SUBREF => \&keys_cmd,
        USAGE  => "keys " . underline("map1") . " [ " . underline("map2") .
                         " ... ]",
        DESC   => 
          "Returns a list of all keys in the specified map(s).",
        RV => {
          result => { TYPE => $TYPE_LIST, DESC => "List of keys." } } },
    "values" => {
        SUBREF => \&values_cmd,
        USAGE  => "values " . underline("map1") . " [ " . underline("map2") .
                         " ... ]",
        DESC   => 
          "Returns a list of all values in the specified map(s).  Note: " .
          "redundant values are not filtered.",
        RV => {
          result => { TYPE => $TYPE_LIST, DESC => "List of keys." } } },
    "invert" => {
        SUBREF => \&invert,
        USAGE  => "invert " . underline("map"),
        DESC   => 
          "Returns the inverse of " . underline("map") . ".  That is, " .
          "if key1 -> val1 in the original map, then the key val1 in the " .
          "inverse map will include key1.  Result is returned as the " .
          "return variable invert.result.  The return map values are " .
          "always lists, even if the input map is one-to-one.",
        RV => { 
          "result" => {
            TYPE => $TYPE_MAP,
            DESC => "Inverted version of the input map." } } },
    "unique" => {
        SUBREF => \&unique,
        USAGE  => "unique " . underline("list1") . " [ " .  underline("list2") .
                          " ... ]",
        DESC   =>
          "Returns a concatenation of the specified lists with all repeated ".
          "elements removed.",
        RV => {
          "result" => {
            TYPE => $TYPE_LIST,
            DESC => "Resulting list containing only unique elements." } } },
    "multiply" => {
        SUBREF => \&multiply,
        USAGE  => "multiply " . underline("map1") . " " .
                                underline("map2") . " ...",
        DESC   =>
          "Multiplies map1::key_i by map2::key_i, for all matching key_i. " .
          "Removes all key_i in map1 that do not exist in map2 (and vice " .
          "versa).  Returns the multiplied map in multiply.result.  Note: ",
        RV => {
          "result" => {
            TYPE => $TYPE_MAP,
            DESC => "Resulting value-multiplied map return value." } } },
    "to_map" => {
        SUBREF => \&to_map,
        USAGE  => "to_map [--delimiter=" . underline("d") . "] " .
                         "[--key-delimiter=" . underline("kd") . "] " . 
                         "[--append|accumulate] " .
                         "[--default-value=" . underline("def_s") . "|" .
                                               underline("def_l") . "]" .
                         underline("list"),
        DESC   => 
          "Attempts to convert " . underline("list") . " into a map by " .
          "interpreting each list element as a key followed by some number ".
          "of values associated with that key.  The --delimiter argument " .
          "specifies the delimiter between key and value elements (whitespace ".
          "by default).  A different delimiter between the key and value list ".
          "may be specified with --key-delimiter.  The delimiter arguments " .
          "are interpreted as Perl regular expressions.  Since the default " .
          "delimiters are whitespace (\\s+), list elements will need to be ".
          "quoted.  The generated map is available in the to_map.result " .
          "return variable.\n\n" .
          "An example:\n\n" .
          "  < \"key1 value1\" \"key2 value2\" >\n\n" .
          "will produce to_map.result::key1 = value1 and " .
          "to_map.result::key2 = value2.\n\n" .
          "The --append-values option will cause the values of any key which " .
          "previously appears in the list to append rather than overwrite " .
          "the previous key's mapping.  With this option, the following " .
          "lists produce the same output map:\n\n" .
          "  < \"key value1\" \"key value2\" >\n" .
          "  < \"key value1 value2\" >\n\n" .
          "Without the --append-values option, the first list will generate ".
          "a map with to_map.result::key = value1.\n",
        RV => { 
          "result" => {
            TYPE => $TYPE_MAP,
            DESC => "Generated map." } } },
    "filter" => {
        SUBREF => \&filter,
        USAGE  => "filter [--values] \"".underline("filter-expr")."\" ".
                          underline("map")."|".underline("list"),
        DESC   => 
          "Filters elements of a map or list.  The filter expression may ".
          "be any valid Perl expression evaluating to a 0 or 1.  It may ".
          "reference the variables \"\$k\" or \"\$v\" if operating on a ".
          "map (corresponding to the key and each value element that the ".
          "key maps to), or \"\$e\" if operating on a list (corresponding ".
          "to each list element.)  If the expression evaluates to 1, the ".
          "key, value, or list element will be retained; if it evaluates to ".
          "0, it will be filtered out.\n\n".
          "By default, when operating on maps, the keys are filtered.  In ".
          "order for a key to survive, all of its values must evaluate to ".
          "1.  If the --values argument is specified, then individual ".
          "values will be filtered if they evaluate to 0.  In this mode, ".
          "if all particular key's values are filtered out, then the key ".
          "will be filtered out.",
        RV => { 
          "result" => {
            TYPE => $TYPE_MAP,
            DESC => "Filtered return value." } } },
    }
  };
}

#
# count
#
sub count {
    my $SS_r = shift;

    # Parse arguments
    my $case = -1;
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($case==-1 && $val eq "--keys") {
                $case = 0;
            }
            elsif ($case==-1 && $val eq "--values") {
                $case = 1;
            }
            else {
                command_die($SS_r, "Bad count argument '$val'.");
            }
        }
        elsif ($type == $TYPE_LIST) {
            if ($case != -1) {
                command_die($SS_r, "A map must accompany --keys/values.");
            }
            if (num_args(\@_) > 0) {
                command_die($SS_r, "Too many arguments to count.");
            }
            print scalar(@{$val}) . "\n";
            return;
        }
        elsif ($type == $TYPE_MAP) {
            if (num_args(\@_) > 0) {
                command_die($SS_r, "Too many arguments to count.");
            }
            my $num_keys = scalar(keys(%{$val}));
            if ($case == 0) {
                print "$num_keys\n";
                return;
            }
            my $num_values;
            foreach my $k (keys (%{$val})) {
                if ($val->{$k}[0] == $TYPE_SCALAR) {
                    $num_values++;
                }
                else {
                    $num_values += scalar(@{$val->{$k}[1]});
                }
            }
            if ($case == 1) {
                print "$num_values\n";
            }
            else {
                print "keys:    $num_keys\n";
                print "values:  $num_values\n";
            }
            return;
        }
        else {
            command_die($SS_r, "Bad argument type.");
        }
    }
    command_die($SS_r, "Must provide a map or list type.");
}


#
# keys
#
sub keys_cmd {
    my $SS_r = shift;
    my @keylist;
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type != $TYPE_MAP) {
            command_die($SS_r, "Bad map type specified.");
        }
        push @keylist, keys(%{$val});
    }
    set_cmd_return_variable($SS_r, "result", [ $TYPE_LIST, \@keylist ]);
    print list_to_string(\@keylist) . "\n";
}

#
# values
#
sub values_cmd {
    my $SS_r = shift;
    my @valuelist;
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type != $TYPE_MAP) {
            command_die($SS_r, "Bad map type specified.");
        }
        foreach my $k (keys %{$val}) {
            if ($val->{$k}[0] == $TYPE_SCALAR) {
                push @valuelist, $val->{$k}[1];
            }
            elsif ($val->{$k}[0] == $TYPE_LIST) {
                push @valuelist, @{$val->{$k}[1]};
            }
        }
    }
    set_cmd_return_variable($SS_r, "result", [ $TYPE_LIST, \@valuelist ]);
    print list_to_string(\@valuelist) . "\n";
}

#
# invert
#
sub invert {
    my $SS_r = shift;

    # parse args
    my ($type, $map_r) = shift_next_arg(\@_);
    if (!defined $type || $type != $TYPE_MAP) {
        command_die($SS_r, "Bad map type specified.");
    }
    command_die($SS_r, "Too many arguments to invert.") if (num_args(\@_));

    # invert the map
    my $inverted_map_r = {};
    foreach my $k (keys %{$map_r}) {
        my $list_r = [];
        if ($map_r->{$k}[0] == $TYPE_SCALAR) {
            push @{$list_r}, $map_r->{$k}[1];
        }
        elsif ($map_r->{$k}[0] == $TYPE_LIST) {
            $list_r = $map_r->{$k}[1];
        }
        foreach my $v (@{$list_r}) {
            if (!exists $inverted_map_r->{$v}) {
                $inverted_map_r->{$v} = [ $TYPE_LIST, [$k] ];
            }
            else {
                push @{$inverted_map_r->{$v}[1]}, $k;
            }
        }
    }
    set_cmd_return_variable($SS_r, "result", [ $TYPE_MAP, $inverted_map_r ]);
}

#
# multiply
#
sub multiply {
    my $SS_r = shift;

    # multiply all specified maps
    my %result;
    my $first = 1;
    while (num_args(\@_)) {
        my ($type, $map_r) = shift_next_arg(\@_);
        if ($type != $TYPE_MAP) {
            command_die($SS_r, "Bad map type specified to multiply.");
        }
        my %hit;
        foreach my $k (keys %{$map_r}) {
            if ($map_r->{$k}[0] != $TYPE_SCALAR) {
                command_die($SS_r, "Maps to list values not supported.");
            }
            if ($first) {
                $result{$k} = [ $TYPE_SCALAR, $map_r->{$k}[1] ];
            }
            elsif (exists $result{$k}) {
                $result{$k}->[1] *= $map_r->{$k}[1];
                $hit{$k} = 1;
            }
        }
        if (!$first) {
            foreach my $k (keys %result) {
                delete $result{$k} if (!exists $hit{$k});
            }
        }
        $first = 0;
    }
    set_cmd_return_variable($SS_r, "result", [ $TYPE_MAP, \%result ]);
    print "Multiplied " . scalar(keys(%result)) . " values.\n";
}

#
# unique
#
sub unique {
    my $SS_r = shift;
    my %elements;
    my $mode;
    my $i = 0;
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            command_die($SS_r, "Bad list argument: $val");
        }
        elsif ($type == $TYPE_LIST && (!defined $mode || $mode==$TYPE_LIST)) {
            foreach my $e (@{$val}) {
                if (!exists $elements{$e}) {
                    $elements{$e} = $i;
                    $i++;
                }
            }
        }
        elsif ($type == $TYPE_LIST) {
            command_die($SS_r, "Can't mix list and map types.");
        }
        elsif ($type == $TYPE_MAP) {
            if (defined $mode) {
                command_die($SS_r, "Can't specify multiple map types.");
            }
            print "Sorry, unique isn't yet implemented for map types.\n";
            return;
            $mode = $type;
        }
    }
    my @result = sort { $elements{$a} <=> $elements{$b} } keys %elements;

    # print result to stdout
    print "< ";
    foreach my $e (@result) {
        print quote_scalar($e) . " ";
    }
    print ">\n";

    # store result in return variable
    set_cmd_return_variable($SS_r, "result", [ $TYPE_LIST, \@result ] );
}

#
# to_map
#
sub to_map {
    my $SS_r = shift;
    my $list_r;
    my $vdelim = "\\s+";
    my $kdelim;
    my $append_values = 0;
    my $accum_values  = 0;
    my $default_type;
    my $default_value;

    # parse args
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($val eq "--delimiter") {
                my $eq = shift_next_scalar(\@_);
                $vdelim = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $vdelim) {
                    command_die($SS_r, "Bad value specified to --delimiter ".
                                       "argument.");
                }
            }
            elsif ($val eq "--key-delimiter") {
                my $eq = shift_next_scalar(\@_);
                $kdelim = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $kdelim) {
                    command_die($SS_r, "Bad value to --key-delimiter " .
                                       "argument.");
                }
            }
            elsif ($val eq "--append") {
                $append_values = 1;
            }
            elsif ($val eq "--accumulate") {
                $accum_values = 1;
            }
            elsif ($val eq "--default-value") {
                my $eq = shift_next_scalar(\@_);
                ($default_type, $default_value) = shift_next_arg(\@_);
                if (!defined $eq || $eq ne "=" ||
                    $default_type != $TYPE_SCALAR && 
                    $default_type != $TYPE_LIST) {
                    command_die($SS_r, "Invalid default value type.");
                }
            }
            else {
                command_die($SS_r, "Unrecognized argument $val specified.");
            }
        }
        elsif ($type == $TYPE_MAP) {
            command_die($SS_r, "Bad argument (map type) specified to to_map.");
        }
        elsif ($type == $TYPE_LIST && defined $list_r) {
            command_die($SS_r, "Can't specify multiple lists to to_map.");
        }
        elsif ($type == $TYPE_LIST) {
            $list_r = $val;
        }
        else {
            command_die($SS_r, "Bad argument to to_map.");
        }
    }
    if ($append_values && $accum_values) {
        command_die($SS_r, "Arguments --append-values and --accumulate-values ".
            "are mutually exclusive.");
    }

    # convert list
    $kdelim = $vdelim if (!defined $kdelim);
    my $map_r = list_to_map($list_r, $kdelim, $vdelim, $append_values, 
                            $accum_values, 0, $default_type, $default_value);
    set_cmd_return_variable($SS_r, "result", [ $TYPE_MAP, $map_r ]);
}

#
# filter
#
sub filter {
    my $SS_r = shift;
    my ($type, $src_r, $dest_r, $expr);
    my $mode = 0;                          # filter keys (if type==TYPE_MAP)
    my $il_r;

    # parse args
    while (num_args(\@_)) {
        my ($t, $v) = shift_next_arg(\@_);
        if ($t == $TYPE_SCALAR) {
            if ($v eq "--values") {
                $mode = 1;
            }
            elsif ($v eq "--intersect") {
                my $eq = shift_next_scalar(\@_);
                (my $il_t, $il_r) = shift_next_arg(\@_);
                if (!defined $eq || $eq ne "=" || !defined $il_t ||
                    $il_t != $TYPE_LIST) {
                    command_die($SS_r, "Bad value to argument $v specified.");
                }
            }
            elsif (defined $expr) {
                command_die($SS_r, "Too many filter arguments.");
            }
            else {
                $expr = $v;
            }
        }
        elsif (($t == $TYPE_LIST || $t == $TYPE_MAP) && defined $type) {
            command_die($SS_r, "Too many filter arguments.");
        }
        elsif ($t == $TYPE_LIST || $t == $TYPE_MAP) {
            $type = $t; $src_r = $v;
        }
        else {
            command_die($SS_r, "Bad argument passed to filter.");
        }
    }
    if (!defined $expr && !defined $il_r || !defined $type) {
        command_die($SS_r, "Insufficient arguments to filter.");
    }
    $expr = "1" if (!defined $expr);

    # Filter the list or map
    if ($type == $TYPE_LIST) {
        if ($expr =~ /\$[^e]/ || $expr =~ /\$e\w/) {
            command_die($SS_r, "May only reference \$e in the filter ".
                               "expression.");
        }
        $dest_r = [];
        foreach my $e (@{$src_r}) {
            my $pass = eval $expr;
            if ($@) {
                chomp $@;
                command_die($SS_r, "Bad filter expression:\n$@");
            }
            if (sprintf("%d",$pass) !~ /^[01]$/) {
                command_die($SS_r, "Non-boolean filter expression specified.");
            }
            if (defined $il_r) {
                $pass &= grep { $_ eq $e } @{$il_r};
            }
            push @{$dest_r}, $e if ($pass);
        }
    }
    else {
        if ($expr =~ /\$[^kv]/ || $expr =~ /\$[kv]\w/) {
            command_die($SS_r, "May only reference \$k or \$v in the filter ".
                               "expression.");
        }
        $dest_r = {};
        foreach my $k (keys %{$src_r}) {
            my $vtype = $src_r->{$k}[0];
            my $vlist = $vtype == $TYPE_SCALAR ? [$src_r->{$k}[1]] : 
                                                 $src_r->{$k}[1];
            my $kpass = 1;
            foreach my $v (@{$vlist}) {
                my $vpass = eval $expr;
                if ($@) {
                    chomp $@;
                    command_die($SS_r, "Bad filter expression:\n$@");
                }
                if (sprintf("%d",$vpass) !~ /^[01]$/) {
                    command_die($SS_r, "Non-boolean filter expression ".
                                       "specified.");
                }
                if ($mode==1 && $vpass==1) {
                    $vpass = grep { $_ eq $v } @{$il_r} if (defined $il_r);
                    if ($vtype == $TYPE_SCALAR) {
                        $dest_r->{$k} = [ $TYPE_SCALAR, $v ];
                    }
                    else {
                        if (!exists $dest_r->{$k}) {
                            $dest_r->{$k} = [ $TYPE_LIST, [] ];
                        }
                        push @{$dest_r->{$k}[1]}, $v;
                    }
                }
                elsif ($mode==0) {
                    $kpass &= $vpass;
                }
            }
            if ($mode==0 && $kpass==1) {
                next if (defined $il_r && !grep { $_ eq $k } @{$il_r});
                if ($vtype == $TYPE_SCALAR) {
                    $dest_r->{$k} = [ $TYPE_SCALAR, $vlist->[0] ];
                }
                else {
                    $dest_r->{$k} = [ $TYPE_LIST, [] ];
                    push @{$dest_r->{$k}[1]}, @{$vlist};
                }
            }
        }
    }

    # Return result
    set_cmd_return_variable($SS_r, "result", [ $type, $dest_r ]);
}
            
                


1;
