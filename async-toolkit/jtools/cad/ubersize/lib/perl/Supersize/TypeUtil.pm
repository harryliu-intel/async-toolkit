#
# TypeUtil
#
# Supersize type (command/scalar/list/map) manipulation utilities.  Includes
# functions for
#
#   - Accessing commands from the Supersize module table.
#   - Accessing arguments passed to commands
#   - Creating new Supersize types from native perl types.
#   - Accessing command-scoped input/return variables.
#
# Also defines the very important constants TYPE_SCALAR, TYPE_LIST,
# TYPE_MAP, and TYPE_ERROR.
#

package Supersize::TypeUtil;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        $TYPE_SCALAR
        $TYPE_LIST
        $TYPE_MAP
        $TYPE_ERROR
        &type_name_to_string
        &num_args
        &shift_next_arg
        &shift_next_scalar
        &next_scalar
        &shift_next_list
        &shift_next_map
        &shift_next_list_of_maps
        &list_to_string
        &type_to_string
        &list_to_map
        &new_scalar
        &new_list
        &new_map_to_scalar
        &new_map_to_list
        &dereference_variable
        &dereference_scalar
        &quote_scalar
        &is_scalar
        &is_defined
        &cmd_input_variable_type
        &get_cmd_input_variable
        &get_cmd_input_variables
        &get_cmd_input_scalar
        &set_cmd_input_variable
        &cmd_return_variable_type
        &get_cmd_return_variable
        &set_cmd_return_variable
        &lookup_command
    );
}

use strict;

#
# Supersize type constants
#
our $TYPE_SCALAR = 0;
our $TYPE_LIST   = 1;
our $TYPE_MAP    = 2;
our $TYPE_ERROR  = 3;

######################## Command Argument Functions ########################

#
# Given a command argument list (reference), returns number of arguments
#
sub num_args {
    return @{$_[0]}/2;
}

#
# Given a command argument list reference, shifts the next argument, 
# returned as a (type, value) pair.  See parse_command_args in supersize
# for more info about the (type, value) pair.
#
sub shift_next_arg {
    my $type = shift @{$_[0]};
    my $arg  = shift @{$_[0]};
    return ($type, $arg);
}

#
# Given a command argument list reference, shifts the next argument, 
# assuming it's a scalar.  Returns the argument value if the next argument 
# is indeed a scalar type.  Otherwise, outputs a terse error message and
# returns "".
#
sub shift_next_scalar {
    my $type = shift @{$_[0]};
    my $arg  = shift @{$_[0]};
    return undef if (!defined $type);
    if ($type==$TYPE_SCALAR) {
        return $arg;
    }
    elsif ($type==$TYPE_LIST) {
        print "Unexpected list argument.\n";
        return "";
    }
    elsif ($type==$TYPE_MAP) {
        print "Unexpected map argument.\n";
        return "";
    }
    elsif ($type==$TYPE_ERROR) {
        print "$arg\n";
        return "";
    }
    else {
        # Unknown type (illegal state)
        return;
    }
}

# Returns same value as shift_next_scalar, but doesn't shift the argument.
# If the next argument isn't a scalar, returns "".
sub next_scalar {
    my $type = ${$_[0]}[0];
    my $arg  = ${$_[0]}[1];
    return "" if (!defined $type || $type != $TYPE_SCALAR);
    return $arg;
}
    
#
# Given a command argument list reference, shifts the next argument, 
# interpreting it as a list.  If the next argument is a scalar, returns
# a length-one list reference containing the scalar.  Otherwise returns
# the list reference.  If the argument is invalid (TYPE_ERROR), returns an 
# empty list.
#
sub shift_next_list {
    return undef if (!defined $_[0] || !@{$_[0]});
    my $type = shift @{$_[0]};
    my $arg  = shift @{$_[0]};
    if ($type==$TYPE_SCALAR) {
        return [$arg];
    }
    elsif ($type==$TYPE_LIST) {
        return $arg;
    }
    elsif ($type==$TYPE_MAP) {
        print STDERR "Unexpected map argument.\n";
        return;
    }
    elsif ($type==$TYPE_ERROR) {
        print STDERR "$arg\n";
        return;
    }
    else {
        # Unknown type (illegal state)
        return;
    }
}

#
# Given a command argument list reference, shifts the next argument, 
# assuming it's a map.  If the argument is actually a scalar or a list,
# or if the argument is invalid (TYPE_ERROR), a null hash is returned.
#
sub shift_next_map {
    return undef if (!defined $_[0] || !@{$_[0]});
    my $type = shift @{$_[0]};
    my $arg  = shift @{$_[0]};
    if ($type==$TYPE_SCALAR) {
        print STDERR "Unexpected scalar argument.\n";
        return;
    }
    elsif ($type==$TYPE_LIST) {
        print STDERR "Unexpected list argument.\n";
        return;
    }
    elsif ($type==$TYPE_MAP) {
        return $arg;
    }
    elsif ($type==$TYPE_ERROR) {
        print STDERR "$arg\n";
        return [];
    }
    else {
        # Unknown type (illegal state)
        return;
    }
}

#
# Given a command list argument of < mapref_1 mapref_2 ... >, where each
# mapref_i are map variable names, returns a perl list of references to
# the (supersize-)dereferenced map types.  If the next argument is a map,
# not a list, then that map will be returned.
#
sub shift_next_list_of_maps {
    my $SS_r = shift;
    return if (!defined $_[0] || !@{$_[0]});
    my $type = shift @{$_[0]};
    my $arg  = shift @{$_[0]};
    my @map_list;
    if ($type==$TYPE_SCALAR) {
        print STDERR "Unexpected scalar argument.\n";
        return;
    }
    elsif ($type == $TYPE_LIST) {
        foreach my $var (@{$arg}) {
            my ($t, $v) = dereference_variable($SS_r, $var);
            if ($t != $TYPE_MAP) {
                print STDERR "Expecting map variable reference, got " .
                    type_name_to_string($t) . " $var.\n";
                return;
            }
            push @map_list, $v;
        }
    }
    elsif ($type == $TYPE_MAP) {
        push @map_list, $arg;
    }
    return @map_list;
}

############################ Basic Type Functions ############################

# Formats a \@ list reference to Supersize string format (< a b c >).
sub list_to_string {
    my $list_ref = shift;
    my $str = "< ";
    foreach my $e (@{$list_ref}) {
        $str .= quote_scalar($e) . " ";
    }
    return $str .= ">";
}

# Prints a scalar, list, or map argument
sub type_to_string {
    my $type = shift;
    my $val  = shift;
    if (!defined $type || !defined $val) {
        return "Undefined";
    }
    elsif ($type == $TYPE_SCALAR) {
        return quote_scalar($val);
    }
    elsif ($type == $TYPE_LIST) {
        return list_to_string $val;
    }
    elsif ($type == $TYPE_MAP) {
        my $str = "";
        foreach my $k (keys %{$val}) {
            $str .= "::$k = " . type_to_string(@{$val->{$k}}) . "\n";
        }
        return $str;
    }
    else {
        # invalid variable
        return $val;
    }
}

sub quote_scalar {
    my ($val) = @_;
    if ($val =~ /\s/) {
        return "\"$val\"";
    }
    else {
        return $val;
    }
}

# Attempts to convert a list of elements (perl ref) to a map (perl ref) by 
# interpreting each list element as 
#
#   "key/kdelim/val1/vdelim/val2/vdelim/..."
#
# If all list elements have only a single value, then a map to supersize 
# scalar types will be created; otherwise a map to supersize list types 
# will be created.  (Setting $map_to_list will also force the latter.)
#
# When keys are repeated in the list, the $append_values and $accum_values
# variables come into play.  With both 0, the value of a repeated key
# will overwrite the prior value.  With $append_values==1, each value is
# forced to be a list (overriding $map_to_list==1) and subsequent values
# will be pushed onto the earlier key's value list.  With $accum_values==1,
# subsequent values will be -added- to the earlier key's values.  For example:
#
#   a 1 2
#   a 2 3
#
# produces a final map ::a = <3 5>.  It's illegal to specify both append_values
# and accum_values.
#
# Any lines that do not match the above pattern will be ignored unless 
# ($def_type, $def_value) are set, in which case the line in its entirety 
# will be treated as a key and will be mapped to ($def_type, $def_value).
sub list_to_map {
    my $list_r        = shift;
    my $kdelim        = shift;
    my $vdelim        = shift;
    my $append_values = shift;
    my $accum_values  = shift;
    my $map_to_list   = shift;    # force map-to-list type
    my $def_type      = shift;
    my $def_value     = shift;

    $map_to_list = 1 if (defined $def_type && $def_type == $TYPE_LIST);

    # Construct perl map
    my %p_map;
    foreach my $e (@{$list_r}) {
        my @values;
        # non-greedy term in next regex causes $1/$2 not to be reset;
        # workaround:
        $e =~ //;   
        if ($e =~ /^(.+?)$kdelim(.*)$/ || 
            defined $def_type && defined $def_value) {
            my $key;
            if (defined $1 && $1 ne "" && defined $2 && $2 ne "") {
                $key = $1;
                @values = split /$vdelim/, $2;
            }
            else {
                $key = $e;
                if ($def_type == $TYPE_SCALAR) {
                    push @values, $def_value;
                }
                else {
                    push @values, @{$def_value};
                }
            }
            if (@values) {
                $map_to_list = 1 if (@values > 1 || $append_values);
                if (exists $p_map{$key} && $append_values) {
                    push @{$p_map{$key}}, @values;
                }
                elsif (exists $p_map{$key} && $accum_values) {
                    for my $i (0..$#values) {
                        if (!defined $p_map{$key}->[$i]) {
                            print STDERR "Warning: Inconsistent list value " .
                                "length detected for key $key.\n";
                            $p_map{$key}->[$i] = 0;
                        }
                        $p_map{$key}->[$i] += $values[$i];
                    }
                }
                else {
                    $p_map{$key} = \@values;
                }
            }
        }
        if (!@values) {
            print STDERR "Warning: Couldn't identify a key/value element ".
                         "in line\n         $e\n";
        }
    }

    # Convert to Supersize map of the appropriate type
    my $ss_map_r = {};
    foreach my $key (keys %p_map) {
        if ($map_to_list || $append_values) {
            $ss_map_r->{$key} = [ $TYPE_LIST, $p_map{$key} ];
        }
        else {
            $ss_map_r->{$key} = [ $TYPE_SCALAR, $p_map{$key}[0] ];
        }
    }
    return $ss_map_r;
}

# Creates a supersize scalar from a perl scalar
sub new_scalar {
    my ($val) = @_;
    return [ $TYPE_SCALAR, $val ];
}

# Creates a supersize list from a perl list (passed by reference)
# (Note: multiple list references can be passed and they'll all be included
# in the supersize list.)
sub new_list {
    my $list_r = shift;
    while (@_) {
        my $lr = shift;
        foreach my $l (@{$lr}) {
            push @{$list_r}, $l;
        }
    }
    return [ $TYPE_LIST, $list_r ];
}

# Creates a supersize map (scalar::scalar) from a perl hash.
sub new_map_to_scalar {
    my ($hash_r) = @_;
    my $map_r = {};
    foreach my $k (%{$hash_r}) {
        $map_r->{$k} = [ $TYPE_SCALAR, $hash_r->{$k} ];
    }
    return [ $TYPE_MAP, $map_r ];
}

# Creates a supersize map (scalar::list) from a perl hash
sub new_map_to_list {
    my ($hash_r) = @_;
    my $map_r = {};
    foreach my $k (%{$hash_r}) {
        $map_r->{$k} = [ $TYPE_LIST, $hash_r->{$k} ];
    }
    return [ $TYPE_MAP, $map_r ];
}

############################ Variable Utilities #############################

# Dereferences a fully-substituted variable reference.  Supported forms:
# \w+ (simple scalar, list, or map reference) and \w+::\w+ (map element)
sub dereference_variable {
    my ($SS_r, $var) = @_;
    # First check for map element
    if ($var =~ /^(\w+)::([\w\.\[\]\(\)\{\},-]+)$/) {
        if (exists $SS_r->{GM}{$1}) {
            if (exists $SS_r->{GM}{$1}{$2}) {
                return @{$SS_r->{GM}{$1}{$2}};
            }
            else {
                return ( $TYPE_ERROR, "Undefined map element $1::$2." );
            }
        }
        else {
            return ( $TYPE_ERROR, "Unknown map $1." );
        }
    }
    elsif ($var =~ /^\w+$/) {
        if (exists $SS_r->{GS}{$var}) {
            return ( $TYPE_SCALAR, $SS_r->{GS}{$var} );
        }
        elsif (exists $SS_r->{GL}{$var}) {
            return ( $TYPE_LIST, $SS_r->{GL}{$var} );
        }
        elsif (exists $SS_r->{GM}{$var}) {
            return ( $TYPE_MAP, $SS_r->{GM}{$var} );
        }
        else {
            return ( $TYPE_ERROR, "Undefined variable $var." );
        }
    }
    elsif ($var =~ /^([\w\/]+)\.(\w+)$/) {
        my ($type, $val) = get_cmd_input_variable($SS_r, $2, $1);
        return ($type, $val) if (defined $type);
        ($type, $val) = get_cmd_return_variable($SS_r, $2, $1);
        return ($type, $val) if (defined $type);
        return ( $TYPE_ERROR, "Undefined variable $var." );
    }
    else {
        return ( $TYPE_ERROR, "Invalid variable reference $var." );
    }
}

# Returns the scalar value of the specified supersize variable.  Supports
# all supersize variable forms (var, mapvar::key, cmd.var, etc.)  Returns
# undef if the variable isn't defined or isn't a scalar.
sub dereference_scalar {
    my ($SS_r, $var) = @_;
    my @value = dereference_variable($SS_r, $var);
    if ($value[0] == $TYPE_SCALAR) {
        return $value[1];
    }
    else {
        return undef;
    }
}

# Determines whether specified variable reference is a scalar.  Supports
# all supersize variable forms: var, mapvar::key, cmd.var, etc.  Returns
# false if the variable isn't defined.
sub is_scalar {
    my ($SS_r, $var) = @_;
    my @value = dereference_variable($SS_r, $var);
    return $value[0] == $TYPE_SCALAR;
}

# Determines whether the specified variable reference is defined.
sub is_defined {
    my ($SS_r, $var) = @_;
    my @value = dereference_variable($SS_r, $var);
    return $value[0] != $TYPE_ERROR;
}

# Returns a string (e.g. "scalar") description of the supersize type name
sub type_name_to_string {
    my $type = shift;
    if ($type eq $TYPE_SCALAR) {
        return "Scalar";
    }
    elsif ($type eq $TYPE_LIST) {
        return "List";
    }
    elsif ($type eq $TYPE_MAP) {
        return "Map";
    }
    elsif ($type eq $TYPE_ERROR) {
        return "Invalid";
    }
    else {
        return "Unknown Type";
    }
}

##################### Command-Scoped Variable Utilities ######################

# Simple utilities to save tedious typing, and maybe to provide some
# modularity
sub cmd_input_variable_type {
    my ($SS_r, $iv, $cmd) = @_;
    if (!defined $cmd) {
        $cmd = ${$SS_r->{IGV}{cmd_stack}}[$#{$SS_r->{IGV}{cmd_stack}}];
    }
    my $cmd_r = (lookup_command($SS_r, $cmd))[0];
    if (exists $cmd_r->{IV} && defined $cmd_r->{IV}{$iv}) {
        if (!defined $cmd_r->{IV}{$iv}{TYPE}) {
            return $TYPE_SCALAR;
        }
        else {
            return $cmd_r->{IV}{$iv}{TYPE};
        }
    }
    else {
        return undef;
    }
}           
 
sub get_cmd_input_variable {
    my ($SS_r, $iv, $cmd) = @_; 
    my $fqcmd;
    if (!defined $cmd) {
        $fqcmd = ${$SS_r->{IGV}{cmd_stack}}[$#{$SS_r->{IGV}{cmd_stack}}];
    }
    else {
        $fqcmd = (lookup_command($SS_r, $cmd))[1];
    }
    if (defined $fqcmd && exists $SS_r->{IV}{$fqcmd} && 
        exists $SS_r->{IV}{$fqcmd}{$iv}) {
        return @{$SS_r->{IV}{$fqcmd}{$iv}};
    }
    else {
        return undef;
    }
}

# return a list of defined input variables
sub get_cmd_input_variables {
    my ($SS_r, $cmd) = @_; 
    if (!defined $cmd) {
        $cmd = ${$SS_r->{IGV}{cmd_stack}}[$#{$SS_r->{IGV}{cmd_stack}}];
    }
    my $cmd_r = (lookup_command($SS_r, $cmd))[0];
    if (exists $cmd_r->{IV}) {
        return keys %{$cmd_r->{IV}};
    }
    else {
        return undef;
    }
}

sub get_cmd_input_scalar {
    my ($SS_r, $iv, $cmd) = @_; 
    my ($type, $val) = get_cmd_input_variable($SS_r, $iv, $cmd);
    if (defined $type && $type == $TYPE_SCALAR) {
        return $val;
    }
    else {
        return undef;
    }
}

# Assigns the specified command input variable to a provided
# Supersize type ([$TYPE, $VAL]).  Command can be specified either
# in an abbreviated or fully-qualified way.  Note: Can't use this
# to set individual (key,value) pairs of a map type, only the entire
# map.
sub set_cmd_input_variable {
    my $SS_r = shift;
    my $iv   = shift;
    my $cmd  = shift;
    my $fqcmd = (lookup_command($SS_r, $cmd))[1];
    $SS_r->{IV}{$fqcmd} = {} if (!$SS_r->{IV}{$fqcmd});
    $SS_r->{IV}{$fqcmd}{$iv} = shift;
}


sub cmd_return_variable_type {
    my ($SS_r, $rv, $cmd) = @_;
    my $cmd_r = (lookup_command($SS_r, $cmd))[0];
    if (exists $cmd_r->{RV} && defined $cmd_r->{RV}{$rv}) {
        if (!defined $cmd_r->{RV}{$rv}{TYPE}) {
            return $TYPE_SCALAR;
        }
        else {
            return $cmd_r->{RV}{$rv}{TYPE};
        }
    }
    else {
        return undef;
    }
}       

sub get_cmd_return_variable {
    my ($SS_r, $rv, $cmd) = @_;
    my ($cmd_r, $fqcmd) = lookup_command($SS_r, $cmd);
    if (defined $cmd_r && exists $cmd_r->{RV}{$rv} && 
        exists $cmd_r->{RV}{$rv}{TYPE}) {
        return @{$SS_r->{RV}{$fqcmd}{$rv}};
    }
    else {
        return undef;
    }
}

# Assigns the specified return variable of the active command to the
# specified Supersize type ([$TYPE, $VAL]).  Gets the command from
# the top of the command stack.
sub set_cmd_return_variable {
    my $SS_r  = shift;
    my $rv    = shift;
    my $val   = shift;
    my $fqcmd = shift;
    $fqcmd = ${$SS_r->{IGV}{cmd_stack}}[$#{$SS_r->{IGV}{cmd_stack}}]
        unless $fqcmd;
    $SS_r->{RV}{$fqcmd} = {} if (!$SS_r->{RV}{$fqcmd});
    $SS_r->{RV}{$fqcmd}{$rv} = $val;
}

############################# Command Utilities #############################

# Given a command name ("command" or "module/command" form), returns a
# list of ( cmd_ref, fully_qualified_command ), where cmd_ref points
# to the command's data structure within the module table and fully_
# qualified_command is of the form "module/command".
sub lookup_command {
    my $SS_r = shift;
    my $cmd  = shift;
    if ($cmd =~ /^(\w*)\/?(\w+)?$/) {
        if (defined $2) {
            return ($SS_r->{IGV}{MODULES}{$1}{COMMANDS}->{$2}, $cmd);
        }
        else {
            foreach my $mod (keys %{$SS_r->{IGV}{MODULES}}) {
                if (grep {$cmd eq $_}
                        keys %{$SS_r->{IGV}{MODULES}{$mod}{COMMANDS}}) {
                    return ($SS_r->{IGV}{MODULES}{$mod}{COMMANDS}{$cmd},
                            "$mod/$cmd");
                }
            }
        }
    }
    return undef;
}


1;
