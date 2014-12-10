# $Id: $
# $DateTime: $

package Pathalyze::Parser;

use strict;
use Pathalyze::Parameter;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &parse_path_spec
    );
}


#
# Parses path timing specification.  Adds to the spec_r structure:
#   CELLS ->
#       <cell> ->
#           PATHS -> 
#               <path_name> -> 
#                   INPUT    -> input_transition
#                   OUTPUT   -> output_transition
#                   SEQUENCE -> [ transition_list ]
#           PARAM -> 
#               <param_name> ->
#                   TYPE -> -1: unknown, old style
#                           0: equation:   t(IO) = t(REF) + d    [delay]
#                           1: constraint: t(IO) + d < t(REF)    [setup]
#                           2: constraint: t(IO) > t(REF) + d    [hold]
#                   REF  -> input_transition  [clock or delay input]
#                   IO   -> input_transition | output_transition
#                   EXPR -> expr_giving_delay_and_slew
#           PARAM_ORDER ->
#               [ param_name_list_as_ordered_in_spec_file ]
#   INFO ->
#       num_files     -> number of files parsed
#       num_paths     -> number of path definitions (non-expanded)
#       num_exp_paths -> number of expanded paths
#       num_params    -> total number of params defined
#
# Notes:
#   <path_name> - Of the form "pname[idx]:up" or "pname[idx]:dn", where
#       "pname" is the name given in the spec file.  The "[idx]:{up,dn}"
#       suffix is assigned by expanding the array variables and inferring
#       the path directionality.
#   transition - A string of the form "node+" or "node-", where "node"
#       is the unchecked node as written in the spec file, with index
#       variables substituted.
#   Param TYPE - If 0, a delay is specified between input transition(s) and
#       output transition(s).  FANIN and FANOUT both contain at least one
#       transition and at most two (of the same node).
#       If 1, FANIN will contain 
#   
#   New param syntax:
#       delay <path>;
#       setup_constraint <in_path> < <ref_path>;
#       hold_constraint <ref_path> < <in_path>;
#
#       where <path> can be a path name -or- a path definition as transitions
#       enclosed in '{' ... '}'.
#
#       The 'delay' type will create at least two parameters: 
#       'max_delay|<pname>' and 'min_delay|<pname>'.
#           
sub parse_path_spec {
    my $file           = shift;
    my $spec_r         = shift;
    my $top            = shift;

    my $cell_r;
    if (defined $top && $top ne "") {
        if (!exists $spec_r->{CELLS}{$top}) {
            $spec_r->{CELLS}{$top} = {
                PATHS => {},
                PARAM => {},
                PARAM_ORDER => []
            };
        }
        $cell_r = $spec_r->{CELLS}{$top};
    }

    open (SPEC, $file) || die "Couldn't read $file.\n";
    $spec_r->{INFO}{num_files}++;
    my $dir = "";
    if ($file =~ /^(.*)\/[^\/]+$/) {
        $dir = $1;
    }
    my $line = "";
    my $lnum = 0;
    my @include_files = ();
    my $unexp_path_r = {};
    while (<SPEC>) {
        $line .= $_; chomp $line;
        $line =~ s/\s*(\#.*)?$/ /;
        $lnum++;
        if ($line =~ s/\s*;\s*$//) {
            $line =~ s/^\s*//;
            if ($line =~ /^cell\s+([\w\(\).,]+)\s*$/) {
                $top = $1;
                if (!exists $spec_r->{CELLS}{$top}) {
                    $spec_r->{CELLS}{$top} = {
                        PATHS => {},
                        PARAM => {},
                        PARAM_ORDER => []
                    };
                }
                $cell_r = $spec_r->{CELLS}{$top};
            }
            elsif ($line =~ /^path\s+([\w\[.,:\]\-\+\/\*]+)\s*=\s*(.*)$/) {
                need_to_set_top($lnum, $file) if (!defined $top || $top eq "");
                $unexp_path_r = { INPUT => "", OUTPUT => "", SEQUENCE => [] };
                my $path_name = $1;
                my $path_def  = $2;
                if (path_already_defined($spec_r->{CELLS}{$top}{PATHS}, $path_name)) {
                    die "Duplicate definition of path $path_name in $file ".
                        "for cell $top.\n";
                }
                my @new_path_names = parse_path($cell_r->{PATHS}, $path_name, 
                                                $path_def);
                $spec_r->{INFO}{num_exp_paths} += scalar(@new_path_names);
                $spec_r->{INFO}{num_paths}++;
            }
            elsif ($line =~ /^param\s+([\w.]+)\s*=\s*(.*)$/) {
                need_to_set_top($lnum, $file) if (!defined $top || $top eq "");
                if (exists $cell_r->{PARAM}{$1}) {
                    die "Duplicate definition of parameter $1 in $file.\n";
                }
                $cell_r->{PARAM}{$1} = {
                    TYPE => -1,
                    REF  => "",
                    IO   => "",
                    EXPR => $2
                };
                push @{$cell_r->{PARAM_ORDER}}, $1;
                $spec_r->{INFO}{num_params}++;
            }
            elsif ($line =~ /^delay\s+(.*)\s*$/) {
                need_to_set_top($lnum, $file) if (!defined $top || $top eq "");
                my $dspec = $1;
                my @path_names = parse_inline_path_ref($spec_r, $top, $dspec);
                foreach my $p (@path_names) {
                    my $param_name = "delay|" . $p;
                    $cell_r->{PARAM}{"max_$param_name"} = {
                        TYPE => 0,
                        REF => $cell_r->{PATHS}{$p}{INPUT},
                        IO  => $cell_r->{PATHS}{$p}{OUTPUT},
                        EXPR => "max($p)"
                    };
                    $cell_r->{PARAM}{"min_${param_name}"} = {
                        TYPE => 0,
                        REF => $cell_r->{PATHS}{$p}{INPUT},
                        IO  => $cell_r->{PATHS}{$p}{OUTPUT},
                        EXPR => "min($p)"
                    };
                    push @{$cell_r->{PARAM_ORDER}}, 
                         ("max_$param_name", "min_$param_name");
                    $spec_r->{INFO}{num_params} += 2;
                }
            }
            elsif ($line =~ /^(setup|hold|constraint)\s+([^<>]*)([<>])\s*(.*)\s*$/) {
                need_to_set_top($lnum, $file) if (!defined $top || $top eq "");
                my $type = $1 eq "setup" || $1 eq "constraint" && $3 eq "<" ?
                           1 : 2;
                my ($iopath, $refpath);
                if ($1 eq "constraint" || $1 eq "setup" && $3 eq "<" ||
                                          $1 eq "hold"  && $3 eq ">") {
                    $iopath  = $2;
                    $refpath = $4;
                }
                else {
                    $iopath  = $4;
                    $refpath = $2;
                }
                $iopath  =~ s/\s*$//; 
                $refpath =~ s/\s*$//;
                my @io_pnames  = parse_inline_path_ref($spec_r, $top, $iopath);
                my @ref_pnames = parse_inline_path_ref($spec_r, $top, $refpath);
                # Note: in general it is possible for |@iopnames| > 1,
                # |@refpnames| > 1, and |@iopnames| != |@refpnames|.
                # The recommended use to avoid these ambiguity is for
                # |@refpnames|==1 (clk+ or clk- transition path).  However,
                # to be conservative for the other cases, the max of
                # @iopnames path delays is taken, minus min of @refpnames 
                # path delays.
                my $io_pbase  = get_common_base_name($cell_r->{PATHS}, 
                                                     @io_pnames);
                my $ref_pbase = get_common_base_name($cell_r->{PATHS},
                                                     @ref_pnames);
                my $io_pname_list  = list_to_string(@io_pnames);
                my $ref_pname_list = list_to_string(@ref_pnames);
                my $io  = get_common_path_input($cell_r->{PATHS}, @io_pnames);
                my $ref = get_common_path_input($cell_r->{PATHS}, @ref_pnames);
                if ($io eq "" || $ref eq "") {
                    die "Incompatible " . ($io eq "" ? "data" : "reference") .
                        " paths specified for constraint on line $lnum.\n";
                }
                my $constraint_name = ($type==1 ? "setup" : "hold") .
                                      "|${io_pbase}|${ref_pbase}";
                $cell_r->{PARAM}{$constraint_name} = {
                    TYPE => $type,
                    IO   => $io,
                    REF  => $ref,
                    EXPR => constraint_prop_expr(
                        ($type==1 ? \@io_pnames  : \@ref_pnames),
                        ($type==1 ? \@ref_pnames : \@io_pnames))
                    #EXPR => "max(" . 
                    #    ($type==1 ? list_to_string(@io_pnames) :
                    #                list_to_string(@ref_pnames)) . ") - min(" .
                    #    ($type==1 ? list_to_string(@ref_pnames) :
                    #                list_to_string(@io_pnames)) . ")"
                };
                push @{$cell_r->{PARAM_ORDER}}, $constraint_name;
                $spec_r->{INFO}{num_params}++;
            }
            elsif ($line =~ /^include\s+(\S+)$/) {
                if ($dir eq "" || $1 =~ /^\//) {
                    push @include_files, [ $1, $top ];
                }
                else {
                    push @include_files, [ "$dir/$1", $top ];
                }
            }
            elsif ($line !~ /^\s*$/) {
                warn "Unrecognized syntax in statement ending on line ".
                     "$lnum in $file\n";
            }
            if ($line =~ /\s+path\s+/ || $line =~ /\s+param\s+/ ||
                $line =~ /\s+include\s+/) {
                die "Error: Semicolon missing in statement ending on line " .
                    $lnum . " in $file\n";
            }
            $line = "";
        }
    }
    close SPEC;
    foreach my $inc (@include_files) {
        parse_path_spec($inc->[0], $spec_r, $inc->[1]);
    }
}


sub need_to_set_top {
    my $line = shift;
    my $file = shift;
    die "Error: Top-level cell needs to be defined for statement ending on ".
        "line $line of\n" .
        "       $file.\n";
}

#
# Check if a path with the same base name as '$path' has already been defined 
#
sub path_already_defined {
    my $paths_r = shift;
    my $path    = shift;
    (my $path_base = $path) =~ s/\[.*//;
    foreach my $p (keys %{$paths_r}) {
        (my $p_base = $p) =~ s/\[.*//;
        return 1 if ($path_base eq $p_base);
    }
    return 0;
}

#
# Returns all expanded path names matching the base name pbase
#
sub get_matching_path_names {
    my $paths_r = shift;
    my $pname   = shift;

    my @matching_paths = ();
    foreach my $p (keys %{$paths_r}) {
        (my $pbase1 = $p) =~ s/:.*$//;
        (my $pbase2 = $pbase1) =~ s/\[.*$//;
        if ($pbase1 eq $pname || $pbase2 eq $pname || $p eq $pname) {
            push @matching_paths, $p;
        }
    }
    return @matching_paths;
}

#
# Given a path name & index range (pname_range), parses an unexpanded path 
# (path_def) of the form
#
#   [(initial_node)[-+]] node1[+-] node2[+-] ... nodeN[-+]
#
# Adds expanded path definitions to the path_r structure:
#   <name> -> INPUT -> initial_node{+-}
#             OUTPUT -> nodeN{-+}
#             SEQUENCE -> [ node1{-+} node2{+-} ... nodeN{-+}
#
# The base path name can be "", in which case a base name will be constructed
# as "INPUT;OUTPUT[;num]", where ";num" (starting at ";2") will be appended
# if "INPUT;OUTPUT" is already defined.  Note: If no initial_node is provided
# (NOT recommended), then "INPUT" will be "".
#
# The fully expanded <name> is constructed as
#   <base_name>['['index_list']']:{up|dn}
# where "up" or "dn" is chosen based on the directionality of the final
# transition.
#
# Returns list of path names that were created for this path definition.
#
sub parse_path {
    my $path_r      = shift;
    my $pname_range = shift;
    my $path_def    = shift;

    my @path_trans = split /\s+/, $path_def;
    die "$pname_range has empty path definition." if (!@path_trans);

    # Establish directionality of path (bidir, +, or -)
    my @dir;
    push @dir, 1 if ($path_trans[$#path_trans] !~ /\-$/);
    push @dir, 0 if ($path_trans[$#path_trans] !~ /\+$/);

    # Set input & output nodes
    my $input = "";
    if ($path_trans[0] =~ /^\((.*)\)[-+]?$/) {
        $input = $1;
        shift @path_trans;
    }
    my $output;
    if (!@path_trans) {
        # empty path... that's okay (as long as an input node is specified)
        $output = "";
    }
    else {
        ($output = $path_trans[$#path_trans]) =~ s/[-+]$//;
    }

    (my $pbase = $pname_range) =~ s/\[.*$//;
    if ($pbase eq "") {
        $pbase = get_next_path_base_name($path_r, $input, $output);
    }
    my @exp_path_names = ();
    if ($pname_range =~ /^[^\[]*$/) {
        foreach my $d (@dir) {
            push @exp_path_names, orient_and_add_path($path_r, $pbase, $d, 
                                                      $input, \@path_trans);
        }
    }
    elsif ($pname_range =~ /^([^\[]*)\[([^\]]+)\]$/) {
        # pbase[i:li..hi,j:lj..hj,...]
        my @pranges = split(/,/,$2);
        # Determine variable names and lo/hi bounds of each loop index
        my $numpaths = 1;
        my @ranges = ();
        foreach my $prange (@pranges) {
            if ($prange =~ /^(\w+):([\d\-\+\/\*\%]+)\.\.([\d\-\+\/\*\%]+)$/) {
                my $lo = eval $2;
                my $hi = eval $3;
                push @ranges, [ $1, $lo, $hi ];
                $numpaths *= ($hi-$lo+1);
            }
            elsif ($prange =~ /^(\w+):([\d\-\+\/\*\%]+)$/) {
                my $num = eval $2;
                push @ranges, [ $1, 0, $num-1 ];
                $numpaths *= $num;
            }
            else {
                warn "Bad path range in $pname_range. Skipping.\n";
            }
        }
        # Expand paths
        for my $i (0..$numpaths-1) {
            my $divisor = 1;
            my @expath = ($input, @path_trans);
            my $expelem = "[";
            for my $range (@ranges) {
                my ($var,$lo,$hi) = @{$range};
                my $val = $lo + ($i/$divisor)%($hi-$lo+1);
                $divisor *= ($hi-$lo+1);
                reassign_path_indices(\@expath,\&substitute_path_variable,
                                      $var,$val);
                $expelem .= "$val,";
            }
            reassign_path_indices(\@expath,\&evaluate_path_indices);
            $expelem =~ s/,$/\]/;
            my $exp_input = shift @expath;
            foreach my $d (@dir) {
                push @exp_path_names, 
                     orient_and_add_path($path_r, $pbase . $expelem, $d, 
                                         $exp_input, \@expath);
            }
        }
    }
    else {
        warn "Bad path name $pname_range. Skipping.\n";
    }
    return @exp_path_names;
}

#
# Parses a path reference, which could either be <pname> or an inline
# path definition of the form 
#   '{ (in)[-+] node1[[+-] node2[-+] .. }['['<var_range>']']".
# Returns the list of all matching, expanded path names.
#
sub parse_inline_path_ref {
    my $spec_r = shift;
    my $top    = shift;
    my $pref   = shift;

    my $paths_r = $spec_r->{CELLS}{$top}{PATHS};
    my @path_names;
    if ($pref =~ /\{\s*(.*)\s*\}\s*(\[.*\])?$/) {
        @path_names = parse_path($paths_r, (defined $2 ? $2 : ""), $1);
        $spec_r->{INFO}{num_exp_paths} += scalar(@path_names);
        $spec_r->{INFO}{num_paths}++;
    }
    elsif ($pref =~ /^\S+$/) {
        @path_names = get_matching_path_names($paths_r, $pref);
        if (!@path_names) {
            die "Undefined path $pref referenced.\n";
        }
    }
    else {
        die "Bad path reference: $pref.\n";
    }
    return @path_names;
}

#
# Tries to identify a common base name of the list of expanded path names;
# will give a unique anonymous name otherwise. Uniqueness depends on state
# of the paths_r map (i.e. number of existing path definitions).
#
sub get_common_base_name {
    my $paths_r = shift;
    my $base    = shift;
    die "Bad path reference.\n" if (!defined $base);
    while (@_) {
        my $p = shift;
        while (substr($p, 0, length $base) ne $base) {
            if ($base =~ /:/) {
                $base =~ s/:.*$//;
            }
            elsif ($base =~ /\[/) {
                $base =~ s/\[.*$//;
            }
            else {
                $base = "";
            }
        }
    }
    $base = scalar(keys %{$paths_r}) if ($base eq "");
    return $base;
}

#
# Given a list of expanded path names, returns the input node or transition
# that is common to them all.  If none exists, "" will be returned.
#
sub get_common_path_input {
    my $paths_r = shift;
    my $path    = shift;
    die "Bad path reference.\n" if (!defined $path);
    die "Undefined path '$path' referenced.\n" if (!exists $paths_r->{$path});
    my $input = $paths_r->{$path}{INPUT};
    while (@_) {
        $path = shift;
        if (!exists $paths_r->{$path}) {
            die "Undefined path '$path' referenced.\n";
        }
        my $i = $paths_r->{$path}{INPUT};
        if (substr($i, 0, length $input) ne $input) {
            $input =~ s/[-+]$//;
            return "" if (substr($i, 0, length $input) ne $input);
        }
    }
    return $input;
}


#
# Finds the next available anonymous path name of the form 
# "input;output[;num]".
#
sub get_next_path_base_name {
    my $path_r = shift;
    my $input  = shift;
    my $output = shift;

    my $bname = "$input;$output";
    my $num = 0;
    foreach my $pname (keys %{$path_r}) {
        if ($pname =~ /([^;]+);([^;]+)(;\d+)?$/) {
            if ($1 eq $input && $2 eq $output) {
                my $n = (defined $3 ? $3 + 1 : 1);
                $num = $n if ($n > $num);
            }
        }
    }
    if ($num == 0) {
        return $bname;
    }
    else {
        $num++;
        return "${bname};$num";
    }
}

#
# Orients an otherwise expanded path in the specified direction and
# adds it to the path structure.  Note: no checking of path redefinition.
# Inverse monotinicity of path transitions is assumed!
#
# Returns the oriented path name for this path (adds ":up" or ":dn").
#
sub orient_and_add_path {
    my $path_r = shift;
    my $pbase  = shift;     # path base name with indices expanded
    my $dir    = shift;     # 0:dn, 1:up
    my $input  = shift;     # input node
    my $t_lr   = shift;     # transition list

    my @dir_str = ("-","+");
    my $pname = $pbase . ($dir==0 ? ":dn" : ":up");
    my $d = ($dir + @{$t_lr})%2;
    $path_r->{$pname} = {
        INPUT => ($input ne "" ? $input . $dir_str[$d] : ""),
        SEQUENCE => []
    };
    $path_r->{$pname}{OUTPUT} = $path_r->{$pname}{INPUT};
    my $t;
    foreach $t (@{$t_lr}) {
        $d = 1 - $d;
        (my $t2 = $t) =~ s/([-+]?)$/$dir_str[$d]/;
        if ($1 ne "" && $1 ne $dir_str[$d]) {
            die "Bad transition direction detected in path $pbase.";
        }
        push @{$path_r->{$pname}{SEQUENCE}}, $t2;
        $path_r->{$pname}{OUTPUT} = $t2;
    }
    return $pname;
}

#
# Reassigns all node indices in a path with some other value determined
# by the funcref function pointer.
#
sub reassign_path_indices {
    my $pathref = shift;
    my $funcref = shift;
    my @newpath = ();
    foreach my $node (@{$pathref}) {
        my $newnode = $node;
        my $offset = 0;
        my $i = 0;
        do {
            $i = index $newnode, "[", $offset;
            if ($i >= 0) {
                my $j = index $newnode, "]", $i;
                die "Bad path node name '$newnode'.\n" if ($j == -1);
                my $index = substr($newnode, $i, $j-$i+1);
                substr($newnode, $i, $j-$i+1) = &{$funcref}($index,@_);
            }
            $offset = $i+1;
        }
        while ($i != -1);
        #print "Evaluated $node to $newnode\n";
        push @newpath, $newnode;
    }
    @{$pathref} = @newpath;
}

#
# Substitutes a variable "var" (e.g. "i") in an index spec (e.g. "[i,j]")
# with a particular value "val" (e.g. "3").
#
sub substitute_path_variable {
    my ($index,$var,$val) = @_;
    $index =~ s/([^\w\.])$var([^\w\.])/$1$val$2/g;
    return $index;
}
    
#
# For a given numeric index spec (e.g. "[10/2,4*2]"), evaluates the 
# expressions to single numbers (e.g. "[5,8]").
#
sub evaluate_path_indices {
    my ($index) = @_;
    $index =~ s/[\[\]]//g;
    my $new_index = "";
    foreach my $term (split(/\,/,$index)) {
        $new_index .= eval "int($term)";
        $new_index .= ",";
    }
    $new_index =~ s/,$//;
    return "[$new_index]";
}

# Given (a, b, c), returns "a, b, c".
sub list_to_string {
    my $str = "";
    while (@_) {
        $str .= shift;
        $str .= ", " if (@_);
    }
    return $str;
}

1;
