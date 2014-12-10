#
# ModifySubtypes
#
# Supersize module for manipulating working directory subtypes
#

package Supersize::ModifySubtypes;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &write_subtype
        &read_subtype
        &copy_subtype
        &set_subtype_directives
        &get_subtype_directives
        &get_or_set_directive_in_subtype
        $LEAF_CELL_TYPE
        $MID_CELL_TYPE
        $MISSING_CELL_TYPE
        $INVALID_CELL_TYPE
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Util;
use Supersize::TypeUtil;

# Constants used/returned by read_subtype, write_subtype
our $MISSING_CELL_TYPE = 0;
our $MID_CELL_TYPE     = 1;
our $LEAF_CELL_TYPE    = 2;
our $INVALID_CELL_TYPE = 3;

# Supersize command module data
sub get_module_data {
    return { NAME => "ModifySubtypes",
             DESC => "Commands for transforming Cast layout subtypes.",
             COMMANDS => {
    "inline_instance" => {
        SUBREF => \&inline_instance,
        USAGE  => "inline_instance " . underline("subtype") . " " .
            underline("instname"),
        DESC   => "Within " . underline("subtype") . ", inlines the " .
            "instance " . underline("instname") . "\n", },
    "resubtype"     => {
        SUBREF => \&resubtype,
        USAGE  => "resubtype " . underline("subtype") . 
                     " [--reuse-spec=" . underline("ubersize_reuse_spec") .
                     "] [" . underline("[instname_1:]new_subtype_1") . " " .
                             underline("[instname_2:]new_subtype_2") . " ...]",
        DESC   =>
          "Within " . underline("subtype") . ", sets any subcells matching " .
          "the type of " . underline("new_subtype_i") . " to have subtype " .
          underline("new_subtype_i") . ".  The " . underline("subtype") . 
          " argument may also be a list, in which case all subtypes in the ".  
          "list will be processed.  If any matching " .
          underline("new_subtype_i") . " does not already exist in both " .
          "SPEC_DIR and the working subtype hierarchy, the new subtype " .
          "will be created from the original subtype.\n\n" .
          "Each destination subtype can be optionally qualified by an " .
          "instance name prefix " . underline("instname_i") . ", which, " .
          "if present, will restrict the resubtyping to matching instance " .
          "names.  The instance name can be an exact match, or it can omit " .
          "all array indices.  For example, to match against all instances " .
          "of the form \"cpy_s[i].buf[j]\", specify an instance name " .
          "\"cpy_s.buf\".\n\n" .
          "If the " . underline("subtype") . " list contains all subtypes " .
          "under TOP, then this command (with --reuse-spec specified) is " .
          "equivalent to 'ubersize reuse_spec'.  FYI, here's how to " .
          "implement 'ubersize reuse':\n\n" .
          "  resubtype [[query subcells]] --reuse-spec=" . 
              underline("file") . "\n" .
          "  copy_floorplan --src-dir=" . underline("src-dfII-dir") . 
          " \$resubtype.matched_subtypes\n",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          SPEC_DIR => {} },
        RV     => {
          "matched_instances" => { 
            TYPE => $TYPE_LIST,
            DESC => "List of resubtyped instances." },
          "matched_subtypes" => { 
            TYPE => $TYPE_LIST,
            DESC => "List of matching " .  underline("new_subtype_i") .
                        " subtypes." },
          "copied_subtypes" => {
            TYPE => $TYPE_MAP,
            DESC => "Map of original_subtype to list of " . 
                    underline("new_subtype_i") . "'s that were copied ".
                    "because they didn't already exist.  Note that " .
                    "currently all < " . underline("new_subtype_i") . 
                    " > lists will contain a single subtype.  In the " .
                    "future, more advanced resubtyping specifications " .
                    "will be supported which will require that this " .
                    "return value be a one-to-many map." }} },
    "remove"        => {
        SUBREF => \&remove,
        USAGE  => "remove " . underline("subtype-list"),
        DESC   => 
          "Removes the subtype(s) from the working directory subtype tree." },
    "reuse_subcells" => {
        SUBREF => \&reuse_subcells,
        USAGE  => "reuse_subcells [--src=" . underline("src-dir") . "] " .
                    "[--copy] " . underline("subtype") . " " .
                    underline("src_subtype"),
        DESC   =>
          "For any instance 'inst' of " . underline("subtype") . ", if " .
          "inst exists in " . underline("src_subtype") . ", sets inst's " .
          "subtype to match that in " . underline("src_subtype") . ".  " .
          "In other words, all matching subcells of " .
          underline("src_subtype") . " are reused " .
          "in " . underline("subtype") . ".\n\n" .
          "If a source directory is not specified with the --src option, " .
          "looks under SPEC_DIR for " . underline("src_subtype") . ".  " .
          "If --copy is specified, any reused subcell from " .
          underline("src_subtype") . " will be copied to WORK_DIR " .
          "if it does not already exist under the WORK_DIR hierarchy.  " .
          "(Beware: no check is made to verify that the WORK_DIR subtype " .
          "matches the source subtype.)" },
    "set_directive" => {
        SUBREF => \&set_directive,
        USAGE  => "set_directive [" . underline("block") . ":]" .
          underline("directive") . "=" .  underline("value") . " [" . 
          underline("subtype_list") . "]",
        DESC   =>
          "Sets the specified directive to " . underline("value") . " in " .
          "all listed subtypes.  The " . underline("subtype_list") . 
          " argument may either be a single cell name or a list.  The " .
          "optional " . underline("block") . " argument identifies " .
          "the parent block of the directive.  Supported values are " .
          "top (i.e. cell-level, the default) and subtypes.\n\n" .
          "Advanced usage: " . underline("value") . " may be a map variable ".
          "of subtype -> directive_value, in which case no " .
          underline("subtype_list") . " argument is specified.  Example: ".
          "applying the auto_layout directives from " .
          bold("optimize_density_factors") . ".  In this case, you'd do\n\n".
          "  set_directive auto_layout = \n" .
          "      \$optimize_density_factors.auto_layout_directives\n\n" .
          "The special case ".underline("value")." of '-' indicates that ".
          "the specified directive should be removed from the cell." },
    "set_height"    => {
        SUBREF => \&set_height,
        USAGE  => "set_height " . underline("subtype") . " " .
                    underline("new_height"),
        DESC   => "Sets the height directive of " . underline("subtype") .
                  " to be " . underline("new_height") . "."},
    "set_fixed"     => {
        SUBREF => \&set_fixed,
        USAGE  => "set_fixed " . underline("subtype") . " [" . 
                    underline("new_value") . "]",
        DESC   => 
          "Sets the 'fixed_size' directive of " . underline("subtype") . " " .
          "to be true, or " . underline("new_value") . " if specified." }
    }};
}

################################## Commands ##################################

#
# remove
#
sub remove {
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};
    usage($SS_r, "Insufficient arguments to remove") if (num_args(\@_)==0);
    while (num_args(\@_)) {
        my @subtypes = @{shift_next_list(\@_)};
        foreach my $subtype (@subtypes) {
            my $file = fqcn_to_file(get_work_dir($SS_r) . "/cast", $subtype);
            if (-e $file) {
                print "Removing subtype $subtype.\n" if ($GS_r->{VERBOSE});
                unlink $file;
            }
            else {
                print "Warning: $file\n         doesn't exist. Ignoring.\n";
            }
        }
    }
}

#
# set_height
#
sub set_height {
    my $SS_r = shift;
    if (num_args(\@_)<2) {
        usage($SS_r, "No height specified");
        return;
    }
    my $subtype    = shift_next_scalar(\@_);
    my $new_height = shift_next_scalar(\@_);
    return if ($subtype eq "" || $new_height eq "");
    print "Setting height of $subtype to $new_height.\n" 
        if ($SS_r->{GS}{VERBOSE});
    my $matched = replace_line($SS_r, $subtype, q(height\s*=\s*[\d\.]+;), 
                               "height = $new_height;");
    print "Warning: No height directive found in $subtype.\n" if (!$matched);
    set_dirty($SS_r, $subtype);
}

#
# set_fixed
#
sub set_fixed {
    my $SS_r       = shift;
    my $subtype    = shift_next_scalar(\@_);
    return if ($subtype eq "");
    my $value      = "true";        # Default is to set fixed_size=true
    if (num_args(\@_) == 1) {
        $value = shift_next_scalar(\@_);
        return if ($value eq "");
    }
    if (num_args(\@_)) {
        usage($SS_r, "Too many arguments to set_fixed");
        return;
    }
    print "Setting fixed_size=$value in $subtype.\n" if ($SS_r->{GS}{VERBOSE});
    my $matched = replace_line($SS_r, $subtype, q(fixed_size\s*=\s*[^;]+;), 
                               "fixed_size = $value;");
    print "Warning: No fixed_size directive found in $subtype.\n" 
        if (!$matched);
    set_dirty($SS_r, $subtype);
}

#
# set_directive
#
sub set_directive  {
    my $SS_r = shift;

    # Determine directive & value
    my $block     = "top";
    my $directive = shift_next_scalar(\@_);
    my $eq        = shift_next_scalar(\@_);
    my ($type,$value) = shift_next_arg(\@_);
    if (!defined $directive || $eq ne "=" || !defined $type) {
        command_die($SS_r, "Invalid directive specification.");
    }
    if ($directive =~ /(\w*):(.*)$/) {
        $block = $1; $directive = $2;
        $block = "top" if ($block eq "");
        if ($block ne "top" && $block ne "subtypes") {
            command_die($SS_r, "Invalid directives block '$block' specified.");
        }
    }

    # Add remaining arguments to the new subtype list
    my $subtype_to_value_mr;

    if ($type == $TYPE_SCALAR) {
        my @subtypes = @{shift_next_list(\@_)};
        if (!@subtypes) {
            command_die($SS_r, "No subtypes specified.");
        }
        foreach my $s (@subtypes) {
            $subtype_to_value_mr->{$s} = $value;
        }
    }
    elsif ($type == $TYPE_MAP) {
        foreach my $s (keys %{$value}) {
            if ($value->{$s}[0] != $TYPE_SCALAR) {
                command_die($SS_r, "Directives map must have scalar values.");
            }
            $subtype_to_value_mr->{$s} = $value->{$s}[1];
        }
    }

    set_subtype_directives($SS_r, $block, $directive, $subtype_to_value_mr,
                           $SS_r->{GS}{VERBOSE});
}

# Set a directive in one or more subtype to the specified value(s).
sub set_subtype_directives {
    my $SS_r                = shift;
    my $block               = shift;
    my $directive           = shift;
    my $subtype_to_value_mr = shift;
    my $verbose             = shift;
    my $root_directory      = shift;

    foreach my $subtype (keys %{$subtype_to_value_mr}) {
        my $value = $subtype_to_value_mr->{$subtype};
        my $map = { $directive => $value eq '-' ? undef : $value };
        get_or_set_directive_in_subtype($SS_r, $root_directory, $subtype,
                                        1, $map, $block, $verbose,
                                        defined($root_directory));
    }
}

#
# subtype (NOT YET IMPLEMENTED)
#
# when called on "fqcn.N", makes any "fqcn :> fqcn.M" subtypes into
# "fqcn :> fqcn.N", creating the cast definition of "fqcn.N" if necessary
# (modelled after "fqcn.N").  When called as "fqcn.M:N", restricts this
# subtyping only to instances that are of type "fqcn.M".  When called as
# "fqcn.M:N(instname)", further restricts subtyping to matching instances.
#
sub subtype {
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};
    # Parse arguments
    if (num_args(\@_)<2) {
        usage($SS_r, "Insufficient arguments to subtype");
        return;
    }
    my $subtype = shift_next_scalar(\@_);
    return if ($subtype eq "");
    
    # Build map of type to subtype info
    my %typemap = ();
    while (num_args(\@_)) {
        my $subspec = shift_next_scalar(\@_);
        return if ($subspec eq "");
        if ($subspec =~ /^(.+)\.(\d[^\.]*(:\d[^\(]*(\(.*\))?)?)/) {
            $typemap{$1} = $2;
        }
        else {
            command_die($SS_r, "Bad subtype spec: $subspec");
            return;
        }
    }
    # continue...
}


#
# resubtype
#
sub resubtype {
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};

    # Parse arguments
    if (num_args(\@_)<2) {
        command_die($SS_r, "Wrong number of arguments to resubtype");
    }

    # Determine source subtype(s) to process
    my @subtypes;
    my ($type, $value) = shift_next_arg(\@_);
    if ($type == $TYPE_SCALAR) {
        push @subtypes, $value;
    }
    elsif ($type == $TYPE_LIST) {
        push @subtypes, @{$value};
    }
    else {
        command_die($SS_r, "Bad subtype argument.");
    }

    # Determine destination subtypes (subtypes to reuse/create)
    my %typemap;
    while (num_args(\@_) && (my $arg = next_scalar(\@_)) =~ /^--/) {
        command_die($SS_r, "Bad scalar argument.") if (!defined $arg);
        shift_next_scalar(\@_);
        if ($arg eq "--reuse-spec") {
            my $eq = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=") {
                command_die($SS_r, "Bad --reuse-spec argument.");
            }
            my ($type, $val) = shift_next_arg(\@_);
            if ($type eq $TYPE_SCALAR) {
                my @files = split /:/, $val;
                foreach my $f (@files) {
                    print "Reading $f\n";
                    read_reuse_spec($SS_r, $f, \%typemap);
                }
            }
            elsif ($type eq $TYPE_LIST) {
                foreach my $f (@{$val}) {
                    print "Reading $f\n";
                    read_reuse_spec($SS_r, $f, \%typemap);
                }
            }
            else {
                command_die($SS_r, "Bad --reuse-spec argument.");
            }
        }
        else {
            command_die($SS_r, "Unknown resubtype option $arg.");
        }
    }

    # Add remaining arguments to the new subtype list
    my @resubtype_list = ();
    push @resubtype_list, @{shift_next_list(\@_)} if (num_args(\@_));
    return if (!@resubtype_list && !%typemap);
    if (num_args(\@_)) {
        command_die($SS_r, "Wrong number of arguments to resubtype");
    }

    # Build map of type (+instance) to subtype number
    print "Determining type map:\n" if ($SS_r->{GS}{DEBUG});
    foreach my $line (@resubtype_list) {
        if ($line =~ /^(.+)\.(\d[^\.]*)$/) {
            $typemap{$1} = $line;
            print "  $1 -> $line\n" if ($SS_r->{GS}{DEBUG});
        }
        elsif ($line =~ /^([^\s:]+)\s*:>\s*([^\s]+)\s*$/) {
            $typemap{$1} = $2;
            print "  $1 -> $2\n" if ($SS_r->{GS}{DEBUG});
        }
        else {
            command_die($SS_r, "Error: Bad subtype specification:\n $line.");
        }
    }

    # Initialize return variables
    set_cmd_return_variable($SS_r, "matched_instances", [ $TYPE_LIST, [] ]);
    set_cmd_return_variable($SS_r, "matched_subtypes", [ $TYPE_LIST, {} ]);
    set_cmd_return_variable($SS_r, "copied_subtypes", [ $TYPE_MAP, {} ]);

    # Parse all subtype files and resubtype all matching subcells
    foreach my $subtype (@subtypes) {
        print "Resubtyping $subtype:\n" if ($GS_r->{VERBOSE});
        transform_subtype($SS_r, $subtype, \&resubtype_match, \%typemap);
        # Set dirty bit for this subtype
        set_dirty($SS_r, $subtype);
    }

    # Turn matched_subtypes variable into a list
    my @matched_subtypes = 
        keys %{$SS_r->{RV}{"ModifySubtypes/resubtype"}{matched_subtypes}[1]};
    $SS_r->{RV}{"ModifySubtypes/resubtype"}{matched_subtypes}[1] =
        \@matched_subtypes;
}

sub resubtype_match {
    my ($SS_r, $typemap_ref, $basetype, $subtype, $instname) = @_;

    my $GS_r        = $SS_r->{GS};
    my $new_subtype = "";
    my $cmd         = "ModifySubtypes/resubtype";
    my $wdir        = get_work_dir($SS_r);

    # required for deprecated and MD5-hashed cell names
    my $base_of_subtype = basetype_of($subtype);

    # Eliminate array indices for instance name matching
    my $instname2 = base_array_instance_name($instname);

    if (my @klist = grep { $base_of_subtype eq $_ ||
                           $base_of_subtype eq basetype_of($typemap_ref->{$_})
                           || /^\Q$instname\E:/
                           || /^\Q$instname2\E:/
                         } keys %{$typemap_ref}) {
        my $k = $klist[0];
        my $inst = "";
        if ($k =~ /:/) {
            ($inst = $k) =~ s/:(.*)$//;
        }
        (my $basetype = $k) =~ s/^[^:]+://;
        if ($inst ne "" && $basetype ne $base_of_subtype) {
            print STDERR "Warning: Inconsistent base type specified for " .
                         "$instname2.\nLeaving instance unchanged.\n";
            $basetype = "";
            return ($basetype, $new_subtype, $instname);
        }
        ($new_subtype = ${$typemap_ref}{$k}) =~ s/^[^:]*://;

        # Check for existence of reused deprecated subtype
        if (($base_of_subtype ne $basetype || 
            basetype_of($new_subtype) ne $basetype) && 
            !-e fqcn_to_file("$wdir/cast", $new_subtype) &&
            !-e fqcn_to_file($SS_r->{GS}{SPEC_DIR}, $new_subtype)) {
            print STDERR "Warning: Deprecation resubtyping of $new_subtype\n".
                         "identified, but destination subtype doesn't exist. ".
                         "Leaving subtype unchanged.\n";
            $basetype = "";
            return ($basetype, $new_subtype, $instname);
        }
        print "  Changing subtype of instance $instname to " .
              ${$typemap_ref}{$k} . ". " if ($GS_r->{VERBOSE});

        # Create new subtype file, preferring spec-dir if it exists there
        if (!-e fqcn_to_file("$wdir/cast", $new_subtype) &&
            !subtype_exists_in_path("$GS_r->{SPEC_DIR}:$GS_r->{CAST_DIR}", $new_subtype)) {
            my $src;
            if (-e fqcn_to_file("$wdir/cast", $subtype)) {
                $src = "$wdir/cast";
            } elsif (-e fqcn_to_file($GS_r->{SPEC_DIR}, $subtype)) {
                $src = $GS_r->{SPEC_DIR};
            }
            if (defined($src)) {
                print "(copy)\n" if ($GS_r->{VERBOSE});
                copy_subtype($SS_r, $src, $subtype, "$wdir/cast", $new_subtype);
                if (!exists $SS_r->{RV}{$cmd}{copied_subtypes}[1]{$subtype}) {
                    $SS_r->{RV}{$cmd}{copied_subtypes}[1]{$subtype} = 
                        [ $TYPE_LIST, [] ];
                }
                push @{$SS_r->{RV}{$cmd}{copied_subtypes}[1]{$subtype}[1]},
                    $new_subtype;
            } else {
                print "(should copy, but old subtype missing)\n"
                    if ($GS_r->{VERBOSE});
            }
        }
        else {
            print "(reuse)\n" if ($GS_r->{VERBOSE});
        }
                
        # Add instance and matched subtype to return variable list
        push @{$SS_r->{RV}{$cmd}{matched_instances}[1]}, $instname;
        $SS_r->{RV}{$cmd}{matched_subtypes}[1]{$new_subtype} = 1;
    }
    else {
        # No change
        $basetype = "";
    }
    return ($basetype, $new_subtype, $instname);
}
    
# check for an existing subtype given a CAST_PATH and FQCN
sub subtype_exists_in_path {
    my ($path,$fqcn) = @_;
    foreach my $dir (split(":",$path)) {
        return 1 if (-e fqcn_to_file($dir, $fqcn));
    }
    return 0;
}

#
# reuse_subcells
#
sub reuse_subcells {
    my $SS_r = shift;

    # Parse arguments, check for required global variables
    my $dest_subtype;
    my $src_subtype;
    my $src_dir       = $SS_r->{GS}{SPEC_DIR};
    my $copy_subtypes = 0;
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--src") {
            my $eq = shift_next_scalar(\@_);
            $src_dir = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $src_dir) {
                usage($SS_r, "Bad --src argument.");
                return;
            }
        }
        elsif ($arg eq "--copy") {
            $copy_subtypes = 1;
        }
        elsif (!defined $dest_subtype) {
            $dest_subtype = $arg;
        }
        elsif (!defined $src_subtype) {
            $src_subtype = $arg;
        }
        else {
            usage($SS_r, "Too many arguments to reuse_subcells.");
            return;
        }
    }
    if (!defined $dest_subtype || !defined $src_subtype) {
        usage($SS_r, "Insufficient arguments to reuse_subcells");
        return;
    }
    if (!defined $src_dir) {
        usage("Required variable SPEC_DIR isn't set.");
        return;
    }

    # Read all instance/basetype/subtype data from the source subtype
    my ($cell_type, $instmap_r) = read_subtype($SS_r, $src_dir, $src_subtype);
    if ($cell_type == $MISSING_CELL_TYPE) {
        command_die($SS_r, "Couldn't locate subtype $src_subtype.");
    }
    elsif ($cell_type == $LEAF_CELL_TYPE) {
        command_die($SS_r, "Subtype $src_subtype is a leaf cell.");
    }
    elsif (!defined $instmap_r) {
        command_die($SS_r, "Bad source subtype $src_subtype in $src_dir.");
    }

    # Change subtypes of the instances that exist in the source cell
    print "Applying matching subtypes\n";
    print "    from: $src_subtype\n";
    print "      to: $dest_subtype:\n";

    my ($modified_old, $modified_new) =
        transform_subtype($SS_r, $dest_subtype, \&reuse_subcells_match, 
                          $instmap_r);

    # Copy new subtypes if specified
    if ($copy_subtypes) {
        foreach my $s (keys %{$modified_new}) {
            my $src_file  = fqcn_to_file($src_dir, $s);
            my $dest_file = fqcn_to_file(get_work_dir($SS_r) . "/cast", $s);
            print "  Copying subtype file for $s.\n" if ($SS_r->{GS}{VERBOSE});
            `cp $src_file $dest_file` if (!-e $dest_file);
        }
    }

    # Set dirty bit for this subtype
    set_dirty($SS_r, $dest_subtype);
}

sub reuse_subcells_match {
    my ($SS_r, $instmap_ref, $basetype, $subtype, $instname) = @_;
    if (defined $instmap_ref->{$instname} && 
        $instmap_ref->{$instname}[0] eq $basetype) {
        print "  Reusing $instmap_ref->{$instname}[1] for instance " .
            "$instname.\n" if ($SS_r->{GS}{VERBOSE});
        $subtype  = $instmap_ref->{$instname}[1];
    }
    else {
        # No change
        $basetype = "";
    }
    return ($basetype, $subtype, $instname);
}

#
# inline_instance
#
sub inline_instance {
    my $SS_r = shift;

    my $root_directory = get_work_dir($SS_r) . "/cast";
    my $parent_subtype;
    my $instname;
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if (!defined $parent_subtype) {
            $parent_subtype = $arg;
        } elsif (!defined $instname) {
            $instname = $arg;
        } else {
            usage($SS_r, "Too many arguments to inline_instance.");
            return;
        }
    }

    if (!defined $parent_subtype || !defined $instname) {
        usage($SS_r, "Insufficient arguments to inline_instance");
        return;
    }

    my ($p_type, $p_map, $p_above, $p_below, $p_fixed) =
        read_subtype($SS_r, $root_directory, $parent_subtype);

    if ($p_type == $MISSING_CELL_TYPE) {
        command_die($SS_r, "Couldn't locate subtype $parent_subtype.");
    } elsif ($p_type == $LEAF_CELL_TYPE) {
        command_die($SS_r, "Subtype $parent_subtype is a leaf cell.");
    } elsif ($p_type != $MID_CELL_TYPE) {
        command_die($SS_r, "Subtype $parent_subtype is invalid.");
    }

    if (not exists($p_map->{$instname})) {
        command_die($SS_r, "Instance $instname does not exist in $parent_subtype");
    }

    my ($c_basetype, $c_subtype) = @{$p_map->{$instname}};
    my ($c_type, $c_map, $c_above, $c_below, $c_fixed) =
        read_subtype($SS_r, $root_directory, $c_subtype);

    if ($c_type == $MISSING_CELL_TYPE) {
        command_die($SS_r, "Couldn't locate subtype $c_subtype.");
    } elsif ($c_type == $LEAF_CELL_TYPE) {
        command_die($SS_r, "Subtype $c_subtype is a leaf cell.");
    } elsif ($c_type != $MID_CELL_TYPE) {
        command_die($SS_r, "Subtype $c_subtype is invalid.");
    }

    delete $p_map->{$instname};

    my $c_inst;
    foreach $c_inst (keys %$c_map) {
        $p_map->{$instname . '.' . $c_inst} = $c_map->{$c_inst};
    }

    write_subtype($SS_r, $root_directory, $parent_subtype,
                  $p_type, $p_map, $p_above, $p_below);

    set_dirty($SS_r, $parent_subtype);
}

############################## Utility functions ##############################


# Replaces all maching lines in the subtype cast file.
#   regexp need not worry about leading/trailing whitespace.
#   new_line should not include a '\n'.
# TODO: Replaced with a more robust set_directive function.
sub replace_line {
    my ($SS_r, $subtype, $regexp, $new_line) = @_;
    my $file = fqcn_to_file(get_work_dir($SS_r) . "/cast", $subtype);
    open(OLD, $file) || command_die($SS_r, "Couldn't open $file.");
    open(NEW, ">${file}.new") || 
        command_die($SS_r, "Couldn't write to ${file}.new.");
    my $found = 0;
    while (<OLD>) {
        if (/^(\s*)$regexp\s*$/) {
            print NEW $1 . $new_line . "\n";
            $found = 1;
        }
        else {
            print NEW;
        }
    }
    close OLD;
    close NEW;
    rename "$file", "${file}.old" if ($SS_r->{GS}{SAVE_BACKUPS});
    rename "${file}.new", $file;
    return $found;
}

# Reads an ubersize reuse-spec, returning its basetype->subtype map
sub read_reuse_spec {
    my ($SS_r, $reuse_spec, $map_r) = @_;
    open(REUSE, $reuse_spec) || 
        command_die($SS_r, "Couldn't read $reuse_spec.");
    while (<REUSE>) {
        next if (/^\s*\#/ || /^\s*$/);
        chomp;
        if (/^\s*([^\s:]+)\s*:>\s*([^\s]+)\s*$/) {
            $map_r->{$1} = $2;
        }
        elsif (/^\s*(\S+)\s*$/) {
            $map_r->{basetype_of($1)} = $1;
        }
        else {
            print "Warning: invalid syntax in reuse file on line $.\n";
        }
    }
    close REUSE;
}

# Parses a subtype file, returning a list of the following:
#
# (For mid-level cells)
#  [0] $MID_CELL_TYPE (1)
#  [1] instance subtype map: instname -> [ basetype, subtype ]
#  [2] list of lines above (and including) the first "subtypes {" block line.
#  [3] list of lines below the closing "}" subtypes block line, or below
#      (and including) the subtypes "directives {" block beginning.
#  [4] Value of fixed_size directive (0 or 1)
#
# (For leaf cells)
#  [0] $LEAF_CELL_TYPE (2)
#  [1] list of lines within the netlist block.
#  [2] list of lines above (and including) the "netlist {" block line.
#  [3] list of lines below (and including) the "}" closing line of the
#      netlist block.
#  [4] Value of fixed_size directive (0 or 1)
#
# If the subtype file doesn't exist, field [0] will be $MISSING_CELL_TYPE
# (0).  If the cell type of the subtype file isn't recognized for some reason,
# field [0] will be $INVALID_CELL_TYPE (3).  In either case, all other
# fields will be undef.
#
sub read_subtype {
    my ($SS_r, $root_dir, $fqcn) = @_;
    my $file = fqcn_to_file($root_dir, $fqcn);
    my %instmap = ();   # instance->subtype map for mid-level cells
    my @netlist = ();   # verbatim netlist block text for leaf cells
    my @head = ();      # Lines above "subtypes {"/"netlist {".
    my @tail = ();      # Lines below subtypes/netlist block.
    my $fixed_size = 1;

    # Parse subtype file
    if (!open SUBTYPE, $file) {
        print "Warning: $file exists, but can't read it.\n" if (-e $file);
        return ((-e $file ? $INVALID_CELL_TYPE : $MISSING_CELL_TYPE), 
                undef, undef, undef);
    }
    print "Reading $file\n" if ($SS_r->{GS}{DEBUG});
    my $parse_state = 0;
    my $basetype    = "";
    my $line        = 0;
    my $cell_type   = $INVALID_CELL_TYPE;
    while (<SUBTYPE>) {
        $line++;
        if ($parse_state==0) {
            push @head, $_;
            if (/^\s*subtypes\s+\{/) {
                $cell_type   = $MID_CELL_TYPE;
                $parse_state = 1;
            }
            elsif (/^\s*netlist\s+\{/) {
                $cell_type   = $LEAF_CELL_TYPE;
                $parse_state = 5;
            }
        }
        elsif ($parse_state==1) {
            next if (/^\s*$/ || /^\s*\/\//);
            if (/^\s*(\S+)\s+:>\s*$/) {
                $basetype = $1;
                $parse_state = 2;
            }
            elsif (/^\s*directives\s+\{/) {
                push @tail, $_;
                $parse_state = 3;
            }
            elsif (/^\s*\}/) {
                push @tail, $_;
                $parse_state = 4;
            }
            else {
                print "Warning: Unexpected syntax in $file, line $line:\n";
                print "         $_";
                print "         This line will be lost.\n";
            }
        }
        elsif ($parse_state==2) {
            if (/^\s*(\S+)\s+([^;\s]+)\s*;/) {
                $instmap{$2} = [ $basetype, $1 ];
            }
            else {
                print "Warning: Unexpected syntax in $file, line $line:\n";
                print "         $_";
            }
            $parse_state = 1;
        }
        elsif ($parse_state==3) {
            # directives block in subtypes/netlist block
            push @tail, $_;
            $parse_state = 4 if (/^\s*\}/);
        }
        elsif ($parse_state==4) {
            # Everything after subtypes block
            push @tail, $_;
            if (/\sfixed_size\s*=\s*(.*)\s*;/) {
                if ($1 eq "false") {
                    $fixed_size = 0;
                }
                else {
                    $fixed_size = 1;
                }
            }
        }
        elsif ($parse_state==5) {
            # Within the netlist block
            if (/^\s*directives\s+\{/) {
                # No directives block currently defined for netlist block,
                # but let's support it just in case it's ever added
                push @tail, $_;
                $parse_state = 3;
            }
            elsif (/^\s*\}/) {
                push @tail, $_;
                $parse_state = 4;
            }
            else {
                push @netlist, $_;
            }
        }
        else {
            command_die($SS_r, "Invalid parse state in read_subtype!");
        }
    }
    close SUBTYPE;
    if ($cell_type == $MID_CELL_TYPE) {
        return ($cell_type, \%instmap, \@head, \@tail, $fixed_size);
    }
    elsif ($cell_type == $LEAF_CELL_TYPE) {
        return ($cell_type, \@netlist, \@head, \@tail, $fixed_size);
    }
    else {
        return ($cell_type, undef, undef, undef, undef);
    }
}

# Creates a subtype file of the specified fqcn, with instance subtypes as
# specified by the provided instance map.  Optional head and tail lists
# (such as those returned by read_subtype) will be used when constructing
# the subtype file.  If a @head list is provided and the specified fqcn
# differs from that referenced in those lines, the @head lines will be 
# changed appropriately (and the subtype will be defined w/out a port
# list).  Returns 1 on success, 0 on error.
#
# Assumptions about @head lines:
#   - "module module_name;" appears on its own line.
#   - "define \"subnum\" (..)(..)(..) <+ attr1 <+ attr2 ... <: parent {"
#     appears one line (with all non-essential whitespace optional and 
#     metaparam/port lists optional.
#   - May or may not contain the module, define, and "subtypes {" lines.
#     (i.e. could just contain comments to include at the top of the file.)
#   - Lines are newline-terminated.
#
# Assumptions about @tail lines:
#   - Correctly closes a subtypes block (i.e. includes "  }\n}" at minimum.
#   - Lines are newline-terminated.
#
# TODO: 
#   - Emit "Automaticall generated by supersize" etc. comments
#   - Create directory structure if it doesn't already exist.
#   - More general support for specifying directives blocks
sub write_subtype {
    my $SS_r        = shift;
    my $root_dir    = shift;
    my $fqcn        = shift;
    my $cell_type   = shift;
    my $cell_data_r = shift;
    my $head_r      = shift;
    my $tail_r      = shift;

    # Subtype file prep work
    if ($cell_type == $MISSING_CELL_TYPE || $cell_type == $INVALID_CELL_TYPE) {
        print STDERR "Skipping writing to missing/invalid subtype $fqcn.\n";
        return 0;
    }
    my $file = fqcn_to_file($root_dir, $fqcn);
    my $last_dot = rindex $fqcn, ".";
    if ($last_dot <= 0 || $last_dot == length($fqcn) - 1) {
        print STDERR "Invalid subtype name '$fqcn'.";
        return 0;
    }
    my $module = substr $fqcn, 0, $last_dot;
    my $subnum = substr $fqcn, $last_dot+1;

    # Directory / file prep work
    (my $dir = $file) =~ s/\/[^\/]+$//;
    `mkdir -p $dir` if (!-e $dir);
    if (-e $file && $SS_r->{GS}{SAVE_BACKUPS}) {
        rename "$file", "${file}.old";
    }

    # Write head lines
    if (!open (SUBTYPE, ">$file")) {
        print STDERR "Error writing to $file.";
        return 0;
    }
    my $did_module   = 0;
    my $did_define   = 0;
    my $did_subtypes = 0;
    my $did_netlist  = 0;
    if (defined $head_r) {
        my $original_module = "";
        foreach my $line (@{$head_r}) {
            if ($line =~ /^module\s+([^;]+);\s*$/ && $1 eq $module) {
                $original_module = $module;
                $did_module = 1;
                print SUBTYPE $line;
            }
            elsif ($line =~ /^module\s+([^;]+);\s*$/ && $1 ne $module) {
                $original_module = $1;
                $did_module = 1;
                print SUBTYPE "module $module;\n";
            }
            elsif ($line =~ /^define\s+\"([^\"]+)\"(.*)$/ && $1 eq $subnum && 
                   $module eq $original_module) {
                $did_define = 1;
                print SUBTYPE $line;
            }
            elsif ($line =~ /^define\s+\"([^\"]+)\"(.*)$/ && ($1 ne $subnum || 
                   $module ne $original_module)) {
                my $original_subnum = $1;
                my $after_subnum = $2;
                if ($original_module ne $module) {
                    my $refine_index = index $line, "<:";
                    print SUBTYPE "define \"$subnum\"()() ";
                    my $attr_index = index "<+", $line;
                    if ($attr_index != -1) {
                        print SUBTYPE substr $line, $attr_index,
                                             $refine_index - $attr_index;
                    }
                }
                else {
                    my $refine_index = index $after_subnum, "<:";
                    print SUBTYPE "define \"$subnum\"";
                    print SUBTYPE substr $after_subnum, 0, $refine_index;
                }
                print SUBTYPE "<: $module {\n";
                $did_define = 1;
            }
            elsif ($line =~ /^\s*subtypes\s+\{/) {
                print SUBTYPE $line;
                $did_subtypes = 1;
            }
            elsif ($line =~ /^\s*netlist\s+\{/) {
                print SUBTYPE $line;
                $did_netlist = 1;
            }
            else {
                print SUBTYPE $line;
            }
        }
    }
    if (!$did_module) {
        print SUBTYPE "module $module;\n";
    }
    if (!$did_define) {
        print SUBTYPE "define \"$subnum\"()() <: $module {\n";
    }
    if ($cell_type == $MID_CELL_TYPE) {
        # Mid-level cell
        if (!$did_subtypes && (%{$cell_data_r} || @{$tail_r})) {
            print SUBTYPE "  subtypes {\n";
        }

        # Write subtypes
        foreach my $inst (sort keys %{$cell_data_r}) {
            print SUBTYPE "    " . ${$cell_data_r->{$inst}}[0] . " :>\n";
            print SUBTYPE "      " . ${$cell_data_r->{$inst}}[1] . " $inst;\n";
        }
    }
    elsif ($cell_type == $LEAF_CELL_TYPE) {
        # Leaf cell
        if (!$did_netlist && (@{$cell_data_r} || @{$tail_r})) {
            print SUBTYPE "  netlist {\n";
        }
        # Write netlist block
        foreach my $line (@{$cell_data_r}) {
            print SUBTYPE $line;
        }
    }

    # Write tail lines
    if (defined $tail_r) {
        foreach my $line (@{$tail_r}) {
            print SUBTYPE $line;
        }
    }
    elsif ($cell_type == $MID_CELL_TYPE && %{$cell_data_r} ||
           $cell_type == $LEAF_CELL_TYPE && @{$cell_data_r}) {
        print SUBTYPE "  }\n";
        print SUBTYPE "}\n";
    }

    close SUBTYPE;
    return 1;
}


# Reads the specified subtype file, transforming any "fqcn :> fqcn.NUM inst"
# line according to the behavior of the supplied matchfunc.  Returns
# references to two hashes:
#   [0] modified_old { old_subtype } = [ old_inst1, old_inst2, ... ]
#       (List of original instances that were modified, organized by
#       original subtype.)
#   [1] modified_new { new_subtype } = [ new_inst1, new_inst2, ... ]
#       (List of new instances that were either added, renamed, etc.
#       due to maching against one of the above old subtypes/instances,
#       organized by new subtype.)
#
sub transform_subtype {
    my ($SS_r, $subtype, $matchfunc_ref, $matchdata_ref) = @_;

    # Parse subtype file and apply matching function for each subcell
    my $file = fqcn_to_file(get_work_dir($SS_r) . "/cast", $subtype);
    command_die($SS_r, "Error: $file doesn't exist.") if (!-e $file);
    open(OLD, $file) || command_die($SS_r, "Couldn't open $file.");
    open(NEW, ">${file}.new") || 
        command_die($SS_r, "Couldn't write to ${file}.new.");
    my $modified_old = {};
    my $modified_new = {};
    my $parse_state  = 0;
    my $baseindent   = "";
    my $baseline     = "";
    my $basetype     = "";
    my $line         = 0;
    while (<OLD>) {
        $line++;
        if (/^\s*$/ || /^\s*\/\//) {
            print NEW;
            next;
        }
        if ($parse_state==0) {
            print NEW;
            $parse_state = 1 if (/^\s*subtypes\s+\{/);
        }
        elsif ($parse_state==1) {
            if (/^(\s*)(\S+)\s+:>\s*$/) {
                $parse_state = 3;
                $baseindent  = $1;
                $baseline    = $_;
                $basetype    = $2;
            }
            elsif (/^\s*directives\s+\{/) {
                print NEW;
                $parse_state = 4;
            }
            elsif (/^\s*\}/) {
                print NEW;
                $parse_state = 0;
            }
            else {
                print "Warning: Unexpected syntax in $file, line $line:\n";
                print "         $_";
            }
        }
        elsif ($parse_state==2) {
            print NEW;
            $parse_state = 1;
        }
        elsif ($parse_state==3) {
            if (/^(\s*)(\S+)\s+([^\s;]+)\s*;\s*(\/\/.*)?$/) {
                my ($new_basetype, $new_subtype, $new_instname) = 
                    &{$matchfunc_ref}($SS_r, $matchdata_ref, $basetype, $2, $3);
                if ($new_basetype ne "") {
                    # Matched; output new subtype
                    print NEW "${baseindent}$new_basetype :>\n";
                    print NEW "$1$new_subtype $new_instname;\n";
                    if ($basetype ne $new_basetype || 
                        $2 ne $new_subtype || $3 ne $new_instname) {
                        push @{$modified_old->{$2}}, $3;
                        push @{$modified_new->{$new_subtype}}, $new_instname;
                    }
                }
                else {
                    # Didn't match; transfer original
                    print NEW $baseline;
                    print NEW;
                }
            }
            else {
                print "Warning: Unexpected syntax in $file, line $line:\n";
                print "         $_";
                print NEW;
            }
            $parse_state = 1;
        }
        elsif ($parse_state==4) {
            # directives block in subtypes block
            print NEW;
            $parse_state = 1 if (/^\s*\}/);
        }
        else {
            command_die($SS_r, "Invalid parse state in " .
                        "transform_subtype_file!");
        }
    }
    close OLD;
    close NEW;
    rename "$file", "${file}.old" if ($SS_r->{GS}{SAVE_BACKUPS});
    rename "${file}.new", $file;
    return ($modified_old, $modified_new);
}


# Copies subtype file (src_root,src_subtype) to (dest_root,dest_subtype),
# renaming the subtype cell name if necessary.  The base type prefix
# of the destination subtype need not match the source type name; if
# they are different, the destination subtype will refine from the
# source's base type name.  This is to support deprecation-style name
# changes.  For example: deprecated.lib.buffer.half.BUF_1of2.666 (src)
# to lib.buffer.half.BUF_1of4.666 <: deprecated.lib.buffer.half.BUF_1of2.
sub copy_subtype {
    my $SS_r            = shift;
    my $src_root        = shift;
    my $src_subtype     = shift;
    my $dest_root       = shift;
    my $dest_subtype    = shift;
    my $if_exists       = shift;    # 0:overwrite, 1:skip, 2:abort
    my $preserve_perm   = shift;
    my $skip_missing    = shift;    # 0(default): die on non-existing source
                                    # 1: warn about non-existing source subtype

    my $src_file = fqcn_to_file($src_root, $src_subtype);
    my $dest_file = fqcn_to_file($dest_root, $dest_subtype);

    # Make sure source subtype exists
    if (!-e $src_file) {
        if (!defined $skip_missing || $skip_missing==0) {
            command_die($SS_r, "Source subtype $src_subtype doesn't exist ".
                               "under\n  $src_root.");
        }
        else {
            print "Warning: Skipping missing subtype $src_subtype.\n"
                if ($SS_r->{GS}{VERBOSE});
            return;
        }
    }

    # Spit up source subtype name
    $src_subtype =~ /^(.*)\.([^\.]+)$/ || 
        command_die($SS_r, "Bad subtype $src_subtype.");
    my $src_base = $1;
    my $src_num = $2;

    # Split up dest subtype name
    $dest_subtype =~ /^(.*)\.([^\.]+)$/ || 
        command_die($SS_r, "Bad subtype $dest_subtype.");
    my $dest_base = $1;
    my $dest_num = $2;

    # Check for deprecation style subtype renaming
    my $deprecation_case = $src_base ne $dest_base;
    if ($deprecation_case && $SS_r->{GS}{DEBUG}) {
        print STDERR "Subtype deprecation detected in copy_subtype:\n";
        print STDERR "  $src_subtype -> $dest_subtype\n";
    }
        
    # Create destination directory if it doesn't exist
    (my $dest_dir = $dest_file) =~ s/\/[^\/]+$//;
    `mkdir -p '$dest_dir'` if (!-e $dest_dir);

    # Check for overwrite cases
    if (-e $dest_file) {
        if (!defined $if_exists || $if_exists == 0) {
            if (-o $dest_file) {
                unlink $dest_file;
            }
            else {
                print "Warning: Can't overwrite existing subtype ".
                      "$dest_subtype.\n";
                return;
            }
        }
        elsif ($if_exists == 1) {
            print "Skipping existing subtype $dest_subtype.\n";
            return;
        }
        elsif ($if_exists == 2) {
            command_die($SS_r, "Detected existing subtype $dest_subtype.\n");
        }
        print "Warning: Overwriting $dest_subtype\n" if ($SS_r->{GS}{VERBOSE});
    }

    # Just copy the file if source and destination subtypes are the same
    if ($src_subtype eq $dest_subtype) {
        `cp -a '$src_file' '$dest_file'`;
        if (defined $preserve_perm && $preserve_perm==0) {
            `chmod u+w '$dest_file'`;
        }
        return;
    }

    # Copy subtype file, renumbering subtype and possibly changing the 
    # refinement parent in the in 'define' line.
    open (SRC, $src_file) || 
        command_die($SS_r, "Couldn't read $src_file.");
    open (DST, ">$dest_file") || 
        command_die($SS_r, "Couldn't write to $dest_file.");
    while (<SRC>) {
        if (/^\s*define\s+\"$src_num\"(.*)<:\s+([^\s]+)\s+\{$/) {
            if ($deprecation_case) {
                print DST "define \"$dest_num\"$1<: $src_base {\n";
            }
            else {
                print DST "define \"$dest_num\"$1<: $2 {\n";
            }
        }
        else {
            print DST;
        }
    }
    close SRC;
    close DST;

    # Make cell unwriteable if source wasn't
    if (defined $preserve_perm && $preserve_perm && !-w $src_file) {
        `chmod a-w '$dest_file'`;
    }
}

# Retrieves the value of directives set in one or many layout subtype files.
# Returns a map reference of subtype -> directive -> value, with value==undef 
# if the directive is not set.
sub get_subtype_directives {
    my $SS_r       = shift;
    my $cell_lr    = shift;     # list of cells
    my $dir_lr     = shift;     # list of directives

    my $values_mr = {};
    foreach my $s (@{$cell_lr}) {
        $values_mr->{$s} = {};
        foreach my $d (@{$dir_lr}) {
            my $values = 
                get_or_set_directive_in_subtype($SS_r, undef, $s, 0,
                                                { $d => undef }, "top", 0);
            $values_mr->{$s}{$d} = $values->{$d};
        }
    }
    return $values_mr;
}

# Either retrieves the value of a directive set in the specified subtype
# or sets the directive to a new value.
sub get_or_set_directive_in_subtype {
    my $SS_r      = shift;
    my $rootdir   = shift;
    my $subtype   = shift;
    my $mode      = shift;  # 0: read 1: write
    my $directive = shift;
    my $block     = shift;  # "top" or "subtypes"
    my $verbose   = shift;

    my $skip_dirtying = $mode==1 ? shift : undef;

    my %value = ();

    $rootdir = get_work_dir($SS_r) . '/cast' unless defined($rootdir);
    my $file = fqcn_to_file($rootdir, $subtype);
    open(OLD, $file) || command_die($SS_r, "Couldn't open $file.");
    if ($mode==1) {
        open(NEW, ">${file}.new") || 
            command_die($SS_r,  "Couldn't write to ${file}.new.");
    }
    my $found = 0;
    my %found = ();
    my @blocks = ();
    my $in_directives = 0;
    my $subtype_line = 0;
    my $skip_line = 0;
    my $top_block;
    while (<OLD>) {
        $skip_line = 0;
        if (@blocks) {
            $top_block = $blocks[$#blocks];
        }
        else {
            $top_block = "";
        }
        if (!@blocks && /^define /) {
            push @blocks, "top";
        }
        elsif (/^\s*directives\s*\{/) {
            $in_directives = 1;
        }
        elsif ($top_block eq "subtypes" && $subtype_line == 0 &&
               /^\s*(\S+)\s+:>\s*$/) {
            $subtype_line = 1;
        }
        elsif ($top_block eq "subtypes" && $subtype_line == 1 &&
               /^(\s*)(\S+)\s+([^\s;]+)\s*;\s*(\/\/.*)?$/) {
            $subtype_line = 0;
        }
        elsif (@blocks && /^\s*([^\s]+)\s*\{/) {
            push @blocks, $1;
        }
        elsif (!$in_directives && /^\s*\}/) {
            # Closing non-directives block
            pop @blocks;
            if ($top_block eq $block && !$found) {
                # No directives block was found, need to add directive line
                my $indent;
                if ($top_block eq "top") {
                    $indent = "  ";
                }
                elsif ($top_block eq "subtypes" && @blocks == 1) {
                    $indent = "    ";
                }
                else {
                    # Wrong (nested) block -- don't add the directive
                    next;
                }
                if ($mode==1) {
                    my $need_header = 1;
                    foreach my $dir (sort keys %{$directive}) {
                        my $val = $directive->{$dir};
                        if (defined $val) {
                            if ($need_header) {
                                print NEW "${indent}directives {\n";
                                $need_header = 0;
                            }
                            print NEW "${indent}  $dir = $val;\n";
                        }
                    }
                    print NEW "${indent}}\n" unless ($need_header);
                    $found = 1;
                }
            }
        }
        elsif ($in_directives && /^\s*\}/) {
            # End of directives block
            $in_directives = 0;
            if ($top_block eq $block && !$found) {
                # Right block, but didn't find specified directive
                my $indent;
                if ($top_block eq "top") {
                    $indent = "  ";
                }
                elsif ($top_block eq "subtypes" && @blocks == 2) {
                    $indent = "    ";
                }
                else {
                    # Wrong (nested) block -- don't add the directive
                    pop @blocks;
                    next;
                }
                if ($mode==1) {
                    foreach my $dir (sort keys %{$directive}) {
                        next if $found{$dir};
                        my $val = $directive->{$dir};
                        print NEW "${indent}  $dir = $val;\n" if defined $val;
                    }
                    $found = 1;
                }
            }
        }
        elsif ($top_block eq $block && $in_directives && 
               /^(\s*)([^\s=]+)\s*=\s*([^;]+);(.*)$/ &&
               exists($directive->{$2})) {
            # Found the directive
            # Skip if this is a nested block (e.g. env { subcells {)
            # or if this is a redundant directive
            next if (@blocks > 2);
            if ($mode==1) {
                $skip_line = 1;
                if (!$found{$2}) {
                    my ($dir, $val) = ($2, $directive->{$2});
                    print NEW "$1$dir = $val;$4\n" if defined $val;
                    $found{$2} = 1;
                }
            }
            else {
                $value{$2} = $3;
            }
        }
    } continue {
        if ($mode==1) {
            # Copy line unmodified to new subtype file
            print NEW unless ($skip_line);
        }
    }
    close OLD;
    if ($mode==1) {
        close NEW;
        if (!$found) {
            print "Did not find block $block in $subtype.  File left " .
                  "unmodified.\n";
            next;
        }
        rename "$file", "${file}.old" if ($SS_r->{GS}{SAVE_BACKUPS});
        rename "${file}.new", $file;
        set_dirty($SS_r, $subtype) unless $skip_dirtying;
        print "Set directive " . join(' ', keys %{$directive}) .
              " in $subtype.\n" if ($verbose);
    }
    else {
        return \%value;
    }
}

1;
