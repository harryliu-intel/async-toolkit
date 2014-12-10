#
# PerforceIntegration
#
# Commands for checking files in & out of Perforce
#

package Supersize::PerforceIntegration;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::LayoutIntegration;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Term::ANSIColor;

sub get_module_data {
  return { 
    NAME => "PerforceIntegration",
    DESC => "Commands for checking files in & out of Perforce",
    COMMANDS => {
    "p4_add" => {
        SUBREF => \&p4_add,
        USAGE  => "p4_add [--cast-only|dfII-only] ".
                         "[--overwrite] ".
                         "[".underline("cell-list")."] ".
                         "[--p4-spec-dir=".underline("p4-spec-dir")."] ".
                         "[--p4-dfII-dir=".underline("p4-dfII-dir")."]",
        DESC   =>
          "Adds all specified cells (or all cells under TOP that do not ".
          "exist in ".underline("p4-spec-dir").", if ".
          underline("cell-list")." isn't specified) to the appropriate ".
          "Perforce client spaces.  'p4 add' is called on each source " .
          "file (with the appropriate client spec), but the final ".
          "'p4 submit' is left to the user.\n\n" .
          "By default, both cast subtypes and dfII floorplan views will ".
          "be added.  The --cast-only and --dfII-only options can be ".
          "specified to restrict the check-in to either set of files.\n\n".
          "If --p4-spec-dir isn't specified, SPEC_DIR is used; likewise, if ".
          "--p4-dfII-dir isn't specified, DFII_DIR is used.\n\n".
          "If the --overwrite option is specified, any subtypes or views ".
          "that already exist in Perforce will first be opened for edit and ".
          "then copied over.  Otherwise, such files will be skipped and ".
          "with a warning message.",
        RV => {
          added_subtypes => {
            TYPE => $TYPE_LIST,
            DESC => "List of all added cast subtypes." },
          edited_subtypes => {
            TYPE => $TYPE_LIST,
            DESC => "List of all cast subtypes that were edited and then ".
                    "overwritted in the client space." },
          skipped_subtypes => {
            TYPE => $TYPE_LIST,
            DESC => "List all cast subtypes that were not copied and added ".
                    "to the client space." },
          added_views => {
            TYPE => $TYPE_LIST,
            DESC => "List of all added cast views." },
          edited_views => {
            TYPE => $TYPE_LIST,
            DESC => "List of all subtypes whose dfII views were edited ".
                    "and then overwritted in the client space." },
          skipped_views => {
            TYPE => $TYPE_LIST,
            DESC => "List all subtypes whose dfII views were not copied ".
                    "and added to the client space." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          P4_CAST_CLIENT   => { REQUIRED => 0 },
          P4_DFII_CLIENT => { REQUIRED => 0 } } },
    "p4_edit" => {
        SUBREF => \&p4_edit,
        USAGE  => "p4_edit [--cast-only|dfII-only] ".
                         "[--view=".underline("view-name")."] ".
                         "[".underline("cell-list")."] ".
                         "[--p4-spec-dir=".underline("p4-spec-dir")."] ".
                         "[--p4-dfII-dir=".underline("p4-dfII-dir")."]",
        DESC   =>
          "Edits all specified cells (or all cells under TOP that exist".
          "in ".underline("p4-spec-dir").", if ".  underline("cell-list").
          " isn't specified).  The floorplan view of each dfII cell is ".
          "edited unless a different view is specified with --view.\n\n".
          "By default, both cast subtypes and dfII views will ".
          "be edited.  The --cast-only and --dfII-only options can be ".
          "specified to restrict the editing to either set of files.\n\n".
          "If --p4-spec-dir isn't specified, SPEC_DIR is used; likewise, if ".
          "--p4-dfII-dir isn't specified, DFII_DIR is used.",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          P4_CAST_CLIENT   => { REQUIRED => 0 },
          P4_DFII_CLIENT => { REQUIRED => 0 } } },
    "p4_branch" => {
        SUBREF => \&p4_branch,
        USAGE  => "p4_branch --src-branch=".underline("src") . " " .
                         "--dest-branch=".underline("dest"). " " .
                         "[--branch-name=".underline("name")."] ".
                         "[--cast-only|dfII-only] ".
                         "[--p4-spec-dir=".underline("p4-spec-dir")."] ".
                         "[--p4-dfII-dir=".underline("p4-dfII-dir")."] ".
                         "[".underline("cell-list")."] ",
        DESC   =>
          "Creates a perforce branch spec for a set of cells, or all cells ".
          "under TOP if ".underline("cell-list")." isn't specified.  If ".
          "a branch name is specified, it will be registered with the ".
          "Perforce server under that name.  Otherwise, it will be saved ".
          "under WORK_DIR/branch.spec without being registered.  The p4 ".
          "spec and dfII directories must refer to the source branch.",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          TOP      => { REQUIRED => 0 },
          P4_CAST_CLIENT   => { REQUIRED => 0 },
          P4_DFII_CLIENT => { REQUIRED => 0 } } }
    },
    GLOBALS => {
      P4_CAST_CLIENT => { DESC => 
          "Perforce client spec containing hw/layout/<process>/spec." },
      P4_DFII_CLIENT => { DESC =>
          "Perforce client spec containing hw/layout/<process>/dfII." }
    }
  };
}

#
# p4_add
#
sub p4_add {
    my $SS_r = shift;

    my $p4_dfII_dir = $SS_r->{GS}{DFII_DIR};
    my $p4_spec_dir = $SS_r->{GS}{SPEC_DIR};
    my $do_cast     = 1;
    my $do_dfII     = 1;
    my $overwrite   = 0;
    my $cells_lr    = [];
   
    # parse args
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($arg eq "--cast-only") {
                $do_dfII = 0;
            }
            elsif ($arg eq "--dfII-only") {
                $do_cast = 0;
            }
            elsif ($arg eq "--overwrite") {
                $overwrite = 1;
            }
            elsif ($arg eq "--p4-spec-dir") {
                my $eq = shift_next_scalar(\@_);
                $p4_spec_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $p4_spec_dir) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--p4-dfII-dir") {
                my $eq = shift_next_scalar(\@_);
                $p4_dfII_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $p4_dfII_dir) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            else {
                command_die($SS_r, "Unrecognized argument $arg.");
            }
        }
        elsif ($type == $TYPE_LIST && @{$cells_lr}) {
            command_die($SS_r, "Too many lists specified to p4_add.");
        }
        elsif ($type == $TYPE_LIST) {
            $cells_lr = $arg;
        }
        else {
            command_die($SS_r, "Bad argument type specified to p4_add.");
        }
    }

    # Sanity check
    if ($do_cast && !-d $p4_spec_dir) {
        command_die($SS_r, "$p4_spec_dir doesn't exist.");
    }
    if ($do_dfII && !-e $p4_dfII_dir) {
        command_die($SS_r, "$p4_dfII_dir doesn't exist.");
    }

    # Determine cell list if one wasn't specified
    if (!@{$cells_lr}) {
        my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells";
        print "Determining cell list.\n";
        my $all_cells_lr = [];
        query_cast_server($SS_r, $cmd, $all_cells_lr, 1);
        foreach my $c (@{$all_cells_lr}) {
            my $f = fqcn_to_file($p4_spec_dir, $c);
            push @{$cells_lr}, $c if (!-e $f);
        }
    }

    # return variables
    my $added_subtypes_lr   = [];
    my $edited_subtypes_lr  = [];
    my $skipped_subtypes_lr = [];
    my $added_views_lr   = [];
    my $edited_views_lr  = [];
    my $skipped_views_lr = [];

    # p4 add cast subtypes
    if ($do_cast) {
        if (!exists $SS_r->{GS}{P4_CAST_CLIENT}) {
            command_die($SS_r, "Required variable P4_CAST_CLIENT isn't set")
        }
        print "Copying and p4 adding " . scalar(@{$cells_lr}) . 
              " cast subtypes...\n";
        foreach my $cell (@{$cells_lr}) {
            my $src_file  = fqcn_to_file("$SS_r->{GS}{WORK_DIR}/cast", $cell);
            my $dest_file = fqcn_to_file($p4_spec_dir, $cell);
            if (!-e $src_file) {
                command_die($SS_r, "Subtype $cell doesn't exist.");
            }
            my $dest_dir = fqcn_to_directory($p4_spec_dir, $cell);
            if (!-e $dest_dir) {
                `mkdir -p \"$dest_dir\"`;
                command_die($SS_r, "Couldn't create $dest_dir.") if ($?);
            }
            
            # check if the subtype already exists in client space
            my $add = 1;
            if (-e $dest_file) {
                if ($overwrite) {
                    if (p4_wrap("edit", $SS_r->{GS}{P4_CAST_CLIENT},
                                $p4_spec_dir, $dest_file)) {
                        print "Warning: Could not edit existing file " .
                              "$dest_file.\n";
                    }
                    else {
                        push @{$edited_subtypes_lr}, $cell;
                        $add = 0;
                    }
                }
                else {
                    print "Warning: Skipping existing subtype $cell.\n";
                    push @{$skipped_subtypes_lr}, $cell;
                    next;
                }
            }
            # copy subtype
            print "Copying subtype $cell.\n" if ($SS_r->{GS}{DEBUG});
            `cp -a \"$src_file\" \"$dest_file\"`;
            command_die($SS_r, "Couldn't copy subtype $cell.") if ($?);

            # add to perforce
            if ($add) {
                if (p4_wrap("add", $SS_r->{GS}{P4_CAST_CLIENT}, $p4_spec_dir,
                            $dest_file)) {
                    command_die($SS_r, "Couldn't p4 add $dest_file.");
                }
                push @{$added_subtypes_lr}, $cell;
            }
        }
    }
    # cdsp4add dfII views
    if ($do_dfII) {
        if (!exists $SS_r->{GS}{P4_DFII_CLIENT}) {
            command_die($SS_r, "Required variable P4_DFII_CLIENT isn't set")
        }
        print "Copying and p4 adding " . scalar(@{$cells_lr}) .
              " dfII floorplan views...\n";
        foreach my $cell (@{$cells_lr}) {
            my $cadence_cell = to_cadence($cell);

            # check if the view already exists in the client space
            my $add = 1;
            my $view_dir = get_cadence_cell_view_dir($p4_dfII_dir, 
                                $cadence_cell, "floorplan");
            if (-e $view_dir) {
                if ($overwrite) {
                    if (p4_wrap("edit", $SS_r->{GS}{P4_DFII_CLIENT},
                            $p4_dfII_dir, "$view_dir/...")) {
                        print "Warning: Could not edit existing view for " .
                              "$cell.\n";
                    }
                    else {
                        push @{$edited_views_lr}, $cell;
                        $add = 0;
                    }
                }
                else {
                    print "Warning: Skipping existing layout view for $cell.\n";
                    push @{$skipped_views_lr}, $cell;
                    next;
                }
            }
            # copy view
            print "Copying view for cell $cell.\n" if ($SS_r->{GS}{DEBUG});
            copy_cadence_cell_view($SS_r, $SS_r->{GS}{DFII_DIR}, $cell,
                "floorplan", $p4_dfII_dir, $cell, "floorplan", 0, 1);
            
            # add the view
            if ($add) {
                my $cmd = (path_to_tool($SS_r, "cdsp4add", $LOCAL_JOB))[0];
                $cmd .= " \"--dfII-dir=$p4_dfII_dir\" ";
                $cmd .= "--view-name=floorplan ";
                $cmd .= "--client-spec=$SS_r->{GS}{P4_DFII_CLIENT} ";
                $cmd .= "\"$cadence_cell\"";
                `$cmd`;
                command_die($SS_r, "Couldn't run $cmd.") if ($?);
                push @{$added_views_lr}, $cell;

                # add library if necessary
                my $libdir = get_cadence_lib_dir($p4_dfII_dir, $cadence_cell);
                $cmd = (path_to_tool($SS_r, "cdsp4addlibs", $LOCAL_JOB))[0];
                $cmd .= " --fast ";
                $cmd .= " \"--dfII-dir=$libdir\" ";
                $cmd .= "--client-spec=$SS_r->{GS}{P4_DFII_CLIENT}";
                `$cmd`;
                command_die($SS_r, "Couldn't run $cmd.") if ($?);
            }
        }
    }

    # set return variables
    set_cmd_return_variable($SS_r, "added_subtypes", 
                            [$TYPE_LIST, $added_subtypes_lr]);
    set_cmd_return_variable($SS_r, "edited_subtypes", 
                            [$TYPE_LIST, $edited_subtypes_lr]);
    set_cmd_return_variable($SS_r, "skipped_subtypes", 
                            [$TYPE_LIST, $skipped_subtypes_lr]);
    set_cmd_return_variable($SS_r, "added_views", 
                            [$TYPE_LIST, $added_views_lr]);
    set_cmd_return_variable($SS_r, "edited_views", 
                            [$TYPE_LIST, $edited_views_lr]);
    set_cmd_return_variable($SS_r, "skipped_views", 
                            [$TYPE_LIST, $skipped_views_lr]);

    print "Done.  Don't forget to p4 submit.\n";
}

#
# p4_edit
#
sub p4_edit {
    my $SS_r = shift;

    my $p4_dfII_dir = $SS_r->{GS}{DFII_DIR};
    my $p4_spec_dir = $SS_r->{GS}{SPEC_DIR};
    my $do_cast     = 1;
    my $do_dfII     = 1;
    my $view        = "floorplan";
    my $cells_lr    = [];
   
    # parse args
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($arg eq "--cast-only") {
                $do_dfII = 0;
            }
            elsif ($arg eq "--dfII-only") {
                $do_cast = 0;
            }
            elsif ($arg eq "--view") {
                my $eq = shift_next_scalar(\@_);
                $view = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $view) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--p4-spec-dir") {
                my $eq = shift_next_scalar(\@_);
                $p4_spec_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $p4_spec_dir) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--p4-dfII-dir") {
                my $eq = shift_next_scalar(\@_);
                $p4_dfII_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $p4_dfII_dir) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            else {
                command_die($SS_r, "Unrecognized argument $arg.");
            }
        }
        elsif ($type == $TYPE_LIST && @{$cells_lr}) {
            command_die($SS_r, "Too many lists specified to p4_add.");
        }
        elsif ($type == $TYPE_LIST) {
            $cells_lr = $arg;
        }
        else {
            command_die($SS_r, "Bad argument type specified to p4_add.");
        }
    }

    # Sanity check
    if ($do_cast && !-d $p4_spec_dir) {
        command_die($SS_r, "$p4_spec_dir doesn't exist.");
    }
    if ($do_dfII && !-e $p4_dfII_dir) {
        command_die($SS_r, "$p4_dfII_dir doesn't exist.");
    }

    # Determine cell list if one wasn't specified
    if (!@{$cells_lr}) {
        my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells";
        print "Determining cell list.\n";
        my $all_cells_lr = [];
        query_cast_server($SS_r, $cmd, $all_cells_lr, 1);
        foreach my $c (@{$all_cells_lr}) {
            my $f = fqcn_to_file($p4_spec_dir, $c);
            push @{$cells_lr}, $c if (-e $f);
        }
    }

    # p4 edit cast subtypes
    if ($do_cast) {
        if (!exists $SS_r->{GS}{P4_CAST_CLIENT}) {
            command_die($SS_r, "Required variable P4_CAST_CLIENT isn't set")
        }
        print "p4 editing " . scalar(@{$cells_lr}) . " cast subtypes...\n";
        foreach my $cell (@{$cells_lr}) {
            my $file = fqcn_to_file($p4_spec_dir, $cell);
            if (-e $file) {
                `p4 -c $SS_r->{GS}{P4_CAST_CLIENT} edit \"$file\"`;
            }
            else {
                print "Warning: Subtype $file doesn't exist.\n";
            }
        }
    }
    # cdsp4edit dfII views
    if ($do_dfII) {
        if (!exists $SS_r->{GS}{P4_DFII_CLIENT}) {
            command_die($SS_r, "Required variable P4_DFII_CLIENT isn't set")
        }
        print "p4 editing ". scalar(@{$cells_lr})." dfII floorplan views...\n";
        foreach my $cell (@{$cells_lr}) {
            my $cadence_cell = to_cadence($cell);
            if (-e get_cadence_cell_view_dir($p4_dfII_dir, $cadence_cell,
                                             $view)) {
                my $cmd = (path_to_tool($SS_r, "cdsp4edit", $LOCAL_JOB))[0];
                $cmd .= " \"--dfII-dir=$p4_dfII_dir\" ";
                $cmd .= "--view-name=$view ";
                $cmd .= "--client-spec=$SS_r->{GS}{P4_DFII_CLIENT} ";
                $cmd .= "\"$cadence_cell\"";
                `$cmd`;
                command_die($SS_r, "Couldn't run $cmd.") if ($?);
            }
            else {
                print "Warning: $view view for $cell doesn't exist.\n";
            }
        }
    }
}

#
# p4_branch
#
sub p4_branch {
    my $SS_r = shift;

    my $p4_dfII_dir = $SS_r->{GS}{DFII_DIR};
    my $p4_spec_dir = $SS_r->{GS}{SPEC_DIR};
    my $src_branch;
    my $dst_branch;
    my $branch_name = "";
    my $do_cast     = 1;
    my $do_dfII     = 1;
    my $cells_lr    = [];
   
    # parse args
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($arg eq "--cast-only") {
                $do_dfII = 0;
            }
            elsif ($arg eq "--dfII-only") {
                $do_cast = 0;
            }
            elsif ($arg eq "--branch-name") {
                my $eq = shift_next_scalar(\@_);
                $branch_name = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $branch_name) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--p4-spec-dir") {
                my $eq = shift_next_scalar(\@_);
                $p4_spec_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $p4_spec_dir) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--p4-dfII-dir") {
                my $eq = shift_next_scalar(\@_);
                $p4_dfII_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $p4_dfII_dir) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--src-branch") {
                my $eq = shift_next_scalar(\@_);
                $src_branch = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $src_branch) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            elsif ($arg eq "--dest-branch") {
                my $eq = shift_next_scalar(\@_);
                $dst_branch = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $dst_branch) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
            }
            else {
                command_die($SS_r, "Unrecognized argument $arg.");
            }
        }
        elsif ($type == $TYPE_LIST && @{$cells_lr}) {
            command_die($SS_r, "Too many lists specified to p4_branch.");
        }
        elsif ($type == $TYPE_LIST) {
            $cells_lr = $arg;
        }
        else {
            command_die($SS_r, "Bad argument type specified to p4_add.");
        }
    }
    if (!defined $src_branch || !defined $dst_branch) {
        command_die($SS_r, "You must specify src-branch and dest-branch.");
    }

    # Sanity check
    if ($do_cast && !-d $p4_spec_dir) {
        command_die($SS_r, "$p4_spec_dir doesn't exist.");
    }
    if ($do_dfII && !-e $p4_dfII_dir) {
        command_die($SS_r, "$p4_dfII_dir doesn't exist.");
    }
    if ($do_cast && !exists $SS_r->{GS}{P4_CAST_CLIENT}) {
        command_die($SS_r, "Required variable P4_CAST_CLIENT ".
            "isn't set.");
    }
    if ($do_dfII && !exists $SS_r->{GS}{P4_DFII_CLIENT}) {
        command_die($SS_r, "Required variable P4_DFII_CLIENT ".
            "isn't set.");
    }

    # Determine cell list if one wasn't specified
    if (!@{$cells_lr}) {
        my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells";
        print "Determining cell list.\n";
        query_cast_server($SS_r, $cmd, $cells_lr, 1);
    }
    
    # create branch.spec file
    my $work_dir = $SS_r->{GS}{WORK_DIR};
    open BS, ">$work_dir/branch.spec" 
        or command_die($SS_r,"Can't write $work_dir/branch.spec.");
    print BS "Branch: $branch_name\n";
    print BS "Owner: $ENV{USER}\n";
    print BS "Description: branch $SS_r->{GS}{TOP} from $src_branch ".
             "to $dst_branch\n";
    print BS "View:\n";

    # find and translate perforce depot locations of all modules
    my %src_spec_depot = ();
    my %dst_spec_depot = ();
    my %src_dfII_depot = ();
    my %dst_dfII_depot = ();
    for my $fqcn (@{$cells_lr}) {
        if ( $fqcn =~ /(.*)\.([^\.]+\.[^\.]+)/ ) {
            my $module = $1;
            $module =~ s:\.:/:g; # replace . with /
            if (!(defined $src_spec_depot{$module})) {
                my $where;
                if ($do_cast) {
                    $where = q4_where($SS_r, $SS_r->{GS}{P4_CAST_CLIENT},
                                    $p4_spec_dir, "${p4_spec_dir}/${module}");
                    $src_spec_depot{$module} = $where;
                    $where =~ s:/${src_branch}/:/${dst_branch}/:;
                    $dst_spec_depot{$module} = $where;
                }
                if ($do_dfII) {
                    $where = q4_where($SS_r, $SS_r->{GS}{P4_DFII_CLIENT},
                                    $p4_dfII_dir, "${p4_dfII_dir}/${module}");
                    $src_dfII_depot{$module} = $where;
                    $where =~ s:/${src_branch}/:/${dst_branch}/:;
                    $dst_dfII_depot{$module} = $where;
                }
            }
        }
    }

    # branch spec
    if ($do_cast) {
        for my $fqcn (@{$cells_lr}) {
            if ( $fqcn =~ /(.*)\.([^\.]+\.[^\.]+)/ ) {
                my $module = $1;
                my $cell = $2;
                $module =~ s:\.:/:g; # replace . with /
                $cell   =~ s:\.:/:g; # replace . with /
                print BS "\t$src_spec_depot{$module}/${cell}.cast "
                    . "$dst_spec_depot{$module}/${cell}.cast\n";
            }
        }
    }
    
    if ($do_dfII) {
        # branch data.dm and cdsinfo.tag of dfII modules
        for my $module (sort keys %src_dfII_depot) {
            # data.dm
            print BS "\t$src_dfII_depot{$module}/data.dm " .
                "$dst_dfII_depot{$module}/data.dm\n";
            print BS "\t$src_dfII_depot{$module}/cdsinfo.tag " .
                "$dst_dfII_depot{$module}/cdsinfo.tag\n";
        }

        # branch dfII of cells
        for my $c (@{$cells_lr}) {
            my $fqcn = to_cadence($c);
            if ( $fqcn =~ /(.*)\.([^\.]+\.[^\.]+)/ ) {
                my $module = $1;
                my $cell = $2;
                $cell = "${module}.${cell}";
                # Escape cell name for p4 
                $cell =~ s/\./#2e/g;
                $cell =~ s/-/#2d/g;
                $cell = p4_escape($cell);
                $module =~ s:\.:/:g;      # replace . with /
                print BS "\t$src_dfII_depot{$module}/${cell}/floorplan/... "
                     . "$dst_dfII_depot{$module}/${cell}/floorplan/...\n";
            }
        }
    }

    # create branch spec with perforce
    close BS;
    if ($branch_name ne "") { 
        system("cat $work_dir/branch.spec | p4 branch -i"); 
    }
    else { 
        print "Warning: --branch-name unspecified, only wrote branch.spec\n"; 
    }
}

# wrapper around p4 commands to determine whether they complete successfully or
# not (since it seems to never set errno in a rational way).  Returns nonzero
# if error.
sub p4_wrap {
    my $action = shift;
    my $client = shift;
    my $cwd = shift;

    my $cmd = "p4 -s -c $client -d \"$cwd\" $action";
    foreach my $arg (@_) { $cmd .= " \"" . p4_escape($arg) . "\""; }
    open(P4, "$cmd|") || return 1;
    my $err = 0;
    while (<P4>) {
        if (/^([^:]+): (.*)$/) {
            print "$2\n" unless ($1 eq "exit");
            $err = 1 if ($1 eq "error");
        }
        else {
            print;
        }
    }
    close P4;
    return $err;
}

# Escape '#' to %23
sub p4_escape {
    my $file = shift;
    $file =~ s/\#/%23/g;
    return $file;
}

# find depot location of a perforce file (using p4 -- no longer w/ q4)
sub q4_where {
    my $SS_r   = shift;
    my $client = shift;
    my $cwd    = shift;
    my $file   = shift;

    my $cmd = "p4 -c $client -d '$cwd' where '$file'";
    open P4_IN, "$cmd|" or command_die($SS_r, "Can't exec $cmd.");
    my $where = "";
    while (my $line = <P4_IN>) {
        if ($line =~ /(^\/\/depot\/\S+)/) { 
            $where = $1; 
        }
        elsif ( $line =~ /\.\.\.\s+depotFile\s+(\S+)/) {
            $where = $1;
        }
    }
    close P4_IN;
    if ($where eq "") {
        command_die($SS_r, "Error obtaining depot location for file $file.");
    }
    return $where;
}



1;
