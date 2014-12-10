#
# LayoutModeling
#
# Supersize module for modeling properties of the layout flow
#
#

package Supersize::LayoutModeling;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &query_manual_density_factors
        &calculate_manual_cost
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Netlist;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;

sub get_module_data {
  return { NAME => "LayoutModeling",
           DESC => "Commands for modeling properties of the layout flow.",
           COMMANDS => {
    "layout_cost"    => {
        SUBREF => \&layout_cost,
        USAGE  => "layout_cost " . underline("cell") . " | " .
                                   underline("cell-list"),
        DESC   =>
          "Estimates the layout cost of the specified cell(s).  ".
          "The cost in measured as the expected number of minutes required ".
          "to complete the cell's layout using the manual (non-GLC) flow.",
        GLOBALS => { WORK_DIR     => { REQUIRED => 1 },
                     CAST_DIR     => { REQUIRED => 1 },
                     SPEC_DIR     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } },
        RV     => {
          result => {
            TYPE => $TYPE_MAP,
            DESC => "Map of cell to layout cost." } } }
    }
  };
}


#
# layout_cost
# 
sub layout_cost {
    my $SS_r = shift;
    my @cells;

    # parse args
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($val =~ /^--/) {
                command_die($SS_r, "Unrecognized option $val.");
            }
            elsif (@cells) {
                command_die($SS_r, "Too many arguments to layout_cost.");
            }
            else {
                push @cells, $val;
            }
        }
        elsif ($type == $TYPE_LIST) {
            if (@cells) {
                command_die($SS_r, "Too many arguments to layout_cost.");
            }
            else {
                push @cells, @{$val};
            }
        }
        else {
            command_die($SS_r, "Bad argument specified to layout_cost.")
        }
    }

    # query netlist info
    my $props_r = {};
    query_netlist_props($SS_r, get_work_dir($SS_r), \@cells, $props_r);
    query_manual_density_factors($SS_r, \@cells, $props_r);

    # Estimate costs
    my $costs_r = {};
    my $c;
    STDOUT->format_top_name("Cost_Top");
    STDOUT->format_name("Cost_List");
    STDOUT->format_lines_per_page(10000);
    STDOUT->format_lines_left(0);
    foreach $c (@cells) {
        $costs_r->{$c} = [ $TYPE_SCALAR, 
                           calculate_manual_cost($c, $props_r->{$c}) ];
        write;
    }

    set_cmd_return_variable($SS_r, "result", [$TYPE_MAP, $costs_r]);

    format Cost_Top =
 Cell                                                  Cost (minutes)
 ----------------------------------------------------- --------------
.
    format Cost_List =
 @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @>>>>>>>>>>>>>
$c,sprintf("%.4g",$costs_r->{$c}[1])
.
}


############################## Utility Functions ##############################

# Look up base cast (manual flow) density factor of specified list of 
# subtypes.  Sets the values in the map reference as fqcn->manual_df.
sub query_manual_density_factors {
    my $SS_r     = shift;
    my $cells_lr = shift;
    my $d_r      = shift;

    # set up map
    my $base_df_mr = {};
    foreach my $c (@{$cells_lr}) {
        my $b = basetype_of($c);
        $base_df_mr->{$b} = 0.0;
    }

    # issue query command
    my $lines_lr = [];
    if (exists $SS_r->{GS}{TOP}) {
        my $TOP_base = basetype_of($SS_r->{GS}{TOP});
        my $cmd = "--cell=$TOP_base --task=density --filter=leaf";
        query_cast_server($SS_r, $cmd, $lines_lr, 1);
    }

    # process output
    foreach my $l (@{$lines_lr}) {
        my ($b, $df) = split /\s+/, $l;
        if (exists $base_df_mr->{$b}) {
            $base_df_mr->{$b} = $df;
        }
    }
    foreach my $c (@{$cells_lr}) {
        my $b = basetype_of($c);
        if ($base_df_mr->{$b} == 0.0) {
            my $cmd = "--cell=$b --task=density";
            query_cast_server($SS_r, $cmd, $lines_lr, 1);
            $base_df_mr->{$b} = (split /\s+/, $lines_lr->[0])[1];
            if (!defined $base_df_mr->{$b} || $base_df_mr->{$b} == 0.0) {
                print STDERR "Warning: Couldn't find manual density factor ".
                             "for\n         $c.  Assigning 20.\n";
                $base_df_mr->{$b} = 20.0;
            }
        }
        $d_r->{$c}{manual_df} = $base_df_mr->{$b};

        if ($d_r->{$c}{manual_df} == 1.0) {
            print STDERR "Warning: Cell $c has density factor 1.0.\n";
            print STDERR "         Overriding to 20.\n";
            $d_r->{$c}{manual_df} = 20.0;
        }
    }
}

# Simple model for the cost of routing a leaf cell manually vs using GLC.
# Cost is measured in minutes of a layout engineer's time.  Calibrated to
# give about 30 minutes for a BUF_1of4 and 4+ hours for more complicated
# leaf cells.
sub calculate_manual_cost {
    my $cell      = shift;
    my $cell_data = shift;

    my $cost = 15.0 * ($cell_data->{gate_cnt} + $cell_data->{stack_cnt}) +
               20.0 * $cell_data->{stack_fet_cnt};
    $cost /= $cell_data->{manual_df};

    # 15-minute penalty per precharge transistor.
    $cost += 15 * $cell_data->{precharge_cnt};

    # fixed cost of doing a manual cell, with savings due to templating
    # modelled.
    $cost += 10 unless ($cell =~ /^lib\.buffer\.half\./);

    return $cost;
}

1;
