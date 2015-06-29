#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use File::Spec::Functions;
use feature "switch";

BEGIN {
    my $lve_root = $0;
    $lve_root =~ s:/[^/]*$::;
    $lve_root =~ s:/[^/]*$::;
    @INC = ("$lve_root/lib/perl", @INC);
}

use LveUtil qw/reName/;

my ($spice, $spef, $dpf, $cell, $nodeprops, $vdd, $temp, $corner, $workdir, $pdkroot);
GetOptions("spice=s"            => \$spice,
           "spef=s"             => \$spef,
           "dpf=s"              => \$dpf,
           "cell=s"             => \$cell,
           "corner=s"           => \$corner,
           "true=s"             => \$vdd,
           "temp=s"             => \$temp,
           "node-props=s"       => \$nodeprops,
           "fulcrum-pdk-root=s" => \$pdkroot,
           "working-dir=s"      => \$workdir) || die;
$vdd =~ s/V$//;
$temp =~ s/C$//;

sub set_app_vars {
    my ($appvars) = @_;
    return join('', map { "set_app_var $_ $appvars->{$_}\n" }
                        sort keys %{$appvars});
}

sub set_ports_dir {
    my ($nodeprops) = @_;
    local $_;
    open(my $fh, $nodeprops) || die "Can't open $nodeprops: $!";
    my %dirs = map { my @f = split;
                     my $offset = $f[0] eq 'SIGNOFF';
                     $f[0 + $offset] => $f[16 + $offset] }
                   <$fh>;
    close($fh);
    my %map = ();
    reName('rename', 'cast', 'gds2', 'node', \%map, [ keys %dirs ]);
    my $result = join('',
                      map { my $opt;
                            given($dirs{$_}) {
                                when ('IN') { $opt = "input"; }
                                when ('OUT') { $opt = "output"; }
                                when ('INOUT') { $opt = "inout"; }
                            }
                            $opt ? "set_port_direction -$opt $map{$_}\n" : "" }
                          keys %dirs);
    return $result;
}

sub set_tech_directives {
    my ($pdkroot, $temp, $dirs) = @_;
    my $default = catfile($pdkroot, 'share', 'Fulcrum', 'spice', 'default.sp');
    return join('', map { "*nanosim tech=\"$_\"\n" } @{$dirs}) .
           ".include '$default'\n" .
           ".temp $temp\n" .
           ".option force_double_value = 1\n" .
           ".lib '/nfs/sc/proj/ctg/mrl108/mrl/collateral/P1273.1_x1r5u1_UPF_Collateral/P1273_1x1r5u1cmi_1.0/p1273_1x1r5u1cmi/p1273_1x1r5u1.hsp' $corner\n";
}

sub write_file {
    my ($workdir, $name, @content) = @_;
    my $full = catfile($workdir, $name);
    open(my $fh, '>', $full) || die "Can't write to $full: $!";
    map { print $fh $_ } @content;
    close($fh);
    return $full;
}

my %appvars = (
    'dcs_enable_analysis' => 'true',
    'dcs_enable_detailed_path_reporting' => 'true',
    'sh_source_logging' => 'false',
    'sh_new_variable_message' => 'false',
    'si_enable_analysis' => 'true',
    'topo_clock_gate_depth' => 20,
    'timing_analysis_coverage' => 'true',
    'lib_resistance_unit' => '1kohm',
    'lib_capacitance_unit' => '1ff',
    'lib_time_unit' => '1ps',
    'oc_global_voltage' => $vdd,
    'rc_input_threshold_full_transition' => 'false',
    'rc_slew_lower_threshold_pct_rise' => 20,
    'rc_slew_lower_threshold_pct_fall' => 20,
    'rc_slew_upper_threshold_pct_rise' => 80,
    'rc_slew_upper_threshold_pct_fall' => 80,
    'timing_enable_multi_input_switching' => 'false',
    'sim_miller_use_active_load' => 'true',
    'link_path' => '{*}',
    'link_transistor_drain_pin_name' => 'DRN',
    'link_transistor_source_pin_name' => 'SRC',
    'link_transistor_gate_pin_name' => 'GATE',
    'link_transistor_bulk_pin_name' => 'BULK',
    'tech_netlist_spice_model_name' => "{${corner}_${vdd}_${temp}}",
    'topo_tgate_mark_all_pairs' => 'true',
    'topo_auto_find_latch_clock' => 'true'
);

my @nanosim_tech = (
    "direct",
    "voltage $vdd",
    "vds 0 $vdd 0.02",
    "vgs 0 $vdd 0.02"
);

my $tech_sp = write_file($workdir, 'tech.sp',
                         set_tech_directives($pdkroot, $temp, \@nanosim_tech));

my $cell_sp = write_file($workdir, 'cell.sp',
                         ".param\n",
                         ".include '$spice'\n");

my $tclcmds = '';
$tclcmds .= <<EOF;
set_message_info -id TOPO-058 -limit 2
set_message_info -id TOPO-059 -limit 2
register_netlist -format spice "$tech_sp $cell_sp"
link_design -verbose -keep_capacitive_coupling {$cell}
read_device_parameters {$dpf}
read_parasitics -keep_capacitive_coupling -complete_with zero {$spef}
set_supply_net Vdd
set_supply_net -gnd GND
set_voltage $vdd Vdd
set_voltage 0 GND
EOF

# explicitly mark feedback transistor in van-Berkel gates based on naming
# convention.
$tclcmds .= <<'EOF';
foreach_in_collection fb [get_cells -quiet {*.Mpf *.Mnf}] {
    set_transistor_direction -transistor bidi $fb
    mark_feedback -transistors $fb
}
EOF

$tclcmds .= set_ports_dir($nodeprops);

$tclcmds .= <<EOF;
create_clock -name VCLK -period 800
set_input_delay -clock VCLK 100 [all_inputs]
set_output_delay -clock VCLK 100 [all_outputs]

match_topology -message_level debug
check_topology -message_level debug
check_design -message_level debug
trace_paths -pbsa
reset_design -paths
set_model_input_transition_indexes -nominal 32 {4 8 16 32 64 128 256} [all_inputs]
set_model_load_indexes {5 8 14 22 37 61 100} [all_outputs]
extract_model -arc_types {max min} -when -bus -pbsa -use_find_path -name "cell" -library_elements {nldm pg_pins} -debug {paths lib}
exit
EOF

my $runtcl = write_file($workdir, 'run.tcl',
                        set_app_vars(\%appvars),
                        $tclcmds);

my @cmd = ($ENV{'NT_SCRIPT'}, 'nt_shell', '-file', $runtcl);
system(@cmd) == 0 || die "Failed to execute @cmd: $!";
