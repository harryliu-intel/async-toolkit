#!/usr/bin/perl
# This script wraps around vs2cast and lve to make it easier to construct a PLT
# simulation from partially extracted .v and .spf files.  The general flow is:
# 1. Munge .v and .spf files to sanitize special characters.
# 2. Run vs2cast to import the .v file into CAST.
# 3. Run lve to create directives.0
# 4. Run generate_plt_subtypes to generate a PLT tree
#
# The .v and .spf files are assumed to contain one cell.  The definitions of
# instantiated cells are assumed to already exist in CAST (so the standard cell
# library should have been imported previously), and should not be included in
# the files.

use strict;
use File::Path;
use File::Spec::Functions qw/:ALL/;
use Getopt::Long;

# find relevant packaged tools and libraries
BEGIN {
    my $package_root = $0;
    my $exe = $package_root;
    $exe =~ s:.*/::;
    if (! ($package_root =~ m:^/:)) {
        my $pwd = `pwd`;
        chomp $pwd;
        $package_root = $pwd;
        $package_root .= "/$0";
        $package_root =~ s:$exe$::;
        $package_root =~ s://:/:g;
        chdir $package_root;
        $package_root = `pwd`;
        chomp $package_root;
        chdir $pwd;
    }
    else {
        $package_root =~ s:/bin/$exe::;
    }
    push @INC, "$package_root/lib/perl";
}

use rvp;

sub munge {
    my $txt = shift;
    $txt =~ s/\\//g;
    $txt =~ s/[\[\]\.\/]/_/g;
    return $txt;
}

sub munge_nodot {
    my $txt = shift;
    $txt =~ s/\\//g;
    $txt =~ s/[\[\]\/]/_/g;
    return $txt;
}

sub munge_verilog {
    local $_;
    my ($oldname, $newname, $ports, $insts, $cin, $cout) = @_;
    my %mungemap;
    foreach my $port (keys %{$ports}) {
        $mungemap{$port} = munge($port);
    }
    $mungemap{$cin} = $cout;
    foreach my $inst (keys %{$insts}) {
        $mungemap{$inst} = munge($inst);
    }

    open(my $ifh, $oldname) || die "Cannot open $oldname: $!";
    open(my $ofh, ">$newname") || die "Cannot open $newname: $!";
    while (<$ifh>) {
        chomp;
        my @tokens = split;
        foreach my $token (@tokens) {
            my $prefix = "";
            my $postfix = "";
            if ($token =~ /(.*)(\(\..*)/) {
                $token = $1;
                $postfix = $2;
            } elsif ($token =~ /(.*\()(.*)/) {
                $prefix = $1;
                $token = $2;
            }
            if (exists($mungemap{$token})) {
                $token = $mungemap{$token};
            }
            $token = $prefix . $token . $postfix;
        }
        print $ofh join(" ", @tokens) . "\n";
    }
    close($ifh);
    close($ofh);
}

sub exist_munge {
    my ($mungemap, $text) = @_;
    if (exists($mungemap->{$text})) {
        return $mungemap->{$text};
    }
    $text =~ s/[\/_]/ /g;
    if (exists($mungemap->{$text})) {
        return $mungemap->{$text};
    } else {
        return undef;
    }
}

my %guids;
my $guid = 10000;
# return a "globally" unique identifier
sub get_guid {
    my $text = shift;
    unless (exists($guids{$text})) {
        $guids{$text} = $guid;
        $guid++;
    }
    return $guids{$text};
}

sub cell2dir {
    my $cell = shift;
    $cell =~ s/\./\//g;
    return $cell;
}

# the SPICE file contains more circuitry than the Verilog file, so some names
# will have been unmunged, but the rawing process doesn't like / in name
my %slashmap;
my %slashids;
sub munge_all {
    my $orig = shift;
    my $name = munge_nodot($orig);
    if ($name ne $orig) {
        my $lookup = $slashmap{$orig};
        return $lookup if (defined($lookup));

        my $id = $slashids{$name};
        $id = 0 unless (defined($id));
        $slashids{$name} = $id++;
        $name .= "_PLT_SCENARIOS$id";
        $slashmap{$orig} = $name;
    }
    return $name;
}

sub munge_spice_token {
    my ($token, $mungemap, $all) = @_;
    my $replace1 = exist_munge($mungemap, $token);
    if (defined($replace1)) {
        $token = $replace1;
    } elsif ($token =~ /([^:]+):(.*)/) {
        my ($net, $subnet) = ($1, $2);
        my $replace2 = exist_munge($mungemap, $net);
        $replace2 = munge_all($net) if (!defined($replace2) && $all);
        if ($subnet =~ /^\d+$/) {
            # assume token is a segment of a net if the subnet consists
            # only of digits
            $token = $replace2 . ':' . $subnet;
        } else {
            # otherwise consider token a port in the form of inst:port
            $token = $replace2 . '.' . $subnet;
            $token = $token . ':' . get_guid($token);
        }
    } elsif ($token =~ /_(Vdd|GND)\b/) {
        # because the power nets are not extracted, StarRC uses
        # placeholders that look like instance_Vdd and instance_GND for
        # Vdd and GND ports in an asynchronous cell, respectively;
        # convert these placeholders to real Vdd and GND.
        $token = $1;
    } else {
        $token = munge_all($token) if $all;
    }
    return $token;
}

sub munge_spice {
    local $_;
    my ($oldname, $newname, $ports, $insts, $cin, $cout, $gates, $lvedir,
        $views) = @_;

    my %mungemap;
    foreach my $port (keys %{$ports}) {
        my $xport = $port;
        $xport =~ s/\\//;
        $mungemap{$xport} = munge($port);
        $xport =~ s/[\/_]/ /g;
        $mungemap{$xport} = munge($port);
    }

    # munge the cell name
    $mungemap{$cin} = $cout;

    # include mapping of standard gates
    foreach my $gate (keys %{$gates}) {
        $mungemap{$gate} = $gates->{$gate};
    }

    foreach my $inst (keys %{$insts}) {
        my $xinst = $inst;
        $xinst =~ s/\\//;
        $xinst =~ s/[\/_]/ /g;
        $mungemap{$xinst} = munge($inst);
    }

    open(my $ifh, $oldname) || die "Cannot open $oldname: $!";
    open(my $ofh, ">$newname") || die "Cannot open $newname: $!";

    my %subckts = ();
    while (<$ifh>) {
        chomp;
        s/\\//g;
        my @tokens = split;
        my $instance = '';
        my $subckt = '';

        if ($tokens[0] =~ /^X(.*)/) {
            my $replace = exist_munge(\%mungemap, $1);
            if (defined($replace)) {
                $instance = 'X' . $replace;
                shift @tokens;
            }
            # convert cell names from Cadence to CAST namespace
            $subckt = pop @tokens;
            $subckt =~ s/-L/(/g;
            $subckt =~ s/-R/)/g;
        }

        foreach my $token (@tokens) {
            $token = munge_spice_token($token, \%mungemap, 1);
        }
        $subckt = munge_spice_token($subckt, \%mungemap, 0);

        unless ($instance eq '') {
            print $ofh "$instance ";
        }
        print $ofh join(" ", @tokens);

        $subckts{$subckt} = 1 if ($subckt);

        # GND and Vdd are not extracted, and do not appear in the port list;
        # fake them here
        print $ofh " GND Vdd" if (/\.SUBCKT/);
        print $ofh " $subckt" if ($subckt);
        print $ofh "\n";
    }
    close($ifh);
    close($ofh);

    open($ofh, ">${newname}_include") || die "Cannot open ${newname}_include: $!";
    # include definition of subcircuits
    foreach my $subckt (keys %subckts) {
        my $spice;
        my $found = 0;
        foreach my $view (@{$views}) {
            $spice = catfile($lvedir, split(/\./, $subckt), $view, 'extracted',
                             'cell.spice');
            if (-r $spice) {
                print $ofh ".inc '$spice'\n";
                $found = 1;
                last;
            }
        }
        if (!$found) {
            print "Cannot found a pre-exising spice file for $subckt\n";
            print $ofh ".inc '$spice'\n";
        }
    }
    close($ofh);
}

sub modcell {
    my $cell = shift;
    my @cellparts = split(/\./, $cell);
    my $castcell = pop(@cellparts);
    my $castmodule = join('.', @cellparts);
    return ($castmodule, $castcell);
}

sub dirstr {
    my $dir = shift;
    return $dir eq 'input' ? '-' : ($dir eq 'output' ? '+' : '-+');
}

sub get_ports {
    my ($vdata, $cell) = @_;
    my (%nets, %ports, %insts);
    if ((my @signals = $vdata->get_modules_signals("$cell"))) {
        foreach my $signal (@signals) {
            my ($s_line,$s_a_line,$s_i_line,$s_type,$s_file,$s_p,$s_n,$s_type2,
                $s_r_file,$s_r_line,$range,$s_a_file,$s_i_file) =
                $vdata->get_module_signal($cell, $signal);
            $nets{$signal} = $s_type;
            if ($s_type =~ 'input|output|inout') {
                $ports{$signal} = $s_type;
            }
        }
    }
    my @inst = $vdata->get_first_instantiation($cell);
    while (@inst) {
        $insts{$inst[2]} = $inst[0];
        @inst = $vdata->get_next_instantiation($cell);
    }
    return (\%nets, \%ports, \%insts);
}

sub generate_cast {
    my ($ports, $outname, $cell) = @_;
    my ($castmodule, $castcell) = modcell($cell);
    ($castmodule, $castcell) = modcell($castmodule);
    open(my $fh, ">$outname") || die "Cannot open $outname: $!";
    print $fh "module $castmodule;\n";

    print $fh "define $castcell()(\n";
    my @sorted_ports = sort keys %{$ports};
    my $count = 1;
    foreach my $port (@sorted_ports) {
        my $dir = $ports->{$port};
        my $pm = dirstr($dir);
        print $fh "  node $pm" . munge($port);
        print $fh ';' if ($count < @sorted_ports);
        print $fh "\n";
        $count++;
    }

    print $fh ") {\n";
    print $fh "  verilog {\n";
    print $fh "    rtl {\n";

    print $fh "      $cell (\n";
    $count = 1;
    foreach my $port (@sorted_ports) {
        print $fh '        .' . munge($port) . '(' . munge($port) . ')';
        print $fh ',' if ($count < @sorted_ports);
        print $fh "\n";
        $count++;
    }
    print $fh "      ) : '';\n";
    print $fh "    }\n";
    print $fh "  }\n";
    print $fh "}\n";
    close($fh);
}

sub prefix {
    my $list = shift;
    my %result;
    my %remain;
    foreach my $elem (@{$list}) {
        if ($elem =~ /(.*)\[(\d+)\]/) {
            push @{$result{$1}}, $2;
        } else {
            $remain{$elem} = 1;
        }
    }
    return (\%result, \%remain);
}

sub getchannel {
    my $parts = shift;
    my $maxval = 0;  # at least create a e1of1
    foreach my $part (keys %{$parts}) {
        if ($part =~ /\.(\d+)$/) {
            $maxval = $1 if $maxval < $1;
        }
    }
    return "e1of" . ($maxval + 1);
}

sub out {
    my ($first, $fh, $txt) = @_;
    if ($first) {
        print $fh "$txt";
    } else {
        print $fh ";\n$txt";
    }
    return 0;
}

sub munge_again {
    my $name = shift;
    if ($name =~ /^(SCAN)\.(OUT|MODE|EN|IN)$/) {
        $name = "$1_$2";
    } else {
        $name =~ s/\[([a-zA-Z_][^\]]*)\]/_$1_/g;
    }
    return $name;
}

# Return a mapping for standard cells.  In the Artisan standard cell library,
# the list of cells can be obtained by reading the cell_list file included in
# the distribution.  Then, map the gate names (e.g., BUFX1HS) to the
# corresponding name in CAST (e.g., vendor.artisan.hs.gates.BUFX1HS.0).
sub get_gate_mapping{
    my ($cell_list, $module) = @_;
    my %gates = ();
    open(my $gfh, $cell_list) || die "Cannot open $cell_list: $!";
    while (<$gfh>) {
        chomp;
        $gates{$_} = "$module.$_.0";
    }
    close ($gfh);
    return \%gates;
}

sub generate_wrapper {
    local $_;
    my ($topv, $cell, $outname) = @_;
    my (%channel, %nonchannel, %portmap, %portdir, %dirs, %inverters, %specials);
    my ($castmodule, $castcell) = modcell($cell);
    open(my $fh, $topv) || die "Cannot open $topv: $!";
    while (<$fh>) {
        if (/^\s*(input|output|inout)\s+(\S+) ; \/\/ (\S+) (\d)/) {
            my ($dir, $port, $realport, $sense) = ($1, $2, $3, $4);
            my $short = (split('/', $realport))[-1];
            if ($short =~ /(.*)\.(\d+|e)$/) {
                $channel{$1}->{$short} = 1;
            } else {
                $nonchannel{$short} = 1;
            }
            $portmap{$port} = $short;
            $portdir{$port} = $dir;
            $dirs{$short} = $dir;
            $inverters{$port} = $sense;
        }
    }
    close($fh);

    open($fh, ">$outname") || die "Cannot open $outname: $!";
    print $fh "module $castmodule;\n";
    print $fh "define ${castcell}_WRAP()(\n";
    my ($groups, $left) = prefix([keys %nonchannel]);
    my $first = 1;
    foreach my $group (keys %{$groups}) {
        my @indicies = sort { $a <=> $b } @{$groups->{$group}};
        my $min = $indicies[0];
        my $max = $indicies[-1];
        my $dir = dirstr($dirs{$group . "[$min]"});
        if ($min == 0) {
            $first = out($first, $fh, "  node[" . ($max + 1) . "] $dir" .
                                      munge_again($group));
        } else {
            $first = out($first, $fh, "  node $dir" . munge_again($group) .
                                      "[$min..$max]");
        }
    }
    foreach my $single (keys %{$left}) {
        if ($single =~ /(SPC\wSCAN_)|(DFT_sdo)|(DFT_sdi)/) {
            $specials{$single} = $dirs{$single};
        } else {
            my $dir = dirstr($dirs{$single});
            $first = out($first, $fh, "  node $dir" . munge_again($single));
        }
    }

    ($groups, $left) = prefix([keys %channel]);
    foreach my $group (keys %{$groups}) {
        my @indicies = sort { $a <=> $b } @{$groups->{$group}};
        my $min = $indicies[0];
        my $max = $indicies[-1];
        my $chan = getchannel($channel{$group . "[$min]"});
        my $dir = dirstr($dirs{$group . "[$min].0"});
        if ($min == 0) {
            $first = out($first, $fh, "  $chan" . '[' . ($max + 1) . "] $dir" .
                                      munge_again($group));
        } else {
            $first = out($first, $fh, "  $chan $dir" . munge_again($group) .
                                      "[$min..$max]");
        }
    }
    foreach my $single (keys %{$left}) {
        my $chan = getchannel($channel{$single});
        my $dir = dirstr($dirs{"$single.0"});
        $first = out($first, $fh, "  $chan $dir". munge_again($single));
    }
    print $fh ") {\n";
    print $fh "  subcells {\n";
    print $fh "    $cell x;\n";
    foreach my $special (keys %specials) {
        print $fh "    node $special";
        print $fh " = GND" if ($specials{$special} eq 'input');
        print $fh ";\n";
    }
    my $dummy = 0;
    foreach my $port (sort keys %portmap) {
        if ($inverters{$port} == 0) {
            print $fh "    lib.util.operator.INV _(";
            if ($portdir{$port} eq "input") {
                print $fh munge_again($portmap{$port}) . ", x." . munge($port);
            } else {
                print $fh "x." . munge($port) . ", " .
                          munge_again($portmap{$port});
            }
            print $fh ");\n";
        } else {
            print $fh "    x." . munge($port) . " = " .
                      munge_again($portmap{$port}) . ";\n";
        }
    }
    print $fh "  }\n";
    print $fh "}\n";
    close($fh);
}

my ($verilog, $spice, $workdir, $cellin, $cellout, $spicecell, $cell_list);
my ($std_cell_module, $lvedir, $castdir, $specdir, $tasks, $view);
my ($output_dir, $pdk_root, $qsub, $jobs, $maxNodePerAlintBin);
my ($corner, $temp, $true);
#$verilog = '/mnt/fulcrum/fiji/youxin/hw-bali/layout/tsmc13/spar/chip/bali/fc/handler/BALI_FRAME_HANDLER/500/FM/plt/chip.bali.fc.handler.BALI_FRAME_HANDLER.511.v';
$verilog = '/scratch/net/rliu/bali-final/layout/tsmc13/spar/chip/bali/fc/handler/BALI_FRAME_HANDLER/500/FM/plt/chip.bali.fc.handler.BALI_FRAME_HANDLER.512.v';
$spice = '/scratch/net/rliu/bali-final/layout/tsmc13/spar/chip/bali/fc/handler/BALI_FRAME_HANDLER/500/FM/plt/chip.bali.fc.handler.BALI_FRAME_HANDLER.512.spf';
$cellin = 'chip.bali.fc.handler.BALI_FRAME_HANDLER.512';
$cellout = 'qa.cdc.egress_throttle.EGRESS_THROTTLE.500';
$spicecell = 'chip.bali.fc.handler.BALI_FRAME_HANDLER.500';
$cell_list = '/scratch/net/rliu/bali-final/layout/tsmc13/spar/vendor/artisan/hs/gates/sc-x/cell_list';
$std_cell_module = 'vendor.artisan.hs.gates';
$lvedir = '/mnt/fulcrum/bali/lve/lve';
$view = 'lvsclean,layout_tag,layout_pg,layout';
$output_dir = '.';
$pdk_root = '';
$corner = 'ff';
$temp = '125';
$true = '1.08';
GetOptions('verilog=s', \$verilog,
           'spice=s', \$spice,
           'cellin=s', \$cellin,
           'cellout=s', \$cellout,
           'workdir=s', \$workdir,
           'standard-cell-module=s', \$std_cell_module,
           'sub-lve-root-dir=s', \$lvedir,
           'cast-dir=s', \$castdir,
           'spec-dir=s', \$specdir,
           'tasks=s', \$tasks,
           'extracted-view=s', \$view,
           'output-dir=s', \$output_dir,
           'fulcrum-pdk-root=s', \$pdk_root,
           'qsub=s', \$qsub,
           'jobs=s', \$jobs,
           'maxNodePerAlintBin=s', \$maxNodePerAlintBin,
           'corner=s', \$corner,
           'temp=s', \$temp,
           'true=s', \$true);

my @actions = split(/,/, $tasks);
my @views = split(/,/, $view);

if (grep(/munge/, @actions)) {
    mkpath($workdir, 0, 0711) unless -d $workdir;

    print "Parsing $verilog\n";
    my $vdata = rvp->read_verilog([$verilog], [],{},1,[],[],'');

    my ($nets, $ports, $insts) = get_ports($vdata, "\\$cellin");

    my $munged_v = catfile($workdir, "$cellout.v");
    print "Munging verilog: $munged_v\n";
    munge_verilog($verilog, $munged_v, $nets, $insts, "\\$cellin", $cellout);

    my $munged_spice = catfile($workdir, "$cellout.spice");
    print "Munging spice: $munged_spice\n";
    my $gates = get_gate_mapping($cell_list, $std_cell_module);
    munge_spice($spice, $munged_spice, $nets, $insts, $spicecell, $cellout,
                $gates, $lvedir, \@views);

    my ($castmodule, $castcell) = modcell($cellout);
    my $wrapped_cast = catdir($specdir, split(/\./, $castmodule));
    mkpath($wrapped_cast, 0, 0777) unless -d $wrapped_cast;
    $wrapped_cast = catfile($wrapped_cast, "${castcell}_WRAP.cast");

    ($castmodule, $castcell) = modcell($castmodule);
    ($castmodule, $castcell) = modcell($castmodule);
    my $munged_cast = catdir($castdir, split(/\./, $castmodule));
    mkpath($munged_cast, 0, 0777) unless -d $munged_cast;
    $munged_cast = catfile($munged_cast, "$castcell.cast");
    print "Generating cast: $munged_cast\n";
    generate_cast($ports, $munged_cast, $cellout);

    my $topverilog = $verilog;
    $topverilog =~ s/\.v/.top.v/g;
    print "Generating wrapper: $wrapped_cast\n";
    generate_wrapper($topverilog, $cellout, $wrapped_cast);
} 
if (grep(/import/, @actions)) {
    my $cache = catdir($workdir, 'cache');
    mkpath($cache, 0, 0711) unless -d $cache;
    my @cmds = ('vs2cast',
                '--cast-dir=' . $castdir,
                '--spec-dir=' . $specdir,
                '--library=' . $cellout,
                "$cellout.v",
                $cellout);
    system(@cmds);
    my ($castmodule, $castcell) = modcell($cellout);
    my $dir = catdir($specdir, split(/\./, $castmodule));
    mkpath($dir, 0, 0777) unless -d $dir;
    @cmds = ('cp',
             "$cellout.cast",
             catfile($dir, "$castcell.cast"));
    system(@cmds);
}
if (grep(/alint/, @actions)) {
    my @cmds = ('lve',
                '--fulcrum-pdk-root=' . $pdk_root,
                '--cast-dir=' . $castdir,
                '--spec-dir=' . $specdir,
                '--mode=custom',
                '--task=alint',
                '--custom-spice=' . catfile($workdir, "$cellout.spice"),
                '--custom-spice-include=' .
                    catfile($workdir, "$cellout.spice_include")
               );
    push @cmds, '--output-dir=' . $output_dir if (defined($output_dir));
    push @cmds, '--qsub=' . $qsub if (defined($qsub));
    push @cmds, '--jobs=' . $jobs if (defined($jobs));
    push @cmds, '--maxNodePerAlintBin=' . $maxNodePerAlintBin
        if (defined($maxNodePerAlintBin));
    push @cmds, '--corner=' . $corner;
    push @cmds, '--temp=' . $temp;
    push @cmds, '--true=' . $true;
    push @cmds, $cellout;
    system(@cmds);
}
