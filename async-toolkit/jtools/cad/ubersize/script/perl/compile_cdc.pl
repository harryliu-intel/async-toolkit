#!/usr/intel/bin/perl -w
# AAG
# $Id$
# $DateTime$

use strict;
use IPC::Open2;
use Getopt::Long;

my $config_r = {
    SS_CREATE_SCRIPT => "",
    SS_SUBTYPE_SCRIPT => "",
    SS_QUERY_SCRIPT => "",
    SS_FILES_TO_KEEP => [
                          [ "wires.routed.debug", "wires.debug" ],
                          [ "cell_list" ],
                          [ "sink_source.debug" ]
                        ],
    CAST_DIR => "",
    SPEC_DIR => "",
    SPAR_DIR => "",
    OUT_DIR => "./",
    TMP_DIR => "",
    SKIP_CLEANUP => 0,
    PDK_ROOT => "",
    DEBUG => 0,
    SUBTYPE_MAX => 509,
    CLIENT => "",
    VERBOSE => 0,
    FORCE => 0,
};

sub usage_exit {
    my $config_r = shift;
    print <<EOF;
Usage: compile_cdc --cast-dir=<cast> --spec-dir=<spec>
            [--out-dir=<dir>]
            [--tmp-dir=<tmp-dir>]
            [--skip-cleanup]
            [--subtype-max=#]
            [--client=<p4 client>]
            [--verbose]
            [--force]   always overwrite perforce
            --cdc-list=<cdc_list_file> | <cdc_list>

       <cdc_list> is a list of abbreviated CDC type names, e.g. SCAN_A2S(5).
       (Fully-qualified cell names are also okay, as long as the subtype
       number is 500.)  <cdc_list_file>, if specified, will be read to obtain 
       a list of CDCs to generate.

       All temporary supersize files will be put under <tmp-dir>.  If
       --tmp-dir is not specified, one will be created under /scratch.
       In this case, the temporary directory will be deleted on exit unless
       the --skip-cleanup option is specified.

       Generates the following files in the following locations:
         CDL netlist                - <dir>/netlist/<fqcn>.cdl
         Verilog top-level netlist  - <dir>/netlist/<fqcn>.v 
         ATPG top-level netlist     - <dir>/atpg/<fqcn>.v 
         Non-default rules          - <dir>/def/<fqcn>.def
         Jauto wires.debug etc.     - <dir>/jauto/<cdc>/wires.debug

EOF
    if ($config_r->{DEBUG}) {
        foreach my $key (sort keys %{$config_r}) {
            if ($config_r->{$key} =~ /ARRAY/) {
                print "$key [".join(",", @{$config_r->{$key}})."]\n";
            }
            else {
                print "$key $config_r->{$key}\n";
            }
        }
    }
    exit;
}

#
# parse command args
#
my @cdc_list;

GetOptions (
    "out-dir=s" => \$config_r->{OUT_DIR},
    "debug" => \$config_r->{DEBUG},
    "cast-dir=s" => \$config_r->{CAST_DIR},
    "spec-dir=s" => \$config_r->{SPEC_DIR},
    "spar-dir=s" => \$config_r->{SPAR_DIR},
    "tmp-dir=s" => \$config_r->{TMP_DIR},
    "skip-cleanup" => \$config_r->{SKIP_CLEANUP},
    "fulcrum-pdk-root=s" => \$config_r->{PDK_ROOT},
    "cdc-list=s" => sub {
        open (LIST, "<".$_[1]) or usage_exit();
        while (<LIST>) {
            chomp;
            s/\s//g;
            if (/^#/) {
                next;
            } elsif(/^(SCAN_)?(A2S|S2A)\(\d+\)$/){
                push @cdc_list, $_;
            } elsif (/^lib.synchronous.conversion.v3\.([^\.]+)\.500$/) {
                push @cdc_list, $1;
            } else {
                die "Bad CDC in $_[1].\n" 
            }
        }
        close LIST;
    },
    # hidden args for debugging
    "query-script=s" => \$config_r->{SS_QUERY_SCRIPT},
    "subtype-script=s" => \$config_r->{SS_SUBTYPE_SCRIPT},
    "create-script=s" => \$config_r->{SS_CREATE_SCRIPT},
    "subtype-max=i" => \$config_r->{SUBTYPE_MAX},
    "client=s" => \$config_r->{CLIENT},
    "verbose" => \$config_r->{VERBOSE},
    "force" => \$config_r->{FORCE},
) or usage_exit($config_r);

my $verbose = ($config_r->{VERBOSE} or $config_r->{DEBUG});

# the following are used to generate the ATPG wrapper
# will not be used when the PDK is fully updated

my %cells130 = (
    "BUF" => ["BUFX1HS", "A", "Y"],
    "FF" => ["SDFFX1HS", "D", "SI", "SE", "CK", "Q"],
);

my %cellsav65 = (
    "BUF" => ["av_buf_a0", "A", "Q"],
    "FF" => ["av_dff0_a1", "D", "SI", "SE", "CK", "Q"],
);

my %cellsar65 = (
    "BUF" => ["BUFX1MA12TR", "A", "Y"],
    "FF" => ["SDFFQX1MA12TR", "D", "SI", "SE", "CK", "Q"],
);

# hack for using Avago cells for ATPG in case require file is not there
our %cells=%cellsav65;
if ( -s "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/CDCcells.pl") {
    require "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/CDCcells.pl";
}

if ( $config_r->{SS_CREATE_SCRIPT} eq "" and -f "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/cdc_create.ss") {
    $config_r->{SS_CREATE_SCRIPT} = "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/cdc_create.ss";
} else {
    $config_r->{SS_CREATE_SCRIPT} = path_to_script_dir()."/cdc_create.ss";
    print STDERR "Warning: cdc_create.ss not found in PDK\n";
}

if ( ! -f $config_r->{SS_CREATE_SCRIPT}) {
    print STDERR "cdc_subtype.ss not found.\n";
    usage_exit($config_r);
}

if ( $config_r->{SS_SUBTYPE_SCRIPT} eq "" and -f "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/cdc_subtype.ss") {
    $config_r->{SS_SUBTYPE_SCRIPT} = "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/cdc_subtype.ss";
} else {
    $config_r->{SS_SUBTYPE_SCRIPT} = path_to_script_dir()."/cdc_subtype.ss";
    print STDERR "Warning: cdc_subtype.ss not found in PDK\n";
}

if ( ! -f $config_r->{SS_SUBTYPE_SCRIPT}) {
    print STDERR "cdc_subtype.ss not found.\n";
    usage_exit($config_r);
}

if ( $config_r->{SS_QUERY_SCRIPT} eq "" and -f "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/cdc_query.ss") {
    $config_r->{SS_QUERY_SCRIPT} = "$config_r->{PDK_ROOT}/share/Fulcrum/cdc/cdc_query.ss";
} else {
    $config_r->{SS_QUERY_SCRIPT} = path_to_script_dir()."/cdc_query.ss";
    print STDERR "Warning: cdc_query.ss not found in PDK\n";
}

if ( ! -f $config_r->{SS_QUERY_SCRIPT}) {
    print STDERR "cdc_query.ss not found.\n";
    usage_exit($config_r);
}

while (@ARGV) {
    if ($ARGV[0] =~ /^(SCAN_)?(A2S|S2A)\(\d+\)$/) {
        push @cdc_list, $ARGV[0];
    }
    elsif ($ARGV[0] =~ /^lib.synchronous.conversion.v3\.([^\.]+)\.500$/) {
        push @cdc_list, $1;
    }
    else {
        usage_exit($config_r);
    }
    shift;
}
usage_exit($config_r) if (!@cdc_list);

if ($config_r->{CLIENT} ne "") {
    open (P4, "p4 -c '$config_r->{CLIENT}' client -o |");
    my $root;
    my $spec;
    my $cast;
    my $spar;
    my $dfII;
    while (<P4>) {
        chomp;
        if (/^Root:\s*(\S+)/) {
            $root = $1 if -d $1;
        }
        elsif (/^Client:\s*(\S+)/) {
            $config_r->{CLIENT} = $1 if $config_r->{CLIENT} eq "";
        }
        elsif ( ! defined($cast) and m://depot/\S*/cast/:) {
            s/.*\s//;
            s:/cast/.*:/cast:;
            s://$config_r->{CLIENT}:$root:;
            s://:/:g;
            $cast = $_ if -d $_;
        }
        elsif ( ! defined($spec) and m://depot/\S*/spec/:) {
            s/.*\s//;
            s:/spec/.*:/spec:;
            s://$config_r->{CLIENT}:$root:;
            s://:/:g;
            $spec = $_ if -d $_;
        }
        elsif ( ! defined($spar) and m://depot/\S*/spar/:) {
            s/.*\s//;
            s:/spar/.*:/spar:;
            s://$config_r->{CLIENT}:$root:;
            s://:/:g;
            $spar = $_ if -d $_;
        }
        last if (defined($cast) and defined($spec) and defined ($spar));
    }
    if (defined ($root) and -d $cast and -d $spec and -d $spar) {
        $config_r->{CAST_DIR} = $cast;
        $config_r->{SPEC_DIR} = $spec;
        $config_r->{SPAR_DIR} = $spar;
        if ($verbose) {
            print STDERR "Resetting cast-dir=$cast\n";
            print STDERR "Resetting spec-dir=$spec\n";
            print STDERR "Resetting spar-dir=$spar\n";
        }
    }
    else {
        print STDERR "Warning: Client $config_r->{CLIENT} does not exist.\n";
        $config_r->{CLIENT} = "";
    }
}

sub p4action {
    my ($src) = @_;
    if ( -d "$config_r->{SPAR_DIR}/lib/synchronous/conversion/v3") {
        my $dest=$src;
        $dest =~ s:$config_r->{OUT_DIR}:$config_r->{SPAR_DIR}/lib/synchronous/conversion/v3:;
        my $destdir=$dest;
        $destdir =~ s/\/[^\/]+$//;
        `mkdir -p "$destdir"` if ! ( -d $destdir );
        if ( -f $dest ) {
            if ( ! -w $dest and $config_r->{CLIENT} ne "" and $config_r->{FORCE}) {
                print STDERR "p4 -c '$config_r->{CLIENT}' edit '$dest'\n" if $verbose;
                `p4 -c '$config_r->{CLIENT}' edit '$dest'`;
            }
        }
        my $existdir=$dest;
        $existdir =~ s:/[^/]+$::;
        `mkdir -p "$existdir"` if ! -d $existdir;
        if ( -w $dest or ! -e $dest ) {
            if ( $config_r->{CLIENT} ne "") {
                print STDERR "p4 -c '$config_r->{CLIENT}' add '$dest'\n" if $verbose;
                `p4 -c '$config_r->{CLIENT}' add -f "$dest"`;
            }
            else {
                print STDERR "Please remember to check in '$dest'\n";
            }
        }
        else {
            print STDERR "Warning: cannot write to $dest\n" if $config_r->{FORCE};
        }
    }
    else {
        printf STDERR "Please remember to put $src into proper %s-dir location and add to p4\n",
            $src =~ /\.cast$/ ? "spec" : "spar";
    }
}

usage_exit($config_r)
    if $config_r->{CAST_DIR} eq "" or $config_r->{SPEC_DIR} eq "";

foreach my $path (split(/:/, $config_r->{CAST_DIR})) {
    if (! -d $path) {
        print STDERR "cast-dir $path does not exist";
        usage_exit($config_r);
    }
}

foreach my $path (split(/:/, $config_r->{SPEC_DIR})) {
    if (! -d $path) {
        print STDERR "spec-dir $path does not exist";
        usage_exit($config_r);
    }
}

#
# create temporary directory
#
my $cleanup_tmp = 0;
if ($config_r->{TMP_DIR} eq "") {
    $config_r->{TMP_DIR} = `mktemp -d -p /scratch compile_cdc.XXXXXX`; 
    chomp $config_r->{TMP_DIR};
    print "Created temporary working directory $config_r->{TMP_DIR}.\n";
    $cleanup_tmp = 1;
} elsif ( ! -d $config_r->{TMP_DIR}) {
    `mkdir -p "$config_r->{TMP_DIR}"`;
}

# create output directory
if ( ! -d $config_r->{OUT_DIR}) {
    `mkdir -p "$config_r->{OUT_DIR}"`;
}

if (! -d $config_r->{TMP_DIR}) {
    print STDERR "tmp-dir does not exist";
    usage_exit($config_r);
}

if (! -d $config_r->{OUT_DIR} ) {
    print STDERR "out-dir does not exist";
    usage_exit($config_r);
}

if (! -d $config_r->{PDK_ROOT}) {
    print STDERR "fulcrum-pdk-root does not exist";
    usage_exit($config_r);
}

#
# Compile each CDC
#
print "Generating CDCs.\n";
eval {
    generate_cdc($config_r, @cdc_list);
};
if ($@) {
    chomp $@;
    print "Error during CDC generation: $@\n";
}

#
# clean up temporary directory
#
END {
    if ($cleanup_tmp and !$config_r->{SKIP_CLEANUP}) {
        print "Removing temporary directory $config_r->{TMP_DIR}.\n";
        `rm -rf $config_r->{TMP_DIR}`;
    }
    else {
        print "Skipping clean-up of temporary directory $config_r->{TMP_DIR}.\n";
    }
}
#
# run supersize
#
sub generate_cdc {
    my $config_r = shift;
    my $tmp_dir  = $config_r->{TMP_DIR};

    open (SS_OUT, ">$tmp_dir/supersize.out") ||
        die "Couldn't create $tmp_dir/supersize.out.\n";

    print "Running Supersize to create and evaluate netlist.\n";
    my $cmd = "supersize --fulcrum-pdk-root='$config_r->{PDK_ROOT}' " .
              "'--work-dir=$tmp_dir/supersize'";
    open2(*SSO, *SSI, "$cmd") || die "Couldn't run $cmd.\n";
    print SSI <<EOF;
set SPEC_DIR = $config_r->{SPEC_DIR}
set CAST_DIR = $config_r->{CAST_DIR}
set TAU = 54
set MEM = 768M
EOF

    foreach my $cdc_type (@_) {
        my $cdc_fqcn = "lib.synchronous.conversion.v3.$cdc_type.500";
        print SSI "set CDC_TYPE = $cdc_type\n";
        print SSI "source $config_r->{SS_CREATE_SCRIPT}\n";
        while (<SSO>) {
            # maybe interpret output for errors, warnings, etc.
            print SS_OUT;
            last if (/CDC Creation Complete/);
            #die "Error running supersize.\n" if (/Sourcing/ and /failed/);
        }
    }

    # Now refresh cast server so subtypes are seen
    #print SSI "cast_server --refresh\n";

    foreach my $cdc_type (@_) {
        my $cdc_fqcn = "lib.synchronous.conversion.v3.$cdc_type.500";
        print SSI "set CDC_TYPE = $cdc_type\n";
        print SSI "source $config_r->{SS_SUBTYPE_SCRIPT}\n";
        while (<SSO>) {
            # maybe interpret output for errors, warnings, etc.
            print SS_OUT;
            last if (/CDC Subtyping Complete/);
            #die "Error running supersize.\n" if (/Sourcing/ and /failed/);
        }
    }

    # Now refresh cast server so all resubtyping takes effect
    print SSI "cast_server --refresh\n";

    foreach my $cdc_type (@_) {
        my $cdc_fqcn = "lib.synchronous.conversion.v3.$cdc_type.500";
        print SSI "set CDC_TYPE = $cdc_type\n";
        print SSI "source $config_r->{SS_QUERY_SCRIPT}\n";
        while (<SSO>) {
            # maybe interpret output for errors, warnings, etc.
            print SS_OUT;
            last if (/CDC Query Complete/);
            #die "Error running supersize.\n" if (/Sourcing/ and /failed/);
        }

        # Copy files of interest to output directory
        my $cdc_jauto_dir = "$config_r->{OUT_DIR}/jauto/$cdc_type";
        `mkdir -p '$cdc_jauto_dir'` if (!-e $cdc_jauto_dir);
        foreach my $f (@{$config_r->{SS_FILES_TO_KEEP}}) {
            my $from = $f->[0];
            my $to = scalar(@{$f}) > 1 ? $f->[1] : $f->[0];
            `cp '$tmp_dir/supersize/$from' '$cdc_jauto_dir/$to'`;
            p4action ("$cdc_jauto_dir/$to");
        }
    }
    close SSI;
    close SSO;
    close SS_OUT;

    foreach my $cdc_type (@_) {
        print "$cdc_type:\n";
        my $cdc_fqcn = "lib.synchronous.conversion.v3.$cdc_type.500";

        # Generate cdl
        print " Generating CDL netlist\n";
        my $netlist_dir = "$config_r->{OUT_DIR}/netlist";
        `mkdir $netlist_dir` if (!-e $netlist_dir);
        $cmd = "cast2cdl --fulcrum-pdk-root='$config_r->{PDK_ROOT}' ";
        $cmd .= "'--cast-path=$config_r->{CAST_DIR}:$tmp_dir/supersize/cast:".
                              $config_r->{SPEC_DIR} . "' ";
        $cmd .= "--routed ";
        $cmd .= "'--cell=$cdc_fqcn' ";
        $cmd .= "'--output=$netlist_dir/$cdc_fqcn.cdl' ";
        `$cmd`;
        p4action ("$netlist_dir/$cdc_fqcn.cdl");

        # Generate verilog netlist
        print " Generating verilog netlist\n";
        `mkdir $tmp_dir/verilog` if (!-e "$tmp_dir/verilog");
        # annoying cross package reference
        $cmd = "prs2verilog ";
        $cmd .= "'--cast-path=$config_r->{CAST_DIR}:$tmp_dir/supersize/cast:".
                              $config_r->{SPEC_DIR} . "' ";
        $cmd .= "--routed ";
        $cmd .= "--converter=netlist ";
        $cmd .= "--translate=cadence ";
        $cmd .= "--by-name ";
        $cmd .= "--skip-power-rail ";
        $cmd .= "--power-grid-template='ZZZ' ";
        $cmd .= "'--cell=$cdc_fqcn' ";
        $cmd .= "--outdir=$tmp_dir/verilog";
        `$cmd`;
        munge_verilog_netlist($config_r, "$tmp_dir/verilog/$cdc_fqcn.v",
                              "$netlist_dir/$cdc_fqcn.v");

        # Generate non-default rules (.def)
        print " Generating non-default rules def\n";
        my $def_dir = "$config_r->{OUT_DIR}/def";
        `mkdir $def_dir` if (!-e $def_dir);
        # another annoying cross package reference
        $cmd = "cast2def ";
        $cmd .= "'--cast-path=$config_r->{CAST_DIR}:$tmp_dir/supersize/cast:".
                              $config_r->{SPEC_DIR} . "' ";
        $cmd .= "--format=def ";
        $cmd .= "--default-rule=\"\" ";
        $cmd .= "'--cell=$cdc_fqcn' ";
        $cmd .= "'--outfile=$def_dir/$cdc_fqcn.def'";
        `$cmd`;
        p4action ("$def_dir/$cdc_fqcn.def");

        # Generate behavioral model
        print " Generating RTL behavioral model\n";
        my $rtl_dir = "$config_r->{OUT_DIR}/rtl";
        `mkdir $rtl_dir` if (!-e $rtl_dir);
        `mkdir $tmp_dir/rtl` if (!-e "$tmp_dir/rtl");
        # still another annoying cross package reference
        $cmd = "prs2verilog ";
        $cmd .= "'--cast-path=$config_r->{CAST_DIR}:$tmp_dir/supersize/cast:".
                              $config_r->{SPEC_DIR} . "' ";
        $cmd .= "--converter=netlist ";
        $cmd .= "--translate=cadence ";
        $cmd .= "--by-name ";
        $cmd .= "--uninline-verilog ";
        $cmd .= "--skip-power-rail ";
        $cmd .= "--power-grid-template='ZZZ' ";
        $cmd .= "'--cell=$cdc_fqcn\{verilog.rtl\}' ";
        $cmd .= "--outdir=$tmp_dir/rtl";
        `$cmd`;
        munge_verilog_netlist($config_r, "$tmp_dir/rtl/$cdc_fqcn.v",
                              "$rtl_dir/$cdc_fqcn.v");
        my $atpg_dir = "$config_r->{OUT_DIR}/atpg";
        `mkdir $atpg_dir` if (!-e $atpg_dir);
        `mkdir $tmp_dir/atpg` if (!-e "$tmp_dir/atpg");
        print " Generating ATPG Model\n";
        atpg_verilog_netlist($config_r, "$netlist_dir/$cdc_fqcn.v",
                             "$atpg_dir/$cdc_fqcn.v");
        my $specfile = "$config_r->{TMP_DIR}/supersize/cast/lib/synchronous/conversion/v3/$cdc_type/500.cast";
        my $b=500;
        my $e=509;
        if ($config_r->{SUBTYPE_MAX} =~ /^\d+$/) {
            $e = $config_r->{SUBTYPE_MAX};
            $e = $e > 500 ? $e : 500;
        }
        for (my $i=$b;$i<=$e;$i++){
            my $existfile = $specfile;
            $existfile =~ s:$config_r->{TMP_DIR}/supersize/cast:$config_r->{SPEC_DIR}:;
            $existfile =~ s/500.cast/$i.cast/;
            if ( -f $existfile ) {
                if ( ! -w $existfile and $config_r->{CLIENT} ne "" and $config_r->{FORCE}) {
                    print STDERR "p4 -c '$config_r->{CLIENT}' edit '$existfile'\n" if $verbose;
                    `p4 -c '$config_r->{CLIENT}' edit '$existfile'`;
                }
            }
            my $existdir=$existfile;
            $existdir =~ s:/[^/]+$::;
            `mkdir -p "$existdir" 2>/dev/null` if ! -d $existdir;
            if ( -w $existfile or ! -e $existfile ) {
                `sed -e 's/"500"/"$i"/' "$specfile" > "$existfile"`;
                if ( $config_r->{CLIENT} ne "") {
                    print STDERR "p4 -c '$config_r->{CLIENT}' add '$existfile'\n" if $verbose;
                    `p4 -c '$config_r->{CLIENT}' add -f "$existfile"`;
                }
                else {
                    print STDERR "Please remember to check in '$existfile'\n";
                }
            }
            else {
                print STDERR "Warning: cannot write to $existfile\n" if $config_r->{FORCE};
            }
        }
        #here we add new files not already in the spec tree
        my @specfiles = `find '$config_r->{TMP_DIR}/supersize/cast' -name '*.cast'`;
        chomp @specfiles;
        foreach my $newspecfile (@specfiles) {
            my $existfile = $newspecfile;
            $existfile =~ s:$config_r->{TMP_DIR}/supersize/cast:$config_r->{SPEC_DIR}:;
            if ( ! -f $existfile ) {
                my $existdir=$existfile;
                $existdir =~ s:/[^/]+$::;
                `mkdir -p "$existdir" 2>/dev/null`;
                `cp "$newspecfile" "$existfile"`;
                if ( $config_r->{CLIENT} ne "") {
                    print STDERR "p4 -c $config_r->{CLIENT} add '$existfile'\n";
                    `p4 -c $config_r->{CLIENT} add -f "$existfile"`;
                }
                else {
                    print STDERR "Please remember to check in '$existfile'\n";
                }
            }
        }
    }
}

#
# Eliminate explicit GND/Vdd connections.  Use 1'b0, 1'b1 when necessary.
#
sub munge_verilog_netlist {
    my $config_r = shift;
    my $src = shift;
    my $dst = shift;

    open (SRC, "<$src") || die "Can't read verilog src file $src.\n";
    open (DST, ">$dst") || die "Can't write verilog netlist $dst.\n";
    while (<SRC>) {
        next if (/^(wire|input) (GND|Vdd);/);
        next if (/^ZZZ tiehilo/);
        s/\(\s*GND\s*\)/(1'b0)/g;
        s/\(\s*Vdd\s*\)/(1'b1)/g;
        print DST;
    }
    close SRC;
    close DST;
    my $b=501;
    my $e=509;
    if ($config_r->{SUBTYPE_MAX} =~ /^\d+$/) {
        $e = $config_r->{SUBTYPE_MAX};
        $e = $e > 500 ? $e : 500;
    }
    p4action ($dst);
    for (my $i=$b;$i<=$e;$i++){
        open (DST, "<$dst") || die "Can't read verilog netlist $dst.\n";
        my $dup = $dst;
        $dup =~ s/500/$i/;
        open (DUP, ">$dup") || die "Can't write verilog netlist $dup.\n";;
        while (<DST>) {
            s/(module\s.+\.)(500)/$1$i/;
            print DUP;
        }
        close DUP;
        close DST;
        p4action ($dup);
    }
}

#
# Returns the parent directory of compile_cdc
#
sub path_to_script_dir {
    if ($0 =~ /(.*)\/[^\/]+$/) {
        return "$1";
    }
    else {
        die "Couldn't determine script parent directory path.\n";
    }
}

sub srt {
    my $ad = $a;
    my $bd = $b;
    $ad =~ s/[^\d]//g;
    $bd =~ s/[^\d]//g;
    $ad - $bd;
}

sub rsrt {
    my $ad = $a;
    my $bd = $b;
    $ad =~ s/[^\d]//g;
    $bd =~ s/[^\d]//g;
    $bd - $ad;
}


sub atpg_verilog_netlist {
    my $config_r = shift;
    my $src = shift;
    my $dst = shift;

    open (P, "<$src") or die "Cannot open $src";
    my $subcell=$src;
    $subcell =~ s/\.v$//;
    $subcell =~ s/.*\///;
    $subcell =~ s/(.*)\((\d+)\).(\d+)$/$1_N${2}_$3/;
    my @header=();
    my @io=();
    my @body=();
    my @L=();
    my @Ld=();
    my @Rd=();
    my @Re=();
    my @LS=();
    my @Le=();
    my @RS=();
    while (<P>) {
        chomp;
        if (/^\s*module/) {
            push @header, $_;
            last;
        }
    }
    while (<P>) {
        chomp;
        push @header, $_;
        last if /;/;
    }
    while (<P>) {
        chomp;
        last if (/^wire/);
        if (! /;/) {
            push @body, $_;
            last;
        }
        if (/^input \\(L\.d\[\d+\])\s*;/) {
            push @Ld, $1;
        }
        if (/^input \\(L\[\d+\]\.\d+)\s*;/) {
            push @L, $1;
        }
        if (/^input \\(LS\.\d+)\s*;/) {
            push @LS, $1;
        }
        if (/^output \\(L\[\d+\]\.e)\s*;/) {
            push @Le, $1;
        }
        if (/^output \\(RS\.\d+)\s*;/) {
            push @RS, $1;
        }
        if (/^output \\(R\.d\[\d+\])\s*;/) { # A2S only
            push @Rd, $1;
        }
        if (/^input \\(R\[\d+\]\.e)\s*;/) {
            push @Re, $1;
        }
        if (/^output \\(R\[\d+\]\.\d+)\s*;/) { # S2A only
            push @Rd, $1;
        }
        push @io, $_;
    }
    if (/^wire/) {
        while (<P>) {
            chomp;
            if (! /^\s*wire/) {
                push @body, $_;
                last;
            }
        }
    }
    while (<P>) {
        chomp;
        last if /^\s*endmodule/;
        push @body, $_;
    }
    close P;
    @L = (sort srt @L);
    @Ld = (sort srt @Ld);
    @Rd = (sort srt @Rd);
    @Re = (sort srt @Re);
    @LS = (sort srt @LS);
    @Le = (sort srt @Le);
    @RS = (sort srt @RS);
    open (P, ">$dst") or warn "Cannot open $dst for writing";
    select P;
    if ($src =~ /A2S/) {
        print "module \\$subcell  ( CLK, L_N_d_4, L_N_e, R__e, R__v, R__d, SCAN__MODE, SCAN__EN, SCAN__IN, SCAN__OUT, LS__d, LS__e, RS__d, RS__e, _RESET, Vdd, GND );\n";
        print "  input \[$#L:0\] L_N_d_4;\n";
        print "  output \[$#Le:0\] L_N_e;\n";
        print "  output \[$#Rd:0\] R__d;\n";
        print "  input \[$#LS:0\] LS__d;\n";
        print "  output \[$#RS:0\] RS__d;\n";
        print "  input CLK, R__e, SCAN__MODE, SCAN__EN, SCAN__IN, RS__e, _RESET, Vdd, GND;\n";
        print "  output R__v, SCAN__OUT, LS__e;\n";
        print "  wire sR__e;\n";
        for (my $n = 0; $n <= $#RS; $n++) {
            print "  assign RS__d\[$n\] = 1'b0;\n";
        }
        print "  assign LS__e = 1'b0;\n";
        for (my $n = 0; $n <= $#Le; $n++) {
            print "  assign L_N_e\[$n\] = 1'b0;\n";
        }
        print "\n";
        print "  $cells{BUF}[0] buf_scan__out ( .$cells{BUF}[1](R__d[$#Rd]), .$cells{BUF}[2](SCAN__OUT) );\n";
        my $x = $#Rd-1;
        if ($#Rd == 0) {
            print "  $cells{FF}[0] scan_R__e ( .$cells{FF}[1](R__e), .$cells{FF}[2](SCAN__IN), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](sR__e) );\n";
            print "  $cells{FF}[0] scan_R__v ( .$cells{FF}[2](sR__e), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](R__v), .$cells{FF}[1](1'bx));\n";
            print "  $cells{FF}[0] scan_R__d0 ( .$cells{FF}[2](R__v), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](R__d[0]), .$cells{FF}[1](R__d[0]) );\n";

        }
        else {
            print "  $cells{FF}[0] scan_R__e ( .$cells{FF}[1](R__e), .$cells{FF}[2](SCAN__IN), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](sR__e) );\n";
            print "  $cells{FF}[0] scan_R__v ( .$cells{FF}[2](sR__e), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](R__v), .$cells{FF}[1](1'bx));\n";
            print "  $cells{FF}[0] scan_R__d0 ( .$cells{FF}[2](R__v), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](R__d[0]), .$cells{FF}[1](R__d[0]) );\n";
            print "  $cells{FF}[0] scan_R__dtop ( .$cells{FF}[2](R__d[$x]), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](R__d[$#Rd]), .$cells{FF}[1](R__d[$#Rd]) );\n";
            for (my $n = 0; $n < $#Rd-1; $n++) {
                my $x=$n+1;
                my $r=$Rd[$n];
                $r =~ s/\./__/;
                my $r1=$Rd[$n+1];
                $r1 =~ s/\./__/;
                print "  $cells{FF}[0] \\DatapathFlops[$x].scan_R__d  ( .$cells{FF}[2]($r), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5]($r1), .$cells{FF}[1]($r1) );\n";
            }
        }
        print "endmodule\n\n";
        print join("\n", @header)."\n";
        print join("\n", @io)."\n";
        print "  assign \\LS.e = 1'b0;\n";
        print "  assign \\".join(" = 1'b0;\n  assign \\", (@Le, @RS))." = 1'b0;\n";
        print "  \\$subcell  inst0 ( .CLK(CLK), .L_N_d_4({\n";
        my $n=0;
        for ($n = $#L; $n > 3; $n -= 4) {
            print "      \\$L[$n] , \\$L[$n-1] , \\$L[$n-2] , \\$L[$n-3] ,\n";
        }
        print "      \\$L[$n] , \\$L[$n-1] , \\$L[$n-2] , \\$L[$n-3] }),\n";
        print "      .R__e(\\R.e ), .R__v(\\R.v ), .R__d({\n";
        for ($n = $#Rd; $n > 4; $n -= 5) {
            print "      \\$Rd[$n] , \\$Rd[$n-1] , \\$Rd[$n-2] , \\$Rd[$n-3] , \\$Rd[$n-4] ,\n";
        }
        printf "      ";
        for (; $n > 0; $n-- ) {
            printf "\\$Rd[$n] ,";
        }
        print "\\$Rd[$n] }),\n";
        print "      .SCAN__MODE(\\SCAN.MODE ), .SCAN__EN(\\SCAN.EN ), .SCAN__IN(\\SCAN.IN ),\n";
        print "      .SCAN__OUT(\\SCAN.OUT ),\n";
        print "      .LS__d({ \\".join(" , \\", (sort rsrt @LS))." }),\n";
        print "      .RS__e(\\RS.e ), ._RESET(_RESET), .Vdd(1'b1), .GND(1'b0) );\n";
        print "endmodule\n";
    }
    elsif ($src =~ /S2A/) {
        if ($#Ld > 0) {
            print "module \\$subcell  ( CLK, L__v, L__d, L__e, R_N_e, R_N_d_4, SCAN__MODE, SCAN__EN, SCAN__IN, SCAN__OUT, LS__d, LS__e, RS__d, RS__e, _RESET, Vdd, GND );\n";
            my $Ldm1 = $#Ld-1;
            print "  input \[$#Ld:0\] L__d ;\n";
            print "  input \[$#Re:0\] R_N_e ;\n";
            print "  output \[$#Rd:0\] R_N_d_4 ;\n";
            print "  input \[$#LS:0\] LS__d ;\n";
            print "  output \[$#RS:0\] RS__d ;\n";
            print "  input CLK, L__v, SCAN__MODE, SCAN__EN, SCAN__IN, RS__e, _RESET, Vdd, GND;\n";
            print "  output L__e, SCAN__OUT, LS__e ;\n";
            print "  wire sL__v;\n";
            print "  wire \[$Ldm1:0\] sL__d;\n";
            for (my $n = 0; $n <= $#RS; $n++) {
                print "  assign RS__d\[$n\] = 1'b0;\n";
            }
            print "  assign LS__e = 1'b0;\n";
            for (my $n = 0; $n <= $#Rd; $n++) {
                print "  assign R_N_d_4\[$n\] = 1'b0;\n";
            }
            print "\n";
            print "  $cells{FF}[0] scan_L__dtop ( .$cells{FF}[1](L__d[$#Ld]), .$cells{FF}[2](sL__d[$Ldm1]), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](SCAN__OUT) );\n";
            print "  $cells{FF}[0] scan_L__v ( .$cells{FF}[1](L__v), .$cells{FF}[2](SCAN__IN), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5]( sL__v) );\n";
            print "  $cells{FF}[0] scan_L__e ( .$cells{FF}[1](1'bx), .$cells{FF}[2](sL__v), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](L__e));\n";
            print "  $cells{FF}[0] scan_L__d0 ( .$cells{FF}[1](L__d[0]), .$cells{FF}[2](L__e), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5]( sL__d[0]) );\n";

            for (my $n = 0; $n < $#Ld-1; $n++) {
                my $x=$n+1;
                my $r=$Rd[$n];
                $r =~ s/\./__/;
                my $r1=$Rd[$n+1];
                $r1 =~ s/\./__/;
                print "  $cells{FF}[0] \\DatapathFlops[$x].scan_L__d  ( .$cells{FF}[1](L__d\[$x\]), .$cells{FF}[2](sL__d\[$n\]), .$cells{FF}[3](SCAN__EN), .$cells{FF}[4](CLK), .$cells{FF}[5](sL__d\[$x\]) );\n";
            }
            print "endmodule\n\n";
        }
        print join("\n", @header)."\n";
        print join("\n", @io)."\n";
        if ($#Ld > 0) {
            print "  assign \\LS.e = 1'b0;\n";
            print "  assign \\".join(" = 1'b0;\n  assign \\", @RS)." = 1'b0;\n";
            print "  assign \\".join(" = 1'b0;\n  assign \\", @Rd)." = 1'b0;\n";
            printf "  \\$subcell  inst0 ( .CLK(CLK), .L__v(\\L.v ), .L__d({\\";
            printf join(" , \\", (sort rsrt @Ld));
            printf " }), .L__e(\\L.e ), .R_N_e({\\";
            printf join(" , \\", (sort rsrt @Re));
            printf " }), .SCAN__MODE( \\SCAN.MODE ), .SCAN__EN(\\SCAN.EN ), .SCAN__IN(\\SCAN.IN ), .SCAN__OUT( \\SCAN.OUT ), .LS__d({\\";
            printf join (" , \\", (sort rsrt @LS));
            print " }), .RS__e(\\RS.e ), ._RESET(_RESET), .Vdd(1'b1), .GND(1'b0) );\n";
        }
        else {
            print "  wire   sL__v;\n";
            print "  assign \\LS.e = 1'b0;\n";
            print "  assign \\".join(" = 1'b0;\n  assign \\", @RS)." = 1'b0;\n";
            print "  assign \\".join(" = 1'b0;\n  assign \\", @Rd)." = 1'b0;\n";
            print "\n";
            print "  $cells{FF}[0] scan_L__d0 ( .$cells{FF}[1](\\L.d\[0\] ), .$cells{FF}[2](\\L.e ), .$cells{FF}[3](\\SCAN.EN ), .$cells{FF}[4](CLK), .$cells{FF}[5](\\SCAN.OUT ) );\n";
            print "  $cells{FF}[0] scan_L__v ( .$cells{FF}[1](\\L.v ), .$cells{FF}[2](\\SCAN.IN ), .$cells{FF}[3](\\SCAN.EN ), .$cells{FF}[4](CLK), .$cells{FF}[5](sL__v) );\n";
            print "  $cells{FF}[0] scan_L__e ( .$cells{FF}[2](sL__v), .$cells{FF}[3](\\SCAN.EN ), .$cells{FF}[4](CLK), .$cells{FF}[5](\\L.e ), .$cells{FF}[1](1'bx) );\n";
        }
        print "endmodule\n";
    }
    else {
        print STDERR "Unknown cdc type\n";
    }
    select STDOUT;
    close P;
    my $b=501;
    my $e=509;
    if ($config_r->{SUBTYPE_MAX} =~ /^\d+$/) {
        $e = $config_r->{SUBTYPE_MAX};
        $e = $e > 500 ? $e : 500;
    }
    p4action($dst);
    for(my $i=$b;$i<=$e;$i++){
        open (DST, "<$dst") || die "Can't read atpg netlist $dst.\n";
        my $dup = $dst;
        $dup =~ s/500/$i/;
        open (DUP, ">$dup") || die "Can't write atpg netlist $dup.\n";;
        while (<DST>) {
            s/(\\lib\.[^\s]+[\._])(500)/$1$i/;
            print DUP;
        }
        close DUP;
        close DST;
        p4action($dup);
    }
}

