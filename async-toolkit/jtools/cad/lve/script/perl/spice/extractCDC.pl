#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;
use IPC::Open2;
use File::stat;

my $base;
my $cast_path;
my $cast_dir;
my $spec_dir;
my $cdccell;
my $cell_lef;
my $client;
my $debug=0;
my $def;
my $cdcdef;
my $design;
my $subtype;
my $designlibrary;
my $max_heap_size="1800M";
my $just_cast=0;
my $dry_run=0;
my $fd;
my $force=0;
my $instance;
my $pdkroot;
my $rc_license="RTL_Compiler_Verification";
my $spar_dir;
my $srcdir;
my $verbose=0;
my $verilog;
my %components=();
my $limit=60;
my $cdc_module;
my $warnings=0;
my $errors=0;
my $prefix="";
my $lveroot="/mnt/fulcrum/alta/lveB/lve";
# create dot files from calling checkDependency
my $dot=0;
# make-like check for existance and time stamps

my %dependcheck=();
my %extracells=();

sub createDirectivesFile {
    my ($verilog,$outfile)=@_;
    local ($/,$_);
    my $fh;
    my $fo;
    $/=";";
    open ($fh, "<$verilog");
    open ($fo, ">$outfile");
    while (<$fh>) {
        chomp;
        s/\s+/ /g;
        my @f=split;
        if (($f[0] =~ /av_/) and ($f[1] =~ /^FE/)) {
            my $inst=$f[1];
            $inst =~ s/\(.*//;
            print $fo "delaybias($inst)=0;";
        }
    }
    close $fh;
    close $fo;
}

sub checkDependency {
    my ($output,$input,$from) = @_;
    my @output_files=split(/ /, $output);
    my @input_files=split(/ /, $input);
    foreach my $in (@input_files) {
        if ($in =~ /\//) { # external file
            $dependcheck{$in}=1;
        }
        print STDERR "$in used before generated at $from" if (! $dependcheck{$in});
    }
    foreach my $out (@output_files) {
        $dependcheck{$out}=1;
    }
    if ($dot) {
        my $n=0;
        foreach my $i (@input_files) {
            $n++;
            last if $n > 5;
            my $in=$i;
            $in =~ s/.*\//...\// if $in =~ /\//;
            foreach my $o (@output_files) {
                my $ou=$o;
                $ou =~ s/.*\//...\// if $ou =~ /\//;
                print DOT "\"$in\"->\"$ou\"";
            }
        }
    }
    my $input_timestamp = 0;
    my $write_secs;
    $from = "Unknown" if ! defined $from;
    my $pout=$output;
    my $pin=$input;
    printf STDERR "Info: checkDependency(Out=$pout, In=$pin, From=$from)=" if $verbose;
    foreach my $file (@input_files) {
        chomp $file;
        if(! -e $file){
            print STDERR "Not OK" if $verbose;
            print STDERR "ERROR: missing dependency $file";
            return 0 if $dry_run;
            exit 1;
        }
        $write_secs = stat($file)->mtime;
        if($write_secs > $input_timestamp){$input_timestamp=$write_secs;}
    }
    if($force){ print STDERR "Force" if $verbose; return 1;}
    foreach my $file (@output_files) {
        chomp $file;
        if(! -e $file){ print STDERR "Need to Run" if $verbose; return $dry_run ? 0 : 1;}
        $write_secs = stat($file)->mtime;
        if($write_secs < $input_timestamp){print STDERR "Need to Run" if $verbose; return 1;}
    }
    print STDERR "Up To Date" if $verbose;
    return 0;
}

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    print STDERR "Usage: extractCDC [options] <encounter def> <instance>";
    print STDERR "   options:";
    my $us=<<US;
    --cast-path=[$cast_path] : normal cast path
    --cast-dir=[$cast_dir] : for output
    --spec-dir=[$spec_dir] : for output
    --cdc-cell=[$cdccell] : name of cdc top cell
    --cell-lef=[$cell_lef] : cell lef if not cellname.lef
    --def=[$def] : name of encounter def, modified
    --dry-run : do not do anything
    --force  : force all dependencies
    --fulcrum-pdk-root=[$pdkroot] : the pdk
    --instance=[$instance] : the verilog hierarchical instance name of the cdc
    --lve-root=[$lveroot] : the root of the lve where to find cell.spice files
    --spar-dir=[$spar_dir] : normal spar-dir
    --src-dir=[$srcdir]    : source dir for the encounter def
    --subtype=[$subtype] : subtype of NEW extracted cdc cell
    --verbose              : verbose
    --verilog=[$verilog] : top design verilog from encounter
    --prefix=[$prefix]   : prefix used in verilog/def for subcells
    --rc-license=[$rc_license] : rc type
    --max-heap-size=[$max_heap_size] : max heap for java
    --just-cast    [ only generate the cast]
US
    my @us=split(/\n/, $us);
    my $l=0;
    foreach my $u (@us) {
        my $l1=index($u, ' : ');
        $l = $l1 if $l1 > $l;
    }
    foreach my $u (@us) {
        my $l1=index($u, ' : ');
        printf "%-*.*s : %s\n", $l,$l,substr($u,0,$l1),substr($u,$l1+3);
    }
    exit 1;
}

sub my_system {
    my @args=@_;
    if ($#args == 0) {
        print STDERR $args[0] if $verbose;
    }
    else {
        print STDERR "'".join("' '", @args)."'" if $verbose;
    }
    system @args;
}

sub get_design_from_def {
    my ($def)=@_;
    my $fh;
    my $dsgn;
    if ($def =~ /\.gz/) {
        open ($fh, "gunzip -c '$def' |");
    }
    else {
        open ($fh, "<$def") or usage "Cannot open \$def $def";
    }
    while (<$fh>) {
        chomp;
        s/\\//g;
        if (/^DESIGN /) {
            s/^DESIGN\s+//;
            s/[\s;]+$//;
            close $fh;
            return $_;
            last;
        }
    }
    close $fh;
    $dsgn;
}

sub cdcnets {
    my ($def,$inst)=@_;
    my $fh;
    my %nets=();
    # TEST
    print STDERR "Reading $def for instance $inst ..." if $verbose;
    if ($def =~ /\.gz/) {
        open ($fh, "gunzip -c '$def' |");
    }
    else {
        open ($fh, "<$def") or usage "Cannot open \$def $def";
    }
    my %print=();
    while (<$fh>) {
        chomp;
        s/\\//g;
        if (/^DESIGN /) {
            s/^DESIGN\s+//;
            s/[\s;]+$//;
            $design=$_;
            next;
        }
        if (/^COMPONENTS/) {
            my $ln="";
            while (<$fh>) {
                chomp;
                last if /^END COMP/;
                $ln .= " $_";
                if (/;\s*$/) {
                    $ln =~ s/\s+/ /g;
                    $ln =~ s/^\s//;
                    my ($x,$comp,$type)=split(/ /,$ln);
                    $components{$comp}=$type;
                    if ( $type !~ /(av_|lib.synchronous.conversion.v3|lib.dft.converter.SYNC_CONVERTER_ARRAY_8)/) {
                        $extracells{$type}=1;
                    }
                    $ln="";
                }
            }
        }
        if (/^NETS/) {
            my $ln="";
            while (<$fh>) {
                chomp;
                last if /^END NETS/;
                $ln .= " $_";
                if (/;\s*$/) {
                    $ln =~ s/\s+/ /g;
                    $ln =~ s/^\s//;
                    $ln =~ s/\)[^)]*$//;
                    my $print=0;
                    print $ln if $print;
                    my @s = split(/[()]/, $ln);
                    my $type="";
                    shift @s;
                    foreach my $s (@s) {
                        last if $s =~ /\+/;
                        $s =~ s/\s+/ /g;
                        $s =~ s/^\s//;
                        $s =~ s/\s$//;
                        next if $s eq "";
                        my ($t,$p)=split(/ /,$s);
                        print "T $t $p" if $print;
                        $type .= " $t:$p";
                    }
                    $type =~ s/^\s*//;
                    my ($x,$comp)=split(/ /,$ln);
                    print "$comp<>$type" if $print;
                    $nets{$comp}=$type;
                    $print{$comp}=$print;
                    $ln="";
                }
            }
        }
    }
    close $fh;
    my $list="";
    my $netcount=0;
    foreach my $comp (sort keys %nets) {
        my $net=$comp;
        my $print=$print{$comp};
        $net =~ s/\\//g;
    #    print "$net $nets{$comp}" if $nets{$comp} =~ m:^$inst/:;
        foreach my $pair (split(/ /, $nets{$comp})) {
            if ($pair =~ m:^$inst/:) {
                $list .= "$net ";
                $netcount++;
                print "N $net" if $print;
                last;
            }
        }
    }
    $list =~ s/\s+$//;
    print STDERR "$netcount nets found" if $verbose;
    $list;
}

# renamer
my %cad2cast=();
my %cast2cad=();

my $apid=0;

sub startrename {
    if ($apid) {
        close ARD;
        close AWR;
        kill $apid;
    }
    else {
        $apid = open2(\*ARD, \*AWR, "rename --type=all");
    }
    die "Cannot start renamer" if ! $apid;
}

sub instcast2cad {
    my ($in)=@_;
    my $out=$in;
    if (defined ($cad2cast{$in})) {
        $out = $cad2cast{$in};
    }
    else {
        startrename if ! $apid;
        print AWR "instance cast cadence $in";
        $out=<ARD>;
        chomp $out;
        if ($out =~ /\s/ or $out eq "") {
            $out = $in;
            print STDERR "Warning: Rename of $in failed";
            startrename();
        }
    }
    print STDERR "Info: instance cast->cadence rename $in => $out" if $verbose and $in ne $out;
    $cad2cast{$in}=$out;
    $cast2cad{$out}=$in;
    $out;
}

sub instcad2cast {
    my ($in)=@_;
    my $out=$in;
    if (defined ($cad2cast{$in})) {
        $out = $cad2cast{$in};
    }
    else {
        startrename if ! $apid;
        print AWR "instance cadence cast $in";
        $out=<ARD>;
        chomp $out;
        if ($out =~ /\s/ or $out eq "") {
            $out = $in;
            print STDERR "Warning: Rename of $in failed";
            startrename();
        }
    }
    print STDERR "Info: instance cadence->cast rename $in => $out" if $verbose and $in ne $out;
    $cad2cast{$in}=$out;
    $cast2cad{$out}=$in;
    $out;
}

sub castrename {
    my ($in)=@_;
    my $out=$in;
    if (defined ($cad2cast{$in})) {
        $out = $cad2cast{$in};
    }
    else {
        startrename if ! $apid;
        print AWR "cell cadence cast $in";
        $out=<ARD>;
        chomp $out;
        if ($out =~ /\s/ or $out eq "") {
            $out = $in;
            print STDERR "Warning: Rename of $in failed";
            startrename();
        }
    }
    print STDERR "Info: cast rename $in => $out" if $verbose and $in ne $out;
    $cad2cast{$in}=$out;
    $cast2cad{$out}=$in;
    $out;
}

sub cadrename {
    my ($in)=@_;
    my $out=$in;
    if (defined ($cast2cad{$in})) {
        $out = $cast2cad{$in};
    }
    else {
        startrename if ! $apid;
        print AWR "cell cast cadence $in";
        $out=<ARD>;
        chomp $out;
        if ($out =~ /\s/ or $out eq "") {
            $out = $in;
            startrename();
        }
    }
    print STDERR "Info: cad rename $in => $out" if $verbose and $in ne $out;
    $cast2cad{$in}=$out;
    $cad2cast{$out}=$in;
    $out;
}

GetOptions (
    "cast-path=s" => \$cast_path,
    "cast-dir=s" => \$cast_dir,
    "spec-dir=s" => \$spec_dir,
    "cdc-cell=s" => \$cdccell,
    "cell-lef=s" => \$cell_lef,
    "def=s" => \$def,
    "force" => \$force,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "instance=s" => \$instance,
    "spar-dir=s" => \$spar_dir,
    "src-dir=s" => \$srcdir,
    "subtype=s" => \$subtype,
    "verbose" => \$verbose,
    "verilog=s" => \$verilog,
    "dot" => \$dot,
    "lve-root=s" => \$lveroot,
    "dry-run" => \$dry_run,
    "prefix=s" => \$prefix,
    "rc-license=s" => \$rc_license,
    "max-heap-size=s" => \$max_heap_size,
    "just-cast" => \$just_cast,
) or usage();

if ($dot) {
    open (DOT, ">extractCDC.dot");
    print DOT "digraph \x7b";
}

$def = shift if ! defined $def;
$instance = shift if ! defined $instance;
$dependcheck{$def}=1 if defined $def; # eliminate a warning

usage("def not defined") if ! defined $def;
$cdcdef=$def;
# create def from src def, it must be modified for starRC by renaming avago cells and removing escapes
if ( defined ($srcdir) and -d $srcdir and $srcdir ne "." and -s "$srcdir/$def") {
    $cdcdef = $def;
    $cdcdef =~ s/\.def/.cdc.def/;
    $cdcdef =~ s/\.gz//;
    if (checkDependency ($cdcdef, "$srcdir/$def", __LINE__)) {
        my $idef;
        my $odef;
        if ($def =~ /\.gz$/) {
            open ($idef, "gunzip -c '$srcdir/$def' |");
#            my_system("gunzip -c '$srcdir/$def' | sed -e 's/ \\(av_[a-z0-9_]*\\) / vendor.avago.svt.gates.\\1.0 /' -e 's/\\\\//g' > $cdcdef");
        }
        else {
            open ($idef, "<$srcdir/$def");
#            my_system("sed -e 's/ \\(av_[a-z0-9_]*\\) / vendor.avago.svt.gates.\\1.0 /' -e 's/\\\\//g' '$srcdir/$def' > $cdcdef");
        }
        open ($odef, ">$cdcdef");
        while (<$idef>) {
            chomp;
            if (/^VIAS/) {
                while ( ! /END VIAS/) { $_=<$idef> }
                $_=<$idef>;
                chomp;
            }
            s/\\//g;
            s/ (av_[a-z0-9_]+) / vendor.avago.svt.gates.$1.0 /;
            print $odef $_;
        }
        close $idef;
        close $odef;
    }
}
$def=$cdcdef;
usage("$def not found") if ! -e "$def";
$design = get_design_from_def($def) if ! defined $design;
my $designbase = $design;
$designbase =~ s/\.[^\.]+$//;
$designlibrary = $designbase;
$designlibrary =~ s/\.[^\.]+$//;
usage("no instance defined") if ! defined $instance;
usage("Undefined or improper spar-dir") if ! -d $spar_dir;
usage("Cast-path undefined") if ! defined $cast_path;
usage("need fulcrum-pdk-root") if ! -d "$pdkroot";
$verilog="$design.v" if ! defined $verilog;
if ( defined ($srcdir) and -d $srcdir and $srcdir ne "." and -s "$srcdir/$verilog") {
    if (checkDependency ($verilog, "$srcdir/$verilog", __LINE__)) {
#        my_system("sed -e 's/ \\(av_[a-z0-9_]*\\) / \\\\vendor.avago.svt.gates.\\1.0  /' '$srcdir/$verilog' > $verilog");
        my_system("/bin/cp '$srcdir/$verilog' $verilog");
    }
}
usage("$verilog does not exist") if ! -s $verilog;

foreach my $path (split(/:/, $cast_path)) {
    usage("Cast-path component $path is not a directory") if ! -d $path;
}

# check instance is legitimate
{
    local($/)=";";
    my %inst=();
    my %type=();
    my $module="";
    my $top="";
    my @inst=split("/", $instance);
    my $fv;
    print STDERR "Reading $verilog, looking for instances...";
    open ($fv, "<$verilog");
    while (<$fv>) {
        chomp;
        if (/^module/ or /\nmodule/) {
            s/\s+/ /g;
            s/\(/ ( /;
            s/\\//g;
            s/.*module\s+//;
            s/\s.*//;
            $module=$top=$_;
        }
        s/\(/ ( /;
        s/\s+/ /g;
        s/^\s+//;
        s/\(/ ( /;
        s/\\//g;
        next if /^\/\// or /^(wire|input|output|reg|inout|endmodule)/;
        my @f=split;
        my $type=$f[0];
        my $inst=$f[1];
        # wrong!!
        $inst{"$module/$inst"}=$type;
        $type{$type}="$module/$inst";
    }
    close $fv;
    $/="\n";
    my %hierinst=();
    my $found=0;
    my $done=0;
    foreach my $key (keys %inst) {
        if ($key =~ /^$top\//) {
            my $hkey=$key;
            $hkey =~ s/$top\///;
            $hierinst{$hkey}=$inst{$key} if $hkey ne "";
        }
    }
    my $depth=0;
    my %touched=();
    my $module;
    while ( ! $done and ! $found and $depth < 2) {
        $depth++;
        $done=1;
        foreach my $key (keys %hierinst) {
            foreach my $ikey (keys %inst) {
                if (($ikey =~ /^$hierinst{$key}\//) and ! $touched{$ikey}) {
                    $touched{$ikey}=1;
                    my $hkey=$ikey;
                    $hkey =~ s/$hierinst{$key}/$key/;
                    $hierinst{$hkey}=$inst{$ikey};
                    $done=0;
                    if ($hkey eq "$instance") {
                        $module=$hierinst{$hkey};
                        $found=1;
                    }
                }
            }
        }
    }
    die "Invalid instance $instance, no module found in $verilog" if $module eq "";
    $cdc_module=castrename($module);
    $cdccell = $cdc_module if ! defined $cdccell;
    if (substr($cdc_module,0,length($cdccell)) ne $cdccell) {
        print STDERR "Warning: $cdccell and $cdc_module do not seem to be same type";
        $warnings++;
    }
    print STDERR "Info: MODULE $cdc_module" if $verbose;
}

$cdccell =~ /\.(\d[^\.]+)$/;
if (! defined $subtype) {
    $subtype=$instance;
    $subtype =~ s:.*/::;
}
$cdccell =~ m:.*\.([^\.]+)\.[^\.]+$:;
my $cdcbase=$1;
my $cdcextracted=$designlibrary.".".$cdcbase.".".$subtype;
my $base = $cdcextracted;
$base =~ s/\.[^\.]+$//;

#usage "subtype of extract cell cannot match $cdccell $cdcextracted" if $cdccell eq $cdcextracted;

my $cdc_cadence_cell=cadrename($cdccell);

# create libs using the fulcrum FQCN's
if (checkDependency("av65gp_svt_ss.lib", "$spar_dir/vendor/avago/svt/gates/synopsys/av65gp_svt_ss.lib", __LINE__)) {
    my_system("sed -e 's/(\\(av_[a-z0-9_]*\\))/(vendor.avago.svt.gates.\\1.0)/' $spar_dir/vendor/avago/svt/gates/synopsys/av65gp_svt_ss.lib > av65gp_svt_ss.lib");
}
usage("Cannot create av65gp_svt_ss.lib") if ! -s "av65gp_svt_ss.lib";
if (checkDependency("av65gp_ck_svt_ss.lib", "$spar_dir/vendor/avago/svt/gates/synopsys/av65gp_ck_svt_ss.lib", __LINE__)) {
    my_system("sed -e 's/(\\(av_[a-z0-9_]*\\))/(vendor.avago.svt.gates.\\1.0)/' $spar_dir/vendor/avago/svt/gates/synopsys/av65gp_ck_svt_ss.lib > av65gp_ck_svt_ss.lib");
}
usage("Cannot create av65gp_ck_svt_ss.lib") if ! -s "av65gp_ck_svt_ss.lib";

# need cdccell.v for generating cast
my $outcell="$cdccell";
$outcell =~ s/$prefix//;
if (checkDependency ("$cdccell.v", $verilog, __LINE__)) {
    my $flatcdctcl=<<EFT;
set verilog_in \$env(VERILOG_INPUT)
set verilog_out \$env(VERILOG_OUTPUT)
set top_cell \$env(TOP_CELL)
set spar_root \$env(SPARDIR)
set memory_root \$spar_root/vendor/artisan/memory
set fhandle [open "\$spar_root/chip/alta/FM/scripts/lib_file_max.list" r]
lappend lib_file_list av65gp_svt_ss.lib
lappend lib_file_list av65gp_ck_svt_ss.lib
while {[gets \$fhandle line] >= 0} {
    set line [string trim \$line]
    if {[string length \$line] > 0} {
        lappend lib_file_list \$spar_root/\$line
    }
}
close \$fhandle
set_attribute library \$lib_file_list 
read_hdl \$verilog_in
elaborate \$top_cell
set_attribute ungroup_separator /
ungroup -all -only_user_hierarchy -flatten
write_hdl > \$verilog_out
exit
EFT
    my $flatcdccmd=<<EFC;
#!/bin/bash
#checklic -w $rc_license
export VERILOG_INPUT='$verilog'
export VERILOG_OUTPUT='$cdccell.v'
export TOP_CELL='$cdc_cadence_cell'
export SPARDIR='$spar_dir'
echo 'exit' | /usr/local/cadence/bin/rc.71usr1 rc -queue -use_license $rc_license -file '$cdccell.flat.tcl'
EFC
    open P, ">$cdccell.flat.tcl";
    print P $flatcdctcl;
    close P;
    open P, ">$cdccell.flat.cmd";
    print P $flatcdccmd;
    close P;
    chmod 0755, "$cdccell.flat.cmd";
    print STDERR "Generating flat CDC netlist for $cdccell" if $verbose;
    my $rv=my_system("./'$cdccell.flat.cmd' 1>'$cdccell'.flat.out 2>'$cdccell'.flat.err");
    print STDERR "Error generating $cdccell.v" if $rv;
    unlink "rc.cmd";
    exit $rv if $rv;
    unlink "rc.log";
}
if (checkDependency ("$outcell.v", "$cdccell.v", __LINE__)) {
    if ($prefix ne "") {
        my_system ( "sed -e 's/$prefix//g' -e 's/ \\(av_[a-z0-9_]*\\) / \\\\vendor.avago.svt.gates.\\1.0  /' '$cdccell.v' > '$outcell.v'");
    }
}
# generate cdc cast
if ( ! defined ($cast_dir) or ! defined ($spec_dir)) {
    ($cast_dir,$spec_dir)=split(/:/, $cast_path, 2);
}
my $targetcast=$cdcextracted;
$targetcast =~ s/\./\//g;
$targetcast =~ s/\/[^\/]+$/\/$subtype.cast/;
$targetcast = "$spec_dir/$targetcast";
if (checkDependency ("$outcell.directives", "$outcell.v", __LINE__)) {
    createDirectivesFile("$outcell.v","$outcell.directives");
}
if (checkDependency ("$targetcast", "$outcell.v $outcell.directives", __LINE__)) {
    # to set delaybias
    my $refineparent=$outcell;
    $refineparent =~ s/\.[^\.]+$//;
    $refineparent =~ s/SCAN_/ABSTRACT_/;
    my @cmd=("vs2cast","--cast-dir",$cast_dir,"--spec-dir", $spec_dir, "--cast-path","$cast_path", "--library", "$designlibrary", "--subtype", $subtype,
        "--cdc-mode", "--refineparent", $refineparent);
    push @cmd, "--verbose" if $verbose;
    push @cmd, ("$outcell.v", $outcell );
    my_system @cmd;
}

exit 0 if $just_cast;
if (checkDependency("$cdcextracted.cdl", $targetcast, __LINE__)) {
    # create cdc cdl
    print STDERR "Creating $cdcextracted.cdl" if $verbose;
    my_system("jflat","--tool=cdl","--cdl-translate=cadence","--cell=$cdcextracted","--cast-path=$cast_dir:$spec_dir:$cast_path","--output-file=$cdcextracted.cdl");
}
if (checkDependency ("$cdcextracted.graybox", "$cdcextracted.cdl", __LINE__)) {
    my @cmd="cast_query";
    push @cmd, "--cast-path=$cast_dir:$spec_dir:$cast_path";
    push @cmd, "--no-header";
    push @cmd, "--max-heap-size=$max_heap_size";
    push @cmd, ("--cell=$cdcextracted");
    push @cmd, ("--task=subcells", "--routed", "--filter=one-level");
    push @cmd, "--output=$cdcextracted.graybox";
    my_system @cmd;
}
usage("Unable to find $cdcextracted.cdl") if ! -s "$cdcextracted.cdl";

# need to create av65gp_svt.lef from spar, this contains fqcn's for avago lib
if (checkDependency("av65gp_svt.lef", "$spar_dir/vendor/avago/svt/gates/lef/av65gp_svt.lef", __LINE__)) {
    my_system("sed -e 's/ \\(av_[a-z0-9_]*\\)/ vendor.avago.svt.gates.\\1.0/' $spar_dir/vendor/avago/svt/gates/lef/av65gp_svt.lef > av65gp_svt.lef");
}
usage("Cannot create av65gp_svt.lef") if ! -s "av65gp_svt.lef";
if (checkDependency("av65gp_ck_svt.lef", "$spar_dir/vendor/avago/svt/gates/lef/av65gp_ck_svt.lef", __LINE__)) {
    my_system("sed -e 's/ \\(av_[a-z0-9_]*\\)/ vendor.avago.svt.gates.\\1.0/' $spar_dir/vendor/avago/svt/gates/lef/av65gp_ck_svt.lef > av65gp_ck_svt.lef");
}
usage("Cannot create av65gp_ck_svt.lef") if ! -s "av65gp_ck_svt.lef";

my $template = <<TEMPLATE;
BLOCK: BLOCK_NAME
TOP_DEF_FILE : DEF_FILE_NAME
LEF_FILE: SPAR_DIR/chip/alta/FM/lef/tech_star.lef SPAR_DIR/lib/synchronous/conversion/v3/lib.synchronous.conversion.v3.lef SPAR_DIR/lib/synchronous/conversion/v3/vendor.avago.svt.gates.lef SPAR_DIR/lib/dft/converter/lib.dft.converter.SYNC_CONVERTER_ARRAY_8.1001.lef_starRC SPAR_DIR/vendor/avago/svt/gates/lef/av65gp_svt.lef SPAR_DIR/vendor/avago/svt/gates/lef/av65gp_ck_svt.lef SPAR_DIR/vendor/avago/av_sites.lef av65gp_svt.lef av65gp_ck_svt.lef BLOCK_LEF_NAME
MACRO_DEF_FILE:
HIERARCHICAL_SEPARATOR: /
TCAD_GRD_FILE: FULCRUM_PDK_ROOT/share/Fulcrum/extract/starRC/nxtgrd
MAPPING_FILE: SPAR_DIR/chip/alta/FM/scripts/def.map
EXTRACTION: RC
EXTRACT_VIA_CAPS: YES
IGNORE_CAPACITANCE: ALL
MODE: 200
COUPLE_TO_GROUND: NO
NETLIST_SELECT_NETS:
REMOVE_DANGLING_NETS: YES
REMOVE_FLOATING_NETS: YES
REMOVE_FLOATING_PORTS: NO
POWER_NETS: VDD VSS Vdd GND
SPICE_SUBCKT_FILE : CDC_CELL_NAME.cdl
POWER_EXTRACT: NO
NETLIST_FILE: CDC_CELL_NAME.spf
NETLIST_FORMAT: SPF
NETLIST_GROUND_NODE_NAME: GND
OPERATING_TEMPERATURE: 90
CASE_SENSITIVE: YES
STAR_DIRECTORY: STARRUNDIRECTORY
TEMPLATE

# do the extract
if (checkDependency("$cdcextracted.spf", "$def $cdcextracted.cdl av65gp_svt.lef $cell_lef", __LINE__)) {
    # generate nets to extract:
    my $nets=cdcnets($def,$instance);

    my $cmd = $template;
    $cmd =~ s;\nNETLIST_SELECT_NETS:;\nNETLIST_SELECT_NETS: $nets;g;
    $cmd =~ s;BLOCK_NAME;$design;g;
    $cmd =~ s;DEF_FILE_NAME;$def;g;
    foreach my $cell (keys %extracells) {
        my $file=`grep "\\<$cell\\>" $spar_dir/chip/alta/FM/scripts/lef_file.list | head -1`;
        chomp $file;
        $cmd =~ s; BLOCK_LEF_NAME; SPAR_DIR/$file BLOCK_LEF_NAME; if $file ne "";
    }
    $cmd =~ s;BLOCK_LEF_NAME;$cell_lef;g;
    $cmd =~ s;FULCRUM_PDK_ROOT;$pdkroot;g;
    $cmd =~ s;CDC_CELL_NAME;$cdcextracted;g;
    $cmd =~ s;SPAR_DIR;$spar_dir;g;
    $cmd =~ s;STARRUNDIRECTORY;$subtype.star.run;;
    `sync`;
    $ENV{QB_LOCAL}=0;
    $ENV{QRSH_FLAGS}="-l a=lx24-amd64,mem=4G,starrc=1 -cwd";
    $ENV{QB_RUN_NAME}="cdcRC";
    $ENV{STAR_SCRIPT}="/p/rrc/tools/bin/star" if ! defined $ENV{STAR_SCRIPT};
    open (P, ">$cdcextracted.cmd");
    print P $cmd;
    close P;
    `/bin/rm -rf "$subtype.star.run"` if -d "$subtype.star.run";
    print STDERR "Extracting $instance" if $verbose;
    my_system("fulcrum qb $ENV{STAR_SCRIPT} StarXtract -clean '$cdcextracted.cmd' 1>'$cdcextracted.out' 2>'$cdcextracted.err'");
    my $errs=0;
    my $warns=0;
    open (P, "<$cdcextracted.err") or warn "Cannot open extract output error file $cdcextracted.err";
    while (<P>) {
        chomp;
        if (/Warnings:\s*(\d+)\s+Errors:\s*(\d+)/) {
            $errs += $2;
            $errors += $2;
            $warns += $1;
            $warnings += $1;
        }
        if (/^ERROR:/) {
            $errs++;
        }
    }
    close P;
    if ($warns or $errs) {
        print STDERR "Extract Errors: $errs, Warnings: $warns, see $cdcextracted.err";
        exit $errs if $errs;
    }
    else {
        print STDERR "No Extract Errors or Warnings";
        unlink "$cdcextracted.out";
        `/bin/rm -rf "$subtype.star.run"`;
    }
}
# create a top verilog from the encounter verilog for cdcs
my $flattoptcl=<<EFT;
set verilog_in \$env(VERILOG_INPUT)
set verilog_out \$env(VERILOG_OUTPUT)
set top_cell \$env(TOP_CELL)
set spar_root \$env(SPARDIR)
set memory_root \$spar_root/vendor/artisan/memory
set fhandle [open "\$spar_root/chip/alta/FM/scripts/lib_file_max.list" r]
lappend lib_file_list av65gp_svt_ss.lib
lappend lib_file_list av65gp_ck_svt_ss.lib
while {[gets \$fhandle line] >= 0} {
    set line [string trim \$line]
    if {[string length \$line] > 0} {
        lappend lib_file_list \$spar_root/\$line
    }
}
close \$fhandle
set_attribute library \$lib_file_list 
read_hdl \$verilog_in
elaborate \$top_cell
set_attribute ungroup_separator /
ungroup -all -only_user_hierarchy -flatten -exclude [filter -regexp subdesign ".*lib.synchronous.conversion.v3.SCAN_(A2S|S2A)-L" [find . -inst *]]
write_hdl > \$verilog_out
exit
EFT
####
my $cdc_cadence_cell=cadrename($cdccell);
my $flattopcmd=<<EFC;
#!/bin/bash
#checklic -w $rc_license
export VERILOG_INPUT='$verilog'
export VERILOG_OUTPUT='$design.cdcflat.v'
export TOP_CELL='$design'
export SPARDIR='$spar_dir'
/usr/local/cadence/bin/rc.71usr1 rc -queue -use_license $rc_license -file '$design.flat.tcl'
EFC
if (checkDependency ("$design.cdcflat.v", $verilog, __LINE__)) {
    open P, ">$design.flat.tcl";
    print P $flattoptcl;
    close P;
    open P, ">$design.flat.cmd";
    print P $flattopcmd;
    close P;
    chmod 0755, "$design.flat.cmd";
    print STDERR "Generating flat CDC netlist for $design" if $verbose;
    my $rv=my_system("./'$design.flat.cmd' 1>'$design'.flat.out 2>'$design'.flat.err");
    print STDERR "Error generating $design.cdcflat.v" if $rv;
    unlink "rc.cmd";
    exit $rv if $rv;
    unlink "rc.log";
}
if (checkDependency ("$cdcextracted.portxref", "$design.cdcflat.v", __LINE__)) {
    print STDERR "Reading $design.cdcflat.v";
    local ($/) = ";";
    if (open (P, "<$design.cdcflat.v")) {
    my $module;
    my $inst;
    my %xref=();
    while (<P>) {
        s/\s+/ /g;
        s/\\//g;
        if (/\smodule\s/) {
            s/.*\s+module\s+//;
            s/\\//g;
            s/\(/ (/;
            s/\s.*//;
            $module=$_;
            %xref=();
        }
        if (m:\s$instance:) {
            s/([\(\)])/ $1 /g;
            s/\s+/ /g;
            s/^\s//;
            my @f=split;
            if ($f[1] eq $instance) {
                my $cell=shift @f;
                my $inst=shift @f;
                for (my $n = 0; $n < $#f; $n++) {
                    if ($f[$n] =~ /^\./) {
                        my $port=$f[$n];
                        $port =~ s/^\.//;
                        my $net = $f[$n+2];
                        while ($f[$n+3] ne ')' and $n+3 < $#f) {
                            $net .= $f[$n+3];
                            $n++;
                        }
                        $net = "GND" if $net eq "1'b0";
                        $net = "Vdd" if $net eq "1'b1";
                        $xref{$port}=$net;
                    }
                }
                last;
            }
        }
    }
    close P;
    open (P, ">$cdcextracted.portxref");
    foreach my $port (sort keys %xref) {
        print P "$xref{$port} $port";
    }
    close P;
    }
}
if ( ! -s "$cdcextracted.portxref") {
   usage("$cdcextracted.portxref is missing or zero size");
}
# generate a complete encounter based alias map
unlink "$cdcextracted.aliases" if ( -f "$cdcextracted.aliases" and ! -s "$cdcextracted.aliases");
if (checkDependency ("$cdcextracted.aliases", $targetcast, __LINE__)) {
    my @cmd="cast_query";
    push @cmd, "--cast-path=$cast_dir:$spec_dir:$cast_path";
    push @cmd, "--no-header";
    push @cmd, "--max-heap-size=$max_heap_size";
    push @cmd, ("--cell=$cdcextracted");
    push @cmd, "--task=external_nodes=al:im:re,local_nodes=all_aliases";
    push @cmd, "--output=$cdcextracted.aliases";
    my_system @cmd;
}
unlink "$design.aliases" if ( -f "$design.aliases" and ! -s "$design.aliases");
if (checkDependency("$design.aliases", $verilog, __LINE__)) {
    my @cmd="cast_query";
    push @cmd, "--cast-path=$cast_dir:$spec_dir:$cast_path";
    push @cmd, "--no-header";
    push @cmd, "--max-heap-size=$max_heap_size";
    push @cmd, ("--cell=$design");
    push @cmd, "--task=external_nodes=al:im:re,local_nodes=all_aliases";
    push @cmd, "--output=$design.aliases";
    my_system @cmd;
}
if ( defined ($srcdir) and -d $srcdir and $srcdir ne "." and -s "$srcdir/$designbase.nodexref") {
    if (checkDependency ("$designbase.nodexref", "$srcdir/$designbase.nodexref", __LINE__)) {
        my_system("cp", "$srcdir/$designbase.nodexref", "$designbase.nodexref");
    }
}
if ( defined ($srcdir) and -d $srcdir and $srcdir ne "." and -s "$srcdir/$designbase.instxref") {
    if (checkDependency ("$designbase.instxref", "$srcdir/$designbase.instxref", __LINE__)) {
        my_system("cp", "$srcdir/$designbase.instxref", "$designbase.instxref");
    }
}

if (checkDependency("$cdcextracted.spice $cdcextracted.cells", "$cdcextracted.aliases $design.aliases $cdcextracted.cdl $cdcextracted.portxref $designbase.nodexref $designbase.instxref $cdcextracted.spf", __LINE__)) {
    my %ncast;
    my %icast;
    my $cadcell=cadrename($cdcextracted);
    my $castinst=$instance;
    $castinst =~ s/\//_slash_/g;
    $castinst =~ s/$/_slash_/;
    my %ok=();

    print STDERR "Reading $designbase.nodexref..." if $verbose;
    open (P, "<$designbase.nodexref") or die "Cannot open nodexref $designbase.nodexref";
    while (<P>) {
        chomp;
        my ($vlg,$cast)=split;
        $vlg =~ s/_slash_/\//g;
        while($cast =~ /^z\./) {
            $cast =~ s/^z\.//;
        }
        $ncast{$vlg}=$cast;
    }
    close P;
    print STDERR "Reading $designbase.instxref..." if $verbose;
    open (P, "<$designbase.instxref") or die "Cannot open instxref $designbase.instxref";
    while (<P>) {
        chomp;
        my ($vlg,$cast)=split;
        $vlg =~ s/_slash_/\//g;
        while($cast =~ /^z\./) {
            $cast =~ s/^z\.//;
        }
        $icast{$vlg}=$cast;
    }
    close P;
    print STDERR "Reading $cdcextracted.aliases..." if $verbose;
    open (P, "<$cdcextracted.aliases") or die "Cannot open aliases $cdcextracted.aliases";
    my %lcanon=();
    while (<P>) {
        chomp;
        s/\s.*//;
        my @f=split(/=/);
        my $canon=$f[0];
        foreach my $f (@f) {
            $lcanon{$f}=$canon;
        }
    }
    close P;
    print STDERR "Reading $design.aliases..." if $verbose;
    open (P, "<$design.aliases") or die "Cannot open designaliases $design.aliases";
    my %sclist=();
    my %canon=();
    while (<P>) {
        chomp;
        s/\s.*//;
        my @f=split(/=/);
        foreach my $f (@f) {
            $f =~ s/^z\.//;
        }
        my $canon=$f[0];
        $sclist{$canon}=[@f];
        foreach my $f (@f) {
            $canon{$f}=$canon;
        }
    }
    close P;
    print STDERR "Reading $cdcextracted.spf..." if $verbose;
    open (P, "<$cdcextracted.spf") or die "cannot open $cdcextracted.spf";
    my %nodes;
    my %inst;
    my %castinst;
    my @spice=();
    my %extractedsubcells=();
    while (<P>) {
        chomp;
        next if ! m:$instance:;
        s/\s+/ /g;
        s/^ //;
        s/ $//;
        next if /^\*/;
        next if /^$/;
        next if /^\./;
        s/Vss/GND/g;
        push @spice, $_;
        my @f=split;
        my $i=shift @f;
        $i =~ s/^x//i;
        my $cell=pop @f;
        $extractedsubcells{castrename($cell)}=1 if /^x/i;
        my $n=0;
        foreach my $node (@f) {
            $nodes{$node}++;
            $inst{$node}="$i";
        }
    }
    close P;
    # node .spice file is cast names!
    open P, ">$cdcextracted.cells";
    foreach my $cell (sort keys %extractedsubcells) {
        print P "$cell";
    }
    close P;
    my %subckts=();
    open (P, "<$cdcextracted.cdl") or die "Cannot open extracted cdl $cdcextracted.cdl";
    my $ln="";
    my %cinst=();
    my %cline=();
    while (<P>) {
        chomp;
        if (/^\+/) {
            s/^\+/ /;
            $ln .= $_;
            next;
        }
        my $lx=$_;
        $_=$ln;
        $ln=$lx;
        if (/^x/i) {
            my $in = $_;
            $in =~ s/\s.*//;
            $in =~ s/^x//i;
            $cinst{$in}=1;
            $cline{$in}=$_;
        }
        next if ! /^.subckt/i;
        %cinst=();
        %cline=();
        my @f=split;
        shift @f;
        my $subcell=shift @f;
        $subckts{$subcell}=[@f];
    }
    close P;
    my %pxref=();
    open (P, "<$cdcextracted.portxref") or die "cannot open $cdcextracted.portxref";
    while (<P>) {
        chomp;
        my ($nd,$pt)=split;
        if ($nd ne "GND" and $nd ne "Vdd") {
            $pxref{$nd}=$pt;
        }
    }
    close P;
    # new try
    my $err=0;
    my %xref=();
    foreach my $node (sort keys %nodes) {
        if ($node =~ /(.*):(\d+)$/) {
            my $nd=$1;
            my $i=$2;
            if (defined ($ncast{$nd})) {
                $nd = $ncast{$nd};
            }
            else {
                print STDERR "Cannnot translate $nd!";
                $err++;
            }
            $nd =~ s/^$castinst//;
            $xref{$node}="$nd:$i";
        }
        elsif ($node =~ /(.*):(.*)$/) {
            my $in=$1;
            my $pt=$2;
            if (defined ($icast{$in})) {
                $in=$icast{$in};
            }
            else {
                print STDERR "Cannot translate instance $in";
                $err++;
            }
            $in =~ s/^$castinst//;
            $xref{$node}="$in.$pt";
        }
        elsif (defined ($pxref{$node})) {
            $xref{$node}=$pxref{$node};
        }
        elsif (substr($node,0,length($inst{$node})) eq $inst{$node}) {
            my $nd = substr($node,length($inst{$node})+1);
            $xref{$node}="$icast{$inst{$node}}.$nd";
            $xref{$node} =~ s:^$instance/::;
            $xref{$node} =~ s/^$castinst//;
        }
        elsif (defined ($ncast{$node})) {
            $xref{$node}=$ncast{$node};
            $xref{$node} =~ s/^$castinst//;
        }
        else {
            print STDERR "Cannot translate node $node $inst{$node}";
            $err++;
        }
    }
    my %canoncnt=();
    foreach my $xlat (sort keys %xref) {
        my $canon=$lcanon{$xref{$xlat}};
        if ($xref{$xlat} =~ /(.*):(\d+)$/) {
            my $s=$1;
            my $n=$2;
            $canon = "$lcanon{$s}:$n" if defined ($lcanon{$s});
        }
        if (! defined ($canon)) {
            if ($xref{$xlat} =~ /(.*):(.*)$/) {
                my $pre=$1;
                my $post=$2;
            }
            else {
                if (defined $sclist{$canon{$xref{$xlat}}}) {
                    $canon=${@{$sclist{$canon{$xref{$xlat}}}}}[0];
                }
            }
        }
        if (defined ($canon) and $canon ne "GND" and $canon ne "Vdd") {
            if (defined ($canoncnt{$canon})) {
                $canon = "$canon:$canoncnt{$canon}";
                $canoncnt{$canon}++;
            }
            else {
                $canoncnt{$canon}=1;
            }
        }
        $xref{$xlat}=$canon;
    }
    open P, ">$cdcextracted.spice";
    select P;
    print "** extracted $cdcextracted from $design instancde $instance\n";
    my %cnodes=();
    print ".SUBCKT $cdcextracted ".join(" ", @{$subckts{"$cadcell"}});
    my %ports=();
    foreach my $node (@{$subckts{$cadcell}}) {
        $cnodes{$node} = $node if ! defined $cnodes{$node};
        $ports{$node}=0;
        $ports{"$node:1"}=0;
    }
    foreach my $line (@spice) {
        my @f=split(/ /, $line);
        if ($f[0] =~ /^X/i) {
            $f[$#f] = castrename($f[$#f]);
            chomp $f[$#f];
        }
        my $ic = $icast{substr($f[0],1)};
        $ic =~ s/^$castinst// if defined $ic;
        if (defined($ic) and $f[0] =~ /^x/i) {
            $f[0] = "X$ic";
            my $ci = instcast2cad($ic);
            $cinst{$ci} |= 2;
        }
        else {
            $f[0] =~ s:$instance/::;
        }
        my $ok=1;
        for (my $n = 1; $n < $#f; $n++) {
            if (($f[$n] =~ m:^$instance/:) and ! defined($xref{$f[$n]})) {
                $xref{$f[$n]}=$f[$n];
                $xref{$f[$n]} =~ s:$instance/::;
            }
            if (! defined $xref{$f[$n]}) {
                $ok=0;
                last;
            }
            else {
                $f[$n] = $xref{$f[$n]};
            }
            $ports{$f[$n]}++ if defined($ports{$f[$n]}) and $f[0] =~ /^x/i;
            $cnodes{$f[$n]}=$f[$n];
        }
        if ($f[0] =~ /^r/i) {
            if ($f[1] ne $f[2]) {
                $ports{$f[1]}++ if defined $ports{$f[1]};
                $ports{$f[2]}++ if defined $ports{$f[2]};
                $ok{$f[2]}=1 if ($f[1] eq "$f[2]:1");
                $ok{$f[1]}=1 if ($f[2] eq "$f[1]:1");
            }
            elsif (($f[1] =~ /:1$/) and defined($ports{$f[1]})) {
                # makes connections which were not there
                $f[1] =~ s/:1//;
                $ports{$f[1]}++;
                $ports{$f[2]}++;
                $ok{$f[1]}=1;
            }
        }
        print join(" ", @f) if $ok;
    }
    foreach my $ci (sort keys %cinst) {
        if ($cinst{$ci} == 1) {
            $cline{$ci} =~ s/\s+\/\s+/ /g;
            my @f=split(/ /, $cline{$ci});
            $f[0] = "X".instcad2cast(substr($f[0],1));
            $f[$#f] = castrename($f[$#f]);
            print STDERR "Adding missing instance: ".join(" ", @f) if $verbose;
            print join(" ", @f);
        }
        elsif ($cinst{$ci} == 2) {
            print STDERR "ERROR: $ci is in spice but not cdl";
        }
    }
    my @fix=();
    # connect up port nodes which are not really connected
    foreach my $port (sort keys %ports) {
        if (! ($port =~ /:/)) {
            $ok{$port}=1 if $ports{$port} > 0 and $ports{"$port:1"}==0;
            if (! $ok{$port}) {
                print "R$port $port $port:1 0";
            }
        }
    }
    print ".ENDS";
    close P;
    select STDOUT;
}
if (checkDependency("$cdcextracted.spice_include", "$cdcextracted.cells", __LINE__)) {
    open (P, "<$cdcextracted.cells") or die "Cannot open extracted cells $cdcextracted.cells";
    my @files=();
    while (<P>) {
        chomp;
        my $cell=$_;
        my $dir=$cell;
        $dir =~ s/\./\//g;
        my $file=`ls $lveroot/"$dir"/*/extracted/cell.spice 2>/dev/null | head -1`;
        chomp $file;
        print STDERR "no spice file for $cell" if ! -s $file;
        push @files, ".include \"$file\"";
    }
    close P;
    open (P, ">$cdcextracted.spice_include");
    print P join("\n", @files);
    close P;
}
if (1) { # let lve do this
    my $MINC=1e-17;
    my $MINR=10;
    my $MINRC=1e-12;
    if (checkDependency("$cdcextracted.cdlaliases", "$cdcextracted.cdl", __LINE__)) {
        my $grayfile="$cdcextracted.cells";

        my_system("cdlaliases --cell='$cdcextracted' --graybox-list='$grayfile' --gates-regex='^(gate|stack)\\\\..*' < '$cdcextracted.cdl' > '$cdcextracted.cdlaliases'");
    }
    if (checkDependency("$cdcextracted.aspice.tmp", "$cdcextracted.spice $cdcextracted.cdlaliases", __LINE__)) {

        my_system("rc_spice2aspice","--alias-file","$cdcextracted.cdlaliases","--minC",$MINC,"--minR",$MINR,"--minRC",$MINRC,"$cdcextracted.spice","$cdcextracted.aspice.tmp");
    }
    if (checkDependency("$cdcextracted.aspice", "$cdcextracted.aspice.tmp $cdcextracted.cells", __LINE__)) {
        open (P, "<$cdcextracted.cells") or die "Cannot open $cdcextracted.cells";
        my @files=();
        while (<P>) {
            chomp;
            my $cell=$_;
            my $dir=$cell;
            $dir =~ s/\./\//g;
            my $file=`ls $lveroot/"$dir"/*/extracted/cell.aspice 2>/dev/null | head -1`;
            chomp $file;
            push @files, ".include \"$file\"";
        }
        close P;
        open (P, ">$cdcextracted.aspice");
        select P;
        print "/* extracted $cdcextracted from $design instance $instance */\n";
        print join("\n", @files);
        close P;
        select STDOUT;
        my_system("cat '$cdcextracted.aspice.tmp' >> '$cdcextracted.aspice'");
    }
} # end let lve do this
# run asta in lve
if (checkDependency("$subtype.todo","$cdcextracted.spice $cdcextracted.cells", __LINE__)) {
    $subtype =~ m:.*_(\d+)ps$:;
    my $tau=$1;
    $tau=53 if ! defined $tau;
    $tau--;
    my @castsplit=split(/:/, $cast_path,2);
    open (P, ">$subtype.todo");
    print P <<ET;
--mode=custom
--custom-spice=$cdcextracted.spice
--custom-spice-include=$cdcextracted.spice_include
--astaTau=$tau
--asta-tau-bound=$tau
--cast-dir=$cast_dir:$castsplit[0]
--spec-dir=$spec_dir:$castsplit[1]
--sub-lve-root-dir=/mnt/fulcrum/alta/lveB/lve
--output-dir=lve
--task=asta
$cdcextracted
ET
    close P;
}

print STDERR "Lve Todo file $subtype.todo";

END {
    if ($dot) {
        print DOT "\x7d";
        close DOT;
        `dot -Tpng -oextractCDC.png extractCDC.dot`;
        if (! fork ()) {
            `display extractCDC.png`;
            exit 0;
        }
    }
}
