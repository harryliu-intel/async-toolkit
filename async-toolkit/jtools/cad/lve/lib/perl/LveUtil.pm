
package LveUtil;

use POSIX;
use LveStatus;
use IPC::Open2;
use IPC::Open3;
use Archive::Zip qw( :ERROR_CODES :CONSTANTS );
use Archive::Extract;
use File::Spec;



BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &round_float &is_numeric &get_nth_run_param &read_localnodes
        &find_cells &find_rawfiles &find_fqcns &fqcn_to_path &partition_fqcnminus 
        &read_include_file &read_cell_list &parseArgs &includeConfig 
        &error &canonicalizePath &my_system &reName &reName2 &reNameNode &dfIIDir
        &ns_to_ps &summarizeStatus &archive_extrace_files
        &mktemp_workdir &cleanUp_workdir &is_node_archive &archive_memberNamed
        &em_unit_freq &em_unit &archive_getcontent &archive_addfile &check_path
        &archive_getfiletimestamp &parse_nodeprops &read_cdl_aliases
    );
}

use strict;

our @cleanMe=();

#
# Rounds num to the nearest 10^digit.
# e.g. (11.25,-1) -> 11.3
#      (11.25,0)  -> 11
#      (11.21,1)  -> 10
#
sub round_float {
    my ($num,$digit) = @_;
    my $shifted_num = $num * 10**(-$digit);
    my $shifted_int = int($shifted_num);
    my $leftover = $shifted_num - $shifted_int;
    $shifted_int++ if ($leftover >= 0.5);
    return $shifted_int * 10**$digit;
}

# convert a list of ns numbers to integer ps, or FAIL
sub ns_to_ps {
    my $status = "PASS";
    for (my $i=0; $i<@_; $i++) {
        if (is_numeric($_[$i])) { 
           if($_[$i]>0){
              $_[$i] = int(1000*$_[$i]+0.5); 
           }else{
              $_[$i] = int(1000*$_[$i]-0.5); 
           }
        }
        else { $_[$i] = "FAIL"; $status = "FAIL"; }
    }
    return $status;
}

sub reName {
    my ($renamer,$from,$to,$type,$map,$strs) = @_;

    my $pid=
    open2(*Reader,*Writer,"$renamer --max-heap-size=256m --from=$from --to=$to --type=$type") ||
        die "Fatal: Can't start $renamer for renaming";

   my $fail=0;
   # rename of abc should always generate abc!
   print Writer "abc\n";
   my $e=<Reader>;
   chomp $e if defined $e;
   if ($e ne "abc") {
       $fail=1;
       print STDERR "$e\n";
       close Writer;
       while (<Reader>) {
           print STDERR;
       }
       close Reader;
   }
   if (! $fail ) {
       foreach my $old (@{$strs}) {
            print Writer "$old\n";
            my $new = <Reader>;
            if (! defined($new) or $new eq "") {
                $fail=1;
                last;
            }
            chomp $new;
            $map->{$old} = $new;
        }
    }
    close Writer;
    close Reader;
    waitpid $pid, 0;
    if ($fail) {
        die "Fatal: reName failed";
    }
    # kill the zombie
}

sub reName2 {
    my ($context, $type, $from, $to, $name) = @_;
    $context->{'pid'} = open2($context->{'read'}, $context->{'write'}, "rename --type=all")
        unless $context->{'pid'} && (kill 0, $context->{'pid'}) == 1;
    my ($rh, $wh) = ($context->{'read'}, $context->{'write'});
    print $wh "$type $from $to $name\n";
    chomp(my $result = <$rh>);
    die "Exception in reName2: type=$type from=$from to=$to name=$name" unless $result;
    return $result;
}

sub reNameNode {
    my ($renamer,$from,$to,$name) = @_;

    my $pid=
    open2(*Reader,*Writer,"$renamer --max-heap-size=256m --from=$from --to=$to --type=node") ||
        die "Fatal: Can't start $renamer for renaming nodes";

    my $fail=0;
    # rename of abc should always generate abc!
    print Writer "abc\n";
    my $e=<Reader>;
    chomp $e if defined $e;
    if ($e ne "abc") {
        $fail=1;
        print STDERR "$e\n";
        close Writer;
        while (<Reader>) {
            print STDERR;
        }
        close Reader;
    }
    my $new=undef;
    if (! $fail ) {
        print Writer "$name\n";
        $new = <Reader>;
        chomp $new;
        if (! defined($new) or $new eq "") {
            $fail=1;
        }
        close Writer;
        close Reader;
    }
    # kill the zombie
    waitpid $pid, 0;
    if ($fail) {
        die "Fatal: reNameNode failed";
    }
    return $new;
}

sub dfIIDir {
    my ($dfII_dir,$cell) = @_;
    my @cell = split("\\.",$cell);
    my @lib = @cell[0..$#cell-2];
    
    my $lib = join("/",@lib);
    $cell =~ s/\./\#2e/g;
    $cell =~ s/-/\#2d/g;
        
    return "$dfII_dir/$lib/$cell";
}

# parses $args >> $options
# args are just the args from the command line
# an option is either a
# 1) key value pair (list reference)
# 2) scalar

sub parseArgs() {
    my ($args,$options) = @_;
    my $pdk_root = undef;
    foreach my $arg (@{$args}) {
        next if($arg =~ /^\#/ );
        if ($arg =~ /^--(.*)/) {
            # key-value pair
            my ($key, $value) = split("=",$1,2);
            if ( ! defined ($value)) {
                $value = 1;
            }
            if(!defined $key || !defined $value) {
                # bad syntax
                error("bad --key=value syntax in ($arg).");
            } elsif($key eq "include" || $key eq "config" ) {
                my @config_args = ();
                &includeConfig($value,\@config_args);
                my $pdk_temp = &parseArgs(\@config_args,$options);
                $pdk_root = $pdk_temp if defined($pdk_temp);
            } elsif($key eq "fulcrum-pdk-root") {
                my $pdk_temp = canonicalizePath($value);
                $pdk_root = $pdk_temp if defined($pdk_temp);
            } else {
                my @pair = ($key,$value);
                push @{$options}, \@pair;
            }
        } else {
            # cell as a convenience translate / to . in a cell name
            $arg =~ s:/:.:g;
            push @{$options}, $arg;
        }
    }
    return $pdk_root;
}

# adds args from $config file to $args
sub includeConfig  {
    my ($config,$args) = @_;
    my @packageroot=split(/\//, $config);
    pop @packageroot;
    while ( @packageroot and -d join("/", @packageroot) and ! -f join("/",@packageroot)."/.fulcrum-package-root") {
        pop @packageroot;
    }
    my $packageroot = join("/",@packageroot);
    undef @packageroot;
    if (open INCLUDE, "<$config") {
        while (<INCLUDE>) {
            my ($line) = split("#",$_);
            foreach my $arg (split(" ",$line)) {
                if ($packageroot ne "" and -d $packageroot and $arg =~ /\$packageroot\$/) {
                    $arg =~ s:\$packageroot\$:$packageroot:g;
                }
                push @{$args}, $arg;
            }        }
        close INCLUDE;
    } else { error("can't include file \"$config\"."); exit;}
}

# print an error message and set ERROR flag
sub error {
    my ($msg) = @_;
    print STDERR "ERROR: $msg\n";
    my $ERROR = 1;
    exit;
}

# check if a string is a legal real number
sub is_numeric {
    my ($parm) = @_;
    if ($parm =~ /^[-+]?[\d]+(\.[\d]+)?([eE]?[-+]?[\d]+)?$/ ) { return 1; }
    else { return 0; }
}

# check if path exist or not.
sub check_path {
    my ($path) = @_;
    foreach my $dir (split(":",$path)) {
        unless (-e $dir) { error("$path does not exist."); exit; }
    }
}

# get a sweep option from the path
sub get_nth_run_param {
    my ($path, $n) = @_;
    my @parts = split("/",$path);
    my $k;
    for(my $i=0; $i<@parts; $i++) {
        my $dir = $parts[$i];
        if(defined($parts[$i+$n]) && ($dir =~ /^estimated$/ ||
           $dir =~ /^totem$/ ||
           $dir =~ /^extracted$/ ||
           $dir =~ /^extracted-/ || # for extract corners
           $dir =~ /^accurate$/ ||
           $dir =~ /^accurate-/ || # for extract corners
           $dir =~ /^nogeometry$/ ||
           $dir =~ /^custom$/)) {
            $k = $i;
        }
    }
    return $parts[$k+$n];
}

# Returns the local nodes as recorded in the LVE cell.localnodes file
sub read_localnodes {
    my ($dir) = @_;
    my $file;
    if ( -f $dir and $dir =~ /localprop/) {
        $file = $dir;
    }
    else {
        $file = "${dir}/cell.localprops";
    }
    my @nodes = ();
    my $lastdir=$dir;
    while( !( -e $file ) and $file ne "/cell.localprops") {
        $dir =~ s=(.*)/.*$=$1=;
        if ($dir eq $lastdir) {
            print STDERR "Warning: Infinite loop in LveUtil::read_localnodes, dir=$dir ($_[0])\n";
            return @nodes;
        }
        $lastdir=$dir;
        $file = "${dir}/cell.localprops";
    }
    open (LOCALPROPS, $file) || warn "Warning: Couldn't read $file.\n";
    while(<LOCALPROPS>){ if($_ =~ /^(\S+)/){ push @nodes, $1; }}
    close LOCALPROPS;
    return @nodes;
}

# Optimized routine for finding all .cellname files (For 3300+ cells,
# 21 seconds versus 23 minutes(!) using find).
# Arguments: the LVE working directory and an empty list reference.
my $find_cells_lvl=0;
my %find_cells_skip_dirs=(
    "plt" => 1,
    "plt1f" => 1,
    "spicelib" => 1,
    "temp" => 1,
);

sub find_cells {
    my ($dir,$cell_list_ref) = @_;
    return if ! -d $dir;
    my $path_tail=$dir;
    $path_tail =~ s/.*\///;
    if ($find_cells_lvl == 1 and $find_cells_skip_dirs{$path_tail}) {
        return;
    }
    $find_cells_lvl++;
    if (-e "$dir/.cellname") {
        push @$cell_list_ref, "$dir/.cellname";
    }
    elsif ( -d "$dir") {
        opendir DIR, $dir || warn "Couldn't read directory $dir.\n";
        my @subdirs = grep { $_ =~ /^[^\.]/ } readdir DIR;
        closedir DIR;
        foreach my $subdir (@subdirs) {
            find_cells("$dir/$subdir",$cell_list_ref);
        }
    }
    $find_cells_lvl--;
}

# Optimized search for lve task related raw files; should especially
# help with flat routed cells since alint dir for these are HUGE!
# For Tahoe lve directory reduces summarize time to less than 1hr instead
# of 26hrs!!!!

#   jlvs.raw (celldir)
#       |
#      (viewdir)
#   hdrc.raw drc.raw,lvs.raw,frc.raw
#       |
#      spice dir (estimated/extracted)
#       |
#      /|\
#   a   a  h
#   l   s  s
#   i   p  i
#   n   i  m
#   t   c
#       e

sub find_rawfiles {
    my $celldir = shift;
    my @rawfiles = ();
    my @l = ();
    
    @l = get_rawfile("$celldir", 0, "jlvs.raw");
    push @rawfiles, @l;
    opendir(CELLDIR, "$celldir") or warn "Cannot open $celldir\n";
    my @allfiles = grep { $_ ne '.' and $_ ne '..'} readdir CELLDIR;
    
    my @viewraws = ("hlvs.raw", "hdrc.raw", "drc.raw", "lvs.raw", "frc.raw");
    foreach my $viewdir(@allfiles){
        $viewdir = "$celldir/$viewdir";
        if(-d $viewdir){
            foreach my $viewraw(@viewraws){
                @l = get_rawfile("$viewdir", 0, $viewraw);
                push @rawfiles, @l;
            }            

            opendir(VIEWDIR, "$viewdir") 
                or warn "Cannot open $viewdir\n";
            my @viewfiles = grep { $_ ne '.' and $_ ne '..'} readdir VIEWDIR;
            foreach my $modedir(@viewfiles){
                $modedir = "$viewdir/$modedir";
                 if(-d $modedir){
                     
                     @l = get_rawfile("$modedir", 0, "extract.raw");
                     push @rawfiles, @l;
                     
                     if(-d "$modedir/aspice"){
                         @l = get_rawfile("$modedir/aspice", 6, "aspice.raw");
                         push @rawfiles, @l;
                         
                     }
                     if(-d "$modedir/hsim"){
                         @l = get_rawfile("$modedir/hsim", 5, "hsim.raw");
                         push @rawfiles, @l;
                         @l = get_rawfile("$modedir/hsim", 5, "totem.raw");
                         push @rawfiles, @l;
                     }
                     if(-d "$modedir/alint"){
                         @l = get_rawfile("$modedir/alint", 3, "alint.raw");
                         push @rawfiles, @l;
                         @l = get_rawfile("$modedir/alint", 3, "lib.raw");
                         push @rawfiles, @l;
                         @l = get_rawfile("$modedir/alint", 3, "asta.raw");
                         push @rawfiles, @l;
                         @l = get_rawfile("$modedir/alint", 3, "slint.raw");
                         push @rawfiles, @l;
                     }
                 }
            }
        }
    }
    return @rawfiles;
}

#get rawfile relative to a give directory and specified depth
sub get_rawfile {
    my $dir = shift;
    my $depth = shift;
    my $taskraw = shift;
    my @rawfiles = ();
    my @allfiles = ();

    if($depth < 0){return @rawfiles; }
    #print "$dir/$taskraw $depth\n";
    if($depth == 0 && -e "$dir/$taskraw"){ 
       
        push @rawfiles, "$dir/$taskraw"; }
    else {
        
        opendir(DIR, "$dir") or warn "Cannot open $dir\n";
        @allfiles = grep { $_ ne '.' and $_ ne '..'} readdir DIR;

        #print "checking: $dir, $taskraw, $depth\n";
        foreach my $file (@allfiles){
            $file = "$dir/$file";
            if(-d $file){ 
                my @r = get_rawfile($file,$depth-1,$taskraw);
                push @rawfiles, @r;
            }
        }
    }
    return @rawfiles;
}

# Just like find_cells, but returns a list of fqcnminus names
sub find_fqcns {
    my ($dir,$cell_list_ref) = @_;
    my @cellnames;
    find_cells($dir,\@cellnames);
    foreach my $cellname (@cellnames) {
#       open CN, $cellname || warn "Couldn't read $cellname.\n";
#       chomp(my $fqcn = <CN>);
#       close CN;
        my $fqcn = $cellname;
        $fqcn =~ s:$dir::;
        $fqcn =~ s:^/::;
        $fqcn =~ s/\/\.cellname$//;
        $fqcn =~ s/\//./g;
        push @$cell_list_ref, $fqcn;
    }
}

# Turns a dotted fqcn into a directory path
sub fqcn_to_path {
    my ($fqcnminus) = @_;
    my ($fqcn,$plusminus) = partition_fqcnminus($fqcnminus);
    $fqcn =~ s/\./\//g;
    return $fqcn . $plusminus;
}

# partition cellname into the fqcn and the plusminus section for 
# partial extraction 
sub partition_fqcnminus {
    my $cell = shift;
    my ($basecell,$plusminus);
    # to avoid problems in the regex below
    my $iscadence=0;
    $iscadence = 1 if $cell =~ /-L/;
    $cell=~s/-L/(/g;
    $cell=~s/-R/)/g;

    if ( $cell =~ /^([^(+-]+)(\([^)]*\)[^+-]*)?(.*)$/ ) {
        $basecell  = $1;
        $basecell .= $2 if (defined $2);
        $plusminus = $3;
    } else {
        $basecell = "";
        $plusminus = "";
    }
    if ($iscadence) {
        # restore the name for rename
        $basecell =~ s/\(/-L/g;
        $basecell =~ s/\)/-R/g;
        my $new = `echo "$basecell" | rename --type=cell --from=cadence --to=cast 2>/dev/null`;
        chomp $new;
        $basecell=$new if $new ne "";
    }
    return ($basecell, $plusminus);
}

# Parse a --include command-line include file, add each line to the beginning
# of the ARGV list (passed by reference as argv_ref).
sub read_include_file {
    my ($file,$argv_ref) = @_;
    open INC, $file || die "Couldn't read $file.\n";
    my @new_args = grep { s/\s*\\?\s*$// && !/^\#/ && !/^$/ } <INC>;
    push @new_args, @$argv_ref;
    @$argv_ref = @new_args;
    close INC;
}

# Build a cell list from a "todo"-style input file
# Just extracts the cells, not the environments.
sub read_cell_list {
    my ($file,$listref) = @_;
    open LIST, $file || die "Couldn't read $file.\n";
    @$listref = grep { s/(:.*)?\n$// } <LIST>;
    close LIST;
}

# canonicalizePath
sub canonicalizePath {
    my ( $thePath ) = @_;

    my $canonPath=$thePath;
    if ( -d "$thePath") {
        # I tried to use Cwd::abs_path, and it works sometimes and
        # others times it does not. This seems much more reliable.

        $canonPath=`cd $thePath; /bin/tcsh -f -c pwd`;
        chomp $canonPath;
    }
    else {
        # TODO: should implement with the above for all but the trailing part
        # of non-directory paths.
        my @dirParts = File::Spec->splitdir( $thePath );
        my $numDirectories = scalar( @dirParts );
        my $i;
        my @canonDirParts;

        if( !File::Spec->file_name_is_absolute( $thePath ) ) {
            @canonDirParts = File::Spec->splitdir( getcwd() );
        }
        my $numCanonDirParts = scalar( @canonDirParts );

        for ( $i=0; $i<$numDirectories; $i++ ) {
            if ( ! ( $dirParts[ $i ] eq "" ) ) {
                if ( $dirParts[ $i ] eq File::Spec->updir ) {
                    if ( $numCanonDirParts != 0 ) {
                        $numCanonDirParts--;
                        @canonDirParts = @canonDirParts[0..($numCanonDirParts-1)];
                    }
                    else {
                        @canonDirParts = ( @canonDirParts, $dirParts[ $i ] );
                    }
                }
                else{
                    if ( ! ( $dirParts[ $i ] eq File::Spec->curdir ) ) {
                        @canonDirParts = ( @canonDirParts, $dirParts[ $i ] );
                        $numCanonDirParts++;
                    }
                }
            }
        }
        $canonPath = File::Spec->catdir( @canonDirParts );

        if ( File::Spec->file_name_is_absolute( $thePath ) ) {
            $canonPath = "/" . $canonPath;
        }
    }
    $canonPath;
}


# execute a command, taking care of exit status and $verbose
sub my_system {
    my ($cmd)=@_;
    print STDERR "$cmd\n" if(defined $main::verbose && $main::verbose);
    system($cmd) == 0 or die "ERROR: $cmd failed.\n";
}

sub summarizeStatus {
    my ($all, $type)=@_;
    my %all=%{$all};
    my %type;
    %type = %{$type} if(defined $type);
    my @all=(keys %all);
    return "PASS" if $all{PASS} and $#all==0;
    return "FAIL" if ($all{FAIL} and defined $type{OTHER} and $type{OTHER});
    return "FAIL_NEWBUMP" if ($all{FAIL} and defined $type{FAIL_NEWBUMP} and $type{FAIL_NEWBUMP});
    return "FAIL_MNOISE"  if ($all{FAIL} and defined $type{FAIL_MNOISE} and $type{FAIL_MNOISE});
    return "FAIL" if ($all{FAIL});
    return "FAIL_NEWBUMP" if ($all{FAIL_NEWBUMP});
    return "FAIL_MNOISE" if ($all{FAIL_MNOISE});
    return "WARNING" if $all{WARNING};
    return "SIGNOFF" if $all{SIGNOFF};
    return "PASS" if $all{NA} and $all{PASS};
    return "NA" if $all{NA};
    return "NOT_TESTED";
}

sub is_node_archive{
  my ($dir,$file)=@_;
  if(-e "$dir/$file"){
    my $zip = Archive::Zip->new();
    unless ( $zip->read( "$dir/$file" ) == AZ_OK ) {
       die "Cannot read $dir/$file";
    }
    my @mbrs = $zip->memberNames();
    return (1, @mbrs);
  }
  return 0;
}

sub archive_extrace_files{
  my ($archive,$extract_path,@files)=@_;
  if (@files){
     my $zip = Archive::Zip->new();
     unless ( $zip->read( $archive ) == AZ_OK ) {
         die "Read error on $archive";
     }
     foreach my $element (@files) {
         $zip->extractMember($element, "$extract_path/$element");
     }
  }else{
     my $ae = Archive::Extract->new( archive => "$archive" );
     my $status = $ae->extract( to => $extract_path ) or die $ae->error;
  }
  `chmod 02775 $extract_path`;
  `chmod -R g+w   $extract_path`;
  return $extract_path;
}

sub archive_memberNamed{
  my ($archive,$name)=@_;
  my $zip = Archive::Zip->new();
  unless ( $zip->read( $archive ) == AZ_OK ) {
      die "Read error on $archive";
  }
  return $zip->memberNamed($name);
}

sub archive_getcontent{
  my ($archive,$name)=@_;
  my $zip = Archive::Zip->new();
  unless ( $zip->read( $archive ) == AZ_OK ) {
      die "Read error on $archive";
  }
  return $zip->contents($name);
}
sub archive_addfile{
  my ($archive,$name)=@_;
  my $zip = Archive::Zip->new();
  my @paths=split("/",$name); #remove directory structure.
  my $rename=$paths[$#paths];

  if(!-e $archive){
     $zip->addFile($name, $rename);
     unless ( $zip->writeToFileNamed("$archive") == AZ_OK ) {
      die "Error: zip write error $archive";
    }
  }else{
    unless ( $zip->read( $archive ) == AZ_OK ) {
        die "Read error on $archive";
    }
    my $member=$zip->memberNamed($rename);
    if(defined $member){
      $zip->removeMember($rename);
    }
    $zip->addFile($name,$rename);
    unless ( $zip->overwrite() == AZ_OK ) {
        die "Add file error on $archive";
    }
  }
}

sub archive_getfiletimestamp{
  my ($archive)=@_;
  my $timestamp={};
  my $zip = Archive::Zip->new();
  unless ( $zip->read( $archive ) == AZ_OK ) {
        die "Read error on $archive";
  }
  my @mbrs = $zip->members();

  foreach my $member (@mbrs){
    my $file=$member->fileName();
    $timestamp->{$file}=$member->lastModTime();
  }
  return $timestamp;

}


sub mktemp_workdir{
  my ($name)=@_;
  my $user =`whoami`; chomp($user);
  $name="lve" if(not defined $name);
  my $tmp;
  if (defined $ENV{TMP}) { $tmp = $ENV{TMP}; }
  else { $tmp = "/scratch"; }
  chomp($tmp);
  if (! -e $tmp) {
    system("mkdir -p $tmp");
  } 

  my $workdir = `mktemp -d ${tmp}/$user.$name.XXXXXX  ` 
    or die "Can't mktemp -d ${tmp}/$user.$name.XXXXXX\n";
  chomp $workdir;
  chmod 02775, "$workdir";
  push @cleanMe, $workdir;
  return $workdir; 
}

sub cleanUp_workdir{
  my ($cleandir)=@_;
  if(defined $cleandir){
    system("rm -rf \"$cleandir\"");
  }else{
    foreach my $dir (@cleanMe) {
        if ( -e "$dir" ) {
            system("rm -rf \"$dir\"");
        }
    }
    @cleanMe=();
  }
}

sub em_unit_freq {
   my ($unit,$freq)=@_; 
   if($unit=~/(\d+)(\S)/){
     my($div,$u)=($1,$2);
     $freq *= 1e+9 if ($u eq "G" or $u eq "g");
     $freq *= 1e+6 if ($u eq "M" or $u eq "m");
     $freq *= 1e+3 if ($u eq "K" or $u eq "k");
     $freq *= $div;
   }
   return $freq;
}

sub em_unit {
  my ($unit,$l)=@_; 
  my $scal=1;
  if($unit=~/(\d+)(\S)/){
    my($div,$u)=($1,$2);
    $scal = 1e-9 if ($u eq "n");
    $scal = 1e-6 if ($u eq "u");
    $scal = 1e-3 if ($u eq "m");
    $scal *= $div;
  }
  return ($scal,$l*$scal) if (defined $l);
  return ($scal);
}

sub parse_nodeprops {
    my ($file) = @_;
    my $result = {};
    local $_;
    my @columns = ('is_signoff',
                   'is_dynamic',
                   'delay+',
                   'delay-',
                   'estimated_delay_signoff+',
                   'estimated_delay_signoff-',
                   'slew_signoff+',
                   'slew_signoff-',
                   'skew_signoff+',
                   'skew_signoff-',
                   'bump_signoff+',
                   'bump_signoff-',
                   'wirewidth',
                   'wirespace',
                   'activity_factor',
                   'alint_max_bump_fanin',
                   'direction',
                   'thresh_bump_signoff+',
                   'thresh_bump_signoff-',
                   'leakage_signoff',
                   'is_staticizer',
                   'is_ground',
                   'is_power');
    open(my $fh, $file) || die "Can't open $file: $!";
    while (<$fh>) {
        my @fields = split;
        my $node = shift @fields;
        my $signoff = $node eq 'SIGNOFF' ? 1 : 0;
        $node = shift @fields if $signoff;
        unshift @fields, $signoff;
        foreach my $col (@columns) {
            my $val = shift @fields;
            if (defined($val)) {
                $result->{$node}->{$col} = $val;
            } else {
                die "Nodeprops file $file is malformed at line $.";
            }
        }
    }
    close $fh;

    return $result;
}

sub read_cdl_aliases {
    my ($canonical, @aliasFiles) = @_;
    foreach my $aliasFile (@aliasFiles) {
        open(my $fh, "$aliasFile") or die "Cannot open $aliasFile: $!";
        chomp(my $fqcn = <$fh>);
        while (<$fh>) {
            chomp;
            my @names = split /=/;
            foreach my $name (@names) {
                $canonical->{$fqcn}->{$name} = $names[0];
            }
        }
        close($fh);
    }
}

1;
