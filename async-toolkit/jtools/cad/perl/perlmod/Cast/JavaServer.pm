#
# JavaServer
#
# Perl module for interfacing with the Java CastFileServer.  Enables multiple
# Java tools to be run without re-parsing the Cast design.
#
# Adapted from Supersize::JavaUtil in the ubersize package.
#

package Cast::JavaServer;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &initialize_cast_server
        &kill_cast_server
        &query_cast_server
        &run_cast_server_command
        &refresh_cast_server
    );
}

use strict;
use FileHandle;
use IPC::Open3;
use IO::Select;

our $SERVER_PID = -1;
our $SEL;
our $SERVER_ERROR = 0;

my %javalookup = (
    CastQuery => "com.avlsi.tools.jauto.CastQuery",
    JFlat => "com.avlsi.tools.jflat.JFlat",
    cast2cdl => "com.avlsi.tools.jauto.Cast2Cdl",
    cast2def => "com.avlsi.tools.cast2def.Cast2Def",
    cast2skill => "com.avlsi.tools.cast2skill.Cast2Skill",
    cast_file_server => "com.avlsi.cast2.util.CastFileServer",
    cast_query => "com.avlsi.tools.jauto.CastQuery",
    caststat => "com.avlsi.tools.jauto.CastStat",
    cdl_renamer => "com.avlsi.file.cdl.util.rename.CDLRenamer",
    cdlaliases => "com.avlsi.file.cdl.parser.CDLAliases",
    cdlstat => "com.avlsi.file.cdl.parser.CDLstat",
    generate_charge_sharing_tests => "com.avlsi.tools.jauto.GenerateChargeSharingTests",
    generate_gdsII_data => "com.avlsi.layout.gdsII.GenerateGDSIIData",
    generate_ip_data => "com.avlsi.tools.ipgen.GenerateIPData",
    generate_plt_subtypes => "com.avlsi.tools.jauto.GeneratePLTSubtypes",
    inliner => "com.avlsi.file.cdl.parser.Inliner",
    jauto => "com.avlsi.tools.jauto.Jauto",
    jflat => "com.avlsi.tools.jflat.JFlat",
    jlvs => "com.avlsi.tools.lvs.LVS",
    jtimer => "com.avlsi.tools.jauto.Jtimer",
    merge_hint => "com.avlsi.tools.jauto.MergeHint",
    query => "com.avlsi.tools.jauto.CastQuery",
    "rename" => "com.avlsi.file.cdl.util.rename.Rename",
    subtype_merge => "com.avlsi.tools.jauto.SubtypeMerge",
    subtype_split => "com.avlsi.tools.jauto.SubtypeSplit",
);

#
# Initialize CastFileServer interface
#
# Variables used (as key/value pairs in the $V_r hash):
#   PACKAGE_ROOT    -   Fulcrum package root. [REQUIRED]
#   PACKAGE_NAME    -   Fulcrum package name (.jar will be appended) [OPTIONAL]
#   CAST_PATH       -   Cast path to use. [REQUIRED]
#   HEAP_SIZE       -   Traditional --max-heap-size argument.
#   CAST_DEFINES    -   --define arguments to the cast parser
#   DEBUG           -   Verbose output (to standard error) if 1.
#
sub initialize_cast_server {
    my $V_r = shift;

    # Check for required settings
    if (!exists $V_r->{CAST_PATH}) {
        die "CastFileServer error: CAST_PATH not specified";
    }

    # Check if a cast server process is running already, kill it if so.
    if ($SERVER_PID != -1) {
        print STDERR "Restarting Java cast server.\n" if ($V_r->{DEBUG});
        close In;
        close Out;
        close Err;
        kill "INT", $SERVER_PID;
        waitpid $SERVER_PID, 0;
        $SERVER_PID = -1;
    }
    else {
        print STDERR "Initializing Java cast server.\n" if ($V_r->{DEBUG});
    }

    # Setup
    my $package_name;
    if (!exists $V_r->{PACKAGE_NAME} && 
        $V_r->{PACKAGE_ROOT} =~ /\/tools\/(\w+)\/\d+/) {
        $package_name = $1;
    }
    else {
        $package_name = $V_r->{PACKAGE_NAME};
    }
    my $classpath = "$V_r->{PACKAGE_ROOT}/share/java/" .
                    $package_name . ".jar:" .
                    "$V_r->{PACKAGE_ROOT}/share/java/antlr-2.7.2.jar";

    # Run the CastFileServer
    my $cmd = "LD_LIBRARY_PATH=" . path_to_libs($V_r) . " ";
    $cmd .= "java -classpath $classpath ";
    $cmd .= "-Xmx$V_r->{HEAP_SIZE} " if (exists $V_r->{HEAP_SIZE});
    $cmd .= "com.avlsi.cast2.util.CastFileServer ";
    $cmd .= "'--cast-path=$V_r->{CAST_PATH}' ";
    if (exists $V_r->{CAST_DEFINES}) {
        foreach my $def (@{$V_r->{CAST_DEFINES}}) {
            $cmd .= "'--define=$def' ";
        }
    }

    print STDERR "  Running $cmd\n" if ($V_r->{DEBUG});
    $SERVER_PID = open3(*In, *Out, *Err, $cmd);
    $SERVER_ERROR = 0;
    #In->autoflush(1);
    $SEL = new IO::Select();
    $SEL->add(*Out);
    $SEL->add(*Err);
}

#
# Kill the cast server (to free system memory)
#
sub kill_cast_server {
    my $V_r = shift;
    if ($SERVER_PID != -1) {
        print STDERR "Terminating Java cast server.\n" if ($V_r->{DEBUG});
        close In;
        close Out;
        close Err;
        kill "INT", $SERVER_PID;
        waitpid $SERVER_PID, 0;
        $SERVER_PID = -1;
    }
}

#
# Issue a CastQuery command to the CastFileServer
#
sub query_cast_server {
    my ($V_r, $query_cmd, $output_ref, $be_quiet, $output_handler) = @_;
    return run_cast_server_command($V_r, 
        "com.avlsi.tools.jauto.CastQuery $query_cmd", 
        ($be_quiet ? undef : \*STDOUT), undef, -1, $output_ref,
        $output_handler);
}

#
# Run an arbitrary Java class command line using the CastFileServer 
# If be_quiet is set, then no output is printed whatsoever except 
# fatal errors.  Otherwise, the Java class' stdout and stderr is 
# printed (stderr is collected and presented on completion).  
# Returns one of three values:
#
#    0 - No problems
#   -1 - Unrecognized output to stderr, but not an exception
#   -2 - Exception, IPC problems, class termination, etc.
#
# If $out_fh or $err_fh are defined, then the command's standard
# output and error will be written to the respective file handle.
#
# $err_mode: If -1, the command's standard error output will be printed
# to our standard error (in addition to writing it to $err_fh, if 
# specified).  If >=0, this number of lines will be saved and reported on 
# command completion in an error condition (Note: 0 is an interesting case).
#
# Pushes all "trimmed" stdout lines to @{$output_ref}, if it's non-undef.  
# ("Trimmed" in the sense that preceding & trailing whitespace is removed
# and empty lines are filtered.)  If output_handler is defined, it will be
# invoked on each complete stdout/stderr line as
#
#   &{$output_handler}(0, stdout_line)
#   &{$output_handler}(1, stderr_line)
#
# In this case, there is no filtering or trimming of the command output.
#
sub run_cast_server_command {
    my $V_r         = shift;
    my $cmd         = shift;
    my $out_fh      = shift;        # standard output filehandle (can be undef)
    my $err_fh      = shift;        # standard error filehandle (can be undef)
    my $err_mode    = shift;        # See above
    my $output_ref  = shift;        # optional
    my $output_handler = shift;     # optional

    # remove shell line extensions from command
    $cmd =~ s/\\\n//sg;
    $cmd =~ s/['"]//g;
    $cmd =~ s/\s+/ /g;

    my @cmd=split(/ /,$cmd);
    if (defined($javalookup{$cmd[0]})) {
        $cmd[0] = $javalookup{$cmd[0]};
        $cmd = join(" ", @cmd);
    }
    # Start cast server if it isn't already running
    initialize_cast_server($V_r) if ($SERVER_PID == -1);

    if ($V_r->{DEBUG}) {
        print STDERR "Issuing command to Java cast server:\n";
        print STDERR "  $cmd\n";
    }
    print In "- - $cmd\n";
    my $class = (split /\s+/, $cmd)[0];
    my $done = 0;
    my @ready;
    my $out_data;
    my $err_data;
    my @saved_err_lines;
    my $out_line = "";
    my $err_line = "";
    my $return_code = 0;
    while (!$done && (@ready = $SEL->can_read())) {
        foreach my $fh (@ready) {
            if ($fh eq *Out) {
                if (!sysread Out, $out_data, 4096) {
                    print STDERR "Warning: Java cast server closed stdout.\n";
                    $return_code = -2;
                    #$done = 1;
                }
                my @lines = split /\n/, $out_data;
                my $leftover = pop @lines if ($out_data !~ /\n$/);
                my $starts_with_newline = ($out_data =~ /^\n/);
                while (@lines || $out_line ne "" && $out_data eq "\n") {
                    $out_line .= shift @lines unless ($starts_with_newline);
                    if (defined $output_handler) {
                        &{$output_handler}(0,$out_line);
                    }
                    print $out_fh "$out_line\n" if (defined $out_fh);
                    $out_line =~ s/\s*$//; $out_line =~ s/^\s*//;
                    if ($out_line !~ /^$/ && defined $output_ref) {
                        push @{$output_ref}, $out_line;
                    }
                    $out_line = ""; $out_data = "";
                    $starts_with_newline = 0;
                }
                $out_line .= $leftover if ($leftover);
            }
            elsif ($fh eq *Err) {
                if (!sysread Err, $err_data, 80) {
                    print STDERR "Warning: Java cast server closed stderr.\n";
                    $return_code = -2;
                    $done = 1;
                }
                my @lines = split /\n/, $err_data;
                my $leftover = pop @lines if ($err_data !~ /\n$/);
                my $starts_with_newline = ($err_data =~ /^\n/);
                while (@lines || $err_line ne "" && $err_data eq "\n") {
                    $err_line .= shift @lines unless ($starts_with_newline);
                    if ($err_line !~ /^CastFileServer:/) {
                        if (defined $output_handler) {
                            &{$output_handler}(1,$err_line);
                        }
                        if ($err_mode==-1) {
                            print STDERR "$err_line\n";
                        }
                        elsif (@saved_err_lines < $err_mode) {
                            push @saved_err_lines, $err_line;
                        }
                        print $err_fh "$err_line\n" if (defined $err_fh);
                    }
                    elsif ($err_line eq "CastFileServer: EXCEPTION") {
                        $return_code = -2;
                        if ($V_r->{DEBUG}) {
                            print STDERR "---------------------------\n";
                            print STDERR "Error running\n$cmd\n";
                            print STDERR "---------------------------\n";
                        }
                        else {
                            print STDERR "Error running $class:\n" 
                        }
                    }
                    elsif ($err_line =~ /^CastFileServer: EXIT (.*)$/) {
                        print STDERR "Warning: $class exited with return ".
                            "code $1 \n";
                        $return_code = -2 if ($1 != 0);
                    }
                    elsif ($err_line eq "CastFileServer: SUCCESS" && 
                           $V_r->{DEBUG}) {
                        print STDERR "Succefully ran $cmd.\n";
                    }
                    elsif ($err_line eq "CastFileServer: READY") {
                        $done = 1;
                    }
                    print STDERR "ERR: $err_line\n" if ($V_r->{DEBUG});
                    $err_line = ""; $err_data = "";
                    $starts_with_newline = 0;
                }
                $err_line .= $leftover if ($leftover);
            }
        }
    }
    $return_code = -2 if (!$done);
    # Drain stdout (not usually necessary)
    while ($SEL->can_read(0)) {
        #print "Draining stdout...\n";
        last if (!sysread Out, $out_data, 4096);
        my @lines = split /\n/, $out_data;
        my $leftover = pop @lines if ($out_data !~ /\n$/);
        while (@lines) {
            $out_line .= shift @lines;
            if (defined $output_handler) {
                &{$output_handler}(0,$out_line);
            }
            print $out_fh "$out_line\n" if (defined $out_fh);
            $out_line =~ s/\s*$//; $out_line =~ s/^\s*//;
            if ($out_line !~ /^$/ && defined $output_ref) {
                push @{$output_ref}, $out_line;
            }
            $out_line = "";
        }
        $out_line = $leftover;
    }
    foreach my $l (@saved_err_lines) {
        if ($l !~ /^SYNTAX/ && $l !~ /^$/ && $return_code > -2) {
            $return_code = -1;
        }
        print STDERR $l . "\n";
    }
    $SERVER_ERROR = 1 if ($return_code == -2);
    return $return_code;
}


# Get LD_LIBRARY_PATH (only valid for local jobs).  Nasty system-specific crap
# (Taken from Supersize::Util)
sub path_to_libs {
    my $V_r = shift;

    my $osType=`uname -s`; chomp $osType;
    my $archType=`uname -m`; chomp $archType;

    my $lib_path = "$V_r->{PACKAGE_ROOT}/$osType-$archType/lib:" .
                   "/usr/local/lib:";

    if ($archType eq "x86_64") {
        $lib_path .= "/usr/lib64:/lib64";
    }
    else {
        $lib_path .= "/usr/lib:/lib";
    }
}


1;
