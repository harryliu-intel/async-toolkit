# $Id$
# $DateTime$

package Cast::SourceControl;

use strict;
use warnings;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &module_to_unqualified_depot_file
        &cell_to_unqualified_depot_file
        &subtype_to_unqualified_depot_file
        &user_file_to_unqualified_depot_file
        &qualify_depot_file
        &unqualified_depot_to_user_file
        &head_revision
        &filelog_since_last_integration
        &cell_dependencies
    );
    our @EXPORT_OK = qw(
        $PROCESS
        $JFLAT_PATH
    );
}
our @EXPORT_OK;


#
# Package Globals
#

# Sets the process to use when determining depot file locations 
# from layout subtype FQCNs.
our $PROCESS = "tsmc13";

# Where to find jflat.  Default is to require that the user has
# the desired executable in his command path.
our $JFLAT_PATH = "fulcrum jflat";


#
# Maps a module name (e.g. lib.buffer.half) to a branch-independent
# p4 depot location (e.g. //depot/hw/cast/lib/*/buffer/half.cast).
#
sub module_to_unqualified_depot_file {
    my ($m) = @_;
    my $f = $m;
    my $layout_subtype = 0;
    $layout_subtype = 1 if ($m =~ /\.\d+[^\.]*$/ && $m ne "lib.sram.10T" &&
                                                    $m ne "lib.sram.6T");
    $f =~ s!\.!/!g;
    $f .= ".cast";
    $f =~ s!^(core|chip)/([^/]+)/!$1/$2/*/!;
    $f =~ s!^(lib|standard)/!$1/*/!;
    $f =~ s!^vendor/([^/]+)/([^/]+)/!vendor/$1/$2/*/!;
    $f =~ s!^deprecated/(core|chip)/([^/]+)/!deprecated/$1/$2/*/!;
    $f =~ s!^deprecated/(lib|standard)/!deprecated/$1/*/!;
    $f =~ s!^deprecated/vendor/([^/]+)/([^/]+)/!deprecated/vendor/$1/$2/*/!;
    if (!$layout_subtype) {
        $f = "//depot/hw/cast/" . $f;
    }
    else {
        $f = "//depot/hw/layout/tsmc13/spec/" . $f;
    }
    return $f;
}

#
# Maps a cell name (e.g. lib.buffer.half.BUF_1of2 or
# lib.buffer.half.BUF_1of2.300) to a branch-independent p4 depot
# location (e.g. //depot/hw/cast/lib/*/buffer/half.cast or
# //depot/hw/layout/tsmc13/spec/lib/*/buffer/half/BUF_1of2/300.cast)
#
sub cell_to_unqualified_depot_file {
    my ($cell) = @_;
    my $is_layout_subtype = 0;
    my $dot = rindex $cell, ".";
    if (substr($cell, $dot+1) =~ /^\d/ &&
        $cell !~ /^lib\.sram\.(10|6T)\.[^\.]+$/) {         # Layout subtype
        #$dot = rindex $cell, ".", $dot-1;
        $dot = length($cell);
        $is_layout_subtype = 1;
    }
    elsif ($cell =~ /^deprecated/ && 
           substr($cell, $dot+1) =~ /^D\d+/) {    # deprecated
        $dot = length($cell);
    }
    my $module = substr $cell, 0, $dot;
    my $file = module_to_unqualified_depot_file($module);
    return ($file,$is_layout_subtype);
}

#
# Like cell_to_unqualified_depot_file, but only for layout subtypes
#
sub subtype_to_unqualified_depot_file {
    my ($m) = @_;
    my $f = $m;
    $f =~ s!\.!/!g;
    $f .= ".cast";
    $f =~ s!^(core|chip)/([^/]+)/!$1/$2/*/!;
    $f =~ s!^(lib|standard)/!$1/*/!;
    $f =~ s!^vendor/([^/]+)/([^/]+)/!vendor/$1/$2/*/!;
    $f = "//depot/hw/layout/tsmc13/spec/" . $f;
    return $f;
}

#
# Given a user's cast-dir and spec-dir, takes a user-space file
# and maps it to a branch-independent depot location
#
sub user_file_to_unqualified_depot_file {
    my ($f,$cdir,$sdir) = @_;
    my $df = "";
    if ($f =~ /^$cdir\/(.*)\.cast$/) {
        $df = module_to_unqualified_depot_file($1);
    }
    elsif ($f =~ /^$sdir\/(.*)\.cast$/) {
        $df = subtype_to_unqualified_depot_file($1);
    }
    else {
        print STDERR "Warning: Unknown file $f.\n";
        $df = $f;
    }
    return $df;
}

#
# Evaluates a branch-independent depot file for a particular branch
#
sub qualify_depot_file {
    my ($f,$b) = @_;
    $f =~ s/\*/$b/;
    return $f;
}

#
# Maps a branch-unqualified depot file to a user-space file, given
# cast & spec root directories.  Flattens out all branch points.
# 
sub unqualified_depot_to_user_file {
    my ($f,$cdir,$sdir) = @_;
    $f =~ s/\*\///;
    if ($f =~ m|^//depot/hw/cast|) {
        $f =~ s|^//depot/hw/cast|$cdir|;
    }
    elsif ($f =~ m|^//depot/hw/layout/$PROCESS/spec|) {
        $f =~ s|^//depot/hw/layout/$PROCESS/spec|$sdir|;
    }
    else {
        warn __PACKAGE__ . ": Bad depot cast file:\n  $f.\n";
    }
    return $f;
}


#
# Determines the head revision number of a specified unqualified
# depot file, for a given branch
#
sub head_revision {
    my ($f,$b) = @_;
    my $depot_file = qualify_depot_file($f,$b);
    my $rev = -1;
    open (P4, "p4 fstat \"$depot_file\" |") || die "Couldn't run p4.\n";
    while (<P4>) {
        if (/\.\.\. headRev (\d+)$/) {
            $rev = $1;
        }
    }
    close P4;
    die "ERROR: No head revision for file $depot_file.\n" if ($rev == -1);
    return $rev;
}

#
# Determines the filelog of the specified depot file (in the given
# source branch) since the file's last integration to or from the 
# specified destination branch
#
sub filelog_since_last_integration {
    my ($f,$src,$dest) = @_;
    my $sf = qualify_depot_file($f,$src);
    my $df = qualify_depot_file($f,$dest);
    my $num = 0;
    my $branched = 0;
    my @changes = ();
    $df =~ s/\(/\\\(/g; $df =~ s/\)/\\\)/g;
    $df =~ s/\{/\\\{/g; $df =~ s/\}/\\\}/g;
    open (P4, "p4 filelog -l \"$sf\" |") || die "Couldn't run p4.\n";
    while (<P4>) {
        push @changes, $_;
        if (/^\.\.\. \#/) { $num++; }
        if (/^\.\.\. \.\.\. (branch|copy|merge) (into|from) $df\#/ &&
            ($1 ne "merge" || $2 ne "from")) {
            $branched = 1;
            last;
        }
    }
    if ($num == 1 && $branched == 1) {
        @changes = ();
    }
    return @changes;
}


#
# Determines all cast file dependencies of the specified cell (either 
# hw/cast base cell or hw/layout/<process> layout subtype).  Needs
# a user-space cast directory (e.g. "~/hw/cast") and layout subtype
# spec directory (e.g. "~/hw/layout/tsmc13/spec"), and a max_heap_size
# for the Java JVM.
#
# If there is a parse error, the jflat error message will be printed to
# standard error.  If the jflat command can't be run, the routine dies.
# Note: 'jflat' must be in the user's command path.
#
# When 'depot_files' is set to 1, returns a list of branch-independent 
# depot files.  If 'depot_files' is set to 0, returns a list of user-space
# files.
#
sub cell_dependencies {
    my ($cell,$cdir,$sdir,$max_heap_size,$depot_files) = @_;
    my @cast_files;
    my $cmd = "$JFLAT_PATH ";
    if ($max_heap_size ne "") {
        $cmd .= " --max-heap-size=$max_heap_size \\\n";
    }
    $cmd .= "--strace-regex='.*\.cast' \\\n";
    $cmd .= "--strace-output=/scratch/cast_integrate.$$ \\\n";
    $cmd .= "--cast-path=$cdir:$sdir \\\n";
    $cmd .= "--tool=check \\\n";
    $cmd .= "\"--cell=$cell\"";
    open (JFLAT, "$cmd |") || die "Couldn't run $cmd.\n";
    my @jflat_output = <JFLAT>;
    if ($jflat_output[$#jflat_output] !~ /Checked.$/) {
        print STDERR "Cast::SourceControl::cell_dependencies: Jflat failed.\n\n";
        print STDERR @jflat_output;
        unlink "/scratch/cast_integrate.$$";
        exit;
    }
    open (FILES, "/scratch/cast_integrate.$$") ||
        die "Couldn't read /scratch/cast_integrate.$$.\n";
    while (<FILES>) {
        chomp;
        next if ($_ !~ /\.cast$/);
        if ($depot_files) {
            push @cast_files, user_file_to_unqualified_depot_file($_,$cdir,$sdir);
        }
        else {
            push @cast_files, $_;
        }
    }
    close FILES;
    unlink "/scratch/cast_integrate.$$";
    return @cast_files;
}

1;
