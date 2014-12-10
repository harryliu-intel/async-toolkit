# vim:et:sw=4:ts=4:tw=79:

###############################################################################
# File:             util.pm
# Author:           Mark Hesselink
# Creation Date:    September 30, 2008
# Description:      Utility subroutines for the Alta regression system
#
# INTEL TOP SECRET
# Copyright 2008 - 2012 Intel Corporation
# All Rights Reserved.
###############################################################################

package util;
use strict;
use warnings;

use base qw(Exporter);

use Fcntl qw(:flock);
require File::Copy;
use File::Basename;
require File::Path;
require File::Temp;
#use File::NFSLock;
use File::stat;
use IO::Handle;
use IO::Select;
use IPC::Open3;
use List::Util qw(max sum);
use POSIX qw(ceil);
use Cwd qw(realpath abs_path);

use verif_util;

our @EXPORT = qw(
    TRUE
    FALSE

    CMD_BUILD
    CMD_RETRY
    CMD_FAIL

    JOB_STATUS_FAILURE
    JOB_STATUS_SUCCESS
    JOB_STATUS_REQUEUE
    JOB_STATUS_MODEL_BUILDING

    JOB_OPTIONS_NONE

    P5_TYPE_UNKNOWN
    P5_TYPE_ARRAY
    P5_TYPE_CODE
    P5_TYPE_HASH
    P5_TYPE_SCALAR

    HLINE

    UMASK
    DMODE
    FMODE

    copy
    cpdup
    fail_test
    get_alt_root
    get_memory_size
    get_module
    get_version
    lock_file
    mkpath
    m_cpath
    m_open3
    rmtree
    tempdir
    type_of
);

###############################################################################
# Constants
###############################################################################

use constant TRUE                       => 1;
use constant FALSE                      => 0;

use constant CMD_BUILD                  => 'M_BUILD';
use constant CMD_RETRY                  => 'M_RETRY';
use constant CMD_FAIL                   => 'M_FAIL';

use constant JOB_STATUS_FAILURE         => -1;
use constant JOB_STATUS_SUCCESS         => 0;
use constant JOB_STATUS_REQUEUE         => 1;
use constant JOB_STATUS_MODEL_BUILDING  => 2;

use constant JOB_OPTIONS_NONE           => '';

use constant P5_TYPE_UNKNOWN            => 0;
use constant P5_TYPE_ARRAY              => 1;
use constant P5_TYPE_CODE               => 2;
use constant P5_TYPE_HASH               => 3;
use constant P5_TYPE_SCALAR             => 4;

use constant HLINE                      => "-" x 50;

###############################################################################
## UMASK
#
# @desc             The file mode creation mask to use when creating files or
#                   directories.
#
###############################################################################
# For security, models/regression logs/etc are stored in /p/$project/
# which is restricted by group access to members of the projects group.  We
# then need to allow chipdev to cleanup the models/etc in these directories,
# requiring us to grant group write access for the project group.  Others
# should strictly be forbidden any sort of access.
use constant UMASK                      => 007;

use constant DMODE                      => 02777 ^ UMASK;
use constant FMODE                      => 00666 ^ UMASK;

###############################################################################
# Private Variables
###############################################################################

my %type_map =
(
    'ARRAY'     => P5_TYPE_ARRAY,
    'CODE'      => P5_TYPE_CODE,
    'HASH'      => P5_TYPE_HASH,
    ''          => P5_TYPE_SCALAR,
    'SCALAR'    => P5_TYPE_SCALAR,
);

###############################################################################
# Public Functions
###############################################################################

###############################################################################
## @method public int copy(char *src, char *dst)
#
# @desc             Copies the contents of the file @a src to the file @a dst.
#
# @param[in]        src is the file to copy from.
#
# @param[in]        dst is the file to copy to.
#
# @return           0 if successful.
# @return           1 otherwise.
#
###############################################################################
sub copy
{
    my ($src, $dst) = @_;

    my $subroutine = get_subroutine();

    # Retrieve the current file mode creation mask.
    my $umask = umask();
    # Temporarily change the file mode creation mask.
    umask(UMASK);
    debug_print(10, "$subroutine: copying $src to $dst\n");
    my $status = File::Copy::copy($src, $dst);
    # Reset the file mode creation mask.
    umask($umask);

    return $status == 1 ? 0 : 1;

}   # end copy;


###############################################################################
## @method public int cpdup(IO::Handle &handle,
#                           char *s_directory,
#                           char *d_directory)
#
# @desc             Copies a set of files from @a s_directory to @a
#                   d_directory.
#
# @param[in]        handle points to a Perl IO::Handle object or a Perl object
#                   that is derived from IO::Handle. @a handle is read until an
#                   end-of-file condition is reached to retrieve the set of
#                   files to copy from @a s_directory to @a d_directory.
#
# @param[in]        s_directory is the directory to copy the set of files from.
#
# @param[in]        d_directory is the directory to copy the set of files to.
#
# @return           0 if successful.
# @return           1 otherwise.
#
###############################################################################
sub cpdup
{
    my ($handle, $s_directory, $d_directory) = @_;

    my $subroutine = get_subroutine();

    debug_print(7, "$subroutine: s_directory=$s_directory\n".
                   "$subroutine: d_directory=$d_directory\n");

    my $bytes = 0;
    while (my $src = <$handle>)
    {
        chomp($src);

        #debug_print(10, "$subroutine: src=$src\n");

        # Remove any quotes that were added to prevent shell expansion of the
        # filename.
        $src =~ s,^'(.*)'$,$1,;
        my $dst = $src;

        # If s_directory is a logical path and the ls command produces a
        # physical path, the search/replace would fail.  Unfortunately,
        # sometimes $handle contains physical paths and sometimes it contains
        # logical paths.  Lets try to determine which.
      
        # NOTE:
        # The following realpath() and abs_path() rely on our custom Cwd.pm
        # non-standard functionality.  This will not work with the standard
        # Cwd.pm module.
        my $s_dir;
        my $src_physical = realpath($src);
        if ($src eq $src_physical)  # src is the physical path
        {
            # use the physical path
            $s_dir = realpath($s_directory);    # physical path
        }
        else                        # src must be the logical path
        {
            # use the logical path
            $s_dir = abs_path($s_directory);    # logical path
        }

        #debug_print(10, "$subroutine: s_dir=$s_dir\n");

        $dst =~ s,\Q$s_dir\E,$d_directory,;

        #debug_print(10, "$subroutine: dst=$dst\n");

        my $directory = dirname($dst);

        #debug_print(10, "$subroutine: directory=$directory\n");

        if (!-d($directory))
        {
            if (mkpath($directory) != 0)
            {
                return 1;
            }

        }   # end if (!-d($directory))

        if (copy($src, $dst) != 0)
        {
            return 1;
        }
        my $st = stat($dst);
        $bytes += $st->size;

    }   # end while (my $line = <$handle>)

    debug_printf(7,
                 "$subroutine: Copied $bytes bytes (%6.2f MB)\n",
                 $bytes / 1024**2);

    return 0;

}   # end cpdup


###############################################################################
## @method public int fail_test(HASH &env, char *cmd)
#
# @desc             Generates a test failure and executes the specified command
#                   to notify the user of the test failure.
#
# @param[in]        env points to a Perl HASH containing the following fields:
#                   <ul>
#                       <li> @c results : points to a Perl HASH containing the
#                           set of test results.
#                       <li> @c ts : points to a Perl HASH containing the
#                           following fields:
#                           <ul>
#                               <li> @c test : the name of the test
#                               <li> @c output : the file to send the failure
#                                   message(s) to.
#                               <li> @c start : the file to create before
#                                   executing the sequence of shell commands
#                                   pointed to by @a cmd.
#                               <li> @c end : the file to create upon
#                                   completion of the test failure procedure.
#                               <li> @c kill : the file to create to kill the
#                                   test failure procedure.
#                           </ul>
#                       <li> @c output_dir : the path to the test output
#                           directory.
#                       <li> @c cfg : points to a Perl HASH containing the set
#                           of global configuration options.
#                   </ul>
#
# @param[in]        cmd is the sequence of shell commands to execute to notify
#                   the user of the test failure
#
# @return           JOB_STATUS_FAILURE
#
###############################################################################
sub fail_test($$)
{
    my ($env, $cmd) = @_;

    my $alt_root    = get_alt_root($env->{'cfg'});
    my $end_file    = $env->{'ts'}->{'end'};
    my $kill_file   = $env->{'ts'}->{'kill'};
    my $output_dir  = $env->{'output_dir'};
    my $output_file = $env->{'ts'}->{'output'};
    my $start_file  = $env->{'ts'}->{'start'};
    my $test        = $env->{'ts'}->{'test'};

    local *job_start = ($env->{'cfg'}->{'regress.job_method'} . '::job_start');

    my $job_id = job_start($cmd,
                           $output_dir,
                           $start_file,
                           $output_file,
                           $end_file,
                           JOB_OPTIONS_NONE,
                           $test);

    set_result_property($env->{'results'}, $test, 'output_file', $output_file);
    set_result_property($env->{'results'}, $test, 'start_file', $start_file);
    $output_file =~ s/$alt_root//;
    set_result_property($env->{'results'},
                        $test,
                        'relative_output_file',
                        $output_file);
    $output_dir =~ s/$alt_root//;
    set_result_property($env->{'results'},
                        $test,
                        'relative_output_dir',
                        $output_dir);
    set_result_property($env->{'results'}, $test, 'end_file', $end_file);
    set_result_property($env->{'results'}, $test, 'kill_file', $kill_file);
    set_result_property($env->{'results'}, $test, 'job_id', $job_id);

    return JOB_STATUS_FAILURE;

}   # end fail_test


###############################################################################
## @method public char* get_alt_root(HASH &cfg)
#
# @desc             Retrieves the alternative regression root.
#
# @param[in]        cfg points to a Perl HASH containing the set of global
#                   configuration options.
#
# @return           the alternative regression root if the 'regress.alt_root'
#                   option has been specified, the regression root otherwise
#
###############################################################################
sub get_alt_root($)
{
    my ($cfg) = @_;

    # At this point, regress.alt_root will already contain the appropriate 
    # load balancing disk in it's path.

    my $alt_root = $cfg->{"regress.alt_root"};
    if (!defined($alt_root) || length($alt_root) == 0)
    {
        $alt_root = $cfg->{"regress.root"};
    }
    return $alt_root;

}   # end get_alt_root


###############################################################################
## @method private int get_memory_size(char *string)
#
# @desc             Converts the specified memory size specification into units
#                   of 1024*1024 bytes.
#
# @param[in]        string points to memory size specification that is to be
#                   converted into units of 1024*1024 bytes.
#
# @return           the memory size in units of 1024*1024 bytes.
#
###############################################################################
sub get_memory_size
{
    my ($string) = @_;

    SWITCH: for (trim($string))
    {
        # memory size specified in units of 1000 bytes.
        m/^([[:digit:].]+)k$/ && return ceil($1 * (1000.0 / 1024.0**2));

        # memory size specified in units of 1024 bytes.
        m/^([[:digit:].]+)K$/ && return ceil($1 * (1024.0 / 1024.0**2));

        # memory size specified in units of 1000*1000 bytes.
        m/^([[:digit:].]+)m$/ && return ceil($1 * (1000.0**2 / 1024.0**2));

        # memory size specified in units of 1024*1024 bytes.
        m/^([[:digit:].]+)M$/ && return ceil($1);

        # memory size specified in units of 1000*1000*1000 bytes.
        m/^([[:digit:].]+)g$/ && return ceil($1 * (1000.0**3 / 1024.0**2));

        # memory size specified in units of 1024*1024*1024 bytes.
        m/^([[:digit:].]+)G$/ && return ceil($1 * 1024.0);

        # memory size specified in bytes.
        m/^([[:digit:]]+)$/   && return $1;

        die("unreachable");
    }

}   # end get_memory_size


###############################################################################
## @method public char* get_module(char *package)
#
# @desc             Retrieves the absolute module path for the specified Perl
#                   package.
#
# @param[in]        package @optional is the package to retrieve the absolute
#                   module path for. If @a package is not provided, the module
#                   path of the caller's package is returned.
#
# @return           the absolute module path of @a package.
#
###############################################################################
sub get_module
{
    my ($package) = (@_, (caller(1))[0]);
    $package =~ s,::,/,g;
    return $INC{"$package.pm"};

}   # end get_module


###############################################################################
## @method public char* get_version(char *tool, char *version)
#
# @desc             Converts the @c --latest and @c --release versions for the
#                   specified tool into a version of the form
#                   @c --before=CHANGE. Versions other than @c --latest and
#                   @c --release are passed through unmodified.
#
# @param[in]        tool is the tool to convert @a version for.
#
# @param[in]        version is the version specification to convert.
#
# @return           the converted version specification if successful.
# @return           @c undef otherwise.
# 
###############################################################################
sub get_version
{
    my ($tool, $version) = @_;

    if ($version =~ /--(latest|release)/)
    {
        # Convert the --latest or --release version specifier into a version
        # specifier of the form --before=<change number>.
        my $cmd = "fulcrum --list $version $tool 2>/dev/null";

        my $handle = IO::Handle->new();
        if (!defined(open($handle, "-|", $cmd)))
        {
            return undef;
        }
        my @contents = <$handle>;
        $handle->close();
        my @change = grep {m/^--change/} @contents;
        map {s/^--change\s+([0-9]+).*/$1/} @change;
        return ($#change >= 0 ? sprintf("--before=%d", max(@change)) : undef);
    }
    return $version;

}   # end get_version


###############################################################################
## @method public File::NFSLock& lock_file(char *filename,
#                                          int   lock_timeout,
#                                          int   stale_timeout)
#
# @desc
#
# @param[in]        filename
#
# @param[in]        lock_timeout
#
# @param[in]        stale_timeout
#
###############################################################################
sub lock_file
{
    my ($filename, $lock_timeout, $stale_timeout) = @_;

    my $subroutine = get_subroutine();
    my ($lock);

    eval
    {
        local $SIG{'ALRM'} = sub { die("alarm\n") };
        # Schedule an alarm for $lock_timeout seconds in the future to force
        # the regression system to give up trying to obtain a lock on the
        # file.
        alarm($lock_timeout);
        while (TRUE)
        {
            $lock = File::NFSLock->new($filename,
                                       LOCK_EX | LOCK_NB,
                                       1,
                                       $stale_timeout);
            last if ($lock);
            debug_print(5, "$subroutine: Waiting for lock on $filename\n");
            sleep(10);
        }
        # Cancel the previously scheduled alarm.
        alarm(0);
    };
    if (!$lock)
    {
        die("ERROR: Could not lock $filename [$File::NFSLock::errstr]");
    }
    return $lock;


}   # end lock_file


###############################################################################
## @method public int mkpath(char *directory)
#
# @desc             Creates the specified directory. Parent directories are
#                   created as needed.
#
# @param[in]        directory is the path of the directory to be created.
#
# @return           0 if successful.
# @return           1 otherwise.
#
###############################################################################
sub mkpath
{
    my ($directory) = @_;

    # Retrieve the current file mode creation mask.
    my $umask = umask();
    # Temporarily change the file mode creation mask.
    umask(UMASK);
    eval { File::Path::mkpath($directory) };
    my $error = $@;
    # Reset the file mode creation mask.
    umask($umask);

    return $error ? 1 : 0;

}   # end mkpath


###############################################################################
## @method public bool m_cpath(char *path, char **error)
#
# @desc             Checks whether all members of a set of directories exist.
#
# @param[in]        path is the colon separated list of directories to be
#                   checked for existence.
#
# @param[in]        error points to a Perl SCALAR in which this function stores
#                   the comma separated list of non-existent directories.
#
# @return           TRUE if all members exist.
# @return           FALSE otherwise.
#
###############################################################################
sub m_cpath
{
    my ($path, $error) = @_;

    my @e = ();
    if (defined($path)
        && ($path =~ m/^[\s]*$/
            || !sum(map {-d($_) ? 0 : (push(@e, $_), 1)} split(/:/, $path))))
    {
        return TRUE;
    }
    ${$error} = defined($path) ? join(',', @e) : m_2str($path);
    return FALSE;

}   # end m_cpath


###############################################################################
## @method public void m_open3(char *command)
#
# @desc             Executes the specified command and captures all output
#                   printed on both stdout and stderr.
#
# @param[in]        command is the command to be executed.
#
# @throws
#
###############################################################################
sub m_open3
{
    local (*CHLD_IN, *CHLD_OUT);
    my $pid = open3(\*CHLD_IN, \*CHLD_OUT, \*CHLD_OUT, shift(@_));
    # To prevent Perl from hanging due to buffer exhaustion caused by the
    # command executed above, the CHLD_OUT file handle will be polled for new
    # data in a blocking fashion until the command has completed.
    my $output = '';
    my $s = IO::Select->new(\*CHLD_OUT);
    WAIT: while (TRUE)
    {
        my @ready = $s->can_read();
        foreach my $handle (@ready)
        {
            last WAIT if eof($handle);
            while (read($handle, $output, 1024, length($output)))
            {
                debug_print(9, $output);
            }
        }
    }
    waitpid($pid, 0);
    if ($?)
    {
        $output =~ s/(^|\n)/$1   /g;
        die($output);
    }

}   # end m_open3


sub rmtree
{
    my ($directory) = @_;

    eval { File::Path::rmtree($directory) };
    return $@ ? 1 : 0;

}   # end rmtree


sub tempdir
{
    my ($template, $dir, $tmpdir) = @_;

    # Create the base directory if necessary.
    if (!-d($dir))
    {
        if (mkpath($dir) != 0)
        {
            return 1;
        }
    }
    # Retrieve the current file mode creation mask.
    my $umask = umask();
    # Temporarily change the file mode creation mask.
    umask(UMASK);
    eval { ${$tmpdir} = File::Temp::tempdir($template, DIR => $dir) };
    my $error = $@;
    # Reset the file mode creation mask.
    umask($umask);

    return $error ? 1 : 0;

}   # end tempdir


###############################################################################
## @method public int type_of(SCALAR object)
#
# @desc             Determines the type of the specified Perl SCALAR.
#
# @param[in]        object is the Perl SCALAR whose type is to be determined.
#
# @return           the type code. Valid values are:
#                   <ul>
#                       <li> @c P5_TYPE_ARRAY: @a object is a Perl ARRAY
#                           reference.
#                       <li> @c P5_TYPE_CODE: @a object is a Perl CODE
#                           reference.
#                       <li> @c P5_TYPE_HASH: @a object is a Perl HASH
#                           reference.
#                       <li> @c P5_TYPE_SCALAR: @a object is a Perl SCALAR
#                           value.
#                       <li> @c P5_TYPE_UNKNOWN
#                   </ul>
#
###############################################################################
sub type_of
{
    return $type_map{ref($_[0])} || P5_TYPE_UNKNOWN;

}   # end type_of

1;
