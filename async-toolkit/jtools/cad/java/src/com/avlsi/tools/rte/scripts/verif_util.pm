# vim:et:sw=4:ts=4:tw=79:

###############################################################################
# File:             verif_util.pm
# Author:           Naru Sundar
# Owner:            Mark Hesselink 
# Creation Date:    December 11, 2003
# Description:      Utilities used by the regression/verification scripts
#
# INTEL TOP SECRET
# Copyright 2003 - 2012 Intel Corporation
# All Rights Reserved.
###############################################################################

package verif_util;
use strict;
use warnings;

use base qw(Exporter);

use Cwd qw(getcwd);
use File::Temp;
use IO::File;
use IO::Handle;

our @EXPORT = 
qw(
    EMPTY_STRING

    debug_get_level
    debug_lprint
    debug_lprintf
    debug_print
    debug_printf
    debug_set_level
    extract_test_name
    find_platform
    get_cv
    get_disk
    get_result_property
    get_subroutine
    get_unique_string
    m_2str
    parse_config_file
    parse_suite_file
    parse_testplan_file
    quote_cmdline
    set_result_property
    sh_quote
    trim
    touch
    unescape
    uniq
    xslt_command
    validate_tool_versions
);

our @EXPORT_FAIL =
qw(
    do_parse_suite_file
);

# Enable output autoflush.
$| = 1;

###############################################################################
# Constants
###############################################################################

use constant EMPTY_STRING               => '';

###############################################################################
# Local Variables
###############################################################################

my $DEBUG_LEVEL = 5;

my $java = '/usr/local/java/bin/java';
$java = 'java' if (!-e($java));

###############################################################################
# Local Functions
###############################################################################

###############################################################################
## @method private void do_parse_suite_file(ARRAY &testcases,
#                                           char * filespec,
#                                           char * root,
#                                           char * platform,
#                                           char * extra,
#                                           char * testplan)
#
# @desc             Parses a Fulcrum suite file. The suite file parser supports
#                   the following set of features:
#                   <ul>
#                       <li>Test options are allowed to follow a test, i.e.
#                       test specifications of the form
#                       <tt>foo.testplan:Bar --variant=baz</tt> are
#                       accepted.</li>
#                       <li>Any text following a hash (#) is treated as a
#                       comment.</li>
#                       <li>The \<newline> sequence is treated as a line
#                       continuation. Whitespace and comments in between \ and
#                       <newline> are accepted.</li>
#                       <li>Empty lines or lines containing only whitespace are
#                       ignored.</li>
#                   </ul>
#
# @param[in,out]    testcases points to a Perl ARRAY in which this function
#                   stores all test cases found in the suite file.
#
# @param[in]        filespec is the PerlIO file specification.
#
# @param[in]        root is the path to the root of the regression tree.
#
# @param[in]        platform is the regression platform.
#
# @param[in]        extra @optional is the set of arguments to add to each test
#                   case.
#
# @param[in]        testplan @optional is the testplan file to be parsed.
#
###############################################################################
sub do_parse_suite_file
{
    my ($testcases, $filespec, $root, $platform, $extra, $testplan) = @_;

    $extra         = defined($extra) ? $extra : EMPTY_STRING;
    my $subroutine = get_subroutine();

    my @contents = ();

    debug_print(10, "** $subroutine($filespec, $root, $platform, $extra)\n");

    # Read the suite file in one go to allow this function to be called
    # recursively.
    my $fh = IO::File->new($filespec);
    if (!defined($fh))
    {
        debug_print(4, "$subroutine: Cannot open `$filespec'\n");
        return;
    }
    my $entry = EMPTY_STRING;
    while (my $line = <$fh>)
    {
        # Remove the newline character.
        chomp($line);
        # Remove comments. 
        $line =~ s/^([^#]*)#.*/$1/o;
        # Remove leading and trailing whitespace and append the result to
        # the current entry.
        $entry .= trim($line);
        if ($entry =~ /\\$/o)
        {
            # This input line is part of a multi-line entry. Process the
            # next line of this entry.
            $entry =~ s/\\$//o;
            $entry .= " ";
            next;
        }
        if (length($entry) > 0)
        {
            push(@contents, $entry);
            $entry = EMPTY_STRING;
        }
    }
    $fh->close();

    # Iterate over all lines.
    foreach $_ (@contents)
    {
        if (m/::(\S+)\s*(.*)/)
        {
            # ::Test indicates within same testplan
            my $testcase = { name => "$testplan:$1", opts => "$2 $extra" };
            push(@{$testcases}, $testcase);

            debug_printf(10,
                         "**\tadded %s with options %s\n",
                         $testcase->{'name'},
                         $testcase->{'opts'});
        }
        elsif (m/^([^:]*):(\S+)\s*(.*)/)
        {
            # :Test indicates default testplan.
            my $filename = "$root/$platform/tests/$1";

            if (-e($filename))
            {
                my $testcase = { name => "$filename:$2", opts => "$3 $extra" };
                push(@{$testcases}, $testcase);

                debug_printf(10,
                             "**\tadded %s with options %s\n",
                             $testcase->{'name'},
                             $testcase->{'opts'});
            }
            else
            {
                debug_print(0, "ERROR: $filename: No such file\n");
            }
        }
        elsif (m/((.*)\.suite)\s*(.*)/)
        {
            # Recursively parse suite files.
            parse_suite_file($testcases,
                             "$root/$platform/tests/$1",
                             $root,
                             $platform,
                             "$3 $extra");
        }
        elsif (m/((.*)\.testplan)\s*(.*)/)
        {
            # Recursively parse testplan files.
            parse_testplan_file($testcases,
                                "$root/$platform/$1",
                                $root,
                                $platform,
                                "$3 $extra");
        }
        elsif (m/((.*)\.test)\s*(.*)/)
        {
            my $filename = "$root/$platform/tests/$1";
            if (-e($filename))
            {
                my $testcase = { name => $filename, opts => "$3 $extra" };
                push(@{$testcases}, $testcase);

                debug_printf(10,
                             "**\tadded %s with options %s\n",
                             $testcase->{'name'},
                             $testcase->{'opts'});
            }
            else
            {
                debug_print(0, "ERROR: $filename: No such file\n");
            }
        }
        elsif (m/--job=(\S+)\s*(.*)/)
        {
            my $testcase = { name => "job=$1", opts => "$2 $extra" };
            push(@{$testcases}, $testcase);

            debug_printf(10,
                         "**\tadded job %s with options %s\n",
                         $testcase->{'name'},
                         $testcase->{'opts'});
        }

    }   # end foreach $_ (@contents)

}   # end do_parse_suite_file

###############################################################################
## @method private HASH do_parse_itools_file(char* itoolsFile)
#
# @desc             Parses the project specific itools file and returns a 
#                   hash of its contents.  If the file isn't found or the 
#                   itools file specifies two different versions for the
#                   same tool, this function will exit.
#
# @param            itoolsFile is the full path to the itools file
#
# @return           A hash containing the itools file contents
###############################################################################
sub do_parse_itools_file
{
    my ($itoolsFile) = @_;

    my $subroutine = get_subroutine();
    
    my %itools = (
        # <tool>    => "<version>",
        # ...
    );

    debug_print(10, "$subroutine($itoolsFile)\n");

    # It's okay to exit because this is for a version check before the 
    # regression system does much
    if (! -f "$itoolsFile")
    {
        debug_print(0, "ERROR: $subroutine: $itoolsFile: File not found\n");
        exit -1;
    }

    open(ITOOLS_FILE, "< $itoolsFile");
    while (my $line = <ITOOLS_FILE>)
    {
        chomp($line);   # strip \n

        # Skip comments and blank lines
        next if ( ($line =~ /^#/) || ($line =~ /^\s*$/) );

        if ($line =~ /^\s*P:(.*?)\s+(.*)$/)     # P:tool   <version>
        {
            my $tool    = $1;
            my $version = $2;

            if (defined $itools{$tool})
            {
                debug_print(0, "$subroutine: ERROR: $itoolsFile: Two ".
                    "different versions specified for $tool\n");
                exit -1;
            }

            $itools{$tool} = "$version";
        }
        elsif ($line =~ /^\s*(.*?)\s+(.*)$/)    # tool      </path/to/bin>
        {
            # Attempt to extract a version number from the path
            my $tool    = $1;
            my $path    = $2;
            
            if (defined $itools{$tool})
            {
                debug_print(0, "$subroutine: ERROR: $itoolsFile: Two ".
                    "different versions specified for $tool\n");
                exit -1;
            }

            # The path is going to contain the version number because itools
            # configuration files only specify tools supported by itools,
            # which will always contain the version number in the path.
            my $pkgPath = '/usr/intel/pkgs';
            if ($path =~ /^$pkgPath\/$tool\/(.*?)\//)
            {
                my $version = $1;
                $itools{$tool} = "$version";
            }
            else
            {
                debug_print(0, "$subroutine: ERROR: Failed to parse path ".
                    "for tool $tool: $path\n");
                exit -1;
            }
        }
        else
        {
            debug_print(6, "$subroutine: Skipping unhandled line: $line\n");
        }
    }
    close(ITOOLS_FILE);
    
    return %itools;
}   # end do_parse_itools_file


###############################################################################
# Public Functions
###############################################################################

###############################################################################
## @method public int debug_get_level(void)
#
# @desc             Retrieves the current debug level.
#
# @return           the current debug level.
#
###############################################################################
sub debug_get_level
{
    return $DEBUG_LEVEL;

}   # end debug_get_level


sub debug_lprint
{
    my ($debug_level, $level, $string) = @_;

    if ($level <= $debug_level)
    {
        print($string);
    }

}   # end debug_lprint


sub debug_lprintf
{
    my $debug_level = shift(@_);
    my $level = shift(@_);

    if ($level <= $debug_level)
    {
        printf(@_);
    }

}   # end debug_lprintf


###############################################################################
## @method public void debug_print(int level, char *message)
#
# @desc             Prints a debug message if the current debug level is less
#                   than or equal to @a level.
#
# @param[in]        level is the minimum debug level at which to print the
#                   message.
#
# @param[in]        message is the message to be printed.
#
###############################################################################
sub debug_print
{
    debug_lprint(debug_get_level(), @_);

}   # end debug_print


###############################################################################
## @method public void debug_printf(int level, char *format, ...)
#
# @desc             Prints a debug message if the current debug level is less
#                   than or equal to @a level.
#
# @param[in]        level is the minimum debug level at which to print the
#                   message.
#
# @param[in]        format is the printf(3) format string.
#
# @param[in]        ... is the variable-length argument list to be printed.
#
###############################################################################
sub debug_printf
{
    debug_lprintf(debug_get_level(), @_);

}   # end debug_printf


###############################################################################
## @method public void debug_set_level(int level)
#
# @desc             Sets the debug level.
#
# @param[in]        level is the new debug level.
#
###############################################################################
sub debug_set_level
{
    my $level = shift(@_);

    $DEBUG_LEVEL = $level;

}   # end debug_set_level


###############################################################################
## @method public char* extract_test_name(char *test_name)
#
# @desc             Extracts the test name from the specified, fully qualified
#                   test name.
#
# @param[in]        test_name is the fully qualified test name from which to
#                   extract the test name.
#
# @return           the test name.
#
###############################################################################
sub extract_test_name
{
    my ($test_name) = @_;

    if ($test_name =~ m/(.*):(.*)/)
    {
        $test_name = $2;
    }
    else
    {
        $test_name =~ s/((.*\/)*)(.*)\.test/$3/;
    }
    return $test_name;

}   # end extract_test_name

###############################################################################
## @method public char* find_platorm(char *root)
#
# @desc             Finds the platform based on current path
#
# @param[in]        root is the regression root
#
# @param[in]        dut is the relative path to a test (optional)
#
# @return           the platform name.
#
###############################################################################
sub find_platform {
    my ($root, $dut) = @_;
    if(!defined $dut) {$dut = ""}
    #my $pwd = `pwd`; chomp($pwd); 
    #my $pwd = $ENV{PWD};
    my $pwd = getcwd();
    my $full_path = "$pwd/$dut";
    my $compressed_root = `cd $root; pwd`; chomp($compressed_root);

    $full_path =~ s/.*$compressed_root\///; 

    my @terms = split("\/", $full_path);

    my $plat = $terms[0];

    for(my $i = 1; $i <= $#terms; $i++) { 
        if(-e "$root/$plat/config") {
            last; 
        } else {
            $plat = $plat . "/" . $terms[$i];
        }
    }

    if(! -e "$root/$plat/config") {
        $plat = "";
    }
    debug_printf(9, "root= $root\ncompressed_root= $compressed_root\npwd= $pwd\nfull path = $full_path\nplat= $plat\n");
    return $plat;

}   # end find_platform


###############################################################################
## @method public CODE& get_cv(char *package, char *subroutine)
#
# @desc             Retrieves the Perl CODE reference for the function @a
#                   subroutine that is defined in the Perl module @a package.
#
# @param[in]        package is the Perl module in which @a subroutine is
#                   defined.
#
# @param[in]        subroutine is the name of the Perl function for which to
#                   retrieve a Perl CODE reference.
#
# @return           the Perl CODE reference.
#
###############################################################################
sub get_cv
{
    my ($package, $subroutine) = @_;

    # Disable strict references within the scope of the get_cv subroutine.
    no strict 'refs';
    return \&{$package . '::' . $subroutine};

}   # end get_cv


###############################################################################
## @method public char* get_disk(char *file)
#
# @desc             Retrieves the disk on which @a file is stored.
#
# @param[in]        file is the absolute path of the file to retrieve the
#                   associated disk for.
#
# @return           the disk @a file is stored on.
#
###############################################################################
sub get_disk
{
    my ($file) = @_;

    my $pipe = IO::Handle->new();
    open($pipe, "df -P |");
    # Ignore the header.
    <$pipe>;
    my $match = '';
    while (my $line = <$pipe>)
    {
        chomp($line);
        my $disk = pop(@{[split(/\s+/, $line)]});
        # Perform a longest prefix match.
        if ($file =~ m/^(\Q$disk\E)/)
        {
            if (length($1) > length($match))
            {
                $match = $1;
            }
        }
    }
    return $match;

}   # end get_disk

###############################################################################
## @method public char* get_result_property(HASH &results,
#                                           char *test,
#                                           char *property)
#
# @desc             Retrieves a test property from a set of test results.
#
# @param[in]        results points to a Perl HASH containing the set of test
#                   results.
#
# @param[in]        test is the test to retrieve a property for.
#
# @param[in]        property is the property to be retrieved.
#
# @return           the requested property if successful.
# @return           @c undef otherwise.
#
###############################################################################
sub get_result_property
{
    my ($results, $test, $property) = @_;

    my @context = caller(1);

    debug_printf(9,
                 "** %s:%d: get_result_property(%s, %s)\n",
                 $context[3],
                 $context[2],
                 $test,
                 $property);

    return $results->{$test}->{$property};

}   # end get_result_property


###############################################################################
## @method public char* get_subroutine()
#
# @desc             Retrieves the name of the subroutine being executed.
#
# @return           the name of the subroutine being executed.
#
###############################################################################
sub get_subroutine
{
    return (caller(1))[3];

}   # end get_subroutine


###############################################################################
## @method public char* get_unique_string(void)
#
# @desc             Returns a unique string using tmpnam(3).
#
# @return           the unique string.
#
###############################################################################
sub get_unique_string()
{
    ####################################################
    # TODO: this is kinda hokey since we're not supposed
    #       to use tmpnam by itself (for race conditions)
    #       but I think we're ok
    ####################################################
    my $s = tmpnam();
    $s =~ s/.*\/(.*)/$1/;
    return $s; 

}   # end get_unique_string


###############################################################################
## @method public char* m_2str(SCALAR object)
#
# @desc             Stringifies the specified Perl SCALAR.
#
# @param[in]        object is the Perl SCALAR to be stringified.
#
# @return           the string representation of @a object.
#
###############################################################################
sub m_2str
{
    my ($object) = @_;

    if (!defined($object))
    {
        return 'undef';
    }
    return $object;

}   # end m_2str


###############################################################################
## @method public HASH parse_config_file(char * filename,
#                                        char * root,
#                                        char * platform,
#                                        char * full_cmd,
#                                        bool   inc_plat,
#                                        bool   inc_cmdline,
#                                        char **result,
#                                        bool   suppress_named_block_warning)
#
# @desc             Parses a XML configuration file.
#
# @param[in]        filename is the name of the XML file to parse.
#
# @param[in]        root is the path to the root of the regression tree.
#
# @param[in]        platform is the regression platform.
#
# @param[in]        full_cmd is the set of command line arguments with which
#                   the regression has been started.
#
# @param[in]        inc_plat is a boolean indicating whether the platform
#                   XML configuration file should be parsed.
#
# @param[in]        inc_cmdline is a boolean indicating whether the set of
#                   command line arguments specified by @a full_cmd should be
#                   passed to the XML parser.
#
# @param[out]       result points to a Perl SCALAR variable in which this
#                   function stores any parser generated (error) messages. @a
#                   result is initialized to the empty string by this function.
#
# @param[in]        suppress_named_block_warning is a boolean indicating
#                   whether the lack of a named XML block should generate a
#                   warning.
#
# @return           a Perl HASH containing the parsed configuration options.
#
###############################################################################
sub parse_config_file
{
    my ($filename,
        $root,
        $platform,
        $full_cmd, 
        $inc_plat,
        $inc_cmdline,
        $result,
        $suppress_named_block_warning) = @_;

    my $classpath = join(":", glob(sprintf("%s/shared/jars/*.jar", $root)));
    my @context   = caller(0);
    my $glob_opt  = "$root/config/regopts.xml";
    my $glob_defs = "$root/config/defaults.xml";
    my $main      = 'com.fulcrummicro.util.properties.PropReader';
    my %options   = ();
    my $plat_def  = "$root/$platform/config/defaults.xml";
    my $plat_opt  = "$root/$platform/config/opts.xml";

    debug_printf(9,
                 "** parse_config_file(%s, %s, %s, %s, %s)\n",
                 $filename,
                 $root,
                 $platform,
                 $inc_plat,
                 $inc_cmdline);

    # Initialize the Perl SCALAR value pointed to by $result to the empty
    # string.
    ${$result} = '';

    my $cmdline = "$java -classpath $classpath $main";
    my $configs = $glob_defs;
    if ($inc_plat)
    {
        $cmdline .= " $glob_opt $plat_opt --topdir=$root --platform=$platform";
        if (-e($plat_def) and ($plat_def ne $filename))
        {
            $configs .="," . $plat_def;
        }
    }
    else
    {
        $cmdline .= " $glob_opt --topdir=$root";
    }

    $filename =~ m/([^:]*)(:[^:]*)?/;
    if (-e($1))
    {
        $configs .= "," . $filename;
    }
    $cmdline .= " --config=$configs";

    if ($inc_cmdline)
    {
        $cmdline .= " $full_cmd";
    }

    debug_print(9, "**\tcmdline = $cmdline\n");

    debug_print(6, "** cmdline **\n");

    # Execute the XML parser and split the resulting string into separate
    # lines.
    my @contents = split(/\n/, `$cmdline`);

    foreach $_ (@contents)
    {
        if (m/([^=]*)=(.*)/)
        {
            my ($key, $value) = ($1, $2);

            $key = trim($key);
            $value = trim($value);

            # Check for quoted values.
            if ($value =~ m/^"([^"]*)"$/ || $value =~ m/^'([^']*)'$/)
            {
                debug_print(3, "WARNING: Quoted option: $key = $value\n");
            }

            $options{$key} = $value;

            debug_print(11, "*@*\tadded $key = $value\n");
        }
        else
        {
            my $error = $_;
            ${$result} .= $error . "\n";
            if ($suppress_named_block_warning
                && (   m/WARNING: no properties from block named /
                    || m/ERROR: .* does not exist in the XML defaults/))
            {
                debug_printf(7,
                             "%s: (suppressed) error: %s\n",
                             $context[3],
                             $error);
            }
            else
            {
                debug_printf(5, "%s: error: %s\n", $context[3], $error);
            }
        }

    }   # end foreach $_ (@contents)

    return %options;

}   # end parse_config_file

###############################################################################
## @method public void parse_suite_file(ARRAY &testcases,
#                                       char * filename,
#                                       char * root,
#                                       char * platform,
#                                       char * extra)
#
# @desc             Parses a suite file for all test cases that are to be run.
#
# @param[in]        testcases points to a Perl ARRAY in which this function
#                   stores all test cases found in the suite file.
#
# @param[in]        filename is the suite file to be parsed.
#
# @param[in]        root is the path to the root of the regression tree.
#
# @param[in]        platform is the regression platform.
#
# @param[in]        extra @optional is the set of arguments to add to each test
#                   case.
#
###############################################################################
sub parse_suite_file
{
    my ($testcases, $filename, $root, $platform, $extra) = @_;

    debug_printf(9,
                 "** parse_suite_file(%s, %s, %s, %s)\n",
                 $filename,
                 $root,
                 $platform,
                 $extra || EMPTY_STRING);

    if (!-e($filename))
    {
        debug_print(4, "ERROR: $filename: No such suite file\n");
        return;
    }

    do_parse_suite_file($testcases, '<' . $filename, $root, $platform, $extra);

}   # end parse_suite_file


###############################################################################
## @method public void parse_testplan_file(ARRAY &testcases,
#                                          char * filename,
#                                          char * root,
#                                          char * platform,
#                                          char * extra)
#
# @desc             Parses a testplan for all test cases that are to be run.
#
# @param[in]        testcases points to a Perl ARRAY in which this function
#                   stores all test cases found in the testplan.
#
# @param[in]        filename is the testplan to be parsed.
#
# @param[in]        root is the path to the root of the regression tree.
#
# @param[in]        platform is the regression platform.
#
# @param[in]        extra @optional is the set of arguments to add to each test
#                   case.
#
###############################################################################
sub parse_testplan_file
{
    my ($testcases, $filename, $root, $platform, $extra) = @_;

    debug_printf(9,
                 "** parse_testplan_file(%s, %s, %s, %s)\n",
                 $filename,
                 $root,
                 $platform,
                 $extra || EMPTY_STRING);

    my $xsl = sprintf("%s/doc/testplan/xsl/suite_completed.xsl", $root);

    if (!-e($filename))
    {
        debug_print(4, "ERROR: $filename: No such testplan file\n");
        return;
    }

    my $cmd = xslt_command($filename, $xsl, $root);

    do_parse_suite_file($testcases,
                        "$cmd |",
                        $root,
                        $platform,
                        $extra,
                        $filename);

}   # end parse_testplan_file


###############################################################################
## @method public char* quote_cmdline(char *cmdline)
#
# @desc             Modifies the supplied command line such that all user
#                   supplied arguments are properly quoted when passed to the
#                   bash(1) shell.
#
# @param[in]        cmdline points to the command line to be quoted.
#
# @return           the quoted command line.
#
###############################################################################
sub quote_cmdline
{
    my ($cmdline) = @_;

    my $re_arg = qr/(^|\s+)-[^\s]+(\s+|$)/;
    my $re_opt = qr/-{1,2}[^\s]+=/;
    if ($cmdline =~ m/(.*?$re_opt)(.*)/)
    {
        my ($option, $rhs) = ($1, trim($2));
        my $match = $rhs =~ m/(.*?)($re_arg|$re_opt)/;
        my $value = $match ? $1 : $rhs;
        $rhs =~ s/^\Q$value\E//g;
        my $nvalue = sh_quote(trim($value));
        if ($rhs =~ m/$re_opt/)
        {
            $cmdline = $option . $value . quote_cmdline(trim($rhs));
        }
        $cmdline =~ s/\Q$option$value\E/$option$nvalue /;
    }
    return $cmdline;

}   # end quote_cmdline


###############################################################################
## @method public void set_result_property(HASH &results,
#                                          char *test,
#                                          char *property,
#                                          char *value)
#
# @desc             Sets a test property in a set of test results.
#
# @param[in]        results points to a Perl HASH containing the set of test
#                   results.
#
# @param[in]        test is the test to set a property for.
#
# @param[in]        property is the property to be set.
#
# @param[in]        value is the value to set the property to.
#
###############################################################################
sub set_result_property
{
    my ($results, $test, $property, $value) = @_;

    my @context = caller(1);

    debug_printf(9,
                 "** %s:%d: set_result_property(%s, %s, %s)\n",
                 $context[3],
                 $context[2],
                 $test,
                 $property,
                 $value);

    $results->{$test}->{$property} = $value;

}   # end set_result_property


###############################################################################
## @method public char* sh_quote(char *s)
#
# @desc             Modifies the supplied string such that it is properly
#                   quoted when passed to the bash(1) shell.
#
# @param[in]        s points to the string to be quoted as specified by the
#                   ANSI-C standard.
#
# @return           the quoted string.
#
# @note             The quoted string should not be enclosed in double quotes
#                   (") when passed to bash(1). For example:
#
#                   @code
#                       my $s = ...
#                       my $cmd = sprintf("echo \"INFO: \"%s", sh_quote($s));
#                       #                               ^^^^
#                   @endcode
#
#                   quotes the string properly, while
#
#                   @code
#                       my $s = ...
#                       my $cmd = sprintf("echo \"INFO: %s\"", sh_quote($s));
#                       #                               ^^^^
#                   @endcode
#
#                   might result in a bash(1) execution error.
#
###############################################################################
sub sh_quote
{
    my ($s) = @_;

    my @sequenceMap =
    (
        ["\\", "\\\\"], # escape character
        ["\n", "\\n" ], # newline
        ["'",  "\\'" ], # single quote
    );
    # Apply all regular expressions to the supplied string.
    map {$s =~ s/\Q$_->[0]\E/$_->[1]/gs} @sequenceMap;
    # Quote the string as specified by the ANSI-C standard.
    # [see also Bash Reference Manual, Section 3.1.2.4]
    return "\$'$s'";

}   # end sh_quote


###############################################################################
## @method public char* trim(char *s)
#
# @desc             Trims leading and trailing whitespace from the string @a s.
#
# @param[in]        s is the string to trim leading and trailing whitespace
#                   from.
#
# @return           the trimmed string.
#
###############################################################################
sub trim
{
    my ($s) = @_;

    $s =~ s/^\s+//o;
    $s =~ s/\s+$//o;
    return $s;

}   # end trim


###############################################################################
## @method public void touch(char *filename, ...)
#
# @desc             Emulates the UNIX touch(1) command.
#
# @param[in]        filename is the name of the file to touch.
#
###############################################################################
sub touch
{
    foreach my $filename (@_)
    {
        if (-e($filename))
        {
            utime(undef, undef, $filename);
        }
        else
        {
            IO::File->new($filename, "w");
        }
    }

}   # end touch


###############################################################################
## @method public char* unescape(char *s)
#
# @desc             Decodes all sequences of the form \x{dd} and replaces the
#                   sequence by the ASCII character corresponding to the
#                   hexadecimal number \c dd. For example, \x{41} is replaced
#                   by "A" (capital A).
#
# @param[in]        s is the string to be decoded.
#
# @return           the decoded string.
#
###############################################################################
sub unescape
{
    my ($s) = @_;

    $s =~ s/\\x\{([[:xdigit:]]{2})\}/chr(hex($1))/eg;
    return $s;

}   # end unescape


###############################################################################
## @method public ARRAY& uniq(ARRAY &in)
#
# @desc             Removes all duplicates from the supplied Perl ARRAY.
#
# @param[in]        in is a (possibly unordered) Perl ARRAY from which all
#                   duplicates are to be removed.
#
# @return           a Perl ARRAY containing all unique entries of @a in.
#
###############################################################################
sub uniq
{
    my (@in) = @_;

    my (%saw);
    my @out = grep(!$saw{$_}++, @in);
    return @out;

}   # end uniq


###############################################################################
## @method public char* xslt_command(char *filename, char *xsl, char *root)
#
# @desc             Constructs the XSLT processor command to run on the
#                   specified file.
#
# @param[in]        filename is the XML file to be parsed.
#
# @param[in]        xsl is the XSL stylesheet.
#
# @param[in]        root is the path to the root of the regression tree.
#
# @return           The XLST processor command.
#
###############################################################################
sub xslt_command
{
    my ($filename, $xsl, $root) = @_;

    my $xalan_lib = "$root/scripts/xalan";
    return "$java -jar $xalan_lib/saxon9.jar -versionmsg:off -s:$filename -xsl:$xsl";

}   # end xslt_command


###############################################################################
## @method public void validate_tool_versions(char* itoolsFile, 
#                                             bool validateToolVersions)
#
# @desc             Validate the tool versions used to run the regression.
#                   Any tool specified as "latest" will be skipped.  
#
#                   The regression system will exit with an error if any of the
#                   following are true:
#                   - itools file is not found
#                   - itools file contains two different versions for a tool
#                   - itools file parsing fails
#                   - A tool needs to be special cased and isn't
#                   - A tool is the wrong version
#
# @param[in]        itoolsFile is the full path to the project specific itools
#                   file containing the tool versions that should be used.
#
# @param[in]        validateToolVersions is a boolean indicating whether to
#                   validate the tool versions or not.
###############################################################################
sub validate_tool_versions
{
    my ($itoolsFile, $validateToolVersions) = @_;
    my $subroutine = get_subroutine();

    debug_print(10, "** $subroutine($itoolsFile, $validateToolVersions)\n");

    if ($validateToolVersions eq "true")
    {
        my %itools = do_parse_itools_file($itoolsFile);

        foreach my $tool (keys %itools)
        {
            my $version = $itools{$tool};

            # Skip any tool that is marked "latest"
            if ($version eq "latest")
            {
                debug_print(7, "Skipping $tool: version latest specified\n");
                next;
            }

            my $which = `iwhich $tool 2>/dev/null`; chomp($which);
            my $pkgPath = '/usr/intel/pkgs';
            my $cVer;
            if ($which =~ /$pkgPath\/$tool\/(.*?)\//)
            {
                $cVer = $1;
            }
            elsif ($tool eq "pkgconfig")
            {
                $which = `pkg-config --version`; chomp($which);
                $cVer = $which;
            }
            elsif ($tool eq "findutils")
            {
                $which = `find --version | head -1`; chomp($which);
                if ($which =~ /GNU find version (.*)/)
                {
                    $cVer = $1;
                }
                else
                {
                    debug_print(0, "$subroutine: ERROR: Unable to determine ".
                        "version for $tool: $which\n");
                    exit -1;
                }
            }
            elsif ($tool eq "libxml2")
            {
                $which = `iwhich xmllint 2>/dev/null`; chomp($which);
                if ($which =~ /$pkgPath\/$tool\/(.*?)\//)
                {
                    $cVer = $1;
                }
                else
                {
                    debug_print(0, "$subroutine: ERROR: Unknown version for ".
                        "$tool: $which\n");
                    exit -1;
                }
            }
            else
            {
                # tools likely needs to be special cased
                debug_print(0, "$subroutine: ERROR: Unknown version for ".
                    "$tool: $which\n");
                exit -1;
            }
           
            debug_print(6, "Verifying $tool: required[$version] =? ".
                "current[$cVer] ... ");

            if ($version =~ /^$cVer(-64)?$/)
            {
                debug_print(6, "OK\n");
            }
            else
            {
                debug_print(6, "FAILED\n");
                debug_print(0, "$subroutine: ERROR: $tool is not the ".
                    "required version: required[$version] != ".
                    "current[$cVer]\n");
                exit -1;
            }
        } # foreach my $tool (keys %itools)
    }
    else
    {
        debug_print(4, "** $subroutine: Skipping tool version validation\n");
    }
}   # end validate_tool_versions

###############################################################################
## @method public char* select_load_balanced_disk(char* baseDir)
#
# @desc             Select the appropriate disk from a set of available disks
#                   based on disk utilization.  The disk that has the most free
#                   space will be selected.  The set of disks will be
#                   determined by the precense of symlinks at baseDir.  The
#                   full path to baseDir/selectedDisk will be returned.
#
# @param            basedir is the base directory, containing symbolic links 
#                   to the disks from which to select.  
#
# @return           The full path to the selected disk.  If the baseDir
#                   doesn't exist, "" is returned.
###############################################################################
sub select_load_balanced_disk
{
    my ($baseDir) = @_;

    my $subroutine = get_subroutine();
    debug_print(10, "$subroutine($baseDir)\n");

    if (! -d "$baseDir")
    {
        debug_print(0, "$subroutine: ERROR: Base directory does not exist: ".
            "$baseDir\n");
        return "";
    }

    my $disks = `find $baseDir -type l`;

    my %selected = (
        disk    => "",
        mbFree  => 0,
    );

    foreach my $disk (split(/\n/, $disks))
    {
        my $mbFree = `df --block-size=1M $disk | tail -1 | awk '{print \$3}'`;
        chomp($mbFree);
        
        debug_print(6, "$subroutine: disk[$disk] free[$mbFree MB]\n");

        if ($mbFree >= $selected{mbFree})
        {
            $selected{disk}     = $disk;
            $selected{mbFree}   = $mbFree;
        }
    }
    
    debug_print(6, "$subroutine: Selected disk: $selected{disk} ".
        "[$selected{mbFree} MB]\n");

    return "$selected{disk}";
}   # end select_load_balanced_disk

1;
