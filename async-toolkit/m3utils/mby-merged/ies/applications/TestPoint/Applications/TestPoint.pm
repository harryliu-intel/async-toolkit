# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint.pm
# Creation Date:    02/16/07
# Description:      Command-line interface for FM* systems.
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2012 Intel Corporation. All Rights Reserved. 
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or 
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
###############################################################################

package Applications::TestPoint;
use strict;
use warnings;

my $startTime;
BEGIN { $startTime = time(); }

require Exporter;
use Carp;
use File::Find;
use POSIX;
# Import all SDK variables to allow these symbols to be used when TestPoint is
# run in expert mode.
use SDK qw(/^\$/);

# Alternate library use
#  Term::ReadLine::Zoid will be used if $useAlternateLibrary is 0.
#  Otherwise, an alternate library will be used.
my $useAlternateLibrary;
my $alternateLibrary1;
my $alternateLibrary2;

BEGIN
{
    # Override the default readpipe and system subroutines with custom versions
    # that use vfork to avoid out-of-memory conditions when forking a new
    # process.
    *CORE::GLOBAL::readpipe = *SDK::Clone::readpipe;
    *CORE::GLOBAL::system = *SDK::Clone::system;
}

our @ISA = qw(
    Exporter
);
our @EXPORT_FAIL = qw(
    EvaluatePerl
    ExpandSymbol
    $GLOB_INST
);

use constant MODE_STANDARD      => 0;
use constant MODE_EXPERT        => 1;

use constant _NAMESPACE_        => '__TestPoint__::';

my $TestPoint_VERSION = "3.0";

our $__terminal;

# this is hacky
my $GLOB_INST   = undef;

# for autoload helper
our $AUTOLOAD;

our @customRcFiles ;

# for handling the loading of the proper packages
{
    my $builtPlatform = "";
    my $envPlatform = $ENV{"PERL_PLATFORM"};
    my $platform = "";

    if (-e "SDKPlatform")
    {
        $builtPlatform = `cat SDKPlatform`; chomp($builtPlatform);
    }

    if (defined($envPlatform) && ($envPlatform ne ""))
    {
        $platform = $envPlatform;
    }
    elsif ($builtPlatform ne "")
    {
        $platform = $builtPlatform;
    }
    else
    {
        $platform = "fm85xxep";
    }

    my $package = "Applications::TestPoint::" . $platform . "::FunctionTree";
    eval("use $package");

    if($@)
    {
        printf("Package Error for platform $platform:\n$@\n");
    }

    # check if an alternative ReadLine library was specified.
    $useAlternateLibrary = CheckAltLibrary();

    # if no alternative ReadLine library was specified, use ReadLine::Zoid
    if ($useAlternateLibrary == 0)
    {
        eval(require Term::ReadLine::Zoid);
    }
}

##@method TestPoint *new()
#   Instantiates a TestPoint object
sub new
{
    # the following allows new to be used as a class method or
    # as the constructor
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $platform = shift;
    my @args = shift;
    my $self = {
        MODE    => MODE_STANDARD,
        OPTS    => {},
        HISTORY => [],
        HISTORY_DELETE_STRING => '%%%%DELETED%%%%',
    };
    bless($self, $class);

    $self->initializeOptions($platform);

    $self->processArguments(@args);

    if (($self->{LOAD} eq "") || ($self->{INTERACTIVE} == 1))
    {
        $self->showPreamble();
    }

    $self->initializeTerminal(@args);

    if (defined($platform) && ($platform =~ m/^fm85xxep/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::fm85xxep::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^vegasNonSwag/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::vegasNonSwag::FunctionTree($platform,
                                                                         $self);
    }
    elsif (defined($platform) && ($platform =~ m/^vegas/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::vegas::FunctionTree($platform,
                                                                  $self);
    }
    elsif (defined($platform) && ($platform =~ m/^Quanta/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                   new Applications::TestPoint::Quanta::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^SanMarino/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                new Applications::TestPoint::SanMarino::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^Camilla/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                  new Applications::TestPoint::Camilla::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && $platform =~ m/^BaliSimulator$/)
    {
        printf("Instantiating platform %s ...\n", $platform);
        $self->{FT} =
            new Applications::TestPoint::BaliSimulator::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^ProdEng/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::ProdEng::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^heavenly/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::heavenly::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^monaco/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::monaco::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^fibmNIC/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::fibmNIC::FunctionTree($platform,
                                                                     $self);
    }
    elsif (defined($platform) && ($platform =~ m/^altaWhiteModel/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::altaWhiteModel::FunctionTree($platform,
                                                                         $self);
    }
    elsif (defined($platform) && ($platform =~ m/^shal/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::shal::FunctionTree($platform, $self);
    }
    elsif (defined($platform) && ($platform =~ m/^barcelona/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::barcelona::FunctionTree($platform,
                                                                      $self);
    }
    elsif (defined($platform) && ($platform =~ m/^mallorca/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::mallorca::FunctionTree($platform,
                                                                      $self);
    }
    elsif (defined($platform) && ($platform =~ m/^seacliff/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::seacliff::FunctionTree($platform,
                                                                      $self);
    }
    elsif (defined($platform) && ($platform =~ m/^IZ1/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::IZ1::FunctionTree($platform,
                                                                      $self);
    }
    elsif (defined($platform) && ($platform =~ m/^whiteModel/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::whiteModel::FunctionTree($platform,$self);    
    }
    elsif (defined($platform) && ($platform =~ m/^altaBurnIn/))
    {
        printf("Instantiating platform $platform...\n");
        $self->{FT} =
                 new Applications::TestPoint::altaBurnIn::FunctionTree($platform,$self);    
    }
    else
    {
        die("Unknown platform \"$platform\"!\n");
    }

    # Indicate if TestPoint is using an alternative library
    if ($useAlternateLibrary == 1)
    {
       print "Using alternative ReadLine library\n";
    }

    if (($self->{LOAD} eq "") || ($self->{INTERACTIVE} == 1))
    {
        $self->showPostamble();
    }

    # load symbols from whatever the function tree wants to use
    foreach my $library ($self->{FT}->getPackageListForSymbolLoading())
    {
        $self->loadSymbolsForUse($library);
    }

    $self->{FT}->initializeFunctionTree();

    # Always points to the last instance (there should only be one)
    $GLOB_INST = $self;

    #Initialize chip specific attributes
    $self->tpInitAttributes();

    # load RC now
    $self->locateRC(@args);



    my $isMaster = 1;
    # Don't load startup RC when not master process
    if (defined($self->{FT}->{CHIP}))
    {
        $self->{FT}->{CHIP}->fmIsMasterProcess(\$isMaster);

        if (defined($ENV{"FM_API_SHM_KEY"}))
        {
            if ($isMaster)
            {
                printf("Running as master process...\n");
            }
            else
            {
                printf("Running as non-master process, skipping startup RC...\n");
                $self->{SKIP} = 1;
            }
        }
        else
        {
            printf("Running in single-process mode...\n");
        }

    }

    if (!$self->{SKIP})
    {
       $self->{FT}->handleSetSwitchConfig("cpu_mac", getCPUMacAddress());
    }
    

    # debug mode skips the default config preamble
    if(!$self->{DEBUG} && !$self->{RESET})
    {
        if($self->{SKIP} == 0)
        {
            if (($self->{OPTS}->{RC} eq ($self->{OPTS}->{CFSTARTUP} .
                                         $self->{OPTS}->{STARTUPFILE})) || 
                ($self->{OPTS}->{RC} eq ($self->{OPTS}->{NFSSTARTUP} .
                                         $self->{OPTS}->{STARTUPFILE})))
            {
                printf("Detected saved startup at " .
                       $self->{OPTS}->{RC} . "\n");
                # Startup file found, load only this
                $self->loadFile($self->{OPTS}->{RC}, "startup");
            } 
            else
            {
                # startup not found, load from platform default startup
                # RC loaded in expert mode
                $self->loadFile($self->{OPTS}->{RC}, "expert");
                push(@{$self->{HISTORY}}, "load " . $self->{OPTS}->{RC});
                # loading all additional startup scripts
                if (scalar(@{$self->{OPTS}->{CUSTOMRCDIR}}) > 0)
                {
                    find(\&loadCustomRC, @{$self->{OPTS}->{CUSTOMRCDIR}});
                }
                foreach (@customRcFiles)
                {
                    if (!(-d $_))
                    {
                        push(@{$self->{HISTORY}}, "load " . $_ );
                        $self->loadFile($_, "expert");
                    }
                }
            }
        }

        if($self->{LOAD} ne "")
        {
            # autodetection will take care of mode types
            push(@{$self->{HISTORY}}, "load " . $self->{LOAD} );
            $self->loadFile($self->{LOAD});

            if ($self->{INTERACTIVE} == 0)
            {
                exit(0);
            }
        }
    }
    else
    {
        printf("**************************************\n");
        printf("* ENTERING DEBUG MODE, ONLY REGISTER *\n");
        printf("* COMMANDS ARE GUARANTEED TO WORK!   *\n");
        printf("**************************************\n");
    }

    printf("TestPoint loaded in %d seconds\n", time() - $startTime);

    return $self;
}

sub loadCustomRC
{
    if ( !(-d $File::Find::name))
    { 
        push(@customRcFiles, $File::Find::name);     
    }
}

##@cmethod public void EvaluatePerl(char *input)
#
# @desc         Evaluates a chunk of command line input as pure Perl code
#
# @param[in]    input The chunk of command line input to be evaluated
sub EvaluatePerl
{
    my ($self, $input) = @_;

    # Remove leading and trailing whitespace.
    $input =~ s/^\s*(.*)\s*$/$1/;

    # Process variable declarations.
    my $chunk = $input;
    my $separator = '[{(\s]';
    while
    (
        $chunk =~ m/^(\s*|.*$separator\s*)(my|our)\s+([\$@%])(\w+)\s*[=(]([^=])/
    )
    {
        my $match = $&;
        my $replacement = $5;

        my $declaration = $2;
        # Install variables declared with 'our' in the main namespace. All
        # other variables are installed in the namespace pointed to by
        # _NAMESPACE_.
        my $namespace = $declaration eq 'our' ? 'main::' : _NAMESPACE_;
        my $type = $3;
        my $variable = $4;
        no strict 'refs';
        if (exists(${$namespace}{$variable}))
        {
            my $format = 
                  '"%s" variable %s%s masks earlier declaration in same scope';
            warn sprintf($format, $declaration, $type, $variable);
        }
        else
        {
            # Create a new symbol in the appropriate symbol table with the
            # appropriate type.
            SWITCH: for ($type)
            {
                /\$/ && do
                {
                    *{$namespace . $variable} = 0;
                    last SWITCH;
                };

                /[@%]/ && do
                {
                    *{$namespace . $variable} = ();
                }
            }
        }
        use strict;

        # Replace the matched pattern by the empty string.
        $chunk =~ s/\Q$match\E/$replacement/;
    }

    # Replace all occurrences of 'my' and 'our'.
    $input =~ s/^(my|our)//;
    $input =~ s/($separator)\s*(my|our)(\s+)/$1$3/g;

    # Perform symbol expansion and evaluate the command line input.
    $input = $self->ExpandSymbol($input);
    eval($input);
    die sprintf("Exception::Evaluation %s", $@) if $@;
}

##@cmethod private char* ExpandSymbol(char *input)
#
# @desc         Performs Perl symbol expansion
#
# @param[in]    input The input for which Perl symbol expansion is to be
#               performed
#
# @return       The input with all Perl symbols expanded
sub ExpandSymbol
{
    my ($self, $input) = @_;

    # Try to find all Perl variables, i.e. all scalars, arrays and hashes.
    my @characters = split(//, $input);
    my %symbols = ('$' => $TRUE, '@' => $TRUE, '%' => $TRUE);
    my @positions = ();
    map {
        if (exists($symbols{$characters[$_]}))
        {
            push(@positions, $_ + 1);
        }
    } (0 .. $#characters);

    # Perform a lookup for all symbols in the namespace pointed to by
    # _NAMESPACE_ and the main:: namespace. If a symbol is found in both
    # namespaces, the symbol in the namespace pointed to by _NAMESPACE_ will
    # trump the symbol in the main:: namespace.
    my $increase = 0; 
    foreach my $position (@positions)
    {
        my $offset = $position + $increase; 
        my $variable = $input; 
        $variable =~ s/^.{$offset}(\w+).*/$1/;

        no strict 'refs';
        my $namespace = exists(${(_NAMESPACE_)}{$variable})
                        ? _NAMESPACE_
                        : exists(${'main::'}{$variable})
                            ? 'main::'
                            : undef;
        use strict;
        if (defined($namespace))
        {
            $input =~ s/^(.{$offset})/$1$namespace/;
            $increase += length($namespace);
        }
    }
    return $input; 
}

##@method private void HandleApplicationSignals(int signal)
#
# @desc         Handles application generated POSIX signals
#
# @param[in]    signal The POSIX signal that is to be handled
#
# @throws       Exception::SIGINT if a keyboard interrupt has occured
sub HandleApplicationSignals
{
    my ($signal) = @_;

    SWITCH: for ($signal)
    {
        $_ eq "INT" && do
        {
            die "Exception::SIGINT";
            last SWITCH;
        };

        /^/ && die sprintf("%s: Unhandled application signal", $_);
    }
}

##@method private void HandleTerminalSignals(int signal)
#
# @desc         Handles terminal generated POSIX signals
#
# @param[in]    signal The POSIX signal that is to be handled
sub HandleTerminalSignals
{
    my ($signal) = @_;

    SWITCH: for ($signal)
    {
        $_ eq "INT" && do
        {
            $__terminal->modifying();
            $__terminal->delete_text();
            $__terminal->Attribs->{'point'} = $__terminal->Attribs->{'end'} = 0;
            $__terminal->redisplay();
            last SWITCH;
        };

        /^/ && die sprintf("%s: Unhandled terminal signal", $_);
    }
}

##@method void initializeOptions()
#   Sets the default state for the TestPoint object, called on startup.
sub initializeOptions
{
    my ($self, $platform) = @_;

    $self->{OPTS}->{PLATFORM}    = $platform;
    $self->{OPTS}->{SYMBOLS}     = [];
    $self->{OPTS}->{MATCHES}     = [];
    $self->{OPTS}->{PROMPT}      = "% ";
    $self->{OPTS}->{STARTUPFILE} = "saved_startup";
    $self->{OPTS}->{CFSTARTUP}   = "/testpoint_scripts/";
    $self->{OPTS}->{NFSSTARTUP}  = "./Config/$platform/";
    $self->{OPTS}->{DEFRC}       = "Config/$platform/testpoint_startup";
    $self->{OPTS}->{RC}          = "";
    $self->{OPTS}->{CUSTOMRCDIR}  = [];
    $self->{OPTS}->{RCSKIP}      = 0;
    $self->{OPTS}->{RCEXIT}      = 0;
    $self->{OPTS}->{SCRIPTS}     = [];
    $self->{OPTS}->{ALIASES}     = [];
    $self->{OPTS}->{COMMANDS}    = {};
}

##@method void showPreamble()
#   Dumps the text header at the beginning of the run
sub showPreamble
{
    printf("
TestPoint - interactive configuration environment [$TestPoint_VERSION]
Copyright (C) 2007-2012 Intel Corporation.  All rights reserved.
Unauthorized disclosure is prohibited.
    \n");
}

##@method void showPostamble()
#   Dumps the text header at the beginning of the run
sub showPostamble
{
    my ($self) = @_;
    
    my $switchCount = scalar(@{[$self->{FT}->tpGetPhysicalSwitches]});
    
    printf("

Basic Help:
    <tab>     - tab complete commands
    ?         - context sensitive help
    !0,2..4   - override selected switches
    load      - load a saved script
    save      - save the command history as a script
    quit      - quit application
    \n");
}

##@method void processArguments(char **args)
#   Process the commandline arguments and sets options accordingly
#
#   @param[in] args Array of strings (probably passed from @ARGV)
sub processArguments
{
    my ($self, @ARGV) = @_;
    use Getopt::Long;

    my ($debug, $debugPlatform, $interactive, $load, $reset, $skip, $help);

    Getopt::Long::Configure("pass_through");

    GetOptions("debug"              => \$debug,
               "debug-platform"     => \$debugPlatform,
               "reset-only"         => \$reset,
               "skip-startup"       => \$skip,
               "i"                  => \$interactive,
               "load=s"             => \$load,
               "help"               => \$help);

    if (defined($help))
    {
        printf(<<EOF
TestPoint - interactive configuration environment [$TestPoint_VERSION]
Copyright (C) 2007-2012 Intel Corporation.  All rights reserved.
Unauthorized disclosure is prohibited.

Commandline options:

    --help
        Displays this help message.

    --debug-platform
        Do not perform identification, no commands are valid other than
        those that call platform services.

    --debug
        Do not reset the chip nor initialize the SDK, register access only.
        Cannot be used in conjunction with --reset-only.

    --reset-only
        Do not initialize the SDK, register access only. Cannot be used in
        conjunction with --debug.

    --load=<script>
        Loads the given script using the same search paths as the
        load command.

    --skip-startup
        Initialize the SDK, but do not execute the testpoint_startup
        script.

    -i
        Interactive mode.  Used in conjunction with --load.  Does not
        apply to normal execution.  When present, TestPoint will not
        terminate after loading the script from --load.

EOF
        );
        exit(0);
    }

    $self->{DEBUGPLAT} = $debugPlatform;
    $self->{DEBUG} = $debugPlatform ? $debugPlatform : $debug;
    $self->{RESET} = $reset;
    $self->{INTERACTIVE} = defined($interactive) ? 1 : 0;
    $self->{PRELOAD} = $load;
    $self->{SKIP} = defined($skip) ? 1 : 0;
    $self->{LOAD} = defined($load) ? $load : "";
}

##@method void locateRC()
#   Finds where a suitable init script can be found
sub locateRC
{
    my ($self) = @_;
    my $defaultRC = $self->{OPTS}->{DEFRC};
    my $platform = $self->{OPTS}->{PLATFORM};

    my $cfstartupfile = $self->{OPTS}->{CFSTARTUP} .
                        $self->{OPTS}->{STARTUPFILE};
    my $nfsstartupfile = $self->{OPTS}->{NFSSTARTUP} .
                         $self->{OPTS}->{STARTUPFILE};

    if (-e $cfstartupfile) 
    {
        # Use the saved startup at the CF location
        $self->{OPTS}->{RC} = $cfstartupfile; 
    }
    elsif (-e $nfsstartupfile)
    {
        # Use the saved startup at the NFS location
        $self->{OPTS}->{RC} = $nfsstartupfile;
    }
    elsif(-e "./$defaultRC")
    {
        # Use the default startup at the NFS location 
        $self->{OPTS}->{RC} = "./$defaultRC";
    }
    else
    {
        printf("neither " . $self->{OPTS}->{STARTUPFILE} .
               " or $defaultRC " .
               "were found, skipping...\n");
    }

    my @directories = (
        "../../../testpoint_scripts/scripts/startup",
        sprintf("../../../testpoint_scripts/%s/scripts/startup", $platform)
    );
    foreach my $directory (@directories)
    {
        if (-d($directory))
        {
            push(@{$self->{OPTS}->{CUSTOMRCDIR}}, $directory);
        }
    }
}

##@method char *validateScriptPath(char *filename)
#   Attempts to find the full path to a script given the filename
#
#   @param[in] filename The script filename to find
#   @return The fully qualified path, or undef if not found
sub validateScriptPath
{
    my ($self, $filename) = @_;
    my $platform = $self->{OPTS}->{PLATFORM};

    my @pathOptions = (
        $filename,
        "../../testpoint_scripts/scripts/$filename",
        "../../testpoint_scripts/$platform/scripts/$filename",
        "Config/$platform/scripts/$filename",
        "Config/scripts/$filename"
    );

    foreach my $fqpath (@pathOptions)
    {
        if (-e $fqpath)
        {
            return $fqpath;
        }
    }

    return undef;
}

##@method void loadFile(char *file, char *mode)
#   Loads the given file
#   @param[in] file The filename to load
#   @param[in] mode If "expert" then load script as raw perl, otherwise
#                   evaluate through function tree
#   @return 1 is successful, 0 otherwise
sub loadFile
{
    my ($self, $file, @args) = @_;

    if (!defined($file))
    {
        printf("Must specify a valid script name!\n");
        return 0;
    }

    my $mode = $args[0];

    if (defined($mode) && (($mode eq "expert") || ($mode eq "startup")))
    {
        shift(@args);
    }
    else
    {
        $mode = undef;
    }

    my $fqpath = $self->validateScriptPath($file);

    if (!defined($fqpath) || ($fqpath eq ""))
    {
        printf("$file was not found in any of the search paths!\n");
        return 0;
    }

    my $code = "";
    my $firstLine = "";

    if (open(FH, "$fqpath"))
    {
        $firstLine = <FH>;
        close(FH);
    }

    # autodetect
    if (defined($firstLine))
    {
        if ($firstLine =~ m/^# expert/)
        {
            $mode = "expert";
        }
    }
    
    if(!defined($mode))
    {
        $mode = "basic";
    }

    printf("Loading $fqpath\n");

    if(!open(SCR, "<$fqpath"))
    {
        printf("Unable to open script $fqpath!\n");
        return 0;
    }

    # enable script debugging with export ECHOSCRIPT={no|yes|time}
    # command echo output prints to STDOUT as:
    # ">> <command string>" if ECHOSCRIPT=yes or
    # ">> hh:mm:ss: <command string>" if ECHOSCRIPT=time
    my $echoScript = 'no';
    $echoScript = $ENV{ECHOSCRIPT} if ( defined $ENV{ECHOSCRIPT} );
    my $printLine = ( $echoScript =~ /yes|time/ ) ? 1 : 0;
    my $timenow   = '';
    my $date      = '';
    if (defined($mode) && ($mode eq "expert"))
    {
        $code .= sprintf("my \$ARGC = %d; my \@ARGV = (%s);",
                         scalar(@args), join(",",map { "\"$_\"" } @args));
        $code .= formatEchoLine("\n");
                    
        while(<SCR>)
        {
            # embed echoLine() calls in the code 
            $code .= formatEchoLine($_);
        }
        # always add a blank line at end
        $code .= formatEchoLine("\n");

        # eval the entire script as a single string -- 
        # allows use of any perl constructs in the script
        # print $code;
        eval($code);
        warn $@ if $@;
        close(SCR);
    }
    else
    {
        foreach (<SCR>)
        {
            # evaluate as TestPoint commands (default)
            $code = $_;
            if ( $echoScript eq 'time' )
            {
                $date = `date +%T`;
                chomp($date);
                $timenow = "$date: ";
            }
            if ($printLine)
            {
                print ">> $timenow$code";
            }
            chomp($code);

            # allow comments in basic script
            if ( $code =~ /^#/ )
            {
                next;
            }
            my $disableHistory = 1;
            if ($mode eq "startup")
            {
                $disableHistory = 0;
            }
            $self->evaluate($code, $disableHistory);

        }

        close(SCR);
    }
    return 1;
}

##@method char *formatEchoLine(char *line)
#   Format command echo output prints to STDOUT
#   @param[in] line The line to format
sub formatEchoLine
{
    my ($line) = @_;
    my $qqline = "'" . $line . "'";

    if ( ($line =~ /^#/) || ($line =~ /^\s*$/) )
    {
        return "Applications::TestPoint::echoLine($qqline);";
    }
    elsif ( $line =~ /tp.*\(|fm.*\(/ )
    {
        return "Applications::TestPoint::echoLine($qqline); $line";
    }
    else
    {
        return $line;
    }
}

##@method char *echoLine(char *line)
#   Echo output prints to STDOUT
#   @param[in] line The line to echo
sub echoLine
{
    # embedded in expert mode scripts by formatEchoLine()
    my ($line) = @_;

    # enable script debugging with export ECHOSCRIPT={no|yes|time}
    # command echo output prints to STDOUT as:
    # ">> <command string>" if ECHOSCRIPT=yes or
    # ">> hh:mm:ss: <command string>" if ECHOSCRIPT=time
    my $echoScript = 'no';
    $echoScript = $ENV{ECHOSCRIPT} if ( defined $ENV{ECHOSCRIPT} );
    my $printLine = ( $echoScript =~ /yes|time/ ) ? 1 : 0;
    my $timenow   = '';
    my $date      = '';
    if ($echoScript eq 'time')
    {
        $date = `date +%T`;
        chomp($date);
        $timenow = "$date: ";
    }
    if ($printLine)
    {
        print ">> $timenow$line";
    }
}

##@method char* getCurrentLine()
#  Gets the current command line from ReadLine::Zoid.
sub getCurrentLine
{
    my $currentLine;
    if (defined($__terminal))
    {
        $currentLine = $__terminal->_return();
    }
    return $currentLine;
}

##@method void initializeTerminal()
#   Sets up the readline terminal to be used, installs handlers as appropriate
sub initializeTerminal
{
    my ($self) = @_;

    if ($useAlternateLibrary == 0)
    {
        # Create a terminal
        $__terminal = new Term::ReadLine::Zoid("TestPoint");
    
        # Configure the terminal
        #  -Set the TAB-completion function
        $__terminal->Attribs->{'completion_function'} = \&completeEntry;
    
        $__terminal->Attribs->{'minline'} = 2;
        $__terminal->Attribs->{'maxcomplete'} = 0;
        $__terminal->Attribs->{'autolist'} = 1;
    
    
        #  -Install a mode switcher that is bound to the Ctrl-p key combination.
        $__terminal->bindkey('^p', sub
        {
            $GLOB_INST->toggleAdminMode();
            $__terminal->return_eof();
        });
    
        #  -Install a help function that is bound to the ? key.
        $__terminal->bindkey('?',sub
        {
            my $currentLine = getCurrentLine();
            if ($self->IsExpertMode())
            {
                $currentLine =~ s/^\s*tp\(\"//;
            }
            $GLOB_INST->{FT}->processHelpForCommand($currentLine);
        });
    
        $self->{TERM} = $__terminal;
        $self->{TERMATTR} = $__terminal->Attribs;
    }
    else
    {
        # Use an alternative library.
        eval("\$__terminal = new $alternateLibrary1 ('TestPoint')");
    
        $__terminal->Attribs->{'completion_entry_function'} = \&completeInput;
    
        # Install a mode switcher that is bound to the C-p key combination.
        $__terminal->add_defun('mode_switcher', sub
        {
            $GLOB_INST->toggleAdminMode();
            my $hier = $GLOB_INST->{FT}->{HIER}->{NAME};
            my $prompt = $GLOB_INST->IsExpertMode() ? "expert" : $hier;
    
            $GLOB_INST->{TERM}->set_prompt($GLOB_INST->getPrompt());
            printf("\n");
            $GLOB_INST->{TERM}->on_new_line();
        });
        $__terminal->bind_key(ord("\cp"), 'mode_switcher');
    
        # Install a help function that is bound to the ? key.
        $__terminal->add_defun('help_helper', sub
        {
            my $currentLine = $__terminal->Attribs->{'line_buffer'};
            if ($self->IsExpertMode())
            {
                $currentLine =~ s/^\s*tp\(\"//;
            }
            $GLOB_INST->{FT}->processHelpForCommand($currentLine);
            $__terminal->on_new_line();
        });
        $__terminal->bind_key(ord("?"), 'help_helper');
    
        $self->{TERM} = $__terminal;
        $self->{TERMATTR} = $__terminal->Attribs;
    }
}

##@cmethod void run()
#
# @desc         The main loop for the TestPoint application
sub run
{
    my ($self) = @_;

    # Install a SIGINT signal handler.
    sigaction(SIGINT, new POSIX::SigAction(
                              \&Applications::TestPoint::HandleTerminalSignals));

    while ($TRUE)
    {
        my $input = $__terminal->readline($self->getPrompt());

        if (!defined($input) || length($input) == 0)
        {
            next;
        }

        local $SIG{'__DIE__'} = sub
        {
            my ($exception) = @_;

            if ($exception =~ m/Exception::SIG(INT)/)
            {
                return;
            }
            elsif ($exception =~ m/Exception::Evaluation/)
            {
                $exception =~ s/Exception::Evaluation\s*//;
                warn($exception);
            }
            else
            {
                Carp::croak($exception);
            }
        };

        my $currentAction = new POSIX::SigAction(
                          \&Applications::TestPoint::HandleApplicationSignals);
        my $previousAction = new POSIX::SigAction();
        sigaction(SIGINT, $currentAction, $previousAction);
        eval {
            $self->evaluate($input);
        };
        warn $@ if $@;
        # Restore the original SIGINT signal handler.
        sigaction(SIGINT, $previousAction);
    }
}

##@method void evaluate()
#   Evaluate a snippet  and track errors
sub evaluate
{
    my ($self, $input, $disableHistory) = @_;
    my $handled = 0;
    my $commands = $self->{OPTS}->{COMMANDS};

    # remove leading and trailing whitespace
    $input =~ s/^[\s]*(.*)[\s]*$/$1/;
    if (length($input) == 0)
    {
        return;
    }

    # grab first word for special commands
    my @arguments = split(/\s+/, $input);
    my $keyword = shift(@arguments);

    # handle special keywords
    SWITCH: for ($keyword)
    {
        $_ eq 'exit' && do
        {
            $self->{MODE} = MODE_STANDARD;
            return;
        };

        $_ eq 'expert' && do
        {
            $self->{MODE} = MODE_EXPERT;
            return;
        };

        $_ eq 'load' && do
        {
            my $loadret = $self->loadFile(@arguments);
            if (($loadret == 1) &&
                ((!(defined $disableHistory)) || ($disableHistory != 1)))
            {
                push(@{$self->{HISTORY}}, $input);
            }
            return $loadret;

        };

        $_ eq 'quit' && do
        {
            print("Exiting TestPoint ...\n");
            eval { $self->{FT}->tpHandleQuit() };
            $self->tpPlatformExit();
            exit(0);
        };
    }

    # reset error
    $@ = "";

    my $ret = 0;

    if ($self->IsExpertMode())
    {
        # in perl mode just do straight evaluation
        $self->EvaluatePerl($input);

        if ($input =~ m/^\s*use (.*);/)
        {
            $self->loadSymbolsForUse($1);
        }
    }
    else
    {
        $ret = $self->evaluateTestPoint($input);
    }

    if($@) {
        printf("\nERROR: $@\n");
        $ret = 0;
    }

    if (($ret != 1) || 
        ($input =~ /(show|packet|del history|save)/) ||
        ((defined $disableHistory) && ($disableHistory == 1)))
    {
        #printf("Skipping errors, show, save or packet...\n");
    } 
    else 
    {
        push(@{$self->{HISTORY}}, $input);
    }
}

##@method int evaluateTestPoint (char *input)
#   Evaluates a TestPoint statement
#   @param[in] input The input line
#   @return 1 if evaluation was error free
sub evaluateTestPoint
{
    my ($self, $input) = @_;
    
    my $oldSwitchSelect = "";
    
    # Process leading switch select override, if present. It takes the form
    # of a bang (!) followed by a switch list spec (one or more switch
    # numbers separated by commas and or ".." (to indicate a range).
    if ($input =~ m/^\s*!(\S*)\s+(.*)/)
    {
        $oldSwitchSelect = $self->getSelectedSwitches();
        
        if ( !($self->{FT}->validateAndSetSwitchSelect($1)) )
        {
            print("Invalid switch select override.\n");
            return 0;
        }
        
        $input = $2;
    }

    # Parse and process the command.
    my $ret = $self->{FT}->processThroughFunctionTree($input);
    
    # Restore the original switch select if it was overridden.
    if ($oldSwitchSelect ne "")
    {
        $self->{FT}->validateAndSetSwitchSelect($oldSwitchSelect);
    }

    return $ret;
}

##@method void loadSymbolsForUse(char *module)
#   Handles the loading of symbols to the tab completion list for use
#   statements
sub loadSymbolsForUse
{
    my ($self, $module) = @_;

    no strict 'refs';
    my $symbolTable = \%{$module . "::"};
    if (exists($symbolTable->{'SYMBOLS'}))
    {
        foreach my $symbol (@{$module . "::SYMBOLS"})
        {
            push(@{$self->{OPTS}->{SYMBOLS}}, $symbol);
        }
    }
    elsif (exists($symbolTable->{'EXPORT'}))
    {
        # The Perl foreach construct aliases $entry to each entry of the ARRAY
        # being looped over. As a consequence, modifying $entry will result in
        # a modification of the ARRAY being looped over. It is therefore
        # necessary to make a copy of $entry which can then safely be modified.
        foreach my $entry (@{$module . "::EXPORT"})
        {
            my $symbol = $entry;
            $symbol =~ s/^(\$|@|%)//;
            push(@{$self->{OPTS}->{SYMBOLS}}, $symbol);
        }
    }
}

##@method void tgoggleAdminMode()
#   Toggles perl evaluation mode
sub toggleAdminMode
{
    my ($self) = @_;

    if ($self->IsExpertMode())
    {
        $self->{MODE} = MODE_STANDARD;
    }
    else
    {
        $self->{MODE} = MODE_EXPERT;
    }
}

##@cmethod private bool IsExpertMode()
#
# @desc         Determines whether TestPoint is operating in standard mode or
#               in expert mode
#
# @return       TRUE if TestPoint is operating in expert mode
# @return       FALSE otherwise
sub IsExpertMode
{
    my ($self) = @_;

    return defined($self->{MODE}) && $self->{MODE} == MODE_EXPERT
           ? $TRUE
           : $FALSE;
}

sub getPrompt
{
    my ($self) = @_;

    my $prompt = $self->IsExpertMode() ? "expert" : $self->getSelectedSwitches();


    return "<$prompt>" . $self->{OPTS}->{PROMPT};
}

##@method getSelectedSwitches()
#   Gets the list of switches currently selected for configuration.
# 
sub getSelectedSwitches
{
    my ($self) = @_;

    my $switchList = "";
    my $switchCount = scalar(@{[$self->{FT}->tpGetPhysicalSwitches]});
    
    $switchList = $self->{FT}->StringifyList($self->{FT}->tpGetSwitches);
    
    # If there are no selected switches and there is at least one switch
    # present (as may happen when a new FIBM-controlled switch is created),
    # then default the selected switch list to all switches present.
    if ($switchList eq "" && $switchCount > 0)
    {
        $self->{FT}->tpSetSelectedSwitches(@{[$self->{FT}->tpGetPhysicalSwitches]});
        $switchList = $self->{FT}->StringifyList($self->{FT}->tpGetSwitches);
    }
    
    return $switchList;
}

##@method void completeEntry(char *word, char *buffer, uint start)
#   This method is a wrapper that allows calling completeInput()
#   from the Term::ReadLine::Zoid library.
#
#   @param[in] word    The word to be completed.
#   @param[in] buffer  The command line so far.
#   @param[in] start   The index of the word to complete. 
sub completeEntry
{
    my ($word, $buffer, $start) = @_;

    completeInput($word,0, $buffer);
    return @{$GLOB_INST->{OPTS}->{MATCHES}};
}

##@method void completeInput(char *text, char *state)
#   Performs the autocompletion function for readline
#
#   @param[in] text The string so far
#   @param[in] state 0 for the first call
sub completeInput
{
    my ($text, $state, $currLine) = @_;
    my $platform = $GLOB_INST->{OPTS}->{PLATFORM};

    my @pathOptions = (
        ".",
        "../../testpoint_scripts/scripts",
        "../../testpoint_scripts/$platform/scripts",
        "Config/$platform/scripts",
        "Config/scripts"
    );

    my $extraPathEnv = $ENV{"TESTPOINT_SCRIPT_PATH"}; 
    
    if (defined($extraPathEnv))
    {
        foreach my $part (split(":", $extraPathEnv))
        {
            push(@pathOptions, $part);
        }
    }

    my $chip = $GLOB_INST->{FT}->{CHIP};
    my $symbols = $GLOB_INST->{OPTS}->{SYMBOLS};

    if ($state == 0)
    {
        if ($useAlternateLibrary == 1)
        {
            $currLine = $GLOB_INST->{TERMATTR}->{line_buffer};
        }

        # in admin mode we start from the beginning of the tp(" part
        if ($GLOB_INST->IsExpertMode())
        {
            $currLine =~ s/^\s*tp\(\"//;
        }
        
        # Strip any switch select override
        $currLine =~ s/^\s*!\S*\s+//;

        # We always complete TestPoint commands.
        $GLOB_INST->{OPTS}->{MATCHES} =
                              $GLOB_INST->{FT}->GenerateCompletions($currLine);

        # we only do symbol completion in admin mode
        if ($GLOB_INST->IsExpertMode())
        {
            # go through all symbols
            foreach my $sym (@{$symbols})
            {
                if ($sym =~ m/^$text/)
                {
                    push(@{$GLOB_INST->{OPTS}->{MATCHES}}, $sym);
                }
            }
        }
        # file completion for load
        elsif ($currLine =~ m/^load (.*)/)
        {
            my $frag = $1;

            foreach my $pt (@pathOptions)
            {
                my @f = <$pt/*>;

                foreach my $file (@f)
                {
                    if ($file =~ m/$frag/)
                    {
                        push(@{$GLOB_INST->{OPTS}->{MATCHES}}, $file);
                    }
                }
            }
        }
        # let all FM_* symbols through for reg commands
        elsif ($currLine =~ m/reg global (read|write|set_bit|clear_bit)/)
        {
            # go through all symbols
            foreach my $symbol (@{$symbols})
            {
                if ($symbol =~ m/^FM_/
                    && $symbol =~ m/^$text/
                    && $chip->isRegister($symbol)
                    && !$chip->registerIsND($symbol))
                {
                    push(@{$GLOB_INST->{OPTS}->{MATCHES}}, $symbol);
                }
            }
        }
        elsif ($currLine =~ m/reg indexed (read|write|set_bit|clear_bit)/)
        {
            # go through all symbols
            foreach my $symbol (@{$symbols})
            {
                if ($symbol =~ m/^FM_/
                    && $symbol =~ m/^$text/
                    && $chip->isRegister($symbol)
                    && ($chip->registerIs1D($symbol)
                        || $chip->registerIs2D($symbol))
                    && !$chip->registerIsPortIndexed($symbol))
                {
                    push(@{$GLOB_INST->{OPTS}->{MATCHES}}, $symbol);
                }
            }
        }
        elsif ($currLine =~ m/reg port (read|write|set_bit|clear_bit) ([all.,0-9]+)/)
        {
            # go through all symbols
            foreach my $symbol (@{$symbols})
            {
                if ($symbol =~ m/^FM_/
                    && $symbol =~ m/^$text/
                    && $chip->isRegister($symbol)
                    && ($chip->registerIs1D($symbol)
                        || $chip->registerIs2D($symbol))
                    && $chip->registerIsPortIndexed($symbol))
                {
                    push(@{$GLOB_INST->{OPTS}->{MATCHES}}, $symbol);
                }
            }
        }
    }

    if($useAlternateLibrary != 0 &&
       scalar(@{$GLOB_INST->{OPTS}->{MATCHES}}) > 0)
    {
        return shift(@{$GLOB_INST->{OPTS}->{MATCHES}});
    }
    return ();
}

sub getVersion
{
    return $TestPoint_VERSION;
}

sub genStringValue
{
    my $inString = shift;
    my $stringValue = 0;

    if (defined $inString )
    {
        use integer;

        foreach (split //, $inString)
        {
            $stringValue *= 31;
            $stringValue += ord($_);
            $stringValue &= 0x3FFF;
        }
    }
    return $stringValue;
}

sub CheckAltLibrary
{
    my ($alternateLibrary) = $ENV{"FM_TESTPOINT_ALT_LIBRARY"};
    my $useAltLib = 0;

    if (defined $alternateLibrary)
    {
        #validate the environment variable.
        if (genStringValue($alternateLibrary) == 12088)
        {
            # load alternate libraries
            $alternateLibrary1 = substr($alternateLibrary,0,14);
            $alternateLibrary2 = $alternateLibrary;

            eval ("require $alternateLibrary1");
            if (!$@)
            {
                eval ("require $alternateLibrary2");
                if (!$@)
                {
                    # Set a flag to indicate testPoint is using
                    #  an alternate library
                    $useAltLib = 1;
                }
            }
            if (!$useAltLib)
            {
                warn "\nError loading required alternate library\n";

            }
        }
    }
    return $useAltLib;
}

# handles calling API functions directly
sub AUTOLOAD
{
    my (@args) = @_;
    
    return if $AUTOLOAD =~ /::DESTROY$/;

    my $chip = $GLOB_INST->{FT}->{CHIP};

    $AUTOLOAD =~ s/^.*:://;

    if (($AUTOLOAD =~ m/^fm.*/) ||
        ($AUTOLOAD =~ m/^FM_*/))
    {
        return $chip->$AUTOLOAD(@args);
    }
    elsif ($AUTOLOAD eq "tp")
    {
        return $GLOB_INST->evaluateTestPoint(@args);
    }
    elsif ($AUTOLOAD =~ m/^tp.+/)
    {
        return $GLOB_INST->{FT}->$AUTOLOAD(@args);   
    }
    die sprintf("%s: Unknown AUTOLOAD method!", $AUTOLOAD);
}

END
{
    my $chip = $GLOB_INST->{FT}->{CHIP};

    printf("\nTerminate the API.\n");
    $chip->fmTerminate();

    if ($useAlternateLibrary == 0)
    {
        # disable canonical input if TestPoint was called
        # from startup_cf
        if (-e "/usr/fulcrum_startup/startup_cf")
        {
            system("stty -icanon");
        }
        # restore terminal (enable 'echo')
        system("stty sane");
    }
}

1;
