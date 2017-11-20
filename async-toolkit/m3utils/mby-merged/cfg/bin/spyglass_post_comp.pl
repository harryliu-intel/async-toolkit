#!/usr/intel/pkgs/perl/5.8.5/bin/perl -w
# -*- perl -*-
use strict;
use Getopt::Long;
use FileHandle;
use vars qw (%opts);
Getopt::Long::config( "pass_through", "no_auto_abbrev" );
GetOptions(
    \%opts,      "lib|l=s",        "comp_libs=s",  "res_path|p=s",
    "summary|s", "run_libs=s",     "top_module=s", "project=s",
    "goal=s",    "max_warnings=i", "quiet",        "debug",
);
my $quiet = $opts{quiet} ? 1 : 0;
my @Lines = ();

if ( ( !$opts{summary} && exists $opts{lib} ) || exists $opts{project} ) {
    my $lib  = exists $opts{project} ? $opts{project} : $opts{lib};
    my $fail = 0;
    my $path = "";
    if ( exists $opts{project} ) {
        if ( $opts{project} eq 'pre_comp' ) {
            $path = "$opts{project}_wdir/$opts{project}/Design_Read";
        }
        else {
            $path =
"$opts{project}_wdir/$opts{project}/$opts{top_module}/$opts{goal}";
        }
    }
    else {
        $path = "${lib}_wdir";
    }
    my $file = "$opts{res_path}/lint/spyglass/${path}/spyglass.log";
    if ( -e $file ) {
        @Lines = `tail -2 $file`;
        $Lines[0] =~ /SpyGlass Exit Code (\d+)/;
        if ($1) {
            print "   spyglass compilation : ${lib} --> FAIL.\n"
              unless ($quiet);
            if ( $Lines[0] =~ /License failure/ ) {
                print "               *****  License Failure  *****\n"
                  unless ($quiet);
                exit 2;
            }
            else {
                &print_first_errors( ${lib}, $path, $quiet );
            }
            $fail = 1;
        }
        else {
            if ( $Lines[0] =~ /Rule-checking completed with errors/ ) {
                print "   spyglass compilation : ${lib} --> FAIL.\n"
                  unless ($quiet);
                &print_first_errors( ${lib}, $path, $quiet );
                $fail = 1;
            }
            elsif ( $Lines[0] =~ /Rule-checking completed with warnings/ ) {
                print "   spyglass compilation : ${lib} --> PASS.
         Log file : ${file}\n" unless ($quiet);
                &print_warning_num( ${lib}, $path, $quiet );
            }
            elsif ( $Lines[0] =~
                /Rule-checking completed without errors or warnings/ )
            {
                print "   spyglass compilation : ${lib} --> PASS.
         Log file : ${file}\n" unless ($quiet);
            }
            else {
                print "   spyglass compilation : ${lib} --> FAIL.\n"
                  unless ($quiet);
                &print_first_errors( ${lib}, $path, $quiet );
                $fail = 1;
            }
        }
    }
    else {
        print "   spyglass compilation : ${lib} --> FAIL.\n" unless ($quiet);
        print
          "               ${lib} seems to be empty, check ${lib} hdl content\n"
          unless ($quiet);
        print "               or put ${lib} under -lint_ignore_libs list \n"
          unless ($quiet);
        $fail = 1;
    }
    if ($fail) {
        exit 1;
    }
    else {
        exit 0;
    }
}
elsif ( $opts{summary} ) {
    my $res = &summary(
        comp_libs    => $opts{comp_libs},
        lib          => $opts{lib},
        max_warnings => $opts{max_warnings},
        top_module   => $opts{top_module},
        res_path     => $opts{res_path},
        debug        => $opts{debug},
    );
    if ($res) {
        exit 1;
    }
    else {
        exit 0;
    }
}
else {
    print
"lib argument is missing for parsing spyglass compilation results in $0 script\n";
    exit 1;
}

#-------------------------------------------------------------------------------

sub print_first_errors {
    my $lib   = shift;
    my $path  = shift;
    my $quiet = shift;
    return if ($quiet);
    my $fh = new FileHandle;
    my $moresimple =
      "$opts{res_path}/lint/spyglass/${path}/spyglass_reports/moresimple.rpt";
    $fh->open("< $moresimple")
      || die "can't open $moresimple for reading. $!";
    my $errCount = 0;
    my $state    = "ERRORS";    #"HEADER";
    my $cnter    = 0;

    while ( my $line = $fh->getline && $cnter < 200 ) {
        $cnter++;
        if ( $state eq "HEADER" ) {
            next unless ( $line =~ /MORESIMPLE REPORT\:/ );
            $fh->getline;
            print "first few errors from:\n${moresimple}\n";
            print $fh->getline;
            print $fh->getline;
            print $fh->getline;
            $state = "ERRORS";
        }
        elsif ( ( $state eq "ERRORS" )
            and ( $line =~ /\s+(Error|Fatal|Syntax|SynthesisError)\s+/ ) )
        {
            $errCount++;
            print "first few errors from:\n${moresimple}\n";
            print $line;
        }
        last if ( $errCount >= 3 );
    }
    if ( !$errCount ) {
        print "$moresimple\n";
    }
    $fh->close;
}

#-------------------------------------------------------------------------------

sub print_warning_num {
    my $lib   = shift;
    my $path  = shift;
    my $quiet = shift;
    return if ($quiet);
    my $moresimple = "${path}/spyglass_reports/moresimple.rpt";
    my $fh         = new FileHandle;
    $fh->open("< $moresimple")
      || die
      "can't open $opts{res_path}/lint/spyglass/${moresimple} for reading. $!";
    while ( my $line = $fh->getline ) {
        next unless ( $line =~ /Number of Reported Messages\s+\:\s+(\d+)/ );
        print "                $1 messages issued - see log file\n";
        last;
    }
    $fh->close;
}

#-------------------------------------------------------------------------------

sub summary {
    my %args         = @_;
    my @Libs         = split( /\s+/, $args{comp_libs} );
    my %res          = ();
    my $lib          = $args{lib};
    my $max_warnings = $args{max_warnings};
    my $top_module   = $args{top_module};
    my $res_path     = $args{res_path};
    my $debug        = $args{debug};
    $res{err} = 0;

    foreach my $lib (@Libs) {
        last if ( $res{err} );
        if ( -e "${res_path}/lint/spyglass/${lib}_wdir/spyglass.log" ) {
            my @Lines =
              `tail -2 ${res_path}/lint/spyglass/${lib}_wdir/spyglass.log`;
            $Lines[0] =~ /SpyGlass Exit Code (\d+)/;
            if ($1) {
                $res{$lib} = 'FAIL';
                $res{err} = 1;
            }
            else {
                if ( $Lines[0] =~ /Rule-checking completed with errors/ ) {
                    $res{$lib} = 'PASS with errors (probably BlackBox)';
                    $res{err} = 0;
                    print "Rule-checking completed with errors $lib\n";
                }
                elsif ( $Lines[0] =~
                    /Rule-checking completed without errors or warnings/
                    or $Lines[0] =~ /Rule-checking completed with warnings/ )
                {
                    $res{$lib} = 'PASS';
                }
                else {
                    $res{$lib} = 'FAIL';
                    $res{err} = 1;
                }
            }
        }
        else {
            $res{$lib} = 'FAIL';
            $res{err} = 1;
        }
    }

    # parsing elaboration warning num
    if ($max_warnings) {
        my $dir =
          $top_module ? "${top_module}_elab_wdir" : "TOP_MODULE_IS_MISSING";

        #	my @Dirs = `/bin/ls ${res_path}/lint/spyglass`;
        #	foreach my $d (@Dirs) {
        #	    next unless($d =~ /\w+_elab_wdir/);
        #	    $dir = $d;
        #	    chomp($dir);
        #	}
        if ( -d "${res_path}/lint/spyglass/$dir" ) {
            my $elab_fh = new FileHandle;
            my $sp_log  = new FileHandle;
            $elab_fh->open(
"< ${res_path}/lint/spyglass/$dir/spyglass_reports/moresimple.rpt"
              )
              || die
"can't open ${res_path}/lint/spyglass/$dir/spyglass_reports/moresimple.rpt for reading. $!";
            $sp_log->open("< ${res_path}/lint/spyglass/$dir/spyglass.log")
              || die
"can't open ${res_path}/lint/spyglass/$dir/spyglass.log for reading. $!";
            my $maxWarnings = ${max_warnings};
            my $warn_idx    = 0;
            my $isHardStop  = 0;
            while ( my $line = $elab_fh->getline ) {
                $warn_idx++   if ( $line =~ /\s+Warning\s+/ );
                $isHardStop++ if ( $line =~ /\s+Error\s+/ );
            }
            if ( $warn_idx > $maxWarnings || $isHardStop ) {
                if ( $warn_idx > $maxWarnings ) {
                    print
"Error: max allowed elaboration Warning exceeded, reached $warn_idx, allowed $maxWarnings\n";
                }
                if ($isHardStop) {
                    print
"Error: found errors during elaboration process, elaboration Log file:\n"
                      . "\t${res_path}/lint/spyglass/$dir/spyglass_reports/moresimple.rpt\n";
                }
                $res{err} = 1;
            }
            elsif ( ( $warn_idx <= $maxWarnings ) && !$isHardStop ) {
                print
"Conditionally PASSED: max allowed elaboration Warnings ($warn_idx) is less than max.permitted ($maxWarnings)\n\n";
            }
        }
        else {
            print
"Error: can't parse elaboration log for redandunt warnings $dir\n";
            $res{err} = 1;
        }
    }

    # sammury
    if ( ${debug} ) {
        foreach my $lib ( keys %res ) {
            next if ( $lib eq "err" );
            print "$lib $res{$lib} \n";
        }
    }
    if ( $res{err} ) {
        print "##########
 FAIL
##########\n";
        return 1;
    }
    else {
        print "##########
 PASS
##########\n";
        return 0;
    }
}

#-------------------------------------------------------------------------------
