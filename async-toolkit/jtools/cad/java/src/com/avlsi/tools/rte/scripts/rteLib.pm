
package rteLib;

use strict;
use warnings;
use Data::Dumper;
use XML::Simple;
use IO::Select;
use File::stat;
use File::Spec;
use IO::File;


# Given an array of [ "abcd,def", "xyz"]
# return ["abcd", "def", "xyz"]
sub expandOption
{
    my ($in) = @_;

    my $out = [];

    foreach my $x (@{$in})
    {
        my @t = split(/,/, $x);
        foreach my $a (@t)
        {
            push(@{$out}, $a);
        }
    }

    return $out;
}


# Uses $cmdLine->{masterCellDb} and $cmdLine->{cellDb}
sub openCellDatabase
{
    my ($cmdLine) = @_;

    my ($db1, $db2);

    if(!($cmdLine->{masterCellDb} eq "") &&
        (-e $cmdLine->{masterCellDb}))
    {
        $db1 = $cmdLine->{masterCellDb};
    }

    if(-e $cmdLine->{cellDb})
    {
        $db2 = $cmdLine->{cellDb};
    }

    return getRawResults($db1, $db2, 1);
}

# getRawResults returns a hash from cell name to raw RTE results; extract
# summary information from the raw results with getResults
sub getRawResults
{
    my ($db1, $db2, $verbose) = @_;

    my $dbHandle = {};
    if(defined($db1)) {
        printf("Opening master cell database...\n") if $verbose;
        $dbHandle= XMLin($db1);
    }

    my $dbHandle2 = {};
    if(defined($db2)) {
        printf("Opening local cell database...\n") if $verbose;
        $dbHandle2 = XMLin($db2);
    }

    # Merge Db results
    foreach my $key (keys(%{$dbHandle2}))
    {
        # Blindly have local entries override master entries
        $dbHandle->{$key} = $dbHandle2->{$key};
    }

    return $dbHandle;
}

# Read in the list of cells to run
# Uses $cmdLine->{suiteFile}
# Results are return in a hash reference with the following structure:
#   $retValue->[]->{cellName, timeout, qsubMem}
sub readSuiteFile
{
    my ($cmdLine) = @_; 

    printf("Reading suite file...\n");
    my @cells;
    if(-e $cmdLine->{suiteFile})
    {
        open(my $FH, "<", $cmdLine->{suiteFile}) or die $!;

        while(<$FH>)
        {
            chomp($_); 

            print $_ . "\n";
            # Look for:
            # cellanme[:env1,env2,...] [--option1=X] [--option2=Y]
            if($_ =~ /^[\s]*([0-9A-Za-z\.\[\]\{\}\(\),-_:]+)[\s]*(.*)/)
            {
                my $name = $1;
                my %options;
                $options{name} = $name;
                my $envList;
                my $optionStr = $2;

                if($name =~ /:/)
                {
                    my @tmp = split(/:/, $name);
                    $name = $tmp[0];
                    $options{name} = $name;
                    $options{envList} = join(":", @tmp[1..(scalar(@tmp)-1)]);
                }else
                {
                    $envList = "";
                }

                #printf("\tname: $name envs: $options{envList} options: $optionStr\n");
                $options{delayExceptions} = [];

                if(!$optionStr eq "")
                {
                    # Options are separated by spaces, remove
                    # any excess spaces
                    $optionStr =~ s/[ ]+/ /g;

                    # Supported options are:
                    #  timeout=integer
                    #  qsubMem=string  (32G, etc)

                    my @optionList = split(/ /,$optionStr);
                    foreach my $option (@optionList)
                    {
                        if($option =~ /[-]*t[imeout]*=(\d)+/)
                        {
                            $options{timeout} = $1;
                        }elsif($option =~ /[-]*qsubMem=([\w]+)/)
                        {
                            $options{qsubMem} = $1;
                        }elsif($option =~ /[-]*pltDelayException=([^\s]+)/)
                        {
                            printf("exception: $1\n");
                            push(@{$options{delayExceptions}}, $1);
                        }else
                        {
                            die "Error parsing suite file, unknown option following cell $name : $option";
                        }
                    }
                }
                push(@cells,  \%options);
            }
        }
    }

    return \@cells;
}

sub updateDb
{
    my ($db, $file) = @_;

    printf("Updating cellDb...\n");
    my $xml = XML::Simple::XMLout($db, KeyAttr=>[]);
    my $FH;
    open($FH, ">", $file);
    print $FH $xml;
    close($FH);
    printf("Done updating cellDb...\n");
}

# Given a cell, return that cell's file mtime
sub getCellTimestamp
{
    my ($cell, $cmdLine) = @_;

    my $file = getFileFromCell($cell, $cmdLine);

    if($file eq "-1"){ return -1; }

    my $stat = stat($file);
    return $stat->mtime;
}

# Given a FQCN, search the cast-path directories
# for the containing file
# Uses cast2 specification found here:
# http://internal/eng/depot/sw/cad/doc/specs/cast/fqcn_mapping.html
# return the full path to the file
sub getFileFromCell
{   
    my ($cell, $cmdLine) = @_;

    foreach my $castPath (split(/:/, $cmdLine->{castPath}))
    {
        # t1/t2/t3/../tN.cast. 
        my $file = getFileFromCell1($cell, $castPath);
        if(!($file eq "-1")) { return $file; }

        # t1/t2/t3/../tN/tN.cast. 
        $file = getFileFromCell2($cell, $castPath);
        if(!($file eq "-1")) { return $file; }

        # t1/t2/t3/../tN/C.cast. 
        $file = getFileFromCell3($cell, $castPath);
        if(!($file eq "-1")) { return $file; }

        # t1/t2/t3/../tN-1/tN-1/tN.cast. 
        $file = getFileFromCell4($cell, $castPath);
        if(!($file eq "-1")) { return $file; }

        # t1/t2/t3/../tN-1/tN-1/tN/C.cast. 
        $file = getFileFromCell5($cell, $castPath);
        if(!($file eq "-1")) { return $file; }

    }

    printf("ERROR unable to find %s in cast-path %s\n", $cell, $cmdLine->{castPath});
    return -1;
}

# t1/t2/t3/../tN.cast. 
sub getFileFromCell1
{   
    my ($cell, $castPath) = @_;

    my @tmp = split(/\./, $cell);

    # Remove the cell name
    splice(@tmp, scalar(@tmp)-1, 1);

    # Paste it back together using /
    my $filename = join("/", @tmp);

    #printf("Looking for $cell in $castPath/$filename.cast\n");
    if(-e $castPath . "/" . $filename . ".cast")
    {
        return $castPath . "/" . $filename . ".cast";
    }
    return -1;
}

# t1/t2/t3/../tN/tN.cast. 
sub getFileFromCell2
{
    my ($cell, $castPath) = @_;

    my @tmp = split(/\./, $cell);

    # Remove the cell name
    splice(@tmp, scalar(@tmp)-1, 1);

    # Repeat the last name
    push(@tmp, $tmp[scalar(@tmp)-1]);

    # Paste it back together using /
    my $filename = join("/", @tmp);

    #printf("Looking for $cell in $castPath/$filename.cast\n");
    if(-e $castPath . "/" . $filename . ".cast")
    {
            return $castPath . "/" . $filename . ".cast";
    }
    return -1;
}

# t1/t2/t3/../tN/C.cast. 
sub getFileFromCell3
{
    my ($cell, $castPath) = @_;

    my @tmp = split(/\./, $cell);

    # Harry says meta parameters are not included
    $tmp[scalar(@tmp)-1] =~ s/([a-zA-Z0-9]+)\(.*\)/$1/;

    # Paste it back together using /
    my $filename = join("/", @tmp);

    #printf("Looking for $cell in $castPath/$filename.cast\n");
    if(-e $castPath . "/" . $filename . ".cast")
    {
        return $castPath . "/" . $filename . ".cast";
    }
    return -1;
}

# t1/t2/t3/../tN-1/tN-1/tN.cast. 
sub getFileFromCell4
{
    my ($cell, $castPath) = @_;

    my @tmp = split(/\./, $cell);

    # Remove the cell name
    splice(@tmp, scalar(@tmp)-1, 1);

    # remove tN and save it
    my $tN = $tmp[scalar(@tmp)-1];
    splice(@tmp, scalar(@tmp)-1, 1);

    # add tN-1 again, then tN
    push(@tmp, $tmp[scalar(@tmp)-1]);
    push(@tmp, $tN);

    # Paste it back together using /
    my $filename = join("/", @tmp);

    #printf("Looking for $cell in $castPath/$filename.cast\n");
    if(-e $castPath . "/" . $filename . ".cast")
    {
        return $castPath . "/" . $filename . ".cast";
    }
    return -1;
}

# t1/t2/t3/../tN-1/tN-1/tN/C.cast. 
sub getFileFromCell5
{
    my ($cell, $castPath) = @_;
    
    my @tmp = split(/\./, $cell);

    # Remove the cell name
    my $cellName = $tmp[scalar(@tmp)-1];
    # Harry says meta parameters are not included
    $cellName =~ s/([a-zA-Z0-9]+)\(.*\)/$1/;
    splice(@tmp, scalar(@tmp)-1, 1);
    
    # remove tN and save it
    my $tN = $tmp[scalar(@tmp)-1];
    splice(@tmp, scalar(@tmp)-1, 1);

    # add tN-1 again, then tN, then C
    push(@tmp, $tmp[scalar(@tmp)-1]); 
    push(@tmp, $tN);
    push(@tmp, $cellName);
    
    # Paste it back together using /
    my $filename = join("/", @tmp);

    #printf("Looking for $cell in $castPath/$filename.cast\n");
    if(-e $castPath . "/" . $filename . ".cast")
    {
        return $castPath . "/" . $filename . ".cast";
    }
    return -1;
}

#given a string, copy it, and escape the parenthesis
sub escapeParenthesis
{
    my ($str) = @_;
    my $tmp = $str;
    $tmp =~ s/(\()/\\\(/g;
    $tmp =~ s/(\))/\\\)/g;
    $tmp =~ s/({)/\\\{/g;
    $tmp =~ s/(\[)/\\\[/g;
    return $tmp;
}   

# Replace ( ) and , with _
sub replaceParenthesisWith_
{
    my ($str) = @_;
    
    my $tmp = $str;
    $tmp =~ s/(\()/_/g;
    $tmp =~ s/(\))/_/g;
    $tmp =~ s/(,)/_/g;
    $tmp =~ s/({)/_/g;
    $tmp =~ s/(})/_/g;
    $tmp =~ s/(\[)/_/g;
    $tmp =~ s/(\])/_/g;
    return $tmp;
}

# Replace ( ) and , with _
sub dbEscapeCellName
{
    my ($str) = @_;

    return replaceParenthesisWith_($str);
}

sub computeCspCoverage
{
    my ($cell, $cmdLine) = @_;

    #printf("Computing csp coverage for cell $cell\n");
    my $path = sprintf("%s/%s/modules/%s",
                    $cmdLine->{outputDir},
                    rteLib::replaceParenthesisWith_($cell),
                    join('/', split(/\./, $cell)));
    my $nonEscapedPath = $path;
    $path = rteLib::escapeParenthesis($path);
   
    system("rm -f $path.total_csp_covered.gz");
    system("rm -f $path.total_csp.gz");
    system("rm -f $path:*.csp_uncovered.gz");
    system("rm -f $path:*.csp_covered.gz");
    system("rm -f $path:*.csp_total.gz");

    my $H;

    my $tmpDir = sprintf("%s/%s/tmp",  
                $cmdLine->{outputDir},
                rteLib::replaceParenthesisWith_($cell));
    system("mkdir $tmpDir");

    open($H, "ls -1 $path:*.csp_coverage.zip |");
    my $envName = "";
    while(<$H>)
    {
        chomp($_);
        $_ =~ /.*:(.*).csp_coverage.zip/;
        $envName = $1;   
        #printf("ENV: $envName\n");

        # Unpack the csp_coverage.zip file
        my $cmd = sprintf("unzip -qq $_ -d $tmpDir");
        $cmd=  rteLib::escapeParenthesis($cmd);
        system($cmd);
       

        system("zcat $path:$envName.csp_misses.gz | grep -v ^Missed | sort | " .
                "sed 's/:/ /' | sed 's/(\\(.*\\)):/\\1/g' | sed 's/ \\+/ /g' > $path:$envName.csp_uncovered");
        system("cat $tmpDir/probes | sort > $path:$envName.csp_total");
        system("comm -13 $path:$envName.csp_uncovered $path:$envName.csp_total > $path:$envName.csp_covered");
        
        my $covered = `cat $path:$envName.csp_covered | wc -l`; 
        my $uncovered = `cat $path:$envName.csp_uncovered | wc -l`; 
        my $total = `cat $path:$envName.csp_total | wc -l`; 
        chomp($covered); chomp($uncovered); chomp($total);

        #printf("Total: $total = covered: $covered - uncovered: $uncovered\n");

        if($total - $covered != $uncovered)
        {
            printf("ERROR total rules - covered != uncovered for env $envName\n");
            return -2;
        }

        system("rm $tmpDir/*");
    }

    system("rmdir $tmpDir");

    system("cat $path:*.csp_covered | sort -u > $path.total_csp_covered");
    system("cat $path:*.csp_total | sort -u > $path.total_csp");
    my $totalCovered = `cat $path.total_csp_covered | wc -l`;
    my $probeCnt = `cat $path.total_csp | wc -l`;
    chomp($totalCovered);
    chomp($probeCnt);
    #printf("total covered: $totalCovered total: $probeCnt\n");

    system("gzip $path:*.csp_covered");
    system("gzip $path:*.csp_uncovered");
    system("gzip $path:*.csp_total");
    system("gzip $path.total_csp_covered");
    system("gzip $path.total_csp");

    if($probeCnt <= 0)
    {
        return -2;
    }else
    {
        return sprintf("%0.2f", 1.0 * $totalCovered/$probeCnt);
    }
}

sub computeRuleCoverage
{
    my ($cell, $cmdLine) = @_;

    #printf("Computing rule coverage for cell $cell\n");
    my $path = sprintf("%s/%s/modules/%s",
                    $cmdLine->{outputDir},
                    rteLib::replaceParenthesisWith_($cell),
                    join('/', split(/\./, $cell)));
    my $tmp = $path;
    $path = rteLib::escapeParenthesis($path);

    # This will only happen if we're re-running
    # postProcessResults after rteLauncher finished
    # Also note -e doesn't like having () escaped
    if(-e "$tmp:total_covered_rules.gz")
    {
        system("rm $path:total_covered_rules.gz");
    }

    if(-e "$tmp:total_uncovered_rules.gz")
    {
        system("rm $path:total_uncovered_rules.gz");
    }

    if(-e "$tmp:total_rules.gz")
    {
        system("rm $path:total_rules.gz");
    }

    # Create total sorted list of all rules
    my $cmd = sprintf("zcat $path:*.uncovered_rules.gz $path:*.covered_rules.gz | sort -T %s/%s -u  > $path:total_rules",
            $cmdLine->{outputDir}, rteLib::replaceParenthesisWith_($cell));
    system($cmd);

    # Create total sorted list of covered rules
    $cmd = sprintf("zcat $path:*.covered_rules.gz | sort -T %s/%s -u > $path:total_covered_rules",
            $cmdLine->{outputDir}, rteLib::replaceParenthesisWith_($cell));
    system($cmd);

    # Create total list of all uncovered rules (purely for designer convinience)
    $cmd = sprintf("comm -13 $path:total_covered_rules $path:total_rules > $path:total_uncovered_rules");
    system($cmd);

    my $total = `cat $path:total_rules | wc -l`;
    my $covered = `cat $path:total_covered_rules | wc -l`;

    $cmd = sprintf("gzip $path:*_rules");
    system($cmd);

    if($total <= 0 || $covered < 0)
    {
        return -1;
    }else
    {   
        return sprintf("%0.2f", 1.0 * $covered / $total);
    }
}

sub postProcessPltResults
{
    my ($cellName, $perCellDelayExceptions, $cmdLine, $db) = @_;

    my %pltResult;

    printf("Plt Post processing cell $cellName\n");
    my $file = $cmdLine->{outputDir} . "/" . rteLib::replaceParenthesisWith_($cellName) . "/" .
                rteLib::replaceParenthesisWith_($cellName) .  ".output";
    my $unfilteredWarnings = $cmdLine->{outputDir} . "/" . 
                rteLib::replaceParenthesisWith_($cellName) . "/" .  
                "unfilteredWarnings.txt";
   
    if(!-e $file)
    {
        return;
    }

    my $pltVersion = `grep AutoSimulateCell $file`;
    $pltVersion =~ /\[INFO\] AutoSimulateCell Build: Linux-(.+)-(.+)-(.+)/;
    # In jdsim-auto version: Linux-i686-225683-official
    #  the code was changed so that a critical file is created per ntpc node
    if($2 >= 225683)
    {
        $pltVersion = 1;
    }else
    {
        $pltVersion = 0;
    }

    $db->{dbEscapeCellName($cellName)}->{pltVersion} = $pltVersion;
    # Per environment extract:
    #   Digital, measured, estimated delay config
    #   ntpc target
    #   measured ntpc
    # Also check that the critical cycle only contains measured delays
    my $H;
    open($H, "<",  $file) or die $!;

    my $UNFILTERED_WARNINGS;
    open($UNFILTERED_WARNINGS, ">", $unfilteredWarnings) or die $!;

    my $env = {};
    my %delayWarnings;
    while(<$H>)
    {
        chomp;

        if($_ =~ /^\/\*+simulating (.*) environment \*+\/$/)
        {
            # First close out last env being processed
            if(defined $env->{name})
            {
                if(defined $env->{digitalDelay})
                { 
                    $env->{delayWarnings} = join(",", keys(%delayWarnings));
                    $pltResult{$env->{name}} = $env;
                    $env = {ntpc_spec=>{}};
                    %delayWarnings = ();
                }
            }
            # Now, start processing new env
            $env = {name => $1};
        }elsif($_ =~ /^\/\*+done with (.*) environment \*+\/$/)
        {
            # Some environments end with "done with * environemt" line
            if(defined $env->{name})
            {
                if(defined $env->{digitalDelay})
                {
                    $env->{delayWarnings} = join(",", keys(%delayWarnings));
                    $pltResult{$env->{name}} = $env;
                    $env = {ntpc_spec=>{}};
                    %delayWarnings = ();
                }
            }
        }elsif($_ =~ /^\[WARN\] Critical cycle contains non-measured transitions/)
        {
            $delayWarnings{critical} = 1;
        }elsif($_ =~ /^Warning: Using estimated delay for/)
        {
            my $estimated = 1;
            foreach my $exception (@{$cmdLine->{pltDelayException}})
            {
                #printf("1 estimated Comparing $_    to cmdline exception   $exception\n");
                if($_ =~ /$exception/)
                {
                    #printf("Match\n");
                    $estimated = 0;
                }
            }
            foreach my $exception (@{$perCellDelayExceptions})
            {
                #printf("2 estimated Comparing $_    to   $exception\n");
                if($_ =~ /$exception/)
                {
                    #printf("Match\n");
                    $estimated = 0;
                }
            }

            if($estimated) 
            { 
                print $UNFILTERED_WARNINGS $_ . "\n";
                $delayWarnings{estimated} = 1; 
            }
        }elsif($_ =~ /^Warning: Using digital delay for/)
        {
            my $digital = 1;
            foreach my $exception (@{$cmdLine->{pltDelayException}})
            {
                #printf("digital Comparing $_    to cmdline exception   $exception\n");
                if($_ =~ /$exception/)
                {
                    #printf("Match\n");
                    $digital = 0;
                }
            }
            foreach my $exception (@{$perCellDelayExceptions})
            {
                #printf("digital Comparing $_    to exception   $exception\n");
                if($_ =~ /$exception/)
                {
                    #printf("Match\n");
                    $digital = 0;
                }
            }
            if($digital) 
            { 
                print $UNFILTERED_WARNINGS $_ . "\n";
                $delayWarnings{digital} = 1; 
            }

        }elsif($_ =~ /^\[INFO\] DIGITAL_TAU: ([0-9\.]+), ESTIMATED_TAU: ([0-9\.\-E]+), MEASURED_TAU: ([0-9\.]+), /)
        {
            $env->{digitalDelay} = $1;
            $env->{estimatedDelay} = $2;
            $env->{measuredDelay} = $3;
        }elsif($_ =~ /^\[.*\] ntpc_spec .*: (.+) \(spec: ([0-9\.]+) measured: ([0-9\.]+)\)/)
        {
            my %node = (name=>$1, cycleTarget=>$2, cycleMeasured=>$3);
            $env->{ntpc_spec}->{dbEscapeCellName($1)} = \%node;
        }elsif($_ =~ /^\[INFO\] cycling on ([^:]+)(:[01])* for \d+ transitions$/)
        {
            $env->{cycleNode} = $1;
            $env->{ntpc_spec}->{dbEscapeCellName($env->{cycleNode})}->{name} = $1;
        }elsif($_ =~ /^\[INFO\] Effective tau of critical cycle: (\d+\.\d+)$/)
        {
            if(!defined($env->{cycleNode}))
            {
                printf("Error found effective tau for cycle node, but don't know what cycle node is yet\n");
            }
            $env->{ntpc_spec}->{dbEscapeCellName($env->{cycleNode})}->{effectiveTau} = $1;
        }elsif($_ =~ /^\[INFO\] Effective tau of critical cycle for (.+): (\d+\.\d+)$/)
        {
            $env->{ntpc_spec}->{dbEscapeCellName($1)}->{effectiveTau} = $2;
        }elsif($_ =~ /\[INFO\] Critical cycle report: (.+)$/)
        {
            if($pltVersion == 0)
            {
                $env->{criticalFile} = $1;
            }else
            {
                if(!defined($env->{cycleNode}))
                {
                    printf("Error found effective tau for cycle node, but don't know what cycle node is yet\n");
                }
                $env->{ntpc_spec}->{dbEscapeCellName($env->{cycleNode})}->{criticalFile} = $1;
            }
        }elsif($_ =~ /\[INFO\] Critical cycle report for (.+): (.+)$/)
        {
            $env->{ntpc_spec}->{dbEscapeCellName($1)}->{criticalFile} = $2;
        }
    }

    close($H);
    close($UNFILTERED_WARNINGS);
    $db->{dbEscapeCellName($cellName)}->{plt} = \%pltResult;
}

sub postProcessResults 
{
    my ($cellName, $cmdLine, $db, $fileTimestamp, $timeoutOcurred) = @_;

    my %result;

    #printf("Plt Post processing cell $cellName\n");
    $result{cellName} = $cellName;
    $result{cellFileTimestamp} = $fileTimestamp;
    $result{outputDir} = File::Spec->rel2abs($cmdLine->{outputDir});
    $result{timeout} = $timeoutOcurred;

    # Version 0:
    #   - Not labeled in DB
    #   - Uses rte server/client which generates html pages
    # Version 1:
    #   - RTE client is now run, only rte client log and cell html page are generated
    # Version 2:
    #   - Adds rule coverage    #NOTE: Rule coverage is incorrect for version 2
    # Version 3:
    #   - Fixed rule coverage totalUncovered number
    # Version 4:
    #   - Specify CosimFail as NA if the cell doesn't have CSP
    # Version 5:
    #   - passed is now a string
    # Version 6:
    #   - Added csp_coverage
    $result{version} = "6";

    my $file = $cmdLine->{outputDir} . "/" . rteLib::replaceParenthesisWith_($cellName) . "/" .
                rteLib::replaceParenthesisWith_($cellName) .  ".output";

    my $skipDbUpdate = 0;

    my $H;
    my $line;

    # For some reason -e doesn't wan't any parenthesis in the filename to be escaped
    $result{failedToComplete} = 0;

    if(-e $file)
    {
        $file = rteLib::escapeParenthesis($file);

        open($H, "grep \"Simulation started at:\" $file |");
        $line = <$H>;
        if((defined $line) && $line =~ /Simulation started at: ([a-zA-Z0-9 :]+)/)
        {
            $result{runStartTime} = $1;
        }else
        {
            if(defined $line) {
                printf("Error parsing Simulation start time line: $line\n");
            }
            $result{runStartTime} = -1;
            $result{failedToComplete} = 1;
        }

        open($H, "grep \"Simulation ended at:\" $file |");
        $line = <$H>;
        if((defined $line) && $line =~ /Simulation ended at: ([a-zA-Z0-9 :]+)/)
        {
            $result{runEndTime} = $1;
        }else
        {
            if(defined $line) {
                printf("Error parsing Simulation end time line: $line\n");
            }
            $result{runEndTime} = -1;
            $result{failedToComplete} = 1;
        }

        open($H, "grep \"Simulation duration:\" $file |");
        $line = <$H>;
        if((defined $line) && $line =~ /Simulation duration: ([a-zA-Z0-9 ]+)/)
        {
            $result{runDurationTime} = $1;
        }else
        {
            if(defined $line) {
                printf("Error parsing Simulation duration line: $line\n");
            }
            $result{runDurationTime} = -1;
        }

        open($H, "grep \"java.lang.OutOfMemoryError\" $file |");
        $line = <$H>;
        if((defined $line) && $line =~ /java.lang.OutOfMemoryError/)
        {
            $result{memUsed} = -1;
            $result{outOfMemory} = 1;
            $result{failedToComplete} = 1;
        }else
        {
            open($H, "grep \"\\[INFO\\] Used Memory:\" $file |");
            $line = <$H>;
            if((defined $line) && $line =~ /\[INFO\] Used Memory: ([0-9]+Kb)/)
            {
                $result{memUsed} = $1;
                $result{outOfMemory} = 0;
            }else
            {
                if(defined $line) {
                    printf("Error parsing Used memory line: $line\n");
                }
                $result{memUsed} = 0;
                $result{outOfMemory} = 0;
                $result{failedToComplete} = 1;
            }
        }
    
        open($H, "grep \"\\[INFO\\] TOTAL COVERAGE:\" $file |");
        $line = <$H>;
        if((defined $line) && ($line =~ /\[INFO\] TOTAL COVERAGE: ([0-9\.]+)/))
        {
                $result{coverage} = $1/100;
        }else
        {
            # There are cases (rte_ignore) where coverage will not be printed
            $result{coverage} = 0;
        }

        open($H, "grep \"Performing reset stress test\" $file |");
        $line = <$H>;
        if((defined $line) && ($line =~ /Performing reset stress test/))
        {
                $result{resetStressRun} = 1;
        }else
        {
            $result{resetStressRun} = 0;
        }


        open($H, "grep -A1 \"Cosim Failure on:\" $file |");
        $line = <$H>;
        if((defined $line) && ($line =~ /CoSim Failure on/))
        {
            $result{cosimFail} = 1;
        }else
        {
            my $cspPresent = 0;
            open($H, "grep \"INFO] Cosim spec:\" $file |");
            while(<$H>)
            {
                if($_ =~ /\{[a-z,]+ \| [a-z,]+/)
                {
                    $cspPresent = 1;
                }
            }
            
            if(!$cspPresent)
            {
                # No csp present, so mark it NA
                $result{cosimFail} = "NA";
            }else
            {
                $result{cosimFail} = 0;
            }
        } 

        open($H, "grep -A1 \"Error instantiating\" $file |");
        $line = <$H>;
        if((defined $line) && ($line =~ /Error instantiating/))
        {
            $result{exceptionOnInstantiation} = 1;
        }else
        {
            $result{exceptionOnInstantiation} = 0;
        }

        open($H, "grep \"^Status: \" $file |");
        $line = <$H>;
        if((defined $line) && ($line =~ /^Status: ([A-Z]+)/))
        {
            $result{passed} = $1;
        }else
        {
            $result{passed} = -1;
        }
        $result{rteIgnore} = getSummaryField($file, "RTE_IGNORE: true");
        $result{cosimWillFail} = getSummaryField($file, "COSIM_WILL_FAIL: true");
        $result{fragment} = getSummaryField($file, "FRAGMENT: true");
        $result{cspErrors} = getSummaryField($file, "CSPERROR: true");
        $result{unimplementable} = getSummaryField($file, "UNIMPLEMENTABLE: true");
#        $result{cellsCosimulated} = getSummaryField($file, "");
        $result{missingTests} = getSummaryField($file, "NOTESTS: true");
        $result{missingNTPC} = getSummaryField($file, "NTPC_SPEC: false");
        $result{missingCycleNode} = getSummaryField($file, "CYCLE_NODE: false");
        $result{failedNTPCTarget} = getSummaryField($file, "NTPC_STAT: false");
        $result{failedNTPCTargetModifiedDelay} = getSummaryField($file, "NTPC_STAT_ESTIMATED_DELAY: false");


        if($result{failedToComplete} == 0)
        {
            open($H, "grep \"Uncovered rules report file: \" $file |");
            $line = <$H>;
            if((defined $line) && ($line =~ /Uncovered rules report file: /))
            { 
                $result{ruleCoverage} = rteLib::computeRuleCoverage($cellName, $cmdLine);
            }else
            {
                $result{ruleCoverage} = "-1";
            }

            open($H, "grep \"CSP coverage database: \" $file |");
            $line = <$H>;
            if((defined $line) && ($line =~ /CSP coverage database:/))
            { 
                $result{cspCoverage} = rteLib::computeCspCoverage($cellName, $cmdLine);
            }
        }else
        {
            $result{ruleCoverage} = -1;
            $result{cspCoverage} = -1;
        }
    }else
    {
        printf("ERROR unable to find result file %s\n", $file);
        $result{coverage} = 0;
        $result{memUsed} = 0;
        $result{runDurationTime} = -1;
        $result{runStartTime} = -1;
        $result{runEndTime} = -1;
        $result{resetStressRun} = -1;
        $result{unimplementedCells} = -1;
        $result{passed} = 0;
        $result{rteIgnore} = -1;
        $result{cosimWillFail} = -1;
        $result{fragment} = -1;
        $result{cspErrors} = -1;
        $result{unimplementable} = -1;
        $result{cellsCosimulated} = -1;
        $result{missingTests} = -1;
        $result{exceptionOnInstantiation} = -1;
        $result{missingNTPC} = -1;
        $result{missingCycleNode} = -1;
        $result{failedNTPCTarget} = -1;
        $result{failedNTPCTargetModifiedDelay} = -1;
    }

    $file = $cmdLine->{outputDir} . "/" . rteLib::replaceParenthesisWith_($cellName) . "/modules/" .
                join('/', split(/\./, $cellName)) . ".html";
    if(-e $file)
    {
        $file = rteLib::escapeParenthesis($file);

        open($H, "grep -A1 \"Unimplemented Cells:\" $file |");
        $line = <$H>;
        if((defined $line) && ($line =~ /Unimplemented Cells:/))
        {
            $line = <$H>;
            if((defined $line) && ($line =~ /None\./))
            {
                    $result{unimplementedCells} = 0;
            }else
            {
                $result{unimplementedCells} = 1;
            }
        }else
        {
                $result{unimplementedCells} = -1;
        }
    }


    printf("Results for $cellName\n");
    print Dumper(\%result); 
    if($skipDbUpdate == 1) 
    {
        printf("Skipping cell database update due to above failure\n");
    }else
    {
        $db->{rteLib::dbEscapeCellName($cellName)} = \%result;
    }
}

sub getSummaryField
{
    my ($file, $grepString) = @_;

    my $H;
    open($H, "grep \"$grepString\"  $file |");
    my $line = <$H>;
    if((defined $line))
    {
        return 1;
    }else
    {
        return 0;
    }
}   

sub memConvert
{
    my ($str) = @_;

    if($str =~ /([0-9]+)k/i)
    {
        return $1;
    }elsif($str =~ /([0-9]+)m/i)
    {
        return $1 * 1024;
    }elsif($str =~ /([0-9]+)g/i)
    {
        return $1 * 1024*1024;
    }elsif($str =~ /([0-9]+)/)
    {
        return $1/1024.0;
    }
    return -1;
}

# Returns 1 if v1 is > v2
# Returns 0 if v1 == v2
# Returns -1 if v1 is < v2
sub memCompare
{
    my ($v1, $v2) = @_;

    if($v1 =~ /([0-9]+)k/i)
    {
        my $x = $1;
        if($v2 =~ /([0-9]+)k/i)
        {
            if($x > $1) { return 1; }
            elsif($x == $1) { return 0; }
            else { return -1; }
        }else
        {
            return -1;
        }
    }elsif($v1 =~ /([0-9]+)m/i)
    {
        my $x = $1;
        if($v2 =~ /([0-9]+)k/i)
        {
            return 1;
        }elsif($v2 =~ /([0-9]+)m/i)
        {
            if($x > $1) { return 1; }
            elsif($x == $1) { return 0; }
            else { return -1; }
        }else
        {
            return -1;
        }
    }elsif($v1 =~ /([0-9]+)g/i)
    {
        my $x = $1;
        if($v2 =~ /([0-9]+)g/i)
        {
            if($x > $1) { return 1; }
            elsif($x == $1) { return 0; }
            else { return -1; }
        }else
        {
            return 1;
        }
    }
}

# Perform: min(2*v1, v2)
sub memDouble
{
    my ($v1, $v2) = @_;

    $v1 =~ /([0-9]+)([kmg])/i;

    my $newV1 = sprintf("%d%s", $1*2, $2);

    if(memCompare($newV1, $v2) == -1)
    {
        return $newV1;
    }else
    {
        return $v2;
    }
}

sub memRoundUpToGig
{
    my ($v1) = @_;

    $v1 =~ /([0-9]+)([kmg])/i;

    if(($2 eq "k") || ($2 eq "K"))
    {
        return sprintf("%d%s", ($1/1000/1000) + 1, "G");
    }elsif(($2 eq "m" || $2 eq "M"))
    { 
        return sprintf("%d%s", ($1/1000) + 1, "G");
    }elsif(($2 eq "g") || ($2 eq "G"))
    {
        $1 = (int($1) == $1) ? $1 : int($1)+1;
        return sprintf("%d%s", $1, "G");
    }
}

# Summarize the result of a particular cell, and return a hash with the
# following keys: coverage, cspCoverage, ruleCoverage, cosimFail, passFail,
# unimplementedCells, resetStressRun
sub getResults
{
    my ($data) = @_;
    
    my %result;

    $result{coverage} = $data->{coverage}*100;

    if(defined($data->{version}) && $data->{version} >= 6)
    {
        if(defined($data->{cspCoverage}) && $data->{cspCoverage} >= 0)
        {
            $result{cspCoverage} = $data->{cspCoverage}*100;
        }elsif(defined($data->{cspCoverage}) && $data->{cspCoverage} == -2)
        {
            if( defined($data->{cosimFail}) && ($data->{cosimFail} eq "NA"))
            {
                $result{cspCoverage} = "NA";
            }else
            {
                $result{cspCoverage} = "error";
            }
        }else
        {
                $result{cspCoverage} = "Unknown";
        }
    }else
    {
        $result{cspCoverage} = "Unknown";
    }

    # Version 2 of the output file has a bug where the totalUncovered rule count
    # is not computed correctly.
    if(defined($data->{version}) && $data->{version} >= 3)
    {
        if(defined($data->{ruleCoverage}) && $data->{ruleCoverage} != -1)
        {
            $result{ruleCoverage} = $data->{ruleCoverage}*100;
        }elsif(defined($data->{ruleCoverage}))
        {
                $result{ruleCoverage} = "Unknown";
        }else
        {
                $result{ruleCoverage} = "Unknown";
        }
    }else
    {
        $result{ruleCoverage} = "Unknown";
    }

    if( defined($data->{cosimFail}) && ($data->{cosimFail} eq "1"))
    {
        $result{cosimFail} = "FAIL";
    }elsif( defined($data->{cosimFail}) && ($data->{cosimFail} eq "NA"))
    {
        $result{cosimFail} = "NA: No CSP";
    }elsif( defined($data->{cosimFail}))
    {
        $result{cosimFail} = "Pass";
    }else
    {
        $result{cosimFail} = "Unknown";
    }

    if(defined($data->{outOfMemory}) && $data->{outOfMemory} == 1)
    {
        $result{passFail} = "FAIL: Out of Memory";
    }elsif($data->{exceptionOnInstantiation} == 1)
    {
        $result{passFail} = "FAIL: Exception on Instantiation";
    }elsif(defined($data->{timeout}) &&
        (!$data->{timeout} eq "") &&
        $data->{timeout} == 1)
    {
        $result{passFail} = "FAIL: Job Timeout";
    }elsif( (defined($data->{failedToComplete})) &&
        ($data->{failedToComplete} == 1) )
    {
        $result{passFail} = "FAIL: Failed To Complete";
    }elsif($data->{passed} eq "FAILED")
    {
        $result{passFail} = "FAIL: RTE Failed";
    }elsif($data->{missingTests} == 1)
    {
        $result{passFail} = "FAIL: Missing Tests";
    }elsif( ($data->{missingCycleNode} == 1) &&
        ($data->{missingNTPC} == 1) )
    {
        $result{passFail} = "FAIL: Missing NTPC/CycleNode";
    }elsif( ($data->{failedNTPCTarget} == 1) &&
        ($data->{failedNTPCTarget} == 1) )
    {
        $result{passFail} = "FAIL: Failed NTPC Spec";
    }else
    {
        $result{passFail} = "Pass";
    }

    if( (defined($data->{unimplementedCells})) &&
        ($data->{unimplementedCells} == 1) )
    {
        $result{unimplementedCells} = "FAIL";
    }elsif(  !(defined($data->{unimplementedCells})) ||

             ((defined($data->{unimplementedCells})) &&
              ($data->{unimplementedCells} == -1) )
          )
    {
        $result{unimplementedCells} = "Unknown";
    }else
    {
        $result{unimplementedCells} = "Pass";
    }

    if( (defined($data->{resetStressRun})) && ($data->{resetStressRun} == 1))
    {
        $result{resetStressRun} = "Pass";
    }elsif(defined($data->{resetStressRun}))
    {
        $result{resetStressRun} = "FAIL";
    }else
    {
        $result{resetStressRun} = "Unknown";
    }

    return \%result;
}
1;
