#! /usr/intel/pkgs/perl/5.14.1/bin/perl


BEGIN {
    if(defined($ENV{RTL_PROJ_BIN}) && -e "$ENV{RTL_PROJ_BIN}/perllib/ToolConfig.pm" &&
       (defined $ENV{MODEL_ROOT} && -e "$ENV{MODEL_ROOT}/cfg/ToolData.pm")) {
        unshift @INC, "$ENV{RTL_PROJ_BIN}/perllib";
        require ToolConfig;
        if (ToolConfig::ToolConfig_tool_exists('puni')) {
            $puniHome = ToolConfig::ToolConfig_get_tool_path('puni');
            $puniVersion = ToolConfig::ToolConfig_get_tool_version('puni');
            $puniHome =~ s/$puniVersion//;
        }
    } 
    if (!defined $puniHome) {
        if (defined ($ENV{PUNI_HOME})) {
            $puniHome = $ENV{PUNI_HOME};
        } elsif (defined ($ENV{RTL_PROJ_TOOLS})) {
            $puniHome = "$ENV{RTL_PROJ_TOOLS}/puni/master";
        } else {
            print "-E- env variable PUNI_HOME not set\n";
            exit(1);
        }
    }
   
    if (!defined $puniVersion) {
        if (defined ($ENV{PUNI_VERSION})) {
            $puniVersion = $ENV{PUNI_VERSION};
        } else {
            $puniVersion = "latest";
        }
    }

}

my $args = join (' ', @ARGV);

use Getopt::Long;
Getopt::Long::Configure("pass_through");
my $cwd = `pwd`;
my $model_root = $cwd;
my $checker_mode = 0;
my $prefix = "";

GetOptions ("model_root=s" => \$model_root,
            "checker_mode" => \$checker_mode,
            "prefix=s"     => \$prefix);

if ($model_root =~ /(\/)$/) {
    $model_root =~ s/(.*)$1/$1/;
}
print "Model Root set as $model_root\n";

chdir $model_root;

my $output_dir = "$model_root/tools/uniquification/outputs";
if (!-e $output_dir) {
    system("mkdir -p $output_dir");
}
my $input_dir = "$model_root/tools/uniquification/inputs";

my $cmd = "$puniHome/$puniVersion/PUNI.pl $args";
print "Running $cmd\n";
my $status = system($cmd);

if($checker_mode and $status != 0) {
    if (($status >> 8) == 4) {
       $status = check_puni();
    }
}

chdir $cwd;
exit ($status >> 8);


sub check_puni {
    my $status = 0;

    my @violations;
    my @lintra_mode_violations;
    my @waivers;
    my @unwaived_violations;
    
    my $report_file = "${prefix}_notuniq.txt";
    # parse report file
    parse_puni_report("$output_dir/$report_file", \@violations);
    convert_violations_to_lintra_xml(\@violations, \@lintra_mode_violations);
    $status = run_lintra(\@lintra_mode_violations);
    return $status;
}
sub parse_puni_report {
    my ($report_file, $violations) = @_;
    my $status = 0;
    # parse report file
    if(!-e $report_file) {
        print("PUNI produced report file $report_file doesn't exist\n");
    }
    open(RF, "$report_file") or die("Couldn't open report file $report_file for reading\n");
    while(<RF>) {
        my $line = $_;
        next if($line =~ 
                /^(\s+|MODEL_ROOT\s*\:|Prefix\s*\:|SUMMARY\s*\:|\#|MODULES\:|MACROS\:|Files\:|PACKAGES\:|COMPILER DIRECTIVES\:|SCOPE\:|LIBRARY NAMES\:)/);
        chomp($line);
        push @{$violations}, $line;
    }
    close(RF);
}

sub convert_violations_to_lintra_xml {
    my ($violations, $lintra_mode_violations) = @_;
    my $header =<<HEADER;
<?xml version="1.0" encoding="UTF-8"?>
<LINTRA_VIOLATIONS>
 <run_info top="puni"/>
HEADER

    my $footer =<<FOOTER;
</LINTRA_VIOLATIONS>
FOOTER

    push @{$lintra_mode_violations}, $header;

    my $id = 0;
    foreach my $violation (@{$violations}) {
        $violation =~ s/$model_root\///;
        push @{$lintra_mode_violations}, "<violation id=\"puni\" file=\"na\" line=\"0\" message=\"$violation\" key=\"$violation\" severity=\"Error\" group=\"PuniRules\" design_id=\"$prefix\" waiver=\"\" />\n";
        $id++;
    }
    push @{$lintra_mode_violations}, $footer;
}

sub write_lintra_violations_xml {
    my ($violations_file, $lintra_mode_violations) = @_;
    open(FILE, ">$violations_file") or die("Couldn't open file $violations_file for writing");
    foreach my $violation (@{$lintra_mode_violations}) {
        print FILE "$violation";
    }
    close(FILE);
}

sub run_lintra {
    my ($lintra_mode_violations) = @_;
    my $violations_file = "$output_dir/${prefix}_puni_lintra_all_violations.xml";
    my $waiver_path = "$input_dir/puni_lintra";
    my $cfg_file = "$input_dir/puni_lintra/LintraPuniConfig.xml";
    my $waiver_file = "puni_lintra_waiver_w.xml";
    my $unwaived_violations = "puni_lintra_unwaived_violations.xml";
    write_lintra_violations_xml($violations_file, $lintra_mode_violations);

    # set up lintra env vars
    ToolConfig::ToolConfig_setup_tools("puni/lintra", 1, undef);

    # lintra batch mode run applies waivers
    # -lr = load violations report
    # -wf = waiver file regex or waiver file (use your waiver file here)
    # -wd = waiver file dir
    # -xrn = new violations report (use one from QC entry)
    my $cmd = &ToolConfig::ToolConfig_get_tool_exec('puni/lintra') . " -top puni -lr $violations_file -wf $waiver_file -wd $waiver_path -od $output_dir -xrn $unwaived_violations -pfn -cfg $cfg_file";
    print("Running command: $cmd\n");
    my $status = system($cmd);
    $status=$status >> 8;
    return $status;
}


