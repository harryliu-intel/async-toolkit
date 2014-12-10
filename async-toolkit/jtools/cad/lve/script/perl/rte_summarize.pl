#!/usr/intel/bin/perl -l
# This script calculates the total csp coverage and the total rule coverage
# The computeCspCovergage and the computeRulecoverage calculate the total csp and rule coverage respectively
# It writes into a file called rte.raw.summary. This script is called in the env.mk file at rte.raw.summary rule.

use strict;
use Getopt::Long;

my $cast_dir="";
my $spec_dir="";
my $fqcn="";
my $lve_dir="";
my $work_dir="";
my $fulcrum_pdk_root="";
my $dir="";

GetOptions (
    "cast-dir=s" => \$cast_dir,
    "spec-dir=s" => \$spec_dir,
    "fqcn=s" => \$fqcn,
    "work-dir=s" => \$work_dir,
    "lve-dir=s" => \$lve_dir,
    "fulcrum-pdk-root=s" => \$fulcrum_pdk_root,
    "dir=s" =>\$dir,
);

my $fqcn_path = $fqcn;
$fqcn_path =~ s/\./\//g;
$dir =~ s/\=//g;
my $cspCoverage;
my $ruleCoverage;

$cspCoverage=computeCspCoverage($dir);
$ruleCoverage= computeRuleCoverage($dir);

if($cspCoverage == -1){
    $cspCoverage = "Unknown";
}
if($ruleCoverage == -1){
    $ruleCoverage = "Unkown";
}
my $status = "N/A";
# Writing the csp Coverage and ruleCovergae in the file (status, task, fqcn and dir is not required) 
print "$status rte $fqcn $dir cspCoverage=$cspCoverage ruleCoverage=$ruleCoverage";


sub computeCspCoverage
{

    my ($rtePath) = @_;

    my $H;
   
    my $escrtePath = escapeParenthesis($rtePath);
    my $tmpDir = "$rtePath/total_csp";
    my $esctmpDir = escapeParenthesis($tmpDir);

    #Remove the directory if it already exists
    if(-e $tmpDir){
        system("rm -rf $esctmpDir");
        #printf ("\nRemoved directory in compute csp");
    }
    system("mkdir $esctmpDir");
    
    #Find the coverage file
    my $findcommand = "find $escrtePath -type f -print | grep '.csp_coverage.zip'";

    open($H, "$findcommand |");
    my $envName = "";
    my $filePath ="";
    my $cnt = 0;
    while(<$H>)
    {
        chomp($_);
        $_ =~ /(.*):(.*).csp_coverage.zip/;
        my $escfileName = escapeParenthesis($_);
        $filePath = $1;
        my $escfilePath = escapeParenthesis($filePath);
        $envName = $2;   

        # Unpack the csp_coverage.zip file
        my $cmd = sprintf("unzip -qqo $escfileName -d $esctmpDir");
        system($cmd);
       

        system("zcat $escfilePath:$envName.csp_misses.gz | grep -v ^Missed | sort | " .
                "sed 's/:/ /' | sed 's/(\\(.*\\)):/\\1/g' | sed 's/ \\+/ /g' > $esctmpDir/$envName.$cnt.csp_uncovered");
        system("cat $esctmpDir/probes | sort > $esctmpDir/$envName.$cnt.csp_total");
        system("comm -13 $esctmpDir/$envName.$cnt.csp_uncovered $esctmpDir/$envName.$cnt.csp_total > $esctmpDir/$envName.$cnt.csp_covered");
        
        my $covered = `cat $esctmpDir/$envName.$cnt.csp_covered | wc -l`; 
        my $uncovered = `cat $esctmpDir/$envName.$cnt.csp_uncovered | wc -l`; 
        my $total = `cat $esctmpDir/$envName.$cnt.csp_total | wc -l`; 
        chomp($covered); chomp($uncovered); chomp($total);


        if($total - $covered != $uncovered)
        {
            return -1;
        }
        $cnt ++;
    }

    #Sort the total and covered files
    system("cat $esctmpDir/*.csp_covered | sort -u > $esctmpDir/rte.total_csp_covered");
    system("cat $esctmpDir/*.csp_total | sort -u > $esctmpDir/rte.total_csp");

    my $totalCovered = `cat $esctmpDir/rte.total_csp_covered | wc -l`;
    my $probeCnt = `cat $esctmpDir/rte.total_csp | wc -l`;
    chomp($totalCovered);
    chomp($probeCnt);

    if($probeCnt <= 0)
    {
        return -1;
    }else
    {
        return sprintf("%0.2f", 1.0*100 * $totalCovered/$probeCnt);
    }
}

sub computeRuleCoverage
{
    my ($rtePath) = @_;
    my $H;
    my $escrtePath = escapeParenthesis($rtePath); 
    my $tmpDir = "$rtePath/total_rule";
    my $esctmpDir = escapeParenthesis($tmpDir);
    if(-e $tmpDir)
    {
        system("rm -rf $esctmpDir");
        #printf ("\nRemoved directory in computerule ");
    }
    system("mkdir $esctmpDir");

    my $uncoveredStr= "";
    my $coveredStr="";
    my $findcommand = "find $escrtePath -type f -print | grep '.uncovered_rules.gz' ";


    open($H, "$findcommand |");
    while(<$H>){
       chomp($_);
       $uncoveredStr = $uncoveredStr ."  ". $_;

    }

    $findcommand ="find $escrtePath -type f -print | grep '.covered_rules.gz' " ;


    open($H, "$findcommand |");
    while(<$H>){
       chomp($_);
       $coveredStr = $coveredStr ."  ". $_;

    }
    my $escuncoveredStr = escapeParenthesis($uncoveredStr);
    my $esccoveredStr = escapeParenthesis($coveredStr);
    # Create total sorted list of all rules
    my $cmd = sprintf("zcat $escuncoveredStr $esccoveredStr | sort -T $esctmpDir -u  > $esctmpDir/rte.total_rules");
    system($cmd);

    # Create total sorted list of covered rules
    $cmd = sprintf("zcat $esccoveredStr | sort -T $esctmpDir -u > $esctmpDir/rte.total_covered_rules");
    system($cmd);

    # Create total list of all uncovered rules (for designer to see)
    $cmd = sprintf("comm -13 $esctmpDir/rte.total_covered_rules $esctmpDir/rte.total_rules > $esctmpDir/rte.total_uncovered_rules");
    system($cmd);

    my $total = `cat $esctmpDir/rte.total_rules | wc -l`;
    my $covered = `cat $esctmpDir/rte.total_covered_rules | wc -l`;

    system($cmd);

    if($total <= 0 || $covered < 0)
    {
        return -1;
    }else
    {   
        return sprintf("%0.2f", 1.0 *100* $covered / $total);
    }
}

# Replace ( ) and , with \( and \)
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


