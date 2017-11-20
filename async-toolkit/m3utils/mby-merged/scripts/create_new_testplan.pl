#!/usr/intel/pkgs/perl/5.14.1/bin/perl
# ------------------------------------------------------------------------
#  Parse a preexisting CSV file and extract the following information 
#  Feature Sub-Feature ID Reference Owner Description Milestone Coverage Description Check Description Generation Description Weight Source 
# Build a new HSDES test plan using the input csv file 
# ------------------------------------------------------------------------

use UsrIntel::R1;

use lib "$ENV{RTL_CAD_ROOT}/dt//HSDES_API/1.0.27_perl/lib";

use HSDES::Api;
use Data::Dumper;
use Getopt::Long;
use Text::CSV;

# ------------- User needs to modify this code for every IP -------------
my $project_test_plan_base_id = "1303395595"; # PROJECT MST TESTPLAN BASE 
my $cafe_test_content_base_id = "1303395600"; # TEST CONTENT CAFE BASE 
my $test_plan_id              = "1303422771";  # gets overriden by call to create_test_plan
my $family                    = "MST";
my $grp_owner_team            = "ip.lan";
my $owner                     = "vkshetra";
my $tenant_name               = "server_platf_comms";
my $ip_category               = "Comms_Modem_Networking";
my $hsdes_server              = "PREPRODUCTION";
my $release                   = "CPM MST";
my $test_plan_title           = "TI DEMO";
my $test_plan_desc            = "TI DEMO TP";

my $START_ROW_NUM = 4;
my $HEADER_MAX_COLS = 15;
my $LAST_ROW_NUM = 14;
my $COL_OFFSET = 3;
my $DESC_LINE = 2;
# ------------- User needs to modify this code -------------

my $header_desc;
my @header_line;
my $row_count = 1;
my @rows;
my $row;
my $feature;
my $last_feature;
my $subfeature;
my $desc;
my $reference;
my $owner_team;
my $cov_desc;
my $check_desc;
my $gen_desc;
my $tcd;
my $tcd_title;
my $tc;
my @tc_array;
my @tc_array_generated;
my $lc;
my $tc_first;

my $feature_id;
my $tcd_id;
my $tc_id;
my $testcase_id;
my $weight;
my $milestone;
my $new_milestone;
my $new_priority;
my $test_cov_id;
my $check_cov_id;

###################################
# Obtain kerberos token
###################################
system('kinit -R >/dev/null 2>&1');
if ( $? ) {
        system('kinit');
}

my $api = HSDES::Api->new();

# PRODUCTION is production
# PREPRODUCTION is staging
$api->init($hsdes_server);

Getopt::Long::Configure( "no_ignore_case",
    "prefix_pattern=(--|-)" );

my @optionSpec = (
   'help|h',
   'display');

GetOptions( \%options, @optionSpec );
my $access_database;

if (defined $options{display} )
{
   $access_database = 0;
} else {
   $access_database = 1; #default
}

usage() if ( defined $options{'help'} );

#############################################################################
sub usage {
    print "Usage: create_new_testplan.pl [options] ...\n";
    print "Options:\n";
    print "   -help | -h  : display this information\n";
    print "   -display    : Do not access PVIM database ; Just display contents only\n";
    exit(0);
}
#############################################################################


open FH1, ">csv_test_plan.log" or die $!;

# ----------- Read CSV file and extract info, build rows data structure ------------------
my $csv = Text::CSV->new ( { binary => 1 } )  # should set binary attribute.
                or die "Cannot use CSV: ".Text::CSV->error_diag ();
 
open my $fh, "<:encoding(utf8)", "test.csv" or die "test.csv: $!";
while ( $row = $csv->getline( $fh ) ) {
    #$row->[2] =~ m/pattern/ or next; # 3rd field should match
    if ( $row_count >= $START_ROW_NUM && $row_count <= $LAST_ROW_NUM) {
      push @rows, $row;
    }
    $header_desc = $row->[$COL_OFFSET+1];
    if ( $row_count > $START_ROW_NUM && $row_count <= $LAST_ROW_NUM) {
       #print "$header_desc\n";
       #print "--->@$row\n";
    }
    if ($row_count == $DESC_LINE ) {
       #print "$header_desc\n";
       #print "================";
       for (my $i=1;$i<$HEADER_MAX_COLS;$i++) {
          $header_desc = $row->[$i];
          #print "$header_desc ";
          push @header_line,$header_desc;
       }
       #print "\n";
    }

    $row_count = $row_count+1;
}
$csv->eof or $csv->error_diag();
close $fh;
 
$csv->eol ("\r\n");
#print "@header_line\n";

# ------------ Parse the row array ref and create Feature,TCD,TC in HSDES -----
# -- Feature + Subfeature (CSV) -> Feature in HSDES 
# -- Desc (CSV) -> TCD in HSDES 
# -- Gen Desc (CSV) -> TC in HSDES 

# 

&create_test_plan;

$row_count = 1;
foreach $row (@rows) {
  if ( @$row[$COL_OFFSET]) { # check feature exists 
    $feature = @$row[$COL_OFFSET];
    $last_feature = $feature;
    print "Detected Feature $feature ..\n";
    print FH1 "Detected Feature $feature ..\n";
  } else {
    $feature = "";
    print "Using previous Feature $feature ..\n";
    print FH1 "Using previous Feature $feature ..\n";
  }
  $subfeature = @$row[$COL_OFFSET+1] eq "" ? "EMPTY_TITLE" : @$row[$COL_OFFSET+1];
  if ($feature) {
    print "         >>> Adding feature $feature as Feature Folder \n";
    print FH1 "         >>> Adding feature $feature as feature Folder\n";
    $feature_id = &create_feature($feature);
  }
  $tcd_title = @$row[$COL_OFFSET+2] eq "" ? "EMPTY_TITLE" : @$row[$COL_OFFSET+2];
  $reference = @$row[$COL_OFFSET+3];
  $owner_team = $grp_owner_team;
  $desc = @$row[$COL_OFFSET+5];
  $milestone = @$row[$COL_OFFSET+6];
  $cov_desc = @$row[$COL_OFFSET+7];
  $check_desc = @$row[$COL_OFFSET+8];
  $gen_desc = @$row[$COL_OFFSET+9];
  $weight = @$row[$COL_OFFSET+10];
  $new_milestone = &parse_milestone($milestone);
  $new_priority = &parse_priority($weight);

  if ( $desc )  {
    print "         >>> Adding subfeature $subfeature as TCD \n";
    print FH1 "         >>> Adding feature $subfeature as TCD \n";
    print FH1 "         +++ Adding Desc \n-- $desc\n--\n";
    my $testcase_name = $subfeature;
    $testcase_name =~ s/\s/_/g;
    $testcase_name .= "_testcase";
    print "         +++ Adding testcase $testcase_name\n";
    print "         +++ Adding milestone $new_milestone\n";
    print "         +++ Adding weight $new_priority\n";
    print FH1 "         +++ Adding testcase $testcase_name\n";
    if ( $feature_id && ($access_database == 1)) {
      $tcd_id = &create_tcd($feature_id,$tcd_title,$desc,$feature,$subfeature,$reference,$owner_team,$milestone,$weight);
      $testcase_id = &create_testcase($tcd_id,$gen_desc,$subfeature,$owner_team,$owner,$milestone,$weight);
      $test_cov_id = &create_test_cov_monitor($tcd_id,"COV::$tcd_title",$cov_desc,$feature,$subfeature,$owner_team,$owner,$milestone,$weight);
      $check_cov_id = &create_test_cov_monitor($tcd_id,"COV CHECK::$tcd_title",$check_desc,$feature,$subfeature,$owner_team,$owner,$milestone,$weight);
    }
  } 

# ---- TC content generation commented out ---
#  $gen_desc = @$row[$COL_OFFSET+9];
#  if ( $gen_desc) {
#    print "         %%% Adding Test Content for \n&&& $gen_desc\n&&&\n";
#    print FH1 "         %%% Adding Gen Desc \n&&& $gen_desc\n&&&\n";
#    @tc_array = split(/\d+\./, $gen_desc);
#    print FH1 "         %%% Parsing Gen Desc for Individual TC's\n";
#
#    $tc_first = "";
#    @tc_array_generated = ();
#    $array_count = scalar(@tc_array);
#    if ( $array_count > 1)  {
#      $lc=0;
#      $tc_first = $tc_array[$lc];
#      shift @tc_array;
#      print FH1 "             **** TC common is $tc_first\n";
#      foreach $individual_tc (@tc_array){
#        push (@tc_array_generated, $tc_first . $individual_tc);
#      }
#    } else {
#        @tc_array_generated = @tc_array;
#    }
#    $lc = 0;
#    foreach $individual_tc (@tc_array_generated){
#       print FH1 "             **** TC is $individual_tc\n";
#       #  &create_tc($tcd_title,$lc,$individual_tc);
#       $lc++;
#    }
#  }

}

# --------------------
# create_test_plan
# --------------------
sub create_test_plan() {
my $articleObj = $api->article();

# get a empty record structure
my $data = $articleObj->newArticle($tenant_name,"test_plan") or die $articleObj->getLastErrorMessage();

# now set some values
$data->{"status"}      = "open";
$data->{"title"}       = $test_plan_title;
$data->{"description"} = $test_plan_desc;
$data->{"component"}   = "";
$data->{"owner"}       = $owner;
$data->{"parent_id"}   = $project_test_plan_base_id;
$data->{"priority"}   = "3-medium";

$data->{"test_plan.owner_team"}   = $grp_owner_team;
$data->{"test_plan.ip_type"}      = "SIP";
#$data->{"test_plan.ip"}          = $family;
#$data->{"test_plan.ip_category"} = $ip_category;
#$data->{"test_plan.scope"}       = "ip";
$data->{"test_plan.native_type"} = "docx";

# if you DO NOT want to generate an email
$data->{send_mail} = "false";


# try to insert
my $newID;

if ( $access_database == 1) {
   $newID = $articleObj->insert($data) or die $articleObj->getLastErrorMessage();
}
print "create test plan Inserted ID $newID\n";
print FH1 "create test plan Inserted ID $newID\n";
$test_plan_id = $newID;

}

# ------------------
# create_feature()
# ------------------
sub create_feature() {
my ($myfeature)= @_;
my $articleObj = $api->article();

# get a empty record structure
my $data = $articleObj->newArticle($tenant_name,"test_plan") or die $articleObj->getLastErrorMessage();

# now set some values
$data->{"status"}      = "open";
$data->{"title"}       = $myfeature;
$data->{"description"} = $myfeature;
$data->{"component"}   = "";
$data->{"owner"}       = $owner;
$data->{"parent_id"}   = $test_plan_id;

#$data->{"release"}     = "0.0";
#$data->{"feature.motivation"}     = "";

$data->{"test_plan.nodetype"}   = "feature";
$data->{"test_plan.owner_team"}   = $grp_owner_team;
$data->{"test_plan.ip_type"}      = "SIP";

# if you DO NOT want to generate an email
$data->{send_mail} = "false";


# try to insert
my $newID;
if ( $access_database == 1) {
 $newID = $articleObj->insert($data) or die $articleObj->getLastErrorMessage();
}
print "create feature Inserted ID $newID\n";
print FH1 "create feature Inserted ID $newID\n";

return $newID;

}

# --------------
# create_tcd()
# --------------
sub create_tcd() {
my ($myfeature_id,$title,$desc,$myfeature,$mysubfeature,$myreference,$myowner_team,$milestone,$priority)= @_;
my $articleObj = $api->article();
my $new_priority;
my $new_milestone;

# get a empty record structure
my $data = $articleObj->newArticle($tenant_name,"test_case_definition") or die $articleObj->getLastErrorMessage();

$new_milestone = &parse_milestone($milestone);
$new_priority = &parse_priority($priority);

# now set some values
$data->{"status"}      = "open";
$data->{"title"}       = $subfeature;
$data->{"description"} = $desc;
$data->{"tag"}         = $title;
$data->{"test_case_definition.owner_team"} = $myowner_team;
$data->{"test_case_definition.spec_source"}= $myreference;
$data->{"test_case_definition.planned_val_environment"}= "simulation";
$data->{"component"}   = "";
$data->{"owner"}       = $owner;
$data->{"parent_id"}   = $myfeature_id;
#$data->{"family"}   = $family;

#$data->{"test_case_definition.ip_type"}      = "SIP";
$data->{"test_case_definition.category"}      = "functional";
$data->{"test_case_definition.required_by_milestone"} = $new_milestone;
$data->{"priority"}    = $new_priority;

# if you DO NOT want to generate an email
$data->{send_mail} = "false";


# try to insert
my $newID;
if ( $access_database == 1) {
 $newID = $articleObj->insert($data) or die $articleObj->getLastErrorMessage();
}
print "create TCD Inserted ID $newID\n";
print FH1 "create TCD Inserted ID $newID\n";

return $newID;

}

# ------------------
# create_testcase()
# ------------------
sub create_testcase() {
my ($mytcd_id,$my_gendesc,$mysubfeature,$owner_team,$owner,$milestone,$priority)= @_;
my $articleObj = $api->article();
my $new_priority;
my $new_milestone;

$new_milestone = &parse_milestone($milestone);
$new_priority = &parse_priority($priority);

# get a empty record structure
my $data = $articleObj->newArticle($tenant_name,"test_case") or die $articleObj->getLastErrorMessage();


# -------- testcase name conversion ----
my $testcase_name = $mysubfeature;
$testcase_name =~ s/\s/_/g;
$testcase_name .= "_testcase";

# now set some values
$data->{"status"}      = "open";
$data->{"title"}       = $testcase_name;
$data->{"description"} = $my_gendesc;
$data->{"component"}   = "";
$data->{"owner"}       = $owner;
$data->{"test_case.owner_team"}       = $owner_team;
$data->{"parent_id"}   = $mytcd_id;
$data->{"family"}      = $family;

#$data->{"test_case.required_by_milestone"} = $new_milestone;
#$data->{"priority"}    = $new_priority;

# if you DO NOT want to generate an email
$data->{send_mail} = "false";

# try to insert
my $newID;
if ( $access_database == 1) {
 $newID = $articleObj->insert($data) or die $articleObj->getLastErrorMessage();
}
print "create Testcase Inserted ID $newID\n";
print FH1 "create Testcase Inserted ID $newID\n";

return $newID;

}

# --------------------------
# create_test_cov_monitor()
# --------------------------
sub create_test_cov_monitor() {
my ($mytcd_id,$title,$my_gendesc,$feature,$mysubfeature,$owner_team,$owner,$milestone,$priority)= @_;
my $articleObj = $api->article();
my $new_priority;
my $new_milestone;

$new_milestone = &parse_milestone($milestone);
$new_priority = &parse_priority($priority);

# get a empty record structure
my $data = $articleObj->newArticle($tenant_name,"test_cov_monitor") or die $articleObj->getLastErrorMessage();

# now set some values
$data->{"status"}      = "open";
$data->{"title"}       = $title;
$data->{"description"} = $my_gendesc;
$data->{"component"}   = "";
$data->{"owner"}       = $owner;
$data->{"parent_id"}   = $mytcd_id;
$data->{"family"}      = $family;

$data->{"test_cov_monitor.nodetype"}    = "coverage_event";
$data->{"test_cov_monitor.owner_team"}  = $owner_team;

#test_cov_monitor.coverage_group
#test_cov_monitor.coverage_method
#test_cov_monitor.current_project
#test_cov_monitor.event_name
#test_cov_monitor.event_status
#test_cov_monitor.expression
#test_cov_monitor.flags
#test_cov_monitor.ip
#test_cov_monitor.ip_category
#test_cov_monitor.notes
#test_cov_monitor.origin_project
#test_cov_monitor.pch
#test_cov_monitor.scope
#test_cov_monitor.segment
#test_cov_monitor.subcovtype
#test_cov_monitor.system
#test_cov_monitor.trash
#test_cov_monitor.trash_initiated_date

# if you DO NOT want to generate an email
$data->{send_mail} = "false";

# try to insert
my $newID;
if ( $access_database == 1) {
 $newID = $articleObj->insert($data) or die $articleObj->getLastErrorMessage();
}
print "create TestcovMonitor Inserted ID $newID\n";
print FH1 "create TestcovMonitor Inserted ID $newID\n";

return $newID;

}

# --------------
# created_tc()
# --------------
sub create_tc() {
my ($title,$count,$desc)= @_;
my $articleObj = $api->article();

# get a empty record structure
my $data = $articleObj->newArticle($tenant_name,"test_content") or die $articleObj->getLastErrorMessage();

# now set some values
$data->{"status"}      = "open"; 
$data->{"title"}       = $title . "_$count";
$data->{"description"} = $desc;
$data->{"component"}   = "";
$data->{"owner"}       = $owner;
$data->{"parent_id"}   = $cafe_test_content_base_id;

$data->{"test_content.owner_team"}   = $grp_owner_team;
$data->{"test_content.category"}   = "functional";
$data->{"test_content.val_environment"}   = "cluster sim";


# if you DO NOT want to generate an email
$data->{send_mail} = "false";


# try to insert
my $newID;
if ( $access_database == 1) {
 $newID = $articleObj->insert($data) or die $articleObj->getLastErrorMessage();
}
print " create TC Inserted ID $newID\n";
print FH1 " create TC Inserted ID $newID\n";

return $newID;

}

# ----------------
# parse_priority
# ----------------
sub parse_priority() {
my ($priority)= @_;
my $newpriority;

  if ( $priority >= 0 && $priority <= 3) {
       $newpriority = "4-low";
  } elsif ( $priority >= 4 && $priority <= 6) {
       $newpriority = "3-medium";
  } elsif ( $priority >= 7 && $priority <= 8) {
       $newpriority = "2-high";
  } elsif ( $priority >= 9 && $priority <= 10) {
       $newpriority = "1-showstopper";
  }

return $newpriority;
}

# --------------------
# parse_milestone
# --------------------
sub parse_milestone() {
my ($milestone)= @_;
my $newmilestone;

   #$newmilestone = $milestone;
   #CPM 1p8.ip_rtl_0p5.ip.cpm

   if ( $milestone =~ m/(\d\.\d)/) {
      $newmilestone = $1;
      $newmilestone =~ s/\./p/;
      $newmilestone = $release . "." . "ip_rtl_" . $newmilestone . "." . $grp_owner_team . ".";
   }

return $newmilestone;
}


close FH1;
