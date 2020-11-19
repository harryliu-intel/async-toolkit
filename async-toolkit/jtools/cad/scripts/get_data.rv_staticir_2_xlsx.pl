#!/usr/intel/bin/perl5.26.1
#################################################################
#
# Program : get_data.rv_staticir_2_xlsx.pl
#
# Version : 1.1
#
# Editor : Ralf Goettsche
#
# Last changes : 19.11.2020
#
#################################################################
#
# Task :
#   Collection of Redhawk's static ir run results out of
#   regression folder and storage in Excel file.
#
#################################################################
#
# Call:
#   get_data.rv_staticir_2_xlsx.pl -srcdir <dir> -output <dir>
# e.g.
#   get_data.rv_staticir_2_xlsx.pl -srcdir ../apr.latest
#   get_data.rv_staticir_2_xlsx.pl -srcdir apr.latest -output rv_summary
#
#################################################################
#
# Used module-files (.pm):               included packages:
# ------------------------               ------------------
#
#################################################################
  

BEGIN {
   use File::Basename;
   use Cwd;
   use File::Path;
   use File::Copy;
   use Getopt::Long; 
   use Time::Local;
   use POSIX; 
   use Excel::Writer::XLSX;
   
   # For debugging purpose
   # use Data::Dumper;
}


### Defining run options
# Define timestamp
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
$year += 1900;
$mon++;
my $date = sprintf("%04d_%02d_%02d", $year,$mon, $mday);

# Define default settings for options
my $srcdir = "";
my $output = ".";
my $mode = "static";
my $xlsxfile = "RV_Summary_$date.xlsx";
my $workbook = "";
my $worksheet = "";
my $xlsxline = 4;

# Read call-defined options
if(!(GetOptions(
         "srcdir=s"   => \$srcdir,
         "output:s"   => \$output,
         "xlsxfile:s" => \$xlsxfile,
         "help:s"     => \&help
   ))) {
  &help();
}

### Reading out main RV data and storing them into excel file
print ("\nINFO: Reading RV rundirs under '$srcdir' and putting RV result extract under \'$output/\'!\n\n");

# Preparing Excel file and header of data sheet
print ("INFO: Preparing Excel sheet '$output/$xlsxfile'!\n\n");

$workbook  = Excel::Writer::XLSX->new("$output/$xlsxfile");
die "ERROR: Problems creating new Excel file: $!" unless defined $workbook;

$worksheet = $workbook->add_worksheet($date);
$worksheet->add_write_handler(qr[\w], \&store_string_widths);

$xlsxline = prep_xlsx($workbook, $worksheet, $xlsxline);

# Get all the regression run dirs
opendir(DIR, $srcdir);
my @srcdir_arr = ();
while (our $dir = readdir(DIR)) {
    if ($dir =~ m/\.cdswd/) {
        $dir =~ s/(.*)\.cdswd/$1/;
        push (@srcdir_arr, $dir);
    }
}      
closedir DIR;

# Extract most important RV data and store the result in Exel sheet
foreach my $dir (sort @srcdir_arr) {
    my $cell = "";
    my $rvdir = `ls -d ${srcdir}/${dir}.cdswd/temp/route/*/proteus/rv/staticir_run 2>/dev/null`;
    chomp($rvdir);
    if (-d $rvdir) {
        # Get gds2 name of block
        my $proteus_config = `ls ${srcdir}/${dir}.cdswd/temp/route/*/proteus.config`;
        chomp(${proteus_config});
        if (-f $proteus_config) {
            # Get full block name
            my $fullcell = `grep -- --cell= ${proteus_config}`;
            chomp(${fullcell});
            if (length($fullcell) > 0) {
                # Extract name part
                my @fullcell_part=split(/\./, $fullcell);
                $cell = $fullcell_part[length($fullcell_part) - 2];
                # Transform it to gds2
                $cell = rename2gds($cell);
            } else {
                print("WARNING: No cell definition in ${proteus_config}! Continuing!\n");
                next;
            }
        } else {
            print("WARNING: No proteus.config file! Continuing!\n");
            next;
        }
        # Extract data and storage in Excel sheet
        print("INFO: Extracting data for IP '$dir'... \n");
        $xlsxline = get_data($mode, $rvdir, $cell, $workbook, $worksheet, $xlsxline);
    } else {
        print("WARNING: No rv dir '${srcdir}/${dir}.cdswd/temp/route/*/proteus/rv/staticir_run'! Continuing!\n");
    }
}

# Adopt cell widths inside Excel file for better readability and close the file
autofit_columns($worksheet);
$workbook->close() or die "ERROR: Problems at closing file '$!'";




###############################################################################
#
# Subroutines
#
###############################################################################

sub prep_xlsx {
    ### Generates sheet header in excel file

    my ($workbook, $worksheet, $xlsxline) = @_;

    # Font definitions
    my $top_format = $workbook->add_format(
        font => 'Calibri',
        size => 11,
        color => 'black',
        bold => 1,
        align => 'center',
        valign => 'top',
        text_wrap => 1,
    );
    my $top_merge_format = $workbook->add_format(
        font => 'Calibri',
        size => 11,
        color => 'black',
        bold => 1,
        align => 'center',
        valign => 'top',
        text_wrap => 1,
    );
    my $tit_format = $workbook->add_format(
        font => 'Calibri',
        size => 11,
        bg_color => 'black',
        border => 1,
        border_color => 'white',
        color => 'white',
        bold => 1,
        align => 'center',
        text_wrap => 1,
    );

    # Top header
    $worksheet->merge_range($xlsxline,  3, $xlsxline,  6, "Connectivity", ${tit_format});
    $worksheet->merge_range($xlsxline,  7, $xlsxline, 12, "Data Integrity", ${tit_format});
    $worksheet->merge_range($xlsxline, 13, $xlsxline, 16, "Static IR", ${tit_format});

    $xlsxline++;

    # Column header
    $worksheet->write($xlsxline, 2,  "Inst.", ${top_format});
    $worksheet->write($xlsxline, 3,  "Worst min res\n(Ohm)", ${top_format});
    $worksheet->write($xlsxline, 4,  "Worst eff res\n(Ohm)", ${top_format});
    $worksheet->write($xlsxline, 5,  "Shorts", ${top_format});
    $worksheet->write($xlsxline, 6,  "Unconn. Inst.", ${top_format});

    $worksheet->write($xlsxline, 7,  "LIB Cov.\n(%)", ${top_format});
    $worksheet->write($xlsxline, 8,  "LEF Cov.\n(%)", ${top_format});
    $worksheet->write($xlsxline, 9,  "SPEF Cov.\n(%)", ${top_format});
    $worksheet->write($xlsxline, 10, "STA Cov.\n(%)", ${top_format});
    $worksheet->write($xlsxline, 11, "IPF Cov.\n(%)", ${top_format});
    $worksheet->write($xlsxline, 12, "Max res (VDD+VSS)\n(Ohm)", ${top_format});

    $worksheet->write($xlsxline, 13, "Total Power\n(W)", ${top_format});
    $worksheet->write($xlsxline, 14, "Worst Inst, Drop\n(mV)", ${top_format});
    $worksheet->merge_range($xlsxline, 15, $xlsxline, 16, "Nr of Inst Failed", ${top_merge_format});

    $xlsxline++;

    return $xlsxline;
}


sub get_data {
    # Generates one line entry for the according instance in the excel sheet

    my ($mode, $rvdir, $inst, $workbook, $worksheet, $xlsxline) = @_;
    my $flt_format = "";
    my $cnt_format = "";
    my $left_format = "";

    # Font definitions
    $str_format = $workbook->add_format(
        font => 'Courier New',
        size => 10,
        color => 'black',
        num_format => '@',
        align => 'right',
    );
    $cnt_format = $workbook->add_format(
        font => 'Courier New',
        size => 10,
        color => 'black',
        num_format => '0',
        align => 'right',
    );
    $flt_format = $workbook->add_format(
        font => 'Courier New',
        size => 10,
        color => 'black',
        num_format => '#,##0.00',
        align => 'right',
    );
    $left_format = $workbook->add_format(
        font => 'Calibri',
        size => 11,
        color => 'black',
        bold => 1,
        align => 'left',
    );
    $worksheet->write($xlsxline, 2,  $inst, ${left_format});

    # Reading and output of main static IR values
    if ($mode eq "static") {

        # Check if needed files have been generated by RV analysis
        my $error = 0;
        if (! -e "$rvdir/adsRpt/apache.gridcheck") { print ("    WARNING: File '$rvdir/adsRpt/apache.gridcheck' does not exist!\n"); $error = 1;}
        if (! -e "$rvdir/adsRpt/$inst.res_calc") { print ("    WARNING: File '$rvdir/adsRpt/$inst.res_calc' does not exist!\n"); $error = 1;}
        if (! -e "$rvdir/rhe.report") { print ("    WARNING: File '$rvdir/rhe.report' does not exist!\n"); $error = 1;}
        if (! -e "$rvdir/adsRHE/adsDWE/reports/perform_gridcheck.rpt") { print ("    WARNING: File '$rvdir/adsRHE/adsDWE/reports/perform_gridcheck.rpt' does not exist!\n"); $error = 1;};
        if (! -e "$rvdir/adsRpt/Static/$inst.inst.worst") { print ("    WARNING: File '$rvdir/adsRpt/Static/$inst.inst.worst' does not exist!\n"); $error = 1;};

        if ($error == 0) {
            ##################
            ### Connectivity
            ###

            my $res_eff_worst = 0.0;
            my $res_min_worst = 0.0;
            my $shorts = 0;
            my $unconn_inst = 0;

            $res_min_worst = `less $rvdir/adsRpt/apache.gridcheck | head -3 | tail -1 | awk '{print \$6}'`;
            $res_eff_worst = `less $rvdir/adsRpt/$inst.res_calc | head -8 | tail -1 | awk '{print \$1}'`;
            chomp($res_eff_worst);
            $shorts = `grep "Number Of Shorts" $rvdir/rhe.report | awk '{print \$5}'`;
            chomp($shorts);
            $unconn_inst = `grep "Number Of Unconnected Instances" $rvdir/rhe.report | awk '{print \$6}'`;
            chomp($unconn_inst);

            $worksheet->write($xlsxline, 3,  $res_min_worst, ${flt_format});
            $worksheet->write($xlsxline, 4,  $res_eff_worst, ${flt_format});
            $worksheet->write($xlsxline, 5,  $shorts, ${cnt_format});
            $worksheet->write($xlsxline, 6,  $unconn_inst, ${cnt_format});


            ##################
            ### Data Integrity
            ###

            my $lib_cov  = 0.0;
            my $lef_cov  = 0.0;
            my $spef_cov = 0.0;
            my $sta_cov  = 0.0;
            my $ipf_cov  = 0.0;
            my $max_res  = 0.0;

            #$lib_cov  = `grep "LIB Coverage" $rvdir/rhe.report | awk '{for(i=4; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            $lib_cov  = `grep "LIB Coverage" $rvdir/rhe.report | awk '{printf \$4}'`;
            chomp($lib_cov);
            #$lef_cov  = `grep "LEF Coverage" $rvdir/rhe.report | awk '{for(i=4; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            $lef_cov  = `grep "LEF Coverage" $rvdir/rhe.report | awk '{printf \$4}'`;
            chomp($lef_cov);
            #$spef_cov = `grep "SPEF Coverage" $rvdir/rhe.report | awk '{for(i=4; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            $spef_cov = `grep "SPEF Coverage" $rvdir/rhe.report | awk '{printf \$4}'`;
            chomp($spef_cov);
            #$sta_cov  = `grep "STA Coverage" $rvdir/rhe.report | awk '{for(i=4; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            $sta_cov  = `grep "STA Coverage" $rvdir/rhe.report | awk '{printf \$4}'`;
            chomp($sta_cov);
            #$ipf_cov  = `grep "IPF Coverage" $rvdir/rhe.report | awk '{for(i=4; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            $ipf_cov  = `grep "IPF Coverage" $rvdir/rhe.report | awk '{printf \$4}'`;
            chomp($ipf_cov);
            $max_res  = `grep "Max resistance" $rvdir/adsRHE/adsDWE/reports/perform_gridcheck.rpt | awk '{print \$6}'`;
            chomp($max_res);

            $worksheet->write($xlsxline, 7,  $lib_cov,  ${flt_format});
            $worksheet->write($xlsxline, 8,  $lef_cov,  ${flt_format});
            $worksheet->write($xlsxline, 9,  $spef_cov, ${flt_format});
            $worksheet->write($xlsxline, 10, $sta_cov,  ${flt_format});
            $worksheet->write($xlsxline, 11, $ipf_cov,  ${flt_format});
            $worksheet->write($xlsxline, 12, $max_res,  ${flt_format});


            ##################
            ### Static IR drop
            ###

            my $stat_tot_pwr         = 0.0;
            my $stat_worst_inst_drop = 0.0;
            my $stat_nr_fail_inst    = 0.0;

            #$stat_tot_pwr = `grep "TOTAL POWER" $rvdir/rhe.report | sort | head -1 | awk '{for(i=4; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            $stat_tot_pwr = `grep "TOTAL POWER" $rvdir/rhe.report | sort | head -1 | awk '{print \$4}'`;
            chomp($stat_tot_pwr);
            $stat_worst_inst_drop = `head -n 4 $rvdir/adsRpt/Static/$inst.inst.worst | grep -v "#" | awk '{print (\$2*1000)}'`;
            chomp($stat_worst_inst_drop);
            $stat_nr_fail_inst = `grep "No Of Instances Failed" $rvdir/rhe.report | grep -v ': NA' | awk '{print \$6}'`;
            $stat_nr_fail_inst_perc = `grep "No Of Instances Failed" $rvdir/rhe.report | grep -v ': NA' | awk ' " "  {for(i=7; i<=NF; ++i) printf "%s ", \$i; print ""}'`;
            chomp($stat_nr_fail_inst);

            $worksheet->write($xlsxline, 13, $stat_tot_pwr, ${flt_format});
            $worksheet->write($xlsxline, 14, $stat_worst_inst_drop, ${flt_format});
            $worksheet->write($xlsxline, 15, $stat_nr_fail_inst, ${cnt_format});
            $worksheet->write_string($xlsxline, 16, $stat_nr_fail_inst_perc, ${str_format});
        }
    }

    $xlsxline++;
    
    return $xlsxline;
}

sub store_string_widths {
    ### Generates uniform column cell width for better readability
 
    my $worksheet = shift;
    my $col       = $_[1];
    my $token     = $_[2];
 
    # Ignore some tokens that we aren't interested in.
    return if not defined $token;       # Ignore undefs.
    return if $token eq '';             # Ignore blank cells.
    return if ref $token eq 'ARRAY';    # Ignore array refs.
    return if $token =~ /^=/;           # Ignore formula
 
    # Ignore numbers
    return if $token =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/;
 
    # Ignore various internal and external hyperlinks. In a real scenario
    # you may wish to track the length of the optional strings used with
    # urls.
    return if $token =~ m{^[fh]tt?ps?://};
    return if $token =~ m{^mailto:};
    return if $token =~ m{^(?:in|ex)ternal:};
 
 
    # We store the string width as data in the Worksheet object. We use
    # a double underscore key name to avoid conflicts with future names.
    #
    my $old_width    = $worksheet->{__col_widths}->[$col];
    my $string_width = string_width($token);
 
    if (not defined $old_width or $string_width > $old_width) {
        # You may wish to set a minimum column width as follows.
        #return undef if $string_width < 10;
 
        $worksheet->{__col_widths}->[$col] = $string_width;
    }
 
 
    # Return control to write();
    return undef;
}

sub string_width {
    ### Adjustment of cell width considering deviations by, e.g. bold characters
 
    return 1.3 * length $_[0];
}

sub autofit_columns {
    ### Setting cell width for all cells of a column
 
    my $worksheet = shift;
    my $col       = 0;
 
    for my $width (@{$worksheet->{__col_widths}}) {
 
        $worksheet->set_column($col, $col, $width) if $width;
        $col++;
    }
}

sub rename2gds {

    my ($name) = @_;

    my $result = "";

    foreach $char (split //, $name) {
        if ( $char eq "." ) {
            $result .= "_D_";
        } elsif ( $char eq "," ) {
            $result .= "_C_";
        } elsif ( $char eq "[" ) {
            $result .= "_l_";
        } elsif ( $char eq "]" ) {
            $result .= "_r_";
        } elsif ( $char eq "(" ) {
            $result .= "_L_";
        } elsif ( $char eq ")" ) {
            $result .= "_R_";
        } elsif ( $char eq "-" ) {
            $result .= "_M_";
        } elsif ( $char eq "_" ) {
            $result .= "_U_";
        } elsif ( $char eq "#" ) {
            $result .= "_H_";
        } elsif ($char =~ m/\w/) {
            $result .= $char;
        } else {
            my $hexstring = sprintf("%00x", ord($char));
            if ( length($hexstring) == 2) {
                $result .= "_$hexsting_";
            } else {
                die "Error(rename): The code point of $char is greater tha 255.\n";
            }
        }
    }

    return $result;
}
 
sub help {
    print "\n";
    print "Usage: get_data.rv_staticir_2_xlsx.pl -srcdir <dir> [-output <dir>|-xlsxfile <name>|-help]\n";
    print "==========================================================================================\n";
    print "-srcdir   <dir>                : Root dir (full path) of regression runs,\n";
    print "                                 e.g. /nfs/pdx/disks/il_n3a_disk001/ppjoshi1/runs/apr.latest\n";
    print "-output   <dir>                : Output directory\n";
    print "                                 (default: ./)\n";
    print "-xlsxfile <name>               : Name of the xlsx file\n";
    print "                                 (default: RV_Summary_<date>.xlsx)\n";
    print "-help                          : Printout of this list\n";

    exit(0);
}

