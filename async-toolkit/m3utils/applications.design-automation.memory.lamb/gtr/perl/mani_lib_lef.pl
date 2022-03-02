#!/usr/intel/bin/perl

use POSIX;
use Getopt::Long;

#die "Usage: $0 <constraint report>\n" if ($#ARGV < 0);

##print @ARGV;
$number_args = $#ARGV + 1;  
if ($number_args != 6) {  
    print "ERROR. Please check number of arguements.\n";  
    exit;  
}


###get details from command line arguments
##GetOptions("wordlines=s"  => \$ARGV[0],
##           "bitlines=s" => \$ARGV[1],
##           "width=s" => \$ARGV[2],
##           "height=s" => \$ARGV[3],
##           "addr_bus_width=s" => \$ARGV[4],
##           "dual_clock=s" => \$ARGV[5]);
##
##print @ARGV;
if ($ARGV[5]==0) {
    $mem_name= "1w1r1c" ;
} else {
    $mem_name= "1w1r2c" ;
}
##print $mem_name;

my $cell_name = "cdp_lamb_${mem_name}_${ARGV[0]}d_${ARGV[1]}b";
my $file_out = "$cell_name.lef";
my $power = "VDD";
my $ground = "VSS";
my $width = $ARGV[2]; 
my $height = $ARGV[3];
my $rclk  = $ARGV[5];
my $cell_llx = 0;
my $cell_lly = 0;
my $cell_urx = $width;
my $cell_ury = $height;
my $layer = "M2";
my $m2_pitch = 0.0350;
my $m2_width = 0.02;
my $m4_width = 0.02;
my $m4_pitch = 0.0420;
my $m2_length = 0.144;
my $keepout_region = 2;
my $temp = $ARGV[1] -1;
my $temp1 = $ARGV[4] -1;
my $temp2 = $temp+1;
my $temp3 = $temp1+1;

my $first_pinLoc1 = floor(2/$m2_pitch) * $m2_pitch + 0.0175;
my $m4_pinloc1 =  floor(2/$m4_pitch) * $m4_pitch + 0.0315;
$m4_pinloc =    sprintf("%4f",$m4_pinloc1);
$first_pinLoc = sprintf("%4f",$first_pinLoc1);

$power_lly = $m4_pinloc - $m4_width/2;
$power_ury = $m4_pinloc + $m4_width/2;
$gnd_lly = $power_lly + $m4_pitch;
$gnd_ury = $power_lly + $m4_pitch;

if ($temp1>0) {
    if ($rclk==1) {
	@pin_names = ("clk","rclk","wen","test__scan_en","dft__core_si[1:0]","icg_force_on","dft_read_bypass","dft__mem_wr_disable","ren","dft__core_so[1:0]","dout[$temp:0]","radr[$temp1:0]","wdata[$temp:0]","wadr[$temp1:0]");
    } else {
	@pin_names = ("clk","wen","test__scan_en","dft__core_si[1:0]","icg_force_on","dft_read_bypass","dft__mem_wr_disable","ren","dft__core_so[1:0]","dout[$temp:0]","radr[$temp1:0]","wdata[$temp:0]","wadr[$temp1:0]");
    }
}
if ($temp1==0) {
    if ($rclk==1) {
	@pin_names = ("clk","rclk","wen","test__scan_en","dft__core_si[1:0]","icg_force_on","dft_read_bypass","dft__mem_wr_disable","ren","dft__core_so[1:0]","dout[$temp:0]","radr","wdata[$temp:0]","wadr");
    } else {
	@pin_names = ("clk","wen","test__scan_en","dft__core_si[1:0]","icg_force_on","dft_read_bypass","dft__mem_wr_disable","ren","dft__core_so[1:0]","dout[$temp:0]","radr","wdata[$temp:0]","wadr");
    }
}

my $l_pin_name;
my $s_idx;
my $e_idx;
my $no_idx;
$counter = 0;


###LEF GENERATION
# write results to  lef
open OUTPUT,'>',$file_out or die "Unable to open $file_out\n";
print OUTPUT "###Dummy lef generation flow\n\n";
print OUTPUT "VERSION 5.8 ;\n";
print OUTPUT ("BUSBITCHARS \"[\]\" ;\n");
print OUTPUT ("DIVIDERCHAR \"/\" ;\n\n");

print OUTPUT ("MACRO $cell_name\n");
print OUTPUT ("   FIXEDMASK ;\n");
print OUTPUT ("   CLASS BLOCK ;\n");
print OUTPUT ("   SIZE $width BY $height ;\n");
print OUTPUT ("   FOREIGN $cell_name 0.000000 0.000000 ;\n");
print OUTPUT ("   ORIGIN  0  0 ;\n");
print OUTPUT ("   SYMMETRY Y ;\n");

foreach $pin (@pin_names)
{
    
    my $str;
    $first_pinLoc = sprintf("%3f",$first_pinLoc + $m2_pitch);
    ##print "$first_pinLoc\n";

    # M2 pin placement calculation
    $llx = 0  ;
    $lly = $first_pinLoc - $m2_width/2;
    $urx = 0.5 ;
    $ury = $first_pinLoc + $m2_width/2;
    if ($counter % 2 == 0) {
	$str = 'MASK 2';
    } else {
	$str = 'MASK 1';
    }
    if(($pin =~ /dout/) or ($pin =~ /dft__core_so/)){
	$pin_dir = output;
    }elsif(($pin =~ /clk/) or ($pin =~ /radr/) or ($pin =~ /wadr/) or ($pin =~ /wdata/) or ($pin =~ /ren/) or ($pin =~ /wen/) or ($pin =~ /test__scan_en/) or ($pin =~ /dft__core_si/) or ($pin =~ /icg_force_on/) or ($pin =~ /dft_read_bypass/) or ($pin =~ /dft/)  ){
	$pin_dir = input;
    }
    if ($pin =~ /(\w+).*\[(\d+)\:(\d+)\]/) { #check for pattern []
        $l_pin_name = $1;
        $s_idx = $2;
        $e_idx = $3;
    }else{
        $no_idx = 1;
        $s_idx = 0;
    }
    if($s_idx != 0) {
	for (my $i=$e_idx; $i<$s_idx+1; $i++){
	    if ($i != $e_idx) {
		$first_pinLoc = $first_pinLoc + $m2_pitch;
		$llx = 0  ;
		$lly = $first_pinLoc - $m2_width/2;
		$urx = 0.5 ;
		$ury = $first_pinLoc + $m2_width/2;
	    }
	    if ($counter % 2 == 0) {
		$str = 'MASK 2';
	    } else {
                $str = 'MASK 1';
	    }
	    print OUTPUT ("   PIN $l_pin_name"."\["."$i"."\]"."\n");
	    print OUTPUT ("      DIRECTION $pin_dir ;\n");
	    print OUTPUT ("      USE SIGNAL ;\n");
	    print OUTPUT ("      PORT\n");
	    print OUTPUT ("         LAYER $layer ;\n");
	    print OUTPUT ("            RECT $str $llx $lly $urx $ury ;\n");
	    print OUTPUT ("      END\n");
	    print OUTPUT ("   END $l_pin_name"."\["."$i"."\]"."\n\n");
	    
	    $counter++ ;
        }
    }elsif($no_idx == 1){
	print OUTPUT ("   PIN $pin\n");
	print OUTPUT ("      DIRECTION $pin_dir ;\n");
	print OUTPUT ("      USE SIGNAL ;\n");
	print OUTPUT ("      PORT\n");
	print OUTPUT ("         LAYER $layer ;\n");
	print OUTPUT ("            RECT $str $llx $lly $urx $ury ;\n");
	print OUTPUT ("      END\n");
	print OUTPUT ("   END $pin\n\n");
	$counter++ ;
    }
}


print OUTPUT ("   PIN $power\n");
print OUTPUT ("      DIRECTION INOUT ;\n");
print OUTPUT ("      USE POWER ;\n");
print OUTPUT ("      PORT\n");
print OUTPUT ("       LAYER M4 ;\n");
print OUTPUT ("          RECT  $cell_llx $power_lly $urx $power_ury ;\n");
print OUTPUT ("      END\n");
print OUTPUT ("   END $power\n\n");
print OUTPUT ("   PIN $ground\n");
print OUTPUT ("      DIRECTION INOUT ;\n");
print OUTPUT ("      USE GROUND ;\n");
print OUTPUT ("      PORT\n");
print OUTPUT ("       LAYER M4 ;\n");
print OUTPUT ("          RECT  $cell_llx $gnd_lly $urx $gnd_ury ;\n");
print OUTPUT ("      END\n");
print OUTPUT ("   END $ground\n\n");
print OUTPUT ("END $cell_name\n");
print OUTPUT ("END LIBRARY\n");

close OUTPUT;


##LIB GENERATION
# write results to  lef
my $file_out1 = "$cell_name.lib";
open OUTPUT1,'>',$file_out1 or die "Unable to open $file_out\n";
print OUTPUT1 ("\/* Dummy lib flow *\/\n");
print OUTPUT1 ("library ($cell_name) {\n");
print OUTPUT1 ("delay_model : table_lookup;\n");
$date = localtime();
print OUTPUT1 ("date : \"$date\";\n");
print OUTPUT1 ("revision : \"1.0\" ;\n");
print OUTPUT1 ("library_features(report_delay_calculation);\n");
print OUTPUT1 ("bus_naming_style : \"%s\[%d\]\" ;\n\n");
print OUTPUT1 (" \/* unit attributes *\/\n");
print OUTPUT1 ("capacitive_load_unit ( 1.0000,pf);\n");
print OUTPUT1 ("current_unit : \"1mA\" ;\n");
print OUTPUT1 ("pulling_resistance_unit : \"1kohm\" ;\n");
print OUTPUT1 ("time_unit : \"1ns\" ;\n");
print OUTPUT1 ("voltage_unit : \"1V\" ;\n");
print OUTPUT1 (" leakage_power_unit : \"1nW\" ;\n\n");
print OUTPUT1 (" \/* threshold definitions *\/\n");
print OUTPUT1 ("input_threshold_pct_fall : 50.0000;\n");
print OUTPUT1 ("input_threshold_pct_rise : 50.0000;\n");
print OUTPUT1 ("output_threshold_pct_fall : 50.0000;\n");
print OUTPUT1 ("output_threshold_pct_rise : 50.0000;\n");
print OUTPUT1 ("slew_lower_threshold_pct_fall : 30.0000;\n");
print OUTPUT1 ("slew_lower_threshold_pct_rise : 30.0000;\n");
print OUTPUT1 ("slew_upper_threshold_pct_fall : 70.0000;\n");
print OUTPUT1 ("slew_upper_threshold_pct_rise : 70.0000;\n");
print OUTPUT1 ("slew_derate_from_library : 0.5000;\n\n");
print OUTPUT1 ("voltage_map( VDD , 0.6750);\n");
print OUTPUT1 ("voltage_map( VSS , 0.0000);\n");
print OUTPUT1 ("\/* operating conditions *\/\n");
print OUTPUT1 ("operating_conditions (SSGNP0P675VN40C ){\n");
print OUTPUT1 ("process :  1.0000;\n");
print OUTPUT1 ("temperature :  -40.0000;\n");
print OUTPUT1 ("voltage :  0.6750;\n");
print OUTPUT1 ("tree_type :  \"best_case_tree\" ;\n");
print OUTPUT1 ("}\n");
print OUTPUT1 ("default_operating_conditions : \"SSGNP0P675VN40C\" ;\n");
print OUTPUT1 ("nom_process : 1.0000;\n");
print OUTPUT1 ("nom_temperature : -40.0000;\n");
print OUTPUT1 ("nom_voltage : 0.6750;\n\n");
print OUTPUT1 ("\/* default attributes *\/\n");
print OUTPUT1 ("default_fanout_load : 1.0000;\n");
print OUTPUT1 ("default_inout_pin_cap : 1.0000;\n");
print OUTPUT1 ("default_input_pin_cap : 1.0000;\n");
print OUTPUT1 ("default_output_pin_cap : 0.0000;\n");
print OUTPUT1 ("default_wire_load_area : 0.0000;\n");
print OUTPUT1 ("default_wire_load_capacitance : 0.0000;\n");
print OUTPUT1 ("default_wire_load_resistance : 3.7000;\n");
print OUTPUT1 ("k_process_cell_rise :  0.0000;\n");
print OUTPUT1 ("k_process_cell_fall :  0.0000;\n");
print OUTPUT1 ("k_volt_cell_rise :  0.0000;\n");
print OUTPUT1 ("k_volt_cell_fall :  0.0000;\n");
print OUTPUT1 ("k_temp_cell_rise :  0.0000;\n");
print OUTPUT1 ("k_temp_cell_fall :  0.0000;\n");
print OUTPUT1 ("k_process_rise_transition :  0.0000;\n");
print OUTPUT1 ("k_process_fall_transition :  0.0000;\n");
print OUTPUT1 ("k_volt_rise_transition :  0.0000;\n");
print OUTPUT1 ("k_volt_fall_transition :  0.0000;\n");
print OUTPUT1 ("k_temp_rise_transition :  0.0000;\n");
print OUTPUT1 ("k_temp_fall_transition :  0.0000;\n\n");
print OUTPUT1 ("\/* templates *\/\n");
print OUTPUT1 ("lu_table_template (lut_timing_1 ){\n");
print OUTPUT1 ("variable_1 : input_net_transition ;\n");
print OUTPUT1 ("index_1(\" 0.0001, 0.3666\");\n");
print OUTPUT1 ("}\n\n");
print OUTPUT1 ("lu_table_template (lut_timing_3 ){\n");
print OUTPUT1 ("variable_1 : input_net_transition ;\n");
print OUTPUT1 ("index_1(\"  0.0001, 0.3666\");\n");
print OUTPUT1 ("variable_2 : total_output_net_capacitance ;\n");
print OUTPUT1 ("index_2(\"  0.0000, 0.1632\");\n");
print OUTPUT1 ("}\n\n");
print OUTPUT1 ("lu_table_template (lut_timing_setuphold ){\n");
print OUTPUT1 ("variable_1 : related_pin_transition ;\n");
print OUTPUT1 ("index_1(\"  0.0001, 0.3666\");\n");
print OUTPUT1 ("variable_2 : constrained_pin_transition ;\n");
print OUTPUT1 ("index_2(\"  0.0000, 0.1632\");\n");
print OUTPUT1 ("}\n\n");
print OUTPUT1 ("lu_table_template (lut_timing_7 ){\n");
print OUTPUT1 ("variable_1 : constrained_pin_transition ;\n");
print OUTPUT1 ("index_1(\"  0.0000, 0.3666\");\n");
print OUTPUT1 ("variable_2 : related_pin_transition ;\n");
print OUTPUT1 ("index_2(\"  0.0000, 0.3666\");\n");
print OUTPUT1 ("}\n\n");
print OUTPUT1 ("define( block_distance , cell , float ) ;\n");
print OUTPUT1 ("define( min_delay_arc , timing , boolean ) ;\n\n");
print OUTPUT1 ("\/* end of header section *\/\n\n");
print OUTPUT1 ("cell ($cell_name ) {\n");
$area= $width * $height ;
print OUTPUT1 ("area :  $area;\n");
print OUTPUT1 ("dont_touch : true ;\n");
print OUTPUT1 ("dont_use : true ;\n");
print OUTPUT1 ("timing_model_type : extracted ;\n");
print OUTPUT1 ("interface_timing : true ;\n");
print OUTPUT1 ("is_macro_cell : true ;\n");
print OUTPUT1 ("pg_pin (VDD ){\n");
print OUTPUT1 ("voltage_name : VDD ;\n");
print OUTPUT1 ("direction : input ;\n");
print OUTPUT1 ("pg_type : primary_power ;\n");
print OUTPUT1 ("}\n");
print OUTPUT1 ("pg_pin (VSS ){\n");
print OUTPUT1 ("voltage_name : VSS ;\n");
print OUTPUT1 ("direction : input ;\n");
print OUTPUT1 ("pg_type : primary_ground ;\n");
print OUTPUT1 ("}\n");
#####Pin section
####Removing dft pins from list as they wont have timing arc wrt functional clock
## my @pin_names = ("dout[$temp:0]","dft__core_so[1:0]","radr[$temp1:0]","ren","wdata[$temp:0]","wadr[$temp1:0]","wen","test__scan_en","dft__core_si[1:0]","icg_force_on","dft_read_bypass","dft__mem_wr_disable","clk");
##my @pin_names = ("dout[$temp:0]","radr[$temp1:0]","ren","wdata[$temp:0]","wadr[$temp1:0]","wen","clk");
$i=0; 
$flag_si=0;
$flag_so=0;
$bus_count=0;

foreach $pin (@pin_names)
{
    if(($pin =~ /dout/) or ($pin =~ /dft__core_so/)){
	$pin_dir = output;
    }elsif(($pin =~ /clk/) or ($pin =~ /radr/) or ($pin =~ /wadr/) or ($pin =~ /wdata/) or ($pin =~ /ren/) or ($pin =~ /wen/) or ($pin =~ /test__scan_en/) or ($pin =~ /dft__core_si/) or ($pin =~ /icg_force_on/) or ($pin =~ /dft_read_bypass/) or ($pin =~ /dft/)  ){
	$pin_dir = input;
    }

    ###Should take care of clk and rclk pin (if 2 clock inputs)
    if(($pin =~ /clk/)) {
	print OUTPUT1 ("pin ($pin ) {\n");
	print OUTPUT1 ("clock : true ;\n");
	print OUTPUT1 ("direction : input ;\n");
	print OUTPUT1 ("capacitance :  1.3305;\n");
	print OUTPUT1 ("max_transition :  0.2000;\n");
	print OUTPUT1 ("related_ground_pin :   \"VSS\" ;\n");
	print OUTPUT1 ("related_power_pin :   \"VDD\" ;\n");
	print OUTPUT1 ("fanout_load :  1192.0000;\n");
	print OUTPUT1 ("timing() {\n");
	print OUTPUT1 ("timing_type : min_clock_tree_path ;\n");
	print OUTPUT1 ("timing_sense : positive_unate ;\n");
	print OUTPUT1 ("cell_rise (lut_timing_1 ){\n");
	print OUTPUT1 ("values(\\\n");
	print OUTPUT1 ("\" 0.0000, 0.0000\" \\\n");
	print OUTPUT1 (");\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("timing() {\n");
	print OUTPUT1 ("timing_type : max_clock_tree_path ;\n");
	print OUTPUT1 ("timing_sense : positive_unate ;\n");
	print OUTPUT1 ("cell_rise (lut_timing_1 ){\n");
	print OUTPUT1 ("values(\\\n");
	print OUTPUT1 ("\" 0.0000, 0.0000\" \\\n");
	print OUTPUT1 (");\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("min_pulse_width_low :  0.0671;\n");
	print OUTPUT1 ("min_pulse_width_high :  0.0656;\n");
	print OUTPUT1 ("}\n");
    }
    if(($pin =~ /icg_force_on/) or ($pin =~ /dft__mem_wr_disable/)  or ($pin =~ /dft_read_bypass/) ) {
	print OUTPUT1 ("pin ($pin ) {\n");
	print OUTPUT1 ("direction : $pin_dir;\n");
	print OUTPUT1 ("capacitance :  0.0000;\n");
	print OUTPUT1 ("max_transition :  0.2000;\n");
	print OUTPUT1 ("}\n");
    }

    if(($pin =~ /dft__core_si/) or ($pin =~ /dft__core_so/) ) {
	if ($flag_si==0) {
	    print OUTPUT1 ("type (bus$bus_count){\n");
	    print OUTPUT1 ("base_type : array ;\n");
	    print OUTPUT1 ("data_type : bit ;\n");
	    print OUTPUT1 ("bit_width :  2;\n");
	    print OUTPUT1 ("bit_from :  1;\n");
	    print OUTPUT1 ("bit_to :  0;\n");
	    print OUTPUT1 ("downto : true ;\n");
	    print OUTPUT1 ("}\n");
	    print OUTPUT1 ("\n");
	    if ($pin =~ /(\w+).*\[(\d+)\:(\d+)\]/) { #check for pattern []
		$l_pin_name = $1;
	    }
	    print OUTPUT1 ("bus ($l_pin_name){\n");
	    print OUTPUT1 ("bus_type :  bus$bus_count ;\n");
	    
	    ##  $flag_si=1;
	    $bus_count++;
	}
	if ($pin =~ /(\w+).*\[(\d+)\:(\d+)\]/) { #check for pattern []
	    $l_pin_name = $1;
	    $s_idx = $2;
	    $e_idx = $3;
	    if($s_idx != 0) {
		for (my $i=$e_idx; $i<$s_idx+1; $i++){
		    print OUTPUT1 ("pin ($l_pin_name"."\["."$i"."\])"."{\n");
		    print OUTPUT1 ("direction : $pin_dir;\n");
		    print OUTPUT1 ("capacitance :  0.0000;\n");
		    print OUTPUT1 ("max_transition :  0.2000;\n");
		    print OUTPUT1 ("}\n");
		}
	    }
	}
	print OUTPUT1 ("}\n");
    }


    if(($pin =~ /ren/) or ($pin =~ /wen/) ) {
	print OUTPUT1 ("pin ($pin ) {\n");
	print OUTPUT1 ("direction : $pin_dir;\n");
	print OUTPUT1 ("capacitance :  0.0000;\n");
	print OUTPUT1 ("max_transition :  0.2000;\n");
	print OUTPUT1 ("related_ground_pin :   \"VSS\" ;\n");
	print OUTPUT1 ("related_power_pin :   \"VDD\" ;\n");
	print OUTPUT1 ("timing() {\n");
	print OUTPUT1 ("timing_type : setup_rising;\n");
	print OUTPUT1 ("timing_sense : non_unate ;\n");
	print OUTPUT1 ("related_pin :\" clk \";\n");
	print OUTPUT1 ("rise_constraint (lut_timing_setuphold ){\n");
	print OUTPUT1 ("values(\\\n");
	print OUTPUT1 ("\" 0.4000, 0.4000\", \\\n");
	print OUTPUT1 ("\" 0.4000, 0.4000\" \\\n");
	print OUTPUT1 (");\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("fall_constraint (lut_timing_setuphold ){\n");
	print OUTPUT1 ("values(\\\n");
	print OUTPUT1 ("\" 0.4000, 0.4000\", \\\n");
	print OUTPUT1 ("\" 0.4000, 0.4000\" \\\n");
	print OUTPUT1 (");\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("}\n");
    }

    if(($pin =~ /dout/) or ($pin =~ /radr/) or ($pin =~ /wdata/)  or ($pin =~ /wadr/)) {
	print OUTPUT1 ("type (bus$bus_count){\n");
	print OUTPUT1 ("base_type : array ;\n");
	print OUTPUT1 ("data_type : bit ;\n");
	if(($pin =~ /dout/) or ($pin =~ /wdata/)) {
	    print OUTPUT1 ("bit_width : $temp2 ;\n");
	    print OUTPUT1 ("bit_from :  $temp;\n");
	    print OUTPUT1 ("bit_to :  0;\n");
	} else {
	    print OUTPUT1 ("bit_width : $temp3 ;\n");
	    print OUTPUT1 ("bit_from :  $temp1;\n");
	    print OUTPUT1 ("bit_to :  0;\n");
	}
	print OUTPUT1 ("downto : true ;\n");
	print OUTPUT1 ("}\n");
	print OUTPUT1 ("\n");
	if ($pin =~ /(\w+).*\[(\d+)\:(\d+)\]/) { #check for pattern []
	    $l_pin_name = $1;
	    print OUTPUT1 ("bus ($l_pin_name){\n");
	    print OUTPUT1 ("bus_type :  bus$bus_count ;\n");
	    
	    ##  $flag_si=1;
	    $bus_count++;
	}
	if ($pin =~ /(\w+).*\[(\d+)\:(\d+)\]/) { #check for pattern []
	    $l_pin_name = $1;
	    $s_idx = $2;
	    $e_idx = $3;
	    if($s_idx != 0) {
		for (my $i=$e_idx; $i<$s_idx+1; $i++){
		    print OUTPUT1 ("pin ($l_pin_name"."\["."$i"."\])"."{\n");
		    print OUTPUT1 ("direction : $pin_dir;\n");
		    print OUTPUT1 ("capacitance :  0.0000;\n");
		    print OUTPUT1 ("max_transition :  0.2000;\n");
		    print OUTPUT1 ("related_ground_pin :   \"VSS\" ;\n");
		    print OUTPUT1 ("related_power_pin :   \"VDD\" ;\n");
		    print OUTPUT1 ("timing() {\n");
		    print OUTPUT1 ("timing_type : setup_rising;\n");
		    print OUTPUT1 ("timing_sense : non_unate ;\n");
		    print OUTPUT1 ("related_pin :\" clk \";\n");
		    print OUTPUT1 ("rise_constraint (lut_timing_setuphold ){\n");
		    print OUTPUT1 ("values(\\\n");
		    print OUTPUT1 ("\" 0.4000, 0.4000\", \\\n");
		    print OUTPUT1 ("\" 0.4000, 0.4000\" \\\n");
		    print OUTPUT1 (");\n");
		    print OUTPUT1 ("}\n");
		    print OUTPUT1 ("fall_constraint (lut_timing_setuphold ){\n");
		    print OUTPUT1 ("values(\\\n");
		    print OUTPUT1 ("\" 0.4000, 0.4000\", \\\n");
		    print OUTPUT1 ("\" 0.4000, 0.4000\" \\\n");
		    print OUTPUT1 (");\n");
		    print OUTPUT1 ("}\n");
		    print OUTPUT1 ("}\n");
		    print OUTPUT1 ("}\n");
		}
	    }
	}
	print OUTPUT1 ("}\n");
    }
}
print OUTPUT1 ("}\n");
print OUTPUT1 ("}\n");

close OUTPUT1;

print OUTPUT1 ("\n");

close(f);


