package security;
%agent_encoding = (
#################
# SAI Label                   		6bINDEX   6bINDEX (hex)	# MBY usage
"SAI_HOSTIA_POSTBOOT"      		=> 0b000000,	#  0	# BaseIA cores and P2SB
"SAI_HOSTIA_UCODE"         		=> 0b000001,	#  1	# BaseIA cores and P2SB
"SAI_HOSTIA_SMM"           		=> 0b000010,	#  2	# BaseIA cores and P2SB
"SAI_HOSTIA_SUNPASS"       		=> 0b000011,	#  3	# BaseIA cores and P2SB
"SAI_HOSTIA_BOOT"          		=> 0b000100,	#  4	# BaseIA cores and P2SB
#"SAI_RESERVED_1"          		=> 0b000101,	#  5	# 
#"SAI_RESERVED_2"          		=> 0b000110,	#  6	# 
#"SAI_RESERVED_3"          		=> 0b000111,	#  7	# 
"SAI_GT"                   		=> 0b001000,	#  8	# n/a to MBY. Documented for Chassis 2.1 compatibility
"SAI_PM_PCS"               		=> 0b001001,	#  9	# BaseIA PUNIT
"SAI_HW_CPU"               		=> 0b001010,	#  A	# BaseIA SA (completions from A/B/C/T Units) and DDRIO (completions)
"SAI_MEM_CPL"              		=> 0b001011,	#  B	# DUNIT (completions) and DUNIT (RAS messages) <- per DNV memory controller; revisit for 10nm MC
"SAI_VTD"                  		=> 0b001100,	#  C	# BaseIA IOMMU
#"RESERVED"                		=> 0b001101,	#  D	# 
#"NORTH_Reserved"          		=> 0b001110,	#  E	#
"SAI_HOSTCP_PMA"           		=> 0b001111,	#  F	# BaseIA PMA
"SAI_CSE_INTEL"            		=> 0b010000,	# 10	# EMP when performing CSME tasks
"SAI_CSE_OEM"              		=> 0b010001,	# 11	# n/a to MBY. Documented for Chassis 2.1 compatibility
"SAI_Fuse_Controller"      		=> 0b010010,	# 12	# Fuse Controller
"SAI_Fuse_Puller"          		=> 0b010011,	# 13	# Fuse Puller
#"Reserved"                		=> 0b010100,	# 14	#
"SAI_PM_IOSS"              		=> 0b010101,	# 15	# EMP when performing PMC tasks
"SAI_BIA_DRNG"             		=> 0b010110,	# 16	# BaseIA DRNG. Note: Chassis 2.1 = SAI_CSE_DNX (which is n/a to MBY), 
								# but Chassis 2.0 = North DRNG (affects BaseIA SA security policies). 
								# MBY follows Chassis 2.0 here
"SAI_DFX_AGGR"             		=> 0b010111,	# 17	# DFx Security Aggregator. Note: Chassis 2.1 = Reserved, 
								# but Chassis 2.0 = DFx Aggregator (affects BaseIA SA security policies). 
								# MBY follows Chassis 2.0 here
"SAI_DFX_INTEL_MANUFACTURING"   	=> 0b011000,	# 18	# TAP2SB
"SAI_DFX_UNTRUSTED"        		=> 0b011001,	# 19	# TAP2SB
"SAI_ICM"                  		=> 0b011010,	# 1A	# iCM. Note: consistent with nCPM assignment in both DNV and CDF
"SAI_XGBE_GRP"             		=> 0b011011,	# 1B	# EPL, ETM, FXP, RDMA, SPM - a bunch of HW blocks collectively doing 
								# "networking" tasks
"SAI_NPK"                  		=> 0b011100,	# 1C	# NPK
#"NORTH_Reserved"          		=> 0b011101,	# 1D	#
#"NORTH_Reserved"          		=> 0b011110,	# 1E	#
"SAI_HW_PCH"               		=> 0b011111,	# 1F	# BaseIA ITSS, DTS, Fuse Controller's DRNG, GPIO, PSF
"SAI_SPI"                  		=> 0b100000,	# 20	# SPI Controller
#"Reserved"                		=> 0b100001,	# 21	#
#"Reserved"                		=> 0b100010,	# 22	#
"SAI_GT_PMA"               		=> 0b100011,	# 23	# n/a to MBY. Documented for Chassis 2.1 compatibility
#"Reserved"                		=> 0b100100,	# 24	#
#"Reserved"                		=> 0b100101,	# 25	#
#"Reserved"                		=> 0b100110,	# 26	#
#"Reserved"                		=> 0b100111,	# 27	#
#"NORTH_Reserved"          		=> 0b101000,	# 28	#
"SAI_RC_MORPHED"           		=> 0b101001,	# 29	# n/a to MBY. Documented for Chassis 2.1 compatibility
"SAI_DFX_INTEL_PRODUCTION"      	=> 0b101010,	# 2A	# TAP2SB
"SAI_DFX_THIRDPARTY"       		=> 0b101011,	# 2B	# TAP2SB
"SAI_DISPLAY"              		=> 0b101100,	# 2C	# n/a to MBY. Documented for Chassis 2.1 compatibility
#"Reserved"             		=> 0b101101,	# 2D	#
#"Reserved"                  		=> 0b101110,	# 2E	#
"SAI_DISPLAY_KVM"          		=> 0b101111,	# 2F	# n/a to MBY. Documented for Chassis 2.1 compatibility
#"NORTH_Reserved"          		=> 0b110000, 	# 30	#
"SAI_HIF"          			=> 0b110001, 	# 31	# HIF Trusted tasks
#"NORTH_Reserved"          		=> 0b110010, 	# 32	#
#"SOUTH_Reserved"          		=> 0b110011, 	# 33	#
"SAI_CORE_EVENT_PROXY"     		=> 0b110100, 	# 34	# BaseIA TUNIT
"SAI_CCK"          			=> 0b110101, 	# 35	# BaseIA CCK. Note: Chassis 2.1 = NORTH_Reserved, 
								# but Chassis 2.0 = Clock (affects BaseIA SA security policies). 
								# MBY follows Chassis 2.0 here
"SAI_RCIOMMU_BYPASS"            	=> 0b110110, 	# 36	# n/a to MBY. Documented for Chassis 2.1 compatibility
#"CrossDie_Reserved"            	=> 0b110111, 	# 37	#
"SAI_EC_ESPI"              		=> 0b111000, 	# 38	# SPI Controller. Note: based on MBY configuration of SPI controller 
								# this SAI value is probably unused
"SAI_EMP"               		=> 0b111001, 	# 39	# EMP when performing EMP tasks
#"Reserved"                  		=> 0b111010, 	# 3A	#
#"Reserved"              		=> 0b111011, 	# 3B	#
#"Reserved"                  		=> 0b111100, 	# 3C	#
"SAI_OOB_MSM"              		=> 0b111101, 	# 3D	# AR: Christine/Erik - what is expectation of this SAI for SPR-D?
#"Reserved"                 		=> 0b111110, 	# 3E	#
"SAI_DEVICE_UNTRUSTED"     		=> 0b111111, 	# 3F	# not explicitly assigned to any MBY IP - results from 8bSAI-to-6bINDEX formula if 8bSAI is missing or 8'h00

#################
# Note: to account for IP's which follow prior SAI naming conventions, add aliases per
# SAI labels below per Chassis 2.0
#################
"SAI_DFX_RED2"   			=> 0b011000,	# 18	# TAP2SB
"SAI_DFX_GREEN"        			=> 0b011001,	# 19	# TAP2SB
"SAI_DFX_RED4"      			=> 0b101010,	# 2A	# TAP2SB
"SAI_DFX_ORANGE"       			=> 0b101011,	# 2B	# TAP2SB
);
1;

#32bit function reference
# A register that holds a 64-bit SAI value rather than 32-bit requires 
# the addition of the following code in security.pm security header file
sub GetSecurityInfo() {
   %security = (
####################################################################
# CGU, One CP policy for all three AC registers
####################################################################

      'CGU_POLICY_GRP0' => '"CGU_POLICY_GRP0"',
      'CGU_POLICY_GRP0_CP_AGENTS' => '"SAI_DFX_INTEL_PRODUCTION | SAI_EMP | SAI_DFX_INTEL_MANUFACTURING | SAI_PM_PCS | SAI_CSE_INTEL"',
      'CGU_POLICY_GRP0_AC_AGENTS' => '"SAI_DFX_INTEL_PRODUCTION | SAI_EMP | SAI_DFX_INTEL_MANUFACTURING | SAI_PM_PCS | SAI_CSE_INTEL | SAI_DFX_THIRDPARTY | SAI_OOB_MSM | SAI_XGBE_GRP"',
      'CGU_POLICY_GRP1' => '"CGU_POLICY_GRP1"',
      'CGU_POLICY_GRP1_CP_AGENTS' => '"SAI_DFX_INTEL_PRODUCTION | SAI_EMP | SAI_DFX_INTEL_MANUFACTURING | SAI_PM_PCS | SAI_CSE_INTEL"',
      'CGU_POLICY_GRP1_AC_AGENTS' => '"SAI_HOSTIA_BOOT | SAI_HOSTIA_SMM | SAI_HOSTIA_SUNPASS | SAI_HOSTIA_UCODE | SAI_DFX_INTEL_MANUFACTURING | SAI_DFX_INTEL_PRODUCTION | SAI_DFX_THIRDPARTY | SAI_PM_PCS | SAI_CSE_INTEL | SAI_OOB_MSM | SAI_EMP | SAI_XGBE_GRP"',
      'CGU_POLICY_GRP2' => '"CGU_POLICY_GRP2"',
      'CGU_POLICY_GRP2_CP_AGENTS' => '"SAI_DFX_INTEL_PRODUCTION | SAI_EMP | SAI_DFX_INTEL_MANUFACTURING | SAI_PM_PCS | SAI_CSE_INTEL"',
      'CGU_POLICY_GRP2_AC_AGENTS' => '"SAI_HOSTIA_BOOT | SAI_HOSTIA_SMM | SAI_HOSTIA_SUNPASS | SAI_HOSTIA_UCODE | SAI_DFX_INTEL_MANUFACTURING | SAI_DFX_INTEL_PRODUCTION | SAI_DFX_THIRDPARTY | SAI_PM_PCS | SAI_CSE_INTEL | SAI_OOB_MSM | SAI_EMP | SAI_XGBE_GRP | SAI_DEVICE_UNTRUSTED"',
      'CGU_POLICY_GRP0_CGU_POLICY_GRP1_CGU_POLICY_GRP2' => '"CGU_POLICY_GRP0, CGU_POLICY_GRP1, CGU_POLICY_GRP2"',


####################################################################
# DRNG 
####################################################################
      #'DRNG_CREG'            => '"DRNG_CREG"',
      #'DRNG_CREG_CP_AGENTS'  => '"SAI_DFX_RED4 | SAI_DFX_RED2 | SAI_P_PM | SAI_CSE_INTEL | SAI_GLM_Microcode"',
      #'DRNG_CREG_WAC_AGENTS' => '"SAI_DFX_RED4 | SAI_DFX_RED2 | SAI_P_PM | SAI_CSE_INTEL | SAI_GLM_Microcode"',
      #'DRNG_CREG_RAC_AGENTS' => '"ALL"',

####################################################################
# IO Widget
####################################################################
    'CPK_IO_WIDGET_GENERAL' => '"CPK_IO_WIDGET_GENERAL"',
    'CPK_IO_WIDGET_GENERAL_WAC_AGENTS' => '"SAI_EMP | SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'CPK_IO_WIDGET_GENERAL_RAC_AGENTS' => '"SAI_EMP | SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'CPK_IO_WIDGET_GENERAL_CP_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS | SAI_EMP"',

    ##===================##
    # SPI does not have separate Control Policy registers for flash, tpm, and sbreg, but the tools
    # expect to see *_CP_AGENTS definitions. Reuse the CTRL_POLICY_CP_AGENTS for each of the above.
    'SPI_CTRL_POLICY' => '"SPI_CTRL_POLICY"',
    'SPI_CTRL_POLICY_CP_AGENTS'  => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS | SAI_EMP"',

    ##===================##
    'SPI_FLASH' => '"SPI_FLASH"',
    'SPI_FLASH_WAC_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'SPI_FLASH_RAC_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'SPI_FLASH_CP_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS | SAI_EMP"',

    ##===================##
    'SPI_TPM' => '"SPI_TPM"',
    'SPI_TPM_AC_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'SPI_TPM_CP_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS | SAI_EMP"',

    ##===================##
    'SPI_SBREG' => '"SPI_SBREG"',
    'SPI_SBREG_WAC_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'SPI_SBREG_RAC_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'SPI_SBREG_CP_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS | SAI_EMP"',

    ##===================##
    'SPI_FIXED_SBREG' => '"SPI_FIXED_SBREG"',
    'SPI_FIXED_SBREG_READ_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS"',
    'SPI_FIXED_SBREG_WRITE_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS"',

    ##===================##
    'SPI_FIXED_HOST' => '"SPI_FIXED_HOST"',
    'SPI_FIXED_HOST_READ_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',
    'SPI_FIXED_HOST_WRITE_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',

    ##===================##
    'SPI_FIXED_CSXE' => '"SPI_FIXED_CSXE"',
    'SPI_FIXED_CSXE_READ_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS"',
    'SPI_FIXED_CSXE_WRITE_AGENTS' => '"SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS"',

    ##== Control via multiple SAI sources  ==##
    'SPI_CTRL_POLICY_SPI_FLASH_SPI_TPM_SPI_SBREG' => '"SPI_CTRL_POLICY,SPI_FLASH,SPI_TPM,SPI_SBREG"',  # single CP reg controls all policy regs
#    'SPI_ESPI_CTRL_POLICY_SPI_ESPI_HOST_SPI_ESPI_CSME_IE' => '"SPI_ESPI_CTRL_POLICY,SPI_ESPI_HOST,SPI_ESPI_CSME_IE"',
    'SPI_FLASH_SPI_FIXED_HOST' => '"SPI_FLASH_SPI_FIXED_HOST"',            # MMIO and Cfg regs use policy and identification

    # MGW -- below attempts to work around Nebulon OVM generation complaint, by establishing above as 'fixed' policygroup
    'SPI_FLASH_SPI_FIXED_HOST_READ_AGENTS' => '"SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS"',

    'SPI_FLASH_SPI_FIXED_CSXE' => '"SPI_FLASH,SPI_FIXED_CSXE"',

    'SPI_CSXE_REG_ACCESS_STRING' => '"AND of (SPI_FLASH policy,csxe_identification(SAI_CSE_INTEL | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_PM_PCS | SAI_EMP))"',

    'SPI_HOST_REG_ACCESS_STRING' => '"AND of (SPI_FLASH policy,host_identification(SAI_HOSTIA_UCODE | SAI_HOSTIA_SMM | SAI_EMP | SAI_DFX_RED2 | SAI_DFX_RED4 | SAI_DFX_ORANGE | SAI_PM_PCS))"',


####################################################################
# MBY_PG0 policy groups
####################################################################
      'MBY_PG0'               => '"MBY_PG0"',
      'MBY_PG0_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG0_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG0_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG1 policy groups
####################################################################
      'MBY_PG1'               => '"MBY_PG1"',
      'MBY_PG1_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG1_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG1_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG2 policy groups
####################################################################
      'MBY_PG2'               => '"MBY_PG2"',
      'MBY_PG2_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG2_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG2_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG3 policy groups
####################################################################
      'MBY_PG3'               => '"MBY_PG3"',
      'MBY_PG3_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG3_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG3_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG4 policy groups
####################################################################
      'MBY_PG4'               => '"MBY_PG4"',
      'MBY_PG4_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG4_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG4_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG5 policy groups
####################################################################
      'MBY_PG5'               => '"MBY_PG5"',
      'MBY_PG5_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG5_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG5_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG6 policy groups
####################################################################
      'MBY_PG6'               => '"MBY_PG6"',
      'MBY_PG6_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG6_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG6_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',

####################################################################
# MBY_PG7 policy groups
####################################################################
      'MBY_PG7'               => '"MBY_PG7"',
      'MBY_PG7_CP_AGENTS'     => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG7_WAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
      'MBY_PG7_RAC_AGENTS'    => '"SAI_CSE_INTEL | SAI_DFX_INTEL_MANUFACTURING | SAI_XGBE_GRP | SAI_EMP"',
   );
   return (%security);
}
sub str2hex64 {
   use Math::BigInt lib => 'GMP';
   $instring = shift;
   $instring =~ s/\s+//g;
   $instring =~ s/\"//g;
   @agent_list = split(/\|/, $instring);
   $vector_val = Math::BigInt->new($str); 
   foreach $a (@agent_list) {
      chomp($a);
      $shift = $agent_encoding{$a};
      $oneshift = Math::BigInt->bone();
      $oneshift->blsft($shift);
      $vector_val->badd($oneshift);
   }
   $vector_val_hex = $vector_val->as_hex();
   $vector_val_hex_verilog = "64'h" . $vector_val_hex;
   $vector_val_hex_verilog =~ s/0x//; return ($vector_val_hex_verilog);
}
sub policybit_get_value {
  $vector_val = $_[1];
  $vector_val =~ s/64'h//g;
  $vector_val_dec = hex($vector_val);
  if ($_[0] eq "RW") {
    return (($vector_val_dec >> $_[2])%2);
  }
  else {
    return 0;
  }
}
sub str2hex_upper32 {
   
   $instring = shift;  
   $instring =~ s/\s+//g;    
   $instring =~ s/\"//g;     
   @agent_list = split(/\|/, $instring);   
   $vector_val = 0;
   foreach $a (@agent_list) {
     chomp($a);
     $shift = $agent_encoding{$a};
     if ($shift < 32) { next; }
     $vector_val = $vector_val +  (1<< ($shift-32));
   }
   $vector_val_hex = sprintf "%lx", $vector_val;
   $vector_val_hex = "32'h" . $vector_val_hex;
   return ($vector_val_hex);
}
sub str2hex_msb {
 $instring = shift;
 $instring =~ s/\s+//g;
 $instring =~ s/\"//g;
 @agent_list = split(/\|/, $instring);
 $vector_val = 0;
 foreach $a (@agent_list) {
  chomp($a);
   $shift = $agent_encoding{$a};
  if ($shift >= 32) {
   $shift -= 32;
   $vector_val = $vector_val + (1<< $shift);
  }
 }

 $vector_val_hex = sprintf "%lx", $vector_val;
 $vector_val_hex = "32'h" . $vector_val_hex;
 return ($vector_val_hex);
}
sub get_value {
  if ($_[0] eq "RW") {
    return (($_[1] >> $_[2])%2);
}  else {
    return 0;
  }
}
sub GetUnsupportedInfo() {
    %security = (
      #'BUNIT_IMRSMR_RACWAC_UNSUPPORTED_AGENTS' => '"SAI_P_PM | SAI_GSA_CR | SAI_GSA_MEM | SAI_OCP_SRAM | SAI_HOST_PMA | SAI_FUSE_CONTROLLER | SAI_FUSE_PULLER | SAI_GPIO | SAI_DRNG | SAI_DFX_AGGR | SAI_PSF | SAI_GT_PMA | SAI_CPGC | SAI_IODRNG | SAI_LDO | SAI_SVID | SAI_GSA_Tunit | SAI_CLK | SAI_Reserved_57 | SAI_Reserved_58 | SAI_DBC_EXI | SAI_Reserved_60 | SAI_Reserved_61 | SAI_NONEXISTENT"',
    );
    return (%security);
}
sub GetSecurityUnsupportedInfo() {
  %security = (
  );
  return (%security);
};
sub str2hex_lower32 {
 $instring = shift;
 $instring =~ s/\s+//g;
 $instring =~ s/\"//g;
 @agent_list = split(/\|/, $instring);
 $vector_val = 0;
 foreach $a (@agent_list) {
  chomp($a);
   $shift = $agent_encoding{$a};
  if ($shift < 32) {
   $vector_val = $vector_val + (1<< $shift);
  }
 }

 $vector_val_hex = sprintf "%lx", $vector_val;
 $vector_val_hex = "32'h" . $vector_val_hex;
 return ($vector_val_hex);
}
sub get_type {
  if ((($_[0] >> $_[1])%2) == 1) {
    return "RW";
  }
  else {
    return "RO";
  }
}
sub policybit_get_type {
  $vector_val = $_[0];
  $vector_val =~ s/64'h//g;
  $vector_val_dec = hex($vector_val);
  if ((($vector_val_dec >> $_[1])%2) == 1) {
    return "RO";
  }
  else {
    return "RW";
  }
}
sub str2hex {
$instring = shift;
$instring =~ s/\s+//g;
$instring =~ s/\"//g;
@agent_list = split(/\|/, $instring);
$vector_val_low = 0;
$vector_val_high = 0;
foreach $a (@agent_list) {
  chomp($a);
  if ($a eq "ALL") {
    $vector_val_low  = 0xFFFFFFFF;
    $vector_val_high = 0xFFFFFFFF;
  } else {
    $shift = $agent_encoding{$a};
    if ($shift > 31) {
      $vector_val_high = $vector_val_high | (1<< ($shift - 32));
    } else {
      $vector_val_low  = $vector_val_low  | (1<< $shift);
    }
  }
}
$vector_val_low_hex  = sprintf "%08lx", $vector_val_low;
$vector_val_high_hex = sprintf "%08lx", $vector_val_high;
$vector_val_hex = "64'h" . $vector_val_high_hex . $vector_val_low_hex;
return ($vector_val_hex);
}
sub str2hex_lsb {
 $instring = shift;
 $instring =~ s/\s+//g;
 $instring =~ s/\"//g;
 @agent_list = split(/\|/, $instring);
 $vector_val = 0;
 foreach $a (@agent_list) {
  chomp($a);
   $shift = $agent_encoding{$a};
  if ($shift < 32) {
   $vector_val = $vector_val + (1<< $shift);
  }
 }

 $vector_val_hex = sprintf "%lx", $vector_val;
 $vector_val_hex = "32'h" . $vector_val_hex;
 return ($vector_val_hex);
}
;
