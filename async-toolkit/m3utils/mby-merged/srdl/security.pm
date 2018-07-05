package security;

%agent_encoding = (
  "HOSTIA_POSTBOOT_SAI" => 0, 
  "HOSTIA_UCODE_SAI" => 1, 
  "HOSTIA_SMM_SAI" => 2, 
  "HOSTIA_SUNPASS_SAI" => 3, 
  "HOSTIA_BOOT_SAI" => 4, 
  "SAI_Reserved_5" => 5, 
  "SAI_Reserved_6" => 6,
  "SAI_Reserved_7" => 7,
  "GT_SAI" => 8, 
  "PM_PCS_SAI" => 9, 
  "HW_CPU_SAI" => 10, 
  "MEM_CPL_SAI" => 11, 
  "VTD_SAI" => 12, 
  "SAI_Reserved_13" => 13, 
  "DISPLAY4_SAI" => 14, 
  "HOSTCP_PMA_SAI" => 15, 
  "CSE_INTEL_SAI" => 16, 
  "CSE_OEM_SAI" => 17, 
  "FUSE_CTRL_SAI" => 18, 
  "FUSE_PULLER_SAI" => 19, 
  "SAI_Reserved_20" => 20, 
  "PM_IOSS_SAI" => 21, 
  "CSE_DNX_SAI" => 22, 
  "FXR_INTERNAL_SAI" => 23, 
  "DFX_INTEL_MANUFACTURING_SAI" => 24, 
  "DFX_UNTRUSTED_SAI" => 25, 
  "SAI_Reserved_26" => 26, 
  "IRC_SAI" => 27, 
  "NPK_SAI" => 28, 
  "DISPLAY2_SAI" => 29, 
  "DISPLAY3_SAI" => 30, 
  "HW_PCH_SAI" => 31, 
  "SAI_Reserved_32" => 32, 
  "SAI_Reserved_33" => 33, 
  "SAI_Reserved_34" => 34, 
  "GT_PMA_SAI" => 35, 
  "SAI_Reserved_36" => 36, 
  "SAI_Reserved_37" => 37, 
  "SAI_Reserved_38" => 38, 
  "SAI_Reserved_39" => 39, 
  "UNCORE_PMA_SAI" => 40, 
  "RC_MORPHED_SAI" => 41, 
  "DFX_INTEL_PRODUCTION_SAI" => 42,
  "DFX_THIRDPARTY_SAI" => 43,
  "DISPLAY_SAI" => 44, 
  "SAI_Reserved_45" => 45, 
  "SAI_Reserved_46" => 46, 
  "DISPLAY_KVM_SAI" => 47,
  "GT2_SAI" => 48, 
  "SAI_Reserved_49" => 49, 
  "GT3_SAI" => 50, 
  "SAI_Reserved_51" => 51, 
  "CORE_EVENT_PROXY_SAI" => 52, 
  "GT4_SAI" => 53, 
  "RCIOMMU_BYPASS_SAI" => 54, 
  "SAI_Reserved_55" => 55,
  "SAI_Reserved_56" => 56, 
  "SAI_Reserved_57" => 57, 
  "SAI_Reserved_58" => 58, 
  "SAI_Reserved_59" => 59, 
  "CPM_SAI" => 60, 
  "OOB_MSM_SAI" => 61, 
  "XGBE_SAI" => 62, 
  "DEVICE_UNTRUSTED_SAI" => 63,
);

sub GetSecurityInfo() {
	%security = (
		# HLP_PG0 policy groups
		'HLP_PG0'               => '"HLP_PG0"',
		'HLP_PG0_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG0_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG0_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG1 policy groups
		'HLP_PG1'               => '"HLP_PG1"',
		'HLP_PG1_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG1_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG1_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG2 policy groups
		'HLP_PG2'               => '"HLP_PG2"',
		'HLP_PG2_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG2_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG2_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG3 policy groups
		'HLP_PG3'               => '"HLP_PG3"',
		'HLP_PG3_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG3_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG3_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG4 policy groups
		'HLP_PG4'               => '"HLP_PG4"',
		'HLP_PG4_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG4_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG4_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG5 policy groups
		'HLP_PG5'               => '"HLP_PG5"',
		'HLP_PG5_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG5_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG5_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG6 policy groups
		'HLP_PG6'               => '"HLP_PG6"',
		'HLP_PG6_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG6_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG6_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

		# HLP_PG7 policy groups
		'HLP_PG7'               => '"HLP_PG7"',
		'HLP_PG7_CP_AGENTS'     => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG7_WAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',
		'HLP_PG7_RAC_AGENTS'    => '"CSE_INTEL_SAI | DFX_INTEL_MANUFACTURING_SAI | XGBE_SAI | IRC_SAI"',

	);
	return (%security);
}

sub str2hex {
	$instring = shift;
	$instring =~ s/\s+//g;
	$instring =~ s/\"//g;
	@agent_list = split(/\|/, $instring);
	$vector_val = 0;
	foreach $a (@agent_list) {
		chomp($a);
		$shift = $agent_encoding{$a};
		$vector_val = $vector_val + (1<<$shift);
	}
	$vector_val_hex = sprintf "%llx", $vector_val;
	$vector_val_hex = "64'h".$vector_val_hex;
	return ($vector_val_hex);
}

1;
