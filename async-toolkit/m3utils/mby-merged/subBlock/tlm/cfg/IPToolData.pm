
package ToolData;
$ToolConfig_tools{ipconfig} = {
    PATH => "$MODEL_ROOT",
    SUB_TOOLS => \%ToolConfig_ips,
};


$ToolConfig_ips{chassis_pg_vc} = {
    PATH => "$ENV{IP_RELEASES}/ChassisPowerGatingVIP/&get_tool_version()",
    VERSION => "ChassisPowerGatingVIP_2013WW30",
};

$ToolConfig_ips{vcc_modeling} = {
    PATH => "$ENV{IP_RELEASES}/VccModeling/&get_tool_version()",
    VERSION => "VccModeling_v130227",
};
$ToolConfig_ips{iosf_primary_bfm} = {
    PATH => "$ENV{IP_RELEASES}/iosf_primary_bfm/&get_tool_version()",
    VERSION => "iosf_primary_bfm",
};
$ToolConfig_ips{iosf_sideband_vc} = {
    PATH => "$ENV{IP_RELEASES}/iosf_sideband_vc/&get_tool_version()",
    VERSION => "iosf_sideband_vc_template",
};

$ToolConfig_ips{ip_ccu_vc} = {
    PATH => "$ENV{IP_RELEASES}/ip-ccu-vc/&get_tool_version()",
    VERSION => "ip-ccu-vc-2013WW21r130521",
};

$ToolConfig_ips{chassis_reset} = {
    PATH => "$ENV{IP_RELEASES}/chassis_reset_pkg/&get_tool_version()",
    VERSION => "chassis_reset_pkg_2013WW30",
};





1;
