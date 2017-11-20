package tlm_ip;
use lib "$ENV{RTL_PROJ_BIN}/perllib";
use ToolConfig;

use File::Basename;
use vars qw(@ISA @EXPORT_OK $___tmp);
require Exporter;
@ISA = qw(Exporter);
my $site = `domainname`;
chomp $site;
format STDOUT = 
    |Setting @<<<<<<<<<<<<<<<< to @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<|
             $__tmp,               $ENV{$__tmp}
.
format STDOUT_TOP = 

          TLM1udio IP revisions values
    -------  IP                   Release-------------------------------------------------------------------------------------------
    ________________________________________________________________________________________________________________________________
.



#-------------------------------------------------------------------------------
sub dprint(@) {
   print @_ if (defined $ENV{IP_CONFIG_DEBUG});
}
#-------------------------------------------------------------------------------
sub set_sip_path (@) {
	my ($ip, $path) = @_;
	$SIP_RELEASE{$ip} = $path;
	$ENV{$ip}= $path;
	$__tmp=$ip;
#	write;
}

#-------------------------------------------------------------------------------
use vars qw (
   %SIP_RELEASE
    );
@EXPORT_OK = qw(
       %SIP_RELEASE
                );

$DB::single = 1;

set_sip_path ( TLM1_IOSF_PRI_VC_ROOT     , &ToolConfig::get_tool_path('ipconfig/iosf_primary_bfm') . "/ph4");
set_sip_path ( TLM1_IOSF_SB_VC_ROOT      ,&ToolConfig::get_tool_path('ipconfig/iosf_sideband_vc'));
set_sip_path ( TLM1_CHASSIS_PG_VC_ROOT   ,&ToolConfig::get_tool_path('ipconfig/chassis_pg_vc'));
set_sip_path ( TLM1_CHASSIS_VCC_VC_ROOT  ,&ToolConfig::get_tool_path('ipconfig/vcc_modeling'));
set_sip_path ( TLM1_CHASSIS_RST_VC_ROOT  ,&ToolConfig::get_tool_path('ipconfig/chassis_reset'));
set_sip_path ( TLM1_CHASSIS_CCU_VC_ROOT  ,&ToolConfig::get_tool_path('ipconfig/ip_ccu_vc') . "/ccu_vc_WW21");


print "    ________________________________________________________________________________________________________________________________\n\n\n";


1;
