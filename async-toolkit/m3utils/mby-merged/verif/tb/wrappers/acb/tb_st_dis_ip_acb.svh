// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Chandran,Ajay K 
// Created On   :  09/22/2016
// Description  :  Disable assertions for statically disabled IPs
// -----------------------------------------------------------------------------

function automatic void set_disable_ind(FC::EBGIp_t ip);
    string _ipn;
    string _fn;
    int _fh;
    _ipn = ip.name();
    _fn = {_ipn,"_disabled.txt"};
    _fh    = $fopen(_fn,"w");
    $fdisplay(_fh,"%s is disabled",_ipn);
    $fclose(_fh);
endfunction:set_disable_ind

always @(fc_sig_if.ip_enable) begin
    // the delay is required to overcome a race between the time Disable_final_checks flag is constrained and the time when the flag is overriden again
    // in the primary_intf strap driving function
    @(fc_sig_if.tb_clk);
    $display("%s : Setting up IP enable flag",`__FILE__);
 *  ----------------
 *    End of file
 *  ----------------
*/
// <<< VIM SETTINGS
// vim: ts=4 et 
// >>>
