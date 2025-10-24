// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2015 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Nelson Crumbaker
// -- Project Name : Madison Bay
// -- Description :
// --
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_pcie_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __PCIE_REPORT_SERVER_SV__
`define __PCIE_REPORT_SERVER_SV__

/*class pcie_report_server extends uvm_report_server;

   static function bit init();
      uvm_report_global_server global_report_server = new;
      pcie_report_server userServer = new;
      global_report_server.set_server(userServer);
   endfunction : init

   virtual function string compose_message(uvm_severity severity, string name,
                                           string id, string message,
                                           string filename, int line);
      uvm_severity_type sev = uvm_severity_type'(severity);
      string  msg_string = "";
      $sformat(msg_string,"[%0t] %0s (%0s): %0s",$time,id,sev.name,message);
      return msg_string;
   endfunction : compose_message

endclass : pcie_report_server
*/
`endif /* __PCIE_REPORT_SERVER_SV__ */

// -------------------------------------------------------------------
//
