// pragma cdn_vip_model -class pcie 
// Do not move the above pragma, it belongs at the top of the file.

// Module:                      pcie_ep_mon_phy 
// SOMA file:                   /nfs/sc/disks/slx_1132/schodnek/apr/work_root/a0/apr2/apr-srvr10nm-a0/verif/tb/common/cdn_pcie_pkg/cdn_pcie/pcie_ep_mon_phy.soma
// Initial contents file:       
// Simulation control flags:    
// Activation plus option:      +enable_pcie_ep_mon_phy

// This instantiation interface supports multiple styles of instantiation:
// dynamic activation through a procedural call, dynamic activation through
// a command-line switch, dynamic activation through a SystemVerilog class
// constructor, and traditional unconditional instantiation.
//
// Unconditional instantiation is the default.
//
// Dynamic activation requires use of VIP library libcdnsv.so INSTEAD OF
// libdenpli.so, libdenver.so or libmtipli.so. These libraries cannot
// be used together. See the VIP Dynamic Activation application note on
// support.cadence.com for simulator-specific instructions on how to use
// libcdnsv.so and the + options.
//
// You can use conditional compilation to choose dynamic activation
// by defining one of the following:
//    CDN_VIP_DYNAMIC_ACTIVATION - for all VIP instances
//    CDN_VIP_DYNAMIC_ACTIVATION_PCIE - for all pcie instances
// Your choice means that instances of this type must be explicitly
// activated in order to participate in simulation. This choice
// does NOT perform the activation itself.
//
// An instantiation can be activated dynamically by:
// 1) Calling the activate function or init_activation task (defined below)
//    from procedural code, using a hierarchical reference to the instantiation.
//    Use the task form for a call from a traditional Verilog module.
//    For example:
//       testbench.my_vip_instance.activate();
//    or
//       testbench.my_vip_instance.init_activation(activation_status);
//    This approach gives you instance-specific control.
// 2) Compiling with CDN_VIP_SV_AUTOMATIC_ACTIVATION defined
//    and creating a denaliMemInstance class object for each instance
//    that you want to be active. For example:
//       instance = new("testbench.my_vip_instance");
//    This approach gives you instance-specific control.
// 3) Compiling with CDN_VIP_SV_AUTOMATIC_ACTIVATION defined
//    and using UVM or OVM to create components for each instance
//    that you want to be active.
// 4) Adding +enable_cdn_vip_pcie to your simulation command.
//    This option activates all pcie instances.
// 5) Adding +enable_cdn_vip_all to your simulation command.
//    This option activates all VIP instances.
// 6) Adding +enable_pcie_ep_mon_phy to your simulation command.
//    This option gives you customized command-line control. All instances
//    that use this instantiation interface are activated with this option
//    (see $test$plusargs below). You can use this custom option to:
//    a. group instances by the instantiation interface that they use and 
//       activate them with a single switch.
//    b. activate different kinds of instances with a single switch by using
//       the same custom option in each of their instantiation interfaces.

module pcie_ep_mon_phy
  #(parameter string interface_soma = "${MODEL_ROOT}/verif/tb/common/cdn_pcie_pkg/cdn_pcie/pcie_ep_mon_phy.soma",
    parameter string init_file   = "",
    parameter string sim_control = ""
  )
  (
    input wire TX,
    input wire TX_,
    input wire RX,
    input wire RX_,
    input wire PERST_,
    inout wire CLK_TX,
    inout wire CLK_RX
  );

  // Do not remove or comment out the timescale information.
  // Do not modify the time precision to be less precise.
  // Such changes may cause inaccurate scheduling, simulation problems,
  // undetected errors, or erroneous errors. The time precision
  // must be 10fs or finer for accurate simulation.
  timeunit 10fs;
  timeprecision 10fs;

  reg den_CLK_TX  = 'bz;
      assign CLK_TX = den_CLK_TX;
  reg den_CLK_RX  = 'bz;
      assign CLK_RX = den_CLK_RX;

  localparam model_class = "pcie";

  localparam integration_properties = "config SOMA;";

`ifdef CDN_VIP_DYNAMIC_ACTIVATION
  `define __INTERNAL__CDN_VIP_DA
`elsif CDN_VIP_DYNAMIC_ACTIVATION_PCIE
  `define __INTERNAL__CDN_VIP_DA
`endif

// NOTE: Do not use __INTERNAL__CDN_VIP_DA in your code or
//       on your simulation command line. It is for internal use
//       in this instantiation interface and gets `undef'ed below.
`ifdef __INTERNAL__CDN_VIP_DA

  string instance_path = $sformatf("%m");

  int activated = 0;

  function int activate ( input string act_soma_spec   = interface_soma,
                          input string act_init_file   = init_file, 
                          input string act_sim_control = sim_control 
                        );
    begin
      activated = $__internal__vip_activate_with_params("pcie", instance_path,
      "interface_soma", act_soma_spec, act_init_file, act_sim_control, activated,
      TX,TX_,RX,RX_,PERST_,CLK_TX,den_CLK_TX,CLK_RX,den_CLK_RX);
      return activated;
    end
  endfunction

  task init_activation ( output integer is_activated );
    is_activated = activate();
  endtask

  initial
  begin
    if ($test$plusargs("enable_pcie_ep_mon_phy") ||
        $test$plusargs("enable_cdn_vip_pcie") ||
        $test$plusargs("enable_cdn_vip_all"))
    begin
        void'(activate());
    end
    if (!activated) $__internal__vip_potential_instance(model_class, instance_path);
  end

  `undef __INTERNAL__CDN_VIP_DA

`else

  // Unconditional instantiation
  initial 
    $pcie_access(TX,TX_,RX,RX_,PERST_,CLK_TX,den_CLK_TX,CLK_RX,den_CLK_RX);

`endif

endmodule


