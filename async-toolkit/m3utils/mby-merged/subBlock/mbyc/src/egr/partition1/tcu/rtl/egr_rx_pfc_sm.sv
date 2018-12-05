/*%W%   %G%*/

/**************************************************************
 * File Name    : egr_rx_pfc_sm.sv
 * Author Name  : John Lo
 * Description  : 
 * Parent Module: 
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-22
 * Notes        : cclk, hreset_n, sreset_n, egr_reset_n
 *
 ***************************************************************/

//`include  "mby_egr_epl_defines.svh"

module egr_rx_pfc_sm (
  // epl_rx_pfc_if
  input  logic [2:0]                    rx_flow_control_tc, 
  input  logic                          rx_pfc_xoff,       
  input  logic [1:0]                    rx_port_num,       
  // epl_pkt_scheduler_if
  output logic [7:0]                    pfc_xoff_port0,
  output logic [7:0]                    pfc_xoff_port1,
  output logic [7:0]                    pfc_xoff_port2,
  output logic [7:0]                    pfc_xoff_port3,
  // csr_if
  input  logic [3:0]                    port_config,   // 0: 4ch, 1: 1 ch, 2: 2 ch, 3: rsvd
  // global signals
  input  logic 	                        reset_n,
  input  logic 	                        clk   
  );

 
  typedef enum logic [1:0] {TDM_SLOT0            = 2'b00 
                           ,TDM_SLOT1            = 2'b01
                           ,TDM_SLOT2            = 2'b10 
                           ,TDM_SLOT3            = 2'b11
                           } statetype;

  statetype                state, nx_state ;
  // seq part
  always_ff @ (posedge clk) begin 
    if (!reset_n) begin
      state          <= TDM_SLOT0;
    end
    else begin
      state          <= nx_state;
    end
  end 
 
    
endmodule : egr_rx_pfc_sm


