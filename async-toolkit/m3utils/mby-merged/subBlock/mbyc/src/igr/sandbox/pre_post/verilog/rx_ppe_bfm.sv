///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author       : Scott Greenfield
// -- Project Name : MBY
// -- Description  : simple RX-PPE BFM for use with IGR testing only
// ---------------------------------------------------------------------------------------------------------------------

module rx_ppe_bfm
  import mby_igr_pkg::*, mby_egr_pkg::*, shared_pkg::*;
  (
   input cclk, 
   input rst,

   input igr_rx_ppe_tail_t          igr_rx_ppe_intf0_tail, // Blasted Interface igr_rx_ppe_if.igr igr_rx_ppe
   input igr_rx_ppe_head_t          igr_rx_ppe_intf0_head, 
   output logic                     igr_rx_ppe_intf0_ack, 
   input igr_rx_ppe_tail_t          igr_rx_ppe_intf1_tail, 
   input igr_rx_ppe_head_t          igr_rx_ppe_intf1_head, 
   output logic                     igr_rx_ppe_intf1_ack, 

   output rx_ppe_igr_t              rx_ppe_igr_intf0, // Blasted Interface rx_ppe_igr_if.igr rx_ppe_igr
   output rx_ppe_igr_t              rx_ppe_igr_intf1


   );

    parameter PIPE_DEPTH=160;
    
    igr_rx_ppe_head_t [PIPE_DEPTH-1:0] intf0_queue;

    rx_ppe_igr_t tmp;

//  typedef struct packed {
//      logic   [122:0]                 reserved;
//      logic   [3:0]                   packet_type;
//      logic   [23:0]                  mod_profile;
//      logic   [19:0]                  mod_md;
//      logic   [11:0]                  ivid;
//      logic   [11:0]                  evid;
//      logic   [3:0]                   vpri;
//      logic   [1:0]                   tx_tag;
//      logic   [3:0]                   tag_flags;      //parser flags
//      logic   [47:0]                  dmac;
//      logic   [7:0]                   rx_port;        //source port
//      logic   [5:0]                   l3d;            //L3 domain
//      logic   [IGR_PPE_TS_WIDTH-1:0]  igr_ts;         //Ingress timestamp
//      logic   [7:0]                   l3_mcgrpid;
//      logic                           tx_drop;
//      logic   [5:0]                   dscp;
//      logic   [3:0]                   ttl_ctl;
//      logic   [1:0]                   ecn_val;
//      logic                           aqm_mark_en;
//      logic   [63:0]                  hdr_ptr;
//      logic   [63:0]                  hdr_ptr_offset;
//  } rx_ppe_igr_port_md_t;
//  
//  typedef union packed {
//      rx_ppe_igr_cpp_md_t     cpp_md;     //CPP metadata
//      rx_ppe_igr_port_md_t    port_md;    //Normal port metadata
//  } rx_ppe_igr_md_t;
//  
//  typedef struct packed {
//      rx_ppe_igr_md_t                         md;             //metadata
//      logic   [$clog2(POLICER_CNT)-1:0]       policer_idx;    //Policer index
//      logic   [$clog2(MGP_TC_CNT)-1:0]        tc;             //Egress TC
//      logic   [$clog2(MGP_PORT_CNT)-1:0]      port;           //egress port ID
//      logic   [2:0]                           pkt_type;       //one hot packet type indication, bit2:INT, bit1:CPP, bit0:normal
//      logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;             //header segment packet ID
//      logic                                   valid;          //valid
//  } rx_ppe_igr_t;
//  

    always_comb begin
        tmp = '0;

        // FIXME -- hardcoded for first packet, but some of these should either
        //            be randomized, or looked up from randomized data in inp_driver
        tmp.valid       = intf0_queue[PIPE_DEPTH-1].valid;
        tmp.id          = intf0_queue[PIPE_DEPTH-1].md.id;
        tmp.pkt_type    = 3'b001; 
        tmp.port        = 8'hAB;   // FIXME -- rx_ppe_igr_t has 5 bits, shouldn't be using MGP_PORT_CNT for this
        tmp.tc          = 4'h1;
        tmp.policer_idx = 12'h000;
        tmp.md.port_md.packet_type = 4'b0000;
        tmp.md.port_md.rx_port = 8'h00; // FIXME -- I might have rx_port in upper bits of ID, but that would be redundant
        tmp.md.port_md.tx_drop = 1'b0;
        
        
    end
    
    
    always_ff @( posedge cclk ) begin

        if( rst ) begin

            intf0_queue = {PIPE_DEPTH{igr_rx_ppe_head_t'('0)}};
    
            rx_ppe_igr_intf0 <= rx_ppe_igr_t'('0);

        end else begin

            intf0_queue[0] <=  igr_rx_ppe_intf0_head;
            
            for( int i=1; i<PIPE_DEPTH; i++ )
              begin
                  intf0_queue[i] <= intf0_queue[i-1];
              end

            rx_ppe_igr_intf0 <= tmp;
            

        end
        
    end
  

  
//typedef struct packed {
//    vp_cpp_rx_md_t                          cpp_md; //Cport metadata
//    logic   [IGR_PPE_TS_WIDTH-1:0]          ts;     //header segment timestamp
//    logic   [$clog2(MGP_TC_CNT)-1:0]        tc;     //header segment TC
//    logic   [$clog2(MGP_PORT_CNT)-1:0]      port;   //header segment incoming port ID
//    logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;     //header segment packet ID
//} igr_rx_ppe_md_t;

//  typedef struct packed {
//    igr_rx_ppe_md_t                     md;     //header segment metadata
//    logic   [IGR_PPE_ECC_WIDTH-1:0]     ecc;    //header segment ECC
//    logic   [IGR_PPE_DATA_WIDTH-1:0]    data;   //header segment data
//    logic                               valid;  //header segment valid
// } igr_rx_ppe_head_t;
   


    
endmodule // rx_ppe_bfm

