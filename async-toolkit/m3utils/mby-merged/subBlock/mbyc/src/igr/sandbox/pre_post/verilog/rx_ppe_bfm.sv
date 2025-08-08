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
  import mby_igr_pkg::*, mby_egr_pkg::*, mby_rx_metadata_pkg::*, shared_pkg::*;
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


  // from shared/rtl/mby_rx_metadata_pkg.sv  
  //   typedef enum logic [3:0] {
  //      MC          = 4'h0,              // MC.
  //      UC          = 4'h1,              // UC.
  //      MIRROR_MC   = 4'h2,              // Mirror MC.
  //      MIRROR_UC   = 4'h3,              // Mirror UC.
  //      INT_MC      = 4'h4,              // INT MC.
  //      INT_UC      = 4'h5,              // INT UC.
  //      VP_MC       = 4'h8,              // Virtual Port MC
  //      VP_UC       = 4'h9               // Virtual Port UC
  //   } pkt_type_t;                    // UC, Mirror, INT, VP
  //   
  //    
 
  //  typedef struct packed {
  //     header_offset_t           header_offset;            
  //     protocol_id_t             protocol_id;       
  //     aqm_quantized_val_t       aqm_quantized_val;          
  //     logic                     aqm_mark_en;            // Active Queue Management Enable
  //     ecn_val_t                 ecn_val;                    
  //     ttl_ctl_t                 ttl_ctl;                    
  //     dscp_t                    dscp;                       
  //     nh_egr_mirror_idx_t       nh_egr_mirror_idx;
  //     mirror_profile_idx_t      mirror_profile_idx2;  
  //     mirror_profile_idx_t      mirror_profile_idx1;  
  //     tag_flags_t               tag_flags;                  
  //     iptag_t                   iptag;                     
  //     vpri_t                    vpri;                           
  //     nh_egr_negh_table_idx_t   nh_egr_negh_table_idx;          
  //     l2d_t                     l2d;                            
  //     ivid_t                    ivid;                           
  //     logic                     route_flag;             // Indicate to Modify the packet should be routed (from Nexthop)
  //     mod_metadata_t            mod_metadata;                   
  //     mod_profile_t             mod_profile;                    
  //  } unicast_meta_t;          
  //    
  //   typedef struct packed {
  //      logic [`MD_MSB-`UC_SIZE:0]   reserved;
  //      unicast_meta_t               unicast_meta;                
  //   } uc_meta_t;               
  //
  //   typedef union packed {
  //      uc_meta_t                     uc_meta;
  //      mc_meta_t                     mc_meta;
  //      uc_telemetry_meta_t           uc_telemetry_meta;
  //      mc_telemetry_meta_t           mc_telemetry_meta;
  //      uc_vport_meta_t               uc_vport_meta;
  //      mc_vport_meta_t               mc_vport_meta;      
  //      uc_int_meta_t                 uc_int_meta;
  //      mc_int_meta_t                 mc_int_meta;      
  //   } ppe_no_pt_meta_data_t;
  //
  //   typedef struct packed {
  //      ppe_no_pt_meta_data_t         md;
  //      pkt_type_t                    pkt_type;           
  //   } ppe_meta_data_t;
  //
  //   typedef struct packed {
  //      ppe_meta_data_t                         md;             //metadata
  //      logic   [$clog2(POLICER_CNT)-1:0]       policer_idx;    //Policer index
  //      logic   [$clog2(MGP_TC_CNT)-1:0]        tc;             //Egress TC
  //      logic   [$clog2(MBY_PORT_CNT)-1:0]      port;           //egress port ID
  //      logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;             //header segment packet ID
  //      logic                                   valid;          //valid
  //   } rx_ppe_igr_t;

    always_comb begin
        tmp = rx_ppe_igr_t'('0);

        // FIXME -- hardcoded for first packet, but some of these should either
        //            be randomized, or looked up from randomized data in inp_driver
        tmp.valid       = intf0_queue[PIPE_DEPTH-1].valid;
        tmp.id          = intf0_queue[PIPE_DEPTH-1].md.id;
        tmp.port        = 9'hAB;
        tmp.tc          = 4'h1;
        tmp.policer_idx = 12'hfed;
        tmp.md.pkt_type = UC;
        //tmp.md.port_md.rx_port = 8'h00; // FIXME -- I might have rx_port in upper bits of ID, but that would be redundant
        //tmp.md.port_md.tx_drop = 1'b0;
        
        tmp.md.md.uc_meta.unicast_meta.header_offset                = 64'h1122334455667788;
        tmp.md.md.uc_meta.unicast_meta.protocol_id                  = 64'haabbccddeeff0011;       
        tmp.md.md.uc_meta.unicast_meta.aqm_quantized_val            = 4'h0;          
        tmp.md.md.uc_meta.unicast_meta.aqm_mark_en                  = '0; 
        tmp.md.md.uc_meta.unicast_meta.ecn_val                      = '0;                    
        tmp.md.md.uc_meta.unicast_meta.ttl_ctl                      = 4'hf;                    
        tmp.md.md.uc_meta.unicast_meta.dscp                         = 6'h0;                       
        tmp.md.md.uc_meta.unicast_meta.nh_egr_mirror_idx            = 4'he;              
        tmp.md.md.uc_meta.unicast_meta.mirror_profile_idx1          = 6'h0;  
        tmp.md.md.uc_meta.unicast_meta.mirror_profile_idx2          = 6'h0;  
        tmp.md.md.uc_meta.unicast_meta.tag_flags                    = '0;                  
        tmp.md.md.uc_meta.unicast_meta.iptag                        = '0;                     
        tmp.md.md.uc_meta.unicast_meta.vpri                         = '0;
        tmp.md.md.uc_meta.unicast_meta.nh_egr_negh_table_idx        = 14'h3344;              
        tmp.md.md.uc_meta.unicast_meta.l2d                          = '0;
        tmp.md.md.uc_meta.unicast_meta.ivid                         = 12'habc;
        tmp.md.md.uc_meta.unicast_meta.route_flag                   = '0;  
        tmp.md.md.uc_meta.unicast_meta.mod_metadata                 = 20'h12345;
        tmp.md.md.uc_meta.unicast_meta.mod_profile                  = 24'h123456;
        


    end
    
    
    always_ff @( posedge cclk ) begin

        if( rst ) begin

            intf0_queue = {PIPE_DEPTH{igr_rx_ppe_head_t'('0)}};
    
            rx_ppe_igr_intf0 <= rx_ppe_igr_t'('0);
            rx_ppe_igr_intf1 <= rx_ppe_igr_t'('0);

        end else begin

            intf0_queue[0] <=  igr_rx_ppe_intf0_head;
            
            for( int i=1; i<PIPE_DEPTH; i++ )
              begin
                  intf0_queue[i] <= intf0_queue[i-1];
              end

            rx_ppe_igr_intf0 <= tmp;
            
            rx_ppe_igr_intf1 <= rx_ppe_igr_t'('0);

        end
        
    end
  

    
endmodule // rx_ppe_bfm

