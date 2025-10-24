// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2015 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Nelson Crumbaker
// -- Project Name : Madison Bay
// -- Description : Configuration file for PCIe RC BFM for DMA transfers
// --
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_tb_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __MBY_CP_PCIE_CFG_SV__
`define __MBY_CP_PCIE_CFG_SV__

`include "pcie_defines.svh"

class mby_cp_pcie_cfg extends uvm_object;

    rand int                num_send_dma_xfers;       // Number of Send DMA transfers
    rand dma_queue_type     send_dma_queue[];         // Queue for each transfer
    rand int                send_dma_hostlen4b[];     // Host Memory Buffer Size for each transfer
    rand int                num_send_dma_q0_buffers;  // Number of Send DMA buffers for Queue0
    rand bit                send_dma_q0_tlp32bit[];   // 32 or 64 bit TLPs for each buffer
    rand int                num_send_dma_q1_buffers;  // Number of Send DMA buffers for Queue1
    rand bit                send_dma_q1_tlp32bit[];   // 32 or 64 bit TLPs for each buffer

    rand int                num_recv_dma_q2_buffers;  // Number of Recv DMA buffers for Queue2
    rand bit                recv_dma_q2_tlp32bit[];   // 32 or 64 bit TLPs for each buffer
    rand int                num_recv_dma_q3_buffers;  // Number of Recv DMA buffers for Queue3
    rand bit                recv_dma_q3_tlp32bit[];   // 32 or 64 bit TLPs for each buffer

    rand bit                pcie_payload_8dw;         // Length of DMA Memory Read/Write TLPs
    // should be limited to 8DWords
    rand bit                pcie_ctrl0_init;          // Byte swap for data going to/from CBUF
    rand bit                pcie_ctrl0_target;        // Byte swap for CSR accesses
    rand bit                pcie_ctrl0_tag;           // Byte swap for CP Tag Out value
    rand bit                pcie_ctrl0_xbar;           // Byte swap for CP Tag Out value
    rand pcie_reset_type    pcie_mid_test_reset;      // Type of reset to be asserted in the middle of the test
    rand int                pcie_max_payld_size;      // Randomize Max Payload Size supported by RC BFM
    rand int                pcie_max_rdreq_size;      // Randomize Max Payload Size supported by RC BFM
    bit                     ral_access = 1'b0; // default access
    bit                     hot_reset_detected = 1'b0; //default
    bit                     mid_test_reset_det = 1'b0; //default
    int                     total_send_desc_filled = 0;
    //bit          induce_que_ovf_err[4];
    rand logic [3:0]        induce_que_ovf_err;
    rand int q0_wtg, q1_wtg, q2_wtg, q3_wtg;

    //PCIe RAM/FIFO SBE count
    rand int pcie_ram_err_count;
    rand int pcie_fifo_err_count;

    `uvm_object_utils_begin(mby_cp_pcie_cfg)
        `uvm_field_int(num_send_dma_xfers, UVM_ALL_ON | UVM_DEC);
        `uvm_field_array_enum(dma_queue_type, send_dma_queue, UVM_ALL_ON);
        `uvm_field_array_int(send_dma_hostlen4b, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(num_send_dma_q0_buffers, UVM_ALL_ON | UVM_DEC);
        `uvm_field_array_int(send_dma_q0_tlp32bit, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(num_send_dma_q1_buffers, UVM_ALL_ON | UVM_DEC);
        `uvm_field_array_int(send_dma_q1_tlp32bit, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(num_recv_dma_q2_buffers, UVM_ALL_ON | UVM_DEC);
        `uvm_field_array_int(recv_dma_q2_tlp32bit, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(num_recv_dma_q3_buffers, UVM_ALL_ON | UVM_DEC);
        `uvm_field_array_int(recv_dma_q3_tlp32bit, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(q0_wtg, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(q1_wtg, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(q2_wtg, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(q3_wtg, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(pcie_payload_8dw, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(pcie_ctrl0_init, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(pcie_ctrl0_target, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(pcie_ctrl0_tag, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(pcie_ctrl0_xbar, UVM_ALL_ON | UVM_HEX);
        `uvm_field_enum(pcie_reset_type, pcie_mid_test_reset, UVM_ALL_ON);
        `uvm_field_int(induce_que_ovf_err, UVM_ALL_ON | UVM_HEX);
        `uvm_field_int(pcie_max_payld_size, UVM_ALL_ON | UVM_DEC);
        `uvm_field_int(pcie_max_rdreq_size, UVM_ALL_ON | UVM_DEC);
        `uvm_field_int(hot_reset_detected, UVM_ALL_ON | UVM_DEC);
        `uvm_field_int(mid_test_reset_det, UVM_ALL_ON | UVM_DEC);
        `uvm_field_int(total_send_desc_filled, UVM_ALL_ON | UVM_DEC);
        `uvm_field_int(pcie_ram_err_count, UVM_ALL_ON | UVM_DEC);
        `uvm_field_int(pcie_fifo_err_count, UVM_ALL_ON | UVM_DEC);
    `uvm_object_utils_end

    function new(string name = "mby_cp_pcie_cfg");
        super.new(name);
    endfunction: new

    constraint c_def_q_ovf_err {
        soft     induce_que_ovf_err == 4'b0000;
    };

    constraint c_def_recv_que_poll_dist {
        q2_wtg == 50;
        q3_wtg == 50;
    };

    constraint c_def_send_que_poll_dist {
        q0_wtg == 50;
        q1_wtg == 50;
    };

    constraint c_num_send_dma_xfers {
        soft num_send_dma_xfers inside {[10:60]};
    };

    //AK: updated to always use all 16 (fifo depth) buffer space in the fifo
    constraint c_num_send_dma_q0_buffers {
        num_send_dma_q0_buffers == 16;       //inside {[10:16]};
    };

    constraint c_num_send_dma_q1_buffers {
        num_send_dma_q1_buffers == 16;       //inside {[10:16]};
    };

    constraint c_num_recv_dma_q2_buffers {
        num_recv_dma_q2_buffers == 16;        // inside {[10:16]};
    };

    constraint c_num_recv_dma_q3_buffers {
        num_recv_dma_q3_buffers == 16;         //inside {[10:16]};
    };

    constraint c_array_sizes {
        solve num_send_dma_xfers before send_dma_queue, send_dma_hostlen4b;
        solve num_send_dma_q0_buffers before send_dma_q0_tlp32bit;
        solve num_send_dma_q1_buffers before send_dma_q1_tlp32bit;
        solve num_recv_dma_q2_buffers before recv_dma_q2_tlp32bit;
        solve num_recv_dma_q3_buffers before recv_dma_q3_tlp32bit;

        send_dma_queue.size == num_send_dma_xfers;
        //send_dma_hostlen4b.size == num_send_dma_xfers;
        send_dma_q0_tlp32bit.size == num_send_dma_q0_buffers;
        send_dma_q1_tlp32bit.size == num_send_dma_q1_buffers;
        recv_dma_q2_tlp32bit.size == num_recv_dma_q2_buffers;
        recv_dma_q3_tlp32bit.size == num_recv_dma_q3_buffers;
        send_dma_hostlen4b.size == num_send_dma_xfers + num_send_dma_q0_buffers + num_send_dma_q1_buffers;
    };

    constraint c_send_dma_queue {
        foreach (send_dma_queue[i]) {
            // Send DMA uses Queues 0 & 1
            send_dma_queue[i] inside {QUEUE_0, QUEUE_1};
        }
    };

    constraint c_send_dma_hostlen4b {
        foreach (send_dma_hostlen4b[i]) {
            // Set to actual size for SEND queues
            send_dma_hostlen4b[i] inside {[`MIN_PCIE_PKTLEN_IN_DWORDS:`MAX_PKTLEN_IN_DWORDS]};
        }
    };

    constraint c_pcie_mid_test_reset {
        pcie_mid_test_reset == NO_RESET;
    };

    constraint c_pcie_max_payld_size {
        pcie_max_payld_size == 128; //DUT Default
    //pcie_max_payld_size inside {128, 256, 512, 1024, 2048, 4096};
    };

    constraint c_pcie_max_rdreq_size {
        pcie_max_rdreq_size == 512; //DUT Default
    //pcie_max_rdreq_size inside {128, 256, 512, 1024, 2048, 4096};
    };


    constraint c_pcie_err_cnt {
        soft pcie_ram_err_count == 0;
        soft pcie_fifo_err_count == 0;

    };

endclass : mby_cp_pcie_cfg

`endif  /* __MBY_CP_PCIE_CFG_SV__ */

// -------------------------------------------------------------------
//
