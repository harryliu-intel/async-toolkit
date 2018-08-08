#/////////////////////////////////////////////////////////////////////////////
#/
#/ Copyright (c) Intel Corp. All Rights Reserved.
#/ Unpublished Proprietary Information.
#/
#/ Function:       Makefile for compiling, running shared verilog testbenches
#/ Authored by:    Jon Bagge
#/
#/////////////////////////////////////////////////////////////////////////////

PPE_SRC = ..
DV_SRC = ../sandbox/dv/verilog

SHARED_SRC = ../../shared
ASSERT_LIB = /nfs/site/disks/ccdo.soc.cad_root.0/cad/x86-64_linux26/dt/sva_lib/6.0p1
SYN_DW = /nfs/site/eda/data/eda_0004/synopsys/designcompiler/I-2013.12-SP3
SIM_DIR = dv/run

INCS =	$(PPE_SRC)/hlp_verilog/parser
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fgrp
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fghash
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fcmn
INCS +=	$(PPE_SRC)/hlp_verilog/fwd
INCS +=	$(PPE_SRC)/hlp_verilog/tail
INCS +=	$(PPE_SRC)/hlp_verilog/tail/pol
INCS +=	$(PPE_SRC)/hlp_verilog/tail/saf
INCS +=	$(PPE_SRC)/hlp_verilog/tail/rx_stats
INCS +=	$(PPE_SRC)/hlp_verilog/tail/triggers_usage
INCS +=	$(PPE_SRC)/hlp_verilog/fwd/hash
INCS +=	$(PPE_SRC)/hlp_verilog/fwd/link_agg
INCS +=	$(PPE_SRC)/hlp_verilog/fwd/ma_lookup
INCS +=	$(PPE_SRC)/hlp_verilog/fwd/triggers
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fgrp/fgrp_action_slice
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fgrp/fgrp_tcam_slice
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fgrp/map
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fghash/fghash_key_pack
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fcmn/ffu_mgmt
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fcmn/mapper
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fcmn/hash_entry_ram
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fcmn/entropy
INCS +=	$(PPE_SRC)/hlp_verilog/ffu/fcmn/mpls_mux
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/parser
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fgrp
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fghash
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fcmn
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/fwd
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/tail
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/parser/mem_shell
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fgrp/mem_shell
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fghash/mem_shell
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fcmn/mem_shell
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/fwd/mem_shell
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/tail/mem_shell
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/parser/mem_wrap
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fgrp/mem_wrap
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fghash/mem_wrap
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/ffu/fcmn/mem_wrap
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/fwd/mem_wrap
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen/tail/mem_wrap
INCS +=	$(PPE_SRC)/hlp_verilog/mem_gen_static
INCS +=	$(PPE_SRC)/hlp_verilog/common
INCS +=	$(PPE_SRC)/hlp_verilog/common/intr_junc
INCS +=	$(PPE_SRC)/hlp_verilog/common/hash
INCS +=	$(PPE_SRC)/hlp_verilog/lib/fifo
INCS +=	$(PPE_SRC)/hlp_verilog/lib/sram
INCS +=	$(PPE_SRC)/hlp_verilog/lib/mgmt
INCS +=	$(PPE_SRC)/hlp_verilog/lib/tcam
INCS +=	$(PPE_SRC)/hlp_verilog/lib/interrupt
INCS +=	$(PPE_SRC)/hlp_verilog/lib/crc
INCS +=	$(PPE_SRC)/hlp_verilog/lib/mux
INCS +=	$(PPE_SRC)/hlp_verilog/lib/csum
INCS +=	$(PPE_SRC)/hlp_verilog/lib/lsfr
INCS +=	$(PPE_SRC)/hlp_verilog/lib/dff_reg
INCS +=	$(PPE_SRC)/hlp_verilog/lib/stats
INCS +=	$(PPE_SRC)/hlp_verilog/lib/AW
INCS +=	$(PPE_SRC)/hlp_verilog/lib/cdp
INCS += $(PPE_SRC)/hlp_verilog/ctech
INCS +=	$(PPE_SRC)/hlp_verilog/bb
INCS += $(SYN_DW)/common/dw/sim_ver
INCS +=	$(PPE_SRC)/hlp_sva
INCS +=	$(PPE_SRC)/hlp_sva/imn
INCS +=	$(PPE_SRC)/hlp_sva/lib/fifo
INCS +=	$(PPE_SRC)/hlp_sva/lib/sram
INCS +=	$(PPE_SRC)/hlp_sva/lib/cdp
INCS +=	$(PPE_SRC)/hlp_sva/parser
INCS +=	$(PPE_SRC)/hlp_sva/ffu/fgrp
INCS +=	$(PPE_SRC)/hlp_sva/ffu/fghash
INCS +=	$(PPE_SRC)/hlp_sva/ffu/fcmn
INCS +=	$(PPE_SRC)/hlp_sva/ffu/fcmn/mapper
INCS +=	$(PPE_SRC)/hlp_sva/ffu/fcmn/hash_entry_ram
INCS +=	$(PPE_SRC)/hlp_sva/fwd
INCS +=	$(PPE_SRC)/hlp_sva/tail
INCS += $(ASSERT_LIB)/SVA_LIB

PPE_INCS = $(PPE_SRC)/rtl/rx_ppe
PPE_INCS += $(SHARED_SRC)/rtl
PPE_INCS += $(SHARED_SRC)/interfaces

VCS_INC = $(addprefix +incdir+,$(INCS))

PPE_VCS_INC = $(addprefix +incdir+,$(PPE_INCS))

VCS_LIB = $(addprefix -y ,$(INCS))

PPE_VCS_LIB = $(addprefix -y ,$(PPE_INCS))
PPE_VCS_LIB += $(addprefix -y ,$(DV_SRC))

VCS_SRC  = $(PPE_SRC)/hlp_verilog/lib/pkt/hlp_pkt_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hlp_register_constants_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hlp_pkt_meta_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hlp_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hlp_ipp_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hlp_register_struct.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/lib/crc/hlp_crc_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/lib/csum/hlp_csum_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hash/hlp_hash_block_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/common/hlp_dfx_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/lib/tcam/hlp_tcam_pkg.sv
VCS_SRC  += $(PPE_SRC)/hlp_verilog/parser/hlp_pa_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/ffu/hlp_ffu_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/ffu/fcmn/ffu_mgmt/hlp_ffu_mgmt_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/ffu/fcmn/mapper/hlp_mapper_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/fwd/hlp_fwd_pkg.vh
VCS_SRC  += $(PPE_SRC)/hlp_verilog/tail/hlp_tail_pkg.vh
VCS_SRC  +=	$(PPE_SRC)/hlp_verilog/lib/sram/hlp_ecc.v

PPE_VCS_SRC  =	$(SHARED_SRC)/rtl/shared_pkg.sv
PPE_VCS_SRC  +=	$(SHARED_SRC)/interfaces/igr_pre_ppe_rx_ppe_if.sv
PPE_VCS_SRC  +=	$(SHARED_SRC)/interfaces/rx_ppe_igr_post_ppe_if.sv
PPE_VCS_SRC  +=	$(SHARED_SRC)/interfaces/igr_pull_sched_rx_ppe_if.sv
PPE_VCS_SRC  +=	$(SHARED_SRC)/interfaces/rx_ppe_tx_ppe_if.sv
PPE_VCS_SRC  +=	$(SHARED_SRC)/interfaces/rx_ppe_ppe_stm_if.sv
PPE_VCS_SRC  += $(SHARED_SRC)/interfaces/ahb_rx_ppe_if.sv
PPE_VCS_SRC  += $(SHARED_SRC)/interfaces/glb_rx_ppe_if.sv

VCS_FLAGS = -full64 +v2k -sverilog -notice +vcs+lic+wait +warn=noUII-L \
    -timescale=10ps/10ps -debug_pp +libext+.v+.sv+.vh+.svh+.vs+.vm+.sva \
    -assert svaext +define+VCSSIM \
    -Mdir=$(SIM_DIR)/csrc -o $(SIM_DIR)/$@

LINTRA_INC = $(addprefix --include ,$(INCS))
LINTRA_TOP = hlp_parser

default: ppe_stm

.PHONY: rx_ppe ppe_stm hlp_parser_apr hlp_fgrp_apr hlp_fcmn_apr hlp_fwd_apr hlp_tail_apr lintra clean

rx_ppe: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(PPE_VCS_LIB) $(PPE_VCS_INC) $(PPE_VCS_SRC) $(DV_SRC)/rx_ppe_tb.sv | tee $(SIM_DIR)/rx_ppe.log

ppe_stm: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(PPE_VCS_LIB) $(PPE_VCS_INC) $(SIM_DIR)/../verilog/ppe_stm_tb.sv | tee $(SIM_DIR)/ppe_stm.log

hlp_parser_apr: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(PPE_SRC)/hlp_verilog/parser/hlp_parser_apr.sv | tee $(SIM_DIR)/hlp_parser_apr.log

hlp_fgrp_apr: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(PPE_SRC)/hlp_verilog/ffu/fgrp/hlp_fgrp_apr.sv | tee $(SIM_DIR)/hlp_fgrp_apr.log

hlp_fcmn_apr: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(PPE_SRC)/hlp_verilog/ffu/fcmn/hlp_fcmn_apr.sv | tee $(SIM_DIR)/hlp_fcmn_apr.log

hlp_fwd_apr: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(PPE_SRC)/hlp_verilog/fwd/hlp_fwd_apr.sv | tee $(SIM_DIR)/hlp_fwd_apr.log

hlp_tail_apr: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(PPE_SRC)/hlp_verilog/tail/hlp_tail_apr.sv | tee $(SIM_DIR)/hlp_tail_apr.log

hlp_rx_stats: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(PPE_SRC)/hlp_verilog/tail/rx_stats/hlp_rx_stats.sv | tee $(SIM_DIR)/hlp_rx_stats.log

#lintra:
#	@../../../scripts/runlintra $(LINTRA_TOP) $(LINTRA_INC) $(VCS_SRC) >lintra.log
lintra:
	@../../../scripts/runlintra $(LINTRA_TOP) $(LINTRA_INC) >lintra.log

clean:
	rm -rf $(SIM_DIR)/csrc $(SIM_DIR)/*.daidir $(SIM_DIR)/*.vdb $(SIM_DIR)/ppe_stm $(SIM_DIR)/hlp_parser_apr $(SIM_DIR)/hlp_fgrp_apr $(SIM_DIR)/hlp_fcmn_apr $(SIM_DIR)/hlp_fwd_apr $(SIM_DIR)/hlp_tail_apr $(SIM_DIR)/*.log $(SIM_DIR)/*.vpd $(SIM_DIR)/DVEfiles $(SIM_DIR)/vc_hdrs.h $(SIM_DIR)/ucli.key

