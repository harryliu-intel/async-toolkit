all: lambmodel_simv nlatch_simv nlatch_pair_simv

LAMBMODEL_TB_DEPS= latch.sv latchblk.sv lambmodel.sv lambmodel_tb.sv
NLATCH_TB_DEPS= latch.sv nlatch.sv nlatch_tb.sv
NLATCH_PAIR_TB_DEPS= latch.sv nlatch_pair.sv nlatch_pair_tb.sv

lambmodel_simv: $(LAMBMODEL_TB_DEPS)
	/p/cth/bin/cth_psetup -p tfc -cfg tfc_n3.cth -tool vcsmx -cmd "vcs -sverilog -debug_access+r -debug_acc+all -debug_region+cell+encrypt -assert svaext lambmodel_tb.sv -o $@"

NLATCH_TB_DEPS= latch.sv nlatch.sv nlatch_tb.sv

nlatch_simv: $(NLATCH_TB_DEPS)
	/p/cth/bin/cth_psetup -p tfc -cfg tfc_n3.cth -tool vcsmx -cmd "vcs -sverilog -debug_access+r -debug_acc+all -debug_region+cell+encrypt -assert svaext nlatch_tb.sv -o $@"

nlatch_pair_simv: $(NLATCH_PAIR_TB_DEPS)
	/p/cth/bin/cth_psetup -p tfc -cfg tfc_n3.cth -tool vcsmx -cmd "vcs -sverilog -debug_access+r -debug_acc+all -debug_region+cell+encrypt -assert svaext nlatch_pair_tb.sv -o $@"

clean:
	/bin/rm -rf simv *_simv *.daidir csrc *~ a.out ucli.key *.vpd vc_hdrs.h DVEfiles .restartSimSession* .synopsys_dve_rebuild.tcl


