all: simv

simv: lambmodel.sv lambmodel_tb.sv latchblk.sv
	/p/cth/bin/cth_psetup -p tfc -cfg tfc_n3.cth -tool vcsmx -cmd "vcs -sverilog -debug_access+r -debug_all lambmodel_tb.sv"

clean:
	/bin/rm -rf simv simv.daidir csrc *~ a.out ucli.key


