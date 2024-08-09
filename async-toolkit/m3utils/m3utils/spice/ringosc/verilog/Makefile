all: incdec_simv oscdecode_simv osc_statemachine_simv

oscdecode_simv: oscdecode_tb.sv oscdecode.sv
	vcs -assert svaext -sverilog $< -debug_acc+all -debug_region+cell+encrypt -timescale=1fs/1fs -o $@

incdec_simv: incdec_tb.sv incdec.sv
	vcs -assert svaext -sverilog $< -debug_acc+all -debug_region+cell+encrypt -timescale=1fs/1fs -o $@

osc_statemachine_simv: osc_statemachine_tb.sv osc_statemachine.sv incdec.sv oscdecode.sv
	vcs -assert svaext -sverilog $< -debug_acc+all -debug_region+cell+encrypt -timescale=1fs/1fs -o $@

clean:
	rm -rf NovasLog/ csrc/ novas.conf novas.rc simv.daidir/ ucli.key simv .Makefile.swp *.daidir oscdecode_simv incdec_simv osc_statemachine_simv vc_hdrs.h
