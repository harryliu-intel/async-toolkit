all: osc_statemachine_simv

osc_statemachine_simv: osc_statemachine.pg_tb.sv osc_statemachine.pg.v
	vcs -assert svaext -sverilog $< -debug_acc+all -debug_region+cell+encrypt -timescale=1fs/1fs -o $@

clean:
	rm -rf NovasLog/ csrc/ novas.conf novas.rc simv.daidir/ ucli.key simv .Makefile.swp *.daidir oscdecode_simv incdec_simv osc_statemachine_simv vc_hdrs.h
