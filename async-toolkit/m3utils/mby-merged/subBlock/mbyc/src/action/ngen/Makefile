BLOCK         = action_top
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../rtl
V_LIBS        =

.PHONY: ngen pre top post rtl copy clean diff

ngen: clean pre top post

pre:
	./pre_process.pl
	$(V2BBOX) -v ./action_shells_wrapper_inc.v -n action_shells_wrapper -p ../../../../../target/mby/mgm_run/action/src,../../../../../target/mby/mgm_run/rtl -o action_shells_wrapper.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/action/src/mem_wrap/action_ff_mems.v -p ../../../../../target/mby/mgm_run/action/src,../../../../../target/mby/mgm_run/rtl -o action_ff_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/action/src/mem_wrap/action_rf_mems.v -p ../../../../../target/mby/mgm_run/action/src,../../../../../target/mby/mgm_run/rtl -o action_rf_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/action/src/mem_wrap/action_sram_mems.v -p ../../../../../target/mby/mgm_run/action/src,../../../../../target/mby/mgm_run/rtl -o action_sram_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/action/src/mem_wrap/action_tcam_mems.v -p ../../../../../target/mby/mgm_run/action/src,../../../../../target/mby/mgm_run/rtl -o action_tcam_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	rm -f action_shells_wrapper_inc.v
	rm -f action_shells_wrapper.v
	rm -f action_ff_mems.v
	rm -f action_rf_mems.v
	rm -f action_sram_mems.v
	rm -f action_tcam_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen action_shells_wrapper_inc.v action_shells_wrapper.v action_ff_mems.v action_rf_mems.v action_sram_mems.v action_tcam_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
