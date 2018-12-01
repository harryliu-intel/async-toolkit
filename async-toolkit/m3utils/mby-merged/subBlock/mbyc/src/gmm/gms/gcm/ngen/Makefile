BLOCK         = mby_gcm_gen_mem
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../../gcm/rtl
V_LIBS        =

.PHONY: ngen pre top rtl copy post clean diff

ngen: clean pre top post

pre:
	./pre_process.pl
	$(V2BBOX) -v ./gcm_shells_wrapper_inc.v -n gcm_shells_wrapper -p ../../../../../../../target/mby/mgm_run/gcm/src,../../../../../../../target/mby/mgm_run/rtl -o gcm_shells_wrapper.v
	$(V2BBOX) -v ../../../../../../../target/mby/mgm_run/gcm/src/mem_wrap/gcm_rf_mems.v -p ../../../../../../../target/mby/mgm_run/gcm/src,../../../../../../../target/mby/mgm_run/rtl -o gcm_rf_mems.v
	$(V2BBOX) -v ../../../../../../../target/mby/mgm_run/gcm/src/mem_wrap/gcm_ff_mems.v -p ../../../../../../../target/mby/mgm_run/gcm/src,../../../../../../../target/mby/mgm_run/rtl -o gcm_ff_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	./post_process.pl
	rm -f gcm_shells_wrapper_inc.v
	rm -f gcm_shells_wrapper.v
	rm -f gcm_rf_mems.v
	rm -f gcm_ff_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen gcm_shells_wrapper_inc.v gcm_shells_wrapper.v gcm_rf_mems.v gcm_ff_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
