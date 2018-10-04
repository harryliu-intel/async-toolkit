BLOCK         = classifier_top
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
	$(V2BBOX) -v ./classifier_shells_wrapper_inc.v -n classifier_shells_wrapper -p ../../../../../target/mby/mgm_run/classifier/src,../../../../../target/mby/mgm_run/rtl -o classifier_shells_wrapper.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/classifier/src/mem_wrap/classifier_ff_mems.v -p ../../../../../target/mby/mgm_run/classifier/src,../../../../../target/mby/mgm_run/rtl -o classifier_ff_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/classifier/src/mem_wrap/classifier_rf_mems.v -p ../../../../../target/mby/mgm_run/classifier/src,../../../../../target/mby/mgm_run/rtl -o classifier_rf_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/classifier/src/mem_wrap/classifier_sram_mems.v -p ../../../../../target/mby/mgm_run/classifier/src,../../../../../target/mby/mgm_run/rtl -o classifier_sram_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/classifier/src/mem_wrap/classifier_tcam_mems.v -p ../../../../../target/mby/mgm_run/classifier/src,../../../../../target/mby/mgm_run/rtl -o classifier_tcam_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	rm -f classifier_shells_wrapper_inc.v
	rm -f classifier_shells_wrapper.v
	rm -f classifier_ff_mems.v
	rm -f classifier_rf_mems.v
	rm -f classifier_sram_mems.v
	rm -f classifier_tcam_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen classifier_shells_wrapper_inc.v classifier_shells_wrapper.v classifier_ff_mems.v classifier_rf_mems.v classifier_sram_mems.v classifier_tcam_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
