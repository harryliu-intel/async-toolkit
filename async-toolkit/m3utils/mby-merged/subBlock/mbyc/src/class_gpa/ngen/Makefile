BLOCK         = class_gpa_top
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
	$(V2BBOX) -v ./class_gpa_shells_wrapper_inc.v -n class_gpa_shells_wrapper -p ../../../../../target/mby/mgm_run/class_gpa/src,../../../../../target/mby/mgm_run/rtl -o class_gpa_shells_wrapper.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/class_gpa/src/mem_wrap/class_gpa_ff_mems.v -p ../../../../../target/mby/mgm_run/class_gpa/src,../../../../../target/mby/mgm_run/rtl -o class_gpa_ff_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/class_gpa/src/mem_wrap/class_gpa_rf_mems.v -p ../../../../../target/mby/mgm_run/class_gpa/src,../../../../../target/mby/mgm_run/rtl -o class_gpa_rf_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/class_gpa/src/mem_wrap/class_gpa_sram_mems.v -p ../../../../../target/mby/mgm_run/class_gpa/src,../../../../../target/mby/mgm_run/rtl -o class_gpa_sram_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/class_gpa/src/mem_wrap/class_gpa_tcam_mems.v -p ../../../../../target/mby/mgm_run/class_gpa/src,../../../../../target/mby/mgm_run/rtl -o class_gpa_tcam_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	rm -f class_gpa_shells_wrapper_inc.v
	rm -f class_gpa_shells_wrapper.v
	rm -f class_gpa_ff_mems.v
	rm -f class_gpa_rf_mems.v
	rm -f class_gpa_sram_mems.v
	rm -f class_gpa_tcam_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen class_gpa_shells_wrapper_inc.v class_gpa_shells_wrapper.v class_gpa_ff_mems.v class_gpa_rf_mems.v class_gpa_sram_mems.v class_gpa_tcam_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
