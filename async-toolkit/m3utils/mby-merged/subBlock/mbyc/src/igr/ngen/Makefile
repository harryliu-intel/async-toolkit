BLOCK         = mby_igr_gen_mem
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../rtl
V_LIBS        =

.PHONY: ngen pre top rtl copy post clean diff

ngen: clean pre top post

pre:
	./pre_process.pl
	$(V2BBOX) -v ./igr_shells_wrapper_inc.v -n igr_shells_wrapper -p ../../../../../target/mby/mgm_run/igr/src,../../../../../target/mby/mgm_run/rtl -o igr_shells_wrapper.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/igr/src/mem_wrap/igr_sram_mems.v -p ../../../../../target/mby/mgm_run/igr/src,../../../../../target/mby/mgm_run/rtl -o igr_sram_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	./post_process.pl
	rm -f igr_shells_wrapper_inc.v
	rm -f igr_shells_wrapper.v
	rm -f igr_sram_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen igr_shells_wrapper_inc.v igr_shells_wrapper.v igr_sram_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
