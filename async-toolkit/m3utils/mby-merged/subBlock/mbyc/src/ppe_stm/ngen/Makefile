BLOCK         = ppe_stm_top
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
	$(V2BBOX) -v ./ppe_stm_shells_wrapper_inc.v -n ppe_stm_shells_wrapper -p ../../../target/mby/mgm_run/ppe_stm/src,../../../target/mby/mgm_run/rtl -o ppe_stm_shells_wrapper.v
	$(V2BBOX) -v ../../../target/mby/mgm_run/ppe_stm/src/mem_wrap/ppe_stm_sram_mems.v -p ../../../target/mby/mgm_run/ppe_stm/src,../../../target/mby/mgm_run/rtl -o ppe_stm_sram_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	rm -f ppe_stm_shells_wrapper_inc.v
	rm -f ppe_stm_shells_wrapper.v
	rm -f ppe_stm_sram_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen ppe_stm_shells_wrapper_inc.v ppe_stm_shells_wrapper.v ppe_stm_sram_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
