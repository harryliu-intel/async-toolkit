PAR         = mce
BLOCK       = mby_$(PAR)_gen_mem
DIFF       := $(if $(DISPLAY),tkdiff,diff -w)
NGEN        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS  = -fi=INTCNOPWR
V2BBOX      = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS      = -d=../rtl
V_LIBS      =
MGM_RUN     = $(MODEL_ROOT)/target/mby/mgm_run

.PHONY: ngen pre top rtl copy clean diff

ngen: clean pre top

pre:
	./pre_process.pl
	$(V2BBOX) -v $(PAR)_shells_wrapper_inc.v -n $(PAR)_shells_wrapper -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_shells_wrapper.v
	$(V2BBOX) -v $(MGM_RUN)/$(PAR)/src/mem_wrap/$(PAR)_rf_mems.v -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_rf_mems.v
	$(V2BBOX) -v $(MGM_RUN)/$(PAR)/src/mem_wrap/$(PAR)_sram_mems.v -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_sram_mems.v

top: pre
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)
	rm -f *.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen *.v *.sv *.tmp)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
