BLOCK         = mby_msh_gen_mem
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
	$(V2BBOX) -v ./msh_shells_wrapper_inc.v -n msh_shells_wrapper -p ../../../../../target/mby/mgm_run/msh/src,../../../../../target/mby/mgm_run/rtl -o msh_shells_wrapper.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/msh/src/mem_wrap/msh_sram_mems.v -p ../../../../../target/mby/mgm_run/msh/src,../../../../../target/mby/mgm_run/rtl -o msh_sram_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	./post_process.pl
	rm -f msh_shells_wrapper_inc.v
	rm -f msh_shells_wrapper.v
	rm -f msh_sram_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen msh_shells_wrapper_inc.v msh_shells_wrapper.v msh_sram_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
