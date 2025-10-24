BLOCK         = igr_pre_post_wrap
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR -B
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../../../pre_ppe/rtl \
                -d=../../../post_ppe/rtl \
                -d=../../../rtl \
                -d=../../../interfaces \
                -d=../../../../shared/rtl \
                -d=../../../../shared/interfaces \
                -d=../../../../../../../target/mby/mgm_run/igr/src \
                -d=../../../../../../../target/mby/mgm_run/igr/src/mem_wrap \
                -d=../../../../../../../target/mby/mgm_run/rtl \
                -d=../verilog
V_LIBS        =

.PHONY: ngen top rtl copy clean diff

ngen: clean top

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
