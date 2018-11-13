BLOCK         = mby_rngstp
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../../igr/rtl \
                -d=../../egr/rtl \
                -d=../../shared/rtl \
                -d=../../shared/interfaces \
                -d=../rtl
V_LIBS        =

.PHONY: ngen pre top post rtl copy clean diff

ngen: clean top

pre:
	./pre_process.pl
	$(V2BBOX) -v ./mgp_shells_wrapper_inc.v -n mgp_shells_wrapper -p ../../../target/mby/mgm_run/mgp/src,../../../target/mby/mgm_run/rtl -o mgp_shells_wrapper.sv
	$(V2BBOX) -v ../../../target/mby/mgm_run/mgp/src/mem_wrap/mgp_ff_mems.v -p ../../../target/mby/mgm_run/mgp/src,../../../target/mby/mgm_run/rtl -o mgp_ff_mems.sv

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	./post_process.pl
	mv ./ngen/mby_mgp_fixed.sv ./ngen/mby_mgp.sv

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv

bbox:
	$(V2BBOX) -v ./ngen/$(BLOCK).sv -n $(BLOCK) -t -o ../bbox/$(BLOCK).sv
