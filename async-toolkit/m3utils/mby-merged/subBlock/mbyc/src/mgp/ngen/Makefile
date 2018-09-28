BLOCK         = mby_mgp
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../../igr/rtl \
                -d=../../rtl/egr/verilog \
                -d=../../tx_ppe/rtl \
                -d=../../rx_ppe/rtl \
                -d=../../shared/rtl \
                -d=../../shared/interfaces
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
	rm -f mgp_shells_wrapper_inc.v
	rm -f mgp_shells_wrapper.v
	rm -f mgp_ff_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
