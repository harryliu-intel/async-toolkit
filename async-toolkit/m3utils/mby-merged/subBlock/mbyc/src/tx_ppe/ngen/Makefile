BLOCK         = tx_ppe_top
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../rtl \
                -d=../../shared/interfaces \
                -d=../../shared/rtl
V_LIBS        =

.PHONY: ngen pre top post rtl copy clean diff

ngen: clean pre top post

pre:
	./pre_process.pl
	$(V2BBOX) -v ./tx_ppe_shells_wrapper_inc.v -n tx_ppe_shells_wrapper -p ../../../../../target/mby/mgm_run/tx_ppe/src,../../../../../target/mby/mgm_run/rtl -o tx_ppe_shells_wrapper.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/tx_ppe/src/mem_wrap/tx_ppe_rf_mems.v -p ../../../../../target/mby/mgm_run/tx_ppe/src,../../../../../target/mby/mgm_run/rtl -o tx_ppe_rf_mems.v
	$(V2BBOX) -v ../../../../../target/mby/mgm_run/tx_ppe/src/mem_wrap/tx_ppe_sram_mems.v -p ../../../../../target/mby/mgm_run/tx_ppe/src,../../../../../target/mby/mgm_run/rtl -o tx_ppe_sram_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	rm -f tx_ppe_shells_wrapper_inc.v
	rm -f tx_ppe_shells_wrapper.v
	rm -f tx_ppe_rf_mems.v
	rm -f tx_ppe_sram_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen tx_ppe_shells_wrapper_inc.v tx_ppe_shells_wrapper.v tx_ppe_rf_mems.v tx_ppe_sram_mems.v)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
