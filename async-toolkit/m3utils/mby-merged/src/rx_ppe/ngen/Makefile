BLOCK         = rx_ppe
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../../parser/rtl \
                -d=../../shared/rtl \
                -d=../../shared/interfaces \
                -d=../rtl
V_LIBS        =

.PHONY: ngen pre top post rtl copy clean diff

ngen: clean top

pre:
	./pre_process.pl
	$(V2BBOX) -v ./parser_shells_wrapper_inc.v -n parser_shells_wrapper -p ../../../target/mby/mgm_run/parser/src,../../../target/mby/mgm_run/rtl -o parser_shells_wrapper.v
	$(V2BBOX) -v ../../../target/mby/mgm_run/parser/src/mem_wrap/parser_ff_mems.v -p ../../../target/mby/mgm_run/parser/src,../../../target/mby/mgm_run/rtl -o parser_ff_mems.v

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	rm -f parser_shells_wrapper_inc.v
	rm -f parser_shells_wrapper.v
	rm -f parser_ff_mems.v

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
