BLOCK         = mby_mpp
DIFF         := $(if $(DISPLAY),tkdiff,diff -w)
NGEN          = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl
NGEN_FLAGS    = -fi=INTCNOPWR -B
V2BBOX        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl

V_DIRS        = -d=../../mgp/rtl \
                -d=../../ppe_stm/rtl \
                -d=../../shared/rtl \
                -d=../../egr/rtl \
                -d=../../igr/rtl \
                -t=../../../../../target/mby/mgm_run/ \
                -d=../../shared/interfaces
V_LIBS        =

.PHONY: ngen pre top post rtl copy clean diff

ngen: clean top 

pre:
	./pre_process.pl
	$(V2BBOX) -v ./mpp_shells_wrapper_inc.v -n mgp_shells_wrapper -p ../../../target/mby/mgm_run/mgp/src,../../../target/mby/mgm_run/rtl -o mgp_shells_wrapper.sv
	$(V2BBOX) -v ../../../target/mby/mgm_run/mpp/src/mem_wrap/mgp_ff_mems.v -p ../../../target/mby/mgm_run/mgp/src,../../../target/mby/mgm_run/rtl -o mgp_ff_mems.sv

top:
	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)

post:
	#./post_process.pl	
	#mv ngen/mby_mpp_fixed.sv ngen/mby_mpp.sv

rtl copy:
	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv

clean:
	@echo "Cleaning $(BLOCK)"
	@rm -rf $(wildcard ngen)

diff:
	@echo "Making a difference..." # one byte at a time
	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv
