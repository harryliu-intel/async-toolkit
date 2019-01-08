mby_igr_gen_mem_NGEN_OUT_FILES = ./ngen/mby_igr_gen_mem_ngen_files
mby_igr_gen_mem_NGEN_DEP_FILES = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/preprocess.pl mby_igr_gen_mem.hier mby_igr_gen_mem.hier mby_igr_gen_mem.sig igr_sram_mems.v igr_sram_mems.map igr_shells_wrapper.v igr_shells_wrapper.map
$(mby_igr_gen_mem_NGEN_OUT_FILES): $(mby_igr_gen_mem_NGEN_DEP_FILES)
    /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i//ngen_i.pl mby_igr_gen_mem
