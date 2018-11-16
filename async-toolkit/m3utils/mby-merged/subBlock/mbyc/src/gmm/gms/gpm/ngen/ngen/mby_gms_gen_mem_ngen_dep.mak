mby_gms_gen_mem_NGEN_OUT_FILES = ./ngen/mby_gms_gen_mem_ngen_files
mby_gms_gen_mem_NGEN_DEP_FILES = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/preprocess.pl mby_gms_gen_mem.hier mby_gms_gen_mem.hier mby_gms_gen_mem.sig gms_shells_wrapper.v gms_shells_wrapper.map gms_sram_mems.v gms_sram_mems.map gms_ff_mems.v gms_ff_mems.map
$(mby_gms_gen_mem_NGEN_OUT_FILES): $(mby_gms_gen_mem_NGEN_DEP_FILES)
    /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i//ngen_i.pl mby_gms_gen_mem
