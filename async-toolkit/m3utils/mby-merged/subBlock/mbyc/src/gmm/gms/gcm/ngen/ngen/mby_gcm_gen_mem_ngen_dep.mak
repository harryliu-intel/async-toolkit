mby_gcm_gen_mem_NGEN_OUT_FILES = ./ngen/mby_gcm_gen_mem_ngen_files
mby_gcm_gen_mem_NGEN_DEP_FILES = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/preprocess.pl mby_gcm_gen_mem.hier mby_gcm_gen_mem.hier mby_gcm_gen_mem.sig gcm_ff_mems.v gcm_ff_mems.map gcm_shells_wrapper.v gcm_shells_wrapper.map
$(mby_gcm_gen_mem_NGEN_OUT_FILES): $(mby_gcm_gen_mem_NGEN_DEP_FILES)
    /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i//ngen_i.pl mby_gcm_gen_mem
