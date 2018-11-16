mby_gpm_gen_mem_NGEN_OUT_FILES = ./ngen/mby_gpm_gen_mem_ngen_files
mby_gpm_gen_mem_NGEN_DEP_FILES = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/preprocess.pl mby_gpm_gen_mem.hier mby_gpm_gen_mem.hier mby_gpm_gen_mem.sig gpm_shells_wrapper.v gpm_shells_wrapper.map gpm_sram_mems.v gpm_sram_mems.map
$(mby_gpm_gen_mem_NGEN_OUT_FILES): $(mby_gpm_gen_mem_NGEN_DEP_FILES)
    /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i//ngen_i.pl mby_gpm_gen_mem
