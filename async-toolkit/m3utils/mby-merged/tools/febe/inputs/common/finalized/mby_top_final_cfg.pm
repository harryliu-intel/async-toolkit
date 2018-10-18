package mby_top_final_cfg;
use ToolConfig;
use vars qw (@ISA @EXPORT_OK $dc_cfg $MODEL_ROOT);
@EXPORT_OK = qw($dc_cfg);

$dc_cfg = {
    -owner => ['Buck'],
    -block_type => 'unit',
    -enable_sg_dft => 0,
    -enable_gkturnin => 0,
    -search => [
    ],
    -rm_search => [
     ],
    -files => [
     ],
    -files_first => [
                     ],
    -rm_files => [
                 ],
    -add_ctech_files => [],
    -rm_ctech_files => [],
    -pre_script => [],
    -cp_tcl_files => [],
    -defines => [
#                    'INTC_MEM_SVA_OFF',
#                    'INTEL_SVA_OFF',
#                    'PKE2_NOCOREKIT',
                ],
    -rm_defines => [],
    -sig_files => [],
    -add_ctech_files => [],
    -post_block_setup => [
        "setvar G_MAX_THREADS 4",
    ],
    -nb_resource => {
                 },
    -children => [
                 ],
},
1;

