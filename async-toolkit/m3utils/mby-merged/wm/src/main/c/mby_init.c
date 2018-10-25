#include "mby_init.h"

#include <mby_bitfield.h>
#include <mby_classifier.h>
#include <mby_common.h>
#include <mby_congmgmt.h>
#include <mby_crc32.h>
#include <mby_hash.h>
#include <mby_mapper.h>
#include <mby_maskgen.h>
#include <mby_model.h>
#include <mby_modifier.h>
#include <mby_nexthop.h>
#include <mby_parser.h>
#include <mby_pipeline.h>
#include <mby_reg_ctrl.h>
#include <mby_rxstats.h>
#include <mby_triggers.h>
#include <mby_txstats.h>

static void set_parser_port_cfg_reg
(
    uint8                     initial_w0_offset,
    uint8                     initial_w1_offset,
    uint8                     initial_w2_offset,
    uint8                     initial_ptr,
    uint16                    initial_state,
    uint12                    initial_op_mask,
    uint4                     initial_op_rot,
    parser_port_cfg_r * const port_cfg
)
{
    port_cfg->INITIAL_W0_OFFSET = initial_w0_offset;
    port_cfg->INITIAL_W1_OFFSET = initial_w1_offset;
    port_cfg->INITIAL_W2_OFFSET = initial_w2_offset;
    port_cfg->INITIAL_PTR       = initial_ptr;
    port_cfg->INITIAL_STATE     = initial_state;
    port_cfg->INITIAL_OP_MASK   = initial_op_mask;
    port_cfg->INITIAL_OP_ROT    = initial_op_rot;
}

static void set_parser_csum_cfg_reg
(
    uint2                     validate_l4_csum,
    uint1                     store_l4_partial_csum,
    uint1                     compute_l4_csum,
    uint2                     validate_l3_length,
    parser_csum_cfg_r * const csum_cfg
)
{
    csum_cfg->VALIDATE_L4_CSUM      = validate_l4_csum;
    csum_cfg->STORE_L4_PARTIAL_CSUM = store_l4_partial_csum;
    csum_cfg->COMPUTE_L4_CSUM       = compute_l4_csum;
    csum_cfg->VALIDATE_L3_LENGTH    = validate_l3_length;
}

static void set_parser_key_s_reg
(
    fm_uint16                 state_value,
    fm_uint16                 state_mask,
    parser_key_s_r * const key_s
)
{
    key_s->STATE_VALUE = state_value;
    key_s->STATE_MASK  = state_mask;
}

static void set_parser_key_w_reg
(
    uint16                 w1_value,
    uint16                 w1_mask,
    uint16                 w0_value,
    uint16                 w0_mask,
    parser_key_w_r * const key_w
)
{
    key_w->W1_VALUE = w1_value;
    key_w->W1_MASK  = w1_mask;
    key_w->W0_VALUE = w0_value;
    key_w->W0_MASK  = w0_mask;
}

static void set_parser_ana_s_reg
(
    uint16                 next_state,
    uint16                 next_state_mask,
    uint16                 next_op,
    parser_ana_s_r * const ana_s
)
{
    ana_s->NEXT_STATE      = next_state;
    ana_s->NEXT_STATE_MASK = next_state_mask;
    ana_s->NEXT_OP         = next_op;
}

static void set_parser_ana_w_reg
(
    uint8                  next_w0_offset,
    uint8                  next_w1_offset,
    uint8                  next_w2_offset,
    uint8                  skip,
    parser_ana_w_r * const ana_w
)
{
    ana_w->NEXT_W0_OFFSET = next_w0_offset;
    ana_w->NEXT_W1_OFFSET = next_w1_offset;
    ana_w->NEXT_W2_OFFSET = next_w2_offset;
    ana_w->SKIP           = skip;
}

static void set_parser_exc_reg
(
    uint8                ex_offset,
    uint1                parsing_done,
    parser_exc_r * const exc
)
{
    exc->EX_OFFSET    = ex_offset;
    exc->PARSING_DONE = parsing_done;
}

static void set_parser_ptype_tcam_ram
(
    uint32                      key_invert,
    uint32                      key,
    uint4                       extract_idx,
    uint10                      ptype,
    parser_ptype_tcam_r * const tcam,
    parser_ptype_ram_r  * const ram
)
{
    tcam->KEY_INVERT = key_invert;
    tcam->KEY        = key;
    ram->EXTRACT_IDX = extract_idx;
    ram->PTYPE       = ptype;
}

static void set_parser_ext_reg
(
    uint8                protocol_id,
    uint8                offset,
    uint6                flag_num,
    uint1                flag_value,
    uint3                ptr_num,
    parser_ext_r * const ext
)
{
    ext->PROTOCOL_ID = protocol_id;
    ext->OFFSET      = offset;
    ext->FLAG_NUM    = flag_num;
    ext->FLAG_VALUE  = flag_value;
    ext->PTR_NUM     = ptr_num;
}


static void set_parser_extract_cfg
(
    uint8                        protocol_id,
    uint8                        offset,
    parser_extract_cfg_r * const extract_cfg
)
{
    extract_cfg->PROTOCOL_ID = protocol_id;
    extract_cfg->OFFSET      = offset;
}

static void reset_parser_regs(mby_ppe_parser_map * const parser_map)
{
    for (fm_uint i = 0; i < MBY_PA_ANA_STAGES; i++) {
        for (fm_uint j = 0; j < MBY_PA_ANA_RULES; j++) {
            parser_map->PARSER_KEY_W[i][j].W0_VALUE                     = 0xFFFF;
            parser_map->PARSER_KEY_W[i][j].W1_VALUE                     = 0xFFFF;
            parser_map->PARSER_KEY_S[i][j].STATE_VALUE                  = 0xFFFF;
            parser_map->PARSER_EXT[i][j].PROTOCOL_ID                    = MBY_PA_PROT_ID_NOP;
            parser_map->PARSER_EXT[i][j + MBY_PA_ANA_RULES].PROTOCOL_ID = MBY_PA_PROT_ID_NOP;
        }
    }

    for (fm_uint i = 0; i < mby_ppe_parser_map_PARSER_EXTRACT_CFG__nd; i++) {
        for (fm_uint j = 0; j < parser_extract_cfg_rf_PARSER_EXTRACT_CFG__nd; j++) {
            parser_map->PARSER_EXTRACT_CFG[i][j].PROTOCOL_ID = MBY_PA_PROT_ID_NOP;
            parser_map->PARSER_EXTRACT_CFG[i][j].OFFSET      = 0;
        }
    }

    for (fm_uint i = 0; i < mby_ppe_parser_map_PARSER_PTYPE_TCAM__nd; i++) {
        for (fm_uint j = 0; j < parser_ptype_tcam_rf_PARSER_PTYPE_TCAM__nd; j++) {
            parser_map->PARSER_PTYPE_TCAM[i][j].KEY_INVERT = 0xFFFFFFFF;
            parser_map->PARSER_PTYPE_TCAM[i][j].KEY        = 0xFFFFFFFF;
        }
    }
}

static void init_parser_regs
(
    mby_ppe_parser_map * const parser_map
)
{
    reset_parser_regs(parser_map);

    for (fm_int i = 0; i < 16/* Nmber of ports? */; i++) {
        set_parser_port_cfg_reg(0xc, 0xe, 0x0, 0x0, 0x7800, 0x0, 0x0, &(parser_map->PARSER_PORT_CFG[i]));
        set_parser_csum_cfg_reg(0x0, 0x0, 0x0, 0x0, &(parser_map->PARSER_CSUM_CFG[i]));
    }

    set_parser_extract_cfg(0, 0, &parser_map->PARSER_EXTRACT_CFG[0][6]);
    set_parser_extract_cfg(0, 2, &parser_map->PARSER_EXTRACT_CFG[0][7]);
    set_parser_extract_cfg(0, 4, &parser_map->PARSER_EXTRACT_CFG[0][8]);
    set_parser_extract_cfg(0, 6, &parser_map->PARSER_EXTRACT_CFG[0][9]);
    set_parser_extract_cfg(0, 8, &parser_map->PARSER_EXTRACT_CFG[0][10]);
    set_parser_extract_cfg(0, 10, &parser_map->PARSER_EXTRACT_CFG[0][11]);
    set_parser_extract_cfg(0, 12, &parser_map->PARSER_EXTRACT_CFG[0][12]);

    set_parser_extract_cfg(1, 0, &parser_map->PARSER_EXTRACT_CFG[1][6]);
    set_parser_extract_cfg(1, 2, &parser_map->PARSER_EXTRACT_CFG[1][7]);
    set_parser_extract_cfg(1, 4, &parser_map->PARSER_EXTRACT_CFG[1][8]);
    set_parser_extract_cfg(1, 6, &parser_map->PARSER_EXTRACT_CFG[1][9]);
    set_parser_extract_cfg(1, 8, &parser_map->PARSER_EXTRACT_CFG[1][10]);
    set_parser_extract_cfg(1, 10, &parser_map->PARSER_EXTRACT_CFG[1][11]);
    set_parser_extract_cfg(1, 16, &parser_map->PARSER_EXTRACT_CFG[1][12]);
    set_parser_extract_cfg(1, 14, &parser_map->PARSER_EXTRACT_CFG[1][14]);
    set_parser_extract_cfg(1, 14, &parser_map->PARSER_EXTRACT_CFG[1][31]);

    set_parser_ptype_tcam_ram(0xFFFFFFFF, 0x0, 0, 0, &parser_map->PARSER_PTYPE_TCAM[0][0],
                              &parser_map->PARSER_PTYPE_RAM[0][0]);
    set_parser_ptype_tcam_ram(0xFFFBFFFF, 0x40000, 1, 1, &parser_map->PARSER_PTYPE_TCAM[0][1],
                              &parser_map->PARSER_PTYPE_RAM[0][1]);

    set_parser_key_s_reg(0x0, 0x3f, &parser_map->PARSER_KEY_S[0][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[0][0]);
    set_parser_ana_s_reg(0x6, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][0]);
    set_parser_ana_w_reg(0xc, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][0]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[0][0]);
    set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][0]);

    set_parser_key_s_reg(0x2000, 0x203f, &parser_map->PARSER_KEY_S[0][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[0][2]);
    set_parser_ana_s_reg(0x81, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][2]);
    set_parser_ana_w_reg(0x10, 0x12, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][2]);
    set_parser_exc_reg(0x10, 0, &parser_map->PARSER_EXC[0][2]);
    set_parser_ext_reg(0x1, 0x0, 0x12, 1, 0x0, &parser_map->PARSER_EXT[0][2]);

    // Keep old parser config for now
#if 0
    set_parser_key_s_reg(0x4000, 0x403f, &parser_map->PARSER_KEY_S[0][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[0][1]);
    set_parser_ana_s_reg(0x81, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][1]);
    set_parser_ana_w_reg(0x10, 0x12, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][1]);
    set_parser_exc_reg(0x10, 0, &parser_map->PARSER_EXC[0][1]);
//  set_parser_ext_reg(0x81, 0xe, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][17]);
//  set_parser_ext_reg(0x6, 0x0, 0x13, 1, 0x0, &parser_map->PARSER_EXT[0][1]);
    set_parser_key_s_reg(0x1000, 0x103f, &parser_map->PARSER_KEY_S[0][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[0][3]);
    set_parser_ana_s_reg(0x41, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][3]);
    set_parser_ana_w_reg(0x10, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][3]);
    set_parser_exc_reg(0x10, 0, &parser_map->PARSER_EXC[0][3]);
//  set_parser_ext_reg(0x1, 0xe, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][19]);
//  set_parser_ext_reg(0x6, 0x0, 0x12, 1, 0x0, &parser_map->PARSER_EXT[0][3]);
    set_parser_key_s_reg(0x800, 0x83f, &parser_map->PARSER_KEY_S[0][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[0][4]);
    set_parser_ana_s_reg(0x41, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][4]);
    set_parser_ana_w_reg(0x10, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][4]);
    set_parser_exc_reg(0x10, 0, &parser_map->PARSER_EXC[0][4]);
//  set_parser_ext_reg(0x1, 0xe, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][20]);
//  set_parser_ext_reg(0x6, 0x0, 0x12, 1, 0x0, &parser_map->PARSER_EXT[0][4]);
    set_parser_key_s_reg(0x1000, 0x103f, &parser_map->PARSER_KEY_S[0][5]);
    set_parser_key_w_reg(0x0, 0xfff, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[0][5]);
    set_parser_ana_s_reg(0x41, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][5]);
    set_parser_ana_w_reg(0x10, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][5]);
    set_parser_exc_reg(0x10, 0, &parser_map->PARSER_EXC[0][5]);
//  set_parser_ext_reg(0x1, 0xe, 0x12, 1, 0x0, &parser_map->PARSER_EXT[0][21]);
//  set_parser_ext_reg(0x6, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[0][5]);
    set_parser_key_s_reg(0x800, 0x83f, &parser_map->PARSER_KEY_S[0][6]);
    set_parser_key_w_reg(0x0, 0xfff, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[0][6]);
    set_parser_ana_s_reg(0x41, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[0][6]);
    set_parser_ana_w_reg(0x10, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][6]);
    set_parser_exc_reg(0x10, 0, &parser_map->PARSER_EXC[0][6]);
//  set_parser_ext_reg(0x1, 0xe, 0x12, 1, 0x0, &parser_map->PARSER_EXT[0][22]);
//  set_parser_ext_reg(0x6, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[0][6]);
    set_parser_key_s_reg(0x0, 0x3f, &parser_map->PARSER_KEY_S[0][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x8847, 0xffff, &parser_map->PARSER_KEY_W[0][7]);
    set_parser_ana_s_reg(0xa0, 0xff, 0x0, &parser_map->PARSER_ANA_S[0][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][7]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[0][7]);
//  set_parser_ext_reg(0x6, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][7]);
    set_parser_key_s_reg(0x0, 0x3f, &parser_map->PARSER_KEY_S[0][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x8848, 0xffff, &parser_map->PARSER_KEY_W[0][8]);
    set_parser_ana_s_reg(0xa0, 0xff, 0x0, &parser_map->PARSER_ANA_S[0][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][8]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[0][8]);
//  set_parser_ext_reg(0x6, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][8]);
    set_parser_key_s_reg(0x0, 0x3f, &parser_map->PARSER_KEY_S[0][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x800, 0xffff, &parser_map->PARSER_KEY_W[0][9]);
    set_parser_ana_s_reg(0x20, 0xff, 0x0, &parser_map->PARSER_ANA_S[0][9]);
    set_parser_ana_w_reg(0x14, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][9]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[0][9]);
//  set_parser_ext_reg(0x6, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][9]);
    set_parser_key_s_reg(0x0, 0x3f, &parser_map->PARSER_KEY_S[0][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x86dd, 0xffff, &parser_map->PARSER_KEY_W[0][10]);
    set_parser_ana_s_reg(0x60, 0xff, 0x0, &parser_map->PARSER_ANA_S[0][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[0][10]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[0][10]);
//  set_parser_ext_reg(0x6, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[0][10]);
    set_parser_key_s_reg(0x81, 0x1ff, &parser_map->PARSER_KEY_S[1][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[1][0]);
    set_parser_ana_s_reg(0x1, 0x3f, 0x0, &parser_map->PARSER_ANA_S[1][0]);
    set_parser_ana_w_reg(0x4, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[1][0]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[1][0]);
    set_parser_key_s_reg(0x41, 0x1ff, &parser_map->PARSER_KEY_S[1][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[1][1]);
    set_parser_ana_s_reg(0x1, 0x3f, 0x0, &parser_map->PARSER_ANA_S[1][1]);
    set_parser_ana_w_reg(0x4, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[1][1]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[1][1]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[1][17]);
//  set_parser_ext_reg(0x81, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[1][1]);
    set_parser_key_s_reg(0x1081, 0x11ff, &parser_map->PARSER_KEY_S[1][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[1][2]);
    set_parser_ana_s_reg(0x101, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[1][2]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[1][2]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[1][2]);
//  set_parser_ext_reg(0x1, 0x6, 0x14, 1, 0x0, &parser_map->PARSER_EXT[1][18]);
//  set_parser_ext_reg(0x81, 0x6, 0x12, 1, 0x0, &parser_map->PARSER_EXT[1][2]);
    set_parser_key_s_reg(0x881, 0x9ff, &parser_map->PARSER_KEY_S[1][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[1][3]);
    set_parser_ana_s_reg(0x101, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[1][3]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[1][3]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[1][3]);
//  set_parser_ext_reg(0x1, 0x6, 0x14, 1, 0x0, &parser_map->PARSER_EXT[1][19]);
//  set_parser_ext_reg(0x81, 0x6, 0x12, 1, 0x0, &parser_map->PARSER_EXT[1][3]);
    set_parser_key_s_reg(0x1081, 0x11ff, &parser_map->PARSER_KEY_S[1][4]);
    set_parser_key_w_reg(0x0, 0xfff, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[1][4]);
    set_parser_ana_s_reg(0x141, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[1][4]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[1][4]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[1][4]);
//  set_parser_ext_reg(0x1, 0x6, 0x14, 1, 0x0, &parser_map->PARSER_EXT[1][20]);
//  set_parser_ext_reg(0x81, 0x6, 0x12, 1, 0x0, &parser_map->PARSER_EXT[1][4]);
    set_parser_key_s_reg(0x881, 0x9ff, &parser_map->PARSER_KEY_S[1][5]);
    set_parser_key_w_reg(0x0, 0xfff, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[1][5]);
    set_parser_ana_s_reg(0x141, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[1][5]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[1][5]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[1][5]);
//  set_parser_ext_reg(0x1, 0x6, 0x14, 1, 0x0, &parser_map->PARSER_EXT[1][21]);
//  set_parser_ext_reg(0x81, 0x6, 0x12, 1, 0x0, &parser_map->PARSER_EXT[1][5]);
    set_parser_key_s_reg(0x4041, 0x41ff, &parser_map->PARSER_KEY_S[1][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[1][6]);
    set_parser_ana_s_reg(0xc1, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[1][6]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[1][6]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[1][6]);
//  set_parser_ext_reg(0x81, 0x6, 0x0, 0, 0x0, &parser_map->PARSER_EXT[1][22]);
//  set_parser_ext_reg(0x81, 0x2, 0x13, 1, 0x0, &parser_map->PARSER_EXT[1][6]);
    set_parser_key_s_reg(0x2041, 0x21ff, &parser_map->PARSER_KEY_S[1][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[1][7]);
    set_parser_ana_s_reg(0xc1, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[1][7]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[1][7]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[1][7]);
//  set_parser_ext_reg(0x81, 0x6, 0x0, 0, 0x0, &parser_map->PARSER_EXT[1][23]);
//  set_parser_ext_reg(0x81, 0x2, 0x13, 1, 0x0, &parser_map->PARSER_EXT[1][7]);
    set_parser_key_s_reg(0x1, 0x3f, &parser_map->PARSER_KEY_S[2][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[2][0]);
    set_parser_ana_s_reg(0x6, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[2][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][0]);
    set_parser_key_s_reg(0x141, 0x1ff, &parser_map->PARSER_KEY_S[2][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[2][1]);
    set_parser_ana_s_reg(0x6, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[2][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][1]);
//  set_parser_ext_reg(0x0, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[2][1]);
    set_parser_key_s_reg(0x1, 0x3f, &parser_map->PARSER_KEY_S[2][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x88e7, 0xffff, &parser_map->PARSER_KEY_W[2][2]);
    set_parser_ana_s_reg(0xe0, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][2]);
    set_parser_key_s_reg(0x1, 0x3f, &parser_map->PARSER_KEY_S[2][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x8847, 0xffff, &parser_map->PARSER_KEY_W[2][3]);
    set_parser_ana_s_reg(0xa0, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][3]);
    set_parser_key_s_reg(0x1, 0x3f, &parser_map->PARSER_KEY_S[2][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x8848, 0xffff, &parser_map->PARSER_KEY_W[2][4]);
    set_parser_ana_s_reg(0xa0, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][4]);
    set_parser_key_s_reg(0x1, 0x3f, &parser_map->PARSER_KEY_S[2][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x800, 0xffff, &parser_map->PARSER_KEY_W[2][5]);
    set_parser_ana_s_reg(0x20, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][5]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][5]);
    set_parser_key_s_reg(0x1, 0x3f, &parser_map->PARSER_KEY_S[2][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x86dd, 0xffff, &parser_map->PARSER_KEY_W[2][6]);
    set_parser_ana_s_reg(0x60, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][6]);
    set_parser_key_s_reg(0x141, 0x1ff, &parser_map->PARSER_KEY_S[2][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x88e7, 0xffff, &parser_map->PARSER_KEY_W[2][7]);
    set_parser_ana_s_reg(0xe0, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][7]);
//  set_parser_ext_reg(0x0, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[2][7]);
    set_parser_key_s_reg(0x141, 0x1ff, &parser_map->PARSER_KEY_S[2][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x8847, 0xffff, &parser_map->PARSER_KEY_W[2][8]);
    set_parser_ana_s_reg(0xa0, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][8]);
//  set_parser_ext_reg(0x0, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[2][8]);
    set_parser_key_s_reg(0x141, 0x1ff, &parser_map->PARSER_KEY_S[2][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x8848, 0xffff, &parser_map->PARSER_KEY_W[2][9]);
    set_parser_ana_s_reg(0xa0, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][9]);
//  set_parser_ext_reg(0x0, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[2][9]);
    set_parser_key_s_reg(0x141, 0x1ff, &parser_map->PARSER_KEY_S[2][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x800, 0xffff, &parser_map->PARSER_KEY_W[2][10]);
    set_parser_ana_s_reg(0x20, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][10]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][10]);
//  set_parser_ext_reg(0x0, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[2][10]);
    set_parser_key_s_reg(0x141, 0x1ff, &parser_map->PARSER_KEY_S[2][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x86dd, 0xffff, &parser_map->PARSER_KEY_W[2][11]);
    set_parser_ana_s_reg(0x60, 0xff, 0x0, &parser_map->PARSER_ANA_S[2][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[2][11]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[2][11]);
//  set_parser_ext_reg(0x0, 0x0, 0x11, 1, 0x0, &parser_map->PARSER_EXT[2][11]);
    set_parser_key_s_reg(0x6, 0x1ff, &parser_map->PARSER_KEY_S[3][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[3][0]);
    set_parser_ana_s_reg(0xa2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[3][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x2, &parser_map->PARSER_ANA_W[3][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[3][0]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[3][0]);
    set_parser_key_s_reg(0x8006, 0x803f, &parser_map->PARSER_KEY_S[3][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[3][1]);
    set_parser_ana_s_reg(0x1d, 0xff, 0x0, &parser_map->PARSER_ANA_S[3][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x2, &parser_map->PARSER_ANA_W[3][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[3][1]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[3][1]);
    set_parser_key_s_reg(0x46, 0x1ff, &parser_map->PARSER_KEY_S[3][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[3][2]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[3][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[3][2]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[3][2]);
//  set_parser_ext_reg(0x0, 0x0, 0x20, 1, 0x0, &parser_map->PARSER_EXT[3][2]);
    set_parser_key_s_reg(0xe0, 0xff, &parser_map->PARSER_KEY_S[4][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[4][0]);
    set_parser_ana_s_reg(0x13, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[4][0]);
    set_parser_ana_w_reg(0x12, 0x0, 0x0, 0x6, &parser_map->PARSER_ANA_W[4][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[4][0]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[4][0]);
    set_parser_key_s_reg(0xa0, 0xff, &parser_map->PARSER_KEY_S[4][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[4][1]);
    set_parser_ana_s_reg(0x2, 0xff, 0x0, &parser_map->PARSER_ANA_S[4][1]);
    set_parser_ana_w_reg(0x4, 0x8, 0x0, 0x2, &parser_map->PARSER_ANA_W[4][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[4][1]);
//  set_parser_ext_reg(0x1, 0x0, 0x15, 1, 0x0, &parser_map->PARSER_EXT[4][1]);
    set_parser_key_s_reg(0x20, 0xff, &parser_map->PARSER_KEY_S[4][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[4][2]);
    set_parser_ana_s_reg(0x83, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[4][2]);
    set_parser_ana_w_reg(0x2, 0xa, 0x2, 0x2, &parser_map->PARSER_ANA_W[4][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[4][2]);
//  set_parser_ext_reg(0x1, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[4][2]);
    set_parser_key_s_reg(0x20, 0xff, &parser_map->PARSER_KEY_S[4][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x3fff, &parser_map->PARSER_KEY_W[4][3]);
    set_parser_ana_s_reg(0x3, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[4][3]);
    set_parser_ana_w_reg(0x2, 0xa, 0x2, 0x2, &parser_map->PARSER_ANA_W[4][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[4][3]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[4][3]);
    set_parser_key_s_reg(0x20, 0xff, &parser_map->PARSER_KEY_S[4][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x2000, 0x3fff, &parser_map->PARSER_KEY_W[4][4]);
    set_parser_ana_s_reg(0x3, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[4][4]);
    set_parser_ana_w_reg(0x2, 0xa, 0x2, 0x2, &parser_map->PARSER_ANA_W[4][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[4][4]);
//  set_parser_ext_reg(0x1, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[4][4]);
    set_parser_key_s_reg(0x60, 0xff, &parser_map->PARSER_KEY_S[4][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[4][5]);
    set_parser_ana_s_reg(0x4, 0x3f, 0x0, &parser_map->PARSER_ANA_S[4][5]);
    set_parser_ana_w_reg(0x2, 0x8, 0x0, 0x2, &parser_map->PARSER_ANA_W[4][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[4][5]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[4][5]);
    set_parser_key_s_reg(0x2, 0xff, &parser_map->PARSER_KEY_S[5][0]);
    set_parser_key_w_reg(0x0, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[5][0]);
    set_parser_ana_s_reg(0x42, 0xff, 0x0, &parser_map->PARSER_ANA_S[5][0]);
    set_parser_ana_w_reg(0xa, 0xe, 0x0, 0x8, &parser_map->PARSER_ANA_W[5][0]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[5][0]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x1, &parser_map->PARSER_EXT[5][0]);
    set_parser_key_s_reg(0x2, 0xff, &parser_map->PARSER_KEY_S[5][1]);
    set_parser_key_w_reg(0x100, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[5][1]);
    set_parser_ana_s_reg(0x5, 0x13f, 0x0, &parser_map->PARSER_ANA_S[5][1]);
    set_parser_ana_w_reg(0x8, 0xe, 0x0, 0x8, &parser_map->PARSER_ANA_W[5][1]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[5][1]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x1, &parser_map->PARSER_EXT[5][1]);
    set_parser_key_s_reg(0x2, 0xff, &parser_map->PARSER_KEY_S[5][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x100, 0x100, &parser_map->PARSER_KEY_W[5][2]);
    set_parser_ana_s_reg(0x5, 0x13f, 0x0, &parser_map->PARSER_ANA_S[5][2]);
    set_parser_ana_w_reg(0x4, 0xa, 0x0, 0x4, &parser_map->PARSER_ANA_W[5][2]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[5][2]);
//  set_parser_ext_reg(0x2, 0x0, 0x0, 0, 0x1, &parser_map->PARSER_EXT[5][2]);
    set_parser_key_s_reg(0x42, 0xff, &parser_map->PARSER_KEY_S[6][0]);
    set_parser_key_w_reg(0x0, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[6][0]);
    set_parser_ana_s_reg(0x82, 0xff, 0x0, &parser_map->PARSER_ANA_S[6][0]);
    set_parser_ana_w_reg(0xa, 0xe, 0x0, 0x8, &parser_map->PARSER_ANA_W[6][0]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[6][0]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[6][0]);
    set_parser_key_s_reg(0x42, 0xff, &parser_map->PARSER_KEY_S[6][1]);
    set_parser_key_w_reg(0x100, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[6][1]);
    set_parser_ana_s_reg(0x5, 0x13f, 0x0, &parser_map->PARSER_ANA_S[6][1]);
    set_parser_ana_w_reg(0x8, 0xe, 0x0, 0x8, &parser_map->PARSER_ANA_W[6][1]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[6][1]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[6][1]);
    set_parser_key_s_reg(0x42, 0xff, &parser_map->PARSER_KEY_S[6][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x100, 0x100, &parser_map->PARSER_KEY_W[6][2]);
    set_parser_ana_s_reg(0x5, 0x13f, 0x0, &parser_map->PARSER_ANA_S[6][2]);
    set_parser_ana_w_reg(0x4, 0xa, 0x0, 0x4, &parser_map->PARSER_ANA_W[6][2]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[6][2]);
//  set_parser_ext_reg(0x2, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[6][2]);
    set_parser_key_s_reg(0x82, 0xff, &parser_map->PARSER_KEY_S[7][0]);
    set_parser_key_w_reg(0x0, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[7][0]);
    set_parser_ana_s_reg(0xa2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[7][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[7][0]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[7][0]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[7][0]);
    set_parser_key_s_reg(0x82, 0xff, &parser_map->PARSER_KEY_S[7][1]);
    set_parser_key_w_reg(0x100, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[7][1]);
    set_parser_ana_s_reg(0x5, 0x13f, 0x0, &parser_map->PARSER_ANA_S[7][1]);
    set_parser_ana_w_reg(0x8, 0xe, 0x0, 0x8, &parser_map->PARSER_ANA_W[7][1]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[7][1]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[7][1]);
    set_parser_key_s_reg(0x82, 0xff, &parser_map->PARSER_KEY_S[7][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x100, 0x100, &parser_map->PARSER_KEY_W[7][2]);
    set_parser_ana_s_reg(0x5, 0x13f, 0x0, &parser_map->PARSER_ANA_S[7][2]);
    set_parser_ana_w_reg(0x4, 0xa, 0x0, 0x4, &parser_map->PARSER_ANA_W[7][2]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[7][2]);
//  set_parser_ext_reg(0x2, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[7][2]);
    set_parser_key_s_reg(0x5, 0x13f, &parser_map->PARSER_KEY_S[8][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[8][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[8][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[8][0]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x2, &parser_map->PARSER_EXT[8][0]);
    set_parser_key_s_reg(0x5, 0x13f, &parser_map->PARSER_KEY_S[8][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[8][1]);
    set_parser_ana_s_reg(0x83, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[8][1]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][1]);
//  set_parser_ext_reg(0x0, 0x0, 0xb, 1, 0x2, &parser_map->PARSER_EXT[8][1]);
    set_parser_key_s_reg(0x5, 0x13f, &parser_map->PARSER_KEY_S[8][2]);
    set_parser_key_w_reg(0x0, 0x3fff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[8][2]);
    set_parser_ana_s_reg(0x3, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[8][2]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][2]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x2, &parser_map->PARSER_EXT[8][2]);
    set_parser_key_s_reg(0x5, 0x13f, &parser_map->PARSER_KEY_S[8][3]);
    set_parser_key_w_reg(0x2000, 0x3fff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[8][3]);
    set_parser_ana_s_reg(0x3, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[8][3]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][3]);
//  set_parser_ext_reg(0x0, 0x0, 0xa, 1, 0x2, &parser_map->PARSER_EXT[8][3]);
    set_parser_key_s_reg(0x5, 0x13f, &parser_map->PARSER_KEY_S[8][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[8][4]);
    set_parser_ana_s_reg(0x4, 0x3f, 0x0, &parser_map->PARSER_ANA_S[8][4]);
    set_parser_ana_w_reg(0x0, 0x6, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][4]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x2, &parser_map->PARSER_EXT[8][4]);
    set_parser_key_s_reg(0x105, 0x13f, &parser_map->PARSER_KEY_S[8][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[8][5]);
    set_parser_ana_s_reg(0x83, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[8][5]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][5]);
//  set_parser_ext_reg(0x0, 0x0, 0xb, 1, 0x2, &parser_map->PARSER_EXT[8][5]);
    set_parser_key_s_reg(0x105, 0x13f, &parser_map->PARSER_KEY_S[8][6]);
    set_parser_key_w_reg(0x0, 0x3fff, 0x0, 0x0, &parser_map->PARSER_KEY_W[8][6]);
    set_parser_ana_s_reg(0x3, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[8][6]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][6]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x2, &parser_map->PARSER_EXT[8][6]);
    set_parser_key_s_reg(0x105, 0x13f, &parser_map->PARSER_KEY_S[8][7]);
    set_parser_key_w_reg(0x2000, 0x3fff, 0x0, 0x0, &parser_map->PARSER_KEY_W[8][7]);
    set_parser_ana_s_reg(0x3, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[8][7]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[8][7]);
//  set_parser_ext_reg(0x0, 0x0, 0xa, 1, 0x2, &parser_map->PARSER_EXT[8][7]);
    set_parser_key_s_reg(0x3, 0x3f, &parser_map->PARSER_KEY_S[9][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][0]);
    set_parser_ana_s_reg(0xe2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][0]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][0]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][0]);
    set_parser_key_s_reg(0x8003, 0x803f, &parser_map->PARSER_KEY_S[9][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][1]);
    set_parser_ana_s_reg(0x15d, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][1]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][1]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][1]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][2]);
    set_parser_key_w_reg(0x4, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][2]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[9][2]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][2]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][2]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][2]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][3]);
    set_parser_key_w_reg(0x29, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][3]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[9][3]);
    set_parser_ana_w_reg(0x0, 0x6, 0x2, 0x0, &parser_map->PARSER_ANA_W[9][3]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][3]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][3]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][4]);
    set_parser_key_w_reg(0x1, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][4]);
    set_parser_ana_s_reg(0x107, 0x13f, 0x0, &parser_map->PARSER_ANA_S[9][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][4]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][4]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][4]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][5]);
    set_parser_key_w_reg(0x6, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][5]);
    set_parser_ana_s_reg(0x108, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][5]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][5]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][5]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][6]);
    set_parser_key_w_reg(0x11, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][6]);
    set_parser_ana_s_reg(0x109, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][6]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][6]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][6]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][6]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][7]);
    set_parser_key_w_reg(0x2f, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][7]);
    set_parser_ana_s_reg(0x10a, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][7]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][7]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][7]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][7]);
    set_parser_key_s_reg(0x83, 0xbf, &parser_map->PARSER_KEY_S[9][8]);
    set_parser_key_w_reg(0x32, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][8]);
    set_parser_ana_s_reg(0xe2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][8]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][8]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][8]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[9][24]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][8]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][9]);
    set_parser_key_w_reg(0x32, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][9]);
    set_parser_ana_s_reg(0x23, 0xff, 0x0, &parser_map->PARSER_ANA_S[9][9]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][9]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][9]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[9][25]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][9]);
    set_parser_key_s_reg(0x3, 0xbf, &parser_map->PARSER_KEY_S[9][10]);
    set_parser_key_w_reg(0x84, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[9][10]);
    set_parser_ana_s_reg(0x10b, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[9][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[9][10]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[9][10]);
//  set_parser_ext_reg(0xa, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[9][10]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][0]);
    set_parser_ana_s_reg(0xc, 0x13f, 0xd7f8, &parser_map->PARSER_ANA_S[10][0]);
    set_parser_ana_w_reg(0x28, 0x0, 0x28, 0x28, &parser_map->PARSER_ANA_W[10][0]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][0]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][0]);
    set_parser_key_s_reg(0x8004, 0x803f, &parser_map->PARSER_KEY_S[10][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][1]);
    set_parser_ana_s_reg(0x5d, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[10][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][1]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][1]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][1]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][2]);
    set_parser_key_w_reg(0x400, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][2]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[10][2]);
    set_parser_ana_w_reg(0x28, 0x30, 0x28, 0x28, &parser_map->PARSER_ANA_W[10][2]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][2]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][2]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][3]);
    set_parser_key_w_reg(0x2900, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][3]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[10][3]);
    set_parser_ana_w_reg(0x28, 0x2e, 0x4, 0x28, &parser_map->PARSER_ANA_W[10][3]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][3]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][3]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][4]);
    set_parser_key_w_reg(0x3a00, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][4]);
    set_parser_ana_s_reg(0x7, 0x13f, 0x0, &parser_map->PARSER_ANA_S[10][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][4]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][4]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][4]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][5]);
    set_parser_key_w_reg(0x600, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][5]);
    set_parser_ana_s_reg(0x8, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[10][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][5]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][5]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][5]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][6]);
    set_parser_key_w_reg(0x1100, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][6]);
    set_parser_ana_s_reg(0x9, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[10][6]);
    set_parser_ana_w_reg(0x28, 0x2a, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][6]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][6]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][6]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][7]);
    set_parser_key_w_reg(0x2f00, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][7]);
    set_parser_ana_s_reg(0xa, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[10][7]);
    set_parser_ana_w_reg(0x28, 0x2a, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][7]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][7]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][7]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][8]);
    set_parser_key_w_reg(0x8400, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][8]);
    set_parser_ana_s_reg(0xb, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[10][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][8]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][8]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][8]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][9]);
    set_parser_key_w_reg(0x2c00, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][9]);
    set_parser_ana_s_reg(0x8c, 0x1bf, 0x0, &parser_map->PARSER_ANA_S[10][9]);
    set_parser_ana_w_reg(0x28, 0x2a, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][9]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][9]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][9]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][10]);
    set_parser_key_w_reg(0x3200, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][10]);
    set_parser_ana_s_reg(0x63, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[10][10]);
    set_parser_ana_w_reg(0x28, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][10]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][10]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][10]);
    set_parser_key_s_reg(0x4, 0x3f, &parser_map->PARSER_KEY_S[10][11]);
    set_parser_key_w_reg(0x3b00, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[10][11]);
    set_parser_ana_s_reg(0x10, 0x13f, 0xe3fc, &parser_map->PARSER_ANA_S[10][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[10][11]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[10][11]);
//  set_parser_ext_reg(0x14, 0x0, 0x16, 1, 0x2, &parser_map->PARSER_EXT[10][11]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[11][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[11][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[11][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[11][0]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[11][0]);
    set_parser_key_s_reg(0x808c, 0x80bf, &parser_map->PARSER_KEY_S[11][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[11][1]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[11][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[11][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][1]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[11][1]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][2]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x0, 0x0, &parser_map->PARSER_KEY_W[11][2]);
    set_parser_ana_s_reg(0xd, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[11][2]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[11][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][2]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][2]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][3]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x400, 0xff00, &parser_map->PARSER_KEY_W[11][3]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[11][3]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[11][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][3]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][3]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][4]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[11][4]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[11][4]);
    set_parser_ana_w_reg(0x8, 0xe, 0xb, 0x8, &parser_map->PARSER_ANA_W[11][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][4]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][4]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][5]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[11][5]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[11][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][5]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][5]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][6]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x600, 0xff00, &parser_map->PARSER_KEY_W[11][6]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[11][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][6]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][6]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][7]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[11][7]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[11][7]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][7]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][7]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][8]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[11][8]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[11][8]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][8]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][8]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][9]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[11][9]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[11][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][9]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][9]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][10]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[11][10]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[11][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][10]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][10]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[11][11]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[11][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[11][11]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[11][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[11][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[11][27]);
    set_parser_key_s_reg(0x8c, 0xbf, &parser_map->PARSER_KEY_S[11][12]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[11][12]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[11][12]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[11][12]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[11][12]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[11][12]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[11][28]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[12][0]);
    set_parser_ana_s_reg(0xd, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[12][0]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[12][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][0]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][0]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x400, 0xff00, &parser_map->PARSER_KEY_W[12][1]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[12][1]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[12][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][1]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][1]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[12][2]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[12][2]);
    set_parser_ana_w_reg(0x8, 0xe, 0x6, 0x8, &parser_map->PARSER_ANA_W[12][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][2]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][2]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[12][3]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[12][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][3]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][3]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x600, 0xff00, &parser_map->PARSER_KEY_W[12][4]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[12][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][4]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][4]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[12][5]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[12][5]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][5]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][5]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[12][6]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[12][6]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][6]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][6]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[12][7]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[12][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][7]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][7]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[12][8]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[12][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][8]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][8]);
    set_parser_key_s_reg(0xc, 0x3f, &parser_map->PARSER_KEY_S[12][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x2c00, 0xff00, &parser_map->PARSER_KEY_W[12][9]);
    set_parser_ana_s_reg(0x8d, 0xbf, 0x0, &parser_map->PARSER_ANA_S[12][9]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][9]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][9]);
    set_parser_key_s_reg(0xc, 0xbf, &parser_map->PARSER_KEY_S[12][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[12][10]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[12][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[12][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[12][10]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[12][10]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[13][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[13][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[13][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[13][0]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[13][0]);
    set_parser_key_s_reg(0x808d, 0x80bf, &parser_map->PARSER_KEY_S[13][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[13][1]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[13][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[13][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][1]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[13][1]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][2]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x0, 0x0, &parser_map->PARSER_KEY_W[13][2]);
    set_parser_ana_s_reg(0xe, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[13][2]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[13][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][2]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][2]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][3]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x400, 0xff00, &parser_map->PARSER_KEY_W[13][3]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[13][3]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[13][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][3]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][3]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][4]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[13][4]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[13][4]);
    set_parser_ana_w_reg(0x8, 0xe, 0xb, 0x8, &parser_map->PARSER_ANA_W[13][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][4]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][4]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][5]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[13][5]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[13][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[13][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][5]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][5]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][6]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x600, 0xff00, &parser_map->PARSER_KEY_W[13][6]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[13][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[13][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][6]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][6]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][7]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[13][7]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[13][7]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[13][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][7]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][7]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][8]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[13][8]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[13][8]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[13][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][8]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][8]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][9]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[13][9]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[13][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[13][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][9]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][9]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][10]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[13][10]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[13][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[13][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][10]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][10]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[13][11]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[13][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[13][11]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[13][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[13][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[13][27]);
    set_parser_key_s_reg(0x8d, 0xbf, &parser_map->PARSER_KEY_S[13][12]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[13][12]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[13][12]);
    set_parser_ana_w_reg(0x8, 0x2, 0x1, 0x8, &parser_map->PARSER_ANA_W[13][12]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[13][12]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[13][12]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[13][28]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[14][0]);
    set_parser_ana_s_reg(0xe, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[14][0]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[14][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][0]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][0]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x400, 0xff00, &parser_map->PARSER_KEY_W[14][1]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[14][1]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[14][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][1]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][1]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[14][2]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[14][2]);
    set_parser_ana_w_reg(0x8, 0xe, 0x4, 0x8, &parser_map->PARSER_ANA_W[14][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][2]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][2]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[14][3]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[14][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][3]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][3]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x600, 0xff00, &parser_map->PARSER_KEY_W[14][4]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[14][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][4]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][4]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[14][5]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[14][5]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][5]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][5]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[14][6]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[14][6]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][6]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][6]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[14][7]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[14][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][7]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][7]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[14][8]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[14][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][8]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][8]);
    set_parser_key_s_reg(0xd, 0x3f, &parser_map->PARSER_KEY_S[14][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x2c00, 0xff00, &parser_map->PARSER_KEY_W[14][9]);
    set_parser_ana_s_reg(0x8e, 0xbf, 0x0, &parser_map->PARSER_ANA_S[14][9]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][9]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][9]);
    set_parser_key_s_reg(0xd, 0xbf, &parser_map->PARSER_KEY_S[14][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[14][10]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[14][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[14][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[14][10]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[14][10]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[15][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[15][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[15][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[15][0]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[15][0]);
    set_parser_key_s_reg(0x808e, 0x80bf, &parser_map->PARSER_KEY_S[15][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[15][1]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[15][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[15][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][1]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[15][1]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][2]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x0, 0x0, &parser_map->PARSER_KEY_W[15][2]);
    set_parser_ana_s_reg(0xf, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[15][2]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[15][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][2]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][2]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][3]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x400, 0xff00, &parser_map->PARSER_KEY_W[15][3]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[15][3]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[15][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][3]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][3]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][4]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[15][4]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[15][4]);
    set_parser_ana_w_reg(0x8, 0xe, 0xb, 0x8, &parser_map->PARSER_ANA_W[15][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][4]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][4]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][5]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[15][5]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[15][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[15][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][5]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][5]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][6]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x600, 0xff00, &parser_map->PARSER_KEY_W[15][6]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[15][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[15][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][6]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][6]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][7]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[15][7]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[15][7]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[15][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][7]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][7]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][8]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[15][8]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[15][8]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[15][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][8]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][8]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][9]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[15][9]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[15][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[15][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][9]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][9]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][10]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[15][10]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[15][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[15][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][10]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][10]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[15][11]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[15][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[15][11]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[15][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[15][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[15][27]);
    set_parser_key_s_reg(0x8e, 0xbf, &parser_map->PARSER_KEY_S[15][12]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[15][12]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[15][12]);
    set_parser_ana_w_reg(0x8, 0x2, 0x1, 0x8, &parser_map->PARSER_ANA_W[15][12]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[15][12]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[15][12]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[15][28]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[16][0]);
    set_parser_ana_s_reg(0xf, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[16][0]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[16][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][0]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][0]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x400, 0xff00, &parser_map->PARSER_KEY_W[16][1]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[16][1]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[16][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][1]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][1]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[16][2]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[16][2]);
    set_parser_ana_w_reg(0x8, 0xe, 0x4, 0x8, &parser_map->PARSER_ANA_W[16][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][2]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][2]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[16][3]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[16][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][3]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][3]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x600, 0xff00, &parser_map->PARSER_KEY_W[16][4]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[16][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][4]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][4]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[16][5]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[16][5]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][5]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][5]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[16][6]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[16][6]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][6]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][6]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[16][7]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[16][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][7]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][7]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[16][8]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[16][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][8]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][8]);
    set_parser_key_s_reg(0xe, 0x3f, &parser_map->PARSER_KEY_S[16][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x2c00, 0xff00, &parser_map->PARSER_KEY_W[16][9]);
    set_parser_ana_s_reg(0x8f, 0xbf, 0x0, &parser_map->PARSER_ANA_S[16][9]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][9]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][9]);
    set_parser_key_s_reg(0xe, 0xbf, &parser_map->PARSER_KEY_S[16][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[16][10]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[16][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[16][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[16][10]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[16][10]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[17][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[17][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[17][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[17][0]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[17][0]);
    set_parser_key_s_reg(0x808f, 0x80bf, &parser_map->PARSER_KEY_S[17][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[17][1]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[17][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[17][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][1]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[17][1]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][2]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x0, 0x0, &parser_map->PARSER_KEY_W[17][2]);
    set_parser_ana_s_reg(0xf, 0xbf, 0xd7f8, &parser_map->PARSER_ANA_S[17][2]);
    set_parser_ana_w_reg(0x8, 0x0, 0x8, 0x8, &parser_map->PARSER_ANA_W[17][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][2]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][2]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][3]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x400, 0xff00, &parser_map->PARSER_KEY_W[17][3]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[17][3]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[17][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][3]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][3]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][4]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[17][4]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[17][4]);
    set_parser_ana_w_reg(0x8, 0xe, 0xb, 0x8, &parser_map->PARSER_ANA_W[17][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][4]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][4]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][5]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[17][5]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[17][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[17][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][5]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][5]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][6]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x600, 0xff00, &parser_map->PARSER_KEY_W[17][6]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[17][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[17][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][6]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][6]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][7]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[17][7]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[17][7]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[17][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][7]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][7]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][8]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[17][8]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[17][8]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[17][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][8]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][8]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][9]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[17][9]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[17][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[17][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][9]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][9]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][10]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[17][10]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[17][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[17][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][10]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][10]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[17][11]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[17][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[17][11]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[17][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[17][11]);
//  set_parser_ext_reg(0x0, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[17][27]);
    set_parser_key_s_reg(0x8f, 0xbf, &parser_map->PARSER_KEY_S[17][12]);
    set_parser_key_w_reg(0x0, 0xfff8, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[17][12]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[17][12]);
    set_parser_ana_w_reg(0x8, 0x2, 0x1, 0x8, &parser_map->PARSER_ANA_W[17][12]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[17][12]);
//  set_parser_ext_reg(0x81, 0x0, 0xa, 1, 0x0, &parser_map->PARSER_EXT[17][12]);
//  set_parser_ext_reg(0x0, 0x0, 0xc, 1, 0x0, &parser_map->PARSER_EXT[17][28]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[18][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[18][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[18][0]);
    set_parser_exc_reg(0x2, 1, &parser_map->PARSER_EXC[18][0]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][0]);
    set_parser_key_s_reg(0x800f, 0x80bf, &parser_map->PARSER_KEY_S[18][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[18][1]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[18][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[18][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][1]);
//  set_parser_ext_reg(0x81, 0x0, 0xb, 1, 0x0, &parser_map->PARSER_EXT[18][1]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x400, 0xff00, &parser_map->PARSER_KEY_W[18][2]);
    set_parser_ana_s_reg(0x97, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[18][2]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[18][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][2]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][2]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x2900, 0xff00, &parser_map->PARSER_KEY_W[18][3]);
    set_parser_ana_s_reg(0x98, 0xbf, 0x0, &parser_map->PARSER_ANA_S[18][3]);
    set_parser_ana_w_reg(0x8, 0xe, 0x4, 0x8, &parser_map->PARSER_ANA_W[18][3]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][3]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][3]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x3a00, 0xff00, &parser_map->PARSER_KEY_W[18][4]);
    set_parser_ana_s_reg(0x7, 0x3f, 0x0, &parser_map->PARSER_ANA_S[18][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][4]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][4]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][4]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x600, 0xff00, &parser_map->PARSER_KEY_W[18][5]);
    set_parser_ana_s_reg(0x8, 0xff, 0x0, &parser_map->PARSER_ANA_S[18][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][5]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][5]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][5]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x1100, 0xff00, &parser_map->PARSER_KEY_W[18][6]);
    set_parser_ana_s_reg(0x9, 0xff, 0x0, &parser_map->PARSER_ANA_S[18][6]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][6]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][6]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x2f00, 0xff00, &parser_map->PARSER_KEY_W[18][7]);
    set_parser_ana_s_reg(0xa, 0xff, 0x0, &parser_map->PARSER_ANA_S[18][7]);
    set_parser_ana_w_reg(0x8, 0xa, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][7]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][7]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][7]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x8400, 0xff00, &parser_map->PARSER_KEY_W[18][8]);
    set_parser_ana_s_reg(0xb, 0xff, 0x0, &parser_map->PARSER_ANA_S[18][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][8]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][8]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][8]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x3b00, 0xff00, &parser_map->PARSER_KEY_W[18][9]);
    set_parser_ana_s_reg(0x10, 0x3f, 0x0, &parser_map->PARSER_ANA_S[18][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][9]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][9]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][9]);
    set_parser_key_s_reg(0xf, 0xbf, &parser_map->PARSER_KEY_S[18][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x3200, 0xff00, &parser_map->PARSER_KEY_W[18][10]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[18][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[18][10]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[18][10]);
//  set_parser_ext_reg(0x81, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[18][10]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][0]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][0]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][0]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][0]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][0]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][16]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][1]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][1]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][1]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][1]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][17]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][1]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][2]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][2]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][2]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][2]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][18]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][2]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][3]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][3]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][3]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][3]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][19]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][3]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][4]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][4]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][4]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][4]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][20]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][4]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][5]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][5]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][5]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][5]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][21]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][5]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][6]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][6]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][6]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][6]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][22]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][6]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][7]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][7]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][7]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][7]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][23]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][7]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][8]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][8]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][8]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][8]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][24]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][8]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][9]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][9]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][9]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][9]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][25]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][9]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][10]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][10]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][10]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][10]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][10]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][26]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][11]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][11]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][11]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][11]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][11]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][27]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][12]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][12]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][12]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][12]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][12]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][12]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][28]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][13]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][13]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][13]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][13]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][13]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][13]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][29]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][14]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][14]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][14]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][14]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][14]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][14]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][30]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[19][15]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[19][15]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[19][15]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[19][15]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[19][15]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[19][15]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[19][31]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][0]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][0]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][0]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][0]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][0]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][16]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][1]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][1]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][1]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][1]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][17]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][1]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][2]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][2]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][2]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][2]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][18]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][2]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][3]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][3]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][3]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][3]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][19]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][3]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][4]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][4]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][4]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][4]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][20]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][4]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][5]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][5]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][5]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][5]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][21]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][5]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][6]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][6]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][6]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][6]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][22]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][6]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][7]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][7]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][7]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][7]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][23]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][7]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][8]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][8]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][8]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][8]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][24]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][8]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][9]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][9]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][9]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][9]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][25]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][9]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][10]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][10]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][10]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][10]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][10]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][26]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][11]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][11]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][11]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][11]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][11]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][27]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][12]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][12]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][12]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][12]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][12]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][12]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][28]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][13]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][13]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][13]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][13]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][13]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][13]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][29]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][14]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][14]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][14]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][14]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][14]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][14]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][30]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[20][15]);
    set_parser_key_w_reg(0x1194, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[20][15]);
    set_parser_ana_s_reg(0xa3, 0xff, 0x0, &parser_map->PARSER_ANA_S[20][15]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[20][15]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[20][15]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[20][15]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[20][31]);
    set_parser_key_s_reg(0x10, 0x3f, &parser_map->PARSER_KEY_S[21][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[21][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[21][0]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x3, &parser_map->PARSER_EXT[21][0]);
    set_parser_key_s_reg(0x7, 0x803f, &parser_map->PARSER_KEY_S[21][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][1]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[21][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][1]);
    set_parser_exc_reg(0x2, 1, &parser_map->PARSER_EXC[21][1]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x3, &parser_map->PARSER_EXT[21][1]);
    set_parser_key_s_reg(0x8, 0xff, &parser_map->PARSER_KEY_S[21][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][2]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[21][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][2]);
    set_parser_exc_reg(0x12, 1, &parser_map->PARSER_EXC[21][2]);
//  set_parser_ext_reg(0x1, 0x10, 0x5, 1, 0x0, &parser_map->PARSER_EXT[21][18]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][2]);
    set_parser_key_s_reg(0x8008, 0x80ff, &parser_map->PARSER_KEY_S[21][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][3]);
    set_parser_ana_s_reg(0x48, 0xff, 0xa03c, &parser_map->PARSER_ANA_S[21][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0xc, 0x0, &parser_map->PARSER_ANA_W[21][3]);
    set_parser_exc_reg(0x12, 0, &parser_map->PARSER_EXC[21][3]);
//  set_parser_ext_reg(0x1, 0x10, 0x5, 1, 0x0, &parser_map->PARSER_EXT[21][19]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][3]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[21][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][4]);
    set_parser_ana_s_reg(0x49, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][4]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[21][4]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x0, &parser_map->PARSER_EXT[21][20]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x3, &parser_map->PARSER_EXT[21][4]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[21][5]);
    set_parser_key_w_reg(0x12b5, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][5]);
    set_parser_ana_s_reg(0x52, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][5]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[21][5]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[21][21]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][5]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[21][6]);
    set_parser_key_w_reg(0x12b6, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][6]);
    set_parser_ana_s_reg(0x12, 0xff, 0xe0fc, &parser_map->PARSER_ANA_S[21][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x10, 0x0, &parser_map->PARSER_ANA_W[21][6]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[21][6]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[21][22]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][6]);
    set_parser_key_s_reg(0x9, 0xff, &parser_map->PARSER_KEY_S[21][7]);
    set_parser_key_w_reg(0x17c1, 0xffff, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][7]);
    set_parser_ana_s_reg(0x11, 0x3f, 0x60fc, &parser_map->PARSER_ANA_S[21][7]);
    set_parser_ana_w_reg(0x7, 0x3, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][7]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[21][7]);
//  set_parser_ext_reg(0x1, 0x6, 0x4, 1, 0x0, &parser_map->PARSER_EXT[21][23]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][7]);
    set_parser_key_s_reg(0xb, 0xff, &parser_map->PARSER_KEY_S[21][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][8]);
    set_parser_ana_s_reg(0x4b, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][8]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0xc, &parser_map->PARSER_ANA_W[21][8]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[21][8]);
//  set_parser_ext_reg(0x2, 0x8, 0x6, 1, 0x0, &parser_map->PARSER_EXT[21][24]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][8]);
    set_parser_key_s_reg(0xa, 0xff, &parser_map->PARSER_KEY_S[21][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x3000, &parser_map->PARSER_KEY_W[21][9]);
    set_parser_ana_s_reg(0x4a, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][9]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][9]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[21][9]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][9]);
    set_parser_key_s_reg(0xa, 0xff, &parser_map->PARSER_KEY_S[21][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x1000, 0x3000, &parser_map->PARSER_KEY_W[21][10]);
    set_parser_ana_s_reg(0x4a, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][10]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x4, &parser_map->PARSER_ANA_W[21][10]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[21][10]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][10]);
    set_parser_key_s_reg(0xa, 0xff, &parser_map->PARSER_KEY_S[21][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x2000, 0x3000, &parser_map->PARSER_KEY_W[21][11]);
    set_parser_ana_s_reg(0x4a, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][11]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x4, &parser_map->PARSER_ANA_W[21][11]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[21][11]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][11]);
    set_parser_key_s_reg(0xa, 0xff, &parser_map->PARSER_KEY_S[21][12]);
    set_parser_key_w_reg(0x0, 0x0, 0x3000, 0x3000, &parser_map->PARSER_KEY_W[21][12]);
    set_parser_ana_s_reg(0x4a, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][12]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x8, &parser_map->PARSER_ANA_W[21][12]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[21][12]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][12]);
    set_parser_key_s_reg(0xa, 0xff, &parser_map->PARSER_KEY_S[21][13]);
    set_parser_key_w_reg(0x6558, 0xffff, 0x2000, 0xffff, &parser_map->PARSER_KEY_W[21][13]);
    set_parser_ana_s_reg(0x13, 0x3f, 0x0, &parser_map->PARSER_ANA_S[21][13]);
    set_parser_ana_w_reg(0x14, 0x16, 0x0, 0x8, &parser_map->PARSER_ANA_W[21][13]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[21][13]);
//  set_parser_ext_reg(0x2, 0x0, 0x17, 1, 0x3, &parser_map->PARSER_EXT[21][13]);
    set_parser_key_s_reg(0xa3, 0xff, &parser_map->PARSER_KEY_S[21][14]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][14]);
    set_parser_ana_s_reg(0x63, 0xff, 0x0, &parser_map->PARSER_ANA_S[21][14]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[21][14]);
    set_parser_exc_reg(0x6, 0, &parser_map->PARSER_EXC[21][14]);
//  set_parser_ext_reg(0x81, 0x4, 0xd, 1, 0x0, &parser_map->PARSER_EXT[21][14]);
    set_parser_key_s_reg(0xe2, 0x1ff, &parser_map->PARSER_KEY_S[21][15]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[21][15]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[21][15]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[21][15]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[21][15]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x3, &parser_map->PARSER_EXT[21][15]);
    set_parser_key_s_reg(0x23, 0xff, &parser_map->PARSER_KEY_S[22][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[22][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][0]);
    set_parser_exc_reg(0x4, 1, &parser_map->PARSER_EXC[22][0]);
//  set_parser_ext_reg(0x2, 0x0, 0x8, 1, 0x7, &parser_map->PARSER_EXT[22][0]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x3, &parser_map->PARSER_EXT[22][16]);
    set_parser_key_s_reg(0x63, 0xff, &parser_map->PARSER_KEY_S[22][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][1]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[22][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][1]);
    set_parser_exc_reg(0x4, 1, &parser_map->PARSER_EXC[22][1]);
//  set_parser_ext_reg(0x2, 0x0, 0x8, 1, 0x7, &parser_map->PARSER_EXT[22][1]);
    set_parser_key_s_reg(0x49, 0xff, &parser_map->PARSER_KEY_S[22][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][2]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[22][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][2]);
    set_parser_exc_reg(0x6, 1, &parser_map->PARSER_EXC[22][2]);
//  set_parser_ext_reg(0x81, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[22][2]);
    set_parser_key_s_reg(0x8049, 0x80ff, &parser_map->PARSER_KEY_S[22][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][3]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[22][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[22][3]);
    set_parser_exc_reg(0x6, 0, &parser_map->PARSER_EXC[22][3]);
//  set_parser_ext_reg(0x81, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[22][3]);
    set_parser_key_s_reg(0x8048, 0x80ff, &parser_map->PARSER_KEY_S[22][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][4]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[22][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][4]);
    set_parser_key_s_reg(0x804b, 0x80ff, &parser_map->PARSER_KEY_S[22][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][5]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[22][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][5]);
    set_parser_key_s_reg(0x8007, 0x803f, &parser_map->PARSER_KEY_S[22][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][6]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[22][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x2, &parser_map->PARSER_ANA_W[22][6]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[22][6]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x3, &parser_map->PARSER_EXT[22][6]);
    set_parser_key_s_reg(0x52, 0xff, &parser_map->PARSER_KEY_S[22][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][7]);
    set_parser_ana_s_reg(0x13, 0x3f, 0x0, &parser_map->PARSER_ANA_S[22][7]);
    set_parser_ana_w_reg(0x1c, 0x1e, 0x0, 0x10, &parser_map->PARSER_ANA_W[22][7]);
    set_parser_exc_reg(0x6, 0, &parser_map->PARSER_EXC[22][7]);
//  set_parser_ext_reg(0x81, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[22][7]);
    set_parser_key_s_reg(0x12, 0xff, &parser_map->PARSER_KEY_S[22][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][8]);
    set_parser_ana_s_reg(0x13, 0x3f, 0x0, &parser_map->PARSER_ANA_S[22][8]);
    set_parser_ana_w_reg(0x1c, 0x12, 0x0, 0x10, &parser_map->PARSER_ANA_W[22][8]);
    set_parser_exc_reg(0x6, 0, &parser_map->PARSER_EXC[22][8]);
//  set_parser_ext_reg(0x81, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[22][8]);
    set_parser_key_s_reg(0x11, 0x3f, &parser_map->PARSER_KEY_S[22][9]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[22][9]);
    set_parser_ana_s_reg(0x13, 0x3f, 0x0, &parser_map->PARSER_ANA_S[22][9]);
    set_parser_ana_w_reg(0x1c, 0x6, 0x0, 0x10, &parser_map->PARSER_ANA_W[22][9]);
    set_parser_exc_reg(0x6, 0, &parser_map->PARSER_EXC[22][9]);
//  set_parser_ext_reg(0x81, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[22][9]);
    set_parser_key_s_reg(0x4a, 0xff, &parser_map->PARSER_KEY_S[22][10]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x8000, &parser_map->PARSER_KEY_W[22][10]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[22][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][10]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[22][10]);
    set_parser_key_s_reg(0x4a, 0xff, &parser_map->PARSER_KEY_S[22][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x8000, 0x8000, &parser_map->PARSER_KEY_W[22][11]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[22][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[22][11]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[22][11]);
    set_parser_key_s_reg(0x4a, 0xff, &parser_map->PARSER_KEY_S[22][12]);
    set_parser_key_w_reg(0x800, 0xffff, 0x0, 0x8000, &parser_map->PARSER_KEY_W[22][12]);
    set_parser_ana_s_reg(0x17, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[22][12]);
    set_parser_ana_w_reg(0x4, 0xc, 0x4, 0x4, &parser_map->PARSER_ANA_W[22][12]);
    set_parser_key_s_reg(0x4a, 0xff, &parser_map->PARSER_KEY_S[22][13]);
    set_parser_key_w_reg(0x800, 0xffff, 0x8000, 0x8000, &parser_map->PARSER_KEY_W[22][13]);
    set_parser_ana_s_reg(0x17, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[22][13]);
    set_parser_ana_w_reg(0x8, 0x10, 0x8, 0x8, &parser_map->PARSER_ANA_W[22][13]);
    set_parser_key_s_reg(0x4a, 0xff, &parser_map->PARSER_KEY_S[22][14]);
    set_parser_key_w_reg(0x86dd, 0xffff, 0x0, 0x8000, &parser_map->PARSER_KEY_W[22][14]);
    set_parser_ana_s_reg(0x18, 0xbf, 0x0, &parser_map->PARSER_ANA_S[22][14]);
    set_parser_ana_w_reg(0x4, 0xa, 0x0, 0x4, &parser_map->PARSER_ANA_W[22][14]);
    set_parser_key_s_reg(0x4a, 0xff, &parser_map->PARSER_KEY_S[22][15]);
    set_parser_key_w_reg(0x86dd, 0xffff, 0x8000, 0x8000, &parser_map->PARSER_KEY_W[22][15]);
    set_parser_ana_s_reg(0x18, 0xbf, 0x0, &parser_map->PARSER_ANA_S[22][15]);
    set_parser_ana_w_reg(0x8, 0xe, 0x0, 0x8, &parser_map->PARSER_ANA_W[22][15]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[23][0]);
    set_parser_ana_s_reg(0x1a2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[23][0]);
    set_parser_ana_w_reg(0xc, 0x8, 0x0, 0xe, &parser_map->PARSER_ANA_W[23][0]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][0]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][0]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[23][1]);
    set_parser_ana_s_reg(0x14, 0x3f, 0x0, &parser_map->PARSER_ANA_S[23][1]);
    set_parser_ana_w_reg(0x10, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[23][1]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][1]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][1]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[23][2]);
    set_parser_ana_s_reg(0x14, 0x3f, 0x0, &parser_map->PARSER_ANA_S[23][2]);
    set_parser_ana_w_reg(0x10, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[23][2]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][2]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][2]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x8847, 0xffff, &parser_map->PARSER_KEY_W[23][3]);
    set_parser_ana_s_reg(0xa1, 0xff, 0x0, &parser_map->PARSER_ANA_S[23][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[23][3]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][3]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][3]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x8848, 0xffff, &parser_map->PARSER_KEY_W[23][4]);
    set_parser_ana_s_reg(0xa1, 0xff, 0x0, &parser_map->PARSER_ANA_S[23][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[23][4]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][4]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][4]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x800, 0xffff, &parser_map->PARSER_KEY_W[23][5]);
    set_parser_ana_s_reg(0x21, 0xff, 0x0, &parser_map->PARSER_ANA_S[23][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[23][5]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][5]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][5]);
    set_parser_key_s_reg(0x13, 0x3f, &parser_map->PARSER_KEY_S[23][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x86dd, 0xffff, &parser_map->PARSER_KEY_W[23][6]);
    set_parser_ana_s_reg(0x61, 0xff, 0x0, &parser_map->PARSER_ANA_S[23][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0xc, &parser_map->PARSER_ANA_W[23][6]);
    set_parser_exc_reg(0xc, 0, &parser_map->PARSER_EXC[23][6]);
//  set_parser_ext_reg(0x6, 0x0, 0x19, 1, 0x4, &parser_map->PARSER_EXT[23][6]);
    set_parser_key_s_reg(0x1d, 0xff, &parser_map->PARSER_KEY_S[23][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[23][7]);
    set_parser_ana_s_reg(0x19d, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[23][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x10, &parser_map->PARSER_ANA_W[23][7]);
//  set_parser_ext_reg(0x4, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[23][23]);
//  set_parser_ext_reg(0x4, 0x0, 0xe, 1, 0x2, &parser_map->PARSER_EXT[23][7]);
    set_parser_key_s_reg(0x4b, 0xff, &parser_map->PARSER_KEY_S[23][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[23][8]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[23][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[23][8]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[23][8]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[24][0]);
    set_parser_ana_s_reg(0x1a2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[24][0]);
    set_parser_ana_w_reg(0x4, 0x2, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][0]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][0]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][0]);
//  set_parser_ext_reg(0x1, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][16]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x8100, 0xffff, &parser_map->PARSER_KEY_W[24][1]);
    set_parser_ana_s_reg(0x15, 0x3f, 0x0, &parser_map->PARSER_ANA_S[24][1]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][1]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][1]);
//  set_parser_ext_reg(0x1, 0x2, 0x1c, 1, 0x0, &parser_map->PARSER_EXT[24][17]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][1]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x88a8, 0xffff, &parser_map->PARSER_KEY_W[24][2]);
    set_parser_ana_s_reg(0x15, 0x3f, 0x0, &parser_map->PARSER_ANA_S[24][2]);
    set_parser_ana_w_reg(0x8, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][2]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][2]);
//  set_parser_ext_reg(0x1, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][18]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][2]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x8847, 0xffff, &parser_map->PARSER_KEY_W[24][3]);
    set_parser_ana_s_reg(0xa1, 0xff, 0x0, &parser_map->PARSER_ANA_S[24][3]);
    set_parser_ana_w_reg(0x4, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][3]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][3]);
//  set_parser_ext_reg(0x1, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][19]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][3]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x8848, 0xffff, &parser_map->PARSER_KEY_W[24][4]);
    set_parser_ana_s_reg(0xa1, 0xff, 0x0, &parser_map->PARSER_ANA_S[24][4]);
    set_parser_ana_w_reg(0x4, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][4]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][4]);
//  set_parser_ext_reg(0x1, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][20]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][4]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x800, 0xffff, &parser_map->PARSER_KEY_W[24][5]);
    set_parser_ana_s_reg(0x21, 0xff, 0x0, &parser_map->PARSER_ANA_S[24][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][5]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][5]);
//  set_parser_ext_reg(0x1, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][21]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][5]);
    set_parser_key_s_reg(0x14, 0x3f, &parser_map->PARSER_KEY_S[24][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x86dd, 0xffff, &parser_map->PARSER_KEY_W[24][6]);
    set_parser_ana_s_reg(0x61, 0xff, 0x0, &parser_map->PARSER_ANA_S[24][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[24][6]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[24][6]);
//  set_parser_ext_reg(0x1, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][22]);
//  set_parser_ext_reg(0x1, 0x0, 0x1a, 1, 0x0, &parser_map->PARSER_EXT[24][6]);
    set_parser_key_s_reg(0x9d, 0xff, &parser_map->PARSER_KEY_S[24][7]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[24][7]);
    set_parser_ana_s_reg(0x1d, 0x3f, 0x0, &parser_map->PARSER_ANA_S[24][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[24][7]);
//  set_parser_ext_reg(0x2, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][23]);
//  set_parser_ext_reg(0x2, 0x0, 0xe, 1, 0x0, &parser_map->PARSER_EXT[24][7]);
    set_parser_key_s_reg(0x5d, 0xff, &parser_map->PARSER_KEY_S[24][8]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[24][8]);
    set_parser_ana_s_reg(0x9d, 0xff, 0x0, &parser_map->PARSER_ANA_S[24][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[24][8]);
//  set_parser_ext_reg(0x2, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[24][24]);
//  set_parser_ext_reg(0x2, 0x0, 0xe, 1, 0x3, &parser_map->PARSER_EXT[24][8]);
    set_parser_key_s_reg(0x15, 0x3f, &parser_map->PARSER_KEY_S[25][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[25][0]);
    set_parser_ana_s_reg(0x1a2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[25][0]);
    set_parser_ana_w_reg(0x4, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[25][0]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[25][0]);
//  set_parser_ext_reg(0x1, 0x0, 0x1b, 1, 0x0, &parser_map->PARSER_EXT[25][0]);
//  set_parser_ext_reg(0x81, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][16]);
    set_parser_key_s_reg(0x15, 0x3f, &parser_map->PARSER_KEY_S[25][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x8847, 0xffff, &parser_map->PARSER_KEY_W[25][1]);
    set_parser_ana_s_reg(0xa1, 0xff, 0x0, &parser_map->PARSER_ANA_S[25][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[25][1]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[25][1]);
//  set_parser_ext_reg(0x81, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][17]);
//  set_parser_ext_reg(0x1, 0x0, 0x1b, 1, 0x0, &parser_map->PARSER_EXT[25][1]);
    set_parser_key_s_reg(0x15, 0x3f, &parser_map->PARSER_KEY_S[25][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x8848, 0xffff, &parser_map->PARSER_KEY_W[25][2]);
    set_parser_ana_s_reg(0xa1, 0xff, 0x0, &parser_map->PARSER_ANA_S[25][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[25][2]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[25][2]);
//  set_parser_ext_reg(0x81, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][18]);
//  set_parser_ext_reg(0x1, 0x0, 0x1b, 1, 0x0, &parser_map->PARSER_EXT[25][2]);
    set_parser_key_s_reg(0x15, 0x3f, &parser_map->PARSER_KEY_S[25][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x800, 0xffff, &parser_map->PARSER_KEY_W[25][3]);
    set_parser_ana_s_reg(0x21, 0xff, 0x0, &parser_map->PARSER_ANA_S[25][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[25][3]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[25][3]);
//  set_parser_ext_reg(0x81, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][19]);
//  set_parser_ext_reg(0x1, 0x0, 0x1b, 1, 0x0, &parser_map->PARSER_EXT[25][3]);
    set_parser_key_s_reg(0x15, 0x3f, &parser_map->PARSER_KEY_S[25][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x86dd, 0xffff, &parser_map->PARSER_KEY_W[25][4]);
    set_parser_ana_s_reg(0x61, 0xff, 0x0, &parser_map->PARSER_ANA_S[25][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x4, &parser_map->PARSER_ANA_W[25][4]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[25][4]);
//  set_parser_ext_reg(0x81, 0x2, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][20]);
//  set_parser_ext_reg(0x1, 0x0, 0x1b, 1, 0x0, &parser_map->PARSER_EXT[25][4]);
    set_parser_key_s_reg(0x1d, 0x3f, &parser_map->PARSER_KEY_S[25][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[25][5]);
    set_parser_ana_s_reg(0x1d, 0x3f, 0x0, &parser_map->PARSER_ANA_S[25][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[25][5]);
//  set_parser_ext_reg(0x2, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][21]);
//  set_parser_ext_reg(0x2, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[25][5]);
    set_parser_key_s_reg(0xa1, 0xff, &parser_map->PARSER_KEY_S[26][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[26][0]);
    set_parser_ana_s_reg(0x16, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[26][0]);
    set_parser_ana_w_reg(0x4, 0x8, 0x0, 0x2, &parser_map->PARSER_ANA_W[26][0]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[26][0]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[26][0]);
    set_parser_key_s_reg(0x21, 0xff, &parser_map->PARSER_KEY_S[26][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[26][1]);
    set_parser_ana_s_reg(0x17, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[26][1]);
    set_parser_ana_w_reg(0x2, 0xa, 0x2, 0x2, &parser_map->PARSER_ANA_W[26][1]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[26][1]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[26][1]);
    set_parser_key_s_reg(0x61, 0xff, &parser_map->PARSER_KEY_S[26][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[26][2]);
    set_parser_ana_s_reg(0x18, 0xbf, 0x0, &parser_map->PARSER_ANA_S[26][2]);
    set_parser_ana_w_reg(0x2, 0x8, 0x0, 0x2, &parser_map->PARSER_ANA_W[26][2]);
    set_parser_exc_reg(0x2, 0, &parser_map->PARSER_EXC[26][2]);
//  set_parser_ext_reg(0x1, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[26][2]);
    set_parser_key_s_reg(0x1d, 0x3f, &parser_map->PARSER_KEY_S[26][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[26][3]);
    set_parser_ana_s_reg(0x1d, 0x3f, 0x0, &parser_map->PARSER_ANA_S[26][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[26][3]);
//  set_parser_ext_reg(0x2, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[26][19]);
//  set_parser_ext_reg(0x2, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[26][3]);
    set_parser_key_s_reg(0x16, 0x3f, &parser_map->PARSER_KEY_S[27][0]);
    set_parser_key_w_reg(0x0, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[27][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[27][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[27][0]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[27][0]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x5, &parser_map->PARSER_EXT[27][0]);
    set_parser_key_s_reg(0x16, 0x3f, &parser_map->PARSER_KEY_S[27][1]);
    set_parser_key_w_reg(0x100, 0x100, 0x0, 0x100, &parser_map->PARSER_KEY_W[27][1]);
    set_parser_ana_s_reg(0x19, 0x3f, 0x0, &parser_map->PARSER_ANA_S[27][1]);
    set_parser_ana_w_reg(0x8, 0x1, 0x0, 0x8, &parser_map->PARSER_ANA_W[27][1]);
    set_parser_exc_reg(0x8, 0, &parser_map->PARSER_EXC[27][1]);
//  set_parser_ext_reg(0x0, 0x0, 0x1d, 1, 0x5, &parser_map->PARSER_EXT[27][1]);
    set_parser_key_s_reg(0x16, 0x3f, &parser_map->PARSER_KEY_S[27][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x100, 0x100, &parser_map->PARSER_KEY_W[27][2]);
    set_parser_ana_s_reg(0x19, 0x3f, 0x0, &parser_map->PARSER_ANA_S[27][2]);
    set_parser_ana_w_reg(0x4, 0x4, 0x0, 0x4, &parser_map->PARSER_ANA_W[27][2]);
    set_parser_exc_reg(0x4, 0, &parser_map->PARSER_EXC[27][2]);
//  set_parser_ext_reg(0x0, 0x0, 0x1d, 1, 0x5, &parser_map->PARSER_EXT[27][2]);
    set_parser_key_s_reg(0x1d, 0x3f, &parser_map->PARSER_KEY_S[27][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[27][3]);
    set_parser_ana_s_reg(0x1d, 0x3f, 0x0, &parser_map->PARSER_ANA_S[27][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x8, &parser_map->PARSER_ANA_W[27][3]);
//  set_parser_ext_reg(0x2, 0x4, 0x0, 0, 0x0, &parser_map->PARSER_EXT[27][19]);
//  set_parser_ext_reg(0x2, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[27][3]);
    set_parser_key_s_reg(0x19, 0x3f, &parser_map->PARSER_KEY_S[28][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[28][0]);
    set_parser_ana_s_reg(0x17, 0xbf, 0x603c, &parser_map->PARSER_ANA_S[28][0]);
    set_parser_ana_w_reg(0x0, 0x8, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][0]);
    set_parser_key_s_reg(0x19, 0x3f, &parser_map->PARSER_KEY_S[28][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[28][1]);
    set_parser_ana_s_reg(0x18, 0xbf, 0x0, &parser_map->PARSER_ANA_S[28][1]);
    set_parser_ana_w_reg(0x0, 0x6, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][1]);
    set_parser_key_s_reg(0x97, 0xbf, &parser_map->PARSER_KEY_S[28][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[28][2]);
    set_parser_ana_s_reg(0x1e2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[28][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][2]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[28][2]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][18]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][2]);
    set_parser_key_s_reg(0x97, 0xbf, &parser_map->PARSER_KEY_S[28][3]);
    set_parser_key_w_reg(0x6, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[28][3]);
    set_parser_ana_s_reg(0x1a, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][3]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[28][3]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][19]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][3]);
    set_parser_key_s_reg(0x97, 0xbf, &parser_map->PARSER_KEY_S[28][4]);
    set_parser_key_w_reg(0x11, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[28][4]);
    set_parser_ana_s_reg(0x1b, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][4]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[28][4]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][20]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][4]);
    set_parser_key_s_reg(0x97, 0xbf, &parser_map->PARSER_KEY_S[28][5]);
    set_parser_key_w_reg(0x84, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[28][5]);
    set_parser_ana_s_reg(0x1c, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][5]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[28][5]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][21]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][5]);
    set_parser_key_s_reg(0x98, 0xbf, &parser_map->PARSER_KEY_S[28][6]);
    set_parser_key_w_reg(0x0, 0x0, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[28][6]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[28][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[28][6]);
    set_parser_exc_reg(0x28, 1, &parser_map->PARSER_EXC[28][6]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][22]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][6]);
    set_parser_key_s_reg(0x98, 0xbf, &parser_map->PARSER_KEY_S[28][7]);
    set_parser_key_w_reg(0x600, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[28][7]);
    set_parser_ana_s_reg(0x1a, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[28][7]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[28][7]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][23]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][7]);
    set_parser_key_s_reg(0x98, 0xbf, &parser_map->PARSER_KEY_S[28][8]);
    set_parser_key_w_reg(0x1100, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[28][8]);
    set_parser_ana_s_reg(0x1b, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[28][8]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[28][8]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][24]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][8]);
    set_parser_key_s_reg(0x98, 0xbf, &parser_map->PARSER_KEY_S[28][9]);
    set_parser_key_w_reg(0x8400, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[28][9]);
    set_parser_ana_s_reg(0x1c, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[28][9]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[28][9]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][25]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][9]);
    set_parser_key_s_reg(0x98, 0xbf, &parser_map->PARSER_KEY_S[28][10]);
    set_parser_key_w_reg(0x3b00, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[28][10]);
    set_parser_ana_s_reg(0x1e2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[28][10]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[28][10]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[28][10]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x3, &parser_map->PARSER_EXT[28][10]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x6, &parser_map->PARSER_EXT[28][26]);
    set_parser_key_s_reg(0x1d, 0x3f, &parser_map->PARSER_KEY_S[28][11]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[28][11]);
    set_parser_ana_s_reg(0x1d, 0x3f, 0x0, &parser_map->PARSER_ANA_S[28][11]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x14, &parser_map->PARSER_ANA_W[28][11]);
//  set_parser_ext_reg(0x8, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[28][11]);
//  set_parser_ext_reg(0x2, 0x10, 0x0, 0, 0x0, &parser_map->PARSER_EXT[28][27]);
    set_parser_key_s_reg(0x17, 0xbf, &parser_map->PARSER_KEY_S[29][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[29][0]);
    set_parser_ana_s_reg(0x1e2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[29][0]);
    set_parser_ana_w_reg(0x0, 0x2, 0x0, 0x0, &parser_map->PARSER_ANA_W[29][0]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[29][0]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][0]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][16]);
    set_parser_key_s_reg(0x19d, 0x1ff, &parser_map->PARSER_KEY_S[29][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[29][1]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[29][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[29][1]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[29][1]);
//  set_parser_ext_reg(0x6, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][17]);
//  set_parser_ext_reg(0x4, 0x0, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][1]);
    set_parser_key_s_reg(0x17, 0xbf, &parser_map->PARSER_KEY_S[29][2]);
    set_parser_key_w_reg(0x6, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[29][2]);
    set_parser_ana_s_reg(0x1a, 0x3f, 0x0, &parser_map->PARSER_ANA_S[29][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[29][2]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[29][2]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][18]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][2]);
    set_parser_key_s_reg(0x17, 0xbf, &parser_map->PARSER_KEY_S[29][3]);
    set_parser_key_w_reg(0x11, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[29][3]);
    set_parser_ana_s_reg(0x1b, 0x3f, 0x0, &parser_map->PARSER_ANA_S[29][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[29][3]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[29][3]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][19]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][3]);
    set_parser_key_s_reg(0x17, 0xbf, &parser_map->PARSER_KEY_S[29][4]);
    set_parser_key_w_reg(0x84, 0xff, 0x4000, 0xf000, &parser_map->PARSER_KEY_W[29][4]);
    set_parser_ana_s_reg(0x1c, 0x3f, 0x0, &parser_map->PARSER_ANA_S[29][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[29][4]);
    set_parser_exc_reg(0x14, 0, &parser_map->PARSER_EXC[29][4]);
//  set_parser_ext_reg(0x4, 0xc, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][20]);
//  set_parser_ext_reg(0x6, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][4]);
    set_parser_key_s_reg(0x18, 0xbf, &parser_map->PARSER_KEY_S[29][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[29][5]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[29][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[29][5]);
    set_parser_exc_reg(0x28, 1, &parser_map->PARSER_EXC[29][5]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][21]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][5]);
    set_parser_key_s_reg(0x18, 0xbf, &parser_map->PARSER_KEY_S[29][6]);
    set_parser_key_w_reg(0x600, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[29][6]);
    set_parser_ana_s_reg(0x1a, 0x3f, 0x0, &parser_map->PARSER_ANA_S[29][6]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[29][6]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[29][6]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][22]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][6]);
    set_parser_key_s_reg(0x18, 0xbf, &parser_map->PARSER_KEY_S[29][7]);
    set_parser_key_w_reg(0x1100, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[29][7]);
    set_parser_ana_s_reg(0x1b, 0x3f, 0x0, &parser_map->PARSER_ANA_S[29][7]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[29][7]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[29][7]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][23]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][7]);
    set_parser_key_s_reg(0x18, 0xbf, &parser_map->PARSER_KEY_S[29][8]);
    set_parser_key_w_reg(0x8400, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[29][8]);
    set_parser_ana_s_reg(0x1c, 0x3f, 0x0, &parser_map->PARSER_ANA_S[29][8]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[29][8]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[29][8]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][24]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][8]);
    set_parser_key_s_reg(0x18, 0xbf, &parser_map->PARSER_KEY_S[29][9]);
    set_parser_key_w_reg(0x3b00, 0xff00, 0x6000, 0xf000, &parser_map->PARSER_KEY_W[29][9]);
    set_parser_ana_s_reg(0x1e2, 0x1ff, 0x0, &parser_map->PARSER_ANA_S[29][9]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x28, &parser_map->PARSER_ANA_W[29][9]);
    set_parser_exc_reg(0x28, 0, &parser_map->PARSER_EXC[29][9]);
//  set_parser_ext_reg(0x10, 0x8, 0x0, 0, 0x0, &parser_map->PARSER_EXT[29][25]);
//  set_parser_ext_reg(0x4, 0x0, 0x1e, 1, 0x6, &parser_map->PARSER_EXT[29][9]);
    set_parser_key_s_reg(0x1a, 0x3f, &parser_map->PARSER_KEY_S[30][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[30][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[30][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[30][0]);
    set_parser_exc_reg(0x4, 1, &parser_map->PARSER_EXC[30][0]);
//  set_parser_ext_reg(0x2, 0x0, 0x1f, 1, 0x7, &parser_map->PARSER_EXT[30][0]);
    set_parser_key_s_reg(0x1b, 0x3f, &parser_map->PARSER_KEY_S[30][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[30][1]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[30][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[30][1]);
    set_parser_exc_reg(0x4, 1, &parser_map->PARSER_EXC[30][1]);
//  set_parser_ext_reg(0x2, 0x0, 0x1f, 1, 0x7, &parser_map->PARSER_EXT[30][1]);
    set_parser_key_s_reg(0x1c, 0x3f, &parser_map->PARSER_KEY_S[30][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[30][2]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[30][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[30][2]);
    set_parser_exc_reg(0x4, 1, &parser_map->PARSER_EXC[30][2]);
//  set_parser_ext_reg(0x2, 0x0, 0x1f, 1, 0x7, &parser_map->PARSER_EXT[30][2]);
    set_parser_key_s_reg(0x1e2, 0x1ff, &parser_map->PARSER_KEY_S[30][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[30][3]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[30][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[30][3]);
    set_parser_exc_reg(0x0, 1, &parser_map->PARSER_EXC[30][3]);
//  set_parser_ext_reg(0x2, 0x0, 0x1f, 1, 0x7, &parser_map->PARSER_EXT[30][3]);
    set_parser_key_s_reg(0x0, 0x0, &parser_map->PARSER_KEY_S[31][0]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[31][0]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[31][0]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[31][0]);
    set_parser_key_s_reg(0x62, 0x1ff, &parser_map->PARSER_KEY_S[31][1]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[31][1]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[31][1]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[31][1]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x1, &parser_map->PARSER_EXT[31][1]);
    set_parser_key_s_reg(0xa2, 0x1ff, &parser_map->PARSER_KEY_S[31][2]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[31][2]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[31][2]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[31][2]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x2, &parser_map->PARSER_EXT[31][2]);
    set_parser_key_s_reg(0x122, 0x1ff, &parser_map->PARSER_KEY_S[31][3]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[31][3]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[31][3]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[31][3]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x4, &parser_map->PARSER_EXT[31][3]);
    set_parser_key_s_reg(0x162, 0x1ff, &parser_map->PARSER_KEY_S[31][4]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[31][4]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[31][4]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[31][4]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x5, &parser_map->PARSER_EXT[31][4]);
    set_parser_key_s_reg(0x1a2, 0x1ff, &parser_map->PARSER_KEY_S[31][5]);
    set_parser_key_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_KEY_W[31][5]);
    set_parser_ana_s_reg(0x0, 0x0, 0x0, &parser_map->PARSER_ANA_S[31][5]);
    set_parser_ana_w_reg(0x0, 0x0, 0x0, 0x0, &parser_map->PARSER_ANA_W[31][5]);
//  set_parser_ext_reg(0x0, 0x0, 0x0, 0, 0x6, &parser_map->PARSER_EXT[31][5]);
#endif

}

void mby_init_common_regs
(
    mby_ppe_rx_top_map * const rx_top_map
)
{
    init_parser_regs
    (
        &(rx_top_map->parser)
    );

    for (fm_uint i = 0; i < 64; i++) {
        glort_cam_r * glort_cam = &(rx_top_map->mst_glort.GLORT_CAM[i]);
        glort_cam->KEY        = 0xffff;
        glort_cam->KEY_INVERT = 0xffff;
    }

    /* Port GLORT allocation. */
    glort_cam_r * glort_cam = &(rx_top_map->mst_glort.GLORT_CAM[0]);
    glort_cam->KEY                = 0x0100;
    glort_cam->KEY_INVERT         = 0xFEE0;

    glort_ram_r * glort_ram = &(rx_top_map->mst_glort.GLORT_RAM[0]);
    glort_ram->STRICT             = 0x3;
    glort_ram->DEST_INDEX         = 0x0;
    glort_ram->RANGE_SUB_INDEX_A  = 0x50;
    glort_ram->RANGE_SUB_INDEX_B  = 0x0;
    glort_ram->DEST_COUNT         = 0x0;
    glort_ram->HASH_ROTATION      = 0x0;
    glort_ram->SKIP_DGLORT_DEC    = 0x0;

    /* Flood GLORT allocation. */
    glort_cam             = &(rx_top_map->mst_glort.GLORT_CAM[1]);
    glort_cam->KEY        = 0x0400;
    glort_cam->KEY_INVERT = 0xfb00;

    glort_ram                    = &(rx_top_map->mst_glort.GLORT_RAM[1]);
    glort_ram->STRICT            = 0x3;
    glort_ram->DEST_INDEX        = 0x14;
    glort_ram->RANGE_SUB_INDEX_A = 0x40;
    glort_ram->RANGE_SUB_INDEX_B = 0x80;
    glort_ram->DEST_COUNT        = 0x0;
    glort_ram->HASH_ROTATION     = 0x0;
    glort_ram->SKIP_DGLORT_DEC   = 0x0;

    glort_direct_map_dst0_r * const map_dst0 = &(rx_top_map->mst_glort.GLORT_DIRECT_MAP_DST0);
    glort_direct_map_dst4_r * const map_dst4 = &(rx_top_map->mst_glort.GLORT_DIRECT_MAP_DST4);
    map_dst4->IP_MULTICAST_INDEX             = 0x1;
    map_dst0->DEST_MASK                      = 0x1fffff;

    flood_glort_table_r * const flood_glort_table = &(rx_top_map->nexthop.FLOOD_GLORT_TABLE[0]);
    flood_glort_table->BROADCAST_GLORT       = 0x0400;
    flood_glort_table->FLOOD_MULTICAST_GLORT = 0x0400;
    flood_glort_table->FLOOD_UNICAST_GLORT   = 0x0400;

    for (fm_uint i = 0; i < 18/* Number of ports? */; i++) {
        map_port_default_r * map_port_default = &(rx_top_map->mapper.MAP_PORT_DEFAULT[i][0]);

        map_port_default->TARGET = 0x52;
        map_port_default->VALUE  = 1;

        map_port_default = &(rx_top_map->mapper.MAP_PORT_DEFAULT[i][1]);

        map_port_default->TARGET = 0x0f;
        map_port_default->VALUE  = 1;

        map_port_default = &(rx_top_map->mapper.MAP_PORT_DEFAULT[i][2]);

        map_port_default->TARGET = 0x2c;
        map_port_default->VALUE  = 0;

        map_len_limit_r * const map_len_limit = &(rx_top_map->mapper.MAP_LEN_LIMIT[i]);
        map_len_limit->INR_MPLS_LEN_LIMIT = 0x5;
        map_len_limit->OTR_MPLS_LEN_LIMIT = 0x5;
        map_len_limit->INR_L2_LEN_LIMIT   = 0x2;
        map_len_limit->OTR_L2_LEN_LIMIT   = 0x2;

        map_port_cfg_r * const map_port_cfg = &(rx_top_map->mapper.MAP_PORT_CFG[i]);
        map_port_cfg->PORT_PROFILE      = 0x0;
        map_port_cfg->DEFAULT_SGLORT_EN = 0x1;
        map_port_cfg->DEFAULT_SGLORT    = 0x100 + i;
    }


    /* Init some defaults for now <-- REVISIT!!! */

    for (fm_uint i = 0; i < 18/* Number of ports? */; i++) {
        fwd_port_cfg_1_r * const port_cfg_1 = &(rx_top_map->fwd_misc.FWD_PORT_CFG_1[i]);

        port_cfg_1->LEARNING_ENABLE     = 0x1;
        port_cfg_1->FILTER_VLAN_INGRESS = 0x1;
        port_cfg_1->DESTINATION_MASK    = 0x3ffff;
    }

    for (fm_uint i = 0; i < 256; i++) {
        fwd_port_cfg_2_r * const port_cfg_2 = &(rx_top_map->fwd_misc.FWD_PORT_CFG_2[i]);

        port_cfg_2->DESTINATION_MASK = 0x3ffff;
    }

    for (fm_uint i = 0; i < 64; i++) {
        em_hash_cfg_r * const em_a_hash_cfg = &(rx_top_map->cgrp_a.EM.HASH_CFG[i]);
        em_hash_cfg_r * const em_b_hash_cfg = &(rx_top_map->cgrp_b.EM.HASH_CFG[i]);

        em_a_hash_cfg->ENTRY_SIZE_0 = 0;
        em_a_hash_cfg->ENTRY_SIZE_1 = 0;
        em_b_hash_cfg->ENTRY_SIZE_0 = 0;
        em_b_hash_cfg->ENTRY_SIZE_1 = 0;
    }

    for (fm_uint i = 0; i < 32; i++) {
        cm_apply_loopback_suppress_r * const lpbk_sup =
            &(rx_top_map->cm_apply.CM_APPLY_LOOPBACK_SUPPRESS[i]);

        lpbk_sup->GLORT_MASK = 0x0;
        lpbk_sup->GLORT      = 0xFFFF;
    }

    for (fm_uint i = 0; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
        for (fm_uint j = 0; j < MBY_FFU_TCAM_ENTRIES_0; j++)
        {
            wcm_tcam_r * wcm_tcam_entry    = &(rx_top_map->cgrp_b.B.WCM_TCAM[i][j]);
            wcm_tcam_entry->KEY            = 0xffffffff;
            wcm_tcam_entry->KEY_INVERT     = 0xffffffff;
            wcm_tcam_entry->KEY_TOP        = 0xff;
            wcm_tcam_entry->KEY_TOP_INVERT = 0xff;
        }

}
