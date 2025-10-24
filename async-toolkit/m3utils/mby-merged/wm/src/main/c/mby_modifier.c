/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation
#include "mby_modifier.h"

#include <assert.h>
#include "mby_crc32.h"
#include "mby_parser.h"

static void initProfAct
(
    mby_shm_map         const * const shm_map,
    fm_uint32                   rx_length,
    fm_uint32                   content_addr,
    mbyModProfileAction * const prof_act
)
{
    prof_act->operating_region     = (rx_length <= MBY_MOD_OPERATING_REGION_MAX) ?
                                        rx_length : MBY_MOD_OPERATING_REGION_MAX;
    prof_act->fld_vector.cur_idx   = 0;
    prof_act->content_ctnr.cur_idx = 0;

    /* Read content data. Content address is in 32B blocks, FWD_TABLE_MOD.DATA is 8B. */
    fm_uint idx_1       = (content_addr * (MBY_MOD_CONTENT_ADDR_BLOCK_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE)) /
                          fwd_table_mod_rf_FWD_TABLE_MOD__nd;
    fm_uint idx_2       = (content_addr * (MBY_MOD_CONTENT_ADDR_BLOCK_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE)) %
                          fwd_table_mod_rf_FWD_TABLE_MOD__nd;
    fm_uint content_idx = 0;

    for (fm_uint i = 0; i < (MBY_MOD_CONTENT_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE); i++)
    {
        for (fm_uint j = 0; j < MBY_MOD_CONTENT_ENTRY_SIZE; j++)
            prof_act->content_ctnr.content[content_idx++] =
                FM_GET_UNNAMED_FIELD(shm_map->FWD_TABLE_MOD[idx_1][idx_2].DATA, j * 8, 8);

        idx_2++;
        if (idx_2 == fwd_table_mod_rf_FWD_TABLE_MOD__nd)
        {
            idx_1++;
            idx_2 = 0;
        }
    }
}

static mbyModProfileGroup getProfileGroup
(
    mby_ppe_modify_map const * const mod_map,
    fm_byte                          mod_prof_idx)
{
    mbyModProfileGroup prof_grp;

    prof_grp.group[0] = 0; // The 1st group is always treated as starting at packet offset zero.
    prof_grp.group[1] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_1;
    prof_grp.group[2] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_2;
    prof_grp.group[3] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_3;
    prof_grp.group[4] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_4;
    prof_grp.group[5] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_5;
    prof_grp.group[6] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_6;
    prof_grp.group[7] = mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_7;

    return prof_grp;
}

static mbyModProfileField getProfileField
(
    mby_ppe_modify_map const * const mod_map,
    fm_byte                          mod_prof_idx
)
{
    mbyModProfileField prof_fld;

    for (fm_uint i = 0; i < (MBY_MOD_FIELD_VECTOR_SIZE / MBY_MOD_FIELDS_PER_REG_ENTRY); i++)
    {
        prof_fld.protocol_id[i * 3 + 0] = mod_map->MOD_PROFILE_FIELD[mod_prof_idx][i].PROTOCOL_ID_0;
        prof_fld.offset     [i * 3 + 0] = mod_map->MOD_PROFILE_FIELD[mod_prof_idx][i].OFFSET_0;
        prof_fld.protocol_id[i * 3 + 1] = mod_map->MOD_PROFILE_FIELD[mod_prof_idx][i].PROTOCOL_ID_1;
        prof_fld.offset     [i * 3 + 1] = mod_map->MOD_PROFILE_FIELD[mod_prof_idx][i].OFFSET_1;
        prof_fld.protocol_id[i * 3 + 2] = mod_map->MOD_PROFILE_FIELD[mod_prof_idx][i].PROTOCOL_ID_2;
        prof_fld.offset     [i * 3 + 2] = mod_map->MOD_PROFILE_FIELD[mod_prof_idx][i].OFFSET_2;
    }

    return prof_fld;
}

static mbyModProfileCmd getProfileCommand
(
    mby_ppe_modify_map const * const mod_map,
    fm_byte                          mod_prof_idx
)
{
    mbyModProfileCmd prof_cmd;

    for (fm_uint i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
    {
        prof_cmd.cmd[i][0] = mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][i * 2    ].CMD_0;
        prof_cmd.cmd[i][1] = mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][i * 2    ].CMD_1;
        prof_cmd.cmd[i][2] = mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][i * 2 + 1].CMD_0;
        prof_cmd.cmd[i][3] = mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][i * 2 + 1].CMD_1;
    }

    return prof_cmd;
}

static void extractFields
(
    varchar_t          const * const rx_data,
    mbyParserHdrPtrs   const * const pa_hdr_ptrs,
    mbyModProfileField const * const prof_fld,
    fm_uint                          operating_region,
    mbyModFieldVector        * const fld_vector
)
{
    for (fm_uint i = 0; i < MBY_MOD_FIELD_VECTOR_SIZE; i++)
    {
        fld_vector->field[i] = 0;

        if (prof_fld->protocol_id[i] < MBY_MOD_PROT_ID_MD_TYPE)
        {
            for (fm_uint j = 0; j < MBY_N_PARSER_PTRS; j++)
            {
                if ((pa_hdr_ptrs->PROT_ID[j] != MBY_PA_PROT_ID_NOP) &
                    (pa_hdr_ptrs->PROT_ID[j] == prof_fld->protocol_id[i]))
                {
                    fm_uint idx = pa_hdr_ptrs->OFFSET[j] + prof_fld->offset[i];
                    if(idx < operating_region) {
                      fld_vector->field[i] = varchar_get(rx_data,idx);
                        break;
                    }
                }
            }
        }
        /* Metadata/Telemetry fields should be handled in else clause <--REVISIT!!! */
    }
}

static void decodeCommand
(
    fm_uint32            cmd,
    mbyModDecCmd * const dec_cmd
)
{
    mbyModCmdType cmd_type = FM_GET_FIELD64(cmd, MBY_MOD_CMD, TYPE);
    switch (cmd_type)
    {
    case MBY_MOD_CMD_TYPE_NOP:
        dec_cmd->type = MBY_MOD_CMD_TYPE_NOP;
        break;
    case MBY_MOD_CMD_TYPE_INSERT:
        dec_cmd->type = MBY_MOD_CMD_TYPE_INSERT;
        dec_cmd->field.insrt.len                = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LEN_MASK_8B);
        dec_cmd->field.insrt.prot_id            = FM_GET_FIELD64(cmd, MBY_MOD_CMD, PROT_ID);
        dec_cmd->field.insrt.source             = FM_GET_BIT64  (cmd, MBY_MOD_CMD, S);
        dec_cmd->field.insrt.mult_hdr_insrt     = FM_GET_BIT64  (cmd, MBY_MOD_CMD, M);
        dec_cmd->field.insrt.mode               = FM_GET_FIELD64(cmd, MBY_MOD_CMD, MODE);
        break;
    case MBY_MOD_CMD_TYPE_INSERT_FIELD:
        dec_cmd->type = MBY_MOD_CMD_TYPE_INSERT_FIELD;
        dec_cmd->field.insrt_fld.len_mask       = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LEN_MASK_16B);
        dec_cmd->field.insrt_fld.shift          = FM_GET_FIELD64(cmd, MBY_MOD_CMD, SHIFT);
        dec_cmd->field.insrt_fld.mode           = FM_GET_FIELD64(cmd, MBY_MOD_CMD, MODE);
        break;
    case MBY_MOD_CMD_TYPE_INSERT_FIELD_LUT:
        dec_cmd->type = MBY_MOD_CMD_TYPE_INSERT_FIELD_LUT;
        dec_cmd->field.insrt_fld_lut.lut_mode   = FM_GET_BIT64  (cmd, MBY_MOD_CMD, LUT_MODE);
        dec_cmd->field.insrt_fld_lut.lut        = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LUT);
        dec_cmd->field.insrt_fld_lut.mode       = FM_GET_FIELD64(cmd, MBY_MOD_CMD, MODE);
        break;
    case MBY_MOD_CMD_TYPE_DELETE:
        dec_cmd->type = MBY_MOD_CMD_TYPE_DELETE;
        dec_cmd->field.del.prot_del             = FM_GET_BIT64  (cmd, MBY_MOD_CMD, PROT_DEL);
        break;
    case MBY_MOD_CMD_TYPE_REPLACE:
        dec_cmd->type = MBY_MOD_CMD_TYPE_REPLACE;
        dec_cmd->field.replace.len_mask         = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LEN_MASK_8B);
        dec_cmd->field.replace.offset           = FM_GET_FIELD64(cmd, MBY_MOD_CMD, OFFSET);
        dec_cmd->field.replace.align            = FM_GET_BIT64  (cmd, MBY_MOD_CMD, ALIGNMENT);
        dec_cmd->field.replace.mode             = FM_GET_FIELD64(cmd, MBY_MOD_CMD, MODE);
        break;
    case MBY_MOD_CMD_TYPE_REPLACE_FIELD:
        dec_cmd->type = MBY_MOD_CMD_TYPE_REPLACE_FIELD;
        dec_cmd->field.replace_fld.len_mask     = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LEN_MASK_8B);
        dec_cmd->field.replace_fld.offset       = FM_GET_FIELD64(cmd, MBY_MOD_CMD, OFFSET);
        dec_cmd->field.replace_fld.align        = FM_GET_BIT64  (cmd, MBY_MOD_CMD, ALIGNMENT);
        dec_cmd->field.replace_fld.mode         = FM_GET_FIELD64(cmd, MBY_MOD_CMD, MODE);
        break;
    case MBY_MOD_CMD_TYPE_REPLACE_FIELD_LUT:
        dec_cmd->type = MBY_MOD_CMD_TYPE_REPLACE_FIELD_LUT;
        dec_cmd->field.replace_fld_lut.mask     = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LEN_MASK_8B);
        dec_cmd->field.replace_fld_lut.offset   = FM_GET_FIELD64(cmd, MBY_MOD_CMD, OFFSET);
        dec_cmd->field.replace_fld_lut.lut_mode = FM_GET_BIT64  (cmd, MBY_MOD_CMD, LUT_MODE);
        dec_cmd->field.replace_fld_lut.lut      = FM_GET_FIELD64(cmd, MBY_MOD_CMD, LUT);
        dec_cmd->field.replace_fld.mode         = FM_GET_FIELD64(cmd, MBY_MOD_CMD, MODE);
        break;
    default:
        /* Command type not recognized. */
        dec_cmd->type = MBY_MOD_CMD_TYPE_NOP;
        break;
    }
}

static void lookupProfile
(
    mby_ppe_modify_map  const * const mod_map,
    varchar_t           const * const rx_data,
    mbyParserHdrPtrs    const * const pa_hdr_ptrs,
    fm_byte                           mod_prof_idx,
    mbyModProfileAction       * const prof_act
)
{
    mbyModProfileField prof_fld;
    mbyModProfileCmd   prof_cmd;

    prof_act->profile_grp = getProfileGroup(mod_map, mod_prof_idx);
    prof_fld              = getProfileField(mod_map, mod_prof_idx);
    prof_cmd              = getProfileCommand(mod_map, mod_prof_idx);

    extractFields(rx_data, pa_hdr_ptrs, &prof_fld, prof_act->operating_region, &prof_act->fld_vector);

    for (fm_uint grp = 0; grp < MBY_MOD_PROFILE_GROUPS; grp++)
    {
        for (fm_uint idx = 0; idx < MBY_MOD_COMMAND_PER_GROUP; idx++)
        {
            decodeCommand(prof_cmd.cmd[grp][idx], &(prof_act->dec_cmd[grp][idx]));
        }
    }
}

static void decodeProfileGroups
(
    const mbyTxInToModifier  * const in,
    mbyModProfileGroup * const prof_grp,
    mbyModGroupConfig  * const grp_list
)
{
    /* Group 0 is always treated as starting at packet offset zero. */
    fm_uint grp_idx = 0;

    grp_list[grp_idx++].valid = TRUE;

    for (int grp = 1; grp < MBY_MOD_PROFILE_GROUPS; grp++)
    {
        /* If CONFIG bit is 0, use offset associated with protocol ID.
         * If CONFIG bit is 1, offset is relative to the previous group.
         */
        fm_bool use_prot_id = !FM_GET_BIT(prof_grp->group[grp], MBY_MOD_PROFILE_GROUP, CONFIG);

        if (use_prot_id)
        {
            fm_byte prot_id = FM_GET_FIELD64(prof_grp->group[grp], MBY_MOD_PROFILE_GROUP, PROT_ID);
            if(prot_id != MBY_PA_PROT_ID_NOP)
            {
                for (fm_int pa_hdr_idx = 0; pa_hdr_idx < MBY_N_PARSER_PTRS; pa_hdr_idx++)
                {
                    if (in->PA_HDR_PTRS.PROT_ID[pa_hdr_idx] == prot_id)
                    {
                        grp_list[grp_idx  ].valid          = TRUE;
                        grp_list[grp_idx  ].dem_by_prot_id = TRUE;
                        grp_list[grp_idx  ].grp_idx        = grp;
                        grp_list[grp_idx  ].pa_hdr_idx     = pa_hdr_idx;
                        grp_list[grp_idx++].pkt_offset     = in->PA_HDR_PTRS.OFFSET[pa_hdr_idx];

                        break;
                    }
                }
            }
        }
        else
        {
            assert(grp_idx > 0);

            fm_byte offset = FM_GET_FIELD64(prof_grp->group[grp], MBY_MOD_PROFILE_GROUP, OFFSET);

            grp_list[grp_idx  ].valid      = TRUE;
            grp_list[grp_idx  ].pkt_offset = grp_list[grp_idx - 1].pkt_offset + offset;
            grp_list[grp_idx  ].grp_idx    = grp;
            grp_list[grp_idx++].pa_hdr_idx = -1;
        }
    }
}

static void setProfileGroupSizes
(
    fm_uint32                 operating_region,
    mbyModGroupConfig * const grp_list
)
{
    fm_uint ctnr_offset = 0;
    fm_int i;

    for (i = 0; i < (MBY_MOD_PROFILE_GROUPS - 1); i++)
    {
        if (grp_list[i].valid)
        {
            /* Allocate output container offsets and sizes. */
            if (grp_list[i + 1].valid)
            {
                grp_list[i].pkt_size  = grp_list[i + 1].pkt_offset - grp_list[i].pkt_offset;
                grp_list[i].ctnr_size = grp_list[i].pkt_size;
            }
            else
            {
                grp_list[i].pkt_size  = operating_region - grp_list[i].pkt_offset;
                grp_list[i].ctnr_size = grp_list[i].pkt_size;
            }

            grp_list[i].ctnr_offset = ctnr_offset;
            ctnr_offset            += grp_list[i].ctnr_size;
        }
        else
        {
            break;
        }
    }

    /* Set for last group. */
    if (grp_list[i].valid)
    {
        grp_list[i].pkt_size  = operating_region - grp_list[i].pkt_offset;
        grp_list[i].ctnr_size = grp_list[i].pkt_size;
    }
}

static void checkGroupMonotonic(mbyModGroupConfig * const grp_list)
{
    fm_uint prev_pkt_offset  = 0;
    fm_uint prev_grp_idx     = 0;
    fm_uint prev_ctnr_offset = 0;

    for (int i = 0 ; i < MBY_MOD_PROFILE_GROUPS ; i++)
    {
        if (grp_list[i].valid)
        {
            if (i == 0)
            {
                assert(grp_list[i].grp_idx     == prev_grp_idx);
                assert(grp_list[i].pkt_offset  == prev_pkt_offset);
                assert(grp_list[i].ctnr_offset == prev_ctnr_offset);
            }
            else
            {
                assert(grp_list[i].grp_idx     >  prev_grp_idx);
                assert(grp_list[i].pkt_offset  >= prev_pkt_offset); // whether a group size can be 0
                assert(grp_list[i].ctnr_offset >= prev_ctnr_offset);
            }
        }
        prev_pkt_offset  = grp_list[i].pkt_offset;
        prev_grp_idx     = grp_list[i].grp_idx;
        prev_ctnr_offset = grp_list[i].ctnr_offset;
    }
}

static void adjustGrpCtnrOffsets
(
    fm_uint                   idx,
    mbyModGroupConfig * const grp_list,
    fm_int                    len
)
{
    for (int i = idx + 1 ; i < MBY_MOD_PROFILE_GROUPS ; i++)
    {
        if (grp_list[i].valid)
            grp_list[i].ctnr_offset += len;
        else
            break;
    }
}

static void initContainer
(
    mbyModProfileAction       * const prof_act,
    varchar_t           const * const rx_data,
    fm_byte                   * const grp_ctnr
)
{
    /* Set all offsets .*/
    fm_int insert_len_sum = 0;

    for (int i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
    {
        if (prof_act->grp_list[i].valid)
        {
            mbyModGroupConfig * const grp = &(prof_act->grp_list[i]);
            fm_uint grp_idx = grp->grp_idx;

            for (int cmd_idx = 0; cmd_idx < MBY_MOD_COMMAND_PER_GROUP; cmd_idx++)
            {
                fm_int insert_len = 0;
                mbyModCmdType cmd_type = prof_act->dec_cmd[grp_idx][cmd_idx].type;

                if (cmd_type == MBY_MOD_CMD_TYPE_INSERT)
                {
                    mbyModCmdInsert * const cmd = &prof_act->dec_cmd[grp_idx][cmd_idx].field.insrt;

                    if ((cmd->mode == MBY_MOD_CMD_MODE_BASIC) || (cmd->mode == MBY_MOD_CMD_MODE_ZEROES))
                        insert_len = cmd->len;

                    grp->ctnr_size  += insert_len;
                    adjustGrpCtnrOffsets(grp_idx, prof_act->grp_list, insert_len);
                }
                else if (cmd_type == MBY_MOD_CMD_TYPE_INSERT_FIELD)
                {
                    mbyModCmdInsertFld * const cmd = &prof_act->dec_cmd[grp_idx][cmd_idx].field.insrt_fld;

                    if (cmd->mode == MBY_MOD_CMD_MODE_BASIC)
                    {
                        insert_len = cmd->len_mask;
                    }
                    else if (cmd->mode == MBY_MOD_CMD_MODE_1B_2B_DECREMENT)
                    {
                        fm_byte pos   = FM_GET_FIELD64(cmd->len_mask, MBY_MOD_CMD_INS_FLD_MASK, POS);
                        fm_byte len   = FM_GET_FIELD64(cmd->len_mask, MBY_MOD_CMD_INS_FLD_MASK, LEN);
                        fm_bool is_1B = (pos + len <= MBY_MOD_CMD_FLD_1B_OPERATION) ? TRUE : FALSE;
                        insert_len    = (is_1B) ? 1 : 2;
                    }
                    else if (cmd->mode == MBY_MOD_CMD_MODE_2B_COMB_INSERT)
                    {
                        insert_len = 2;
                    }

                    grp->ctnr_size  += insert_len;
                    adjustGrpCtnrOffsets(grp_idx, prof_act->grp_list, insert_len);
                }
                else if (cmd_type == MBY_MOD_CMD_TYPE_INSERT_FIELD_LUT)
                {
                    mbyModCmdInsertFldLut * const cmd = &prof_act->dec_cmd[grp_idx][cmd_idx].field.insrt_fld_lut;

                    if (cmd->mode == MBY_MOD_CMD_MODE_BASIC)
                        insert_len = MBY_MOD_MAP_INS_FLD_LUT_LEN;
                    else if  (cmd->mode == MBY_MOD_CMD_MODE_2B_COMB_INSERT)
                        insert_len = MBY_MOD_MAP_INS_FLD_LUT_LEN;

                    grp->ctnr_size  += insert_len;
                    adjustGrpCtnrOffsets(grp_idx, prof_act->grp_list, insert_len);
                }
                else if (cmd_type == MBY_MOD_CMD_TYPE_DELETE)
                {
                    fm_uint delete_len = (grp->pkt_size > MBY_MOD_OPERATION_SIZE_MAX) ? 0 : grp->pkt_size;
                    grp->ctnr_size -= delete_len;
                    adjustGrpCtnrOffsets(grp_idx, prof_act->grp_list, -(delete_len));
                }

                insert_len_sum += insert_len;
            }
        }
    }

    assert(insert_len_sum <= MBY_MOD_OPERATION_SIZE_MAX);

    /* Build up the internal container with rx data and 0s padding. */
    for (int i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
    {
        if ((prof_act->grp_list[i].valid) && (prof_act->grp_list[i].ctnr_size > 0))
        {
            mbyModGroupConfig * const grp = &(prof_act->grp_list[i]);

            fm_uint offset = grp->ctnr_offset;
            /* If insert happens. */
            if (grp->ctnr_size > grp->pkt_size)
            {
                for (fm_uint j = 0; j < grp->ctnr_size - grp->pkt_size; j++)
                    grp_ctnr[offset++] = 0; /* pad 0s */
            }

            for (fm_uint j = 0; j < grp->pkt_size; j++)
              grp_ctnr[offset++] = varchar_get(rx_data, grp->pkt_offset + j);
        }
    }
}

static void buildProfileGroupList
(
    mbyTxInToModifier   const * const in,
    mbyModProfileAction       * const prof_act
)
{
    mbyModGroupConfig grp_list[MBY_MOD_PROFILE_GROUPS];

    for (fm_int i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
    {
        grp_list[i].valid          = FALSE;
        grp_list[i].dem_by_prot_id = FALSE;
        grp_list[i].grp_idx        = 0;
        grp_list[i].pkt_offset     = 0;
        grp_list[i].pkt_size       = 0;
        grp_list[i].ctnr_offset    = 0;
        grp_list[i].ctnr_size      = 0;
        grp_list[i].grp_offset     = 0;
        grp_list[i].pa_hdr_idx     = 0;
    }

    decodeProfileGroups(in, &(prof_act->profile_grp), grp_list);

    setProfileGroupSizes(prof_act->operating_region, grp_list);

    checkGroupMonotonic(grp_list);

    for (fm_int i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
        prof_act->grp_list[i] = grp_list[i];
}

static void copyFromTo
(
    fm_byte const * const from,
    fm_uint               from_offset,
    fm_byte       * const to,
    fm_uint               to_offset,
    fm_byte               bitmask,
    fm_uint               len,
    fm_int                assertlen
)
{
    assert(assertlen == -1
           ||
           ( (fm_int)(from_offset + len) >= 0 &&
             (fm_int)(from_offset + len) <= assertlen) );
    for (fm_uint i = 0; i < len; i++)
        to[to_offset + i] = (to[to_offset + i] & (~bitmask)) | (from[from_offset + i] & bitmask);
}

void updateCtnrIdx(fm_uint * idx, fm_uint delta, fm_uint boundary)
{
    if ((*idx + delta) < boundary)
        *idx += delta;
}

static fm_uint16 lookupModMap
(
    mby_ppe_modify_map const * const mod_map,
    fm_byte                          lut,
    mbyModCmdLutMode                 lm,
    mbyModFieldVector        * const fld_vector
)
{
    fm_uint16  value = 0;

    if (lut <= MBY_MOD_MAP_MAX_SINGLE_LUT)
    {
        fm_byte idx_mask_len     = mod_map->MOD_MAP_CFG[lut].IDX_MASK_LEN;
        fm_byte idx_shift_right  = mod_map->MOD_MAP_CFG[lut].IDX_SHIFT_RIGHT;
        fm_byte base_mask_len    = mod_map->MOD_MAP_CFG[lut].BASE_MASK_LEN;
        fm_byte base_shift_right = mod_map->MOD_MAP_CFG[lut].BASE_SHIFT_RIGHT;
        fm_uint16 output_mask    = mod_map->MOD_MAP_CFG[lut].OUTPUT_MASK;
        fm_byte output_shift     = mod_map->MOD_MAP_CFG[lut].OUTPUT_SHIFT;
        fm_uint16 idx            = 0;
        fm_uint16 base           = 0;

        if (lm == MBY_MOD_CMD_LUT_MODE_DIRECT)
        {
            idx = (fld_vector->field[fld_vector->cur_idx] << 8) | fld_vector->field[fld_vector->cur_idx + 1];
            updateCtnrIdx(&(fld_vector->cur_idx), 2, MBY_MOD_FIELD_VECTOR_SIZE - 1);

            idx = idx >> idx_shift_right;
            idx = idx & ((1 << idx_mask_len) - 1);
            idx = idx & 0x7ff; /* MOD_MAP reg is 2K entries */
        }
        else
        {
            idx  = (fld_vector->field[fld_vector->cur_idx    ] << 8) | fld_vector->field[fld_vector->cur_idx + 1];
            base = (fld_vector->field[fld_vector->cur_idx + 2] << 8) | fld_vector->field[fld_vector->cur_idx + 3];
            updateCtnrIdx(&(fld_vector->cur_idx), 4, MBY_MOD_FIELD_VECTOR_SIZE - 1);

            idx  = idx >> idx_shift_right;
            idx  = idx & ((1 << idx_mask_len) - 1);
            base = base >> base_shift_right;
            base = base & ((1 << base_mask_len) - 1);
            idx = (idx + base) & 0x7ff;  /* MOD_MAP reg is 2K entries */
        }

        switch (idx % MBY_MOD_MAP_VALUES_PER_ENTRY) {
        case 0:
            value = mod_map->MOD_MAP[lut][idx / MBY_MOD_MAP_VALUES_PER_ENTRY].VALUE0;
            break;
        case 1:
            value = mod_map->MOD_MAP[lut][idx / MBY_MOD_MAP_VALUES_PER_ENTRY].VALUE1;
            break;
        case 2:
            value = mod_map->MOD_MAP[lut][idx / MBY_MOD_MAP_VALUES_PER_ENTRY].VALUE2;
            break;
        case 3:
            value = mod_map->MOD_MAP[lut][idx / MBY_MOD_MAP_VALUES_PER_ENTRY].VALUE3;
            break;
        }

        value = value << output_shift;
        value = value & output_mask;
    }
    else if (lut == MBY_MOD_MAP_DUAL_LUT)
    {
        fm_byte idx0_mask_len    = mod_map->MOD_MAP_DUAL_CFG.IDX0_MASK_LEN;
        fm_byte idx0_shift_right = mod_map->MOD_MAP_DUAL_CFG.IDX0_SHIFT_RIGHT;
        fm_byte idx1_mask_len    = mod_map->MOD_MAP_DUAL_CFG.IDX1_MASK_LEN;
        fm_byte idx1_shift_right = mod_map->MOD_MAP_DUAL_CFG.IDX1_SHIFT_RIGHT;
        fm_uint16 output_mask    = mod_map->MOD_MAP_DUAL_CFG.OUTPUT_MASK;
        fm_byte output_shift     = mod_map->MOD_MAP_DUAL_CFG.OUTPUT_SHIFT;

        fm_uint16 idx0 = (fld_vector->field[fld_vector->cur_idx] << 8) | fld_vector->field[fld_vector->cur_idx + 1];
        fm_uint16 idx1 = fld_vector->field[fld_vector->cur_idx + 2];
        updateCtnrIdx(&(fld_vector->cur_idx), 3, MBY_MOD_FIELD_VECTOR_SIZE - 1);

        idx0 = idx0 >> idx0_shift_right;
        idx0 = idx0 & ((1 << idx0_mask_len) - 1);
        idx0 = idx0 & 0x3ff;  /* MOD_MAP_DUAL reg is 1K entries */
        idx1 = idx1 >> idx1_shift_right;
        idx1 = idx1 & ((1 << idx1_shift_right) - 1);

        switch (idx1 % MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY)
        {
        case 0:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE0;
            break;
        case 1:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE1;
            break;
        case 2:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE2;
            break;
        case 3:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE3;
            break;
        case 4:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE4;
            break;
        case 5:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE5;
            break;
        case 6:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE6;
            break;
        case 7:
            value = mod_map->MOD_MAP_DUAL[idx0][idx1 / MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY].VALUE7;
            break;
        }

        value = value << output_shift;
        value = value & output_mask;
    }

    return value;
}

static fm_uint16 getContainerOffset(mbyModGroupConfig * const grp,
                                    fm_byte                   offset,
                                    mbyModCmdAlignment        alignment)
{
    fm_uint16 result = 0;

    if (alignment == MBY_MOD_CMD_ALIGN_TOP)
        result = grp->ctnr_offset + offset + (grp->ctnr_size - grp->pkt_size);
    else if (alignment == MBY_MOD_CMD_ALIGN_BOTTOM)
        result = grp->ctnr_size + grp->ctnr_offset - offset - 1;

    return result;
}

static void performInsert(mbyParserHdrPtrs       * const pa_hdr_ptrs,
                          mbyModCmdInsert        * const cmd,
                          mbyModGroupConfig      * const grp_list,
                          fm_int                         grp_list_idx,
                          fm_byte                * const grp_ctnr,
                          mbyModContentContainer * const content_ctnr,
                          mbyModFieldVector      * const fld_vector)
{
    mbyModGroupConfig * const grp = &(grp_list[grp_list_idx]);

    /* Read (protocol ID, Offset) pairs. */
    fm_byte number_of_tuples = 0;
    fm_byte prot_id[MBY_N_PARSER_PTRS] = { 0 };
    fm_byte offset [MBY_N_PARSER_PTRS] = { 0 };

    if (cmd->mult_hdr_insrt)
    {
        number_of_tuples = (cmd->prot_id <= MBY_N_PARSER_PTRS) ? cmd->prot_id : 0;

        for (fm_int i = 0; i < number_of_tuples; i++)
        {
            if (cmd->source == MBY_MOD_CMD_SOURCE_CONTENT_REGION)
            {
                prot_id[i] = content_ctnr->content[content_ctnr->cur_idx    ];
                offset [i] = content_ctnr->content[content_ctnr->cur_idx + 1];
                updateCtnrIdx(&content_ctnr->cur_idx, 2, MBY_MOD_CONTENT_SIZE - 1);
            }
            else if (cmd->source == MBY_MOD_CMD_SOURCE_FIELD_CONTAINER)
            {
                prot_id[i] = fld_vector->field[fld_vector->cur_idx    ];
                offset [i] = fld_vector->field[fld_vector->cur_idx + 1];
                updateCtnrIdx(&fld_vector->cur_idx, 2, MBY_MOD_FIELD_VECTOR_SIZE - 1);
            }
        }
    }
    else if (cmd->prot_id != MBY_PA_PROT_ID_NOP)
    {
        number_of_tuples = 1;
        prot_id[0]       = cmd->prot_id;
        offset [0]       = grp->ctnr_offset + grp->grp_offset;
    }

    /* Perform insert. */
    fm_uint insert_len = 0;
    if (cmd->mode == MBY_MOD_CMD_MODE_BASIC)
    {
        insert_len = cmd->len;

        fm_byte mask = 0xff;
        if (cmd->source == MBY_MOD_CMD_SOURCE_CONTENT_REGION)
        {
          copyFromTo(content_ctnr->content, content_ctnr->cur_idx, grp_ctnr, grp->ctnr_offset + grp->grp_offset, mask, insert_len, -1);
            updateCtnrIdx(&content_ctnr->cur_idx, insert_len, MBY_MOD_CONTENT_SIZE - 1);
        }
        else if (cmd->source == MBY_MOD_CMD_SOURCE_FIELD_CONTAINER)
        {
          copyFromTo(fld_vector->field, fld_vector->cur_idx, grp_ctnr, grp->ctnr_offset + grp->grp_offset, mask, insert_len, -1);
            updateCtnrIdx(&fld_vector->cur_idx, insert_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);
        }
    }
    else if (cmd->mode == MBY_MOD_CMD_MODE_ZEROES)
    {
        insert_len = cmd->len;
        /* Container is already padded with 0s in initContainer function. */
    }

    /* Insert/Adjust protocol IDs. */
    fm_int cur_hdr_idx = grp->pa_hdr_idx;

    if (number_of_tuples)
    {
        /* Adjust header pointers offsets. */
        for (fm_int j = cur_hdr_idx; j <(MBY_N_PARSER_PTRS -number_of_tuples); j++)
        {
            if (pa_hdr_ptrs->PROT_ID[j] != MBY_PA_PROT_ID_NOP)
            {
                if ((pa_hdr_ptrs->OFFSET[j] + insert_len) < MBY_MOD_CTNR_SIZE)
                {
                    pa_hdr_ptrs->OFFSET[j] += insert_len;
                }
                else
                {
                    /* Invalidate pointer as offset is beyond container size. */
                    pa_hdr_ptrs->PROT_ID     [j] = MBY_PA_PROT_ID_NOP;
                    pa_hdr_ptrs->OFFSET_VALID[j] = FALSE;
                    pa_hdr_ptrs->OFFSET      [j] = 0;
                }
            }
        }

        /* Insert new headers. */
        for (fm_int i = 0; i < number_of_tuples; i++)
        {
            for (fm_int j = (MBY_N_PARSER_PTRS - number_of_tuples); j > cur_hdr_idx; j--)
            {
                if (pa_hdr_ptrs->PROT_ID[j - 1] != MBY_PA_PROT_ID_NOP)
                {
                    /* Adjust header pointers. */
                    pa_hdr_ptrs->PROT_ID     [j]  = pa_hdr_ptrs->PROT_ID     [j - 1];
                    pa_hdr_ptrs->OFFSET_VALID[j]  = pa_hdr_ptrs->OFFSET_VALID[j - 1];
                    pa_hdr_ptrs->OFFSET      [j]  = pa_hdr_ptrs->OFFSET      [j - 1];
                }
            }

            /* Insert new PROT ID. */
            pa_hdr_ptrs->PROT_ID[cur_hdr_idx] = prot_id[i];
            pa_hdr_ptrs->OFFSET [cur_hdr_idx] = offset [i];
            cur_hdr_idx++;
        }
    }
    else if (grp->dem_by_prot_id)
    {
        /* Adjust offsets. */
        for (fm_int i = cur_hdr_idx; i < MBY_N_PARSER_PTRS; i++)
            if (pa_hdr_ptrs->PROT_ID[i] != MBY_PA_PROT_ID_NOP)
                pa_hdr_ptrs->OFFSET[i] += insert_len;
    }

    grp->grp_offset += insert_len;
}

static void performInsertField(mbyParserHdrPtrs       * const pa_hdr_ptrs,
                               mbyModCmdInsertFld     * const cmd,
                               mbyModGroupConfig      * const grp_list,
                               fm_int                         grp_list_idx,
                               fm_byte                * const grp_ctnr,
                               mbyModContentContainer * const content_ctnr,
                               mbyModFieldVector      * const fld_vector)
{
    mbyModGroupConfig * const grp = &(grp_list[grp_list_idx]);

    fm_uint insert_len = 0;

    /* Variable names come from spec. */
    fm_uint16 C = 0;
    fm_uint16 F = 0;
    fm_uint16 H = 0;

    switch (cmd->mode)
    {
    case MBY_MOD_CMD_MODE_BASIC:
        insert_len = cmd->len_mask;

        fm_byte mask = 0xff;
        copyFromTo(fld_vector->field, fld_vector->cur_idx, grp_ctnr, grp->ctnr_offset + grp->grp_offset, mask, insert_len, -1);

        updateCtnrIdx(&fld_vector->cur_idx, insert_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

        break;
    case MBY_MOD_CMD_MODE_1B_2B_DECREMENT:
        insert_len = 1;

        fm_byte pos   = FM_GET_FIELD64(cmd->len_mask, MBY_MOD_CMD_INS_FLD_MASK, POS);
        fm_byte len   = FM_GET_FIELD64(cmd->len_mask, MBY_MOD_CMD_INS_FLD_MASK, LEN);
        fm_bool is_1B = (pos + len <= MBY_MOD_CMD_FLD_1B_OPERATION) ? TRUE : FALSE;

        if (is_1B)
        {
            C  = content_ctnr->content[content_ctnr->cur_idx];
            F  = fld_vector->field[fld_vector->cur_idx];

            updateCtnrIdx(&content_ctnr->cur_idx, insert_len, MBY_MOD_CONTENT_SIZE - 1);
            updateCtnrIdx(&fld_vector->cur_idx, insert_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

            fm_uint16 M = (1 << len) - 1;
            fm_uint16 x = (F >> pos) & M;
            x--;
            H = C | (x << pos);

            grp_ctnr[grp->ctnr_offset + grp->grp_offset] = H & 0xff;
        }
        else
        {
            insert_len = 2;

            C = (content_ctnr->content[content_ctnr->cur_idx] << 8) |
                 content_ctnr->content[content_ctnr->cur_idx + 1];
            F = (fld_vector->field[fld_vector->cur_idx] << 8) |
                 fld_vector->field[fld_vector->cur_idx + 1];

            updateCtnrIdx(&content_ctnr->cur_idx, insert_len, MBY_MOD_CONTENT_SIZE - 1);
            updateCtnrIdx(&fld_vector->cur_idx, insert_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

            fm_uint16 M = (1 << len) - 1;
            fm_uint16 x = (F >> pos) & M;
            x--;
            fm_uint16 H = C | (x << pos);

            grp_ctnr[grp->ctnr_offset + grp->grp_offset    ] = (H >> 8) & 0xff;
            grp_ctnr[grp->ctnr_offset + grp->grp_offset + 1] = H & 0xff;
        }

        break;
    case MBY_MOD_CMD_MODE_2B_COMB_INSERT:
        insert_len = 2;

         C = (content_ctnr->content[content_ctnr->cur_idx] << 8) |
              content_ctnr->content[content_ctnr->cur_idx + 1];
        F = (fld_vector->field[fld_vector->cur_idx] << 8) |
             fld_vector->field[fld_vector->cur_idx + 1];

        updateCtnrIdx(&content_ctnr->cur_idx, insert_len, MBY_MOD_CONTENT_SIZE - 1);
        updateCtnrIdx(&fld_vector->cur_idx, insert_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

        switch (cmd->shift)
        {
        case MBY_MOD_CMD_INS_FLD_SHIFT_NONE:
            break;
        case MBY_MOD_CMD_INS_FLD_SHIFT_2_LEFT:
            F = F << 2;
            break;
        case MBY_MOD_CMD_INS_FLD_SHIFT_2_RIGHT:
            F = F >> 2;
            break;
        case MBY_MOD_CMD_INS_FLD_SHIFT_4_RIGHT:
            F = F >> 4;
            break;
        default:
            break;
        }

        fm_uint16 R = (C & ~cmd->len_mask) | (F & cmd->len_mask);

        grp_ctnr[grp->ctnr_offset + grp->grp_offset    ] = (R >> 8) & 0xff;
        grp_ctnr[grp->ctnr_offset + grp->grp_offset + 1] = R & 0xff;

        break;
    case MBY_MOD_CMD_MODE_1B_INSERT:
        insert_len = 1;

        C  = content_ctnr->content[content_ctnr->cur_idx];
        F  = fld_vector->field[fld_vector->cur_idx];

        updateCtnrIdx(&content_ctnr->cur_idx, insert_len, MBY_MOD_CONTENT_SIZE - 1);
        updateCtnrIdx(&fld_vector->cur_idx, insert_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

        H = (C & ~cmd->len_mask) | (F & cmd->len_mask);
        grp_ctnr[grp->ctnr_offset + grp->grp_offset] = H & 0xff;
        break;
    default:
        break;
    }

    grp->grp_offset += insert_len;

    /* Adjust offsets. */
    fm_int pa_hdr_idx = grp->pa_hdr_idx;

    for (fm_int i = pa_hdr_idx + 1; i < MBY_N_PARSER_PTRS; i++)
        if (pa_hdr_ptrs->PROT_ID[i] != MBY_PA_PROT_ID_NOP)
            pa_hdr_ptrs->OFFSET[i] += insert_len;
}

static void performInsertFieldLut(mby_ppe_modify_map     const * const mod_map,
                                  mbyParserHdrPtrs             * const pa_hdr_ptrs,
                                  mbyModCmdInsertFldLut        * const cmd,
                                  mbyModGroupConfig            * const grp_list,
                                  fm_int                               grp_list_idx,
                                  fm_byte                      * const grp_ctnr,
                                  mbyModContentContainer       * const content_ctnr,
                                  mbyModFieldVector            * const fld_vector)
{
    mbyModGroupConfig * const grp = &(grp_list[grp_list_idx]);

    fm_uint insert_len = 0;

    fm_uint16 value = lookupModMap(mod_map, cmd->lut, cmd->lut_mode, fld_vector);

    if (cmd->mode == MBY_MOD_CMD_MODE_BASIC)
    {
        insert_len = MBY_MOD_MAP_INS_FLD_LUT_LEN;

        grp_ctnr[grp->ctnr_offset + grp->grp_offset]     = (value >> 8) & 0xff;
        grp_ctnr[grp->ctnr_offset + grp->grp_offset + 1] = value & 0xff;
    }
    else if  (cmd->mode == MBY_MOD_CMD_MODE_2B_COMB_INSERT)
    {
        insert_len = MBY_MOD_MAP_INS_FLD_LUT_LEN;

        fm_uint16 C = (content_ctnr->content[content_ctnr->cur_idx] << 8) |
                       content_ctnr->content[content_ctnr->cur_idx + 1];

        updateCtnrIdx(&content_ctnr->cur_idx, insert_len, MBY_MOD_CONTENT_SIZE - 1);

        fm_uint16 R = C | value;

        grp_ctnr[grp->ctnr_offset + grp->grp_offset]     = (R >> 8) & 0xff;
        grp_ctnr[grp->ctnr_offset + grp->grp_offset + 1] = R & 0xff;

    }

    grp->grp_offset += insert_len;

    /* Adjust offsets. */
    fm_int pa_hdr_idx = grp->pa_hdr_idx;

    for (fm_int i = pa_hdr_idx + 1; i < MBY_N_PARSER_PTRS; i++)
        if (pa_hdr_ptrs->PROT_ID[i] != MBY_PA_PROT_ID_NOP)
            pa_hdr_ptrs->OFFSET[i] += insert_len;
}

static void performDelete(mbyParserHdrPtrs  * const pa_hdr_ptrs,
                          mbyModCmdDelete   * const cmd,
                          mbyModGroupConfig * const grp_list,
                          fm_int                    grp_idx)
{
    mbyModGroupConfig * const grp = &(grp_list[grp_idx]);

    /* If deletion exceed MBY_MOD_OPERATION_SIZE_MAX, it is considered as mis-configuration. */
    fm_uint delete_len = (grp->pkt_size > MBY_MOD_OPERATION_SIZE_MAX) ? 0 : grp->pkt_size;

    /* Delete/Adjust protocol ID. */
    fm_int pa_hdr_idx = grp->pa_hdr_idx;

    if (cmd->prot_del)
    {
        fm_int i;
        for (i = pa_hdr_idx; i < MBY_N_PARSER_PTRS - 1; i++)
        {
            pa_hdr_ptrs->PROT_ID     [i]  = pa_hdr_ptrs->PROT_ID     [i + 1];
            pa_hdr_ptrs->OFFSET_VALID[i]  = pa_hdr_ptrs->OFFSET_VALID[i + 1];
            pa_hdr_ptrs->OFFSET      [i]  = pa_hdr_ptrs->OFFSET      [i + 1];
            /* In case offsets are not properly set. */
            if (pa_hdr_ptrs->OFFSET[i] > 0)
                pa_hdr_ptrs->OFFSET[i] -= delete_len;
        }

        pa_hdr_ptrs->PROT_ID[MBY_N_PARSER_PTRS] = MBY_PA_PROT_ID_NOP;
    }
    else
    {
        for (fm_int i = pa_hdr_idx + 1; i < MBY_N_PARSER_PTRS; i++)
            if (pa_hdr_ptrs->PROT_ID[i] != MBY_PA_PROT_ID_NOP)
                pa_hdr_ptrs->OFFSET[i] -= delete_len;
    }
}

static void performReplace(mbyModCmdReplace       * const cmd,
                           mbyModGroupConfig      * const grp,
                           fm_byte                * const grp_ctnr,
                           mbyModContentContainer * const content_ctnr)
{
    assert(grp->pkt_size != 0);

    fm_byte ctnr_offset = getContainerOffset(grp, cmd->offset, cmd->align);
    fm_byte replace_len = 0;
    fm_byte bitmask     = 0;
    fm_bool skip_copy   = FALSE;

    switch (cmd->mode)
    {
    case MBY_MOD_CMD_MODE_BASIC:
        replace_len = cmd->len_mask;
        bitmask     = 0xff;
        break;
    case MBY_MOD_CMD_MODE_ZEROES:
        replace_len = cmd->len_mask;

        for (fm_uint i = 0; i < replace_len; i++)
            grp_ctnr[ctnr_offset + i] = 0;

        skip_copy = TRUE;
        break;
    case MBY_MOD_CMD_MODE_1B_REPLACE:
        replace_len = 1;
        bitmask     = cmd->len_mask;
        break;
    default:
        skip_copy = TRUE;
        break;
    }


    if (!skip_copy)
    {
      copyFromTo(content_ctnr->content, content_ctnr->cur_idx, grp_ctnr, ctnr_offset, bitmask, replace_len, -1);

        updateCtnrIdx(&content_ctnr->cur_idx, replace_len, MBY_MOD_CONTENT_SIZE - 1);
    }
}

static void performReplaceField(mbyModCmdReplaceFld * const cmd,
                                mbyModGroupConfig   * const grp,
                                fm_byte             * const grp_ctnr,
                                mbyModFieldVector   * const fld_vector)
{
    assert(grp->pkt_size != 0);

    fm_byte ctnr_offset = getContainerOffset(grp, cmd->offset, cmd->align);
    fm_byte replace_len = 0;
    fm_byte bitmask     = 0;
    fm_bool skip_copy   = FALSE;

    switch (cmd->mode)
    {
    case MBY_MOD_CMD_MODE_BASIC:
        replace_len = cmd->len_mask;
        bitmask     = 0xff;
        break;
    case MBY_MOD_CMD_MODE_1B_REPLACE:
        replace_len = 1;
        bitmask     = cmd->len_mask;
        break;
    case MBY_MOD_CMD_MODE_1B_XOR:
        bitmask   = cmd->len_mask;
        fm_byte F = fld_vector->field[fld_vector->cur_idx];
        fm_byte H = grp_ctnr[ctnr_offset];
        H         = (H & (~bitmask)) | ((H^F) & bitmask);

        updateCtnrIdx(&fld_vector->cur_idx, 1, MBY_MOD_FIELD_VECTOR_SIZE - 1);

        grp_ctnr[ctnr_offset] = H;

        skip_copy = TRUE;
        break;
    case MBY_MOD_CMD_MODE_1B_2B_DECREMENT:
        replace_len = 1;

        fm_byte pos   = FM_GET_FIELD64(cmd->len_mask, MBY_MOD_CMD_INS_FLD_MASK, POS);
        fm_byte len   = FM_GET_FIELD64(cmd->len_mask, MBY_MOD_CMD_INS_FLD_MASK, LEN);
        fm_bool is_1B = (pos + len <= MBY_MOD_CMD_FLD_1B_OPERATION) ? TRUE : FALSE;

        if (is_1B)
        {
            fm_byte F   = fld_vector->field[fld_vector->cur_idx];
            fm_uint16 H = grp_ctnr[ctnr_offset];

            updateCtnrIdx(&fld_vector->cur_idx, replace_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

            fm_uint16 M = (1 << len) - 1;
            fm_uint16 x = (F >> pos) & M;
            x--;
            H = (H & ~(M << pos)) | (x << pos);

            grp_ctnr[grp->ctnr_offset + grp->grp_offset] = H & 0xff;
        }
        else
        {
            replace_len = 2;

            fm_uint16 F = (fld_vector->field[fld_vector->cur_idx] << 8) |
                           fld_vector->field[fld_vector->cur_idx + 1];

            fm_uint16 H = (grp_ctnr[ctnr_offset] << 8) | grp_ctnr[ctnr_offset + 1];

            updateCtnrIdx(&fld_vector->cur_idx, replace_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);

            fm_uint16 M = (1 << len) - 1;
            fm_uint16 x = (F >> pos) & M;
            x--;
            H = (H & ~(M << pos)) | (x << pos);

            grp_ctnr[grp->ctnr_offset + grp->grp_offset    ] = (H >> 8) & 0xff;
            grp_ctnr[grp->ctnr_offset + grp->grp_offset + 1] = H & 0xff;
        }

        skip_copy = TRUE;
        break;
    default:
        skip_copy = TRUE;
        break;
    }

    if (!skip_copy)
    {
      copyFromTo(fld_vector->field, fld_vector->cur_idx, grp_ctnr, ctnr_offset, bitmask, replace_len, -1);

        updateCtnrIdx(&fld_vector->cur_idx, replace_len, MBY_MOD_FIELD_VECTOR_SIZE - 1);
    }
}

static void performReplaceFieldLut(mby_ppe_modify_map     const * const mod_map,
                                   mbyModCmdReplaceFldLut       * const cmd,
                                   mbyModGroupConfig            * const grp,
                                   fm_byte                      * const grp_ctnr,
                                   mbyModFieldVector            * const fld_vector)
{
    assert(grp->pkt_size != 0);

    fm_byte ctnr_offset = getContainerOffset(grp, cmd->offset, MBY_MOD_CMD_ALIGN_TOP);
    fm_byte bitmask     = cmd->mask;

    fm_uint16 value = lookupModMap(mod_map, cmd->lut, cmd->lut_mode, fld_vector);

    if (cmd->mode == MBY_MOD_CMD_MODE_BASIC)
    {
        /* Mask is interpreted as a 16b mask, where a 0b expands to 00b and a 1b expands to 11b. */
        fm_uint16 mask_16 = 0;

        for (fm_uint16 i = 0; i < 8; i++)
        {
            if (bitmask & (1 << i))
                mask_16 |= (0x3 << (i * 2));
        }

        fm_uint16 H = (grp_ctnr[ctnr_offset] << 8) | grp_ctnr[ctnr_offset + 1];
        H = (H & (~mask_16)) | (value & mask_16);

        grp_ctnr[ctnr_offset    ] = (H >> 8) & 0xff;
        grp_ctnr[ctnr_offset + 1] = H & 0xff;
    }
    else if (cmd->mode == MBY_MOD_CMD_MODE_1B_REPLACE)
    {
        fm_uint16 H = grp_ctnr[ctnr_offset];

        value = value & 0xff;
        H     = (H & (~bitmask)) | (value & bitmask);

        grp_ctnr[ctnr_offset] = H;
    }
}

static void performCmds(mby_ppe_modify_map  const * const mod_map,
                        varchar_t           const * const rx_data,
                        mbyModProfileAction       * const prof_act,
                        mbyParserHdrPtrs          * const pa_hdr_ptrs,
                        fm_byte                   * const grp_ctnr)
{
    for (int i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
    {
        if (prof_act->grp_list[i].valid)
        {
            mbyModGroupConfig * const grp = &(prof_act->grp_list[i]);

            fm_uint grp_idx = grp->grp_idx;
            fm_bool deleted = FALSE;

            for (int cmd_idx = 0; cmd_idx < MBY_MOD_COMMAND_PER_GROUP; cmd_idx++)
            {
                switch (prof_act->dec_cmd[grp_idx][cmd_idx].type)
                {
                case MBY_MOD_CMD_TYPE_NOP:
                    /* No operation. */
                    break;
                case MBY_MOD_CMD_TYPE_INSERT:
                    performInsert(pa_hdr_ptrs, &(prof_act->dec_cmd[grp_idx][cmd_idx].field.insrt),
                                  prof_act->grp_list, grp_idx, grp_ctnr, &prof_act->content_ctnr, &prof_act->fld_vector);
                        break;
                case MBY_MOD_CMD_TYPE_INSERT_FIELD:
                    performInsertField(pa_hdr_ptrs, &(prof_act->dec_cmd[grp_idx][cmd_idx].field.insrt_fld),
                                       prof_act->grp_list, grp_idx, grp_ctnr, &prof_act->content_ctnr, &prof_act->fld_vector);
                    break;
                case MBY_MOD_CMD_TYPE_INSERT_FIELD_LUT:
                    performInsertFieldLut(mod_map, pa_hdr_ptrs, &(prof_act->dec_cmd[grp_idx][cmd_idx].field.insrt_fld_lut),
                                          prof_act->grp_list, grp_idx, grp_ctnr, &prof_act->content_ctnr,
                                          &prof_act->fld_vector);
                    break;
                case MBY_MOD_CMD_TYPE_DELETE:
                    if (!deleted)
                    {
                        performDelete(pa_hdr_ptrs, &(prof_act->dec_cmd[grp_idx][cmd_idx].field.del),
                                      prof_act->grp_list, grp_idx);
                        deleted = TRUE;
                    }
                    break;
                case MBY_MOD_CMD_TYPE_REPLACE:
                    if(grp->pkt_size != 0)
                        performReplace(&(prof_act->dec_cmd[grp_idx][cmd_idx].field.replace), grp,
                                       grp_ctnr, &prof_act->content_ctnr);
                    break;
                case MBY_MOD_CMD_TYPE_REPLACE_FIELD:
                    if(grp->pkt_size != 0)
                        performReplaceField(&(prof_act->dec_cmd[grp_idx][cmd_idx].field.replace_fld), grp,
                                            grp_ctnr, &prof_act->fld_vector);
                    break;
                case MBY_MOD_CMD_TYPE_REPLACE_FIELD_LUT:
                    if(grp->pkt_size != 0)
                        performReplaceFieldLut(mod_map, &(prof_act->dec_cmd[grp_idx][cmd_idx].field.replace_fld_lut),
                                               grp, grp_ctnr, &prof_act->fld_vector);
                    break;
                default :
                    /* Nothing to be done here. */
                    break;
                }
            }
        }
    }
}

static void packPacket(mbyModProfileAction     * const prof_act,
                       fm_uint32                       rx_length,
                       fm_byte                 * const grp_ctnr,
                       varchar_t         const * const rx_data,
                       varchar_builder_t       * const tx_data_builder)
{
    for (fm_uint i = 0; i < MBY_MOD_PROFILE_GROUPS; i++)
      if (prof_act->grp_list[i].valid)
        for (fm_uint j = 0; j < prof_act->grp_list[i].ctnr_size; j++)
          {
            fm_uint offset = prof_act->grp_list[i].ctnr_offset;
            varchar_builder_put(tx_data_builder, grp_ctnr[offset + j]);
          }
    
    for (fm_uint i = 0; i < (rx_length - prof_act->operating_region); i++)
      varchar_builder_put(tx_data_builder, varchar_get(rx_data, i + prof_act->operating_region));
}

void Modifier
(
    mby_ppe_modify_map   const * const mod_map,
    mby_shm_map          const * const shm_map,
    varchar_t            const *       rx_data,
    mbyTxInToModifier    const * const in,
    mbyModifierToTxStats       * const out,
    varchar_builder_t          * const tx_data_builder
)
{
    // Read inputs:
    fm_uint32                   content_addr = in->CONTENT_ADDR;
    fm_uint32                   fnmask       = in->FNMASK;
    fm_byte                     mod_prof_idx = in->MOD_PROF_IDX;
    mbyParserHdrPtrs            pa_hdr_ptrs  = in->PA_HDR_PTRS;

    fm_uint32                   rx_length    = rx_data->length;

    fm_byte                     grp_ctnr[MBY_MOD_CTNR_SIZE];
    fm_uint                     pkt_idx   = 0;
    mbyModProfileAction         prof_act;
    fm_uint32                   tx_port   = 0;

    // Select egress port:
    for (fm_uint i = 0; i < 24; i++)
        if (fnmask & (1uL << i)) {
            tx_port = i;
            break;
        }

    initProfAct(shm_map, rx_length, content_addr, &prof_act);

    lookupProfile(mod_map, rx_data, &pa_hdr_ptrs, mod_prof_idx, &prof_act);

    buildProfileGroupList(in, &prof_act);

    initContainer(&prof_act, rx_data, grp_ctnr);

    performCmds(mod_map, rx_data, &prof_act, &pa_hdr_ptrs, grp_ctnr);

    packPacket(&prof_act,
               rx_length,
               grp_ctnr,
               rx_data,
               tx_data_builder);

    // Write outputs:
    out->TX_PORT     = tx_port;
    out->PA_HDR_PTRS = pa_hdr_ptrs;
}
