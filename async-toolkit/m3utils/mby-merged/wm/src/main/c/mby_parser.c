// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_parser.h"

// Get 16-bit word at given index in the segment data buffer
static inline fm_uint16 getSegDataWord(fm_byte index, fm_uint32 adj_seg_len,
                                       fm_byte seg_data[MBY_PA_MAX_SEG_LEN])
{
    fm_uint16 value = 0;
    if ( ((fm_uint32) index) < (adj_seg_len - 1) )
        value = ((((fm_uint16) seg_data[index    ]) << 8) & 0xff00) |
                ((((fm_uint16) seg_data[index + 1])     ) & 0x00ff) ;
    return value;
}

// Calculate generic L4 checksum (UDP/TCP)
static fm_uint16 calcGenericChksum(fm_byte *buf, fm_uint16 len)
{
    fm_uint32 chksum = 0;
    for (fm_uint k = 0; k < len; k += 2) {
        if ((k + 1) == len)
            chksum += ((((fm_uint16) buf[k  ]) << 8) & 0xff00) ;
        else
            chksum += ((((fm_uint16) buf[k  ]) << 8) & 0xff00) |
                      ((((fm_uint16) buf[k+1])     ) & 0x00ff) ;
    }
    fm_uint16 chksum_hi = ((chksum >> 16) & 0xffff);
    fm_uint16 chksum_lo = ((chksum      ) & 0xffff);
    chksum = chksum_hi + chksum_lo;

    // check if there was a carry value generated
    chksum_hi = ((chksum >> 16) & 0xffff);
    chksum_lo = ((chksum      ) & 0xffff);
    if (chksum_hi > 0)
        chksum = chksum_hi + chksum_lo;

    chksum = ~chksum;
    chksum_lo = ((chksum      ) & 0xffff);
    fm_uint16 result = chksum_lo;
    return result;
}

// Verify IPv4 header checksum value
static fm_bool checkIPv4Chksum(fm_byte seg_data[MBY_PA_MAX_SEG_LEN],
                               fm_uint32 p_beg, fm_uint32 p_end)
{
    fm_byte  *buf = seg_data + p_beg;
    fm_uint16 len = (p_end > p_beg) ? (p_end - p_beg + 1) : 0;
    fm_uint32 chksum = calcGenericChksum(buf, len);
    fm_bool   chksum_ok = ((chksum & 0xffff) > 0);
    return chksum_ok;
}

static void lookUpPtypeTcam
(
    parser_ptype_tcam_r const tcam[MBY_PA_PTYPE_ENTRIES],
    parser_ptype_ram_r  const ram [MBY_PA_PTYPE_ENTRIES],
          fm_uint32     const flags,
          fm_uint16   * const ptype,
          fm_byte     * const extract_idx
)
{
    // No match
    *ptype       = 0;
    *extract_idx = 0;

    // The highest numbered PARSER_PTYPE_TCAM entry has highest precedence
    for (fm_int i = (MBY_PA_PTYPE_ENTRIES  - 1); i >= 0; i--)
    {
        fm_uint32 mask = tcam[i].KEY ^ tcam[i].KEY_INVERT;

        if ((tcam[i].KEY & tcam[i].KEY_INVERT) == 0 &&
            (flags & mask) == (tcam[i].KEY & mask)) {
            *ptype       = ram[i].PTYPE;
            *extract_idx = ram[i].EXTRACT_IDX;
            break;
        }
    }
}

// Parse the incoming packet and extracts useful fields from it
void Parser
(
    mby_ppe_parser_map      * const parser_map,
    mbyRxMacToParser  const * const in,
    mbyParserToMapper       * const out
)
{
    // Read inputs:
    fm_byte   const * const rx_data_in = in->RX_DATA;
    fm_uint32 const         rx_length  = in->RX_LENGTH;
    fm_uint32 const         rx_port    = in->RX_PORT;

    // Initialize:
    fm_byte * const rx_packet = (fm_byte *) rx_data_in;

    // On initial entry to the parser block, read in the inital pointer, analyzer state, ALU op,
    // and word offsets from the MBY_PARSER_PORT_CFG register file:
    parser_port_cfg_r const * const port_cfg = &(parser_map->PARSER_PORT_CFG[rx_port]);

    fm_byte   init_ptr       = port_cfg->INITIAL_PTR;
    fm_uint16 ana_state      = port_cfg->INITIAL_STATE;
    fm_uint16 op_mask        = port_cfg->INITIAL_OP_MASK;
    fm_byte   op_rot         = port_cfg->INITIAL_OP_ROT;
    fm_byte   init_w0_offset = port_cfg->INITIAL_W0_OFFSET;
    fm_byte   init_w1_offset = port_cfg->INITIAL_W1_OFFSET;
    fm_byte   init_w2_offset = port_cfg->INITIAL_W2_OFFSET;

    fm_byte cur_ptr = init_ptr; // current pointer

    // Initialize:
    fm_byte   pa_ex_stage        = 0;
    fm_bool   pa_ex_depth_exceed = FALSE;
    fm_bool   pa_ex_trunc_header = FALSE;
    fm_bool   pa_ex_parsing_done = FALSE;

    fm_uint16 pa_keys        [MBY_N_PARSER_KEYS] = { 0     };
    fm_bool   pa_keys_valid  [MBY_N_PARSER_KEYS] = { FALSE };
    fm_bool   pa_flags       [MBY_N_PARSER_FLGS] = { FALSE };
    fm_byte   pa_offset      [MBY_N_PARSER_PTRS] = { 0     };
    fm_bool   pa_offset_valid[MBY_N_PARSER_PTRS] = { FALSE };
    fm_byte   pa_prot_id     [MBY_N_PARSER_PTRS] = { MBY_PA_PROT_ID_NOP };
    fm_byte   hit_idx        [MBY_PA_ANA_STAGES] = { 0     };
    fm_bool   hit_idx_v      [MBY_PA_ANA_STAGES] = { FALSE };

    // Calculate end-of-packet (EOP) and adjusted segment length (adj_seg_len):
    fm_uint32 pa_adj_seg_len = 0;
    fm_bool   eop = 0;

    if (rx_length < 4) { // don't remove fcs if less than 4 bytes
        pa_adj_seg_len = rx_length;
        eop = 1;
    } else if ((rx_length - 4) > MBY_PA_MAX_SEG_LEN) { // if full seg, then set len to max & eop to 0
        pa_adj_seg_len = MBY_PA_MAX_SEG_LEN;
        eop = 0;
    } else {
        pa_adj_seg_len = (rx_length - 4);
        eop = 1;
    }

    // Read in segment data:
    fm_byte seg_data[MBY_PA_MAX_SEG_LEN];
    for (fm_uint i = 0; i < MBY_PA_MAX_SEG_LEN; i++)
        seg_data[i] = (i < rx_length) ? rx_data_in[i] : 0;

    fm_uint16 w0 = getSegDataWord(init_w0_offset, pa_adj_seg_len, seg_data);
    fm_uint16 w1 = getSegDataWord(init_w1_offset, pa_adj_seg_len, seg_data);
    fm_uint16 w2 = getSegDataWord(init_w2_offset, pa_adj_seg_len, seg_data);

    fm_byte ptr [MBY_PA_ANA_STAGES];

    // Carry out analyzer actions (see corresponding section in functional spec):
    for (fm_uint s = 0; s < MBY_PA_ANA_STAGES; s++)
    {
        fm_bool rule_matched = FALSE;

        for (fm_uint r = 0; r < MBY_PA_ANA_RULES; r++)
        {
            parser_key_w_r const * const key_w = &(parser_map->PARSER_KEY_W[s][r]);
            parser_key_s_r const * const key_s = &(parser_map->PARSER_KEY_S[s][r]);

            fm_uint16 w0_mask        = key_w->W0_MASK;
            fm_uint16 w0_val         = key_w->W0_VALUE;
            fm_uint16 w1_mask        = key_w->W1_MASK;
            fm_uint16 w1_val         = key_w->W1_VALUE;

            fm_uint16 ana_state_mask = key_s->STATE_MASK;
            fm_uint16 ana_state_val  = key_s->STATE_VALUE;
            // CAM Matching:
            if ( ( (w0 & w0_mask) == w0_val ) &&
                 ( (w1 & w1_mask) == w1_val ) &&
                 ( (ana_state & ana_state_mask) == ana_state_val) )
            {
                hit_idx  [s] = ((fm_byte) r) & 0x1F;
                hit_idx_v[s] = 1;
                rule_matched = TRUE;
            }

        } // for r ...

        //  Store pointer before updating:
        ptr[s] = cur_ptr;

        // Update if a rule matched this stage, else pass unchanged to next stage:
        if (rule_matched)
        {
            // Get analyzer fields:
            fm_int    r_hit = hit_idx[s];
            parser_ana_w_r const * const ana_w = &(parser_map->PARSER_ANA_W[s][r_hit]);
            parser_ana_s_r const * const ana_s = &(parser_map->PARSER_ANA_S[s][r_hit]);

            fm_byte   skip                = ana_w->SKIP;
            fm_byte   next_w0_offset      = ana_w->NEXT_W0_OFFSET;
            fm_byte   next_w1_offset      = ana_w->NEXT_W1_OFFSET;
            fm_byte   next_w2_offset      = ana_w->NEXT_W2_OFFSET;

            fm_uint16 next_ana_state      = ana_s->NEXT_STATE;
            fm_uint16 next_ana_state_mask = ana_s->NEXT_STATE_MASK;
            fm_uint16 next_op             = ana_s->NEXT_OP;
            fm_uint32 tmp_op_result = ((((fm_uint32) w2) << 16) & 0xffff0000) |
                                      ((((fm_uint32) w2)      ) & 0x0000ffff) ;

            op_mask &= MBY_PA_ANA_OP_MASK_BITS;
            op_rot  &= MBY_PA_ANA_OP_ROT_BITS;

            fm_uint16 op_result = (fm_uint16) ( (tmp_op_result >> op_rot) & ((fm_uint32) op_mask) );
            cur_ptr += op_result;
            if (cur_ptr >= MBY_PA_MAX_PTR_LEN)
                cur_ptr = MBY_PA_MAX_PTR_LEN;

            // Update w0..w2 fields for next stage:
            w0 = getSegDataWord(cur_ptr + next_w0_offset, pa_adj_seg_len, seg_data);
            w1 = getSegDataWord(cur_ptr + next_w1_offset, pa_adj_seg_len, seg_data);
            w2 = getSegDataWord(cur_ptr + next_w2_offset, pa_adj_seg_len, seg_data);

            // Update state for next stage:
            ana_state = (ana_state & ~next_ana_state_mask) | (next_ana_state & next_ana_state_mask);

            // Update ALU op for next stage:
            op_rot  = (next_op >> MBY_PA_ANA_OP_ROT_SHIFT) & MBY_PA_ANA_OP_ROT_BITS;
            op_mask =  next_op & MBY_PA_ANA_OP_MASK_BITS;

            // Update pointer for next stage:
            if ((cur_ptr  + skip) >= MBY_PA_MAX_PTR_LEN)
                cur_ptr = MBY_PA_MAX_PTR_LEN;
            else
                cur_ptr += skip;
        }

    } // for s ...

    fm_bool s_ena = TRUE; // initially enable

    // Carry out exception and extract actions (see correcponding sections in functional spec):
    for (fm_uint s = 0; s < MBY_PA_ANA_STAGES; s++)
    {
        // If stage is enabled, and have valid hit, then continue:
        if (s_ena && (hit_idx_v[s] == 1))
        {
            // Exception action:
            fm_int r_hit = hit_idx[s];
            parser_exc_r const * const exc = &(parser_map->PARSER_EXC[s][r_hit]);

            fm_byte xa_ex_offset    = exc->EX_OFFSET;
            fm_bool xa_parsing_done = exc->PARSING_DONE;
            fm_bool eof_exc         = (pa_adj_seg_len < (ptr[s] + xa_ex_offset)); // a.k.a. EOS

            if (eof_exc) // end-of-file exception
             {
                s_ena = FALSE; // disable further processing
                pa_ex_stage = s & 0x1F; // 5 bits
                if (eop == 1)
                    pa_ex_trunc_header = TRUE;
                else
                    pa_ex_depth_exceed = TRUE;
            }

            // Extraction action:
            for (fm_uint wd = 0; wd < 2; wd++)
            {
                fm_int r_hit_ex = r_hit + (wd * MBY_PA_ANA_RULES);
                parser_ext_r const * const ext = &(parser_map->PARSER_EXT[s][r_hit_ex]);

                fm_byte protocol_id = ext->PROTOCOL_ID;
                fm_byte offset      = ext->OFFSET;
                fm_byte flag_num    = ext->FLAG_NUM;
                fm_bool flag_val    = ext->FLAG_VALUE;
                fm_byte ptr_num     = ext->PTR_NUM;

                if ((flag_num != 0) && (flag_num < MBY_N_PARSER_FLGS))
                    pa_flags[flag_num] = flag_val;

                if ( (protocol_id != MBY_PA_PROT_ID_NOP) && (ptr_num < MBY_N_PARSER_PTRS) ) {
                    pa_offset      [ptr_num] = ptr[s] + offset;
                    pa_offset_valid[ptr_num] = TRUE;
                    pa_prot_id     [ptr_num] = protocol_id;
                }
            } // for wd ...

            if (xa_parsing_done == 1) {
                s_ena              = FALSE;
                pa_ex_stage        = s & 0x1F; // 5 bits
                pa_ex_parsing_done = TRUE;
            }

        } // if s_ena ...

    } // for s ...

    fm_uint32 pa_packet_flags = 0;
    fm_uint16 pa_packet_type  = 0;
    fm_byte   pa_extract_idx  = 0;

    for (fm_int i = 0; i < 32; i++)
        pa_packet_flags |= ((pa_flags[i]) << i);

    /* @todo: should we support index of 1 for PARSER_PTYPE_CAM/RAM ? */
    lookUpPtypeTcam(parser_map->PARSER_PTYPE_TCAM[0],
                    parser_map->PARSER_PTYPE_RAM[0],
                    pa_packet_flags,
                    &pa_packet_type,
                    &pa_extract_idx);

    for (fm_uint i = 0; i < MBY_N_PARSER_KEYS; i++)
    {
        parser_extract_cfg_r const * const extract_cfg = &(parser_map->PARSER_EXTRACT_CFG[pa_extract_idx][i]);
        fm_byte pa_protocol_id = extract_cfg->PROTOCOL_ID;

        if (pa_protocol_id != MBY_PA_PROT_ID_NOP)
        {
            for (fm_uint j = 0; j < MBY_N_PARSER_PTRS; j++)
            {
                if (pa_prot_id[j] == pa_protocol_id)
                {
                    fm_byte offset = extract_cfg->OFFSET + pa_offset[j];
                    w0 = getSegDataWord(offset, pa_adj_seg_len, seg_data);

                    pa_keys[i] = w0;
                    break;
                }
            }
        }
    }

    // Perform checksum offloads & validations (see corresponding section in the func. spec):
    fm_byte pa_csum_ok = 0;

    for (fm_uint p = 0; p <= 1; p++)
    {
        fm_byte pr = 2 + (p * 4); // 4 bytes / ptr
        fm_byte p0 = pa_offset[pr  ];
        fm_byte p1 = pa_offset[pr+1] - 1;

        // Validate checksum:
        if (pa_offset_valid[pr] && pa_offset_valid[pr+1]) {
            fm_bool csum_chk_result = checkIPv4Chksum(seg_data, p0, p1);
            if ( csum_chk_result && ((p1 - p0) <= 64) && (((fm_uint32) (p1 + 1)) <= rx_length))
                pa_csum_ok |= (1 << p);  // ok -> set bit p
        }
    } // for p ...

    // Fetch flags and pointers:
    fm_byte otr_l3_ptr = pa_offset[MBY_OTR_L3_PTR];
    fm_bool otr_l3_v   = pa_flags[MBY_PA_OTR_L3_V_FLAG];

    // TODO Variable 'is_ipv4' is assigned a value that is never used.
    fm_bool is_ipv4  = otr_l3_v &&  pa_keys_valid[MBY_OTR_IPHDR_KEY];
    fm_bool is_ipv6  = otr_l3_v && !pa_keys_valid[MBY_OTR_IPHDR_KEY];

    fm_byte ip_len   = (is_ipv6) ?  4 : 1; // IPv6 Length field (2 empty keys precede IPv6 hdr)
    fm_byte n_keys   = (is_ipv6) ? 16 : 4; // number of outer IP addr keys

    fm_bool l3_vld_chk = pa_offset_valid[MBY_OTR_L3_PTR] && pa_keys_valid[MBY_OTR_IPHDR_KEY + ip_len];
    for (fm_uint i = 0; i < n_keys; i++)
        l3_vld_chk &= pa_keys_valid[MBY_OTR_IPADDR_KEY + i];

    // Read checksum configuration registers:
    parser_csum_cfg_r const * const csum_cfg = &(parser_map->PARSER_CSUM_CFG[rx_port]);

    fm_bool val_l3_len = csum_cfg->VALIDATE_L3_LENGTH;

    fm_uint16 l3_len   = pa_keys[MBY_OTR_IPHDR_KEY + ip_len];

    // Clear flags:
    fm_bool pa_l3len_err = FALSE;
    fm_bool pa_drop      = FALSE;

    // Validate packet length:
    if (val_l3_len && l3_vld_chk)
    {
        fm_uint32 min_pkt_len = l3_len + otr_l3_ptr + 4 + ((is_ipv6) ? MBY_PSEUDOHEADER_SIZE : 0);

        if (rx_length < min_pkt_len)
        {
            pa_l3len_err = TRUE;
            if (val_l3_len)
                pa_drop = TRUE;
        }
    }

    // Write outputs:
    out->PA_ADJ_SEG_LEN     = pa_adj_seg_len;
    out->PA_CSUM_OK         = pa_csum_ok;
    out->PA_DROP            = pa_drop;
    out->PA_EX_DEPTH_EXCEED = pa_ex_depth_exceed;
    out->PA_EX_PARSING_DONE = pa_ex_parsing_done;
    out->PA_EX_STAGE        = pa_ex_stage;
    out->PA_EX_TRUNC_HEADER = pa_ex_trunc_header;

    for (fm_uint i = 0; i < MBY_N_PARSER_FLGS; i++)
        out->PA_FLAGS[i] = pa_flags[i];

    for (fm_uint i = 0; i < MBY_N_PARSER_KEYS; i++) {
        out->PA_KEYS      [i] = pa_keys      [i];
        out->PA_KEYS_VALID[i] = pa_keys_valid[i];
    }

    out->PA_L3LEN_ERR       = pa_l3len_err;
    out->PA_PACKET_TYPE     = pa_packet_type;

    for (fm_uint i = 0; i < MBY_N_PARSER_PTRS; i++) {
        out->PA_HDR_PTRS.OFFSET      [i] = pa_offset      [i];
        out->PA_HDR_PTRS.OFFSET_VALID[i] = pa_offset_valid[i];
        out->PA_HDR_PTRS.PROT_ID     [i] = pa_prot_id     [i];
    }

    out->RX_DATA            = rx_packet;
    out->RX_PORT            = rx_port;
    out->RX_LENGTH          = rx_length;
}
