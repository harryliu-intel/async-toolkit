// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_parser.h"

// Get 16-bit word at given index in the segment data buffer
static inline fm_uint16 getSegDataWord(fm_byte index, fm_uint32 adj_seg_len,
                                      fm_byte seg_data[MBY_PA_MAX_SEG_LEN]) {
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
                               fm_uint32 p_beg, fm_uint32 p_end) {
    fm_byte  *buf = seg_data + p_beg;
    fm_uint16 len = (p_end > p_beg) ? (p_end - p_beg + 1) : 0;
    fm_uint32 chksum = calcGenericChksum(buf, len);
    fm_bool chksum_ok = ((chksum & 0xffff) > 0);
    return chksum_ok; 
}

// Parse the incoming packet and extracts useful fields from it
void Parser
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser    * const in, 
          mbyParserToMapper * const out
)
{
    // On initial entry to the parser block, read in the inital pointer, analyzer state, ALU op,
    // and word offsets from the MBY_PARSER_PORT_CFG register file:
    fm_uint32 pa_port_cfg_vals[MBY_PARSER_PORT_CFG_WIDTH] = { 0 };

    mbyModelReadCSRMult(regs, MBY_PARSER_PORT_CFG(in->RX_PORT, 0), MBY_PARSER_PORT_CFG_WIDTH, pa_port_cfg_vals);

    fm_byte   init_ptr     = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_PTR);
    fm_uint16 ana_state    = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_STATE);
    fm_uint16 op_mask      = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_OP_MASK);
    fm_byte   op_rot       = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_OP_ROT);
    fm_byte init_w0_offset = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_W0_OFFSET);
    fm_byte init_w1_offset = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_W1_OFFSET);
    fm_byte init_w2_offset = FM_ARRAY_GET_FIELD(pa_port_cfg_vals, MBY_PARSER_PORT_CFG, INITIAL_W2_OFFSET);

    fm_byte cur_ptr = init_ptr; // current pointer

    // Pass-thru (some) inputs to outputs:
    out->RX_PORT = in->RX_PORT;

#if 0 // Obsolete <-- REVISIT!!!
    for (fm_uint i = 0; i < MBY_PA_ANA_STAGES; i++)
        out->PKT_META[i] = in->PKT_META[i];
#endif

    // Initialize (some of) the outputs:
    out->PA_EX_DEPTH_EXCEED = 0;
    out->PA_EX_TRUNC_HEADER = 0;
    out->PA_EX_PARSING_DONE = 0;
    out->PA_PACKET_TYPE     = 0;  // added for MBY <-- REVISIT!!!
    
    for (fm_uint i = 0; i < MBY_N_PARSER_KEYS; i++) {
        out->PA_KEYS      [i] = 0;  
        out->PA_KEYS_VALID[i] = 0;  
    }

    for (fm_uint i = 0; i < MBY_N_PARSER_FLAGS; i++)
        out->PA_FLAGS[i] = 0; 

    for (fm_uint i = 0; i < MBY_N_PARSER_PTRS; i++) {
        out->PA_PTRS      [i] = 0; 
        out->PA_PTRS_VALID[i] = 0; 
    }

    fm_byte hit_idx  [MBY_PA_ANA_STAGES];
    fm_bool hit_idx_v[MBY_PA_ANA_STAGES];
    for (fm_uint s = 0; s < MBY_PA_ANA_STAGES; s++) {
        hit_idx  [s] = 0;
        hit_idx_v[s] = 0;
    }

    // Calculate end-of-packet (EOP) and adjusted segment length (adj_seg_len):
    fm_uint32 rlen = in->RX_LENGTH;
    fm_uint32 adj_seg_len = 0;
    fm_bool   eop = 0;

    if (rlen < 4) { // don't remove fcs if less than 4 bytes
        adj_seg_len = rlen;
        eop = 1;
    } else if ((rlen - 4) > MBY_PA_MAX_SEG_LEN) { // if full seg, then set len to max & eop to 0
        adj_seg_len = MBY_PA_MAX_SEG_LEN;
        eop = 0;
    } else {
        adj_seg_len = (rlen - 4);
        eop = 1;
    }

    out->PA_ADJ_SEG_LEN = adj_seg_len;

    // Read in segment data:
    fm_byte seg_data[MBY_PA_MAX_SEG_LEN];
    for (fm_uint a = 0; a < MBY_PA_MAX_SEG_LEN; a++)
	seg_data[a] = a < in->RX_LENGTH ? in->RX_DATA[a] : 0;			
    
    fm_uint16 w0 = getSegDataWord(init_w0_offset, adj_seg_len, seg_data);
    fm_uint16 w1 = getSegDataWord(init_w1_offset, adj_seg_len, seg_data);
    fm_uint16 w2 = getSegDataWord(init_w2_offset, adj_seg_len, seg_data);

    fm_byte ptr [MBY_PA_ANA_STAGES];

    // Carry out analyzer actions (see corresponding section in functional spec):
    for (fm_uint s = 0; s < MBY_PA_ANA_STAGES; s++)
    {
        fm_bool rule_matched = FALSE;

        for (fm_uint r = 0; r < MBY_PA_ANA_RULES; r++)
        {
            fm_uint32 key_w_vals[MBY_PARSER_KEY_W_WIDTH] = { 0 };

            mbyModelReadCSRMult(regs, MBY_PARSER_KEY_W(s, r, 0), MBY_PARSER_KEY_W_WIDTH, key_w_vals);

            fm_uint16 w0_mask = FM_ARRAY_GET_FIELD(key_w_vals, MBY_PARSER_KEY_W, W0_MASK); 
            fm_uint16 w0_val  = FM_ARRAY_GET_FIELD(key_w_vals, MBY_PARSER_KEY_W, W0_VALUE);
            fm_uint16 w1_mask = FM_ARRAY_GET_FIELD(key_w_vals, MBY_PARSER_KEY_W, W1_MASK); 
            fm_uint16 w1_val  = FM_ARRAY_GET_FIELD(key_w_vals, MBY_PARSER_KEY_W, W1_VALUE);

            fm_uint32 key_s_vals[MBY_PARSER_KEY_S_WIDTH] = { 0 };

            mbyModelReadCSRMult(regs, MBY_PARSER_KEY_S(s, r, 0), MBY_PARSER_KEY_S_WIDTH, key_s_vals);

            fm_uint16 ana_state_mask = FM_ARRAY_GET_FIELD(key_s_vals, MBY_PARSER_KEY_S, STATE_MASK);
            fm_uint16 ana_state_val  = FM_ARRAY_GET_FIELD(key_s_vals, MBY_PARSER_KEY_S, STATE_VALUE);

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
            fm_uint32 ana_w_vals[MBY_PARSER_ANA_W_WIDTH] = { 0 };
            
            mbyModelReadCSRMult(regs, MBY_PARSER_ANA_W(s, r_hit, 0), MBY_PARSER_ANA_W_WIDTH, ana_w_vals);

            fm_byte skip           = FM_ARRAY_GET_FIELD(ana_w_vals, MBY_PARSER_ANA_W, SKIP);
            fm_byte next_w0_offset = FM_ARRAY_GET_FIELD(ana_w_vals, MBY_PARSER_ANA_W, NEXT_W0_OFFSET);
            fm_byte next_w1_offset = FM_ARRAY_GET_FIELD(ana_w_vals, MBY_PARSER_ANA_W, NEXT_W1_OFFSET);
            fm_byte next_w2_offset = FM_ARRAY_GET_FIELD(ana_w_vals, MBY_PARSER_ANA_W, NEXT_W2_OFFSET);

            fm_uint32 ana_s_vals[MBY_PARSER_ANA_S_WIDTH] = { 0 };

            mbyModelReadCSRMult(regs, MBY_PARSER_ANA_S(s, r_hit, 0), MBY_PARSER_ANA_S_WIDTH, ana_s_vals);

            fm_uint16 next_ana_state      = FM_ARRAY_GET_FIELD(ana_s_vals, MBY_PARSER_ANA_S, NEXT_STATE);
            fm_uint16 next_ana_state_mask = FM_ARRAY_GET_FIELD(ana_s_vals, MBY_PARSER_ANA_S, NEXT_STATE_MASK); 
            fm_uint16 next_op             = FM_ARRAY_GET_FIELD(ana_s_vals, MBY_PARSER_ANA_S, NEXT_OP);

            fm_uint32 tmp_op_result = ((((fm_uint32) w2) << 16) & 0xffff0000) |
                                      ((((fm_uint32) w2)      ) & 0x0000ffff) ; 

            op_mask &= MBY_PA_ANA_OP_MASK_BITS;
            op_rot  &= MBY_PA_ANA_OP_ROT_BITS;

            fm_uint16 op_result = (fm_uint16) ( (tmp_op_result >> op_rot) & ((fm_uint32) op_mask) );
            cur_ptr += op_result;
            if (cur_ptr >= MBY_PA_MAX_PTR_LEN)
                cur_ptr = MBY_PA_MAX_PTR_LEN;

            // Update w0..w2 fields for next stage:
            w0 = getSegDataWord(cur_ptr + next_w0_offset, adj_seg_len, seg_data);
            w1 = getSegDataWord(cur_ptr + next_w1_offset, adj_seg_len, seg_data);
            w2 = getSegDataWord(cur_ptr + next_w2_offset, adj_seg_len, seg_data);

            // Update state for next stage:
            ana_state = (ana_state & ~next_ana_state_mask) | (next_ana_state & next_ana_state_mask);

            // Update ALU op for next stage:
            op_rot  = (next_op >> MBY_PA_ANA_OP_ROT_SHIFT) & MBY_PA_ANA_OP_ROT_BITS;
            op_mask =  next_op & MBY_PA_ANA_OP_MASK_BITS;

            // Update pointer for next stage:
            cur_ptr += skip;
            if (cur_ptr >= MBY_PA_MAX_PTR_LEN) 
                cur_ptr = MBY_PA_MAX_PTR_LEN;
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
            fm_int    r_hit = hit_idx[s];
            fm_uint32 pa_exc_vals[MBY_PARSER_EXC_WIDTH] = { 0 };

            mbyModelReadCSRMult(regs, MBY_PARSER_EXC(s, r_hit, 0), MBY_PARSER_EXC_WIDTH, pa_exc_vals);

            fm_byte xa_ex_offset    = FM_ARRAY_GET_FIELD(pa_exc_vals, MBY_PARSER_EXC, EX_OFFSET);
            fm_bool xa_parsing_done = FM_ARRAY_GET_BIT  (pa_exc_vals, MBY_PARSER_EXC, PARSING_DONE);
            fm_bool eof_exc         = (adj_seg_len < (ptr[s] + xa_ex_offset)); // a.k.a. EOS

            if (eof_exc) // end-of-file exception
             {
                s_ena = FALSE; // disable further processing
                out->PA_EX_STAGE = s & 0x1F; // 5 bits
                if (eop == 1)
                    out->PA_EX_TRUNC_HEADER = 1; 
                else
                    out->PA_EX_DEPTH_EXCEED = 1; 
            }

            // Extraction action:
            for (fm_uint wd = 0; wd < 2; wd++)
            {
                fm_uint32 pa_ext_vals[MBY_PARSER_EXT_WIDTH] = { 0 };
                fm_int    r_hit_ex = r_hit + (wd * MBY_PA_ANA_RULES);

                mbyModelReadCSRMult(regs, MBY_PARSER_EXT(s, r_hit_ex, 0), MBY_PARSER_EXT_WIDTH, pa_ext_vals);

                fm_byte xa_key_start  = FM_ARRAY_GET_FIELD(pa_ext_vals, MBY_PARSER_EXT, KEY_START);
                fm_byte xa_key_len    = FM_ARRAY_GET_FIELD(pa_ext_vals, MBY_PARSER_EXT, KEY_LEN);
                fm_byte xa_key_offset = FM_ARRAY_GET_FIELD(pa_ext_vals, MBY_PARSER_EXT, KEY_OFFSET);
                fm_byte xa_flag_num   = FM_ARRAY_GET_FIELD(pa_ext_vals, MBY_PARSER_EXT, FLAG_NUM);
                fm_bool xa_flag_val   = FM_ARRAY_GET_BIT  (pa_ext_vals, MBY_PARSER_EXT, FLAG_VALUE);
                fm_byte xa_ptr_num    = FM_ARRAY_GET_FIELD(pa_ext_vals, MBY_PARSER_EXT, PTR_NUM);

                // Apply keys to target key array and track keys_valid:
                for (fm_uint k = 0; k < xa_key_len; k++)
                {
                    if ((xa_key_start + k) < MBY_N_PARSER_KEYS)
                    {
                        fm_byte   key_off = ptr[s] + xa_key_offset + (k*2);
                        fm_uint16 key_val = getSegDataWord(key_off, adj_seg_len, seg_data);
                        fm_bool   key_vld = (((fm_uint32) key_off) < (adj_seg_len - 1));

                        out->PA_KEYS      [xa_key_start + k] = key_val;
                        out->PA_KEYS_VALID[xa_key_start + k] = key_vld;
                    }
                }

                if ((xa_flag_num != 0) && (xa_flag_num < MBY_N_PARSER_FLAGS))
                    out->PA_FLAGS[xa_flag_num] = xa_flag_val;

                if ( (xa_ptr_num != 0) && (xa_ptr_num < MBY_N_PARSER_PTRS) ) {
                    out->PA_PTRS      [xa_ptr_num] = ptr[s];
                    out->PA_PTRS_VALID[xa_ptr_num] = 1;
                }

            } // for wd ...

            if (xa_parsing_done == 1) {
                s_ena                   = FALSE;
                out->PA_EX_STAGE        = s & 0x1F; // 5 bits
                out->PA_EX_PARSING_DONE = 1;
            }

        } // if s_ena ...

    } // for s ...

    // Perform checksum offloads & validations (see corresponding section in the func. spec):
    out->PA_CSUM_OK = 0;

    for (fm_uint p = 0; p <= 1; p++)
    {
        fm_byte pr = 2 + (p * 4); // 4 bytes / ptr
        fm_byte p0 = out->PA_PTRS[pr  ];
        fm_byte p1 = out->PA_PTRS[pr+1] - 1;

        // Validate checksum:
        if ( (out->PA_PTRS_VALID[pr] == 1) && (out->PA_PTRS_VALID[pr+1] == 1) ) {
            fm_bool csum_chk_result = checkIPv4Chksum(seg_data, p0, p1);
            if ( csum_chk_result && ((p1 - p0) <= 64) && (((fm_uint32) (p1 + 1)) <= rlen))
                out->PA_CSUM_OK |= (1 << p);  // ok -> set bit p
        }
    } // for p ...

    // Fetch flags and pointers:
    fm_byte otr_l3_ptr         = out->PA_PTRS[MBY_OTR_L3_PTR];
    fm_bool otr_l3_v           = out->PA_FLAGS[MBY_PA_OTR_L3_V_FLAG];

    fm_bool is_ipv4  = (otr_l3_v && (out->PA_KEYS_VALID[MBY_OTR_IPHDR_KEY] == 1));
    fm_bool is_ipv6  = (otr_l3_v && (out->PA_KEYS_VALID[MBY_OTR_IPHDR_KEY] == 0));
    
    fm_byte ip_len   = (is_ipv6) ?  4 : 1; // IPv6 Length field (2 empty keys precede IPv6 hdr)
    fm_byte n_keys   = (is_ipv6) ? 16 : 4; // number of outer IP addr keys

    fm_bool l3_vld_chk = (out->PA_PTRS_VALID[MBY_OTR_L3_PTR            ] == 1) &&
                         (out->PA_KEYS_VALID[MBY_OTR_IPHDR_KEY + ip_len] == 1) ;

    for (fm_uint i = 0; i < n_keys; i++)
        l3_vld_chk &= (out->PA_KEYS_VALID[MBY_OTR_IPADDR_KEY + i] == 1);

    // Read checksum configuration registers:
    fm_uint32 pa_csum_cfg_vals[MBY_PARSER_CSUM_CFG_WIDTH] = { 0 };

    mbyModelReadCSRMult(regs, MBY_PARSER_CSUM_CFG(in->RX_PORT, 0), MBY_PARSER_CSUM_CFG_WIDTH, pa_csum_cfg_vals);

    fm_bool val_l3_len = FM_ARRAY_GET_FIELD(pa_csum_cfg_vals, MBY_PARSER_CSUM_CFG, VALIDATE_L3_LENGTH);
    fm_uint16 l3_len   = out->PA_KEYS[MBY_OTR_IPHDR_KEY + ip_len];

    // Clear flags:
    out->PA_L3LEN_ERR  = 0;
    
    // Validate packet length:
    if (val_l3_len && l3_vld_chk)
    {
        fm_uint32 min_pkt_len = l3_len + otr_l3_ptr + 4 + ((is_ipv6) ? MBY_PSEUDOHEADER_SIZE : 0);

        if (rlen < min_pkt_len)
        {
            out->PA_L3LEN_ERR = 1;
            if (val_l3_len)
                out->PA_DROP = 1;
        }
    }
}
