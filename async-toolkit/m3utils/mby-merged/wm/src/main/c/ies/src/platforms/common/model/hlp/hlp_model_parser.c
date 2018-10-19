/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_parser.c
 * Creation Date:   May 8, 2015
 * Description:     PARSER stage of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2013 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Global Veriables
 *****************************************************************************/
fm_int      parserDisplayVerbose = 1;    /* 0=always prints; 1=ulv prints; 2=unused */

fm_byte     HLP_PA_MAX_SEG_LEN = 192;
fm_int      HLP_PA_MAX_PTR_LEN = 255;  /* Used as a threshold and not an array size */

fm_uint16   HLP_PA_MAX_DATA_SZ = 16384;

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*
#define GET_RX_DATA_WORD(state, index, value)                                                   \
    if ( ( ((fm_uint32) index > adj_seg_len) && ((fm_uint32) index < HLP_PA_MAX_SEG_LEN) ) ||   \
         ( (fm_uint32) index > HLP_PA_MAX_SEG_LEN ) )                                           \
    {                                                                                           \
        *(value) = (fm_uint16) 0x0000;                                                          \
    }                                                                                           \
    else                                                                                        \
    {                                                                                           \
        *(value) = ( (fm_uint16) ( (seg_data[(index)]   << 8) & 0xFF00 ) ) |                    \
                   ( (fm_uint16) ( (seg_data[(index)+1]       & 0xFF) ));                       \
    }                                                                            
*/

#define GET_RX_DATA_WORD(index, value)                                           \
    if ( (fm_uint32) index > adj_seg_len )                                       \
    {                                                                            \
        *(value) = (fm_uint16) 0x0000;                                           \
    }                                                                            \
    else                                                                         \
    {                                                                            \
        *(value) = ( (fm_uint16) ( (seg_data[(index)]   << 8) & 0xFF00 ) ) |     \
                   ( (fm_uint16) ( (seg_data[(index)+1]       & 0xFF) ));        \
    }                                                                            

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

static fm_bool calcIPv4Chksum(hlp_model         *model,
                              fm_byte            seg_data[HLP_PA_MAX_SEG_LEN],
                              fm_uint32          p_beg,
                              fm_uint32          p_end);

static fm_uint16 calcGenChksum(fm_byte         *buf,
                               fm_uint16        len);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************/
/** calcIPv4Chksum
 * \ingroup intModel
 *
 * \desc            Verifies IPv4 header checksum value
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       seg_data contains the segment data
 *
 * \param[in]       p_beg holds the pointer to the beginning of the IPv4 header
 *
 * \param[in]       p_end holds the pointer to the end of the IPv4 header
 *
 *
 *****************************************************************************/
static fm_bool calcIPv4Chksum(hlp_model         *model,
                              fm_byte            seg_data[HLP_PA_MAX_SEG_LEN],
                              fm_uint32          p_beg,
                              fm_uint32          p_end)
{
    hlp_modelState          *state = &model->packetState;
    fm_uint32               chksum = 0;

    for (fm_uint32 k = p_beg; k < p_end; k += 2)
    {
        chksum += ( ( ( seg_data[k] << 8 ) & 0xFF00 ) |
                    ( seg_data[k+1] & 0xFF ) );
    }

    chksum = ( chksum & 0xFFFF ) + ( (chksum >> 16) & 0xFFFF );

    // check if there was a carry value generated
    if ( ( (chksum >> 16) & 0xFFFF ) > 0)
    {
        chksum = ( chksum & 0xFFFF ) + ( (chksum >> 16) & 0xFFFF );
    }

    // check if there was a carry value generated
    if ( ( (chksum >> 16) & 0xFFFF ) > 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_MODEL_PARSER, "Checksum carry > 0 (IPv4)\n");
    }

    chksum = ~chksum;

    return ( ( ( chksum & 0xFFFF ) > 0 ) ? 0x0 : 0x1 );

}   /* end calcIPv4Chksum */


/*****************************************************************************/
/** calcIPv4Chksum
 * \ingroup intModel
 *
 * \desc            Calculates generic L4 checksum (UDP/TCP)
 *
 * \param[in]       buf points to the buffer for which to compute the checksum
 *
 * \param[in]       len is the number of bytes to include in the computation
 *
 *
 *****************************************************************************/
static fm_uint16 calcGenChksum(fm_byte         *buf,
                               fm_uint16        len)
{
    fm_uint32               chksum = 0;

    for (fm_uint16 k = 0; k < len; k += 2)
    {
        if (k == (len - 1)) {
            chksum += ( ( buf[k] << 8 ) & 0xFF00 );
        } else {
            chksum += ( ( ( buf[k] << 8 ) & 0xFF00 ) |
                        ( buf[k+1] & 0xFF ) );
        }
    }

    chksum = ( chksum & 0xFFFF ) + ( (chksum >> 16) & 0xFFFF );

    // check if there was a carry value generated
    if ( ( (chksum >> 16) & 0xFFFF ) > 0)
    {
        chksum = ( chksum & 0xFFFF ) + ( (chksum >> 16) & 0xFFFF );
    }

    // check if there was a carry value generated
    if ( ( (chksum >> 16) & 0xFFFF ) > 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_MODEL_PARSER, "Checksum carry > 0 (Gen L4)\n");
    }

    chksum = ~chksum;

    return ( chksum & 0xFFFF );

}   /* end calcGenChksum */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelParser
 *
 * \ingroup intModel
 *
 * \desc            Parses the incoming packet and extracts useful fields from
 *                  it.
 *
 * \param[in]       model points to the switch model state.
 *
 * \see             FM4000 Datasheet, Section 2.2.
 *
 *****************************************************************************/
void hlpModelParser(hlp_model *model)
{

    hlp_modelState     *state = &model->packetState;

    fm_status           status;
    fm_uint32           rlen;
    fm_uint32           adj_seg_len;
    fm_bool             eop;
    fm_byte             HLP_DSI_TX_TYPE         = 0x16;
    fm_byte             HLP_PKT_META_TYPE_OFFSET         = 0; // pkt metadata index for Type
    fm_byte             HLP_PKT_META_NH_OFFSET           = 10; // pkt metadata index for Next Header
    fm_byte             HLP_PKT_META_ESP_OFFSET          = 15; // pkt metadata index for ESP Header offset
    fm_byte             HLP_PKT_META_EHL_OFFSET          = 16; // pkt metadata index for EHL
    fm_byte             HLP_PKT_META_DSI_RX_FLAGS_OFFSET = 18; // pkt metadata index for Flags (DSI-RX)
    fm_byte             HLP_PKT_META_DSI_TX_FLAGS_OFFSET = 22; // pkt metadata index for Flags (DSI-TX)
    fm_int              HLP_PA_ANA_STAGES       = 32;
    fm_int              HLP_PA_ANA_RULES        = 16;
    fm_int              HLP_MAX_PA_KEY_LEN      = 80;
    fm_int              HLP_MAX_PA_PTR_NUM      = 7;
    fm_int              HLP_MAX_PA_FLAGS        = 47;
    fm_byte             HLP_OTR_IPHDR_KEY       = 42; // *** Note: (If IPv6, add 2)
    fm_byte             HLP_OTR_IPADDR_KEY      = 48;
    fm_byte             HLP_L4CSUM_KEY          = 32;
    fm_byte             HLP_L4LEN_KEY           = 35;
    fm_byte             HLP_OTR_L3_PTR          = 2;
    fm_byte             HLP_OTR_L4_PTR          = 3;
    fm_byte             HLP_PA_SUPRESS_CSUM_VAL_FLAG   = 3;  // supress_l4_checksum_val flag
    fm_byte             HLP_PA_OTR_L4_UDP_V_FLAG       = 4;  // otr_l4_udp_v flag
    fm_byte             HLP_PA_OTR_L4_TCP_V_FLAG       = 5;  // otr_l4_tcp_v flag
    fm_byte             HLP_PA_OTR_L4_SCTP_V_FLAG      = 6;  // otr_l4_sctp_v flag
    fm_byte             HLP_PA_WIN_PARSE_FLAG          = 9;  // window_parse_v flag
    fm_byte             HLP_PA_OTR_HEAD_FRAG_V_FLAG    = 10; // otr_head_frag_v flag
    fm_byte             HLP_PA_OTR_PAYLOAD_FRAG_V_FLAG = 11; // otr_payload_frag_v flag
    fm_byte             HLP_PA_OTR_L3_V_FLAG           = 22; // otr_l3_v flag
    fm_uint16           HLP_PA_ANA_OP_MASK_BITS = 0xFFF;
    fm_byte             HLP_PA_ANA_OP_ROT_BITS  = 0x0F;
    fm_byte             HLP_PA_ANA_OP_ROT_SHIFT = 0xC;

    fm_byte             seg_data[HLP_PA_MAX_SEG_LEN];

    /* pa register reference */
    fm_uint32           pa_nh_cfg_val[HLP_PARSER_NH_CFG_WIDTH];
    fm_uint32           pa_window_cfg_val[HLP_PARSER_WINDOW_CFG_WIDTH];
    fm_uint32           pa_port_cfg_val[HLP_PARSER_PORT_CFG_WIDTH];
    fm_uint32           pa_csum_cfg_val[HLP_PARSER_CSUM_CFG_WIDTH];
    fm_uint32           key_w_val[HLP_PARSER_KEY_W_WIDTH];
    fm_uint32           key_s_val[HLP_PARSER_KEY_S_WIDTH];
    fm_uint32           ana_w_val[HLP_PARSER_ANA_W_WIDTH];
    fm_uint32           ana_s_val[HLP_PARSER_ANA_S_WIDTH];
    fm_uint32           pa_ext_val[HLP_PARSER_EXT_WIDTH];
    fm_uint32           pa_exc_val[HLP_PARSER_EXC_WIDTH];

    /* pa_csum_cfg fields */
    fm_byte             val_l4_csum = 0;
    fm_byte             store_l4_partial = 0;
    fm_byte             compute_l4_csum = 0;
    fm_byte             val_l3_len = 0;

    /* pa_window_cfg fields */
    fm_byte             port_0;
    fm_byte             mode_0;
    fm_byte             port_1;
    fm_byte             mode_1;

    /* pa_port_cfg fields */
    fm_byte             init_w0_offset;
    fm_byte             init_w1_offset;
    fm_byte             init_w2_offset;
    fm_byte             init_ptr;
    fm_uint16           old_ana_state;
    fm_uint16           ana_state;
    fm_uint16           op_mask;
    fm_byte             op_rot;

    /* Pa window variables */
    fm_byte             nh_cfg_idx = 0;
    fm_byte             next_header = 0;
    fm_byte             esp_offset = 0;
    fm_byte             ehl = 0;
    fm_uint16           win_offset = 0; // use extra bits for rollover

    /* Analyzer Action variables */
    fm_uint16           w0, w1, w2;
    fm_int              i, s, r;
    fm_uint16           w0_mask;
    fm_uint16           w0_val;
    fm_uint16           w1_mask;
    fm_uint16           w1_val;
    fm_uint16           state_mask;
    fm_uint16           state_value;
    fm_uint16           ana_state_mask;
    fm_uint16           ana_state_val;
    fm_byte             cp;        /* current pointer */
    fm_byte             old_cp;
    fm_bool             cp_roll_flag;
    fm_uint16           ix; 
    fm_byte             hit_idx[HLP_PA_ANA_STAGES];
    fm_bool             hit_idx_v[HLP_PA_ANA_STAGES];
    fm_bool             rule_matched;
    fm_byte             ptr[HLP_PA_ANA_STAGES];
    fm_int              r_hit;
    fm_byte             skip;
    fm_uint32           tmp_op_result;
    fm_uint16           op_result;
    fm_byte             next_w0_offset;
    fm_byte             next_w1_offset;
    fm_byte             next_w2_offset;
    fm_uint16           next_ana_state;
    fm_uint16           next_ana_state_mask;
    fm_uint16           next_op;

    /* Resolve variables */
    fm_int      rh; 
    fm_int      rh_ex; 
    fm_byte     xa_ex_offset;
    fm_bool     xa_parsing_done;
    fm_byte     eof_exc;
    fm_bool     s_ena;
    fm_int      a, k, wd;
    fm_byte     xa_key_start;
    fm_byte     xa_key_len;
    fm_byte     xa_key_offset;
    fm_byte     xa_flag_num;
    fm_bool     xa_flag_val;
    fm_byte     xa_ptr_num;
    fm_uint16   key_val;
    fm_bool     key_vld;
    fm_uint16   rdata_val;

    /* Checksum variables */
    fm_bool     validate_csum_en = 0;
    fm_bool     store_partial_csum_en = 0;
    fm_bool     compute_csum_en = 0;
    fm_bool     suppress_csum_val = 0;
    fm_bool     otr_l3_v;
    fm_bool     otr_l4_udp_v = 0;
    fm_bool     otr_l4_tcp_v = 0;
    fm_bool     otr_l4_sctp_v = 0;
    fm_bool     otr_payload_frag_v = 0;
    fm_bool     otr_head_frag_v = 0;
    fm_uint16   l3_len = 0;
    fm_uint32   min_pkt_len;
    fm_uint32   l4_csum = 0;
    fm_uint32   calc_l4_csum = 0;
    fm_uint16   l4_len;
    fm_bool     is_ipv4 = 0;
    fm_bool     is_ipv6 = 0;
    fm_bool     l3_vld_chk = 0;
    fm_bool     l4_vld_chk = 0;
    fm_byte     otr_l3_ptr = 0;
    fm_byte     otr_l4_ptr = 0;
    fm_uint16   start_ptr = 0;
    fm_uint16   end_ptr = 0;
    fm_uint16   csum_start_ptr = 0;
    fm_uint16   csum_end_ptr = 0;
    fm_uint16   esp_trailer_len = 0;
    fm_byte     dsi_tx_flags_bit1 = 0;
    fm_uint     ph, pl; // pseudoheader loop, payload loop
    fm_byte    *csum_bytes;
    fm_uint16   cb_ptr = 0;

    fm_uint32   p;
    fm_byte     pr, p0, p1;
    fm_bool     csum_chk_result = 0;

    if(testPlusArgs("HLP_PARSER_WM_PRINT_VERBOSE") >= 0)
        parserDisplayVerbose = testPlusArgs("HLP_PARSER_WM_PRINT_VERBOSE");
    WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "parserDisplayVerbose=%0d\n", parserDisplayVerbose)

    status = hlpModelReadCSRMult (model->sw, 
                                 HLP_PARSER_WINDOW_CFG(0), 
                                 HLP_PARSER_WINDOW_CFG_WIDTH,
                                 pa_window_cfg_val);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    port_0       = (fm_byte)   FM_ARRAY_GET_FIELD(pa_window_cfg_val, HLP_PARSER_WINDOW_CFG, PORT_0);
    mode_0       = (fm_bool)   FM_ARRAY_GET_BIT(pa_window_cfg_val, HLP_PARSER_WINDOW_CFG, MODE_0);
    port_1       = (fm_byte)   FM_ARRAY_GET_FIELD(pa_window_cfg_val, HLP_PARSER_WINDOW_CFG, PORT_1);
    mode_1       = (fm_bool)   FM_ARRAY_GET_BIT(pa_window_cfg_val, HLP_PARSER_WINDOW_CFG, MODE_1);

    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "(DBG) Window Cfg Info: port_0=%0d, mode_0=%0d\n",
                port_0, mode_0)
    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "(DBG) Window Cfg Info: port_1=%0d, mode_1=%0d\n",
                port_1, mode_1)

    status = hlpModelReadCSRMult (model->sw, 
                                 HLP_PARSER_PORT_CFG(state->RX_PORT, 0), 
                                 HLP_PARSER_PORT_CFG_WIDTH,
                                 pa_port_cfg_val);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    /* Initialize: */
    state->PA_EX_DEPTH_EXCEED   = 0;
    state->PA_EX_TRUNC_HEADER   = 0;
    state->PA_EX_PARSING_DONE   = 0;

    /* Check if window port and calculate offset */
    if ((state->RX_PORT == port_0) && (mode_0 != 0))
    {
        if (mode_0 != 1)
        {
            FM_LOG_ERROR(FM_LOG_CAT_MODEL_PARSER, "Error: Window mode = %0d not supported yet by Parser stage\n", mode_0);
        }
        else
        {
            nh_cfg_idx = 0;

            next_header = state->PKT_META[HLP_PKT_META_NH_OFFSET];

            esp_offset = state->PKT_META[HLP_PKT_META_ESP_OFFSET];

            ehl = (state->PKT_META[HLP_PKT_META_EHL_OFFSET] & 0xF); // EHL is 4 bit value and byte units

            if ((esp_offset != 0) && (ehl != 0)) {
                win_offset = esp_offset + (ehl * 4);
            }
            else {
                win_offset = 0;
            }

            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG) Window Port 0: nh=0x%02x, esp_offset=0x%02x, ehl=0x%02x\n",
                        next_header, esp_offset, ehl)
        }
    }
    else if ((state->RX_PORT == port_1) && (mode_1 != 0))
    {
        if (mode_1 != 1)
        {
            FM_LOG_ERROR(FM_LOG_CAT_MODEL_PARSER, "Error: Window mode = %0d not supported yet by Parser stage\n", mode_1);
        }
        else
        {
            nh_cfg_idx = 1;

            next_header = state->PKT_META[HLP_PKT_META_NH_OFFSET];

            esp_offset = state->PKT_META[HLP_PKT_META_ESP_OFFSET];

            ehl = (state->PKT_META[HLP_PKT_META_EHL_OFFSET] & 0xF); // EHL is 4 bit value and in terms of 4-byte units

            if ((esp_offset != 0) && (ehl != 0)) {
                win_offset = esp_offset + (ehl * 4);
            }
            else {
                win_offset = 0;
            }

            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG) Window Port 1: nh=0x%02x, esp_offset=0x%02x, ehl=0x%02x\n",
                        next_header, esp_offset, ehl)
        }
    }
    else 
    {
        // incoming port is NOT a window port
        win_offset = 0;
    }

    /* If win_offset is 0 (either because it is not window port, or ESP Hdr Offset == 0 || EHL == 0)
     * then grab initial offsets / values from PA_PORT_CFG.  Otherwise, use appropriate PA_NH_CFG entry) */
    if (win_offset == 0) {
        init_w0_offset      = (fm_byte)   FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_W0_OFFSET);
        init_w1_offset      = (fm_byte)   FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_W1_OFFSET);
        init_w2_offset      = (fm_byte)   FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_W2_OFFSET);

        init_ptr            = (fm_byte)   FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_PTR);
        ana_state           = (fm_uint16) FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_STATE);
        op_mask             = (fm_uint16) FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_OP_MASK);
        op_rot              = (fm_byte)   FM_ARRAY_GET_FIELD(pa_port_cfg_val, HLP_PARSER_PORT_CFG, INITIAL_OP_ROT);
    }
    else {
        status = hlpModelReadCSRMult (model->sw, 
                                     HLP_PARSER_NH_CFG(nh_cfg_idx, next_header, 0), 
                                     HLP_PARSER_NH_CFG_WIDTH,
                                     pa_nh_cfg_val);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        init_w0_offset      = (fm_byte)   FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_W0_OFFSET);
        init_w1_offset      = (fm_byte)   FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_W1_OFFSET);
        init_w2_offset      = (fm_byte)   FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_W2_OFFSET);

        init_ptr            = (fm_byte)   FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_PTR);
        ana_state           = (fm_uint16) FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_STATE);
        op_mask             = (fm_uint16) FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_OP_MASK);
        op_rot              = (fm_byte)   FM_ARRAY_GET_FIELD(pa_nh_cfg_val, HLP_PARSER_NH_CFG, INITIAL_OP_ROT);
    }
    
    cp                  = init_ptr; // will be relative to calculated win_offset
    rlen                = state->RX_LENGTH;

    /******************************************************************************************************/
    /* check if offset is greater than received segment, or past max esp adjustment (refer to bug #31056) */
    /******************************************************************************************************/
    if (win_offset >= (rlen - 4))
    {
        state->PA_EX_TRUNC_HEADER = 1;

        if (win_offset > 0) {
            // set WINDOW_PARSE_V flag for this scenario (even though offset is invalid)
            state->PA_FLAGS[HLP_PA_WIN_PARSE_FLAG] = 1;
        }

        eop = 1;

        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG OFFSET) Info:  offset=%0d, rlen=%0d (fcs removed), max_offset=%0d\n",
                    win_offset, (rlen-4), HLP_PA_MAX_PTR_LEN); 

        goto CHECKSUM;
    }
    else if (win_offset > (HLP_PA_MAX_PTR_LEN + 1)) // 256 is the max offset allowed per HAS (and allows 192B window)
    {
        state->PA_EX_DEPTH_EXCEED = 1;

        if (win_offset > 0) {
            // set WINDOW_PARSE_V flag for this scenario (even though offset is invalid)
            state->PA_FLAGS[HLP_PA_WIN_PARSE_FLAG] = 1;
        }

        eop = 1;

        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG OFFSET) Info:  offset=%0d, rlen=%0d (fcs removed), max_offset=%0d\n",
                    win_offset, (rlen-4), HLP_PA_MAX_PTR_LEN); 

        goto CHECKSUM;
    }


    /********************************************************************************************/
    /* Now use the calculated offset to fetch the correct slice of data and initialize Seg Data */
    /********************************************************************************************/
    FM_CLEAR(seg_data);
    for (a=0; a<HLP_PA_MAX_SEG_LEN; a++)
    {
        seg_data[a] = state->RX_DATA[win_offset + a];
    }
    /********************************************************************************************/


    /* Initialize the output */
    for (i = 0; i < HLP_MAX_PA_KEY_LEN; i++)
    {
        state->PA_KEYS[i]       = 0x0;  
        state->PA_KEYS_VALID[i] = 0;  

        if (i <= HLP_MAX_PA_PTR_NUM) 
        {
            state->PA_PTRS[i]       = 0x0; 
            state->PA_PTRS_VALID[i] = 0; 
        }

        if (i <= HLP_MAX_PA_FLAGS) 
        {
            state->PA_FLAGS[i] = 0; 
        }
    }

    for (s=0; s<HLP_PA_ANA_STAGES; s++) {
        hit_idx[s]   = 0;
        hit_idx_v[s] = 0;
    } // initialize hit_idx to zero


    /* ***************************************************************************** */
    /* 8/27/15                                                                       */
    /* Adding per rtl bug 29482, "ex_offset use seg_len adjusted for CRC field size" */

/*    if ( (state->RX_EOP == 1) && (rlen >= 4) )
    {
        adj_seg_len = rlen - 4;
    }
    else if ( (state->RX_EOP == 0) && (state->RX_NEXT_LEN > 0) && (state->RX_NEXT_LEN < 4 ) )
    {
        adj_seg_len = rlen - (4 - state->RX_NEXT_LEN);
    }
    else
    {
        adj_seg_len = rlen;
    }*/


    /***************************************************************/
    /* calculate EOP and adj_seg_len based on offset and RX_LENGTH */
    /***************************************************************/
    if ((rlen - win_offset) < 4) // don't remove fcs if less than 4 bytes
    {
        adj_seg_len = (rlen - win_offset);
        eop = 1;
    }
    else if ((rlen - win_offset - 4) > HLP_PA_MAX_SEG_LEN) // if full seg, then set len to max & eop to 0
    {
        adj_seg_len = HLP_PA_MAX_SEG_LEN;
        eop = 0;
    }
    else // otherwise set the length to remove 4 fcs bytes and set eop to 1
    {
        adj_seg_len = (rlen - win_offset - 4);
        eop = 1;
    }

    state->PA_ADJ_SEG_LEN = adj_seg_len;

    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "Info: Adjusted seg_len: [Port=%0d, eop=%0d offset=%0d]"
                "  rlen=%0d, adj_seg_len=%0d\n",
                state->RX_PORT, eop, win_offset, rlen, adj_seg_len);
    /***************************************************************/


    if (init_ptr > adj_seg_len)
    {
        WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "Info: Parser InitialPtr=[%0d] > adj_seg_len=%0d\n",
                                                                  init_ptr, adj_seg_len);
    }


    /* First header: pull values for w0, w1 and w2 */
    ix = init_w0_offset;
    GET_RX_DATA_WORD(ix, &w0);

    ix = init_w1_offset;
    GET_RX_DATA_WORD(ix, &w1);

    ix = init_w2_offset;
    GET_RX_DATA_WORD(ix, &w2);

    /* ************************************************************************************ */
    /* 8/27/15                                                                              */
    /* Adding per rtl bug 29310, "pointer...not protected from rollover", Comment #15.      */
    /* - Check if initial_w*_offsets + 1 >= adj_seg_len.  If yes:                           */
    /*    - Cap pointer to max=255. Set up EXC for next hit to mimic RTL overflow. zero wX. */ 

/*
    cp_roll_flag    = 0;

    if  ( ( (fm_uint32) init_w0_offset + 1) >= adj_seg_len ) 
    {
        DISPLAY("[Info] (init_w0_offset  + 1) >= adj_seg_len (%0d >= %0d).  "
                "Pointer capped at %0d.  w0 set to 0. \n",
                (init_w0_offset + 1), adj_seg_len, HLP_PA_MAX_PTR_LEN );
        cp_roll_flag    = 1;
        w0 = 0x0;
    }
    if ( ( (fm_uint32) init_w1_offset + 1) >= adj_seg_len ) 
    {
        DISPLAY("[Info] (init_w1_offset  + 1) >= adj_seg_len (%0d >= %0d). "
                "Pointer capped at %0d.  w1 set to 0. \n",
                (init_w1_offset + 1), adj_seg_len, HLP_PA_MAX_PTR_LEN );
        cp_roll_flag    = 1;
        w1 = 0x0;
    }
    if ( ( (fm_uint32) init_w2_offset + 1) >= adj_seg_len ) 
    {
        DISPLAY("[Info] (init_w2_offset  + 1) >= adj_seg_len (%0d >= %0d). "
                "Pointer capped at %0d.  w2 set to 0. \n",
                (init_w2_offset + 1), adj_seg_len, HLP_PA_MAX_PTR_LEN );
        cp_roll_flag    = 1;
        w2 = 0x0;
    }

    if ( cp_roll_flag == 1) {
        cp = HLP_PA_MAX_PTR_LEN;                                  
    }
*/

    /* ************************************************************************************ */
    /* 4/22/16                                                                              */
    /* Updating per rtl bug 30796                                                           */
    /* - Check if initial_w*_offsets + 1 >= adj_seg_len.  If true for ANY:                  */
    /*    - Set ALL w* values to 0 (to prevent unintended "false" hits downstream)          */ 
    /*    - Do NOT "cap" the pointer                                                        */ 

    cp_roll_flag    = 0;

    if  ( ( (fm_uint32) init_w0_offset + 1) >= adj_seg_len ) 
    {
        WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (init_w0_offset  + 1) >= adj_seg_len (%0d >= %0d).\n",
                                                                  (init_w0_offset + 1), adj_seg_len );
        cp_roll_flag    = 1;
    }
    if ( ( (fm_uint32) init_w1_offset + 1) >= adj_seg_len ) 
    {
        WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (init_w1_offset  + 1) >= adj_seg_len (%0d >= %0d).\n",
                                                                  (init_w1_offset + 1), adj_seg_len );
        cp_roll_flag    = 1;
    }
    if ( ( (fm_uint32) init_w2_offset + 1) >= adj_seg_len ) 
    {
        WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (init_w2_offset  + 1) >= adj_seg_len (%0d >= %0d).\n",
                                                                  (init_w2_offset + 1), adj_seg_len );
        cp_roll_flag    = 1;
    }

    if ( cp_roll_flag == 1) {
        WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] w0/w1/w2 set to 0\n");
        w0 = 0x0;
        w1 = 0x0;
        w2 = 0x0;
    }

    /* ******************************************************************************* */

    if (win_offset == 0) {
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_PORT_CFG[Port=%0d].Initialw0Offset=0x%02x [%0d] (w0 value=0x%04x)\n",
                    state->RX_PORT, init_w0_offset, init_w0_offset, w0)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_PORT_CFG[Port=%0d].Initialw1Offset=0x%02x [%0d] (w1 value=0x%04x)\n",
                    state->RX_PORT, init_w1_offset, init_w1_offset, w1)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_PORT_CFG[Port=%0d].Initialw2Offset=0x%02x [%0d] (w2 value=0x%04x)\n",
                    state->RX_PORT, init_w2_offset, init_w2_offset, w2)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_PORT_CFG[Port=%0d].InitialState=0x%04x [%0d]\n",
                    state->RX_PORT, ana_state, ana_state)
    } else {
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_NH_CFG[%0d][%0d].Initialw0Offset=0x%02x [%0d] (w0 value=0x%04x)\n",
                    nh_cfg_idx, next_header, init_w0_offset, init_w0_offset, w0)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_NH_CFG[%0d][%0d].Initialw1Offset=0x%02x [%0d] (w1 value=0x%04x)\n",
                    nh_cfg_idx, next_header, init_w1_offset, init_w1_offset, w1)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_NH_CFG[%0d][%0d].Initialw2Offset=0x%02x [%0d] (w2 value=0x%04x)\n",
                    nh_cfg_idx, next_header, init_w2_offset, init_w2_offset, w2)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG INIT) Info: PARSER_NH_CFG[%0d][%0d].InitialState=0x%04x [%0d]\n",
                    nh_cfg_idx, next_header, ana_state, ana_state)
    }

    /* ********** Analyzer Action ********** */
    for (s = 0; s < HLP_PA_ANA_STAGES; s++)
    {

        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG ANA) Info: Stage #%0d (w0 value=0x%04x)\n", s, w0)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG ANA) Info: Stage #%0d (w1 value=0x%04x)\n", s, w1)
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG ANA) Info: Stage #%0d (st value=0x%04x)\n", s, ana_state)

        rule_matched    = 0;
        for (r = 0; r < HLP_PA_ANA_RULES; r++)
        {
            status = hlpModelReadCSRMult (model->sw, 
                                          HLP_PARSER_KEY_W(s, r, 0), 
                                          HLP_PARSER_KEY_W_WIDTH,
                                          key_w_val);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

            w0_mask     = (fm_uint16) FM_ARRAY_GET_FIELD(key_w_val, HLP_PARSER_KEY_W, W0_MASK);
            w0_val      = (fm_uint16) FM_ARRAY_GET_FIELD(key_w_val, HLP_PARSER_KEY_W, W0_VALUE);
            w1_mask     = (fm_uint16) FM_ARRAY_GET_FIELD(key_w_val, HLP_PARSER_KEY_W, W1_MASK);
            w1_val      = (fm_uint16) FM_ARRAY_GET_FIELD(key_w_val, HLP_PARSER_KEY_W, W1_VALUE);

            status = hlpModelReadCSRMult (model->sw, 
                                          HLP_PARSER_KEY_S(s, r, 0), 
                                          HLP_PARSER_KEY_S_WIDTH,
                                          key_s_val);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

            ana_state_mask  = (fm_uint16) FM_ARRAY_GET_FIELD(key_s_val, HLP_PARSER_KEY_S, STATE_MASK);
            ana_state_val   = (fm_uint16) FM_ARRAY_GET_FIELD(key_s_val, HLP_PARSER_KEY_S, STATE_VALUE);

            /* CAM Matching */     
            if ( ( (w0    &    w0_mask) ==    w0_val ) &&
                 ( (w1    &    w1_mask) ==    w1_val ) &&
                 ( (ana_state & ana_state_mask) == ana_state_val ) )
            {
                hit_idx[s]      = (fm_byte) r & 0x1F;
                hit_idx_v[s]    = 1;
                rule_matched    = 1;
            } 
        }  // end for loop (RULES : Analyzer Action)

        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG ANA) Info:  ptr=0x%02x (stage=%0d)\n", cp, s);

        /*  Store ptr before updating. */
        ptr[s]          = cp;

        r_hit           = hit_idx[s];

        /*  Get PA_ANA_W fields. */
        status = hlpModelReadCSRMult (model->sw, 
                                      HLP_PARSER_ANA_W(s, r_hit, 0), 
                                      HLP_PARSER_ANA_W_WIDTH,
                                      ana_w_val);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        skip            = (fm_byte) FM_ARRAY_GET_FIELD(ana_w_val, HLP_PARSER_ANA_W, SKIP);
        next_w0_offset  = (fm_byte) FM_ARRAY_GET_FIELD(ana_w_val, HLP_PARSER_ANA_W, NEXT_W0_OFFSET);
        next_w1_offset  = (fm_byte) FM_ARRAY_GET_FIELD(ana_w_val, HLP_PARSER_ANA_W, NEXT_W1_OFFSET);
        next_w2_offset  = (fm_byte) FM_ARRAY_GET_FIELD(ana_w_val, HLP_PARSER_ANA_W, NEXT_W2_OFFSET);

        /* Update pointer: use previous w2. */    
        op_mask         &= HLP_PA_ANA_OP_MASK_BITS;    /* 12 bits used */
        op_rot          &= HLP_PA_ANA_OP_ROT_BITS;     /*  4 bits used */

        tmp_op_result   = ( (fm_uint32) ( (w2 << 16) & 0xFFFF0000 ) ) | 
                          ( (fm_uint32) (  w2        & 0x0000FFFF ) ); 
        op_result       = (fm_uint16) ( (tmp_op_result >> op_rot) & (fm_uint32) op_mask );

        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG ANA) Info:  w2=0x%04x, op_mask=0x%03x, op_rot=%0d ... op_result=%0d\n",
                    w2, op_mask, op_rot, op_result);

        status = hlpModelReadCSRMult (model->sw, 
                                      HLP_PARSER_ANA_S(s, r_hit, 0), 
                                      HLP_PARSER_ANA_S_WIDTH,
                                      ana_s_val);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        next_ana_state      = (fm_uint16) FM_ARRAY_GET_FIELD(ana_s_val, HLP_PARSER_ANA_S, NEXT_STATE);
        next_ana_state_mask = (fm_uint16) FM_ARRAY_GET_FIELD(ana_s_val, HLP_PARSER_ANA_S, NEXT_STATE_MASK); 
        next_op             = (fm_uint16) FM_ARRAY_GET_FIELD(ana_s_val, HLP_PARSER_ANA_S, NEXT_OP);   

        /* Update if a rule matched this stage, else pass unchanged to next stage. */ 
        if (rule_matched == 1) 
        {
            old_cp  = cp;
            // update pointer with op_result 
            // (skip will be added after w0/w1/w2 fetch)
            cp += op_result;

            /* **************************************************************************** */
            /* *** Check if pointer passes segment length, or passes rollover values.   *** */
            /* *** See: MAS bug 29442, WM bug 29335 and rtl bug 29310, Comment #14.     *** */
            if ( (old_cp + op_result) > HLP_PA_MAX_PTR_LEN ) 
            {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (Stage #%0d, Rule #%0d) Pointer:  cp + op_result (%0d + %0d) =%0d "
                                                                          "> %0d, now capped at %0d.\n", 
                                                                          s, r_hit, old_cp, op_result, (old_cp + op_result),
                                                                          HLP_PA_MAX_PTR_LEN, HLP_PA_MAX_PTR_LEN );  

                cp = HLP_PA_MAX_PTR_LEN;                        // Len is 255;
            }
            else if ( (old_cp + op_result) > adj_seg_len )      // 8/27/15, rlen changed to adj_len
            {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (Stage #%0d, Rule #%0d) Pointer:  cp + op_result (%0d + %0d) = %0d"
                                                                          "  is past adjusted segment length = %0d [eop=%0d].\n" ,
                                                                          s, r_hit, old_cp, op_result, (old_cp + op_result), adj_seg_len, eop);  
            }
            /* ******************************************************************************/

            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG ANA) Info:  nxt ptr=0x%02x (stage=%0d)\n", cp, s);

            ix = cp + next_w0_offset;
            GET_RX_DATA_WORD(ix, &w0);

            ix = cp + next_w1_offset;
            GET_RX_DATA_WORD(ix, &w1);
                      
            ix = cp + next_w2_offset;
            GET_RX_DATA_WORD(ix, &w2);


            /* ******************************************************************************* */
            /* 8/27/15                                                                         */
            /* Adding per rtl bug 29310, "pointer...not protected from rollover", Comment #14. */
            /* - Check if new w* are oob:  cp' + next_w*_offset + 1 >= adj_seg_len.  If yes:   */
            /*    - Cap pointer to max of 255. Sets up EXC for next hit to mimic RTL overflow. */ 

/*
            cp_roll_flag    = 0;

            if  ( ((fm_uint32) cp + (fm_uint32) next_w0_offset + 1) >= adj_seg_len ) 
            {
                DISPLAY("[Info] (cp' + next_w0_offset + 1) >= adj_seg_len (%0d + %0d + 1 >= %0d). "
                        "Pointer capped at %0d. w0 set to 0. \n",
                        cp, next_w0_offset, adj_seg_len, HLP_PA_MAX_PTR_LEN );
                cp_roll_flag    = 1;
                w0 = 0x0;
            }
            if ( ((fm_uint32) cp + (fm_uint32) next_w1_offset + 1) >= adj_seg_len ) 
            {
                DISPLAY("[Info] (cp' + next_w1_offset + 1) >= adj_seg_len (%0d + %0d + 1 >= %0d). "
                        "Pointer capped at %0d. w1 set to 0. \n",
                        cp, next_w1_offset, adj_seg_len, HLP_PA_MAX_PTR_LEN );
                cp_roll_flag    = 1;
                w1 = 0x0;
            }
            if ( ((fm_uint32) cp + (fm_uint32) next_w2_offset + 1) >= adj_seg_len ) 
            {
                DISPLAY("[Info] (cp' + next_w2_offset + 1) >= adj_seg_len (%0d + %0d + 1 >= %0d). "
                        "Pointer capped at %0d. w2 set to 0. \n",
                        cp, next_w2_offset, adj_seg_len, HLP_PA_MAX_PTR_LEN );
                cp_roll_flag    = 1;
                w2 = 0x0;
            }

            if ( cp_roll_flag == 1) {
                cp = HLP_PA_MAX_PTR_LEN;                                  
            }
*/

            /* ************************************************************************************ */
            /* 4/22/16                                                                              */
            /* Updating per rtl bug 30796                                                           */
            /* - Check if initial_w*_offsets + 1 >= adj_seg_len.  If true for ANY:                  */
            /*    - Set ALL w* values to 0 (to prevent unintended "false" hits downstream)          */ 
            /*    - Do NOT "cap" the pointer                                                        */ 

            cp_roll_flag    = 0;

            if  ( ((fm_uint32) cp + (fm_uint32) next_w0_offset + 1) >= adj_seg_len ) 
            {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (cp' + next_w0_offset + 1) >= adj_seg_len (%0d + %0d + 1 >= %0d).\n",
                                                                          cp, next_w0_offset, adj_seg_len );
                cp_roll_flag    = 1;
            }
            if  ( ((fm_uint32) cp + (fm_uint32) next_w1_offset + 1) >= adj_seg_len ) 
            {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (cp' + next_w1_offset + 1) >= adj_seg_len (%0d + %0d + 1 >= %0d).\n",
                                                                          cp, next_w1_offset, adj_seg_len );
                cp_roll_flag    = 1;
            }
            if  ( ((fm_uint32) cp + (fm_uint32) next_w2_offset + 1) >= adj_seg_len ) 
            {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (cp' + next_w2_offset + 1) >= adj_seg_len (%0d + %0d + 1 >= %0d).\n",
                                                                          cp, next_w2_offset, adj_seg_len );
                cp_roll_flag    = 1;
            }

            if ( cp_roll_flag == 1) {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] w0/w1/w2 set to 0\n");
                w0 = 0x0;
                w1 = 0x0;
                w2 = 0x0;
            }

            /* ******************************************************************************* */



            old_ana_state   = ana_state;

            ana_state = (ana_state & ~next_ana_state_mask) | (next_ana_state & next_ana_state_mask);

            /* op_mask is 12 bits wide per MAS bug 29442. */
            op_rot  = (next_op >> HLP_PA_ANA_OP_ROT_SHIFT) & HLP_PA_ANA_OP_ROT_BITS;     /* Upper  4 of 16 bits used */
            op_mask =  next_op & HLP_PA_ANA_OP_MASK_BITS;                                /* Lower 12 of 16 bits used */

            /* Check for pointer rollover, see RTL bug 29310. */
            if ( (cp + skip) > HLP_PA_MAX_PTR_LEN ) 
            {
                old_cp          = cp;
                cp              = HLP_PA_MAX_PTR_LEN;       // Len is 255;

                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "[Info] (Stage #%0d, Rule #%0d) Pointer:  cp + skip"
                                                                          " (%0d + %0d) = %0d > %0d, now capped at %0d.\n", 
                                                                           s, r_hit, old_cp, skip, (old_cp + skip),
                                                                           HLP_PA_MAX_PTR_LEN, HLP_PA_MAX_PTR_LEN );  
            }
            else
            {
                cp += skip; // now the skip can be added (w0/w1/w2 have been fetched)
            }

            WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "Info:  Highest matching rule for analyzer stage #%0d was rule #%0d (new ptr = %0d)\n", s, r_hit, cp);
            WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "       (Next stage values: w0=0x%04x   w1=0x%04x   w2=0x%04x   op={0x%01x,0x%03x}   state=0x%04x)\n",
                                                                      w0, w1, w2, op_rot, op_mask, ana_state);
        }
        else    /* Else, rule is not matched. */
        {
            WM_DISPLAY3(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "Info:  No rule matched for analyzer stage = %0d.  "
                                                                       "Passing w0[x%04x]/w1[x%04x]/w2[x%04x]/state[x%04x]/op_result[%0d]"
                                                                       " unchanged to next stage ( cp=[%0d], skip=[%0d] ).\n", 
                                                                       s, w0, w1, w2, ana_state, op_result, cp, skip);
        }
    } // end for loop (STAGES : Analyzer Action)

//    state->PA_STATE = ana_state;  // Removed per bug 29541


    /* ********** Resolve ********** */


    s_ena = 1;

    /* Apply and resolve. */
    for (s = 0; s < HLP_PA_ANA_STAGES; s++)
    {
        rh = hit_idx[s];

        /* Calculate eof_exc: */
        status = hlpModelReadCSRMult (model->sw, 
                                          HLP_PARSER_EXC(s, rh, 0), 
                                          HLP_PARSER_EXC_WIDTH,
                                          pa_exc_val);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        xa_ex_offset    = (fm_byte) FM_ARRAY_GET_FIELD(pa_exc_val, HLP_PARSER_EXC, EX_OFFSET);
      //xa_parsing_done = (fm_bool) FM_GET_BIT64(pa_exc_val, HLP_PARSER_EXC, PARSING_DONE);
        xa_parsing_done = (fm_bool) FM_ARRAY_GET_BIT(pa_exc_val, HLP_PARSER_EXC, PARSING_DONE);

        
        /* ****************************************************************************** */
        /* 8/27/15.  changing rlen to adj_seg_len per rtl bug 29482                       */
        /* ****************************************************************************** */
        eof_exc         = (fm_byte) ( (ptr[s] + xa_ex_offset > adj_seg_len ) ? 0x1 : 0x0 );
        

        /* Disable starting from this rule. */
        if ((s_ena == 1) && (hit_idx_v[s] == 1) && (eof_exc == 0x1))
        {
            s_ena = 0;
            state->PA_EX_STAGE  = s & 0x1F;

            if (eop == 1)
            {
                state->PA_EX_TRUNC_HEADER = 1; 
            }
            else
            {
                state->PA_EX_DEPTH_EXCEED = 1; 
            }
        }

        if (hit_idx_v[s] == 1) 
        { 
            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG EXC) Info:  exc=%0d, parsing_done=%0d (stage=%0d)\n",
                        eof_exc, xa_parsing_done, s); 
        }
        
        if ((s_ena == 1) && (hit_idx_v[s] == 1))
        {
            for (wd = 0; wd < 2; wd++)
            {
                rh_ex = rh + (wd * HLP_PA_ANA_RULES);

                status = hlpModelReadCSRMult (model->sw, 
                                              HLP_PARSER_EXT(s, rh_ex, 0), 
                                              HLP_PARSER_EXT_WIDTH,
                                              pa_ext_val);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

                xa_key_start        = (fm_byte) FM_ARRAY_GET_FIELD(pa_ext_val, HLP_PARSER_EXT, KEY_START);
                xa_key_len          = (fm_byte) FM_ARRAY_GET_FIELD(pa_ext_val, HLP_PARSER_EXT, KEY_LEN);
                xa_key_offset       = (fm_byte) FM_ARRAY_GET_FIELD(pa_ext_val, HLP_PARSER_EXT, KEY_OFFSET);
                xa_flag_num         = (fm_byte) FM_ARRAY_GET_FIELD(pa_ext_val, HLP_PARSER_EXT, FLAG_NUM);
                xa_flag_val         = (fm_bool) FM_ARRAY_GET_BIT(pa_ext_val, HLP_PARSER_EXT, FLAG_VALUE);
                xa_ptr_num          = (fm_byte) FM_ARRAY_GET_FIELD(pa_ext_val, HLP_PARSER_EXT, PTR_NUM);

                WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                            "(DBG EXT) Info:  key_start=%0d, key_len=%0d, key_offset=%0d, flag_num=%0d, "
                            "flag_val=%0d, ptr_num=%0d, ptr_value=%0d (stage=%0d)\n", xa_key_start, 
                            xa_key_len, xa_key_offset, xa_flag_num, xa_flag_val, xa_ptr_num, ptr[s], s);


                /* ********** Apply keys to target key array and track keys_valid ********** */
                /*   xa_key_len = 0 means nothing is extracted.                              */
                if (xa_key_len > 0)
                {
                    for (k=0; k < xa_key_len; k++)
                    {
                        if ((xa_key_start + k) < HLP_MAX_PA_KEY_LEN)
                        {
                            /* *************************************************************************** */
                            /* RTL Bug 29310 and WM bug 29335: clear valid bits if offset > seg len.       */
                            /* MAS Bug 29482, Comment 8: oob key ptr is compared against adjusted seg len. */
                            /* MAS Bug 29552 updated offset chk to use <= and removed even byte mask that  */
                            /*               had been applied to adj_seg_len (now allows for odd bytes)    */

                            // if (even adj seg len check)
                            // then fetch the key data and set key + valid
                            if ((fm_uint32) (ptr[s] + xa_key_offset + (k*2) + 1) <= (fm_uint32) adj_seg_len) {
                                GET_RX_DATA_WORD((ptr[s] + xa_key_offset + (k*2)), &key_val);

                                key_vld = 1;
                            }
                            else
                            {
                                key_val = 0;

                                key_vld = 0;
                            }

                            state->PA_KEYS[xa_key_start + k] = key_val;

                            state->PA_KEYS_VALID[xa_key_start + k] = (key_vld & 0x1);

                        }           // End: if ((xa_key_start + k) < HLP_MAX_PA_KEY_LEN)
                    }           // End: for (k=0; k < xa_key_len; k++)
                }           // End: if (xa_key_len > 0)

                if ((xa_flag_num != 0) && (xa_flag_num <= HLP_MAX_PA_FLAGS) &&
                    (xa_flag_num != HLP_PA_WIN_PARSE_FLAG)) // ***NOTE: Hardware set flag!!!
                {
                    /* Apply flags: downstream stages can overwrite a previouly applied flag bit (with 0 or 1). */
                    state->PA_FLAGS[xa_flag_num]        = xa_flag_val;  
                }


              //if ( (xa_ptr_num != 0) && (xa_ptr_num <= HLP_MAX_PA_PTR_NUM) && (ptr[s] < (adj_seg_len & 0xFFFFFFFE)) )
              // Last term above removed per bug 29581, Comments 1 & 2.
                if ( (xa_ptr_num != 0) && (xa_ptr_num <= HLP_MAX_PA_PTR_NUM) )
                {
                    /* Apply pointers and pointer valid bits. */
                    state->PA_PTRS[xa_ptr_num]          = ptr[s];
                    state->PA_PTRS_VALID[xa_ptr_num]    =  1;
                }
//                else 
//                {
//                    state->PA_PTRS[xa_ptr_num]          = 0;
//                    state->PA_PTRS_VALID[xa_ptr_num]    = 0;
//                }

            }   // End: for (wd=0..1)

        }   // End: if (s_ena == 1)

        if ((s_ena == 1) && (hit_idx_v[s] == 1) && (xa_parsing_done == 1))
        {
            s_ena                       = 0;
            state->PA_EX_STAGE          = s & 0x1F;
            state->PA_EX_PARSING_DONE   = 1;  
        }

    } // End: for (s=0..HLP_PA_ANA_STAGES)


    /* ********** Check if window parsing was performed (set flag accordingly) ********** */

    if (win_offset != 0) {
        state->PA_FLAGS[HLP_PA_WIN_PARSE_FLAG] = 1;
    }


    /* ********** Checksum results for outer and inner IPv4. ********** */

    state->PA_CSUM_OK = 0x0;

    for (p = 0; p < 2; p++)
    {
        switch (p)
        {
            case 0:
                pr = 2; break;
            case 1:
                pr = 6; break;
            default:
                pr = 0;
        }
        p0      = state->PA_PTRS[pr];
        p1      = state->PA_PTRS[(pr+1)] - 1;

        /* Check if pointers are valid */
        if ( (state->PA_PTRS_VALID[pr] == 1) && (state->PA_PTRS_VALID[pr+1] == 1) )
        {

            /* Compute checksum */
            csum_chk_result = calcIPv4Chksum(model, seg_data, p0, p1);

            /* Verify checksum conditions. */

            /* Bug 29283 - scenario where csm_ok not getting set as expected. */
            if ( (p1 - p0) > 64 )
            {
                WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                            "Info: NOT Setting CSUM_OK[%0d]: (ptr1 - ptr0) > 64, or (%0d - %0d) > 64 \n", p, p1, p0);

                state->PA_CSUM_OK |= 0x0 << p;
            }
            else if ( ( csum_chk_result == 0x1 ) && ( ((fm_uint32) (p1 + 1)) <= rlen ) )
            {
                WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                            "Info: [hdr ptr=%0d] Checksum valid: Setting CSUM_OK[%0d] \n", p0, p);

                state->PA_CSUM_OK |= 0x1 << p;
            }
            else
            {
                WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                            "Info: [hdr ptr=%0d] Checksum invalid: NOT Setting CSUM_OK[%0d] \n", p0, p);
            }
        }
    }   // End for (p=0..1)


    /* ****************************************************************************************** */
    /* *******************       Checksum / length validation additions       ******************* */
    /* ****************************************************************************************** */

    // initialize state variables
    state->PA_CSUM_ERR   = 0;
    state->PA_L3LEN_ERR  = 0;
    state->TAIL_CSUM_LEN = 0;

    // Fetch the related settings from PA_FLAGS that pertain to this functionality
    suppress_csum_val  = state->PA_FLAGS[HLP_PA_SUPRESS_CSUM_VAL_FLAG];
    otr_l4_udp_v       = state->PA_FLAGS[HLP_PA_OTR_L4_UDP_V_FLAG];
    otr_l4_tcp_v       = state->PA_FLAGS[HLP_PA_OTR_L4_TCP_V_FLAG];
    otr_l4_sctp_v      = state->PA_FLAGS[HLP_PA_OTR_L4_SCTP_V_FLAG];
    otr_payload_frag_v = state->PA_FLAGS[HLP_PA_OTR_PAYLOAD_FRAG_V_FLAG];
    otr_head_frag_v    = state->PA_FLAGS[HLP_PA_OTR_HEAD_FRAG_V_FLAG];

    // Fetch the related pointers from PA_PTRS that pertain to this functionality
    otr_l3_ptr        = state->PA_PTRS[HLP_OTR_L3_PTR];
    otr_l4_ptr        = state->PA_PTRS[HLP_OTR_L4_PTR];

    otr_l3_v          = state->PA_FLAGS[HLP_PA_OTR_L3_V_FLAG];
    is_ipv4           = ((otr_l3_v == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPHDR_KEY] == 1)) ? 1 : 0;
    is_ipv6           = ((otr_l3_v == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPHDR_KEY] == 0)) ? 1 : 0;

    if (is_ipv6) {
        l3_vld_chk = ((state->PA_PTRS_VALID[HLP_OTR_L3_PTR] == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPHDR_KEY + 4] == 1) && // IPv6 length location
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY] == 1)      && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 1] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 2] == 1)  && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 3] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 4] == 1)  && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 5] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 6] == 1)  && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 7] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 8] == 1)  && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 9] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 10] == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 11] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 12] == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 13] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 14] == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 15] == 1));
    } else { // assumed to be IPv4
        l3_vld_chk = ((state->PA_PTRS_VALID[HLP_OTR_L3_PTR] == 1) && (state->PA_KEYS_VALID[HLP_OTR_IPHDR_KEY + 1] == 1) && // IPv4 length location
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY] == 1)      && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 1] == 1) &&
                      (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 2] == 1)  && (state->PA_KEYS_VALID[HLP_OTR_IPADDR_KEY + 3] == 1));
    }
    l4_vld_chk = ((((state->PA_KEYS_VALID[HLP_L4LEN_KEY] == 1) && otr_l4_udp_v) || otr_l4_tcp_v || otr_l4_sctp_v) &&
                  (state->PA_PTRS_VALID[HLP_OTR_L4_PTR] == 1));

    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "(DBG CSUM) Info:  suppress_l4_csum_val=%0d, otr_payload_frag_v=%0d, otr_head_frag_v=%0d\n",
                suppress_csum_val, otr_payload_frag_v, otr_head_frag_v);
    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "(DBG CSUM) Info:  otr_l4_udp_v=%0d, otr_l4_tcp_v=%0d, otr_l4_sctp_v=%0d\n",
                otr_l4_udp_v, otr_l4_tcp_v, otr_l4_sctp_v);

    // Fetch the appropriate PARSER_CSUM_CFG entry
    status = hlpModelReadCSRMult (model->sw, 
                                 HLP_PARSER_CSUM_CFG(state->RX_PORT, 0), 
                                 HLP_PARSER_CSUM_CFG_WIDTH,
                                 pa_csum_cfg_val);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    val_l4_csum         = (fm_byte)   FM_ARRAY_GET_FIELD(pa_csum_cfg_val, HLP_PARSER_CSUM_CFG, VALIDATE_L4_CSUM);
    store_l4_partial    = (fm_byte)   FM_ARRAY_GET_BIT(pa_csum_cfg_val, HLP_PARSER_CSUM_CFG, STORE_L4_PARTIAL_CSUM);
    compute_l4_csum     = (fm_byte)   FM_ARRAY_GET_BIT(pa_csum_cfg_val, HLP_PARSER_CSUM_CFG, COMPUTE_L4_CSUM);
    val_l3_len          = (fm_byte)   FM_ARRAY_GET_FIELD(pa_csum_cfg_val, HLP_PARSER_CSUM_CFG, VALIDATE_L3_LENGTH);

    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
               "(DBG CSUM) Info:  ValidateL4Csum=%0d, StoreL4PartialCsum=%0d,"
               " ComputeL4Csum=%0d, ValidateL3Len=%0d\n",
               val_l4_csum, store_l4_partial, compute_l4_csum, val_l3_len);


    if (state->PKT_META[HLP_PKT_META_TYPE_OFFSET] == HLP_DSI_TX_TYPE) {
        dsi_tx_flags_bit1 = (state->PKT_META[HLP_PKT_META_DSI_TX_FLAGS_OFFSET] >> 1) & 0x1;
    }
    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "(DBG CSUM) Info:  PKT_META_TYPE=0x%02x (DSI_TX=0x16),   DSI_TX_FLAGS[1]=%0d\n",
                state->PKT_META[HLP_PKT_META_TYPE_OFFSET], dsi_tx_flags_bit1);


    //////////////////////////
    // Calculate the pointers
    //////////////////////////

    // start_ptr = i_window_offset + i_l4_ptr;
    // end_ptr = i_window_offset + i_l3_ptr + (i_ipv6_v ? (i_ipv6_len + 40) : i_ipv4_len)
    //           - (i_window_parse_v ? i_seg.s.pkt_meta.lan_rx_common.esp_trailer_len : 0);
    start_ptr = win_offset + otr_l4_ptr;
    end_ptr = win_offset + otr_l3_ptr;
    if (is_ipv6) {
        l3_len = (state->PA_KEYS[HLP_OTR_IPHDR_KEY + 4]); // IPv6 Length field (2 empty keys precede IPv6 hdr)

        end_ptr += l3_len + 40; // add 40 (for the fixed IPv6 header size which isn't included in the len field)
    }
    else {
        l3_len = state->PA_KEYS[HLP_OTR_IPHDR_KEY + 1]; // IPv4 Length field

        end_ptr += l3_len;
    }
//    if (win_offset != 0) {
//        esp_trailer_len = ( ( (state->PKT_META[HLP_PKT_META_EHL_OFFSET + 1] & 0x7F) << 2 ) |   // Upper seven bits are in byte 17
//                            ( (state->PKT_META[HLP_PKT_META_EHL_OFFSET] & 0xC0) >> 6 ) );        // Lower two bits are in byte 16
//        end_ptr -= esp_trailer_len;
//    }
//    DISPLAY2("(DBG CSUM) Info:  is_ipv4=%0d / is_ipv6=%0d   (start_ptr=%0d / end_ptr=%0d)  {esp_trailer_len=%0d}\n", is_ipv4, is_ipv6, start_ptr, end_ptr, esp_trailer_len);
    WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                "(DBG CSUM) Info:  is_ipv4=%0d / is_ipv6=%0d"
                "   (start_ptr=%0d / end_ptr=%0d)\n",
                is_ipv4, is_ipv6, start_ptr, end_ptr);

    // hdr_csum_start_ptr = start_ptr + (i_tcp_v ? 16 : i_udp_v ? 6 : i_sctp_v ? 8 : 0);
    if (otr_l4_tcp_v == 1) { csum_start_ptr = start_ptr + 16; }
    else if (otr_l4_udp_v == 1) { csum_start_ptr = start_ptr + 6; }
    else if (otr_l4_sctp_v == 1) { csum_start_ptr = start_ptr + 8; }
    else { csum_start_ptr = 0; }
    // hdr_csum_end_ptr = start_ptr + (i_tcp_v ? 18 : i_udp_v ? 8 : i_sctp_v ? 12 : 0);
    if (otr_l4_tcp_v == 1) { csum_end_ptr = start_ptr + 18; }
    else if (otr_l4_udp_v == 1) { csum_end_ptr = start_ptr + 8; }
    else if (otr_l4_sctp_v == 1) { csum_end_ptr = start_ptr + 12; }
    else { csum_end_ptr = 0; }


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Determine which checksum case is enabled
    // per MAS: if all enabled, precedence should be: ValidateL4Checksum, StoreL4PartialChecksum, ComputeL4Checksum
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // ValidateL4Checksum is enabled when (i_parser_cksum_cfg.ValidateL4Checksum != 0) && ~i_fragment_v && ~i_disable && (implicit valid bits)
    if ((val_l4_csum != 0) && (otr_payload_frag_v == 0) && (otr_head_frag_v == 0) && (suppress_csum_val == 0) && l3_vld_chk && l4_vld_chk &&
        ((otr_l4_tcp_v && (state->PA_KEYS_VALID[HLP_L4CSUM_KEY] == 1)) ||
         (otr_l4_sctp_v && (state->PA_KEYS_VALID[HLP_L4CSUM_KEY] == 1) && (state->PA_KEYS_VALID[HLP_L4CSUM_KEY+1] == 1)) ||
         (otr_l4_udp_v && (state->PA_KEYS[HLP_L4CSUM_KEY] != 0) && (state->PA_KEYS_VALID[HLP_L4CSUM_KEY] == 1)))) {
        validate_csum_en = 1;

        if (win_offset != 0)
            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG CSUM) Info:  Window Parsing / ValidateL4Csum scenario hit!\n");

        // TODO -- remove! (being removed from tail_info interface
        // (SAF_MATRIX in Tail now required to be programmed correctly)
        state->PA_FORCE_SAF = 1;
    }
    // StoreL4PartialChecksum is enabled when (i_parser_cksum_cfg.StoreL4PartialChecksum != 0) && i_fragment_v && i_head_frag_v
    else if ((store_l4_partial != 0) && (otr_head_frag_v == 1)) {
//    // StoreL4PartialChecksum is enabled when (i_parser_cksum_cfg.StoreL4PartialChecksum != 0) && i_fragment_v && i_head_frag_v && ~i_disable
//    else if ((store_l4_partial != 0) && (otr_head_frag_v == 1) && (suppress_csum_val == 0)) {
        store_partial_csum_en = 1;
    }
    // ComputeL4Checksum is enabled when (i_parser_cksum_cfg.ComputeL4Checksum != 0) && ~i_fragment_v && i_seg.s.pkt_meta.dsi_tx_common.typ==DSI_TX
    //                                    && i_seg.s.pkt_meta.dsi_tx_common.tx_flags[1] 
    else if ((compute_l4_csum != 0) && (state->PKT_META[HLP_PKT_META_TYPE_OFFSET] == HLP_DSI_TX_TYPE) && (dsi_tx_flags_bit1 == 1) &&
             (otr_payload_frag_v == 0) && (otr_head_frag_v == 0) && (otr_l4_tcp_v || otr_l4_sctp_v || otr_l4_udp_v) && l3_vld_chk && l4_vld_chk) {
//    // ComputeL4Checksum is enabled when (i_parser_cksum_cfg.ComputeL4Checksum != 0) && i_seg.s.pkt_meta.dsi_tx_common.typ==DSI_TX && i_seg.s.pkt_meta.dsi_tx_common.tx_flags[1]
//    //                                    && ~i_disable && (implicit valid bits)
//    else if ((compute_l4_csum != 0) && (state->PKT_META[HLP_PKT_META_TYPE_OFFSET] == HLP_DSI_TX_TYPE) && (dsi_tx_flags_bit1 == 1) && (suppress_csum_val == 0)) {
        compute_csum_en = 1;
    }


CHECKSUM:

    /////////////////////////////////////////////////////////////////
    // allocate memory for the byte array to do the csum calculation
    /////////////////////////////////////////////////////////////////
    {
      fm_uint csum_sz;
      const fm_uint HLP_PSEUDOHEADER_SIZE = 40; /* should be better documented */

      csum_sz = MAX(HLP_PA_MAX_DATA_SZ, HLP_PSEUDOHEADER_SIZE + rlen - 4);
      csum_bytes = fmAlloc(csum_sz);
    }


    /////////////////////////////////////////////////////////////////////////////////////////////////
    // Prepare pseudoheaders -- UDP header includes length / TCP must be calculated (using pointers)
    //                          (SCTP no pseudoheader)
    /////////////////////////////////////////////////////////////////////////////////////////////////
    if ((validate_csum_en == 1) || (compute_csum_en == 1)) { // no pseudoheader for Partial csum case (software will recompute)
        if (is_ipv6 && ((otr_l4_tcp_v == 1) || (otr_l4_udp_v == 1))) {
            for (ph = 0; ph < 16; ph++) {
                csum_bytes[ph*2] = (state->PA_KEYS[HLP_OTR_IPADDR_KEY + ph] >> 8) & 0xFF;
                csum_bytes[(ph*2)+1] = state->PA_KEYS[HLP_OTR_IPADDR_KEY + ph] & 0xFF;
            }
            csum_bytes[32] = 0;
            csum_bytes[33] = 0;
            csum_bytes[36] = 0;
            csum_bytes[37] = 0;
            csum_bytes[38] = 0;
            if (otr_l4_udp_v == 1) { // Length comes from the UDP header
                csum_bytes[34] = (state->PA_KEYS[HLP_L4LEN_KEY] >> 8) & 0xFF;
                csum_bytes[35] = state->PA_KEYS[HLP_L4LEN_KEY] & 0xFF;
                csum_bytes[39] = 0x11; // set protocol to UDP
            } else {
                l4_len = end_ptr - start_ptr; // must calculate the length
                csum_bytes[34] = (l4_len >> 8) & 0xFF;
                csum_bytes[35] = l4_len & 0xFF;
                csum_bytes[39] = 0x06; // set protocol to TCP
            }
            // set the byte pointer to the end of the pseudoheader
            cb_ptr = 40;
        } else if ((otr_l4_tcp_v == 1) || (otr_l4_udp_v == 1)) { // NOTE: assume is_ipv4 if is_ipv6 isn't true
            for (ph = 0; ph < 4; ph++) {
                csum_bytes[ph*2]     = (state->PA_KEYS[HLP_OTR_IPADDR_KEY + ph] >> 8) & 0xFF;
                csum_bytes[(ph*2)+1] = state->PA_KEYS[HLP_OTR_IPADDR_KEY + ph] & 0xFF;
            }
            csum_bytes[8] = 0;
            if (otr_l4_udp_v == 1) { // Length comes from the UDP header
                csum_bytes[9] = 0x11; // set protocol to UDP
                csum_bytes[10] = (state->PA_KEYS[HLP_L4LEN_KEY] >> 8) & 0xFF;
                csum_bytes[11] = state->PA_KEYS[HLP_L4LEN_KEY] & 0xFF;
            } else {
                csum_bytes[9] = 0x06; // set protocol to TCP
                l4_len = end_ptr - start_ptr; // must calculate the length
                csum_bytes[10] = (l4_len >> 8) & 0xFF;
                csum_bytes[11] = l4_len & 0xFF;
            }
            // set the byte pointer to the end of the pseudoheader
            cb_ptr = 12;
        }
    } // else leave cb_ptr == 0 (no pseudoheader)


    /////////////////////////////
    // populate the payload data
    /////////////////////////////
    if ((validate_csum_en == 1) || (store_partial_csum_en == 1) || (compute_csum_en == 1)) {
        for (pl = start_ptr; ((pl < end_ptr) & (cb_ptr < HLP_PA_MAX_DATA_SZ) & (pl < rlen)); pl++) {
            if ((pl >= csum_start_ptr) && (pl < csum_end_ptr)) {
                csum_bytes[cb_ptr] = 0; // zero out the csum bytes
            } else {
                csum_bytes[cb_ptr] = state->RX_DATA[pl];
            }
            cb_ptr++;
        }
//        for (pl = 0; pl < cb_ptr; pl++) {
//            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "(DBG CSUM) Info: csum_bytes[%0d] = 0x%02x\n", pl, csum_bytes[pl]);
//        }
    } else {
        // Default case 16b 1s complement sum, byte 192 to FCS
        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG CSUM) Default case (1s complement sum from byte 192 to FCS)\n");
        if ((rlen-4) > 192) {
            for (pl = 192; pl < (rlen-4); pl++) {
                csum_bytes[cb_ptr] = state->RX_DATA[pl];
                cb_ptr++;
            }
            calc_l4_csum = calcGenChksum(csum_bytes, cb_ptr);
        } else {
            calc_l4_csum = 0;
        }
//        for (pl = 0; pl < cb_ptr; pl++) {
//            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "(DBG CSUM) Info: csum_bytes[%0d] = 0x%02x\n", pl, csum_bytes[pl]);
//        }
    }


    //////////////////////////////////////
    // Perform actual checksum operations
    //////////////////////////////////////
    if (validate_csum_en) {

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 30, 3, 0x1); // set l4csum_len struct 'code' value

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 16, 14, rlen); // set l4csum_len 'packet length' value

        if ((otr_l4_tcp_v == 1) || (otr_l4_udp_v == 1)) {
            l4_csum = state->PA_KEYS[HLP_L4CSUM_KEY]; // located in key 32 per MAS and bug #32074

            calc_l4_csum = calcGenChksum(csum_bytes, cb_ptr);
            if ((otr_l4_udp_v == 1) && (calc_l4_csum == 0)) {
                WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "Info: UDP Calculated csum is 0x0, inverting to 0xFFFF!\n");
                calc_l4_csum = 0xFFFF;
            }
            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG CSUM) Info: actual csum=0x%08x   calculated csum=0x%08x\n",
                        l4_csum, calc_l4_csum);

            // check for UDP checksum of all zeroes (not present / don't validate)
            ///////////////////////////////////////////////////////////////////////
            if (((otr_l4_tcp_v == 1) || ((otr_l4_udp_v == 1) && (l4_csum != 0))) && (calc_l4_csum != l4_csum)) {
                // if val_l4_csum == 1, validate and drop
                if (val_l4_csum == 1) {
                    state->PA_DROP = 1;

                    FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 2, 1, 0x1); // also mark drop in l4csum_len struct
                }

                state->PA_CSUM_ERR = 1;

                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 1, 0x1); // also mark l4 csum error in l4csum_len struct
            }
        } else if (otr_l4_sctp_v == 1) {
            l4_csum = ( ( ( state->PA_KEYS[HLP_L4CSUM_KEY] << 16 ) & 0xFFFF0000 ) |
                        ( ( state->PA_KEYS[HLP_L4CSUM_KEY + 1] )   & 0x0000FFFF ) );

            calc_l4_csum = fmCrc32C(csum_bytes, cb_ptr);

            // Byte swap (based on rfc3309 generate_crc32c() function example)
            calc_l4_csum = ((calc_l4_csum >> 24) & 0xff) |
                           ((calc_l4_csum >> 8) & 0xff00) |
                           ((calc_l4_csum << 8) & 0xff0000) |
                           ((calc_l4_csum << 24) & 0xff000000);
            WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                        "(DBG CSUM) Info: actual csum=0x%08x   calculated csum=0x%08x\n",
                        l4_csum, calc_l4_csum);

            if (calc_l4_csum != l4_csum) {
                // if val_l4_csum == 1, validate and drop
                if (val_l4_csum == 1) {
                    state->PA_DROP = 1;

                    FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 2, 1, 0x1); // also mark drop in l4csum_len struct
                }

                state->PA_CSUM_ERR = 1;

                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 1, 0x1); // also mark error in l4csum_len struct
            }
        }

    }
    else if (store_partial_csum_en) {

        if ((otr_l4_tcp_v == 1) || (otr_l4_udp_v == 1)) {

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 30, 3, 0x3); // set l4csum_len struct 'code' value

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 16, 14, rlen); // set l4csum_len 'packet length' value

            /////////////////////////////////////////////////////////////////////////////////////////////
            // Store case is "not complemented" per bug #32144
            // (gets inverted when setting L4CSUM_LEN field to undo complement already done in function)
            /////////////////////////////////////////////////////////////////////////////////////////////
            calc_l4_csum = calcGenChksum(csum_bytes, cb_ptr);

            // Note: since UDP doesn't allow 0 csum, design sets this "pre-complemented value" to 0 (so the final csum will be 0xffff)
            // (This is done to match design behavior, technically the store case can represent 0 in either form since it's an intermediate value)
            if (calc_l4_csum == 0) {
                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, calc_l4_csum); // set l4csum_len 'Partial checksum' value
            } else {
                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, ~calc_l4_csum); // set l4csum_len 'Partial checksum' value
            }

        } else if (otr_l4_sctp_v == 1) {

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 32, 1, 0x1); // set l4csum_len struct 'code' value

            // DON'T set l4csum_len 'packet length' value for SCTP Partial csum case

            // check for invalid case where no data is available for crc calculation
            if (cb_ptr > 0) {
                ////////////////////////////////////////////////////////////////////////
                // Store case is "not complemented" per bug #32144
                // (invert function output to undo complement already done in function)
                ////////////////////////////////////////////////////////////////////////
                calc_l4_csum = ~( fmCrc32C(csum_bytes, cb_ptr) );

                // Byte swap (based on rfc3309 generate_crc32c() function example)
                calc_l4_csum = ((calc_l4_csum >> 24) & 0xff) |
                               ((calc_l4_csum >> 8) & 0xff00) |
                               ((calc_l4_csum << 8) & 0xff0000) |
                               ((calc_l4_csum << 24) & 0xff000000);
            } else {
                calc_l4_csum = 0;
            }

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 32, calc_l4_csum); // set l4csum_len 'SCTP Partial checksum' value

        } else {

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 30, 3, 0x3); // set l4csum_len struct 'code' value

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 16, 14, rlen); // set l4csum_len 'packet length' value

            /////////////////////////////////////////////////////////////////////////////////////////////
            // Store case is "not complemented" per bug #32144
            // (gets inverted when setting L4CSUM_LEN field to undo complement already done in function)
            /////////////////////////////////////////////////////////////////////////////////////////////
            calc_l4_csum = calcGenChksum(csum_bytes, cb_ptr);

            // Note: since UDP doesn't allow 0 csum, design sets this "pre-complemented value" to 0 (so the final csum will be 0xffff)
            if (calc_l4_csum == 0) {
                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, calc_l4_csum); // set l4csum_len 'Partial checksum' value
            } else {
                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, ~calc_l4_csum); // set l4csum_len 'Partial checksum' value
            }

        }

    }
    else if (compute_csum_en) {

        if ((otr_l4_tcp_v == 0) && (otr_l4_udp_v == 0) && (otr_l4_sctp_v == 1)) {
            WM_DISPLAY(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER, "Info: Compute / SCTP scenario (invalid), expect design mismatch for TAIL_CSUM_LEN!\n");
        }

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 30, 3, 0x2); // set l4csum_len struct 'code' value

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 16, 14, rlen); // set l4csum_len 'packet length' value

//        if (otr_l4_udp_v == 1) { // only UDP case for ComputeL4Checksum case (DSI Egress -- Ipsec with NAT-T only, other cases will already have correct csum & then encrypted)
//            calc_l4_csum = calcGenChksum(csum_bytes, cb_ptr);
//
//            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, calc_l4_csum); // set l4csum_len 'UDP checksum' value
//        }

        // Compute case IS "complemented" per bug #32144 (same form returned by function)
        //////////////////////////////////////////////////////////////////////////////////
        calc_l4_csum = calcGenChksum(csum_bytes, cb_ptr);

        // for UDP 0 csum must be represented as 0xffff (TCP can be either way)
        if (calc_l4_csum == 0) { calc_l4_csum = ~calc_l4_csum; }

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, calc_l4_csum); // set l4csum_len 'UDP checksum' value

    } else { // default case

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 30, 3, 0x0); // set l4csum_len struct 'code' value

        FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 16, 14, rlen); // set l4csum_len 'packet length' value

        // Note: since UDP doesn't allow 0 csum, design sets this "pre-complemented value" to 0 (so the final csum will be 0xffff)
        if (((rlen-4) > 192) && (calc_l4_csum != 0)) {
            // Default case is "not complemented" per bug #32144
            /////////////////////////////////////////////////////
            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, ~calc_l4_csum); // set l4csum_len 'checksum' value
        } else {
            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 0, 16, calc_l4_csum); // set l4csum_len 'checksum' value (calc_l4_csum should be 0 since pkt was < 192B)
        }

    }


    /* ***************************************************************************** */
    /* *******************       Length validation section       ******************* */
    /* ***************************************************************************** */
    if ((val_l3_len != 0) && (compute_csum_en == 0) && l3_vld_chk) {
        if (is_ipv6) { min_pkt_len = l3_len + win_offset + otr_l3_ptr + 40 + 4; }
        else         { min_pkt_len = l3_len + win_offset + otr_l3_ptr + 4; }

        WM_DISPLAY2(parserDisplayVerbose, FM_LOG_CAT_MODEL_PARSER,
                    "(DBG CSUM) Info: rlen=%0d / min_pkt_len=%0d (l3_len=%0d,"
                    " otr_l3_ptr=%0d, is_ipv6=%0d)\n",
                    rlen, min_pkt_len, l3_len, otr_l3_ptr, is_ipv6);

        if (rlen < min_pkt_len) {
            state->PA_L3LEN_ERR = 1;
        }

        // Per Internal Interfaces MAS:
        // 3b’001 = ValidateL4Checksum: contains packet length and error flags. This format may be used due either
        //          to the ValidateL4 offload having been selected, or due to the L3 length check having failed
        //          while performing some other offload.
        // *** Note wording that only used for L3 length check when it failed...
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////
        if (state->PA_L3LEN_ERR != 0) {
            ///////////////////////////////////////////////////////////////////////////////////////////////////////
            // per "Fragment Handling" table in Parser HAS, L3 Len Validation overrides csum offload for fragments
            ///////////////////////////////////////////////////////////////////////////////////////////////////////
            if (((validate_csum_en == 0) && (store_partial_csum_en == 0) && (compute_csum_en == 0)) || (store_partial_csum_en)) {
                state->TAIL_CSUM_LEN = 0; // clear any results that were stored for StoreL4Partial case ...or from "default case" (if all were disabled)
            }

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 30, 3, 0x1); // set l4csum_len struct 'code' value

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 16, 14, rlen); // set l4csum_len 'packet length' value

            if (val_l3_len == 1) {
                state->PA_DROP = 1;

                FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 2, 1, 0x1); // also mark drop in l4csum_len struct
            } else if (val_l3_len == 2) {
                // check if need to override ValidateL4Csum results
                ////////////////////////////////////////////////////
                if ((validate_csum_en) && (state->PA_CSUM_ERR == 1) && (val_l4_csum == 1)) {
                    state->PA_DROP = 0; // clear DROP

                    FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 2, 1, 0x0); // also CLEAR drop in l4csum_len struct
                }
            }

            FM_SET_UNNAMED_FIELD64(state->TAIL_CSUM_LEN, 1, 1, 0x1); // also mark l3 len error in l4csum_len struct
        }
    }


    /* ***************************************************************************** */
    /* *******************        Assign PARSER_PKT_META         ******************* */
    /* ***************************************************************************** */

    for (i = 0; i < 32; i++)
    {
        state->PARSER_PKT_META[i] = state->PKT_META[i] & 0xFF;
    }


    /////////////////////////////////////////////////////////////////////////
    // release the memory used for the byte array to do the csum calculation
    /////////////////////////////////////////////////////////////////////////
    fmFree(csum_bytes);


EXIT:

#ifdef PA_DUMP 
    hlpModelDbgDumpParserState(model, hit_idx, hit_idx_v);
#endif
    
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpParser);

    return;

ABORT:
    FM_LOG_ERROR(FM_LOG_CAT_MODEL_PARSER, "Reached ABORT at end of Parser stage\n");

    return;

}


//  /*****************************************************************************/
//  /** hlpModelParserWriteCSR
//   * \ingroup intModel
//   *
//   * \desc            Processes CSR writes pertinent to the Parser HLP white
//   *                  model stage.
//   *
//   * \param[in]       model points to the switch model state.
//   *
//   * \param[in]       addr is the register address.
//   *
//   * \param[in]       value is the 32-bit value to write.
//   *
//   * \return          FM_OK if successful.
//   * \return          Other ''Status Codes'' as appropriate in case of failure.
//   *
//   *****************************************************************************/
//  fm_status hlpModelParserWriteCSR(hlp_model        *model,
//                                       fm_uint32     addr,
//                                       fm_uint32     value)
//  {
//   fm_status   status = FM_OK;
//  
//   HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM, 
//                                "model=%p addr=0x%x value=0x%x\n",
//                                (void *) model,
//                                addr,
//                                value);
//      /* PLACEHOLDER, if necessary. */
//
//    
//    status = FM_OK;
//    DISPLAY("Inside hlpModelParserWriteCSR as a placeholder.\n");
//  
//  // ABORT:
//  //     HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, status);
//  
//  } /* hlpModelParserWriteCSR */
  
