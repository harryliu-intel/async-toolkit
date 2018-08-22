// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_rxtotx.h"
#include "mby_modifier.h"

void Modifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyTxInToModifier     * const in,
          mbyModifierToTxStats  * const out
)
{
    // Read inputs:
    fm_bool   no_modify         = in->NO_MODIFY;  // skip most of modifications in Modifier
    fm_uint32 rx_length         = in->RX_LENGTH;  // ingress packet data length [bytes]
    fm_byte  *rx_data           = in->RX_DATA;
    fm_bool   tx_drop           = in->TX_DROP;
    fm_byte   tx_tag            = in->TX_TAG;
    fm_uint32 tx_stats_last_len = in->TX_STATS_LAST_LEN;
    
    // input from the outside:
    fm_byte *packet;

    // Chunked Packet:
    mbyModRegData reg_data;

//> GetModRegData(key, model, &reg_data);

    mbyModControlData ctrl_data;
    
//> CalcIngressCRC(key, model, &ctrl_data);

    mbyChunkedSeg chunked_seg;

//> InitChunkedSeg(key, model, packet, &chunked_seg);

//> InitControl(key, model, &reg_data, &ctrl_data, &chunked_seg);

    // L2 Modifications:

//> GetRxL2Tags(key, model, &ctrl_data, &chunked_seg);

//> DropPacket(key, model, &ctrl_data);

//> VlanLookup(key, model, &reg_data, &ctrl_data, &chunked_seg);

    // DMAC/SMAC update:

//> ctrl_data.isRoutable = ctrl_data.routeA && (tx_tag == MBY_NORMAL_TAGGING) && !(ctrl_data.isMirror);

//> UpdateMacAddrIPP(key, model, &reg_data, &ctrl_data, &chunked_seg);

    // Step 3 in EAS: Construct VLAN Tags (and copy to output):

//> UpdateVlanIPP(key, model, &reg_data, &ctrl_data, &chunked_seg);

    fm_uint32 tx_length = 0; // egress packet data length [bytes]

//> PackPacket(packet, no_modify, rx_length, rx_data, &chunkedSeg, &ctrl_data, &tx_length, key);

//> MiscOps(key, model, &reg_data, &ctrl_data, &chunkedSeg, packet); // if minFrameSize, updatePktmeta use min size

    fm_uint32 tx_stats_length = 0; //> (!ctrl_data.mirrorTrunc && !tx_drop) ? ctrl_data.egressSeg0Bytes + tx_stats_last_len : ctrl_data.refcnt_tx_len;

    // Write outputs:
    out->TX_LENGTH       = tx_length;
    out->TX_STATS_LENGTH = tx_stats_length;
    out->SEG_DROP        = tx_drop;
}

#if 0 // HLP code trimmed down to a minumum for the EOM MBY sprint by Arek/Andrea -- for reference only:

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define GET_BYTE(value, offset)                                 \
    ( (fm_byte) (((value) >> offset) & 0xFF) )

/* conditional print, saves line-length real estate */
#define PRINT(wmDisplayVerbose, key, ...)                        \
    do {                                                        \
        if (wmDisplayVerbose >= 1){                  \
            FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MODIFY, "dxb[%s]: ", key);     \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG, __VA_ARGS__);      \
        }                                                       \
    }while(0);

#define PRINTF(wmDisplayVerbose, ...)                            \
    do {                                          \
        if(wmDisplayVerbose >= 1){                   \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG, __VA_ARGS__);      \
        }                                            \
    }while(0);

#define PRINT2(wmDisplayVerbose, key, ...)                        \
    do {                                         \
        if (wmDisplayVerbose >= 2){                  \
            FM_LOG_DEBUG2(FM_LOG_CAT_MODEL_MODIFY, "dxb[%s]: ", key);     \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG2, __VA_ARGS__);      \
        }                                         \
    }while(0);

#define PRINTF2(wmDisplayVerbose, ...)                            \
    do {                                         \
        if(wmDisplayVerbose >= 2){                   \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG2, __VA_ARGS__);      \
        }                                            \
    }while(0);

#define PRINT3(wmDisplayVerbose, key, ...)                        \
    do {                                         \
        if (wmDisplayVerbose == 3){                  \
            FM_LOG_DEBUG3(FM_LOG_CAT_MODEL_MODIFY, "dxb[%s]: ", key);     \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG3, __VA_ARGS__);      \
        }                                         \
    }while (0);

#define PRINTF3(wmDisplayVerbose, ...)                            \
    do {                                         \
        if(wmDisplayVerbose == 3){                   \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG3, __VA_ARGS__);      \
        }                                            \
    }while(0);

#define VLAN_TAG_BYTES          4
#define MAC_ADDR_BYTES          6
#define DEFAULT_SEGMENT_BYTES   192
#define MIN_EGRESS_BYTES        18

enum // as per index
{//                         index          priority
    DISP_TXECCDROP      =     0,             //1         pm_read.err
    DISP_TXERRORDROP    =     1,             //2         mod_ctrl.tx_drop
    DISP_MARKERERRDROP  =     2,             //3
    DISP_L3ERRDROP      =     3,             //3
    DISP_L4ERRDROP      =     4,             //3
    DISP_L3L4ERRMARK    =     5,             //3
    DISP_TIMEOUTDROP    =     6,             //4         mod_ctrl.is_timeout
    DISP_LOOPBACKDROP   =     7,             //5
    DISP_TTL1DROP       =     8,             //6         fwd_modify.ttl1
    DISP_ECNDROP        =     9,             //7
    DISP_MODERRORDROP   =     10,            //8
    DISP_TXERROR        =     11,            //9
    DISP_OOMTRUNC       =     12,            //10
    DISP_UCAST          =     13,            //11
    DISP_BCAST          =     14,            //12
    DISP_MCAST          =     15             //13
};

enum
{
    IS_OK = 0,
    EOP_ERROR,
    NOT_ENOUGH_RX_DATA,
    INVALID_IP_VERSION,
    COPY_ERROR
};

enum
{
    NODROP = 0,
    DROP = 1
};

enum
{
    NOSKIP = 0,
    SKIP = 1
};

enum
{
    NOLOG = 0,
    LOG = 1
};

enum // REASON CODE for MOD_UPDATE_DEBUG_RECORD
{
    ERR_VLAN_TAG_FULL  = 0x01, // done
    ERR_OTR_L2_FULL    = 0x02, // done
    ERR_INR_L2_FULL    = 0x03, // done
    ERR_VLAN_TAG_EMPTY = 0x04, //      the desired deleted vid not present
    ERR_OTR_L2_EMPTY   = 0x05, // done can't remove the number as per mode
    ERR_INR_L2_EMPTY   = 0x06, // done
    ERR_PUSH_AL        = 0x07, // done
    ERR_PUSH_ELI       = 0x08, // done
    ERR_PUSH_G         = 0x09, // done
    ERR_POP_AL         = 0x0a, // done
    ERR_POP_ELI        = 0x0b, // done
    ERR_POP_G          = 0x0c, // done
    ERR_OTR_IPV        = 0x0d, // done
    ERR_INR_IPV        = 0x0e, // done
    ERR_TTLDS_TGT      = 0x0f, // done
    ERR_TTLDS_SRC      = 0x10, //
    ERR_TTL_0          = 0x11, // done
    ERR_PKT_LEN        = 0x12, //
    ERR_SEG_G_320      = 0x13,
    ERR_SEG_S_18       = 0x14,
    ERR_OTR_MAC        = 0x15,
    ERR_INR_MAC        = 0x16,
    ERR_OTR_IP         = 0x17,
    ERR_INR_IP         = 0x18,
    ERR_OTR_L4         = 0x19,
    ERR_INR_L4         = 0x1a
};

enum { // interrupt disp
    INTR_MEM_ECC_ERR                 = 0,
    INTR_U_INSERT_VLAN_IPP           = 1, 
    INTR_U_INSERT_L2_TAG_OTR         = 2,
    INTR_U_INSERT_L2_TAG_INR         = 3,
    INTR_U_REMOVE_VLAN_IPP           = 4,
    INTR_U_REMOVE_L2_TAG_OTR         = 5,
    INTR_U_REMOVE_L2_TAG_INR         = 6,
    INTR_U_PUSH_AL                   = 7,
    INTR_U_PUSH_ELI                  = 8,
    INTR_U_PUSH_G                    = 9,
    INTR_U_POP_AL                    = 10,
    INTR_U_POP_ELI                   = 11,
    INTR_U_POP_G                     = 12,
    INTR_OTR_IPV_MISMATCH            = 13,
    INTR_INR_IPV_MISMATCH            = 14,
    INTR_TTLDS_NON_TGT               = 15,
    INTR_TTLDS_NON_SRC               = 16,
    INTR_TTL_DEC_ERR                 = 17,
    INTR_PKT_LEN_MD_UPDATE_ERR       = 18,
    INTR_BIGGER_320B                 = 19,
    INTR_SMALLER_18B                 = 20,
    INTR_OTR_MAC_NONEXIST            = 21,
    INTR_INR_MAC_NONEXIST            = 22,
    INTR_OTR_IP_NONEXIST             = 23,
    INTR_INR_IP_NONEXIST             = 24,
    INTR_OTR_L4_NONEXIST             = 25,
    INTR_INR_L4_NONEXIST             = 26,
    INTR_TX_ECC_DROP                 = 27,
    INTR_TX_ERR_DROP                 = 28,
    INTR_MARKER_ERR_DROP             = 29,
    INTR_L3_LEN_ERR_DROP             = 30,
    INTR_L4_CSUM_ERR_DROP            = 31, 
    INTR_L3_LEN_L4_CSUM_ERR_MASK     = 32,
    INTR_TIMEOUT_DROP                = 33,
    INTR_LPBK_DROP                   = 34,
    INTR_TTL1_DROP                   = 35,
    INTR_ECN_DROP                    = 36
};
// TODO
// ds_src > n_otr_g or n_otr_mpls

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_int      modDisplayVerbose;    

#ifdef IPV4_CSUM_RECALC
    fm_bool     ipv4CsumRecalc = 1;
#else
    fm_bool     ipv4CsumRecalc = 0;
#endif

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** CopyVlanToOutput
 * \ingroup intModel
 *
 * \desc            Copies VLAN tag data to output buffer
 *
 * \param[in]       etype is the TPID value (first 16 bits of tag)
 *
 * \param[in]       vpri is the PCP/DEI value (next 4 bits of tag)
 *
 * \param[in]       vid is the VID value (next 12 bits of tag)
 *
 * \param[out]      tags is pointing to the structure that is holding the
 *                  output vlan tags.
 *
 * \param[in]       vlan2 adjusts the index of the otr_tags
 *
 * \return          None
 *
 *****************************************************************************/
static void CopyVlanToOutput(fm_uint16           etype,
                             fm_byte             vpri,
                             fm_uint16           vid,
                             fm_byte             *tags,
                             fm_bool             vlan2)
{
	fm_uint32 idx;
	idx = 0;
	if (vlan2)
	{
		idx = 4;
	}
    *(tags+idx)   = (etype >> 8) & 0xFF;
    *(tags+idx+1) = (etype & 0xFF);
    *(tags+idx+2) = (vpri << 4) | (vid >> 8);
    *(tags+idx+3) = (vid & 0xFF);
}

/*****************************************************************************/
/** GetVPRI
 * \ingroup intModel
 *
 * \desc            Reads a VPRI map and returns the correct 4 bits
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       whichVlan is 1 for vlan1, 2 for vlan2
 *
 * \param[in]       vpri is vpri from frame processor, used as index
 *
 * \return          4-bit VPRI value for which_vlan, indexed by vpri
 *
 *****************************************************************************/
static fm_byte  GetVPRI(hlp_model        *model,
                        fm_byte          whichVlan,
                        fm_byte          vpri,
                        fm_byte          p_profile,
                        fm_text          key)
{
    fm_byte                 retVal;
    fm_uint64               temp64;
    hlp_modelState          *state;
    fm_uint32               *vpri_map;

    state = &model->packetState;

    if (whichVlan == 1) {
        vpri_map = FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_VPRI1_MAP(p_profile, 0));
            //HLP_MOD_VPRI1_MAP(state->TX_PORT, 0));
    	temp64 = FM_ARRAY_GET_UNNAMED_FIELD64(vpri_map, 0, 64);
    }
    else
    {
        vpri_map = FM_MODEL_GET_REG_PTR(model,
        	HLP_MOD_VPRI2_MAP(p_profile, 0));
        	//HLP_MOD_VPRI2_MAP(state->TX_PORT, 0));
    	temp64 = FM_ARRAY_GET_UNNAMED_FIELD64(vpri_map, 0, 64);
    }

    PRINT2(modDisplayVerbose, key, "MOD_VPRI%0d_MAP[%0d]=0x%016llx\n", whichVlan, p_profile, temp64);

    /* this seems easier than storing reg nibble fields in byte array,
     * or than constructing a name for FM_GET_FIELD */
    temp64 >>= (vpri * 4);
    temp64 &= 0xF;
    retVal = (fm_byte)temp64;
    return retVal;
}

/*****************************************************************************/
/** PrintChunkedSeg
 * \ingroup intModel
 *
 * \desc            Prints out the valid/length bits of the ChunkedSeg.
 *
 * \param[in]		chunkedSeg points to the broken down packet.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static void PrintChunkedSeg(fm_text key, hlp_modelChunkedSeg *chunkedSeg)
{
    PRINT(modDisplayVerbose, key, "ChunkedSeg: frag_v=%0d; n_otr_tag=%0d; n_otr_mpls=%0d; otr_ip_size=%0d; "
        "otr_udp_v=%0d; otp_tcp_v=%0d; tun_size_in_l4_chunk=%0d;\n",
        chunkedSeg->ftag_v, chunkedSeg->n_otr_tag, chunkedSeg->n_otr_mpls,
        chunkedSeg->otr_ip_size, chunkedSeg->otr_udp_v, chunkedSeg->otr_tcp_v,
        chunkedSeg->tun_size_in_l4_chunk);

    PRINT(modDisplayVerbose, key, "ChunkedSeg Inner: tun_opt_size=%0d; inr_l2_v=%0d; "
        "n_inr_tag=%0d; inr_mpls_len=%d; inr_ip_size=%0d; inr_udp_v=%0d; inr_tcp_v=%0d;\n",
        chunkedSeg->tun_opt_size, chunkedSeg->inr_l2_v, chunkedSeg->n_inr_tag,
        chunkedSeg->n_inr_mpls, chunkedSeg->inr_ip_size, chunkedSeg->inr_udp_v, chunkedSeg->inr_tcp_v);

    PRINT(modDisplayVerbose, key, "ChunkedSeg Payload: start=%0d; size=%0d;\n",
        chunkedSeg->payload_start, chunkedSeg->payload_size);
}

fm_int GetL4hdrIdx(fm_text key,
                   hlp_modelChunkedSeg *chunkedSeg,
			       hlp_modelParserInfo *parserInfo)
{
    fm_int idx = 0;
    fm_int otr_l2_len;
    fm_int otr_ip_size;
    fm_int n_otr_mpls;
	/* OTR ETH */
    if (parserInfo->otr_l2_len > 5) {
        otr_l2_len = 5;
    }
    else {
        otr_l2_len = parserInfo->otr_l2_len;
    }
    if (otr_l2_len > 0) {
        for (int i = 0; i < 6; i++)
            idx++;
        for (int i = 0; i < 6; i++)
            idx++;
        for (int i = 0; i < (4*chunkedSeg->n_otr_tag); i++)
            idx++;
        /* Etype */
        for (int i = 0; i < 2; i++)
            idx++;
    }
	/* OTR MPLS */
    if (parserInfo->otr_mpls_len > 7) {
        n_otr_mpls = 7;
    }
    else {
        n_otr_mpls = parserInfo->otr_mpls_len;
    }
	for (int i = 0; i < (4*n_otr_mpls); i++)
        idx++;
	/* OTR L3 */
    if (parserInfo->otr_l3_len > 14) {
        otr_ip_size = 14;
    }
    else {
        otr_ip_size = parserInfo->otr_l3_len;
    }
    for (int i = 0; i < (4*otr_ip_size); i++)
        idx++;
    /* Layer 4 */
	PRINT2(modDisplayVerbose, key, "l4hdr start idx = %0d;\n", idx);
    return idx;
}

void SelectL4CsumSrc(fm_text key,
                     hlp_model *model,
				 	 fm_byte   *rx_packet,
                     hlp_modelChunkedSeg *chunkedSeg)
{
    fm_int l4hdrIdx = 0;
    hlp_modelState *state = &model->packetState;
    l4hdrIdx = GetL4hdrIdx(key, chunkedSeg, &state->PARSER_INFO);
	PRINT2(modDisplayVerbose, key, "tail_csum_len: 0x%0llx; udp_v(%0d); tcp_v(%0d);\n", state->TAIL_CSUM_LEN, chunkedSeg->otr_udp_v, chunkedSeg->otr_tcp_v);
    if((state->TAIL_CSUM_LEN>>30) == 2) { // COMPUTEL4CHECKSUM
	    PRINT2(modDisplayVerbose, key, "tail_csum_len: COMPUTEL4CSUM CASE\n");
        if(chunkedSeg->otr_udp_v) {
            chunkedSeg->otr_l4[6] = (state->TAIL_CSUM_LEN >> 8)&0xFF;
            rx_packet[l4hdrIdx+6] = (state->TAIL_CSUM_LEN >> 8)&0xFF;
            chunkedSeg->otr_l4[7] = state->TAIL_CSUM_LEN & 0xFF;
            rx_packet[l4hdrIdx+7] = state->TAIL_CSUM_LEN & 0xFF;
        }
        if(chunkedSeg->otr_tcp_v) {
            chunkedSeg->otr_l4[16] = (state->TAIL_CSUM_LEN >> 8)&0xFF; 
            rx_packet[l4hdrIdx+16] = (state->TAIL_CSUM_LEN >> 8)&0xFF;
            chunkedSeg->otr_l4[17] = state->TAIL_CSUM_LEN & 0xFF; 
            rx_packet[l4hdrIdx+17] = state->TAIL_CSUM_LEN & 0xFF;
        }
    }
}

/*****************************************************************************/
/** UnpackPacket
 * \ingroup intModel
 *
 * \desc            Unpacks the packet into the ChunkedSeg data structure.
 *
 * \param[in]		packet points to the actual packet data.
 *
 * \param[in]       chuckedSeg is a pointer to the unpacked packet.
 *
 * \param[in]		parserInfo is the information from the parser about what
 * 					header information is in the packet.
 *
 * \param[in]		rx_length is the length of the incoming packet.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status UnpackPacket(fm_byte             *packet,
				 	          hlp_modelChunkedSeg *chunkedSeg,
				 	          hlp_modelParserInfo *parserInfo,
				 	          fm_uint32			   rx_length,
                              fm_text              key)
{
	fm_status status;
	status = FM_OK;

	fm_uint32 idx;
	fm_uint32 tmp_idx;
	fm_int otr_l2_len;
	fm_int otr_tun_len;
	fm_int inr_l2_len;

	idx = 0;

	/* OTR ETH */
    if (parserInfo->otr_l2_len > 5) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_l2_len=%0d, overrun 30B otr_l2 chunk segment\n", parserInfo->otr_l2_len);
        otr_l2_len = 5;
    }
    else {
        otr_l2_len = parserInfo->otr_l2_len;
    }
    if (otr_l2_len > 0) {
        for (int i = 0; i < 6; i++, idx++)
            chunkedSeg->otr_dmac[i] = packet[idx];
        for (int i = 0; i < 6; i++, idx++)
            chunkedSeg->otr_smac[i] = packet[idx];
        chunkedSeg->n_otr_tag = (otr_l2_len - 1);
        for (int i = 0; i < (4*chunkedSeg->n_otr_tag); i++, idx++)
            chunkedSeg->otr_tags[i] = packet[idx];
        /* Etype */
        for (int i = 0; i < 2; i++, idx++)
            chunkedSeg->otr_et[i] = packet[idx];
    }
	/* OTR MPLS */
    if (parserInfo->otr_mpls_len > 7) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_mpls_len=%0d, overrun 28B otr_mpls chunk segment\n", parserInfo->otr_mpls_len);
        chunkedSeg->n_otr_mpls = 7;
    }
    else {
        chunkedSeg->n_otr_mpls = parserInfo->otr_mpls_len;
    }
	chunkedSeg->n_otr_mpls_pre = parserInfo->otr_mpls_len;
	for (int i = 0; i < (4*chunkedSeg->n_otr_mpls); i++, idx++) {
		chunkedSeg->otr_mpls[i] = packet[idx];
    }
    // try to mimic rtl
    tmp_idx = idx;
	for (int i = 4*chunkedSeg->n_otr_mpls; i < 28; i++, tmp_idx++) {
		chunkedSeg->otr_mpls[i] = packet[tmp_idx];
    }
	/* OTR L3 */
    if (parserInfo->otr_l3_len > 14) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_l3_len=%0d, overrun 56B otr_l3 chunk segment\n", parserInfo->otr_l3_len);
        chunkedSeg->otr_ip_size = 14;
    }
    else {
        chunkedSeg->otr_ip_size = parserInfo->otr_l3_len;
    }
    chunkedSeg->otr_l3_v6 = parserInfo->otr_l3_v6;
    for (int i = 0; i < (4*chunkedSeg->otr_ip_size); i++, idx++)
		chunkedSeg->otr_ip[i] = packet[idx];
	/* OTR L4 */
    if (parserInfo->otr_l4_udp && parserInfo->otr_l4_tcp) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: In no way parserInfo->otr_l4_udp and parserInfo->otr_l4_tcp should be both asserted\n");
    }
    chunkedSeg->otr_udp_v = parserInfo->otr_l4_udp;
    chunkedSeg->otr_tcp_v = parserInfo->otr_l4_tcp;
    if (chunkedSeg->otr_udp_v)
    {
        if (parserInfo->otr_tun_len > 18) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_tun_len=%0d, overrun 80B combined otr_l4+tunnel_opt chunk segments\n", parserInfo->otr_tun_len);
            otr_tun_len = 18;
        }
        else {
            otr_tun_len = parserInfo->otr_tun_len;
        }
        for (int i = 0; i < (8 + otr_tun_len*4); i++, idx++)
        {
            if (i < 40)
                chunkedSeg->otr_l4[i] = packet[idx];
            else
                chunkedSeg->tun_opt[i-40] = packet[idx];
        }
        // it is not l4 hdr size, you can think of it is the tunnel hdr len in
        // l4 chunk and tunnel chunk
        chunkedSeg->tun_size_in_l4_chunk = (otr_tun_len < 8) ? otr_tun_len : 8;
        chunkedSeg->tun_opt_size = (otr_tun_len < 8) ? 0 : otr_tun_len - 8;
    }
    else if (chunkedSeg->otr_tcp_v)
    {
        for (int i = 0; i < 18; i++, idx++)
            chunkedSeg->otr_l4[i] = packet[idx];
        // TODO if tcp could followed by a tunnel hdr our codes in
        // UpdateL3LenCsum will fail since no tun_opt_size set for tcp
        // TODO comment out should be ok
        //chunkedSeg->tun_size_in_l4_chunk = 3;
    }
    else if (parserInfo->otr_tun_len) { // any tunnel protocol following L3 immediately
        if (parserInfo->otr_tun_len > 18) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_tun_len=%0d, overrun 80B combined otr_l4+tunnel_opt chunk segments\n", parserInfo->otr_tun_len);
            otr_tun_len = 18;
        }
        else {
            otr_tun_len = parserInfo->otr_tun_len;
        }
        for (int i = 0; i < otr_tun_len*4; i++, idx++)
        {
            if (i < 40)
                chunkedSeg->otr_l4[i] = packet[idx];
            else
                chunkedSeg->tun_opt[i-40] = packet[idx];
        }
        chunkedSeg->tun_size_in_l4_chunk = (otr_tun_len < 10) ? otr_tun_len : 10;
        chunkedSeg->tun_opt_size = (otr_tun_len < 10) ? 0 : otr_tun_len - 10;
    }

    /* Inner */
    if (parserInfo->inr_l2_len > 5) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->inr_l2_len=%0d, overrun 30B inr_l2 chunk segment\n", parserInfo->inr_l2_len);
        inr_l2_len = 5;
    }
    else {
        inr_l2_len = parserInfo->inr_l2_len;
    }
    if (inr_l2_len > 0)
	{
		/* ETH */
        chunkedSeg->inr_l2_v = 1;
        for (int i = 0; i < 6; i++, idx++)
			chunkedSeg->inr_dmac[i] = packet[idx];
		for (int i = 0; i < 6; i++, idx++)
			chunkedSeg->inr_smac[i] = packet[idx];
		/* VLAN & Custom Tags */
		chunkedSeg->n_inr_tag = parserInfo->inr_l2_len - 1;
		for (int i = 0; i < (4*chunkedSeg->n_inr_tag); i++, idx++)
			chunkedSeg->inr_tags[i] = packet[idx];
	    /* Etype */
        for (int i = 0; i < 2; i++, idx++)
			chunkedSeg->inr_et[i] = packet[idx];
        /* INR MPLS */
        if (parserInfo->inr_mpls_len > 7) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->inr_mpls_len=%0d, overrun 28B inr_mpls chunk segment\n", parserInfo->inr_mpls_len);
            chunkedSeg->n_inr_mpls = 7;
        }
        else {
            chunkedSeg->n_inr_mpls = parserInfo->inr_mpls_len;
        }
        for (int i = 0; i < (4*chunkedSeg->n_inr_mpls); i++, idx++)
            chunkedSeg->inr_mpls[i] = packet[idx];
    }
    {
        /* INR L3 */
        if (parserInfo->inr_l3_len > 14) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->inr_l3_len=%0d, overrun 56B inr_l3 chunk segment\n", parserInfo->inr_l3_len);
            chunkedSeg->inr_ip_size = 14;
        }
        else {
            chunkedSeg->inr_ip_size = parserInfo->inr_l3_len;
        }
        chunkedSeg->inr_l3_v6 = parserInfo->inr_l3_v6;
		for (int i = 0; i < (chunkedSeg->inr_ip_size*4); i++, idx++)
			chunkedSeg->inr_ip[i] = packet[idx];
        /* INR L4 */
        if (parserInfo->inr_l4_udp && parserInfo->inr_l4_tcp) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: In no way parserInfo->inr_l4_udp and parserInfo->inr_l4_tcp should be both asserted\n");
        }
        chunkedSeg->inr_udp_v = parserInfo->inr_l4_udp;
        chunkedSeg->inr_tcp_v = parserInfo->inr_l4_tcp;
        if (parserInfo->inr_l4_udp)
        {
			for (int i = 0; i < 8; i++, idx++)
				chunkedSeg->inr_l4[i] = packet[idx];
		}
        if (parserInfo->inr_l4_tcp)
        {
			for (int i = 0; i < 18; i++, idx++)
				chunkedSeg->inr_l4[i] = packet[idx];
		}
	}
	chunkedSeg->payload_start = idx;
	chunkedSeg->payload_size = rx_length - idx;

    PrintChunkedSeg(key, chunkedSeg);

    return status;
}

void GetModRegData(fm_text key,
                   hlp_model *model,
                   hlp_modelModRegData *r)
{
    hlp_modelState                  *state;
    fm_uint64               *modPerPortCfg1;
    fm_uint64               *modPerPortCfg2;
    fm_uint64               *modAqmProfile;
    fm_uint64               *modWredLevel;

    FM_NOT_USED(key);

    state = &model->packetState;

    modPerPortCfg1 = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_PER_PORT_CFG1(state->TX_PORT, 0));
    r->modPerPortCfg1.ENABLE_VLAN_UPDATE = FM_GET_BIT64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, ENABLE_VLAN_UPDATE);
    r->modPerPortCfg1.VID2_MAP_INDEX = FM_GET_FIELD64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, VID2_MAP_INDEX);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT = FM_GET_FIELD64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_GLORT);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK = FM_GET_FIELD64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_MASK);

    modPerPortCfg2 = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_PER_PORT_CFG2(state->TX_PORT, 0));
    r->modPerPortCfg2.ENABLE_DMAC_ROUTING = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_DMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_SMAC_ROUTING = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_SMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_TTL_DECREMENT = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_TTL_DECREMENT);
    r->modPerPortCfg2.ENABLE_ECN_MODIFICATION = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_ECN_MODIFICATION);
    r->modPerPortCfg2.VLAN2_E_TYPE = FM_GET_FIELD64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VLAN2_E_TYPE);
    r->modPerPortCfg2.VLAN1_E_TYPE = FM_GET_FIELD64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VLAN1_E_TYPE);
    r->modPerPortCfg2.ENABLE_DEI2_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_DEI2_UPDATE);
    r->modPerPortCfg2.ENABLE_DEI1_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_DEI1_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP2_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_PCP2_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP1_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_PCP1_UPDATE);
    r->modPerPortCfg2.VID2_FIRST = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VID2_FIRST);
    r->modPerPortCfg2.VLAN_TAGGING = FM_GET_FIELD64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VLAN_TAGGING);
    r->modPerPortCfg2.MIN_FRAME_SIZE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, MIN_FRAME_SIZE);

}

void CalcIngressCRC(fm_text key,
                         hlp_model *model,
                         hlp_modelModControlData *c)
{
    hlp_modelState *state = &model->packetState;
    fm_uint32 ingress_crc = 0;
    fm_uint32 good_ingress_crc;

    for (int i = 0; i < 4; i++)
        ingress_crc |= state->RX_DATA[state->RX_LENGTH-4+i] << (i*8);

    // calculate good ingress crc
    good_ingress_crc = fmCrc32(state->RX_DATA, state->RX_LENGTH-4);
    // calculate ingress crc error differential
    c->crc_ingress_diff = good_ingress_crc ^ ingress_crc;

    if (c->crc_ingress_diff != 0) {
        PRINT(modDisplayVerbose, key, "Ingress packet has bad CRC. ingress crc=0x%08x, good crc=0x%08x, crc_diff=0x%08x\n", ingress_crc, good_ingress_crc, c->crc_ingress_diff);
    }
    else {
        PRINT(modDisplayVerbose, key, "Ingress packet has good CRC. ingress crc=0x%08x, good crc=0x%08x\n", ingress_crc, good_ingress_crc);
    }
}

void InitChunkedSeg(fm_text key,
                   hlp_model *model,
                   fm_byte *packet,
                   hlp_modelChunkedSeg *chunkedSeg)
{
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(packet);

    UnpackPacket(state->RX_DATA, chunkedSeg, &state->PARSER_INFO, state->RX_LENGTH, key);

    SelectL4CsumSrc(key, model, state->RX_DATA, chunkedSeg);
    state->TX_REASONCODE = 0x00;

    state->NO_PRI_ENC = !((state->PARSER_INFO.otr_l2_vlan1>0) || (state->PARSER_INFO.otr_mpls_len>0) || (state->PARSER_INFO.otr_l3_len>0));

    PRINT2(modDisplayVerbose, key, "state->RX_LENGTH = %0d, state->TX_PORT = 0x%02x\n",
        state->RX_LENGTH, state->TX_PORT);
    PRINT2(modDisplayVerbose, key, "sglort = 0x%04x (from modelState)\n", state->SGLORT);
    PRINT(modDisplayVerbose, key, "parserInfo = OTR_L2_len=%d, "
    "V1=%0d, V2=%0d, V2FIRST=%0d MPLS_LEN=%0d, OTR_L3_LEN=%0d, "
    "OTR_L3_V6=%0d, OTR_TUN_LEN=%0d, OTR_UDP=%0d, OTR_TCP=%0d,\n",
        state->PARSER_INFO.otr_l2_len,
        state->PARSER_INFO.otr_l2_vlan1,
        state->PARSER_INFO.otr_l2_vlan2,
        state->PARSER_INFO.otr_l2_v2first,
        state->PARSER_INFO.otr_mpls_len,
        state->PARSER_INFO.otr_l3_len,
        chunkedSeg->otr_l3_v6,
        state->PARSER_INFO.otr_tun_len,
        state->PARSER_INFO.otr_l4_udp,
        state->PARSER_INFO.otr_l4_tcp);
    PRINT2(modDisplayVerbose, key, "INR_L2_LEN=%0d, INR_L3_LEN=%0d, INR_L3_V6=%0d, "
        "INR_UDP=%0d, INR_TCP=%0d\n",
        state->PARSER_INFO.inr_l2_len,
        state->PARSER_INFO.inr_l3_len,
        chunkedSeg->inr_l3_v6,
        state->PARSER_INFO.inr_l4_udp,
        state->PARSER_INFO.inr_l4_tcp);
}

static fm_bool isWindowParsing(hlp_modelParserInfo *paInfo)
{
    if (paInfo->otr_l2_len     == 0 &&
        paInfo->otr_l2_vlan1   == 0 &&
        paInfo->otr_l2_vlan2   == 0 &&
        paInfo->otr_l2_v2first == 0 &&
        paInfo->otr_mpls_len   == 0 &&
        paInfo->otr_l3_len     == 0 &&
        paInfo->otr_l3_v6      == 0 &&
        paInfo->otr_l4_udp     == 0 &&
        paInfo->otr_l4_tcp     == 0 &&
        paInfo->otr_tun_len    == 0 &&
        paInfo->inr_l2_len     == 0 &&
        paInfo->inr_l2_vlan1   == 0 &&
        paInfo->inr_l2_vlan2   == 0 &&
        paInfo->inr_l2_v2first == 0 &&
        paInfo->inr_mpls_len   == 0 &&
        paInfo->inr_l3_len     == 0 &&
        paInfo->inr_l3_v6      == 0 &&
        paInfo->inr_l4_udp     == 0 &&
        paInfo->inr_l4_tcp     == 0)
        return 1;
    else
        return 0;
}

void InitControl(fm_text key,
                 hlp_model *model,
                 hlp_modelModRegData *r,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg)
{
	hlp_modelState *state;
    fm_uint64 *modIm;

    FM_NOT_USED(key);
    FM_NOT_USED(r);

	state = &model->packetState;

    c->dvStatus = IS_OK;

    if (isWindowParsing(&(state->PARSER_INFO)) && !state->NO_MODIFY) {
        c->isWindowParsing = 1;
        PRINT(modDisplayVerbose, key, "Ingress flow 2nd pass window parsing packet: passing through\n");
    }
    else {
        c->isWindowParsing = 0;
    }

    c->isMarkerPkt = 0;
	c->evidA = state->L2_EVID1;
	c->dglortA = state->EDGLORT;
	c->isMirror = ((state->MIRTYP == HLP_MODEL_MIRTYPE_MIR0) || (state->MIRTYP == HLP_MODEL_MIRTYPE_MIR1));
    c->mirrorTrunc = 0;
    c->rx_n_tag = 0;
    c->otrL3Modified = 0;
    c->otrL4Modified = 0;
    c->internalDS = state->QOS_L3_DSCP << 2 | (state->ECN & 0x03);
    c->isInterLSR = 0;
    c->skipDscp = 0;
    c->skipTtl = 0;
    c->ecn_tx_drop = 0;
    c->timeout_tx_drop = 0;
    c->non_cm_tx_drop = 0;
    c->cancelled_tx_disp = 0;
    c->cancel_drop_on_marker = 0;
    c->ecn_mark = 0;
    FM_CLEAR(c->mplsData);

    /* Grabbing l3Idx for checksum updates */
    if (chunkedSeg->otr_ip_size > 0)
    {
        c->l3Idx = 8*(chunkedSeg->ftag_v) + 14 +
            4*(chunkedSeg->n_otr_tag) + 4*(chunkedSeg->n_otr_mpls);
    }
	if (chunkedSeg->otr_udp_v | chunkedSeg->otr_tcp_v)
    {
        c->l4Idx = 8*chunkedSeg->ftag_v + 14 +
            4*chunkedSeg->n_otr_tag + 4*chunkedSeg->n_otr_mpls +
            4*chunkedSeg->otr_ip_size;
    }

    c->routeA = state->MARK_ROUTED;
	c->vlanSwitched = 0;
    c->loopbackSuppressDrop = 0;

	c->numVlans = 0;
	if (state->PARSER_INFO.otr_l2_vlan1) ++(c->numVlans);
	if (state->PARSER_INFO.otr_l2_vlan2) ++(c->numVlans);
    c->rxVlan1 = 0;
    c->rxVlan2 = 0;
    c->rxV2first = 0;
    c->txVlan1 = 0;
	c->txVlan2 = 0;
	c->preserveVlan1 = 0;
	c->preserveVlan2 = 0;
	c->txVpri1 = 0;
	c->txVpri2 = 0;
	c->txVid2 = 0;
	c->txVid2 = 0;
	c->evidB = 0;

	c->modIdx = state->MOD_IDX;
	c->encap = 0;
	c->decap = 0;

    c->bytesAdded = 0;
    c->egressSeg0Bytes = (state->RX_LENGTH < DEFAULT_SEGMENT_BYTES)
        ? state->RX_LENGTH : DEFAULT_SEGMENT_BYTES;
    c->refcnt_tx_len = state->TX_LENGTH;
    
    c->intr_occured = 0;
    c->tail_len = (((state->TAIL_CSUM_LEN >> 16) & 0x3FFF) < DEFAULT_SEGMENT_BYTES) ? 0 : (((state->TAIL_CSUM_LEN >> 16) & 0x3FFF) - DEFAULT_SEGMENT_BYTES);

    modIm = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_IM(0));
    c->mod_im = FM_GET_UNNAMED_FIELD64(*modIm, 0, 63);

    // calculate the correct ingress L3 total length for incremental len update
    c->igL3TotalLen = chunkedSeg->otr_ip_size*4 + chunkedSeg->otr_tcp_v*18 +
        chunkedSeg->otr_udp_v*8 + chunkedSeg->tun_size_in_l4_chunk*4 +
        chunkedSeg->tun_opt_size*4 + chunkedSeg->inr_l2_v*14 +
        chunkedSeg->n_inr_tag*4 + chunkedSeg->n_inr_mpls*4 +
        chunkedSeg->inr_ip_size*4 + chunkedSeg->inr_udp_v*8 +
        chunkedSeg->inr_tcp_v*18 + chunkedSeg->payload_size - 4;
    if (chunkedSeg->otr_l3_v6)
        c->igL3TotalLen -= 40;
    PRINT2(modDisplayVerbose, key, "ingress L3 total length = %0d\n", c->igL3TotalLen);
}

void LoopbackSuppress(fm_text key,
                      hlp_model *model,
                      hlp_modelModRegData *r,
                      hlp_modelModControlData *c)
{
    hlp_modelState *state;
    fm_uint16 csGlortF;

    state = &model->packetState;

    csGlortF = state->SGLORT & r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK;
    if (state->MOD_IP_MCAST_IDX != 0)
    {
        c->vlanSwitched = (c->evidA == state->L2_IVID1);
        if (c->vlanSwitched)
        {
            c->routeA = 0;
        }
        c->loopbackSuppressDrop = (r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT == csGlortF)
            & c->vlanSwitched;
    }
    else
    {
        c->loopbackSuppressDrop = 0;
    }
    PRINT2(modDisplayVerbose, key, "route = %0d\n", state->MARK_ROUTED);
    PRINT2(modDisplayVerbose, key, "routeA = %0d\n", c->routeA);
    PRINT2(modDisplayVerbose, key, "glortMask = 0x%04x\n", r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK);
    PRINT2(modDisplayVerbose, key, "csGlortF = 0x%04x (sglort & glortMask)\n", csGlortF);
    PRINT2(modDisplayVerbose, key, "csGlortX = 0x%04x\n", r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT);
    PRINT2(modDisplayVerbose, key, "vlanSwitched = %0d\n", c->vlanSwitched);
    PRINT2(modDisplayVerbose, key, "drop (because of loopback suppress) = %0d\n", c->loopbackSuppressDrop);
}

void GetRxL2Tags(fm_text key,
                 hlp_model *model,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg)
{
    hlp_modelState *state = &model->packetState;


    for (int i = 0; i < VLAN_TAG_BYTES*4; i++)
        c->rx_tags[i] = chunkedSeg->otr_tags[i];
    c->rx_n_tag = chunkedSeg->n_otr_tag;
    c->rxVlan1 = state->PARSER_INFO.otr_l2_vlan1;
    c->rxVlan2 = state->PARSER_INFO.otr_l2_vlan2;
    c->rxV2first = state->PARSER_INFO.otr_l2_v2first;

    PRINT2(modDisplayVerbose, key, "rx_n_tag=%0d rx_tags=0x", c->rx_n_tag);
    for (int i = 0; i < c->rx_n_tag; i++)
    {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ", c->rx_tags[i*4],c->rx_tags[(i*4)+1],
            c->rx_tags[(i*4)+2],c->rx_tags[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");
}

void LogError(fm_text key,
              hlp_model *model,
              hlp_modelModControlData *c)
{
    hlp_modelState *state = &model->packetState;
    fm_uint64 *modUpdateErrorRecord;
    fm_byte mir_type = 0;
    fm_byte mir_prof = 0;
    fm_byte tx_tag = 0;
    fm_byte mpls_pop = 0;
    fm_uint mod_idx = 0;

    FM_NOT_USED(key);

    modUpdateErrorRecord = (fm_uint64*) FM_MODEL_GET_REG_PTR(model, HLP_MOD_UPDATE_ERROR_RECORD(0));
    FM_SET_BIT64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, WINDOW_PARSING, isWindowParsing(&(state->PARSER_INFO)));
    if(state->TX_REASONCODE == ERR_VLAN_TAG_FULL ||
       state->TX_REASONCODE == ERR_VLAN_TAG_EMPTY) {// VLAN tag errors
        switch(state->MIRTYP) {
            case HLP_MODEL_MIRTYPE_NORMAL:
                mir_prof = 0;
                break;
            case HLP_MODEL_MIRTYPE_MIR0:
                mir_prof = state->MIRROR0_PROFILE_IDX;
                break;
            case HLP_MODEL_MIRTYPE_MIR1:
                mir_prof = state->MIRROR1_PROFILE_IDX;
                break;
            default:
                break;
        }
        mir_type = state->MIRTYP;
        tx_tag = state->TX_TAG;
    }
    if(state->TX_REASONCODE == ERR_PUSH_AL ||
       state->TX_REASONCODE == ERR_PUSH_ELI ||
       state->TX_REASONCODE == ERR_PUSH_G ||
       state->TX_REASONCODE == ERR_POP_AL ||
       state->TX_REASONCODE == ERR_POP_ELI ||
       state->TX_REASONCODE == ERR_POP_G) { // mpls desc errors
        mpls_pop = state->MPLS_POP;
    }
    if (state->TX_REASONCODE != ERR_VLAN_TAG_FULL && state->TX_REASONCODE != ERR_VLAN_TAG_EMPTY && state->TX_REASONCODE != ERR_PKT_LEN) { // desc errors
        mod_idx = (c->modIdx << 2);
    }
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MIR_TYPE, mir_type);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, TX_TAG, tx_tag);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MIRROR_PROFILE, mir_prof);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MPLS_POP, mpls_pop);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MOD_IDX, mod_idx);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, TX_PORT, state->TX_PORT);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, REASON_CODE, state->TX_REASONCODE);
}

void DropAndLog(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c,
                fm_byte dropDisp,
                fm_bool isMarker,
                fm_bool isDrop,
                fm_bool isErrLog,
                fm_bool isSkip,
                fm_byte reasonCode,
                fm_int  intrDisp)
{
    hlp_modelState *state = &model->packetState;

    if (isSkip) return;

    if (dropDisp < state->TX_DISP && !isMarker) {
        if (dropDisp == DISP_MODERRORDROP) {
            if (isDrop) {
                state->TX_DISP = dropDisp;
            }
        }
        else {
            state->TX_DISP = dropDisp;
        }
    }

    if(state->TX_DROP == 0)
        state->TX_DROP = ((!isMarker) & isDrop);

    // this block serves a few purposes:
    // 1. for state->ECN_TX_DROP prediction, which should only be set if a packet
    //    is dropped due to ecn but not dropped for any other reason
    // 2. c->cancelled_tx_disp memorizes the drop that would have happend were it
    //    not cancelled by isMarker
    if (isDrop) {
        if (!isMarker) {
            if (dropDisp == DISP_ECNDROP)
                c->ecn_tx_drop = 1;
            else if (dropDisp == DISP_TIMEOUTDROP)
                c->timeout_tx_drop = 1;
            else
                c->non_cm_tx_drop = 1;
        }
        else {
            c->cancel_drop_on_marker = 1;
            if (dropDisp < c->cancelled_tx_disp)
                c->cancelled_tx_disp = dropDisp;
        }
    }

    PRINT2(modDisplayVerbose, key, "state->TX_DROP = %0d\n", state->TX_DROP);
    if(isErrLog && (!((c->mod_im)>>intrDisp & 0x1))) {
        if(reasonCode < state->TX_REASONCODE || state->TX_REASONCODE == 0) {
            state->TX_REASONCODE = reasonCode;
            LogError(key, model, c);
        }
    }
 
    // interrupt prediction. Only valid for single-step tests
    if(intrDisp >= 0){
        fm_uint64 *modIp;
        fm_uint64 ip = 0;

        modIp = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_IP(0));
        ip = FM_GET_UNNAMED_FIELD64(*modIp, 0 ,63);
        ip |= FM_LITERAL_U64(1) << intrDisp;
        FM_SET_UNNAMED_FIELD64(*modIp, 0, 63, ip);

        if(!((c->mod_im)>>intrDisp & 0x1))
            c->intr_occured = 1;
        PRINT2(modDisplayVerbose, key, "INTR: set intr type %0d\n", intrDisp);
    }
}

void ExtractPriorityProfile(fm_text key,
                            hlp_model *model,
                            hlp_modelModRegData *r,
                            hlp_modelModControlData *c)
{
     fm_uint32 *modDomainTcamKey;
     fm_uint32 *modDomainTcamMask;
     fm_uint32 *modDomainAction;
     fm_uint32 hitIdx = 100;
     hlpModDomainTcamKey key_data;
     hlpModDomainTcamMask mask_data;
     hlpModDomainAction   modDomainTcamAction_data;
     fm_uint64 keyReg, keyIngress, keyMask;
     hlp_modelState *state;

    FM_NOT_USED(r);

     state = &model->packetState;

     PRINT(modDisplayVerbose, key, "------- Extract Priority Profile -------:\n");
     for (int i=63; i >= 0; i--) {
         modDomainTcamKey = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DOMAIN_TCAM_KEY(i, 0));
         modDomainTcamMask = FM_MODEL_GET_REG_PTR(model,HLP_MOD_DOMAIN_TCAM_MASK(i, 0));
         key_data.PORT = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, PORT);
         key_data.OPERATOR_ID = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, OPERATOR_ID);
         key_data.VID1 = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, VID1);
         key_data.VID2 = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, VID2);

         mask_data.PORT = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, PORT);
         mask_data.OPERATOR_ID = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, OPERATOR_ID);
         mask_data.VID1 = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, VID1);
         mask_data.VID2 = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, VID2);

         keyReg = FM_ARRAY_GET_UNNAMED_FIELD64(modDomainTcamKey, 0, 34);
         keyIngress = (((fm_uint64)(state->TX_PORT&0x3F) << 28) | ((fm_uint64)(c->operator_id&0x000F) << 24)
                      | ((fm_uint64)(c->txVid1&0x0FFF) << 12) |
                      (c->txVid2&0x0FFF))&0x3FFFFFFFF;
         keyMask = FM_ARRAY_GET_UNNAMED_FIELD64(modDomainTcamMask, 0, 34);

         PRINT3(modDisplayVerbose, key, "index at %0d\n", i);
         PRINT3(modDisplayVerbose, key, "keyReg=0x%0llx\n", keyReg);
         PRINT3(modDisplayVerbose, key, "keyIngress=0x%0llx\n", keyIngress);
         PRINT3(modDisplayVerbose, key, "keyMask=0x%0llx\n", keyMask);
         PRINT3(modDisplayVerbose, key, "0x%0llx : 0x%0llx\n",(keyReg & ~keyMask) ,(keyIngress & ~keyMask));
         if (keyReg == (keyIngress & keyMask)) {
             hitIdx = i;
             PRINT2(modDisplayVerbose, key, "key.Port=0x%0x, key.Operator_id=0x%0x, key.vid1=0x%0x, key.Vid2=0x%0x\n",
                   key_data.PORT, key_data.OPERATOR_ID, key_data.VID1, key_data.VID2);
             PRINT2(modDisplayVerbose, key, "Mask.Port=0x%0x, Mask.Operator_id=0x%0x, Mask.vid1=0x%0x,Mask.Vid2=0x%0x\n",
                   mask_data.PORT, mask_data.OPERATOR_ID, mask_data.VID1,mask_data.VID2);
             break;
         }
     }

     if (hitIdx == 100) {
        PRINT2(modDisplayVerbose, key, "no hit, set hitIdx to 0\n");
        hitIdx = 0;
     }
     else
        PRINT2(modDisplayVerbose, key, "hitIdx in DomainTCAMKey %0d\n", hitIdx);
     modDomainAction = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DOMAIN_ACTION(hitIdx, 0));
     c->priority_profile = FM_ARRAY_GET_UNNAMED_FIELD64(modDomainAction, 0, 5);
     PRINT2(modDisplayVerbose, key, "Prioriy Profile is 0x%0x\n", c->priority_profile);
}

void VlanLookup(fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg)
{
    hlp_modelState *state;

    fm_uint16 rxVlan1Type;
    fm_uint16 rxVlan2Type;
    fm_byte rxVpri1;
    fm_byte rxVpri2;
    fm_uint16 rxVid1;
    fm_uint16 rxVid2;

    fm_uint16 evidB;
    fm_uint16 modVid1MapAddr;
    fm_uint16 modVid2MapAddr;
    fm_uint16 modVlanTagAddr;
    fm_uint64 *rdval;
    fm_uint64 temp64;

    fm_byte newVpri1;
    fm_byte newVpri2;
    fm_uint16 newVid1;
    fm_uint16 newVid2;
    fm_bool vtag;

    FM_NOT_USED(chunkedSeg);

    state = &model->packetState;
    rxVpri1 = 0;
    rxVpri2 = 0;
    rxVid1 = 0;
    rxVid2 = 0;
    evidB = 0;
    newVpri1 = 0;
    newVpri2 = 0;
    newVid1 = 0;
    newVid2 = 0;
    vtag = 0;

    PRINT2(modDisplayVerbose, key, "------- VLAN LOOKUP -------\n");

    PRINT2(modDisplayVerbose, key, "initial numVlans = %0d\n", c->numVlans);
    PRINT2(modDisplayVerbose, key, "vpri = 0x%01x (from modelState)\n", state->QOS_L2_VPRI1);
    PRINT2(modDisplayVerbose, key, "enableVlanUpdate = %0d\n", r->modPerPortCfg1.ENABLE_VLAN_UPDATE);
    PRINT2(modDisplayVerbose, key, "vid2First = %0d (in cfg2)\n", r->modPerPortCfg2.VID2_FIRST);

    evidB = r->modPerPortCfg1.ENABLE_VLAN_UPDATE ? c->evidA : state->L2_IVID1;
    PRINT2(modDisplayVerbose, key, "evidB = 0x%03x (enableVlanUpdate ? evidA : ivid)\n",
        evidB);

    /* grab rx vlan data */
    //FIXME:
    //if (state->PARSER_INFO.ftag_v > 0)
    //{
    //    rxFtagVpri = state->RX_DATA[2] >> 4;
    //}

    if (c->numVlans == 1)
    {
        /* assume that we don't need to check vlan 2 first */
        if (c->rxVlan1)
        {
            rxVlan1Type = c->rx_tags[0]; /*X repetitive operation*/
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) c->rx_tags[1];
            rxVpri1 = c->rx_tags[2] >> 4;
            rxVid1 = (c->rx_tags[2] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) c->rx_tags[3];
        }
        else
        {
            rxVlan2Type = c->rx_tags[0];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) c->rx_tags[1];
            rxVpri2 = c->rx_tags[2] >> 4;
            rxVid2 = (c->rx_tags[2] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) c->rx_tags[3];
        }
    }
    else if (c->numVlans == 2)
    {
        if (state->PARSER_INFO.otr_l2_v2first)
        {
            rxVlan2Type = c->rx_tags[0];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) c->rx_tags[1];
            rxVpri2 = c->rx_tags[2] >> 4;
            rxVid2 = (c->rx_tags[2] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) c->rx_tags[3];
            rxVlan1Type = c->rx_tags[4];
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) c->rx_tags[5];
            rxVpri1 = c->rx_tags[6] >> 4;
            rxVid1 = (c->rx_tags[6] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) c->rx_tags[7];

        }
        else
        {
            rxVlan1Type = c->rx_tags[0];
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) c->rx_tags[1];
            rxVpri1 = c->rx_tags[2] >> 4;
            rxVid1 = (c->rx_tags[2] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) c->rx_tags[3];
            rxVlan2Type = c->rx_tags[4];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) c->rx_tags[5];
            rxVpri2 = c->rx_tags[6] >> 4;
            rxVid2 = (c->rx_tags[6] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) c->rx_tags[7];
        }
    }

    /* VLAN tag lookups: */
    modVid1MapAddr = evidB;
    modVid2MapAddr = evidB;
    modVlanTagAddr = evidB;
    c->evidB = evidB;
    switch (r->modPerPortCfg1.VID2_MAP_INDEX) {
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_VID:
            break;
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_SGLORT:
            modVid2MapAddr = state->SGLORT & 0xFFF;
            break;
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_DGLORT:
            modVid2MapAddr = c->dglortA & 0xFFF;
            break;
        defualt:
            PRINT2(modDisplayVerbose, key, "vid2MapIndex = %0d (UNDEFINED!!)\n",
                r->modPerPortCfg1.VID2_MAP_INDEX);
            break;
    }

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_VID1_MAP(modVid1MapAddr, 0));

    newVid1 = FM_GET_FIELD64(*rdval, HLP_MOD_VID1_MAP, VID);

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_VLAN_TAG(modVlanTagAddr, 0));
    temp64 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_TAG, TAG);
    vtag = ((temp64 >> state->TX_PORT) & 0x01);

    PRINT2(modDisplayVerbose, key, "MOD_VID1_MAP[%0d] = %0x\n", modVid1MapAddr, newVid1);
    PRINT2(modDisplayVerbose, key, "MOD_VLAN_TAG[%0d] = %016llx\n", modVlanTagAddr, temp64);

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_VID2_MAP(modVid2MapAddr, 0));
    newVid2 = FM_GET_FIELD64(*rdval, HLP_MOD_VID2_MAP, VID);
    PRINT2(modDisplayVerbose, key, "MOD_VID2_MAP[%0d] = %0x\n", modVid2MapAddr, newVid2);

    c->txVid1  = newVid1;
    c->txVid2  = newVid2;

    // TODO, if leave-as-rx should we use rxvid also as final vid?
    ExtractPriorityProfile(key, model, r, c);

    newVpri1 = GetVPRI(model, 1, state->QOS_L2_VPRI1, c->priority_profile, key);
    newVpri2 = GetVPRI(model, 2, state->QOS_L2_VPRI1, c->priority_profile, key);

    PRINT2(modDisplayVerbose, key, "enablePcp1Update = %0d\n", r->modPerPortCfg2.ENABLE_PCP1_UPDATE);
    PRINT2(modDisplayVerbose, key, "enableDei1Update = %0d\n", r->modPerPortCfg2.ENABLE_DEI1_UPDATE);
    PRINT2(modDisplayVerbose, key, "enablePcp2Update = %0d\n", r->modPerPortCfg2.ENABLE_PCP2_UPDATE);
    PRINT2(modDisplayVerbose, key, "enableDei2Update = %0d\n", r->modPerPortCfg2.ENABLE_DEI2_UPDATE);
    PRINT2(modDisplayVerbose, key, "rxVpri1 = 0x%01x\n", rxVpri1);
    PRINT2(modDisplayVerbose, key, "rxVid1  = 0x%03x\n", rxVid1);
    PRINT2(modDisplayVerbose, key, "rxVpri2 = 0x%01x\n", rxVpri2);
    PRINT2(modDisplayVerbose, key, "rxVid2  = 0x%03x\n", rxVid2);
    PRINT2(modDisplayVerbose, key, "newVpri1 = 0x%01x (from MOD_VPRI1_MAP)\n", newVpri1);
    PRINT2(modDisplayVerbose, key, "newVid1  = 0x%03x (from MOD_VID1_MAP)\n", newVid1);
    PRINT2(modDisplayVerbose, key, "newVpri2 = 0x%01x (from MOD_VPRI2_MAP)\n", newVpri2);
    PRINT2(modDisplayVerbose, key, "newVid2  = 0x%03x (from MOD_VID2_MAP)\n", newVid2);

    /* EAS Step 2a : Update VLAN Fields, Non-mirrored and non-special frames */
    if (!c->isMirror)
    {
        /* figure out what action will be taken on each vlan */
        if (state->TX_TAG == HLP_MODEL_INSERT) {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
            c->txVlan1 = (c->txVid1 != 0);
            c->txVlan2 = (c->txVid2 != 0);
            c->preserveVlan1 = (c->rxVlan1) != 0;
            c->preserveVlan2 = (c->rxVlan2) != 0;

            c->numVlans += 2;
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = INSERT, if "
                "txVid1/txVid2 != 0); any RX'd tags will also be transmitted\n",
                state->TX_TAG);
        }
        else if (state->TX_TAG == HLP_MODEL_DELETE)
        {
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = DELETE); no "
                "new or RX'd tags will be transmitted\n",
                state->TX_TAG);
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
            if(!(c->rxVlan1 && c->rxVlan2)) {
                PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
            }
            c->numVlans = 0;
        }
        else if (state->TX_TAG == HLP_MODEL_UPDATE_ADD)
        {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
            c->txVlan1 = (c->txVid1 != 0);
            c->txVlan2 = (c->txVid2 != 0);
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = UPDATE or "
                "ADD, if txVid1/txVid2 != 0); if txVlan1 or txVlan2, new "
                "tag data will be used\n",
                state->TX_TAG);
            if (!c->rxVlan1 && c->txVlan1)
                c->numVlans++;
            if (!c->rxVlan2 && c->txVlan2)
                c->numVlans++;
        }
        else
        {   /* TX_TAG == HLP_MODEL_NORMAL_TAGGING */
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (NORMAL TAGGING)\n", state->TX_TAG);
            PRINT2(modDisplayVerbose, key, "PTAG = %0d\n", r->modPerPortCfg2.VLAN_TAGGING);
            PRINT2(modDisplayVerbose, key, "VTAG = %0d\n", vtag);

            /* note that some ptag/vtag combo's are (intentionally) duplicates;
             *  this is coded per the table in EAS, in the effort to make
             *  maintenance easier if one combo changes and another does not */
            switch (r->modPerPortCfg2.VLAN_TAGGING)
            {
                case 5:
                    c->txVid1  = rxVid1;
                    c->txVid2  = rxVid2;
                    c->txVlan1 = (c->rxVlan1 != 0);
                    c->txVlan2 = (c->rxVlan2 != 0);
                    PRINT2(modDisplayVerbose, key, "vid1 = leave-as-rx, vid2 = leave-as-rx\n");
                    break;
                case 0:
                    if (vtag == 0) { /*X similar operation*/
                        /* leave-as-rx allows tag to transmit even if txVid2 == 0 */
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

                        c->txVid2  = rxVid2;
                        c->txVlan2 = (c->rxVlan2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = leave-as-rx\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVid2  = rxVid2;
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->rxVlan2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1 != 0), vid2 = leave-as-rx\n");
                    }
                    break;
                case 1:
                    if (vtag == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

                        c->txVid2  = rxVid2;
                        c->txVlan2 = (c->rxVlan2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = leave-as-rx\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                    }
                    else
                    {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan2 = (c->txVid2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = update-or-add (if txVid2 != 0)\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                    }
                    break;
                case 2:
                    if (vtag == 0) {
                        // TODO is possible 4 cstum tags comes in?
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVid2  = rxVid2;
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->rxVlan2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1" "!= 0), vid2 = leave-as-rx\n");
                    }
                    else {

                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->txVid2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1" "!= 0), vid2 = update-or-add (if txVid2 != 0)\n");
                    }
                    break;
                case 3:
                    if (vtag == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan2 = (c->txVid2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = " "update-or-add (if txVid2 != 0)\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->txVid2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1"
                            "!= 0), vid2 = update-or-add (if txVid2 != 0)\n");
                    }
                    break;
                case 4:
                    if (vtag == 0)
                    {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = delete\n");
                        if(!(c->rxVlan1 && c->rxVlan2)) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        c->numVlans = 0;
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan1 = (c->txVid1 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if "
                            "txVid1 != 0), vid2 = delete\n");
                        if(!c->rxVlan2) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                    }
                    break;
                default:
                    PRINT2(modDisplayVerbose, key, "ptag = %0d (UNDEFINED!!)\n",
                        r->modPerPortCfg2.VLAN_TAGGING);
                    break;
            }
        }
    }

    if (!c->isMirror)
    {
        /* VPRI Updates (step 2a and 2c) */
        c->txVpri1 = 0;
        c->txVpri2 = 0;
        if ((c->rxVlan1) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_PCP1_UPDATE == 0))
        {
            c->txVpri1 |= (rxVpri1 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri1 PCP (bits 3:1) set to rxVpri1 PCP "
                "because ingress VLAN1 present and enablePcp1Update = 0\n");
        }
        //FIXME:
        /*else if ((state->PARSER_INFO.ftag_v > 0) &&
                 (r.modPerPortCfg2.ENABLE_PCP1_UPDATE == 0))
        {
            txVpri1 |= (rxFtagVpri & 0x0E);
            PRINT(key, "txVpri1 PCP (bits 3:1) set to rxFtagVpri "
                "PCP because ingress FTAG present and enablePcp1Update = 0\n");
        }*/
        else
        {
            c->txVpri1 |= (newVpri1 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri1 PCP (bits 3:1) set to newVpri1 "
                "PCP (from map)\n");
        }

        if ((c->rxVlan1) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_DEI1_UPDATE == 0))
        {
            c->txVpri1 = ((c->txVpri1) & 0xFE) | (rxVpri1 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to rxVpri1 DEI "
                "because ingress VLAN1 present and enableDei1Update = 0\n");
        }
        //FIXME:
        /*else if ((state->PARSER_INFO.ftag_v > 0) &&
                 (r.modPerPortCfg2.ENABLE_DEI1_UPDATE == 0))
        {
            txVpri1 = (txVpri1 & 0xFE) | (rxFtagVpri & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to rxFtagVpri DEI "
                "because ingress FTAG present and enableDei1Update = 0\n");
        }*/
        else
        {
            c->txVpri1 = (c->txVpri1 & 0xFE) | (newVpri1 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to newVpri1 DEI "
                "(from map)\n");
        }

        if ((c->rxVlan2) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_PCP2_UPDATE == 0))
        {
            c->txVpri2 |= (rxVpri2 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri2 PCP (bits 3:1) set to rxVpri2 PCP "
                "because ingress VLAN2 present and enablePcp2Update = 0\n");
        }
        else
        {
            c->txVpri2 |= (newVpri2 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri2 PCP (bits 3:1) set to newVpri2 "
                "PCP (from map)\n");
        }

        if ((c->rxVlan2) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_DEI2_UPDATE == 0))
        {
            c->txVpri2 = (c->txVpri2 & 0xFE) | (rxVpri2 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri2 DEI (bit 0) set to rxVpri2 DEI "
                "because ingress VLAN2 present and enableDei2Update = 0\n");
        }
        else
        {
            c->txVpri2 = (c->txVpri2 & 0xFE) | (newVpri2 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri2 DEI (bit 0) set to newVpri2 DEI "
                "(from map)\n");
        }
    }

    PRINT2(modDisplayVerbose, key, "txVlan1 = %0d\n", c->txVlan1);
    PRINT2(modDisplayVerbose, key, "txVlan2 = %0d\n", c->txVlan2);
    PRINT2(modDisplayVerbose, key, "preserveVlan1 = %0d\n", c->preserveVlan1);
    PRINT2(modDisplayVerbose, key, "preserveVlan2 = %0d\n", c->preserveVlan2);
    PRINT2(modDisplayVerbose, key, "txVpri1 = 0x%01x\n", c->txVpri1);
    PRINT2(modDisplayVerbose, key, "txVid1  = 0x%03x\n", c->txVid1);
    PRINT2(modDisplayVerbose, key, "txVpri2 = 0x%01x\n", c->txVpri2);
    PRINT2(modDisplayVerbose, key, "txVid2  = 0x%03x\n", c->txVid2);
}

void UpdateMacAddrIPP(fm_text key,
                      hlp_model *model,
                      hlp_modelModRegData *r,
                      hlp_modelModControlData *c,
                      hlp_modelChunkedSeg *chunkedSeg)
{
    fm_macaddr  origDmac = 0;
    fm_macaddr  origSmac = 0;
    fm_uint64	*modRouterSmac;
    fm_uint64   temp64;
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(r);

    PRINT(modDisplayVerbose, key, "------- Update MAC IPP -------\n");
    if (!c->isMirror) {
        for (int i = 0 ; i < MAC_ADDR_BYTES ; i++) {
	    	origDmac |= (fm_macaddr) chunkedSeg->otr_dmac[i];
	    	origSmac |= (fm_macaddr) chunkedSeg->otr_smac[i];
	    	if (i < 5) {
	    		origDmac <<= 8;
	    		origSmac <<= 8;
	    	}
	    }
        if (c->isRoutable && r->modPerPortCfg2.ENABLE_DMAC_ROUTING) {
        	/* modify DMAC */
            if(state->PARSER_INFO.otr_l2_len == 0 && !state->NO_MODIFY) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_MAC, INTR_OTR_MAC_NONEXIST);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: otr mac non-exist\n");
            }
        	for (int i = 0, bitOffset = 40 ;
                 i < MAC_ADDR_BYTES;
                 i++, bitOffset -= 8) {
        		chunkedSeg->otr_dmac[i] = GET_BYTE(state->L2_DMAC, bitOffset);
            }

            PRINT2(modDisplayVerbose, key, "Changed DMAC from 0x%012lx to 0x%012lx\n",
                (unsigned long int)origDmac, (unsigned long int)state->L2_DMAC);
        }
        else {
        	PRINT2(modDisplayVerbose, key, "No DMAC modification (0x%012lx)\n",
                (unsigned long int)origDmac);
        }

        if (c->isRoutable && r->modPerPortCfg2.ENABLE_SMAC_ROUTING) {
            /* modify SMAC */
            if(state->PARSER_INFO.otr_l2_len == 0 && !state->NO_MODIFY) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_MAC, INTR_OTR_MAC_NONEXIST);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: otr mac non-exist\n");
            }
            PRINT2(modDisplayVerbose, key, "L3_Domain = 0x%02x\n", c->l3_domain);
            modRouterSmac = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            		HLP_MOD_ROUTER_SMAC(c->l3_domain, 0));
            temp64 = FM_GET_FIELD64(*modRouterSmac, HLP_MOD_ROUTER_SMAC, SMAC);
            for (int i = 0, bitOffset = 40 ;
                 i < MAC_ADDR_BYTES ;
                 i++, bitOffset -= 8) {
            	chunkedSeg->otr_smac[i] = GET_BYTE(temp64, bitOffset);
            }

            PRINT2(modDisplayVerbose, key, "Changed SMAC from 0x%012lx to 0x%012lx "
                "(MOD_ROUTER_SMAC[L3_Domain])\n",
                (unsigned long int)origSmac, (unsigned long int)temp64);
        }
        else {
            PRINT2(modDisplayVerbose, key, "No SMAC modification (0x%012lx)\n",
                (unsigned long int)origSmac);
        }
    }
}

void UpdateVlanIPP(fm_text key,
                   hlp_model *model,
                   hlp_modelModRegData *r,
                   hlp_modelModControlData *c,
                   hlp_modelChunkedSeg *chunkedSeg)
{ // TODO can be further splited
    fm_byte	n_tag = 0;
    fm_uint64 *rdval;
    fm_uint16 txEtype1 = 0;
    fm_uint16 txEtype2 = 0;
    fm_byte tags[16];

    hlp_modelState *state  = &model->packetState;
    for (int i = 0; i < VLAN_TAG_BYTES*4; i++)
        tags[i] = 0;

    /* read etype values */
    if (c->txVlan1) {
        rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_VLAN_ETYPE(r->modPerPortCfg2.VLAN1_E_TYPE, 0));
        txEtype1 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_ETYPE, TAG_TYPE);
    }

    if (c->txVlan2) {
        rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_VLAN_ETYPE(r->modPerPortCfg2.VLAN2_E_TYPE, 0));
        txEtype2 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_ETYPE, TAG_TYPE);
    }

    /* Create New VLANS */
    if (c->txVlan1 && c->txVlan2) {
        if (r->modPerPortCfg2.VID2_FIRST) {
            CopyVlanToOutput(txEtype2, c->txVpri2, c->txVid2, &tags[0], 0);
            PRINT2(modDisplayVerbose, key, "VLAN2 data = 0x%02x%02x%02x%02x\n",
                tags[0], tags[1], tags[2], tags[3]);

            CopyVlanToOutput(txEtype1, c->txVpri1, c->txVid1, &tags[0], 1); /* X */
            PRINT2(modDisplayVerbose, key, "VLAN1 data = 0x%02x%02x%02x%02x\n",
				tags[4], tags[5], tags[6], tags[7]);
            n_tag += 2; /* Y */
        }
        else {
            CopyVlanToOutput(txEtype1, c->txVpri1, c->txVid1, &tags[0], 0);
            PRINT2(modDisplayVerbose, key, "VLAN1 data = 0x%02x%02x%02x%02x\n",
				tags[0], tags[1], tags[2], tags[3]);

            CopyVlanToOutput(txEtype2, c->txVpri2, c->txVid2, &tags[0], 1);
            PRINT2(modDisplayVerbose, key, "VLAN2 data = 0x%02x%02x%02x%02x\n",
            	tags[4], tags[5], tags[6], tags[7]);
            n_tag += 2;
        }
    }
    else if (c->txVlan1) {
        CopyVlanToOutput(txEtype1, c->txVpri1, c->txVid1, &tags[0], 0);
        PRINT2(modDisplayVerbose, key, "VLAN1 data = 0x%02x%02x%02x%02x\n",
			tags[0], tags[1], tags[2], tags[3]);
        n_tag++;
    }
    else if (c->txVlan2) {
        CopyVlanToOutput(txEtype2, c->txVpri2, c->txVid2, &tags[0], 0);
        PRINT2(modDisplayVerbose, key, "VLAN2 data = 0x%02x%02x%02x%02x\n",
			tags[0], tags[1], tags[2], tags[3]);
        n_tag++;
    }

    if (c->rxVlan1 && c->rxVlan2 &&
		!c->preserveVlan1 && !c->preserveVlan2) {
		    PRINT2(modDisplayVerbose, key, "Delete RX Vlan1 & RX Vlan2 and shift otr_tags bytes\n");
		    c->rx_n_tag -= 2;
            if (c->rx_n_tag < 0)
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                        "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                        c->rx_n_tag);
            for (int i = 0; i < c->rx_n_tag*VLAN_TAG_BYTES; i++)
		    	c->rx_tags[i] = c->rx_tags[i+2*VLAN_TAG_BYTES];
  	}
	/* Delete first tag in rx_tags */
	else if ((c->rxVlan1 && !c->preserveVlan1 && !c->rxV2first) ||
	  	 (c->rxVlan2 && !c->preserveVlan2 && c->rxV2first)  ||
           (c->rxVlan2 && !c->preserveVlan2 && !c->rxVlan1)) {
		PRINT2(modDisplayVerbose, key, "Delete first RX Vlan (by order) and shift otr_tags "
            "bytes\n");
		c->rx_n_tag--;
        if (c->rx_n_tag < 0)
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                    "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                    c->rx_n_tag);
		for (int i = 0; i < c->rx_n_tag*VLAN_TAG_BYTES; i++)
			c->rx_tags[i] = c->rx_tags[i+VLAN_TAG_BYTES];
	}
	/* Delete second tag in rx_tags */
	else if ((c->rxVlan1 && !c->preserveVlan1 && c->rxV2first) ||
			 (c->rxVlan2 && !c->preserveVlan2 && !c->rxV2first)) {
		PRINT2(modDisplayVerbose, key, "Delete second RX Vlan (by order) and shift otr_tags "
            "bytes\n");
		c->rx_n_tag--;
        if (c->rx_n_tag < 0)
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                    "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                    c->rx_n_tag);
		for (int i = 0; i < (c->rx_n_tag*VLAN_TAG_BYTES); i++)
			c->rx_tags[i+VLAN_TAG_BYTES] = c->rx_tags[i+2*VLAN_TAG_BYTES];
	}

    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && n_tag > 0 && !c->isMirror), ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
    for (int i = 0; i < (n_tag*VLAN_TAG_BYTES); i++)
        chunkedSeg->otr_tags[i] = tags[i];
    if ((n_tag+c->rx_n_tag) > 4) { // chop excessive tags since chunk seg can house up to 4 tags
        c->rx_n_tag = 4-n_tag;
        if (c->rx_n_tag < 0)
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                    "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                    c->rx_n_tag);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: can't insert vlan ipp");
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);// insert vlan during route
    }
    for (int i = 0; i < (c->rx_n_tag*VLAN_TAG_BYTES); i++)
        chunkedSeg->otr_tags[(n_tag*VLAN_TAG_BYTES)+i] = c->rx_tags[i];
    chunkedSeg->n_otr_tag = n_tag + c->rx_n_tag;

    PRINT2(modDisplayVerbose, key, "n_otr_tags=%0d, tx_tags=0x", chunkedSeg->n_otr_tag);
    for (int i = 0; i < chunkedSeg->n_otr_tag; i++) {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
            chunkedSeg->otr_tags[i*4], chunkedSeg->otr_tags[(i*4)+1],
            chunkedSeg->otr_tags[(i*4)+2], chunkedSeg->otr_tags[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");
}

/* return num of vlan tags in desc*/
fm_int NumL2VlanTag(fm_text key,
                    fm_byte *l2tag,
                    fm_int l2_tag_size) {
    fm_int vlan_num;
    vlan_num = 0;

    FM_NOT_USED(key);

    if(l2_tag_size == 1) {
        if((l2tag[0] == 0x81 && l2tag[1] == 0x00) || (l2tag[0] == 0x88 && l2tag[1] == 0xA8))
            vlan_num++;
    }
    else if(l2_tag_size > 1) {
        if((l2tag[0] == 0x81 && l2tag[1] == 0x00) || (l2tag[0] == 0x88 && l2tag[1] == 0xA8))
            vlan_num++;
        if((l2tag[4] == 0x81 && l2tag[5] == 0x00) || (l2tag[4] == 0x88 && l2tag[5] == 0xA8))
            vlan_num++;
    }
    return vlan_num;
}

void DropPacket(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c)
{
    hlp_modelState *state;
    state = &model->packetState;

    /* set TX_DISP initial value :
     *  this is the value that would egress with first segment's CmTxInfo;
     *  values seem to apply to the whole frame.
     *  Caveats : - does not predict anything based on ModCtrl.ps.err
     *               (RTL generates DISP_TXERRORDROP if 1st seg
     *               or DISP_TXERROR on other seg's)
     *            - does not predict anything based on ModCtrl.ps.is_timeout
    */
    state->TX_DISP = DISP_UCAST;
    state->TX_DROP = 0;
    c->cancelled_tx_disp = DISP_UCAST;

    /* kellysca: changed if to be based on xcast instead of egress DMAC
     * for stats registers because we use TX_DISP to pick with stats bin
     * to increment */
    /*if (fmModelIsBroadcastMacAddress(state->L2_DMAC)) */
    if (state->XCAST == 2)
    {
        state->TX_DISP = DISP_BCAST;
        PRINT2(modDisplayVerbose, key, "state->TX_DISP = DISP_BCAST (%0d) "
            "based on state->L2_DMAC\n",
            DISP_BCAST);
    }
    /*else if (fmModelIsMulticastMacAddress(state->L2_DMAC))*/
    else if (state->XCAST == 1)
    {
        state->TX_DISP = DISP_MCAST;
        PRINT2(modDisplayVerbose, key, "state->TX_DISP = DISP_MCAST (%0d) "
            "based on state->L2_DMAC\n",
            DISP_MCAST);
    }
    else
    {
        PRINT2(modDisplayVerbose, key, "state->TX_DISP = DISP_UCAST (%0d) "
            "based on state->L2_DMAC\n",
            DISP_UCAST);
    }

    // TODO marker pkt err only disp as marker error
//    if(!c->isMarkerPkt) {
        if (c->routeA && !(c->isMirror) && state->DROP_TTL && !state->TX_DROP) {
            //TODO same error code?
            DropAndLog(key, model, c, DISP_TTL1DROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, ERR_TTL_0, INTR_TTL1_DROP);
            PRINT2(modDisplayVerbose, key, "TTL1: setting state->TX_DISP = DISP_TTL1DROP (%0d) "
                "because because TTL <= 1\n",
                DISP_TTL1DROP);
        }

        if (c->loopbackSuppressDrop && !state->TX_DROP) {
            DropAndLog(key, model, c, DISP_LOOPBACKDROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, 0, INTR_LPBK_DROP);
            PRINT2(modDisplayVerbose, key, "LPBK: setting state->TX_DISP = DISP_LOOPBACKDROP (%0d)"
                " and state->TX_DROP = 1\n",
                DISP_LOOPBACKDROP);
        }
        /* prior to checking IS_TIMEOUT, the RTL checks for uncorrectable
         * SRAM errors here (and in a later stage as well);
         * TX_DISP set to DISP_TXECCDROP not supported by Modify WM stage */
        //istimeout pkt data sits in memory for too long that we internally
        //drop it
        if (state->IS_TIMEOUT) {
            DropAndLog(key, model, c, DISP_TIMEOUTDROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, 0, INTR_TIMEOUT_DROP);
            PRINT2(modDisplayVerbose, key, "TIMEOUT: setting state->TX_DISP = DISP_TIMEOUTDROP (%0d) "
                "and state->TX_DROP = 1 because state->IS_TIMEOUT = 1\n",
                DISP_TIMEOUTDROP);
        }
//    }
    // if OOM and pm_read.err both set at EOP, we count it as OOM in stats
    if ((state->OOM > 0 && state->OOM <= state->PM_ERR_NONSOP) || (state->OOM > 0 && state->PM_ERR_NONSOP == 0)) {
        if (!c->mirrorTrunc) { // if mirTrunc we only receive sop, shouldn't set oom at sop
            DropAndLog(key, model, c, DISP_OOMTRUNC, 0, NODROP, NOLOG, NOSKIP, 0, -1);
            PRINT2(modDisplayVerbose, key, "OOM: setting state->TX_DISP = DISP_OOMTRUNC (%0d)\n", DISP_OOMTRUNC);
        }
    }
//    else if ((state->SEG_META_ERR>0 && state->SEG_META_ERR!=1) ||
//             (state->SEG_META_ERR==1 && !c->mirrorTrunc)) { 
    else if (state->PM_ERR_NONSOP && !c->mirrorTrunc) { //
        state->SEG_META_ERR = 3;
        DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
        PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR casue pm_read.err on nonsop (%0d)\n", 3);
    }
    // Mirror trunc will always maintain seg_meta.err even recalc good crc
    else {// mirror trunc | no pm_err_nonsop
        if (state->RX_LENGTH > DEFAULT_SEGMENT_BYTES) {
            if (c->mirrorTrunc) {
                state->SEG_META_ERR = 0;
                PRINT2(modDisplayVerbose, key, "MirTrunc on Multi-seg: state->SEG_META_ERR to 0\n");
            }
            else {
                if(state->SEG_META_ERR) {
                    DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
                    PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR cause seg_meta.err (%0d)\n", state->SEG_META_ERR);
                }
            }
        }
        else { // single seg
            if(state->SEG_META_ERR) {
                DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
                PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR cause seg_meta.err (%0d)\n", state->SEG_META_ERR);
            }
        } 
    }
//    else if (state->SEG_META_ERR) { 
//    //else if (state->SEG_META_ERR && !c->mirrorTrunc) { 
//        DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
//        PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR cause seg_meta.err (%0d)\n", state->SEG_META_ERR);
//    }


    if (state->SAF_ERROR) {
        if (!c->isMarkerPkt) {
            DropAndLog(key, model, c, DISP_TXERRORDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_TX_ERR_DROP);
            PRINT2(modDisplayVerbose, key, "SAF: setting state->TX_DISP = DISP_TXERR_ORDROP (%0d) "
                "and state->TX_DROP = 1 because state->SAF_ERR_OR = 1\n",
                DISP_TXERRORDROP);
        }
        else {
            DropAndLog(key, model, c, DISP_MARKERERRDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_MARKER_ERR_DROP);
            PRINT2(modDisplayVerbose, key, "MARKER: setting state->TX_DISP = DISP_MARKERERRDROP (%0d) "
                "and state->TX_DROP = 1 because state->SAF_ERR_OR = 1\n",
                DISP_TXERRORDROP);
        }
    }
    if (state->PM_ERR) {
        if (!c->isMarkerPkt) {
            DropAndLog(key, model, c, DISP_TXECCDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_TX_ECC_DROP);
            PRINT2(modDisplayVerbose, key, "PM_ERR: setting state->TX_DISP = DISP_TXECCDROP (%0d) "
                "and state->TX_DROP = 1 because state->PM_ERR = 1\n",
                DISP_MARKERERRDROP);
        }
        else {
            DropAndLog(key, model, c, DISP_MARKERERRDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_MARKER_ERR_DROP);
            PRINT2(modDisplayVerbose, key, "PM_ERR: setting state->TX_DISP = DISP_MARKERERRDROP (%0d) "
                "and state->TX_DROP = 1 because state->PM_ERR = 1\n",
                DISP_MARKERERRDROP);
        }
    }
}

/*****************************************************************************/
/** PackPacket
 * \ingroup intModel
 *
 * \desc            Packs the chunkedSeg data structure into a packet.
 *
 * \param[out]		packet points to the output packet data.
 *
 * \param[in]		rx_packet points to the original packet data.
 *
 * \param[in]       chuckedSeg is a pointer to the unpacked packet.
 *
 * \param[in]		parserInfo is the information from the parser about what
 * 					header information is in the packet.
 *
 * \param[out]		tx_length is the length of the outgoing packet.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status PackPacket(fm_byte             *packet,
				 	        fm_byte             no_modify,
				 	        fm_uint32           rx_length,
				 	        fm_byte             *rx_packet,
                            hlp_modelChunkedSeg *chunkedSeg,
                            hlp_modelModControlData *c,
				 	        fm_uint32			*tx_length,
                            fm_text             key)
{
	fm_status status;
	status = FM_OK;

	fm_uint32 idx;
	fm_uint32 inr_l2_ptr;

	idx = 0;
	inr_l2_ptr =0;

    // isWindowParsing parser_info is 0, however no_modify not neccessary
    if (!c->isWindowParsing && !no_modify) {
        /* FTAG */
        if (chunkedSeg->ftag_v)
        {
            for (int i = 0; i < 8; i++, idx++)
                packet[idx] = chunkedSeg->ftag[i];
        }
        /* OTR ETH */
        for (int i = 0; i < 6; i++, idx++)
            packet[idx] = chunkedSeg->otr_dmac[i];
        for (int i = 0; i < 6; i++, idx++)
            packet[idx] = chunkedSeg->otr_smac[i];
        /* VLAN & Custom Tags */
        for (int i = 0; (i < (4*chunkedSeg->n_otr_tag) && i<16); i++, idx++)
            packet[idx] = chunkedSeg->otr_tags[i];
        /* Etype */
        for (int i = 0; i < 2; i++, idx++)
            packet[idx] = chunkedSeg->otr_et[i];
        /* OTR MPLS */
        for (int i = 0; i < (4*chunkedSeg->n_otr_mpls); i++, idx++)
            packet[idx] = chunkedSeg->otr_mpls[i];
        /* OTR IP */
        for (int i = 0; i < (4*chunkedSeg->otr_ip_size); i++, idx++)
            packet[idx] = chunkedSeg->otr_ip[i];
        /* Layer 4 */
        if (chunkedSeg->otr_udp_v)
        {
            for (int i = 0; i < (8 + chunkedSeg->tun_size_in_l4_chunk*4); i++, idx++)
                packet[idx] = chunkedSeg->otr_l4[i];
            for (int i = 0; i < chunkedSeg->tun_opt_size*4; i++, idx++)
                packet[idx] = chunkedSeg->tun_opt[i];
        }
        else if (chunkedSeg->otr_tcp_v)
        {
            for (int i = 0; i < 18; i++, idx++)
                packet[idx] = chunkedSeg->otr_l4[i];
        }
        else if (chunkedSeg->tun_size_in_l4_chunk || chunkedSeg->tun_opt_size) {
            for (int i = 0; i < chunkedSeg->tun_size_in_l4_chunk*4; i++, idx++)
                packet[idx] = chunkedSeg->otr_l4[i];
            for (int i = 0; i < chunkedSeg->tun_opt_size*4; i++, idx++)
                packet[idx] = chunkedSeg->tun_opt[i];
        }
        /* Inner */
        if (chunkedSeg->inr_l2_v)
        {
            /* ETH */
            for (int i = 0; i < 6; i++, idx++)
                packet[idx] = chunkedSeg->inr_dmac[i];
            for (int i = 0; i < 6; i++, idx++)
                packet[idx] = chunkedSeg->inr_smac[i];
            /* VLAN & Custom Tags */
            for (int i = 0; i < (4*chunkedSeg->n_inr_tag); i++, idx++)
                packet[idx] = chunkedSeg->inr_tags[i];
            /* Etype */
            for (int i = 0; i < 2; i++, idx++)
                packet[idx] = chunkedSeg->inr_et[i];
            /* INR MPLS */
            for (int i = 0; i < (4*chunkedSeg->n_inr_mpls); i++, idx++)
                packet[idx] = chunkedSeg->inr_mpls[i];
        }
        {
            /* IP */
            /* inr_ip_size is expressed as a multiple of 4B */
            for (int i = 0; i < (chunkedSeg->inr_ip_size*4); i++, idx++)
                packet[idx] = chunkedSeg->inr_ip[i];
            if (chunkedSeg->inr_udp_v)
            {
                for (int i = 0; i < 8; i++, idx++)
                    packet[idx] = chunkedSeg->inr_l4[i];
            }
            if (chunkedSeg->inr_tcp_v)
            {
                for (int i = 0; i < 18; i++, idx++)
                    packet[idx] = chunkedSeg->inr_l4[i];
            }
        }
    }
    //else if (c->isWindowParsing) {
    //    if (c->isMirror) {
    //        for (int i = 0; i < 6; i++, idx++)
    //            packet[idx] = rx_packet[idx];
    //        for (int i = 0; i < 6; i++, idx++)
    //            packet[idx] = rx_packet[idx];
    //        chunkedSeg->payload_start += idx;
    //        chunkedSeg->payload_size -= idx;
    //        for (int i = 0; (i < (4*chunkedSeg->n_otr_tag) && i<16); i++, idx++)
    //            packet[idx] = chunkedSeg->otr_tags[i];

    //    }
    //}

    if(!no_modify) {
	    for (int i = 0; i < chunkedSeg->payload_size; i++, idx++)
	    	packet[idx] = rx_packet[i + chunkedSeg->payload_start];

        PRINT2(modDisplayVerbose, key, "tx_length=%0d; chunkedSeg->payload_start=%0d; "
                "chunkedSeg->payload_size=%0d;\n",
                idx, chunkedSeg->payload_start, chunkedSeg->payload_size);
	    *tx_length = idx;
    }
    else {
        for (unsigned int i = 0; i < rx_length; i++, idx++)
      	    packet[idx] = rx_packet[i];
        *tx_length = rx_length;
    }

	return status;
}

void MiscOps(fm_text key,
             hlp_model *model,
             hlp_modelModRegData *r,
             hlp_modelModControlData *c,
             hlp_modelChunkedSeg *chunkedSeg,
             fm_byte *packet)
{
    hlp_modelState *state  = &model->packetState;
    fm_byte crc_idx = 0;

    FM_NOT_USED(chunkedSeg);

    if (state->TX_LENGTH >= state->RX_LENGTH) {
        c->bytesAdded = (state->TX_LENGTH - state->RX_LENGTH);
        c->egressSeg0Bytes = (state->RX_LENGTH >= 192) ?
                           192 + (uint) c->bytesAdded : state->TX_LENGTH;
    }
    else {
        c->bytesAdded = (state->RX_LENGTH - state->TX_LENGTH);
        c->egressSeg0Bytes = (state->RX_LENGTH >= 192) ?
                           192 - (uint) c->bytesAdded: state->TX_LENGTH;
        c->bytesAdded |= 0x80; /* MS bit set -> negative value */
    }
    if (c->mirrorTrunc)
    {
        if ((int)(state->RX_LENGTH) >= (int)(DEFAULT_SEGMENT_BYTES+4))
            crc_idx = 4;
        else if ((int)(state->RX_LENGTH) == (int)(DEFAULT_SEGMENT_BYTES+3))
            crc_idx = 3;
        else if ((int)(state->RX_LENGTH) == (int)(DEFAULT_SEGMENT_BYTES+2))
            crc_idx = 2;
        else if ((int)(state->RX_LENGTH) == (int)(DEFAULT_SEGMENT_BYTES+1))
            crc_idx = 1;
        else
            crc_idx = 0;

        if (crc_idx != 0) {
            if (c->bytesAdded != 0) {
                if (c->bytesAdded & 0x80) {
                    fm_byte bytesDeleted;
                    bytesDeleted = c->bytesAdded & 0x7F;
                    state->TX_LENGTH =(fm_uint32)
                        DEFAULT_SEGMENT_BYTES-bytesDeleted+crc_idx;
                }
                else {
                        state->TX_LENGTH =(fm_uint32)
                            DEFAULT_SEGMENT_BYTES+c->bytesAdded+crc_idx;

                }
            }
            else {
                state->TX_LENGTH = (fm_uint32)DEFAULT_SEGMENT_BYTES+crc_idx;
            }
            // when pkt is larger than 192, we add crc to sop seg when
            // trunc, so stats needs adjusted

            c->egressSeg0Bytes = state->TX_LENGTH;
            PRINT2(modDisplayVerbose, key, "Truncating frame to %0d bytes\n", state->TX_LENGTH);
        }
    }

    // calculate good egress crc
    c->crc_egress = fmCrc32(packet, state->TX_LENGTH-4);

    // apply ingress crc error differential onto egress crc
    // use recalculated egress CRC instead of incremental update for mirror
    // trunc frame, that is how RTL behaves now
    if (!c->mirrorTrunc) {
        c->crc_egress ^= c->crc_ingress_diff;
    }

    for (int i = 0; i < 4; i++)
        packet[state->TX_LENGTH-4+i] = (c->crc_egress >> (i*8)) & 0x0FF;

    PRINT2(modDisplayVerbose, key, "CRC updated to: 0x%2x%2x%2x%2x\n",
        packet[state->TX_LENGTH-4],
        packet[state->TX_LENGTH-3],
        packet[state->TX_LENGTH-2],
        packet[state->TX_LENGTH-1]);

    PRINT2(modDisplayVerbose, key, "bytesAdded by modify = %0d (high bit set -> bytes "
        "subtracted)\n", c->bytesAdded);


    /* mkhan3: egressSeg0Bytes needs to be set properly as well if TX_LENGTH <= MIN_EGRESS_BYTES */
	if (state->TX_LENGTH < MIN_EGRESS_BYTES) {
		PRINT2(modDisplayVerbose, key, "MISCONFIG: segment size < 18B after modification\n");
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, NOSKIP, ERR_SEG_S_18, INTR_SMALLER_18B);
	}

    c->refcntSeg0Bytes = c->egressSeg0Bytes;
    c->refcnt_tx_len = state->TX_LENGTH;
    if (r->modPerPortCfg2.MIN_FRAME_SIZE && state->TX_LENGTH < 64) {
		PRINT2(modDisplayVerbose, key, "MinFrameSize asserted, state->TX_LENGTH was %0d; setting refcnt tx_len to 64 bytes\n", state->TX_LENGTH);
        c->refcnt_tx_len = 64;
        c->refcntSeg0Bytes = 64;
    }

}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelModify
 * \ingroup intModel
 *
 * \desc            Modifies incoming packet based on information from prior
 *                  stages, and stores new packet into parameter
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       packet points to output packet buffer
 *
 * \param[in]       maxPktSize is number of caller-allocated bytes in packet
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 * \see             MODIFY section of EAS for HLP
 *
 *****************************************************************************/
fm_status hlpModelModify(hlp_model *         model,
                         fm_byte *           packet,
                         fm_uint32           maxPktSize)
{
    hlp_modelState *        state = &model->packetState;
    fm_text                 key = fmAlloc(20);
    fm_status               status;
    int rval = 0;

    /* data from configuration registers and state->RX_TAG */
    hlp_modelModRegData     r;
    fm_bool                 portIsFtagged = 0;
    hlp_modelModControlData	c;

    /* Chunked Packet */
    hlp_modelChunkedSeg		chunkedSeg;

    FM_NOT_USED(maxPktSize);

    sprintf(key, "%0x_%0d_%0d", state->ADDR, state->TX_PORT,
        state->MOD_IP_MCAST_IDX);

    FM_CLEAR(chunkedSeg);
    FM_CLEAR(c);
    FM_CLEAR(r);

    if (testPlusArgs("HLP_MOD_WM_PRINT_VERBOSE") >= 0)
        modDisplayVerbose = testPlusArgs("HLP_MOD_WM_PRINT_VERBOSE");
    else
        modDisplayVerbose = 1;

    status = FM_OK;

    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  MODIFY WM \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    GetModRegData(key, model, &r);

    CalcIngressCRC(key, model, &c);

    InitChunkedSeg(key, model, packet, &chunkedSeg);

    InitControl(key, model, &r, &c, &chunkedSeg);

    /*******************************************************
     * L2 Modifications
     *******************************************************/
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  L2 MODIFICATIONS \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    GetRxL2Tags(key, model, &c, &chunkedSeg);

    DropPacket(key, model, &c);

    VlanLookup(key, model, &r, &c, &chunkedSeg);

    /* DMAC/SMAC update */
    PRINT2(modDisplayVerbose, key, "enableDmacRouting = %0d\n", r.modPerPortCfg2.ENABLE_DMAC_ROUTING);
    PRINT2(modDisplayVerbose, key, "enableSmacRouting = %0d\n", r.modPerPortCfg2.ENABLE_SMAC_ROUTING);
    c.isRoutable = c.routeA &&
        (state->TX_TAG == HLP_MODEL_NORMAL_TAGGING) &&
        !(c.isMirror);

    PRINT2(modDisplayVerbose, key, "isRoutable:%0d, (%0d && %0d && %0d) && %0d\n",
        c.isRoutable, c.routeA, (state->TX_TAG == HLP_MODEL_NORMAL_TAGGING),
        (state->PARSER_INFO.otr_l3_len > 0), !(c.isMirror));

    UpdateMacAddrIPP(key, model, &r, &c, &chunkedSeg);

    /* Step 3 in EAS: Construct VLAN Tags (and copy to output) */
    UpdateVlanIPP(key, model, &r, &c, &chunkedSeg);

DONE:
    PackPacket(packet, state->NO_MODIFY, state->RX_LENGTH, state->RX_DATA, &chunkedSeg, &c, &state->TX_LENGTH, key);

    // if minFrameSize, updatePktmeta use min size
    MiscOps(key, model, &r, &c, &chunkedSeg, packet);

    // TODO
    if (state->PM_ERR_NONSOP > 1 && !c.mirrorTrunc && !state->TX_DROP) {
        state->TX_STATS_LENGTH = c.egressSeg0Bytes + ((state->PM_ERR_NONSOP-1)*256) + state->TX_STATS_LAST_LEN;
    }
    else if (state->PM_ERR_NONSOP > 0 && !c.mirrorTrunc && !state->TX_DROP) {
        state->TX_STATS_LENGTH = c.egressSeg0Bytes + state->TX_STATS_LAST_LEN;
    }
    else {
        state->TX_STATS_LENGTH = c.refcnt_tx_len;
    }

    state->SEG_DROP = state->TX_DROP;

	PRINT2(modDisplayVerbose, key, "egressSeg0Bytes = %0d (includes egress FTAG)\n", c.egressSeg0Bytes);

    //TODO if(!c.skipStats)
        hlpModelStatsTx(model);
    if(!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpModify);

    fmFree(key);

    return status;
}   /* end hlpModelModify */
#endif
