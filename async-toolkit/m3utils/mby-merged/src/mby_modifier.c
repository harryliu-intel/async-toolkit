// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation
#include "mby_rxtotx.h"
#include "mby_modifier.h"
#include "fm_crc32.h"

static void getModRegData
(
    fm_uint32             regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32       tx_port,
    mbyModRegData * const r
)
{
    fm_uint64 mod_per_port_cfg1 = 0;
    mbyModelReadCSR64(regs, MBY_MOD_PER_PORT_CFG1(tx_port, 0), &mod_per_port_cfg1);

    r->modPerPortCfg1.ENABLE_VLAN_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg1, MBY_MOD_PER_PORT_CFG1, ENABLE_VLAN_UPDATE);
    r->modPerPortCfg1.VID2_MAP_INDEX          = FM_GET_FIELD64(mod_per_port_cfg1, MBY_MOD_PER_PORT_CFG1, VID2_MAP_INDEX);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT = FM_GET_FIELD64(mod_per_port_cfg1, MBY_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_GLORT);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK  = FM_GET_FIELD64(mod_per_port_cfg1, MBY_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_MASK);

    fm_uint64 mod_per_port_cfg2 = 0;
    mbyModelReadCSR64(regs, MBY_MOD_PER_PORT_CFG2(tx_port, 0), &mod_per_port_cfg2);
                                                                     
    r->modPerPortCfg2.ENABLE_DMAC_ROUTING     = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_DMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_SMAC_ROUTING     = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_SMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_TTL_DECREMENT    = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_TTL_DECREMENT);
    r->modPerPortCfg2.ENABLE_ECN_MODIFICATION = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_ECN_MODIFICATION);
    r->modPerPortCfg2.VLAN2_E_TYPE            = FM_GET_FIELD64(mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, VLAN2_E_TYPE);
    r->modPerPortCfg2.VLAN1_E_TYPE            = FM_GET_FIELD64(mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, VLAN1_E_TYPE);
    r->modPerPortCfg2.ENABLE_DEI2_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_DEI2_UPDATE);
    r->modPerPortCfg2.ENABLE_DEI1_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_DEI1_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP2_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_PCP2_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP1_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, ENABLE_PCP1_UPDATE);
    r->modPerPortCfg2.VID2_FIRST              = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, VID2_FIRST);
    r->modPerPortCfg2.VLAN_TAGGING            = FM_GET_FIELD64(mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, VLAN_TAGGING);
    r->modPerPortCfg2.MIN_FRAME_SIZE          = FM_GET_BIT64  (mod_per_port_cfg2, MBY_MOD_PER_PORT_CFG2, MIN_FRAME_SIZE);
}

static void calcIngressCRC
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    fm_byte           * const rx_data,
    const fm_uint32           rx_length,
    mbyModControlData * const c
)
{
    fm_uint32 ingress_crc = 0;
    for (fm_uint i = 0; i < 4; i++)
        ingress_crc |= rx_data[rx_length - 4 + i] << (i * 8);

    // calculate good ingress crc
    fm_uint32 good_ingress_crc = fmCrc32(rx_data, rx_length-4);

    // calculate ingress crc error differential
    fm_uint32 crc_ingress_diff = good_ingress_crc ^ ingress_crc;

    if (crc_ingress_diff != 0) { /* Ingress packet has bad CRC */ }

    c->crc_ingress_diff = crc_ingress_diff;
}

static void unpackPacket
(
    const fm_byte * const rx_data,
    const fm_uint32	  rx_length,
    const mbyParserInfo   parser_info,
    mbyChunkedSeg * const chunked_seg
)
{
    // OTR ETH:
    fm_uint32 otr_l2_len = (parser_info.otr_l2_len > 5)
        ? 5 // "WARNING: parser_info.otr_l2_len overrun 30B otr_l2 chunk segment"
        : parser_info.otr_l2_len;

    fm_uint32 idx = 0;
    
    if (otr_l2_len > 0) {
        for (fm_uint i = 0; i < 6; i++, idx++)
            chunked_seg->otr_dmac[i] = rx_data[idx];
        for (fm_uint i = 0; i < 6; i++, idx++)
            chunked_seg->otr_smac[i] = rx_data[idx];
        chunked_seg->n_otr_tag = (otr_l2_len - 1);
        for (fm_uint i = 0; i < (4*chunked_seg->n_otr_tag); i++, idx++)
            chunked_seg->otr_tags[i] = rx_data[idx];
        // Etype:
        for (fm_uint i = 0; i < 2; i++, idx++)
            chunked_seg->otr_et[i] = rx_data[idx];
    }

    // OTR MPLS:
    chunked_seg->n_otr_mpls = (parser_info.otr_mpls_len > 7)
        ? 7 // "WARNING: parser_info.otr_mpls_len overrun 28B otr_mpls chunk segment
        : parser_info.otr_mpls_len;

    chunked_seg->n_otr_mpls_pre = parser_info.otr_mpls_len;
    for (fm_uint i = 0; i < (4 * chunked_seg->n_otr_mpls); i++, idx++)
        chunked_seg->otr_mpls[i] = rx_data[idx];

    fm_uint32 tmp_idx = idx;
    for (fm_uint i = (4 * chunked_seg->n_otr_mpls); i < 28; i++, tmp_idx++)
        chunked_seg->otr_mpls[i] = rx_data[tmp_idx];

    // OTR L3:
    chunked_seg->otr_ip_size = (parser_info.otr_l3_len > 14)
        ? 14 // WARNING: parser_info.otr_l3_len overrun 56B otr_l3 chunk segment
        : parser_info.otr_l3_len;

    chunked_seg->otr_l3_v6 = parser_info.otr_l3_v6;
    for (fm_uint i = 0; i < (4 * chunked_seg->otr_ip_size); i++, idx++)
        chunked_seg->otr_ip[i] = rx_data[idx];

    // OTR L4:
    if (parser_info.otr_l4_udp && parser_info.otr_l4_tcp) { /* WARNING: cannot both be true */ }

    chunked_seg->otr_udp_v = parser_info.otr_l4_udp;
    chunked_seg->otr_tcp_v = parser_info.otr_l4_tcp;

    if (chunked_seg->otr_udp_v)
    {
        fm_uint32 otr_tun_len = (parser_info.otr_tun_len > 18)
            ? 18 // WARNING: parser_info.otr_tun_len overrun 80B combined otr_l4+tunnel_opt chunk segments"
            : parser_info.otr_tun_len;

        for (fm_uint i = 0; i < (8 + otr_tun_len*4); i++, idx++)
            if (i < 40)
                chunked_seg->otr_l4[i]     = rx_data[idx];
            else
                chunked_seg->tun_opt[i-40] = rx_data[idx];

        // it is not l4 hdr size, you can think of it is the tunnel hdr len in l4 chunk and tunnel chunk
        chunked_seg->tun_size_in_l4_chunk = (otr_tun_len < 8) ? otr_tun_len : 8;
        chunked_seg->tun_opt_size         = (otr_tun_len < 8) ? 0           : otr_tun_len - 8;
    }
    else if (chunked_seg->otr_tcp_v)
    {
        for (fm_uint i = 0; i < 18; i++, idx++)
            chunked_seg->otr_l4[i] = rx_data[idx];
    }
    else if (parser_info.otr_tun_len) // any tunnel protocol following L3 immediately
    {
        fm_uint32 otr_tun_len = (parser_info.otr_tun_len > 18)
            ? 18 // WARNING: parser_info.otr_tun_len overrun 80B combined otr_l4+tunnel_opt chunk segments
            : parser_info.otr_tun_len;

        for (fm_uint i = 0; i < (otr_tun_len * 4); i++, idx++)
            if (i < 40)
                chunked_seg->otr_l4[i]     = rx_data[idx];
            else
                chunked_seg->tun_opt[i-40] = rx_data[idx];

        chunked_seg->tun_size_in_l4_chunk = (otr_tun_len < 10) ? otr_tun_len : 10;
        chunked_seg->tun_opt_size         = (otr_tun_len < 10) ? 0           : otr_tun_len - 10;
    }

    // Inner:
    fm_uint32 inr_l2_len = (parser_info.inr_l2_len > 5)
        ? 5 // WARNING: parser_info.inr_l2_len overrun 30B inr_l2 chunk segment
        : parser_info.inr_l2_len;

    if (inr_l2_len > 0)
    {
        // ETH:
        chunked_seg->inr_l2_v = 1;
        for (fm_uint i = 0; i < 6; i++, idx++)
            chunked_seg->inr_dmac[i] = rx_data[idx];
        for (fm_uint i = 0; i < 6; i++, idx++)
            chunked_seg->inr_smac[i] = rx_data[idx];
        // VLAN & Custom Tags
        chunked_seg->n_inr_tag = parser_info.inr_l2_len - 1;
        for (fm_uint i = 0; i < (4 * chunked_seg->n_inr_tag); i++, idx++)
            chunked_seg->inr_tags[i] = rx_data[idx];
        // Etype:
        for (fm_uint i = 0; i < 2; i++, idx++)
            chunked_seg->inr_et[i] = rx_data[idx];
        // INR MPLS:
        chunked_seg->n_inr_mpls = (parser_info.inr_mpls_len > 7)
            ? 7 // WARNING: parser_info.inr_mpls_len overrun 28B inr_mpls chunk segment
            : parser_info.inr_mpls_len;
        for (int i = 0; i < (4 * chunked_seg->n_inr_mpls); i++, idx++)
            chunked_seg->inr_mpls[i] = rx_data[idx];
    }

    // INR L3:
    chunked_seg->inr_ip_size = (parser_info.inr_l3_len > 14)
        ? 14 // WARNING: parser_info.inr_l3_len overrun 56B inr_l3 chunk segment
        : parser_info.inr_l3_len;

    chunked_seg->inr_l3_v6 = parser_info.inr_l3_v6;
    for (fm_uint i = 0; i < (chunked_seg->inr_ip_size * 4); i++, idx++)
        chunked_seg->inr_ip[i] = rx_data[idx];

    // INR L4:
    if (parser_info.inr_l4_udp && parser_info.inr_l4_tcp) { /* WARNING: cannot both be true! */ }

    chunked_seg->inr_udp_v = parser_info.inr_l4_udp;
    chunked_seg->inr_tcp_v = parser_info.inr_l4_tcp;

    if (parser_info.inr_l4_udp)
        for (fm_uint i = 0; i < 8; i++, idx++)
            chunked_seg->inr_l4[i] = rx_data[idx];

    if (parser_info.inr_l4_tcp)
        for (fm_uint i = 0; i < 18; i++, idx++)
            chunked_seg->inr_l4[i] = rx_data[idx];

    chunked_seg->payload_start = idx;
    chunked_seg->payload_size  = rx_length - idx;
}

static void initChunkedSeg
(
    fm_uint32             regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyParserInfo   parser_info,
    fm_byte       * const rx_data,
    const fm_uint32       rx_length,
    mbyChunkedSeg * const chunked_seg
)
{
    unpackPacket(rx_data, rx_length, parser_info, chunked_seg);
#if 0

    SelectL4CsumSrc(key, model, state->RX_DATA, chunkedSeg);
    state->TX_REASONCODE = 0x00;

    out->NO_PRI_ENC = !((state->PARSER_INFO.otr_l2_vlan1>0)
                        || (state->PARSER_INFO.otr_mpls_len>0) || (state->PARSER_INFO.otr_l3_len>0));
#endif
}

static fm_bool isWindowParsing
(
    const mbyParserInfo parser_info
)
{
    fm_bool is_win =
        parser_info.otr_l2_len     == 0 &&
        parser_info.otr_l2_vlan1   == 0 &&
        parser_info.otr_l2_vlan2   == 0 &&
        parser_info.otr_l2_v2first == 0 &&
        parser_info.otr_mpls_len   == 0 &&
        parser_info.otr_l3_len     == 0 &&
        parser_info.otr_l3_v6      == 0 &&
        parser_info.otr_l4_udp     == 0 &&
        parser_info.otr_l4_tcp     == 0 &&
        parser_info.otr_tun_len    == 0 &&
        parser_info.inr_l2_len     == 0 &&
        parser_info.inr_l2_vlan1   == 0 &&
        parser_info.inr_l2_vlan2   == 0 &&
        parser_info.inr_l2_v2first == 0 &&
        parser_info.inr_mpls_len   == 0 &&
        parser_info.inr_l3_len     == 0 &&
        parser_info.inr_l3_v6      == 0 &&
        parser_info.inr_l4_udp     == 0 &&
        parser_info.inr_l4_tcp     == 0 ;

    return is_win;
}

static void initControl
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyParserInfo       parser_info,
    const fm_bool             no_modify,
    const fm_uint16           l2_evid1,
    const fm_uint16           edglort,
    const mbyMirrorType       mirtyp,
    const fm_byte             qos_l3_dscp,
    const fm_byte             ecn,
    const fm_bool             mark_routed,
    const fm_uint32           mod_idx,
    const fm_uint32           rx_length,
    const fm_uint32           tx_length,    
    const fm_uint64           tail_csum_len,
    const mbyChunkedSeg       chunked_seg,
    mbyModControlData * const ctrl_data
)
{
    mbyModControlData c;

    // Ingress flow 2nd pass window parsing packet: passing through
    c.isWindowParsing = (isWindowParsing(parser_info) && !no_modify);
    
    c.dvStatus              = IS_OK;
    c.isMarkerPkt           = 0;
    c.evidA                 = l2_evid1;
    c.dglortA               = edglort;
    c.isMirror              = ((mirtyp == MBY_MIRTYPE_MIR0) || (mirtyp == MBY_MIRTYPE_MIR1));
    c.mirrorTrunc           = 0;
    c.rx_n_tag              = 0;
    c.otrL3Modified         = 0;
    c.otrL4Modified         = 0;
    c.internalDS            = (qos_l3_dscp << 2) | (ecn & 0x03);
    c.isInterLSR            = 0;
    c.skipDscp              = 0;
    c.skipTtl               = 0;
    c.ecn_tx_drop           = 0;
    c.timeout_tx_drop       = 0;
    c.non_cm_tx_drop        = 0;
    c.cancelled_tx_disp     = 0;
    c.cancel_drop_on_marker = 0;
    c.ecn_mark              = 0;

#if 0
    FM_CLEAR(c.mplsData);  // skipping MPLS for now <-- REVISIT!!!
#endif

    // Grabbing l3Idx for checksum updates:
    if (chunked_seg.otr_ip_size > 0)
        c.l3Idx =
             8 * chunked_seg.ftag_v +
            14 +
             4 * chunked_seg.n_otr_tag +
             4 * chunked_seg.n_otr_mpls;

    if (chunked_seg.otr_udp_v | chunked_seg.otr_tcp_v)
        c.l4Idx =
             8 * chunked_seg.ftag_v +
            14 +
             4 * chunked_seg.n_otr_tag +
             4 * chunked_seg.n_otr_mpls +
             4 * chunked_seg.otr_ip_size;

    c.routeA               = mark_routed;
    c.vlanSwitched         = 0;
    c.loopbackSuppressDrop = 0;
    c.numVlans             = 0;

    if (parser_info.otr_l2_vlan1)
        c.numVlans++;

    if (parser_info.otr_l2_vlan2)
        c.numVlans++;

    c.rxVlan1         = 0;
    c.rxVlan2         = 0;
    c.rxV2first       = 0;
    c.txVlan1         = 0;
    c.txVlan2         = 0;
    c.preserveVlan1   = 0;
    c.preserveVlan2   = 0;
    c.txVpri1         = 0;
    c.txVpri2         = 0;
    c.txVid2          = 0;
    c.txVid2          = 0;
    c.evidB           = 0;
    c.modIdx          = mod_idx;
    c.encap           = 0;
    c.decap           = 0;
    c.bytesAdded      = 0;

    c.egressSeg0Bytes = (rx_length < DEFAULT_SEGMENT_BYTES) ? rx_length : DEFAULT_SEGMENT_BYTES;
    c.refcnt_tx_len   = tx_length;
    c.intr_occured    = 0;

    c.tail_len = (((tail_csum_len >> 16) & 0x3FFF) < DEFAULT_SEGMENT_BYTES)
        ? 0 : (((tail_csum_len >> 16) & 0x3FFF) - DEFAULT_SEGMENT_BYTES);
    
    fm_uint64 mod_im_reg = 0;
    mbyModelReadCSR64(regs, MBY_MOD_IM(0), &mod_im_reg);
    c.mod_im = FM_GET_UNNAMED_FIELD64(mod_im_reg, 0, 63);

    // calculate the correct ingress L3 total length for incremental len update
    c.igL3TotalLen =
        chunked_seg.otr_ip_size  *  4 + chunked_seg.otr_tcp_v            * 18 +
        chunked_seg.otr_udp_v    *  8 + chunked_seg.tun_size_in_l4_chunk *  4 +
        chunked_seg.tun_opt_size *  4 + chunked_seg.inr_l2_v             * 14 +
        chunked_seg.n_inr_tag    *  4 + chunked_seg.n_inr_mpls           *  4 +
        chunked_seg.inr_ip_size  *  4 + chunked_seg.inr_udp_v            *  8 +
        chunked_seg.inr_tcp_v    * 18 + chunked_seg.payload_size         -  4 ;

    if (chunked_seg.otr_l3_v6)
        c.igL3TotalLen -= 40;

    *ctrl_data = c;
}

void Modifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyTxInToModifier     * const in,
          mbyModifierToTxStats  * const out
)
{
    // Read inputs:
    mbyParserInfo parser_info       = in->PARSER_INFO;
    fm_bool       no_modify         = in->NO_MODIFY;  // skip most of modifications in Modifier
    fm_uint32     rx_length         = in->RX_LENGTH;  // ingress packet data length [bytes]
    fm_byte     * rx_data           = in->RX_DATA;
    fm_uint32     tx_port           = in->TX_PORT;
    fm_bool       tx_drop           = in->TX_DROP;
    fm_uint32     tx_length         = in->TX_LENGTH;
    fm_byte       tx_tag            = in->TX_TAG;
    fm_uint32     tx_stats_last_len = in->TX_STATS_LAST_LEN;
    fm_uint16     l2_evid1          = in->L2_EVID1;
    fm_uint16     edglort           = in->EDGLORT;
    mbyMirrorType mirtyp            = in->MIRTYP;
    fm_byte       qos_l3_dscp       = in->QOS_L3_DSCP;
    fm_byte       ecn               = in->ECN;
    fm_bool       mark_routed       = in->MARK_ROUTED;
    fm_uint32     mod_idx           = in->MOD_IDX;
    fm_uint64     tail_csum_len     = in->TAIL_CSUM_LEN;

    // input from the outside:
    fm_byte *packet;

    // Chunked Packet:
    mbyChunkedSeg chunked_seg;
    initChunkedSeg(regs, parser_info, rx_data, rx_length, &chunked_seg);

    mbyModRegData reg_data;
    getModRegData(regs, tx_port, &reg_data);

    mbyModControlData ctrl_data;
    calcIngressCRC(regs, rx_data, rx_length, &ctrl_data);

    initControl
    (
        regs,
        parser_info,
        no_modify,
        l2_evid1,
        edglort,
        mirtyp,
        qos_l3_dscp,
        ecn,
        mark_routed,
        mod_idx,
        rx_length,
        tx_length,
        tail_csum_len,
        chunked_seg,
        &ctrl_data
    );

    // L2 Modifications:

//> GetRxL2Tags(key, model, &ctrl_data, &chunked_seg);

//> DropPacket(key, model, &ctrl_data);

//> VlanLookup(key, model, &reg_data, &ctrl_data, &chunked_seg);

    // DMAC/SMAC update:

//> ctrl_data.isRoutable = ctrl_data.routeA && (tx_tag == MBY_NORMAL_TAGGING) && !(ctrl_data.isMirror);

//> UpdateMacAddrIPP(key, model, &reg_data, &ctrl_data, &chunked_seg);

    // Step 3 in EAS: Construct VLAN Tags (and copy to output):

//> UpdateVlanIPP(key, model, &reg_data, &ctrl_data, &chunked_seg);

//> PackPacket(packet, no_modify, rx_length, rx_data, &chunkedSeg, &ctrl_data, &tx_length, key);

//> MiscOps(key, model, &reg_data, &ctrl_data, &chunkedSeg, packet); // if minFrameSize, updatePktmeta use min size

    fm_uint32 tx_stats_length = 0; //> (!ctrl_data.mirrorTrunc && !tx_drop) ? ctrl_data.egressSeg0Bytes + tx_stats_last_len : ctrl_data.refcnt_tx_len;

    // Write outputs:
    out->TX_LENGTH       = tx_length;
    out->TX_STATS_LENGTH = tx_stats_length;
    out->SEG_DROP        = tx_drop;
}
