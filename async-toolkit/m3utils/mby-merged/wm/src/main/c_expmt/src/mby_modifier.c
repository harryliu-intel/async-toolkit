// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation
#include "mby_rxtotx.h"
#include "mby_modifier.h"
#include "mby_crc32.h"

static void getModRegCfgData
(
    fm_uint32             regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32       tx_port,
    mbyModRegData * const r
)
{
    fm_uint64 mod_per_port_cfg1_reg = 0;
    mbyModelReadCSR64(regs, MBY_MOD_PER_PORT_CFG1(tx_port, 0), &mod_per_port_cfg1_reg);

    r->modPerPortCfg1.ENABLE_VLAN_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg1_reg, MBY_MOD_PER_PORT_CFG1, ENABLE_VLAN_UPDATE);
    r->modPerPortCfg1.VID2_MAP_INDEX          = FM_GET_FIELD64(mod_per_port_cfg1_reg, MBY_MOD_PER_PORT_CFG1, VID2_MAP_INDEX);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT = FM_GET_FIELD64(mod_per_port_cfg1_reg, MBY_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_GLORT);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK  = FM_GET_FIELD64(mod_per_port_cfg1_reg, MBY_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_MASK);

    fm_uint64 mod_per_port_cfg2_reg = 0;
    mbyModelReadCSR64(regs, MBY_MOD_PER_PORT_CFG2(tx_port, 0), &mod_per_port_cfg2_reg);

    r->modPerPortCfg2.ENABLE_DMAC_ROUTING     = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_DMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_SMAC_ROUTING     = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_SMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_TTL_DECREMENT    = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_TTL_DECREMENT);
    r->modPerPortCfg2.ENABLE_ECN_MODIFICATION = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_ECN_MODIFICATION);
    r->modPerPortCfg2.VLAN2_E_TYPE            = FM_GET_FIELD64(mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, VLAN2_E_TYPE);
    r->modPerPortCfg2.VLAN1_E_TYPE            = FM_GET_FIELD64(mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, VLAN1_E_TYPE);
    r->modPerPortCfg2.ENABLE_DEI2_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_DEI2_UPDATE);
    r->modPerPortCfg2.ENABLE_DEI1_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_DEI1_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP2_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_PCP2_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP1_UPDATE      = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, ENABLE_PCP1_UPDATE);
    r->modPerPortCfg2.VID2_FIRST              = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, VID2_FIRST);
    r->modPerPortCfg2.VLAN_TAGGING            = FM_GET_FIELD64(mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, VLAN_TAGGING);
    r->modPerPortCfg2.MIN_FRAME_SIZE          = FM_GET_BIT64  (mod_per_port_cfg2_reg, MBY_MOD_PER_PORT_CFG2, MIN_FRAME_SIZE);
}

static void calcIngressCRC
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32           rx_length,
    const fm_byte     * const rx_packet,
    mbyModControlData * const c
)
{
    fm_uint32 ingress_crc = 0;
    for (fm_uint i = 0; i < 4; i++)
        ingress_crc |= rx_packet[rx_length - 4 + i] << (i * 8);

    // calculate good ingress crc
    fm_uint32 good_ingress_crc = mbyCrc32(rx_packet, rx_length-4);

    // calculate ingress crc error differential
    fm_uint32 crc_ingress_diff = good_ingress_crc ^ ingress_crc;

    if (crc_ingress_diff != 0) { /* Ingress packet has bad CRC */ }

    c->crc_ingress_diff = crc_ingress_diff;
}

static void unpackPacket
(
    const fm_uint32	  rx_length,
    const fm_byte * const rx_packet,
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
            chunked_seg->otr_dmac[i] = rx_packet[idx];
        for (fm_uint i = 0; i < 6; i++, idx++)
            chunked_seg->otr_smac[i] = rx_packet[idx];
        chunked_seg->n_otr_tag = (otr_l2_len - 1);
        for (fm_uint i = 0; i < (4*chunked_seg->n_otr_tag); i++, idx++)
            chunked_seg->otr_tags[i] = rx_packet[idx];
        // Etype:
        for (fm_uint i = 0; i < 2; i++, idx++)
            chunked_seg->otr_et[i] = rx_packet[idx];
    }

    // OTR MPLS:
    chunked_seg->n_otr_mpls = (parser_info.otr_mpls_len > 7)
        ? 7 // "WARNING: parser_info.otr_mpls_len overrun 28B otr_mpls chunk segment
        : parser_info.otr_mpls_len;

    chunked_seg->n_otr_mpls_pre = parser_info.otr_mpls_len;
    for (fm_uint i = 0; i < (4 * chunked_seg->n_otr_mpls); i++, idx++)
        chunked_seg->otr_mpls[i] = rx_packet[idx];

    fm_uint32 tmp_idx = idx;
    for (fm_uint i = (4 * chunked_seg->n_otr_mpls); i < 28; i++, tmp_idx++)
        chunked_seg->otr_mpls[i] = rx_packet[tmp_idx];

    // OTR L3:
    chunked_seg->otr_ip_size = (parser_info.otr_l3_len > 14)
        ? 14 // WARNING: parser_info.otr_l3_len overrun 56B otr_l3 chunk segment
        : parser_info.otr_l3_len;

    chunked_seg->otr_l3_v6 = parser_info.otr_l3_v6;
    for (fm_uint i = 0; i < (4 * chunked_seg->otr_ip_size); i++, idx++)
        chunked_seg->otr_ip[i] = rx_packet[idx];

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
                chunked_seg->otr_l4[i]     = rx_packet[idx];
            else
                chunked_seg->tun_opt[i-40] = rx_packet[idx];

        // it is not l4 hdr size, you can think of it is the tunnel hdr len in l4 chunk and tunnel chunk
        chunked_seg->tun_size_in_l4_chunk = (otr_tun_len < 8) ? otr_tun_len : 8;
        chunked_seg->tun_opt_size         = (otr_tun_len < 8) ? 0           : otr_tun_len - 8;
    }
    else if (chunked_seg->otr_tcp_v)
    {
        for (fm_uint i = 0; i < 18; i++, idx++)
            chunked_seg->otr_l4[i] = rx_packet[idx];
    }
    else if (parser_info.otr_tun_len) // any tunnel protocol following L3 immediately
    {
        fm_uint32 otr_tun_len = (parser_info.otr_tun_len > 18)
            ? 18 // WARNING: parser_info.otr_tun_len overrun 80B combined otr_l4+tunnel_opt chunk segments
            : parser_info.otr_tun_len;

        for (fm_uint i = 0; i < (otr_tun_len * 4); i++, idx++)
            if (i < 40)
                chunked_seg->otr_l4[i]     = rx_packet[idx];
            else
                chunked_seg->tun_opt[i-40] = rx_packet[idx];

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
            chunked_seg->inr_dmac[i] = rx_packet[idx];
        for (fm_uint i = 0; i < 6; i++, idx++)
            chunked_seg->inr_smac[i] = rx_packet[idx];
        // VLAN & Custom Tags
        chunked_seg->n_inr_tag = parser_info.inr_l2_len - 1;
        for (fm_uint i = 0; i < (4 * chunked_seg->n_inr_tag); i++, idx++)
            chunked_seg->inr_tags[i] = rx_packet[idx];
        // Etype:
        for (fm_uint i = 0; i < 2; i++, idx++)
            chunked_seg->inr_et[i] = rx_packet[idx];
        // INR MPLS:
        chunked_seg->n_inr_mpls = (parser_info.inr_mpls_len > 7)
            ? 7 // WARNING: parser_info.inr_mpls_len overrun 28B inr_mpls chunk segment
            : parser_info.inr_mpls_len;
        for (int i = 0; i < (4 * chunked_seg->n_inr_mpls); i++, idx++)
            chunked_seg->inr_mpls[i] = rx_packet[idx];
    }

    // Should this be conditional on something?! <-- REVISIT!!!
    {
        // INR L3:
        chunked_seg->inr_ip_size = (parser_info.inr_l3_len > 14)
            ? 14 // WARNING: parser_info.inr_l3_len overrun 56B inr_l3 chunk segment
            : parser_info.inr_l3_len;

        chunked_seg->inr_l3_v6 = parser_info.inr_l3_v6;
        for (fm_uint i = 0; i < (chunked_seg->inr_ip_size * 4); i++, idx++)
            chunked_seg->inr_ip[i] = rx_packet[idx];

        // INR L4:
        if (parser_info.inr_l4_udp && parser_info.inr_l4_tcp) { /* WARNING: cannot both be true! */ }

        chunked_seg->inr_udp_v = parser_info.inr_l4_udp;
        chunked_seg->inr_tcp_v = parser_info.inr_l4_tcp;

        if (parser_info.inr_l4_udp)
            for (fm_uint i = 0; i < 8; i++, idx++)
                chunked_seg->inr_l4[i] = rx_packet[idx];

        if (parser_info.inr_l4_tcp)
            for (fm_uint i = 0; i < 18; i++, idx++)
                chunked_seg->inr_l4[i] = rx_packet[idx];
    }

    chunked_seg->payload_start = idx;
    chunked_seg->payload_size  = rx_length - idx;
}

static void packPacket
(
    const fm_bool                   no_modify,
    const fm_uint32                 rx_length,
    const fm_byte           * const rx_packet,
    const mbyModControlData * const ctrl_data,
    const mbyChunkedSeg     * const chunked_seg,
    fm_uint32	            * const tx_length,
    fm_byte                         tx_packet[MBY_MAX_PACKET_SIZE]
)
{
    // initialize index into TX packet buffer:
    fm_uint32 idx = 0;

    if (!ctrl_data->isWindowParsing && !no_modify)
    {
        // FTAG:
        if (chunked_seg->ftag_v)
            for (fm_uint i = 0; i < 8; i++, idx++)
                tx_packet[idx] = chunked_seg->ftag[i];

        // OTR ETH:
        for (fm_uint i = 0; i < MAC_ADDR_BYTES; i++, idx++)
            tx_packet[idx] = chunked_seg->otr_dmac[i];

        for (fm_uint i = 0; i < MAC_ADDR_BYTES; i++, idx++)
            tx_packet[idx] = chunked_seg->otr_smac[i];

        // VLAN & Custom Tags:
        for (fm_uint i = 0; ((i < (4 * chunked_seg->n_otr_tag)) && (i < 16)); i++, idx++)
            tx_packet[idx] = chunked_seg->otr_tags[i];

        // Etype:
        for (fm_uint i = 0; i < 2; i++, idx++)
            tx_packet[idx] = chunked_seg->otr_et[i];

        // OTR MPLS:
        for (fm_uint i = 0; i < (4 * chunked_seg->n_otr_mpls); i++, idx++)
            tx_packet[idx] = chunked_seg->otr_mpls[i];

        // OTR IP:
        for (fm_uint i = 0; i < (4 * chunked_seg->otr_ip_size); i++, idx++)
            tx_packet[idx] = chunked_seg->otr_ip[i];

        // Layer 4:
        if (chunked_seg->otr_udp_v)
        {
            for (fm_uint i = 0; i < ((4 * chunked_seg->tun_size_in_l4_chunk) + 8uL); i++, idx++)
                tx_packet[idx] = chunked_seg->otr_l4[i];

            for (fm_uint i = 0; i < (4 * chunked_seg->tun_opt_size); i++, idx++)
                tx_packet[idx] = chunked_seg->tun_opt[i];
        }
        else if (chunked_seg->otr_tcp_v)
        {
            for (fm_uint i = 0; i < 18; i++, idx++)
                tx_packet[idx] = chunked_seg->otr_l4[i];
        }
        else if (chunked_seg->tun_size_in_l4_chunk || chunked_seg->tun_opt_size)
        {
            for (fm_uint i = 0; i < (4 * chunked_seg->tun_size_in_l4_chunk); i++, idx++)
                tx_packet[idx] = chunked_seg->otr_l4[i];

            for (fm_uint i = 0; i < (4 * chunked_seg->tun_opt_size); i++, idx++)
                tx_packet[idx] = chunked_seg->tun_opt[i];
        }

        // Inner:
        if (chunked_seg->inr_l2_v)
        {
            // ETH:
            for (fm_uint i = 0; i < MAC_ADDR_BYTES; i++, idx++)
                tx_packet[idx] = chunked_seg->inr_dmac[i];

            for (fm_uint i = 0; i < MAC_ADDR_BYTES; i++, idx++)
                tx_packet[idx] = chunked_seg->inr_smac[i];

            // VLAN & Custom Tags:
            for (fm_uint i = 0; i < (4 * chunked_seg->n_inr_tag); i++, idx++)
                tx_packet[idx] = chunked_seg->inr_tags[i];

            // Etype:
            for (fm_uint i = 0; i < 2; i++, idx++)
                tx_packet[idx] = chunked_seg->inr_et[i];

            // INR MPLS:
            for (fm_uint i = 0; i < (4 * chunked_seg->n_inr_mpls); i++, idx++)
                tx_packet[idx] = chunked_seg->inr_mpls[i];
        }

        // Should this be conditional on something?! <-- REVISIT!!!
        {
            // IP:
            for (fm_uint i = 0; i < (4 * chunked_seg->inr_ip_size); i++, idx++)
                tx_packet[idx] = chunked_seg->inr_ip[i];

            if (chunked_seg->inr_udp_v)
                for (fm_uint i = 0; i < 8; i++, idx++)
                    tx_packet[idx] = chunked_seg->inr_l4[i];

            if (chunked_seg->inr_tcp_v)
                for (fm_uint i = 0; i < 18; i++, idx++)
                    tx_packet[idx] = chunked_seg->inr_l4[i];
        }
    }

    // Payload:
    if (no_modify) {
        *tx_length = rx_length;
        for (fm_uint i = 0; i < rx_length; i++, idx++)
      	    tx_packet[idx] = rx_packet[i];
    } else {
        *tx_length = chunked_seg->payload_size;
        for (fm_uint i = 0; i < chunked_seg->payload_size; i++, idx++)
            tx_packet[idx] = rx_packet[chunked_seg->payload_start + i];
    }
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
    const fm_uint32           tx_length_in,
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
    c.refcnt_tx_len   = tx_length_in;
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

    // get Rx L2 tags:
    for (fm_uint i = 0; i < (VLAN_TAG_BYTES * 4); i++)
        c.rx_tags[i] = chunked_seg.otr_tags[i];

    c.rx_n_tag  = chunked_seg.n_otr_tag;
    c.rxVlan1   = parser_info.otr_l2_vlan1;
    c.rxVlan2   = parser_info.otr_l2_vlan2;
    c.rxV2first = parser_info.otr_l2_v2first;

    // copy final struct:
    *ctrl_data = c;
}

static void dropAndLog
(
          fm_uint32           regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte             drop_disp,
    const mbyMarkerFlag       marker_flag,
    const mbyDropFlag         drop_flag,
    const mbyDropErrCode      drop_code,
    const mbyIntrErrCode      intr_disp,
    fm_uint16         * const tx_disp,
    fm_bool           * const tx_drop,
    mbyDropErrCode    * const tx_reasoncode,
    mbyModControlData * const ctrl_data
)
{
    if ((drop_disp < *tx_disp) && (marker_flag != MARKER) &&
        ((drop_disp != DISP_MODERRORDROP) || (drop_flag == DROP)))
        *tx_disp = drop_disp;

    if (((ctrl_data->mod_im >> intr_disp) != 1) &&
        ((drop_code < *tx_reasoncode) || (*tx_reasoncode == 0)))
        *tx_reasoncode = drop_code;

    // update flag if not already set:
    *tx_drop = *tx_drop || ((marker_flag != MARKER) && (drop_flag == DROP));

    if (drop_flag == DROP)
    {
        if (marker_flag == MARKER)
        {
            if      (drop_disp == DISP_ECNDROP)
                ctrl_data->ecn_tx_drop = TRUE;
            else if (drop_disp == DISP_TIMEOUTDROP)
                ctrl_data->timeout_tx_drop = TRUE;
            else
                ctrl_data->non_cm_tx_drop = TRUE;
        }
        else
        {
            ctrl_data->cancel_drop_on_marker = TRUE;
            if (drop_disp < ctrl_data->cancelled_tx_disp)
                ctrl_data->cancelled_tx_disp = drop_disp;
        }
    }

    // interrupt prediction. Only valid for single-step tests
    if (intr_disp != INTR_DISREGARD_ERR)
    {
        fm_uint64 mod_im_reg = 0;
        mbyModelReadCSR64(regs, MBY_MOD_IM(0), &mod_im_reg);

        fm_uint64 ip = FM_GET_UNNAMED_FIELD64(mod_im_reg, 0, 63);
        ip |= FM_LITERAL_U64(1) << intr_disp;
        FM_SET_UNNAMED_FIELD64(mod_im_reg, 0, 63, ip);

        mbyModelWriteCSR64(regs, MBY_MOD_IM(0), mod_im_reg);

        if (!((ctrl_data->mod_im) >> intr_disp & 0x1))
            ctrl_data->intr_occured = TRUE;
    }
}

static void dropPacket
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32           rx_length,
    const fm_bool             drop_ttl,
    const fm_bool             is_timeout,
    const fm_bool             out_of_mem,
    const fm_bool             pm_err,
    const fm_bool             pm_err_nonsop,
    const fm_bool             saf_error,
    fm_uint16         * const tx_disp,
    fm_bool           * const tx_drop,
    mbyDropErrCode    * const tx_reasoncode,
    mbyModControlData * const ctrl_data
)
{
    const fm_bool is_marker    = ctrl_data->isMarkerPkt;
    const fm_bool is_mirror    = ctrl_data->isMirror;
    const fm_bool mirror_trunc = ctrl_data->mirrorTrunc;

    const mbyMarkerFlag marker_flag = ((is_marker) ? MARKER : NOMARKER);

    // L2 Modifications:
    ctrl_data->cancelled_tx_disp = DISP_UCAST;

//>  *seg_meta_err = FALSE; <-- FIXME!!!

    if (ctrl_data->routeA && !is_mirror && drop_ttl && !(*tx_drop))
        dropAndLog(regs, DISP_TTL1DROP,     marker_flag, DROP, ERR_TTL_0, INTR_TTL1_DROP,
                   tx_disp, tx_drop, tx_reasoncode, ctrl_data);

    if (ctrl_data->loopbackSuppressDrop && !(*tx_drop))
        dropAndLog(regs, DISP_LOOPBACKDROP, marker_flag, DROP, ERR_NONE, INTR_LPBK_DROP, tx_disp, tx_drop, tx_reasoncode, ctrl_data);

    if (is_timeout)
        dropAndLog(regs, DISP_TIMEOUTDROP,  marker_flag, DROP, ERR_NONE, INTR_TIMEOUT_DROP, tx_disp, tx_drop, tx_reasoncode, ctrl_data);

    if (out_of_mem && !mirror_trunc) // HLP logic for this was quizzical, so simplified <-- REVISIT!!!
        dropAndLog(regs, DISP_OOMTRUNC, NOMARKER, NODROP, ERR_NONE, INTR_DISREGARD_ERR, tx_disp, tx_drop, tx_reasoncode, ctrl_data);
    else if (((rx_length > DEFAULT_SEGMENT_BYTES) || pm_err_nonsop) && !mirror_trunc)
        dropAndLog(regs, DISP_TXERROR,  NOMARKER, NODROP, ERR_NONE, INTR_DISREGARD_ERR, tx_disp, tx_drop, tx_reasoncode, ctrl_data);

    if (saf_error)
    {
        if (is_marker)
            dropAndLog(regs, DISP_TXERRORDROP,   NOMARKER, DROP, ERR_NONE, INTR_TX_ERR_DROP,     tx_disp, tx_drop, tx_reasoncode, ctrl_data);
        else
            dropAndLog(regs, DISP_MARKERERRDROP, NOMARKER, DROP, ERR_NONE, INTR_MARKER_ERR_DROP, tx_disp, tx_drop, tx_reasoncode, ctrl_data);
    }

    if (pm_err)
    {
        if (!is_marker)
            dropAndLog(regs, DISP_TXECCDROP,     NOMARKER, DROP, ERR_NONE, INTR_TX_ECC_DROP,     tx_disp, tx_drop, tx_reasoncode, ctrl_data);
        else
            dropAndLog(regs, DISP_MARKERERRDROP, NOMARKER, DROP, ERR_NONE, INTR_MARKER_ERR_DROP, tx_disp, tx_drop, tx_reasoncode, ctrl_data);
    }
}

#if 0
static void updateVlan
(
    const fm_uint16                 l2_ivid1,
    const mbyModRegData     * const reg_data,
          mbyModControlData * const ctrl_data
)
{
    fm_uint16  rxVlan1Type    = 0;
    fm_uint16  rxVlan2Type    = 0;
    fm_byte    rxVpri1        = 0;
    fm_byte    rxVpri2        = 0;
    fm_uint16  rxVid1         = 0;
    fm_uint16  rxVid2         = 0;

    fm_uint16  modVid1MapAddr = 0;
    fm_uint16  modVid2MapAddr = 0;
    fm_uint16  modVlanTagAddr = 0;

    fm_byte    newVpri1       = 0;
    fm_byte    newVpri2       = 0;
    fm_uint16  newVid1        = 0;
    fm_uint16  newVid2        = 0;
    fm_bool    vtag           = 0;


    // enable vlan update:
    fm_uint16 evid_b = (reg_data.modPerPortCfg1.ENABLE_VLAN_UPDATE) ? ctrl_data->evidA : l2_ivid1;

    // grab rx vlan data:
    if (ctrl_data->numVlans == 1)
    {
        fm_uint16 vlan_type = (((fm_uint16) ctrl_data->rx_tags[0]) << 8)         | ctrl_data->rx_tags[1];
        fm_byte   vlan_pri  = ctrl_data->rx_tags[2] >> 4;
        fm_uint16 vlan_id   = (((fm_uint16)(ctrl_data->rx_tags[2] & 0x0F)) << 8) | ctrl_data->rx_tags[3];

        if (ctrl_data->rxVlan1) {
            rxVlan1Type = vlan_type;
            rxVpri1     = vlan_pri;
            rxVid1      = vlan_id;
        } else {
            rxVlan2Type = vlan_type;
            rxVpri2     = vlan_pri;
            rxVid2      = vlan_id;
        }
    }
    else if (ctrl_data->numVlans == 2)
    {
        if (state->PARSER_INFO.otr_l2_v2first)
        {
            rxVlan2Type = ctrl_data->rx_tags[0];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) ctrl_data->rx_tags[1];
            rxVpri2 = ctrl_data->rx_tags[2] >> 4;
            rxVid2 = (ctrl_data->rx_tags[2] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) ctrl_data->rx_tags[3];
            rxVlan1Type = ctrl_data->rx_tags[4];
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) ctrl_data->rx_tags[5];
            rxVpri1 = ctrl_data->rx_tags[6] >> 4;
            rxVid1 = (ctrl_data->rx_tags[6] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) ctrl_data->rx_tags[7];

        }
        else
        {
            rxVlan1Type = ctrl_data->rx_tags[0];
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) ctrl_data->rx_tags[1];
            rxVpri1 = ctrl_data->rx_tags[2] >> 4;
            rxVid1 = (ctrl_data->rx_tags[2] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) ctrl_data->rx_tags[3];
            rxVlan2Type = ctrl_data->rx_tags[4];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) ctrl_data->rx_tags[5];
            rxVpri2 = ctrl_data->rx_tags[6] >> 4;
            rxVid2 = (ctrl_data->rx_tags[6] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) ctrl_data->rx_tags[7];
        }
    }

    /* VLAN tag lookups: */
    modVid1MapAddr = evid_b;
    modVid2MapAddr = evid_b;
    modVlanTagAddr = evid_b;

    ctrl_data->evidB = evid_b;

    switch (reg_data->modPerPortCfg1.VID2_MAP_INDEX)
    {
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_VID:
            break;
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_SGLORT:
            modVid2MapAddr = state->SGLORT & 0xFFF;
            break;
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_DGLORT:
            modVid2MapAddr = ctrl_data->dglortA & 0xFFF;
            break;
        defualt:
            PRINT2(modDisplayVerbose, key, "vid2MapIndex = %0d (UNDEFINED!!)\n",
                reg_data->modPerPortCfg1.VID2_MAP_INDEX);
            break;
    }

    fm_uint64 *rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_VID1_MAP(modVid1MapAddr, 0));
    newVid1 = FM_GET_FIELD64(*rdval, HLP_MOD_VID1_MAP, VID);
    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_VLAN_TAG(modVlanTagAddr, 0));

    fm_uint64  temp64;
    temp64 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_TAG, TAG);
    vtag = ((temp64 >> state->TX_PORT) & 0x01);

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_VID2_MAP(modVid2MapAddr, 0));
    newVid2 = FM_GET_FIELD64(*rdval, HLP_MOD_VID2_MAP, VID);

    ctrl_data->txVid1  = newVid1;
    ctrl_data->txVid2  = newVid2;

    // TODO, if leave-as-rx should we use rxvid also as final vid?
    ExtractPriorityProfile(key, model, r, c);

    newVpri1 = GetVPRI(model, 1, state->QOS_L2_VPRI1, ctrl_data->priority_profile, key);
    newVpri2 = GetVPRI(model, 2, state->QOS_L2_VPRI1, ctrl_data->priority_profile, key);

    // EAS Step 2a : Update VLAN Fields, Non-mirrored and non-special frames:
    if (!ctrl_data->isMirror)
    {
        /* figure out what action will be taken on each vlan */
        if (state->TX_TAG == HLP_MODEL_INSERT)
        {
            dropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);

            ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
            ctrl_data->txVlan2 = (ctrl_data->txVid2 != 0);
            ctrl_data->preserveVlan1 = (ctrl_data->rxVlan1) != 0;
            ctrl_data->preserveVlan2 = (ctrl_data->rxVlan2) != 0;

            ctrl_data->numVlans += 2;
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = INSERT, if "
                "txVid1/txVid2 != 0); any RX'd tags will also be transmitted\n",
                state->TX_TAG);
        }
        else if (state->TX_TAG == HLP_MODEL_DELETE)
        {
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = DELETE); no "
                "new or RX'd tags will be transmitted\n",
                state->TX_TAG);

            dropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

            if(!(ctrl_data->rxVlan1 && ctrl_data->rxVlan2)) {
                PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
            }
            ctrl_data->numVlans = 0;
        }
        else if (state->TX_TAG == HLP_MODEL_UPDATE_ADD)
        {
            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
            ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
            ctrl_data->txVlan2 = (ctrl_data->txVid2 != 0);
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = UPDATE or "
                "ADD, if txVid1/txVid2 != 0); if txVlan1 or txVlan2, new "
                "tag data will be used\n",
                state->TX_TAG);
            if (!ctrl_data->rxVlan1 && ctrl_data->txVlan1)
                ctrl_data->numVlans++;
            if (!ctrl_data->rxVlan2 && ctrl_data->txVlan2)
                ctrl_data->numVlans++;
        }
        else
        {   /* TX_TAG == HLP_MODEL_NORMAL_TAGGING */
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (NORMAL TAGGING)\n", state->TX_TAG);
            PRINT2(modDisplayVerbose, key, "PTAG = %0d\n", reg_data->modPerPortCfg2.VLAN_TAGGING);
            PRINT2(modDisplayVerbose, key, "VTAG = %0d\n", vtag);

            /* note that some ptag/vtag combo's are (intentionally) duplicates;
             *  this is coded per the table in EAS, in the effort to make
             *  maintenance easier if one combo changes and another does not */
            switch (reg_data->modPerPortCfg2.VLAN_TAGGING)
            {
                case 5:
                    ctrl_data->txVid1  = rxVid1;
                    ctrl_data->txVid2  = rxVid2;
                    ctrl_data->txVlan1 = (ctrl_data->rxVlan1 != 0);
                    ctrl_data->txVlan2 = (ctrl_data->rxVlan2 != 0);
                    PRINT2(modDisplayVerbose, key, "vid1 = leave-as-rx, vid2 = leave-as-rx\n");
                    break;
                case 0:
                    if (vtag == 0) { /*X similar operation*/
                        /* leave-as-rx allows tag to transmit even if txVid2 == 0 */
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

                        ctrl_data->txVid2  = rxVid2;
                        ctrl_data->txVlan2 = (ctrl_data->rxVlan2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = leave-as-rx\n");
                        if(!ctrl_data->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            ctrl_data->numVlans--;
                        }
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVid2  = rxVid2;
                        ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
                        ctrl_data->txVlan2 = (ctrl_data->rxVlan2 != 0);
                        if(!ctrl_data->rxVlan1 && ctrl_data->txVlan1)
                            ctrl_data->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1 != 0), vid2 = leave-as-rx\n");
                    }
                    break;
                case 1:
                    if (vtag == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

                        ctrl_data->txVid2  = rxVid2;
                        ctrl_data->txVlan2 = (ctrl_data->rxVlan2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = leave-as-rx\n");
                        if(!ctrl_data->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            ctrl_data->numVlans--;
                        }
                    }
                    else
                    {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVlan2 = (ctrl_data->txVid2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = update-or-add (if txVid2 != 0)\n");
                        if(!ctrl_data->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            ctrl_data->numVlans--;
                        }
                        if(!ctrl_data->rxVlan2 && ctrl_data->txVlan2)
                            ctrl_data->numVlans++;
                    }
                    break;
                case 2:
                    if (vtag == 0) {
                        // TODO is possible 4 cstum tags comes in?
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVid2  = rxVid2;
                        ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
                        ctrl_data->txVlan2 = (ctrl_data->rxVlan2 != 0);
                        if(!ctrl_data->rxVlan1 && ctrl_data->txVlan1)
                            ctrl_data->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1" "!= 0), vid2 = leave-as-rx\n");
                    }
                    else {

                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
                        ctrl_data->txVlan2 = (ctrl_data->txVid2 != 0);
                        if(!ctrl_data->rxVlan1 && ctrl_data->txVlan1)
                            ctrl_data->numVlans++;
                        if(!ctrl_data->rxVlan2 && ctrl_data->txVlan2)
                            ctrl_data->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1" "!= 0), vid2 = update-or-add (if txVid2 != 0)\n");
                    }
                    break;
                case 3:
                    if (vtag == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVlan2 = (ctrl_data->txVid2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = " "update-or-add (if txVid2 != 0)\n");
                        if(!ctrl_data->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            ctrl_data->numVlans--;
                        }
                        if(!ctrl_data->rxVlan2 && ctrl_data->txVlan2)
                            ctrl_data->numVlans++;
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
                        ctrl_data->txVlan2 = (ctrl_data->txVid2 != 0);
                        if(!ctrl_data->rxVlan1 && ctrl_data->txVlan1)
                            ctrl_data->numVlans++;
                        if(!ctrl_data->rxVlan2 && ctrl_data->txVlan2)
                            ctrl_data->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1"
                            "!= 0), vid2 = update-or-add (if txVid2 != 0)\n");
                    }
                    break;
                case 4:
                    if (vtag == 0)
                    {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = delete\n");
                        if(!(ctrl_data->rxVlan1 && ctrl_data->rxVlan2)) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        ctrl_data->numVlans = 0;
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, DROP, LOG, !ctrl_data->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        ctrl_data->txVlan1 = (ctrl_data->txVid1 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if "
                            "txVid1 != 0), vid2 = delete\n");
                        if(!ctrl_data->rxVlan2) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, ctrl_data->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            ctrl_data->numVlans--;
                        }
                        if(!ctrl_data->rxVlan1 && ctrl_data->txVlan1)
                            ctrl_data->numVlans++;
                    }
                    break;
                default:
                    PRINT2(modDisplayVerbose, key, "ptag = %0d (UNDEFINED!!)\n",
                        reg_data->modPerPortCfg2.VLAN_TAGGING);
                    break;
            }
        }
    }

    if (!ctrl_data->isMirror)
    {
        /* VPRI Updates (step 2a and 2c) */
        ctrl_data->txVpri1 = 0;
        ctrl_data->txVpri2 = 0;
        if ((ctrl_data->rxVlan1) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (reg_data->modPerPortCfg2.ENABLE_PCP1_UPDATE == 0))
        {
            ctrl_data->txVpri1 |= (rxVpri1 & 0x0E);
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
            ctrl_data->txVpri1 |= (newVpri1 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri1 PCP (bits 3:1) set to newVpri1 "
                "PCP (from map)\n");
        }

        if ((ctrl_data->rxVlan1) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (reg_data->modPerPortCfg2.ENABLE_DEI1_UPDATE == 0))
        {
            ctrl_data->txVpri1 = ((ctrl_data->txVpri1) & 0xFE) | (rxVpri1 & 0x01);
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
            ctrl_data->txVpri1 = (ctrl_data->txVpri1 & 0xFE) | (newVpri1 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to newVpri1 DEI "
                "(from map)\n");
        }

        if ((ctrl_data->rxVlan2) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (reg_data->modPerPortCfg2.ENABLE_PCP2_UPDATE == 0))
        {
            ctrl_data->txVpri2 |= (rxVpri2 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri2 PCP (bits 3:1) set to rxVpri2 PCP "
                "because ingress VLAN2 present and enablePcp2Update = 0\n");
        }
        else
        {
            ctrl_data->txVpri2 |= (newVpri2 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri2 PCP (bits 3:1) set to newVpri2 "
                "PCP (from map)\n");
        }

        if ((ctrl_data->rxVlan2) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (reg_data->modPerPortCfg2.ENABLE_DEI2_UPDATE == 0))
        {
            ctrl_data->txVpri2 = (ctrl_data->txVpri2 & 0xFE) | (rxVpri2 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri2 DEI (bit 0) set to rxVpri2 DEI "
                "because ingress VLAN2 present and enableDei2Update = 0\n");
        }
        else
        {
            ctrl_data->txVpri2 = (ctrl_data->txVpri2 & 0xFE) | (newVpri2 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri2 DEI (bit 0) set to newVpri2 DEI "
                "(from map)\n");
        }
    }
}
#endif

static void updateMacAddrIPP
(
    fm_uint32                   regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte               tx_tag,
    const fm_byte               otr_l2_len,
    const fm_bool               no_modify,
    const fm_macaddr            l2_dmac,
    const mbyModRegData * const reg_data,
    fm_uint16           * const tx_disp,
    fm_bool             * const tx_drop,
    mbyDropErrCode      * const tx_reasoncode,
    mbyModControlData   * const ctrl_data,
    mbyChunkedSeg       * const chunked_seg
)
{
    ctrl_data->isRoutable = ctrl_data->routeA && (tx_tag == MBY_NORMAL_TAGGING) && !(ctrl_data->isMirror);

    if (!ctrl_data->isMirror)
    {
        fm_bool   modify_dmac = (ctrl_data->isRoutable && reg_data->modPerPortCfg2.ENABLE_DMAC_ROUTING);
        fm_bool   modify_smac = (ctrl_data->isRoutable && reg_data->modPerPortCfg2.ENABLE_SMAC_ROUTING);
        fm_bool   marker_flag = (ctrl_data->isMarkerPkt) ? MARKER : NOMARKER;

        if ((modify_dmac || modify_smac) && (otr_l2_len == 0) && !no_modify)
        {
            dropAndLog(regs, DISP_MODERRORDROP, marker_flag, NODROP, ERR_OTR_MAC, INTR_OTR_MAC_NONEXIST,
                       tx_disp, tx_drop, tx_reasoncode, ctrl_data);
        }

        if (modify_dmac)
        {
            for (fm_uint i = 0, bit_offset = 40; i < MAC_ADDR_BYTES; i++, bit_offset -= 8)
                chunked_seg->otr_dmac[i] = ((fm_byte) ((l2_dmac >> bit_offset) & 0xff));
        }

        if (modify_smac)
        {
            fm_macaddr mod_router_smac_reg = 0;
            mbyModelReadCSR64(regs, MBY_MOD_ROUTER_SMAC(ctrl_data->l3_domain, 0), &mod_router_smac_reg);
            fm_uint64 l2_smac = FM_GET_FIELD64(mod_router_smac_reg, MBY_MOD_ROUTER_SMAC, SMAC);

            for (fm_uint i = 0, bit_offset = 40; i < MAC_ADDR_BYTES; i++, bit_offset -= 8)
            	chunked_seg->otr_smac[i] = ((fm_byte) ((l2_smac >> bit_offset) & 0xff));
        }
    }
}

void doMiscOps
(
    fm_uint32                   regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyModRegData * const reg_data,
    const fm_uint32             rx_length,
    const fm_uint32             tx_stats_last_len,
    fm_uint32           * const tx_length_inout,
    fm_byte                     tx_packet[MBY_MAX_PACKET_SIZE],
    fm_uint16           * const tx_disp,
    fm_bool             * const tx_drop,
    mbyDropErrCode      * const tx_reasoncode,
    fm_uint32           * const tx_stats_length,
    mbyModControlData   * const ctrl_data
)
{
    fm_uint32 tx_length = *tx_length_inout; // local var

    if (tx_length >= rx_length)
    {
        fm_uint32 bytes_added      = tx_length - rx_length;
        ctrl_data->bytesAdded      = bytes_added;
        ctrl_data->egressSeg0Bytes = (rx_length >= 192) ? (192 + bytes_added) : tx_length;
    }
    else
    {
        fm_uint32 bytes_deleted    = rx_length - tx_length;
        ctrl_data->bytesAdded      = 0x80 | bytes_deleted; // MS bit set to flag negative value
        ctrl_data->egressSeg0Bytes = (rx_length >= 192) ? (192 - bytes_deleted): tx_length;
    }

    if (ctrl_data->mirrorTrunc)
    {
        fm_byte crc_idx =
            (rx_length >= (DEFAULT_SEGMENT_BYTES + 4)) ? 4 :
            (rx_length == (DEFAULT_SEGMENT_BYTES + 3)) ? 3 :
            (rx_length == (DEFAULT_SEGMENT_BYTES + 2)) ? 2 :
            (rx_length == (DEFAULT_SEGMENT_BYTES + 1)) ? 1 : 0;

        if (crc_idx != 0)
        {
            tx_length = DEFAULT_SEGMENT_BYTES + crc_idx;

            if (ctrl_data->bytesAdded != 0)
            {
                if (ctrl_data->bytesAdded & 0x80)
                    tx_length -= ctrl_data->bytesAdded & 0x7F; // bytes deleted
                else
                    tx_length += ctrl_data->bytesAdded;
            }

            // when pkt is larger than 192, we add crc to sop seg when
            // trunc, so stats need to be adjusted:
            ctrl_data->egressSeg0Bytes = tx_length; // truncating frame
        }
    }

    // calculate good egress crc
    ctrl_data->crc_egress = mbyCrc32(tx_packet, tx_length - 4);

    // apply ingress crc error differential onto egress crc
    // use recalculated egress CRC instead of incremental update for mirror
    // trunc frame, that is how RTL behaves now
    if (!ctrl_data->mirrorTrunc)
        ctrl_data->crc_egress ^= ctrl_data->crc_ingress_diff;

    for (fm_uint i = 0; i < 4; i++)
        tx_packet[tx_length - 4 + i] = (ctrl_data->crc_egress >> (i * 8)) & 0xFF;

    fm_bool marker_flag = (ctrl_data->isMarkerPkt) ? MARKER : NOMARKER;

    if (tx_length < MIN_EGRESS_BYTES)
        dropAndLog(regs, DISP_MODERRORDROP, marker_flag, DROP, ERR_SEG_S_18, INTR_SMALLER_18B,
                   tx_disp, tx_drop, tx_reasoncode, ctrl_data);

    fm_bool is_min_frame = reg_data->modPerPortCfg2.MIN_FRAME_SIZE && (tx_length < 64);

    ctrl_data->refcntSeg0Bytes = (is_min_frame) ? 64 : ctrl_data->egressSeg0Bytes;
    ctrl_data->refcnt_tx_len   = (is_min_frame) ? 64 : tx_length;

    // Update lengths:
    *tx_length_inout = tx_length;
    *tx_stats_length = (!ctrl_data->mirrorTrunc && !tx_drop)
        ? (ctrl_data->egressSeg0Bytes + tx_stats_last_len) : ctrl_data->refcnt_tx_len;
}

void Modifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyTxInToModifier     * const in,
          mbyModifierToTxStats  * const out
)
{
    // Read inputs:
    const mbyParserInfo   parser_info       = in->PARSER_INFO;
    const fm_bool         no_modify         = in->NO_MODIFY;  // skip most of modifications in Modifier
    const fm_uint32       rx_length         = in->RX_LENGTH;  // ingress packet data length [bytes]
    const fm_byte * const rx_packet         = in->RX_DATA;    // packet RX data
    const fm_uint32       tx_port           = in->TX_PORT;
    const fm_bool         tx_drop_in        = in->TX_DROP;
    const fm_uint32       tx_length_in      = in->TX_LENGTH;
    const fm_byte         tx_tag            = in->TX_TAG;
    const fm_uint32       tx_stats_last_len = in->TX_STATS_LAST_LEN;
    const fm_uint16       l2_evid1          = in->L2_EVID1;
    const fm_uint16       edglort           = in->EDGLORT;
    const mbyMirrorType   mirtyp            = in->MIRTYP;
    const fm_byte         qos_l3_dscp       = in->QOS_L3_DSCP;
    const fm_byte         ecn               = in->ECN;
    const fm_bool         mark_routed       = in->MARK_ROUTED;
    const fm_uint32       mod_idx           = in->MOD_IDX;
    const fm_uint64       tail_csum_len     = in->TAIL_CSUM_LEN;
    const fm_byte         xcast             = in->XCAST;
    const fm_bool         drop_ttl          = in->DROP_TTL;
    const fm_bool         is_timeout        = in->IS_TIMEOUT;
    const fm_bool         out_of_mem        = in->OOM;
    const fm_bool         pm_err_nonsop     = in->PM_ERR_NONSOP;
    const fm_bool         pm_err            = in->PM_ERR;
    const fm_bool         saf_error         = in->SAF_ERROR;
    const fm_macaddr      l2_dmac           = in->L2_DMAC;

    // Initialize:
    fm_bool        tx_drop         = tx_drop_in;
    fm_uint16      tx_disp         = (xcast == 2) ? DISP_BCAST : (xcast == 1) ? DISP_MCAST : DISP_UCAST;
    fm_uint32      tx_length       = 0;
    fm_uint32      tx_stats_length = 0;
    mbyDropErrCode tx_reasoncode   = ERR_NONE;
    fm_bool        no_pri_enc      = !((parser_info.otr_l2_vlan1 > 0) || (parser_info.otr_mpls_len > 0) || (parser_info.otr_l3_len > 0));

    // Unpack RX packet into chunked segment:
    mbyChunkedSeg chunked_seg;
    unpackPacket
    (
        rx_length,
        rx_packet,
        parser_info,
        &chunked_seg
    );

    // Read registered config data:
    mbyModRegData reg_data;

    getModRegCfgData
    (
        regs,
        tx_port,
        &reg_data
    );

    // Initialize control data:
    mbyModControlData ctrl_data;

    calcIngressCRC
    (
        regs,
        rx_length,
        rx_packet,
        &ctrl_data
    );

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
        tx_length_in,
        tail_csum_len,
        chunked_seg,
        &ctrl_data
    );

    // Drop packet, if needed:
    dropPacket
    (
        regs,
        rx_length,
        drop_ttl,
        is_timeout,
        out_of_mem,
        pm_err,
        pm_err_nonsop,
        saf_error,
        &tx_disp,
        &tx_drop,
        &tx_reasoncode,
        &ctrl_data
    );

#ifdef ENABLE_VLAN // skipping for now <-- REVISIT!!!
    updateVlan(l2_evid1, &reg_data, &ctrl_data);
#endif

    // Update DMAC/SMAC:
    updateMacAddrIPP
    (
        regs,
        tx_tag,
        parser_info.otr_l2_len,
        no_modify,
        l2_dmac,
        &reg_data,
        &tx_disp,
        &tx_drop,
        &tx_reasoncode,
        &ctrl_data,
        &chunked_seg
    );

#ifdef ENABLE_VLAN // skipping for now <-- REVISIT!!!
    updateVlanIPP(key, model, &reg_data, &ctrl_data, &chunked_seg);
#endif

    fm_byte tx_packet[MBY_MAX_PACKET_SIZE] = { 0 };

    packPacket
    (
        no_modify,
        rx_length,
        rx_packet,
        &ctrl_data,
        &chunked_seg,
        &tx_length,
        tx_packet
    );

    doMiscOps
    (
        regs,
        &reg_data,
        rx_length,
        tx_stats_last_len,
        &tx_length,
        tx_packet,
        &tx_disp,
        &tx_drop,
        &tx_reasoncode,
        &tx_stats_length,
        &ctrl_data
    );

    // Write outputs:
    out->TX_STATS_LENGTH = tx_stats_length;
    out->TX_DISP         = tx_disp;
    out->TX_LENGTH       = tx_length;
    out->NO_PRI_ENC      = no_pri_enc; // is this output still needed? <-- REVISIT!!!

    for (fm_uint i = 0; i < MBY_MAX_PACKET_SIZE; i++)
        out->TX_DATA[i] = tx_packet[i];

    // Pass thru:
    out->TX_PORT = tx_port;
}
