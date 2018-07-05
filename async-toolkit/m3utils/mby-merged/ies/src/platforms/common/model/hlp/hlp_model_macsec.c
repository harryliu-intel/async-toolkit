/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_macsec.c
 * Creation Date:   August 4, 2017
 * Description:     RX_MACsec and TX_MACsec stages of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2016 Intel Corporation. All Rights Reserved.
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
#include <platforms/common/model/hlp/hlp_model_macsec_aux.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/* Return the MACsec block ID and lane number, if available for the given port
 * Based on table:
 * https://securewiki.ith.intel.com/display/NDhlp/Ethernet+Ports#EthernetPorts-MACSEC
 */
static fm_int getBlockIdAndLane(fm_byte port, fm_byte *blockId, fm_byte *lane) {
    if (port % 8 >= 4 || port > 27) {
        MSEC_LOG("MACsec can't be used on PhysicalPort %d\n", port);
        return FM_ERR_INVALID_PORT;
    }
    *blockId = 2 * (port / 8) + ((port % 8) / 2);
    *lane = port % 2;
    MSEC_LOG("MACsec PhysicalPort=%d => MACsec blockId=%d - lane=%d\n",
            port, *blockId, *lane);

    // TODO this is an optional check that can be removed
    if (*blockId > 7 || *lane > 1) {
        MSEC_ERR("Invalid blockId / lane - check function implementation\n");
        return FM_ERR_ASSERTION_FAILED;
    }
    return FM_OK;
}

/* Returns the MACsec bit from packet metadata HLP -> RMN.
 * In egress direction, the bit is used to decide if the packet should be sent
 * to the controlled port or to the uncontrolled one.
 */
static fm_bool getMsecMetadataBit(hlp_model *model, fm_byte *meta) {
    fm_bool msecBit;

    FM_NOT_USED(model);

    msecBit = meta[2] & 0x1;
    return msecBit;
}

/* Sets the MACsec bit in packet metadata RMN -> HLP.
 * In ingress direction, the bit is set if the packet must be sent to the
 * controlled port.
 */
static void setMsecMetadataBit(hlp_model *model, fm_byte *meta, fm_bool val) {
    fm_bool msecBit;

    FM_NOT_USED(model);

    if (val)
        meta[2] |= 0x1;
    else
        meta[2] &= ~0x1;
}

/* Whether MACsec is enabled: if true the MACsec block is bypassed.
 * Note: this is actually a setting of MAC block but for simplicity is
 * implemeted inside MACsec.
 */
static fm_bool isMsecEnabled(hlp_model *model, fm_byte port) {
    fm_bool enabled;

    enabled = FM_ARRAY_GET_BIT(
               FM_MODEL_GET_REG_PTR(model, HLP_MAC_PATH_CFG(port, 0)),
               HLP_MAC_PATH_CFG, MSEC_ENABLE);

    return enabled;
}

/* Whether MACsec is on hold: if true packets are discarded. */
static fm_bool isMsecOnHold(hlp_model *model, fm_byte port) {
    fm_bool hold;

    hold  = FM_ARRAY_GET_BIT(
               FM_MODEL_GET_REG_PTR(model, HLP_MAC_PATH_CFG(port, 0)),
               HLP_MAC_PATH_CFG, MSEC_HOLD);

    return hold;
}

/* Whether the controlled port is enabled AND operational and can process MACsec
 * frames.  From API point of view, this is a read only bit that is set when an
 * error condition is detected
 */
static fm_bool isPortEnabledAndOperational(hlp_model *model, fm_byte blockId, fm_byte lane) {
    fm_bool enabled;
    fm_bool operational;

    enabled = FM_ARRAY_GET_BIT(
               FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_LANE_CPORT_EN(blockId, lane, 0)),
               HLP_MSEC_GLOBAL_LANE_CPORT_EN, CONTROLLEDPORTENABLED);

    // TODO check other conditions that disable the port (non-operational)
    // See MSEC_GLOBAL_INTERRUPT_STATUS, however who is setting these bits?

    operational = FM_ARRAY_GET_BIT(
               FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_LANE_CPORT_OPER(blockId, lane, 0)),
               HLP_MSEC_GLOBAL_LANE_CPORT_OPER, CPORT_OPER);

    MSEC_LOG("Controlled Port enabled=%d - operational=%d - blockId=%d - lane=%d\n",
            enabled, operational, blockId, lane);
    return enabled; // TODO && operational
}

/* True if the frame Ethernet Type is not MACsec (0x88E5) */
static fm_bool untagged(fm_byte *secTag) {
    fm_uint16 ethType;
    fm_bool isMacsec;

    ethType = SECTAG_ETHTYPE(secTag);
    isMacsec = ethType == MACSEC_ETHTYPE;
    MSEC_LOG("Frame EthType = 0x%04x -> %s\n", ethType, isMacsec ? "MACsec" : "Other");

    return !isMacsec;
}

/* True if a frame doesn’t contain a valid SecTAG, one or more bytes of User
 * Data, and ICV. List of conditions from IEEE Std 802.1AE-2006
 */
static fm_bool invalid_tag_or_icv(fm_byte *secTag, fm_uint32 mpduLength) {
    fm_uint16 ethType;
    fm_bool Vbit, ESbit, SCbit, SCBbit, Cbit;
    fm_uint32 SL; // Short Length
    fm_uint32 PN; // Packet Number

    ethType = SECTAG_ETHTYPE(secTag);
    Vbit = TCI_V_BIT(secTag);
    ESbit = TCI_E_BIT(secTag);
    SCbit = TCI_SC_BIT(secTag);
    SCBbit = TCI_SCB_BIT(secTag);
    Cbit = TCI_C_BIT(secTag);
    SL = secTag[SL_OFF];
    PN = SECTAG_PN(secTag);

    MSEC_LOG("Validate secTag (MPDU length = %u)\n", mpduLength);
    hexDump(secTag, SCbit ? 16 : 8);

    // there are at least 17 valid bytes
    if (mpduLength < 17) {
        MSEC_LOG("MPDU length (%u) must be at least 17\n", mpduLength);
        return TRUE;
    }

    // the first 2 SecTAG bytes are MACsec Ether Type
    if (ethType != MACSEC_ETHTYPE) {
        MSEC_LOG("EthType = 0x%04x is not MACsec (0x%x)\n", ethType, MACSEC_ETHTYPE);
        return TRUE;
    }

    // the V bit in TCI is clear
    if (Vbit != FALSE) {
        MSEC_LOG("Vbit=%d - must be clear\n", Vbit);
        return TRUE;
    }

    // if the ES or SCB bit in TCI is set, then the SC bit is clear
    if ((ESbit || SCBbit) && !SCbit) {
        MSEC_LOG("Ebit=1  && SCBbit=1 => SCbit (=%d) must be clear\n", SCbit);
        return TRUE;
    }

    // the two left-most bits of byte 4 (SL) are clear
    if (SL & 11000000) {
        MSEC_LOG("SL = %x first 2 bits must be clear\n", SL);
        return TRUE;
    }

    // if the C and SC bits in TCI are clear, MPDU comprises 24 bytes plus the
    // number of bytes indicated by the SL field if that is non-zero, and at
    // least 72 bytes otherwise
    if ((!Cbit && !SCbit) &&
        ((SL != 0 && SL + 24 != mpduLength) || (SL == 0 && mpduLength < 72))) {
        MSEC_LOG("Cbit=0 && SCBbit=0 => SL=%u and MPDUlen=%u not valid\n", SL, mpduLength);
        return TRUE;
    }


    // if the C bit is clear and the SC bit is set, the MPDU comprises 32 bytes
    // plus the number of bytes indicated by the SL field if that is non-zero
    // and at least 80 bytes otherwise
    if ((!Cbit && SCbit) &&
        ((SL != 0 && SL + 32 != mpduLength) || (SL == 0 && mpduLength < 80))) {
        MSEC_LOG("Cbit=0 && SCBbit=1 => SL=%u and MPDUlen=%u not valid\n", SL, mpduLength);
        return TRUE;
    }

    // if the C bit is set and the SC bit is clear, then MPDU comprises 8 bytes
    // plus the ICV length, plus the number of bytes indicated by SL field if
    // that is non–zero and at least 48 additional bytes otherwise
    if ((Cbit && !SCbit) &&
        ((SL != 0 && SL + 8 + ICV_LEN != mpduLength) || (SL == 0 && mpduLength < 48))) {
        MSEC_LOG("Cbit=1 && SCBbit=0 => SL=%u and MPDUlen=%u not valid\n", SL, mpduLength);
        return TRUE;
    }

    // if the C and SC bits are both set, the frame comprises at least 16 bytes
    // plus the ICV length, plus the number of bytes indicated by the SL field
    // if that is non-zero and at least 48 additional bytes otherwise
    if ((Cbit && SCbit) &&
        ((SL != 0 && SL + 16 + ICV_LEN != mpduLength) || (SL == 0 && mpduLength < 48))) {
        MSEC_LOG("Cbit=1 && SCBbit=1 => SL=%u and MPDUlen=%u not valid\n", SL, mpduLength);
        return TRUE;
    }

    // SecTAG PN field is zero
    if (PN == 0) {
        MSEC_LOG("PN=0 => Not valid condition\n");
        return TRUE;
    }

    return FALSE;
}

/* Find the Rx Secure Channel (0, 1) depending on secTag content or default SC
 * set in HLP config register. Return FALSE if not found.
 * Use the default SCI if the SC bit is not set.
 */
static fm_bool findSCI(hlp_model *model, fm_byte blockId, fm_byte lane,
                       fm_byte *secTag, fm_byte *SC) {
    fm_uint64 rxSCI; // SCI received in the frame, if SCbit is set
    fm_uint64 SCI0, SCI1; // SCI configured in HLP for SC0 and SC1
    fm_bool SCbit;

    SCbit = TCI_SC_BIT(secTag);

    if (!SCbit) {
        *SC = FM_ARRAY_GET_BIT(
                FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_LANE_SCI_DFLT(blockId, lane, 0)),
                HLP_MSEC_RX_LANE_SCI_DFLT, BITS);
        MSEC_LOG("SC bit not set => Use default SC = %d\n", *SC);
        return TRUE;

    }

    rxSCI = SECTAG_SCI(secTag);

    SCI0 = FM_ARRAY_GET_FIELD64(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_LANE_SC0_SCI(blockId, lane, 0)),
            HLP_MSEC_RX_LANE_SC0_SCI, BITS);

    SCI1 = FM_ARRAY_GET_FIELD64(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_LANE_SC1_SCI(blockId, lane, 0)),
            HLP_MSEC_RX_LANE_SC1_SCI, BITS);

    if (rxSCI != SCI0 && rxSCI != SCI1) {
        MSEC_LOG("Can't find SC: rxSCI (0x%llx) != SCI0 (0x%llx) or SCI1 (0x%llx)\n",
                rxSCI, SCI0, SCI1);
        *SC = 0xff; // when the SCI does not match
        return FALSE;
    }

    *SC = (rxSCI == SCI0) ? 0 : 1;
    MSEC_LOG("Matched with SCI%d: rxSCI (0x%llx) - SCI0 (0x%llx) - SCI1 (0x%llx)\n",
            *SC, rxSCI, SCI0, SCI1);
    return TRUE;
}


/* Remove the secTAG and ICV from the received frame.
 * New RX_DATA will contain the (unencrypted?) payload of the MACsec frame */
static void remove_secTAG_and_icv(hlp_modelState *state, fm_bool addFCS, fm_uint32 rxLength) {
    fm_uint32 newPacketLen;
    fm_uint32 payloadLen;
    fm_uint32 payloadOff;
    fm_byte *newPacket;
    fm_byte *secTag;
    fm_byte *data;

    // Extract info from the rx frame
    // Use rxLength as updated by rxRelayTagging instead of state->RX_LENGTH
    data = state->RX_DATA;
    secTag = data + DA_SA_LEN; // Skip DA and SA

    // SCI (8bit) is included in secTag only if SCbit is set
    payloadOff = DA_SA_LEN + (TCI_SC_BIT(secTag) ? 16 : 8);
    payloadLen = rxLength - (payloadOff + ICV_LEN + FCS_LEN);

    newPacketLen = DA_SA_LEN + payloadLen;
    newPacketLen += addFCS ? FCS_LEN : 0;

    newPacket = fmAlloc(newPacketLen);
    if (newPacket == NULL) {
        MSEC_ERR("Could not allocate memory for new packet in %s()", __func__);
        exit(1);
    }

    FM_MEMCPY_S(newPacket, payloadLen, data, DA_SA_LEN);
    FM_MEMCPY_S(newPacket + DA_SA_LEN, payloadLen, data + payloadOff, payloadLen);
    FM_MEMCPY_S(newPacket + DA_SA_LEN, payloadLen, data + payloadOff, payloadLen);

    if (addFCS) {
        fm_uint32 fcs;
        fm_uint32 i;

        fcs = fmCrc32(newPacket, newPacketLen - FCS_LEN);
        MSEC_LOG("Append FCS (0x%x) to packet sent to MAC stage\n", fcs);
        for (i = 0; i < FCS_LEN; ++i) {
            newPacket[DA_SA_LEN + payloadLen + i] = (fm_byte) ((fcs >> i*8) & 0xFF);
        }
    }
    else {
        MSEC_LOG("FCS won't be added to packet sent to MAC stage\n");
    }

    fmFree(state->RX_DATA);
    state->RX_DATA = newPacket;
    state->RX_LENGTH = newPacketLen;

    MSEC_LOG("Removed secTag and ICV: new packet (len=%u):\n", newPacketLen);
    hexDump(newPacket, newPacketLen);
}

/* Return the number of receive SCs with SAs enabled for reception */
static fm_byte numEnabledRxSC(hlp_model *model, fm_byte blockId, fm_byte lane) {
    hlp_msec_rx_sa SA;
    fm_byte SC, AN;
    fm_byte result = 0;

    for (SC=0; SC<2; ++SC) {
        for (AN=0; AN<4; ++AN) {
            readMacsecRxSA(model, blockId, lane, SC, AN, &SA);
            if (SA.enableRx) {
                MSEC_LOG("Enabled RX - blockId=%d, lane=%d, SC=%d, AN=%d\n", blockId, lane, SC, AN);
                result++;
                break;
            }
        }
    }

    MSEC_LOG("Number of enabled RX SCs = %d\n", result);
    return result;
}

/* Create the secTag for Tx packets based on HW configuration */
static void buildSecTag(hlp_model *model, hlp_msec_tx_sa *SA, fm_uint32 PN,
        fm_uint32 payloadLen, fm_byte *secTag, fm_byte *secTagLen) {

    fm_bool alwaysIncludeSCI;
    fm_uint64 SCI = 0;
    fm_bool useSCB;
    fm_bool SCBbit;
    fm_bool useES;
    fm_bool ESbit;
    fm_bool SCbit;
    fm_bool Ebit;
    fm_bool Cbit;
    fm_byte SL;

    alwaysIncludeSCI = FM_ARRAY_GET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_CNTRL(SA->blockId, 0)),
            HLP_MSEC_TX_CNTRL, ALWAYSINCLUDESCI);

    useES = FM_ARRAY_GET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_CNTRL(SA->blockId, 0)),
            HLP_MSEC_TX_CNTRL, USEES);

    useSCB = FM_ARRAY_GET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_CNTRL(SA->blockId, 0)),
            HLP_MSEC_TX_CNTRL, USESCB);

    ESbit = useES && !alwaysIncludeSCI;
    MSEC_LOG("useES=%d - alwaysIncludeSCI=%d => ESbit=%d\n", useES, alwaysIncludeSCI, ESbit);

    SCbit = alwaysIncludeSCI || (numEnabledRxSC(model, SA->blockId, SA->lane) > 1 && !useES && !useSCB);
    MSEC_LOG("alwaysIncludeSCI=%d - useES=%d - useSCB=%d => SCbit=%d\n", alwaysIncludeSCI, useES, useSCB, SCbit);

    SCBbit = useSCB && !alwaysIncludeSCI;
    MSEC_LOG("useSCB=%d - alwaysIncludeSCI=%d => SCBbit=%d\n", useSCB, alwaysIncludeSCI, SCBbit);

    if (SCbit) {
        SCI = FM_ARRAY_GET_FIELD64(
                FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_LANE_SC0_SCI(SA->blockId, SA->lane, 0)),
                HLP_MSEC_RX_LANE_SC0_SCI, BITS);
        MSEC_LOG("SCbit=1 => SCI for Tx SC%d = 0x%llx\n", SA->SC, SCI);
    }
    else {
        MSEC_LOG("SCbit=0 => SCI won't be added to secTag\n");
    }

    Ebit = SA->confid;
    Cbit = SA->confid;
    MSEC_LOG("SA.confidentiality=%d => Ebit=%d - Cbit=%d\n", SA->confid, Ebit, Cbit);

    SL = payloadLen > 48 ? 0 : payloadLen;
    MSEC_LOG("Payload length=%u => SL=%u\n", payloadLen, SL);

    *secTagLen = SCbit ? 16 : 8;
    FM_MEMSET_S(secTag, SECTAG_MAX_LEN, 0, SECTAG_MAX_LEN);

    secTag[0] = (MACSEC_ETHTYPE >> 8) & 0xFF;
    secTag[1] = MACSEC_ETHTYPE & 0xFF;

    if (ESbit) TCI_SET_ES_BIT(secTag);
    if (SCbit) TCI_SET_SC_BIT(secTag);
    if (SCBbit) TCI_SET_SCB_BIT(secTag);
    if (Ebit) TCI_SET_E_BIT(secTag);
    if (Cbit) TCI_SET_C_BIT(secTag);
    TCI_SET_AN_BITS(secTag, SA->AN);

    secTag[SL_OFF] = SL;
    SECTAG_SET_PN(secTag, PN);
    if (SCbit) SECTAG_SET_SCI(secTag, SCI);

    MSEC_LOG("Created secTag - length=%d\n", *secTagLen);
    hexDump(secTag, *secTagLen);
}

/* Decrypt and validate the packet in RX flow */
static void decryptGcm(hlp_model *model, hlp_msec_rx_sa *SA, fm_byte *iv,
        fm_uint32 iv_len, fm_byte *aad, fm_uint32 aad_len, fm_byte *ct, fm_byte
        ct_len, fm_byte *tag, fm_byte *pt, fm_uint32 *pt_len, fm_bool *valid) {

    fm_byte confidOff;

    FM_NOT_USED(iv);
    FM_NOT_USED(aad);
    FM_NOT_USED(tag);

    confidOff = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_CONFID_OFF(SA->blockId, 0)),
            HLP_MSEC_GLOBAL_CONFID_OFF, CONFIDOFF);

    MSEC_LOG("************* DECRYPTION *************\n");
    MSEC_LOG("Using Rx SA: blockId=%d - lane=%d - SC=%d - AN=%d\n",
            SA->blockId, SA->lane, SA->SC, SA->AN);
    MSEC_LOG("Algorithm: GCM-AES-%u\n", SA->keyLen);
    MSEC_LOG("Decryption key (len=%u)\n", SA->keyLen/8);
    hexDump(SA->key, SA->keyLen/8);
    MSEC_LOG("Initialization Vector (len=%u)\n", iv_len);
    hexDump(iv, iv_len);
    MSEC_LOG("Confidentiality offset: %x - Not used!\n", confidOff);
    MSEC_LOG("Cipher text (len=%u)\n", ct_len);
    if (ct_len) hexDump(ct, ct_len);
    MSEC_LOG("Additional authenticated data (len=%u)\n", aad_len);
    hexDump(aad, aad_len);
    // The tag length supported by MACsec block is fixed to 16
    MSEC_LOG("Tag used for validation (len=%u)\n", ICV_LEN);
    hexDump(tag, ICV_LEN);

    // TODO implement crypto - for now just set valid = TRUE and copy ct to pt
    *valid = TRUE;
    if (ct != pt) FM_MEMCPY_S(pt, ct_len, ct, ct_len);
    if (pt_len) *pt_len = ct_len;

    if (pt) { // This is NULL for integrity only
        MSEC_LOG("Plain text (len=%u)\n", *pt_len);
        hexDump(pt, *pt_len);
    }
    MSEC_LOG("Integrity check is %s\n", *valid ? "OK" : "NOT OK");
}

/* Encrypt the packet in TX flow */
static void encryptGcm(hlp_model *model, hlp_msec_tx_sa *SA, fm_byte *iv,
        fm_uint32 iv_len, fm_byte *aad, fm_uint32 aad_len, fm_byte *pt,
        fm_uint32 pt_len, fm_byte *ct, fm_uint32 *ct_len, fm_byte *tag) {

    fm_byte confidOff;

    FM_NOT_USED(iv);
    FM_NOT_USED(aad);

    confidOff = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_CONFID_OFF(SA->blockId, 0)),
            HLP_MSEC_GLOBAL_CONFID_OFF, CONFIDOFF);

    MSEC_LOG("************* ENCRYPTION *************\n");
    MSEC_LOG("Using Tx SA: blockId=%d - lane=%d - SC=%d - AN=%d\n",
            SA->blockId, SA->lane, SA->SC, SA->AN);
    MSEC_LOG("Algorithm: GCM-AES-%u\n", SA->keyLen);
    MSEC_LOG("Encryption key (len=%u)\n", SA->keyLen/8);
    hexDump(SA->key, SA->keyLen/8);
    MSEC_LOG("Initialization Vector (len=%u)\n", iv_len);
    hexDump(iv, iv_len);
    MSEC_LOG("Confidentiality offset: %x - Not used!\n", confidOff);
    MSEC_LOG("Plain text (len=%u)\n", pt_len);
    if (pt_len) hexDump(pt, pt_len);
    MSEC_LOG("Additional authenticated data (len=%u)\n", aad_len);
    hexDump(aad, aad_len);

    // TODO implement crypto - for now just copy pt to ct
    if (ct != pt) FM_MEMCPY_S(ct, pt_len, pt, pt_len);
    if (ct_len) *ct_len = pt_len;
    MSEC_LOG("ICV has not been calculated - filling tag with 0x1C\n");
    FM_MEMSET_S(tag, ICV_LEN, 0x1C, ICV_LEN);

    if (ct) { // This is NULL for integrity only
        MSEC_LOG("Cipher text (len=%u)\n", *ct_len);
        hexDump(ct, *ct_len);
    }
    MSEC_LOG("Tag (len=%u)\n", ICV_LEN);
    hexDump(tag, ICV_LEN);
}

/* Read the registers used for relay tagging in both Rx and Tx */
static void readRelayRegs(hlp_model *model, fm_byte blockId, fm_byte lane,
        fm_byte *mode, fm_uint16 *searchEType, fm_uint16 *replaceEType,
        fm_byte *relayLen) {

    *mode = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_LANE_RLY_MODE(blockId, lane, 0)),
            HLP_MSEC_GLOBAL_LANE_RLY_MODE, RLY_MODE);
    *searchEType = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_LANE_SRCH_ETYPE(blockId, lane, 0)),
            HLP_MSEC_GLOBAL_LANE_SRCH_ETYPE, SRC_ET);
    *replaceEType = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_LANE_RPLC_ETYPE(blockId, lane, 0)),
            HLP_MSEC_GLOBAL_LANE_RPLC_ETYPE, DST_ET);
    *relayLen = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_LANE_RLY_LEN(blockId, lane, 0)),
            HLP_MSEC_GLOBAL_LANE_RLY_LEN, RLY_LEN);
}

static fm_bool relayTagMatch(fm_byte *packet, fm_uint16 tag) {
    return (packet[DA_SA_LEN] == ((tag >> 8) & 0xFF)) &&
           (packet[DA_SA_LEN + 1] == (tag & 0xFF));
}

static void rxRelayTagging(hlp_model *model, fm_byte blockId, fm_byte lane,
        fm_byte *packet, fm_uint32 *packetLen) {
    fm_uint16 replaceEType;
    fm_uint16 searchEType;
    fm_byte removeLen; /* Num bytes to be removed before secTag */
    fm_byte relayLen;
    fm_byte mode;

    readRelayRegs(model, blockId, lane, &mode, &searchEType, &replaceEType, &relayLen);

    switch (mode) {
        case RLY_MODE_NO:
            MSEC_LOG("Relay tagging mode: no tagging\n");
            removeLen = 0;
            break;

        case RLY_MODE_SINGLE:
        case RLY_MODE_DUAL:
            MSEC_LOG("Relay mode: %s - searchEType=%x - relayLen=%d\n",
                    mode == RLY_MODE_SINGLE ? "single" : "dual", searchEType, relayLen);

            removeLen = mode == RLY_MODE_SINGLE ? 4 : 8;
            if (relayLen != removeLen) {
                MSEC_ERR("Relay tag len reg is set to %d instead of expected value %d\n",
                        relayLen, removeLen);
            }
            break;

        case RLY_MODE_REPLACE:
            MSEC_LOG("Relay mode: replace - searchEType=%x - relayLen=%d\n",
                    searchEType, relayLen);

            removeLen = 4;
            if (relayLen != removeLen) {
                MSEC_ERR("Relay tag len reg is set to %d instead of expected value %d\n",
                        relayLen, removeLen);
            }
            break;

        case RLY_MODE_GENERIC:
            MSEC_LOG("Relay mode: generic - searchEType=%x - relayLen=%d\n",
                    searchEType, relayLen);

            if (relayLen < 1 || relayLen > 8) {
                MSEC_ERR("Relay tag len reg is set to %d instead of expected value [1..8]\n",
                        relayLen);
                // TODO not sure what's the correct thing to do here
                removeLen = 4;
            }

            removeLen = relayLen;
            break;

        default:
            MSEC_ERR("Relay tagging mode (%d) is invalid\n", mode);
            removeLen = 0;
    }

    MSEC_LOG("Bytes to be removed (len=%d)\n", removeLen);
    if (removeLen) {
        hexDump(packet + DA_SA_LEN, removeLen);

        *packetLen -= removeLen;
        FM_MEMMOVE_S(packet + DA_SA_LEN, *packetLen,
                packet + DA_SA_LEN + removeLen, *packetLen - DA_SA_LEN);
    }
}

/* Check the tx frame and determine if a relay tag must be inserted. If true,
 * the tag will be copied in relayTag */
static void txRelayTagging(hlp_model *model, fm_byte blockId, fm_byte lane,
        fm_byte *packet, fm_byte *relayTag, fm_byte *relayTagLen) {
    fm_uint16 replaceEType;
    fm_uint16 searchEType;
    fm_byte relayLen;
    fm_byte mode;

    readRelayRegs(model, blockId, lane, &mode, &searchEType, &replaceEType, &relayLen);

    switch (mode) {
        case RLY_MODE_NO:
            MSEC_LOG("Relay tagging mode: no tagging\n");
            *relayTagLen = 0;
            break;

        case RLY_MODE_SINGLE:
        case RLY_MODE_DUAL:
            MSEC_LOG("Relay mode: %s - searchEType=%x - relayLen=%d\n",
                    mode == RLY_MODE_SINGLE ? "single" : "dual", searchEType, relayLen);

            if (!relayTagMatch(packet, searchEType)) {
                MSEC_LOG("Packet Eth Type (0x%x%x) does not match searchEType (0x%x)\n",
                        packet[DA_SA_LEN], packet[DA_SA_LEN+1], searchEType);
                break;
            }

            *relayTagLen = mode == RLY_MODE_SINGLE ? 4 : 8;
            if (relayLen != *relayTagLen) {
                MSEC_LOG("WARNING: relay tag len reg is set to %d instead of expected value %d\n",
                        relayLen, *relayTagLen);
            }

            FM_MEMCPY_S(relayTag, MAX_RLYTAG_LEN, packet + DA_SA_LEN, *relayTagLen);
            break;

        case RLY_MODE_REPLACE:
            MSEC_LOG("Relay mode: replace - searchEType=%x - relayLen=%d\n",
                    searchEType, relayLen);

            if (!relayTagMatch(packet, searchEType)) {
                MSEC_LOG("Packet Eth Type (0x%x%x) does not match searchEType (0x%x)\n",
                        packet[DA_SA_LEN], packet[DA_SA_LEN+1], searchEType);
                break;
            }

            *relayTagLen = 4;
            if (relayLen != *relayTagLen) {
                MSEC_ERR("Relay tag len reg is set to %d instead of expected value %d\n",
                        relayLen, *relayTagLen);
            }

            relayTag[0] = replaceEType >> 8;
            relayTag[1] = replaceEType & 0xFF;
            FM_MEMCPY_S(relayTag + 2, MAX_RLYTAG_LEN, packet + DA_SA_LEN + 2, 2);
            break;

        case RLY_MODE_GENERIC:
            MSEC_LOG("Relay mode: replace - searchEType=%x - relayLen=%d\n",
                    searchEType, relayLen);

            if (relayLen < 1 || relayLen > 8) {
                MSEC_ERR("Relay tag len reg is set to %d instead of expected value [1..8]\n",
                        relayLen);
                // TODO not sure what's the correct thing to do here
                *relayTagLen = 4;
            }

            *relayTagLen = relayLen;
            FM_MEMCPY_S(relayTag, MAX_RLYTAG_LEN, packet + DA_SA_LEN, *relayTagLen);
            break;

        default:
            MSEC_ERR("Relay tagging mode (%d) is invalid\n", mode);
            *relayTagLen = 0;
    }

    MSEC_LOG("Relay tag to be used - length=%d\n", *relayTagLen);
    if (*relayTagLen)
        hexDump(relayTag, *relayTagLen);
}

/* Security zeroization process: the security keys and other important security
 * data are internally reset to zero.
 */
static void zeroization(hlp_model *model, fm_int blockId) {
    fm_byte lane;

    // TODO what are the other security data?
    MSEC_LOG("Security zeroization - blockId=%d\n", blockId);
    FM_ARRAY_SET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_ZERO_STS(blockId, 0)),
            HLP_MSEC_GLOBAL_ZERO_STS, ZEROSTATUS, 1);

#define RESET_KEY_SC(blockId, lane, dir, sc, sa) \
        FM_MEMSET_S(\
            FM_MODEL_GET_REG_PTR(model, \
                HLP_MSEC_ ## dir ## _LANE_SC ## sc ## _SA ## sa ## _KEY(blockId, lane, 0)), \
            MAX_KEY_LEN, 0, MAX_KEY_LEN);

#define RESET_KEY(blockId, lane, dir, sc) \
    RESET_KEY_SC(blockId, lane, dir, sc, 0); \
    RESET_KEY_SC(blockId, lane, dir, sc, 1); \
    RESET_KEY_SC(blockId, lane, dir, sc, 2); \
    RESET_KEY_SC(blockId, lane, dir, sc, 3);

    for (lane = 0; lane < 2; ++lane) {
        RESET_KEY(blockId, lane, RX, 0);
        RESET_KEY(blockId, lane, RX, 1);
        RESET_KEY(blockId, lane, TX, 0);

        /* TODO probably not required - pay attention to reg cache in API!!
        FM_ARRAY_SET_FIELD64(
                FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_LANE_SC0_SCI(blockId, lane, 0)),
                HLP_MSEC_RX_LANE_SC0_SCI, BITS, 0);
        FM_ARRAY_SET_FIELD64(
                FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_LANE_SC1_SCI(blockId, lane, 0)),
                HLP_MSEC_RX_LANE_SC1_SCI, BITS, 0);
        FM_ARRAY_SET_FIELD64(
                FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_LANE_SC0_SCI(blockId, lane, 0)),
                HLP_MSEC_TX_LANE_SC0_SCI, BITS, 0);
        */

    }

#undef RESET_KEY
#undef RESET_KEY_SC
    FM_ARRAY_SET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_ZERO_STS(blockId, 0)),
            HLP_MSEC_GLOBAL_ZERO_STS, ZEROSTATUS, 1);
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelMacsecRx
 * \ingroup intModel
 *
 * \desc            Perform the MACsec RX flow as described in IEE 802.1AE
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if the packet is to be processed by the MAC stage,
 *                  != FM_OK otherwise.
 *
 *****************************************************************************/
fm_int hlpModelMacsecRx(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    hlp_msec_rx_sa SA;
    fm_byte validateFrames;
    fm_uint32 replayWindow;
    fm_bool replayProtect;
    fm_uint32 rxLength;
    fm_byte *secTag;
    fm_byte blockId; // MACsec block ID (0..7)
    fm_bool addFCS;
    fm_byte *data;
    fm_bool valid;
    fm_byte lane;    // lane (0, 1)
    fm_bool Ebit;
    fm_bool Cbit;
    fm_uint32 PN;
    fm_byte SC; // Secure Channel
    fm_byte AN; // Association Number

    rxLength = state->RX_LENGTH;
    data = state->RX_DATA;

    MSEC_LOG(">> Rx packet from port %d - lenght=%u\n", state->RX_PORT, rxLength);
    hexDump(data, rxLength);

    if (isMsecOnHold(model, state->RX_PORT)) {
        MSEC_ERR("Port %d is on hold, traffic is discarded\n", state->RX_PORT);
        goto RX_ERROR;
    }

    if (getBlockIdAndLane(state->RX_PORT, &blockId, &lane) != FM_OK) {
        MSEC_LOG("<< Sending packet received on port %d to MAC stage\n", state->RX_PORT);
        return FM_OK;
    }

    if (!isMsecEnabled(model, state->RX_PORT)) {
        MSEC_LOG("<< MAC_PATH_CFG.MSEC_ENABLE not set on port %d. MACsec module is bypassed\n", state->RX_PORT);
        return FM_OK;
    }

    if (!isPortEnabledAndOperational(model, blockId, lane)) {
        MSEC_LOG("Controlled port is not enabled/operational => LANE_THROUGH++\n");
        INCR_CNTR(model, HLP_MSEC_PTI_RX_LANE_THROUGH(blockId, lane, 0), 1, FALSE);
        goto RX_UNCONTROLLED;
    }

    // Read HLP configuration
    validateFrames = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_CNTRL(blockId, 0)),
            HLP_MSEC_RX_CNTRL, VALIDATEFRAMES);

    replayProtect = FM_ARRAY_GET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_CNTRL(blockId, 0)),
            HLP_MSEC_RX_CNTRL, REPLAYPROTECT);

    replayWindow = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_RPLY_WIN(blockId, 0)),
            HLP_MSEC_RX_RPLY_WIN, BITS);

    addFCS = FM_ARRAY_GET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_ADD_FCS(blockId, 0)),
            HLP_MSEC_RX_ADD_FCS, BITS);

    rxRelayTagging(model, blockId, lane, data, &rxLength);

    // Extract info from the rx frame
    secTag = data + DA_SA_LEN; // Skip DA and SA
    Cbit = TCI_C_BIT(secTag);
    Ebit = TCI_E_BIT(secTag);
    AN = TCI_AN_BITS(secTag);
    PN = SECTAG_PN(secTag);


    if (untagged(secTag)) {
        if (validateFrames == VALIDATE_STRICT) {
            MSEC_LOG("validateFrame (0x%x) == strict, InPktsNoTag++\n", validateFrames);
            INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_RX_NOTAG(blockId, lane, 0), 1, FALSE);
            goto RX_ERROR;
        }
        else {
            MSEC_LOG("validateFrame (0x%x) != strict - InPktsUntagged++\n", validateFrames);
            INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_RX_UNTAG(blockId, lane, 0), 1, FALSE);
            goto RX_UNCONTROLLED;
        }
    }

    if (!Cbit && Ebit) {
        MSEC_LOG("Cbit=0 && Ebit=1 => Frame used for key exchange\n");
        goto RX_UNCONTROLLED;
    }

    if (invalid_tag_or_icv(secTag, rxLength - DA_SA_LEN - FCS_LEN)) {
        MSEC_LOG("Tag or ICV is invalid - InPktsBadTag++\n");
        INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_RX_BADTAG(blockId, lane, 0), 1, FALSE);
        goto RX_ERROR;
    }

    SC = 0;
    if (!findSCI(model, blockId, lane, secTag, &SC)) {
        if (validateFrames == VALIDATE_STRICT || Cbit) {
            MSEC_LOG("validateFrame (0x%x) == strict, InPktsNoSCI++\n", validateFrames);
            INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_RX_NOSCI(blockId, lane, 0), 1, FALSE);
            goto RX_ERROR;
        }
        else {
            MSEC_LOG("validateFrame (0x%x) != strict - InPktsUnknownSCI++\n", validateFrames);
            INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_RX_UNKSCI(blockId, lane, 0), 1, FALSE);
            goto RX_REMOVE_TAG;
        }
    }

    readMacsecRxSA(model, blockId, lane, SC, AN, &SA);

    if (!SA.inUse) {
        MSEC_LOG("SC=%d - AN=%d - SA is not in use\n", SC, AN);
        if (validateFrames == VALIDATE_STRICT || Cbit) {
            MSEC_LOG("validateFrame (0x%x) == strict, InPktsNotUsingSA++\n", validateFrames);
            INCR_SA_CNTR(model, SC, AN, blockId, lane, RX, NO_USING_SA, 1);
            goto RX_ERROR;
        }
        else {
            MSEC_LOG("validateFrame (0x%x) != strict - InPktsUnusedSA++\n", validateFrames);
            INCR_SA_CNTR(model, SC, AN, blockId, lane, RX, UNUSED_SA, 1);
            goto RX_REMOVE_TAG;
        }
    }

    if (validateFrames == VALIDATE_DISABLED) {
        MSEC_LOG("Validate frames is disabled => valid = false\n");
        valid = FALSE;
    }
    else {
        fm_uint32 payloadOff, payloadLen;
        fm_byte *payload, *ICV;
        fm_byte IV[IV_LEN];

        payloadOff = DA_SA_LEN + (TCI_SC_BIT(secTag) ? 16 : 8);
        payloadLen = rxLength - (payloadOff + ICV_LEN + FCS_LEN);
        payload = data + payloadOff;
        ICV = payload + payloadLen;

        // TODO create IV - where to get SCI?
        FM_MEMSET_S(IV, IV_LEN, 0, IV_LEN);

        if (Ebit) {
            MSEC_LOG("Crypto enabled - mode: confidentiality + integrity\n");
            decryptGcm(model, &SA, IV, IV_LEN, data, payloadOff, payload,
                    payloadLen, ICV, payload, &payloadLen, &valid);
            MSEC_LOG("Ebit is set => SC%d->InOctetsDecrypted += payloadLen (%u)\n", SC, payloadLen);
            INCR_SC_CNTR(model, SC, blockId, lane, RX, DECR_BYTE, payloadLen);
        }
        else {
            MSEC_LOG("Crypto enabled - mode: integrity only\n");
            decryptGcm(model, &SA, IV, IV_LEN, data, payloadOff + payloadLen,
                    NULL, 0, ICV, NULL, 0, &valid);
            MSEC_LOG("Ebit is not set => SC%d->InOctetsValidated += payloadLen (%u)\n", SC, payloadLen);
            INCR_SC_CNTR(model, SC, blockId, lane, RX, VLDT_BYTE, payloadLen);
        }
    }

    if (replayProtect && (PN < SA.lowestPN)) {
        MSEC_LOG("replayProtext is ON and PN (%u) < SA->lowestPN (%llu) => SC%d->InPktsLate\n",
                PN, SA.lowestPN, SC);
        INCR_SC_CNTR(model, SC, blockId, lane, RX, LATE, 1);
        goto RX_ERROR;
    }

    if (!valid && (validateFrames == VALIDATE_STRICT || Cbit)) {
        MSEC_LOG("Not valid && (validateFrame == strict || Cbit (%d)), InPktsNotValid++\n", Cbit);
        INCR_SC_CNTR(model, SC, blockId, lane, RX, NOT_VLD, 1);
        INCR_SA_CNTR(model, SC, AN, blockId, lane, RX, NOT_VLD, 1);
        goto RX_ERROR;
    }

    if (!valid && validateFrames == VALIDATE_CHECK) {
        MSEC_LOG("Not valid && (validateFrame == check), InPktsInvalid++\n");
        INCR_SC_CNTR(model, SC, blockId, lane, RX, INVLD, 1);
        INCR_SA_CNTR(model, SC, AN, blockId, lane, RX, INVLD, 1);
        goto RX_REMOVE_TAG;
    }

    if (PN < SA.lowestPN) {
        MSEC_LOG("replayProtext is OFF and PN (%u) < SA->lowestPN (%llu) => SC%d->InPktsDelayed++\n",
                PN, SA.lowestPN, SC);
        INCR_SC_CNTR(model, SC, blockId, lane, RX, DLY, 1);
        goto RX_REMOVE_TAG;
    }

    if (!valid) {
        MSEC_LOG("Not valid => SC%d->InPktsUnchecked++\n", SC);
        INCR_SC_CNTR(model, SC, blockId, lane, RX, UNCHK, 1);
        goto RX_REMOVE_TAG;
    }

    if (PN >= SA.nextPN) {
        MSEC_LOG("PN=%u >= SA->nextPN=%llu => Update SA\n", PN, SA.nextPN);
        SA.nextPN = PN + 1;
        SA.lowestPN = MAX(SA.lowestPN, SA.nextPN - replayWindow);
        writeMacsecRxSA(model, &SA);
    }

    MSEC_LOG("MACsec Rx processing completed => InPktsOK++\n");
    INCR_SC_CNTR(model, SC, blockId, lane, RX, OK, 1);
    INCR_SA_CNTR(model, SC, AN, blockId, lane, RX, OK, 1);

RX_REMOVE_TAG:
    setMsecMetadataBit(model, state->PKT_META, TRUE);
    remove_secTAG_and_icv(state, addFCS, rxLength);
    MSEC_LOG("<< secTag removed: send packet to Controlled Port\n");
    return FM_OK;

RX_UNCONTROLLED:
    setMsecMetadataBit(model, state->PKT_META, FALSE);
    INCR_CNTR(model, HLP_MSEC_PTI_RX_LANE_THROUGH(blockId, lane, 0), 1, FALSE);
    MSEC_LOG("<< Send packet to Uncontrolled Port\n");
    return FM_OK;

RX_ERROR:
    // TODO is it OK to drop the packets?
    MSEC_LOG("<< MACsec error: dropping the packet\n");
    return FM_ERR_INVALID_STATE;
}   /* end hlpModelMacsecRx */

/*****************************************************************************/
/** hlpModelMacsecTx
 * \ingroup intModel
 *
 * \desc            Perform the MACsec RX flow as described in IEE 802.1AE
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       packet points to the caller-allocated egress packet data
 *                  buffer.
 *
 * \param[in]       maxPktSize is the maximum number of packet bytes this
 *                  function can store in the buffer pointed to by packet.
 *
 * \param[out]      encrypted points to caller-allocated bool that indicates
 *                  whether the pkt has been encrypted and the FCS needs to be
 *                  recalculated.
 *
 * \return          != FM_OK in case of errors and the packet must be dropped by
 *                  to the MAC block, FM_OK otherwise.
 *
 *****************************************************************************/
fm_int hlpModelMacsecTx(hlp_model *model, fm_byte *packet, fm_uint32 maxPktSize,
                        fm_bool *encrypted)
{
    hlp_modelState *state = &model->packetState;
    fm_byte relayTag[MAX_RLYTAG_LEN];
    fm_byte secTag[SECTAG_MAX_LEN];
    fm_uint32 maxFrameLength;
    fm_byte relayTagLen = 0;
    fm_uint32 payloadLen;
    fm_bool protectFrame;
    fm_byte ICV[ICV_LEN];
    fm_byte IV[IV_LEN];
    fm_byte encodingSA;
    hlp_msec_tx_sa SA;
    fm_byte secTagLen;
    fm_byte *payload;
    fm_byte blockId;
    fm_byte lane;
    fm_uint64 PN;

    MSEC_LOG(">> Tx packet to port %d - lenght=%u\n", state->TX_PORT, state->TX_LENGTH);
    hexDump(packet, state->TX_LENGTH);
    *encrypted = FALSE;

    if (!getMsecMetadataBit(model, state->TX_PKT_META)) {
        MSEC_LOG("<< M bit not set in metadata. MACsec module bypassed\n");
        return FM_OK;
    }

    if (isMsecOnHold(model, state->TX_PORT)) {
        MSEC_ERR("<< Port %d is on hold, traffic is discarded\n", state->TX_PORT);
        return FM_ERR_INVALID_STATE;
    }

    if (getBlockIdAndLane(state->TX_PORT, &blockId, &lane) != FM_OK) {
        MSEC_LOG("<< Sending packet received on port %d to MAC stage\n", state->RX_PORT);
        return FM_OK;
    }

    if (!isMsecEnabled(model, state->TX_PORT)) {
        MSEC_LOG("<< MAC_PATH_CFG.MSEC_ENABLE not set on port %d. MACsec module is bypassed\n", state->TX_PORT);
        return FM_OK;
    }

    if (state->TX_LENGTH < (DA_SA_LEN + 1)) {
        MSEC_LOG("<< Packet is too short (min length = 13) => LANE_DROPPED++\n");
        INCR_CNTR(model, HLP_MSEC_TX_LANE_DROPPED(blockId, lane, 0), 1, FALSE);
        return FM_ERR_INVALID_STATE;
    }

    if (!isPortEnabledAndOperational(model, blockId, lane)) {
        MSEC_LOG("<< Controlled port is not enabled/operational => LANE_DISCARDED++\n");
        INCR_CNTR(model, HLP_MSEC_TX_LANE_DISCARDED(blockId, lane, 0), 1, FALSE);
        return FM_OK;
    }

    txRelayTagging(model, blockId, lane, packet, relayTag, &relayTagLen);

    protectFrame = FM_ARRAY_GET_BIT(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_CNTRL(blockId, 0)),
            HLP_MSEC_TX_CNTRL, PROTECTFRAMES);

    encodingSA = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_LANE_SC0_ENCOD_SA(blockId, lane, 0)),
            HLP_MSEC_TX_LANE_SC0_ENCOD_SA, BITS);

    maxFrameLength = FM_ARRAY_GET_FIELD(
            FM_MODEL_GET_REG_PTR(model, HLP_MAC_CFG(state->TX_PORT, 0)),
            HLP_MAC_CFG, MAX_FRAME_LENGTH);

    if (protectFrame == FALSE) {
        MSEC_LOG("protectFrame == false, OutPktsUntagged++\n");
        INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_TX_UNTAG(blockId, lane, 0), 1, FALSE);
    }

    readMacsecTxSA(model, blockId, lane, 0, encodingSA, &SA);
    PN = SA.nextPN++;
    writeMacsecTxSA(model, &SA);
    MSEC_LOG("PN=%llu - SA.nextPN=%llu\n", PN, SA.nextPN);

    if (SA.nextPN == 0) {
        // TODO If the nextPN variable for the encodingSA is zero (or 232) and the
        // protectFrames control is set, MAC_Operational transitions to False for
        // the Controlled Port and frames are neither accepted or delivered
        MSEC_LOG("nextPN == 0 - port is not operational! Not implemented!!\n");
    }

    payload = packet + DA_SA_LEN;
    payloadLen = state->TX_LENGTH - DA_SA_LEN - FCS_LEN;
    buildSecTag(model, &SA, PN, payloadLen, secTag, &secTagLen);

    FM_MEMMOVE_S(packet + DA_SA_LEN + secTagLen, maxPktSize, payload, payloadLen);
    payload += secTagLen;
    FM_MEMCPY_S(packet + DA_SA_LEN, maxPktSize, secTag, secTagLen);

    // TODO create IV - where to get SCI?
    FM_MEMSET_S(IV, IV_LEN, 0, IV_LEN);

    if (TCI_E_BIT(secTag)) {
        MSEC_LOG("Crypto mode: confidentiality + integrity\n");
        encryptGcm(model, &SA, IV, IV_LEN, packet, DA_SA_LEN + secTagLen,
                payload, payloadLen, payload, &payloadLen, ICV);
        MSEC_LOG("Ebit is set - OutPktsEncrypted++\n");
        INCR_SC_CNTR(model, 0, blockId, lane, TX, ENCR_PCKT, 1);
        INCR_SA_CNTR(model, 0, encodingSA, blockId, lane, TX, ENCR_PCKT, 1);
    }
    else {
        MSEC_LOG("Crypto mode: integrity only\n");
        encryptGcm(model, &SA, IV, IV_LEN, packet, DA_SA_LEN + secTagLen +
                payloadLen, NULL, 0, NULL, 0, ICV);
        MSEC_LOG("Ebit is not set - OutPktsProtected++\n");
        INCR_SC_CNTR(model, 0, blockId, lane, TX, PROT_PCKT, 1);
        INCR_SA_CNTR(model, 0, encodingSA, blockId, lane, TX, PROT_PCKT, 1);
    }

    FM_MEMCPY_S(packet + DA_SA_LEN + secTagLen + payloadLen, maxPktSize, ICV, ICV_LEN);
    state->TX_LENGTH += secTagLen + ICV_LEN;

    if (state->TX_LENGTH > maxFrameLength) {
        MSEC_LOG("<< Packet length (%u) > MAC max_length (%u) - OutPktsTooLong++\n",
                state->TX_LENGTH, maxFrameLength);
        INCR_CNTR(model, HLP_MSEC_GLOBAL_LANE_STAT_CPORT_TX_TOOLONG(blockId, lane, 0), 1, FALSE);
        // TODO check what to do with the error condition
        return FM_OK;
    }

    if (relayTagLen != 0) {
        if (state->TX_LENGTH + relayTagLen > maxPktSize) {
            MSEC_LOG("No space in packet (len=%u - max=%u) to add relay tag (len=%d) => RLY_SHORT++\n",
                    state->TX_LENGTH, maxPktSize, relayTagLen);
            INCR_CNTR(model, HLP_MSEC_TX_LANE_RLY_SHORT(blockId, lane, 0), 1, FALSE);
        }
        else {
            // Add relayTag before secTag
            MSEC_LOG("Add relay tag (len=%d) before secTag\n", relayTagLen);
            FM_MEMMOVE_S(packet + DA_SA_LEN + relayTagLen, maxPktSize,
                    packet + DA_SA_LEN, state->TX_LENGTH - DA_SA_LEN);
            FM_MEMCPY_S(packet + DA_SA_LEN, maxPktSize, relayTag, relayTagLen);
            state->TX_LENGTH += relayTagLen;
        }
    }

    *encrypted = TRUE;

    MSEC_LOG("<< MACsec completed - Packet sent to MAC - lenght=%u\n", state->TX_LENGTH);
    hexDump(packet, state->TX_LENGTH);

    return FM_OK;
}   /* end hlpModelMacsecTx */

/*****************************************************************************/
/** hlpModelMacsecWriteCSR
 * \ingroup intModel
 *
 * \desc            Processes CSR writes pertinent to the RX and TX MAC HLP
 *                  white model stages.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       newValue is the 32-bit value to write.
 *
 * \param[in]       oldValue is the value before the new value was written.
 *
 * \param[in]       init is a boolean indicating whether this write operation
 *                  is trying to initialize the register related white model
 *                  cached state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelMacsecWriteCSR(hlp_model *model,
                              fm_uint32 addr,
                              fm_uint32 newValue,
                              fm_uint32 oldValue,
                              fm_bool init)
{
    fm_bool match = FALSE;
    fm_bool txEnable;
    fm_bool rxEnable;
    fm_uint blockId;
    fm_uint txAN;
    fm_uint rxSC;
    fm_uint rxAN;
    fm_uint lane;
    fm_int  word;
    fm_uint AN;

    FM_NOT_USED(init);

    // MACSec registers zeroization
    for (blockId = 0; blockId < HLP_MSEC_GLOBAL_ZERO_ENTRIES; ++blockId) {
        if (addr == HLP_MSEC_GLOBAL_ZERO(blockId, 0) &&
            !FM_GET_BIT(oldValue, HLP_MSEC_GLOBAL_ZERO, ZERO) &&
            FM_GET_BIT(newValue, HLP_MSEC_GLOBAL_ZERO, ZERO)) {

            zeroization(model, blockId);
            return FM_OK;
        }
    }

    // Handle TX_LANE_SC0_SAx_EN_TRN
    for (blockId = 0; blockId < 8; ++blockId) {
        for (lane = 0; lane < 2; ++lane) {
            match = TRUE;
            if (addr == HLP_MSEC_TX_LANE_SC0_SA0_EN_TRN(blockId, lane, 0)) {
                txAN = 0;
                txEnable = FM_GET_BIT(newValue, HLP_MSEC_TX_LANE_SC0_SA0_EN_TRN,
                                      SA_EN_TRN);
            }
            else if (addr == HLP_MSEC_TX_LANE_SC0_SA1_EN_TRN(blockId, lane, 0)) {
                txAN = 1;
                txEnable = FM_GET_BIT(newValue, HLP_MSEC_TX_LANE_SC0_SA1_EN_TRN,
                                      SA_EN_TRN);
            }
            else if (addr == HLP_MSEC_TX_LANE_SC0_SA2_EN_TRN(blockId, lane, 0)) {
                txAN = 2;
                txEnable = FM_GET_BIT(newValue, HLP_MSEC_TX_LANE_SC0_SA2_EN_TRN,
                                      SA_EN_TRN);
            }
            else if (addr == HLP_MSEC_TX_LANE_SC0_SA3_EN_TRN(blockId, lane, 0)) {
                txAN = 3;
                txEnable = FM_GET_BIT(newValue, HLP_MSEC_TX_LANE_SC0_SA3_EN_TRN,
                                      SA_EN_TRN);
            }
            else {
                match = FALSE;
            }

            if (match)
                goto UPDATE_TX_SA;
        }
    }

UPDATE_TX_SA:
    if (match) {
        MSEC_LOG("Update Tx SA: blockId=%d - lane=%d - AN=%d - txEnable=%d\n",
                 blockId, lane, txAN, txEnable);
        for (AN = 0; AN < 4; ++AN) {
            hlp_msec_tx_sa SA;
            readMacsecTxSA(model, blockId, lane, 0, AN, &SA);
            SA.inUse = (txAN == AN);
            SA.enableTx = (txAN == AN) && txEnable;
            writeMacsecTxSA(model, &SA);
        }
        if (txEnable) {
            FM_ARRAY_SET_FIELD64(
                    FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_LANE_SC0_ENCOD_SA(blockId, lane, 0)),
                    HLP_MSEC_TX_LANE_SC0_ENCOD_SA, BITS, txAN);
        }
        return FM_OK;
    }

    // Handle RX_LANE_SCx_SAx_EN_RCV
    for (blockId = 0; blockId < 8; ++blockId) {
        for (lane = 0; lane < 2; ++lane) {
            match = TRUE;
            if (addr == HLP_MSEC_RX_LANE_SC0_SA0_EN_RCV(blockId, lane, 0)) {
                rxSC = 0;
                rxAN = 0;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC0_SA0_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC0_SA1_EN_RCV(blockId, lane, 0)) {
                rxSC = 0;
                rxAN = 1;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC0_SA1_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC0_SA2_EN_RCV(blockId, lane, 0)) {
                rxSC = 0;
                rxAN = 2;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC0_SA2_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC0_SA3_EN_RCV(blockId, lane, 0)) {
                rxSC = 0;
                rxAN = 3;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC0_SA3_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC1_SA0_EN_RCV(blockId, lane, 0)) {
                rxSC = 1;
                rxAN = 0;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC1_SA0_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC1_SA1_EN_RCV(blockId, lane, 0)) {
                rxSC = 1;
                rxAN = 1;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC1_SA1_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC1_SA2_EN_RCV(blockId, lane, 0)) {
                rxSC = 1;
                rxAN = 2;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC1_SA2_EN_RCV,
                                      ENABLERECEIVE);
            }
            else if (addr == HLP_MSEC_RX_LANE_SC1_SA3_EN_RCV(blockId, lane, 0)) {
                rxSC = 1;
                rxAN = 3;
                rxEnable = FM_GET_BIT(newValue, HLP_MSEC_RX_LANE_SC1_SA3_EN_RCV,
                                      ENABLERECEIVE);
            }
            else {
                match = FALSE;
            }

            if (match)
                goto UPDATE_RX_SA;
        }
    }

UPDATE_RX_SA:
    if (match) {
        MSEC_LOG("Update RX SA: blockId=%d - lane=%d - SC=%d - AN=%d - rxEnable=%d\n",
                 blockId, lane, rxSC, rxAN, rxEnable);
        hlp_msec_rx_sa SA;
        readMacsecRxSA(model, blockId, lane, rxSC, rxAN, &SA);
        SA.inUse = rxEnable;
        SA.enableRx = rxEnable;
        writeMacsecRxSA(model, &SA);
    }

    return FM_OK;
}
