/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_mac.c
 * Creation Date:   June 22, 2012
 * Description:     RX_MAC and TX_MAC stages of HLP white model
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
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

//FIXME: Remove these defines once hlp_model_types.h is updated */
/* Metadata */
#define HLP_MODEL_META_TYPE_OFF       0

#define HLP_MODEL_META_TYPE_LAN_RX    0x0
#define HLP_MODEL_META_TYPE_LAN_TX    0x1
#define HLP_MODEL_META_TYPE_MARKER    0x2
#define HLP_MODEL_META_TYPE_DSI_RX    0x14
#define HLP_MODEL_META_TYPE_DSI_TX    0x16
#define HLP_MODEL_META_TYPE_RIMMON_RX 0x18

#define HLP_ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))

#define INCR_CNTR(model, addr, increment, saturate)                            \
    IncrementCounter((model),                                                  \
                     FM_MODEL_GET_REG_PTR((model), (addr)),                    \
                     (increment),                                              \
                     (saturate));

/* Not generalized if stride is changed */
#define HLP_MODEL_MAC_CFG_0_IN_RANGE(addr)                                     \
            ((0xFFFE0FFF & addr) == HLP_MAC_CFG(0,0))
#define HLP_MODEL_MAC_STATS_IN_RANGE(addr)                                     \
            (((0xFFFE0FFF & addr) >= HLP_MAC_STAT_TX_OCTET(0,0)) &&            \
             ((0xFFFE0FFF & addr) <= HLP_MAC_STAT_RX_MSDATA_DROP(0,0)))
#define HLP_MODEL_MAC_IM_IN_RANGE(addr)                                        \
            ((0xFFFE0FFF & addr) == HLP_MAC_IM(0,0))
#define HLP_MODEL_MAC_IP_IN_RANGE(addr)                                        \
            ((0xFFFE0FFF & addr) == HLP_MAC_IP(0,0))

typedef enum
{
    HLP_MODEL_PRI_PREC_VLAN = 0,

    HLP_MODEL_PRI_PREC_MPLS = 1,

    HLP_MODEL_PRI_PREC_IP = 2,

    HLP_MODEL_PRI_PREC_META = 3,

} hlp_modelPriPrec;


/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

static void IncrementCounter(hlp_model *model,
                             fm_uint32 *    addr,
                             fm_uint32      increment,
                             fm_bool        saturate);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

static void IncrementCounter(hlp_model *model,
                             fm_uint32 *    addr,
                             fm_uint32      increment,
                             fm_bool        saturate)
{
    fm_uint32 delta;

    FM_NOT_USED(model);

    delta = 0xFFFFFFFF - *addr;
    if (saturate)
    {
        *addr += MIN(increment, delta);
    }
    else
    {
        if (increment > delta)
        {
            /* Ensure that 0xFFFFFFFF + 1 wraps around to zero. */
            *addr = increment - (1 + delta);
        }
        else
        {
            *addr += increment;
        }
    }

}   /* end IncrementCounter */

static void UpdateInterruptChain(hlp_model *model)
{
    fm_uint32   *mac_ip;
    fm_uint32   *mac_im;
    fm_uint32   *ports_ip;
    fm_uint32   *global_intr;
    fm_uint      intr;
    fm_uint      port;

    intr = 0;
    for (port = 0; port < HLP_MAC_IP_ENTRIES; port++)
    {
        mac_ip = FM_MODEL_GET_REG_PTR(model, HLP_MAC_IP(port, 0));
        mac_im = FM_MODEL_GET_REG_PTR(model, HLP_MAC_IM(port, 0));
        /* Assume this is 32-bit */
        if (mac_ip[0] & ~mac_im[0])
        {
            intr = (1 << port);
        }
    }

    ports_ip = FM_MODEL_GET_REG_PTR(model, HLP_PORTS_IP(0));
    if (FM_ARRAY_GET_FIELD(ports_ip, HLP_PORTS_IP, MAC) != intr)
    {
        FM_ARRAY_SET_FIELD(ports_ip, HLP_PORTS_IP, MAC, intr);
        global_intr = FM_MODEL_GET_REG_PTR(model, HLP_GLOBAL_INTERRUPT(0));
        FM_ARRAY_SET_BIT(global_intr, HLP_GLOBAL_INTERRUPT, PORTS, intr?1:0);
    }

    return;
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelMacRx
 * \ingroup intModel
 *
 * \desc            Checks if the packet length is within the excepted range and
 *                  if the packet has been corrupted during transit.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          0 if the packet is to be processed by the frame processor,
 *                  1 otherwise.
 *
 *****************************************************************************/
fm_int hlpModelMacRx(hlp_model *model)
{
    hlp_modelState *         state = &model->packetState;
    fm_uint32 *                  mac_cfg;
    fm_uint32 *                  mac_rx_mii_cfg;
    fm_uint32                    fcs;
    fm_uint32                    maxLength;
    fm_uint32                    miiWidth;
    fm_uint32                    minLength;
    fm_uint32                    p;
    fm_uint32                    rxLength;
    fm_uint32                    rxUseLength;
    fm_int                       drop = 0;
    fm_int                       port;
    fm_int                       i;
    fm_int                       s;
    fm_bool                      badFcs = FALSE;
    fm_bool                      badFrame = FALSE;
    fm_bool                      rxCntFrame = TRUE;
    fm_bool                      overSize = FALSE;

    fm_uint32                   *mac_ip;
    fm_uint32                   *mac_rx_parser_cfg_1;
    fm_uint32                   *mac_rx_parser_cfg_2;
    fm_uint32                   *mac_rx_parser_cfg_3;
    fm_uint32                   *mac_pia_cfg;
    fm_uint32                    eventCntSel;
    fm_uint32                    userEthTypeSel1;
    fm_uint32                    userEthTypeSel2;
    fm_uint32                    userEthType1;
    fm_uint32                    userEthType2;
    fm_uint32                    ipv4EthType;
    fm_uint32                    ipv6EthType;
    fm_uint32                    mplsUcEthType;
    fm_uint32                    mplsMcEthType;
    fm_uint32                    vlan1EthType;
    fm_uint32                    vlan2EthType;
    fm_uint32                    priPrec1;
    fm_uint32                    priPrec2;
    fm_uint32                    priPrec3;
    fm_uint32                    priPrec4;

    fm_uint32                    priEncMeta;
    fm_uint32                    priEncMplsMc;
    fm_uint32                    priEncMplsUc;
    fm_uint32                    priEncVlan1;
    fm_uint32                    priEncVlan2;
    fm_uint32                    priEncIp;
    fm_uint32                    pktEthType;

    fm_uint32                    onpiSpeed;

    fm_int                       ipVer = -1;
    fm_int                       vpri1 = -1;
    fm_int                       vpri2 = -1;
    fm_int                       mplsMcVpri = -1;
    fm_int                       mplsUcVpri = -1;
    fm_int                       dscp = -1;
    fm_int                       metaTc = -1;
    fm_int                       defaultLossless = 0;
    fm_int                       lossless;
    fm_uint                      off = 12;
    fm_byte                     *data;
    fm_bool                     upIntr = FALSE;

    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Model %p RX_PORT=%d RX_LEN %d\n",
        (void *)model, state->RX_PORT, state->RX_LENGTH);

    port = state->RX_PORT;
    rxLength = state->RX_LENGTH;
    rxUseLength = state->RX_LENGTH;

    /* XXX Enforcement of the minimum distance between each frame can't be
     * implemented due to the zero time assumption. */

    mac_ip = FM_MODEL_GET_REG_PTR(model, HLP_MAC_IP(port, 0));
    mac_rx_parser_cfg_1 = FM_MODEL_GET_REG_PTR(model, HLP_MAC_RX_PARSER_CFG_1(port, 0));
    mac_rx_parser_cfg_2 = FM_MODEL_GET_REG_PTR(model, HLP_MAC_RX_PARSER_CFG_2(port, 0));
    mac_rx_parser_cfg_3 = FM_MODEL_GET_REG_PTR(model, HLP_MAC_RX_PARSER_CFG_3(port, 0));
    mac_pia_cfg         = FM_MODEL_GET_REG_PTR(model, HLP_MAC_PIA_CFG(port, 0));
    eventCntSel = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_1, HLP_MAC_RX_PARSER_CFG_1,
                    EVENT_COUNTER_SELECT);
    userEthTypeSel1 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_1, HLP_MAC_RX_PARSER_CFG_1,
                     USER_ETHERTYPE1_SEL);
    userEthTypeSel2 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_1, HLP_MAC_RX_PARSER_CFG_1,
                        USER_ETHERTYPE2_SEL);
    userEthType1 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_1, HLP_MAC_RX_PARSER_CFG_1,
                    USER_ETHERTYPE1);
    userEthType2 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_1, HLP_MAC_RX_PARSER_CFG_1,
                    USER_ETHERTYPE2);

    FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, RX_FRAME, 1);
    upIntr = TRUE;

    ipv4EthType = 0x0800;
    ipv6EthType = 0x86DD;
    mplsUcEthType = 0x8847;
    mplsMcEthType = 0x8848;
    vlan1EthType = 0x8100;
    vlan2EthType = 0x88A8;

    switch (userEthTypeSel1)
    {
        case 1:
            vlan1EthType = userEthType1;
            break;
        case 2:
            mplsUcEthType = userEthType1;
            break;
        case 3:
            ipv4EthType = userEthType1;
            break;
    }
    switch (userEthTypeSel2)
    {
        case 1:
            vlan2EthType = userEthType2;
            break;
        case 2:
            mplsMcEthType = userEthType2;
            break;
        case 3:
            ipv6EthType = userEthType2;
            break;
    }

    priPrec1 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_PRECEDENCE1);
    priPrec2 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_PRECEDENCE2);
    priPrec3 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_PRECEDENCE3);
    priPrec4 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_PRECEDENCE4);

    priEncMeta = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_ENC_META);
    priEncMplsMc = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_ENC_MPLS1);
    priEncMplsUc = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_ENC_MPLS2);
    priEncVlan1 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_ENC_VLAN1);
    priEncVlan2 = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_2, HLP_MAC_RX_PARSER_CFG_2,
                    PRI_ENC_VLAN2);
    priEncIp = FM_ARRAY_GET_FIELD(mac_rx_parser_cfg_3, HLP_MAC_RX_PARSER_CFG_3,
                    PRI_ENC_IP);

    metaTc = -1;
    switch (state->PKT_META[HLP_MODEL_META_TYPE_OFF])
    {
        case HLP_MODEL_META_TYPE_LAN_RX:
        case HLP_MODEL_META_TYPE_DSI_RX:
            metaTc = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META, 27, 2);
            break;
        case HLP_MODEL_META_TYPE_MARKER:
            metaTc = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META, 21, 2);
            break;
        case HLP_MODEL_META_TYPE_LAN_TX:
        case HLP_MODEL_META_TYPE_DSI_TX:
            metaTc = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META, 40, 2);
            break;
    }
    data = state->RX_DATA;

    while (off < (rxLength - 4))
    {
        pktEthType = data[off] << 8 | data[off+1];
        FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,"off %d ethType 0x%x ipVer %d\n",
            off, pktEthType, ipVer);
        if (ipVer >= 0)
        {
            if (ipVer == 4)
            {
                FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, PRI_ENC_IPV4, 1);
                upIntr = TRUE;
                if (eventCntSel == 0)
                    INCR_CNTR(model, HLP_MAC_RX_PARSER_COUNTER(port, 0), 1, FALSE);
                if (dscp < 0) dscp = (data[off + 1] >> 2);
            }
            else if (ipVer == 6)
            {
                FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, PRI_ENC_IPV6, 1);
                upIntr = TRUE;
                if (eventCntSel == 1)
                    INCR_CNTR(model, HLP_MAC_RX_PARSER_COUNTER(port, 0), 1, FALSE);
                if (dscp < 0) dscp = ((data[off] & 0xF) << 2) | (data[off + 1] >> 6);
            }
            off = rxLength; //break out of while loop
        }
        else if (pktEthType == ipv4EthType)
        {
            off += 2;
            ipVer = 4; //Next is IPV4 header
        }
        else if (pktEthType == ipv6EthType)
        {
            off += 2;
            off += 2;
            ipVer = 6; //Next is IPV6 header
        }
        else if (pktEthType == vlan1EthType)
        {
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, PRI_ENC_VLAN1, 1);
            upIntr = TRUE;
            if (eventCntSel == 4)
                INCR_CNTR(model, HLP_MAC_RX_PARSER_COUNTER(port, 0), 1, FALSE);
            if (vpri1 < 0) vpri1 = (data[off+2] >> 5) & 0x7;
            off += 4;
        }
        else if (pktEthType == vlan2EthType)
        {
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, PRI_ENC_VLAN2, 1);
            upIntr = TRUE;
            if (eventCntSel == 5)
                INCR_CNTR(model, HLP_MAC_RX_PARSER_COUNTER(port, 0), 1, FALSE);
            if (vpri2 < 0) vpri2 = (data[off+2] >> 5) & 0x7;
            off += 4;
        }
        else if (pktEthType == mplsUcEthType || pktEthType == mplsMcEthType)
        {
            off += 2;
            if (pktEthType == mplsMcEthType)
            {
                FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, PRI_ENC_MPLS2, 1);
                upIntr = TRUE;
                if (eventCntSel == 2) 
                    INCR_CNTR(model, HLP_MAC_RX_PARSER_COUNTER(port, 0), 1, FALSE);
                if (mplsMcVpri < 0) mplsMcVpri = ((data[off + 3] >> 1) & 0x7);
            }
            if (pktEthType == mplsUcEthType)
            {
                FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, PRI_ENC_MPLS1, 1);
                upIntr = TRUE;
                if (eventCntSel == 3) 
                    INCR_CNTR(model, HLP_MAC_RX_PARSER_COUNTER(port, 0), 1, FALSE);
                if (mplsUcVpri < 0) mplsUcVpri = ((data[off + 3] >> 1) & 0x7);
            }
            for (i = 0; i < 7; i++)
            {
                off += 4;
                if (off >= rxLength) break;
                /* Bottom of stack */
                if (data[off -4 + 2] & 1)
                {
                    ipVer = (data[off] >> 4);
                    break;
                }
            }
        }
        else if (pktEthType < 0x600)
        {
            off += 2;
            /* IPv4/6 without the ethType tag? */
            ipVer = (data[off] >> 4);
        }
        else
        {
            FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,"Unhandled ethType 0x%04x\n", pktEthType);
            off = rxLength; //break out of while loop
        }
    }

    /* Precedence */
    lossless = -1;
    switch (priPrec1)
    {
        case HLP_MODEL_PRI_PREC_VLAN:
            if (vpri1 >= 0)
            {
                lossless = ((priEncVlan1 >> vpri1) & 1);
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec1:Vlan vpri1 %d\n", vpri1);
            }
            break;
        case HLP_MODEL_PRI_PREC_MPLS:
            if (mplsUcVpri >= 0)
            {
                lossless = ((priEncMplsUc >> mplsUcVpri) & 1);
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec1:Mpls mplsUcVpri %d\n", mplsUcVpri);
            }
            else if (mplsMcVpri >= 0)
            {
                lossless = ((priEncMplsMc >> mplsMcVpri) & 1);
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec1:Mpls mplsMcVpri %d\n", mplsMcVpri);
            }
            break;
        case HLP_MODEL_PRI_PREC_IP:
            if (dscp >= 0)
            {
                lossless = ((priEncIp >> dscp) & 1);
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec1:IP dscp %d\n", dscp);
            }
            break;
        case HLP_MODEL_PRI_PREC_META:
            if (metaTc >= 0)
            {
                lossless = (priEncMeta >> metaTc) & 1;
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec1:Meta %d\n", metaTc);
            }
            break;
    }
    if (lossless < 0)
    {
        switch (priPrec2)
        {
            case HLP_MODEL_PRI_PREC_VLAN:
                if (vpri1 >= 0)
                {
                    lossless = ((priEncVlan1 >> vpri1) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec2:Vlan vpri1 %d\n", vpri1);
                }
                break;
            case HLP_MODEL_PRI_PREC_MPLS:
                if (mplsUcVpri >= 0)
                {
                    lossless = ((priEncMplsUc >> mplsUcVpri) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec2:Mpls mplsUcVpri %d\n", mplsUcVpri);
                }
                else if (mplsMcVpri >= 0)
                {
                    lossless = ((priEncMplsMc >> mplsMcVpri) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec2:Mpls mplsMcVpri %d\n", mplsMcVpri);
                }
                break;
            case HLP_MODEL_PRI_PREC_IP:
                if (dscp >= 0)
                {
                    lossless = ((priEncIp >> dscp) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec2:IP dscp %d\n", dscp);
                }
                break;
            case HLP_MODEL_PRI_PREC_META:
                if (metaTc >= 0)
                {
                    lossless = (priEncMeta >> metaTc) & 1;
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec2:Meta %d\n", metaTc);
                }
                break;
        }
    }
    if (lossless < 0)
    {
        switch (priPrec3)
        {
            case HLP_MODEL_PRI_PREC_VLAN:
                if (vpri1 >= 0)
                {
                    lossless = ((priEncVlan1 >> vpri1) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec3:Vlan vpri1 %d\n", vpri1);
                }
                break;
            case HLP_MODEL_PRI_PREC_MPLS:
                if (mplsUcVpri >= 0)
                {
                    lossless = ((priEncMplsUc >> mplsUcVpri) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec3:Mpls mplsUcVpri %d\n", mplsUcVpri);
                }
                else if (mplsMcVpri >= 0)
                {
                    lossless = ((priEncMplsMc >> mplsMcVpri) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec3:Mpls mplsMcVpri %d\n", mplsMcVpri);
                }
                break;
            case HLP_MODEL_PRI_PREC_IP:
                if (dscp >= 0)
                {
                    lossless = ((priEncIp >> dscp) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec3:IP dscp %d\n", dscp);
                }
                break;
            case HLP_MODEL_PRI_PREC_META:
                if (metaTc >= 0)
                {
                    lossless = (priEncMeta >> metaTc) & 1;
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec3:Meta %d\n", metaTc);
                }
                break;
        }
    }
    if (lossless < 0)
    {
        switch (priPrec4)
        {
            case HLP_MODEL_PRI_PREC_VLAN:
                if (vpri1 >= 0)
                {
                    lossless = ((priEncVlan1 >> vpri1) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec4:Vlan vpri1 %d\n", vpri1);
                }
                break;
            case HLP_MODEL_PRI_PREC_MPLS:
                if (mplsUcVpri >= 0)
                {
                    lossless = ((priEncMplsUc >> mplsUcVpri) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec4:Mpls mplsUcVpri %d\n", mplsUcVpri);
                }
                else if (mplsMcVpri >= 0)
                {
                    lossless = ((priEncMplsMc >> mplsMcVpri) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec4:Mpls mplsMcVpri %d\n", mplsMcVpri);
                }
                break;
            case HLP_MODEL_PRI_PREC_IP:
                if (dscp >= 0)
                {
                    lossless = ((priEncIp >> dscp) & 1);
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec4:IP dscp %d\n", dscp);
                }
                break;
            case HLP_MODEL_PRI_PREC_META:
                if (metaTc >= 0)
                {
                    lossless = (priEncMeta >> metaTc) & 1;
                    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Prec4:Meta %d\n", metaTc);
                }
                break;
        }
    }
    if (lossless < 0)
    {
        lossless = defaultLossless;
    }

    //FIXME: lossless is passed thru SAI (Add a state variable?)

    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,
        "vpri1 %d vpri2 %d  mplsUcVpri %d mplsMcVpri %d dscp %d metaTc %d lossless %d\n", 
        vpri1, vpri2, mplsUcVpri, mplsMcVpri, dscp, metaTc, lossless);


    mac_rx_mii_cfg = FM_MODEL_GET_REG_PTR(model, HLP_MAC_RX_MII_CFG(port, 0));
    mac_cfg = FM_MODEL_GET_REG_PTR(model, HLP_MAC_CFG(port, 0));
    maxLength = FM_ARRAY_GET_FIELD(mac_cfg, HLP_MAC_CFG, MAX_FRAME_LENGTH);
    minLength = 64;

    if (maxLength < 64)
    {
        FM_LOG_ERROR(FM_LOG_CAT_MODEL_MAC,
            "HLP_MAC_CFG[%d].max_frame_length is %d\n", port, maxLength);
        maxLength = 1519;
    }

    if (rxLength < minLength)
    {
        state->RX_FLAGS |= HLP_MODEL_PACKET_TOO_SHORT;
        badFrame = TRUE;
        rxCntFrame = FALSE;
        drop = 1;
    }
    else if (rxLength > maxLength)
    {
      onpiSpeed = FM_ARRAY_GET_FIELD(mac_pia_cfg, HLP_MAC_PIA_CFG,
                                     ONPI_SPEED);

      // MAC will truncate at 4 (lower speed) or 8 (higher speed) byte boundaries.
      if ( onpiSpeed < HLP_MAC_ONPI_SPEED_S_40G ) {
        state->RX_LENGTH = ( maxLength + 4 ) & ~0x3 ;
      } else {
        state->RX_LENGTH = ( maxLength + 8 ) & ~0x7 ;
      }

      // Handle the corner case where the rxLength is now less than the padded length
      if ( rxLength < state->RX_LENGTH ) {
        state->RX_LENGTH = rxLength ;
      }
      
      if ( ( FM_ARRAY_GET_BIT(mac_rx_mii_cfg,
                              HLP_MAC_RX_MII_CFG,
                              IGNORE_OVERSIZE_ERROR) == FALSE ) ||
           ( FM_ARRAY_GET_BIT(mac_rx_mii_cfg,
                              HLP_MAC_RX_MII_CFG,
                              IGNORE_FCS_ERROR) == FALSE ) )
        {
          state->RX_FLAGS |= HLP_MODEL_PACKET_TOO_LONG;
        }
      overSize = TRUE;
      badFrame = TRUE;
    }

    /* Compute and verify the actual FCS. */
    FM_CLEAR(fcs);
    p = ( rxLength < 4 ? 0 : rxLength - 4 );

    for (i = 0, s = 0; i < 4 && p < rxLength; i++, p++, s += 8)
    {
        fcs |= ((fm_uint32) state->RX_DATA[p]) << s;
    }

    if ( ( rxLength <= 4 ) ||
         ( fmCrc32(state->RX_DATA, rxLength - 4) != fcs ) )
    {
        badFcs = TRUE;
        FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Length %d Expected FCS %08x Got %08x\n",
            rxLength, fmCrc32(state->RX_DATA, rxLength - 4), fcs);
    }

    if ( FM_ARRAY_GET_BIT(mac_rx_mii_cfg, HLP_MAC_RX_MII_CFG, FORCE_BAD_FCS) )
    {
        /* The FCS of the frame is forced bad by the MAC. */
        state->RX_FLAGS |= HLP_MODEL_PACKET_BAD_FCS;
    }

    else if ( badFcs &&
              ( FM_ARRAY_GET_BIT(mac_rx_mii_cfg,
                                 HLP_MAC_RX_MII_CFG,
                                 IGNORE_FCS_ERROR) == FALSE ) )
    {
        state->RX_FLAGS |= HLP_MODEL_PACKET_BAD_FCS;
    }

    if (badFcs)
    {
        FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_RX_FCS, 1);
        upIntr = TRUE;
    }

    if (state->RX_FLAGS & HLP_MODEL_PACKET_TOO_SHORT)
    {
        if (badFcs)
        {
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_RX_RUNT, 1);
            upIntr = TRUE;
            INCR_CNTR(model, HLP_MAC_STAT_RX_RUNT(port, 0), 1, FALSE);
        }
        else
        {
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_RX_UNDERSIZE, 1);
            upIntr = TRUE;
            INCR_CNTR(model, HLP_MAC_STAT_RX_UNDERSIZE(port, 0), 1, FALSE);
        }
    }
    else if (overSize)
    {
        if (badFcs)
        {
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_RX_JABBER, 1);
            upIntr = TRUE;
            INCR_CNTR(model,HLP_MAC_STAT_RX_JABBER(port, 0), 1, FALSE);
        }
        else
        {
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_RX_OVERSIZE, 1);
            upIntr = TRUE;
            INCR_CNTR(model, HLP_MAC_STAT_RX_OVERSIZE(port, 0), 1, FALSE);
        }
    }

    /* ================== Beginning of MACsec custom code ===================
     * This is not part of the vanilla WM from DV and should not be removed
     * during the integration of newer code.
     */

    if (hlpModelMacsecRx(model) != FM_OK) {
        // TODO - Should I update a flag?
        // state->RX_FLAGS |= HLP_MODEL_PACKET_TOO_SHORT;
        badFrame = TRUE;
        drop = 1;
    }

    /* ==================== End of MACsec custom code ==================== */

    /* No support */
    /* RX_PAUSE
       RX_PFC
       DRAIN_DROP
       LINK_COUNTER
    */
    if (state->RX_FLAGS & HLP_MODEL_PACKET_BAD_FCS) {
        state->SEG_META_ERR = 1;
    }
    else if((state->RX_FLAGS & HLP_MODEL_PACKET_TOO_SHORT) || (state->RX_FLAGS & HLP_MODEL_PACKET_TOO_LONG)) {
        state->SEG_META_ERR = 2;
    }

    if ( rxCntFrame == TRUE ) {

      /* Set rxUseLength to maxLength for stats purposes */
      if (rxUseLength > maxLength) {
        rxUseLength = maxLength ;
      }
    
      INCR_CNTR(model, HLP_MAC_STAT_RX_OCTET(port, 0), rxUseLength, FALSE);
      INCR_CNTR(model, HLP_MAC_STAT_RX_FRAME(port, 0), 1, FALSE);
      
      if (badFcs || badFrame) {
        INCR_CNTR(model, HLP_MAC_STAT_RX_BAD_OCTET(port, 0), rxUseLength, FALSE);
        INCR_CNTR(model, HLP_MAC_STAT_RX_BAD_FRAME(port, 0), 1, FALSE);
        INCR_CNTR(model, HLP_MAC_STAT_RX_FCS_ERROR(port, 0), badFcs ? 1:0, FALSE);
      }
        
      if (!overSize) {

         if (rxLength == 64)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_64(port, 0), 1, FALSE);
           }
         else if (rxLength >= 65 && rxLength < 128)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_65(port, 0), 1, FALSE);
           }
         else if (rxLength >= 128 && rxLength < 256)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_128(port, 0), 1, FALSE);
           }
         else if (rxLength >= 256 && rxLength < 512)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_256(port, 0), 1, FALSE);
           }
         else if (rxLength >= 512 && rxLength < 1024)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_512(port, 0), 1, FALSE);
           }
         else if (rxLength >= 1024 && rxLength < 1519)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_1024(port, 0), 1, FALSE);
           }
         else if (rxLength >= 1519 && rxLength <= maxLength)
           {
             INCR_CNTR(model, HLP_MAC_STAT_RX_1519(port, 0), 1, FALSE);
           }
        }
    }

    if (upIntr) UpdateInterruptChain(model);
    
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpMacRx);

    return drop;

}   /* end hlpModelMacRx */




/*****************************************************************************/
/** hlpModelMacTx
 * \ingroup intModel
 *
 * \desc            Pads or truncates the packet if necessary and updates the
 *                  FCS.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       packet points to the caller-allocated egress packet data
 *                  buffer.
 *
 * \param[in]       maxPktSize is the maximum number of packet bytes this
 *                  function can store in the buffer pointed to by packet.
 *
 * \return          0 if the packet is to be forwarded to the link partner,
 *                  1 otherwise.
 *
 *****************************************************************************/
fm_int hlpModelMacTx(hlp_model *model,
                         fm_byte *      packet,
                         fm_uint32      maxPktSize)
{
    hlp_modelState *         state = &model->packetState;
    fm_uint32 *                  mac_tx_path_cfg;
    fm_uint32 *                  mac_tx_mii_cfg;
    fm_uint32 *                  mac_cfg;
    fm_uint32 *                  mac_path_cfg;
    fm_uint32 *                  mac_ip;
    fm_uint32                    fcs;
    fm_uint32                    incoming_bad_fcs;
    fm_uint32                    minLength;
    fm_uint32                    txLength;
    fm_uint32                    maxTxLength;
    fm_uint32                    p;
    fm_int                       drop = 0;
    fm_int                       port;
    fm_int                       i;
    fm_int                       s;
    fm_bool                      badFrame = FALSE;
    fm_bool                      badRxFcs = FALSE;
    fm_bool                      badFcs = FALSE;
    fm_bool                      upIntr = FALSE;
    fm_bool                      underSize = FALSE;
    fm_bool                      overSize = FALSE;
    fm_bool                      encrypted = FALSE;

    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Model %p TX_PORT=%d TX_LEN %d\n",
            (void *)model, state->TX_PORT, state->TX_LENGTH);

    port = state->TX_PORT;

    mac_ip = FM_MODEL_GET_REG_PTR(model, HLP_MAC_IP(port, 0));
    mac_tx_path_cfg = FM_MODEL_GET_REG_PTR(model, HLP_MAC_TX_PATH_CFG(port, 0));
    mac_tx_mii_cfg = FM_MODEL_GET_REG_PTR(model, HLP_MAC_TX_MII_CFG(port, 0));
    mac_cfg = FM_MODEL_GET_REG_PTR(model, HLP_MAC_CFG(port, 0));
    mac_path_cfg = FM_MODEL_GET_REG_PTR(model, HLP_MAC_PATH_CFG(port, 0));
    maxTxLength = FM_ARRAY_GET_FIELD(mac_cfg, HLP_MAC_CFG, MAX_FRAME_LENGTH);
    minLength = 64;
    incoming_bad_fcs = 0;

    FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, TX_FRAME, 1);
    upIntr = TRUE;

    if (maxTxLength < 64)
    {
        FM_LOG_ERROR(FM_LOG_CAT_MODEL_MAC,
            "HLP_MAC_TX_MII_CFG[%d].max_frame_length is %d\n", port, maxTxLength);
        maxTxLength = 1519;
    }

    txLength = state->TX_LENGTH;
    /* Compute and verify the actual FCS. */
    FM_CLEAR(fcs);
    p = ( txLength < 4 ? 0 : txLength - 4 );

    for (i = 0, s = 0; i < 4 && p < txLength; i++, p++, s += 8)
    {
        fcs |= ((fm_uint32) packet[p]) << s;
    }

    badRxFcs = FALSE;
    if ( ( txLength <= 4 ) ||
         ( fmCrc32(packet, txLength - 4) != fcs ) )
    {
        //TX_DATA_CORRUPT should be set only for unexpected FCS errors, not for
        //all FCS errors.
        //FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_TX_DATA_CORRUPT, 1);
        //upIntr = TRUE;

        //INCR_CNTR(model, HLP_MAC_STAT_TX_DATA_CORRUPT(port, 0), 1, FALSE);
        badRxFcs = TRUE;
        incoming_bad_fcs = fcs ;
    }

    /* ================== Beginning of MACsec custom code ===================
     * This is not part of the vanilla WM from DV and should not be removed
     * during the integration of newer code.
     */

    /* Send frame to MACsec Tx and in case of failure return with drop flag */
    drop = hlpModelMacsecTx(model, packet, maxPktSize, &encrypted);
    if (drop)
    {
        FM_LOG_ERROR(FM_LOG_CAT_MODEL_MAC,
                "Failure in Macsec Tx, stop execution of MAC and drop frame\n");
        return drop;
    }

    /* If packet was encrypted -> recompute the FCS */
    if (encrypted)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,
                "Frame has been encrypted -> FCS will be recomputed\n");
    }
    /* ==================== End of MACsec custom code ==================== */

    /* Pad the frame to the configured minimum length if necessary. */
    if (txLength < minLength)
    {
        p = state->TX_LENGTH - 4;
        for ( ; p < minLength && p < maxPktSize; p++)
        {
            packet[p] = 0;
        }

        state->TX_LENGTH = minLength;
        underSize = TRUE;
    }
    else if (txLength > maxTxLength)
    {
        state->TX_LENGTH = maxTxLength + 1;
        badFrame = TRUE;
        // we shouldn't look at the badRxFcs indication because we are truncating
        badRxFcs = FALSE;
        overSize = TRUE;
    }
    
    //force bad CRC if framing_err is flagging by tail or if mac_rx_mii_cfg.force_bad_fcs=1
    if((state->SEG_META_ERR != 0) || (state->RX_FLAGS & HLP_MODEL_PACKET_BAD_FCS))
    {
        badFrame = 1;
    }

    txLength = state->TX_LENGTH;

    /* Update the FCS. */
    p = ( txLength < 4 ? 0 : txLength - 4 );

    fcs = fmCrc32(packet, p);

    badFcs = badRxFcs | badFrame;

    FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "FCS = 0x%x, badFcs=%d, badFrame=%d\n",
                 fcs, badFcs, badFrame);

    if (badFcs)
    {
      /* Poison the FCS. */
      //MAC recalculates CRC for undersized packets
      if (badRxFcs && !underSize) {
        fcs = incoming_bad_fcs ;
        FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "BadRXFCS. using incoming fcs. New fcs = 0x%x\n", fcs);
      } else {
        fcs = ~fcs;
        FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "Bad FCS. Inverting fcs. New fcs = 0x%x\n", fcs);
      }
    }

    for ( i = 0, s = 0 ;
          i < 4 && p < txLength && p < maxPktSize ;
          i++, p++, s += 8 )
    {
        packet[p] = (fm_byte) ((fcs >> s) & 0xFF);
        FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC, "packet[%d]=0x%x\n", p, packet[p]);
    }
    
    if (!overSize)
    {
        if (txLength == 64)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_64(port, 0), 1, FALSE);
        }
        else if (txLength >= 65 && txLength < 128)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_65(port, 0), 1, FALSE);
        }
        else if (txLength >= 128 && txLength < 256)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_128(port, 0), 1, FALSE);
        }
        else if (txLength >= 256 && txLength < 512)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_256(port, 0), 1, FALSE);
        }
        else if (txLength >= 512 && txLength < 1024)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_512(port, 0), 1, FALSE);
        }
        else if (txLength >= 1024 && txLength < 1519)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_1024(port, 0), 1, FALSE);
        }
        else if (txLength >= 1519 && txLength <= maxTxLength)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_1519(port, 0), 1, FALSE);
        }
    }
    /* No support */
    /* TX_UNDERRUN
       TX_TIMEOUT
       TX_CONTROL
    */
    
    INCR_CNTR(model, HLP_MAC_STAT_TX_FRAME(port, 0), 1, FALSE);
    INCR_CNTR(model, HLP_MAC_STAT_TX_OCTET(port, 0), txLength, FALSE);
    if (badFrame || badFcs)
    {
      if (badRxFcs) {
        FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, ERROR_TX_FCS, 1);
        upIntr = TRUE;
      }
      INCR_CNTR(model, HLP_MAC_STAT_TX_FCS_ERROR(port, 0), 1, FALSE);
      INCR_CNTR(model, HLP_MAC_STAT_TX_BAD_OCTET(port, 0), txLength, FALSE);
      INCR_CNTR(model, HLP_MAC_STAT_TX_BAD_FRAME(port, 0), 1, FALSE);
    }


    if (txLength > maxTxLength)
    {
        if (badFrame)
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_JABBER(port, 0), 1, FALSE);
        }
        /*
        else
        {
            INCR_CNTR(model, HLP_MAC_STAT_TX_OVERSIZE(port, 0), 1, FALSE);
        }
        */

    }

    if (upIntr) UpdateInterruptChain(model);
    
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpMacTx);

    if (FM_ARRAY_GET_BIT(mac_path_cfg, HLP_MAC_PATH_CFG, MII_LOOPBACK))
    {
        /* Only implement loopback back to the switch */
        return FM_ERR_SPARE1;
    }

    return drop;

}   /* end hlpModelMacTx */




/*****************************************************************************/
/** hlpModelMacWriteCSR
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
fm_status hlpModelMacWriteCSR(hlp_model *model,
                              fm_uint32 addr,
                              fm_uint32 newValue,
                              fm_uint32 oldValue,
                              fm_bool init)
{
    fm_status err = FM_OK;
    fm_uint32 *mac_ip;
    fm_uint32 *mac_stat;
    fm_int port;
    fm_int quadPort;
    fm_int word;
    fm_uint off;
    fm_bool upIntr = FALSE;


    FM_NOT_USED(model);
    FM_NOT_USED(addr);
    FM_NOT_USED(newValue);
    FM_NOT_USED(init);

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_MODEL_MAC,
                                "model=%p addr=0x%x newValue=0x%x\n",
                                (void *) model,
                                addr,
                                newValue);

    /* The FM_MODEL_IN_RANGE_XXX does not match only the specific register */
    if (HLP_MODEL_MAC_CFG_0_IN_RANGE(addr))
    {
        if (!FM_GET_BIT(oldValue, HLP_MAC_CFG, STATS_REQUEST) &&
            FM_GET_BIT(newValue, HLP_MAC_CFG, STATS_REQUEST))
        {
            FM_MODEL_GET_OFFSET_MULT_1(addr,
                                       HLP_MAC_CFG,
                                       &port,
                                       &word);
            if (port >= HLP_MAC_CFG_ENTRIES)
            {
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,
                             "Invalid port index %d for MAC CFG addr 0x%x\n",
                             port, addr); 
                return FM_OK;
            }

            quadPort = port/4;
            if (quadPort >= HLP_MAC4_SCRATCH_ENTRIES)
            {
                FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,
                             "Invalid quad port index %d for MAC CFG addr 0x%x\n",
                             quadPort, addr); 
                return TRUE;
            }

            /* Copy the mac stats to the temporary memory, assume continous memory */
            for (off = 0; off < HLP_ARRAY_SIZE(model->MAC_TX_STATS[0]); off++)
            {
                mac_stat = FM_MODEL_GET_REG_PTR(model, HLP_MAC_STAT_TX_OCTET(port, 0));
                model->MAC_TX_STATS[quadPort][off] = mac_stat[off];
                if (FM_GET_BIT(newValue, HLP_MAC_CFG, STATS_CLR_ON_SNAP))
                {
                    mac_stat[off] = 0;
                }
            }
            for (off = 0; off < HLP_ARRAY_SIZE(model->MAC_RX_STATS[0]); off++)
            {
                mac_stat = FM_MODEL_GET_REG_PTR(model, HLP_MAC_STAT_RX_OCTET(port, 0));
                model->MAC_RX_STATS[quadPort][off] = mac_stat[off];
                if (FM_GET_BIT(newValue, HLP_MAC_CFG, STATS_CLR_ON_SNAP))
                {
                    mac_stat[off] = 0;
                }
            }

            mac_ip = FM_MODEL_GET_REG_PTR(model, (addr + HLP_MAC_IP(0, 0)) - HLP_MAC_CFG(0, 0));
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, STATS_READY, 1);
            upIntr = TRUE;
        }
        else if (FM_GET_BIT(oldValue, HLP_MAC_CFG, STATS_REQUEST) &&
                 !FM_GET_BIT(newValue, HLP_MAC_CFG, STATS_REQUEST))
        {
            mac_ip = FM_MODEL_GET_REG_PTR(model, (addr + HLP_MAC_IP(0, 0)) - HLP_MAC_CFG(0, 0));
            upIntr = TRUE;
            FM_ARRAY_SET_BIT(mac_ip, HLP_MAC_IP, STATS_READY, 0);
        }
    } else if (!init && HLP_MODEL_MAC_IM_IN_RANGE(addr)) {
        upIntr = TRUE;
    } else if (!init && HLP_MODEL_MAC_IP_IN_RANGE(addr)) {
        upIntr = TRUE;
    }

    if (upIntr) UpdateInterruptChain(model);

ABORT:
    HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_MODEL_MAC, err);

}   /* end hlpModelMacWriteCSR */



/*****************************************************************************/
/** hlpModelMacStatsGet
 * \ingroup intModel
 *
 * \desc            Return true if request mac stats without enabling
 *                  MAC_CFG.stats_request sequence.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       val is the caller-allocated storage where the function
 *                  will place the register value for the given address
 *                  if the address is for the MAC stats.
 *
 * \return          TRUE if addr is for MAC stats.
 *
 *****************************************************************************/
fm_bool hlpModelMacStatsGet(hlp_model *model, fm_uint32 addr, fm_uint32 *val)
{
    fm_status err = FM_OK;
    fm_int port;
    fm_int word;
    fm_int quadPort;
    fm_uint off;

    if (HLP_MODEL_MAC_STATS_IN_RANGE(addr))
    {
        /* Not very generalized code.
         * Normalized to TX_OCTET(0,0) to get port index
         */
        FM_MODEL_GET_OFFSET_MULT_1((addr & 0xFFFFF000) | 0x100,
                                   HLP_MAC_STAT_TX_OCTET,
                                   &port,
                                   &word);
        if (port >= HLP_MAC_IP_ENTRIES)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MAC,
                         "Invalid port index %d for MAC STAT addr 0x%x\n",
                         port, addr); 
            return FALSE;
        }

        quadPort = port/4;
        if (quadPort >= HLP_MAC4_SCRATCH_ENTRIES)
        {
            return FALSE;
        }

        /* Not generalized check for TX and RX MAC stats registers */
        if ((addr & 0x100) == 0x100)
        {
            off = (addr & 0xFF)/4;
            if (off < HLP_ARRAY_SIZE(model->MAC_TX_STATS[0]))
            {
                *val = model->MAC_TX_STATS[quadPort][off];
                return TRUE;
            }
        }
        else if ((addr & 0x200) == 0x200)
        {
            off = (addr & 0xFF)/4;
            if (off < HLP_ARRAY_SIZE(model->MAC_RX_STATS[quadPort]))
            {
                *val = model->MAC_RX_STATS[quadPort][off];
                return TRUE;
            }
        }
        
    }

    return FALSE;

}   /* end hlpModelMacStatsGet */


