// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_congmgmt.h"
#include "mby_rxstats.h"
#include "mby_maskgen.h" // action codes

static fm_uint16 getBankIndex(const fm_uint32 rx_port)
{
    fm_uint16 index = 0;
    if (rx_port < MBY_FABRIC_LOG_PORTS)
        index = ((rx_port << 4) & 0x03F0);
    return index;
}

static fm_uint64 incrRxCounter(const fm_uint64 cnt_in, const fm_uint64 inc)
{
    const fm_uint64 cnt_max = FM_LITERAL_U64(0xFFFFFFFFFFFF); // 48-bit counter
    const fm_uint64 delta   = cnt_max - cnt_in; // delta between current and maximum counter values
    const fm_uint64 cnt_wrp = inc - (FM_LITERAL_U64(1) + delta); // safely wrap around
    // TODO Variable 'cnt_inc' is assigned a value that is never used.
    const fm_uint64 cnt_inc = cnt_in + inc;
    const fm_uint64 cnt_out = (inc > delta) ? cnt_wrp : cnt_inc;  // wrap around or increment

    return cnt_out;
}

static void updateRxStatsBank
(
    fm_uint32       regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 bank,
    const fm_uint16 index,
    const fm_uint64 len
)
{
    // Update (read/modify/write) frame count:
    fm_uint64 rx_stats_bank_frame_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_BANK_FRAME(bank, index, 0), &rx_stats_bank_frame_reg);
    fm_uint64 frame_cnt = FM_GET_FIELD64(rx_stats_bank_frame_reg, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER);
    fm_uint64 one       = FM_LITERAL_U64(1);
    frame_cnt = incrRxCounter(frame_cnt, one);
    FM_SET_FIELD64(rx_stats_bank_frame_reg, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER, frame_cnt);
    mbyModelWriteCSR64(regs, MBY_RX_STATS_BANK_FRAME(bank, index, 0), rx_stats_bank_frame_reg);

    // Update (read/modify/write) byte count:
    fm_uint64 rx_stats_bank_byte_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_BANK_BYTE(bank, index, 0), &rx_stats_bank_byte_reg);
    fm_uint64 byte_cnt = FM_GET_FIELD64(rx_stats_bank_byte_reg, MBY_RX_STATS_BANK_BYTE, BYTE_COUNTER);
    byte_cnt = incrRxCounter(byte_cnt, len);
    FM_SET_FIELD64(rx_stats_bank_byte_reg, MBY_RX_STATS_BANK_BYTE, BYTE_COUNTER, byte_cnt);
    mbyModelWriteCSR64(regs, MBY_RX_STATS_BANK_BYTE(bank, index, 0), rx_stats_bank_byte_reg);
}

static void updateRxStatsVlan
(
    fm_uint32       regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint16 index,
    const fm_uint64 len
)
{
    // Update (read/modify/write) frame count:
    fm_uint64 rx_stats_vlan_frame_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_VLAN_FRAME(index, 0), &rx_stats_vlan_frame_reg);
    fm_uint64 frame_cnt = FM_GET_FIELD64(rx_stats_vlan_frame_reg, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER);
    fm_uint64 one       = FM_LITERAL_U64(1);
    frame_cnt = incrRxCounter(frame_cnt, one);
    FM_SET_FIELD64(rx_stats_vlan_frame_reg, MBY_RX_STATS_VLAN_FRAME, FRAME_COUNTER, frame_cnt);
    mbyModelWriteCSR64(regs, MBY_RX_STATS_VLAN_FRAME(index, 0), rx_stats_vlan_frame_reg);

    // Update (read/modify/write) byte count:
    fm_uint64 rx_stats_vlan_byte_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_VLAN_BYTE(index, 0), &rx_stats_vlan_byte_reg);
    fm_uint64 byte_cnt = FM_GET_FIELD64(rx_stats_vlan_byte_reg, MBY_RX_STATS_VLAN_BYTE, BYTE_COUNTER);
    byte_cnt = incrRxCounter(byte_cnt, len);
    FM_SET_FIELD64(rx_stats_vlan_byte_reg, MBY_RX_STATS_VLAN_BYTE, BYTE_COUNTER, byte_cnt);
    mbyModelWriteCSR64(regs, MBY_RX_STATS_VLAN_BYTE(index, 0), rx_stats_vlan_byte_reg);
}

static void handleRxBank0
(
          fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 rx_length,
    const fm_uint32 rx_port,
    const fm_bool   is_ipv4,
    const fm_bool   is_ipv6,
    const fm_bool   is_bcast,
    const fm_bool   is_mcast,
    const fm_bool   is_ucast
)
{
    fm_uint32 bank  = 0;
    fm_uint16 index = getBankIndex(rx_port);
    fm_uint64 len   = rx_length;

    // IPv4 packets:
    if      (is_ipv4 && is_bcast)
        index += STAT_RxBcstPktsIPv4;
    else if (is_ipv4 && is_mcast)
        index += STAT_RxMcstPktsIPv4;
    else if (is_ipv4 && is_ucast)
        index += STAT_RxUcstPktsIPv4;
    // IPv6 packets:
    else if (is_ipv6 && is_bcast)
        index += STAT_RxBcstPktsIPv6;
    else if (is_ipv6 && is_mcast)
        index += STAT_RxMcstPktsIPv6;
    else if (is_ipv6 && is_ucast)
        index += STAT_RxUcstPktsIPv6;
    // Non-IP packets:
    else if (is_bcast)
        index += STAT_RxBcstPktsNonIP;
    else if (is_mcast)
        index += STAT_RxMcstPktsNonIP;
    else if (is_ucast)
        index += STAT_RxUcstPktsNonIP;

    updateRxStatsBank(regs, bank, index, len);
}

static void handleRxBank1
(
          fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 rx_length,
    const fm_uint32 rx_port,
    const fm_byte   traffic_class
)
{
    fm_uint32 bank  = 1;
    fm_uint16 index = getBankIndex(rx_port);
    fm_uint64 len   = rx_length;

    index += (traffic_class & 0x7);

    updateRxStatsBank(regs, bank, index, len);
}

static void handleRxBank2
(
          fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 rx_length,
    const fm_uint32 rx_port,
    const fm_uint32 action
)
{
    fm_uint32 bank  = 2;
    fm_uint16 index = getBankIndex(rx_port);
    fm_uint64 len   = rx_length;

    switch (action)
    {
        case MBY_ACTION_NORMAL:             index += STAT_FIDForwarded;                   break;
        case MBY_ACTION_FLOOD:              index += STAT_FloodForwarded;                 break;
        case MBY_ACTION_SPECIAL:            index += STAT_TargetedDeterministicForwarded; break;
        case MBY_ACTION_DROP_PARSE:         index += STAT_ParseErrDrops;                  break;
        case MBY_ACTION_DROP_PARITY:        index += STAT_ParityErrorDrops;               break;
        case MBY_ACTION_TRAP:               index += STAT_Trapped;                        break;
        case MBY_ACTION_DROP_CONTROL:       index += STAT_CtrlDrops;                      break;
        case MBY_ACTION_DROP_STP:           index += STAT_STPDrops;                       break;
        case MBY_ACTION_DROP_SV:            index += STAT_SecurityViolations;             break;
        case MBY_ACTION_MARKER_ERROR_DROPS: index += STAT_MarkerErrorDrops;               break;
        case MBY_ACTION_DROP_IV:            index += STAT_VlanIngressDrops;               break;
        case MBY_ACTION_DROP_EV:            index += STAT_VlanEgressDrops;                break;
        case MBY_ACTION_DROP_CAM:           index += STAT_GlortMissDrops;                 break;
        case MBY_ACTION_DROP_FFU:           index += STAT_FFUDrops;                       break;
        case MBY_ACTION_DROP_TRIG:          index += STAT_TriggerDrops;                   break;
        case MBY_ACTION_DROP_L3_PYLD_LEN:   index += STAT_L3PayloadLengthValidationDrops; break;
        default:                                                                          break;
    }

    updateRxStatsBank(regs, bank, index, len);
}

static void handleRxBank3
(
          fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 rx_length,
    const fm_uint32 rx_port,
    const fm_uint32 action
)
{
    fm_uint32 bank  = 3;
    fm_uint16 index = getBankIndex(rx_port);
    fm_uint64 len   = rx_length;

    switch (action)
    {
        case MBY_ACTION_DROP_POLICER:      index += STAT_PolicerDrops;              break;
        case MBY_ACTION_DROP_TTL:          index += STAT_TTLDrops;                  break;
        case MBY_ACTION_DROP_CM_GLOBAL:    index += STAT_CMGlobalDrops;             break;
        case MBY_ACTION_DROP_CM_SMP0:      index += STAT_SMP0Drops;                 break;
        case MBY_ACTION_DROP_CM_SMP1:      index += STAT_SMP1Drops;                 break;
        case MBY_ACTION_DROP_CM_RX_HOG0:   index += STAT_RXHog0Drops;               break;
        case MBY_ACTION_DROP_CM_RX_HOG1:   index += STAT_RXHog1Drops;               break;
        case MBY_ACTION_DROP_CM_TX_HOG0:   index += STAT_TXHog0Drops;               break;
        case MBY_ACTION_DROP_CM_TX_HOG1:   index += STAT_TXHog1Drops;               break;
        case MBY_ACTION_DROP_FRAME_ERR:    index += STAT_FrameErrorDrops;           break;
        case MBY_ACTION_REDIRECT_TRIG:     index += STAT_TriggerRedirects;          break;
        case MBY_ACTION_DROP_DLF:          index += STAT_FloodControlDrops;         break;
        case MBY_ACTION_GLORT_FORWARDED:   index += STAT_GlortForwarded;            break;
        case MBY_ACTION_DROP_LOOPBACK:     index += STAT_LoopbackSuppDrops;         break;
        case MBY_ACTION_BANK5_OTHER_DROPS: index += STAT_OtherDrops;                break;
        case MBY_ACTION_DROP_L4_CSUM:      index += STAT_L4CheckSumValidationDrops; break;
        default:                                                                    break;
    }

    updateRxStatsBank(regs, bank, index, len);
}

static void handleRxBankVlan
(
          fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 rx_length,
    const fm_uint32 action,
    const fm_uint16 l2_ivlan1_cnt,
    const fm_bool   is_bcast,
    const fm_bool   is_mcast,
    const fm_bool   is_ucast
)
{
    if (l2_ivlan1_cnt != 0)
    {
        fm_bool is_drop = !(action == MBY_ACTION_NORMAL ||
                            action == MBY_ACTION_FLOOD ||
                            action == MBY_ACTION_SPECIAL ||
                            action == MBY_ACTION_TRAP ||
                            action == MBY_ACTION_REDIRECT_TRIG ||
                            action == MBY_ACTION_GLORT_FORWARDED);

        fm_uint16 l2_type = (is_drop) ? 3 : (is_ucast) ? 0 : (is_mcast) ? 1 : (is_bcast) ? 2 : 0;
        fm_uint16 index   = ((l2_ivlan1_cnt << 2) | l2_type ) & 0x3FFF;
        fm_uint64 len     = rx_length;

        updateRxStatsVlan(regs, index, len);
    }
}

void RxStats
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyCongMgmtToRxStats * const in,
          mbyRxStatsToRxOut    * const out
)
{
    // Read inputs:
    const fm_uint32  rx_length     = in->RX_LENGTH;
    const fm_uint32  rx_port       = in->RX_PORT;
    const fm_bool    is_ipv4       = in->IS_IPV4;
    const fm_bool    is_ipv6       = in->IS_IPV6;
    const fm_macaddr l2_dmac       = in->L2_DMAC;
    const fm_uint16  l2_ivlan1_cnt = in->L2_IVLAN1_CNT;
    const fm_byte    traffic_class = in->TRAFFIC_CLASS;
    const fm_uint    action        = in->ACTION;

    fm_bool is_bcast = isBroadcastMacAddress(l2_dmac);
    fm_bool is_mcast = isMulticastMacAddress(l2_dmac);
    fm_bool is_ucast =   isUnicastMacAddress(l2_dmac);

    // Handle RX frame classification:
    handleRxBank0(regs, rx_length, rx_port, is_ipv4, is_ipv6, is_bcast, is_mcast, is_ucast);

    // Handle per-port RX TC counters:
    handleRxBank1(regs, rx_length, rx_port, traffic_class);

    // Perform RX forwarding action:
    fm_bool drop_act = ((action & 0x10) != 0);
    if (drop_act)
        handleRxBank3(regs, rx_length, rx_port, action);  // drop actions
    else
        handleRxBank2(regs, rx_length, rx_port, action);

    // Handle RX VLAN counters:
    handleRxBankVlan(regs, rx_length, action, l2_ivlan1_cnt, is_bcast, is_mcast, is_ucast);

    // Write outputs:
    out->RX_LENGTH         = rx_length;

    // Pass thru:
    out->DROP_TTL          = in->DROP_TTL;
    out->ECN               = in->ECN;
    out->EDGLORT           = in->EDGLORT;
    out->FNMASK            = in->FNMASK;
    out->IS_TIMEOUT        = in->IS_TIMEOUT;
    out->L2_DMAC           = in->L2_DMAC;
    out->L2_EVID1          = in->L2_EVID1;
    out->MARK_ROUTED       = in->MARK_ROUTED;
    out->MIRTYP            = in->MIRTYP;
    out->MOD_IDX           = in->MOD_IDX;
    out->NO_MODIFY         = in->NO_MODIFY;
    out->OOM               = in->OOM;
    out->PARSER_INFO       = in->PARSER_INFO;
    out->PM_ERR            = in->PM_ERR;
    out->PM_ERR_NONSOP     = in->PM_ERR_NONSOP;
    out->QOS_L3_DSCP       = in->QOS_L3_DSCP;
    out->RX_DATA           = in->RX_DATA;
    out->SAF_ERROR         = in->SAF_ERROR;
    out->TAIL_CSUM_LEN     = in->TAIL_CSUM_LEN;
    out->TX_DROP           = in->TX_DROP;
    out->TX_LENGTH         = in->TX_LENGTH;
    out->TX_TAG            = in->TX_TAG;
    out->XCAST             = in->XCAST;
}
