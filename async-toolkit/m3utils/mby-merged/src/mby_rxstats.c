// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_rxstats.h"
#include "mby_maskgen.h" // action codes

static void handleTail
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyPolicerToRxStats  * const in,
          mbyRxStatsToModifier * const out
)
{
    // Read inputs:
    fm_uint32 rx_length    = in->RX_LENGTH;
    fm_uint32 rx_port      = in->RX_PORT;
    fm_uint64 fnmask       = in->FNMASK;
    fm_bool   seg_meta_err = in->SEG_META_ERR;

    // remove FCS bytes from array storage:
    fm_uint32 pkt_len  = (rx_length < 4) ? 0 : (rx_length - 4);
    fm_uint32 num_segs = 0;

    // First segment is 192 bytes
    if (pkt_len <= 192) {
        pkt_len   = 0;
        num_segs  = 1;
    } else {    
        pkt_len  -= 192;
        num_segs += 1 + (pkt_len + MBY_SEGMENT_LEN - 1) / MBY_SEGMENT_LEN; // check <-- REVISIT!!!
    }
    
    fm_uint32 saf_matrix_vals[MBY_SAF_MATRIX_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_SAF_MATRIX(rx_port, 0), MBY_SAF_MATRIX_WIDTH, saf_matrix_vals);

    fm_uint64 enable_snf    = FM_ARRAY_GET_FIELD64(saf_matrix_vals, MBY_SAF_MATRIX, ENABLE_SNF);
    fm_uint32 cut_thru_mode = FM_ARRAY_GET_FIELD  (saf_matrix_vals, MBY_SAF_MATRIX, CUT_THRU_MODE);
    fm_bool   ign_frame_err = FM_ARRAY_GET_BIT    (saf_matrix_vals, MBY_SAF_MATRIX, IGNORE_FRAME_ERROR);
    
    fm_uint64 dmask = fnmask;

#if 0 // should this code be in MaskGen rather than here? <-- REVISIT!!!!

    if (state->MIRROR1_PROFILE_V)
        dmask |= (FM_LITERAL_U64(1) << state->MIRROR1_PORT);
    if(state->MIRROR0_PROFILE_V)
        dmask |= (FM_LITERAL_U64(1) << state->MIRROR0_PORT);
    dmask &= ( 0xFFFFFFFFFFFF ); // 48 bit port mask
    enable_snf = (enable_snf & dmask) & 0xFFFFFF;
    
    // Decide whether frame is SAF:
    fm_bool is_saf = FALSE;
    if (enable_snf)
        is_saf = TRUE;
    else if ((cut_thru_mode == 0) || (cut_thru_mode == 1))
        is_saf = (num_segs == 1);
    else if (cut_thru_mode == 2)
        is_saf = (num_segs <= 2);
    else
        is_saf = TRUE; // end-of-frame

    // SAF ERROR in tail processing:
    fm_bool saf_error = FALSE;
    if ((seg_meta_err == 1) || (seg_meta_err == 2))
    {
        if (is_saf || (dmask == 0)) {
            saf_error = !ign_frame_err;
            if (!enable_snf && (cut_thru_mode == 0) && (dmask != 0))
                saf_error = FALSE;
        }    
    }
    
    if (saf_error)
    {
        if ((num_segs == 1) || (dmask == 0))
        {    
            out->ACTION            = MBY_ACTION_DROP_FRAME_ERR;
            out->FNMASK            = 0;
            out->MIRROR1_PROFILE_V = 0;
            out->MIRROR0_PROFILE_V = 0;
        }
        else // multi-segment
        {
            out->TX_DROP = 1;
        }
    }
#endif
    
#if 0
    // Update Action code for CSUM and L3 Length errors
    // Applies to only single segment packets. Multi-segment packets are handled by Modify
    if (rx_length <= 192)
    { 
        if (in->ACTION == MBY_ACTION_NORMAL ||
            in->ACTION == MBY_ACTION_FLOOD ||
            in->ACTION == MBY_ACTION_GLORT_FORWARDED ||
            in->ACTION == MBY_ACTION_TRAP ||
            in->ACTION == MBY_ACTION_SPECIAL ||
            in->ACTION == MBY_ACTION_REDIRECT_TRIG ||
            in->ACTION == MBY_ACTION_DROP_CONTROL ||
            in->ACTION == MBY_ACTION_DROP_IV ||
            in->ACTION == MBY_ACTION_DROP_EV ||
            in->ACTION == MBY_ACTION_DROP_STP ||
            in->ACTION == MBY_ACTION_DROP_CAM ||
            in->ACTION == MBY_ACTION_DROP_FFU ||
            in->ACTION == MBY_ACTION_DROP_TRIG ||
            in->ACTION == MBY_ACTION_DROP_TTL ||
            in->ACTION == MBY_ACTION_DROP_DLF ||
            in->ACTION == MBY_ACTION_BANK5_OTHER_DROPS ||
            in->ACTION == MBY_ACTION_DROP_SV)
        {    
            if (in->PA_DROP && in->PA_L3LEN_ERR)
                out->ACTION = MBY_ACTION_DROP_L3_PYLD_LEN;
            else if (in->PA_DROP && in->PA_CSUM_ERR)
                out->ACTION = MBY_ACTION_DROP_L4_CSUM;
        }

        // Drop single-segment packets with l4csum error /l3 length error:
        if ((out->ACTION == MBY_ACTION_DROP_L3_PYLD_LEN) || (out->ACTION == MBY_ACTION_DROP_L4_CSUM))
        {
            out->FNMASK = 0;
            out->MIRROR1_PROFILE_V = 0;
            out->MIRROR0_PROFILE_V = 0;
        }
    } 
    else if (in->PA_DROP && (in->PA_L3LEN_ERR || in->PA_CSUM_ERR))
        out->SEG_META_ERR = 2; // framing error in multi-segment packet

    // Write outputs:
    out->SAF_ERROR = saf_error;
    
    // clear parser_info for window parsing
    if (out->PARSER_INFO.window_parse_v)
        FM_CLEAR(state->PARSER_INFO);
#endif
}

static fm_uint16 getBankIndex(const fm_uint32 rx_port)
{
    fm_uint16 index = 0;
    if (rx_port < MBY_FABRIC_LOG_PORTS)
        index = ((rx_port << 4) & 0x03F0);
    return index;
}

static fm_uint64 incrCounter(const fm_uint64 cnt_in, const fm_uint64 inc)
{
    const fm_uint64 cnt_max = FM_LITERAL_U64(0xFFFFFFFFFFFF); // 48-bit counter
    const fm_uint64 delta   = cnt_max - cnt_in; // delta between current and maximum counter values
    const fm_uint64 cnt_wrp = inc - (FM_LITERAL_U64(1) + delta); // safely wrap around
    const fm_uint64 cnt_inc = cnt_in + inc;
    const fm_uint64 cnt_out = (inc > delta) ? cnt_wrp : cnt_in;  // wrap around or increment

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
    fm_uint32 rx_stats_bank_frame_vals[MBY_RX_STATS_BANK_FRAME_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_RX_STATS_BANK_FRAME(bank, index, 0), MBY_RX_STATS_BANK_FRAME_WIDTH, rx_stats_bank_frame_vals);
    fm_uint64 frame_cnt = FM_ARRAY_GET_FIELD64(rx_stats_bank_frame_vals, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER);
    fm_uint64 one       = FM_LITERAL_U64(1);
    frame_cnt = incrCounter(frame_cnt, one);
    FM_ARRAY_SET_FIELD64(rx_stats_bank_frame_vals, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER, frame_cnt);

    // Update (read/modify/write) byte count:
    fm_uint32 rx_stats_bank_byte_vals[MBY_RX_STATS_BANK_BYTE_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_RX_STATS_BANK_BYTE(bank, index, 0), MBY_RX_STATS_BANK_BYTE_WIDTH, rx_stats_bank_byte_vals);
    fm_uint64 byte_cnt = FM_ARRAY_GET_FIELD64(rx_stats_bank_byte_vals, MBY_RX_STATS_BANK_BYTE, BYTE_COUNTER);
    byte_cnt = incrCounter(byte_cnt, len);
    FM_ARRAY_SET_FIELD64(rx_stats_bank_byte_vals, MBY_RX_STATS_BANK_BYTE, BYTE_COUNTER, byte_cnt);
}

static void updateRxStatsVlan
(
    fm_uint32       regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint16 index,
    const fm_uint64 len
)
{
    // Update (read/modify/write) frame count:
    fm_uint32 rx_stats_vlan_frame_vals[MBY_RX_STATS_VLAN_FRAME_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_RX_STATS_VLAN_FRAME(index, 0), MBY_RX_STATS_VLAN_FRAME_WIDTH, rx_stats_vlan_frame_vals);
    fm_uint64 frame_cnt = FM_ARRAY_GET_FIELD64(rx_stats_vlan_frame_vals, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER);
    fm_uint64 one       = FM_LITERAL_U64(1);
    frame_cnt = incrCounter(frame_cnt, one);
    FM_ARRAY_SET_FIELD64(rx_stats_vlan_frame_vals, MBY_RX_STATS_VLAN_FRAME, FRAME_COUNTER, frame_cnt);

    // Update (read/modify/write) byte count:
    fm_uint32 rx_stats_vlan_byte_vals[MBY_RX_STATS_VLAN_BYTE_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_RX_STATS_VLAN_BYTE(index, 0), MBY_RX_STATS_VLAN_BYTE_WIDTH, rx_stats_vlan_byte_vals);
    fm_uint64 byte_cnt = FM_ARRAY_GET_FIELD64(rx_stats_vlan_byte_vals, MBY_RX_STATS_VLAN_BYTE, BYTE_COUNTER);
    byte_cnt = incrCounter(byte_cnt, len);
    FM_ARRAY_SET_FIELD64(rx_stats_vlan_byte_vals, MBY_RX_STATS_VLAN_BYTE, BYTE_COUNTER, byte_cnt);
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
    const fm_byte   tc // traffic class
)
{
    fm_uint32 bank  = 1;
    fm_uint16 index = getBankIndex(rx_port);
    fm_uint64 len   = rx_length;

    index += (tc & 0x7);
    
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
    const fm_uint16 l2_ivlan1_cnt_index,
    const fm_bool   is_bcast,
    const fm_bool   is_mcast,
    const fm_bool   is_ucast
)
{
    if (l2_ivlan1_cnt_index != 0)
    {
        fm_bool is_drop = !(action == MBY_ACTION_NORMAL ||
                            action == MBY_ACTION_FLOOD ||
                            action == MBY_ACTION_SPECIAL ||
                            action == MBY_ACTION_TRAP ||
                            action == MBY_ACTION_REDIRECT_TRIG ||
                            action == MBY_ACTION_GLORT_FORWARDED);

        fm_uint16 l2_type = (is_drop) ? 3 : (is_ucast) ? 0 : (is_mcast) ? 1 : (is_bcast) ? 2 : 0;
        fm_uint16 index   = ((l2_ivlan1_cnt_index << 2) | l2_type ) & 0x3FFF;
        fm_uint64 len     = rx_length;

        updateRxStatsVlan(regs, index, len);
    }
}

void RxStats
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyPolicerToRxStats  * const in,
          mbyRxStatsToModifier * const out
)
{
    // Read inputs:
    fm_bool    state_chg           = in->allowStateChange;
    fm_uint32  rx_length           = in->RX_LENGTH;
    fm_uint32  rx_port             = in->RX_PORT;
    fm_bool    is_ipv4             = in->IS_IPV4;
    fm_bool    is_ipv6             = in->IS_IPV6;
    fm_macaddr l2_dmac             = in->L2_DMAC;
    fm_uint16  l2_ivlan1_cnt_index = in->L2_IVLAN1_CNT_INDEX;
    fm_byte    tc                  = in->TC;      // = traffic class
    fm_uint    action              = in->ACTION;

    fm_bool is_bcast = isBroadcastMacAddress(l2_dmac);
    fm_bool is_mcast = isMulticastMacAddress(l2_dmac);
    fm_bool is_ucast =   isUnicastMacAddress(l2_dmac);    

    if (state_chg)
    {
        // Handle tail
        handleTail(regs, in, out); // code that likely does not belong to RxStats <-- REVISIT!!!!

        // Handle RX frame classification:
        handleRxBank0(regs, rx_length, rx_port, is_ipv4, is_ipv6, is_bcast, is_mcast, is_ucast);

        // Handle per-port RX TC counters:
        handleRxBank1(regs, rx_length, rx_port, tc);
    
        // Perform RX forwarding action:
        fm_bool drop_act = ((action & 0x10) != 0);
        if (drop_act)
            handleRxBank3(regs, rx_length, rx_port, action);  // drop actions
        else
            handleRxBank2(regs, rx_length, rx_port, action);

        // Handle RX VLAN counters:
        handleRxBankVlan(regs, rx_length, action, l2_ivlan1_cnt_index, is_bcast, is_mcast, is_ucast);
    }
}
