// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_modifier.h"
#include "mby_txstats.h"

static fm_uint64 incrTxCounter(const fm_uint64 cnt_in, const fm_uint64 inc)
{
    const fm_uint64 cnt_max = FM_LITERAL_U64(0xFFFFFFFFFFFF); // 48-bit counter
    const fm_uint64 delta   = cnt_max - cnt_in; // delta between current and maximum counter values
    const fm_uint64 cnt_wrp = inc - (FM_LITERAL_U64(1) + delta); // safely wrap around
    const fm_uint64 cnt_inc = cnt_in + inc;
    const fm_uint64 cnt_out = (inc > delta) ? cnt_wrp : cnt_in;  // wrap around or increment

    return cnt_out;
}

static void updateTxStatsBank
(
    fm_uint32       regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint64 len,
    const fm_uint16 index
)
{
    // Update (read/modify/write) frame count:
    fm_uint32 mod_stats_bank_frame_vals[MBY_MOD_STATS_BANK_FRAME_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_MOD_STATS_BANK_FRAME(index, 0), MBY_MOD_STATS_BANK_FRAME_WIDTH, mod_stats_bank_frame_vals);
    fm_uint64 frame_cnt = FM_ARRAY_GET_FIELD64(mod_stats_bank_frame_vals, MBY_MOD_STATS_BANK_FRAME, FRAME_COUNTER);
    fm_uint64 one       = FM_LITERAL_U64(1);
    frame_cnt = incrTxCounter(frame_cnt, one);
    FM_ARRAY_SET_FIELD64(mod_stats_bank_frame_vals, MBY_MOD_STATS_BANK_FRAME, FRAME_COUNTER, frame_cnt);
    mbyModelWriteCSRMult(regs, MBY_MOD_STATS_BANK_FRAME(index, 0), MBY_MOD_STATS_BANK_FRAME_WIDTH, mod_stats_bank_frame_vals);

    // Update (read/modify/write) byte count:
    fm_uint32 mod_stats_bank_byte_vals[MBY_MOD_STATS_BANK_BYTE_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_MOD_STATS_BANK_BYTE(index, 0), MBY_MOD_STATS_BANK_BYTE_WIDTH, mod_stats_bank_byte_vals);
    fm_uint64 byte_cnt = FM_ARRAY_GET_FIELD64(mod_stats_bank_byte_vals, MBY_MOD_STATS_BANK_BYTE, BYTE_COUNTER);
    byte_cnt = incrTxCounter(byte_cnt, len);
    FM_ARRAY_SET_FIELD64(mod_stats_bank_byte_vals, MBY_MOD_STATS_BANK_BYTE, BYTE_COUNTER, byte_cnt);
    mbyModelWriteCSRMult(regs, MBY_MOD_STATS_BANK_BYTE(index, 0), MBY_MOD_STATS_BANK_BYTE_WIDTH, mod_stats_bank_byte_vals);
}

void TxStats
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyModifierToTxStats  * const in,
          mbyTxStatsToTxOut     * const out
)
{
    // Read inputos:
    fm_uint32 tx_port         = in->TX_PORT;
    fm_uint16 tx_disp         = in->TX_DISP;
    fm_uint32 tx_stats_length = in->TX_STATS_LENGTH;

    fm_uint16 index   = (tx_port * 16) + tx_disp;

    updateTxStatsBank(regs, tx_stats_length, index);
}
