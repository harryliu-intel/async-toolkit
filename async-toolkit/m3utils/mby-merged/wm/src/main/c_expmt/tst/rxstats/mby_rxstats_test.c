#include <stdlib.h>
#include <stdio.h>

#include <mby_rxstats_test.h>
#include <mby_pipeline.h>
#include <mby_reg_ctrl.h>

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

fm_uint tests = 0;
fm_uint fails = 0;
fm_uint passes = 0;

void test_pass(const char * name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
    passes++;
    tests++;
}

void test_fail(const char * name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
    fails++;
    tests++;
}

static void rxstats_test_setup
(
    mbyCongMgmtToRxStats     * const congMgmtToRxStats,
    mby_rxstats_test_data_in * const test_in
)
{
    congMgmtToRxStats->RX_LENGTH     = test_in->rx_length;
    congMgmtToRxStats->RX_PORT       = test_in->rx_port;
    congMgmtToRxStats->IS_IPV4       = test_in->is_ipv4;
    congMgmtToRxStats->IS_IPV6       = test_in->is_ipv6;
    congMgmtToRxStats->L2_DMAC       = test_in->l2_dmac;
    congMgmtToRxStats->L2_IVLAN1_CNT = test_in->l2_ivlan1_cnt;
    congMgmtToRxStats->TRAFFIC_CLASS = test_in->traffic_class;
    congMgmtToRxStats->ACTION        = test_in->action;
}

static fm_bool rxstats_test_verify_bank
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_stats_map      * const stats_map,
#else
    fm_uint32 *                       regs,
#endif
    fm_uint32 bank,
    fm_uint16 index,
    fm_uint32 rx_length,
    fm_uint64 exp_val
)
{
#ifdef USE_NEW_CSRS
    rx_stats_bank_frame_r * const bank_frame = &(stats_map->RX_STATS_BANK_FRAME[bank][index]);
    fm_uint64 frame_cnt = bank_frame->FRAME_COUNTER;

    if (frame_cnt != exp_val)
        return FALSE;

    rx_stats_bank_byte_r * const bank_byte = &(stats_map->RX_STATS_BANK_BYTE[bank][index]);
    fm_uint64 byte_cnt = bank_byte->BYTE_COUNTER;

    if (byte_cnt != (exp_val * rx_length))
        return FALSE;
#else
    fm_uint64 rx_stats_bank_frame_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_BANK_FRAME(bank, index, 0), &rx_stats_bank_frame_reg);
    fm_uint64 frame_cnt = FM_GET_FIELD64(rx_stats_bank_frame_reg, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER);

    if (frame_cnt != exp_val)
        return FALSE;

    fm_uint64 rx_stats_bank_byte_reg  = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_BANK_BYTE(bank, index, 0), &rx_stats_bank_byte_reg);
    fm_uint64 byte_cnt = FM_GET_FIELD64(rx_stats_bank_byte_reg, MBY_RX_STATS_BANK_BYTE, BYTE_COUNTER);

    if (byte_cnt != (exp_val * rx_length))
        return FALSE;
#endif

    return TRUE;
}

static fm_bool rxstats_test_verify_vlan
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_stats_map      * const stats_map,
#else
    fm_uint32 *                       regs,
#endif
    fm_uint16 index,
    fm_uint32 rx_length,
    fm_uint64 exp_val
)
{
#ifdef USE_NEW_CSRS
    rx_stats_vlan_frame_r * const vlan_frame = &(stats_map->RX_STATS_VLAN_FRAME[index]);
    fm_uint64 frame_cnt = vlan_frame->FRAME_COUNTER;

    if (frame_cnt != exp_val)
        return FALSE;

    rx_stats_vlan_byte_r * const vlan_byte = &(stats_map->RX_STATS_VLAN_BYTE[index]);
    fm_uint64 byte_cnt = vlan_byte->BYTE_COUNTER;

    if (byte_cnt != (exp_val * rx_length))
        return FALSE;
#else
    fm_uint64 rx_stats_vlan_frame_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_VLAN_FRAME(index, 0), &rx_stats_vlan_frame_reg);
    fm_uint64 frame_cnt = FM_GET_FIELD64(rx_stats_vlan_frame_reg, MBY_RX_STATS_BANK_FRAME, FRAME_COUNTER);

    if (frame_cnt != exp_val)
        return FALSE;

    fm_uint64 rx_stats_vlan_byte_reg = 0;
    mbyModelReadCSR64(regs, MBY_RX_STATS_VLAN_BYTE(index, 0), &rx_stats_vlan_byte_reg);
    fm_uint64 byte_cnt = FM_GET_FIELD64(rx_stats_vlan_byte_reg, MBY_RX_STATS_VLAN_BYTE, BYTE_COUNTER);

    if (byte_cnt != (exp_val * rx_length))
        return FALSE;
#endif

    return TRUE;
}

static fm_bool rxstats_test_verify
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_stats_map      * const stats_map,
#else
    fm_uint32 *                       regs,
#endif
    fm_uint32                         rx_length,
    mby_rxstats_test_data_out * const test_out
)
{
#ifdef USE_NEW_CSRS
    fm_bool pass = TRUE;

    /* Verify bank 0. */
    pass = rxstats_test_verify_bank(stats_map, 0, test_out->index0, rx_length, test_out->exp_val0);
    if (!pass)
        return pass;

    /* Verify bank 1. */
    pass = rxstats_test_verify_bank(stats_map, 1, test_out->index1, rx_length, test_out->exp_val1);
    if (!pass)
        return pass;

    /* Verify bank 2. */
    pass = rxstats_test_verify_bank(stats_map, 2, test_out->index2, rx_length, test_out->exp_val2);
    if (!pass)
        return pass;

    /* Verify bank 3. */
    pass = rxstats_test_verify_bank(stats_map, 3, test_out->index3, rx_length, test_out->exp_val3);
    if (!pass)
        return pass;

#else
    fm_bool pass = TRUE;

    /* Verify bank 0. */
    pass = rxstats_test_verify_bank(regs, 0, test_out->index0, rx_length, test_out->exp_val0);
    if (!pass)
        return pass;

    /* Verify bank 1. */
    pass = rxstats_test_verify_bank(regs, 1, test_out->index1, rx_length, test_out->exp_val1);
    if (!pass)
        return pass;

    /* Verify bank 2. */
    pass = rxstats_test_verify_bank(regs, 2, test_out->index2, rx_length, test_out->exp_val2);
    if (!pass)
        return pass;

    /* Verify bank 3. */
    pass = rxstats_test_verify_bank(regs, 3, test_out->index3, rx_length, test_out->exp_val3);
    if (!pass)
        return pass;

    /* Verify vlan counter. */
    pass = rxstats_test_verify_vlan(regs, test_out->vlan_index, rx_length, test_out->exp_vlan_val);
    if (!pass)
        return pass;
#endif

    return pass;
}

static void rxstats_run_test(rxstats_test_data * const test_data)
{
#ifdef USE_NEW_CSRS
    mby_ppe_rx_stats_map stats_map = { 0 };
#else
    fm_uint32 *regs;
#endif

    mbyCongMgmtToRxStats congMgmtToRxStats = { 0 };
    mbyRxStatsToRxOut    out               = { 0 };

    rxstats_test_setup(&congMgmtToRxStats, &(test_data->in));

#ifdef USE_NEW_CSRS
    RxStats(&stats_map, &congMgmtToRxStats, &out);

    fm_bool pass = rxstats_test_verify(&stats_map, test_data->in.rx_length, &(test_data->out));
#else
    regs = calloc(MBY_REGISTER_ARRAY_SIZE, sizeof(fm_uint32));

    RxStats(regs, &congMgmtToRxStats, &out);

    fm_bool pass = rxstats_test_verify(regs, test_data->in.rx_length, &(test_data->out));

    free(regs);
#endif

    if (pass)
        test_pass(test_data->name);
    else
        test_fail(test_data->name);
}

int main()
{
    printf("--------------------------------------------------------------------------------\n");

    int err;

    fm_uint tests_num = sizeof(rxstats_tests) / sizeof(rxstats_test_data);

    for (fm_uint test_num = 0; test_num < tests_num; test_num++)
        rxstats_run_test(&rxstats_tests[test_num]);

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - Rxstats tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;
    return rv;
}
