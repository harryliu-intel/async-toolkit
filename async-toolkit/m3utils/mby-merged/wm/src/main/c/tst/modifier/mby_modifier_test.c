#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <uint.h>

#include <mby_modifier_test.h>
#include <mby_modifier.h>

#include <mby_top_map.h>
#include <model_c_write.h> // write_field()

#include <mby_pipeline.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"


#define SIMPLE_MODIFIER_TEST(name, fails) {if (!run_on_simple_modifier(simple_modifier_ ## name ## _test_setup, \
                simple_modifier_ ## name ## _test_check)) pass(#name); else {++fails; fail(#name);} }

typedef void(*run_on_simple_modifier_setup_fn)
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    mbyTxInToModifier  * const tx_in
);

typedef int(*run_on_simple_modifier_check_fn)
(
    mbyModifierToTxStats const * const modif_out
);

static void pass(const char* name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
}

static void fail(const char* name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
}

static void allocMem
(
    mby_ppe_modify_map **mod_map,
    mby_shm_map        **shm_map,
    fm_byte            **tx_data
)
{
    *mod_map = calloc(1, sizeof(mby_ppe_modify_map));
    if (*mod_map == NULL)
    {
        printf("Could not allocate memory for mod_map -- exiting!\n");
        exit(-1);
    }

    *shm_map = calloc(1, sizeof(mby_shm_map));
    if (*shm_map == NULL)
    {
        printf("Could not allocate memory for shm_map -- exiting!\n");
        exit(-1);
    }

    *tx_data = calloc(MBY_MAX_PACKET_SIZE, sizeof(fm_byte));
    if (*tx_data == NULL)
    {
        printf("Could not allocate memory for tx_data -- exiting!\n");
        exit(-1);
    }
}

static void freeMem
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    fm_byte            * const tx_data
)
{
    free(mod_map);
    free(shm_map);
    free(tx_data);
}

static void simple_modifier_no_operation_test_setup
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    mbyTxInToModifier  * const tx_in
)
{
    fm_byte test_no  = 0;
    tx_in->RX_DATA   = modifier_tests[test_no].rx_data;
    tx_in->RX_LENGTH = modifier_tests[test_no].rx_len;
}

static int simple_modifier_no_operation_test_check
(
    mbyModifierToTxStats const * const modif_out
)
{
    fm_byte test_no = 0;

    if (modif_out->TX_LENGTH != modifier_tests[test_no].tx_len)
    {
        printf("TX packet length is invalid\n");
        return 1;
    }

    for (fm_uint32 i = 0; i < modif_out->TX_LENGTH; i++)
    {
        if (modif_out->TX_DATA[i] != modifier_tests[test_no].tx_data[i])
        {
            printf("TX packet is invalid\n");
            return 1;
        }
    }

    return 0;
}

static void simple_modifier_insert_vlan_test_setup
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    mbyTxInToModifier  * const tx_in
)
{
    fm_byte test_no        = 1;
    fm_uint16 mod_prof_idx = 1;
    fm_byte hdr_ptr_idx    = 1;
    fm_byte prot_id        = 6;
    fm_byte offset         = 12;
    fm_uint32 content_addr = 0x0;

    for (fm_int i = 0; i < MBY_N_PARSER_PTRS; i++)
        tx_in->PA_HDR_PTRS.PROT_ID[i] = MBY_PA_PROT_ID_NOP;

    tx_in->RX_DATA                          = modifier_tests[test_no].rx_data;
    tx_in->RX_LENGTH                        = modifier_tests[test_no].rx_len;
    tx_in->MOD_PROF_IDX                     = mod_prof_idx;
    tx_in->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx] = prot_id;
    tx_in->PA_HDR_PTRS.OFFSET [hdr_ptr_idx] = offset;
    tx_in->CONTENT_ADDR                     = content_addr;

    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_1 = prot_id;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_2 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_3 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_4 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_5 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_6 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_7 = MBY_PA_PROT_ID_NOP;

    /* Command for group 1:
     * TYPE        = INSERT
     * LEN         = 4B
     * PROTOCOL_ID = 5
     * S           = MBY_MOD_CMD_SOURCE_CONTENT_REGION
     * M           = FALSE
     * MODE        = BASIC
     */
    fm_uint32 cmd     = 4;
    fm_byte prot_id_2 = 5;

    cmd = cmd | (prot_id_2 << 8);
    cmd = cmd | (MBY_MOD_CMD_SOURCE_CONTENT_REGION << 16);
    cmd = cmd | (0 << 17);
    cmd = cmd | (MBY_MOD_CMD_MODE_BASIC << 18);
    cmd = cmd | (MBY_MOD_CMD_TYPE_INSERT << 21);

    mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][2].CMD_0 = cmd;

    /* Set content data. */
    fm_uint idx_1       = (content_addr * (MBY_MOD_CONTENT_ADDR_BLOCK_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE)) /
                          fwd_table_mod_rf_FWD_TABLE_MOD__nd;
    fm_uint idx_2       = (content_addr * (MBY_MOD_CONTENT_ADDR_BLOCK_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE)) %
                          fwd_table_mod_rf_FWD_TABLE_MOD__nd;

    shm_map->FWD_TABLE_MOD[idx_1][idx_2].DATA = 0xef200081;
}

static int simple_modifier_insert_vlan_test_check
(
    mbyModifierToTxStats const * const modif_out
)
{
    fm_byte test_no = 1;

    if (modif_out->TX_LENGTH != modifier_tests[test_no].tx_len)
    {
        printf("TX packet length is invalid\n");
        return 1;
    }

    for (fm_uint32 i = 0; i < modif_out->TX_LENGTH; i++)
    {
        if (modif_out->TX_DATA[i] != modifier_tests[test_no].tx_data[i])
        {
            printf("TX packet is invalid\n");
            return 1;
        }
    }

    fm_byte hdr_ptr_idx_1 = 1;
    fm_byte prot_id_1     = 5;
    fm_byte offset_1      = 12;
    fm_byte hdr_ptr_idx_2 = 2;
    fm_byte prot_id_2     = 6;
    fm_byte offset_2      = 16;
    if ((modif_out->PA_HDR_PTRS.OFFSET [hdr_ptr_idx_1] != offset_1)  ||
        (modif_out->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx_1] != prot_id_1) ||
        (modif_out->PA_HDR_PTRS.OFFSET [hdr_ptr_idx_2] != offset_2)  ||
        (modif_out->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx_2] != prot_id_2))
    {
        printf("Parser header pointers are invalid\n");
        return 1;
    }

    return 0;
}

static void simple_modifier_zero_ttl_test_setup
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    mbyTxInToModifier  * const tx_in
)
{
    fm_byte test_no        = 2;
    fm_uint16 mod_prof_idx = 2;
    fm_byte hdr_ptr_idx    = 0;
    fm_byte hdr_ptr_idx_2  = 1;
    fm_byte prot_id        = 3;
    fm_byte prot_id_2      = 4;
    fm_byte offset         = 12;
    fm_byte offset_2       = 14;

    tx_in->RX_DATA                            = modifier_tests[test_no].rx_data;
    tx_in->RX_LENGTH                          = modifier_tests[test_no].rx_len;
    tx_in->MOD_PROF_IDX                       = mod_prof_idx;
    tx_in->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx]   = prot_id;
    tx_in->PA_HDR_PTRS.OFFSET [hdr_ptr_idx]   = offset;
    tx_in->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx_2] = prot_id_2;
    tx_in->PA_HDR_PTRS.OFFSET [hdr_ptr_idx_2] = offset_2;

    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_1 = prot_id;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_2 = prot_id_2;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_3 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_4 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_5 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_6 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_7 = MBY_PA_PROT_ID_NOP;

    /* Command for group 2:
     * TYPE        = REPLACE
     * LEN         = 1B
     * OFFSET      = 8
     * A           = MBY_MOD_CMD_ALIGN_TOP
     * MODE        = ZEROES
     */
    fm_uint32 cmd = 1;

    cmd = cmd | (8 << 8);
    cmd = cmd | (MBY_MOD_CMD_ALIGN_TOP << 14);
    cmd = cmd | (MBY_MOD_CMD_MODE_ZEROES << 18);
    cmd = cmd | (MBY_MOD_CMD_TYPE_REPLACE << 21);

    mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][5].CMD_0 = cmd;
}

static int simple_modifier_zero_ttl_test_check
(
    mbyModifierToTxStats const * const modif_out
)
{
    fm_byte test_no = 2;

    if (modif_out->TX_LENGTH != modifier_tests[test_no].tx_len)
    {
        printf("TX packet length is invalid\n");
        return 1;
    }

    for (fm_uint32 i = 0; i < modif_out->TX_LENGTH; i++)
    {
        if (modif_out->TX_DATA[i] != modifier_tests[test_no].tx_data[i])
        {
            printf("TX packet is invalid\n");
            return 1;
        }
    }

    return 0;
}

static void simple_modifier_delete_vlan_test_setup
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    mbyTxInToModifier  * const tx_in
)
{
    fm_byte test_no        = 3;
    fm_uint16 mod_prof_idx = 2;
    fm_byte hdr_ptr_idx    = 0;
    fm_byte hdr_ptr_idx_2  = 1;
    fm_byte prot_id        = 3;
    fm_byte prot_id_2      = 4;
    fm_byte offset         = 12;
    fm_byte offset_2       = 16;

    tx_in->RX_DATA                            = modifier_tests[test_no].rx_data;
    tx_in->RX_LENGTH                          = modifier_tests[test_no].rx_len;
    tx_in->MOD_PROF_IDX                       = mod_prof_idx;
    tx_in->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx]   = prot_id;
    tx_in->PA_HDR_PTRS.OFFSET [hdr_ptr_idx]   = offset;
    tx_in->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx_2] = prot_id_2;
    tx_in->PA_HDR_PTRS.OFFSET [hdr_ptr_idx_2] = offset_2;

    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_1 = prot_id;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_2 = prot_id_2;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_3 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_4 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_5 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_6 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_7 = MBY_PA_PROT_ID_NOP;

    /* Command for group 1:
     * TYPE = DELETE
     * P    = TRUE
     */
    fm_uint32 cmd = 0;

    cmd = cmd | (1 << 18);
    cmd = cmd | (MBY_MOD_CMD_TYPE_DELETE << 21);

    mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][2].CMD_1 = cmd;
}

static int simple_modifier_delete_vlan_test_check
(
    mbyModifierToTxStats const * const modif_out
)
{
    fm_byte test_no = 3;

    if (modif_out->TX_LENGTH != modifier_tests[test_no].tx_len)
    {
        printf("TX packet length is invalid\n");
        return 1;
    }

    for (fm_uint32 i = 0; i < modif_out->TX_LENGTH; i++)
    {
        if (modif_out->TX_DATA[i] != modifier_tests[test_no].tx_data[i])
        {
            printf("TX packet is invalid\n");
            return 1;
        }
    }

    fm_byte hdr_ptr_idx = 0;
    fm_byte prot_id     = 4;
    fm_byte offset      = 12;
    if ((modif_out->PA_HDR_PTRS.OFFSET[hdr_ptr_idx]  != offset) ||
        (modif_out->PA_HDR_PTRS.PROT_ID[hdr_ptr_idx] != prot_id))
    {
        printf("Parser header pointers are invalid\n");
        return 1;
    }

    return 0;
}

static void simple_modifier_vxlan_encap_test_setup
(
    mby_ppe_modify_map * const mod_map,
    mby_shm_map        * const shm_map,
    mbyTxInToModifier  * const tx_in
)
{
    fm_byte test_no        = 4;
    fm_uint16 mod_prof_idx = 1;
    fm_uint32 content_addr = 0x0;

    for (fm_int i = 0; i < MBY_N_PARSER_PTRS; i++)
        tx_in->PA_HDR_PTRS.PROT_ID[i] = MBY_PA_PROT_ID_NOP;

    tx_in->PA_HDR_PTRS.PROT_ID[0] = 3;
    tx_in->PA_HDR_PTRS.OFFSET [0] = 0;
    tx_in->PA_HDR_PTRS.PROT_ID[1] = 4;
    tx_in->PA_HDR_PTRS.OFFSET [1] = 0xe;
    tx_in->PA_HDR_PTRS.PROT_ID[2] = 5;
    tx_in->PA_HDR_PTRS.OFFSET [2] = 0x22;

    tx_in->RX_DATA                          = modifier_tests[test_no].rx_data;
    tx_in->RX_LENGTH                        = modifier_tests[test_no].rx_len;
    tx_in->MOD_PROF_IDX                     = mod_prof_idx;
    tx_in->CONTENT_ADDR                     = content_addr;

    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_1 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_2 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_3 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_4 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_5 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_6 = MBY_PA_PROT_ID_NOP;
    mod_map->MOD_PROFILE_GROUP[mod_prof_idx].GROUP_7 = MBY_PA_PROT_ID_NOP;

    /* Command for group 1:
     * TYPE        = INSERT
     * LEN         = 50B
     * PROTOCOL_ID = 3
     * S           = MBY_MOD_CMD_SOURCE_CONTENT_REGION
     * M           = TRUE
     * MODE        = BASIC
     */
    fm_uint32 cmd   = 50;
    fm_byte prot_id = 3;

    cmd = cmd | (prot_id << 8);
    cmd = cmd | (MBY_MOD_CMD_SOURCE_CONTENT_REGION << 16);
    cmd = cmd | (1 << 17);
    cmd = cmd | (MBY_MOD_CMD_MODE_BASIC << 18);
    cmd = cmd | (MBY_MOD_CMD_TYPE_INSERT << 21);

    mod_map->MOD_PROFILE_COMMAND[mod_prof_idx][0].CMD_0 = cmd;

    /* Set content data. */
    fm_uint idx_1       = (content_addr * (MBY_MOD_CONTENT_ADDR_BLOCK_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE)) /
                          fwd_table_mod_rf_FWD_TABLE_MOD__nd;
    fm_uint idx_2       = (content_addr * (MBY_MOD_CONTENT_ADDR_BLOCK_SIZE / MBY_MOD_CONTENT_ENTRY_SIZE)) %
                          fwd_table_mod_rf_FWD_TABLE_MOD__nd;

    /* 3 x (protocol ID, Offset) pairs (first 6B) + data (last 2B). */
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0x000822020e010000;
    /* Rest of data (48B). */
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0xae2700088c1df227;
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0x860000450008624d;
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0x656f1140004099d9;
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0x0c38a8c00b38a8c0;
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0x00007200b51206bc;
    shm_map->FWD_TABLE_MOD[idx_1][idx_2++].DATA = 0x007b000000000008;
}

static int simple_modifier_vxlan_encap_test_check
(
    mbyModifierToTxStats const * const modif_out
)
{
    fm_byte test_no = 4;

    if (modif_out->TX_LENGTH != modifier_tests[test_no].tx_len)
    {
        printf("TX packet length is invalid\n");
        return 1;
    }

    for (fm_uint32 i = 0; i < modif_out->TX_LENGTH; i++)
    {
        if (modif_out->TX_DATA[i] != modifier_tests[test_no].tx_data[i])
        {
            printf("TX packet is invalid\n");
            return 1;
        }
    }

    if ((modif_out->PA_HDR_PTRS.PROT_ID[0] != 0) || (modif_out->PA_HDR_PTRS.OFFSET [0] != 0)  ||
        (modif_out->PA_HDR_PTRS.PROT_ID[1] != 1) || (modif_out->PA_HDR_PTRS.OFFSET [1] != 14) ||
        (modif_out->PA_HDR_PTRS.PROT_ID[2] != 2) || (modif_out->PA_HDR_PTRS.OFFSET [2] != 34) ||
        (modif_out->PA_HDR_PTRS.PROT_ID[3] != 3) || (modif_out->PA_HDR_PTRS.OFFSET [3] != 50) ||
        (modif_out->PA_HDR_PTRS.PROT_ID[4] != 4) || (modif_out->PA_HDR_PTRS.OFFSET [4] != 64) ||
        (modif_out->PA_HDR_PTRS.PROT_ID[5] != 5) || (modif_out->PA_HDR_PTRS.OFFSET [5] != 84))
    {
        printf("Parser header pointers are invalid\n");
        return 1;
    }

    return 0;
}

static int run_on_simple_modifier
(
    run_on_simple_modifier_setup_fn setup,
    run_on_simple_modifier_check_fn check
)
{
    mby_ppe_modify_map *mod_map   = NULL;
    mby_shm_map        *shm_map   = NULL;
    fm_byte            *tx_data   = NULL;

    allocMem(&mod_map, &shm_map, &tx_data);

    mbyTxInToModifier    modif_in     = { 0 };
    mbyModifierToTxStats modif_out    = { 0 };
    fm_int               max_pkt_size = 0;

    setup(mod_map, shm_map, &modif_in);

    modif_out.TX_DATA = tx_data;

    Modifier
    (
        mod_map,
        shm_map,
        &modif_in,
        &modif_out,
        MAX_MOD_TEST_PKT_LEN
    );

    int ret = check(&modif_out);

    // Free up memory:
    freeMem(mod_map, shm_map, tx_data);

  return ret;
}


int main(void)
{
    printf("--------------------------------------------------------------------------------\n");

    fm_uint tests = 0;
    fm_uint fails = 0;

    SIMPLE_MODIFIER_TEST(no_operation, fails); tests++;
    SIMPLE_MODIFIER_TEST(insert_vlan, fails); tests++;
    SIMPLE_MODIFIER_TEST(zero_ttl, fails); tests++;
    SIMPLE_MODIFIER_TEST(delete_vlan, fails); tests++;
    SIMPLE_MODIFIER_TEST(vxlan_encap, fails); tests++;

    fm_uint passes = (tests > fails) ? tests - fails : 0;

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - Modifier tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;

    return rv;
}
