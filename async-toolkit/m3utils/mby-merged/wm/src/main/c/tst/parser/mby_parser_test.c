#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mby_top_map.h>

#include <mby_pipeline.h>
#include <mby_crc32.h>
#include <mby_init.h>
#include <mby_reg_ctrl.h>
#include "mby_parser_test.h"

fm_bool load_img = FALSE;

static void initRegs
(
    mby_ppe_rx_top_map * const rx_top_map,
    mby_ppe_tx_top_map * const tx_top_map
)
{
    // Initialize model registers
    mby_init_common_regs
    (
        rx_top_map,
        tx_top_map
    );
}

static void reset_parser_regs(mby_ppe_parser_map * const parser_map)
{
    for (fm_uint i = 0; i < MBY_PA_ANA_STAGES; i++) {
        for (fm_uint j = 0; j < MBY_PA_ANA_RULES; j++) {
            parser_map->PARSER_KEY_W[i][j].W0_VALUE                     = 0xFFFF;
            parser_map->PARSER_KEY_W[i][j].W1_VALUE                     = 0xFFFF;
            parser_map->PARSER_KEY_S[i][j].STATE_VALUE                  = 0xFFFF;
            parser_map->PARSER_EXT[i][j].PROTOCOL_ID                    = MBY_PA_PROT_ID_NOP;
            parser_map->PARSER_EXT[i][j + MBY_PA_ANA_RULES].PROTOCOL_ID = MBY_PA_PROT_ID_NOP;
        }
    }

    for (fm_uint i = 0; i < mby_ppe_parser_map_PARSER_EXTRACT_CFG__nd; i++) {
        for (fm_uint j = 0; j < parser_extract_cfg_rf_PARSER_EXTRACT_CFG__nd; j++) {
            parser_map->PARSER_EXTRACT_CFG[i][j].PROTOCOL_ID = MBY_PA_PROT_ID_NOP;
            parser_map->PARSER_EXTRACT_CFG[i][j].OFFSET      = 0;
        }
    }

    for (fm_uint i = 0; i < mby_ppe_parser_map_PARSER_PTYPE_TCAM__nd; i++) {
        for (fm_uint j = 0; j < parser_ptype_tcam_rf_PARSER_PTYPE_TCAM__nd; j++) {
            parser_map->PARSER_PTYPE_TCAM[i][j].KEY_INVERT = 0xFFFFFFFF;
            parser_map->PARSER_PTYPE_TCAM[i][j].KEY        = 0xFFFFFFFF;
        }
    }
}

static void set_parser_port_cfg
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx,
    fm_uint64                  val
)
{
    parser_port_cfg_r * const port_cfg = &(rx_top_map->parser.PARSER_PORT_CFG[idx]);

    port_cfg->INITIAL_OP_ROT    = FM_GET_UNNAMED_FIELD64(val, 0, 4);
    port_cfg->INITIAL_OP_MASK   = FM_GET_UNNAMED_FIELD64(val, 4, 12);
    port_cfg->INITIAL_STATE     = FM_GET_UNNAMED_FIELD64(val, 16, 16);
    port_cfg->INITIAL_PTR       = FM_GET_UNNAMED_FIELD64(val, 32, 8);
    port_cfg->INITIAL_W2_OFFSET = FM_GET_UNNAMED_FIELD64(val, 40, 8);
    port_cfg->INITIAL_W1_OFFSET = FM_GET_UNNAMED_FIELD64(val, 48, 8);
    port_cfg->INITIAL_W0_OFFSET = FM_GET_UNNAMED_FIELD64(val, 56, 8);
}

static void set_parser_csum_cfg
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx,
    fm_uint64                  val
)
{
    parser_csum_cfg_r * const csum_cfg = &(rx_top_map->parser.PARSER_CSUM_CFG[idx]);

    csum_cfg->VALIDATE_L3_LENGTH           = FM_GET_UNNAMED_FIELD64(val, 0, 2);
    csum_cfg->COMPUTE_L4_CSUM              = FM_GET_UNNAMED_FIELD64(val, 2, 1);
    csum_cfg->STORE_L4_PARTIAL_CSUM        = FM_GET_UNNAMED_FIELD64(val, 3, 1);
    csum_cfg->VALIDATE_L4_CSUM             = FM_GET_UNNAMED_FIELD64(val, 4, 2);
    csum_cfg->VALIDATE_IPV4_LENGTH         = FM_GET_UNNAMED_FIELD64(val, 6, 1);
    csum_cfg->VALIDATE_IPV4_HDR_LENGTH     = FM_GET_UNNAMED_FIELD64(val, 7, 1);
    csum_cfg->VALIDATE_IPV4_HDR_TRUNCATION = FM_GET_UNNAMED_FIELD64(val, 8, 1);
    csum_cfg->VALIDATE_IPV4_HDR_CSUM       = FM_GET_UNNAMED_FIELD64(val, 9, 1);
}

static void set_parser_key_s
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_key_s_r * const key_s = &(rx_top_map->parser.PARSER_KEY_S[idx1][idx2]);

    key_s->STATE_MASK  = FM_GET_UNNAMED_FIELD64(val, 0, 16);
    key_s->STATE_VALUE = FM_GET_UNNAMED_FIELD64(val, 16, 16);
}

static void set_parser_key_w
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_key_w_r * const key_w = &(rx_top_map->parser.PARSER_KEY_W[idx1][idx2]);

    key_w->W0_MASK  = FM_GET_UNNAMED_FIELD64(val, 0, 16);
    key_w->W0_VALUE = FM_GET_UNNAMED_FIELD64(val, 16, 16);
    key_w->W1_MASK  = FM_GET_UNNAMED_FIELD64(val, 32, 16);
    key_w->W1_VALUE = FM_GET_UNNAMED_FIELD64(val, 48, 16);
}

static void set_parser_ana_s
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_ana_s_r * const ana_s = &(rx_top_map->parser.PARSER_ANA_S[idx1][idx2]);

    ana_s->NEXT_OP         = FM_GET_UNNAMED_FIELD64(val, 0, 16);
    ana_s->NEXT_STATE_MASK = FM_GET_UNNAMED_FIELD64(val, 16, 16);
    ana_s->NEXT_STATE      = FM_GET_UNNAMED_FIELD64(val, 32, 16);
}

static void set_parser_ana_w
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_ana_w_r * const ana_w = &(rx_top_map->parser.PARSER_ANA_W[idx1][idx2]);

    ana_w->SKIP           = FM_GET_UNNAMED_FIELD64(val, 0, 8);
    ana_w->NEXT_W2_OFFSET = FM_GET_UNNAMED_FIELD64(val, 8, 8);
    ana_w->NEXT_W1_OFFSET = FM_GET_UNNAMED_FIELD64(val, 16, 8);
    ana_w->NEXT_W0_OFFSET = FM_GET_UNNAMED_FIELD64(val, 24, 8);
}

static void set_parser_exc
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_exc_r * const exc = &(rx_top_map->parser.PARSER_EXC[idx1][idx2]);

    exc->PARSING_DONE = FM_GET_UNNAMED_FIELD64(val, 0, 1);
    exc->EX_OFFSET    = FM_GET_UNNAMED_FIELD64(val, 1, 8);
}

static void set_parser_ext
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_ext_r * const ext = &(rx_top_map->parser.PARSER_EXT[idx1][idx2]);

    ext->PTR_NUM     = FM_GET_UNNAMED_FIELD64(val, 0, 3);
    ext->FLAG_VALUE  = FM_GET_UNNAMED_FIELD64(val, 3, 1);
    ext->FLAG_NUM    = FM_GET_UNNAMED_FIELD64(val, 4, 6);
    ext->OFFSET      = FM_GET_UNNAMED_FIELD64(val, 10, 8);
    ext->PROTOCOL_ID = FM_GET_UNNAMED_FIELD64(val, 18, 8);
}

static void set_parser_extract_cfg
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_extract_cfg_r * const extract_cfg = &(rx_top_map->parser.PARSER_EXTRACT_CFG[idx1][idx2]);

    extract_cfg->PROTOCOL_ID = FM_GET_UNNAMED_FIELD64(val, 0, 8);
    extract_cfg->OFFSET      = FM_GET_UNNAMED_FIELD64(val, 8, 8);
}

static void set_parser_ptype_tcam
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_ptype_tcam_r * const ptype_tcam = &(rx_top_map->parser.PARSER_PTYPE_TCAM[idx1][idx2]);

    ptype_tcam->KEY        = FM_GET_UNNAMED_FIELD64(val, 0, 32);
    ptype_tcam->KEY_INVERT = FM_GET_UNNAMED_FIELD64(val, 32, 32);
}

static void set_parser_ptype_ram
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    parser_ptype_ram_r * const ptype_ram = &(rx_top_map->parser.PARSER_PTYPE_RAM[idx1][idx2]);

    ptype_ram->PTYPE       = FM_GET_UNNAMED_FIELD64(val, 0, 10);
    ptype_ram->EXTRACT_IDX = FM_GET_UNNAMED_FIELD64(val, 10, 4);
}

static void setOneDimReg
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_char            * const reg_name,
    fm_int                     idx,
    fm_uint64                  val
)
{
    if (!strcmp(reg_name, "PARSER_PORT_CFG"))
        set_parser_port_cfg(rx_top_map, idx, val);
    else if (!strcmp(reg_name, "PARSER_CSUM_CFG"))
        set_parser_csum_cfg(rx_top_map, idx, val);
    else
        printf("ERROR: Unrecognized register name: %s\n", reg_name);
}

static void setTwoDimReg
(
    mby_ppe_rx_top_map * const rx_top_map,
    fm_char            * const reg_name,
    fm_int                     idx1,
    fm_int                     idx2,
    fm_uint64                  val
)
{
    if (!strcmp(reg_name, "PARSER_KEY_S"))
        set_parser_key_s(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_KEY_W"))
        set_parser_key_w(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_ANA_S"))
        set_parser_ana_s(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_ANA_W"))
        set_parser_ana_w(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_EXC"))
        set_parser_exc(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_EXT"))
        set_parser_ext(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_EXTRACT_CFG"))
        set_parser_extract_cfg(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_PTYPE_TCAM"))
        set_parser_ptype_tcam(rx_top_map, idx1, idx2, val);
    else if (!strcmp(reg_name, "PARSER_PTYPE_RAM"))
        set_parser_ptype_ram(rx_top_map, idx1, idx2, val);
    else
        printf("ERROR: Unrecognized register name: %s\n", reg_name);
}

static int loadTextImg(fm_char *filename, mby_ppe_rx_top_map * const rx_top_map)
{
    size_t line_size = 0;
    fm_char *line = NULL;
    fm_uint64 val;
    FILE *fp;
    fm_int len;
    fm_int err;
    fm_int cnt = 0;
    fm_int lineNum = -1;

    if (!filename)
        return -1;

    if (strlen(filename) < 1)
        return 0;

    fp = fopen(filename, "r");

    if (!fp) {
        printf("Unable to open '%s'\n", filename);
        return -1;
    }
    printf("Loading text file: %s\n", filename);
    while ((len = getline(&line, &line_size, fp)) != -1) {
        lineNum++;
        if (len <= 2 || line[0] == '#')
            continue;

        /* Remove CR */
        if ((line[len - 1] == '\n') || (line[len - 1] == '\r')) {
            len--;
            line[len] = '\0';
        }
        if ((line[len - 1] == '\n') || (line[len - 1] == '\r')) {
            len--;
            line[len] = '\0';
        }

        if (line[0] == '#')
            continue;

        fm_char reg_name[100] = { 0 };
        fm_uint64 val         = 0;
        fm_int idx1           = 0;
        fm_int idx2           = 0;
        fm_int match          = 0;
        fm_bool one_dim       = FALSE;
        fm_bool two_dim       = FALSE;

        /* Try to match with one dimensional arrays. */
        match = sscanf(line, "%[^[][%d] 0x%llx\n", reg_name, &idx1, &val);
        if (match == 3)
            one_dim = TRUE;

        /* Try to match with two dimensional arrays. */
        match = sscanf(line, "%[^[][%d][%d] 0x%llx\n", reg_name, &idx1, &idx2, &val);
        if (match == 4)
            two_dim = TRUE;

        if (one_dim)
            setOneDimReg(rx_top_map, reg_name, idx1, val);
        else if (two_dim)
            setTwoDimReg(rx_top_map, reg_name, idx1, idx2, val);
        else
            printf("ERROR: Fail to parse line#%d: %s\n", lineNum, line);
    }

    if (line)
        free(line);

    fclose(fp);

    return 0;
}

void initOutput
(
    mbyParserToMapper * const out
)
{
    out->RX_PORT        = 0;
    out->PA_ADJ_SEG_LEN = 0;

    for (fm_uint i = 0; i < MBY_N_PARSER_KEYS; i++) {
        out->PA_KEYS      [i] = 0;
        out->PA_KEYS_VALID[i] = FALSE;
    }

    for (fm_uint i = 0; i < MBY_N_PARSER_FLGS; i++)
        out->PA_FLAGS[i] = 0;

    for (fm_uint i = 0; i < MBY_N_PARSER_PTRS; i++) {
        out->PA_HDR_PTRS.OFFSET      [i] = 0;
        out->PA_HDR_PTRS.OFFSET_VALID[i] = FALSE;
    }

    out->PA_CSUM_OK         = 0;
    out->PA_EX_STAGE        = 0;
    out->PA_EX_DEPTH_EXCEED = FALSE;
    out->PA_EX_TRUNC_HEADER = FALSE;
    out->PA_EX_PARSING_DONE = FALSE;
    out->PA_DROP            = FALSE;
    out->PA_L3LEN_ERR       = FALSE;
    out->PA_PACKET_TYPE     = 0;
}

void prepareData
(
    struct TestData           test_struct,
    const fm_uint             test_num,
    fm_byte           * const rx_packet,
    mbyRxMacToParser  * const mac2par,
    mbyParserToMapper * const par2map,
    mbyParserToMapper * const par2map_ref
)
{
    // Prepare input:
    fm_uint32 rx_port   = test_struct.in.RX_PORT;
    fm_uint32 rx_length = test_struct.in.RX_LENGTH;

    // Scan in RX_DATA from string:
    const char *  pkt_str_ptr = (char *) test_struct.in.RX_DATA;
    const fm_uint pkt_str_len = (fm_uint) strlen(pkt_str_ptr);

    for (fm_uint i = 0; i < pkt_str_len/2; i++)
    {
        fm_uint val = 0;
        int rv = sscanf(pkt_str_ptr + 2*i, "%02x", &val); // 2 hex chars per byte
        if (rv == 1)
            rx_packet[i] = val & 0xff;
        else
            break;
    }

    // Fill payload with ascending numbers starting from 0:
    for (fm_uint i = pkt_str_len/2; i < rx_length; i++)
        rx_packet[i] = i - pkt_str_len/2; // ramp

    // Compute CRC32 checksum:
    fm_uint crc = mbyCrc32(rx_packet, rx_length - 4);

    // Store CRC:
    for (fm_uint i = 0; i < 4; i++)
        rx_packet[rx_length - 4 + i] = (crc >> (8 * i)) & 0xff;

    mac2par->RX_PORT   = rx_port;
    mac2par->RX_LENGTH = rx_length;
    mac2par->RX_DATA   = rx_packet;

    // Clear outputs:
    initOutput (par2map);
    initOutput (par2map_ref);

    // Init reference outputs:
    par2map_ref->RX_PORT = rx_port;

    for (fm_uint i = 0; i < MBY_N_PARSER_KEYS; i++)
        par2map_ref->PA_KEYS[i] = test_struct.out.PA_KEYS[i];

    for (fm_uint i = 0; i < MBY_N_PARSER_FLGS; i++)
        par2map_ref->PA_FLAGS[i] = test_struct.out.PA_FLAGS[i];

    for (fm_uint i = 0; i < MBY_N_PARSER_PTRS; i++)
        par2map_ref->PA_HDR_PTRS.OFFSET[i] = test_struct.out.PA_HDR_PTRS.OFFSET[i];
}

fm_uint compareValues(mbyParserToMapper * const out, mbyParserToMapper * const out_ref)
{
    fm_uint fails = 0;

    if (out->RX_PORT != out_ref->RX_PORT)
        fails++;

    for (fm_uint i = 0; i < MBY_N_PARSER_KEYS; i++)
        if (out->PA_KEYS[i] != out_ref->PA_KEYS[i])
            fails++;

    for (fm_uint i = 0; i < MBY_N_PARSER_FLGS; i++)
        if (out->PA_FLAGS[i] != out_ref->PA_FLAGS[i])
            fails++;

    for (fm_uint i = 0; i < MBY_N_PARSER_PTRS; i++)
        if (out->PA_HDR_PTRS.OFFSET[i] != out_ref->PA_HDR_PTRS.OFFSET[i])
            fails++;

    return fails;
}

fm_status runTest
(
    mby_ppe_parser_map * const parser_map,
    const fm_uint             test_num,
    char              * const test_name,
    mbyRxMacToParser  * const mac2par,
    mbyParserToMapper * const par2map,
    mbyParserToMapper * const par2map_ref
)
{
    Parser
    (
        parser_map,
        mac2par, par2map
    );

    fm_status test_status = (compareValues(par2map, par2map_ref) > 0) ? FM_FAIL : FM_OK;

    if (test_status == FM_OK)
        printf(COLOR_GREEN "[pass]" COLOR_RESET);
    else
        printf(COLOR_RED   "[FAIL]" COLOR_RESET);

    printf(" Test number %3d:  %s\n", test_num, test_name);

    return test_status;
}

int main(int argc, char *argv[])
{
    for (fm_int i = 1 ; i < argc ; i++)
    {
        if (!strcmp(argv[i], "-i") )
            load_img = TRUE;
    }

    // Allocate storage for registers on the heap:
    fm_uint32 *regs = malloc(MBY_REGISTER_ARRAY_SIZE * sizeof(fm_uint32));
    if (regs == NULL) {
        printf("Could not allocate heap memory for register buffer -- exiting!\n");
        exit(-1);
    }

    fm_byte *rx_packet = malloc(MBY_MAX_DATA_LEN * sizeof(fm_byte));
    if (rx_packet == NULL) {
        printf("Could not allocate heap memory for rx packet buffer -- exiting!\n");
        exit(-1);
    }

    mby_ppe_rx_top_map rx_top_map;
    mby_ppe_tx_top_map tx_top_map;

    if (load_img)
    {
        reset_parser_regs(&rx_top_map.parser);
        if (loadTextImg("img/mby-parser-raw.txt", &rx_top_map))
            return -1;
    }
    else
    {
        initRegs
        (
            &rx_top_map,
            &tx_top_map
        );
    }

#if 1
    fm_uint tests_num = TEST_PASS_MAX;
    struct TestData *test_data = passing_tests;
#else
    fm_uint tests_num = TEST_FAIL_MAX;
    struct TestData *test_data = failing_tests;
#endif

    printf("--------------------------------------------------------------------------------\n");

    fm_uint pass_num = 0;

    for (fm_uint test_num = 0; test_num < tests_num; test_num++)
    {
        struct TestData    test_struct = test_data[test_num];
        char              *test_name   = test_data[test_num].name;

        mbyRxMacToParser  mac2par;     // input
        mbyParserToMapper par2map;     // output
        mbyParserToMapper par2map_ref; // reference output

        prepareData(test_struct, test_num, rx_packet, &mac2par, &par2map, &par2map_ref);

        fm_status test_status = runTest
        (
            &(rx_top_map.parser),
            test_num, test_name, &mac2par, &par2map, &par2map_ref
        );

        if (test_status == FM_OK)
            pass_num++;
    }

    printf("--------------------------------------------------------------------------------\n");

    fm_bool tests_passed = (pass_num == tests_num);

    if (tests_passed)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf("  %3d/%3d - Parser tests\n" COLOR_RESET, pass_num, TEST_PASS_MAX);

    printf("--------------------------------------------------------------------------------\n");

    // Free up buffer memory:
    free(regs);
    free(rx_packet);

    int rv = (tests_passed) ? 0 : -1;

    return rv;
}
