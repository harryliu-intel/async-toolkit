#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mby_common.h>
#include <mby_pipeline.h>
#include <mby_crc32.h>

#include "mby_parser_test.h"

void readRegs(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE])
{
    FILE *file = fopen ("regs.txt", "r");
    if (file == NULL) {
        printf("Could not open file regs.txt -- exiting!\n");
        exit(-1);
    }

    int i = 0;
    while (!feof(file) && (i < MBY_REGISTER_ARRAY_SIZE))
    {  
        fm_uint32 val = 0;
        int rv = fscanf (file, "%u", &val); 
        if (rv != 1) {
            printf("Got an error while reading file regs.txt -- exiting!\n");
            exit(-1);
        }
        regs[i] = val;
        i++;
    }

    fclose (file);        
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
        out->PA_PTRS      [i] = 0; 
        out->PA_PTRS_VALID[i] = FALSE; 
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
        par2map_ref->PA_PTRS[i] = test_struct.out.PA_PTRS[i];
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
        if (out->PA_PTRS[i] != out_ref->PA_PTRS[i])
            fails++;

    return fails;
}

fm_status runTest
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint             test_num,
    char              * const test_name,
    mbyRxMacToParser  * const mac2par,
    mbyParserToMapper * const par2map,
    mbyParserToMapper * const par2map_ref
)
{
    Parser(regs, mac2par, par2map);

    fm_status test_status = (compareValues(par2map, par2map_ref) > 0) ? FM_FAIL : FM_OK;

    if (test_status == FM_OK)
        printf(COLOR_GREEN "[pass]" COLOR_RESET);
    else
        printf(COLOR_RED   "[FAIL]" COLOR_RESET);

    printf(" Test number %3d:  %s\n", test_num, test_name);

    return test_status;
}

int main()
{
    // Allocate storage for registers on the heap:
    fm_uint32 *regs = malloc(MBY_REGISTER_ARRAY_SIZE * sizeof(fm_uint32)); 
    if (regs == NULL) {
        printf("Could not allocate heap memory for register buffer -- exiting!\n");
        exit(-1);
    }

    fm_byte *rx_packet = malloc(MBY_MAX_PACKET_SIZE * sizeof(fm_byte)); 
    if (rx_packet == NULL) {
        printf("Could not allocate heap memory for rx packet buffer -- exiting!\n");
        exit(-1);
    }

    // Read registers from "regs.txt":
    readRegs(regs);

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
 
        fm_status test_status = runTest(regs, test_num, test_name, &mac2par, &par2map, &par2map_ref);

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
