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
    while (!feof (file))
    {  
        fm_uint32 val = 0;
        int rv = fscanf (file, "%u", &val); 
        regs[i] = (rv == 1) ? val : 0;
        i++;
    }

    fclose (file);        
}

fm_uint compareValues(mbyParserToMapper * const out, mbyParserToMapper outRef)
{
    fm_uint fails = 0;

    for (fm_uint v = 0; v < MBY_N_PARSER_KEYS; v++)
        if (out->PA_KEYS[v] != outRef.PA_KEYS[v])
            fails++;

    for (fm_uint v = 0; v < MBY_N_PARSER_FLGS; v++)
        if (out->PA_FLAGS[v] != outRef.PA_FLAGS[v])
            fails++;

    for (fm_uint v = 0; v < MBY_N_PARSER_PTRS; v++)
        if (out->PA_PTRS[v] != outRef.PA_PTRS[v])
            fails++;

    return fails;
}

void initOutput
(
    mbyParserToMapper * const out
)
{
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

fm_uint runTests
(
    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    fm_uint   testsNum,
    struct TestData tests[]
)
{
    printf("--------------------------------------------------------------------------------\n");

    mbyParserToMapper par2map;
    mbyParserToMapper * const out = &par2map;

    initOutput (out);
    
    fm_uint passNum = 0;

    for (fm_uint x = 0; x < testsNum; x++)
    {
        mbyRxMacToParser * const in  = &(tests[x].in);

        Parser(regs, in, out);
        
        if (compareValues(out, tests[x].out) > 0)
            printf(COLOR_RED   "[FAIL]" COLOR_RESET);
        else {
            printf(COLOR_GREEN "[pass]" COLOR_RESET);
            passNum++;
        }

        printf(" Test number %3d:  %s\n", x, tests[x].name);
    }

    printf("--------------------------------------------------------------------------------\n");

    if (passNum != testsNum)
        printf(COLOR_RED   "[FAIL]");
    else 
        printf(COLOR_GREEN "[pass]");

    printf("  %3d/%3d - Parser tests\n" COLOR_RESET, passNum, testsNum);

    printf("--------------------------------------------------------------------------------\n");

    return passNum;
}

void prepareData(fm_uint testsNum, struct TestData tests[])
{
    for (fm_uint i = 0; i < testsNum; i++)
    {
        // Scan in RX_DATA from string:
        const char *  packetStrPtr = (char *) tests[i].in.RX_DATA;
        const fm_uint packetStrLen = (fm_uint) strlen(packetStrPtr);

        // Point at RX_DATA array:
        fm_byte *ptr = tests[i].in.RX_DATA;

        for (fm_uint x = 0; x < packetStrLen/2; x++)
        {
            fm_uint val = 0;
            int rv = sscanf(packetStrPtr + 2*x, "%02x", &val); // 2 hex chars per byte
            if (rv == 1)
                ptr[x] = val & 0xff; // write in place back to RX_DATA array
            else
                break;
        }

        // Fill payload with ascending numbers starting from 0:
        for (fm_uint x = packetStrLen/2; x < tests[i].in.RX_LENGTH; x++)
            ptr[x] = x - packetStrLen/2; // ramp

        // Compute CRC32 checksum:
        fm_uint crc = mbyCrc32(ptr, tests[i].in.RX_LENGTH - 4);

        // Store CRC:
        for (fm_uint x = 0; x < 4; x++)
            ptr[tests[i].in.RX_LENGTH-4+x] = (crc >> (8*x)) & 0xff;
    }   
}

int main()
{
    // Allocate storage for registers on the heap:
    fm_uint32 *regs = malloc(MBY_REGISTER_ARRAY_SIZE * sizeof(fm_uint32)); 
    if (regs == NULL) {
        printf("Could not allocate heap memory for registers -- exiting!\n");
        exit(-1);
    }

    // Read registers from "regs.txt":
    readRegs(regs);

    prepareData(TEST_PASS_MAX, testsPass);
 
    fm_uint passNum = runTests(regs, TEST_PASS_MAX, testsPass);

#if 0 // Tests currently failing -> REVISIT!!!
    prepareData(TEST_FAIL_MAX, testsFail);
    runTests(regs, TEST_FAIL_MAX, testsFail);
#endif  

    // Free up registers:
    free(regs);
    
    int rv = (passNum == TEST_PASS_MAX) ? 0 : -1;
    return rv;
}
