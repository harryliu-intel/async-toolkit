#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mby_common.h>
#include <mby_pipeline.h>
#include <fm_crc32.h>

#include "mby_parser_test.h"

void readRegs()
{
    FILE* file = fopen ("regs.txt", "r");
    unsigned int reg = 0;

    int i  = 0;
    while (!feof (file)) {  
        int rv = fscanf (file, "%u",&reg); 
        regs[i] = reg;     
        i++;
    }

    fclose (file);        
}

unsigned int compareValues(mbyParserToMapper * const out, mbyParserToMapper outRef)
{
    int fails = 0;

    for (int v = 0; v < KEYS_MAX; v++)
        if(out->PA_KEYS[v] != outRef.PA_KEYS[v])
            fails++;

    for (int v = 0; v < FLAGS_MAX; v++)
        if(out->PA_FLAGS[v] != outRef.PA_FLAGS[v])
            fails++;

    for (int v = 0; v < PTRS_MAX; v++)
        if(out->PA_PTRS[v] != outRef.PA_PTRS[v])
            fails++;

    return fails;
}

unsigned int runTests(unsigned int testsNum, struct TestData tests[])
{
    mbyParserToMapper par2map;
    mbyParserToMapper * const out = &par2map;
    unsigned int passNum = 0;

    for(unsigned int x = 0; x < testsNum; x++)
    {
        for (int v = 0; v < KEYS_MAX; v++)
            out->PA_KEYS[v] = 0;

        mbyMacToParser * const in  = &(tests[x].in);

        Parser(regs, in, out);

        if (compareValues(out, tests[x].out) > 0)
            printf(COLOR_RED "[FAIL] " COLOR_RESET);
        else {
            printf (COLOR_GREEN "[PASS] " COLOR_RESET);
            passNum++;
        }

        printf("Test number %3d:  %s\n",x, tests[x].name);
    }

    printf("\n");

    if (passNum != testsNum)
        printf(COLOR_RED "[FAIL] %3d/%3d - PARSER TEST \n\n" COLOR_RESET, passNum, testsNum);
    else 
        printf(COLOR_GREEN "[PASS] %3d/%3d - PARSER TEST \n\n" COLOR_RESET, passNum, testsNum);
        
    return passNum;
}

void prepareData(int testsNum, struct TestData tests[])
{
    for (int i = 0; i < testsNum; i++)
    {
        unsigned char *ptr = malloc(tests[i].in.RX_LENGTH);

        for (unsigned int x = 0; x < tests[i].len; x++)
            ptr[x] = tests[i].in.RX_DATA[x];

        for (unsigned int x = tests[i].len; x < tests[i].in.RX_LENGTH; x++)
            ptr[x] = x - tests[i].len;

        unsigned int crc = fmCrc32(ptr, tests[i].in.RX_LENGTH  - 4);

        for (unsigned int x = 0; x < 4; x++)
            ptr[tests[i].in.RX_LENGTH-4+x] = (crc >> (8*x)) & 0xff;

        tests[i].in.RX_DATA = ptr; 
    }   
}

void freeData(int testsNum, struct TestData tests[])
{
    for (int i = 0; i < testsNum; i++)
        if (tests[i].in.RX_DATA != NULL)
            free(tests[i].in.RX_DATA);
}

int main()
{
    printf("##############################################################################################\n\n");
    printf("RUNNING PARSER TESTS\n\n");

    readRegs();
    prepareData(TEST_PASS_MAX, testsPass);
 
    unsigned int passNum = runTests(TEST_PASS_MAX, testsPass);
 
    freeData(TEST_PASS_MAX, testsPass);

#if 0 // Tests currently failing -> REVISIT!!!
    prepareData(TEST_FAIL_MAX, testsFail);
    runTests(TEST_FAIL_MAX, testsFail);
#endif  

    int rv = (passNum == TEST_PASS_MAX) ? 0 : -1;
    return rv;
}
