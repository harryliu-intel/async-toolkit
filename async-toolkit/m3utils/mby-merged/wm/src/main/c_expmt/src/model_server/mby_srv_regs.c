


#include <stdio.h>
#include <sys/time.h>

#include "mby_model.h"
#include "mby_srv_log.h"


#define HLP_MAC_SCRATCH(...) 0
void runRegBenchmark()
{
    unsigned int numops = 1000;
    struct timeval start;
    struct timeval end;
    fm_uint64 addr;
    fm_uint64 writeVal;
    fm_uint64 readVal;
    unsigned int i;
    double usec;

    /* Basic sanity check before testing performance */
    addr = HLP_MAC_SCRATCH(0, 0);
    writeVal = 0x1234567890abcdef;
    mbyWriteReg(0, addr, writeVal);
    mbyReadReg(0, addr, &readVal);
    if (readVal != writeVal) {
        printf("Sanity check failed: register addr=%02llx writeVal=%02llx readVal=%02llx\n",
                addr, writeVal, readVal);
        return;
    }

    /* Benchmark register write */
    gettimeofday(&start, NULL);
    for (i = 0; i < numops; ++i)
        mbyWriteReg(0, HLP_MAC_SCRATCH(i % HLP_MAC_SCRATCH_ENTRIES, 0), writeVal);
    gettimeofday(&end, NULL);
    usec = (double)(end.tv_sec - start.tv_sec) * 1.0e6 +
           (double)(end.tv_usec - start.tv_usec);
    printf("Register write avg time: %0.3f usec\n", usec / numops);

    /* Benchmark register read */
    gettimeofday(&start, NULL);
    for (i = 0; i < numops; ++i)
        mbyReadReg(0, HLP_MAC_SCRATCH(i % HLP_MAC_SCRATCH_ENTRIES, 0), &readVal);
    gettimeofday(&end, NULL);
    usec = (double)(end.tv_sec - start.tv_sec) * 1.0e6 +
           (double)(end.tv_usec - start.tv_usec);
    printf("Register read avg time: %0.3f usec\n", usec / numops);

}

