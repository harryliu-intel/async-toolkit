// file = 0; split type = patterns; threshold = 100000; total count = 0.
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "rmapats.h"

void  hsG_0__0 (struct dummyq_struct * I1251, EBLK  * I1245, U  I674);
void  hsG_0__0 (struct dummyq_struct * I1251, EBLK  * I1245, U  I674)
{
    U  I1503;
    U  I1504;
    U  I1505;
    struct futq * I1506;
    struct dummyq_struct * pQ = I1251;
    I1503 = ((U )vcs_clocks) + I674;
    I1505 = I1503 & ((1 << fHashTableSize) - 1);
    I1245->I714 = (EBLK  *)(-1);
    I1245->I718 = I1503;
    if (I1503 < (U )vcs_clocks) {
        I1504 = ((U  *)&vcs_clocks)[1];
        sched_millenium(pQ, I1245, I1504 + 1, I1503);
    }
    else if ((peblkFutQ1Head != ((void *)0)) && (I674 == 1)) {
        I1245->I720 = (struct eblk *)peblkFutQ1Tail;
        peblkFutQ1Tail->I714 = I1245;
        peblkFutQ1Tail = I1245;
    }
    else if ((I1506 = pQ->I1151[I1505].I732)) {
        I1245->I720 = (struct eblk *)I1506->I731;
        I1506->I731->I714 = (RP )I1245;
        I1506->I731 = (RmaEblk  *)I1245;
    }
    else {
        sched_hsopt(pQ, I1245, I1503);
    }
}
#ifdef __cplusplus
extern "C" {
#endif
void SinitHsimPats(void);
#ifdef __cplusplus
}
#endif
