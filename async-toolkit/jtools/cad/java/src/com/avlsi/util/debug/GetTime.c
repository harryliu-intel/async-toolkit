#include <unistd.h>
#include <sys/times.h>

#ifndef FULCRUM_EXCLUDED_GENERATED_INCLUDES
#include "com_avlsi_util_debug_GetTime.h"
#endif

static long ticks_per_second=0;

void init() __attribute__ ((constructor));

void init() {
    ticks_per_second=sysconf(_SC_CLK_TCK);
}

JNIEXPORT jlong JNICALL Java_com_avlsi_util_debug_GetTime_userCPUTime
(JNIEnv *env, jclass thisClass)
{
    struct tms t;
    times(&t);
    return t.tms_utime*1000/ticks_per_second;
}
JNIEXPORT jlong JNICALL Java_com_avlsi_util_debug_GetTime_systemCPUTime
(JNIEnv *env, jclass thisClass)
{
    struct tms t;
    times(&t);
    return t.tms_stime*1000/ticks_per_second;
}
JNIEXPORT jlong JNICALL Java_com_avlsi_util_debug_GetTime_childrenUserCPUTime
(JNIEnv *env, jclass thisClass)
{
    struct tms t;
    times(&t);
    return t.tms_cutime*1000/ticks_per_second;
}
JNIEXPORT jlong JNICALL Java_com_avlsi_util_debug_GetTime_childrenSystemCPUTime
(JNIEnv *env, jclass thisClass)
{
    struct tms t;
    times(&t);
    return t.tms_cstime*1000/ticks_per_second;
}
