/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_debug_int.h
 * Creation Date:   April 17, 2007
 * Description:     Provide debugging functions.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or 
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#ifndef __FM_FM_DEBUG_INT_H
#define __FM_FM_DEBUG_INT_H


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 *  Identifies miscellaneous attributes used by the debug function.
 *  Used as an argument to fmDbgSetMiscAttribute(). 
 *****************************************************************************/
typedef enum
{
    /* Used to indicate to read the FM_PORT_MASK attribute instead
       of reading the PORT_CFG_2 register from the switch. */
    FM_DBG_ATTR_PORT_CFG2 = 0,

} fm_dbgMiscAttribute;


/**************************************************/
/** \ingroup intTypeScalar
 * A debug register dump callback function for
 * outputing or archiving the results of a
 * register dump. This function returns
 * fm_bool indicating if it is done (TRUE) or still
 * busy and there should be no intermediate dumping
 * (FALSE). Takes as arguments:
 *                                                                      \lb\lb
 *  sw - The switch from which the register
 *       was dumped.
 *                                                                      \lb\lb
 * regID - The index into the register table.
 *                                                                      \lb\lb
 * regAddress - The absolute address of the register
 *              as dumped, including any indexing.
 *                                                                      \lb\lb
 * regSize - The number of words in register.
 *                                                                      \lb\lb
 * isStatReg - TRUE if register is a statistics register.
 *                                                                      \lb\lb
 * regValue1 - low-order 64 bits of register.
 *                                                                      \lb\lb
 * regValue2 - high-order 64 bits of register.
 *                                                                      \lb\lb
 * callbackInfo - Cookie provided to callback.
 **************************************************/
typedef fm_bool (*fm_regDumpCallback)(fm_int    sw,
                                      fm_int    regID,
                                      fm_uint   regAddress,
                                      fm_int    regSize,
                                      fm_bool   isStatReg,
                                      fm_uint64 regValue1,
                                      fm_uint64 regValue2,
                                      fm_voidptr callbackInfo);


/*****************************************************************************
 * Structures and Typedefs
 *****************************************************************************/

typedef struct
{
    fm_int    regId;
    fm_uint   regAddress;
    fm_int    regSize;
    fm_bool   isStatReg;
    fm_uint64 regValue1;
    fm_uint64 regValue2;

} fmDbgFulcrumRegisterSnapshot;

typedef struct
{
    fm_int                       sw;
    fm_timestamp                 timestamp;
    fm_int                       regCount;
    fmDbgFulcrumRegisterSnapshot registers[FM_DBG_MAX_SNAPSHOT_REGS];

} fmDbgFulcrumSnapshot;


typedef struct
{
    fm_int               sw;
    fm_int               port;
    fm_int               mac;
    fm_int               lane;
    fm_int               sampleCount;
    fm_eyeDiagramSample *eyeDiagram;

} fmDbgEyeDiagram;


#ifdef FM_DBG_NEED_TRACK_FUNC

typedef struct
{
    const char *address;
    int         lineNum;
    fm_uint64   execCount;

} fmDbgExecTraceEntry;

#define FM_DBG_MAX_EXEC_TRACE_ENTRIES  500

struct _fm_debugTrace
{
    fmDbgExecTraceEntry fmDbgExecTraceTable[FM_DBG_MAX_EXEC_TRACE_ENTRIES];
    fm_int              fmDbgExecTraceEntryCount;
    fm_uint64           fmDbgExecTraceOverflows;

};

#else
struct _fm_debugTrace;
#endif

typedef struct _fm_debugTrace    fm_debugTrace;

typedef struct
{
    /* Set sw to -1 to monitor all switches. */
    fm_int    sw;
    fm_uint32 regOffset;

} fmDbgMonitoredRegInfo;

/**************************************************
 * Driver event counters.
 *
 * Note: driver events are not the same thing as
 * trace events.
 **************************************************/

typedef struct
{
    fm_uint64 numAllocs;
    fm_uint64 numFrees;
    fm_uint64 numReidentifies;

} drvEventCounter;

/**************************************************
 * Trace buffer
 *
 * The trace buffer is 4 words per entry.  The first
 * word is an event code.  The remaining 3 words are
 * event code specific.  Any value may be used for
 * all words, except the event code must never be
 * 00000000 or FFFFFFFF, which are reserved for
 * future use.
 **************************************************/
typedef struct
{
    int          eventCode;
    unsigned int data1;
    unsigned int data2;
    unsigned int data3;
    fm_timestamp timestamp;

} TRACE_ENTRY;

typedef struct
{
    int          samples;
    fm_float     avgTime;
    fm_float     minTime;
    fm_float     maxTime;
    fm_float     lastSample;

    fm_timestamp startTime;
    fm_timestamp endTime;

} fmTimerMeasurement;


typedef struct _fm_rootDebug
{
    /**************************************************
     * fm_debug.c
     **************************************************/

    /* stores diagnostic information per switch */
    fm_switchDiagnostics  fmSwitchDiagnostics[FM_MAX_NUM_SWITCHES];

    /* Stores global diagnostic information. */
    fm_globalDiagnostics  fmGlobalDiagnostics;

    /* Only used if FM_DBG_NEED_TRACK_FUNC is true */
    fm_debugTrace *       trace;

    /* Callback function and cookies for passing strings to calling process.
     * used for routing output to CLI applications (e.g., ControlPoint). */
    fm_dbgPrintCallBack   fmDebugPrintCallback;
    void *                fmDbgCookie1;
    void *                fmDbgCookie2;

    /* for preventing multiple threads from squashing printfs */
    fm_lock               fmDbgPrintfAccessLock;

    /* debug level used for debug printfs */
    fm_uint               fmDbgLevel;

    /* used for tracking packet receive size histograms */
    int                   dbgPacketSizeDist[FM_DBG_MAX_PACKET_SIZE];

    /* generic debug subsystem lock */
    fm_lock               fmDbgLock;

    /**************************************************
     * fm_debug_regs.c
     **************************************************/
    int                   fmDbgMonitoredRegCount;
    fmDbgMonitoredRegInfo fmDbgMonitoredRegs[FM_DBG_MAX_MONITORED_REGS];

    /**************************************************
     * fm_debug_snapshots.c
     **************************************************/
    fmDbgFulcrumSnapshot *fmDbgSnapshots[FM_DBG_MAX_SNAPSHOTS];


    /**************************************************
     * fm_debug_eye_diagrams.c
     **************************************************/
    fmDbgEyeDiagram      *fmDbgEyeDiagrams[FM_DBG_MAX_EYE_DIAGRAMS];
    

    /**************************************************
     * fm_debug_trace.c
     **************************************************/
    drvEventCounter       drvEventCounters[FM_EVID_MAX];
    TRACE_ENTRY           traceBfr[FM_DBG_TRACE_BFR_SIZE];
    TRACE_ENTRY *         TBInPtr;  /* Input pointer */
    TRACE_ENTRY *         TBOutPtr; /* Output pointer */
    int                   TBcount;
    fm_timestamp          traceStartTime;

    /**************************************************
     * When TBlock is non-zero, fmDbgTracePost (which
     * can be called by tasks or ISRs) cannot write
     * to the trace buffer.  fmDbgTracePost cannot
     * touch TBlock.
     **************************************************/
    int                   TBlock;
    int                   TBmode;

    /**************************************************
     * TBtail is the number of events to add to the
     * trace buffer following a trigger event for
     * MODE_TRIGGER.
     **************************************************/
    int                   TBtailReset;
    int                   TBtail;
    int                   TBtriggerEvent;

    /**************************************************
     * Trigger table
     *
     * Event codes appearing in this table will cause
     * event capture to cease if TBmode = MODE_TRIGGER.
     **************************************************/
    int                   trigTable[5];

    /**************************************************
     * Exclusion table
     *
     * Event codes in this table will not be posted to
     * the trace buffer.  Can be used to filter the
     * trace buffer at run-time.
     **************************************************/
    int                   exclusions[FM_DBG_EXCLUSION_TABLE_SIZE];
    int                   numberOfExclusions;
    fmTimerMeasurement    dbgTimerMeas[FM_DBG_MAX_TIMER_MEAS];

    /* Event queue debugging globals. */
    fm_tree               dbgEventQueueList;
    fm_lock               dbgEventQueueListLock;

} fm_rootDebug;

extern fm_rootDebug *fmRootDebug;

/*****************************************************************************
 * Function Prototypes
 *****************************************************************************/

extern void      fmDbgInitSnapshots   (void);
extern void      fmDbgInitTrace       (void);
extern fm_status fmDbgInitEyeDiagrams (void);

extern fm_bool fmDbgPrintRegValue(fm_int    sw,
                                  fm_int    regId,
                                  fm_uint   regAddress,
                                  fm_int    regSize,
                                  fm_bool   isStatReg,
                                  fm_uint64 regValue1,
                                  fm_uint64 regValue2,
                                  fm_voidptr callbackInfo);

extern void fmDbgP(const char *fmt, ...)
#ifdef __GNUC__
__attribute__ ( ( format(printf, 1, 2) ) )
#endif
;

extern fm_status fmDbgSetMiscAttribute(fm_int sw, fm_uint attr, fm_int value);

#ifdef FM_SUPPORT_FM2000
extern fm_status fm2000DbgDumpChipRegister(fm_int             sw,
                                           fm_int             index,
                                           fm_int             regid,
                                           fm_voidptr         callbackInfo,
                                           fm_regDumpCallback callback);
#endif


#endif /* __FM_FM_DEBUG_INT_H */
