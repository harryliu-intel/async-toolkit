/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_types.h
 * Creation Date:   June 5, 2012
 * Description:     Platform specific definitions
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved.
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

#ifndef __FM_PLATFORM_TYPES_H
#define __FM_PLATFORM_TYPES_H

#include <platforms/common/packet/generic-packet/fm_generic_packet.h>

/* Include FM4000 common headers to make sure we don't introduce
   compilation errors */
#include <platforms/common/packet/generic-packet/fm4000/fm4000_generic_tx.h>
#include <platforms/common/packet/generic-packet/fm4000/fm4000_generic_rx.h>

/* For buffer management */
#include <platforms/common/buffers/std-alloc/fm_buffer_std_alloc.h>

/* For attribute loader */
#include <platforms/common/fm_file_attr_loader.h>

/* For network sockets library */
#include <platforms/common/lib/net/fm_netsock.h>

/* For dynamic library loading (microcode support, white model) */
#include <platforms/common/lib/dll/fm_dynamicLib.h>

#include <platform_buffer_defs.h>
#include <platform_attr.h>

/* Headers for the Model Packet Queue */
#include <platforms/common/model/fm_model_message.h>
#include <platforms/common/model/fm_model_packet_queue.h>

#ifdef FM_SUPPORT_FM4000
#include <platforms/common/model/fm4000/fm4000_model.h>
#endif

#ifdef FM_SUPPORT_FM6000
/* For ucode load */
#include <platforms/common/lib/ucode/fm_ucodeLoad.h>

#include <platforms/common/model/fm6000/fm6000_model.h>
#endif

#ifdef FM_SUPPORT_FM10000
#include <platforms/common/model/fm10000/fm10000_model.h>
#endif

#ifdef FM_SUPPORT_HLP
#include <platforms/common/model/hlp/hlp_model.h>
#endif

#ifdef FM_SUPPORT_CPK
#include <platforms/common/model/cpk/cpk_model.h>
#endif


/* Headers for the packet queue external interface, used by TestPoint */
#include <platforms/common/model/testpoint_support.h>


/* defines the platform state structure */
typedef struct
{
    /* switch number represented by this device */
    int                    sw;

    /* this is so the platform knows what kind of device it is */
    fm_switchFamily        family;
    fm_switchModel         model;
    fm_switchVersion       version;

    /* access lock for mapped spaces */
    fm_lock                accessLocks[FM_MAX_PLAT_LOCKS];

    /* interrupt source */
    fm_uint                intrSource;

    /* Switch interrupt enable/disable */
    fm_bool                swInterruptEnable;

    /* interrupt mask for the EPLs on this switch */
    fm_uint32              portInterruptMask;

    /* thread handle for interrupt listener */
    fm_thread              intrListener;

    /* to make rest of API happy */
    fm_packetHandlingState packetState;

    /* Chip model state object */
    void *                 chipModel;

#ifdef FM_SUPPORT_FM6000
    struct _fm6000_ucFuncs *ucFuncs;

    fm_int                  microcodeLibHandle;
#endif

    fm_platformSwitchType   switchType;
    /* This is maximum physical ports */
    fm_int                  maxPorts;
    /* Function pointers for switch specific functions */
    fm_status               (*ModelGetPortMap)(fm_platformPort *portMap,
                                               fm_int           numPorts);
    fm_status               (*ModelInitialize)(void **chipModel,
                                               fm_int sw,
                                               void * funcPtrs);
    fm_status               (*ModelReadCSR)(fm_int     sw,
                                            fm_uint32  addr,
                                            fm_uint32 *value);
    fm_status               (*ModelReadCSRMult)(fm_int     sw,
                                                fm_uint32  addr,
                                                fm_int     n,
                                                fm_uint32 *value);
    fm_status               (*ModelReadCSR64)(fm_int     sw,
                                              fm_uint32  addr,
                                              fm_uint64 *value);
    fm_status               (*ModelReadCSRMult64)(fm_int     sw,
                                                  fm_uint32  addr,
                                                  fm_int     n,
                                                  fm_uint64 *value);
    fm_status               (*ModelReset)(fm_int sw);
    fm_status               (*ModelResetV2)(fm_int sw, fm_int domain);
    fm_status               (*ModelTick)(fm_int sw, fm_uint32 *interrupt);
    fm_status               (*ModelWriteCSR)(fm_int    sw,
                                             fm_uint32 addr,
                                             fm_uint32 newValue);

} fm_platform_state;

typedef struct _fm_rootPlatform
{
    /**************************************************
     * platform.c
     **************************************************/
    fm_platform_state          fmPlatformState[FM_MAX_NUM_SWITCHES];

    /* the global state structure that manages the buffers */
    fm_bufferAllocState        bufferAllocState;

    /***************************************************
     * The global state for the packet queue that is
     * used by all model instances.
     **************************************************/
    void *                     packetQueue;

    /* TestPoint support state */
    void *                     tpState;

    /* For generic packet send code */
    fm_bool                    dmaEnabled;

    /* Primary switch number of the platform */
    fm_int                     primarySw;

} fm_rootPlatform;

/* the global state per supported focalpoint */
extern fm_rootPlatform *         fmRootPlatform;

/* macros to deal with the access locks */
#define TAKE_PLAT_LOCK(sw, type)                                               \
    fmCaptureLock( &fmRootPlatform->fmPlatformState[(sw)].accessLocks[(type)], \
                   FM_WAIT_FOREVER);
#define DROP_PLAT_LOCK(sw, type) \
    fmReleaseLock( &fmRootPlatform->fmPlatformState[(sw)].accessLocks[(type)]);

/* many of the components require this macro */
#define TAKE_PLAT_STATE_LOCK()                                                    \
    fmCaptureLock( &fmRootPlatform->fmPlatformState[0].accessLocks[FM_PLAT_INFO], \
                   FM_WAIT_FOREVER);
#define DROP_PLAT_STATE_LOCK() \
    fmReleaseLock( &fmRootPlatform->fmPlatformState[0].accessLocks[FM_PLAT_INFO]);

fm_status fmPlatformWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 newValue);

fm_status fmPlatformMapCardinalToPhysical(fm_int    sw,
                                          fm_int    cpi,
                                          fm_int *  physPort);

fm_status fmPlatformMapPhysicalToCardinal(fm_int    sw,
                                          fm_int    physPort,
                                          fm_int *  cpi);


/**************************************************
 * platform_api_stubs.c
 **************************************************/

/*
 * These functions have default or dummy implementations
 * that may be used in some platforms.
 *
 * Comment out the macro definition to enable the stub; uncomment
 * it to indicate that you are implementing the function in your
 * platform layer.
 */
/* #define FM_HAVE_fmPlatformAddGlortToPortMapping */
/* #define FM_HAVE_fmPlatformBypassEnabled */
#define FM_HAVE_fmPlatformCreateSWAG
#define FM_HAVE_fmPlatformDeleteSWAG
/* #define FM_HAVE_fmPlatformGetPortCapabilities */
/* #define FM_HAVE_fmPlatformGetPortClockSel */
/* #define FM_HAVE_fmPlatformGetPortDefaultSettings */
/* #define FM_HAVE_fmPlatformGetSwitchPartNumber */
/* #define FM_HAVE_fmPlatformMACMaintenanceSupported */
/* #define FM_HAVE_fmPlatformReceivePackets */
#define FM_HAVE_fmPlatformSendPacket
#define FM_HAVE_fmPlatformSendPacketDirected
/* #define FM_HAVE_fmPlatformSendPackets */
#define FM_HAVE_fmPlatformSendPacketSwitched
#define FM_HAVE_fmPlatformSendPacketISL
/* #define FM_HAVE_fmPlatformSetBypassMode */
/* #define FM_HAVE_fmPlatformSetPortDefaultVlan */
/* #define FM_HAVE_fmPlatformShowPortState */
/* #define FM_HAVE_fmPlatformSWAGInitialize */
/* #define FM_HAVE_fmPlatformSwitchPreInitialize */
/* #define FM_HAVE_fmPlatformSwitchTerminate */

/*
 * These functions are platform-specific, and must be fully
 * implemented for the API to execute. Non-functional stubs
 * are provided for documentation purposes.
 */
#define FM_HAVE_fmPlatformDisableInterrupt
#define FM_HAVE_fmPlatformEnableInterrupt
#define FM_HAVE_fmPlatformGetInterrupt
#define FM_HAVE_fmPlatformGetSchedulerConfig
#define FM_HAVE_fmPlatformInitialize
#define FM_HAVE_fmPlatformMapLogicalPortToPhysical
#define FM_HAVE_fmPlatformMapPhysicalPortToLogical
#define FM_HAVE_fmPlatformRelease
#define FM_HAVE_fmPlatformReset
#define FM_HAVE_fmPlatformSwitchInitialize
#define FM_HAVE_fmPlatformSwitchPostInitialize

/* This function is a candidate for a common implementation. */
#define FM_HAVE_fmPlatformLoadMicrocode


/**************************************************
 * platform_app_stubs.c
 **************************************************/

/*
 * These functions have default or dummy implementations
 * that may be used in some platforms.
 *
 * Comment out the macro definition to enable the stub; uncomment
 * it to indicate that you are implementing the function in your
 * platform layer.
 */
/* #define FM_HAVE_fmPlatformMdioRead */
/* #define FM_HAVE_fmPlatformMdioWrite */

#endif /* __FM_PLATFORM_TYPES_H */

