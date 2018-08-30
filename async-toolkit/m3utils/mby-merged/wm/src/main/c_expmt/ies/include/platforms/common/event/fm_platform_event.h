/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_event.h
 * Creation Date:   2012
 * Description:     Platform event specific definitions
 *
 * INTEL CONFIDENTIAL
 * Copyright 2011-2013  Intel Corporation. All Rights Reserved.
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

#ifndef __FM_PLATFORM_EVENT_H
#define __FM_PLATFORM_EVENT_H

/* This macro is also used to protect again platform event structure size
 * defined greater than the predefined size in the API */
#define FM_PLAT_INIT_EVENT_PLATFORM(p, _type, _structType)                  \
    if (sizeof(_structType) > FM_EVENT_PLATFORM_MAX_SIZE)                   \
    {                                                                       \
        FM_LOG_PRINT("ERROR: Platform Event struct size is too large\n");   \
    }                                                                       \
    p->type = _type;                                                        \
    memset(p->eventData, 0, sizeof(_structType))

fm_status fmPlatformEventSendPortXcvrState(fm_int           sw,
                                           fm_int           port,
                                           fm_int           mac,
                                           fm_int           lane,
                                           fm_uint32        xcvrSignals,
                                           fm_eventPriority priority);

fm_status fmPlatformEventSendPscStatus(fm_int           psc,
                                       fm_uint32        status,
                                       fm_eventPriority priority);

fm_status fmPlatformEventSendPsuStatus(fm_int           psu,
                                       fm_uint32        status,
                                       fm_eventPriority priority);

fm_status fmPlatformEventSendFanStatus(fm_int           fan,
                                       fm_uint32        status,
                                       fm_eventPriority priority);

fm_status fmPlatformEventSendSwTemperature(fm_int           sw,
                                           fm_uint32        type,
                                           fm_int           temperature,
                                           fm_eventPriority priority);

/*****************************************************************************
 * NOTE: Defines and functions below here are intended for external use.
 *****************************************************************************/

typedef enum
{
    /* Port SFP+ or QSFP module status update. See fm_eventPlatformPortXcvr. */
    FM_EVENT_PLATFORM_TYPE_PORT_XCVR = 1,

    /* Power controller status update. See fm_eventPlatformPscStatus. */
    FM_EVENT_PLATFORM_TYPE_PSC_STATUS,

    /* Removable Power Supply status update. See fm_eventPlatformPsuStatus. */
    FM_EVENT_PLATFORM_TYPE_PSU_STATUS,

    /* Fan status update. See fm_eventPlatformFanStatus. */
    FM_EVENT_PLATFORM_TYPE_FAN_STATUS,

    /* Switch has reached warning temperature limit.
     * See fm_eventPlatformSwTemperature. */
    FM_EVENT_PLATFORM_TYPE_SW_WARN_TEMP,

    /* Switch temperature has come down below warning limit.
     * See fm_eventPlatformSwTemperature */
    FM_EVENT_PLATFORM_TYPE_SW_NORMAL_TEMP,

    /* Switch has reached max temperature limit and has been shut down.
     * See fm_eventPlatformSwTemperature */
    FM_EVENT_PLATFORM_TYPE_SW_OVER_TEMP,

    /* Add new types above this line */
    FM_EVENT_PLATFORM_TYPE_MAX,

} fm_eventPlatformType;


/* NOTE: All the platform event structure size must be less than
         FM_EVENT_PLATFORM_MAX_SIZE defined in the API */
 
typedef struct _fm_eventPlatformPortXcvr
{
    /** The logical port for which the event is being reported. */
    fm_uint32         port;

    /** The MAC for the logical port on which the event occurred. */
    fm_int32          mac;

    /** The port's zero-based lane number for which a per-lane event
     *  being reported. Should be set to FM_PORT_LANE_NA for events that
     *  don't have a per-lane scope .   */
    fm_int32         lane; 

    /** A bitmask where each bit represents the status
     *  of a given signal (see the ''Transceiver Signals'' definitions). */
    fm_uint32         xcvrSignals;

} fm_eventPlatformPortXcvr;


typedef struct _fm_eventPlatformPscStatus
{
    /** Power Controller type (such as VDD, VDDA) for which the event is
     *  being reported. */
    fm_uint32         psc;

    /** Power Controller status (see the ''FM_PSC_STATUS'' definitions). */
    fm_uint32         status;

} fm_eventPlatformPscStatus;


typedef struct _fm_eventPlatformPsuStatus
{
    /** The zero-based psu number for which the event is being reported. */
    fm_uint32         psu;

    /** PSU status (see the ''FM_PSU_STATUS'' definitions). */
    fm_uint32         status;

} fm_eventPlatformPsuStatus;


typedef struct _fm_eventPlatformFanStatus
{
    /** The zero-based fan number, or FM_PLATFORM_FAN_ALL to indicate all
     *  fans for which the event is being reported. */
    fm_uint32         fan;

    /** PSU status (see the ''FM_FAN_STATUS'' definitions). */
    fm_uint32         status;

} fm_eventPlatformFanStatus;



typedef struct _fm_eventPlatformSwTemperature
{
    /** The switch number for which the event is being reported. */
    fm_uint32         sw;

    /** The temperature of the switch in centi-Celsius. */
    fm_int32         temperature;

} fm_eventPlatformSwTemperature;

#endif /* __FM_PLATFORM_EVENT_H */
