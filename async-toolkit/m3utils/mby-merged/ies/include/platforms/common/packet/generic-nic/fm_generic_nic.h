/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_nic.h
 * Creation Date:   Sep 1, 2008
 * Description:     Header file for generic NIC packet I/O
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

#ifndef __FM_FM_GENERIC_NIC_H
#define __FM_FM_GENERIC_NIC_H

#define FM_DEV_NAME_LEN             32

typedef struct _fm_nic
{
    /* Net device name used by this NIC. */
    fm_char devName[FM_DEV_NAME_LEN];

    /* Socket File descriptore returned by socket() */
    fm_int sockFd;

    /* Indicate whether the NIC is set (true) in promiscuous mode or not.*/
    fm_bool promiscuous;

    /* Number of switched attached to this NIC */
    fm_int numSwitches;

    /* Semaphore for holding the thread until the socket is part of the
       'select' list */
    fm_semaphore selectSemaphore;

    /* Indicate whether the NIC is part of the 'select' list.*/
    fm_bool inSelectList;

    /* Indicate whether the NIC is part of the 'select' list.*/
    fm_bool isWaitingOnSemaphore;

    /* Contains mac addr of the NIC used by the switch */
    unsigned char          nicMacAddr[6];
} fm_nic;


/* NIC function prototypes */
fm_status fmNicGetDeviceName(fm_int sw, fm_char *devName);
fm_status fmNicSetDeviceName(fm_int sw, fm_char *devName);
fm_status fmNicPacketHandlingInitialize(void);
fm_status fmNicSendPackets(fm_int sw);
fm_status fmNicIsDeviceReady(fm_int sw);

#endif /* __FM_FM_GENERIC_NIC_H */
