/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_regs_cache.h
 * Creation Date:   Dec 4, 2008
 * Description:     Definitions specific to the FM4000-family of switch chips.
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

#ifndef __FM_FM4000_REGS_CACHE_H
#define __FM_FM4000_REGS_CACHE_H

/* Register cache for switch */
enum
{
    FM4000_CACHE_TRAP_GLORT,  /* Must be cached for fibm */
    FM4000_CACHE_LCI_CFG,     /* Must be cached for fibm */

    FM4000_SW_REG_CACHE_MAX,
};

/* Per port register cache */
enum
{
    FM4000_CACHE_PORT_CFG_1,  /* Must be cached for fibm */
    FM4000_CACHE_PORT_CFG_2,
    FM4000_CACHE_PORT_CFG_3,  /* Must be cached for fibm */
    FM4000_CACHE_PORT_CFG_ISL,
    FM4000_CACHE_MAC_CFG_1,
    FM4000_CACHE_MAC_CFG_2,
    FM4000_CACHE_MAC_CFG_3,
    FM4000_CACHE_MAC_CFG_5,
    FM4000_CACHE_MAC_CFG_6,
    FM4000_CACHE_TX_VPRI_MAP_1,
    FM4000_CACHE_TX_VPRI_MAP_2,
    FM4000_CACHE_PARSE_CFG,
    FM4000_CACHE_MAC_VLAN_ETYPE_1,
    FM4000_CACHE_MAC_VLAN_ETYPE_2,   /* Must be cached for fibm */
    FM4000_CACHE_PCS_IM,
    FM4000_CACHE_CM_RX_SMP_PAUSE_WM_SMP0,
    FM4000_CACHE_CM_RX_SMP_PAUSE_WM_SMP1,
    FM4000_CACHE_RX_VPRI_MAP_w0,     /* Must be consecutive for 64-bit */
    FM4000_CACHE_RX_VPRI_MAP_w1,

    FM4000_PORT_REG_CACHE_MAX,
};

/* Per trigger register cache */
enum
{
    FM4000_CACHE_TRIGGER_CONDITION_CFG,
    FM4000_CACHE_TRIGGER_CONDITION_PARAM,
    FM4000_CACHE_TRIGGER_CONDITION_FFU,
    FM4000_CACHE_TRIGGER_CONDITION_TYPE,
    FM4000_CACHE_TRIGGER_CONDITION_GLORT,
    FM4000_CACHE_TRIGGER_CONDITION_RX,
    FM4000_CACHE_TRIGGER_CONDITION_TX,
    FM4000_CACHE_TRIGGER_CONDITION_AMASK_1,
    FM4000_CACHE_TRIGGER_CONDITION_AMASK_2,

    FM4000_TRIG_REG_CACHE_MAX,
};

/* Per port cache */
typedef struct
{
    fm_uint32   reg[FM4000_PORT_REG_CACHE_MAX];
} fm_fm4000PortRegCache;

/* Per trigger cache */
typedef struct
{
    fm_uint32   reg[FM4000_TRIG_REG_CACHE_MAX];
} fm_fm4000TrigRegCache;


/* Register cache structure */
typedef struct _fm_fm4000RegisterCache
{
    /* Port registers cache, index by physical port */
    fm_fm4000PortRegCache  port[FM4000_MAX_PORT + 1];

    /* Trigger registers cache, index by trig id */
    fm_fm4000TrigRegCache  trig[FM4000_MAX_HW_TRIGGERS];

    /* switch registers cache */
    fm_uint32              reg[FM4000_SW_REG_CACHE_MAX];

    /* This is for saving the hardware read/write pointers and overide
     * the switch read/write pointers with cache functions
     */
    fm_status (*WriteUINT32)(fm_int sw, fm_uint reg, fm_uint32 value);
    fm_status (*ReadUINT32)(fm_int sw, fm_uint reg, fm_uint32 *value);
    fm_status (*MaskUINT32)(fm_int sw, fm_uint reg, fm_uint32 mask, fm_bool on);
    fm_status (*WriteUINT32Mult)(fm_int sw, fm_uint reg, fm_int count, fm_uint32 *ptr);
    fm_status (*ReadUINT32Mult)(fm_int sw, fm_uint reg, fm_int count, fm_uint32 *value);

    fm_status (*WriteUINT64)(fm_int sw, fm_uint reg, fm_uint64 value);
    fm_status (*ReadUINT64)(fm_int sw, fm_uint reg, fm_uint64 *value);
    fm_status (*WriteUINT64Mult)(fm_int sw, fm_uint reg, fm_int count, fm_uint64 *ptr);
    fm_status (*ReadUINT64Mult)(fm_int sw, fm_uint reg, fm_int count, fm_uint64 *value);

    fm_status (*ReadScatterGather)(fm_int sw, fm_int nEntries, fm_scatterGatherListEntry *sgList);
    fm_status (*WriteScatterGather)(fm_int sw, fm_int nEntries, fm_scatterGatherListEntry *sgList);

} fm_fm4000RegisterCache;

fm_status fm4000RegCacheInit(fm_int sw);
fm_status fm4000RegCacheCleanup(fm_int sw);
fm_status fm4000RegCacheVerify(fm_int sw);

fm_status fm4000CacheReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);
fm_status fm4000CacheWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 value);
fm_status fm4000CacheMaskCSR(fm_int sw, fm_uint reg, fm_uint32 mask, fm_bool on);
fm_status fm4000CacheReadCSRMult(fm_int sw, fm_uint32  addr, fm_int n, fm_uint32 *value);
fm_status fm4000CacheWriteCSRMult(fm_int sw, fm_uint32 addr, fm_int n, fm_uint32 *value);

fm_status fm4000CacheReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value);
fm_status fm4000CacheWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value);
fm_status fm4000CacheReadCSR64Mult(fm_int sw, fm_uint32  addr, fm_int n, fm_uint64 *value);
fm_status fm4000CacheWriteCSR64Mult(fm_int sw, fm_uint32 addr, fm_int n, fm_uint64 *value);

#endif /* __FM_FM4000_REGS_CACHE_H */
