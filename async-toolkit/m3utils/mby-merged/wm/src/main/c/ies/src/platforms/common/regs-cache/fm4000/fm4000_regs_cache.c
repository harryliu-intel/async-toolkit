/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_regs_cache.c
 * Creation Date:   Dec 4, 2008
 * Description:     FM4000 FIBM implementation
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2013 Intel Corporation. All Rights Reserved.
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


#include <fm_sdk_fm4000_int.h>
#include <platforms/common/regs-cache/fm4000/fm4000_regs_cache.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
#define DO_SANITY_CHECK
#define SANITY_CHECK_INVALID 0x12345761
#define MAX_SG_LIST \
        (FM4000_SW_REG_CACHE_MAX + \
         FM4000_PORT_REG_CACHE_MAX*(FM4000_MAX_PORT+1) + \
         FM4000_TRIG_REG_CACHE_MAX*FM4000_MAX_HW_TRIGGERS)


/* These are for helping to get the address based on an index
 * So we can just do a loop instead of add one entry at a time
 */
#define GET_PORT_ADDR(index, port) \
    (portRegCacheList[index].base + portRegCacheList[index].mult*port)
#define GET_TRIG_ADDR(index, trig) \
    (trigRegCacheList[index].base + trigRegCacheList[index].mult*trig)

typedef struct
{
    fm_uint32   base;
    fm_uint32   mult;
} fm_fm4000RegBase;

/* NOTE: There are 3 steps to add a cached entry
 *       1. Add an cached entry in the correct reg_cache.h header
 *       2. Add the corresponding register in the xxCacheList
 *       3. Add code in fm4000RegCacheFindIndex to return the correct index
 *       Also enable DO_SANITY_CHECK to do some sanity check
 */

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_uint32 swRegCacheList[] = {
    FM4000_TRAP_GLORT,
    FM4000_LCI_CFG,
};

fm_fm4000RegBase portRegCacheList[] = {
    {FM4000_PORT_CFG_1(0), 1},
    {FM4000_PORT_CFG_2(0), 1},
    {FM4000_PORT_CFG_3(0), 1},
    {FM4000_PORT_CFG_ISL(0), 1},
    {FM4000_TX_VPRI_MAP_1(0), 0x400},
    {FM4000_TX_VPRI_MAP_2(0), 0x400},
    {FM4000_MAC_CFG_1(0), 0x400},
    {FM4000_MAC_CFG_2(0), 0x400},
    {FM4000_MAC_CFG_3(0), 0x400},
    {FM4000_MAC_CFG_5(0), 0x400},
    {FM4000_MAC_CFG_6(0), 0x400},
    {FM4000_PARSE_CFG(0), 0x400},
    {FM4000_MAC_VLAN_ETYPE_1(0), 0x400},
    {FM4000_MAC_VLAN_ETYPE_2(0), 0x400},
    {FM4000_PCS_IM(0), 0x400},
    {FM4000_CM_RX_SMP_PAUSE_WM(0,0), 2},
    {FM4000_CM_RX_SMP_PAUSE_WM(0,1), 2},
    {FM4000_RX_VPRI_MAP(0,0), 2},
    {FM4000_RX_VPRI_MAP(0,1), 2},
};

fm_fm4000RegBase trigRegCacheList[] = {
    {FM4000_TRIGGER_CONDITION_CFG(0), 1},
    {FM4000_TRIGGER_CONDITION_PARAM(0), 1},
    {FM4000_TRIGGER_CONDITION_FFU(0), 1},
    {FM4000_TRIGGER_CONDITION_TYPE(0), 1},
    {FM4000_TRIGGER_CONDITION_GLORT(0), 1},
    {FM4000_TRIGGER_CONDITION_RX(0), 1},
    {FM4000_TRIGGER_CONDITION_TX(0), 1},
    {FM4000_TRIGGER_CONDITION_AMASK_1(0), 1},
    {FM4000_TRIGGER_CONDITION_AMASK_2(0), 1},
};

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fm4000RegCacheFindIndex
 * \ingroup intRegCache
 *
 * \desc            Return the cached index for a given cached address.
 *
 * \param[in]       addr is the register index.
 *
 * \param[out]      port points to caller-allocated storage where this function 
 *                  should place the port number, or -1 if not a port register.
 *
 * \param[out]      trig points to caller-allocated storage where this function 
 *                  should place the trigger number, or -1 if not a trigger 
 *                  register.
 *
 * \return          the cached index or -1 if not found.
 *
 *****************************************************************************/
static fm_int fm4000RegCacheFindIndex(fm_uint addr, fm_int *port, fm_int *trig)
{
    fm_uint temp;

    /* By default, it is not port register, unless specify it later */
    *port = -1;
    *trig = -1;

    if (addr < FM4000_LSM_BASE)     /* HSM block*/
    {
        switch (addr)
        {
            case FM4000_LCI_CFG:
                return FM4000_CACHE_LCI_CFG;
        }
        return -1;
    }

    if (addr < FM4000_EPL_BASE)    /* LSM Block */
    {
        return -1;
    }

    if (addr < FM4000_SCHED2_BASE)    /* EPL Block */
    {
        temp = addr&0xFF;

        /* We know the pattern of how the registers are mapped */
        if (temp == (FM4000_PCS_IM(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_PCS_IM;
        }

        if (temp == (FM4000_TX_VPRI_MAP_1(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_TX_VPRI_MAP_1;
        }

        if (temp == (FM4000_TX_VPRI_MAP_2(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_TX_VPRI_MAP_2;
        }

        if (temp == (FM4000_MAC_CFG_1(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_CFG_1;
        }

        if (temp == (FM4000_MAC_CFG_2(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_CFG_2;
        }

        if (temp == (FM4000_MAC_CFG_3(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_CFG_3;
        }

        if (temp == (FM4000_MAC_CFG_5(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_CFG_5;
        }

        if (temp == (FM4000_MAC_CFG_6(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_CFG_6;
        }

        if (temp == (FM4000_PARSE_CFG(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_PARSE_CFG;
        }

        if (temp == (FM4000_MAC_VLAN_ETYPE_1(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_VLAN_ETYPE_1;
        }

        if (temp == (FM4000_MAC_VLAN_ETYPE_2(0)&0xFF))
        {
            *port = (addr>>10)&0x3F;
            return FM4000_CACHE_MAC_VLAN_ETYPE_2;
        }
        return -1;
    }
    if (addr < FM4000_MSB_BASE)    /* SCHED2 Block */
    {
        return -1;
    }
    if (addr < FM4000_PORT_STATS_BASE)    /* MSB Block */
    {
        return -1;
    }
    if (addr < FM4000_ASSOC_BASE)    /* Handler Block */
    {
        switch (addr)
        {
            case FM4000_TRAP_GLORT:
                return FM4000_CACHE_TRAP_GLORT;
        }
        return -1;
    }
    if (addr < FM4000_CM_BASE)    /* Assoc Block */
    {
        if ( (addr >= FM4000_PORT_CFG_1(0)) &&
             (addr <= FM4000_PORT_CFG_1(24)) )
        {
            *port = addr - FM4000_PORT_CFG_1(0);
            return FM4000_CACHE_PORT_CFG_1;
        }
        if ( (addr >= FM4000_PORT_CFG_2(0)) &&
             (addr <= FM4000_PORT_CFG_2(24)) )
        {
            *port = addr - FM4000_PORT_CFG_2(0);
            return FM4000_CACHE_PORT_CFG_2;
        }
        if ( (addr >= FM4000_PORT_CFG_3(0)) &&
             (addr <= FM4000_PORT_CFG_3(24)) )
        {
            *port = addr - FM4000_PORT_CFG_3(0);
            return FM4000_CACHE_PORT_CFG_3;
        }
        if ( (addr >= FM4000_PORT_CFG_ISL(0)) &&
             (addr <= FM4000_PORT_CFG_ISL(24)) )
        {
            *port = addr - FM4000_PORT_CFG_ISL(0);
            return FM4000_CACHE_PORT_CFG_ISL;
        }

        if ( (addr >= FM4000_RX_VPRI_MAP(0,0)) &&
             (addr <= FM4000_RX_VPRI_MAP(24,1)) )
        {
            *port = addr - FM4000_RX_VPRI_MAP(0,0);
            temp = *port%2;
            *port /= 2;
            return (FM4000_CACHE_RX_VPRI_MAP_w0 + temp);
        }
        return -1;
    }
    if (addr < FM4000_LAG_BASE)    /* CM Block */
    {
        if ( (addr >= FM4000_CM_RX_SMP_PAUSE_WM(0,0)) &&
             (addr <= FM4000_CM_RX_SMP_PAUSE_WM(24,1)) )
        {
            *port = (addr - FM4000_CM_RX_SMP_PAUSE_WM(0,0)) / 2;
            return FM4000_CACHE_CM_RX_SMP_PAUSE_WM_SMP0 +
                ( (addr - FM4000_CM_RX_SMP_PAUSE_WM(0,0)) % 2);
        }
        return -1;
    }
    if (addr < FM4000_TRIG_BASE)    /* LAG Block */
    {
        return -1;
    }
    if (addr < FM4000_GLORT_BASE)    /* Trigger Block */
    {
        if ( (addr >= FM4000_TRIGGER_CONDITION_CFG(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_CFG(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_CFG(0);
            return FM4000_CACHE_TRIGGER_CONDITION_CFG;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_PARAM(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_PARAM(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_PARAM(0);
            return FM4000_CACHE_TRIGGER_CONDITION_PARAM;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_FFU(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_FFU(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_FFU(0);
            return FM4000_CACHE_TRIGGER_CONDITION_FFU;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_TYPE(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_TYPE(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_TYPE(0);
            return FM4000_CACHE_TRIGGER_CONDITION_TYPE;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_GLORT(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_GLORT(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_GLORT(0);
            return FM4000_CACHE_TRIGGER_CONDITION_GLORT;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_RX(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_RX(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_RX(0);
            return FM4000_CACHE_TRIGGER_CONDITION_RX;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_TX(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_TX(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_TX(0);
            return FM4000_CACHE_TRIGGER_CONDITION_TX;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_AMASK_1(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_AMASK_1(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_AMASK_1(0);
            return FM4000_CACHE_TRIGGER_CONDITION_AMASK_1;
        }
        if ( (addr >= FM4000_TRIGGER_CONDITION_AMASK_2(0)) &&
             (addr <= FM4000_TRIGGER_CONDITION_AMASK_2(63)) )
        {
            *trig = addr - FM4000_TRIGGER_CONDITION_AMASK_2(0);
            return FM4000_CACHE_TRIGGER_CONDITION_AMASK_2;
        }


        return -1;
    }

    return -1;


}   /* end fm4000RegCacheFindIndex */



static fm_status InitSwRegCache(fm_int      sw,
                                fm_scatterGatherListEntry *sgList,
                                fm_int *    sgListCnt,
                                fm_int      sgListSize,
                                fm_uint32   addr)
{
    fm_int      off;
    fm_status   err = FM_OK;
    fm_int      port;
    fm_int      trig;
    fm_fm4000RegisterCache *switchRegCache;

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    off = fm4000RegCacheFindIndex(addr, &port, &trig);

    if (off < 0 || off >= FM4000_SW_REG_CACHE_MAX)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheInit: invalid switch reg off %d\n",
                     off);
        return FM_FAIL;
    }

    if (*sgListCnt < 0 || *sgListCnt >= sgListSize)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "sgListCnt (%d) out of bounds\n",
                     *sgListCnt);
        return FM_FAIL;
    }

    sgList[*sgListCnt].addr = addr;
    sgList[*sgListCnt].data = &switchRegCache->reg[off];
    sgList[*sgListCnt].count = 1;
    (*sgListCnt)++;

    return err;

}   /* end InitSwRegCache */




static fm_status InitPortRegCache(fm_int    sw,
                                  fm_scatterGatherListEntry *sgList,
                                  fm_int *  sgListCnt,
                                  fm_int    sgListSize,
                                  fm_uint32 addr,
                                  fm_int    port)
{
    fm_int                  off;
    fm_int                  portIdx;
    fm_int                  trigIdx;
    fm_fm4000RegisterCache *switchRegCache;

    off = fm4000RegCacheFindIndex(addr, &portIdx, &trigIdx);

    if (port != portIdx)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "InitPortRegCache: port %d not the same as "
                     "portIdx %d at addr 0x%08x\n",
                     port,
                     portIdx,
                     addr);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, FM_FAIL);
    }

    if (off < 0)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheFindIndex returned %d\n",
                     off);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, FM_FAIL);
    }

    if (*sgListCnt < 0 || *sgListCnt >= sgListSize)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "sgListCnt (%d) out of bounds\n",
                     *sgListCnt);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, FM_FAIL);
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    sgList[*sgListCnt].addr = addr;
    sgList[*sgListCnt].data = &switchRegCache->port[portIdx].reg[off];
    sgList[*sgListCnt].count = 1;
    (*sgListCnt)++;

    return FM_OK;

}   /* end InitPortRegCache */




static fm_status InitTrigRegCache(fm_int    sw,
                                  fm_scatterGatherListEntry *sgList,
                                  fm_int *  sgListCnt,
                                  fm_int    sgListSize,
                                  fm_uint32 addr,
                                  fm_int    trig)
{
    fm_int                  off;
    fm_int                  portIdx;
    fm_int                  trigIdx;
    fm_fm4000RegisterCache *switchRegCache;

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    off = fm4000RegCacheFindIndex(addr, &portIdx, &trigIdx);

    if (trig != trigIdx)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                 "InitPortRegCache: trig %d not the same as "
                 "trigIdx %d at addr 0x%08x\n", trig, trigIdx, addr);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, FM_FAIL);
    }

    if ( (off < 0) || (off >= FM4000_TRIG_REG_CACHE_MAX) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheFindIndex return value out of range: %d\n",
                     off);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, FM_FAIL);
    }

    if (*sgListCnt < 0 || *sgListCnt >= sgListSize)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "sgListCnt (%d) out of bounds\n",
                     *sgListCnt);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, FM_FAIL);
    }

    sgList[*sgListCnt].addr = addr;
    sgList[*sgListCnt].data = &switchRegCache->trig[trigIdx].reg[off];
    sgList[*sgListCnt].count = 1;
    (*sgListCnt)++;

    return FM_OK;

}   /* end InitTrigRegCache */




static fm_status VerifySwRegCache(fm_int sw, fm_uint32 addr)
{
    fm_int      off;
    fm_status   err = FM_OK;
    fm_int      port;
    fm_int      trig;
    fm_uint32   val;
    fm_fm4000RegisterCache *switchRegCache;

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    off = fm4000RegCacheFindIndex(addr, &port, &trig);

    if (off < 0 || off >= FM4000_SW_REG_CACHE_MAX)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                 "VerifySwRegCache: invalid switch reg off %d\n", off);
        return FM_FAIL;
    }

    err = switchRegCache->ReadUINT32(sw, addr, &val);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    if (switchRegCache->reg[off] != val)
    {
        FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                 "Cached value 0x%08x at addr 0x%08x is different than hw value 0x%08x\n",
                  switchRegCache->reg[off], val, addr);
        return FM_FAIL;
    }

ABORT:
    return err;

}   /* end VerifySwRegCache */



static fm_status VerifyPortRegCache(fm_int      sw,
                                    fm_uint32   addr,
                                    fm_int      port)
{
    fm_status   err = FM_OK;
    fm_int      off;
    fm_int      portIdx;
    fm_int      trigIdx;
    fm_uint32   val;
    fm_fm4000RegisterCache *switchRegCache;

    off = fm4000RegCacheFindIndex(addr, &portIdx, &trigIdx);

    if (off < 0 || off >= FM4000_PORT_REG_CACHE_MAX)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "VerifyPortRegCache: invalid port reg off %d\n",
                     off);
        return FM_FAIL;
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (port!=portIdx)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                 "VerifyPortRegCache: port %d not the same as "
                 "portIdx %d at addr 0x%08x\n", port, portIdx, addr);
    }

    err = switchRegCache->ReadUINT32(sw, addr, &val);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    if (switchRegCache->port[portIdx].reg[off] != val)
    {
        FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                 "Port cached value 0x%08x at addr 0x%08x is different than hw value 0x%08x\n",
                  switchRegCache->port[portIdx].reg[off], val, addr);
        return FM_FAIL;
    }

ABORT:
    return err;

}   /* end VerifyPortRegCache */



static fm_status VerifyTrigRegCache(fm_int      sw,
                                    fm_uint32   addr,
                                    fm_int      trig)
{
    fm_status   err = FM_OK;
    fm_int      off;
    fm_int      portIdx;
    fm_int      trigIdx;
    fm_uint32   val;
    fm_fm4000RegisterCache *switchRegCache;

    off = fm4000RegCacheFindIndex(addr, &portIdx, &trigIdx);

    if (off < 0 || off >= FM4000_TRIG_REG_CACHE_MAX)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "VerifyTrigRegCache: invalid switch reg off %d\n",
                     off);
        return FM_FAIL;
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (trig!=trigIdx)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                 "VerifyTrigRegCache: trig %d not the same as "
                 "trigIdx %d at addr 0x%08x\n", trig, trigIdx, addr);
    }

    err = switchRegCache->ReadUINT32(sw, addr, &val);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    if (switchRegCache->trig[trig].reg[off] != val)
    {
        FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                 "Trigger cached value 0x%08x at addr 0x%08x is different than hw value 0x%08x\n",
                  switchRegCache->trig[trig].reg[off], val, addr);
        return FM_FAIL;
    }

ABORT:
    return err;

}   /* end VerifyTrigRegCache */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fm4000RegCacheInit
 * \ingroup intRegCache
 *
 * \desc            Perform initialization to enable caching.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000RegCacheInit(fm_int sw)
{

    fm_switch           *switchPtr;
    fm_status           err;
    fm_int              physPort;
    fm_int              trig;
    fm_int              cnt;
    fm_scatterGatherListEntry sgList[MAX_SG_LIST];
    fm_int              sgListCnt;
    fm_fm4000RegisterCache *switchRegCache;

    switchRegCache = fmAlloc(sizeof(fm_fm4000RegisterCache));
    if (switchRegCache == NULL)
    {
        return FM_ERR_NO_MEM;
    }

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    fmRootApi->fmSwRegCache[sw] = (void *) switchRegCache;

#ifdef DO_SANITY_CHECK
    for (cnt = 0 ; cnt < FM4000_SW_REG_CACHE_MAX ; cnt++)
    {
        switchRegCache->reg[cnt] = SANITY_CHECK_INVALID;
    }
    for (cnt = 0 ; cnt < FM4000_PORT_REG_CACHE_MAX ; cnt++)
    {
        for ( physPort = 0 ; physPort <= switchPtr->maxPhysicalPort ; physPort++ )
        {
            switchRegCache->port[physPort].reg[cnt] = SANITY_CHECK_INVALID;
        }
    }
    for (cnt = 0 ; cnt < FM4000_TRIG_REG_CACHE_MAX ; cnt++)
    {
        for ( trig = 0 ; trig < FM4000_MAX_HW_TRIGGERS ; trig++ )
        {
            switchRegCache->trig[trig].reg[cnt] = SANITY_CHECK_INVALID;
        }
    }
#endif

    sgListCnt = 0;

    for (cnt = 0 ; cnt < FM4000_SW_REG_CACHE_MAX ; cnt++)
    {
        err = InitSwRegCache(sw,
                             sgList,
                             &sgListCnt,
                             MAX_SG_LIST,
                             swRegCacheList[cnt]);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    for ( physPort = 0 ; physPort <= switchPtr->maxPhysicalPort ; physPort++ )
    {
        for (cnt = 0 ; cnt < FM4000_PORT_REG_CACHE_MAX ; cnt++)
        {
            err = InitPortRegCache(sw,
                                   sgList,
                                   &sgListCnt,
                                   MAX_SG_LIST,
                                   GET_PORT_ADDR(cnt, physPort),
                                   physPort);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
        }
    }

    for ( trig = 0 ; trig < FM4000_MAX_HW_TRIGGERS ; trig++ )
    {
        for (cnt = 0 ; cnt < FM4000_TRIG_REG_CACHE_MAX ; cnt++)
        {
            err = InitTrigRegCache(sw,
                                   sgList,
                                   &sgListCnt,
                                   MAX_SG_LIST,
                                   GET_TRIG_ADDR(cnt, trig),
                                   trig);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
        }
    }

    if (sgListCnt < 0 || sgListCnt > MAX_SG_LIST)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "sgListCnt (%d) out of bounds\n",
                     sgListCnt);
        err = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    if (sgListCnt)
    {
        err = fmReadScatterGather(sw, sgListCnt, sgList);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

#ifdef DO_SANITY_CHECK
    for (cnt = 0 ; cnt < FM4000_SW_REG_CACHE_MAX ; cnt++)
    {
        if (switchRegCache->reg[cnt] == SANITY_CHECK_INVALID)
        {
            FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
             "fm4000RegCacheInit: Entry %d of reg cache is not inited\n", cnt);
        }
    }
    for (cnt = 0 ; cnt < FM4000_PORT_REG_CACHE_MAX ; cnt++)
    {
        for ( physPort = 0 ; physPort <= switchPtr->maxPhysicalPort ; physPort++ )
        {
            if (switchRegCache->port[physPort].reg[cnt] == SANITY_CHECK_INVALID)
            {
                FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                        "fm4000RegCacheInit: Entry %d of port %d "
                        "reg cache is not inited\n",
                        cnt, physPort);
            }
        }
    }
    for (cnt = 0 ; cnt < FM4000_TRIG_REG_CACHE_MAX ; cnt++)
    {
        for ( trig = 0 ; trig < FM4000_MAX_HW_TRIGGERS ; trig++ )
        {
            if (switchRegCache->trig[trig].reg[cnt] == SANITY_CHECK_INVALID)
            {
                FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                        "fm4000RegCacheInit: Entry %d of trig %d "
                        "reg cache is not inited\n",
                        cnt, trig);
                /* Reset to zero, so it won't be written to hardware */
                switchRegCache->trig[trig].reg[cnt] = 0;
            }
        }
    }
#endif

    /* Save the hardware read/write pointers */
    switchRegCache->WriteUINT32      = switchPtr->WriteUINT32;
    switchRegCache->ReadUINT32       = switchPtr->ReadUINT32;
    switchRegCache->MaskUINT32       = switchPtr->MaskUINT32;
    switchRegCache->WriteUINT32Mult  = switchPtr->WriteUINT32Mult;
    switchRegCache->ReadUINT32Mult   = switchPtr->ReadUINT32Mult;
    switchRegCache->WriteUINT64      = switchPtr->WriteUINT64;
    switchRegCache->ReadUINT64       = switchPtr->ReadUINT64;
    switchRegCache->WriteUINT64Mult  = switchPtr->WriteUINT64Mult;
    switchRegCache->ReadUINT64Mult   = switchPtr->ReadUINT64Mult;

    /* Save the basic read/write for API to get uncached value */ 
    switchPtr->ReadUncacheUINT32         = switchPtr->ReadUINT32;
    switchPtr->ReadUncacheUINT32Mult     = switchPtr->ReadUINT32Mult;
    switchPtr->ReadUncacheUINT64         = switchPtr->ReadUINT64;
    switchPtr->ReadUncacheUINT64Mult     = switchPtr->ReadUINT64Mult;

    /* Now override the switch read/write with the cache functions  */ 
    switchPtr->WriteUINT32        = fm4000CacheWriteCSR;
    switchPtr->ReadUINT32         = fm4000CacheReadCSR;
    switchPtr->MaskUINT32         = fm4000CacheMaskCSR;
    switchPtr->WriteUINT32Mult    = fm4000CacheWriteCSRMult;
    switchPtr->ReadUINT32Mult     = fm4000CacheReadCSRMult;
    switchPtr->WriteUINT64        = fm4000CacheWriteCSR64;
    switchPtr->ReadUINT64         = fm4000CacheReadCSR64;
    switchPtr->WriteUINT64Mult    = fm4000CacheWriteCSR64Mult;
    switchPtr->ReadUINT64Mult     = fm4000CacheReadCSR64Mult;
    
ABORT:
    return err;
} /* end fm4000RegCacheInit */




/*****************************************************************************/
/** fm4000RegCacheCleanup
 * \ingroup intRegCache
 *
 * \desc            Perform cleanup when a switch goes down.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000RegCacheCleanup(fm_int sw)
{

    fm_switch           *switchPtr;
    fm_fm4000RegisterCache *switchRegCache;

    switchPtr = GET_SWITCH_PTR(sw);
    if (switchPtr == NULL)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (switchRegCache != NULL)
    {
        /* Restore back the switch read/write with the non-cache functions  */
        switchPtr->WriteUINT32        = switchRegCache->WriteUINT32;
        switchPtr->ReadUINT32         = switchRegCache->ReadUINT32;
        switchPtr->MaskUINT32         = switchRegCache->MaskUINT32;
        switchPtr->WriteUINT32Mult    = switchRegCache->WriteUINT32Mult;
        switchPtr->ReadUINT32Mult     = switchRegCache->ReadUINT32Mult;
        switchPtr->WriteUINT64        = switchRegCache->WriteUINT64;
        switchPtr->ReadUINT64         = switchRegCache->ReadUINT64;
        switchPtr->WriteUINT64Mult    = switchRegCache->WriteUINT64Mult;
        switchPtr->ReadUINT64Mult     = switchRegCache->ReadUINT64Mult;

        fmFree(switchRegCache);
    }
    
    return FM_OK;

} /* end fm4000RegCacheCleanup */




/*****************************************************************************/
/** fm4000RegCacheVerify
 * \ingroup intRegCache
 *
 * \desc            Verify if cached entries in sync with hardware.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000RegCacheVerify(fm_int sw)
{

    fm_int              physPort;
    fm_switch          *switchPtr;
    fm_int              trig;
    fm_int              cnt;
    fm_status           err;
    fm_status           retErr = FM_OK;

    switchPtr = GET_SWITCH_PTR(sw);

    for (cnt = 0 ; cnt < FM4000_SW_REG_CACHE_MAX ; cnt++)
    {
        err = VerifySwRegCache(sw, swRegCacheList[cnt]);
        if (err != FM_OK && retErr == FM_OK)
        {
            retErr = err;
        }
    }

    for ( physPort = 0 ; physPort <= switchPtr->maxPhysicalPort ; physPort++ )
    {
        for (cnt = 0 ; cnt < FM4000_PORT_REG_CACHE_MAX ; cnt++)
        {
            err = VerifyPortRegCache(sw,
                    GET_PORT_ADDR(cnt, physPort), physPort);
            if (err != FM_OK && retErr == FM_OK)
            {
                retErr = err;
            }
        }
    }

    for ( trig = 0 ; trig < FM4000_MAX_HW_TRIGGERS ; trig++ )
    {
        for (cnt = 0 ; cnt < FM4000_TRIG_REG_CACHE_MAX ; cnt++)
        {
            err = VerifyTrigRegCache(sw,
                    GET_TRIG_ADDR(cnt, trig), trig);
            if (err != FM_OK && retErr == FM_OK)
            {
                retErr = err;
            }
        }
    }
    
    return FM_OK;
} /* end fm4000RegCacheVerify */



/*****************************************************************************/
/** fm4000RegCacheRead
 * \ingroup intRegCache
 *
 * \desc            Read a cache register
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if no cache entry found
 *
 *****************************************************************************/
fm_status fm4000RegCacheRead(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    fm_switch          *switchPtr;
    fm_int              off;
    fm_int              port;
    fm_int              trig;
    fm_fm4000RegisterCache *switchRegCache;

    switchPtr = GET_SWITCH_PTR(sw);

    off = fm4000RegCacheFindIndex(addr, &port, &trig);

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (off < 0)
    {
        return FM_ERR_NOT_FOUND;
    }

    /* Reg is in trigger block */
    if (trig >= 0)
    {
        if (off < FM4000_TRIG_REG_CACHE_MAX)
        {
            *value = switchRegCache->trig[trig].reg[off];
            return FM_OK;
        }

        return FM_ERR_NOT_FOUND;
    }

    if (port < 0)
    {
        /* Not per port reg */
        if (off < FM4000_SW_REG_CACHE_MAX)
        {
            *value = switchRegCache->reg[off];
            return FM_OK;
        }
        return FM_ERR_NOT_FOUND;
    }

    if (port > switchPtr->maxPhysicalPort)
    {
        /* Should not get here if working properly */
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheRead: Invalid port %d for addr 0x%08x\n", port, addr);
        return FM_ERR_NOT_FOUND;
    }

    if (off < FM4000_PORT_REG_CACHE_MAX)
    {
        *value = switchRegCache->port[port].reg[off];
        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;
        
} /* end fm4000RegCacheRead */



/*****************************************************************************/
/** fm4000RegCacheWrite
 * \ingroup intRegCache
 *
 * \desc            Update a cache register
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the register address to read
 *
 * \param[out]      value is the data to write.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if no cache entry found
 *
 *****************************************************************************/
fm_status fm4000RegCacheWrite(fm_int sw, fm_uint32 addr, fm_uint32 value)
{
    fm_switch          *switchPtr;
    fm_int              off;
    fm_int              port;
    fm_int              trig;
    fm_fm4000RegisterCache *switchRegCache;

    switchPtr = GET_SWITCH_PTR(sw);

    off = fm4000RegCacheFindIndex(addr, &port, &trig);

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (off < 0)
    {
        return FM_ERR_NOT_FOUND;
    }

    /* Reg is in trigger block */
    if (trig >= 0)
    {
        if (trig >= FM4000_MAX_HW_TRIGGERS)
        {
            /* Should not get here if working properly */
            FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheRead: Invalid trig %d for addr 0x%08x\n", trig, addr);
        }

        if (off < FM4000_TRIG_REG_CACHE_MAX)
        {
            switchRegCache->trig[trig].reg[off] = value;
            return FM_OK;
        }

        return FM_ERR_NOT_FOUND;
    }

    if (port < 0)
    {
        /* Not per port reg */
        if (off < FM4000_SW_REG_CACHE_MAX)
        {
            switchRegCache->reg[off] = value;
            return FM_OK;
        }
        return FM_ERR_NOT_FOUND;
    }

    if (port > switchPtr->maxPhysicalPort)
    {
        /* Should not get here if working properly */
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheRead: Invalid port %d for addr 0x%08x\n", port, addr);
        return FM_ERR_NOT_FOUND;
    }

    if (off < FM4000_PORT_REG_CACHE_MAX)
    {
        switchRegCache->port[port].reg[off] = value;
        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;
        
} /* end fm4000RegCacheWrite */




/*****************************************************************************/
/** fm4000RegCacheRead64
 * \ingroup intRegCache
 *
 * \desc            Read a cache register
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if no cache entry found
 * \return          FM_ERR_INVALID_ARGUMENT if the switch index is invalid.
 *
 *****************************************************************************/
fm_status fm4000RegCacheRead64(fm_int sw, fm_uint32 addr, fm_uint64 *value)
{
    fm_switch          *switchPtr;
    fm_int              off;
    fm_int              port;
    fm_int              trig;
    fm_fm4000RegisterCache *switchRegCache;

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    off = fm4000RegCacheFindIndex(addr, &port, &trig);

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (off < 0)
    {
        return FM_ERR_NOT_FOUND;
    }

    if (port < 0)
    {
        /* Not per port reg */
        if (off < (FM4000_SW_REG_CACHE_MAX - 1))
        {
            *value = switchRegCache->reg[off+1];
            *value = (*value << 32);
            *value |= switchRegCache->reg[off];
            return FM_OK;
        }
        return FM_ERR_NOT_FOUND;
    }

    if (port > switchPtr->maxPhysicalPort)
    {
        /* Should not get here if working properly */
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheRead: Invalid port %d for addr 0x%08x\n", port, addr);
        return FM_ERR_NOT_FOUND;
    }

    if (off < (FM4000_PORT_REG_CACHE_MAX - 1))
    {
        *value = switchRegCache->port[port].reg[off+1];
        *value = (*value << 32);
        *value |= switchRegCache->port[port].reg[off];
        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;
        
} /* end fm4000RegCacheRead64 */


/*****************************************************************************/
/** fm4000RegCacheWrite64
 * \ingroup intRegCache
 *
 * \desc            Update a cache register
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the register address to read
 *
 * \param[out]      value is the 64-bit data value to write.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if no cache entry found
 *
 *****************************************************************************/
fm_status fm4000RegCacheWrite64(fm_int sw, fm_uint32 addr, fm_uint64 value)
{
    fm_switch          *switchPtr;
    fm_int              off;
    fm_int              port;
    fm_int              trig;
    fm_fm4000RegisterCache *switchRegCache;

    switchPtr = GET_SWITCH_PTR(sw);

    off = fm4000RegCacheFindIndex(addr, &port, &trig);

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    if (off < 0)
    {
        return FM_ERR_NOT_FOUND;
    }

    if (port < 0)
    {
        /* Not per port reg */
        if (off < (FM4000_SW_REG_CACHE_MAX - 1))
        {
            switchRegCache->reg[off] = value;
            switchRegCache->reg[off+1] = (value>>32);
            return FM_OK;
        }
        return FM_ERR_NOT_FOUND;
    }

    if (port > switchPtr->maxPhysicalPort)
    {
        /* Should not get here if working properly */
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                     "fm4000RegCacheRead: Invalid port %d for addr 0x%08x\n", port, addr);
        return FM_ERR_NOT_FOUND;
    }

    if (off < (FM4000_PORT_REG_CACHE_MAX - 1))
    {
        switchRegCache->port[port].reg[off] = value;
        switchRegCache->port[port].reg[off+1] = (value>>32);
        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;
        
} /* end fm4000RegCacheWrite64 */



/*****************************************************************************/
/** fm4000CacheReadCSR
 * \ingroup intRegCache
 *
 * \desc            Read a CSR register from cached if exists
 * \note            Assume switch lock is taken appropriately.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    fm_status               status;
    fm_fm4000RegisterCache *switchRegCache;

     /* No caching for reading mult */
    status = fm4000RegCacheRead(sw, addr, value);
    if (status == FM_OK)
    {
        /* Got a cached value */
        return status;
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    return switchRegCache->ReadUINT32(sw, addr, value);

}   /* end fm4000CacheReadCSR */


/*****************************************************************************/
/** fm4000CacheWriteCSR
 * \ingroup intRegCache
 *
 * \desc            Write a CSR register, update cache if exist
 * \note            Assume switch lock is taken appropriately.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 value)
{
    fm_fm4000RegisterCache *switchRegCache;

     /* No caching for reading mult */
    (void)fm4000RegCacheWrite(sw, addr, value);

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    return switchRegCache->WriteUINT32(sw, addr, value);

}   /* end fm4000CacheWriteCSR */


/*****************************************************************************/
/** fm4000CacheMaskCSR
 * \ingroup intRegCache
 *
 * \desc            Mask on or off the bits in a single 32-bit register.
 * \note            Assume switch lock is taken appropriately.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       reg is the word offset into the switch's register file.
 *
 * \param[in]       mask is the bit mask to turn on or off.
 *
 * \param[in]       on should be TRUE to set the masked bits in the register
 *                   or FALSE to clear the masked bits in the register.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheMaskCSR(fm_int sw, fm_uint reg, fm_uint32 mask, fm_bool on)
{
    fm_status err = FM_OK;
    fm_uint32 value;

    err = fm4000CacheReadCSR(sw, reg, &value);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    if (on)
    {
        value |= mask;
    }
    else
    {
        value &= ~mask;
    }

    return fm4000CacheWriteCSR(sw, reg, value);

}   /* end fm4000CacheMaskCSR */


/*****************************************************************************/
/** fm4000CacheReadCSRMult
 * \ingroup intRegCache
 *
 * \desc            Read multiple CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an aggregate to be filled in with
 *                  the register data read. The aggregate must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheReadCSRMult(fm_int sw, fm_uint32  addr, fm_int n, fm_uint32 *value)
{
    fm_fm4000RegisterCache *switchRegCache;

    /* No caching for reading mult */
 
    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];
    return switchRegCache->ReadUINT32Mult(sw, addr, n, value);

}   /* end fm4000CacheReadCSRMult */


/*****************************************************************************/
/** fm4000CacheWriteCSRMult
 * \ingroup intRegCache
 *
 * \desc            Write multiple CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an aggregate of values to be written. The
 *                  aggregate must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheWriteCSRMult(fm_int sw, fm_uint32  addr, fm_int n, fm_uint32 *value)
{
    fm_int cnt;
    fm_fm4000RegisterCache *switchRegCache;

    for (cnt = 0 ; cnt < n ; cnt++)
    {
        (void)fm4000RegCacheWrite(sw, addr+cnt, value[cnt]);
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    return switchRegCache->WriteUINT32Mult(sw, addr, n, value);

}   /* end fm4000CacheWriteCSRMult */


/*****************************************************************************/
/** fm4000CacheReadCSR64
 * \ingroup intRegCache
 *
 * \desc            Read a 64-bit CSR register, from cache if exists.
 * \note            Assume switch lock is taken appropriately.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to storage where the 64-bit read data value
 *                  will be stored by this function.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value)
{
    fm_status status;
    fm_fm4000RegisterCache *switchRegCache;

    status = fm4000RegCacheRead64(sw, addr, value);
    if (status == FM_OK)
    {
        /* Got a cached value */
        return status;
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];
    return switchRegCache->ReadUINT64(sw, addr, value);

}   /* end fm4000CacheReadCSR64 */




/*****************************************************************************/
/** fm4000CacheWriteCSR64
 * \ingroup intRegCache
 *
 * \desc            Writes a 64-bit CSR register, and update cache.
 * \note            Assume switch lock is taken appropriately.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the 64-bit data value to write.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value)
{
    fm_fm4000RegisterCache *switchRegCache;

    (void)fm4000RegCacheWrite64(sw, addr, value);
    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];

    return switchRegCache->WriteUINT64(sw, addr, value);

}   /* end fm4000CacheWriteCSR64 */


/*****************************************************************************/
/** fm4000CacheReadCSR64Mult
 * \ingroup intRegCache
 *
 * \desc            Read multiple 64-bit CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an aggregate to be filled in with
 *                  the register data read. The aggregate must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheReadCSR64Mult(fm_int sw, fm_uint32  addr, fm_int n, fm_uint64 *value)
{
    fm_fm4000RegisterCache *switchRegCache;

    /* No caching for reading mult */
 
    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];
    return switchRegCache->ReadUINT64Mult(sw, addr, n, value);

}   /* end fm4000CacheReadCSR64Mult */


/*****************************************************************************/
/** fm4000CacheWriteCSR64Mult
 * \ingroup intRegCache
 *
 * \desc            Write multiple 64-bit CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an aggregate of values to be written. The
 *                  aggregate must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000CacheWriteCSR64Mult(fm_int sw, fm_uint32  addr, fm_int n, fm_uint64 *value)
{
    fm_int cnt;
    fm_fm4000RegisterCache *switchRegCache;

    for (cnt = 0 ; cnt < n ; cnt++)
    {
        (void)fm4000RegCacheWrite64(sw, addr+cnt, value[cnt]);
    }

    switchRegCache = (fm_fm4000RegisterCache *) fmRootApi->fmSwRegCache[sw];
    return switchRegCache->WriteUINT64Mult(sw, addr, n, value);

}   /* end fm4000CacheWriteCSR64Mult */


