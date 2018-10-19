/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_stacking.h
 * Creation Date:   June 11, 2008
 * Description:     Prototypes for managing stacked intra and extra switch
 *                  aggregate systems.
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

#ifndef __FM_FM_API_STACKING_H
#define __FM_FM_API_STACKING_H

#define FM_MAX_STACKING_FORWARDING_RULES    256

/** \ingroup macroSynonym
 * @{ */

/** A legacy synonym for ''fmGetStackForwardingRuleList''. */
#define fmGetForwardingRuleList(sw, numForwardingRules, forwardingRuleIDs, max) \
        fmGetStackForwardingRuleList( (sw), (numForwardingRules), (forwardingRuleIDs), (max) )

/** @} (end of Doxygen group) */


/**************************************************/
/** \ingroup typeStruct
 *  Used by ''fmGetStackGlortRangeExt'' and
 *  ''fmSetStackGlortRangeExt'', this structure
 *  specifies how the glort space is allocated.
 **************************************************/
typedef struct _fm_glortRange
{
    /** The first glort to use in the range. It should generally be
     *  a power of 2 to facilitate masked glort ranges for more efficient 
     *  CAM usage. Note that while this member is 32 bits wide, the hardware
     *  supports only 16-bit glorts (the member was made oversized to 
     *  accommodate future devices with wider glorts). */
    fm_uint32 glortBase;

    /** Identifies the range of glorts starting with glortBase. The mask 
    *   is an "inverse mask" where the 1 bits identify the wildcarded bits 
    *   in the glort range. The 1 bits in the mask must be contiguous. 
    *   Note that while this member is 32 bits wide, the hardware supports 
    *   only 16-bit glorts (the member was made oversized to accommodate 
    *   future devices with wider glorts). */
    fm_uint32 glortMask;

    /** Starting glort number for switch ports. */
    fm_uint32 portBaseGlort;

    /** Number of glorts reserved for switch ports. */
    fm_int    portCount;

    /** Range A length to use when creating CAM entries for switch ports. */
    fm_int    portALength;

    /** Number of glorts reserved for CPU port */
    fm_int    cpuPortCount;

    /** CPU management glort number (i.e. FIBM glort). */
    fm_uint32 cpuMgmtGlort;

    /** Starting glort number for LAGs. */
    fm_uint32 lagBaseGlort;

    /** Number of glorts reserved for LAG members. */
    fm_int    lagCount;

    /** Starting glort number for multicast groups. */
    fm_uint32 mcastBaseGlort;

    /** Number of glorts reserved for multicast groups. */
    fm_int    mcastCount;

    /** Number of glorts reserved for load balancing groups. */
    fm_uint32 lbgBaseGlort;

    /** Number of glorts reserved for load balancing groups members. */
    fm_int    lbgCount;

} fm_glortRange;


/*****************************************************************************
 * Glort space and glort management functions
 *****************************************************************************/

fm_status fmSetStackGlortRange(fm_int sw,
                               fm_uint32 glortBase, 
                               fm_uint32 mask);
fm_status fmGetStackGlortRange(fm_int sw, 
                               fm_uint32 *glortBase, 
                               fm_uint32 *mask);

fm_status fmSetStackGlortRangeExt(fm_int         sw,
                                  fm_glortRange *glortRange);
fm_status fmGetStackGlortRangeExt(fm_int         sw,
                                  fm_glortRange *glortRange);

fm_status fmGetStackGlort(fm_int sw, 
                          fm_int port, 
                          fm_uint32 *glort);

fm_status fmCreateStackLogicalPort(fm_int sw, 
                                   fm_uint32 glort, 
                                   fm_int *logicalPort);

fm_status fmDeleteStackLogicalPort(fm_int sw, 
                                   fm_int port);

fm_status fmSetStackLogicalPortState(fm_int sw, fm_int port, fm_int mode);

fm_status fmGetStackLogicalPortState(fm_int sw, fm_int port, fm_int *mode);

/*****************************************************************************
 * Glort forwarding rule management functions
 *****************************************************************************/

/****************************************************************************/
/** \ingroup typeStruct
 *
 * Defines a forwarding rule for a glort range.
 ****************************************************************************/

typedef struct _fm_forwardRule
{
    /** The base glort value in the range to be forwared by this rule. */
    fm_uint32  glort;
    
    /** A bit mask identifying the range of glorts to be forwarded by this
     *  rule. The mask is an "inverse mask" where the 1 bits identify the 
     *  wildcarded bits in the glort range. */
    fm_uint32  mask;

    /** The destination logical port to which the range of glorts should be 
    *   forwarded. */
    fm_int     logicalPort;

} fm_forwardRule;

/*****************************************************************************
 * Stacking APIs
 *****************************************************************************/

fm_status fmCreateStackForwardingRule(fm_int sw, 
                                      fm_int *forwardingRuleID, 
                                      fm_forwardRule *rule);

fm_status fmGetStackForwardingRule(fm_int sw, 
                                   fm_int forwardingRuleID, 
                                   fm_forwardRule *rule);

fm_status fmDeleteStackForwardingRule(fm_int sw, fm_int forwardingRuleID);

fm_status fmGetStackForwardingRuleFirst(fm_int sw, 
                                        fm_int *firstRuleId, 
                                        fm_forwardRule *firstRule);

fm_status fmGetStackForwardingRuleNext(fm_int sw, 
                                       fm_int currentRuleId,
                                       fm_int *nextRuleId, 
                                       fm_forwardRule *nextRule);

fm_status fmGetStackForwardingRuleList(fm_int sw,
                                       fm_int *numForwardingRules,
                                       fm_int *forwardingRuleIDs,
                                       fm_int max); 

fm_status fmAllocateStackMcastGroups(fm_int    sw,
                                    fm_uint    startGlort,
                                    fm_uint    glortCount,
                                    fm_int    *baseMcastGroup,
                                    fm_int    *numMcastGroups,
                                    fm_int    *step);

fm_status fmFreeStackMcastGroups(fm_int sw, fm_int baseMcastGroup);

fm_status fmCreateStackMcastGroup(fm_int sw, fm_int mcastGroup);

fm_status fmCreateStackMcastGroupExt(fm_int  sw,
                                     fm_int  logicalPort,
                                     fm_int *mcastGroup);

fm_status fmAllocateStackLAGs(fm_int     sw,
                              fm_uint    startGlort,
                              fm_uint    glortCount,
                              fm_int    *baseLagNumber,
                              fm_int    *numLags,
                              fm_int    *step);

fm_status fmFreeStackLAGs(fm_int sw, fm_int baseLagNumber);

fm_status fmCreateStackLAG(fm_int sw, fm_int lagNumber);

fm_status fmAllocateStackLBGs(fm_int     sw,
                              fm_uint    startGlort,
                              fm_uint    glortCount,
                              fm_int    *baseLbgNumber,
                              fm_int    *numLbgs,
                              fm_int    *step);

fm_status fmFreeStackLBGs(fm_int sw, fm_int baseLbgNumber);

fm_status fmCreateStackLBG(fm_int sw, fm_int lbgNumber);
fm_status fmCreateStackLBGExt(fm_int sw, 
                              fm_int lbgNumber, 
                              fm_LBGParams *params);

#endif /* __FM_FM_API_STACKING_H */
