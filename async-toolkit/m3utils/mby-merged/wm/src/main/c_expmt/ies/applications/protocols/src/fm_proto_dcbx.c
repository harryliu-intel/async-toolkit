/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_proto_dcbx.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX Fulcrum Interface.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#include <fm_sdk.h>

#include <lldp.h>
#include <dcbx.h>
#include <dcbx_mib.h>

#include <fm_proto_lldp.h>
#include <fm_proto_dcbx.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define VERIFY_DCBX_IS_RUNNING() \
    do { \
        if (!fmDcbxIsRunning()) { \
            FM_LOG_PRINT("DCBX module is not initialized. run 'dcbx start' to initialize.\n"); \
            return FM_FAIL; \
        } \
    } while (0)

//#undef  FM_LOG_DEBUG
//#define FM_LOG_DEBUG(_CAT,...) FM_LOG_PRINT(__VA_ARGS__)

//#undef  FM_LOG_ERROR
//#define FM_LOG_ERROR(_CAT,...) FM_LOG_PRINT(__VA_ARGS__)

#define DCBX_SWITCH_NUM         0	/* Switch-id (currently only single switch is supported) */
#define DCBX_MAX_PORTS          25	/* Number of ports in switch */
#define DCBX_MAX_ACL_RULES      512	/* Maximum acl rules in ACL slice */

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static int fmUpdatePFCStatus(int portNum, struct lldpXdot1dcbxLocPFC *locPFC);
static int fmUpdateETSStatus(int portNum, struct lldpXdot1dcbxLocETSConfiguration *locETSConfiguration);
static int fmUpdateApplicationPriorityStatus(int portNum, struct lldpXdot1dcbxLocApplicationPriority *locApplicationPriority);
static int fmUpdateCongestionNotificationStatus(int portNum, struct lldpXdot1dcbxLocCongestionNotification *locCongestionNotification);

static int fmApplyPFCSettings(int portNum, struct lldpXdot1dcbxLocPFC *locPFC);
static int fmApplyETSSettings(int portNum, struct lldpXdot1dcbxLocETSConfiguration *locETSConfiguration);
static int fmApplyApplicationPrioritySettings(int portNum, struct lldpXdot1dcbxLocApplicationPriority *locApplicationPriority);
static int fmApplyCongestionNotificationSettings(int portNum, struct lldpXdot1dcbxLocCongestionNotification *locCongestionNotification);

fm_status fmDcbxAddPort(fm_int portNum);
fm_status fmDcbxRemovePort(fm_int portNum);


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

static struct dcbxPhysicalIf fmDcbxPhysicalInterface = {
    fmUpdatePFCStatus,
    fmUpdateETSStatus,
    fmUpdateApplicationPriorityStatus,
    fmUpdateCongestionNotificationStatus,

    fmApplyPFCSettings,
    fmApplyETSSettings,
    fmApplyApplicationPrioritySettings,
    fmApplyCongestionNotificationSettings
};

/* ApplicationPriority (ACL) cache */
struct lldpXdot1dcbxLocApplicationPriorityAEPriority 
       fmDcbxAppCache[DCBX_MAX_PORTS][DCBX_TLV_MAX_APPLICATION_PRIORITIES];

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmUpdatePFCStatus
 * \ingroup dcbx
 *
 * \brief           Update PFC local data (operational state). This function
 *                  is called by the DCBX manager before it sends PFC TLV
 *                  to remote port.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locPFC points to current local PFC data, that need to be
 *                  updated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmUpdatePFCStatus(int portNum, struct lldpXdot1dcbxLocPFC *locPFC)
{
    fm_int verbose = 0;
    fm_int p, priority;

    /** It seems that we cannot denote PFC disabled with a zero, so this
     * must always be 1 **/
    locPFC->lldpXdot1dcbxLocPFCCap = 1;

    fm_int cb_mask = 0;
    fm_status ret = fmGetPortAttribute(DCBX_SWITCH_NUM, portNum, 
            FM_PORT_RX_CLASS_PAUSE, &cb_mask);
    if (ret != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed reading rx-pause on port %d\n",portNum);
        return LLDP_ERR_UNKNOWN;
    }

    fm_int cb_mode = 0;
    ret = fmGetPortAttribute(DCBX_SWITCH_NUM, portNum, 
            FM_PORT_TX_PAUSE_MODE, &cb_mode);
    if (ret != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed getting port %d PFC mode\n",portNum);
        return LLDP_ERR_UNKNOWN;
    }

    if (verbose)
        FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "Updating PFC status port %d...", portNum);

    for (p=0; p<8; p++)
    {
        if ((cb_mode == FM_PORT_TX_PAUSE_CLASS_BASED) &&
            ((cb_mask & (1<<p)) != 0))
        {
            if (verbose) FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " %d ON", p);
            locPFC->lldpXdot1dcbxLocPFCEnable[p] = 1;
        }
        else
        {
            if (verbose) FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " %d OFF", p);
            locPFC->lldpXdot1dcbxLocPFCEnable[p] = 0;
        }
    }
    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "\n");

    return LLDP_OK;
}

/*****************************************************************************/
/** fmUpdateETSStatus
 * \ingroup dcbx
 *
 * \brief           Update ETS local data (operational state). This function
 *                  is called by the DCBX manager before it sends ETS TLV
 *                  to remote port.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locETSConfiguration points to current local ETS data,
 *                  that need to be updated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmUpdateETSStatus(int portNum,
               struct lldpXdot1dcbxLocETSConfiguration *locETSConfiguration)
{
    fm_int verbose = 0;

    locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassesSupported = 8;

    /** Need the max_frame_size for ETS weight calcs
     *  Will scale all weights by 2*max_frame_size **/
    fm_int maxFrameSize;
    fm_status ret = fmGetPortAttribute(DCBX_SWITCH_NUM, portNum,
            FM_PORT_MAX_FRAME_SIZE, &maxFrameSize);
    if (ret != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed reading max-frame on port %d\n",portNum);
        return LLDP_ERR_UNKNOWN;
    }

    fm_int total = 0;
    fm_int bandwidth[8];

    for(fm_int tcIndex=0; tcIndex<8; tcIndex++)
    {
        if (verbose)
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "Updating ETS status port %d...", portNum);

        /** Read out the RX priority map **/
        fm_int vlanToTc = 0;
        fm_status ret = fmGetPortQOS(DCBX_SWITCH_NUM, portNum,
                FM_QOS_RX_PRIORITY_MAP, tcIndex<<1, &vlanToTc);
        if (ret != FM_OK)
        {
            if (verbose) FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed getting qos-rx-priority map on port %d\n",portNum);
            return LLDP_ERR_UNKNOWN;
        }
        locETSConfiguration->lldpXdot1dcbxLocETSConPriorityAssignmentTable[tcIndex]
            = vlanToTc>>1; 

        if (verbose)
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "TC %d VLANP %d", tcIndex, 
                         locETSConfiguration->lldpXdot1dcbxLocETSConPriorityAssignmentTable[tcIndex]);

        fm_int weight = 0;
        ret = fmGetPortQOS(DCBX_SWITCH_NUM, portNum, FM_QOS_TC_GROUP_WEIGHT, tcIndex, &weight);
        if (ret != FM_OK)
        {
            if (verbose) FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed getting scheduling weight on port %d\n",portNum);
            return LLDP_ERR_UNKNOWN;
        }
        bandwidth[tcIndex] = weight;
        total += bandwidth[tcIndex];

        if (verbose)
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " W %d",  
                         locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex]);

        fm_int sched_mode = 0;
        ret = fmGetPortQOS(DCBX_SWITCH_NUM, portNum,
                FM_QOS_TC_GROUP_MODE, tcIndex, &sched_mode);

        if (ret != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed getting scheduling mode on port %d\n",portNum);
            return LLDP_ERR_UNKNOWN;
        }
        locETSConfiguration->lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[tcIndex] = 0;

        if (sched_mode == FM_QOS_TC_GROUP_MODE_BALANCED)
            locETSConfiguration->lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[tcIndex] = 2;

        if (verbose)
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " M %d\n",  
                         locETSConfiguration->lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[tcIndex]);
    }

    for(fm_int tcIndex=0;tcIndex<8;tcIndex++)
    {
        locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex]
            =  (100 * bandwidth[tcIndex]) / total;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** fmUpdateApplicationPriorityStatus
 * \ingroup dcbx
 *
 * \brief           Update Application-Priority local data (operational state).
 *                  This function is called by the DCBX manager before it sends
 *                  Application-Priority TLV to remote port.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locApplicationPriority points to current local Application-
 *                  Priority data, that need to be updated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmUpdateApplicationPriorityStatus(int portNum,
               struct lldpXdot1dcbxLocApplicationPriority *locApplicationPriority)
{
   int i;

   memcpy(&locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEPriority, 
          fmDcbxAppCache[portNum], 
          sizeof(struct lldpXdot1dcbxLocApplicationPriorityAEPriority));

   for (i = 0; i<DCBX_TLV_MAX_APPLICATION_PRIORITIES; i++)
       if (fmDcbxAppCache[portNum][i].lldpXdot1dcbxLocApplicationPriorityAESelector == 0) break;

   locApplicationPriority->
   lldpXdot1dcbxLocApplicationPriorityAEPriorityNum = i;

    return LLDP_OK;
}

/*****************************************************************************/
/** fmUpdateCongestionNotificationStatus
 * \ingroup dcbx
 *
 * \brief           Update Congestion-Notification local data (operational
 *                  state). This function is called by the DCBX manager before
 *                  it sends Congestion-Notification TLV to remote port.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locCongestionNotification points to current local 
 *                  Congestion-Notification data, that need to be updated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmUpdateCongestionNotificationStatus(int portNum,
               struct lldpXdot1dcbxLocCongestionNotification *locCongestionNotification)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyPFCSettings
 * \ingroup dcbx
 *
 * \brief           Apply PFC local data (operational state) to Fulcrum device
 *                  This function is called by the DCBX manager once the
 *                  operational state is changed, due to admin change or due
 *                  to recommendation acceptance.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locPFC points to current local PFC data, that need to be
 *                  applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyPFCSettings(int portNum,
               struct lldpXdot1dcbxLocPFC *locPFC)
{
    /**
     * Note:  FM3000/4000 (Bali) devices configured PFC globally, not 
     *        per-port.  Thus, the port number variable here is ignored.
     *
     *        Enabling PFC on one port will configure the switch memory
     *        to be able to support PFC on all ports. 
     *
     *        The only per-port setting that happens is that if the PFC
     *        capabilities is > 0 then the port will be configured to generate
     *        PFC formatted PAUSE frames.
     **/
    fm_status ret;
    fm_int portIndex = 0;

    fm_int cb_mode = FM_PORT_TX_PAUSE_NORMAL;
    cb_mode = FM_PORT_TX_PAUSE_CLASS_BASED;
    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "PFC ON port %d", portNum);

    ret = fmSetPortAttribute(DCBX_SWITCH_NUM, portNum, 
            FM_PORT_TX_PAUSE_MODE, &cb_mode);
    if (ret != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting port %d PFC mode\n",portNum);
        return LLDP_ERR_UNKNOWN;
    }

    fm_int rx_pause = 0;
    for (fm_int i=0;i<8;i++)
    {
        if (locPFC->lldpXdot1dcbxLocPFCEnable[i])
        {
            rx_pause |= (1<<i);
        }    
    }
    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " RX PAUSE 0x%x", rx_pause);

    ret = fmSetPortAttribute(DCBX_SWITCH_NUM, portNum, 
            FM_PORT_RX_CLASS_PAUSE, &rx_pause);
    if (ret != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting port %d rx pause\n",portNum);
        return LLDP_ERR_UNKNOWN;
    }

    if (locPFC->lldpXdot1dcbxLocPFCCap == 1)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " Split SMP ");
        fm_bool arg = 1;
        ret = fmSetApiAttribute("api.FM4000.splitSMP", FM_API_ATTR_BOOL,
                                &arg);

        if (ret != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed splitting SMP\n");
            return LLDP_ERR_UNKNOWN;
        }
        for (portIndex=1;portIndex<DCBX_MAX_PORTS;portIndex++)
        {
            fm_int smp_mask = 2; /* SMP 1 Set */
            ret = fmSetPortAttribute(DCBX_SWITCH_NUM, portIndex, 
                    FM_PORT_SMP_LOSSLESS_PAUSE, &smp_mask);
            if (ret != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed adding port %d to SMP 1\n",portIndex);
                return LLDP_ERR_UNKNOWN;
            }
        }
    }

    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "\nPFC TC:");

    /* Add all priorities with PFC enabled into SMP 1 */
    fm_int smpVal = FM_QOS_TC_SMP_0;
    for (fm_int i=0;i<8;i++)
    {
        if (locPFC->lldpXdot1dcbxLocPFCEnable[i])
        {
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " [%d ON]", i);
            smpVal = FM_QOS_TC_SMP_1;
        } else {
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, " [%d OFF]", i);
            smpVal = FM_QOS_TC_SMP_0;
        }
        fmSetSwitchQOS(DCBX_SWITCH_NUM, FM_QOS_TC_SMP_MAP, i, 
                &smpVal);
        if (ret != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting priority %d\n",i);
            return LLDP_ERR_UNKNOWN;
        }
    }
    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "\n");

    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyETSSettings
 * \ingroup dcbx
 *
 * \brief           Apply ETS local data (operational state) to Fulcrum device
 *                  This function is called by the DCBX manager once the
 *                  operational state is changed, due to admin change or due
 *                  to recommendation acceptance.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locETSConfiguration points to current local ETS data,
 *                  that need to be applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyETSSettings(int portNum,
               struct lldpXdot1dcbxLocETSConfiguration *locETSConfiguration)
{
    fm_int maxTC = 
        locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassesSupported;
    if (maxTC == 0) maxTC = 8;

    /** Need the max_frame_size for ETS weight calcs
     *  Will scale all weights by 2*max_frame_size **/
    fm_int maxFrameSize;
    fm_status ret = fmGetPortAttribute(DCBX_SWITCH_NUM, portNum,
            FM_PORT_MAX_FRAME_SIZE, &maxFrameSize);
    if (ret != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed reading max-frame on port %d\n",portNum);
        return LLDP_ERR_UNKNOWN;
    }

    fm_int minWeight = 100;
    for(fm_int tcIndex=0;tcIndex<8;tcIndex++)
    {
        if ((locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex] < minWeight) &&
            (locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex] > 0))
        {
            minWeight = locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex]; 
        }
    }

    for(fm_int tcIndex=0;tcIndex<8;tcIndex++)
    {
        fm_int rxVPRI = 
        locETSConfiguration->lldpXdot1dcbxLocETSConPriorityAssignmentTable[tcIndex];
        if (tcIndex < maxTC)
        {
            fm_int sh_mode = FM_QOS_TC_GROUP_MODE_STRICT;
            switch(locETSConfiguration->lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[tcIndex])
            {
              case 2: sh_mode = FM_QOS_TC_GROUP_MODE_BALANCED;
            }
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "TC %d select %d priority-assign %d ETS BW %d%%\n", tcIndex,
                         sh_mode, rxVPRI, locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex]);

            /** map the RX VLAN priority into a VPRI 
             *  Done for both CFI=0 and CFI=1 cases **/

            fm_int vlan_pri = (tcIndex<<1);
            fm_int rxVPRI_base = rxVPRI<<1;
            fm_status ret = fmSetPortQOS(DCBX_SWITCH_NUM, portNum,
                    FM_QOS_RX_PRIORITY_MAP, vlan_pri, &rxVPRI_base);

            if (ret != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting qos-rx-priority map on port %d\n",portNum);
                return LLDP_ERR_UNKNOWN;
            }

            vlan_pri = (tcIndex<<1)|1;
            fm_int rxVPRI_CFI = (rxVPRI<< 1)|1;
            ret = fmSetPortQOS(DCBX_SWITCH_NUM, portNum,
                    FM_QOS_RX_PRIORITY_MAP, vlan_pri, &rxVPRI_CFI);

            if (ret != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting qos-rx-priority map on port %d\n",portNum);
                return LLDP_ERR_UNKNOWN;
            }

            fm_int weight = 2*maxFrameSize/minWeight* 
                locETSConfiguration->lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[tcIndex];
            ret = fmSetPortQOS(DCBX_SWITCH_NUM, portNum,
                    FM_QOS_TC_GROUP_WEIGHT, tcIndex, &weight);
            if (ret != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting scheduling weight on port %d\n",portNum);
                return LLDP_ERR_UNKNOWN;
            }

            ret = fmSetPortQOS(DCBX_SWITCH_NUM, portNum,
                    FM_QOS_TC_GROUP_MODE, tcIndex, &sh_mode);
            if (ret != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting scheduling mode on port %d\n",portNum);
                return LLDP_ERR_UNKNOWN;
            }
            if ((sh_mode == FM_QOS_TC_GROUP_MODE_BALANCED) &&
                (weight > 0))
            {
                /** DWRR queues need a TX/TC private watermark set **/
                fm_int txtc = 2048;
                ret = fmSetPortQOS(DCBX_SWITCH_NUM, portNum,
                        FM_QOS_TX_TC_PRIVATE_WM, tcIndex, &txtc);
                if (ret != FM_OK)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting scheduling mode on port %d\n",portNum);
                    return LLDP_ERR_UNKNOWN;
                }
            }
        }
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyApplicationPriorityCreate
 * \ingroup dcbx
 *
 * \brief           Create ACL rule for Application-Priority entry..
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       locApplicationPriority points to current local Application-,
 *                  Priority data that need to be applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyApplicationPriorityCreate(int portNum, 
               struct lldpXdot1dcbxLocApplicationPriorityAEPriority *locApplicationPriority)
{
    fm_int i, val, ret;
    fm_aclCondition cond;
    fm_aclValue     value;
    fm_aclActionExt action;
    fm_aclParamExt  param;

    memset(&cond, 0, sizeof(cond));
    memset(&value, 0, sizeof(value));
    memset(&action, 0, sizeof(action));
    memset(&param, 0, sizeof(param));

    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBx: Creating Application-Priority ACL rule (port:%d acl:%d selector:%d protocol:%04x priority:%d)\n", 
                 portNum,
                 DCBX_APPLICATION_PRIORITIES_ACL_ID,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEPriority);

    /* if selector is for IP port address, make sure to port parser is set to L4 */
    if (locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector >= 2 &&
        locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector <= 4)
    {
        val =  FM_PORT_PARSER_STOP_AFTER_L4;
        ret = fmSetPortAttribute(DCBX_SWITCH_NUM, portNum, FM_PORT_PARSER, &val);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting port parser mode (port:%d msg:%s).\n", portNum, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }
    }

    cond = FM_ACL_MATCH_INGRESS_PORT_MASK;
    value.ingressPortMask = 1 << portNum;
    
    switch (locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector) {
    case 1: // EtherType
        cond |= FM_ACL_MATCH_ETHERTYPE;
        value.ethType = locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol;
        value.ethTypeMask = 0xffff;
        break;
    case 2: // Well Known Port number over TCP, or SCTP
        cond |= FM_ACL_MATCH_ETHERTYPE | 
                FM_ACL_MATCH_PROTOCOL  | 
                FM_ACL_MATCH_L4_DST_PORT_WITH_MASK;
        value.ethType = 0x0800;     // IP
        value.ethTypeMask = 0xffff;
        value.protocol = 0x06;      // TCP
        value.protocolMask = 0xff;
        value.L4DstStart = locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol;
        value.L4DstEnd = value.L4DstStart;
        value.L4DstMask = 0xffff;
        break;
    case 3: // Well Known Port number over UDP, or DCCP
        cond |= FM_ACL_MATCH_ETHERTYPE | 
                FM_ACL_MATCH_PROTOCOL  | 
                FM_ACL_MATCH_L4_DST_PORT_WITH_MASK;
        value.ethType = 0x0800;     // IP
        value.ethTypeMask = 0xffff;
        value.protocol = 0x11;      // UDP
        value.protocolMask = 0xff;
        value.L4DstStart = locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol;
        value.L4DstEnd = value.L4DstStart;
        value.L4DstMask = 0xffff;
        break;
    case 4: // Well Known Port number over TCP, SCTP, UDP, and DCCP
        cond |= FM_ACL_MATCH_ETHERTYPE | 
                FM_ACL_MATCH_L4_DST_PORT_WITH_MASK;
        value.ethType = 0x0800;     // IP
        value.ethTypeMask = 0xffff;
        value.L4DstStart = locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol;
        value.L4DstEnd = value.L4DstStart;
        value.L4DstMask = 0xffff;
        break;
    default:
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting ACL rule (acl:%d selector:%d).\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector);
        return LLDP_ERR_INVALID_ARG;
    }

    action = FM_ACL_ACTIONEXT_SET_SWITCH_PRIORITY;
    param.switchPriority = locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEPriority;

    for (i=1; i < DCBX_MAX_ACL_RULES; i++) {
        fm_aclEntryState state;
        ret = fmGetACLRuleState(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, i, &state);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed getting ACL rule state (acl:%d rule:%d msg:%s).\n",
                         DCBX_APPLICATION_PRIORITIES_ACL_ID,
                         locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        if (state == FM_ACL_RULE_ENTRY_STATE_INVALID) break;
    }
    if (i == DCBX_MAX_ACL_RULES) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed allocating ACL rule (acl:%d).\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID);
        return LLDP_ERR_INVALID_ARG;
    }

    /* assign a unique rule id */
    locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId = i;
    
    ret = fmUpdateACLRule(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                          locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, 
                          cond, &value, action, &param);
    if (ret != FM_OK) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed adding ACL rule (acl:%d rule:%d msg:%s)\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
        return LLDP_ERR_UNKNOWN;
    }

    val = FM_ACL_RULE_ENTRY_STATE_VALID;
    ret = fmSetACLRuleState(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                            locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, val);
    if (ret != FM_OK) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed enabling ACL state (acl:%d rule:%d msg:%s)\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
        return LLDP_ERR_UNKNOWN;
    }

    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBx: ACL rule created (acl:%d rule:%d)\n",
                 DCBX_APPLICATION_PRIORITIES_ACL_ID,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId);
 
    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyApplicationPriorityModify
 * \ingroup dcbx
 *
 * \brief           Modify ACL rule for Application-Priority entry..
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       locApplicationPriority points to current local Application-,
 *                  Priority data that need to be applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyApplicationPriorityModify(int portNum, 
               struct lldpXdot1dcbxLocApplicationPriorityAEPriority *locApplicationPriority)
{
    fm_int ret;
    fm_aclCondition cond;
    fm_aclValue     value;
    fm_aclActionExt action;
    fm_aclParamExt  param;

    memset(&cond, 0, sizeof(cond));
    memset(&value, 0, sizeof(value));
    memset(&action, 0, sizeof(action));
    memset(&param, 0, sizeof(param));

    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBx: Modifying Application-Priority ACL rule (port:%d acl:%d rule:%d selector:%d protocol:%04x priority:%d)\n", 
                 portNum,
                 DCBX_APPLICATION_PRIORITIES_ACL_ID,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEPriority);

    ret = fmGetACLRule(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID,
                       locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId,
                       &cond, &value, &action, &param);
    if (ret != FM_OK) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed getting ACL rule (acl:%d rule:%d msg:%s).\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
        return LLDP_ERR_UNKNOWN;
    }

    param.switchPriority = locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEPriority;

    ret = fmUpdateACLRule(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID,
                          locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId,
                          cond, &value, action, &param);
    if (ret != FM_OK) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed updating ACL rule (acl:%d rule:%d msg:%s).\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyApplicationPriorityDelete
 * \ingroup dcbx
 *
 * \brief           Delete ACL rule for Application-Priority entry..
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       locApplicationPriority points to current local Application-,
 *                  Priority data that need to be applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyApplicationPriorityDelete(int portNum, 
               struct lldpXdot1dcbxLocApplicationPriorityAEPriority *locApplicationPriority)
{
    fm_int val, ret;
    fm_aclCondition cond;
    fm_aclValue     value;
    fm_aclActionExt action;
    fm_aclParamExt  param;

    memset(&cond, 0, sizeof(cond));
    memset(&value, 0, sizeof(value));
    memset(&action, 0, sizeof(action));
    memset(&param, 0, sizeof(param));

    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBx: Deleting Application-Priority ACL rule (port:%d rule:%d selector:%d protocol:%04x priority:%d)\n", 
                 portNum,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAESelector,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                 locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAEPriority);

    val = FM_ACL_RULE_ENTRY_STATE_INVALID;
    ret = fmSetACLRuleState(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                            locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, val);
    if (ret != FM_OK) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed disabling ACL state (acl:%d rul:%d msg:%s)\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
        return LLDP_ERR_UNKNOWN;
    }

    ret = fmUpdateACLRule(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID,
                          locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId,
                          cond, &value, action, &param);
    if (ret != FM_OK) {
        FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed clearing ACL rule (acl:%d rule:%d msg:%s).\n",
                     DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                     locApplicationPriority->lldpXdot1dcbxLocApplicationPriorityAERuleId, fmErrorMsg(ret));
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyApplicationPrioritySettings
 * \ingroup dcbx
 *
 * \brief           Apply Application-Priority local data (operational state) 
 *                  to Fulcrum device. This function is called by the DCBX
 *                  manager once the operational state is changed, due to admin
 *                  change or due to recommendation acceptance.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locApplicationPriority points to current local Application-,
 *                  Priority data that need to be applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyApplicationPrioritySettings(int portNum, 
               struct lldpXdot1dcbxLocApplicationPriority *locApplicationPriority)
{
    fm_int i, val;
    fm_status ret;
    fm_aclArguments args;
    fm_char statusText[1024];

    /* check that DCBx ACL exists, if not create it. */
    if (fmGetACL(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, &args) != FM_OK) {
        fm_aclCondition cond;
        fm_aclValue     value;

        memset(&cond, 0, sizeof(cond));
        memset(&value, 0, sizeof(value));

        /* create ACL */
        ret = fmCreateACLExt(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, 
                             FM_ACL_SCENARIO_ANY_FRAME_TYPE | FM_ACL_SCENARIO_ANY_ROUTING_TYPE,
                             FM_ACL_DEFAULT_PRECEDENCE);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed creating ACL (acl:%d msg:%s)\n", DCBX_APPLICATION_PRIORITIES_ACL_ID, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        /* Set incremental mode */
        val = FM_ACL_MODE_INCREMENTAL;
        ret = fmSetACLAttribute(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, FM_ACL_MODE, &val);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting ACL incremental mode (acl:%d msg:%s)\n", DCBX_APPLICATION_PRIORITIES_ACL_ID, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        /* Set supported conditions */
        cond = FM_ACL_MATCH_INGRESS_PORT_MASK |
               FM_ACL_MATCH_ETHERTYPE         | 
               FM_ACL_MATCH_PROTOCOL          |
               FM_ACL_MATCH_L4_DST_PORT_WITH_MASK;
        ret = fmSetACLAttribute(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, FM_ACL_INCR_SUP_COND, &cond);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting ACL conditions (acl:%d msg:%s)\n", DCBX_APPLICATION_PRIORITIES_ACL_ID, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        /* Set condition mask */
        value.ingressPortMask = 0x1ffffff;
        value.ethTypeMask = 0xffff;
        value.protocolMask = 0xff;
        value.L4DstMask = 0xffff;
        ret = fmSetACLAttribute(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, FM_ACL_INCR_SUP_COND_MASK, &value);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting ACL conditions mask (acl:%d msg:%s)\n", DCBX_APPLICATION_PRIORITIES_ACL_ID, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        /* Set maximum actions */
        val = 1;
        ret = fmSetACLAttribute(DCBX_SWITCH_NUM, DCBX_APPLICATION_PRIORITIES_ACL_ID, FM_ACL_INCR_NUM_ACTIONS, &val);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed setting ACL maximum actions (acl:%d msg:%s)\n", DCBX_APPLICATION_PRIORITIES_ACL_ID, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        ret = fmCompileACL(DCBX_SWITCH_NUM, statusText, sizeof(statusText), FM_ACL_COMPILE_FLAG_PARALLEL);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed compiling ACL (status:%s msg:%s)\n", statusText, fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }

        ret = fmApplyACL(DCBX_SWITCH_NUM, 0);
        if (ret != FM_OK) {
            FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed applying ACL (msg:%s)\n", fmErrorMsg(ret));
            return LLDP_ERR_UNKNOWN;
        }
    }

    for (i=0; i<locApplicationPriority->
                lldpXdot1dcbxLocApplicationPriorityAEPriorityNum; i++) {

        /* entry */
        struct lldpXdot1dcbxLocApplicationPriorityAEPriority *ae = 
                   locApplicationPriority->
                   lldpXdot1dcbxLocApplicationPriorityAEPriority + i;

        /* cache */
        struct lldpXdot1dcbxLocApplicationPriorityAEPriority *ac = 
                   fmDcbxAppCache[portNum] + i;

        if ((ac->lldpXdot1dcbxLocApplicationPriorityAESelector !=
             ae->lldpXdot1dcbxLocApplicationPriorityAESelector) ||
            (ac->lldpXdot1dcbxLocApplicationPriorityAEProtocol !=
             ae->lldpXdot1dcbxLocApplicationPriorityAEProtocol))
        {
            /* new entry */

            /* destroy old entry if needed */
            if (ac->lldpXdot1dcbxLocApplicationPriorityAESelector != 0) {
                if (fmApplyApplicationPriorityDelete(portNum, ac) != LLDP_OK)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed deleting ACL rule (selector:%d protocol:%04x priority:%d)\n", 
                         ac->lldpXdot1dcbxLocApplicationPriorityAESelector,
                         ac->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                         ac->lldpXdot1dcbxLocApplicationPriorityAEPriority);
                    return LLDP_ERR_UNKNOWN;
                }

                /* clear cache entry */
                memset(ac, 0, sizeof(struct lldpXdot1dcbxLocApplicationPriorityAEPriority));
            }

            if (fmApplyApplicationPriorityCreate(portNum, ae) != LLDP_OK) {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed creating ACL rule (selector:%d protocol:%04x priority:%d)\n", 
                     ae->lldpXdot1dcbxLocApplicationPriorityAESelector,
                     ae->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                     ae->lldpXdot1dcbxLocApplicationPriorityAEPriority);
                return LLDP_ERR_UNKNOWN;
            }

            /* update cache entry with new entry */
            memcpy(ac, ae, sizeof(struct lldpXdot1dcbxLocApplicationPriorityAEPriority));
        }
        else if (ac->lldpXdot1dcbxLocApplicationPriorityAEPriority !=
                 ae->lldpXdot1dcbxLocApplicationPriorityAEPriority)
        {
            /* entry changed */
            if (fmApplyApplicationPriorityModify(portNum, ae) != LLDP_OK) {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed modifying ACL rule (selector:%d protocol:%04x priority:%d)\n", 
                     ae->lldpXdot1dcbxLocApplicationPriorityAESelector,
                     ae->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                     ae->lldpXdot1dcbxLocApplicationPriorityAEPriority);
                return LLDP_ERR_UNKNOWN;
            }

            /* update cache entry with new entry */
            memcpy(ac, ae, sizeof(struct lldpXdot1dcbxLocApplicationPriorityAEPriority));
        }
    }

    for (;i<DCBX_TLV_MAX_APPLICATION_PRIORITIES; i++)
    {
        struct lldpXdot1dcbxLocApplicationPriorityAEPriority *ac = 
                   fmDcbxAppCache[portNum] + i;

        if (ac->lldpXdot1dcbxLocApplicationPriorityAESelector != 0) {
            if (fmApplyApplicationPriorityDelete(portNum, ac) != LLDP_OK) {
                FM_LOG_ERROR(FM_LOG_CAT_DEFAULT, "DCBx: Failed deleting ACL rule (selector:%d protocol:%04x priority:%d)\n", 
                     ac->lldpXdot1dcbxLocApplicationPriorityAESelector,
                     ac->lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                     ac->lldpXdot1dcbxLocApplicationPriorityAEPriority);
                return LLDP_ERR_UNKNOWN;
            }

            /* clear cache entry */
            memset(ac, 0, sizeof(struct lldpXdot1dcbxLocApplicationPriorityAEPriority));
        }
        else 
            break;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** fmApplyCongestionNotificationSettings
 * \ingroup dcbx
 *
 * \brief           Apply Congestion-Notification local data (operational state) 
 *                  to Fulcrum device. This function is called by the DCBX
 *                  manager once the operational state is changed, due to admin
 *                  change or due to recommendation acceptance.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      locCongestionNotification points to current local Congestion-,
 *                  Notification data that need to be applied.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR if is failed.
 *
 *****************************************************************************/
static int fmApplyCongestionNotificationSettings(int portNum,
               struct lldpXdot1dcbxLocCongestionNotification *locCongestionNotification)
{
    return LLDP_OK;
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmDcbxInitialize
 * \ingroup dcbx
 *
 * \desc            Initializes DCBX manager, by creating the DCBX data-base.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxInitialize()
{
    if (!fmLldpIsRunning()) {
        FM_LOG_PRINT("LLDP module is not initialized. run 'lldp start <mac>' to initialize.\n");
        return FM_FAIL;
    }

    /* initialize ACL cache */
    memset(fmDcbxAppCache, 0, sizeof(fmDcbxAppCache));

    return dcbxInitialize(&fmDcbxPhysicalInterface) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxTerminate
 * \ingroup dcbx
 *
 * \desc            Terminates DCBX manager, by destroying the DCBX data-base.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxTerminate()
{
    fm_int portNum;

    /* remove all DCBX ports */
    for (portNum=0; portNum<FM_LLDP_MAX_PORTS; portNum++) {
        if (fmLldpPortIsRunning(portNum))
            fmDcbxRemovePort(portNum);
    }

    return dcbxTerminate() == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxAddPort
 * \ingroup dcbx
 *
 * \desc            Add port to DCBX.
 *
 * \param[in]       portNum is the local port number to be added.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxAddPort(fm_int portNum)
{
    fm_status status;

    /* add DCBX port */
    if ((dcbxAddPort(portNum) != LLDP_OK))
        return FM_FAIL;

    return FM_OK;
}

/*****************************************************************************/
/** fmDcbxRemovePort
 * \ingroup dcbx
 *
 * \desc            Remove port from DCBX, that was added with fmDcbxAddPort().
 *
 * \param[in]       portNum is the local port number to be removed.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxRemovePort(fm_int portNum)
{
    fm_status status;

    /* remove DCBX port */
    if ((dcbxRemovePort(portNum) != LLDP_OK))
        return FM_FAIL;

    return FM_OK;
}

/*****************************************************************************/
/** fmDcbxStart
 * \ingroup dcbx
 *
 * \desc            Initializes and starts DCBX manager on all ports.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxStart()
{
    fm_status     status;
    fm_int        portNum;

    if ((status = fmDcbxInitialize()) != FM_OK)
        FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBX: Error initializing LLDP module.\n");
        return status;

    for (portNum=0; portNum<FM_LLDP_MAX_PORTS; portNum++) {
        if (fmLldpPortIsRunning(portNum)) {
            if (fmDcbxAddPort(portNum) == FM_OK)
            {    
                FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBX: Added DCBX portNum:%d\n", portNum);
            }
            else
            {    
                FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "DCBX: Error adding DCBX portNum:%d\n", portNum);
            }
        }
    }

    return FM_OK;
}

/*****************************************************************************/
/** fmDcbxStop
 * \ingroup dcbx
 *
 * \desc            Stops and destroys DCBX manager on all ports.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxStop()
{
    return fmDcbxTerminate();
}

/*****************************************************************************/
/** fmDcbxIsRunning
 * \ingroup dcbx
 *
 * \desc            Checks if DCBX manager is running.
 *
 * \return          TRUE if running.
 * \return          FALSE if not-running.
 *
 *****************************************************************************/
fm_bool fmDcbxIsRunning()
{
    return (lldpDB != NULL);
}

/*****************************************************************************/
/** fmDcbxPortIsRunning
 * \ingroup dcbx
 *
 * \desc            Checks if DCBX port is running.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \return          TRUE if running.
 * \return          FALSE if not-running.
 *
 *****************************************************************************/
fm_bool fmDcbxPortIsRunning(fm_int portNum)
{
    return (dcbxConfigGet(portNum) != NULL);
}


/*****************************************************************************
 * Management
 *****************************************************************************/

/*****************************************************************************/
/** fmDcbxSetProtocolVersion
 * \ingroup dcbx
 *
 * \desc            Sets DCBX protocol version for port.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       protVersion is the DCBX protocol version, which can be -
 *                  DCBX_VERSION_BASE or DCBX_VERSION_IEEE.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetProtocolVersion(int portNum, int protVersion)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetProtocolVersion(portNum, protVersion, 0) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetTLVsTxEnable
 * \ingroup dcbx
 *
 * \desc            Set which optional TLVs are enabled for transmit.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvsTxMask is the mask of TLVs to be changed.
 *
 * \param[in]       tlvsTxEnable is the mask of TLVs to be enabled.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetTLVsTxEnable(int portNum, int tlvsTxMask, int tlvsTxEnable)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetTLVsTxEnable(portNum, tlvsTxMask, tlvsTxEnable) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminPfcWilling
 * \ingroup dcbx
 *
 * \desc            Sets port PFC willing state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       willing is the willing state.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminPfcWilling(int portNum, bool willing)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminPfcWilling(portNum, willing) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminPfcMBC
 * \ingroup dcbx
 *
 * \desc            Sets port PFC MBC indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       mbc is the MACsec-Bypass-Capability indication.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminPfcMBC(int portNum, bool mbc)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminPfcMBC(portNum, mbc) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminPfcCap
 * \ingroup dcbx
 *
 * \desc            Sets port PFC capability indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       cap indicates the limitation of how many traffic classes
 *                  may simultaneously support PFC.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminPfcCap(int portNum, int cap)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminPfcCap(portNum, cap) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminPfcEnable
 * \ingroup dcbx
 *
 * \desc            Sets port PFC enable-on-priority state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       bX is the enable state for priority X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminPfcEnable(int portNum, bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7)
{
    bool data[8] = { b0, b1, b2, b3, b4, b5, b6, b7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminPfcEnable(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsConfigurationWilling
 * \ingroup dcbx
 *
 * \desc            Sets port ETS willing state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       willing is the willing state.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsConfigurationWilling(int portNum, bool willing)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsConfigurationWilling(portNum, willing) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsConfigurationCreditBasedShaperSupport
 * \ingroup dcbx
 *
 * \desc            Sets port ETS CBS indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       supported is the Credit-Based-Shaper-Support indication.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsConfigurationCreditBasedShaperSupport(int portNum, bool supported)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsConfigurationCreditBasedShaperSupport(portNum, supported) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsConfigurationTrafficClassesSupported
 * \ingroup dcbx
 *
 * \desc            Sets port ETS maximum traffic classes supported.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       max_tc is the maximum traffic classes supported indication.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsConfigurationTrafficClassesSupported(int portNum, int max_tc)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsConfigurationTrafficClassesSupported(portNum, max_tc) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsConfigurationTrafficClassBandwidth
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Configuration bandwidth.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       iX is the bandwidth allocated for traffic-class X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsConfigurationTrafficClassBandwidth(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
    int data[8] = { i0, i1, i2, i3, i4, i5, i6, i7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsConfigurationTrafficClassBandwidth(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Configuration traffic-algorithm.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       iX is the traffic-algorithm set for traffic-class X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
    int data[8] = { i0, i1, i2, i3, i4, i5, i6, i7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsConfigurationPriorityAssignment
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Configuration priority-assignment.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       iX is the traffic-class assigned to priority X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsConfigurationPriorityAssignment(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
    int data[8] = { i0, i1, i2, i3, i4, i5, i6, i7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsConfigurationPriorityAssignment(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsRecommendationTrafficClassBandwidth
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Recommendation bandwidth.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       iX is the bandwidth recommended for traffic-class X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsRecommendationTrafficClassBandwidth(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
    int data[8] = { i0, i1, i2, i3, i4, i5, i6, i7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsRecommendationTrafficClassBandwidth(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Recommendation traffic-algorithm.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       iX is the traffic-algorithm recommended for traffic-
 *                  class X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
    int data[8] = { i0, i1, i2, i3, i4, i5, i6, i7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminEtsRecommendationPriorityAssignment
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Recommendation priority-assignment.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       iX is the traffic-class recommended for priority X.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminEtsRecommendationPriorityAssignment(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
    int data[8] = { i0, i1, i2, i3, i4, i5, i6, i7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminEtsRecommendationPriorityAssignment(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminApplicationPriorityWilling
 * \ingroup dcbx
 *
 * \desc            Sets port Application-Priority willing state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       willing is the willing state.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminApplicationPriorityWilling(int portNum, bool willing)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminApplicationPriorityWilling(portNum, willing) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminApplicationPriorityAddMod
 * \ingroup dcbx
 *
 * \desc            Adds/Modifies port Application-Priority entry.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       sel is the protocol type selection.
 *
 * \param[in]       proto is the protocol code.
 *
 * \param[in]       pri is the priority to which the protocol should be 
 *                  assigned.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminApplicationPriorityAddMod(int portNum, int sel, int proto, int pri)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminApplicationPriorityAddMod(portNum, sel, proto, pri) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminApplicationPriorityRemove
 * \ingroup dcbx
 *
 * \desc            Removes port Application-Priority entry.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       sel is the protocol type selection.
 *
 * \param[in]       proto is the protocol code.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminApplicationPriorityRemove(int portNum, int sel, int proto)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminApplicationPriorityRemove(portNum, sel, proto) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminCongestionNotificationCnpvSupported
 * \ingroup dcbx
 *
 * \desc            Sets port Congestion-Notification supported CNPV indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       bX indicates if the CNPV is supported.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminCongestionNotificationCnpvSupported(int portNum, bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7)
{
    bool data[8] = { b0, b1, b2, b3, b4, b5, b6, b7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminCongestionNotificationCnpvSupported(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxSetAdminCongestionNotificationCnpvSupported
 * \ingroup dcbx
 *
 * \desc            Sets port Congestion-Notification ready CNPV indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       bX indicates if the CNPV is ready.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxSetAdminCongestionNotificationCnpvReady(int portNum, bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7)
{
    bool data[8] = { b0, b1, b2, b3, b4, b5, b6, b7 };

    VERIFY_DCBX_IS_RUNNING();
    return dcbxSetAdminCongestionNotificationCnpvReady(portNum, data) == LLDP_OK ? FM_OK : FM_FAIL;
}


/*****************************************************************************
 * Status
 *****************************************************************************/

/*****************************************************************************/
/** fmDcbxShowConfiguration
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX configuration information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxShowConfiguration(int portNum)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxShowConfiguration(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxShowLocalData
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX local-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxShowLocalData(int portNum)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxShowLocalData(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxShowAdminData
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX admin-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxShowAdminData(int portNum)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxShowAdminData(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxShowRemoteData
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX remote-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxShowRemoteData(int portNum)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxShowRemoteData(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmDcbxShowCounters
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX counters information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmDcbxShowCounters(int portNum)
{
    VERIFY_DCBX_IS_RUNNING();
    return dcbxShowCounters(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

