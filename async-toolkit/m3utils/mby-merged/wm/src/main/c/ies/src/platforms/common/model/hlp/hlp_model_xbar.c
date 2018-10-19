/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_ffu.c
 * Creation Date:   July 18, 2012
 * Description:     MAPPER stage of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 Intel Corporation. All Rights Reserved.
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

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelLogicalToPhysical
 * \ingroup intModel
 * 
 * \desc            Looks at the scheduler registers to map logical port 
 *                  number to physical port number.
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       logPort is the logical port being looked up.
 *
 * \return          The physical port number.
 *
 *****************************************************************************/
fm_uint32 hlpModelLogicalToPhysical(fm_int sw, fm_uint32 logPort)
{
    fm_uint32   maxIndex = 0;
    fm_uint32   value;
    fm_uint32   i;
    fm_uint32   page = 0;
    fm_status   status;
    fm_int      port = -1;
    fm_bool     no_eligibility_check = 0;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, 
            "LogicalToPhysical( logPort=%d)\n", logPort);

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "logical (%d) to physical...\n", logPort);
    
    if(testPlusArgs("NO_SCHED_ELIGIBILITY_CHECK") >= 0)
        no_eligibility_check = testPlusArgs("NO_SCHED_ELIGIBILITY_CHECK");

    status = hlpModelReadCSR(sw, HLP_EARB_CTRL(0), &value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if (FM_GET_BIT(value, HLP_EARB_CTRL, ADAPTIVE_SCHED_ENABLE))
    {
        page = FM_GET_BIT(value, HLP_EARB_CTRL, ADAPTIVE_SCHED_PAGE);
        for(i = 0 ; i < 32 ; i++)
        {
            status = hlpModelReadCSR(sw, HLP_EARB_ADAPTIVE_SCHEDULE(page, i, 0), &value);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            if ((FM_GET_FIELD(value, HLP_EARB_ADAPTIVE_SCHEDULE, PORT) == logPort) &&
               ((FM_GET_FIELD(value, HLP_EARB_ADAPTIVE_SCHEDULE, ELIGIBLE) !=0) || (no_eligibility_check == 1)))
            {
                port =  i;
                FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "\tADAPTIVE_SCHEDULE[%d]: 0x%x <- schedLogPort: %d\n",
                        port, value, logPort);
                break;
            }
        }
    }
    else
    {
        page = FM_GET_BIT(value, HLP_EARB_CTRL, FIXED_SCHED_PAGE);
        for(i = 0 ; i < 512 ; i++)
        {
            status = hlpModelReadCSR(sw, HLP_EARB_FIXED_SCHEDULE(page, i, 0), &value);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

            FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_PLATFORM, "\t%d: 0x%x Port: %d\n", i, value, 
                FM_GET_FIELD(value, HLP_EARB_FIXED_SCHEDULE, PORT));
            if ((FM_GET_FIELD(value, HLP_EARB_FIXED_SCHEDULE, PORT) == logPort) &&
                (FM_GET_BIT(value, HLP_EARB_FIXED_SCHEDULE, IDLE) == 0) )
            {
                port = FM_GET_FIELD(value, HLP_EARB_FIXED_SCHEDULE, PHYS_PORT);
                FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "\tFIXED_SCHEDULE[%d] -> physPort = %d\n",
                    i, port);
                break;
            }
        } 
    }

    if (port < 0) 
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                "Can't find schedLogPort %d in page=%d\n",
                logPort, page);
    }

ABORT:
    return port;

} /* hlpModelLogicalToPhysical */

/*****************************************************************************/
/** hlpModelPhysicalToLogical
 * \ingroup intModel
 * 
 * \desc            Looks at the scheduler registers to map physical port 
 *                  number to logical port number.
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       physPort is the physical port being looked up.
 *
 * \return          The logical port number.
 *
 *****************************************************************************/
fm_uint32 hlpModelPhysicalToLogical(fm_int sw, fm_uint32 physPort)
{
    fm_uint32   page = 0;
    fm_uint32   value;
    fm_uint32   i;
    fm_status   status;
    fm_int      port = -1;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT, 
            "PhysicalToLogical( physPort=%d)\n",
            physPort);

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT, "physical (%d) to logical...\n", physPort);

    status = hlpModelReadCSR(sw, HLP_EARB_CTRL(0), &value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if (FM_GET_BIT(value, HLP_EARB_CTRL, ADAPTIVE_SCHED_ENABLE))
    {
        page = FM_GET_BIT(value, HLP_EARB_CTRL, ADAPTIVE_SCHED_PAGE);
        if (physPort < 32)
        {
            status = hlpModelReadCSR(sw, HLP_EARB_ADAPTIVE_SCHEDULE(page, physPort, 0), &value);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            if ((FM_GET_FIELD(value, HLP_EARB_ADAPTIVE_SCHEDULE, ELIGIBLE) != 0) || testPlusArgs("NO_SCHED_ELIGIBILITY_CHECK"))
            {
                port = FM_GET_FIELD(value, HLP_EARB_ADAPTIVE_SCHEDULE, PORT);
                FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "\tADAPTIVE_SCHEDULE[%d]: 0x%x -> schedLogPort: %d\n", physPort, value, port);
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "\tADAPTIVE_SCHEDULE[%d]: 0x%x -> Invalid ELIGIBLE config\n", physPort, value);
            }
        }
    }
    else
    {
        page = FM_GET_BIT(value, HLP_EARB_CTRL, FIXED_SCHED_PAGE);
        for(i = 0 ; i < 512 ; i++)
        {
            status = hlpModelReadCSR(sw, HLP_EARB_FIXED_SCHEDULE(page, i, 0), &value);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

            FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_PLATFORM, "\t%d: 0x%x physport: %d\n", i, value, 
                FM_GET_FIELD(value, HLP_EARB_FIXED_SCHEDULE, PHYS_PORT));
            if ((FM_GET_FIELD(value, HLP_EARB_FIXED_SCHEDULE, PHYS_PORT) == physPort) &&
               (FM_GET_BIT(value, HLP_EARB_FIXED_SCHEDULE, IDLE) == 0))
            {
                port = FM_GET_FIELD(value, HLP_EARB_FIXED_SCHEDULE, PORT);
                FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "\tFIXED_SCHEDULE[%d] -> schedLogPort = %d\n",
                    i, port);
                break;
            }
        } 
    }

    if (port < 0) 
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                "Can't find physPort %d in page=%d\n",
                physPort, page);
    }

ABORT:
    return port;

} /* hlpModelPhysicalToLogical */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelXbarRx
 *  \ingroup intModel
 *
 *  \desc           Modesls the Ingress Xbar performing physical to logical
 *                  mapping.
 *
 *  \param[in]      model points to the switch data structure.
 *
 * \return          FM_OK if physical to logical port mapping is found.
 *
 *****************************************************************************/
fm_status hlpModelXbarRx(hlp_model *model)
{
    hlp_modelState *state;

    state = &model->packetState;

    state->RX_SOP = 1;
    if (state->RX_LENGTH <= 192)
    {
        state->RX_EOP = 1;
    }

    state->RX_PORT = hlpModelPhysicalToLogical(model->sw, state->RX_PORT);
    
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpXbarRx);

    if (state->RX_PORT < 0)
    {
        return FM_ERR_INVALID_PORT;
    }

    return FM_OK;

}   /* end hlpModelXbarRx */

/*****************************************************************************/
/** hlpModelXbarTx
 *  \ingroup intModel
 *
 *  \desc           Modesls the Egress Xbar performing logical to physical 
 *                  mapping.
 *
 *  \param[in]      model points to the switch data structure.
 *
 * \return          FM_OK if logical to physical port mapping is found.
 *
 *****************************************************************************/
fm_status hlpModelXbarTx(hlp_model *model)
{
    hlp_modelState *state;

    state = &model->packetState;

    state->TX_PORT = hlpModelLogicalToPhysical(model->sw, state->TX_PORT);
    
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpXbarTx);

    if (state->RX_PORT < 0)
    {
        return FM_ERR_INVALID_PORT;
    }

    return FM_OK;

}   /* end hlpModelXbarTx */
