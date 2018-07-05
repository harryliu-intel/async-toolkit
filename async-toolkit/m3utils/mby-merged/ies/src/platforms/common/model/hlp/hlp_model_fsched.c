/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_fsched.c
 * Creation Date:   June 25, 2012
 * Description:     FSCHED stage of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2016 Intel Corporation. All Rights Reserved.
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
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/****************************************************************************/
/** fschedGetRxqMcastLenTable
 * \ingroup intModel
 *
 * \desc            Reads RXQ_MCAST_LEN_TABLE, populates and returns register 
 *                  structure
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       index is the table index to read
 *
 * \param[in,out]   tableEntry is the hlpRxqMcastLenTable structure that will
 *                  be populated
 *
 ****************************************************************************/
static void fschedGetRxqMcastLenTable(hlp_model           *model,
                                      fm_uint16            index,
                                      hlpRxqMcastLenTable *tableEntry)
{
    fm_uint32   value;
    fm_status   err;

    FM_NOT_USED(model);

    err = hlpModelReadCSR(0, HLP_RXQ_MCAST_LEN_TABLE(index, 0), &value);

    tableEntry->L3_REPCNT   = FM_GET_FIELD(value,
                                           HLP_RXQ_MCAST_LEN_TABLE,
                                           L3_REPCNT);

    tableEntry->L3_MCAST_IDX = FM_GET_FIELD(value,
                                           HLP_RXQ_MCAST_LEN_TABLE,
                                           L3_MCAST_IDX);
} /* fschedGetRxqMcastLenTable */

/****************************************************************************/
/** fschedGetRxqMcastDestTable
 * \ingroup intModel
 *
 * \desc            Reads RXQ_MCAST_DEST_TABLE, populates and returns register 
 *                  structure
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       index is the table index to read
 *
 * \param[in,out]   tableEntry is the hlpRxqMcastDestTable structure that will
 *                  be populated
 *
 ****************************************************************************/
static void fschedGetRxqMcastDestTable(hlp_model            *model,
                                       fm_uint16             index,
                                       hlpRxqMcastDestTable *tableEntry)
{
    fm_uint64   value;
    fm_status   err;

    FM_NOT_USED(model);

    err = hlpModelReadCSR64(0, HLP_RXQ_MCAST_DEST_TABLE(index,0), &value);

    tableEntry->LEN_TABLE_IDX = FM_GET_FIELD(value, 
                                           HLP_RXQ_MCAST_DEST_TABLE,
                                           LEN_TABLE_IDX);

    tableEntry->PORT_MASK    = FM_GET_FIELD(value, 
                                           HLP_RXQ_MCAST_DEST_TABLE,
                                           PORT_MASK);
} /* fschedGetRxqMcastDestTable */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelFsched
 * \ingroup intModel
 *
 * \desc            Replicates packets.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          TRUE if there is a frame to transmit, FALSE if otherwise
 *
 *****************************************************************************/
fm_bool hlpModelFsched(hlp_model *model)
{
    hlp_modelState         *state = &model->packetState;
    fm_uint32               curPort;
    fm_bool                 transmit;
    fm_uint32               lenTableIdx;
    fm_uint32               l3RepCnt;
    hlpRxqMcastDestTable    mcastDestTable;
    hlpRxqMcastLenTable     mcastLenTable;
    fm_uint32               i;

    /* Here's the overview:
     *   An external traffic generator will send a packet into
     *   hlp_model_basic:hlpModelSendPacket() which ripples through
     *   the pipeline and hits this stage. The traffic receiver will
     *   then call hlp_model_basic:hlpModelReceivePacket() which calls
     *   this function to get one packet copy. This copy is run through
     *   the egress pipeline and out to the traffic receiver. This 
     *   function is called again and again until every copy of the
     *   original ingress packet is delivered (L2, mirror, and L3MC copies)
     *
     * For each ingress packet, we need to remember:
     *  * which L2 port we're working on
     *  * which L3 copy we're working on
     *  * which mirror copy we're working on
     */     

    /* Set the egress dglort to the ingress dglort */
    state->EDGLORT = state->IDGLORT;
    state->MIRTYP  = HLP_MODEL_MIRTYPE_NORMAL;

    transmit = FALSE;
    /* Walk through the L2 port numbers to deliver N copes to each port */
    for (curPort = state->FSCHED_L2_PORTNUM ; 
         curPort <= HLP_MAX_FABRIC_LOG_PORT && !transmit; 
         curPort++)
    {

        if ((state->FNMASK & (1 << curPort)) != 0)
        {
            fschedGetRxqMcastDestTable(model, 
                                       state->IP_MCAST_IDX,
                                       &mcastDestTable);

            /* L3 replication is possible*/
            if ((mcastDestTable.PORT_MASK & (1<<curPort)) != 0)
            {
                /* Definitely L3 replication, send L3_Repcnt copies to
                 * the tx port */
                /* Calculate offset into mcast len table*/
                state->FSCHED_LEN_TABLE_OFFSET = 0;
                for (i = 0; i < curPort; i++) {
                    if (mcastDestTable.PORT_MASK & (1 << i))
                        state->FSCHED_LEN_TABLE_OFFSET++;
                }
                
                fschedGetRxqMcastLenTable(model, 
                                          mcastDestTable.LEN_TABLE_IDX +
                                          state->FSCHED_LEN_TABLE_OFFSET,
                                          &mcastLenTable);

                if (model->FSCHED_L3COPY <= mcastLenTable.L3_REPCNT)
                {
                    state->TX_PORT          = curPort;

                    /* Per MAS, start at last vlan index and count down to
                     * 0 */
                    state->MOD_IP_MCAST_IDX = mcastLenTable.L3_MCAST_IDX +  
                                              mcastLenTable.L3_REPCNT   - 
                                              model->FSCHED_L3COPY;

                    transmit                = TRUE;
                    model->FSCHED_L3COPY++;
                }
                else
                {
                    /* No more copies to send on this tx port, move on */
                    model->FSCHED_L3COPY        = 0;
                    state->FSCHED_L2_PORTNUM++;
                }
            }
            else
            {
                /* FNMASK has bit set, but MCAST_DEST_TABLE port mask isn't
                 * so only doing L2 copy on this port */
                state->TX_PORT              = curPort;
                state->MOD_IP_MCAST_IDX     = 0;
                model->FSCHED_L3COPY        = 0;
                transmit                    = TRUE;
                state->FSCHED_L2_PORTNUM++;
            }
        }
        else
        {
            state->FSCHED_L2_PORTNUM++;
        }
    }

    /* So if transmit is true, we found a packet to send. If transmit is false,
     * there was no normal L2 or L3 copies to send. So we should look to
     * see if we can send the mirror copies */
    if (!transmit)
    {
        /* Per MAS, mirror1 is sent before mirror0 */
        if ((state->MIRROR1_PORT < HLP_MAX_FABRIC_LOG_PORT) && 
            (state->MIRROR1_PROFILE_V) &&
            (!state->MIRROR1_SENT))
        {
            
            state->MIRTYP           = HLP_MODEL_MIRTYPE_MIR1;
            state->TX_PORT          = state->MIRROR1_PORT;
            state->IP_MCAST_IDX     = 0;
            state->MIRROR1_SENT     = TRUE;
            model->FSCHED_L3COPY    = 0;
            transmit                = TRUE;
        }
        else if ((state->MIRROR0_PORT < HLP_MAX_FABRIC_LOG_PORT) &&
                 (state->MIRROR0_PROFILE_V) &&
                 (!state->MIRROR0_SENT))
        {
            state->MIRTYP           = HLP_MODEL_MIRTYPE_MIR0;
            state->TX_PORT          = state->MIRROR0_PORT;
            state->IP_MCAST_IDX     = 0;
            state->MIRROR0_SENT     = TRUE;
            model->FSCHED_L3COPY    = 0;
            transmit                = TRUE;
        }
    }

EXIT:
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpFsched);

    return transmit;

}   /* end hlpModelFsched */


