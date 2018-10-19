/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_basic.c
 * Creation Date:   June 12, 2012
 * Description:     HLP white model (basic)
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2014 Intel Corporation. All Rights Reserved.
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
#include <platforms/common/model/hlp/hlp_model.h>
#include <platforms/common/model/hlp/hlp_model_types.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
#define QEWMA_SIZE  2

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelReceivePacket
 * \ingroup model10k
 *
 * \desc            Retrieve a packet egressing any of the white model's
 *                  switch ports. This function may be called iteratively
 *                  until no more egress packets are available.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[out]      port points to caller-allocated storage where this
 *                  function should write the egress physical port number,
 *                  if any.
 *
 * \param[out]      packet points to caller-allocated storage where this
 *                  function should write the packet contents.
 *
 * \param[out]      length points to caller-allocated storage where this
 *                  function should write the egress packet length in
 *                  bytes.
 *
 * \param[in]       maxPktSize is the size of the buffer pointed to by
 *                  packet in bytes.
 *
 * \param[out]      sbData points to caller-allocated storage where this
 *                  function places the sideband data.
 *
 * \return          FM_OK if a packet was successfully retrieved.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''hlpModelInitialize''.
 * \return          FM_ERR_NO_MORE if there are no packets to retrieve.
 *
 *****************************************************************************/
fm_status hlpModelReceivePacket(fm_int                sw,
                                    fm_int  *             port,
                                    fm_byte *             packet,
                                    fm_int  *             length,
                                    fm_int                maxPktSize,
                                    fm_modelSidebandData *sbData)
{
    hlp_model *     model;
    hlp_modelState *state;
    fm_status       status = FM_OK;
    fm_bool         aplLoopback = FALSE;
    fm_int          val;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d, port=%p, packet=%p, length=%p, maxPktSize=%d\n",
                 sw,
                 (void *)port,
                 (void *)packet,
                 (void *)length,
                 maxPktSize);

    model = FM_MODEL_GET_STATE(sw);
    state = &model->packetState;

RETRY:
    if (hlpModelFsched(model))
    {
        status = hlpModelModify(model, packet, maxPktSize);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        if (state->TX_DROP)
        {
            /* This packet has been dropped by the loopback suppression
             * mechanism. Try to transmit the next packet. */
            goto RETRY;
        }

        status = hlpModelXbarTx(model);
        if (status)
        {
            /* No mapping found. Not exactly HW behavior since the TXQ
             * will never be serviced and frames will be stucked in the
             * fabric. However here we will continue on and try to
             * transmit the next packet. */
            goto RETRY;
        }

        /* TODO APL loopback not yet supported */
        aplLoopback = FALSE;
        if (aplLoopback)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT, "Setting APL loopback for TX_PORT %d\n",
                    state->TX_PORT);

            /* hack - use the SPARE1 code to indicate to fm_model_packet_queue
             * that loopback is requested for this packet */
            status = FM_ERR_SPARE1;

            goto DONE;
        }

        if ((val = hlpModelMacTx(model, packet, maxPktSize)) != 0)
        {
            if (val == FM_ERR_SPARE1)
            {
                /* Special case for MII_LOOPBACK, might not work
                 * properly for multicast packets */
                FM_LOG_DEBUG(FM_LOG_CAT_EVENT, "MII loopback for TX_PORT %d\n",
                    state->TX_PORT);
                status = FM_ERR_SPARE1;
                goto DONE;
            }
            /* This packet has been dropped by the TX MAC. Try to transmit the
             * next packet. */
            goto RETRY;
        }

DONE:
        *port = state->TX_PORT;
        *length = (fm_int) state->TX_LENGTH;
        sbData->idTag = state->SB_DATA->idTag;
        sbData->tc = state->TC;
        FM_MEMCPY_S(sbData->pktMeta,
                    sizeof(sbData->pktMeta),
                    state->TX_PKT_META,
                    sizeof(state->TX_PKT_META));
    }
    else
    {
        /* No (more) packets are to be transmitted. */
        fmFree(state->RX_DATA);
        fmFree(state->SB_DATA);

        status = FM_ERR_NO_MORE;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end hlpModelReceivePacket */




/*****************************************************************************/
/** hlpModelSendPacket
 * \ingroup model10k
 *
 * \desc            Send a packet into the white model on the specified port.
 *                  The packet will be processed by the chip model's
 *                  frame processing pipeline immediately, in the context
 *                  of this function call.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       port is the physical port number of the model's ingress
 *                  port into which the packet is being sent.
 *
 * \param[in]       packet is a pointer to the packet content.
 *
 * \param[in]       length is the packet length in bytes.
 *
 * \param[in]       sbData is a pointer to the sideband data.
 *
 * \return          FM_OK if packet successfully sent into white model.
 * \return          FM_ERR_NO_MEM if no memory available to hold packet.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelSendPacket(fm_int                sw,
                                 fm_int                port,
                                 fm_byte *             packet,
                                 fm_int                length,
                                 fm_modelSidebandData *sbData)
{
    hlp_model *     model;
    hlp_modelState *state;
    fm_status          status = FM_OK;
    errno_t            rv;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d, port=%d, packet=%p, length=%d\n",
                 sw,
                 port,
                 (void *)packet,
                 length);

    if (port < 0 || port > 31)
    {
        FM_LOG_ERROR(FM_LOG_CAT_EVENT, "Invalid port %d\n",
                    port);
        return FM_ERR_INVALID_PORT;
    }

    model = FM_MODEL_GET_STATE(sw);
    state = &model->packetState;

    FM_MODEL_TAKE_LOCK(model);

    /* Initialize the packet state. */
    FM_CLEAR(*state);
    state->RX_LENGTH = length;
    state->RX_PORT = port;
    rv = FM_MEMCPY_S(state->PKT_META,
                     sizeof(state->PKT_META),
                     sbData->pktMeta,
                     sizeof(sbData->pktMeta));
    if (rv != 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }

    state->RX_DATA = fmAlloc(state->RX_LENGTH);

    if (state->RX_DATA == NULL)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_ERR_NO_MEM);
    }

    rv = FM_MEMCPY_S(state->RX_DATA,
                     state->RX_LENGTH,
                     packet,
                     state->RX_LENGTH);
    if (rv != 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }

    state->SB_DATA = fmAlloc(sizeof(fm_modelSidebandData));

    if (state->SB_DATA == NULL)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_ERR_NO_MEM);
    }

    rv = FM_MEMCPY_S(state->SB_DATA,
                     sizeof(fm_modelSidebandData),
                     sbData,
                     sizeof(fm_modelSidebandData));
    if (rv != 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }


    /* Process all pipeline stages. */
    if (hlpModelMacRx(model) != 0)
    {
        /* This packet has been dropped by the MAC and should not be processed
         * any further. */
        goto ABORT;
    }

    if (hlpModelXbarRx(model) != FM_OK)
    {
        /* This packet should dropped due to no mapping found.
         * In real HW, should cause SIA overrun? */
        goto ABORT;
    }

    hlpModelParser(model);

    hlpModelMapper(model);

    status = hlpModelFfuClassifier(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    status = hlpModelFfuFinalActions(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    hlpModelHash(model);

    status = hlpModelNextHop(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    status = hlpModelPolicer(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    status = hlpModelL2Lookup(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    hlpModelGlort(model);

    hlpModelGenMask1(model);

    status = hlpModelTriggers(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    hlpModelCm(model);

    status = hlpModelGenMask2(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    status = hlpModelLearning(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    status = hlpModelStatsRx(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    FM_MODEL_DROP_LOCK(model);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end hlpModelSendPacket */


