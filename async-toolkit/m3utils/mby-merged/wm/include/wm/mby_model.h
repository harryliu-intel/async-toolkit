/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            mby_model.h
 * Creation Date:   June 5, 2012
 * Description:     General prototypes for the MBY model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2014 Intel Corporation. All Rights Reserved.
 * Copyright 2018. All Rights Reserved.
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

/* Taken from hlp_model.h

   Updating for MBY by Mika Nystrom <mika.nystroem@intel.com>
 */

#ifndef __MBY_MODEL_H
#define __MBY_MODEL_H

#include "fm_std.h" /* from std/intel on x86 */
#include "common/fm_errno.h"
#inlcude "platform_types.h"

  /*****************************************************************************
   * These prototypes can be called from the platform initialization code and
   * from within the standard platform services.
   *****************************************************************************/

  /*****************************************************************************/
  /** mbyModelGetPortMap
   * \ingroup intPlatform
   *
   * \desc            Returns port mapping for logical to physical port depending
   *                  on the map type.
   *
   * \param[in]       numPorts is the number of ports to be mapped.
   *
   * \param[out]      portMap is the structure which contains mapping for
   *                  logical to physical port.
   *
   * \return          FM_OK if successful.
   * \return          Other ''Status Codes'' as appropriate in case of failure.
   *
   *****************************************************************************/
fm_status mbyModelGetPortMap(fm_platformPort *portMap, fm_int numPorts);

/*****************************************************************************/
/** mbyModelInitialize
 * \ingroup model10k
 *
 * \desc            Allocate and initialize an instance of the MBY white
 *                  model. This function must be called once at initialization
 *                  time for each chip to be instantiated (and prior to calling
 *                  any other model interface functions).
 *
 * \param[in]       chipModel points to the address of a void pointer which
 *                  will be filled in by this function with the address of a
 *                  MBY model object.
 *
 * \param[in]       sw is the switch number for the model instance.
 *
 * \param[in]       funcPtrs should be set to NULL for the MBY white model.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if funcPtrs is non-NULL.
 * \return          FM_ERR_NO_MEM if the MBY model object could not be
 *                  allocated.
 *
 *****************************************************************************/
fm_status mbyModelInitialize(void **chipModel, fm_int sw, void *funcPtrs);

/*****************************************************************************/
/** mbyModelCheckBpduDropping
 * \ingroup intPlatform
 *
 * \desc            Checks whether the received packet is a BPDU packet and if
 *                  so, whether the BPDU packet should be trapped to the CPU or
 *                  silently dropped.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       pktEvent points to the ''fm_eventPktRecv'' structure
 *                  describing the received packet.
 *
 * \param[in]       vlan is the VLAN on which the packet has been received.
 *
 * \param[out]      dropBpdu points to caller-allocated storage in which this
 *                  function stores the drop decision.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelPlatformCheckBpduDropping(fm_int           sw,
                                            fm_eventPktRecv *pktEvent,
                                            fm_int           vlan,
                                            fm_bool *        dropBpdu);

/*****************************************************************************/
/** mbyModelPlatformGetCpuMaxFrameSize
 * \ingroup intPlatform
 *
 * \desc            Returns the maximum frame size supported by the CPU port.
 *
 * \param[in]       sw is the switch on which to operate. The switch number
 *                  must have already been validated.
 *
 * \param[out]      maxFrameSize points to caller-allocated storage where this
 *                  function places the maximum frame size supported by the CPU
 *                  port.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelPlatformGetCpuMaxFrameSize(fm_int     sw,
                                             fm_uint32 *cpuMaxFrameSize);

/*****************************************************************************/
/** mbyModelPlatformGetCpuVlanTag
 * \ingroup intPlatform
 *
 * \desc            Returns the VLAN tagging mode for the CPU port.
 *
 * \param[in]       sw is the switch on which to operate. The switch number
 *                  must have already been validated.
 *
 * \param[in]       vlan is the VLAN number whose tagging mode is to be
 *                  returned.
 *
 * \param[out]      tag points to caller-allocated storage where this function
 *                  places the VLAN tagging mode for the CPU port.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelPlatformGetCpuVlanTag(fm_int   sw,
                                        fm_int   vlan,
                                        fm_bool *tag);

/*****************************************************************************/
/** mbyModelPlatformGetNumPorts
 * \ingroup intPlatform
 *
 * \desc            Returns the number of ports supported by the MBY switch
 *                  model.
 *
 * \param[in]       sw is the switch on which to operate. The switch number
 *                  must already have been validated.
 *
 * \param[out]      numPorts points to caller-allocated storage where this
 *                  function places the number of supported ports.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelPlatformGetNumPorts(fm_int sw, fm_int *numPorts);

/*****************************************************************************/
/** mbyModelReadCSR
 * \ingroup model10k
 *
 * \desc            Read 32 bits of an MBY CSR register from the model.
 *                                                                      \lb\lb
 *                  The tree is searched for an existing entry. If there is
 *                  none, the defaults table is searched for a non-zero
 *                  default value. If none, zero is returned as the default,
 *                  otherwise the default from the table is returned.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function will place the read register value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''mbyModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);

/*****************************************************************************/
/** mbyModelReadCSR64
 * \ingroup model10k
 *
 * \desc            Read a 64-bit MBY CSR register.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function will place the read 64-bit register value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''mbyModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value);

/*****************************************************************************/
/** mbyModelReadCSRMult
 * \ingroup model10k
 *
 * \desc            Read multiple 32-bit MBY CSR registers from the model.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an array to be filled in with
 *                  the register data read. The array must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''mbyModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelReadCSRMult(fm_int     sw,
                              fm_uint32  addr,
                              fm_int     n,
                              fm_uint32 *value);

/*****************************************************************************/
/** mbyModelReadCSRMult64
 * \ingroup model10k
 *
 * \desc            Read multiple 64-bit MBY CSR registers.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       n contains the number of register addresses to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an array of to be filled in with
 *                  the 64-bit register data read. The array must be n elements
 *                  in length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''mbyModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelReadCSRMult64(fm_int     sw,
                                fm_uint32  addr,
                                fm_int     n,
                                fm_uint64 *value);

/*****************************************************************************/
/** mbyModelWriteCSRMult64
 * \ingroup model10k
 *
 * \desc            Write multiple 64-bit values to one or more MBY CSR
 *                  registers.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the starting CSR register address to write
 *                  to.
 *
 * \param[in]       n is the number of consecutive CSR register addresses to
 *                  write to.
 *
 * \param[in]       newValue points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelWriteCSRMult64(fm_int     sw,
                                 fm_uint32  addr,
                                 fm_int     n,
                                 fm_uint64 *value);

/** mbyModelReceivePacket
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
 *                  previously instantiated by ''mbyModelInitialize''.
 * \return          FM_ERR_NO_MORE if there are no packets to retrieve.
 *
 *****************************************************************************/
fm_status mbyModelReceivePacket(fm_int                sw,
                                fm_int *              port,
                                fm_byte *             packet,
                                fm_int *              length,
                                fm_int                maxPktSize,
                                fm_modelSidebandData *sbData);

/** mbyModelReset
 * \ingroup model10k
 *
 * \desc            Used to simulate a reset on the MBY white model. This
 *                  service can be used to re-initialize the state or value of
 *                  variables.
 *
 * \param[in]       sw is the switch number for the model instance.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelReset(fm_int sw);

/*****************************************************************************/
/** mbyModelResetV2
 * \ingroup model10k
 *
 * \desc            Used to simulate a reset on the MBY white model. This
 *                  service can be used to re-initialize the state or value of
 *                  variables.
 *
 * \param[in]       sw is the switch number for the model instance.
 *
 * \param[in]       domain is the reset domain being reset.
 *
 * \return          FM_OK
 *
 *****************************************************************************/
fm_status mbyModelResetV2(fm_int sw, fm_int domain);

/*****************************************************************************/
/** mbyModelSendPacket
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
 *                  previously instantiated by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelSendPacket(fm_int                sw,
                             fm_int                port,
                             fm_byte *             packet,
                             fm_int                length,
                             fm_modelSidebandData *sbData);

/*****************************************************************************/
/** mbyModelTick
 * \ingroup model10k
 *
 * \desc            The top-level handler for MBY white model periodic
 *                  processing and interrupt condition polling. This function
 *                  should be called on a periodic basis to detect chip
 *                  interrupt conditions and to provide the model with the
 *                  opportunity to run its background tasks.
 *                                                                      \lb\lb
 * \note            The calling platform layer implementation must define
 *                  values for ''FM_MODEL_TICK_SECS'' and
 *                  ''FM_MODEL_TICK_NANOSECS'' that correspond to the
 *                  periodicity with which this function will be called.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[out]      interrupt is written with the chip's interrupt mask, if any.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelTick(fm_int sw, fm_uint32 *interrupt);

/*****************************************************************************/
/** mbyModelWriteCSR
 * \ingroup model10k
 *
 * \desc            Write a 32-bit value to an MBY CSR register.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       newValue is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''mbyModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''mbyModelInitialize''.
 *
 *****************************************************************************/
fm_status mbyModelWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 newValue);

/*****************************************************************************/
/** mbyModelWriteCSRMult
 * \ingroup model10k
 *
 * \desc            Write multiple 32-bit values to one or more MBY CSR
 *                  registers.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the starting CSR register address to write
 *                  to.
 *
 * \param[in]       n is the number of consecutive CSR register addresses to
 *                  write to.
 *
 * \param[in]       newValue points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelWriteCSRMult(fm_int     sw,
                               fm_uint32  addr,
                               fm_int     n,
                               fm_uint32 *newValue);

/*****************************************************************************/
/** mbyModelWriteCSR64
 * \ingroup model10k
 *
 * \desc            Write 64-bit values to one or more MBY CSR
 *                  registers.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the starting CSR register address to write
 *                  to.
 *
 * \param[in]       newValue points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelWriteCSR64(fm_int     sw,
                             fm_uint32  addr,
                             fm_uint64  newValue);

/*****************************************************************************/
/** mbyModelWriteCSRForce64
 * \ingroup model10k
 *
 * \desc            Write 64-bit values to one or more MBY CSR registers.
 *
 *                  This function is provided for the benefit of the application
 *                  which must update the global interrupt register to
 *                  control the interrupt generation. The
 *                  mbyModelWriteCSRAbsolute64 is not visible to the
 *                  application.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the starting CSR register address to write
 *                  to.
 *
 * \param[in]       newValue points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status mbyModelWriteCSRForce64(fm_int     sw,
                                  fm_uint32  addr,
                                  fm_uint64  newValue);

#endif /* __MBY_MODEL_H */

