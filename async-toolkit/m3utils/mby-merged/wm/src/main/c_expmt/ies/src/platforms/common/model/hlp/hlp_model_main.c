/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_main.c
 * Creation Date:   June 6, 2012
 * Description:     HLP white model (main)
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
 * Local Function Prototypes
 *****************************************************************************/

static fm_status WriteCSRAbsolute(hlp_model *model,
                                  fm_uint32 addr,
                                  fm_uint32 newValue,
                                  fm_uint32 *oldValue);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** WriteCSRAbsolute
 * \ingroup intModel
 *
 * \desc            Writes a CSR register without regard for field types within
 *                  the register and without taking the switch model lock.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr contains the CSR register address to write.
 *                  addr is byte offset.
 *
 * \param[in]       newValue is the data value to write to the CSR register.
 *
 * \param[in]       oldValue is the caller-allocated storage where the
 *                  place the old value, if oldValue is not NULL.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status WriteCSRAbsolute(hlp_model *model,
                                  fm_uint32 addr,
                                  fm_uint32 value,
                                  fm_uint32 *oldValue)
{
    if (addr > HLP_MODEL_MAX_HW_ADDR)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    if (oldValue)
    {
        *oldValue = model->registers[DWORD_OFF(addr)];
    }

    /* Write the new specified value. */
    model->registers[DWORD_OFF(addr)] = value;

    return FM_OK;

}   /* end WriteCSRAbsolute */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelCheckBpduDropping
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
fm_status hlpModelCheckBpduDropping(fm_int           sw,
                                    fm_eventPktRecv *pktEvent,
                                    fm_int           vlan,
                                    fm_bool *        dropBpdu)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(pktEvent);
    FM_NOT_USED(vlan);

    /* Never silently drop BPDU packets. */
    *dropBpdu = FALSE;
    return FM_OK;

}   /* end hlpModelCheckBpduDropping */




/*****************************************************************************/
/** hlpModelGetOffsetMult1
 *
 * \desc            Retrieves the 1-dimensional register offset in terms of
 *                  (index, word) for the specified register address.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       baseAddr is the register base address, i.e. the address of
 *                  entry 0, word 0.
 *
 * \param[in]       entries is the number of register entries.
 *
 * \param[in]       stride is the number of words in between each register
 *                  entry.
 *
 * \param[in]       index points to caller-allocated storage in which this
 *                  function stores the index portion of the register offset.
 *
 * \param[in]       word points to caller-allocated storage in which this
 *                  function stores the word portion of the register offset.
 *                  Set to NULL if the register is a 1-dimensional,
 *                  single-word, multi-entry register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if the register address is out-of-bounds.
 *
 *****************************************************************************/
fm_status hlpModelGetOffsetMult1(fm_uint32 addr,
                                 fm_uint32 baseAddr,
                                 fm_int    entries,
                                 fm_int    stride,
                                 fm_int *  index,
                                 fm_int *  word)
{
    fm_int lindex;
    fm_int lword;

    lindex = (addr - baseAddr) / stride;
    lword = DWORD_OFF((addr - baseAddr) % stride);

    /* Note: The "lword < stride" check is a weak check as the stride might be
     * larger than the register width. */
    if ( ( ( lindex >= 0 ) && ( lindex < entries ) ) &&
         ( ( lword >= 0 ) && ( lword < stride ) ) )
    {
        *index = lindex;

        if (word != NULL)
        {
            *word = lword;
        }

        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;

}   /* end hlpModelGetOffsetMult1 */




/*****************************************************************************/
/** hlpModelGetOffsetMult2
 *
 * \desc            Retrieves the 2-dimensional register offset in terms of
 *                  (index1, index0, word) for the specified register address.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       baseAddr is the register base address, i.e. the address of
 *                  entry (0, 0), word 0.
 *
 * \param[in]       entries1 is the number of banks.
 *
 * \param[in]       entries0 is the number of register entries per bank.
 *
 * \param[in]       stride1 is the number of words in between each bank.
 *
 * \param[in]       stride0 is the number of words in between each register
 *                  entry.
 *
 * \param[in]       index1 points to caller-allocated storage in which this
 *                  function stores the index1 portion (the bank) of the
 *                  register offset.
 *
 * \param[in]       index0 points to caller-allocated storage in which this
 *                  function stores the index0 portion (the bank entry) of the
 *                  register offset.
 *
 * \param[in]       word points to caller-allocated storage in which this
 *                  function stores the word portion of the register offset.
 *                  Set to NULL if the register is a 2-dimensional,
 *                  single-word, multi-entry register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if the register address is out-of-bounds.
 *
 *****************************************************************************/
fm_status hlpModelGetOffsetMult2(fm_uint32 addr,
                                 fm_uint32 baseAddr,
                                 fm_int    entries1,
                                 fm_int    entries0,
                                 fm_int    stride1,
                                 fm_int    stride0,
                                 fm_int *  index1,
                                 fm_int *  index0,
                                 fm_int *  word)
{
    fm_int lindex0;
    fm_int lindex1;
    fm_int lword;

    lindex1 = (addr - baseAddr) / stride1;
    lindex0 = ((addr - baseAddr) % stride1) / stride0;
    lword = DWORD_OFF(((addr - baseAddr) % stride1) % stride0);

    /* Note: The "lword < stride" check is a weak check as the stride might be
     * larger than the register width. */
    if ( ( ( lindex1 >= 0 ) && ( lindex1 < entries1 ) ) &&
         ( ( lindex0 >= 0 ) && ( lindex0 < entries0 ) ) &&
         ( ( lword >= 0 ) && ( lword < stride0 ) ) )
    {
        *index1 = lindex1;
        *index0 = lindex0;

        if (word != NULL)
        {
            *word = lword;
        }

        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;

}   /* end hlpModelGetOffsetMult2 */


/*****************************************************************************/
/** hlpModelGetOffsetMult3
 *
 * \desc            Retrieves the 3-dimensional register offset in terms of
 *                  (index2, index1, index0, word) for the specified register 
 *                  address.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       baseAddr is the register base address, i.e. the address of
 *                  entry (0, 0, 0), word 0.
 *
 * \param[in]       entries2 is the number of groups.
 * * 
 * \param[in]       entries1 is the number of banks.
 *
 * \param[in]       entries0 is the number of register entries per bank.
 *
 * \param[in]       stride2 is the number of words in between each group.
 *
 * \param[in]       stride1 is the number of words in between each bank.
 *
 * \param[in]       stride0 is the number of words in between each register
 *                  entry.
 * 
 * \param[in]       index2 points to caller-allocated storage in which this
 *                  function stores the index2 portion (the group) of the
 *                  register offset.
 *
 * \param[in]       index1 points to caller-allocated storage in which this
 *                  function stores the index1 portion (the bank) of the
 *                  register offset.
 *
 * \param[in]       index0 points to caller-allocated storage in which this
 *                  function stores the index0 portion (the bank entry) of the
 *                  register offset.
 *
 * \param[in]       word points to caller-allocated storage in which this
 *                  function stores the word portion of the register offset.
 *                  Set to NULL if the register is a 2-dimensional,
 *                  single-word, multi-entry register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if the register address is out-of-bounds.
 *
 *****************************************************************************/
fm_status hlpModelGetOffsetMult3(fm_uint32 addr,
                                 fm_uint32 baseAddr,
                                 fm_int    entries2,
                                 fm_int    entries1,
                                 fm_int    entries0,
                                 fm_int    stride2,
                                 fm_int    stride1,
                                 fm_int    stride0,
                                 fm_int *  index2,
                                 fm_int *  index1,
                                 fm_int *  index0,
                                 fm_int *  word)
{
    fm_int lindex0;
    fm_int lindex1;
    fm_int lindex2;
    fm_int lword;
    
    lindex2 = (addr - baseAddr) / stride2;
    lindex1 = ((addr - baseAddr) % stride2) / stride1;
    lindex0 = (((addr - baseAddr) % stride2) % stride1) / stride0;
    lword = DWORD_OFF((((addr - baseAddr) % stride2) % stride1) % stride0);

    /* Note: The "lword < stride" check is a weak check as the stride might be
     * larger than the register width. */
    if ( ( ( lindex2 >= 0 ) && ( lindex2 < entries2 ) ) &&
         ( ( lindex1 >= 0 ) && ( lindex1 < entries1 ) ) &&
         ( ( lindex0 >= 0 ) && ( lindex0 < entries0 ) ) &&
         ( ( lword >= 0 ) && ( lword < stride0 ) ) )
    {
        *index2 = lindex2;
        *index1 = lindex1;
        *index0 = lindex0;

        if (word != NULL)
        {
            *word = lword;
        }

        return FM_OK;
    }

    return FM_ERR_NOT_FOUND;

}   /* end hlpModelGetOffsetMult3 */


/*****************************************************************************/
/** hlpModelGetPortMap
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
fm_status hlpModelGetPortMap(fm_platformPort *portMap, fm_int numPorts)
{
    fm_int i;
    fm_int mapType;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "portMap=%p numPorts=%d\n",
                 (void *) portMap,
                 numPorts);

    if (portMap == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    mapType = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_PORT_MAP_TYPE,
                                   FM_AAD_API_PLATFORM_MODEL_PORT_MAP_TYPE);

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "Using %s = %d\n", 
                 FM_AAK_API_PLATFORM_MODEL_PORT_MAP_TYPE,
                 mapType);

    if (mapType == 0)
    {
        for (i = 0; i < numPorts; i++)
        {
            portMap[i].logPort  = i;
            portMap[i].physPort = i;
        }

        /* End of table delimiter */
        portMap[numPorts].logPort  = -1;
        portMap[numPorts].physPort = -1;
    }
    else if (mapType == 1)
    {
        /* Port Map for used for DV/SV (1:1 mapping) */
        for (i = 0; i < numPorts; i++)
        {
            portMap[i].logPort  = i;
            portMap[i].physPort = i;
        }

        portMap[numPorts].logPort  = -1;
        portMap[numPorts].physPort = -1;
    }
    else
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Invalid HLP platform port map type (%d)\n",
                     mapType);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end hlpModelGetPortMap */




/*****************************************************************************/
/** hlpModelInitialize
 * \ingroup model10k
 *
 * \desc            Allocate and initialize an instance of the HLP white
 *                  model. This function must be called once at initialization
 *                  time for each chip to be instantiated (and prior to calling
 *                  any other model interface functions).
 *
 * \param[in]       chipModel points to the address of a void pointer which
 *                  will be filled in by this function with the address of a
 *                  HLP model object.
 *
 * \param[in]       sw is the switch number for the model instance.
 *
 * \param[in]       funcPtrs should be set to NULL for the HLP white model.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if funcPtrs is non-NULL.
 * \return          FM_ERR_NO_MEM if the HLP model object could not be
 *                  allocated.
 *
 *****************************************************************************/
fm_status hlpModelInitialize(void **chipModel, fm_int sw, void *funcPtrs)
{
    hlp_model *model;
    fm_status     status;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "chipModel=%p sw=%d funcPtrs=%p\n",
                 (void *) chipModel,
                 sw,
                 funcPtrs);

    /* Check min/max */
    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }

    if (funcPtrs != NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    model = (hlp_model *) fmAlloc( sizeof(hlp_model) );

    /* Return if no space */
    if (model == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_NO_MEM);
    }

    *chipModel = model;

    FM_CLEAR(*model);

    /* Set what needs to be set to something else than 0 */
    model->sw = sw;
    model->allowStateChange = TRUE;

    /**************************************************
     * Create a lock to provide exclusive access to
     * model state and registers. Note that while the
     * purpose of this lock mirrors the
     * regLock/Platform CSR lock, it cannot have a
     * precedence relationship with any API or platform
     * layer locks.
     **************************************************/

    status = fmCreateLockV2("HLP Model",
                            sw,
                            FM_LOCK_SUPER_PRECEDENCE,
                            &model->modelLock);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    /***************************************************
     * Initialize the HLP white model stages if
     * needed.
     **************************************************/

    status = hlpModelInitializeInternal(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

ABORT:
    fmFree(model);
    *chipModel = NULL;

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end hlpModelInitialize */




/*****************************************************************************/
/** hlpModelTick
 * \ingroup model10k
 *
 * \desc            The top-level handler for HLP white model periodic
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
 *                  previously instantiated by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelTick(fm_int sw, fm_uint32 *interrupt)
{
    hlp_model *model;

    model = FM_MODEL_GET_STATE(sw);

    /* Check the switch exists */
    if (!model)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    FM_MODEL_TAKE_LOCK(model);

    /* FIXME. */
    /* Call different tick */
    //hlpModelL2LTick(model);

    /* Return interrupts */
    *interrupt = model->INTERRUPT_DETECT != FM_LITERAL_U64(0);;

    FM_MODEL_DROP_LOCK(model);
    return FM_OK;

}   /* end hlpModelTick */



/*****************************************************************************/
/** hlpModelReadCSR
 * \ingroup model10k
 *
 * \desc            Read 32 bits of an HLP CSR register from the model.
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
 *                  previously instantiated by ''hlpModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    hlp_model *model;
    fm_status      status;

    if (addr & 0x3)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Unaligned 32-bit address 0x%x\n", addr); 
        return FM_ERR_INVALID_ARGUMENT;        
    }

    model = FM_MODEL_GET_STATE(sw);
    
    /* Check the switch exists */
    if (model == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }
    else if (addr > HLP_MODEL_MAX_HW_ADDR)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    FM_MODEL_TAKE_LOCK(model);

    /*for L2 lookup unit: MA_TABLE_INDIRECT_READ response; returns "FM_OK" all the time*/
  	status = hlpModelReadUpdateCSRInternal(model, addr, *value);

    /* Mac stats is returned from a temporary memory and not directly from
     * model registers
     */
    if (!hlpModelMacStatsGet(model, addr, value))
    {
        *value = model->registers[DWORD_OFF(addr)];
    }

    /* Read through to the HLP white model. */
    status = hlpModelReadCSRInternal(model, addr, *value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    FM_MODEL_DROP_LOCK(model);

    return status;

}   /* end hlpModelReadCSR */



/*****************************************************************************/
/** hlpModelReadCSRMult
 * \ingroup model10k
 *
 * \desc            Read multiple 32-bit HLP CSR registers from the model.
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
 *                  previously instantiated by ''hlpModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelReadCSRMult(fm_int     sw,
                              fm_uint32  addr,
                              fm_int     n,
                              fm_uint32 *value)
{
    fm_int        i;
    hlp_model *model;
    fm_status     err = FM_OK;

    model = FM_MODEL_GET_STATE(sw);

    /* Check the switch exists */
    if (!model)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    FM_MODEL_TAKE_LOCK(model);

    for (i = 0 ; i < n ; i++)
    {
        err = hlpModelReadCSR(sw, addr + i*4, &value[i]);

        if (err != FM_OK)
        {
            break;
        }
    }

    FM_MODEL_DROP_LOCK(model);
    return err;


}   /* end hlpModelReadCSRMult */



/*****************************************************************************/
/** hlpModelReadCSR64
 * \ingroup model10k
 *
 * \desc            Read a 64-bit HLP CSR register.
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
 *                  previously instantiated by ''hlpModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value)
{
    fm_uint32     hi;
    fm_uint32     lo;
    hlp_model *model;
    fm_status     err = FM_OK;

    if (addr & 0x7)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Unaligned 64-bit address 0x%x\n", addr); 
        return FM_ERR_INVALID_ARGUMENT;        
    }

    model = FM_MODEL_GET_STATE(sw);

    /* Check the switch exists */
    if (!model)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    FM_MODEL_TAKE_LOCK(model);

    err = hlpModelReadCSR(sw, addr + 0, &lo);

    if (err == FM_OK)
    {
        err = hlpModelReadCSR(sw, addr + 4, &hi);

        if (err == FM_OK)
        {
            *value = ( ( (fm_uint64) hi ) << 32 ) | ( (fm_uint64) lo );
        }
    }

    FM_MODEL_DROP_LOCK(model);
    return err;

}   /* end hlpModelReadCSR64 */




/*****************************************************************************/
/** hlpModelReadCSRMult64
 * \ingroup model10k
 *
 * \desc            Read multiple 64-bit HLP CSR registers.
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
 *                  previously instantiated by ''hlpModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelReadCSRMult64(fm_int     sw,
                                fm_uint32  addr,
                                fm_int     n,
                                fm_uint64 *value)
{
    fm_int        i;
    hlp_model *model;
    fm_status     err = FM_OK;

    model = FM_MODEL_GET_STATE(sw);

    /* Check the switch exists */
    if (!model)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    FM_MODEL_TAKE_LOCK(model);

    for (i = 0 ; i < n ; i++)
    {
        err = hlpModelReadCSR64(sw, addr + (i * 2)*4, &value[i]);

        if (err != FM_OK)
        {
            break;
        }
    }

    FM_MODEL_DROP_LOCK(model);
    return err;

}   /* end hlpModelReadCSRMult64 */




/*****************************************************************************/
/** hlpModelWriteCSR
 * \ingroup model10k
 *
 * \desc            Write a 32-bit value to an HLP CSR register.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       newValue is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw does not identify a model
 *                  previously instantiated by ''hlpModelInitialize''.
 * \return          FM_ERR_UNINITIALIZED if the model was not properly
 *                  intialized by ''hlpModelInitialize''.
 *
 *****************************************************************************/
fm_status hlpModelWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 newValue)
{
    hlp_model *model;
    fm_uint32     rwMask;
    fm_uint32     roMask;
    fm_uint32     cwMask;
    fm_uint32     cw1Mask;
    fm_uint32     rvMask;
    fm_uint32     value;
    fm_uint32     oldValue;
    fm_status     err = FM_OK;
    
    if (addr & 0x3)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Unaligned 32-bit address 0x%x\n", addr); 
        return FM_ERR_INVALID_ARGUMENT;        
    }

    model = FM_MODEL_GET_STATE(sw);

    /* Check the switch exists */
    if (!model)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    /**************************************************
     * The register access table is first scanned to see
     * if there are any fields in the register word that
     * are not RW. If there are, then special processing
     * is done to manage the non-RW bit fields.
     **************************************************/

    hlpModelGetRegisterAccess(addr,
                              &rwMask,
                              &roMask,
                              &cwMask,
                              &cw1Mask,
                              &rvMask);

//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: hlpModelWriteCSR: newValue=0x%x, addr=0x%x: rwMask=0x%x, roMask=0x%x, cwMask=0x%x, cw1Mask=0x%x, rvMask=0x%x. \n", newValue, addr, rwMask, roMask, cwMask, cw1Mask, rvMask);

    if (roMask == 0xFFFFFFFF) /* ||
        ((roMask == 0xFFFFFF1F) && (!rwMask) && (!cwMask) && (!cw1Mask)) ||
        ((roMask == 0x00FF7FFF) && (!rwMask) && (!cwMask) && (!cw1Mask))) */
    {
        /* No bits can be modified */
        return FM_OK;
    }

    FM_MODEL_TAKE_LOCK(model);

    if (rwMask == 0xffffffff)
    {
        /**************************************************
         * Most registers are fully read-write, so just
         * take care of them in the most efficient manner,
         * even though the 'else' section below would handle
         * them just fine.
         *************************************************/

        err = WriteCSRAbsolute(model, addr, newValue, &oldValue);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        /**************************************************
         * Write through to the white model.
         **************************************************/

        err = hlpModelWriteCSRInternal(model, addr, newValue, oldValue, FALSE);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }
    else
    {
        /**************************************************
         * Deal with registers with non-RW bit fields.
         **************************************************/

        /* Get current value for register. */
//        err = hlpModelReadCSR(sw, addr, &value);
//        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
        value = model->registers[DWORD_OFF(addr)];

        /**************************************************
         * Read-Write bits.
         **************************************************/

        if (rwMask)
        {
            /* Zero out all RW bits. */
            value &= ~rwMask;

            /* Set back to 1 any RW bit specified as a 1. */
            value |= (newValue & rwMask);
        }

        /**************************************************
         * Clear-on-Write bits.
         **************************************************/

        if (cwMask)
        {
            /* Zero out all CW bits regardless of what we're writing. */
            value &= ~cwMask;
        }

        /**************************************************
         * Clear-on-Write-1 bits.
         **************************************************/

        if (cw1Mask)
        {
        	/* see bug 31104 */
        	if (addr != HLP_ENTRY_COUNT_IP(0)) /* ??? see bug 33070 ??? */
        	{
        		/* Limit CW1 mask to the '1' bits in newValue. */
        		cw1Mask &= newValue;
        		/* Zero-out those bits in the final value (do 'clear on write'). */
        		value &= ~cw1Mask;
        	}
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: hlpModelWriteCSR cw1Mask: addr=0x%x newValue=0x%x oldValue=0x%x value=0x%x (cw1Mask=0x%x).\n", addr, newValue, oldValue, value, cw1Mask);
        }

        /**************************************************
         * Reserved bits.
         *
         * These should always be written as a zero. It is
         * the caller's responsibility to do this, so we
         * should really take no action here, but there is
         * also a requirement that RV bits read back as a
         * zero regardless of what may be written to them.
         * We could handle that in hlpModelReadCSR, but
         * it's easier to just make sure the caller never
         * writes a 1 here, so treat it the same as CW.
         **************************************************/

        if (rvMask)
        {
            /* Zero out all RV bits regardless of what we're writing. */
            value &= ~rvMask;
        }

        /**************************************************
         * Note: We don't have to do anything for read-only
         * bits since their original value should be
         * preserved. We are assuming that none of the masks
         * returned by hlpModelGetRegisterAccess overlap.
         **************************************************/

        /**************************************************
         * Record the final value.
         **************************************************/

        err = WriteCSRAbsolute(model, addr, value, &oldValue);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        /**************************************************
         * Write through to the white model.
         **************************************************/

        err = hlpModelWriteCSRInternal(model, addr, value, oldValue, FALSE);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    }   /* end if (rwMask == 0xffffffff) */

ABORT:
    FM_MODEL_DROP_LOCK(model);

    return err;

}   /* end hlpModelWriteCSR */




/*****************************************************************************/
/** hlpModelWriteCSRMult
 * \ingroup model10k
 *
 * \desc            Write multiple 32-bit values to one or more HLP CSR
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
fm_status hlpModelWriteCSRMult(fm_int     sw,
                               fm_uint32  addr,
                               fm_int     n,
                               fm_uint32 *newValue)
{
    hlp_model *model;
    fm_status     status = FM_OK;
    fm_int        i;
    
    model = FM_MODEL_GET_STATE(sw);

    if (model == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    FM_MODEL_TAKE_LOCK(model);

    for (i = 0; i < n; i++)
    {
        status = hlpModelWriteCSR(model->sw, addr + i*4, newValue[i]);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

ABORT:
    FM_MODEL_DROP_LOCK(model);

    return status;

}   /* end hlpModelWriteCSRMult */




/*****************************************************************************/
/** hlpModelWriteCSR64
 * \ingroup model10k
 *
 * \desc            Write 64-bit values to one or more HLP CSR
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
fm_status hlpModelWriteCSR64(fm_int     sw,
                             fm_uint32  addr,
                             fm_uint64  newValue)
{
    hlp_model *model;
    fm_status     status = FM_OK;
    fm_uint32     hi;
    fm_uint32     lo;
    
    if (addr & 0x7)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Unaligned 64-bit address 0x%x\n", addr); 
        return FM_ERR_INVALID_ARGUMENT;        
    }

    model = FM_MODEL_GET_STATE(sw);

    if (model == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    hi = (fm_uint32) (newValue >> 32);
    lo = (fm_uint32) (newValue & FM_LITERAL_U64(0xFFFFFFFF));
    FM_MODEL_TAKE_LOCK(model);

    status = hlpModelWriteCSR(model->sw, addr, lo);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    status = hlpModelWriteCSR(model->sw, addr + 4, hi);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    FM_MODEL_DROP_LOCK(model);
    return status;

} /* end hlpModelWriteCSR64 */



/*****************************************************************************/
/** hlpModelWriteCSRForce64
 * \ingroup model10k
 *
 * \desc            Write 64-bit values to one or more HLP CSR registers.
 *
 *                  This function is provided for the benefit of the application
 *                  which must update the global interrupt register to
 *                  control the interrupt generation. The
 *                  hlpModelWriteCSRAbsolute64 is not visible to the
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
fm_status hlpModelWriteCSRForce64(fm_int     sw,
                                  fm_uint32  addr,
                                  fm_uint64  newValue)
{
    hlp_model *model;
    fm_status     status = FM_OK;

    if (addr & 0x7)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Unaligned 64-bit address 0x%x\n", addr); 
        return FM_ERR_INVALID_ARGUMENT;        
    }

    model = FM_MODEL_GET_STATE(sw);

    if (model == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }
    status = hlpModelWriteCSRAbsolute64(model, addr, newValue);

ABORT:
    FM_MODEL_DROP_LOCK(model);
    return status;

} /* end hlpModelWriteCSRForce64 */




/*****************************************************************************/
/** hlpModelWriteCSRMult64
 * \ingroup model10k
 *
 * \desc            Write multiple 64-bit values to one or more HLP CSR
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
fm_status hlpModelWriteCSRMult64(fm_int     sw,
                                 fm_uint32  addr,
                                 fm_int     n,
                                 fm_uint64 *newValue)
{
    hlp_model *model;
    fm_status     status = FM_OK;
    fm_int        i;
    
    model = FM_MODEL_GET_STATE(sw);

    if (model == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    FM_MODEL_TAKE_LOCK(model);

    for (i = 0; i < n; i++)
    {
        status = hlpModelWriteCSR64(model->sw, addr + (i * 2)*4, newValue[i]);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

ABORT:
    FM_MODEL_DROP_LOCK(model);

    return status;

}   /* end hlpModelWriteCSRMult64 */




/*****************************************************************************/
/** hlpModelWriteCSRAbsolute
 * \ingroup intPlatform
 *
 * \desc            Write a CSR register absolutely, without regard for
 *                  field types within the register.
 *                                                                      \lb\lb
 *                  This function is provided for the benefit of the white
 *                  model which must update register contents from the
 *                  hardware's perspective and not be constrained by the
 *                  field types (RO, CW, CW1, etc.), which are relevant only
 *                  from software's perspective.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[in]       newValue is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelWriteCSRAbsolute(hlp_model *model,
                                   fm_uint32     addr,
                                   fm_uint32     newValue)
{
    fm_status err;

    FM_MODEL_TAKE_LOCK(model);

    err = WriteCSRAbsolute(model, addr, newValue, NULL);

    FM_MODEL_DROP_LOCK(model);

    return err;

}   /* end hlpModelWriteCSRAbsolute */




/*****************************************************************************/
/** hlpModelWriteCSRMultAbsolute
 * \ingroup intPlatform
 *
 * \desc            Write multiple CSR registers bypassing the cache.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelWriteCSRMultAbsolute(hlp_model *model,
                                       fm_uint32     addr,
                                       fm_int        n,
                                       fm_uint32 *   value)
{
    fm_int        i;
    fm_status     err = FM_OK;

    FM_MODEL_TAKE_LOCK(model);

    for (i = 0 ; i < n ; i++)
    {
        err = WriteCSRAbsolute(model, addr + i*4, value[i], NULL);

        if (err != FM_OK)
        {
            break;
        }
    }

    FM_MODEL_DROP_LOCK(model);
    return err;

}   /* end hlpModelWriteCSRMultAbsolute */




/*****************************************************************************/
/** hlpModelWriteCSRAbsolute64
 * \ingroup intPlatform
 *
 * \desc            Writes a 64-bit CSR register.
 *                                                                      \lb\lb
 *                  This function is provided for the benefit of the white
 *                  model which must update register contents from the
 *                  hardware's perspective and not be constrained by the
 *                  field types (RO, CW, CW1, etc.), which are relevant only
 *                  from software's perspective.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the 64-bit data value to write.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelWriteCSRAbsolute64(hlp_model *model,
                                     fm_uint32     addr,
                                     fm_uint64     value)
{
    fm_uint32     hi  = (value >> 32);
    fm_uint32     lo  = (value & FM_LITERAL_64(0xffffffff));
    fm_status     err = FM_OK;

    FM_MODEL_TAKE_LOCK(model);

    err = WriteCSRAbsolute(model, addr + 0, lo, NULL);

    if (err == FM_OK)
    {
        err = WriteCSRAbsolute(model, addr + 4, hi, NULL);
    }

    FM_MODEL_DROP_LOCK(model);

    return err;

}   /* end hlpModelWriteCSRAbsolute64 */




/*****************************************************************************/
/** hlpModelReset
 * \ingroup model10k
 *
 * \desc            Used to simulate a reset on the HLP white model. This
 *                  service can be used to re-initialize the state or value of
 *                  variables.
 *
 * \param[in]       sw is the switch number for the model instance.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelReset(fm_int sw)
//fm_status hlpModelReset(fm_int sw, fm_int domain)
{
	FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hlp_model_main reset: hlpModelReset: sw=%d. \n", sw);
//	FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hlp_model_main reset: hlpModelReset: sw=%d domain=%d. \n", sw, domain);
    return hlpModelResetV2(sw, HLP_RESET_DOMAIN_CHIP);
}   /* end hlpModelReset */




/*****************************************************************************/
/** hlpModelResetV2
 * \ingroup model10k
 *
 * \desc            Used to simulate a reset on the HLP white model. This
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
fm_status hlpModelResetV2(fm_int sw, fm_int domain)
{
    hlp_model *     model;
    hlp_resetDomain dom;
    fm_status       status = FM_OK;
    fm_uint32       addr;
    fm_uint32       value;
    fm_bool         allowStateChange;
    fm_bool         reset;
    fm_bool         print_dbg = FALSE;

//    print_dbg = TRUE;
    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "hlp_model_main reset: sw=%d domain=%d \n", sw, domain);
    if (print_dbg)
    {
    	FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hlp_model_main reset: sw=%d domain=%d. \n", sw, domain);
    }

    model = FM_MODEL_GET_STATE(sw);

    if (model == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }
    else if ( ( domain < HLP_RESET_DOMAIN_CHIP ) ||
              ( domain >= HLP_RESET_DOMAIN_MAX ) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    dom = (hlp_resetDomain) domain;

    FM_MODEL_TAKE_LOCK(model);

    /* Temporarily disallow register cache state changes to prevent register
     * cache updates from interfering with the reset procedure. */
    allowStateChange = model->allowStateChange;
    model->allowStateChange = FALSE;

    /***************************************************
     * Re-initialize the register related HLP white
     * model state.
     **************************************************/

    for (addr = 0; addr <= HLP_MODEL_MAX_HW_ADDR; addr += 4)
    {
        if (dom == HLP_RESET_DOMAIN_CHIP ||
            dom == HLP_RESET_DOMAIN_EPL ||
            dom == HLP_RESET_DOMAIN_SWITCH)
        {
            /* All register related HLP white model state should be reset on
             * chip reset. */
            reset = TRUE;
        }
        else
        {
            /* Do not reset the HLP white model state for the current
             * address unless the address's set of reset domain contains the
             * supplied reset domain. */
            reset = hlpModelFindRegisterResetDomain(addr, dom);
        }
        if (print_dbg)
        {
//        	if ((addr >= 0x3750000) && (addr <= 0x37501ff)) {
//            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hlp_model_main reset: addr=0x%0x: reset=%0d (domain=%0d, dom=%0d). \n", addr, reset, domain, dom);
//        	}
        }

        if (reset)
        {
            value = hlpModelGetRegisterDefault(addr);

            model->registers[DWORD_OFF(addr)] = value;

            /* Write through to the HLP white model. */
            status = hlpModelWriteCSRInternal(model, addr, value, value, TRUE);
            if (status == FM_ERR_NOT_FOUND)
            {
                /* Ignore out-of-bounds errors. */
                status = FM_OK;
            }
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
        }
    }

    model->allowStateChange = allowStateChange;

ABORT:
    FM_MODEL_DROP_LOCK(model);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end hlpModelResetV2 */

