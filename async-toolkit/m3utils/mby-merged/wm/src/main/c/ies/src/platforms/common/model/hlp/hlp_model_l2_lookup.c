/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_l2_lookup.c
 * Creation Date:   June 20, 2012
 * Description:     L2_LOOKUP stage of the HLP white model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2017 Intel Corporation. All Rights Reserved.
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
fm_int l2LookupDisplayVerbose = 1;

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

fm_uint32 AllMATableIndexGlobal = 0;
fm_bool   DOSProtectionActiveGlobal[HLP_MODEL_PORTS_COUNT];

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

fm_status hlpModelL2LookupWriteCSR(hlp_model *model,
		                           fm_uint32 addr,
								   fm_uint32 value,
								   fm_bool   init)
{
	fm_int    regIndex;
	fm_int    ii;
	fm_bool   DOSProtectionActive;
    fm_status status = FM_OK;

    FM_NOT_USED(value);

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM,
                            "model=%p addr=0x%x value=0x%x\n",
                            (void *) model, addr, value);

    if (init)
    {
        /* WM reset, global variables (CSR WR or RD: doesn't matter) */
    	AllMATableIndexGlobal = 0;
    	for(ii = 0; ii < HLP_MODEL_PORTS_COUNT; ii++) {
            SetDOSProtectionActiveGlobal(ii, 0);
    	}
        return status; //so mgmt_lookup won't get triggered at cold_rest of the test
    }

    if ((addr >= HLP_MA_TABLE_INDIRECT_WRITE(0, 0)) && (addr <= HLP_MA_TABLE_INDIRECT_WRITE((HLP_MA_TABLE_INDIRECT_WRITE_ENTRIES - 1), (HLP_MA_TABLE_INDIRECT_WRITE_WIDTH - 1))))
    {
    	if ((addr & 0xf) == (HLP_MA_TABLE_INDIRECT_WRITE_WIDTH - 1)*4) /* the last word was written */
    	{
    		regIndex = (addr >> 4) & 0xff;
    	    /* MA_TABLE_INDIRECT_WRITE write access (MGMT lookup) handler */
    	    status = hlpModelL2MGMTLookup(model, regIndex);
        }
    }

    if ((addr >= HLP_MA_TABLE(0, 0, 0)) && (addr <= HLP_MA_TABLE(HLP_MA_TABLE_ENTRIES_1 - 1, (HLP_MA_TABLE_TCAM_SIZE - 1), (HLP_MA_TABLE_WIDTH - 1))))
    {
    	if ((addr & 0xf) == (HLP_MA_TABLE_WIDTH - 1)*4) /* the last word was written */
    	{
    		regIndex = (addr >> 4) & 0xffff;
    		status = hlpModelL2MATableDirectWrite(model, regIndex);
    	}
    }

    if ((addr >= HLP_MA_TABLE_SECURITY_CFG(0, 0)) && (addr <= HLP_MA_TABLE_SECURITY_CFG((HLP_MA_TABLE_SECURITY_CFG_ENTRIES - 1), (HLP_MA_TABLE_SECURITY_CFG_WIDTH - 1))))
    {
    	if ((addr & 0x7) == (HLP_MA_TABLE_SECURITY_CFG_WIDTH - 1)*4) /* the last word was written */
    	{
    		regIndex = (addr >> 3) & 0x1f;
            UpdateDOSAttackActive(model, regIndex, 0);
        }
    }

    if (addr == HLP_ENTRY_COUNT_IM(0) || addr == HLP_FWD_IM(0))
    {
    	for (ii = 0; ii < HLP_MODEL_PORTS_COUNT; ii++) {
    	    UpdateDOSAttackActive(model, ii, 4);
    	}
    }

    if (addr == HLP_ENTRY_COUNT_IP(0))
    {
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: HLP_ENTRY_COUNT_IP write access: addr=0x%x value=0x%x.\n", addr, value);
    	for (ii = 0; ii < HLP_MODEL_PORTS_COUNT; ii++) {
    		if ((value >> ii) & 0x1) {
    			UpdateDOSAttackActive(model, ii, 1);
    		}
    	}
    }

ABORT:
    HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, status);
    return status;
}

fm_status hlpModelL2LookupReadCSR(hlp_model *model,
		                          fm_uint32 addr,
								  fm_uint32 value)
{
	fm_int    regIndex;
    fm_status status = FM_OK;

    FM_NOT_USED(value);

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM,
                            "model=%p addr=0x%x value=0x%x\n",
                            (void *) model, addr, value);

/*
    if ((addr >= HLP_MA_TABLE_INDIRECT_READ(0, 0)) && (addr <= HLP_MA_TABLE_INDIRECT_READ((HLP_MA_TABLE_INDIRECT_READ_ENTRIES - 1), (HLP_MA_TABLE_INDIRECT_READ_WIDTH - 1))))
    {
    	if ((addr & 0xf) == (HLP_MA_TABLE_INDIRECT_READ_WIDTH - 1)*4) //the last word was read
    	{
    		regIndex = (addr >> 4) & 0xff;
    	    //MA_TABLE_INDIRECT_READ read access (dirty reporting) handler
    	    status = hlpModelL2MGMTIndirectMATableRead(model, regIndex);
    	}
    }
*/

    if ((addr >= HLP_MA_TABLE(0, 0, 0)) && (addr <= HLP_MA_TABLE(HLP_MA_TABLE_ENTRIES_1 - 1, (HLP_MA_TABLE_TCAM_SIZE - 1), (HLP_MA_TABLE_WIDTH - 1))))
    {
    	if ((addr & 0xf) == (HLP_MA_TABLE_WIDTH - 1)*4) /* the last word was read */
    	{
    		regIndex = (addr >> 4) & 0xffff;
            status = hlpModelL2MATableDirectRead(model, regIndex);
    	}
    }

ABORT:
    HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, status);
    return status;
}

fm_status hlpModelL2LookupReadUpdateCSR(hlp_model *model,
		                                fm_uint32 addr,
								        fm_uint32 value)
{
	fm_int regIndex;
////	fm_int regSlice;
    fm_status status = FM_OK;

    FM_NOT_USED(value);

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM,
                            "model=%p addr=0x%x value=0x%x\n",
                            (void *) model, addr, value);

    if ((addr >= HLP_MA_TABLE_INDIRECT_READ(0, 0)) && (addr <= HLP_MA_TABLE_INDIRECT_READ((HLP_MA_TABLE_INDIRECT_READ_ENTRIES - 1), (HLP_MA_TABLE_INDIRECT_READ_WIDTH - 1))))
    {
//    	if ((addr & 0xf) == (HLP_MA_TABLE_INDIRECT_READ_WIDTH - 1)*4) /* the last word was read */
        if ((addr & 0xf) == 0) /* the first word was read */
    	{
////    		regSlice = (addr >> 2) & 0x3;
    		regIndex = (addr >> 4) & 0xff;
    	    /* MA_TABLE_INDIRECT_READ read access (dirty reporting) handler */
    	    status = hlpModelL2MGMTIndirectMATableRead(model, regIndex);
////    	    status = hlpModelL2MGMTIndirectMATableRead(model, regIndex, regSlice);
    	}
    }

ABORT:
    HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, status);
    return status;
}

/***************************************************************************/
/** SetDOSProtectionActiveGlobal
 *
 * \desc         Set/Reset the bit in DOSProtectionActiveGlobal array
 *
 * \param[in]    updatePortNumber: the Index of the array member that been updated
 *
 * \param[in]    val: the value of the array member written
 * **************************************************************************/
void SetDOSProtectionActiveGlobal(fm_byte updatePortNumber,
                                  fm_bool val)
{
    DOSProtectionActiveGlobal[updatePortNumber] = val;
}

/***************************************************************************/
/** GetDOSProtectionActiveGlobal
 *
 * \desc         Get the bit in DOSProtectionActiveGlobal array
 *
 * \param[in]    retrPort: the Index of the array member that been retrieved
 * 
 * \return       the value of DOSProtectionActiveGlobal[retrPort]
 * **************************************************************************/
fm_bool GetDOSProtectionActiveGlobal(fm_byte retrPort)
{
    return DOSProtectionActiveGlobal[retrPort];
}

/*****************************************************************************/
/** GetIngressVidTableEntry
 * \ingroup intModel
 *
 * \desc            Reads INGRESS_VID_TABLE register data from register cache
 *                  based on ingress vlan ID and populates corresponding
 *                  register struct,
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   entry is the struct for INGRESS_VID_TABLE that is populated
 *                  with data from register cache.
 *
 *****************************************************************************/
static void GetIngressVidTableEntry(hlp_model            *model,
                                    hlpIngressVidTable   *entry)
{
    hlp_modelState  *state;
    fm_uint32       *regPtr;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_INGRESS_VID_TABLE(state->L2_IVID1, 0));
    
    entry->TRAP_IGMP = 
        FM_ARRAY_GET_BIT(regPtr, HLP_INGRESS_VID_TABLE, TRAP_IGMP);
    entry->REFLECT = 
        FM_ARRAY_GET_BIT(regPtr, HLP_INGRESS_VID_TABLE, REFLECT);
//    entry->COUNTER_INDEX =
//        FM_ARRAY_GET_FIELD(regPtr, HLP_INGRESS_VID_TABLE, COUNTER_INDEX);
    entry->MEMBERSHIP = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_INGRESS_VID_TABLE, MEMBERSHIP);
    
} /* end GetIngressVidTableEntry*/

/*****************************************************************************/
/** GetEgressVidTableEntry
 * \ingroup intModel
 *
 * \desc            Reads EGRESS_VID_TABLE register data from register cache
 *                  based on egress vlan ID and populates corresponding register
 *                  struct.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   entry is the struct for EGRESS_VID_TABLE that is populated
 *                  with data from register cache.
 *
 *****************************************************************************/
static void GetEgressVidTableEntry(hlp_model           *model,
                                   hlpEgressVidTable   *entry)
{
    hlp_modelState  *state;
    fm_uint32       *regPtr;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_EGRESS_VID_TABLE(state->L2_EVID1,0));
    
    entry->TRIG_ID = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_EGRESS_VID_TABLE, TRIG_ID);
    entry->MEMBERSHIP = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_EGRESS_VID_TABLE, MEMBERSHIP);
    
} /* end GetEgressVidTableEntry*/


/*****************************************************************************/
/** GetMtuSize
 * \ingroup intModel
 *
 * \desc            Reads MTU_TABLE entry at specified index and returns MTU
 *                  size
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       mtuIndex is the MTU_TABLE index
 *  
 * \return          MTU Size          
 *****************************************************************************/
static fm_uint16 GetMtuSize(hlp_model   *model,
                            fm_byte     mtuIndex)
{
    fm_uint32   *regPtr;
    fm_uint16   mtuSize;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MTU_TABLE(mtuIndex, 0));
    
    mtuSize = FM_ARRAY_GET_FIELD(regPtr, HLP_MTU_TABLE, MTU);
    
    return mtuSize;
} /* end GetMtuSize*/


/*****************************************************************************/
/** GetMaTableEntry
 * \ingroup intModel
 *
 * \desc            Reads MA_TABLE register data from register cache
 *                  at specified index and populates corresponding register
 *                  struct.
 *
 * \param[in]       model points to the switch data structure.
 * 
 * \param[in]       set is the MA Table bank for which MA Table entry needs to be
 *                  read.
 *
 * \param[in]       maTblIdx is the index at which MA Table entry needs to be
 *                  read.
 *
 * \param[in,out]   entry is the struct for MA_TABLE that is populated
 *                  with data from register cache.
 *
 *****************************************************************************/
static void GetMaTableEntry(hlp_model    *model,
                            fm_byte      set,
                            fm_uint16    maTblIdx, 
                            hlpMaTable   *entry)
{
    hlp_modelState  *state;
    fm_uint32       *regPtr;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE(set, maTblIdx, 0));
    
    entry->OLD_PORT =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, OLD_PORT);
    entry->NEW_PORT =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, NEW_PORT);
    entry->ENTRY_TYPE = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, ENTRY_TYPE);
    entry->TRIG_ID = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, TRIG_ID);
    entry->S_GLORT = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, S_GLORT); 
    entry->D_GLORT = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, D_GLORT);
    entry->L2_DOMAIN = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, L2_DOMAIN);
    entry->VID = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, VID);
    entry->MAC_ADDRESS = 
        FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_TABLE, MAC_ADDRESS);
    
} /* end GetMaTableEntry*/


/*****************************************************************************/
/** ComputeMaTableHash
 * \ingroup intModel
 *
 * \desc            For a given MAC address, VID and L2Domain, calculate the
 *                  corresponding hash entries in the MAC Table for all banks.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       macAddr is the MAC address.
 *
 * \param[in]       vid is the 12-bit Vlan ID.
 *
 * \param[in]       l2Domain  is the 9-bit L2 Domain.
 *
 * \param[out]      hashes is an array into which this function should place
 *                  the indexes of each of the hash bin entries for the
 *                  specified arguments. 
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/

static fm_status ComputeMaTableHash(hlp_model     *model,
                                    fm_macaddr    macAddr,
                                    fm_uint16     vid,
                                    fm_uint16     l2Domain,
                                    fm_uint16     *hashes)
{

    fm_int          i;
    fm_byte         hashKey[HLP_MODEL_N_MA_HASH_KEYS];
    fm_uint32       hash;
    fm_uint32       chash;
    fm_uint32       khash;

    FM_NOT_USED(model);
    
    for(i = 0; i < 6; i++)
    {
        hashKey[5-i] = (macAddr >> 8*i) & 0xff;
    
    }

    hashKey[6] = (vid >> 4) & 0xff;
    hashKey[7] = ((vid & 0xf) << 4)| ((l2Domain >> 5) & 0xf);
    hashKey[8] = (l2Domain << 3) & 0xff;
    
    //for(i = 0; i<HLP_MODEL_N_MA_HASH_KEYS; i++)
    //    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hash_key[%d]=0x%x\n", i, hashKey[i]);
  

    hash = fmCrc32ByteSwap(hashKey, HLP_MODEL_N_MA_HASH_KEYS); 
    chash = fmCrc32CByteSwap(hashKey, HLP_MODEL_N_MA_HASH_KEYS); 
    khash = fmCrc32KByteSwap(hashKey, HLP_MODEL_N_MA_HASH_KEYS); 

    hashes[0] = hash & 0x1fff;
    hashes[1] = ((hash >> 16) & 0x1fff);
    hashes[2] = (chash & 0x1fff);
    hashes[3] = ((chash >> 16) & 0x1fff);
    hashes[4] = (khash & 0x1fff);

    return FM_OK;

}    /* end ComputeMaTableHash */



/*****************************************************************************/
/** UpdateMaUsedTable
 * \ingroup intModel
 *
 * \desc            Updates Source and destination MA_USED Tables if entry 
 *                  matches on MAC Lookup.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
//static void UpdateMaUsedTable(hlp_model *model)
//{
//    hlp_modelState      *state = &model->packetState;
//    fm_uint16           maUsedIndex;
//    fm_uint16           maTblIndex;
//    fm_uint64           used;           
//    fm_uint32           *regPtr;
//
//    /*Update Destination MA Used table */
//    if (state->DA_HIT)
//    {
//        maTblIndex = (state->DA_INDEX) | (state->DA_SET << 13);
// 
//        maUsedIndex = maTblIndex >> 6;
//        
//        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_USED_TABLE(0, maUsedIndex, 0));
//
//    
//
//        used = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_USED_TABLE, USED);
//        FM_SET_UNNAMED_FIELD64(used, (maTblIndex & 0x3F), 1, 1);
//        
//        FM_ARRAY_SET_FIELD64(regPtr, HLP_MA_USED_TABLE, USED, used);
//    }
//   
//    /*Update Source MA Used table */
//    if (state->SA_HIT)
//    {
//        maTblIndex = (state->SA_INDEX) | (state->SA_SET << 13);
// 
//        maUsedIndex = maTblIndex >> 6;
//        
//        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_USED_TABLE(1, maUsedIndex, 0));
//
//    
//
//        used = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_USED_TABLE, USED);
//        FM_SET_UNNAMED_FIELD64(used, (maTblIndex & 0x3F), 1, 1);
//        
//        FM_ARRAY_SET_FIELD64(regPtr, HLP_MA_USED_TABLE, USED, used);
//    }
//
//}   /* end UpdateMaUsedTable */



/*****************************************************************************/
/** ProcessLookup
 * \ingroup intModel
 *
 * \desc            Processes the destination MAC address lookup result.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *****************************************************************************/
fm_status ProcessLookup(hlp_model *model)
{
    hlp_modelState  *state = &model->packetState;
    fm_uint32       *regPtr;
    fm_status       err = FM_OK;

    state->GLORT_FORWARDED = 0;
    state->FLOOD_FORWARDED = 0;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FLOOD_GLORT_TABLE(state->L2_EDOMAIN, 0));
    
    if ((state->IDGLORT != 0) && (state->FLOOD_SET == 0))
    {
        /* no change to dglort */
        state->GLORT_FORWARDED = 1;
    }
    else 
    {   
        err = hlpModelLookupAddress(model,
                                0, /*DST lookup*/
                                state->L2_DMAC,
                                state->L2_EVID1,
                                state->L2_EDOMAIN,
                                &state->DA_INDEX,
                                &state->DA_SET,
                                &state->DA_HIT,
                                &state->DA_RESULT);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
        state->DA_LOOKUP_PERFORMED = 1;

        if ((state->DA_HIT) && (state->DA_RESULT.D_GLORT != 0))
        {
            state->IDGLORT = state->DA_RESULT.D_GLORT;
            if(state->DA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL)
            {
                state->AMASK |= HLP_MODEL_AMASK_DROP_PROVISIONAL;
                state->DROP_PROVISIONAL = 1;
            }
        }
        else if ((state->IDGLORT != 0) && (state->FLOOD_SET == 1))
        {
            /* no change to dglort */
            state->GLORT_FORWARDED = 1;
        }
        else if (fmModelIsBroadcastMacAddress(state->L2_DMAC))
        {
            state->IDGLORT = FM_ARRAY_GET_FIELD(regPtr, HLP_FLOOD_GLORT_TABLE, BROADCAST_GLORT);
        }
        else if (fmModelIsMulticastMacAddress(state->L2_DMAC))
        {
            state->IDGLORT = FM_ARRAY_GET_FIELD(regPtr, HLP_FLOOD_GLORT_TABLE, FLOOD_MULTICAST_GLORT);
            state->FLOOD_FORWARDED = 1;
        }
        else
        {
            state->IDGLORT = FM_ARRAY_GET_FIELD(regPtr, HLP_FLOOD_GLORT_TABLE, FLOOD_UNICAST_GLORT);
            state->FLOOD_FORWARDED = 1;
        }
    }

    ABORT:
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
}   /* end ProcessLookup */



/*****************************************************************************/
/** GetCanonicalGlortCamEntry 
 * \ingroup intModel
 *
 * \desc            Reads CANONICAL_GLORT_CAM register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       camIndex points to the entry that needs to be read from CAM.
 *
 * \param[in,out]   camEntry is the struct for CANONICAL_GLORT_CAM that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetCanonicalGlortCamEntry(hlp_model                 *model,
                                      fm_byte                   camIndex,
                                      hlpFwdCanonicalGlortCam   *camEntry)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_CANONICAL_GLORT_CAM(camIndex, 0));

    camEntry->PORT_FIELD_SIZE = FM_GET_FIELD(*regPtr, HLP_FWD_CANONICAL_GLORT_CAM, PORT_FIELD_SIZE);
    camEntry->MASK_SIZE = FM_GET_FIELD(*regPtr, HLP_FWD_CANONICAL_GLORT_CAM, MASK_SIZE);
    camEntry->LAG_GLORT = FM_GET_FIELD(*regPtr, HLP_FWD_CANONICAL_GLORT_CAM, LAG_GLORT);

} /* GetCanonicalGlortCamEntry */



/*****************************************************************************/
/** ComputeCSGlort
 * \ingroup intModel
 *
 * \desc            Computes the canonical source GLORT.
 *
 * \param[in]       model points to the switch model state.
 *
 *****************************************************************************/
static void ComputeCSGlort(hlp_model *model)
{
    hlpFwdCanonicalGlortCam     camEntry;
    hlp_modelState              *state;
    fm_uint16                   mask;
    fm_int                      i;
    
    state = &model->packetState;
    state->PORT_FIELD_SIZE = 0;

    for (i = 0; i < HLP_FWD_CANONICAL_GLORT_CAM_ENTRIES; i++)
    {
        GetCanonicalGlortCamEntry(model, i, &camEntry);
        mask = (0xFFFF << camEntry.MASK_SIZE) & 0xFFFF;
        if ( (state->SGLORT & mask) == camEntry.LAG_GLORT )
        {
            state->PORT_FIELD_SIZE = camEntry.PORT_FIELD_SIZE;
        }
    }
    state->CSGLORT = 
        state->SGLORT & (0xFFFF << state->PORT_FIELD_SIZE) & 0xFFFF;

}   /* end ComputeCSGlort */



static void ExistingProvEntryCheck(hlp_model    *model,
                                   fm_macaddr   macAddr,
                                   fm_uint16    vid,
                                   fm_uint16    l2Domain)
{
    hlp_modelState      *state = &model->packetState;
    fm_byte             lset = HLP_MAC_ADDR_BANK_COUNT - 1;
    fm_uint16           camIndex = HLP_MODEL_MA_TABLE_CAM_ENTRIES;
    hlpMaTable          lentry;
    fm_status           status = FM_OK;
    fm_uint16           lindices[HLP_MAC_ADDR_BANK_COUNT];
    fm_uint16           lvid = (state->LEARN_MODE == 1) ? vid : 0;

  	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
                "Looking for an existing provisional entry with {L2Domain=0x%x, VID=0x%x, MACAddress=0x%llx}...\n",
                l2Domain, lvid, macAddr);

    state->PROV_SET = 0;
    state->PROV_INDEX = 0xffff;
    state->MOVED_PROV_ENTRY_EXISTED = FALSE;

    //CAM LOOKUP
    while (camIndex > 0)
    {
        
        camIndex--;
        GetMaTableEntry(model, lset, camIndex, &lentry);

        if ((lentry.MAC_ADDRESS == macAddr) &&
            (lentry.VID == lvid) &&
            (lentry.L2_DOMAIN == l2Domain) &&
            (lentry.ENTRY_TYPE != HLP_MODEL_MA_ENTRY_TYPE_NOT_USED))
            //&&(lentry.S_GLORT != state->CSGLORT))
        {

           // FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
           // 		      "lentry.TRIG_ID=0x%x, lentry.NewPort=0x%x, lentry.OldPort=0x%x\n",
		   //		      lentry.TRIG_ID, lentry.NewPort, lentry.OldPort);
           if (lentry.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL)
           {
                state->MOVED_PROV_ENTRY_EXISTED = TRUE;
                state->PROV_INDEX = camIndex;
                state->PROV_SET = lset;
                WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
                             "Existing Provisional Entry found in CAM, set=%d, idx=%d\n",
                             lset, camIndex);
           }
           break;
        }
    }
    
    if(!state->MOVED_PROV_ENTRY_EXISTED)
    {
        //HASH Lookup
        status = ComputeMaTableHash(model, 
                                    macAddr, 
                                    lvid, 
                                    l2Domain, 
                                    lindices);
        
        while (lset > 0)
        {
            lset--;
            GetMaTableEntry(model, lset, lindices[lset], &lentry);

            if ((lentry.MAC_ADDRESS == macAddr) &&
                (lentry.VID == lvid) &&
                (lentry.L2_DOMAIN == l2Domain) &&
                (lentry.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL))
    //            && (lentry.S_GLORT != state->CSGLORT))
            {
            	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
                             "Existing provisional entry found in HASH, set=%d, idx=%d\n",
                             lset, lindices[lset]);
     
                state->MOVED_PROV_ENTRY_EXISTED = TRUE;
                state->PROV_INDEX = lindices[lset];
                state->PROV_SET = lset;
                break;
            }
            else
            {
				WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
					"Existing provisional entry NOT found in HASH "
					" set=%d, idx=%d (HASH = {L2Domain=0x%x, VID=0x%x, MACAddress=0x%llx}, EntryType=%d, --S_GLORT=0x%x) --state->CSGLORT=0x%x\n",
					lset, lindices[lset],
					lentry.L2_DOMAIN, lentry.VID, lentry.MAC_ADDRESS,
					lentry.ENTRY_TYPE,
					lentry.S_GLORT, state->CSGLORT);
            }
        }
    }


}   /* end ExistingProvEntryCheck*/



/*****************************************************************************/
/** ComputeSVDrop
 * \ingroup intModel
 *
 * \desc           Computes security violation drop (SV_DROP).
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ComputeSVDrop(hlp_model *model)
{
    hlp_modelState *state;
    fm_uint32      *regPtr;

    state = &model->packetState;

	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_SECURITY_CFG(state->RX_PORT, 0));
	state->SECURED_PORT = FM_ARRAY_GET_BIT(regPtr, HLP_MA_TABLE_SECURITY_CFG, SECURE);

	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "SA_HIT=%d, SA_RESULT.ENTRY_TYPE=%d, SA_RESULT.S_GLORT=0x%x, CSGLORT=0x%x, RX_PORT=%0d, SECURED_PORT=%d\n",
    		     state->SA_HIT, state->SA_RESULT.ENTRY_TYPE, state->SA_RESULT.S_GLORT, state->CSGLORT, state->RX_PORT, state->SECURED_PORT);

	state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_RESERVED;
	if (state->SA_HIT) /*S key hit*/
	{
		if (state->SA_RESULT.S_GLORT != state->CSGLORT) /*MAC Moved (port move)*/
		{
	        if (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL)
				state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_PORT; /*secure port violation*/
	        else if ((state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_DYNAMIC) && state->SECURED_PORT)
				state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_PORT; /*secure port violation*/
	        else if (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_SECURE)
				state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_ADDR; /*secure address violation*/
	        else if ((state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_STATIC) && state->SECURED_PORT)
				state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_STATIC; /*static address violation*/
	        else if (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_SECURE_STATIC)
				state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_STATIC; /*static address violation*/
		}
		else
		{
	        if ((state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL) && state->SECURED_PORT)
				state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_PORT; /*secure port violation*/
		}
	}
	else /*no SA_HIT*/
	{
		if (state->SECURED_PORT) {
			state->SV_DROP = HLP_MODEL_SV_MOVE_DROP_PORT; /*secure port violation*/
		}
	}
}   /* end ComputeSVDrop */



/*****************************************************************************/
/** ComputeNoLearn
 * \ingroup intModel
 *
 * \desc           Computes NO_LEARN.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ComputeNoLearn(hlp_model *model)
{
    hlp_modelState *state;
    fm_uint32      *regPtr;
    fm_bool        DOSAttackActive;

    state = &model->packetState;

	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
				 "SA_HIT=%d, SA_RESULT.S_GLORT=0x%x, CSGLORT=0x%x, SECURED_PORT=%d, SA_RESULT.EntryType=%d, MOVED_PROV_ENTRY_EXISTED=%d, PROV_SET=%d, PROV_INDEX=%0d, SA_SET=%d, FREE_SET=%d, FREE_INDEX=%d\n",
				 state->SA_HIT, state->SA_RESULT.S_GLORT, state->CSGLORT, state->SECURED_PORT, state->SA_RESULT.ENTRY_TYPE, state->MOVED_PROV_ENTRY_EXISTED, state->PROV_SET, state->PROV_INDEX, state->SA_SET, state->FREE_SET, state->FREE_INDEX);
	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
				 "SA_HIT=%d, SA_RESULT.OldPort = %d, state->RX_PORT = %0d, DOSProtectionActiveGlobal[state->RX_PORT] = %0d \n",
				 state->SA_HIT, state->SA_RESULT.OLD_PORT, state->RX_PORT, DOSProtectionActiveGlobal[state->RX_PORT]);

	if (state->SA_HIT)
	{
		if (state->SA_RESULT.S_GLORT != state->CSGLORT) /*MAC Moved (port move)*/
		{
            if ((state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_DYNAMIC && state->SECURED_PORT) ||
                state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_SECURE) {
            	if((state->MOVED_PROV_ENTRY_EXISTED == FALSE) && ((state->SA_SET == 5 && state->FREE_SET == 5) || (state->FREE_INDEX == 0xffff))) {
                	state->NO_LEARN = 1;
                }
                if(state->MOVED_PROV_ENTRY_EXISTED && state->PROV_SET == 5 && state->SA_SET == 5 && state->FREE_SET == 5) {
                	state->NO_LEARN = 1;
                }
            } else if (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_STATIC) {
                state->NO_LEARN = 1;
            } else if (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_SECURE_STATIC) {
                state->NO_LEARN = 1;
            } else if (state->SA_RESULT.ENTRY_TYPE > 5) { /*should not appear*/
                state->NO_LEARN = 1;
            }
		} else {
            state->NO_LEARN = 1; /*no need to learn, already learned*/
        }
	}

	//update NO_LEARN when DOS is ON:
	if(GetDOSProtectionActiveGlobal(state->RX_PORT))
	{
		if((!state->SA_HIT) || (state->SA_HIT && (state->RX_PORT != state->SA_RESULT.OLD_PORT)))
		{
			state->NO_LEARN = 1; //apply DOS violation
		}
	}
}   /* end ComputeNoLearn */



/***************************************************************************/
/** UpdateMaTableEntryCount
 *
 * \desc         keeps track of MA_TABLE_ENTRY_COUNT.COUNT and update
 *               DOSProtectionActiveGlobal afterwards
 *
 * \param[in]    incrPort: MA_TABLE_ENTRY_COUNT[incrPort]++ because of the learn/update
 *                         event
 *
 * \param[in]    decrPort: MA_TABLE_ENTRY_COUNT[decrPort]-- because of the learn/update
 *                         event
 *
 * \param[in]   entryType: the ENTRY_TYPE written into the related MA_TABLE entry
 * **************************************************************************/
void UpdateMaTableEntryCount(hlp_model            *model,
		                     fm_byte               incrPort,
						     fm_byte               decrPort,
                             fm_byte               entryType)
{
    fm_uint32   *regPtr;
    fm_uint16   countVal;

    if(((incrPort & 0x1F) < HLP_MODEL_PORTS_COUNT) && 
        entryType != HLP_MODEL_MA_ENTRY_TYPE_NOT_USED) //if not_used, no increment
    {
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_ENTRY_COUNT(incrPort, 0));
        countVal = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_ENTRY_COUNT, COUNT);
        if(countVal < 0xFFFF)
        {
            countVal++;
            FM_ARRAY_SET_FIELD(regPtr, HLP_MA_TABLE_ENTRY_COUNT, COUNT, countVal);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: UpdateMaTableEntryCount: port %0d: DOS attack protection: MA_TABLE_ENTRY_COUNT=0x%x [+] \n", incrPort, countVal);
        }
      	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "set MA_TABLE_ENTRY_COUNT[%0d]=0x%x \n", incrPort, countVal);
        UpdateDOSAttackActive(model, incrPort, 2);
    }

    if((decrPort & 0x1F) < HLP_MODEL_PORTS_COUNT)
    {
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_ENTRY_COUNT(decrPort, 0));
        countVal = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_ENTRY_COUNT, COUNT);
        if(countVal > 0x0000)
        {
            countVal--;
            FM_ARRAY_SET_FIELD(regPtr, HLP_MA_TABLE_ENTRY_COUNT, COUNT, countVal);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: UpdateMaTableEntryCount: port %0d: DOS attack protection: MA_TABLE_ENTRY_COUNT=0x%x [-] \n", decrPort, countVal);
        }
       	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "set MA_TABLE_ENTRY_COUNT[%0d]=0x%x \n", decrPort, countVal);
        UpdateDOSAttackActive(model, decrPort, 3);
    }
}

/***************************************************************************/
/** UpdateDOSAttackActive
 *
 * \desc         keeps track of portLimit and update DOSProtectionActiveGlobal 
 *               afterwards, and fire an interrupt - record into ENTRY_COUNT_IP 
 *               if necessary.
 *
 * \param[in]    updatePortNumber: number of the updated port
 *
 * \param[in]    callerInfo: info about what called for this update:
 *               0 - HLP_MA_TABLE_SECURITY_CFG register write
 *               1 - HLP_ENTRY_COUNT_IP register write with "1" for that port
 *                   (HLP_ENTRY_COUNT_IP.PENDING is RW/1C/V)
 *               2 - an increment in MA_TABLE_ENTRY_COUNT
 *               3 - a decrement in MA_TABLE_ENTRY_COUNT
 *               4 - HLP_ENTRY_COUNT_IM or HLP_FWD_IM register write
 *
 * **************************************************************************/
void UpdateDOSAttackActive(hlp_model    *model,
                           fm_byte       updatePortNumber,
						   fm_byte       callerInfo)
{
    fm_uint32 *regPtr;
    fm_uint16 countVal, entryLimitVal;
    fm_uint32 pendingValOriginal, pendingVal, maskVal;
    fm_byte fwdIpEntryCount, fwdImEntryCount;
    fm_uint64 fwdIpTrigger, fwdImTrigger;
    fm_byte globalIntFwd = 0;

	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_ENTRY_COUNT(updatePortNumber, 0));
	countVal = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_ENTRY_COUNT, COUNT);

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_SECURITY_CFG(updatePortNumber, 0));
	entryLimitVal = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_SECURITY_CFG, ENTRY_LIMIT);

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTRY_COUNT_IP(0));
	pendingVal = FM_ARRAY_GET_FIELD(regPtr, HLP_ENTRY_COUNT_IP, PENDING);
	pendingValOriginal = pendingVal;

//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: UpdateDOSAttackActive: START: processing port %0d: [caller info: %0d] MA_TABLE_ENTRY_COUNT.COUNT=0x%x, HLP_MA_TABLE_SECURITY_CFG.ENTRY_LIMIT=0x%x. \n", updatePortNumber, callerInfo, countVal, entryLimitVal);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: UpdateDOSAttackActive: PRE : processing port %0d: [caller info: %0d] HLP_ENTRY_COUNT_IP.PENDING[%0d]=%0d (PENDING=0x%x). \n", updatePortNumber, callerInfo, updatePortNumber, ((pendingVal >> updatePortNumber) & 0x1), pendingVal);
	/* update pendingVal (if needed) */
	if((callerInfo == 0) || (callerInfo == 2) || (callerInfo == 3))
	{
		if ((countVal >= entryLimitVal) && (entryLimitVal != 0))
		{
			SetDOSProtectionActiveGlobal(updatePortNumber, 1);
			WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "port %0d: DOS attack protection is now 1 (countVal = %0d, entryLimitVal = %0d) [caller info: %0d] \n", updatePortNumber, countVal, entryLimitVal, callerInfo);
			pendingVal |= ((fm_uint32) 1) << updatePortNumber; /* set bit */
		}
		else
		{
			SetDOSProtectionActiveGlobal(updatePortNumber, 0);
			WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "port %0d: DOS attack protection is now 0 (countVal = %0d, entryLimitVal = %0d) [caller info: %0d] \n", updatePortNumber, countVal, entryLimitVal, callerInfo);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: port %0d: DOS attack protection is now 0 (countVal = %0d, entryLimitVal = %0d) [caller info: %0d] \n", updatePortNumber, countVal, entryLimitVal, callerInfo);
			/* however, do not clear the HLP_ENTRY_COUNT_IP.PENDING bit */
//			pendingVal &= ~(((fm_uint32) 1) << updatePortNumber); /* clear bit */
		}
	}
	if (callerInfo == 1)
	{
		pendingVal &= ~(((fm_uint32) 1) << updatePortNumber); /* clear bit (RW/1C/V access) */
//        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: after ANDING PENDING=0x%x). \n", pendingVal);
		if ((countVal >= entryLimitVal) && (entryLimitVal != 0))
		{
            pendingVal |= ((fm_uint32) 1) << updatePortNumber; /* set bit */
//            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: after  ORING PENDING=0x%x). \n", pendingVal);
        }
	}
	pendingVal &= 0xffffff;
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: UpdateDOSAttackActive: POST: processing port %0d: [caller info: %0d] HLP_ENTRY_COUNT_IP.PENDING[%0d]=%0d (orig=0x%x, PENDING=0x%x). \n", updatePortNumber, callerInfo, updatePortNumber, ((pendingVal >> updatePortNumber) & 0x1), pendingValOriginal, pendingVal);

    /* set and propagate ENTRY_COUNT_IP to FWD_IP and GLOBAL_INTERRUPT regs */
    if (pendingValOriginal != pendingVal)
    {
    	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTRY_COUNT_IP(0));
    	FM_ARRAY_SET_FIELD(regPtr, HLP_ENTRY_COUNT_IP, PENDING, pendingVal);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTRY_COUNT_IM(0));
	    maskVal = FM_ARRAY_GET_FIELD(regPtr, HLP_ENTRY_COUNT_IM, MASK);

	    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IP(0));
	    fwdIpEntryCount = ((pendingVal & ~maskVal) != 0);
        FM_ARRAY_SET_BIT(regPtr, HLP_FWD_IP, ENTRY_COUNT, fwdIpEntryCount);
        fwdIpTrigger = FM_GET_BIT(*regPtr, HLP_FWD_IP, TRIGGER);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IM(0));
        fwdImEntryCount = FM_GET_BIT(*regPtr, HLP_FWD_IM, ENTRY_COUNT);
        fwdImTrigger = FM_GET_BIT(*regPtr, HLP_FWD_IM, TRIGGER);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLOBAL_INTERRUPT(0));
        if ((fwdIpEntryCount & ~fwdImEntryCount) || (fwdIpTrigger & ~fwdImTrigger))
        {
        	globalIntFwd = 1;
		}
		if ((fwdIpEntryCount & ~fwdImEntryCount) == 0 && (fwdIpTrigger & ~fwdImTrigger) == 0)
		{
			globalIntFwd = 0;
		}
        FM_ARRAY_SET_BIT(regPtr, HLP_GLOBAL_INTERRUPT, FWD, globalIntFwd);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: UpdateDOSAttackActive: END: processing port %0d: [caller info: %0d] HLP_GLOBAL_INTERRUPT.FWD=%0d. \n", updatePortNumber, callerInfo, globalIntFwd);
    }
}

/*****************************************************************************/
/** hlpModelLookupAddress
 * \ingroup intModel
 *
 * \desc            Retrieves the MAC address table entry best matching the
 *                  specified (MAC address, VID, L2Domain). Also determines
 *                  free index for SMAC learning if MA Table entry did not
 *                  match
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       lookupKind denotes the lookup type/kind:
 *                  0 = DMAC lookup, 1 = SMAC lookup, 2 = MGMT lookup
 *
 * \param[in]       macAddr is the MAC address to search for.
 *
 * \param[in]       vid is the 12-bit Vlan ID.
 *
 * \param[in]       l2Domain is the 9-bit L2 Domain.
 *
 * \param[out]      index points to caller-allocated storage in which this
 *                  function will store the index of the best matching MAC
 *                  address table entry.
 *
 * \param[out]      set points to caller-allocated storage in which this
 *                  function will store the set of the best matching MAC
 *                  address table entry.
 *
 * \param[out]      hit points to caller-allocated storage in which this
 *                  function will store the lookup result.
 *
 * \param[out]      entry points to caller-allocated storage in which this
 *                  function will store the best matching MAC address table
 *                  entry.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelLookupAddress(hlp_model    *model,
		                        fm_byte      lookupKind,
                                fm_macaddr   macAddr,
                                fm_uint16    vid,
                                fm_uint16    l2Domain,
                                fm_uint16    *index,
                                fm_byte      *set,
                                fm_bool      *hit,
                                hlpMaTable   *entry)
{
    hlpMaTable          lentry;
    hlpMaTable          debugentry;//TOFIX
    fm_status           status = FM_OK;
    fm_uint16           lindices[HLP_MAC_ADDR_BANK_COUNT];
    fm_byte             lset = HLP_MAC_ADDR_BANK_COUNT - 1;
    fm_bool             lhit = FALSE;
    hlp_modelState      *state;
    fm_uint32           *regPtr;
    fm_uint16           camIndex;
    fm_byte             entryType = HLP_MODEL_MA_ENTRY_TYPE_NOT_USED;
    fm_uint16           lvid;
    fm_uint16           freeIndices[HLP_MAC_ADDR_BANK_COUNT];

    state = &model->packetState;

    if (lookupKind != 2)
    {
        //LEARN_MODE = 1 for IVL, LEARN_MODE = 0 for SVL
        lvid = (state->LEARN_MODE == 1) ? vid : 0;
		WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
				" ID:%x doing L2 lookup for: (lookupKind=%d), l2Domain=0x%x, lvid=0x%x, macAddr=0x%llx [vid=0x%x] ...\n",
				state->SB_DATA->idTag, lookupKind, l2Domain, lvid, macAddr, vid);
    }
    else
    {
    	lvid = vid;
		WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
				" ID:[NA] doing L2 lookup for: (lookupKind=%d), l2Domain=0x%x, lvid=0x%x, macAddr=0x%llx [vid=0x%x] ...\n",
				lookupKind, l2Domain, lvid, macAddr, vid);
    }

    camIndex = HLP_MODEL_MA_TABLE_CAM_ENTRIES; /*1024*/

    //CAM Lookup
    freeIndices[lset] = 0xffff; /*lset = 5*/
//    freeIndices[lset] = 0x3ff; /*lset = 5*/
    while (camIndex > 0)
    {
        camIndex--;
        GetMaTableEntry(model, lset, camIndex, &lentry);

        if ((lentry.MAC_ADDRESS == macAddr) &&
            (lentry.VID == lvid) &&
            (lentry.L2_DOMAIN == l2Domain) &&
            (lentry.ENTRY_TYPE != HLP_MODEL_MA_ENTRY_TYPE_NOT_USED))
        {
        	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MA TCAM: lookupKind=%d, set=%d, idx=%d, EntryType=%d hit!\n", lookupKind, lset, camIndex, lentry.ENTRY_TYPE);
            lhit = TRUE;
            *index = camIndex;
            entryType = lentry.ENTRY_TYPE;
            *set = lset;
            *entry = lentry;
            break;
        }
//        else if (lentry.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED)
//        {
//            freeIndices[lset] = camIndex;
//        }
    }

    for(camIndex=0; camIndex<HLP_MODEL_MA_TABLE_CAM_ENTRIES; camIndex++) {
        GetMaTableEntry(model, lset, camIndex, &lentry);//lset=5
        //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Debugging, camIndex=%d, lentry.ENTRY_TYPE=%d\n", camIndex, lentry.EntryType);
        if (lentry.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED) {
            freeIndices[lset] = camIndex;
            break;
        }
    }

   	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MA TCAM: lookupKind=%d, free set=%d, free idx=%d\n", lookupKind, lset, freeIndices[lset]);

    //HASH Lookup
    status = ComputeMaTableHash(model,
                                macAddr,
                                lvid,
                                l2Domain,
                                lindices);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
   	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MA HASH: lookupKind=%d, hash indices[4]=%d, indices[3]=%d, indices[2]=%d, indices[1]=%d, indices[0]=%d\n", lookupKind, lindices[4], lindices[3], lindices[2], lindices[1], lindices[0]);

    //TOFIX
	for(int i=0; i<5; i++) {
		GetMaTableEntry(model, i, lindices[i], &debugentry);
		WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
						   "i=%1d MACAddress=0x%llx VID=%-4d "
						   "L2Domain=0x%03x S_GLORT=0x%04x "
						   "DGlort=0x%04x TrigID=%-2d "
						   "EntryType=%1d, OldPort=%1d, NewPort=%1d\n",
						   i,
						   debugentry.MAC_ADDRESS,
						   debugentry.VID,
						   debugentry.L2_DOMAIN,
						   debugentry.S_GLORT,
						   debugentry.D_GLORT,
						   debugentry.TRIG_ID,
						   debugentry.ENTRY_TYPE,
						   debugentry.OLD_PORT,
						   debugentry.NEW_PORT);
	}

    lset--;
    while (TRUE)
    {
        GetMaTableEntry(model, lset, lindices[lset], &lentry);
        freeIndices[lset] = 0xffff;

        if ((lentry.MAC_ADDRESS == macAddr) &&
            (lentry.VID == lvid) &&
            (lentry.L2_DOMAIN == l2Domain) &&
            (lentry.ENTRY_TYPE != HLP_MODEL_MA_ENTRY_TYPE_NOT_USED) &&
			(lentry.ENTRY_TYPE > entryType)) /*if multiple hit entries have same EntryType, the priority is: TCAM, then hash sets: 4, 3, 2, 1, 0*/
        {
       		WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MA HASH: lookupKind=%d, set=%d, idx=%d, EntryType=%d hit!\n", lookupKind, lset, lindices[lset], lentry.ENTRY_TYPE);
            lhit = TRUE;
            *index = lindices[lset];
            *set = lset;
            *entry = lentry;
            entryType = lentry.ENTRY_TYPE;
        }
        else if (lentry.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED)
        {
            freeIndices[lset] = lindices[lset];
           	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MA HASH: lookupKind=%d, free set=%d, free idx=%d\n", lookupKind, lset, freeIndices[lset]);
        }
        if (lset == 0)
        {
            break;
        }
        lset--;
    }

    //free index calculation for SMAC lookup:
//    if (lookupKind == 1)
    if (lookupKind == 1 || lookupKind == 2)
    {
        state->FREE_SET = 0;
        state->FREE_INDEX = 0xffff;

        //if (!lhit)
        //{
        for (lset = 0; lset < HLP_MAC_ADDR_BANK_COUNT; lset++)
        {
            if (freeIndices[lset] != 0xffff)
            {
                state->FREE_SET = lset;
                state->FREE_INDEX = freeIndices[lset];
                break;
            }
        }
        //}
    }

    if (hit != NULL)
    {
        *hit = lhit;
    }

    return status;
}   /* end hlpModelLookupAddress */



/*****************************************************************************/
/** hlpModelL2MGMTLookup
 * \ingroup intModel
 *
 * \desc           Handles the MGMT lookups (handles the
 *                 MA_TABLE_INDIRECT_WRITE WR register access).
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
fm_status hlpModelL2MGMTLookup(hlp_model *model, fm_int regIndex)
{
    fm_status      status = FM_OK;
    hlp_modelState *state;
    fm_uint32      *regPtr;
    fm_byte        MGMTPort;
	fm_byte        MGMTEntryType;
	fm_byte        MGMTTrigID;
	fm_uint16      MGMTSGlort;
	fm_uint16      MGMTL2Domain;
	fm_uint16      MGMTVID;
	fm_macaddr     MGMTMACAddress;
	fm_uint16      LookupIndex = 0;
    fm_byte        LookupSet = 0;
    fm_bool        LookupHit = 0;
    hlpMaTable     LookupEntry;
	fm_uint32      AllMATableIndex = 0;
	hlpMaTable     entry; /* MA_TABLE entry */
	fm_bool        DOSProtectionActive;

	state = &model->packetState;

	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_INDIRECT_WRITE(regIndex, 0));
	MGMTPort = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, PORT);
	MGMTEntryType = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, ENTRY_TYPE);
	MGMTTrigID = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, TRIG_ID);
	MGMTSGlort = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, S_GLORT);
	MGMTL2Domain = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, L2_DOMAIN);
	MGMTVID = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, VID);
	MGMTMACAddress = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_TABLE_INDIRECT_WRITE, MAC_ADDRESS);

	if (MGMTPort >= HLP_MODEL_PORTS_COUNT) {
		WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
					"MGMT lookup MA_TABLE_INDIRECT_WRITE[%d]: ignored due to invalid port: Port=0x%x, EntryType=0x%x, TrigID=0x%x, SGlort=0x%x, L2Domain=0x%x, VID=0x%x, MACAddress=0x%llx.\n",
					regIndex, MGMTPort, MGMTEntryType, MGMTTrigID, MGMTSGlort, MGMTL2Domain, MGMTVID, MGMTMACAddress);
		return status; //do nothing else if invalid port
	}

	status = hlpModelLookupAddress(model,
                                   2, /*MGMT lookup*/
						           MGMTMACAddress,
						           MGMTVID,
						           MGMTL2Domain,
                                   &LookupIndex,
                                   &LookupSet,
                                   &LookupHit,
                                   &LookupEntry);

	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
				 "MGMT lookup MA_TABLE_INDIRECT_WRITE[%d]: Port=0x%x, EntryType=0x%x, TrigID=0x%x, SGlort=0x%x, L2Domain=0x%x, VID=0x%x, MACAddress=0x%llx: [hit=%d: set=%d, index=0x%x; FREE_SET=%d, FREE_INDEX=0x%x].\n",
				 regIndex, MGMTPort, MGMTEntryType, MGMTTrigID, MGMTSGlort, MGMTL2Domain, MGMTVID, MGMTMACAddress, LookupHit, LookupSet, LookupIndex, state->FREE_SET, state->FREE_INDEX);

	if (LookupHit) /* the MGMT lookup has hit MA_TABLE, do MA_TABLE update or erase */
	{
//	    entry.OLD_PORT = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : LookupEntry.OLD_PORT;
	    entry.OLD_PORT = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTPort;
	    entry.NEW_PORT = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTPort;
	    entry.ENTRY_TYPE = MGMTEntryType;
	    entry.TRIG_ID = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTTrigID;
	    entry.S_GLORT = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTSGlort;
//	    entry.D_GLORT = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : LookupEntry.D_GLORT;
	    entry.D_GLORT = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTSGlort; //DGLORT = SGLORT so the packet will be forwarded to it
	    entry.L2_DOMAIN = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTL2Domain;
	    entry.VID = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTVID;
	    entry.MAC_ADDRESS = MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED ? 0 : MGMTMACAddress;

	    //Write indirect "entry" to MAC table (this function is implemented in learning.c)
	    WriteMaTableEntry(model,
	    		          LookupSet,
						  LookupIndex,
	                      &entry);
	    AllMATableIndex = LookupIndex | (LookupSet << 13);
        UpdateDirtyTableAndCounter(model, AllMATableIndex, 1, "MGMT lookup update"); //function implemented in hlp_model_learning.c
        UpdateMaTableEntryCount(model, MGMTPort, LookupEntry.NEW_PORT, entry.ENTRY_TYPE);
	}
	else /* the MGMT lookup has missed MA_TABLE, do MGMT learning if possible */
	{
		if ((MGMTEntryType == HLP_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL ||
       	     MGMTEntryType == HLP_MA_LOOKUP_ENTRY_TYPE_DYNAMIC     ||
			 MGMTEntryType == HLP_MA_LOOKUP_ENTRY_TYPE_SECURE      ||
			 MGMTEntryType == HLP_MA_LOOKUP_ENTRY_TYPE_STATIC      ||
			 MGMTEntryType == HLP_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC) && 
			(state->FREE_INDEX != 0xffff)) /* not NOT_USED (or unknown) and MA_TABLE not full */
		{
	        entry.OLD_PORT = MGMTPort;
	        entry.NEW_PORT = MGMTPort;
	        entry.ENTRY_TYPE = MGMTEntryType;
	        entry.TRIG_ID = MGMTTrigID;
	        entry.S_GLORT = MGMTSGlort;
	        entry.D_GLORT = MGMTSGlort;
	        entry.L2_DOMAIN = MGMTL2Domain;
	        entry.VID = MGMTVID;
	        entry.MAC_ADDRESS = MGMTMACAddress;
	        AllMATableIndex = (state->FREE_INDEX) | ((state->FREE_SET) << 13);

	        if (!GetDOSProtectionActiveGlobal(MGMTPort))
	        {
				WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "Indirect learning MAC 0x%llx, vid 0x%x, l2_domain 0x%x at index: 0x%x (ma_set = %0d, ma_tbl_idx = %0d).\n",
							 entry.MAC_ADDRESS, entry.VID, entry.L2_DOMAIN, AllMATableIndex, state->FREE_SET, state->FREE_INDEX);
	        	//Write indirect "entry" to MAC table (this function is implemented in learning.c)
		        WriteMaTableEntry(model,
		        		          state->FREE_SET,
								  state->FREE_INDEX,
		                          &entry);
                UpdateDirtyTableAndCounter(model, AllMATableIndex, 1, "MGMT lookup learning"); //function implemented in hlp_model_learning.c
                UpdateMaTableEntryCount(model, MGMTPort, 0x1F, entry.ENTRY_TYPE);
//        		//for aging checker: let it know that a learning occurred (due to MGMT lookup)
//        		state->L2_MGMT_LEARN = 1; //the valid flag
//        		state->L2_MGMT_OVERWRITE = LookupHit;
//        		state->L2_MGMT_ENTRY_TYPE = MGMTEntryType;
//        		state->L2_MGMT_MA_TABLE_INDEX = AllMATableIndex;
	        }
	        else //DOS (at MGMT lookup)
	        {
	        	state->NO_LEARN = 1; //update NO_LEARN (DOS protection is ON) [needed?]

        		WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "DOS attack protection: not indirect learning MAC 0x%llx, vid 0x%x, l2_domain 0x%x at index 0x%x, port %0d.\n",
                             entry.MAC_ADDRESS, entry.VID, entry.L2_DOMAIN, AllMATableIndex, entry.NEW_PORT);
	        	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "hlpModelL2MGMTLookup: DOS attack protection: state->NO_LEARN=%0d\n", state->NO_LEARN);
	        }
	    }
		else
		{
			if (MGMTEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED)
			{
//		        DOSProtectionActive = UpdateEntryCount(model, 1, MGMTPort, 0, 0);
			}
		}
        
	}

    return status;
} /* end hlpModelL2MGMTLookup */



/*****************************************************************************/
/** hlpModelL2MATableDirectWrite
 * \ingroup intModel
 *
 * \desc           Handles the MGMT lookups (handles the MA_TABLE direct WR
 *                 register access).
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
fm_status hlpModelL2MATableDirectWrite(hlp_model *model, fm_uint16 regIndex)
{
    fm_status      status = FM_OK;
//    hlp_modelState *state;
    fm_uint32      *regPtr;
    fm_byte        DirectNewPort;
    fm_byte        DirectOldPort;
	fm_byte        DirectEntryType;
	fm_byte        DirectTrigId;
	fm_uint16      DirectSGlort;
	fm_uint16      DirectDGlort;
	fm_uint16      DirectL2_DOMAIN;
	fm_uint16      DirectVID;
	fm_macaddr     DirectMACAddress;
	fm_uint16      MATableIndex = 0;
    fm_byte        MATableSet = 0;
	fm_uint32      AllMATableIndex = 0;
	fm_bool        DOSProtectionActive;
    hlpMaTable     entry; /* a MA_TABLE entry */
    fm_uint64      val = 0;

//	state = &model->packetState;

	AllMATableIndex = regIndex;
	MATableSet = (AllMATableIndex >> 13) & 0x7;
	MATableIndex = AllMATableIndex & 0x1fff;

	if (MATableSet == (HLP_MAC_ADDR_BANK_COUNT - 1) && MATableIndex > (HLP_MODEL_MA_TABLE_CAM_ENTRIES - 1))
	{
		return status; //do nothing for unused TCAM addresses
	}

	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE(MATableSet, MATableIndex, 0));
	DirectNewPort = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, NEW_PORT);
	DirectOldPort = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, OLD_PORT);
	DirectEntryType = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, ENTRY_TYPE);
	DirectTrigId = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, TRIG_ID);
	DirectSGlort = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, S_GLORT);
	DirectDGlort = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, D_GLORT);
	DirectL2_DOMAIN = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, L2_DOMAIN);
    DirectVID = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE, VID);
    DirectMACAddress = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_TABLE, MAC_ADDRESS);

	if (DirectNewPort >= HLP_MODEL_PORTS_COUNT) {
		WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
					"MA_TABLE direct write at index [%d]: has invalid new port: NewPort=0x%x, EntryType=0x%x [set=%d, index=0x%x].\n",
					AllMATableIndex, DirectNewPort, DirectEntryType, MATableSet, MATableIndex);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: hlpModelL2MATableDirectWrite: MA_TABLE direct write at index [%d]: has invalid new port: NewPort=0x%x, EntryType=0x%x [set=%d, index=0x%x].\n", AllMATableIndex, DirectNewPort, DirectEntryType, MATableSet, MATableIndex);
//	    //Mark the just written MAC table entry as unused (WriteMaTableEntry function is implemented in learning.c)
//	    entry.OLD_PORT = 0;
//	    entry.NEW_PORT = 0;
//	    entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_NOT_USED;
//	    entry.TRIG_ID = 0;
//	    entry.S_GLORT = 0;
//	    entry.D_GLORT = 0;
//	    entry.L2_DOMAIN = 0;
//	    entry.VID = 0;
//	    entry.MAC_ADDRESS = 0;
//	    WriteMaTableEntry(model,
//	    				  MATableSet,
//						  MATableIndex,
//	                      &entry);
		return status; //do nothing else if invalid new port
	}

	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
				 "MA_TABLE direct write at index [%d]: NewPort=0x%x, EntryType=0x%x [set=%d, index=0x%x].\n",
				 AllMATableIndex, DirectNewPort, DirectEntryType, MATableSet, MATableIndex);

	if (CheckIfEntryTypeValid(DirectEntryType))
	{
        UpdateMaTableEntryCount(model, DirectNewPort, DirectOldPort, DirectEntryType);
        ClearDirtyBitIfSet(model, AllMATableIndex, "MGMT direct WR");
		WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MA_TABLE direct write learning MAC 0x%llx, vid 0x%x, l2_domain 0x%x at index: 0x%x (ma_set = %0d, ma_tbl_idx = %0d).\n",
					 DirectMACAddress, DirectVID, DirectL2_DOMAIN, AllMATableIndex, MATableSet, MATableIndex);
	}
	else if (DirectEntryType == HLP_MODEL_MA_ENTRY_TYPE_NOT_USED)
	{
        UpdateMaTableEntryCount(model, DirectNewPort, DirectOldPort, DirectEntryType);
        ClearDirtyBitIfSet(model, AllMATableIndex, "MGMT direct WR with ENTRY_TYPE NOT_USED");
	}

    return status;
} /* end hlpModelL2MATableDirectWrite */



/*****************************************************************************/
/** hlpModelL2MGMTIndirectMATableRead
 * \ingroup intModel
 *
 * \desc           Reports dirty entries in MA_TABLE (handles the
 *                 MA_TABLE_INDIRECT_READ RD register access).
 *
 * \param[in]      model points to the switch model state.
 *                 regIndex is the register index (0..31).
  *
 *****************************************************************************/
fm_status hlpModelL2MGMTIndirectMATableRead(hlp_model *model, fm_int regIndex)
////fm_status hlpModelL2MGMTIndirectMATableRead(hlp_model *model, fm_int regIndex, fm_int regSlice)
{
    fm_status  status = FM_OK;
    fm_uint32  *regPtr;
    fm_uint16  ii, jj;
    fm_uint16  MADirtyTableIndex = 0;
    fm_uint32  AllMATableIndex;
    fm_uint32  MADirtyTableSize;
    fm_bool    MADirtyFound = 0;
    fm_uint64  Dirty64; /* MA_DIRTY_TABLE entry */
    fm_byte    MATableSet = 0;
    fm_uint16  MATableIndex = 0;
    hlpMaTable MATableEntry; /* MA_TABLE entry */

////    if(regSlice == 0)
////    {

        /* try to find a dirty entry in MA_DIRTY_TABLE */
        MADirtyTableSize = (HLP_MAC_ADDR_BANK_SIZE*(HLP_MAC_ADDR_BANK_COUNT - 1) + HLP_MODEL_MA_TABLE_CAM_ENTRIES)/64;
        MADirtyTableIndex = AllMATableIndexGlobal + 1;
        for(ii = 0; ii < MADirtyTableSize; ii++)
        {
        	MADirtyTableIndex = (MADirtyTableIndex < MADirtyTableSize) ? MADirtyTableIndex : 0;
            regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_DIRTY_TABLE(MADirtyTableIndex, 0));
            Dirty64 = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_DIRTY_TABLE, DIRTY);
            if (Dirty64 != 0)
            {
                for(jj = 0; jj < 64; jj++)
                {
                	if (((Dirty64 >> jj) & 0x1) == 1)
                	{
                		MADirtyFound = 1;
                		AllMATableIndex = MADirtyTableIndex*64 + jj;
                		/* save the table index in a global variable to be used next time */
                		AllMATableIndexGlobal = AllMATableIndex;
                		break; /* break for jj */
                	}
                }
                break; /* break for ii */
            }
            MADirtyTableIndex++;
        }

        /* process the MA_TABLE_INDIRECT_READ */
        if (MADirtyFound)
        {
        	MATableSet = (AllMATableIndex >> 13) & 0x7;
	    	MATableIndex = AllMATableIndex & 0x1fff;
			WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
						 "MA_TABE dirty entry report MA_TABLE_INDIRECT_READ[%d]: set=%d, index=0x%x.\n",
						 regIndex, MATableSet, MATableIndex);

        	/* read the dirty entry from MA_TABLE, at the found dirty index (AllMATableIndex) */
        	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE(MATableSet, MATableIndex, 0));
        	MATableEntry.OLD_PORT    = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, OLD_PORT);
        	MATableEntry.NEW_PORT    = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, NEW_PORT);
        	MATableEntry.ENTRY_TYPE  = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, ENTRY_TYPE);
        	MATableEntry.TRIG_ID     = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, TRIG_ID);
        	MATableEntry.S_GLORT     = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, S_GLORT);
        	MATableEntry.D_GLORT     = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, D_GLORT);
        	MATableEntry.L2_DOMAIN   = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, L2_DOMAIN);
        	MATableEntry.VID         = FM_ARRAY_GET_FIELD(regPtr,   HLP_MA_TABLE, VID);
        	MATableEntry.MAC_ADDRESS = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_TABLE, MAC_ADDRESS);

//		if(SetDirtyBit(model, AllMATableIndex, 0)) //clear the dirty bit just found in MA_DIRTY_TABLE (this function is implemented in learning.c)
//        {
//		    UpdateDirtyCount(model, 0); //this function is implemented in learning.c
//        }
            UpdateDirtyTableAndCounter(model, AllMATableIndex, 0, "MGMT indirect RD"); //function implemented in hlp_model_learning.c
        }
        else
        {
			WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP,
						 "MA_TABE dirty entry report MA_TABLE_INDIRECT_READ[%d]: set=[NA], index=[NA] (not found).\n",
						 regIndex);

        	/* all zero (dummy MATableEntry) */
        	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE(0, 0, 0));
        	MATableEntry.OLD_PORT    = 0;
        	MATableEntry.NEW_PORT    = 0;
        	MATableEntry.ENTRY_TYPE  = HLP_MODEL_MA_ENTRY_TYPE_NOT_USED;
        	MATableEntry.TRIG_ID     = 0;
        	MATableEntry.S_GLORT     = 0;
        	MATableEntry.D_GLORT     = 0;
        	MATableEntry.L2_DOMAIN   = 0;
        	MATableEntry.VID         = 0;
        	MATableEntry.MAC_ADDRESS = 0;
        }

////    }

    /* update the MA_TABLE_INDIRECT_READ register */
	regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_INDIRECT_READ(regIndex, 0));
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, NEW_PORT,    MATableEntry.NEW_PORT);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, ENTRY_TYPE,  MATableEntry.ENTRY_TYPE);
	FM_ARRAY_SET_BIT(regPtr,     HLP_MA_TABLE_INDIRECT_READ, DIRTY,       MADirtyFound);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, TRIG_ID,     MATableEntry.TRIG_ID);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, S_GLORT,     MATableEntry.S_GLORT);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, BANK,        MATableSet);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, INDEX,       MATableIndex);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, L2_DOMAIN,   MATableEntry.L2_DOMAIN);
	FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE_INDIRECT_READ, VID,         MATableEntry.VID);
	FM_ARRAY_SET_FIELD64(regPtr, HLP_MA_TABLE_INDIRECT_READ, MAC_ADDRESS, MATableEntry.MAC_ADDRESS);

    return status;
} /* end hlpModelL2MGMTIndirectMATableRead */

//
/*****************************************************************************/
/** hlpModelL2MATableDirectRead
 * \ingroup intModel
 *
 * \desc            clear DirtyTable and decrement MA_TABLE_DIRTY_COUNT if
 *                  the entry in the MA_TABLE being read has its bit been set
 *                  in DirtyTable
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       regIndex is the index of the MA_TABLE being read

 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status hlpModelL2MATableDirectRead(hlp_model *model,
                                      fm_uint16  regIndex)
{
    fm_status    status = FM_OK;

    ClearDirtyBitIfSet(model, regIndex, "MGMT direct RD");
    
    return status;
}

/*****************************************************************************/
/** hlpModelL2Lookup
 * \ingroup intModel
 *
 * \desc            Performs the ingress and egress VLAN and forwarding ID
 *                  lookups and the destination and source MAC address lookups.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelL2Lookup(hlp_model *model)
{
    hlpEgressVidTable           egressVidTable;
    hlpIngressVidTable          ingressVidTable;
    hlp_modelState              *state = &model->packetState;
    fm_status                   err;
    fm_int                      mtuIndex;
    fm_uint32                   mtuLength;
    fm_uint32                   *regPtr;
//    fm_bool                     learningEnable;
    fm_bool                     mplsTtl = 0;

    if(testPlusArgs("HLP_L2_LOOKUP_WM_PRINT_VERBOSE") >= 0)
        l2LookupDisplayVerbose = testPlusArgs("HLP_L2_LOOKUP_WM_PRINT_VERBOSE");
    WM_DISPLAY(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "l2LookupDisplayVerbose=%0d\n", l2LookupDisplayVerbose)

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "model=%p\n", (void *)model);
    
    err = FM_OK;

//    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_PORT_CFG_1(state->RX_PORT, 0));
//    learningEnable = FM_GET_BIT(*regPtr, HLP_FWD_PORT_CFG_1, LEARNING_ENABLE);

    mplsTtl = (((state->TTL_CTRL & 0x7) == 0x2) || ((state->TTL_CTRL & 0x7) >= 0x4));
    state->DROP_TTL &= (state->MARK_ROUTED || mplsTtl);
    if(state->DROP_TTL)
    {
        if(state->MARK_ROUTED)
        {
        	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "drop_ttl set for arp_routed case.\n");
        }
        if(mplsTtl)
        {
        	WM_DISPLAY3(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "drop_ttl set for MPLS label case.\n");
        }
    }
    state->TRAP_ICMP &= state->MARK_ROUTED;

    /***************************************************
     * Perform ingress & egress VLAN lookups.
     **************************************************/
    GetIngressVidTableEntry(model, &ingressVidTable);
    GetEgressVidTableEntry(model, &egressVidTable);

    state->L2_IVLAN1_REFLECT = ingressVidTable.REFLECT;
    state->TRAP_IGMP &= ingressVidTable.TRAP_IGMP;
////    state->L2_IVLAN1_CNT_INDEX = ingressVidTable.COUNTER_INDEX;
//    state->L2_IVLAN1_CNT_INDEX = state->VLAN_COUNTER; //aaverche 9/23/2016: removed as requested by bug 32024

    state->L2_IVLAN1_MEMBERSHIP = 
        FM_GET_UNNAMED_FIELD(ingressVidTable.MEMBERSHIP,
                             state->RX_PORT,
                             1);

    state->L2_EVLAN1_MEMBERSHIP = egressVidTable.MEMBERSHIP;
    state->L2_EVLAN1_TRIG = egressVidTable.TRIG_ID;
    
    //Add 40B for IPV6 frame to include header size for MTU check
    if (state->IS_IPV6 == 1)
    {
    	mtuLength = state->L3_LENGTH + 40;
    	mtuLength = (mtuLength > 0xffff) ? 0xffff : mtuLength;
    }
    else
    {
    	mtuLength = state->L3_LENGTH;
    }
    if (state->MARK_ROUTED == TRUE && 
        mtuLength > GetMtuSize(model, state->MTU_INDEX))
    {
        state->MTU_VIOLATION = TRUE;
        WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "MTU Violation occurred\n");
    }

    /***************************************************
     * Perform destination & source MA Table lookups.
     **************************************************/
    
    WM_DISPLAY2(l2LookupDisplayVerbose, FM_LOG_CAT_MODEL_L2_LOOKUP, "smac=0x%llx, ivid=0x%x, l2_domain=0x%x\n", state->L2_SMAC, state->L2_IVID1, state->L2_IDOMAIN);
    err = hlpModelLookupAddress(model,
                                1, /*SRC lookup*/
                                state->L2_SMAC,
                                state->L2_IVID1,
                                state->L2_IDOMAIN,
                                &state->SA_INDEX,
                                &state->SA_SET,
                                &state->SA_HIT,
                                &state->SA_RESULT);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    //if(learningEnable == 0)
    //    state->SA_HIT = 0;

    /* Update MA_USED Table */
//    UpdateMaUsedTable(model);    
//    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    ProcessLookup(model);

    /***************************************************
     * Perform ingress & egress forwarding ID lookups.
     **************************************************/
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_INGRESS_MST_TABLE(state->L2_IVID1, 0));
    state->L2_IFID1_STATE = FM_ARRAY_GET_UNNAMED_FIELD64(regPtr,
                                                         (state->RX_PORT) * 2,
                                                         2);
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_EGRESS_MST_TABLE(state->L2_EVID1, 0));
    state->L2_EFID1_STATE = FM_ARRAY_GET_FIELD(regPtr,
    		                                   HLP_EGRESS_MST_TABLE,
                                               FORWARDING);

    ComputeCSGlort(model);

    ExistingProvEntryCheck(model,
                           state->L2_SMAC,
                           state->L2_IVID1,
                           state->L2_IDOMAIN);

    ComputeSVDrop(model); /*it needs state->CSGLORT, so ComputeCSGlort must be called before*/

    ComputeNoLearn(model);

////    state->VLAN_COUNTER = state->L2_IVLAN1_CNT_INDEX;

ABORT:
	if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpL2Lookup);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end hlpModelL2Lookup */
