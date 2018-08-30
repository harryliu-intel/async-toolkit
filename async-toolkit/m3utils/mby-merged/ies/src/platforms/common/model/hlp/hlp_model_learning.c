/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_learning.c
 * Creation Date:   June 14, 2012
 * Description:     LEARNING stage of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2016 Intel Corporation. All Rights Reserved.
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
fm_int learningDisplayVerbose = 1;

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

static void UpdateInterruptChain(hlp_model *model)
{
    fm_uint32   *ip;
    fm_uint32   *im;
    fm_uint32   *global_intr;
    fm_uint     imVal, ipVal;

    im = FM_MODEL_GET_REG_PTR(model, HLP_FWD_MA_TABLE_DIRTY_IM(0));
    imVal = FM_GET_BIT(*im, HLP_FWD_MA_TABLE_DIRTY_IM, MASK);

    ip = FM_MODEL_GET_REG_PTR(model, HLP_FWD_MA_TABLE_DIRTY_IP(0));
    ipVal = FM_GET_BIT(*ip, HLP_FWD_MA_TABLE_DIRTY_IP, PENDING);

    global_intr = FM_MODEL_GET_REG_PTR(model, HLP_GLOBAL_INTERRUPT(0));
    FM_ARRAY_SET_BIT(global_intr, HLP_GLOBAL_INTERRUPT, L2_LOOKUP,
                     ((ipVal & ~imVal) != 0));

    return;
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelFwdMiscWriteCSR
 * \ingroup intModel
 *
 * \desc            Processes CSR writes pertinent to the FWD_MISC
 *                  white model stages.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       newValue is the 32-bit value to write.
 *
 * \param[in]       oldValue is the value before the new value was written.
 *
 * \param[in]       init is a boolean indicating whether this write operation
 *                  is trying to initialize the register related white model
 *                  cached state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelFwdMiscWriteCSR(hlp_model *model,
                                  fm_uint32 addr,
                                  fm_uint32 newValue,
                                  fm_uint32 oldValue,
                                  fm_bool init)
{
    fm_status err = FM_OK;

    FM_NOT_USED(model);
    FM_NOT_USED(addr);
    FM_NOT_USED(newValue);
    FM_NOT_USED(oldValue);
    FM_NOT_USED(init);

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM,
                            "model=%p addr=0x%x newValue=0x%x\n",
                            (void *) model, addr, newValue);

    if (!init &&
        (addr == HLP_FWD_MA_TABLE_DIRTY_IM(0) ||
         addr == HLP_FWD_MA_TABLE_DIRTY_IP(0)))
    {
        UpdateInterruptChain(model);
    }

ABORT:
    HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, err);
}

/*****************************************************************************/
/** CheckIfEntryTypeValid
 * \ingroup intModel
 *
 * \desc            Checks if the entryType is valid
 *                  
 * \param[in]       entryType: ENTRY_TYPE been checked
 *
 * \return          TRUE: Valid
 *                  FALSE: Either undefied or not_used
 *
 *****************************************************************************/
fm_bool CheckIfEntryTypeValid(fm_byte entryType)
{
    if (entryType == HLP_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL ||
    	entryType == HLP_MA_LOOKUP_ENTRY_TYPE_DYNAMIC     ||
		entryType == HLP_MA_LOOKUP_ENTRY_TYPE_SECURE      ||
		entryType == HLP_MA_LOOKUP_ENTRY_TYPE_STATIC      ||
		entryType == HLP_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC)
    {
        return TRUE;
    }
    else
    {   
        return FALSE;
    }
}

/*****************************************************************************/
/** ClearDirtyBitIfSet
 * \ingroup intModel
 *
 * \desc            Used by MGMT_RD/WR, if the DirtyBit for the MA_TABLE been
 *                  RD/WR is 1, clear it and decrement MA_TABLE_DIRTY_COUNT;
 *                  meaning S/W already knows about the event.
 *                  
 * \param[in]       model: points to the switch model state.
 *
 * \param[in]       AllMATableIndex: the index of the MA_TABLE, {set,index}
 *
 * \param[in]       message: debug string
 *
 *****************************************************************************/
void ClearDirtyBitIfSet(hlp_model    *model,
                        fm_uint16    AllMATableIndex,
						fm_char      *message)
{
    fm_uint32    *regPtr;
    fm_uint64    val;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_DIRTY_TABLE(AllMATableIndex/64, 0));
    val = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_DIRTY_TABLE, DIRTY);    
//    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "[%s] AllMATableIndex=%16x, bit=%llx`", message, AllMATableIndex, FM_GET_UNNAMED_FIELD64(val, (AllMATableIndex%64), 1));
    if(FM_GET_UNNAMED_FIELD64(val, (AllMATableIndex%64), 1))
    {
        UpdateDirtyTableAndCounter(model, AllMATableIndex, 0, message);
    }
}

/*****************************************************************************/
/** UpdateDirtyTableAndCounter
 * \ingroup intModel
 *
 * \desc            Set/Reset a bit in MA_DIRTY_TABLE associated with a MA_TABLE
 *                  entry and update the MA_TABLE_DIRTY_COUNT. For set case, no 
 *                  update needed if the bit in MA_DIRTY_TABLE is already one.
 *                  
 * \param[in]       model: points to the switch model state.
 *
 * \param[in]       MACTableIndex: the index of the MA_TABLE been updated
 *
 * \param[in]       bitWritten: 
 *                    0 - reset bit in MA_DIRTY_TABLE and decease counter
 *                    1 - set bit in MA_DIRTY_TABLE and increase counter
 *****************************************************************************/
void UpdateDirtyTableAndCounter(hlp_model *model,
                                fm_uint32 MACTableIndex,
                                fm_bool   bitWritten,
								fm_char   *message)
{
    fm_uint32 *regPtr;
    fm_uint64 val;
    fm_bool   updateNeeded = FALSE;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_DIRTY_TABLE(MACTableIndex/64, 0));
    val = FM_ARRAY_GET_FIELD64(regPtr, HLP_MA_DIRTY_TABLE, DIRTY);    
    if(bitWritten)
    {
        if(!FM_GET_UNNAMED_FIELD64(val, (MACTableIndex%64), 1))
        {
            updateNeeded = TRUE;
           	WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "[%s] UpdateDirtyTableAndCounter, set bit for MA_TABLE[%0d]\n", message, MACTableIndex);
        }
    }
    else
    {
            updateNeeded = TRUE;
           	WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "[%s] UpdateDirtyTableAndCounter, clear bit for MA_TABLE[%0d]\n", message, MACTableIndex);
    }

    if(updateNeeded)
    {
            FM_SET_UNNAMED_FIELD64(val, (MACTableIndex%64), 1, bitWritten);
            FM_ARRAY_SET_FIELD64(regPtr, HLP_MA_DIRTY_TABLE, DIRTY, val);        
            UpdateDirtyCount(model, bitWritten);
           	WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "[%s] UpdateDirtyTableAndCounter, MA_DIRTY_TABLE[%0d]=0x%llx.\n", message, MACTableIndex/64, val);
    }

}

/*****************************************************************************/
/** UpdateDirtyCount
 * \ingroup intModel
 *
 * \desc            Increse of Decrease the value of MA_TABLE_DIRTY_COUNT, and
 *                  the update should not cause the counter over/under-flow 
 *                  
 * \param[in]       model: points to the switch model state.
 *
 * \param[in]       IncrOrDecr:
 *                   1 - Increase
 *                   0 - Decrease
 *
 *****************************************************************************/
void UpdateDirtyCount(hlp_model *model, fm_bool IncrOrDecr)
{
    fm_uint32 *regPtr;
    fm_uint32 val;
    fm_byte pendingVal;
//	fm_byte maskVal;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_DIRTY_COUNT(0));
    val = FM_ARRAY_GET_FIELD(regPtr, HLP_MA_TABLE_DIRTY_COUNT, COUNT);
    if (IncrOrDecr && val < 0xffff)
    {
        val++;
       	WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "MA_TABLE_DIRTY_COUNT incremented = 0x%x\n", val);
    }
    if ((IncrOrDecr == 0) && (val > 0))
    {
    	val--;
   		WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "MA_TABLE_DIRTY_COUNT decremented = 0x%x\n", val);
    }
    FM_ARRAY_SET_FIELD(regPtr, HLP_MA_TABLE_DIRTY_COUNT, COUNT, val);

    /* update FWD_MA_TABLE_DIRTY_IP */

//    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_MA_TABLE_DIRTY_IM(0));
//    maskVal = FM_GET_BIT(*regPtr, HLP_FWD_MA_TABLE_DIRTY_IM, MASK);

    pendingVal = (val > 0); //val is COUNT value HLP_MA_TABLE_DIRTY_COUNT
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_MA_TABLE_DIRTY_IP(0));
    FM_ARRAY_SET_BIT(regPtr, HLP_FWD_MA_TABLE_DIRTY_IP, PENDING, pendingVal);

    /* propagate FWD_MA_TABLE_DIRTY_IP to GLOBAL_INTERRUPT reg */

    UpdateInterruptChain(model);
}

/*****************************************************************************/
/** WriteMaTableEntry
 * \ingroup intModel
 *
 * \desc            Wrirtes an entry in MA_TABLE.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       set is the MA_TABLE set.
 *
 * \param[in]       idx is the index in the set.
 *
 * \param[in]       entry points to the MA_TABLE entry word to be written.
 *
 *****************************************************************************/
void WriteMaTableEntry(hlp_model   *model,
                       fm_uint32    set,
                       fm_uint32    idx,
                       hlpMaTable   *entry)
{
    fm_uint32 *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE(set, idx, 0));

	WM_DISPLAY2(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
				 "write MA_TABLE entry: set=%d idx=0x%x, mac=0x%llx, vid=0x%x, l2d=0x%x, type=%d, trigID=0x%x.\n",
				 set, idx, entry->MAC_ADDRESS, entry->VID, entry->L2_DOMAIN, entry->ENTRY_TYPE, entry->TRIG_ID);

    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, OLD_PORT,    entry->OLD_PORT);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, NEW_PORT,    entry->NEW_PORT);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, ENTRY_TYPE,  entry->ENTRY_TYPE);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, TRIG_ID,     entry->TRIG_ID);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, S_GLORT,     entry->S_GLORT);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, D_GLORT,     entry->D_GLORT);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, L2_DOMAIN,   entry->L2_DOMAIN);
    FM_ARRAY_SET_FIELD(regPtr,   HLP_MA_TABLE, VID,         entry->VID);
    FM_ARRAY_SET_FIELD64(regPtr, HLP_MA_TABLE, MAC_ADDRESS, entry->MAC_ADDRESS);
}

/*****************************************************************************/
/** hlpModelLearning
 * \ingroup intModel
 *
 * \desc            Checks if the source MAC address should and can be learned
 *                  and if so, updates the MAC address table and TCN FIFO
 *                  registers.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelLearning(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status      status = FM_OK;
    fm_bool        learningEnabled;
    fm_bool 	   DOSProtectionActive = 0;
    hlpMaTable     entry;
    fm_uint32      *regPtr;
    fm_uint32      maTblIndex = 0; //TOFIX: tmp for warning uint //TODO
    fm_uint32      ma_table_full_count;
    fm_byte        incrPort = 0;
    fm_byte        decrPort = 0;
    fm_bool        ma_tcam_learning_disabled;
    fm_byte        entryOvewrite = 0;

    if(testPlusArgs("HLP_LEARNING_WM_PRINT_VERBOSE") >= 0)
        learningDisplayVerbose = testPlusArgs("HLP_LEARNING_WM_PRINT_VERBOSE");
    WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "learningDisplayVerbose=%0d\n", learningDisplayVerbose)

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "\n");
                    
    entry.OLD_PORT = 0;
    entry.NEW_PORT = 0;
    entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_NOT_USED;
    entry.TRIG_ID = 0;
    entry.S_GLORT = 0;
    entry.D_GLORT = 0;
    entry.L2_DOMAIN = 0;
    entry.VID = 0;
    entry.MAC_ADDRESS = 0;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_PORT_CFG_1(state->RX_PORT, 0));
    learningEnabled = FM_GET_BIT(*regPtr, HLP_FWD_PORT_CFG_1, LEARNING_ENABLE); //cfg

    WM_DISPLAY2(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "Learning for port %0d is %s (configuration in HLP_FWD_PORT_CFG_1).\n", state->RX_PORT, (learningEnabled ?  "enabled" : "disabled"));

    /* Learning disabled for BAD_FCS or Policer Drop or
     * by Triggers, or if Learning is disabled */ 
    if ( ((state->RX_FLAGS & HLP_MODEL_PACKET_BAD_FCS) ||
          (!state->LEARNING_ENABLED) ||
          (!learningEnabled))                          &&
          state->TRIGGERS.learningAction != HLP_MODEL_TRIG_ACTION_LEARNING_FORCE_LEARN) //TODO: I think learningEnabled is included in LEARNING_ENABLED so no need for ORing it
//        (state->POLICER_DROP) ||
    {
    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
        		     " NOT learning MAC=0x%llx vid=0x%x l2_domain=0x%x; possible causes: BAD FCS = %s, state->LEARNING_ENABLED = %s, FWD_PORT_CFG_1[%d].LEARNING_ENABLE = %s.\n\n",
					 state->L2_SMAC, state->L2_IVID1, state->L2_IDOMAIN,
					 (state->RX_FLAGS & HLP_MODEL_PACKET_BAD_FCS) ? "TRUE" : "FALSE",
			         (state->LEARNING_ENABLED) ? "TRUE" : "FALSE",
			         state->RX_PORT, learningEnabled ? "TRUE" : "FALSE");

        return FM_OK; /* abort learning */
    }

    /* Select entry:
     *   hit-index if it's valid
     *   else free-index if it's valid
     *   else, discard
     */   
    if (state->SA_HIT && state->MAC_MOVED) /*MAC Moved (port move) - begin*/
    {
    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "MAC Moved (port move).\n");
        if ( (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL) ||
             ( (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_DYNAMIC) && !state->SECURED_PORT ) )
        {
            maTblIndex = (state->SA_INDEX) | (state->SA_SET << 13);
            entry.ENTRY_TYPE = state->SA_RESULT.ENTRY_TYPE;        
            WM_DISPLAY2(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "MAC Moved: Overwrite the original MA_TABLE entry, type=%d.\n", state->SA_RESULT.ENTRY_TYPE);
            entry.TRIG_ID = state->SA_RESULT.TRIG_ID; //workaroud-Bug31605
            incrPort = state->RX_PORT;
            decrPort = state->SA_RESULT.NEW_PORT;
            entryOvewrite = 1;
        }
        else if ( ( (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_DYNAMIC) && state->SECURED_PORT ) ||
                  (state->SA_RESULT.ENTRY_TYPE == HLP_MODEL_MA_ENTRY_TYPE_SECURE)                              )
        {
            if (state->MOVED_PROV_ENTRY_EXISTED)
            {
                maTblIndex = (state->PROV_INDEX) | (state->PROV_SET << 13);
                entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL;        
                WM_DISPLAY2(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "MAC Moved: Overwrite the existing provisional entry.\n");
                entry.TRIG_ID = state->SA_RESULT.TRIG_ID; //workaroud-Bug31605
                incrPort = state->RX_PORT;
                decrPort = state->SA_RESULT.NEW_PORT;
                entryOvewrite = 1;
            }
            else if ( ((state->FREE_SET < 5) && (state->FREE_INDEX != 0xffff)) ||
                      ((state->SA_SET != 5) && (state->FREE_SET == 5) && (state->FREE_INDEX != 0xffff)) )
            {
                maTblIndex = (state->FREE_INDEX) | (state->FREE_SET << 13);
                entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL;        
                WM_DISPLAY2(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "MAC Moved: Learn a new provisional entry.\n");
                entry.TRIG_ID = 0; //workaroud-Bug31605
                incrPort = state->RX_PORT;
                decrPort = 0x1F;
            }
        }
    }  /*MAC Moved (port move) - end*/
    else if (!state->SA_HIT && (state->FREE_INDEX != 0xffff))
    {
    	WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "Using FREE_INDEX.\n");
        maTblIndex = (state->FREE_INDEX) | (state->FREE_SET << 13);
        if(state->SECURED_PORT) {
            entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL;
        } else {
            entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_DYNAMIC;
        }
        entry.TRIG_ID = 0;
        incrPort = state->RX_PORT;
        decrPort = 0x1F;
    }
    else
    {
    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
        		"NOT learning MAC=0x%llx vid=0x%x l2_domain=0x%x due to MA_TABLE full; discarding learning.\n",
        		state->L2_SMAC, state->L2_IVID1, state->L2_IDOMAIN);
        /* No room for learning, discard */
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TABLE_FULL(0));
        ma_table_full_count = FM_GET_FIELD(*regPtr, HLP_MA_TABLE_FULL, COUNT);
        if(ma_table_full_count != ((FM_LITERAL_U64(1) << 32) - 1)) //counter not saturated
        {
        	WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "pre-write  MA_TABLE_FULL = 0x%x.\n", ma_table_full_count);
            ma_table_full_count += 1;
            FM_SET_FIELD(*regPtr, HLP_MA_TABLE_FULL, COUNT, ma_table_full_count);
            WM_DISPLAY3(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING, "post-write MA_TABLE_FULL = 0x%x.\n", ma_table_full_count);
        }
        return FM_OK; /* abort learning */
    }
    
    entry.OLD_PORT = state->RX_PORT;//OLD_PORT in the MA_TABLE is not accurate, but as long as entryCount acts reasonably, it should be fine
    entry.NEW_PORT = state->RX_PORT;
    //entry.ENTRY_TYPE = HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL; //Provisional or from SA_RESULT for update case? //TO DO: setting entry type to 1 to be in sync with RTL. will need to be revised.
    //entry.TRIG_ID = 0;
    entry.S_GLORT = state->CSGLORT;
    entry.D_GLORT = state->CSGLORT;
    entry.L2_DOMAIN = state->L2_IDOMAIN;
    entry.VID = (state->LEARN_MODE == 1) ? state->L2_IVID1 : 0;
    entry.MAC_ADDRESS = state->L2_SMAC;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MA_TCAM_LEARNING(0));
    ma_tcam_learning_disabled = FM_ARRAY_GET_BIT(regPtr, HLP_MA_TCAM_LEARNING, DISABLED);

    if ((((maTblIndex >> 13) & 0x7) == HLP_MAC_ADDR_BANK_COUNT - 1) && ma_tcam_learning_disabled) {
    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
    			     "NOT learning MAC=0x%llx vid=0x%x l2_domain=0x%x at index: 0x%x (since MA_TCAM_LEARNING.DISABLED=1).\n",
    			     entry.MAC_ADDRESS, entry.VID, entry.L2_DOMAIN, maTblIndex);
    }
//    else if ((state->RX_PORT) >= HLP_MODEL_PORTS_COUNT)
//    {
//    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
//    			     "NOT learning MAC=0x%llx vid=0x%x l2_domain=0x%x at index: 0x%x (since RX_PORT is not valid).\n",
//    			     entry.MAC_ADDRESS, entry.VID, entry.L2_DOMAIN, maTblIndex);
//    }
    else if (CheckIfEntryTypeValid(entry.ENTRY_TYPE) && (!GetDOSProtectionActiveGlobal(state->RX_PORT)))
//    if (CheckIfEntryTypeValid(entry.ENTRY_TYPE) && (!GetDOSProtectionActiveGlobal(state->RX_PORT)))
    {
    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
				     "Learning MAC=0x%llx vid=0x%x l2_domain=0x%x at index: 0x%x (ma_set = %0d, ma_tbl_idx = %0d).\n",
					 entry.MAC_ADDRESS, entry.VID, entry.L2_DOMAIN, maTblIndex, ((maTblIndex >> 13) & 0x7), (maTblIndex & 0x1fff));

		//Write "entry" to MAC table
		WriteMaTableEntry(model,
						  (maTblIndex >> 13) & 0x7,
						  maTblIndex & 0x1fff,
						  &entry);
        UpdateDirtyTableAndCounter(model, maTblIndex, 1, "learning");
        UpdateMaTableEntryCount(model, incrPort, decrPort, entry.ENTRY_TYPE);

		//for aging checker: let it know that a learning occurred (due to frame lookup)
		state->L2_S_KEY_LEARN = 1; //the valid flag
		state->L2_S_MA_TABLE_OVERWRITE = entryOvewrite;
		state->L2_S_MA_TABLE_INDEX = maTblIndex;
    }
    else
    {
    	WM_DISPLAY(learningDisplayVerbose, FM_LOG_CAT_MODEL_LEARNING,
    			     "NOT learning MAC=0x%llx vid=0x%x l2_domain=0x%x at index: 0x%x (since DOS protection is ON).\n",
    			     entry.MAC_ADDRESS, entry.VID, entry.L2_DOMAIN, maTblIndex);
    }

ABORT:
	if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpLearning);

    return status;
} /*hlpModelLearning*/
