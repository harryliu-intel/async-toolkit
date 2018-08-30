/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_next_hop.c
 * Creation Date:   July 10, 2012
 * Description:     NEXT_HOP stage of HLP white model
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
fm_int nextHopDisplayVerbose = 1;

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
/** GetARPUsedEntry
 * \ingroup intModel
 *
 * \desc            Reads ARP_USED register data from register cache and
 *                  populates corresponding register struct.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   hlp_modelArpUsed is the struct for ARP_USED that is populated
 *                  with data from register cache.
 *
 *****************************************************************************/
static void GetARPUsedEntry(hlp_model            *model,
                            hlpArpUsed     *arp_used)
{
    fm_uint32   *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ARP_USED(0,0));

    arp_used->USED= FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_USED, USED);
} /* end GetARPUsedEntry*/

/*****************************************************************************/
/** GetARPTableEntry
 * \ingroup intModel
 *
 * \desc            Reads ARP_TABLE register data from register cache and
 *                  populates corresponding register struct.
 *
 * \param[in]       model points to the switch data structure.
 *  
 *  \param[in]      arpIndex is the index into ARP_TABLE.
 *
 * \param[in,out]   arp_table is the struct for ARPTable that is populated
 *                  with data from register cache.
 *
 *****************************************************************************/
static void GetARPTableEntry(hlp_model            *model,
                             fm_uint16            arpIndex,
                             hlp_modelArpTable    *arpTable)
{
    fm_uint32   *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ARP_TABLE(arpIndex,0));
    
    arpTable->DMAC = FM_ARRAY_GET_FIELD64(regPtr, HLP_ARP_TABLE, DST_MAC);
    arpTable->EntryType = FM_ARRAY_GET_BIT(regPtr, HLP_ARP_TABLE, ENTRY_TYPE);
    arpTable->IPv6Entry = FM_ARRAY_GET_BIT(regPtr, HLP_ARP_TABLE, IPV6_ENTRY);
    arpTable->EVID = FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_TABLE, EVID);
    arpTable->MTU_Index = FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_TABLE, MTU_INDEX);
    arpTable->ModIdx= FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_TABLE, MOD_IDX);
    arpTable->L3Domain= FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_TABLE, L3_DOMAIN);
    arpTable->L2Domain= FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_TABLE, L2_DOMAIN);
    arpTable->UpdateL3Domain= FM_ARRAY_GET_BIT(regPtr, HLP_ARP_TABLE, UPDATE_L3_DOMAIN);
    arpTable->UpdateL2Domain= FM_ARRAY_GET_BIT(regPtr, HLP_ARP_TABLE, UPDATE_L2_DOMAIN);
    arpTable->DGLORT = 0;
    arpTable->markRouted = 0;

    WM_DISPLAY3(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "arp index = 0x%x\n", arpIndex);

    if (arpTable->EntryType == HLP_MODEL_ARP_TYPE_GLORT)
    {
        arpTable->DGLORT = FM_ARRAY_GET_FIELD(regPtr, HLP_ARP_ENTRY_GLORT, DGLORT);
        arpTable->markRouted = FM_ARRAY_GET_BIT(regPtr, HLP_ARP_ENTRY_GLORT, markRouted);
        WM_DISPLAY3(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "DGLORT=0x%x, mtuIndex=%d, markrouted=%d\n", arpTable->DGLORT, arpTable->MTU_Index, arpTable->markRouted);

    }
} /* end GetARPTableEntry*/

/*****************************************************************************/
/** SetARPUsedEntry
 * \ingroup intModel
 *
 * \desc            Write into the ARP_USED register according to the 
 *                  ARP_TABLE_INDEX.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       index is the ARP_TABLE_INDEX. 
 *
 *****************************************************************************/
static void SetARPUsedEntry(hlp_model   *model,
                            fm_uint32   index)
{   
    fm_uint32       *arpUsedEntry;
    fm_uint64       usedValue;

    arpUsedEntry = FM_MODEL_GET_REG_PTR(model, HLP_ARP_USED((index  >> 6), 0));
    usedValue = FM_ARRAY_GET_FIELD64(arpUsedEntry, HLP_ARP_USED, USED);
    FM_ARRAY_SET_FIELD64(arpUsedEntry, HLP_ARP_USED, USED,
                         usedValue |( FM_LITERAL_U64(1) << (index & 0x3f)));
    WM_DISPLAY3(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "arp index = 0x%x, usedIndex=0x%x\n",
    		     index, (index >> 6));
} /* end SetARPUsedEntry */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelNextHop
 * \ingroup intModel
 *
 * \desc            Performs the next hop lookup.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelNextHop(hlp_model *model)
{
    hlp_modelArpTable      arpTable;
    fm_uint32              value[4];
    fm_uint32              FullArpIndex;
    fm_uint32              ArpIndex;
    fm_uint16              rawHashMask;
    fm_byte                groupSize;
    fm_bool                groupType;
    fm_bool                isGlortRouted;
    fm_bool                isIPUcst = FALSE;
    //fm_bool                l2_mcast_bcast;
    fm_status              status = FM_OK;
    fm_bool                doArpLkup = FALSE;
    hlp_modelState         *state = &model->packetState;
    
    if(testPlusArgs("HLP_NEXT_HOP_WM_PRINT_VERBOSE") >= 0)
        nextHopDisplayVerbose = testPlusArgs("HLP_NEXT_HOP_WM_PRINT_VERBOSE");
    WM_DISPLAY(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "nextHopDisplayVerbose=%0d\n", nextHopDisplayVerbose)

    groupType = FM_GET_BIT(state->FFU_ROUTE,
                           HLP_MODEL_FFU_ROUTE,
                           GROUP_TYPE);

    groupSize = FM_GET_FIELD(state->FFU_ROUTE,
                             HLP_MODEL_FFU_ROUTE,
                             GROUP_SIZE);

    FullArpIndex = FM_GET_FIELD(state->FFU_ROUTE, HLP_MODEL_FFU_ROUTE, ARP_INDEX);//route
    ArpIndex = FullArpIndex & 0x3FFF; /* only 14 bits are used for actual index */
    
    if (groupType == 0)
    {
        state->ARP_TABLE_INDEX = ArpIndex +
                state->ARP_HASH[groupSize];
        state->ARP_TABLE_INDEX &= ( HLP_ARP_TABLE_ENTRIES - 1 );
    }
    else
    {
        //rawHashMask = (1 << groupSize) - 1;
        state->ARP_TABLE_INDEX = ArpIndex +
                ((state->RAW_HASH << groupSize) >> 12);
        state->ARP_TABLE_INDEX &= ( HLP_ARP_TABLE_ENTRIES - 1 );
    }
    
    /*set ingress L2/L3 domain, based on PKT_META, DSI ingress CPM to HLP pkt*/
    state->L2_IDOMAIN = (state->PKT_META[20] >> 4) & 0x0f;
    state->L2_IDOMAIN |= ((state->PKT_META[21] & 0x1f) << 4);
    state->L3_IDOMAIN = (state->PKT_META[21] >> 5) & 0x07;
    state->L3_IDOMAIN |= ((state->PKT_META[22] & 0x07) << 3);
////    state->L2_IDOMAIN   = state->L2_DOMAIN;
////    state->L3_IDOMAIN   = state->L3_DOMAIN;

    state->L2_EVID1     = state->L2_IVID1; 
    state->L2_EDOMAIN   = state->L2_IDOMAIN;
    state->L3_EDOMAIN   = state->L3_IDOMAIN;
    state->MTU_INDEX    = 0;
    state->FLOOD_SET    = 0;

    isGlortRouted = !FM_GET_BIT(state->FFU_ROUTE,
                               HLP_MODEL_FFU_ROUTE,
                               ARP_ROUTE);


    if (isGlortRouted)
    {
    	WM_DISPLAY2(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "Glort Routed\n");
        state->FLOOD_SET = FM_GET_BIT(state->FFU_ROUTE,
                                      HLP_MODEL_FFU_ROUTE,
                                      FLOODSET);

        if(FM_GET_FIELD(state->FFU_ROUTE, HLP_MODEL_FFU_ROUTE, DGLORT) != 0)
        {
            state->IDGLORT = FM_GET_FIELD(state->FFU_ROUTE,
                                              HLP_MODEL_FFU_ROUTE,
                                              DGLORT);
        }
    }    
    else if (!(state->FFU_FLAGS.no_route) )
    {
    	WM_DISPLAY2(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "ARP Routed\n");
        state->MARK_ROUTED = TRUE;

        doArpLkup = TRUE;

        state->IDGLORT = 0;
        
        WM_DISPLAY3(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "ARP Group Size = %d\n", groupSize);
        WM_DISPLAY3(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "ARP Group type = %d\n", groupType);
    }

    if (doArpLkup)
    {
        GetARPTableEntry(model, state->ARP_TABLE_INDEX, &arpTable);
        if (arpTable.EntryType == HLP_MODEL_ARP_TYPE_MAC)
        {
        	WM_DISPLAY2(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "ARP Entry Type MAC\n");
            
            state->L2_EVID1 = arpTable.EVID;

            /* type==MAC */
            if (arpTable.IPv6Entry == 1)
            {
            	WM_DISPLAY2(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "ARP Entry Type IPV6\n");
            
                /* type==MAC, IPv6 (markRouted == 1 & IPv6Entry == 1) */
                /* Convert the modified EUI-64 interface ID to a 48-bit
                 * destination MAC address. */
                state->L2_DMAC = state->DMAC_FROM_IPV6;
            }
            else
            {    
                state->L2_DMAC = arpTable.DMAC;
            }    
        }
        else
        {
        	WM_DISPLAY2(nextHopDisplayVerbose, FM_LOG_CAT_MODEL_NEXT_HOP, "ARP Entry Type Glort\n");
            
            state->IDGLORT = arpTable.DGLORT;
            state->MARK_ROUTED = arpTable.markRouted;
            state->L2_EVID1 = arpTable.EVID;
        }

        if (arpTable.UpdateL3Domain)
            state->L3_EDOMAIN = arpTable.L3Domain;
        
        if (arpTable.UpdateL2Domain)
            state->L2_EDOMAIN = arpTable.L2Domain;
        
        state->MTU_INDEX = arpTable.MTU_Index;
        state->MOD_IDX   = (arpTable.ModIdx>>2) & 0xFFFF;
        state->DECAP     = (arpTable.ModIdx>>1) & 0x1;
        state->ENCAP     =  arpTable.ModIdx     & 0x1;
    }

    if ( doArpLkup && model->allowStateChange )
    {   
       SetARPUsedEntry(model, state->ARP_TABLE_INDEX);
    }

ABORT:
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpNextHop);

    return status;

}   /* end hlpModelNextHop */


