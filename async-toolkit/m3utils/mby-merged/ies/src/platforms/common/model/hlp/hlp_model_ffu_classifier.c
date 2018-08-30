/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_ffu.c
 * Creation Date:   August 14, 2015
 * Description:     FFU Classifier stage of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2015 Intel Corporation. All Rights Reserved.
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
#define FFU_GROUP_ADDR_MASK       0xFFFFF


#define FFU_GROUP_IN_RANGE_MULT_3(addr, csr)                                \
    ( ( ( (fm_int) ( (addr)  & FFU_GROUP_ADDR_MASK ) ) >=                   \
        ( csr(0, 0, 0, 0) & FFU_GROUP_ADDR_MASK ) ) &&                      \
      ( ( (addr) & FFU_GROUP_ADDR_MASK ) <=                                 \
        ( csr(csr ## _ENTRIES_2 - 1,                                        \
              csr ## _ENTRIES_1 - 1,                                        \
              csr ## _ENTRIES_0 - 1,                                        \
              ( ( csr(0, 0, 1, 0) - csr(0, 0, 0, 0) ) >> 2) - 1) & FFU_GROUP_ADDR_MASK ) ) )      

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_int     fgrpDisplayVerbose = 1;    /* 0=always prints; 1=ip & ulv prints; 
2=ulvprints */

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
/** GetTcamCfg 
 * \ingroup intModel
 *
 * \desc            Reads FFU_TCAM_CFG register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is number of the group used as index for accessing
 *                  FFU_TCAM_CFG
 *
 * \param[in]       slice is number of the slice used as index for accessing
 *                  FFU_TCAM_CFG
 *
 * \param[in,out]   tcamCfg is the struct for FFU_TCAM_CFG that is populated
 *                  with data from register cache
 *
 *****************************************************************************/
static void GetTcamCfg(hlp_model            *model,
                       fm_byte              group,
                       fm_byte              slice,
                       hlpFfuTcamCfg        *tcamCfg)
{
    hlp_modelState      *state;
    fm_uint32           *regPtr;
    fm_uint32           addr;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, 
                                  HLP_FFU_TCAM_CFG(group, 
                                                   slice, 
                                                   state->FFU_SCENARIO, 
                                                   0));
    
    tcamCfg->CHUNK_MASK = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_TCAM_CFG, CHUNK_MASK);
    tcamCfg->START_COMPARE = FM_ARRAY_GET_BIT(regPtr, HLP_FFU_TCAM_CFG, START_COMPARE);
    tcamCfg->SELECT_TOP = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_TCAM_CFG, SELECT_TOP);
    tcamCfg->SELECT0 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_TCAM_CFG, SELECT0);
    tcamCfg->SELECT1 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_TCAM_CFG, SELECT1);
    tcamCfg->SELECT2 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_TCAM_CFG, SELECT2);
    tcamCfg->SELECT3 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_TCAM_CFG, SELECT3);
    tcamCfg->START_SET = FM_ARRAY_GET_BIT(regPtr, HLP_FFU_TCAM_CFG, START_SET);

    /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "GetTcamCfg grp=%d, slice=%d, sel0=0x%x,
     * sel1=0x%x, sel2=0x%x, sel3=0x%x, selTop=0x%x, chunk_mask=0x%x,
     * start_compare=%d\n", group, slice, tcamCfg->SELECT0, tcamCfg->SELECT1,
     * tcamCfg->SELECT2, tcamCfg->SELECT3, tcamCfg->SELECT_TOP,
     * tcamCfg->CHUNK_MASK, tcamCfg->START_COMPARE);*/
}


/*****************************************************************************/
/** SelectKeyMask
 * \ingroup intModel
 *
 * \desc            Computes tcam key and mask based on slice configuration.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       tcamCfg contains slice configuration used to compute key and
 *                  mask.
 * 
 * \param[out]      info points to caller-allocated storage in which key and
 *                  mask required for TCAM lookup is set in this function.
 *
 *****************************************************************************/
static void SelectKeyMask(hlp_model             *model,
                          hlpFfuTcamCfg         tcamCfg,
                          hlp_modelLookupInfo   *info)
{
    hlp_modelState      *state;
    hlp_modelFfuKeys    *keys;
    fm_byte             mux[5];
    fm_int              i;
    fm_byte             temp;
    fm_uint32           vlanVpri;
    fm_int              lowBit;
    fm_int              highBit;

    state = &model->packetState;
    keys = &state->FFU_KEYS;

    info->key = 0;
    info->keyInvert = FM_LITERAL_U64(0xFFFFFFFFF);
    
    mux[0] = tcamCfg.SELECT0;
    mux[1] = tcamCfg.SELECT1;
    mux[2] = tcamCfg.SELECT2;
    mux[3] = tcamCfg.SELECT3;
    mux[4] = tcamCfg.SELECT_TOP+HLP_MODEL_FFU_N_KEY16;
    

    /** Loop through the 5 bytes of key ({7:0}, {15:8}, {23,16}, {31:24},
     * {39:32}) */
    for (i = 0; i < 5; i++)
    {
        /* Sets bottom and top limits */
        lowBit  = 8 * i;
        highBit = lowBit + 7;
        temp = 0;
        
        /* key8 */
        if ((mux[i] >= HLP_MODEL_FFU_KEY8_BASE) && 
                (mux[i] < (HLP_MODEL_FFU_KEY8_BASE + HLP_MODEL_FFU_N_KEY8))) 
        {
            temp = keys->key8[mux[i] - HLP_MODEL_FFU_KEY8_BASE];
        } 
        
        /* key16 and key32 are invalid for SelectTop*/
        if(i < 4)
        {
            /* key16 */
            if (mux[i] < (HLP_MODEL_FFU_KEY16_BASE + HLP_MODEL_FFU_N_KEY16)) 
            {
                temp = FM_GET_UNNAMED_FIELD(
                            keys->key16[mux[i] - HLP_MODEL_FFU_KEY16_BASE], 
                            lowBit % 16, 
                            8);
            }

            /* key32 */
            if ((mux[i] >= HLP_MODEL_FFU_KEY32_BASE) &&
                     (mux[i] < (HLP_MODEL_FFU_KEY32_BASE + HLP_MODEL_FFU_N_KEY32))) 
            {
                temp = 
                    FM_GET_UNNAMED_FIELD(
                        keys->key32[mux[i] - HLP_MODEL_FFU_KEY32_BASE], 
                        lowBit, 
                        8);
            } 
        }
       FM_SET_UNNAMED_FIELD64(info->key, lowBit, 8, temp);
    }

    info->keyInvert = ~info->key;

    WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "SelectKeyMask sel0=0x%x, sel1=0x%x, sel2=0x%x, sel3=0x%x, selTop=0x%x, key=%llx \n", mux[0], mux[1], mux[2], mux[3], mux[4], info->key)

} /* end SelectKeyMask */


/*****************************************************************************/
/** LookupTcam
 * \ingroup intModel
 *
 * \desc            Get raw hits for ternary compare, updates rawHits vector
 *                  with every TCAM line that is hit.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is the number of the ffu group in which lookup has to
 *                  be done.
 *
 * \param[in]       slice is the number of the slice in which lookup has to
 *                  be done.
 *
 * \param[in]       chunkMask indicates if any of 64-bit chunks of TCAM rules
 *                  have valid entry
 *
 * \param[in, out]  info points to the structure which has key and mask values
 *                  required for TCAM lookup and rawHits vector which is updated
 *                  with results pf TCAM lookup on each TCAM line
 *
 *****************************************************************************/
static void LookupTcam(hlp_model           *model,
                       fm_byte             group,
                       fm_byte             slice,
                       fm_uint16           chunkMask,
                       hlp_modelLookupInfo *info)
{
    fm_uint64       camKeyInvert;
    fm_uint64       camKey;
    fm_uint64       mask;
    fm_uint16       tcamIndex ;
    fm_byte         tcamChunk;

    tcamIndex = 0;
    FM_CLEAR(info->rawHits);

    while (tcamIndex < HLP_FFU_TCAM_ENTRIES_0)
    {
        tcamChunk = tcamIndex >> 6;
        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "LookupTcam tcamIndex=%d,
         * tcamChunk=%d, chunkMask==0x%x\n", tcamIndex, tcamChunk, chunkMask);*/
        
        if ((chunkMask >> tcamChunk) & 1)
        {    
            camKeyInvert = model->FFU_TCAM[group][slice][tcamIndex].KeyInvert;
            camKey = model->FFU_TCAM[group][slice][tcamIndex].Key;
            mask = camKey ^ camKeyInvert;
            
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "LookupTcam camKey=0x%llx,
             * camKeyInvert=0x%llx mask=0x%llx\n", camKey, camKeyInvert,
             * mask);*/
            
            if(model->FFU_TCAM[group][slice][tcamIndex].Valid)
            {
                if (((camKey & camKeyInvert) == 0) && 
                   ((info->key & mask) == (camKey & mask)))
                {    
                    info->rawHits[tcamIndex] = 1;
                    WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "LookupTcam grp=%0d, Slice=%0d : TCAM[%0d] hit \n", group, slice, tcamIndex)
                }
            }
            tcamIndex++;
        }
        else
        {
            tcamIndex+= 64;
        }    
    }
  
} /* end LookupTcam */

/*****************************************************************************/
/** CascadeLookupTcam
 * \ingroup intModel
 *
 * \desc            Cascades lookup of TCAM, sets whether TCAM lookup is hit
 *                  updates the hits vector retrieved from LookupTcam and
 *                  sets the index of high priority hit line in the TCAM slice.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is the number of the ffu group in which lookup has to
 *                  be done.
 *
 * \param[in,out]   info points to the structure which has per slice hitIndex
 *                  and hitIndexValid 
 *
 *****************************************************************************/
static void CascadeLookupTcam(hlp_model           *model,
                              fm_byte             group,
                              hlp_modelFfuHitInfo *tcamHitInfo)
{
    fm_int                  i, j;
    fm_byte                 slice;
    fm_byte                 slice_info;
    fm_byte                 cascadeWidth[HLP_FFU_TCAM_CFG_ENTRIES_1];
    hlpFfuTcamCfg           tcamCfg;
    hlpFfuTcamCfg           tcamCfg1;
    hlp_modelLookupInfo     info;
    fm_bool                 hits[HLP_FFU_TCAM_ENTRIES_0];
    fm_bool                 exclusionSet;
    fm_bool                 setHit;
    fm_bool                 fsc;

    FM_CLEAR(cascadeWidth);
    FM_CLEAR(hits);
    FM_CLEAR(exclusionSet);
    FM_CLEAR(setHit);
    
    fsc = 0;

    /* Compute cascade width */
    for (slice = 0 ; slice < HLP_FFU_TCAM_CFG_ENTRIES_1 ; slice++)
    {
        GetTcamCfg(model, group, slice, &tcamCfg);
        
        if (tcamCfg.START_COMPARE)
        {
            cascadeWidth[slice]++;
            if (slice < HLP_FFU_TCAM_CFG_ENTRIES_1-1)
            {
                for (i = slice+1 ; i < HLP_FFU_TCAM_CFG_ENTRIES_1 ; i++)
                {
                    GetTcamCfg(model, group, i, &tcamCfg1);

                    if (!tcamCfg1.START_COMPARE)
                        cascadeWidth[slice]++;
                    else
                        break;
                }
            }
        }
    }
    
    for (slice = 0 ; slice < HLP_FFU_TCAM_CFG_ENTRIES_1 ; slice++)
    {

        if (cascadeWidth[slice])
        {
            FM_CLEAR(hits);
            GetTcamCfg(model, group, slice, &tcamCfg);

            WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "group=%d, start_slice=%d, cascade_width=%d, start_set=%d\n",group, slice, cascadeWidth[slice], tcamCfg.START_SET)
            
            /* Start Exclusion Set */
            if (tcamCfg.START_SET)
            {    
                //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Starting Set. Slice=%d\n", slice);
                exclusionSet = TRUE;
                setHit = FALSE;
            } 

            for (i = slice; i < slice + cascadeWidth[slice]; i++)
            {    
                GetTcamCfg(model, group, i, &tcamCfg1);
                
                /* End set if slice is not used (chunk_mask = 0) */
                //if (tcamCfg1.CHUNK_MASK == 0)
                //{    
                //    exclusionSet = FALSE;
                //    setHit = FALSE;
                //} 

                /* Compute TCAM key */
                SelectKeyMask(model, tcamCfg1, &info); 

                /** Lookup in the TCAM and update rawHits with results of lookup
                 * on each line of TCAM slice #sliceNum */
                LookupTcam(model, group, i, tcamCfg1.CHUNK_MASK, &info);
                for (j = 0; j < HLP_FFU_TCAM_ENTRIES_0; j++)
                {
                    hits[j] = (fsc || tcamCfg1.START_COMPARE || hits[j]) && info.rawHits[j];
                }
                if (i == slice && tcamCfg1.START_COMPARE == 1 && tcamCfg1.CHUNK_MASK == 0)
                {
                    fsc = 1;
                }
                else
                {
                    fsc = 0;
                }
            }
            for (j = HLP_FFU_TCAM_ENTRIES_0-1; j >= 0; j--)
            {
                if (hits[j] & !setHit)
                {
                    // introduce slice_info to fix klocwork error
                    slice_info = slice+cascadeWidth[slice]-1;
                    tcamHitInfo[slice_info].hitIndexValid = TRUE;
                    tcamHitInfo[slice_info].hitIndex = j;
                    WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "group=%d, slice=%d, hitIndex=%d\n", group, slice+cascadeWidth[slice]-1, j)
                    if (exclusionSet)
                        setHit = TRUE;
                    break;
                }      
            }
        }
    }
    
} /* end CascadeLookupTcam */


/*****************************************************************************/
/** SetPrec
 * \ingroup intModel
 *
 * \desc            Utility function for setting the fields of actions if the
 *                  new update has higher precedence.
 *
 * \param[in,out]   old is the structure with precedence and value whose value
 *                  will be updated if the new prec is higher.
 *
 * \param[in]       prec is the precedence for the current update.
 *
 * \param[in]       value is the new value to be set.
 *
 *****************************************************************************/
static void SetPrec(hlp_modelPrecVal *old, fm_byte prec, fm_uint32 value)
{
    if ((prec >= old->prec) && (prec > 0))
    {
        old->prec = prec;
        old->val  = value;
    }

} /* end SetPrec */


/*****************************************************************************/
/** DoAction
 * \ingroup intModel
 *
 * \desc            Decodes action entry and populates FFU_ACTIONS
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       action points to the caller-allocated storage for the action
 *                  entry
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status DoAction(hlp_model    *model,
                          fm_uint32    action)
{
    
    hlp_modelState                  *state;
    hlp_modelFfuActions             *actions;
    fm_status                       status = FM_OK;
    fm_byte                         prec;
    hlp_modelFfuActionEntryType     entryType;
    fm_int                          i, j;
    fm_byte                         index;
    fm_byte                         indexA;
    fm_byte                         indexB;
    fm_byte                         indexC;
    fm_uint32                       value;
    fm_byte                         valueA;
    fm_byte                         valueB;
    fm_byte                         valueC;
    fm_byte                         enable;
    fm_bool                         enableA;
    fm_bool                         enableB;
    fm_bool                         enableC;
    fm_byte                         encode;
    
    state = &model->packetState;
    actions = &state->FFU_ACTIONS;
    
    /* Get Precedence */
    prec = FM_GET_FIELD(action, HLP_MODEL_FFU_ACTION, PREC);
    
    /* Get entryType*/
    entryType = HLP_MODEL_FFU_ACTION_NOP;
    encode = FM_GET_UNNAMED_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION_l_ENTRYTYPE,
                                  5);
    if(encode == 1)
    {
        entryType = HLP_MODEL_FFU_ACTION_SET4_4B;
    }
    else if (encode == 2)
    {
        entryType = HLP_MODEL_FFU_ACTION_SET8_1B;
    }
    else if (encode == 4)
    {
        entryType = HLP_MODEL_FFU_ACTION_SET3_1B;
    } 
    else if (((encode >> 3) & 0x3) == 1)
    {
           entryType = HLP_MODEL_FFU_ACTION_SET3_4B;
    }
    else if (((encode >> 4) & 0x1) == 1)
    {
           entryType = HLP_MODEL_FFU_ACTION_SET1_24B;
    }

    /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Action 0x%x \n", action);*/
    
    switch (entryType)
    {
        case HLP_MODEL_FFU_ACTION_NOP:
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Action: NOP\n");*/
            break;

        case HLP_MODEL_FFU_ACTION_SET4_4B:
            
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Action: SET4_4B\n")

            index = FM_GET_FIELD(action, 
                                 HLP_MODEL_FFU_ACTION,
                                 SET4_4B_INDEX);
            enable = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION,
                                  SET4_4B_ENABLE);
            value = FM_GET_FIELD(action, 
                                 HLP_MODEL_FFU_ACTION,
                                 SET4_4B_VALUE);
            
            for (i = 0 ; i < 4 ; i++)
            {
                if (index*4+i < HLP_MODEL_FFU_N_ACT4)
                {    
                    if ( enable  & (1 << i))
                    {
                        SetPrec(&actions->act4[index*4+i], 
                                prec, 
                                ((value >> 4*i) & 0xF));
                    }
                }
            }
            break;

        case HLP_MODEL_FFU_ACTION_SET8_1B:
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Action: SET8_1B\n")


            index = FM_GET_FIELD(action, 
                                 HLP_MODEL_FFU_ACTION,
                                 SET8_1B_INDEX);
            enable = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION,
                                  SET8_1B_ENABLE);
            value = FM_GET_FIELD(action, 
                                 HLP_MODEL_FFU_ACTION,
                                 SET8_1B_VALUE);
            
            for (i = 0 ; i < 8 ; i++)
            {
                if (index*8+i < HLP_MODEL_FFU_N_ACT1)
                {
                    if ( enable  & (1 << i))
                    {
                        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Action: SET8_1B :
                         * act1[%0d]=%0d\n", index*8+i, ((value >> i) & 0x1));*/

                        SetPrec(&actions->act1[index*8+i], 
                                prec, 
                                ((value >> i) & 0x1));
                    }
                }
            }
            
            break;
            
        case HLP_MODEL_FFU_ACTION_SET3_1B:
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Action: SET3_1B\n")
            enableA = FM_GET_BIT(action, HLP_MODEL_FFU_ACTION, SET3_1B_EA);
            valueA = FM_GET_BIT(action, HLP_MODEL_FFU_ACTION, SET3_1B_VA);
            indexA = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION, 
                                  SET3_1B_INDEXA);                 
            if (enableA && indexA < HLP_MODEL_FFU_N_ACT1)
            { 
                SetPrec(&actions->act1[indexA], prec, valueA);
            }
            
            enableB = FM_GET_BIT(action, HLP_MODEL_FFU_ACTION, SET3_1B_EB);
            valueB = FM_GET_BIT(action, HLP_MODEL_FFU_ACTION, SET3_1B_VB);
            indexB = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION, 
                                  SET3_1B_INDEXB);                 
            if (enableB && indexB < HLP_MODEL_FFU_N_ACT1)
            { 
                SetPrec(&actions->act1[indexB], prec, valueB);
            }
            
            enableC = FM_GET_BIT(action, HLP_MODEL_FFU_ACTION, SET3_1B_EC);
            valueC = FM_GET_BIT(action, HLP_MODEL_FFU_ACTION, SET3_1B_VC);
            indexC = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION, 
                                  SET3_1B_INDEXC);                 
            if (enableC && indexC < HLP_MODEL_FFU_N_ACT1)
            { 
                SetPrec(&actions->act1[indexC], prec, valueC);
            }
            
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "DoAction SET3_1B, EA=%d, VA=%d,
             * IndexA=%d EB=%d, VB=%d, IndexB=%d EC=%d, VC=%d, IndexC=%d\n",
             * enableA, valueA, indexA, enableB, valueB, indexB, enableC,
             * valueC, indexC);*/

            break;
    
        case HLP_MODEL_FFU_ACTION_SET3_4B:
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Action: SET3_4B\n")
            indexA = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION, 
                                  SET3_4B_INDEXA); 
            indexB = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION, 
                                  SET3_4B_INDEXB); 
            indexC = FM_GET_FIELD(action, 
                                  HLP_MODEL_FFU_ACTION, 
                                  SET3_4B_INDEXC);               
            valueA = FM_GET_FIELD(action, 
                                   HLP_MODEL_FFU_ACTION, 
                                   SET3_4B_VALUEA);
            valueB = FM_GET_FIELD(action, 
                                   HLP_MODEL_FFU_ACTION, 
                                   SET3_4B_VALUEB);
            valueC = FM_GET_FIELD(action, 
                                   HLP_MODEL_FFU_ACTION, 
                                   SET3_4B_VALUEC);
            if(indexA < HLP_MODEL_FFU_N_ACT4)
            {
                SetPrec(&actions->act4[indexA], prec, valueA);
            }
            if(indexB < HLP_MODEL_FFU_N_ACT4)
            {
                SetPrec(&actions->act4[indexB], prec, valueB);
            }
            if(indexC < HLP_MODEL_FFU_N_ACT4)
            {
                SetPrec(&actions->act4[indexC], prec, valueC);
            } 
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Action: SET3_4B : indexA=%d
             * indexB=%d, indexC=%d\n", indexA, indexB, indexC);*/
            break;
        
        case HLP_MODEL_FFU_ACTION_SET1_24B:
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Action: SET1_24B\n")
            index = FM_GET_FIELD(action, 
                                 HLP_MODEL_FFU_ACTION, 
                                 SET1_24B_INDEX);
            value = FM_GET_FIELD(action, 
                                   HLP_MODEL_FFU_ACTION, 
                                   SET1_24B_VALUE);
            if(index < HLP_MODEL_FFU_N_ACT24)
            {
                SetPrec(&actions->act24[index], prec, value);
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "SET1_24B: prec=%d index=%d,
             * value=0x%x\n", prec, index, value);*/
            
            break;

        default:
            status = FM_ERR_INVALID_ARGUMENT;
            break;

    }
    return status;
    
} /* end DoAction */

/*****************************************************************************/
/** ActionResolution 
 * \ingroup intModel
 *
 * \desc            Reads action entries for all slices with valid hit index,
 *                  resolves precedence among all the actions and populates
 *                  FFU_ACTIONS structure.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is the number of the ffu group for which SRAM
 *                  actions have to be resolved.
 *
 * \param[in]       tcamHitInfo points to the caller-allocated structure which
 *                  has the valid ram indices for each slice
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status ActionResolution(hlp_model               *model,
                                  fm_byte                 group,
                                  hlp_modelFfuHitInfo     *tcamHitInfo)
{
    
    hlp_modelState      *state;
    fm_status           status = FM_OK;
    fm_int              actionRamNum;
    fm_uint32 *         actionCfg;
    fm_byte             slice;
    fm_uint32 *         actionRamEntry;
    fm_uint32           action[HLP_MODEL_FFU_ACTIONS_PER_ENTRY];
    fm_int              i;
    fm_bool             enable;

    
    state = &model->packetState;
    

    actionCfg = FM_MODEL_GET_REG_PTR(model, 
                                     HLP_FFU_ACTION_CFG(group, 
                                                        state->FFU_SCENARIO,
                                                        0));
    
    for (actionRamNum = 0 ; 
         actionRamNum < HLP_FFU_ACTION_ENTRIES_1; 
         actionRamNum++)
    {
        
        enable = FM_ARRAY_GET_UNNAMED_FIELD(actionCfg, 
                                            actionRamNum + HLP_FFU_ACTION_ENTRIES_1*4, 
                                            1);
        if(enable == FALSE)
        {
            /* skip action RAM if disabled */
            continue;
        }    
        slice = FM_ARRAY_GET_UNNAMED_FIELD(actionCfg, actionRamNum*4, 4);
        
        if (tcamHitInfo[slice].hitIndexValid)
        {
            
            actionRamEntry = FM_MODEL_GET_REG_PTR(model, 
                                                  HLP_FFU_ACTION(group, 
                                                                 actionRamNum, 
                                                                 tcamHitInfo[slice].hitIndex, 
                                                                 0));
    
            action[0] = FM_ARRAY_GET_FIELD(actionRamEntry, HLP_FFU_ACTION, ACTION0); 
    
            action[1] = FM_ARRAY_GET_FIELD(actionRamEntry, HLP_FFU_ACTION, ACTION1); 
    
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "ActionResolution group=%d, slice=%d, actionRam=%d, index=%d, action0=0x%x, action1=0x%x\n", group, slice, actionRamNum, tcamHitInfo[slice].hitIndex, action[0], action[1])
            
            
            /* loop for action0 and action1 */
            for (i = 0 ; i < HLP_MODEL_FFU_ACTIONS_PER_ENTRY ; i++)
            {
                status = DoAction(model, action[i]); 
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }    
        }
    }

ABORT:
    return status;

}   /* end GetActions */

/*****************************************************************************/
/** DoKeyCompaction 
 * \ingroup intModel
 *
 * \desc            Applies key mask to input ffu keys and packs them into array
 *                  of bytes with masked keys 
 *
 * \param[in]       ffuKeys is the input struct that provides FFU keys to be
 *                  converted to bytes
 *
 * \param[in]       keyMaskCfg points to the caller allocated storage for
 *                  KeyMask configuation(FFU_KEY_MASK)
 *
 * \param[in,out]   packedKeys is the caller allocated storage which is populated
 *                  with packed array of bytes built from masked keys
 *
 * \param[in,out]   keySize is the size of the compact array of bytes
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status DoKeyCompaction(hlp_modelFfuKeys       ffuKeys,
                                 hlp_modelFfuKeyMaskCfg keyMaskCfg,
                                 fm_byte                *packedKeys,
                                 fm_byte                *keySize)
{
    fm_status   status = FM_OK;
    fm_byte     keyIdx;
    fm_int      i;
    fm_int      j;

    keyIdx = 0;
    /* KEY32 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY32; i++)
    {
        if (FM_GET_UNNAMED_FIELD(keyMaskCfg.Key32Mask, i, 1))
        {
            for (j = 0; j < 4; j++)
            {
                packedKeys[keyIdx + j] = (ffuKeys.key32[i] >> (8*(3-j))) & 0xFF;
                /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "packedKey[%0d]=0x%x\n",
                 * keyIdx + j, packedKeys[keyIdx + j])*/
            }
            keyIdx += 4;
        }
    }
    /* KEY16 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        if (FM_GET_UNNAMED_FIELD(keyMaskCfg.Key16Mask, i, 1))
        {
            packedKeys[keyIdx] = (ffuKeys.key16[i] >> 8) & 0xFF;
            packedKeys[keyIdx + 1] = ffuKeys.key16[i] & 0xFF;
            /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "packedKey[%0d]=0x%x\n", keyIdx,
             * packedKeys[keyIdx])*/
            /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "packedKey[%0d]=0x%x\n", keyIdx + 1,
            packedKeys[keyIdx + 1])*/
            keyIdx = keyIdx + 2;
        }
                    
    }
    /* KEY8 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY8; i++)
    {
        if (FM_GET_UNNAMED_FIELD(keyMaskCfg.Key8Mask, i, 1))
        {
            packedKeys[keyIdx] = ffuKeys.key8[i]; 
            /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "packedKey[%0d]=0x%x\n", keyIdx,
             * packedKeys[keyIdx])*/
            keyIdx++;
        }
    }
    // pad key size to be multiple of 4B
    if(keyIdx%4 != 0)
    {
        for (i = keyIdx % 4; i < 4; i++)
        {
            packedKeys[keyIdx] = 0;
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "packedKey[%0d]=0x%x\n",
             * keyIdx, packedKeys[keyIdx]);*/
            keyIdx++;
            if(keyIdx == HLP_MODEL_FFU_N_HASH_KEYS) 
            {
                break;
            }
        }
    }
    *keySize = keyIdx;
    
ABORT:
    return status;

}   /* end DoKeyCompaction */

/*****************************************************************************/
/** GetKeyMaskCfg
 * \ingroup intModel
 *
 * \desc            Reads FFU_KEY_MASK[0,1,2] register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       hashNum is the hash number for which key masks need to be
 *                  read
 *
 * \param[in,out]   hashCfg is the combined struct for FFU_KEY_MASK[0,1,2] that
 *                  is populated with data from register cache
 * 
 *****************************************************************************/
static void GetKeyMaskCfg(hlp_model                 *model,
                          fm_byte                   group,
                          fm_byte                   hashNum,
                          hlp_modelFfuKeyMaskCfg   *keyMaskCfg)
{
    fm_uint32       *regPtr;
    hlp_modelState  *state;
    
    state = &model->packetState;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FFU_KEY_MASK0(group, 
                                                           hashNum, 
                                                           state->FFU_SCENARIO, 
                                                           0));
    keyMaskCfg->Key8Mask  = 
        FM_ARRAY_GET_FIELD64(regPtr, HLP_FFU_KEY_MASK0, KEY8_MASK);

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FFU_KEY_MASK1(group, 
                                                           hashNum, 
                                                           state->FFU_SCENARIO, 
                                                           0));
    keyMaskCfg->Key16Mask = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_KEY_MASK1, KEY16_MASK);
    keyMaskCfg->Key32Mask = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_KEY_MASK1, KEY32_MASK);
    keyMaskCfg->KeySubmode[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_KEY_MASK1, KEY_SUBMODE0);
    keyMaskCfg->KeySubmode[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_KEY_MASK1, KEY_SUBMODE1);
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FFU_KEY_MASK2(group, 
                                                           hashNum, 
                                                           state->FFU_SCENARIO, 
                                                           0));
    keyMaskCfg->KeySubmask[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_KEY_MASK2, KEY_SUBMASK0);
    keyMaskCfg->KeySubmask[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_KEY_MASK2, KEY_SUBMASK1);  
    
    /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Key16Mask=0x%x, Key8Mask=0x%x,
     * Key32Mask=0x%x, KeySubmode[0]=0x%x, KeySubmode[1]=0x%x,
     * KeySubmask[0]=0x%x, KeySubmask[1]=0x%x\n", keyMaskCfg->Key16Mask,
     * keyMaskCfg->Key8Mask, keyMaskCfg->Key32Mask, keyMaskCfg->KeySubmode[0],
     * keyMaskCfg->KeySubmode[1], keyMaskCfg->KeySubmask[0],
     * keyMaskCfg->KeySubmask[1]);*/    
}

/*****************************************************************************/
/** GetHashCfg 
 * \ingroup intModel
 *
 * \desc            Reads FFU_HASH_CFG register data from register cache and
 *  
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is number of the group used as index for accessing
 *                  FFU_HASH_CFG
 *
 * \param[in,out]   hashCfg is the struct for FFU_HASH_CFG[0,1] that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetHashCfg(hlp_model            *model,
                       fm_byte              group,
                       hlp_modelFfuHashCfg  *hashCfg)
{
    hlp_modelState  *state;
    fm_uint32       *regPtr;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, 
                                  HLP_FFU_HASH_CFG(group, 
                                                   state->FFU_SCENARIO, 
                                                   0));
    
    hashCfg->mode = 
        FM_ARRAY_GET_BIT(regPtr, HLP_FFU_HASH_CFG, MODE);
    hashCfg->base_ptr[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_CFG, BASE_PTR_0);
    hashCfg->base_ptr[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_CFG, BASE_PTR_1);
    hashCfg->hash_size[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_CFG, HASH_SIZE_0);
    hashCfg->hash_size[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_CFG, HASH_SIZE_1);
    hashCfg->entry_size[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_CFG, ENTRY_SIZE_0);
    hashCfg->entry_size[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_CFG, ENTRY_SIZE_1);

     /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "mode=%0d,
      * base_ptr[0]=0x%x, base_ptr[1]=0x%x, hash_size[0]=0x%x,
      * hash_size[1]=0x%x, entry_size[0]=0x%x, entry_size[1]=0x%x\n",
      * hashCfg->mode, hashCfg->base_ptr[0],
      * hashCfg->base_ptr[1], hashCfg->hash_size[0], hashCfg->hash_size[1],
      * hashCfg->entry_size[0], hashCfg->entry_size[1]);*/

} /* GetHashCfg */


/*****************************************************************************/
/** GetLookupEntry 
 * \ingroup intModel
 *
 * \desc            Reads FFU_HASH_CFG register data from register cache and
 *  
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is number of the group used as index for accessing
 *                  FFU_HASH_LOOKUP
 *
 * \param[in]       hashNum is the hash number for which lookup table
 *                  data needs to be read    
 *
 * \param[in]       lookupPtr is the index from which lookup entry
 *                  needs to be read  
 *
 * \param[in,out]   lookupEntry is the struct for FFU_HASH_LOOKUP that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetLookupEntry(hlp_model        *model,
                           fm_byte          group,
                           fm_byte          hashNum,
                           fm_uint16        lookupPtr,
                           hlpFfuHashLookup *lookupEntry)
{
    hlp_modelState  *state;
    fm_uint32       *regPtr;

    FM_NOT_USED(hashNum);

    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FFU_HASH_LOOKUP(group, lookupPtr, 0));

    lookupEntry->PTR = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, PTR);
    lookupEntry->SELECT_4 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, SELECT_4);
    lookupEntry->SELECT_3 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, SELECT_3);
    lookupEntry->SELECT_2 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, SELECT_2);
    lookupEntry->SELECT_1 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, SELECT_1);
    lookupEntry->SELECT_0 = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, SELECT_0);
    lookupEntry->MASK = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_LOOKUP, MASK);

    /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ptr=0x%x, select_4=0x%x,
     * select_3=0x%x, select_2=0x%x, select_1=0x%x, select_0=0x%x,
     * Mask=0x%x\n", lookupEntry->PTR, lookupEntry->SELECT_4,
     * lookupEntry->SELECT_3, lookupEntry->SELECT_2, lookupEntry->SELECT_1,
     * lookupEntry->SELECT_0, lookupEntry->MASK);*/

} /* GetLookupEntry */

/*****************************************************************************/
/** GetHashRamEntry
 * \ingroup intModel
 *
 * \desc            Read on hash ram entry
 *  
 * \param[in]       model points to the switch data structure.
 *  
 * \param[in]       hashNum specifies which hash ram to use
 *  
 * \param[in]       hashRamAddr is the address of hash ram
 *
 * \param[out]      hashEntry points the data read from Hash Ram Entry
 *
 *****************************************************************************/
static fm_uint64 GetHashRamEntry(hlp_model          *model,
                                 fm_int              hashNum,
                                 fm_uint32           hashRamAddr)
{
    fm_uint32               *regPtr;
    fm_uint64                hashEntry;
    
    hashEntry = 0;
    //if(entryMode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B)
    //{
    if (hashNum == 0)
    {
        regPtr = 
            FM_MODEL_GET_REG_PTR(model,
                HLP_HASH_ENTRY0(hashRamAddr, 0));
        hashEntry = 
            FM_ARRAY_GET_FIELD64(regPtr, 
                    HLP_HASH_ENTRY0, DATA);
    }
    else
    {    
        regPtr = 
            FM_MODEL_GET_REG_PTR(model,
                HLP_HASH_ENTRY1(hashRamAddr, 0));
        hashEntry = 
            FM_ARRAY_GET_FIELD64(regPtr, 
                    HLP_HASH_ENTRY1, DATA);
    } 
    //}
    //else
    //{
        //regPtr = 
        //        FM_MODEL_GET_REG_PTR(model,
        //            HLP_HASH_ENTRY(hashRamAddr, 0));
        //hashEntry = 
        //        FM_ARRAY_GET_FIELD64(regPtr, 
        //                HLP_HASH_ENTRY, data);
    //}

    return hashEntry;

} /* GetHashRamEntry */


/*****************************************************************************/
/** GetHashRamData
 * \ingroup intModel
 *
 * \desc            Calculates Hash Ram addresses and reads data from rams
 *  
 * \param[in]       model points to the switch data structure.
 *  
 * \param[in]       group specifies which group to use
 *  
 * \param[in]       hashNum specifies which hash to use
 *
 * \param[in]       bucket is data from FFU_HASH_LOOKUP and used to calculate
 *                  Hash Ram address
 *
 * \param[in]       hashCfg provides entry mode and entry size to be used to
 *                  calculate Hash Ram address
 *
 * \param[in]       hashMore is the upper 16b from hash value, which will be
 *                  used to determine the offset
 *
 * \param[out]      ramKey points the data read from Hash Ram, it contains keys
 *                  and actions
 *
 * \param[out]      ramDataOk is set when alloc is OK and Mask[bucket_hash] is
 *                  set.
 *                  
 *****************************************************************************/
static void GetHashRamData(hlp_model          *model,
                           fm_byte             group,
                           fm_int              hashNum,
                           hlpFfuHashLookup    bucket, 
                           hlp_modelFfuHashCfg hashCfg,
                           fm_uint16           hashMore,
                           fm_byte            *ramKey,
                           fm_bool            *ramDataOk)
{
    fm_uint32               *regPtr;
    fm_uint16               offset;
    fm_uint32               rawPtr;     
    fm_byte                 bucketHash = 0;
    //fm_byte                 mmuIndex;
    //fm_byte                 mmuValue;
    fm_uint16               line;
    fm_byte                 startByte;
    fm_uint32               hashRamAddr;
    fm_uint32               hashRamEntryIdx;
    fm_byte                 hashRamKey[HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE];
    fm_uint64               hashEntry;
    fm_int                  i;
    fm_int                  k;
    fm_int                  entryNum;
    fm_int                  maxBytes;
    fm_int                  remainBytes;
    fm_int                  keyIdx;
    fm_bool                 readRamNum = 0;
    fm_byte                 ramAlloc[HLP_FFU_KEY_MASK0_ENTRIES_1] = {0, 0};
    fm_bool                 alloc;
    fm_int                  col;

    FM_NOT_USED(group);

    FM_SET_UNNAMED_FIELD(bucketHash,
                         0,
                         1,
                         FM_GET_UNNAMED_FIELD(hashMore, bucket.SELECT_0, 1));
    FM_SET_UNNAMED_FIELD(bucketHash,
                         1,
                         1,
                         FM_GET_UNNAMED_FIELD(hashMore, bucket.SELECT_1, 1));
    FM_SET_UNNAMED_FIELD(bucketHash,
                         2,
                         1,
                         FM_GET_UNNAMED_FIELD(hashMore, bucket.SELECT_2, 1));
    FM_SET_UNNAMED_FIELD(bucketHash,
                         3,
                         1,
                         FM_GET_UNNAMED_FIELD(hashMore, bucket.SELECT_3, 1));
    FM_SET_UNNAMED_FIELD(bucketHash,
                         4,
                         1,
                         FM_GET_UNNAMED_FIELD(hashMore, bucket.SELECT_4, 1));
    
    offset = 0;
    //Buckets valid entries are stored contiguously in the entry table
    for (i = 0; i < 32; i++)
    {
        if (i == bucketHash)
        {
            break;
        }        
        if ((bucket.MASK >> i) & 1)
        {
           offset++;
        }
    }

    /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "bucketHash=%0d, offset=%0d\n",
     * bucketHash, offset);*/
    
    hashRamAddr = bucket.PTR*4 + offset * hashCfg.entry_size[hashNum] * 4;
    /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hashRamAddr=0x%x\n", hashRamAddr);*/
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_HASH_ENTRY_RAM_ALLOC(0, 0));
    ramAlloc[0] = 
        FM_ARRAY_GET_FIELD64(regPtr,  HLP_HASH_ENTRY_RAM_ALLOC, GP_SEL);
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_HASH_ENTRY_RAM_ALLOC(1, 0));
    ramAlloc[1] = 
        FM_ARRAY_GET_FIELD64(regPtr,  HLP_HASH_ENTRY_RAM_ALLOC, GP_SEL);
    WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "ramAlloc[0]=0x%x, ramAlloc[1]=0x%x.\n", ramAlloc[0], ramAlloc[1])
    if(hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B)
    {
        //mmuIndex = FM_GET_UNNAMED_FIELD(rawPtr, 15, 4);
        //mmuValue = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, mmuIndex*4, 4);
        //hashRamAddr = (mmuValue << 16) | (rawPtr & 0xFFFF);

        line = FM_GET_UNNAMED_FIELD(hashRamAddr, 5, 14);
        startByte = FM_GET_UNNAMED_FIELD(hashRamAddr, 0, 5);
        hashRamEntryIdx = (hashRamAddr >> 3) & 0xFFFF;
        
        maxBytes = HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE/2;
    }
    else
    {
        //mmuIndex = FM_GET_UNNAMED_FIELD(rawPtr, 16, 4);
        //mmuValue = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, mmuIndex*4, 4);
        //hashRamAddr = (mmuValue << 16) | (rawPtr & 0xFFFF);

        line = FM_GET_UNNAMED_FIELD(hashRamAddr, 6, 14);
        startByte = FM_GET_UNNAMED_FIELD(hashRamAddr, 0, 5);
        hashRamEntryIdx = line * 4 + startByte / 8;
        //hashRamEntryIdx = hashRamAddr >> 3;
        readRamNum = FM_GET_UNNAMED_FIELD(hashRamAddr, 5, 1);
        //hashRamEntryIdx = (hashRamAddr >> 3) & 0xFFFF;
        //reassign start_byte to 6b to indicate it's position in 64B
        startByte = FM_GET_UNNAMED_FIELD(hashRamAddr, 0, 6);
        maxBytes = HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE;
    }

    WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "hashEntry %0d\n", hashRamEntryIdx)
    
    keyIdx = 0;
        
    entryNum = (maxBytes - startByte) / 8 + 
           (((maxBytes - startByte) % 8 == 0) ? 0 : 1);
    remainBytes = hashCfg.entry_size[hashNum] * 4 - maxBytes 
                + startByte; 
    entryNum += remainBytes / 8 + 
               ((remainBytes % 8 == 0) ? 0 : 1);
    *ramDataOk  = TRUE;

    /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Start reading from hashRamEntryIdx
     * %0d\n", hashRamEntryIdx)*/

    for (i = 0; i < entryNum; i++)
    {   
        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hashEntry %0d. readRam %0d\n",
         * hashRamEntryIdx, readRamNum);*/

        if(hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B)
        {
            //Each col has 8192 entries. 65536/8
            col   = hashRamEntryIdx / 8192;
            alloc = FM_GET_UNNAMED_FIELD(ramAlloc[hashNum], col, 1);
            if(alloc != group)
            {
                *ramDataOk = FALSE;
                break;
            }
            hashEntry = GetHashRamEntry(model,
                                        hashNum,
                                        hashRamEntryIdx);  
            hashRamEntryIdx ++;
        }  
        else
        {
            if(readRamNum == 0)
            {
                col   = hashRamEntryIdx / 8192;
                alloc = FM_GET_UNNAMED_FIELD(ramAlloc[readRamNum], col, 1);
                if(alloc != group)
                {
                    *ramDataOk = FALSE;
                    break;
                }
                hashEntry = GetHashRamEntry(model,
                                            readRamNum,
                                            hashRamEntryIdx); 
                if((hashRamEntryIdx % 4) == 3)
                {
                    readRamNum = 1;
                    hashRamEntryIdx = (hashRamEntryIdx / 4) * 4; 
                }
                else
                {
                    hashRamEntryIdx ++; 
                }
            }
            else
            {
                col   = hashRamEntryIdx / 8192;
                alloc = FM_GET_UNNAMED_FIELD(ramAlloc[readRamNum], col, 1);
                if(alloc != group)
                {
                    *ramDataOk = FALSE;
                    break;
                }
                hashEntry = GetHashRamEntry(model,
                                            readRamNum,
                                            hashRamEntryIdx); 
                if((hashRamEntryIdx % 4) == 3)
                {
                    readRamNum = 0;
                }
                hashRamEntryIdx ++; 
            }
        } 

        if (i == 0)
        {
            for (k = startByte % 8; k < 8; k++)
            {
                 ramKey[keyIdx] = (hashEntry >> 8*(7-k)) & 0xFF;
                 /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "ramKey[%0d]=0x%x\n", keyIdx,
                  * ramKey[keyIdx])*/
                 keyIdx++;
            }
        }
        else
        {
             for (k = 0; k < 8; k++)
            {
                 if(keyIdx >= hashCfg.entry_size[hashNum] * 4) break;
                 ramKey[keyIdx] = (hashEntry >> 8*(7-k)) & 0xFF;
                 /*WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "ramKey[%0d]=0x%x\n", keyIdx,
                  * ramKey[keyIdx])*/
                 keyIdx++;
            }
        }
        if(keyIdx >= hashCfg.entry_size[hashNum] * 4) break;
    }

    if(FM_GET_UNNAMED_FIELD(bucket.MASK, bucketHash, 1) == 0)
    {
        *ramDataOk = FALSE;
    }


    // if another line is needed, it might span to another column and has
    // different mmu value, so we'll need a new hashRamEntryIdx
    //if ((startByte + hashCfg.entry_size[hashNum] * 4) >= maxBytes)
    //{
    //    if(hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B)
    //    {
    //        FM_SET_UNNAMED_FIELD(rawPtr,
    //                             5,
    //                             14,
    //                             line + 1); 
    //        FM_SET_UNNAMED_FIELD(rawPtr,
    //                             0,
    //                             5,
    //                             0);

    //        mmuIndex = FM_GET_UNNAMED_FIELD(rawPtr, 15, 4);
    //        mmuValue = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, mmuIndex*4, 4);
    //        hashRamAddr = (mmuValue << 16) | (rawPtr & 0xFFFF);
    //        hashRamEntryIdx = (hashRamAddr >> 3) & 0xFFFF;
    //    }
    //    else
    //    {
    //        FM_SET_UNNAMED_FIELD(rawPtr,
    //                             6,
    //                             14,
    //                             line + 1); 
    //        FM_SET_UNNAMED_FIELD(rawPtr,
    //                             0,
    //                             6,
    //                             0);

    //        mmuIndex = FM_GET_UNNAMED_FIELD(rawPtr, 16, 4);
    //        mmuValue = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, mmuIndex*4, 4);
    //        hashRamAddr = (mmuValue << 16) | (rawPtr & 0xFFFF);
    //        hashRamEntryIdx = hashRamAddr >> 3;
    //    }
    //}

    //remainBytes = hashCfg.entry_size[hashNum] * 4 - maxBytes 
    //            + startByte; 
    //entryNum = remainBytes / 8 + 
    //           ((remainBytes % 8 == 0) ? 0 : 1);
    //for (i = 0; i < entryNum; i++)
    //{
    //    hashEntry = GetHashRamEntry(model,
    //                               hashNum,
    //                               hashCfg.mode,
    //                               hashRamEntryIdx + i);
 
    //    for (k = 0; k < 8; k++)
    //    {
    //        if(keyIdx >= hashCfg.entry_size[hashNum] * 4) break;
    //         ramKey[keyIdx] = (hashEntry >> 8*(7-k)) & 0xFF;
    //         /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ramKey[%0d]=0x%x\n", keyIdx,
    //          * ramKey[keyIdx]);*/
    //         keyIdx++;
    //    }
    //    if(keyIdx >= hashCfg.entry_size[hashNum] * 4) break;
    //}
} /* GetHashRamData */

/*****************************************************************************/
/** GetHashActions 
 * \ingroup intModel
 *
 * \desc            Get hash actions
 *  
 * \param[in]       entry points to the structure which stores the data bytes
 *                  read from ram or cam
 *
 * \param[in]       keySize is the size of padded keys
 *
 * \param[in]       entrySize is the size of entry
 *
 * \param[out]      actions point to the structure which stores the actions
 *
 *****************************************************************************/
static void GetHashActions(fm_byte          *entry,
                           fm_byte           entryMode,
                           fm_byte           keySize,
                           fm_uint16         entrySize,
                           fm_bool           isCam,
                           fm_uint32        *actions)
{
    fm_int bytesNum;
    fm_int maxActBytesNum;
    fm_int readBytesNum;
    fm_int actIdx = 0;
    fm_int i;
    fm_int j;

    FM_NOT_USED(entryMode);

    bytesNum = entrySize - keySize;
    maxActBytesNum = (entryMode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B) ?
                     16 : 8;
    //maxActBytesNum = HLP_MODEL_FFU_MAX_HASH_ACTIONS * 4;
    readBytesNum = (bytesNum > maxActBytesNum) ? 
                   maxActBytesNum : bytesNum;
    actIdx = (isCam) ? ((entryMode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B) ? 2 : 0) : 
                       (readBytesNum / 4 - 1);

    WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "KeySize=0x%x, maxActBytes=%0d, entrySize=0x%x\n", keySize, maxActBytesNum, entrySize);
    if(entrySize > 0)
    {
        for(i = 0; i < readBytesNum / 4; i++)
        {

            
            for(j = 0; j < 4; j++)
            {
                FM_SET_UNNAMED_FIELD(actions[actIdx],
                                     8 * j,
                                     8,
                                     entry[entrySize - i * 4 - j - 1]);
            }

            /*for(j = 0; j < 4; j++)
            {
                FM_SET_UNNAMED_FIELD(actions[actIdx],
                                     8 * (3-j),
                                     8,
                                     entry[entrySize - (readBytesNum / 4 - i) * 4 + j]);
            }*/

            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "actions[%0d]=0x%x\n", actIdx, actions[actIdx])

            if(isCam)
            {   
                /* Get next actIdx */
                switch((i+1))
                {
                    case HLP_MODEL_FFU_HASH_CAM_ETY_7_BITS_63_32:
                         actIdx ++;
                         break;
                    case HLP_MODEL_FFU_HASH_CAM_ETY_6_BITS_31_0: 
                         actIdx = 0;
                         break;
                    case HLP_MODEL_FFU_HASH_CAM_ETY_6_BITS_63_32: 
                         actIdx = 1;
                         break;
                    default: 
                         break;  
                }
            }
            else
            {
                actIdx --;
            }
        }
    }
}

/*****************************************************************************/
/** FGHash 
 * \ingroup intModel
 *
 * \desc            Hash Operation. 
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       group is the number of the group for which hash operation
 *                  has to be performed 
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status FGHash(hlp_model               *model,
                        fm_byte                 group)
{
    hlp_modelState          *state;
    hlp_modelFfuKeys        *ffuKeys;
    hlp_modelFfuKeys        hashKeys;
    fm_byte                 hashBytes[HLP_MODEL_FFU_N_HASH_KEYS];
    fm_status               status = FM_OK;
    fm_int                  hashNum;
    hlp_modelFfuKeyMaskCfg  keyMaskCfg;
    fm_uint32               *regPtr;
    fm_uint32               hash;
    fm_uint16               hashIndex;
    fm_uint16               hashMore;
    hlp_modelFfuHashCfg     hashCfg;
    fm_uint16               lookupPtr;
    hlpFfuHashLookup        bucket;
    fm_byte                 packedKeys[HLP_MODEL_FFU_N_HASH_KEYS];
    fm_byte                 keySize;
    fm_byte                 hashRamKey[HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE];
    fm_byte                 camKey[HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE];
    fm_byte                 minCamKeySize;
    fm_uint64               camEntry;   
    fm_bool                 camHit;
    fm_bool                 hashRamHit;
    fm_uint64               scenarioMask;
    fm_bool                 hashCamEn;
    fm_uint32               actions[HLP_MODEL_FFU_MAX_HASH_ACTIONS];
    fm_bool                 isCam;
    fm_int                  keyIdx;
    fm_int                  camEntrySize;
    fm_int                  i;
    fm_int                  j;
    fm_int                  k;
    fm_bool                 ramDataOk;
     
    state = &model->packetState;
    ffuKeys = &state->FFU_KEYS;

    for(i = 0; i < HLP_MODEL_FFU_MAX_HASH_ACTIONS; i++)
    {
         state->FGHASH_ACTIONS[group].actionValid[i] = 0;
    }


    for (hashNum = 0 ; hashNum < HLP_FFU_KEY_MASK0_ENTRIES_1 ; hashNum++)
    {
        // Initialize actions
        for(i = 0; i < HLP_MODEL_FFU_MAX_HASH_ACTIONS; i++)
        {
            actions[i] = 0;
        }
        
        /* Get FFU_KEY_MASK register fields */
        GetKeyMaskCfg(model, group, hashNum, &keyMaskCfg);

        /* Apply KeyMask and KeySubmask on FFU Keys */
        status = hlpModelApplyKeyMask(model, keyMaskCfg, &hashKeys);
        
        /* Convert Keys into array of bytes */ 
        status = hlpModelConvertKeysToBytes(hashKeys, hashBytes);
        
        /* Get hash value from crc */
        if (hashNum == 0)
        {    
            /* HASH0 - CRC-32 (Ethernet) */
            hash = fmCrc32ByteSwap(hashBytes, HLP_MODEL_FFU_N_HASH_KEYS); 
        } 
        else
        {    
            /* HASH1 - CRC-32C (iSCSI) */
            hash = fmCrc32CByteSwap(hashBytes, HLP_MODEL_FFU_N_HASH_KEYS); 
        }    
        WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "CRC32=0x%x\n", hash)

        
        /* Key Compaction */
        status =  DoKeyCompaction(hashKeys, 
                                  keyMaskCfg, 
                                  packedKeys, 
                                  &keySize);
        
        //Index  = hash[12:0], More = hash[28:13]
        hashIndex = hash & 0x1fff;
        hashMore = (hash >> 16) & 0xffff;
        
        GetHashCfg(model, group, &hashCfg);

        /* Don't perform lookups if hashNum is 1 in non-split mode, */
        if( ( hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B ) &&
            ( hashNum == HLP_FFU_KEY_MASK0_ENTRIES_1 - 1 ) )        
        {
           break;
        }
        
        /* Don't perform lookups if hash entry size is 0 */
        if(hashCfg.entry_size[hashNum] == 0)
        {
           WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "hashCfg.entry_size[%0d]=0x%x\n", hashNum, hashCfg.entry_size[hashNum])
           continue;
        }

        lookupPtr = hashCfg.base_ptr[hashNum] + hashIndex % (1 <<
                hashCfg.hash_size[hashNum]) + 
                ((hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B && 
                hashNum == 1) ? 4096 : 0);
        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "lookupPtr=0x%x\n", lookupPtr);*/
        
        GetLookupEntry(model, group, hashNum, lookupPtr, &bucket); 
                        
        //keyIdx = 0;
        minCamKeySize = 
           (hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B) ?
           (HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE - HLP_MODEL_FFU_MAX_HASH_ACTIONS*4) :
           (HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE/2 -
            (HLP_MODEL_FFU_MAX_HASH_ACTIONS/2)*4);

        hashCamEn = FALSE;
        for (i = HLP_FFU_HASH_CAM_ENTRIES_1-1; i >= 0; i--)
        {
            /* Initialize cam key to 0*/
            for (j = 0; j < HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE; j++)
            {
                camKey[j] = 0;
            }
            /* Build Cam Key */
            keyIdx = 0;
            for (j = 0; j < HLP_FFU_HASH_CAM_ENTRIES_0; j++)
            {
                if ((hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B) ||
                    ((hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B) && 
                     (((hashNum == 0) && (j < 4)) || ((hashNum == 1) && (j >= 4)))))
                {
                    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FFU_HASH_CAM(group, i, j, 0));
                    camEntry = FM_ARRAY_GET_FIELD64(regPtr, HLP_FFU_HASH_CAM, DATA);
                
                    for (k = 0; k < 8; k++)
                    {
                        camKey[keyIdx] = (camEntry >> 8*(7-k)) & 0xFF;
                        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "i=%0d, j=%0d,
                         * k=%0d, camkey[%0d]=0x%x\n", i, j, k, keyIdx,
                         * camKey[keyIdx]);*/
                        keyIdx++;
                    }
                }      
            }

            /* Compare Cam key with packeyKey */
            camHit = TRUE;          
            if(keySize < HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE)
            {
                for (j = 0; j < keySize; j++)
                {
                    if (packedKeys[j] != camKey[j])
                    {
                       camHit = FALSE;
                       break;
                    }
                }
                // If keySize < minCamKeySize, packed keys need to be padded
                // with 0's to minCamKeySize.
                // packedKeys[keySize...minCamKeySize-1] are 0 in this case, 
                // so we can get camHit by checking if 
                // camKey[keySize...minCamKeySize-1] are 0.
                if(keySize < minCamKeySize)
                {
                    for (j = keySize; j < minCamKeySize; j++)
                    {
                        if (camKey[j] != 0)
                        {
                           camHit = FALSE;
                           break;
                        }
                    }
                }
            }

            if(camHit)
            {
                regPtr = FM_MODEL_GET_REG_PTR(model, 
                            HLP_FFU_HASH_CAM_EN(group, hashNum, i, 0));
                scenarioMask = FM_ARRAY_GET_FIELD64(regPtr, HLP_FFU_HASH_CAM_EN, MASK);
                hashCamEn = (scenarioMask >> (state->FFU_SCENARIO)) & 1;
                // 64B entry mode needs to consider CAM_EN[0/1]
                if(hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B)
                { 
                     regPtr = FM_MODEL_GET_REG_PTR(model, 
                              HLP_FFU_HASH_CAM_EN(group, hashNum+1, i, 0));
                     scenarioMask = FM_ARRAY_GET_FIELD64(regPtr, HLP_FFU_HASH_CAM_EN, MASK);
                     hashCamEn &= (scenarioMask >> (state->FFU_SCENARIO)) & 1;
                }
                /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hashCamEn=%0d\n",
                 * hashCamEn);*/
                /* Get CAM Actions */
                if (hashCamEn)
                {
                    camEntrySize = (hashCfg.mode ==
                                    HLP_MODEL_FFU_HASH_ENTRY_MODE_64B) ?
                                    HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE :
                                   (HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE / 2);
                    isCam = 1;
                    WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Reading cam actions\n")
                    GetHashActions(camKey,
                                   hashCfg.mode,
                                   keySize, 
                                   camEntrySize,
                                   isCam, 
                                   actions);
                    break;
                }
            }
        }
        
        // Check if hit Hash Ram, then get actions from Ram
        hashRamHit = TRUE;
        if(!hashCamEn)
        {
            GetHashRamData(model, 
                           group,
                           hashNum, 
                           bucket, 
                           hashCfg, 
                           hashMore, 
                           hashRamKey,
                           &ramDataOk);
            /* Compare Cam key with packeyKey */
            if(keySize < HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE)
            {
                for (j = 0; j < keySize; j++)
                {
                    if (packedKeys[j] != hashRamKey[j])
                    {
                        hashRamHit = FALSE;
                        break;
                    }
                }
            }

            // bucket is invalid when MASK == 0 or ramDataOk == 0
            if((bucket.MASK == 0) || (ramDataOk == 0))
            {
                hashRamHit = FALSE;
            }

            if(hashRamHit)
            {
                isCam = 0;
                WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Reading hash ram actions\n")
                GetHashActions(hashRamKey,
                               hashCfg.mode, 
                               keySize, 
                               hashCfg.entry_size[hashNum]*4,
                               isCam, 
                               actions);
            }
            else
            {
                // Get actions from FFU_HASH_MISS register since both cam and
                // ram missed
                WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Reading hash miss actions\n")
                regPtr = FM_MODEL_GET_REG_PTR(model, 
                                         HLP_FFU_HASH_MISS(group,
                                         hashNum, state->FFU_SCENARIO, 0));
                actions[0] = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_MISS, ACTION0);
                actions[1] = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_MISS, ACTION1);

                if(hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_64B)
                {
                    regPtr = FM_MODEL_GET_REG_PTR(model, 
                                             HLP_FFU_HASH_MISS(group,
                                             1, state->FFU_SCENARIO, 0));
                    actions[2] = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_MISS, ACTION0);
                    actions[3] = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_MISS, ACTION1);
                }
                //regPtr = FM_MODEL_GET_REG_PTR(model, 
                //                         HLP_FFU_HASH_MISS1(group,
                //                         hashNum, state->FFU_SCENARIO, 0));
                //actions[2] = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_MISS1, ACTION0);
                //actions[3] = FM_ARRAY_GET_FIELD(regPtr, HLP_FFU_HASH_MISS1, ACTION1);
            }

        } //hashCamEn

        if(hashCfg.mode == HLP_MODEL_FFU_HASH_ENTRY_MODE_32B)
        {
            for(i = 0; i < HLP_MODEL_FFU_MAX_HASH_ACTIONS/2; i++)
            {
                 state->FGHASH_ACTIONS[group].actionValid
                    [(HLP_MODEL_FFU_MAX_HASH_ACTIONS/2)*hashNum + i] = 1;
                 state->FGHASH_ACTIONS[group].action
                    [(HLP_MODEL_FFU_MAX_HASH_ACTIONS/2)*hashNum + i] =
                    actions[i];
            }
        }
        else
        {
            for(i = 0; i < HLP_MODEL_FFU_MAX_HASH_ACTIONS; i++)
            {
                state->FGHASH_ACTIONS[group].actionValid[i] = 1;
                state->FGHASH_ACTIONS[group].action[i] = actions[i];
            }
        }

        /* loop for actions */
        for (i = 0; i < HLP_MODEL_FFU_MAX_HASH_ACTIONS ; i++)
        {
            //WM_DISPLAY3(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "fghash_action_valid[%0d] = %0d\n", i, state->FGHASH_ACTIONS[group].actionValid[i])
            status = DoAction(model, actions[i]); 
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
        }

        ///* Read Data and Color from Hash Ram */
        //regPtr = FM_MODEL_GET_REG_PTR(model, 
        //                              HLP_HASH_DATA(hashNum, hashRamAddr, 0));
        //chunks[hashNum*4 + 0] = FM_ARRAY_GET_FIELD64(regPtr, 
        //                                             HLP_HASH_DATA, 
        //                                             Chunk0);
        //chunks[hashNum*4 + 1] = FM_ARRAY_GET_FIELD64(regPtr, 
        //                                             HLP_HASH_DATA, 
        //                                             Chunk1);
        //chunks[hashNum*4 + 2] = FM_ARRAY_GET_FIELD64(regPtr, 
        //                                             HLP_HASH_DATA, 
        //                                             Chunk2);
        //chunks[hashNum*4 + 3] = FM_ARRAY_GET_FIELD64(regPtr, 
        //                                             HLP_HASH_DATA, 
        //                                             Chunk3);
        //
        //regPtr = FM_MODEL_GET_REG_PTR(model, 
        //                              HLP_HASH_COLOR(hashNum, hashRamAddr, 0));
        //keyColor[hashNum*4 + 0] = FM_GET_FIELD(*regPtr, 
        //                                       HLP_HASH_COLOR, 
        //                                       Color01);
        //keyColor[hashNum*4 + 1] = keyColor[hashNum*4 + 0];

        //keyColor[hashNum*4 + 2] = FM_GET_FIELD(*regPtr, 
        //                                       HLP_HASH_COLOR, 
        //                                       Color23);
        //keyColor[hashNum*4 + 3] = keyColor[hashNum*4 + 2];
        
    }

    
    ///* Chunk wise Key and key color Comparison */
    //keyMatch = 0;
    //for (chunk = 0; chunk < HLP_MODEL_FFU_N_HASH_CHUNKS; chunk++)
    //{
    //    if (compactKeys[chunk] == chunks[chunk] &&
    //        rowCfg[hashNum].EntryColor == keyColor[chunk])
    //    {
    //        FM_SET_UNNAMED_FIELD(keyMatch, chunk, 1, 1);
    //    }
    //}

    ///* Derive actionMask based on EntrySize, keyMatch, keyMask and keySize */
    //actionMask = 0;
    //if (rowCfg[0].EntrySize == HLP_MODEL_FFU_HASH_ENTRY_SIZE_64B)
    //{
    //    keyMask = (1 << keySize[0]) - 1;
    //    actionMask = (keyMatch | ~keyMask) ? ~keyMask : 0;
    //}
    //else
    //{
    //    /* Bank 0..1 */
    //    switch(rowCfg[0].EntrySize)
    //    {
    //        case HLP_MODEL_FFU_HASH_ENTRY_SIZE_8B:
    //            FM_SET_UNNAMED_FIELD(actionMask, 
    //                                 0, 
    //                                 8, 
    //                                 (keyMatch & 0xFF) & 0xAA); /* b10101010 */
    //            break;
    //        case HLP_MODEL_FFU_HASH_ENTRY_SIZE_16B:
    //            /* Bank 0 */
    //            keyMask = (1 << keySize[0]) - 1;
    //            FM_SET_UNNAMED_FIELD(
    //                actionMask, 
    //                0, 
    //                4, 
    //                ((keyMatch & 0xF) | ((~keyMask) & 0xF)) ? ((~keyMask) & 0xF) : 0);
    //            
    //            /* bank 1 */
    //            keyMask = (1 << keySize[1]) - 1;
    //            FM_SET_UNNAMED_FIELD(
    //                actionMask, 
    //                4, 
    //                8, 
    //                (((keyMatch >> 4) & 0xF) | ((~keyMask) & 0xF)) ? ((~keyMask) & 0xF) : 0);
    //            
    //            break;
    //        case HLP_MODEL_FFU_HASH_ENTRY_SIZE_32B:
    //            keyMask = (1 << keySize[0]) - 1;
    //            FM_SET_UNNAMED_FIELD(
    //                actionMask, 
    //                0, 
    //                8, 
    //                ((keyMatch & 0xFF) | ((~keyMask) & 0xFF)) ? ((~keyMask) & 0xFF) : 0);
    //            break;
    //        default:
    //            actionMask = actionMask & 0xFF00;
    //            break;
    //    }

    //    /* Bank 2..3 */
    //    switch(rowCfg[2].EntrySize)
    //    {
    //        case HLP_MODEL_FFU_HASH_ENTRY_SIZE_8B:
    //            FM_SET_UNNAMED_FIELD(actionMask, 8, 8, ((keyMatch >> 8) & 0xFF) & 0xAA); /* b10101010 */
    //            break;
    //        case HLP_MODEL_FFU_HASH_ENTRY_SIZE_16B:
    //            /* Bank 2 */
    //            keyMask = (1 << keySize[2]) - 1;
    //            FM_SET_UNNAMED_FIELD(
    //                actionMask, 
    //                8, 
    //                4, 
    //                (((keyMatch >> 8) & 0xF) | ((~keyMask) & 0xF)) ? ((~keyMask) & 0xF) : 0);
    //            
    //            /* Bank 3 */
    //            keyMask = (1 << keySize[3]) - 1;
    //            FM_SET_UNNAMED_FIELD(
    //                actionMask, 
    //                12, 
    //                4, 
    //                (((keyMatch >> 12) & 0xF) | ((~keyMask) & 0xF)) ? ((~keyMask) & 0xF) : 0);
    //            
    //            break;
    //        case HLP_MODEL_FFU_HASH_ENTRY_SIZE_32B:
    //            keyMask = (1 << keySize[2]) - 1;
    //            FM_SET_UNNAMED_FIELD(
    //                actionMask, 
    //                8, 
    //                8, 
    //                (((keyMatch >> 8) & 0xFF) | ((~keyMask) & 0xFF)) ? ((~keyMask) & 0xFF) : 0);
    //            break;
    //        default:
    //            actionMask = actionMask & 0x00FF;
    //            break;
    //    }
    //}

    ///* Apply Actions */
    ///* Start from chunk 1 since chunk 0 is always a key */
    //for (chunk = 1; chunk < HLP_MODEL_FFU_N_HASH_CHUNKS; chunk++)
    //{
    //    if ((actionMask >> chunk) & 1)
    //    {
    //        status = DoAction(model, chunks[chunk]); 
    //        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    //    }
    //}
ABORT:
    return status;

}   /* end FGHash */


/*****************************************************************************/
/** Remap 
 * \ingroup intModel
 *
 * \desc            Remaps a subset of keys going into next FFU Group.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status Remap(hlp_model    *model)
{
    hlp_modelState          *state;
    hlp_modelFfuKeys        *keys;
    hlp_modelFfuActions     *actions;
    fm_status               status = FM_OK;
    fm_uint32               remapAction;
    fm_uint16               value16;
    fm_byte                 value8;
    fm_byte                 mask;
    fm_int                  i;
    fm_int                  j;
    fm_byte                 key;
    fm_byte                 index[HLP_MODEL_FFU_N_REMAP_ACTIONS];
    fm_uint                 keyIdx;
    fm_byte                 idx16;
    fm_uint                 set1_16_base;

    state   = &model->packetState;
    keys    = &state->FFU_KEYS;
    actions = &state->FFU_ACTIONS;
    
    /* Resolve precedence between remap actions*/
    
    for (i = 0; i < HLP_MODEL_FFU_N_REMAP_ACTIONS; i++)
    {    
        index[i] = FM_GET_FIELD(actions->act24[HLP_MODEL_FFU_ACTION_REMAP0+i].val, 
                                HLP_MODEL_FFU_REMAP, 
                                SET1_16B_INDEX);
    }
    
    for (i = 0; i < HLP_MODEL_FFU_N_REMAP_ACTIONS; i++)
    {    
        if (actions->act24[HLP_MODEL_FFU_ACTION_REMAP0+i].prec == 0) 
        {
            actions->act24[HLP_MODEL_FFU_ACTION_REMAP0+i].val = 0;
            continue;
        }
        remapAction = actions->act24[HLP_MODEL_FFU_ACTION_REMAP0+i].val;
        set1_16_base = HLP_MODEL_FFU_N_KEY32*4 + HLP_MODEL_FFU_N_KEY16*2 + HLP_MODEL_FFU_N_KEY8;
        /* SET1_16b */
        if(index[i] >= (set1_16_base))
        {
            idx16 = index[i] - set1_16_base;
            value16 = FM_GET_FIELD(remapAction, 
                                   HLP_MODEL_FFU_REMAP, 
                                   SET1_16B_VALUE);
            
            WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Remap Action: SET1_16b. act24=0x%x index=%d, value16=0x%x idx16=%d\n", remapAction, index[i], value16, idx16)


            if (idx16 < HLP_MODEL_FFU_N_KEY16)
            {
                keys->key16[idx16] = value16;;
            }   
            else
            {
                /* set 16-bits of KEY32 */
                keyIdx = (idx16 - HLP_MODEL_FFU_N_KEY16) >> 1;
                FM_SET_UNNAMED_FIELD(keys->key32[keyIdx], 
                                     (idx16 % 2) * 16, 
                                     16, 
                                     value16);
            }
        }
        /* SET8_1b */
        else 
        {
            
            mask = FM_GET_FIELD(remapAction, 
                                HLP_MODEL_FFU_REMAP, 
                                SET8_1B_MASK);
            
            if (mask != 0)
            {    
                //WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Remap Action: SET8_1b\n")
                
                value8 = FM_GET_FIELD(remapAction, 
                                      HLP_MODEL_FFU_REMAP, 
                                      SET8_1B_VALUE);

                WM_DISPLAY2(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Remap Action: SET8_1b. act24=0x%x index=%d, value8=0x%x mask=0x%x\n", remapAction, index[i], value8, mask)
                
                
                if (index[i] < HLP_MODEL_FFU_N_KEY16*2)
                {
                    /* set 8-bits of KEY16 */
                    key = FM_GET_UNNAMED_FIELD(keys->key16[index[i] >> 1],
                                               (index[i] % 2) * 8,
                                               8);
                    key = ( key & ~mask ) | ( value8 & mask);

                    FM_SET_UNNAMED_FIELD(keys->key16[index[i] >> 1],
                                         (index[i] % 2) * 8,
                                         8,
                                         key); 
                }   
                else if (index[i] < (HLP_MODEL_FFU_N_KEY16*2 + HLP_MODEL_FFU_N_KEY8))
                {
                    /* set entire KEY8 */
                    key = keys->key8[index[i] - (HLP_MODEL_FFU_N_KEY16*2)];

                    key = ( key & ~mask ) | ( value8 & mask);

                    keys->key8[index[i] - (HLP_MODEL_FFU_N_KEY16*2)]  = key;
                }
                else
                {
                    /* set 8-bits of KEY32 */
                    keyIdx = (index[i] - (HLP_MODEL_FFU_N_KEY16*2 +
                                HLP_MODEL_FFU_N_KEY8)) >> 2;
                    key = FM_GET_UNNAMED_FIELD(keys->key32[keyIdx],
                                              (index[i] % 4) * 8,
                                              8);
                    key = ( key & ~mask ) | ( value8 & mask);
                    
                    FM_SET_UNNAMED_FIELD(keys->key32[keyIdx], 
                                        (index[i] % 4) * 8, 
                                         8, 
                                         key);
                }

            }
            
        }

        /* After remap action is applied to keys, zero out the remap action so that next group
         * doesnot take same remap action */
        actions->act24[HLP_MODEL_FFU_ACTION_REMAP0+i].val = 0;
        actions->act24[HLP_MODEL_FFU_ACTION_REMAP0+i].prec = 1;
    }   
ABORT:
    return status;

}   /* end Remap */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** hlpModelApplyKeyMask 
 * \ingroup intModel
 *
 * \desc            Applies keyMask on FFU KEYS to determine HASH KEYS used to
 *                  calcualte hash
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       keyMaskCfg points to the caller allocated storage for
 *                  KeyMask configuation(FFU_KEY_MASK)
 *
 * \param[in,out]   hashKeys is the caller allocated storage which is populated
 *                  with calculated hash keys
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
 fm_status hlpModelApplyKeyMask(hlp_model               *model,
                                hlp_modelFfuKeyMaskCfg   keyMaskCfg,
                                hlp_modelFfuKeys        *hashKeys)
{
    hlp_modelState      *state;
    hlp_modelFfuKeys    *ffuKeys;
    fm_status           status = FM_OK;
    fm_int              i;
    fm_int              j;

    state = &model->packetState;
    ffuKeys = &state->FFU_KEYS;
    
    /* Apply Key Mask */
    /* KEY16 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        if (FM_GET_UNNAMED_FIELD(keyMaskCfg.Key16Mask, i, 1))
        {
            hashKeys->key16[i] = ffuKeys->key16[i];
        }
        else
        {
            hashKeys->key16[i] = 0;
        }
    }
    /* KEY8 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY8; i++)
    {
        if (FM_GET_UNNAMED_FIELD64(keyMaskCfg.Key8Mask, i, 1))
        {
            hashKeys->key8[i] = ffuKeys->key8[i];;
        }
        else
        {
            hashKeys->key8[i] = 0;
        }
    }
    /* KEY32 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY32; i++)
    {
        if (FM_GET_UNNAMED_FIELD(keyMaskCfg.Key32Mask, i, 1))
        {
            hashKeys->key32[i] = ffuKeys->key32[i];;
        }
        else
        {
            hashKeys->key32[i] = 0;
        }
    }
    
    /* Apply keySubmask */
    for (i = 0; i < 2; i++)
    {
        switch (keyMaskCfg.KeySubmode[i])
        {
            case HLP_MODEL_BIT_MASK_MPLS0:
                hashKeys->key16[24] &= keyMaskCfg.KeySubmask[i] & 0xFFFF;
                hashKeys->key16[25] &= (keyMaskCfg.KeySubmask[i] >> 16) & 0xFFFF;
                break;
            case HLP_MODEL_BIT_MASK_MPLS1:
                hashKeys->key16[26] &= keyMaskCfg.KeySubmask[i] & 0xFFFF;
                hashKeys->key16[27] &= (keyMaskCfg.KeySubmask[i] >> 16) & 0xFFFF;
                break;
            case HLP_MODEL_NIBBLE_MASK_MPLS:
                for (j = 0; j < 32; j++)
                {
                    if (!((keyMaskCfg.KeySubmask[i] >> j) & 1))
                    {
                        FM_SET_UNNAMED_FIELD(hashKeys->key16[24+(j>>2)], 
                                             (j%4)*4, 
                                             4, 
                                             0);
                    }    
                }
                break;
            case HLP_MODEL_NIBBLE_MASK_IP:
                for (j = 0; j < 32; j++)
                {
                    if (!((keyMaskCfg.KeySubmask[i] >> j) & 1))
                    {
                        FM_SET_UNNAMED_FIELD(hashKeys->key32[(j>>3)], 
                                             (j%8)*4, 
                                             4, 
                                             0);
                    }    
                }
                break;
            default:
                break;

        }
    }
    
ABORT:
    return status;

}   /* end hlpModelApplyKeyMask */

/*****************************************************************************/
/** hlpModelConvertKeysToBytes 
 * \ingroup intModel
 *
 * \desc            Converts FFU KEYS into array of bytes. 
 *
 * \param[in]       ffuKeys is the input struct that provides FFU keys to be
 *                  converted to bytes
 *
 * \param[in,out]   keyBytes is the caller allocated storage which is populated
 *                  with array of bytes
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelConvertKeysToBytes(hlp_modelFfuKeys   ffuKeys,
                                     fm_byte            *keyBytes)
{
    
    fm_status   status = FM_OK;
    fm_int      i;
    fm_int      j;
    
    /* Convert FFU Keys into array of bytes */ 
    /* KEY16 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        keyBytes[2*i + 1] = ffuKeys.key16[i] & 0xFF;
        keyBytes[2*i] = (ffuKeys.key16[i] >> 8) & 0xFF;
    }
    /* KEY8 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY8; i++)
    {
        keyBytes[HLP_MODEL_FFU_N_KEY16*2 + i] = ffuKeys.key8[i];
    }
    /* KEY32 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY32; i++)
    {
        for( j = 0; j < 4; j++)
        {
            keyBytes[HLP_MODEL_FFU_N_KEY16*2 + HLP_MODEL_FFU_N_KEY8 + i*4 + (3-j)] = 
                (ffuKeys.key32[i] >> (j * 8 )) & 0xFF;
        } 
    }
       
ABORT:
    return status;

}   /* end hlpModelConvertKeysToBytes */


/*****************************************************************************/
/** hlpModelFfuClassifier
 * \ingroup intModel
 *
 * \desc            Models the Classifier stage of Filtering and Forwarding Unit
 *                  (FFU).
 *
 * \param[in]       model points to the switch data structure.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelFfuClassifier(hlp_model *model)
{
    hlp_modelState          *state;
    //hlpFfuTcamCfg           tcamCfg;
    //hlp_modelLookupInfo     info;
    fm_status               status = FM_OK;
    fm_byte                 group;
    //fm_byte                 slice;
    hlp_modelFfuHitInfo     tcamHitInfo[HLP_FFU_TCAM_ENTRIES_1];
    fm_int                  i;
    
    if(testPlusArgs("HLP_FFU_CLASSIFIER_WM_PRINT_VERBOSE") >= 0)
        fgrpDisplayVerbose = testPlusArgs("HLP_FFU_CLASSIFIER_WM_PRINT_VERBOSE");
    WM_DISPLAY(fgrpDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "fgrpDisplayVerbose=%0d\n", fgrpDisplayVerbose)


    state = &model->packetState;

    for (group = 0 ; group < HLP_FFU_TCAM_CFG_ENTRIES_2 ; group++)
    {    
        FM_CLEAR(tcamHitInfo);

        /* Get hit index for each tcam slice */
        CascadeLookupTcam(model, group, tcamHitInfo);
        
        /* Apply and resolve actions from actionRams based on tcam hit index per
         * slice */
        status = ActionResolution(model, group, tcamHitInfo);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
        
        status = FGHash(model, group);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);  

        /* Remap subset of keys going into next group */
        status = Remap(model);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        /*  Update scenario based on scenario action*/
        for(int i = 0; i < 6; i++) 
        {
            if (state->FFU_ACTIONS.act1[HLP_MODEL_FFU_ACTION_SCENARIO0+i].prec != 0)
            {    
                FM_SET_UNNAMED_FIELD(
                        state->FFU_SCENARIO,
                        i,
                        1,
                        (state->FFU_ACTIONS.act1[HLP_MODEL_FFU_ACTION_SCENARIO0+i].val & 0x1)
                        );
            }     
        }                
        /* Assign FFU Group outputs */
        if (group < (HLP_FFU_TCAM_ENTRIES_2-1))
        {
            state->FFU_GRP_KEYS[group] = state->FFU_KEYS;
            state->FFU_GRP_ACTIONS[group] = state->FFU_ACTIONS;
            state->FFU_GRP_SCENARIO[group] = state->FFU_SCENARIO;
        }
    }
     
    
ABORT:
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpFfuClassifier);

    return status;

}   /* end hlpModelFfuClassifier */




/*****************************************************************************/
/** hlpModelFfuClassifierWriteCSR
 * \ingroup intModel
 *
 * \desc            Stores values of frequently used registers pertinent to this
 *                  this module in local data structures for higher performance.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the address of the register.
 *
 * \param[in]       newValue is the new value of the register.
 *
 * \param[in]       init is a boolean indicating whether this write operation
 *                  is trying to initialize the register related to white model
 *                  cached state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelFfuClassifierWriteCSR(hlp_model     *model,
                                        fm_uint32     addr,
                                        fm_uint32     newValue,
                                        fm_bool       init)
{
    hlp_modelFfuTcam        *ffuTcam;
    fm_status               err = FM_OK;
    fm_uint32               value[4];
    fm_int                  index;
    fm_int                  index0;
    fm_int                  index1;
    fm_int                  index2;
    fm_int                  word;

    FM_NOT_USED(newValue);

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM,
                                "model=%p addr=0x%x newValue=0x%x\n",
                                (void *) model,
                                addr,
                                newValue);
    
    if ( FM_MODEL_IN_RANGE_MULT_3(addr, HLP_FFU_TCAM) &&
         FFU_GROUP_IN_RANGE_MULT_3(addr, HLP_FFU_TCAM))
    {
        
        err = FM_MODEL_GET_OFFSET_MULT_3(addr,
                                         HLP_FFU_TCAM,
                                         &index2,
                                         &index1,
                                         &index0,
                                         &word);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
        
        if ( word != (HLP_FFU_TCAM_WIDTH - 1) )
        {
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
        }
        
        err = hlpModelReadCSRMult(model->sw,
                                  HLP_FFU_TCAM(index2, index1, index0, 0),
                                  HLP_FFU_TCAM_WIDTH,
                                  value);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        ffuTcam = &model->FFU_TCAM[index2][index1][index0];
        ffuTcam->Key = FM_ARRAY_GET_FIELD64(value, HLP_FFU_TCAM, KEY);
        ffuTcam->Key |= FM_ARRAY_GET_FIELD64(value, HLP_FFU_TCAM, KEY_TOP) << 32;
        ffuTcam->KeyInvert = FM_ARRAY_GET_FIELD64(value, HLP_FFU_TCAM, KEY_INVERT);
        ffuTcam->KeyInvert |= FM_ARRAY_GET_FIELD64(value, 
                                                   HLP_FFU_TCAM, 
                                                   KEY_TOP_INVERT) << 32;
        if(init)
        {
            /* Mark Cam entries invalid during reset */
            ffuTcam->Valid = 0;
        }
        else
        {
            /* TCAM entry becomes valid on first write to either Key or
             * KeyInvert */
            ffuTcam->Valid = 1;
        }
    }

ABORT:
HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, err);

} /* end hlpModelFfuClassifierWriteCSR */

