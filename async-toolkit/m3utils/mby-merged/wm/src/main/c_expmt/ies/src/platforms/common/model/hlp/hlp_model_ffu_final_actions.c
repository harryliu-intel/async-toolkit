/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_ffu_final_actions.c
 * Creation Date:   August 14, 2014
 * Description:     FFU final actions stage of HLP white model
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
fm_int     ffuFinalDisplayVerbose = 1;    /* 0=always prints; 1=ip & ulv prints; 
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

static fm_macaddr ExtractDmac(fm_uint32 ipLo, fm_uint32 ipHi)
{
    return (((fm_macaddr) (ipLo & 0xFFFFFF)) |
            (((fm_macaddr) (ipHi & 0xFDFFFF00)) << 16) |
            (((fm_macaddr) (~ipHi & 0x2000000)) << 16));
}

/*****************************************************************************/
/** GetEntropyCfg 
 * \ingroup intModel
 *
 * \desc            Reads FFU_ENTROPY_CFG[0,1] register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       hashNum is the hash number for which entropy cfg needs to be
 *                  read
 *
 * \param[in]       hashProf is the hash profile number for which entropy cfg
 *                  data needs to be read                  
 *
 * \param[in,out]   entropyCfg is the combined struct for FFU_ENTROPY_CFG[0,1] that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetEntropyCfg(hlp_model                 *model,
                          fm_byte                   hashNum,
                          fm_byte                   hashProf,
                          hlp_modelFfuKeyMaskCfg    *entropyCfg)
{
    fm_uint32   *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTROPY_HASH_CFG0(hashNum, 
                                                               hashProf, 
                                                               0));
    entropyCfg->Key8Mask  = 
        FM_ARRAY_GET_FIELD64(regPtr, HLP_ENTROPY_HASH_CFG0, KEY_MASK8);

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTROPY_HASH_CFG1(hashNum, 
                                                               hashProf, 
                                                               0));
    entropyCfg->Key16Mask = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_ENTROPY_HASH_CFG1, KEY_MASK16);
    entropyCfg->Key32Mask = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_ENTROPY_HASH_CFG1, KEY_MASK32);
    entropyCfg->KeySubmode[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_ENTROPY_HASH_CFG1, KEY_SUBMODE0);
    entropyCfg->KeySubmode[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_ENTROPY_HASH_CFG1, KEY_SUBMODE1);
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTROPY_HASH_CFG2(hashNum, 
                                                               hashProf, 
                                                               0));
    entropyCfg->KeySubmask[0] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_ENTROPY_HASH_CFG2, KEY_SUBMASK0);
    entropyCfg->KeySubmask[1] = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_ENTROPY_HASH_CFG2, KEY_SUBMASK1);     
    
    WM_DISPLAY3(ffuFinalDisplayVerbose, FM_LOG_CAT_MODEL_FFU,"key8Mask = 0x%llx key16Mask=0x%x, key32Mask=0x%x\n", entropyCfg->Key8Mask, entropyCfg->Key16Mask, entropyCfg->Key32Mask)
}

/*****************************************************************************/
/** GetEntropyMetaCfg 
 * \ingroup intModel
 *
 * \desc            Reads FFU_ENTROPY_META_CFG register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       hashProf is the hash profile number for which meta cfg
 *                  data needs to be read    
 *
 * \param[in,out]   metaCfg is the struct for FFU_ENTROPY_META_CFG[0,1] that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetEntropyMetaCfg(hlp_model         *model,
                              fm_byte           hashProf,
                              hlpEntropyMetaCfg *metaCfg)
{
    fm_uint32   *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_ENTROPY_META_CFG(hashProf, 0));

    metaCfg->BYTE_DEFAULTS = 
        FM_GET_FIELD(*regPtr, HLP_ENTROPY_META_CFG, BYTE_DEFAULTS);
    metaCfg->HASH_START = FM_GET_FIELD(*regPtr, HLP_ENTROPY_META_CFG, HASH_START);
    metaCfg->HASH_SIZE = FM_GET_FIELD(*regPtr, HLP_ENTROPY_META_CFG, HASH_SIZE);
    
} /* GetEntropyMetaCfg */

/*****************************************************************************/
/** MplsMux 
 * \ingroup intModel
 *
 * \desc            Decodes FFU ACTIONS ECN-CTRL, TC_CTRL, TTL_CTRL and
 *                  DSCP_CTRL and populates FFU_MUXED_ACTION.
 *
 * \param[in]       model points to the switch data structure.
 *
 *****************************************************************************/
static void MuxedAction(hlp_model           *model) 
{
    hlp_modelState          *state;
    hlp_modelFfuActions     ffuActions; 
    hlp_modelFfuKeys        ffuKeys;
    hlp_modelFfuMuxedAction *muxedAction;
    fm_byte                 ttl;
    fm_byte                 mplsPop;
    fm_byte                 ecn_ctrl;
    fm_byte                 tc_ctrl;
    fm_byte                 ttl_ctrl;
    fm_byte                 dscp_ctrl;
    fm_byte                 exp;
    fm_byte                 dscp;
    fm_byte                 priority_profile;
    fm_uint32               *regPtr;

    state = &model->packetState;
    ffuKeys = state->FFU_KEYS;
    ffuActions = state->FFU_ACTIONS;
    muxedAction = &state->FFU_MUXED_ACTION;
    priority_profile = state->PRIORITY_PROFILE;
    mplsPop = ffuActions.act4[HLP_MODEL_FFU_ACTION_MPLS_POP].val;
    ecn_ctrl = ffuActions.act4[HLP_MODEL_FFU_ACTION_ECN_CTRL].val;
    tc_ctrl = ffuActions.act4[HLP_MODEL_FFU_ACTION_TC_CTRL].val;
    ttl_ctrl = ffuActions.act4[HLP_MODEL_FFU_ACTION_TTL_CTRL].val;
    dscp_ctrl = ffuActions.act4[HLP_MODEL_FFU_ACTION_DSCP_CTRL].val; 
    exp = 0;
    dscp = 0;

    muxedAction->ecn = 0;
    muxedAction->aqm_mark_en = 0;

    /* FFU directly specifies ECN value */
    if (ecn_ctrl < 4)
    {
        muxedAction->ecn = ecn_ctrl;
        muxedAction->aqm_mark_en = 0;
        if (ecn_ctrl == 2)
        {    
            muxedAction->ecn = 1;     
            muxedAction->aqm_mark_en = 1;
        
        }
    } 
    /* Define ECN source and marking rules */
    else if (ecn_ctrl < 16)
    {
        /* ECN */ 
        switch (ecn_ctrl & 3)
        {
            /* ECN-CTRL[1:0] = 0 : ECN source is outer IP DS.ECN */
            case 0:
                muxedAction->ecn = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key8[HLP_MODEL_KEY8_OUTER_DS], 
                                    0, 
                                    2);
                break;
            /* ECN-CTRL[1:0] = 1 : ECN source is  Inner IP DS.ECN */    
            case 1:
                muxedAction->ecn = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key8[HLP_MODEL_KEY8_INNER_DS], 
                                    0, 
                                    2);
                break;
            /* ECN-CTRL[1:0] = 2 : ECN source is MPLS label 1, i.e.
             * MPLS_MUX_EXP_DS[mpls_labels[0].exp].ecn */
            case 2:      
                exp = FM_GET_UNNAMED_FIELD(
                                ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1], 
                                9, 
                                3);
                regPtr = FM_MODEL_GET_REG_PTR(model, 
                                HLP_MPLS_MUX_EXP_DS(((priority_profile << 3) | exp), 0));
                
                muxedAction->ecn = FM_ARRAY_GET_FIELD(regPtr, HLP_MPLS_MUX_EXP_DS, ECN);
                break;
            /* ECN_CTRL[1:0]=3: ECN source is MPLS label exposed after MPLS_POP,
             * i.e. MPLS_MUX_EXP_DS[mpls_labels[mpls_pop_index].exp].ecn */
            case 3:
                /* MPLS Labels 1..4 are in Key8 */
                if(mplsPop < 4)
                {
                    exp = FM_GET_UNNAMED_FIELD(
                            ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1 + mplsPop*2],
                            9,
                            3);
                }
                /* MPLS Labels 5 & 6 are in Key8 */
                else if (mplsPop < 6)
                {
                    exp = FM_GET_UNNAMED_FIELD(
                            ffuKeys.key8[HLP_MODEL_KEY8_MPLS_LABEL5_2 + (mplsPop-4)*4],
                            1,
                            3);
                }
                regPtr = FM_MODEL_GET_REG_PTR(model, 
                                              HLP_MPLS_MUX_EXP_DS(((priority_profile << 3) | exp), 0));
                
                muxedAction->ecn = FM_ARRAY_GET_FIELD(regPtr, HLP_MPLS_MUX_EXP_DS, ECN);
                break;
            default :
                muxedAction->ecn = 0;
                break;
        }

        /*  ECN_CTRL[3:2]=3: mark in FFU if ingress ECN is 01 or 10 (aqm_mark_en=0) */
        if ((((ecn_ctrl >> 2) & 3) == 3) && (muxedAction->ecn == 1 || muxedAction->ecn == 2))
        {
            muxedAction->ecn = 3;
        }
        /* ECN marking */
        if (((ecn_ctrl >> 2) & 3) == 2)
        {
            muxedAction->aqm_mark_en = 1;
        }
    }
    
    muxedAction->dscp = 0;
    /* Update DSCP based on DSCP_CTRL */
    switch(dscp_ctrl)
    {
        /* 0 = Use DSCP Action */
        case 0:
            muxedAction->dscp = 
                (ffuActions.act4[HLP_MODEL_FFU_ACTION_DSCP_LOW].val & 0xF) |
                ((ffuActions.act4[HLP_MODEL_FFU_ACTION_DSCP_HIGH].val & 0xF) << 4);
            break;
        /* 4 = Use Ingress outer IP DSCP field */
        case 4:
            muxedAction->dscp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key8[HLP_MODEL_KEY8_OUTER_DS], 
                                    2, 
                                    6);
            break;
        /* 5 = Use Ingress Inner IP DSCP field */    
        case 5: 
            muxedAction->dscp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key8[HLP_MODEL_KEY8_INNER_DS], 
                                    2, 
                                    6);
            break;
        /* 6 = Use EXP value pointed to by MPLS_POP */    
        case 6:
            /* MPLS Labels 1..4 are in Key8 */
            if(mplsPop < 4)
            {
                exp = FM_GET_UNNAMED_FIELD(
                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1 + mplsPop*2],
                    9,
                    3);
            }
            /* MPLS Labels 5 & 6 are in Key8 */
            else if (mplsPop < 6)
            {
                exp = FM_GET_UNNAMED_FIELD(
                    ffuKeys.key8[HLP_MODEL_KEY8_MPLS_LABEL5_2 + (mplsPop-4)*4],
                    1,
                    3);
            }
            break;
        /* 8..11 = Use MPLS EXP from label 1..4 */    
        case 8:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1], 
                                    9, 
                                    3);
            break;
        case 9:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL2_1], 
                                    9, 
                                    3);
            break;
        case 10:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL3_1], 
                                    9, 
                                    3);
            break;
        case 11:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL4_1], 
                                    9, 
                                    3);
            break;
        default:
            muxedAction->dscp = 0;
            break;

    }
    if(dscp_ctrl == 6 || (dscp_ctrl >=8 && dscp_ctrl <= 11))
    {
        regPtr = FM_MODEL_GET_REG_PTR(model, 
                     HLP_MPLS_MUX_EXP_DS(((priority_profile << 3) | exp), 0));
            
        muxedAction->dscp = FM_ARRAY_GET_FIELD(regPtr, HLP_MPLS_MUX_EXP_DS, DSCP);
    }

    muxedAction->swpri = 0;
    /* Update SWPRI based on TC_CTL Action */
    switch(tc_ctrl)
    {
        /* 0 = Use SWPRI Action */
        case 0:
            muxedAction->swpri = ffuActions.act4[HLP_MODEL_FFU_ACTION_TC].val;
            break;
        /* 4 = Use Ingress outer IP DSCP field */
        case 4:
            dscp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key8[HLP_MODEL_KEY8_OUTER_DS], 
                                    2, 
                                    6);
            regPtr = FM_MODEL_GET_REG_PTR(model, 
                                      HLP_MPLS_MUX_DSCP_TC(dscp, 0));
            
            muxedAction->swpri = FM_ARRAY_GET_FIELD(regPtr, HLP_MPLS_MUX_DSCP_TC, TC);

            break;
        /* 5 = Use Ingress Inner IP DSCP field */    
        case 5: 
            dscp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key8[HLP_MODEL_KEY8_INNER_DS], 
                                    2, 
                                    6);
            regPtr = FM_MODEL_GET_REG_PTR(model, 
                                      HLP_MPLS_MUX_DSCP_TC(dscp, 0));
            
            muxedAction->swpri = FM_ARRAY_GET_FIELD(regPtr, HLP_MPLS_MUX_DSCP_TC, TC);
            
            break;
        /* 6 = Use EXP value pointed to by MPLS_POP */    
        case 6:
            /* MPLS Labels 1..4 are in Key16 */
            if(mplsPop < 4)
            {
                exp = FM_GET_UNNAMED_FIELD(
                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1 + mplsPop*2],
                    9,
                    3);
            }
            /* MPLS Labels 5 & 6 are in Key8 */
            else if (mplsPop < 6)
            {
                exp = FM_GET_UNNAMED_FIELD(
                    ffuKeys.key8[HLP_MODEL_KEY8_MPLS_LABEL5_2 + (mplsPop-4)*4],
                    1,
                    3);
            }
            break;
        /* 8..11 = Use MPLS EXP from label 1..4 */    
        case 8:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1], 
                                    9, 
                                    3);
            break;
        case 9:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL2_1], 
                                    9, 
                                    3);
            break;
        case 10:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL3_1], 
                                    9, 
                                    3);
            break;
        case 11:
            exp = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL4_1], 
                                    9, 
                                    3);
            break;
            
        default:
            muxedAction->swpri = 0;
            break;
    }
    if(tc_ctrl == 6 || (tc_ctrl >= 8 && tc_ctrl <= 11))
    {
        //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "tc_ctrl: 0x%x, exp = 0x%x\n", tc_ctrl, exp);

        regPtr = FM_MODEL_GET_REG_PTR(model, 
                     HLP_MPLS_MUX_EXP_DS(((priority_profile << 3) | exp), 0));
            
        muxedAction->swpri = FM_ARRAY_GET_FIELD(regPtr, HLP_MPLS_MUX_EXP_DS, TC);
    }

    /* get TTL value based on TTL_CTRL Action */
    switch(ttl_ctrl)
    {
        /* 0 = Use Ingress outer IP TTL field */
        case 0:
            ttl = ffuKeys.key8[HLP_MODEL_KEY8_OUTER_TTL];
            break;
        /* 1 = Use Ingress Inner IP TTL field */    
        case 1: 
            ttl = ffuKeys.key8[HLP_MODEL_KEY8_INNER_TTL];
            break;
        /* 2 = Use TTL value pointed to by MPLS_POP */    
        case 2:
            /* MPLS Labels 1..4 are in Key8 */
            if(mplsPop < 4)
            {
                ttl = FM_GET_UNNAMED_FIELD(
                        ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1 + mplsPop*2],
                        0,
                        8);
            }
            /* MPLS Labels 5 & 6 are in Key8 */
            else if (mplsPop < 6)
            {
                ttl = ffuKeys.key8[HLP_MODEL_KEY8_MPLS_LABEL5_3 + (mplsPop-4)*4];
            }
            else
            {
                ttl = 0;
            }
            break;
        /* 4..7 = Use MPLS TTL from label 1..4 */    
        case 4:
            ttl = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL1_1], 
                                    0, 
                                    8);
            break;
        case 5:
            ttl = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL2_1], 
                                    0, 
                                    8);
            break;
        case 6:
            ttl = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL3_1], 
                                    0, 
                                    8);
            break;
        case 7:
            ttl = FM_GET_UNNAMED_FIELD(
                                    ffuKeys.key16[HLP_MODEL_KEY16_MPLS_LABEL4_1], 
                                    0, 
                                    8);
            break;
        default:
            ttl = 0;
            break;

    }
    /* Update TTL01 based on TTL value */
    muxedAction->ttl01 = 0;
    if(ttl == 0)
    {
        FM_SET_UNNAMED_FIELD(muxedAction->ttl01, 0, 1, 1);
    }
    else if(ttl == 1)
    {
        FM_SET_UNNAMED_FIELD(muxedAction->ttl01, 1, 1, 1);
    }
    /* TTL_CTRL */
    muxedAction->ttl_ctrl = ttl_ctrl;

    /* Merged VPRI */
    muxedAction->vpri = ffuActions.act4[HLP_MODEL_FFU_ACTION_VPRI_HIGH].val;
    if(ffuActions.act4[HLP_MODEL_FFU_ACTION_VPRI_LOW].prec >= 
       ffuActions.act4[HLP_MODEL_FFU_ACTION_VPRI_HIGH].prec)
    {
        muxedAction->vpri = ffuActions.act4[HLP_MODEL_FFU_ACTION_VPRI_LOW].val;
    }
    
    /* Route */
    muxedAction->route = 0;
    if(ffuActions.act1[HLP_MODEL_FFU_ACTION_NO_ROUTE].val == 0 &&
       ffuActions.act24[HLP_MODEL_FFU_ACTION_FWD].val != 0)
    {    
        muxedAction->route = 1;
    }

} /* end MuxedAction */

/*****************************************************************************/
/** Entropy 
 * \ingroup intModel
 *
 * \desc            Calculates hash value from FFU KEYS and populates ECMP_HASH
 *                  and MOD_META
 *
 * \param[in]       model points to the switch data structure.
 *
 *****************************************************************************/
static void Entropy(hlp_model           *model) 
{
    hlp_modelState          *state;
    hlp_modelFfuKeys        hashKeys;
    fm_byte                 hashBytes[HLP_MODEL_FFU_N_HASH_KEYS];
    hlp_modelFfuActions     ffuActions;
    hlp_modelFfuKeyMaskCfg  entropyCfg;
    hlpEntropyMetaCfg       metaCfg;
    fm_byte                 hashProf[2] = { 0 };
    fm_uint32               hash[2];
    fm_int                  i;
    fm_status               status = FM_OK;
    fm_int                  j;
    fm_uint64               mask;
    fm_uint64               meta_hash;

    state = &model->packetState;
    ffuActions = state->FFU_ACTIONS;

    for(i = 0; i < 2; i++)
    {
        FM_SET_UNNAMED_FIELD(
                hashProf[i],
                0,
                4,
                ffuActions.act4[HLP_MODEL_FFU_ACTION_HASH_PROFILE_ECMP_0+2*i].val);
        
        FM_SET_UNNAMED_FIELD(
                hashProf[i],
                4,
                2,
                (ffuActions.act4[HLP_MODEL_FFU_ACTION_HASH_PROFILE_ECMP_1+2*i].val & 0x3));

        /* Get ENTROPY_HASH_CFG register fields */
        GetEntropyCfg(model, i, hashProf[i], &entropyCfg);
        
        WM_DISPLAY3(ffuFinalDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "hash_profile[%d]=%d\n", i, hashProf[i])
        /* Apply KeyMask and KeySubmask on FFU Keys */
        status = hlpModelApplyKeyMask(model, entropyCfg, &hashKeys);
        
        /* Convert Keys into array of bytes */ 
        status = hlpModelConvertKeysToBytes(hashKeys, hashBytes);
        
        /* 
        for(j = 0; j < HLP_MODEL_FFU_N_HASH_KEYS; j++)
            if(hashBytes[j] != 0)
                FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "hashBytes[%d]=0x%x\n", j, hashBytes[j]);
        */

        /* Get hash value from crc */
        if (i == 0)
        {    
            /* HASH0 - CRC-32 (Ethernet) */
            hash[i] = fmCrc32ByteSwap(hashBytes, HLP_MODEL_FFU_N_HASH_KEYS); 
        } 
        else
        {    
            /* HASH1 - CRC-32C (iSCSI) */
            hash[i] = fmCrc32CByteSwap(hashBytes, HLP_MODEL_FFU_N_HASH_KEYS); 
        }    
        WM_DISPLAY3(ffuFinalDisplayVerbose, FM_LOG_CAT_MODEL_FFU,"CRC32=0x%x\n", hash[i])

    }
    
    /* ECMP HASH for ARP_TABLE */
    state->ECMP_HASH = hash[0] & 0xFFFFFF;

    /* MOD_META for modify */
    GetEntropyMetaCfg(model, hashProf[1], &metaCfg);
    /* Apply Defaults to MOD_META */
    for(i = 0; i < 6; i++)
    {
        switch(FM_GET_UNNAMED_FIELD(metaCfg.BYTE_DEFAULTS, i*2, 2))
        {
            case 0:
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8,
                                     4,
                                     ffuActions.act4[HLP_MODEL_FFU_ACTION_META0_LOW].val);
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8 + 4,
                                     4,
                                     ffuActions.act4[HLP_MODEL_FFU_ACTION_META0_HIGH].val);
                break;          
            case 1:
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8,
                                     4,
                                     ffuActions.act4[HLP_MODEL_FFU_ACTION_META1_LOW].val);
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8 + 4,
                                     4,
                                     ffuActions.act4[HLP_MODEL_FFU_ACTION_META1_HIGH].val);
                break;  
            case 2:
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8,
                                     4,
                                     ffuActions.act4[HLP_MODEL_FFU_ACTION_META2_LOW].val);
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8 + 4,
                                     4,
                                     ffuActions.act4[HLP_MODEL_FFU_ACTION_META2_HIGH].val);
                break;  
            case 3:
                FM_SET_UNNAMED_FIELD64(state->MOD_META,
                                     i*8,
                                     8,
                                     0);
                break;       
        }
    }
    
    WM_DISPLAY3(ffuFinalDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "Default MOD_META=0x%llx\n", state->MOD_META)


    /* Apply Hash to MOD_META */
    mask = 0;
    for (i = 0; i < 48; i++)
    {
        if ((i >= metaCfg.HASH_START) && (i < (metaCfg.HASH_START + metaCfg.HASH_SIZE)))
            mask = mask | (FM_LITERAL_U64(1) << i);
    }
    meta_hash = hash[1];
    meta_hash = (meta_hash << 32) | hash[1];

    state->MOD_META = (state->MOD_META & ~mask) | (meta_hash & mask);
    WM_DISPLAY3(ffuFinalDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "MOD_META=0x%llx, mask=0x%llx, hash=0x%llx\n", state->MOD_META, mask, meta_hash)

    /* Update Meta Data */
    //Hash1 has precedence over hash0
    state->HQM_HASH = hash[0] & 0xFFFF;
    state->HQM_HASH_V = 0;
    if ((ffuActions.act4[HLP_MODEL_FFU_ACTION_HASH_PROFILE_MOD_1].val >> 3) & 0x1) {
        state->HQM_HASH = hash[1] & 0xFFFF;
        state->HQM_HASH_V = 1;
    } 
    else if ((ffuActions.act4[HLP_MODEL_FFU_ACTION_HASH_PROFILE_ECMP_1].val >> 3) & 0x1) {
        state->HQM_HASH_V = 1;
    }
} /* end Entropy */

/*****************************************************************************/
/** Xform 
 * \ingroup intModel
 *
 * \desc            Transforms FFU Keys and Actions into FWD keys and Actions.
 *
 * \param[in]       model points to the switch data structure.
 *
 *****************************************************************************/
static void Xform(hlp_model           *model)
{
    fm_uint32                  *regPtr;
    hlp_modelState             *state;
    hlp_modelFfuActions        ffuActions;
    hlp_modelFfuKeys           ffuKeys;
    hlp_modelFfuMuxedAction    muxedAction;
    fm_int                     index;
    fm_int                     i;
    fm_byte                    etype0_ptr;
    fm_byte                    etype1_ptr;
    fm_byte                    vlan1_ptr;
    fm_byte                    vlan2_ptr;
    fm_uint16                  ip_protocol;

    state       = &model->packetState;

    regPtr  = 
        FM_MODEL_GET_REG_PTR(model, HLP_MAP_PORT_CFG(state->RX_PORT, 0));
    ffuActions  = state->FFU_ACTIONS;
    ffuKeys     = state->FFU_KEYS;
    muxedAction = state->FFU_MUXED_ACTION;
    
    for(int i = 0; i < 32; i++)
        state->PKT_META[i] = ffuKeys.key8[63-i];

    // FWD Keys
    //TODO: update new fields 
    state->L34_HASH     = state->ECMP_HASH;
    state->L2_IDOMAIN   = 0;
    state->L3_IDOMAIN   = 0;
    //state->LEARN_MODE   = 0;
    state->MOD_IDX      = (ffuActions.act24[HLP_MODEL_FFU_ACTION_MOD_IDX].val >> 2) & 0xFFFF;
    state->DECAP        = (ffuActions.act24[HLP_MODEL_FFU_ACTION_MOD_IDX].val >> 1) & 0x1;
    state->ENCAP        = ffuActions.act24[HLP_MODEL_FFU_ACTION_MOD_IDX].val & 0x1;
    state->MPLS_POP     = ffuActions.act4[HLP_MODEL_FFU_ACTION_MPLS_POP].val;
    state->QOS_SWPRI    = muxedAction.swpri;
    FM_SET_UNNAMED_FIELD64(state->SGLORT,
                   0,
                   8,
                   ffuKeys.key8[(HLP_RE_KEYS_SGLORT-HLP_RE_KEYS_GENERAL_8B)*2+1]);
    FM_SET_UNNAMED_FIELD64(state->SGLORT,
                   8,
                   16,
                   ffuKeys.key8[(HLP_RE_KEYS_SGLORT-HLP_RE_KEYS_GENERAL_8B)*2]);
    FM_SET_UNNAMED_FIELD64(state->IDGLORT,
                   0,
                   8,
                   ffuKeys.key8[(HLP_RE_KEYS_DGLORT-HLP_RE_KEYS_GENERAL_8B)*2+1]);
    FM_SET_UNNAMED_FIELD64(state->IDGLORT,
                   8,
                   16,
                   ffuKeys.key8[(HLP_RE_KEYS_DGLORT-HLP_RE_KEYS_GENERAL_8B)*2]);
    
    if (state->DECAP)
    {
        //ethertype
        state->L2_ETYPE = ffuKeys.key16[HLP_RE_KEYS_INNER_ETYPE];
        // smac
        FM_SET_UNNAMED_FIELD64(state->L2_SMAC,
                               0,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_INNER_SMAC + 2]);
        FM_SET_UNNAMED_FIELD64(state->L2_SMAC,
                               16,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_INNER_SMAC + 1]);
        FM_SET_UNNAMED_FIELD64(state->L2_SMAC,
                               32,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_INNER_SMAC]);

        // dmac
        FM_SET_UNNAMED_FIELD64(state->L2_DMAC,
                               0,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_INNER_DMAC + 2]);
        FM_SET_UNNAMED_FIELD64(state->L2_DMAC,
                               16,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_INNER_DMAC + 1]);
        FM_SET_UNNAMED_FIELD64(state->L2_DMAC,
                               32,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_INNER_DMAC]);

        // dmac from ipv6
        state->DMAC_FROM_IPV6 = ExtractDmac(
                                    ffuKeys.key32[HLP_MODEL_KEY32_INNER_DIP_31_0], 
                                    ffuKeys.key32[HLP_MODEL_KEY32_INNER_DIP_63_32]);

        
        //IPV4/6
        state->IS_IPV4 = state->PARSER_INFO.inr_l3_len && 
                     (!state->PARSER_INFO.inr_l3_v6);
        state->IS_IPV6 = state->PARSER_INFO.inr_l3_len && 
                      state->PARSER_INFO.inr_l3_v6;

        // length
        FM_SET_UNNAMED_FIELD64(state->L3_LENGTH,
                               0,
                               8,
                               ffuKeys.key8[HLP_MODEL_KEY8_INNER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(state->L3_LENGTH,
                               8,
                               8,
                               ffuKeys.key8[HLP_MODEL_KEY8_INNER_LEN]);
        // length
        ip_protocol = ffuKeys.key8[HLP_MODEL_KEY8_INNER_PROT];

    }
    else
    {
        //ethertype
        state->L2_ETYPE = ffuKeys.key16[HLP_RE_KEYS_OUTER_ETYPE];
        // smac
        FM_SET_UNNAMED_FIELD64(state->L2_SMAC,
                               0,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_OUTER_SMAC + 2]);
        FM_SET_UNNAMED_FIELD64(state->L2_SMAC,
                               16,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_OUTER_SMAC + 1]);
        FM_SET_UNNAMED_FIELD64(state->L2_SMAC,
                               32,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_OUTER_SMAC]);
        // dmac
        FM_SET_UNNAMED_FIELD64(state->L2_DMAC,
                               0,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_OUTER_DMAC + 2]);
        FM_SET_UNNAMED_FIELD64(state->L2_DMAC,
                               16,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_OUTER_DMAC + 1]);
        FM_SET_UNNAMED_FIELD64(state->L2_DMAC,
                               32,
                               16,
                               ffuKeys.key16[HLP_RE_KEYS_OUTER_DMAC]);
        
        state->DMAC_FROM_IPV6 = ExtractDmac(
                                    ffuKeys.key32[HLP_MODEL_KEY32_OUTER_DIP_31_0], 
                                    ffuKeys.key32[HLP_MODEL_KEY32_OUTER_DIP_63_32]);

        //IPV4/6
        state->IS_IPV4 = state->PARSER_INFO.otr_l3_len && 
                     (!state->PARSER_INFO.otr_l3_v6);
        state->IS_IPV6 = state->PARSER_INFO.otr_l3_len && 
                      state->PARSER_INFO.otr_l3_v6;

        // length
        FM_SET_UNNAMED_FIELD64(state->L3_LENGTH,
                               0,
                               8,
                               ffuKeys.key8[HLP_MODEL_KEY8_OUTER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(state->L3_LENGTH,
                               8,
                               8,
                               ffuKeys.key8[HLP_MODEL_KEY8_OUTER_LEN]);
        //prot 
        ip_protocol = ffuKeys.key8[HLP_MODEL_KEY8_OUTER_PROT];

    }
    state->TRAP_IP_OPTIONS = state->IP_OPTION[state->DECAP];
    state->DROP_TTL = 
        (state->IS_IPV4 | state->IS_IPV6 | state->PA_FLAGS[HLP_PA_FLAGS_OTR_MPLS_V]) && 
        ((muxedAction.ttl01 & 1) | ((muxedAction.ttl01 >> 1) & 1));
    state->TRAP_ICMP = state->DROP_TTL & 
                     ((state->IS_IPV4 & (ip_protocol == HLP_MODEL_PROT_ICMPv4))|
                      (state->IS_IPV6 & (ip_protocol == HLP_MODEL_PROT_ICMPv6)));
    state->TRAP_IGMP = state->IS_IPV4 & (ip_protocol == HLP_MODEL_PROT_IGMP);
    state->TTL_CTRL = muxedAction.ttl_ctrl;
    state->ECN      = muxedAction.ecn;     
    state->AQM_MARK_EN = muxedAction.aqm_mark_en;

    index = (HLP_RE_KEYS_INNER_IP_LEN - HLP_RE_KEYS_GENERAL_8B) * 2 + 1;
    FM_SET_UNNAMED_FIELD64(state->INNER_L3_LENGTH,
                           0,
                           8,
                           ffuKeys.key8[index]);
    FM_SET_UNNAMED_FIELD64(state->INNER_L3_LENGTH,
                           8,
                           8,
                           ffuKeys.key8[index - 1]);
    index = (HLP_RE_KEYS_OUTER_IP_LEN - HLP_RE_KEYS_GENERAL_8B) * 2 + 1;
    FM_SET_UNNAMED_FIELD64(state->OUTER_L3_LENGTH,
                           0,
                           8,
                           ffuKeys.key8[index]);
    FM_SET_UNNAMED_FIELD64(state->OUTER_L3_LENGTH,
                           8,
                           8,
                           ffuKeys.key8[index - 1]);

    // xformActions
    state->FFU_FLAGS.drop = ffuActions.act1[HLP_MODEL_FFU_ACTION_DROP].val;
    state->FFU_FLAGS.trap = ffuActions.act1[HLP_MODEL_FFU_ACTION_TRAP].val;
    state->FFU_FLAGS.log  = ffuActions.act1[HLP_MODEL_FFU_ACTION_LOG].val;
    state->FFU_FLAGS.no_route 
                        = ffuActions.act1[HLP_MODEL_FFU_ACTION_NO_ROUTE].val;
    state->FFU_FLAGS.rx_mirror 
                        = ffuActions.act1[HLP_MODEL_FFU_ACTION_RX_MIRROR].val;
    state->FFU_FLAGS.capture_time
                        = ffuActions.act1[HLP_MODEL_FFU_ACTION_CAPT_TIME].val;
    for( i = 0; i <= 1; i++)
    {
        FM_SET_UNNAMED_FIELD(state->FFU_FLAGS.tx_tag,
                             i,
                             1,
                             ffuActions.act1[i+HLP_MODEL_FFU_ACTION_TX_TAG0].val);
    }
    state->TX_TAG = state->FFU_FLAGS.tx_tag;

    // route_val
    FM_SET_UNNAMED_FIELD64(state->FFU_ROUTE,
                         0,
                         22,
                         FM_GET_UNNAMED_FIELD64(
                         ffuActions.act24[HLP_MODEL_FFU_ACTION_FWD].val,
                         0, 
                         22));
    
    FM_SET_UNNAMED_FIELD(state->NO_LEARN,
                         0,
                         1,
                         !ffuActions.act1[HLP_MODEL_FFU_ACTION_LEARN].val);
    

    // set_vlan_val
    if(state->DECAP && ffuActions.act4[HLP_MODEL_FFU_ACTION_VID_LOW].prec <= 1)
    {
        state->L2_IVID1 = ffuKeys.key16[HLP_RE_KEYS_INNER_VLAN1];
    }
    else
    {
        for(i = 0; 
            i <= HLP_MODEL_FFU_ACTION_VID_HIGH-HLP_MODEL_FFU_ACTION_VID_LOW; 
            i++)
        {
            FM_SET_UNNAMED_FIELD(state->L2_IVID1,
                                 i * 4,
                                 4,
                                 ffuActions.act4[HLP_MODEL_FFU_ACTION_VID_LOW+i].val);
        }
    }
    // set_vpri_val
    if(state->DECAP && ffuActions.act4[HLP_MODEL_FFU_ACTION_VPRI_LOW].val <= 1
       && ffuActions.act4[HLP_MODEL_FFU_ACTION_VPRI_HIGH].val <= 1)
    {
       //FIXME: update act1/key16 index when act1 updates. use define foe key16
       //index
       state->QOS_L2_VPRI1 = ffuActions.act1[HLP_MODEL_FFU_ACTION_COPY_OTR_VPRI].val ? 
           ((ffuKeys.key16[HLP_RE_KEYS_OUTER_VLAN1] >> 12) & 0xF) : 
           ((ffuKeys.key16[HLP_RE_KEYS_INNER_VLAN1] >> 12) & 0xF) ;
    }
    else
    {
        state->QOS_L2_VPRI1 = muxedAction.vpri;
    }
    // set_dscp_val
    state->QOS_L3_DSCP = muxedAction.dscp;
    // set_trig_val
    for(i = HLP_MODEL_FFU_ACTION_TRIGGER0;i <= HLP_MODEL_FFU_ACTION_TRIGGER7;i++)
    {
        FM_SET_UNNAMED_FIELD(state->FFU_TRIG,
                             i - HLP_MODEL_FFU_ACTION_TRIGGER0,
                             1,
                             ffuActions.act1[i].val);
    }
    // police actions
    for(i = HLP_MODEL_FFU_ACTION_POLICER0;i <= HLP_MODEL_FFU_ACTION_POLICER3;i++)
    {
        state->POLICER_ACTION[i] = ffuActions.act24[i].val;
    }
    //FIXME: value of PARITY_ERR is uncorrectable tcam/sram error, also is pkt_meta
    //directly from parser? 
    state->PARITY_ERROR = 0;
    
    //Meta Data updates
    if (state->HQM_HASH_V) {
        state->PKT_META[26] = state->HQM_HASH & 0xFF;
        state->PKT_META[27] = (state->HQM_HASH >> 8) & 0xFF;
    }
} /* end Xform */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelFfuFinalActions
 * \ingroup intModel
 *
 * \desc           Models the FFU Final Actions of Ingress Processing pipeline.
 *
 * \param[in]      model points to the switch data structure.
 *
 * \return         FM_OK if successful.
 * \return         Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelFfuFinalActions(hlp_model *model)
{
    hlp_modelState *    state = &model->packetState;
    fm_status           status = FM_OK;
    
    if (testPlusArgs("HLP_FFU_FINAL_ACTIONS_WM_PRINT_VERBOSE") >= 0)
        ffuFinalDisplayVerbose = testPlusArgs("HLP_FFU_FINAL_ACTIONS_WM_PRINT_VERBOSE");
    WM_DISPLAY(ffuFinalDisplayVerbose, FM_LOG_CAT_MODEL_FFU, "ffuFinalDisplayVerbose=%0d\n", ffuFinalDisplayVerbose)    

    state = &model->packetState;
    
    /* ECN/DSCP/SWPRI/TTL and merged VPRI */
    MuxedAction(model);

    /* Get ECMP Hash  and MOD_META */
    Entropy(model);
    
    /* Transform FFU KEYS and ACTIONS to >FWD KEYS and ACTIONS */
    Xform(model);
    
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    
ABORT:
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpFfuFinalActions);

    return status;

}   /* end hlpModelFfuFinalActions */

