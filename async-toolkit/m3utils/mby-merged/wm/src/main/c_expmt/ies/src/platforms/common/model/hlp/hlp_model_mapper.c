/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_mapper.c
 * Creation Date:   May 18, 2015
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
#define N_REALIGN_KEYS      80


#define TC_SOURCE_VPRI      0
#define TC_SOURCE_MPLS      1
#define TC_SOURCE_DSCP      2
#define TC_SOURCE_META      3

#define SOURCE_NOOP                 0
#define SOURCE_MAP_PORT             1
#define SOURCE_MAP_OUTER_PROT       2
#define SOURCE_MAP_OUTER_ETYPE      3
#define SOURCE_MAP_OUTER_DMAC_H     4
#define SOURCE_MAP_OUTER_DMAC_L     5
#define SOURCE_MAP_OUTER_SMAC_H     6
#define SOURCE_MAP_OUTER_SMAC_L     7
#define SOURCE_MAP_OUTER_DIP        8
#define SOURCE_MAP_OUTER_SIP        9
//#define SOURCE_MAP_OUTER_L3_LENGTH  10
#define SOURCE_MAP_OUTER_L4_SRC_L   12
#define SOURCE_MAP_OUTER_L4_SRC_H   15
#define SOURCE_MAP_OUTER_L4_DST_L   16
#define SOURCE_MAP_OUTER_L4_DST_H   19
#define SOURCE_PA_FLAGS_L           20
#define SOURCE_PA_FLAGS_H           31
#define SOURCE_FFU_SCENARIO_L       32
#define SOURCE_FFU_SCENARIO_H       33
#define SOURCE_MAP_INNER_PROT       34
#define SOURCE_MAP_INNER_ETYPE      35
#define SOURCE_MAP_INNER_DMAC_H     36
#define SOURCE_MAP_INNER_DMAC_L     37
#define SOURCE_MAP_INNER_SMAC_H     38
#define SOURCE_MAP_INNER_SMAC_L     39
#define SOURCE_MAP_INNER_DIP        40
#define SOURCE_MAP_INNER_SIP        41
//#define SOURCE_MAP_INNER_L3_LENGTH  42
#define SOURCE_MAP_INNER_L4_SRC_L   44
#define SOURCE_MAP_INNER_L4_SRC_H   47
#define SOURCE_MAP_INNER_L4_DST_L   48
#define SOURCE_MAP_INNER_L4_DST_H   51
#define SOURCE_EX                   52
#define SOURCE_CSUM                 53
#define SOURCE_IP_INFO              54


/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_int     mapperDisplayVerbose = 1;    /* 0=no print;*/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

static fm_uint32 Rotate(fm_uint key, fm_uint rot)
{
    return ((key << rot) | (key >> (32 - rot)));
}

static fm_uint32 MaskGen(fm_uint start, fm_uint len)
{
    fm_uint j;
    fm_uint32 mask;

    mask = 0;
    for (j = start; j < (start + len); j++)
    {
        mask |= (1 << j);
    }

    return mask;
}

/*****************************************************************************/
/** GetPortCfg
 * \ingroup intModel
 *
 * \desc           Get port_cfg and port_default.
 *
 * \param[in]      model pointer to the switch data structure
 *
 * \param[out]     portCfg points to caller-allocated structure which will be
 *                 carried through the rest of the mapper pipeline
 *
 *****************************************************************************/
static void GetPortCfg(hlp_model            *model,
                       hlpMapPortCfg        *portCfg)
{
    fm_uint32            *regPtr;
    hlp_modelState       *state;

    state = &model->packetState;

    /* Mapper port config */
    regPtr  =
        FM_MODEL_GET_REG_PTR(model, HLP_MAP_PORT_CFG(state->RX_PORT, 0));

    portCfg->DEFAULT_SGLORT =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_CFG, DEFAULT_SGLORT);
    portCfg->DEFAULT_SGLORT_EN =
        FM_ARRAY_GET_BIT(regPtr, HLP_MAP_PORT_CFG, DEFAULT_SGLORT_EN);
    portCfg->PORT_SCENARIO =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_CFG, PORT_SCENARIO);

} /* end GetPortCfg */


/*****************************************************************************/
/** RealignKeys
 * \ingroup intModel
 *
 * \desc            Realign parser keys into the same key format
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       isIPv4 points to caller-allocated storage which tells if
 *                  the current packet header is IPv4 or not
 *
 * \param[in]       isIPv6 points to caller-allocated storage which tells if
 *                  the current packet header is IPv6 or not
 *
 * \param[out]      realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys
 *
 * \param[out]      realignKeysValid points to caller-allocated storage which
 *                  is the results after realign parser_keys_v
 *
 *****************************************************************************/
static void RealignKeys(hlp_model        *model,
                        fm_bool          *isIPv4,
                        fm_bool          *isIPv6,
                        fm_uint16        *realignKeys,
                        fm_bool          *realignKeysValid,
                        fm_bool          *ihl_ok,
                        fm_bool          *ihl_fits)
{
     hlp_modelState     *state;
     fm_int             i;
     fm_bool            innerHf, innerIhlNot5, outerHf, outerIhlNot5;
     fm_byte            ihl;

    state = &model->packetState;

    for (i = 0; i < N_REALIGN_KEYS; i++)
    {
        realignKeys[i]      = state->PA_KEYS[i];
        realignKeysValid[i] = state->PA_KEYS_VALID[i];
    }

    /* Realign for IPv4 */

    /* Inner IP Header */
    if ( isIPv4[1] )
    {
        realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT] =
            state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 4];

        realignKeysValid[HLP_PA_KEYS_INNER_IP_HEADER + 4] = 0;
        realignKeysValid[HLP_RE_KEYS_INNER_IP_TTL_PROT] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER + 4];

        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             8,
                             8,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER],
                             0,
                             8) );
        //MISC
        innerHf =
            ( !FM_GET_UNNAMED_FIELD(
              state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 3], 0, 13) ) ? 1 : 0;
        innerIhlNot5 =
            ( FM_GET_UNNAMED_FIELD(
              state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER], 8, 4) != 5 ) ? 1 : 0;
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             4,
                             1,
                             innerIhlNot5);
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             5,
                             1,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 3],
                             14,
                             1) );
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             6,
                             1,
                             innerHf);
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             7,
                             1,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 3],
                             13,
                             1) );
        realignKeysValid[HLP_RE_KEYS_INNER_IP_DS_FLOW] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER];
        //FLOW
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             0,
                             4,
                             0);
        realignKeys[HLP_RE_KEYS_INNER_IP_FLOW] = 0;
        realignKeysValid[HLP_RE_KEYS_INNER_IP_FLOW] = 0;

        realignKeysValid[HLP_PA_KEYS_INNER_IP_HEADER + 4] = 0;
        realignKeysValid[HLP_PA_KEYS_INNER_IP_HEADER + 5] = 0;

        /* inner DIP */
        realignKeys[HLP_RE_KEYS_INNER_DIP] =
            state->PA_KEYS[HLP_PA_KEYS_INNER_SIPDIP + 2];
        realignKeys[HLP_RE_KEYS_INNER_DIP + 1] =
            state->PA_KEYS[HLP_PA_KEYS_INNER_SIPDIP + 3];

        realignKeysValid[HLP_PA_KEYS_INNER_SIPDIP + 2] = 0;
        realignKeysValid[HLP_PA_KEYS_INNER_SIPDIP + 3] = 0;

        realignKeysValid[HLP_RE_KEYS_INNER_DIP] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_SIPDIP + 2];
        realignKeysValid[HLP_RE_KEYS_INNER_DIP + 1] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_SIPDIP + 3];
    }
    /* Outer IP Header */
    *ihl_ok = 1;
    *ihl_fits = 1;
    if ( isIPv4[0] )
    {
        realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT] =
            state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 4];

        realignKeysValid[HLP_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_TTL_PROT] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER + 4];

        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             8,
                             8,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER],
                             0,
                             8) );
        //MISC
        outerHf =
            ( !FM_GET_UNNAMED_FIELD(
              state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 3], 0, 13) ) ? 1 : 0;

        ihl = FM_GET_UNNAMED_FIELD(
              state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER], 8, 4);
        outerIhlNot5 = (ihl != 5 ) ? 1 : 0;

        if(ihl < 5)
        {
            *ihl_ok = 0;
        }

        if(realignKeys[HLP_RE_KEYS_OUTER_IP_LEN] < 4*ihl)
        {
            *ihl_fits = 0;
        }

        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             4,
                             1,
                             outerIhlNot5);
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             5,
                             1,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 3],
                             14,
                             1) );
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             6,
                             1,
                             outerHf);
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             7,
                             1,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 3],
                             13,
                             1) );
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_DS_FLOW] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER];
        //FLOW
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             0,
                             4,
                             0);
        realignKeys[HLP_RE_KEYS_OUTER_IP_FLOW] = 0;
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_FLOW] = 0;

        realignKeysValid[HLP_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realignKeysValid[HLP_PA_KEYS_OUTER_IP_HEADER + 5] = 0;

        /* outer DIP */
        realignKeys[HLP_RE_KEYS_OUTER_DIP] =
            state->PA_KEYS[HLP_PA_KEYS_OUTER_SIPDIP + 2];
        realignKeys[HLP_RE_KEYS_OUTER_DIP + 1] =
            state->PA_KEYS[HLP_PA_KEYS_OUTER_SIPDIP + 3];

        realignKeysValid[HLP_PA_KEYS_OUTER_SIPDIP + 2] = 0;
        realignKeysValid[HLP_PA_KEYS_OUTER_SIPDIP + 3] = 0;

        realignKeysValid[HLP_RE_KEYS_OUTER_DIP] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_SIPDIP + 2];
        realignKeysValid[HLP_RE_KEYS_OUTER_DIP + 1] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_SIPDIP + 3];
     }

    /* Realign for IPv6 */

    /* Inner IP Header */
    if ( isIPv6[1] )
    {
        //TTL
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT],
                             8,
                             8,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 5],
                             0,
                             8) );
        //PROT
        if ( state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER + 1] )
        {
            FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT],
                                 0,
                                 8,
                                 FM_GET_UNNAMED_FIELD(
                                 state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 1],
                                 8,
                                 8) );
        }
        else
        {
            FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT],
                                 0,
                                 8,
                                 FM_GET_UNNAMED_FIELD(
                                 state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 5],
                                 8,
                                 8) );
        }

        realignKeysValid[HLP_PA_KEYS_INNER_IP_HEADER + 5] = 0;
        realignKeysValid[HLP_RE_KEYS_INNER_IP_TTL_PROT] =
                state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER + 5];

        //LEN
        realignKeys[HLP_RE_KEYS_INNER_IP_LEN]      =
            state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 4];

        realignKeysValid[HLP_PA_KEYS_INNER_IP_HEADER + 4] = 0;
        realignKeysValid[HLP_RE_KEYS_INNER_IP_LEN] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER + 4];
        //DS
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             8,
                             8,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 2],
                             4,
                             8) );
        //MISC
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             4,
                             4,
                             0);
        realignKeysValid[HLP_RE_KEYS_INNER_IP_DS_FLOW] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER + 2];
        //FLOW
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_DS_FLOW],
                             0,
                             4,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 2],
                             0,
                             4) );
        realignKeys[HLP_RE_KEYS_INNER_IP_FLOW] =
            state->PA_KEYS[HLP_PA_KEYS_INNER_IP_HEADER + 3];
        realignKeysValid[HLP_RE_KEYS_INNER_IP_FLOW] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER + 3];
     }
     /* Outer IP Header */
     if ( isIPv6[0] )
     {
        //TTL
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT],
                             8,
                             8,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 5],
                             0,
                             8) );
        //PROT
        if ( state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER + 1] == 1 )
        {
            FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT],
                                 0,
                                 8,
                                 FM_GET_UNNAMED_FIELD(
                                 state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 1],
                                 8,
                                 8) );
        }
        else
        {
            FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT],
                                 0,
                                 8,
                                 FM_GET_UNNAMED_FIELD(
                                 state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 5],
                                 8,
                                 8) );
        }

        realignKeysValid[HLP_PA_KEYS_OUTER_IP_HEADER + 5] = 0;
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_TTL_PROT] =
                state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER + 5];

        //LEN
        realignKeys[HLP_RE_KEYS_OUTER_IP_LEN]  =
            state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 4];

        realignKeysValid[HLP_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_LEN] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER + 4];
        //DS
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             8,
                             8,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 2],
                             4,
                             8) );
        //MISC
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             4,
                             4,
                             0);
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_DS_FLOW] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER + 2];
        //FLOW
        FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW],
                             0,
                             4,
                             FM_GET_UNNAMED_FIELD(
                             state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 2],
                             0,
                             4) );
        realignKeys[HLP_RE_KEYS_OUTER_IP_FLOW] =
            state->PA_KEYS[HLP_PA_KEYS_OUTER_IP_HEADER + 3];
        realignKeysValid[HLP_RE_KEYS_OUTER_IP_FLOW] =
            state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER + 3];
    }

    /*
    for (i = 0; i < N_REALIGN_KEYS; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,"realignKeys[%0d]=0x%x, valid=%d\n", i, realignKeys[i], realignKeysValid[i]);
    }
    */
} /* end RealignKeys */


/*****************************************************************************/
/** GetDglortFromDglortKey
 * \ingroup intModel
 *
 * \desc            Get dGlort from MAP_DGLORT_TCAM table.
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       dglortFromDglortKey points to caller-allocated storage which
 *                  store the dglort if matched.
 *                  If not, then it will return -1;
 *
 *****************************************************************************/
static void GetDglortFromDglortKey(hlp_model *model, fm_int *dglortFromDglortKey)
{
    hlp_modelState     *state;
    fm_uint32          *regPtr;
    fm_uint16           srcPort;
    fm_int              i;
    fm_uint             inputKey;
    fm_uint             key;
    fm_uint             mask;
    fm_uint             onpiType;
    fm_uint             lanDstPort;
    fm_uint             dsiDstPort;
    fm_int              dglort;
    fm_uint             rotate;
    fm_uint             maskGen;


    state   = &model->packetState;

    onpiType = state->PKT_META[HLP_MODEL_META_TYPE_OFF];
    lanDstPort = state->PKT_META[2] & 0x1F;
    dsiDstPort =  state->PKT_META[4];
    /*
    switch (onpiType)
    {
        case HLP_MODEL_META_TYPE_LAN_TX:
            lanDstPort = state->PKT_META[2] & 0x1F;
            dsiDstPort = 0;
            break;
        case HLP_MODEL_META_TYPE_DSI_TX:
            lanDstPort = 0;
            dsiDstPort =  state->PKT_META[4];
            break;
        default:
            lanDstPort = 0;
            dsiDstPort = 0;
            break;
    }*/
    inputKey = onpiType | (dsiDstPort << 8) | (lanDstPort << 16);
    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "DGLORT inputKey 0x%06x\n", inputKey)
    //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
    //            "DGLORT inputKey 0x%06x\n", inputKey);

    for (i = HLP_MAP_DGLORT_TCAM_ENTRIES - 1; i >= 0; i--)
    {
        regPtr =  FM_MODEL_GET_REG_PTR(
                      model, HLP_MAP_DGLORT_TCAM(i, 0));
        key = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DGLORT_TCAM, KEY);
        mask = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DGLORT_TCAM, KEY_INVERT);

        //if ((inputKey & mask) == key)
        if ( ! ( ( key & ( ~inputKey & 0x1FFFFF ) ) || ( mask & inputKey ) ) )
        {
            regPtr =  FM_MODEL_GET_REG_PTR(
                      model, HLP_MAP_DGLORT_ACTION(i, 0));
            if (FM_GET_BIT(*regPtr, HLP_MAP_DGLORT_ACTION, ENABLE))
            {
                dglort = FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, BASE);
                rotate = Rotate(inputKey, FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, ROT)) & 0xFFFF;
                maskGen = MaskGen(FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, START),
                                  FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, LENGTH)) & 0xFFFF;
                //dglort |= Rotate(inputKey, FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, ROT)) &
                //          MaskGen(FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, START),
                //                  FM_GET_FIELD(*regPtr, HLP_MAP_DGLORT_ACTION, LEN));
                dglort = (dglort | (rotate & maskGen)) & 0xFFFF;
                WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "rotate=0x%x, maskgen=0x%x dglort=0x%x\n", rotate, maskGen, dglort)
                //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                //            "rotate=0x%x, maskgen=0x%x dglort=0x%x\n", rotate, maskGen, dglort);

            }
            else
            {
                dglort = -1;
            }

            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Matched DGLORT_KEY %d. DGLORT 0x%x\n", i, dglort)
            //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
            //    "Matched DGLORT_KEY %d. DGLORT 0x%x\n", i, dglort);
            *dglortFromDglortKey = dglort;
            return;
        }
    }

    *dglortFromDglortKey = -1;

} /* end GetDglortFromDglortKey */


/*****************************************************************************/
/** OnpiPhyFunc
 * \ingroup intModel
 *
 * \desc            Modify packet metadata from Rimmon PHY.
 *
 * \param[in]       model points to the switch data structure
 *
 *****************************************************************************/
static void OnpiPhyFunc(hlp_model *model)
{
    hlp_modelState     *state;
    fm_uint16           srcPort;
    fm_uint32           *regPtr;

    state   = &model->packetState;

    if (state->PKT_META[HLP_MODEL_META_TYPE_OFF] ==
        HLP_MODEL_META_TYPE_RIMMON_RX)
    {
        memset(state->PKT_META + 8, 0, 32 - 8);

        /* Set the source port */
        srcPort = state->RX_PORT;
        //state->PKT_META[22] = srcPort & 0xFF;
        //state->PKT_META[23] &= (srcPort >> 4) & 1; /* Bit4 */
        FM_SET_UNNAMED_FIELD(state->PKT_META[18],
                             0,
                             5,
                             srcPort & 0x1F);


        /* Move the timestamp */
        state->PKT_META[8] = state->PKT_META[1];

        /* Move M bit to rx flags */
        FM_SET_UNNAMED_FIELD(state->PKT_META[18],
                             5,
                             1,
                             (state->PKT_META[2] & 0x1));

        /* Clear the original first 4 bytes */
        memset(state->PKT_META, 0, 4);
        /* Set the type */
        state->PKT_META[HLP_MODEL_META_TYPE_OFF] =
                HLP_MODEL_META_TYPE_LAN_RX;

    }

    regPtr =  FM_MODEL_GET_REG_PTR(
                  model, HLP_MAP_CPM_FN(0));
    if ( (FM_GET_FIELD(*regPtr, HLP_MAP_CPM_FN, PORT_MASK)  &
            (1 << state->RX_PORT)) &&
          state->PA_FLAGS[HLP_PA_FLAGS_OTR_ESP_V])
    {
          state->PKT_META[HLP_MODEL_META_ESP_HDR_OFF] =
              state->PA_PTRS[HLP_PA_PTRS_ESP_PTR];
          state->PKT_META[HLP_MODEL_META_IP_HDR_OFF] =
              state->PA_PTRS[HLP_PA_PTRS_OTR_L3_PTR];
    }

} /* end OnpiPhyFunc */


/*****************************************************************************/
/** InsertDefaults
 * \ingroup intModel
 *
 * \desc            Insert default values to FFU keys and actions
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       portCfg is the associated port config
 *
 * \param[in]       dglortFromDglortKey is the dglort value to override if >= 0.
 *
 * \param[in]       realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys
 *
 * \param[in]       realignKeysValid points to caller-allocated storage which
 *                  is the results after realign parser_keys_v
 *
 *****************************************************************************/
static void InsertDefaults(hlp_model            *model,
                           hlpMapPortCfg        portCfg,
                           fm_int               dglortFromDglortKey,
                           fm_uint16            *realignKeys,
                           fm_bool              *realignKeysValid)
{
    hlpMapPortDefault     portDefaults;
    hlp_modelState       *state;
    fm_uint32            *regPtr;
    fm_byte               target;
    fm_int                i;
    fm_int                j;
    fm_uint16             dglort;
    hlp_modelFfuKeys     *ffuKeys;
    hlp_modelFfuActions  *ffuActions;

    state   = &model->packetState;

    ffuKeys = &state->FFU_KEYS;
    ffuActions = &state->FFU_ACTIONS;

    /* Set act24 to 0 first */
    for (i = 0; i < HLP_MODEL_FFU_N_ACT24; i++)
    {
        ffuActions->act24[i].prec = 1;
        ffuActions->act24[i].val = 0;
    }
    /* Set act4 to 0 first */
    for (i = 0; i < HLP_MODEL_FFU_N_ACT4; i++)
    {
        ffuActions->act4[i].prec = 1;
        ffuActions->act4[i].val = 0;
    }
    /* Set act4 to 0 first */
    for (i = 0; i < HLP_MODEL_FFU_N_ACT1; i++)
    {
        ffuActions->act1[i].prec = 1;
        ffuActions->act1[i].val = 0;
    }

    /* Clear SGLORT and DGLORT */
    realignKeys[HLP_RE_KEYS_SGLORT] = 0;
    realignKeys[HLP_RE_KEYS_DGLORT] = 0;

    //set PA_PTR[0] = 0 since parse only provides [7:1] ptrs
    state->PA_PTRS[0] = 0;

    //Apply defaults on keys
    for (i = 0; i < HLP_MAP_PORT_DEFAULT_ENTRIES_0; i++)
    {
        regPtr =  FM_MODEL_GET_REG_PTR(
                          model, HLP_MAP_PORT_DEFAULT(state->RX_PORT, i, 0));

        portDefaults.TARGET =
            FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_DEFAULT, TARGET);
        target = portDefaults.TARGET;

        if (target > HLP_MODEL_DEFAULT_TARGET_FORCE_KEYS_H)
            continue;

        portDefaults.USE_PARSE_PTR =
                  FM_ARRAY_GET_BIT(
                      regPtr, HLP_MAP_PORT_DEFAULT, USE_PARSE_PTR);
        portDefaults.VALUE = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_DEFAULT, VALUE);
        if ( (i < 4) && (portDefaults.USE_PARSE_PTR))
        {

            if (state->PA_PTRS_VALID[i * 2])
            {
                FM_SET_UNNAMED_FIELD(portDefaults.VALUE,
                                     8,
                                     8,
                                     state->PA_PTRS[i * 2]);
            }
            if (state->PA_PTRS_VALID[i * 2 + 1])
            {
                FM_SET_UNNAMED_FIELD(portDefaults.VALUE,
                                     0,
                                     8,
                                     state->PA_PTRS[i * 2 + 1]);
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "i = %d, value = %x,
             * PA_PTRS[i*2] = 0x%x, PA_PTRS[i*2+1] = 0x%x\n", i,
             * portDefaults.VALUE, state->PA_PTRS[i * 2], state->PA_PTRS[i * 2 +
             * 1]);*/
        }

        portDefaults.USE_KEY =
            FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_DEFAULT, USE_KEY);
        if( (i < 2) && (portDefaults.USE_KEY < 255) &&
            (portDefaults.USE_KEY < HLP_MODEL_N_REALIGN_KEYS))
        {
            if(realignKeysValid[portDefaults.USE_KEY])
            {
                portDefaults.VALUE = realignKeys[portDefaults.USE_KEY];
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "i = %d, value = %x, key_num =
             * %d\n", i, portDefaults.VALUE, portDefaults.USE_KEY);*/
        }

        /* target 0..79 apply keys if they are not valid*/
        if (target <= HLP_MODEL_DEFAULT_TARGET_KEYS_H)
        {
            if (realignKeysValid[target] == 0)
            {
                realignKeys[target] = portDefaults.VALUE;
                realignKeysValid[target] = 1;
                /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "realignKeys[%d] = 0x%x\n",
                 * target, realignKeys[target]);*/

            }
            else if ( ( target == HLP_RE_KEYS_OUTER_VLAN1 ) &&
                    ( FM_GET_UNNAMED_FIELD(realignKeys[target], 0, 12) == 0 ) )
            {
                FM_SET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_VLAN1],
                                     0,
                                     12,
                                     FM_GET_UNNAMED_FIELD(
                                     portDefaults.VALUE, 0, 12) );
                realignKeysValid[target] = 1;
            }
        }

        /* target 80..95 force key value with port default value even if key is
         * valid from parser */
        else if ( (target <= HLP_MODEL_DEFAULT_TARGET_FORCE_KEYS_H) &&
             (target >= HLP_MODEL_DEFAULT_TARGET_FORCE_KEYS_L) )
        {
            realignKeys[target - 68] = portDefaults.VALUE;
            realignKeysValid[target - 68] = 1;
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "realignKeys[%d] = 0x%x\n",
             * target-68, realignKeys[target-68]);*/
        }
    }

    /* Set DS actions to outer_ip.ds */

    if (realignKeysValid[HLP_RE_KEYS_OUTER_IP_DS_FLOW])
    {
        FM_SET_UNNAMED_FIELD(ffuActions->act4[HLP_MODEL_FFU_ACTION_DSCP_LOW].val,
                             0,
                             4,
                             FM_GET_UNNAMED_FIELD(
                             realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW], 10, 4) );
        FM_SET_UNNAMED_FIELD(ffuActions->act4[HLP_MODEL_FFU_ACTION_DSCP_HIGH].val,
                             0,
                             2,
                             FM_GET_UNNAMED_FIELD(
                             realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW], 14, 2));
    }


    /* Set VPRI/VID actions */
    if(realignKeysValid[HLP_RE_KEYS_OUTER_VLAN1])
    {
        ffuActions->act4[HLP_MODEL_FFU_ACTION_VPRI_LOW].val =
            FM_GET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 12, 4);
        ffuActions->act4[HLP_MODEL_FFU_ACTION_VPRI_HIGH].val =
            FM_GET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 12, 4);
        ffuActions->act4[HLP_MODEL_FFU_ACTION_VID_LOW].val =
                             FM_GET_UNNAMED_FIELD(
                                     realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 0, 4);
        ffuActions->act4[HLP_MODEL_FFU_ACTION_VID_MID].val =
                             FM_GET_UNNAMED_FIELD(
                                     realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 4, 4);
        ffuActions->act4[HLP_MODEL_FFU_ACTION_VID_HIGH].val =
                             FM_GET_UNNAMED_FIELD(
                                     realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 8, 4);

    }

    //Apply defaults on actions
    for (i = 0; i < HLP_MAP_PORT_DEFAULT_ENTRIES_0; i++)
    {
        regPtr =  FM_MODEL_GET_REG_PTR(
                      model, HLP_MAP_PORT_DEFAULT(state->RX_PORT, i, 0));
        portDefaults.TARGET =
            FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_DEFAULT, TARGET);
        target = portDefaults.TARGET;

        if (target <= HLP_MODEL_DEFAULT_TARGET_FORCE_KEYS_H)
           continue;

        portDefaults.USE_PARSE_PTR =
                  FM_ARRAY_GET_BIT(
                      regPtr, HLP_MAP_PORT_DEFAULT, USE_PARSE_PTR);
        portDefaults.VALUE = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_DEFAULT, VALUE);
        if ( (i < 4) && (portDefaults.USE_PARSE_PTR))
        {

            if (state->PA_PTRS_VALID[i * 2])
            {
                FM_SET_UNNAMED_FIELD(portDefaults.VALUE,
                                     8,
                                     8,
                                     state->PA_PTRS[i * 2]);
            }
            if (state->PA_PTRS_VALID[i * 2 + 1])
            {
                FM_SET_UNNAMED_FIELD(portDefaults.VALUE,
                                     0,
                                     8,
                                     state->PA_PTRS[i * 2 + 1]);
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "i = %d, value = %x,
             * PA_PTRS[i*2] = 0x%x, PA_PTRS[i*2+1] = 0x%x\n", i,
             * portDefaults.VALUE, state->PA_PTRS[i * 2], state->PA_PTRS[i * 2 +
             * 1]);*/
        }

        portDefaults.USE_KEY =
            FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT_DEFAULT, USE_KEY);
        if( (i < 2) && (portDefaults.USE_KEY < 255) &&
            (portDefaults.USE_KEY < HLP_MODEL_N_REALIGN_KEYS))
        {
            if(realignKeysValid[portDefaults.USE_KEY])
            {
                portDefaults.VALUE = realignKeys[portDefaults.USE_KEY];
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "i = %d, value = %x, key_num =
             * %d\n", i, portDefaults.VALUE, portDefaults.USE_KEY);*/
        }

        /* target 96...105 matches lower act24[0...10] */
        if ( (target <= HLP_MODEL_DEFAULT_TARGET_ACT24_L_H) &&
             (target >= HLP_MODEL_DEFAULT_TARGET_ACT24_L_L) &&
             ((target - HLP_MODEL_DEFAULT_TARGET_ACT24_L_L) < HLP_MODEL_FFU_N_ACT24))
        {
            FM_SET_UNNAMED_FIELD(ffuActions->act24[target - 96].val,
                                 0,
                                 16,
                                 portDefaults.VALUE);

            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ACT24: i = %d, value = 0x%x,
             * target = %d\n", i, portDefaults.VALUE, portDefaults.TARGET);*/
        }
        else if ( (target <= HLP_MODEL_DEFAULT_TARGET_ACT24_U_H) &&
             (target >= HLP_MODEL_DEFAULT_TARGET_ACT24_U_L) &&
             ((target - HLP_MODEL_DEFAULT_TARGET_ACT24_U_L) < HLP_MODEL_FFU_N_ACT24))
        {
            /* target 112...121 matches upper act24[0...10] */
            FM_SET_UNNAMED_FIELD(ffuActions->act24[target - 112].val,
                                 16,
                                 8,
                                 FM_GET_UNNAMED_FIELD(
                                 portDefaults.VALUE,
                                 0,
                                 8) );
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ACT24: i = %d, value = 0x%x,
             * target = %d\n", i, portDefaults.VALUE, portDefaults.TARGET);*/
        }
        else if ( (target <= HLP_MODEL_DEFAULT_TARGET_ACT4_4_H) &&
             (target >= HLP_MODEL_DEFAULT_TARGET_ACT4_4_L) &&
             ((target - HLP_MODEL_DEFAULT_TARGET_ACT4_4_L) < HLP_MODEL_FFU_N_ACT4))
        {
            for (j = 0; j < 4; j++)
            {
                if ((target - HLP_MODEL_DEFAULT_TARGET_ACT4_4_L + j) < HLP_MODEL_FFU_N_ACT4)
                {
                    ffuActions->act4[target - HLP_MODEL_DEFAULT_TARGET_ACT4_4_L + j].val =
                        FM_GET_UNNAMED_FIELD(portDefaults.VALUE, j*4, 4);
                }
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ACT4: i = %d, value = 0x%x, target =
             * %d\n", i, portDefaults.VALUE, portDefaults.TARGET);*/

        }
        else if ( (target <= HLP_MODEL_DEFAULT_TARGET_ACT4_2_H) &&
             (target >= HLP_MODEL_DEFAULT_TARGET_ACT4_2_L) &&
             ((target - HLP_MODEL_DEFAULT_TARGET_ACT4_2_L) < HLP_MODEL_FFU_N_ACT4))
        {
            for (j = 0; j < 2; j++)
            {
                if ((target - HLP_MODEL_DEFAULT_TARGET_ACT4_2_L + j) < HLP_MODEL_FFU_N_ACT4)
                {
                    ffuActions->act4[target - HLP_MODEL_DEFAULT_TARGET_ACT4_2_L + j].val =
                        FM_GET_UNNAMED_FIELD(portDefaults.VALUE, j*4, 4);
                }
            }
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ACT4: i = %d, value = 0x%x,
             * target = %d\n", i, portDefaults.VALUE, portDefaults.TARGET);*/
        }
        else if ( (target <= HLP_MODEL_DEFAULT_TARGET_ACT4_1_H) &&
             (target >= HLP_MODEL_DEFAULT_TARGET_ACT4_1_L)  &&
             ((target - HLP_MODEL_DEFAULT_TARGET_ACT4_1_L) < HLP_MODEL_FFU_N_ACT4))
        {
            ffuActions->act4[target - HLP_MODEL_DEFAULT_TARGET_ACT4_1_L].val     =
                FM_GET_UNNAMED_FIELD(portDefaults.VALUE, 0, 4);
            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "ACT4: i = %d, value = 0x%x, target =
             * %d\n", i, portDefaults.VALUE, portDefaults.TARGET);*/
        }
        else if ( target == HLP_MODEL_DEFAULT_TARGET_ACT1_FLAGS)
        {
            for (j = 0; j < 16; j++)
            {
                ffuActions->act1[j].val =
                    FM_GET_UNNAMED_FIELD(portDefaults.VALUE, j, 1);
            }
        }

    }

    if (portCfg.DEFAULT_SGLORT_EN)
    {
        realignKeys[HLP_RE_KEYS_SGLORT]       = portCfg.DEFAULT_SGLORT;
        realignKeysValid[HLP_RE_KEYS_SGLORT]  = 1;
    }

    if (dglortFromDglortKey >= 0)
    {
        realignKeys[HLP_RE_KEYS_DGLORT]       = dglortFromDglortKey;
        realignKeysValid[HLP_RE_KEYS_DGLORT]  = 1;
    }

    /*
    for(i = 0; i < HLP_MODEL_N_REALIGN_KEYS; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "realignKeys[%d] = 0x%x\n", i, realignKeys[i]);
    }
    */

    /* Set FFU keys from realignKeys */
    /* KEY16 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        ffuKeys->key16[i] = realignKeys[i];
    }
    /* KEY8 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY8; i += 2)
    {
        ffuKeys->key8[i]   =
            FM_GET_UNNAMED_FIELD(realignKeys[(i>>1) + HLP_RE_KEYS_GENERAL_8B], 8, 8);
        ffuKeys->key8[i+1] =
            FM_GET_UNNAMED_FIELD(realignKeys[(i>>1) + HLP_RE_KEYS_GENERAL_8B], 0, 8);
    }
    /* KEY32 */
    for (i = 0; i < HLP_MODEL_FFU_N_KEY32; i++)
    {
        FM_SET_UNNAMED_FIELD(ffuKeys->key32[i],
                             16,
                             16,
                             realignKeys[i * 2 + HLP_RE_KEYS_OUTER_SIP]);
        FM_SET_UNNAMED_FIELD(ffuKeys->key32[i],
                             0,
                             16,
                             realignKeys[i * 2 + HLP_RE_KEYS_OUTER_SIP + 1]);
    }

    /*
    for(i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key16[%d] = 0x%x\n", i, ffuKeys->key16[i]);
    }
    */

} /* InsertDefaults */



/*****************************************************************************/
/**  GetDomainTcamEntry
 * \ingroup intModel
 *
 * \desc            Reads DOMAIN_TCAM register data from register cache
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       camIndex is the entry that needs to be read from TCAM.
 *
 * \param[in,out]   camEntry is the struct for DOMAIN_TCAM that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetDomainTcamEntry(hlp_model        *model,
                               fm_int           camIndex,
                               hlpMapDomainTcam *camEntry)
{
    fm_uint32       *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MAP_DOMAIN_TCAM(camIndex, 0));

    camEntry->_RSVD1_ = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, _RSVD1_);
    camEntry->PORT_KEY_INVERT =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, PORT_KEY_INVERT);
    camEntry->VID2_VALID_INVERT =
        FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_TCAM, VID2_VALID_INVERT);
    camEntry->VID2_KEY_INVERT =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, VID2_KEY_INVERT);
    camEntry->VID1_VALID_INVERT =
        FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_TCAM, VID1_VALID_INVERT);
    camEntry->VID1_KEY_INVERT =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, VID1_KEY_INVERT);
    camEntry->_RSVD0_  = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, _RSVD0_);
    camEntry->PORT_KEY = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, PORT_KEY);
    camEntry->VID2_VALID = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_TCAM, VID2_VALID);
    camEntry->VID2_KEY = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, VID2_KEY);
    camEntry->VID1_VALID = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_TCAM, VID1_VALID);
    camEntry->VID1_KEY = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_TCAM, VID1_KEY);

} /* GetDomainTcamEntry */


/*****************************************************************************/
/**  GetDomainRamEntry
 * \ingroup intModel
 *
 * \desc            Reads DOMAIN_ACTION and DOMAIN_PROFILE register data.
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       camIndex points to the entry that needs to be read from TCAM.
 *
 * \param[out]      act0 is the struct for DOMAIN_ACTION0 that
 *                  is populated with data from register cache
 *
 * \param[out]      act1 is the struct for DOMAIN_ACTION1 that
 *                  is populated with data from register cache
 *
 * \param[out]      prof is the struct for DOMAIN_PROFILE that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
/*static void GetDomainRamEntry(hlp_model        *model,
                              fm_byte           camIndex,
                              hlpMapDomainAction0 *act0,
                              hlpMapDomainAction1 *act1,
                              hlpMapDomainProfile *prof)
{
    fm_uint32       *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MAP_DOMAIN_ACTION0(camIndex, 0));

    act0->L2_DOMAIN = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_ACTION0, L2_DOMAIN);
    act0->L3_DOMAIN = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_ACTION0, L3_DOMAIN);
    act0->OPERATOR_ID = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_ACTION0, OPERATOR_ID);
    act0->UPDATE_DOMAINS = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_ACTION0, UPDATE_DOMAINS);
    act0->LEARN_EN = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_ACTION0, LEARN_EN);
    act0->LEARN_MODE = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_DOMAIN_ACTION0, LEARN_MODE);
    act0->priority_profile =
        FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_ACTION0, PRIORITY_PROFILE);
    act0->PRI_SOURCE = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_ACTION0, PRI_SOURCE);
    act0->DEFAULT_PRI = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_ACTION0, DEFAULT_PRI);

} *//* GetDomainRamEntry */


/*****************************************************************************/
/** LookupDomainTcam
 * \ingroup intModel
 *
 * \desc            Return highest match domain TCAM entry.
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys
 *
 * \param[in]       domainIndex points to caller-allocated structure in which
 *                  the function will return match index entry. If no match,
 *                  then match index is 0.
 *
 *****************************************************************************/
static void LookupDomainTcam(hlp_model   *model,
                             fm_uint16   *realignKeys,
                             fm_uint     *tcamIdx)
{
    hlp_modelState     *state;
    fm_int              i;
    hlpMapDomainTcam    domainTcam;
    fm_byte             port;
    fm_uint16           vid1;
    fm_uint16           vid2;
    fm_byte             vid1Valid;
    fm_byte             vid2Valid;
    fm_byte             maskPort;
    fm_uint16           maskVid1;
    fm_uint16           maskVid2;
    fm_byte             maskVid1Valid;
    fm_byte             maskVid2Valid;

    state = &model->packetState;


    port = state->RX_PORT;
    vid1 = FM_GET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 0, 12);
    vid2 = FM_GET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_VLAN2], 0, 12);
    vid1Valid = state->PA_FLAGS[HLP_PA_FLAGS_OTR_L2_VLAN1];
    vid2Valid = state->PA_FLAGS[HLP_PA_FLAGS_OTR_L2_VLAN2];

    WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "LookupDomainTcam: Port %d vid1 0x%x vid2 0x%x vid1_valid=%d vid2_valid=%d\n",
            port, vid1, vid2, vid1Valid, vid2Valid)
    //fm_log_debug(FM_LOG_CAT_PLATFORM,
    //    "LookupDomainTcam: Port %d vid1 0x%x vid2 0x%x vid1_valid=%d vid2_valid=%d\n",
    //        port, vid1, vid2, vid1Valid, vid2Valid);

    /* The highest numbered DOMAIN_CAM entry has highest precendence. */
    for (i = HLP_MAP_DOMAIN_TCAM_ENTRIES - 1; i >= 0; i--)
    {
        GetDomainTcamEntry(model, i, &domainTcam);

        maskPort = domainTcam.PORT_KEY ^ domainTcam.PORT_KEY_INVERT;
        maskVid1Valid = domainTcam.VID1_VALID ^ domainTcam.VID1_VALID_INVERT;
        maskVid1 = domainTcam.VID1_KEY ^ domainTcam.VID1_KEY_INVERT;
        maskVid2Valid = domainTcam.VID2_VALID ^ domainTcam.VID2_VALID_INVERT;
        maskVid2 = domainTcam.VID2_KEY ^ domainTcam.VID2_KEY_INVERT;

        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
        "LookupDomainTcam: i=%d Port %d vid1 0x%x vid2 0x%x vid1_valid=%d vid2_valid=%d\n",
            i, domainTcam.PORT_KEY, domainTcam.VID1_KEY, domainTcam.VID2_KEY, domainTcam.VID1_VALID, domainTcam.VID2_VALID);*/


        if ((((domainTcam.PORT_KEY & domainTcam.PORT_KEY_INVERT) == 0) &&
             ((port & maskPort) == (domainTcam.PORT_KEY & maskPort))) &&
             (((domainTcam.VID1_VALID & domainTcam.VID1_VALID_INVERT) == 0) &&
             ((vid1Valid & maskVid1Valid) == (domainTcam.VID1_VALID & maskVid1Valid))) &&
            (((domainTcam.VID1_KEY & domainTcam.VID1_KEY_INVERT) == 0) &&
             ((vid1 & maskVid1) == (domainTcam.VID1_KEY & maskVid1))) &&
            (((domainTcam.VID2_VALID & domainTcam.VID2_VALID_INVERT) == 0) &&
             ((vid2Valid & maskVid2Valid) == (domainTcam.VID2_VALID & maskVid2Valid))) &&
            (((domainTcam.VID2_KEY & domainTcam.VID2_KEY_INVERT) == 0) &&
             ((vid2 & maskVid2) == (domainTcam.VID2_KEY & maskVid2))) &&
            ((FM_GET_UNNAMED_FIELD(domainTcam._RSVD0_, 0, 6) == 0) &&
             (FM_GET_UNNAMED_FIELD(domainTcam._RSVD1_, 0, 6) == 0)) ) 
            //Checking on rsvd is because RTL checks 40b for tcam hit
        {
            WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Matched Domain TCAM Index = %d\n", i)
            //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
            //    "Matched Domain TCAM Index = %d\n", i);
            *tcamIdx = i;
            return;
        }
    }
    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "NO match on Domain TCAM. Index = 0\n")
    //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
    //            "NO match on Domain TCAM. Index = 0\n");

    *tcamIdx = 0; /* No match */
}


/*****************************************************************************/
/** GetTcFromPriSource
 * \ingroup intModel
 *
 * \desc            Return the TC value from priority sources.
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       priority_profile is the priority profile.
 *
 * \param[in]       domAct0 points to caller-allocated storage which contains
 *                  the associated domain action0 register value.
 *
 * \param[in]       realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys.
 *
 *****************************************************************************/
static fm_int GetTcFromPriSource(hlp_model *model,
                                 fm_byte    priority_profile,
                                 fm_uint32 *domAct0,
                                 fm_uint16 *realignKeys)
{
    hlp_modelState *state;
    fm_uint32 *regPtr;
    fm_byte  priSource;
    fm_bool  forceDefaultPri;
    fm_int   tc = -1;
    fm_uint  vpri;
    fm_uint  dscp;
    fm_uint  exp;
    fm_int   i;

    state = &model->packetState;

    priSource =
        FM_ARRAY_GET_FIELD(domAct0, HLP_MAP_DOMAIN_ACTION0, PRI_SOURCE);
    forceDefaultPri =
        FM_ARRAY_GET_BIT(domAct0, HLP_MAP_DOMAIN_ACTION0, FORCE_DEFAULT_PRI);

    for (i = 0; i < 4; i++)
    {
        switch (FM_GET_UNNAMED_FIELD(priSource, 6 - (i*2), 2))
        {
            case TC_SOURCE_VPRI:
                if (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L2_VLAN1])
                {
                    regPtr = FM_MODEL_GET_REG_PTR(model,
                        HLP_MAP_VPRI_TC(priority_profile, 0));
                    vpri = FM_GET_UNNAMED_FIELD(
                        realignKeys[HLP_RE_KEYS_OUTER_VLAN1], 12, 4);
                    tc = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, vpri*3, 3);
                }
                break;
            case TC_SOURCE_MPLS:
                if (state->PA_FLAGS[HLP_PA_FLAGS_OTR_MPLS_V])
                {
                    regPtr = FM_MODEL_GET_REG_PTR(model,
                        HLP_MAP_EXP_TC(priority_profile, 0));
                    exp = FM_GET_UNNAMED_FIELD(
                                 realignKeys[HLP_RE_KEYS_MPLS + 1],
                                 9,
                                 3);
                    tc = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, exp*3, 3);
                }
                break;
            case TC_SOURCE_DSCP:
                if (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V])
                {
                    dscp = FM_GET_UNNAMED_FIELD(
                                realignKeys[HLP_RE_KEYS_OUTER_IP_DS_FLOW], 10, 6);
                    regPtr = FM_MODEL_GET_REG_PTR(model,
                        HLP_MAP_DSCP_TC(((priority_profile << 6) | dscp), 0));
                    tc = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DSCP_TC, TC);
                    /* mkhan3: DEBG */
                    WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAP_DSCP_TC[pri_prof=0x%x; dscp=0x%x]: TC=%d\n", priority_profile, dscp, tc)
                }
                break;
            case TC_SOURCE_META:
                switch (state->PKT_META[HLP_MODEL_META_TYPE_OFF])
                {
                    case HLP_MODEL_META_TYPE_LAN_RX:
                    case HLP_MODEL_META_TYPE_DSI_RX:
                        tc = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META, 27, 3);
                        break;
                    case HLP_MODEL_META_TYPE_MARKER:
                        tc = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META, 21, 3);
                        break;
                    case HLP_MODEL_META_TYPE_DSI_TX:
                        tc = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META, 40, 3);
                        break;
                }
                break;
        }
        if (tc >= 0)
        {
            WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "pri_source[%d]=%d TC=%d\n", 3-i, FM_GET_UNNAMED_FIELD(priSource, 6 - (i*2), 2), tc)
            break;
        }
    }

    state->NO_PRI_ENC = 0;

    if ((forceDefaultPri) || (tc < 0))
    {
        tc = FM_ARRAY_GET_FIELD(domAct0, HLP_MAP_DOMAIN_ACTION0, DEFAULT_PRI);
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "forceDefaultPri=%d default TC=%d\n", forceDefaultPri, tc)
        state->NO_PRI_ENC = 1;
    }

    return tc;
} /* end GetTcFromPriSource */


/*****************************************************************************/
/** MapperScalar
 * \ingroup intModel
 *
 * \desc            Compress keys into smaller size, and compute map8 and
 *                  map16
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       portCfg is the hlpMapPortCfg structure in which
 *                  scalar_source will be used to locate realign keys
 *
 * \param[in]       realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys
 *
 * \param[in]       isIPv4 points to caller-allocated storage which tells if
 *                  the current packet header is IPv4 or not
 *
 * \param[in]       isIPv6 points to caller-allocated storage which tells if
 *                  the current packet header is IPv6 or not
 *
 * \param[in]       domainIndex is the index to the domain tcam.
 *
 * \param[out]      mapScenKey0 points to caller-allocated hlpMapScenKey0 for
 *                  later matching to MAP_CAM_KEY0.
 *
 * \param[out]      mapScenKey1 points to caller-allocated hlpMapScenKey0 for
 *                  later matching to MAP_CAM_KEY1. Most of the fields are
 *                  set here, except for the domain action.
 *
 * \param[out]      mappedKey points to caller-allocated hlp_modelMapKey where
 *                  the function returns the mapped key values.
 *
 * \param[in]       priorityProfile points to caller-allocated storage which
 *                  the function will return the priority profile.
 *
 *****************************************************************************/
static void MapperScalar(hlp_model        *model,
                         hlpMapPortCfg    portCfg,
                         fm_uint16        *realignKeys,
                         fm_bool          *isIPv4,
                         fm_bool          *isIPv6,
                         fm_uint          domainIndex,
                         fm_bool          ihl_ok,
                         fm_bool          ihl_fits,
                         hlpMapScenKey0   *mapScenKey0,
                         hlpMapScenKey1   *mapScenKey1,
                         hlp_modelMapKey  *mappedKey,
                         fm_uint          *priorityProfile)
{
    hlp_modelState        *state;
    fm_uint32             *regPtr, *regPtr0, *regPtr1;
    fm_uint32             *mapIpLoPtr, *mapIpHiPtr;
    fm_bool               oDmacMulticast = FALSE;
    fm_bool               oDmacBroadcast = FALSE;
    fm_int                i;
    fm_byte               valid;
    fm_byte               prot;
    fm_uint16             type;
    fm_uint16             length0;
    fm_uint16             length1;
    fm_macaddr            mac;
    fm_macaddr            keyMac = 0;
    fm_uint16             vid1;
    fm_uint16             vid2;
    fm_uint64             mask;
    /* Ignore Length */
    fm_int                igLen;
    /* Match Length */
    fm_int                matchLen;
    fm_uint64             loMask;
    fm_uint64             hiMask;
    fm_uint64             loIP;
    fm_uint64             hiIP;
    fm_uint64             headerIPhi;
    fm_uint64             headerIPlo;
    fm_uint32             temp;
    fm_uint32             currMap;
    fm_uint32             nextMap;
    fm_bool               is_ipv6;
    fm_bool               ip_fits;

    fm_byte               priority_profile;
    fm_uint16             l2_domain;
    fm_byte               l3_domain;
    fm_byte               operator_id;
    fm_byte               tc;
    fm_uint16             l2Policer;
    fm_uint16             l3Policer;

    hlp_modelFfuKeys     *ffuKeys;

    state = &model->packetState;

    headerIPhi = 0;
    headerIPlo = 0;

    regPtr   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_PORT(state->RX_PORT, 0));
    mappedKey->MAP_PORT = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PORT, MAP_PORT);

    /* Compress PROT to 3 bits, highest index match wins */
    for (i = 0; i < HLP_MAP_PROT_ENTRIES; i++)
    {
        regPtr    = FM_MODEL_GET_REG_PTR(model, HLP_MAP_PROT(i, 0));
        prot      = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PROT, PROT);

        if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V])
        {
            if ( prot == FM_GET_UNNAMED_FIELD(
                         realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT], 0, 8) )
            {
                mappedKey->MAP_INNER_PROT = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PROT, MAP_PROT);
                WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Index=%d inner_prot=0x%x MAP_OUTER_PROT=0x%x\n", i, FM_GET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT], 0, 8), mappedKey->MAP_INNER_PROT)

            }
        }

        if(state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V])
        {

            if ( prot == FM_GET_UNNAMED_FIELD(
                        realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8) )
            {
                mappedKey->MAP_OUTER_PROT = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_PROT, MAP_PROT);
                WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Index=%d outer_prot=0x%x MAP_OUTER_PROT=0x%x\n", i, FM_GET_UNNAMED_FIELD(realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8), mappedKey->MAP_OUTER_PROT)
            }
        }
    }

    /* Compress TYPE to 4 bits, highest index match wins */
    for (i = 0; i < HLP_MAP_TYPE_ENTRIES; i++)
    {
        regPtr   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_TYPE(i, 0));
        type     = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_TYPE, TYPE_XXX);
        valid    = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_TYPE, VALID);

        if ( (state->PA_FLAGS[HLP_PA_FLAGS_INR_L2_V]) &&
             ( (valid >> 1) & 1 ) &&
             ( type == realignKeys[HLP_RE_KEYS_INNER_ETYPE]) )
        {
            mappedKey->MAP_INNER_ETYPE = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_TYPE, MAP_TYPE);
            WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Index=%d inner_type=0x%x MAP_INNER_ETYPE=0x%x\n", i, realignKeys[HLP_RE_KEYS_INNER_ETYPE], mappedKey->MAP_INNER_ETYPE)
        }
        if ( ( valid & 1 ) &&
             ( type == realignKeys[HLP_RE_KEYS_OUTER_ETYPE]) )
        {
            mappedKey->MAP_OUTER_ETYPE = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_TYPE, MAP_TYPE);
            WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Index=%d outer_type=0x%x MAP_OUTER_ETYPE=0x%x\n", i, realignKeys[HLP_RE_KEYS_OUTER_ETYPE], mappedKey->MAP_OUTER_ETYPE)
        }
    }

    /* Map INNER LENGTH to 4 bits */
    /*if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V])
    {
        for (i = 0; i < HLP_MAP_LENGTH_ENTRIES - 1; i++)
        {
            regPtr0   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_LENGTH(i, 0));
            length0   = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_LENGTH, LENGTH);
            if ((i + 1) != HLP_MAP_LENGTH_ENTRIES)
            {
                regPtr1   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_LENGTH(i+1, 0));
                length1   = FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_LENGTH, LENGTH);
            }

            if (realignKeys[HLP_RE_KEYS_INNER_IP_LEN] >= length0 &&
                ((i + 1) == HLP_MAP_LENGTH_ENTRIES ||
                 realignKeys[HLP_RE_KEYS_INNER_IP_LEN] < length1))
            {
                mappedKey->MAP_INNER_L3_LENGTH =
                    FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_LENGTH, MAP_LENGTH);
            }
        }
    }*/

    /* Map OUTER LENGTH to 4 bits */
    /*if(state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V])
    {
        for (i = 0; i < HLP_MAP_LENGTH_ENTRIES - 1; i++)
        {
            regPtr0   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_LENGTH(i, 0));
            length0   = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_LENGTH, LENGTH);
            if ((i + 1) != HLP_MAP_LENGTH_ENTRIES)
            {
                regPtr1   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_LENGTH(i+1, 0));
                length1   = FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_LENGTH, LENGTH);
            }

            if (realignKeys[HLP_RE_KEYS_OUTER_IP_LEN] >= length0 &&
                ((i + 1) == HLP_MAP_LENGTH_ENTRIES ||
                  realignKeys[HLP_RE_KEYS_OUTER_IP_LEN] < length1))
            {
                mappedKey->MAP_OUTER_L3_LENGTH =
                    FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_LENGTH, MAP_LENGTH);*/
                /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "MAP OTR_LEN : entry %d
                 * match, len = 0x%x\n", i,
                 * realignKeys[HLP_RE_KEYS_OUTER_IP_LEN]);*/
      //      }
     //   }
    //}

    /* Compress SMAC and DMAC to 4 bits */
    /* outer DMAC */
    FM_SET_UNNAMED_FIELD64(keyMac,
                           0,
                           16,
                           realignKeys[HLP_RE_KEYS_OUTER_DMAC + 2]);
    FM_SET_UNNAMED_FIELD64(keyMac,
                           16,
                           16,
                           realignKeys[HLP_RE_KEYS_OUTER_DMAC + 1]);
    FM_SET_UNNAMED_FIELD64(keyMac,
                           32,
                           16,
                           realignKeys[HLP_RE_KEYS_OUTER_DMAC]);

    oDmacMulticast = fmModelIsMulticastMacAddress(keyMac);
    oDmacBroadcast = fmModelIsBroadcastMacAddress(keyMac);

    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Outer DMAC = 0x%llx\n", keyMac)

    for (i = HLP_MAP_MAC_ENTRIES - 1; i >= 0; i--)
    {
        regPtr    = FM_MODEL_GET_REG_PTR(model, HLP_MAP_MAC(i, 0));
        mask  = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) <<
                FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, IGNORE_LENGTH);
        mac   = FM_ARRAY_GET_FIELD64(regPtr, HLP_MAP_MAC, MAC);
        valid = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, VALID);

        if ( ( valid & 1) &&
             ( mac == (keyMac  & mask) ) )
        {
            mappedKey->MAP_OUTER_DMAC = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, MAP_MAC);
            FM_SET_UNNAMED_FIELD(mapScenKey1->MAC_ROUTABLE,
                             0,
                             1,
                             FM_ARRAY_GET_BIT(regPtr, HLP_MAP_MAC, MAC_ROUTABLE));
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Outer DMAC=0x%llx, hit=%d, map_outer_dmac=0x%x, mac_routable=%d\n", keyMac, i, mappedKey->MAP_OUTER_DMAC, mapScenKey1->MAC_ROUTABLE)

            break;
        }
    }

    /* outer SMAC */
    FM_SET_UNNAMED_FIELD64(keyMac,
                           0,
                           16,
                           realignKeys[HLP_RE_KEYS_OUTER_SMAC + 2]);
    FM_SET_UNNAMED_FIELD64(keyMac,
                           16,
                           16,
                           realignKeys[HLP_RE_KEYS_OUTER_SMAC + 1]);
    FM_SET_UNNAMED_FIELD64(keyMac,
                           32,
                           16,
                           realignKeys[HLP_RE_KEYS_OUTER_SMAC]);

    for (i = HLP_MAP_MAC_ENTRIES - 1; i >= 0; i--)
    {
        regPtr    = FM_MODEL_GET_REG_PTR(model, HLP_MAP_MAC(i, 0));
        mask  = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) <<
                FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, IGNORE_LENGTH);
        mac   = FM_ARRAY_GET_FIELD64(regPtr, HLP_MAP_MAC, MAC);
        valid = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, VALID);

        if ( ( (valid >> 1) & 1 ) &&
             (mac == (keyMac  & mask) ) )
        {
            mappedKey->MAP_OUTER_SMAC =
                FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, MAP_MAC);
            FM_SET_UNNAMED_FIELD(mapScenKey1->MAC_ROUTABLE,
                             1,
                             1,
                             FM_ARRAY_GET_BIT(
                                 regPtr, HLP_MAP_MAC, MAC_ROUTABLE));

            break;
        }
    }

    if (state->PA_FLAGS[HLP_PA_FLAGS_INR_L2_V])
    {
        /* inner DMAC */
        FM_SET_UNNAMED_FIELD64(keyMac,
                               0,
                               16,
                               realignKeys[HLP_RE_KEYS_INNER_DMAC + 2]);
        FM_SET_UNNAMED_FIELD64(keyMac,
                               16,
                               16,
                               realignKeys[HLP_RE_KEYS_INNER_DMAC + 1]);
        FM_SET_UNNAMED_FIELD64(keyMac,
                               32,
                               16,
                               realignKeys[HLP_RE_KEYS_INNER_DMAC]);

        for (i = HLP_MAP_MAC_ENTRIES - 1; i >= 0; i--)
        {
            regPtr    = FM_MODEL_GET_REG_PTR(model, HLP_MAP_MAC(i, 0));
            mask  = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) <<
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, IGNORE_LENGTH);
            mac   = FM_ARRAY_GET_FIELD64(regPtr, HLP_MAP_MAC, MAC);
            valid = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, VALID);

            if ( ( (valid >> 2) & 1 ) &&
                 (mac == (keyMac  & mask) ) )
            {
                mappedKey->MAP_INNER_DMAC =
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, MAP_MAC);
                FM_SET_UNNAMED_FIELD(mapScenKey1->MAC_ROUTABLE,
                                 2,
                                 1,
                                 FM_ARRAY_GET_BIT(
                                     regPtr, HLP_MAP_MAC, MAC_ROUTABLE));

                WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAP_INNER_DMAC: Index %d hit. MAP_DMAC = 0x%x, DMAC = 0x%llx, mac_routable=%d\n", i, mappedKey->MAP_INNER_DMAC, keyMac, mapScenKey1->MAC_ROUTABLE)
                break;
            }
        }

        /* inner SMAC */
        FM_SET_UNNAMED_FIELD64(keyMac,
                               0,
                               16,
                               realignKeys[HLP_RE_KEYS_INNER_SMAC + 2]);
        FM_SET_UNNAMED_FIELD64(keyMac,
                               16,
                               16,
                               realignKeys[HLP_RE_KEYS_INNER_SMAC + 1]);
        FM_SET_UNNAMED_FIELD64(keyMac,
                               32,
                               16,
                               realignKeys[HLP_RE_KEYS_INNER_SMAC + 0]);
        for (i = HLP_MAP_MAC_ENTRIES - 1; i >= 0; i--)
        {
            regPtr    = FM_MODEL_GET_REG_PTR(model, HLP_MAP_MAC(i, 0));
            mask  = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) <<
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, IGNORE_LENGTH);
            mac   = FM_ARRAY_GET_FIELD64(regPtr, HLP_MAP_MAC, MAC);
            valid = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, VALID);

            if ( ( (valid >> 3) & 1 ) &&
                 (mac == (keyMac & mask) ) )
            {
                mappedKey->MAP_INNER_SMAC =
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_MAC, MAP_MAC);
                FM_SET_UNNAMED_FIELD(mapScenKey1->MAC_ROUTABLE,
                                 3,
                                 1,
                                 FM_ARRAY_GET_BIT(
                                     regPtr, HLP_MAP_MAC, MAC_ROUTABLE));

               break;
            }

        }
    }

    /* Compress outer SIP to 4 bits */
    if (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V])
    {
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 3]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 2]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 1]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 0]);

        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 7]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 6]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 5]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_SIP + 4]);

        WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OUTER SIP LO = 0x%llx HI = 0x%llx\n", headerIPlo, headerIPhi)
        for (i = HLP_MAP_IP_CFG_ENTRIES - 1; i >= 0 ; i--)
        {
            regPtr  = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_CFG(i, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, VALID);
            is_ipv6 = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_IP_CFG, IS_IPV6);

            if ( ! ( (valid >> 1) & 1 ) )
            {
                continue;
            }
            if( ( ! is_ipv6 && isIPv6[0]) || (is_ipv6 && isIPv4[0]) )
            {
                continue;
            }

            matchLen  = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MATCH_LENGTH);
            igLen  = 128 - matchLen;
            loMask = igLen < 64 ? FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << igLen : 0;
            hiMask = igLen > 64 ?
                     ( (igLen == 128)?
                     0 : FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << (igLen - 64) ) :
                     FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF);

            mapIpLoPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_LO(i, 0));
            mapIpHiPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_HI(i, 0));
            loIP   = FM_ARRAY_GET_FIELD64(mapIpLoPtr, HLP_MAP_IP_LO, IP_LO);
            hiIP   = FM_ARRAY_GET_FIELD64(mapIpHiPtr, HLP_MAP_IP_HI, IP_HI);

            if ( (loIP == (headerIPlo & loMask) ) &&
                 (hiIP == (headerIPhi & hiMask) ) )
            {

                mappedKey->MAP_OUTER_SIP =
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MAP_IP);
                /* drive to scenario_key */
                FM_SET_UNNAMED_FIELD(mapScenKey1->IP_SCENARIO,
                                     0,
                                     2,
                                     FM_ARRAY_GET_FIELD(
                                         regPtr, HLP_MAP_IP_CFG, IP_SCENARIO)
                                         );
                WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OUTER SIP hit index %0d\n", i)
                break;
            }
        }
    }

    /* Compress inner SIP to 4 bits */
    if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V])
    {
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 3]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 2]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 1]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 0]);

        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 7]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 6]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 5]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_SIP + 4]);


        for (i = HLP_MAP_IP_CFG_ENTRIES - 1; i >= 0; i--)
        {
            regPtr  = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_CFG(i, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, VALID);
            is_ipv6 = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_IP_CFG, IS_IPV6);

            if ( ! ( (valid >> 3) & 1 ) )
            {
                continue;
            }
            if( ( ! is_ipv6 && isIPv6[1]) || (is_ipv6 && isIPv4[1]) )
            {
                continue;
            }

            matchLen  = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MATCH_LENGTH);
            igLen  = 128 - matchLen;
            loMask = igLen < 64 ? FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << igLen : 0;
            hiMask = igLen > 64 ?
                     ( (igLen == 128)?
                     0 : FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << (igLen - 64) ) :
                     FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF);

            mapIpLoPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_LO(i, 0));
            mapIpHiPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_HI(i, 0));
            loIP   = FM_ARRAY_GET_FIELD64(mapIpLoPtr, HLP_MAP_IP_LO, IP_LO);
            hiIP   = FM_ARRAY_GET_FIELD64(mapIpHiPtr, HLP_MAP_IP_HI, IP_HI);

            if ( (loIP == (headerIPlo & loMask) ) &&
                 (hiIP == (headerIPhi & hiMask) ) )
            {
                mappedKey->MAP_INNER_SIP =
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MAP_IP);
                /* drive to scenario_key */
               FM_SET_UNNAMED_FIELD(mapScenKey1->IP_SCENARIO,
                                     4,
                                     2,
                                     FM_ARRAY_GET_FIELD(
                                         regPtr, HLP_MAP_IP_CFG, IP_SCENARIO)
                                         );
               /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "INNER SIP hit index %0d\n",
                * i);*/
                break;

            }
        }
    }

    /* Compress outer DIP to 4 bits */
    if(state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V])
    {
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 3]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 2]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 1]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 0]);

        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 7]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 6]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 5]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_OUTER_DIP + 4]);

        WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OUTER DIP LO = 0x%llx HI = 0x%llx\n", headerIPlo, headerIPhi)
        /* Compress outer DIP to 4 bits */
        for (i = HLP_MAP_IP_CFG_ENTRIES - 1; i >= 0; i--)
        {
            regPtr  = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_CFG(i, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, VALID);
            is_ipv6 = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_IP_CFG, IS_IPV6);

            if ( ! (valid  & 1 ) )
            {
                continue;
            }
            if( ( ! is_ipv6 && isIPv6[0]) || (is_ipv6 && isIPv4[0]) )
            {
                continue;
            }

            matchLen  = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MATCH_LENGTH);
            igLen  = 128 - matchLen;
            loMask = igLen < 64 ? FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << igLen : 0;
            hiMask = igLen > 64 ?
                     ( (igLen == 128)?
                     0 : FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << (igLen - 64) ) :
                     FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF);

            mapIpLoPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_LO(i, 0));
            mapIpHiPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_HI(i, 0));
            loIP   = FM_ARRAY_GET_FIELD64(mapIpLoPtr, HLP_MAP_IP_LO, IP_LO);
            hiIP   = FM_ARRAY_GET_FIELD64(mapIpHiPtr, HLP_MAP_IP_HI, IP_HI);

            if ( (loIP == (headerIPlo & loMask) ) &&
                 (hiIP == (headerIPhi & hiMask) ) )
            {
                mappedKey->MAP_OUTER_DIP =
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MAP_IP);
                /* drive to scenario_key */
                FM_SET_UNNAMED_FIELD(mapScenKey1->IP_SCENARIO,
                                     2,
                                     2,
                                     FM_ARRAY_GET_FIELD(
                                         regPtr, HLP_MAP_IP_CFG, IP_SCENARIO)
                                         );
                WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OUTER DIP hit index %0d\n", i)
                break;
            }
        }
    }

    /* Compress inner DIP to 4 bits */
    if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V])
    {
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 3]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 2]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 1]);
        FM_SET_UNNAMED_FIELD64(headerIPhi,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 0]);

        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             0,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 7]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             16,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 6]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             32,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 5]);
        FM_SET_UNNAMED_FIELD64(headerIPlo,
                             48,
                             16,
                             realignKeys[HLP_RE_KEYS_INNER_DIP + 4]);

        /* Compress inner DIP to 4 bits */
        for (i = HLP_MAP_IP_CFG_ENTRIES - 1; i >= 0; i--)
        {
            regPtr  = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_CFG(i, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, VALID);
            is_ipv6 = FM_ARRAY_GET_BIT(regPtr, HLP_MAP_IP_CFG, IS_IPV6);

            if ( ! ( (valid >> 2) & 1 ) )
            {
                continue;
            }
            if( ( ! is_ipv6 && isIPv6[1]) || (is_ipv6 && isIPv4[1]) )
            {
                continue;
            }

            matchLen  = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MATCH_LENGTH);
            igLen  = 128 - matchLen;
            loMask = igLen < 64 ? FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << igLen : 0;
            hiMask = igLen > 64 ?
                     ( (igLen == 128)?
                     0 : FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << (igLen - 64) ) :
                     FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF);

            mapIpLoPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_LO(i, 0));
            mapIpHiPtr     = FM_MODEL_GET_REG_PTR(model, HLP_MAP_IP_HI(i, 0));
            loIP   = FM_ARRAY_GET_FIELD64(mapIpLoPtr, HLP_MAP_IP_LO, IP_LO);
            hiIP   = FM_ARRAY_GET_FIELD64(mapIpHiPtr, HLP_MAP_IP_HI, IP_HI);

            if ( (loIP == (headerIPlo & loMask) ) &&
                 (hiIP == (headerIPhi & hiMask) ) )
            {
                mappedKey->MAP_INNER_DIP =
                    FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_IP_CFG, MAP_IP);
                /* drive to scenario_key */
                FM_SET_UNNAMED_FIELD(mapScenKey1->IP_SCENARIO,
                                     6,
                                     2,
                                     FM_ARRAY_GET_FIELD(
                                         regPtr, HLP_MAP_IP_CFG, IP_SCENARIO)
                                         );
                /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "INNER DIP hit index %0d\n",
                 * i);*/
                break;
            }
        }
    }

    /* Map VLAN has been removed */
    /* Map OUTER L4 SRC ports */
    if(state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V])
    {
        temp = realignKeys[HLP_RE_KEYS_OUTER_L4SRC];
        mappedKey->MAP_OUTER_L4_SRC = realignKeys[HLP_RE_KEYS_OUTER_L4SRC];

        nextMap = 0;
        for (i = 0; i < HLP_MAP_L4_SRC_ENTRIES; i++)
        {
            regPtr0 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_SRC(i, 0));
            regPtr1 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_SRC(i+1, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, VALID);

            if ( ( valid & 1 ) &&
                (mappedKey->MAP_OUTER_PROT ==
                 FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, MAP_PROT) ) )
            {
                currMap = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, L4_SRC);

                if ( (i + 1) != HLP_MAP_L4_SRC_ENTRIES)
                {
                    nextMap = FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_SRC, L4_SRC);
                }

                if ( (currMap <= temp) &&
                     ( ( (i + 1) == HLP_MAP_L4_SRC_ENTRIES) ||
                       (mappedKey->MAP_OUTER_PROT !=
                        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_SRC, MAP_PROT) ) ||
                        temp < nextMap) )
                {
                    mappedKey->MAP_OUTER_L4_SRC =
                        FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, MAP_L4_SRC);
                }
            }
        }
    }

    /* Map INNER L4 SRC ports */
    if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V])
    {
        temp = realignKeys[HLP_RE_KEYS_INNER_L4SRC];
        mappedKey->MAP_INNER_L4_SRC = realignKeys[HLP_RE_KEYS_INNER_L4SRC];
        nextMap = 0;
        for (i = 0; i < HLP_MAP_L4_SRC_ENTRIES; i++)
        {
            regPtr0 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_SRC(i, 0));
            regPtr1 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_SRC(i+1, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, VALID);

            if ( ( (valid >> 1) & 1 ) &&
                (mappedKey->MAP_INNER_PROT ==
                 FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, MAP_PROT) ) )
            {
                currMap = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, L4_SRC);

                if ( (i + 1) != HLP_MAP_L4_SRC_ENTRIES)
                {
                    nextMap = FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_SRC, L4_SRC);
                }

                if ( (currMap <= temp) &&
                     ( ( (i + 1) == HLP_MAP_L4_SRC_ENTRIES) ||
                       (mappedKey->MAP_INNER_PROT !=
                        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_SRC, MAP_PROT) ) ||
                        temp < nextMap) )
                {
                    mappedKey->MAP_INNER_L4_SRC =
                        FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_SRC, MAP_L4_SRC);
                }
            }
        }
    }

    /* Map OUTER L4 DST ports */
    if(state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V])
    {
        temp = realignKeys[HLP_RE_KEYS_OUTER_L4DST];
        mappedKey->MAP_OUTER_L4_DST = realignKeys[HLP_RE_KEYS_OUTER_L4DST];
        nextMap = 0;
        for (i = 0; i < HLP_MAP_L4_DST_ENTRIES; i++)
        {
            regPtr0 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_DST(i, 0));
            regPtr1 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_DST(i+1, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, VALID);

            if ( ( valid & 1 ) &&
                (mappedKey->MAP_OUTER_PROT ==
                 FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, MAP_PROT) ) )
            {
                currMap =  FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, L4_DST);

                if ( (i + 1) != HLP_MAP_L4_DST_ENTRIES)
                {
                    nextMap = FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_DST, L4_DST);
                }

                if ( (currMap <= temp) &&
                     ( ( (i + 1) == HLP_MAP_L4_DST_ENTRIES) ||
                       (mappedKey->MAP_OUTER_PROT !=
                        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_DST, MAP_PROT) ) ||
                        temp < nextMap) )
                {
                    mappedKey->MAP_OUTER_L4_DST =
                        FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, MAP_L4_DST);
                    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "L4_DST = 0x%x, MAP_OUTER_L4_DST entry = %d match. MAP_OUTER_PROT = 0x%x, MAP_OUTER_L4_DST=0x%x\n", temp, i, mappedKey->MAP_OUTER_PROT, mappedKey->MAP_OUTER_L4_DST)
                }
            }
        }
    }

    /* Map INNER L4 DST ports */
    if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V])
    {
        temp = realignKeys[HLP_RE_KEYS_INNER_L4DST];
        mappedKey->MAP_INNER_L4_DST = realignKeys[HLP_RE_KEYS_INNER_L4DST];
        nextMap = 0;
        for (i = 0; i < HLP_MAP_L4_DST_ENTRIES; i++)
        {
            regPtr0 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_DST(i, 0));
            regPtr1 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_L4_DST(i+1, 0));
            valid   = FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, VALID);

            if ( ( (valid >> 1) & 1 )  &&
                (mappedKey->MAP_INNER_PROT ==
                 FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, MAP_PROT) ) )
            {
                currMap =  FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, L4_DST);

                if ( (i + 1) != HLP_MAP_L4_DST_ENTRIES)
                {
                    nextMap = FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_DST, L4_DST);
                }

                if ( (currMap <= temp) &&
                     ( ( (i + 1) == HLP_MAP_L4_DST_ENTRIES) ||
                       (mappedKey->MAP_INNER_PROT !=
                        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_L4_DST, MAP_PROT) ) ||
                        temp < nextMap) )
                {
                    mappedKey->MAP_INNER_L4_DST =
                        FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_L4_DST, MAP_L4_DST);
                }
            }
        }
    }

    /* Drive scenario_key_t */
    FM_SET_UNNAMED_FIELD(mapScenKey0->EX,
                         0,
                         1,
                         state->PA_EX_PARSING_DONE);
    FM_SET_UNNAMED_FIELD(mapScenKey0->EX,
                         1,
                         1,
                         state->PA_EX_TRUNC_HEADER);
    FM_SET_UNNAMED_FIELD(mapScenKey0->EX,
                         2,
                         1,
                         state->PA_EX_DEPTH_EXCEED);

    //ip_fits = otr_l3_v && (otr_l4_ptr - otr_l3_ptr <= 56);
    ip_fits = state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V] &
        state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V] &
        ((state->PA_PTRS[HLP_PA_PTRS_OTR_L4_PTR] -
          state->PA_PTRS[HLP_PA_PTRS_OTR_L3_PTR]) <= 56);
    FM_SET_UNNAMED_FIELD(mapScenKey0->IP_FITS,
                         0,
                         1,
                         ip_fits);
    //ip_fits = inr_l3_v && (inr_l4_ptr - inr_l3_ptr <= 56);
    ip_fits = state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V] &
        state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V] &
        ((state->PA_PTRS[HLP_PA_PTRS_INR_L4_PTR] -
          state->PA_PTRS[HLP_PA_PTRS_INR_L3_PTR]) <= 56);
    FM_SET_UNNAMED_FIELD(mapScenKey0->IP_FITS,
                         1,
                         1,
                         ip_fits);

    mapScenKey0->IHL_OK = ihl_ok;
    mapScenKey0->IHL_FITS = ihl_fits;

    for( i = 1; i <= HLP_MODEL_N_PARSER_FLAGS; i++)
    {
        FM_SET_UNNAMED_FIELD64(mapScenKey0->FLAGS,
                             i-1,
                             1,
                             state->PA_FLAGS[i]);
    }
    //mapScenKey0->CSUM = state->PA_CSUM_OK;
    for( i = 0; i < HLP_MODEL_N_IS_IP_BITS; i++)
    {
        FM_SET_UNNAMED_FIELD64(mapScenKey0->CSUM,
                             i,
                             1,
                             ( FM_GET_UNNAMED_FIELD(
                               state->PA_CSUM_OK,
                               i,
                               1)
                               | ( !isIPv4[i] ) ) );

        FM_SET_UNNAMED_FIELD64(mapScenKey0->IP_IS_V6,
                             i,
                             1,
                             isIPv6[i]);
    }


    mapScenKey1->METADATA_TYPE = state->PKT_META[HLP_MODEL_META_TYPE_OFF];
    switch (state->PKT_META[0])
    {
        case HLP_MODEL_META_TYPE_DSI_RX:
            mapScenKey1->METADATA_FLAGS =
                FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META,
                                            4*32 + 16 + 5, 11);
            break;
        case HLP_MODEL_META_TYPE_DSI_TX:
            mapScenKey1->METADATA_FLAGS =
                FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)state->PKT_META,
                                            5*32 + 16, 8);
            break;
        default:
            mapScenKey1->METADATA_FLAGS = 0;
            break;
    }
    mapScenKey1->PORT_SCENARIO = portCfg.PORT_SCENARIO;
    FM_SET_UNNAMED_FIELD(mapScenKey1->MAC_MBCAST,
                         0,
                         1,
                         oDmacMulticast);
    FM_SET_UNNAMED_FIELD(mapScenKey1->MAC_MBCAST,
                         1,
                         1,
                         oDmacBroadcast);

    ffuKeys    = &state->FFU_KEYS;

    //mapScenKey1->L2_DOMAIN = 0;
    //mapScenKey1->L3_DOMAIN = 0;

    /* domain TCAM - domainIndex is always valid here */
    regPtr0 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_DOMAIN_ACTION0(domainIndex, 0));

    if (FM_ARRAY_GET_BIT(regPtr0, HLP_MAP_DOMAIN_ACTION0, UPDATE_DOMAINS))
    {
        l2_domain =
            FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_DOMAIN_ACTION0, L2_DOMAIN);
        l3_domain =
            FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_DOMAIN_ACTION0, L3_DOMAIN);
        operator_id =
            FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_DOMAIN_ACTION0, OPERATOR_ID);
        priority_profile =
            FM_ARRAY_GET_FIELD(regPtr0, HLP_MAP_DOMAIN_ACTION0, PRIORITY_PROFILE);
        WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "Domain Action : l2_domain=0x%x, l3_domain=0x%x, operator_id=0x%x, priority_profile=%d \n", l2_domain, l3_domain, operator_id, priority_profile)

        FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                    0,
                                    4,
                                    operator_id);
        FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                    4,
                                    9,
                                    l2_domain);
        FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                    13,
                                    6,
                                    l3_domain);

        /* Set these only for certain ONPI Type */
        /*switch (state->PKT_META[0])
        {
            case HLP_MODEL_META_TYPE_LAN_RX:
            case HLP_MODEL_META_TYPE_DSI_RX:
                 FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                      0,
                                      4,
                                      operator_id);
                 FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                      4,
                                      9,
                                      l2_domain);
                 FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                      13,
                                      6,
                                      l3_domain);

                 mapScenKey1->L2_DOMAIN = l2_domain;
                 mapScenKey1->L3_DOMAIN = l3_domain;
                break;
            case HLP_MODEL_META_TYPE_DSI_TX:
                 FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                      0,
                                      4,
                                      operator_id);
                 FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                      4,
                                      9,
                                      l2_domain);
                 mapScenKey1->L2_DOMAIN = l2_domain;
                break;
        }*/

    }
    else
    {

        l2_domain = FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                      4,
                                      9);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MAP_DOMAIN_PROFILE(l2_domain, 0));
        priority_profile =
            FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DOMAIN_PROFILE, PRIORITY_PROFILE);
        WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "priority_profile=%d from MAP_DOMAIN_PROFILE[%d]\n", priority_profile, l2_domain);
    }

    state->PRIORITY_PROFILE = priority_profile;

    mapScenKey1->L2_DOMAIN =
       FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                         4,
                         9);
    mapScenKey1->L3_DOMAIN =
       FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                         13,
                         6);

    /*
    switch (state->PKT_META[HLP_MODEL_META_TYPE_OFF])
    {
        case HLP_MODEL_META_TYPE_LAN_RX:
            if(FM_ARRAY_GET_BIT(regPtr0, HLP_MAP_DOMAIN_ACTION0, UPDATE_DOMAINS))
            {
                mapScenKey1->L2_DOMAIN =
                   FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                     4,
                                     9);
                mapScenKey1->L3_DOMAIN =
                   FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                     13,
                                     6);
             }
             break;
        case HLP_MODEL_META_TYPE_DSI_RX:
            mapScenKey1->L2_DOMAIN =
                FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                  4,
                                  9);
             mapScenKey1->L3_DOMAIN =
                FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                  13,
                                  6);
            break;
        case HLP_MODEL_META_TYPE_DSI_TX:
             mapScenKey1->L2_DOMAIN =
                FM_ARRAY_GET_UNNAMED_FIELD((fm_uint32*)(state->PKT_META + 5*4),
                                  4,
                                  9);
            break;
    }
    */

    state->FFU_ACTIONS.act1[HLP_MODEL_FFU_ACTION_LEARN].val =
        FM_ARRAY_GET_BIT(regPtr0, HLP_MAP_DOMAIN_ACTION0, LEARN_EN);
    state->LEARN_MODE =
        FM_ARRAY_GET_BIT(regPtr0, HLP_MAP_DOMAIN_ACTION0, LEARN_MODE);

    tc = GetTcFromPriSource(model, priority_profile, regPtr0, realignKeys);

    state->FFU_ACTIONS.act4[HLP_MODEL_FFU_ACTION_TC].val = tc;

    regPtr1 = FM_MODEL_GET_REG_PTR(model, HLP_MAP_DOMAIN_ACTION1(domainIndex, 0));
    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "domain_action1[%d] addr=0x%x, val=0x%x\n", domainIndex, HLP_MAP_DOMAIN_ACTION1(domainIndex, 0), *regPtr1)
    mapScenKey1->DOMAIN_SCENARIO =
        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_DOMAIN_ACTION1, DOMAIN_SCENARIO);

    l2Policer =
        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_DOMAIN_ACTION1, L2_POLICER);
    l3Policer =
        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_DOMAIN_ACTION1, L3_POLICER);
    state->L2_IVLAN1_CNT_INDEX =
        FM_ARRAY_GET_FIELD(regPtr1, HLP_MAP_DOMAIN_ACTION1, VLAN_COUNTER);

    regPtr1 =  FM_MODEL_GET_REG_PTR(model, HLP_MAP_DOMAIN_POL_CFG(0));

    if(l2Policer != 0)
    {
        //if l2_policer is nonzero, then the default POLICER[0] action is
        //(bank=0, index=l2_policer).
        state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER0].val = 0;
        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER0].val,
                23,
                1,
                1);

        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER0].val,
                20,
                3,
                FM_ARRAY_GET_FIELD(
                    regPtr1,
                    HLP_MAP_DOMAIN_POL_CFG, L2_COLOR_CFG));

        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER0].val,
                0,
                12,
                (l2Policer & 0xFFF));

    }

    if(l3Policer != 0)
    {
        //if l3_policer is nonzero, then the default POLICER[1] action is
        //(bank=5, index=l3_policer).
        state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER1].val = 0;
        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER1].val,
                23,
                1,
                1);

        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER1].val,
                0,
                12,
                (l3Policer & 0xFFF));

        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER1].val,
                20,
                3,
                FM_ARRAY_GET_FIELD(
                    regPtr1,
                    HLP_MAP_DOMAIN_POL_CFG, L3_COLOR_CFG));

        FM_SET_UNNAMED_FIELD(
                state->FFU_ACTIONS.act24[HLP_MODEL_FFU_ACTION_POLICER1].val,
                16,
                4,
                5);
    }

    /* Write PKT_META to keys: MD write block */
    for (i = 0; i < 32; i++)
    {
        ffuKeys->key8[63-i] = state->PKT_META[i];
    }

    *priorityProfile = priority_profile;

} /* end MapperScalar */

/*****************************************************************************/
/** LenEncode
 * \ingroup intModel
 *
 * \desc            Encode one field of parser info
 *
 * \param[in]       hdrLen is number of bytes found by oarser. can be <0
 *
 * \param[in]       hdrValid is set if parser found header
 *
 * \param[in]       min is the min number of legal bytes
 *
 * \param[in]       max is the max number of bytes per config
 *
 * \param[in]       offset is the number of bytes encoded as "0" in parser info
 *
 * \param[out]      len points the len field of parser info
 *
 * \param[inout]    fits points to the status of accumulating header-fits
 *
 * \param[inout]    ptrsErr points to the status of accumulating encode-error
 *
 *****************************************************************************/
static void LenEncode(fm_byte  hdrLen,
                      fm_bool  hdrValid,
                      fm_byte  min,
                      fm_byte  max,
                      fm_byte  offset,
                      fm_byte *len,
                      fm_bool *fits,
                      fm_bool *ptrsErr)
{
    fm_byte temp;

    if ((max < min) || (max < offset))
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: LenEncode max is less than min, or max is less than offset!\n")
    }

    if(!(*fits) || (*ptrsErr) || !hdrValid)
    {
        *len = 0;
    }
    else if (hdrLen > max)
    {
        *len  = ((max >> 2) & 0x3F) - ((offset >> 2) & 0x3F);
        *fits = 0;
    }
    else if (hdrLen < min)
            //((hdrLen & 0x3) != (offset & 0x3)))
    {
        *len     = 0;
        *ptrsErr = 1;
    }
    else if ((FM_GET_UNNAMED_FIELD(hdrLen, 0, 2)
             != FM_GET_UNNAMED_FIELD(offset, 0, 2)))
    {
        temp = hdrLen - offset;
        *len = (temp >> 2) & 0x3F;
        *fits = 0;
    }
    else
    {
        *len = ((hdrLen >> 2) & 0x3F) - ((offset >> 2) & 0x3F);
    }

    WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "hdrLen=0x%x, hdrValid=%0d, min=0x%x, max=0x%x, offset=0x%x, len=0x%x, fits=%0d, ptrsErr=%0d\n", hdrLen, hdrValid, min, max, offset, *len, *fits, *ptrsErr)
} /* LenEncode */

/*****************************************************************************/
/** GetParserInfo
 * \ingroup intModel
 *
 * \desc            Compute parser_info which will be used in Modify
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys
 *
 * \param[in]       isIPv4 points to caller-allocated storage which tells if
 *                  the current packet header is IPv4 or not
 *
 * \param[in]       isIPv6 points to caller-allocated storage which tells if
 *                  the current packet header is IPv6 or not
 *
 *****************************************************************************/
static void GetParserInfo(hlp_model            *model,
                          fm_uint16            *realignKeys,
                          //fm_bool              *isIPv4,
                          fm_bool              *isIPv6,
                          fm_bool              *ptrsErr)
{
    hlp_modelState       *state;
    hlp_modelParserInfo  *parserInfo;
    fm_uint32            *regPtr;
    fm_byte               ptrs[8];
    fm_bool               hdrValid[8];
    fm_byte               hdrLen[8];
    fm_byte               hdrLenLimit[8];
    fm_byte               outerProt, innerProt;
    fm_bool               tcp[2];
    fm_bool               udp[2];
    fm_int                i;
    fm_byte               nextHdrStart;
    fm_bool               fits = 1;
    //fm_bool               ptrsErr = 0;
    fm_byte               otrL4Len;
    fm_byte               otrL4Min;
    fm_byte               otrL4Offset;
    fm_byte               inrL4Len;

    //FM_NOT_USED(isIPv4);

    state      = &model->packetState;
    parserInfo = &state->PARSER_INFO;

    parserInfo->window_parse_v = state->PA_FLAGS[HLP_PA_FLAGS_WINDOW_PARSE_V];

    /*if(state->PA_FLAGS[HLP_PA_FLAGS_WINDOW_PARSE_V])
    {
        FM_CLEAR(state->PARSER_INFO);
        return;
    }*/
    outerProt = FM_GET_UNNAMED_FIELD(
                    realignKeys[HLP_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8);

    tcp[0] = (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V]) && (outerProt == HLP_MODEL_PROT_TCP);
    udp[0] = (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V]) && (outerProt == HLP_MODEL_PROT_UDP);

    innerProt = FM_GET_UNNAMED_FIELD(
                    realignKeys[HLP_RE_KEYS_INNER_IP_TTL_PROT], 0, 8);

    tcp[1] = (state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V]) && (innerProt == HLP_MODEL_PROT_TCP);
    udp[1] = (state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V]) && (innerProt == HLP_MODEL_PROT_UDP);

    ptrs[HLP_MODEL_PA_INFO_OTR_L2] = 0;
    for(i = HLP_MODEL_PA_INFO_OTR_MPLS; i <= HLP_MODEL_PA_INFO_INR_L4; i++)
    {
        ptrs[i] = state->PA_PTRS[i];
    }

    hdrValid[HLP_MODEL_PA_INFO_OTR_L2]   = (state->PA_FLAGS[HLP_PA_FLAGS_WINDOW_PARSE_V] == 1) ? 0 : 1;
    hdrValid[HLP_MODEL_PA_INFO_OTR_MPLS] = state->PA_FLAGS[HLP_PA_FLAGS_OTR_MPLS_V];
    hdrValid[HLP_MODEL_PA_INFO_OTR_L3]   = state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V];
    hdrValid[HLP_MODEL_PA_INFO_OTR_L4]   = state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V];
    hdrValid[HLP_MODEL_PA_INFO_INR_L2]   = state->PA_FLAGS[HLP_PA_FLAGS_INR_L2_V];
    hdrValid[HLP_MODEL_PA_INFO_INR_MPLS] = state->PA_FLAGS[HLP_PA_FLAGS_INR_MPLS_V];
    hdrValid[HLP_MODEL_PA_INFO_INR_L3]   = state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V];
    hdrValid[HLP_MODEL_PA_INFO_INR_L4]   = state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V];

    nextHdrStart = state->PA_ADJ_SEG_LEN;
    for(i = HLP_MODEL_PA_INFO_INR_L4; i >= HLP_MODEL_PA_INFO_OTR_L2; i--)
    {
        hdrLen[i] = nextHdrStart - ptrs[i];
        WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "hdrLen[%0d]=0x%x, nextHdrStart=0x%x, ptrs=0x%x\n",i, hdrLen[i], nextHdrStart, ptrs[i])
        //if(hdrValid[i] || ((i == HLP_MODEL_PA_INFO_INR_L4) &&
        //  state->PA_PTRS_VALID[HLP_PA_PTRS_INR_L4_PTR]))
        if(state->PA_PTRS_VALID[i])
        {
            nextHdrStart = ptrs[i];
        }
    }

    /* The parser always sets pr.ptrs[OTR_L3] to indicate the end of the MPLS
       stack (either BOS or the point where the parser has decided to stop
       looking deeper in the MPLS stack).
       parser_info will require pr.ptrs[OTR_L3] to determine the end of the
       MPLS stack in cases where the IP header is not recognized or has an EOS
       exception, or the MPLS stack was not fully parsed. */
    //if (hdrValid[HLP_MODEL_PA_INFO_OTR_MPLS] &&
    //   //!hdrValid[HLP_MODEL_PA_INFO_OTR_L3]   &&
    //   state->PA_PTRS_VALID[HLP_PA_PTRS_OTR_L3_PTR])
    //{
    //    hdrLen[HLP_MODEL_PA_INFO_OTR_MPLS] =
    //        state->PA_PTRS[HLP_PA_PTRS_OTR_L3_PTR] -
    //        state->PA_PTRS[HLP_PA_PTRS_OTR_MPLS_PTR];
    //}
    //else
    //{
    //    hdrLen[HLP_MODEL_PA_INFO_OTR_MPLS] = 0;
    //}

    //if (hdrValid[HLP_MODEL_PA_INFO_INR_MPLS] &&
    //   //!hdrValid[HLP_MODEL_PA_INFO_INR_L3]   &&
    //   state->PA_PTRS_VALID[HLP_PA_PTRS_INR_L3_PTR])
    //{
    //    hdrLen[HLP_MODEL_PA_INFO_INR_MPLS] =
    //        state->PA_PTRS[HLP_PA_PTRS_INR_L3_PTR] -
    //        state->PA_PTRS[HLP_PA_PTRS_INR_MPLS_PTR];
    //}
    //else
    //{
    //    hdrLen[HLP_MODEL_PA_INFO_INR_MPLS] = 0;
    //}

    /* maximum number of bytes per config */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_MAP_LEN_LIMIT(state->RX_PORT, 0));
    hdrLenLimit[HLP_MODEL_PA_INFO_OTR_L2] =
       FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_LEN_LIMIT, OTR_L2_LEN_LIMIT) * 4 + 14;
    hdrLenLimit[HLP_MODEL_PA_INFO_OTR_MPLS] =
       FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_LEN_LIMIT, OTR_MPLS_LEN_LIMIT) * 4;
    hdrLenLimit[HLP_MODEL_PA_INFO_OTR_L3] = HLP_MODEL_OTR_L3_LEN_LIMIT * 4;
    hdrLenLimit[HLP_MODEL_PA_INFO_OTR_L4] = tcp[0] ? HLP_MODEL_L4_TCP_MIN_SIZE :
        HLP_MODEL_OTR_TUN_LEN_LIMIT * 4;
    hdrLenLimit[HLP_MODEL_PA_INFO_INR_L2] =
       FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_LEN_LIMIT, INR_L2_LEN_LIMIT) * 4 + 14;
    hdrLenLimit[HLP_MODEL_PA_INFO_INR_MPLS] =
       FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_LEN_LIMIT, INR_MPLS_LEN_LIMIT) * 4;
    hdrLenLimit[HLP_MODEL_PA_INFO_INR_L3] = HLP_MODEL_INR_L3_LEN_LIMIT * 4;
    hdrLenLimit[HLP_MODEL_PA_INFO_INR_L4] = tcp[1] ? HLP_MODEL_L4_TCP_MIN_SIZE :
       HLP_MODEL_L4_MIN_SIZE;

    /* otr_l2 */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_OTR_L2],
              hdrValid[HLP_MODEL_PA_INFO_OTR_L2],
              14,
              hdrLenLimit[HLP_MODEL_PA_INFO_OTR_L2],
              10,
              &parserInfo->otr_l2_len,
              &fits,
              ptrsErr);
    parserInfo->otr_l2_len &= 0x7;
    parserInfo->otr_l2_vlan1    =
        (parserInfo->otr_l2_len != 0) && state->PA_FLAGS[HLP_PA_FLAGS_OTR_L2_VLAN1];
    parserInfo->otr_l2_vlan2    =
        (parserInfo->otr_l2_len != 0) && state->PA_FLAGS[HLP_PA_FLAGS_OTR_L2_VLAN2];
    parserInfo->otr_l2_v2first  =
        (parserInfo->otr_l2_len != 0) && state->PA_FLAGS[HLP_PA_FLAGS_OTR_L2_V2FIRST];
    if (parserInfo->otr_l2_len > 5)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->otr_l2_len=%0d!\n", parserInfo->otr_l2_len)
    }


    /* otr_mpls */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_OTR_MPLS],
              hdrValid[HLP_MODEL_PA_INFO_OTR_MPLS],
              0,
              hdrLenLimit[HLP_MODEL_PA_INFO_OTR_MPLS],
              0,
              &parserInfo->otr_mpls_len,
              &fits,
              ptrsErr);
    parserInfo->otr_mpls_len &= 0x7;
    if (parserInfo->otr_mpls_len > 7)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->otr_mpls_len=%0d!\n", parserInfo->otr_mpls_len)
    }


    /* otr_l3 */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_OTR_L3],
              hdrValid[HLP_MODEL_PA_INFO_OTR_L3],
              20,
              hdrLenLimit[HLP_MODEL_PA_INFO_OTR_L3],
              0,
              &parserInfo->otr_l3_len,
              &fits,
              ptrsErr);
    parserInfo->otr_l3_len &= 0xF;
    parserInfo->otr_l3_v6 = isIPv6[0];
    if (parserInfo->otr_l3_len > 14)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->otr_l3_len=%0d!\n", parserInfo->otr_l3_len)
    }


    /* otr_l4 */
    parserInfo->otr_l4_udp
        = fits && (*ptrsErr == 0) && (hdrLen[HLP_MODEL_PA_INFO_OTR_L4] >= 8)
        && state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V] && udp[0];
    parserInfo->otr_l4_tcp
        = fits && (*ptrsErr == 0) && (hdrLen[HLP_MODEL_PA_INFO_OTR_L4] >= 18)
        && state->PA_FLAGS[HLP_PA_FLAGS_OTR_L4_V] && tcp[0];
    otrL4Min = tcp[0] ? HLP_MODEL_L4_TCP_MIN_SIZE : (udp[0] ? HLP_MODEL_L4_MIN_SIZE : 4);
    otrL4Offset = tcp[0] ? HLP_MODEL_L4_TCP_MIN_SIZE : (udp[0] ? HLP_MODEL_L4_MIN_SIZE : 0);
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_OTR_L4],
              hdrValid[HLP_MODEL_PA_INFO_OTR_L4],
              otrL4Min,
              hdrLenLimit[HLP_MODEL_PA_INFO_OTR_L4],
              otrL4Offset,
              &parserInfo->otr_tun_len,
              &fits,
              ptrsErr);
    parserInfo->otr_tun_len &= 0x1F;
    if (parserInfo->otr_tun_len > 18)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->otr_tun_len=%0d!\n", parserInfo->otr_tun_len)
    }


    /* stop if Outer L4 is present and not UDP */
    fits &= udp[0] || (!hdrValid[HLP_MODEL_PA_INFO_OTR_L4]);

    /* inr_l2 */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_INR_L2],
              hdrValid[HLP_MODEL_PA_INFO_INR_L2],
              14,
              hdrLenLimit[HLP_MODEL_PA_INFO_INR_L2],
              10,
              &parserInfo->inr_l2_len,
              &fits,
              ptrsErr);
    parserInfo->inr_l2_len &= 0x7;
    parserInfo->inr_l2_vlan1   =
        (parserInfo->inr_l2_len != 0) && state->PA_FLAGS[HLP_PA_FLAGS_INR_L2_VLAN1];
    parserInfo->inr_l2_vlan2   =
        (parserInfo->inr_l2_len != 0) && state->PA_FLAGS[HLP_PA_FLAGS_INR_L2_VLAN2];
    parserInfo->inr_l2_v2first =
        (parserInfo->inr_l2_len != 0) && state->PA_FLAGS[HLP_PA_FLAGS_INR_L2_V2FIRST];
    if (parserInfo->inr_l2_len > 5)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->inr_l2_len=%0d!\n", parserInfo->inr_l2_len)
    }

    /* inr_mpls */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_INR_MPLS],
              hdrValid[HLP_MODEL_PA_INFO_INR_MPLS],
              0,
              hdrLenLimit[HLP_MODEL_PA_INFO_INR_MPLS],
              0,
              &parserInfo->inr_mpls_len,
              &fits,
              ptrsErr);
    parserInfo->inr_mpls_len &= 0x7;
    if (parserInfo->inr_mpls_len > 7)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->inr_mpls_len=%0d!\n", parserInfo->inr_mpls_len)
    }

    /* inr_l3 */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_INR_L3],
              hdrValid[HLP_MODEL_PA_INFO_INR_L3],
              20,
              hdrLenLimit[HLP_MODEL_PA_INFO_INR_L3],
              0,
              &parserInfo->inr_l3_len,
              &fits,
              ptrsErr);
    parserInfo->inr_l3_len &= 0xF;
    parserInfo->inr_l3_v6 = isIPv6[1];
    if (parserInfo->inr_l3_len > 14)
    {
        WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAPPER_WM_WARNING: illegal parserInfo->inr_l3_len=%0d!\n", parserInfo->inr_l3_len)
    }

    /* inr_l4 */
    LenEncode(hdrLen[HLP_MODEL_PA_INFO_INR_L4],
              hdrValid[HLP_MODEL_PA_INFO_INR_L4],
              (tcp[1] ? HLP_MODEL_L4_TCP_MIN_SIZE :
              HLP_MODEL_L4_MIN_SIZE),
              hdrLenLimit[HLP_MODEL_PA_INFO_INR_L4],
              0,
              &inrL4Len,
              &fits,
              ptrsErr);
    parserInfo->inr_l4_udp
        = inrL4Len && state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V] && udp[1];
    parserInfo->inr_l4_tcp
        = inrL4Len && state->PA_FLAGS[HLP_PA_FLAGS_INR_L4_V] && tcp[1];

} /* GetParserInfo */


/*****************************************************************************/
/** GetScenario
 * \ingroup intModel
 *
 * \desc            Computes scenario from the scenario CAM
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       key0 points to caller-allocated hlpMapScenKey0
 *                  structure for matching.
 *
 * \param[in]       key1 points to caller-allocated hlpMapScenKey1
 *                  structure for matching.
 *
 * \param[out]      mapScenAction points to the caller-allocated structure in
 *                  which scenario will be driven to FFU, and rewrite_cfg will
 *                  be used to rewrite key
 *
 *****************************************************************************/
static void GetScenario(hlp_model            *model,
                        fm_uint16            *realignKeys,
                        hlpMapScenKey0       key0,
                        hlpMapScenKey1       key1,
                        hlpMapScenAction     *mapScenAction)
                        //fm_byte              *rewriteProfile)
{
    hlp_modelState     *state;
    fm_uint32          *scenKeyPtr, *scenKeyPtr1;
    fm_uint32          *scenMaskPtr, *scenMaskPtr1;
    fm_uint32          *scenActionPtr;
    fm_int              i;
    fm_byte             scenarioIdx;
    fm_byte             trigIdx;
    fm_byte             priosIdx;
    hlp_modelFfuActions *ffuActions;

    FM_NOT_USED(realignKeys);

    state     = &model->packetState;

    ffuActions = &state->FFU_ACTIONS;


    scenarioIdx = 0;
    trigIdx = 0;
    priosIdx = 0;

    WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER,
            "Scenario Key0 :  ptrs_err=%d, ex=%d, flags=0x%llx, csum=%d, "
            "ip_is_v6=%d, ip_fits=%d, ihl_ok=%d ihl_fits=%d\n",
            key0.PTRS_ERR, key0.EX, key0.FLAGS,
            key0.CSUM, key0.IP_IS_V6,
            key0.IP_FITS, key0.IHL_OK, key0.IHL_FITS)
    WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER,
            "Scenario Key1 :  meta_type=0x%x, meta_flags=0x%x, "
            "l2_domain=0x%x, l3_domain=0x%x "
            "ip_scenario=0x%x, port_scenario=0x%x "
            "domain_scenario=0x%x, mac_routable=0x%x "
            "mac_mbcast=%d\n",
            key1.METADATA_TYPE, key1.METADATA_FLAGS,
            key1.L2_DOMAIN, key1.L3_DOMAIN,
            key1.IP_SCENARIO, key1.PORT_SCENARIO,
            key1.DOMAIN_SCENARIO, key1.MAC_ROUTABLE,
            key1.MAC_MBCAST)

    for( i = HLP_MAP_SCEN_KEY0_ENTRIES - 1; i >= 0; i--)
    {
        scenKeyPtr    = FM_MODEL_GET_REG_PTR(model, HLP_MAP_SCEN_KEY0(i, 0));
        scenMaskPtr   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_SCEN_KEY_INVERT0(i, 0));
        scenKeyPtr1   = FM_MODEL_GET_REG_PTR(model, HLP_MAP_SCEN_KEY1(i, 0));
        scenMaskPtr1  = FM_MODEL_GET_REG_PTR(model, HLP_MAP_SCEN_KEY_INVERT1(i, 0));
        scenActionPtr = FM_MODEL_GET_REG_PTR(model, HLP_MAP_SCEN_ACTION(i, 0));

        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
            "MAP_SCEN cam index = %d\n", i);

        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
            "Scenario Key0 :  ex=%d, flags=0x%llx, csum=%d, "
            "ip_is_v6=%d, ip_fits=%d, ihl_ok=%d ihl_fits=%d\n",
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, EX),
            FM_ARRAY_GET_FIELD64(scenKeyPtr, HLP_MAP_SCEN_KEY0, FLAGS),
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, CSUM),
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, IP_IS_V6),
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, IP_FITS),
            FM_ARRAY_GET_BIT(scenKeyPtr, HLP_MAP_SCEN_KEY0, IHL_OK),
            FM_ARRAY_GET_BIT(scenKeyPtr, HLP_MAP_SCEN_KEY0, IHL_FITS));

        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
            "Scenario mask0 :  ex=%d, flags=0x%llx, csum=%d, "
            "ip_is_v6=%d, ip_fits=%d, ihl_ok=%d ihl_fits=%d\n",
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, EX),
            FM_ARRAY_GET_FIELD64(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, FLAGS),
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, CSUM),
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IP_IS_V6),
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IP_FITS),
            FM_ARRAY_GET_BIT(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IHL_OK),
            FM_ARRAY_GET_BIT(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0,
            IHL_FITS));*/

        if( ( ( (~key0.PTRS_ERR) & 0x1 ) &
            FM_ARRAY_GET_BIT(scenKeyPtr, HLP_MAP_SCEN_KEY0, PTRS_ERR) ) ||
            ( key0.PTRS_ERR &
            FM_ARRAY_GET_BIT(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, PTRS_ERR) ) )
        {
            continue;
        }

        if( ( ( (~key0.EX) & 0x7 ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, EX) ) ||
            ( key0.EX &
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, EX) ) )
        {
            continue;
        }
        if( ( ( (~key0.CSUM) & 0x3 ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, CSUM) ) ||
            ( key0.CSUM &
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, CSUM) ) )
        {
            continue;
        }
        if( ( ( (~key0.IP_IS_V6) & 0x3 ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, IP_IS_V6) ) ||
            ( key0.IP_IS_V6 &
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IP_IS_V6) ) )
        {
            continue;
        }
        if( ( ( (~key0.IP_FITS) & 0x3 ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr, HLP_MAP_SCEN_KEY0, IP_FITS) ) ||
            ( key0.IP_FITS &
            FM_ARRAY_GET_FIELD(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IP_FITS) ) )
        {
            continue;
        }
        if( ( ( (~key0.IHL_OK) & 0x1 ) &
            FM_ARRAY_GET_BIT(scenKeyPtr, HLP_MAP_SCEN_KEY0, IHL_OK) ) ||
            ( key0.IHL_OK &
            FM_ARRAY_GET_BIT(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IHL_OK) ) )
        {
            continue;
        }
        if( ( ( (~key0.IHL_FITS) & 0x1 ) &
            FM_ARRAY_GET_BIT(scenKeyPtr, HLP_MAP_SCEN_KEY0, IHL_FITS) ) ||
            ( key0.IHL_FITS &
            FM_ARRAY_GET_BIT(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, IHL_FITS) ) )
        {
            continue;
        }
        if( ( ( (~key0.FLAGS) & 0x7FFFFFFFFFFF ) &
            FM_ARRAY_GET_FIELD64(scenKeyPtr, HLP_MAP_SCEN_KEY0, FLAGS) ) ||
            ( key0.FLAGS &
            FM_ARRAY_GET_FIELD64(scenMaskPtr, HLP_MAP_SCEN_KEY_INVERT0, FLAGS) ) )
        {
            continue;
        }

        WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAP_SCEN cam key0.HIT index = %d\n", i)

        if( ( ( (~key1.METADATA_TYPE) & 0xFF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, METADATA_TYPE) ) ||
            ( key1.METADATA_TYPE &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, METADATA_TYPE) ) )
        {
            continue;
        }
        if( ( ( (~key1.METADATA_FLAGS) & 0x7FF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, METADATA_FLAGS) ) ||
            ( key1.METADATA_FLAGS &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, METADATA_FLAGS) ) )
        {
            continue;
        }
        if( ( ( (~key1.L2_DOMAIN) & 0x1FF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, L2_DOMAIN) ) ||
            ( key1.L2_DOMAIN &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, L2_DOMAIN) ) )
        {
            continue;
        }
        if( ( ( (~key1.L3_DOMAIN) & 0x3F ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, L3_DOMAIN) ) ||
            ( key1.L3_DOMAIN &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, L3_DOMAIN) ) )
        {
            continue;
        }
        if( ( ( (~key1.IP_SCENARIO) & 0xFF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, IP_SCENARIO) ) ||
            ( key1.IP_SCENARIO &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, IP_SCENARIO) ) )
        {
            continue;
        }
        if( ( ( (~key1.PORT_SCENARIO) & 0xF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, PORT_SCENARIO) ) ||
            ( key1.PORT_SCENARIO &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, PORT_SCENARIO) ) )
        {
            continue;
        }
        if( ( ( (~key1.DOMAIN_SCENARIO) & 0xFF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, DOMAIN_SCENARIO) ) ||
            ( key1.DOMAIN_SCENARIO &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, DOMAIN_SCENARIO) ) )
        {
            continue;
        }
        if( ( ( (~key1.MAC_ROUTABLE) & 0xF ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, MAC_ROUTABLE) ) ||
            ( key1.MAC_ROUTABLE &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, MAC_ROUTABLE) ) )
        {
            continue;
        }
        if( ( ( (~key1.MAC_MBCAST) & 0x3 ) &
            FM_ARRAY_GET_FIELD(scenKeyPtr1, HLP_MAP_SCEN_KEY1, MAC_MBCAST) ) ||
            ( key1.MAC_MBCAST &
            FM_ARRAY_GET_FIELD(scenMaskPtr1, HLP_MAP_SCEN_KEY_INVERT1, MAC_MBCAST) ) )
        {
            continue;
        }

        WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "MAP_SCEN cam hit index = %d\n", i)

        if (FM_ARRAY_GET_BIT(scenActionPtr, HLP_MAP_SCEN_ACTION, SCENARIO_VALID))
        {
            if (scenarioIdx == 0)
            {
                scenarioIdx = i;
                mapScenAction->SCENARIO_VALID = 1;
                mapScenAction->SCENARIO =
                    FM_ARRAY_GET_FIELD(scenActionPtr, HLP_MAP_SCEN_ACTION, SCENARIO);
                mapScenAction->REWRITE_PROFILE =
                    FM_ARRAY_GET_FIELD(scenActionPtr, HLP_MAP_SCEN_ACTION, REWRITE_PROFILE);
                WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "scenario cam hit index = %d. scenario %d, rewrite_profile %d\n",
                    scenarioIdx, mapScenAction->SCENARIO, mapScenAction->REWRITE_PROFILE)
            }
        }
        if (FM_ARRAY_GET_BIT(scenActionPtr, HLP_MAP_SCEN_ACTION, TRIG_VALID))
        {
            if (trigIdx == 0)
            {
                trigIdx = i;
                mapScenAction->TRIG_VALID = 1;
                mapScenAction->SCEN_TRIG =
                    FM_ARRAY_GET_FIELD(
                        scenActionPtr, HLP_MAP_SCEN_ACTION, SCEN_TRIG);
                mapScenAction->IP_OPTIONS_MASK =
                    FM_ARRAY_GET_FIELD(
                        scenActionPtr, HLP_MAP_SCEN_ACTION, IP_OPTIONS_MASK);
                mapScenAction->PARSER_ERROR =
                    FM_ARRAY_GET_BIT(
                        scenActionPtr, HLP_MAP_SCEN_ACTION, PARSER_ERROR);
                WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "trig scenario cam hit index = %d, scen_trig=0x%x, op_mask=0x%x, parser_error=%0d\n",
                    trigIdx, mapScenAction->SCEN_TRIG, mapScenAction->IP_OPTIONS_MASK, mapScenAction->PARSER_ERROR)
            }
        }
        if (FM_ARRAY_GET_BIT(scenActionPtr, HLP_MAP_SCEN_ACTION, PRIOS_VALID))
        {
            if (priosIdx == 0)
            {
                priosIdx = i;
                mapScenAction->PRIOS_VALID = 1;
                mapScenAction->VPRI_TGT =
                    FM_ARRAY_GET_FIELD(
                        scenActionPtr, HLP_MAP_SCEN_ACTION, VPRI_TGT);
                mapScenAction->DSCP_TGT =
                    FM_ARRAY_GET_FIELD(
                        scenActionPtr, HLP_MAP_SCEN_ACTION, DSCP_TGT);
                WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "prios scenario cam hit index = %d. vpri_tgt=%d dscp_tgt=%d\n",
                    priosIdx, mapScenAction->VPRI_TGT,
                    mapScenAction->DSCP_TGT)
            }
        }

        /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Cam hit Index: scenario = %d,
         * trig=%d, prios=%d\n", scenarioIdx, trigIdx, priosIdx);*/
        /* Found all types */
        if ((scenarioIdx > 0) && (trigIdx > 0) && (priosIdx > 0)) {
            break;
        }
    }

    state->FFU_SCENARIO =  mapScenAction->SCENARIO;

    /* Set Scenario action */
    for (i = 0; i < 6; i++)
    {
        ffuActions->act1[HLP_MODEL_FFU_ACTION_SCENARIO0 + i].val =
            (state->FFU_SCENARIO >> i) & 1;
    }

} /* end GetScenario */


/*****************************************************************************/
/**  RewriteSourceNybble
 * \ingroup intModel
 *
 * \desc            Rewrite source nybble from various sources.
 *
 * \param[in]       model points to the switch data structure

 *
 * \param[in]       mappedKey points to caller-allocated hlp_modelMapKey which
 *                  contains the mapped keys.
 *
 * \param[in]       key0 points to caller-allocated hlpMapScenKey0 which
 *                  stores the scenario key0.
 *
 * \param[in]       nybbleIdx is the nybble index in the keys to modify.
 *
 * \param[in]       sourceId is the source id of where to obtain the data.
 *
 * \param[out]      ffuKeys points to caller-allocated hlp_modelFfuKeys
 *                  in which the function will modify the keys.
 *
 *****************************************************************************/
static void RewriteSourceNybble(hlp_model *model,
                                hlp_modelMapKey mappedKey,
                                hlpMapScenKey0 key0,
                                fm_int nybbleIdx,
                                fm_int sourceId,
                                hlp_modelFfuKeys *ffuKeys)
{
    hlp_modelState *state;
    fm_int  keyIdx;
    fm_int  keyOff;
    fm_byte val;

    state   = &model->packetState;

    if (nybbleIdx < 4)
    {
        keyIdx = 13;
        keyOff = nybbleIdx*4;
    }
    else if (nybbleIdx < 8)
    {
        keyIdx = 19;
        keyOff = (nybbleIdx%4)*4;
    }
    else if (nybbleIdx < 24)
    {
        keyIdx = (nybbleIdx - 8)/2;
        keyOff = (nybbleIdx%2)*4;
    }
    else if (nybbleIdx <= 31)
    {
        keyIdx = 16 + (nybbleIdx - 24)/2;
        keyOff = (nybbleIdx%2)*4;
    }
    else return;

    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "nybbleIdx=%d, keyIdx=%d, keyOff=%d\n", nybbleIdx, keyIdx, keyOff)
    if (sourceId == SOURCE_NOOP)
        return;
    else if (sourceId == SOURCE_MAP_PORT)
        val = mappedKey.MAP_PORT;
    else if (sourceId == SOURCE_MAP_OUTER_PROT)
        val = mappedKey.MAP_OUTER_PROT;
    else if (sourceId == SOURCE_MAP_OUTER_ETYPE)
        val = mappedKey.MAP_OUTER_ETYPE;
    else if (sourceId == SOURCE_MAP_OUTER_DMAC_H)
        val = mappedKey.MAP_OUTER_DMAC >> 4;
    else if (sourceId == SOURCE_MAP_OUTER_DMAC_L)
        val = mappedKey.MAP_OUTER_DMAC >> 0;
    else if (sourceId == SOURCE_MAP_OUTER_SMAC_H)
        val = mappedKey.MAP_OUTER_SMAC >> 4;
    else if (sourceId == SOURCE_MAP_OUTER_SMAC_L)
        val = mappedKey.MAP_OUTER_SMAC >> 0;
    else if (sourceId == SOURCE_MAP_OUTER_DIP)
        val = mappedKey.MAP_OUTER_DIP;
    else if (sourceId == SOURCE_MAP_OUTER_SIP)
        val = mappedKey.MAP_OUTER_SIP;
    //else if (sourceId == SOURCE_MAP_OUTER_L3_LENGTH)
    //    val = mappedKey.MAP_OUTER_L3_LENGTH;
    /* Don't need to mask first 4 bits, since we will only
     * using the first 4 bits */
    else if (sourceId >= SOURCE_MAP_OUTER_L4_SRC_L &&
             sourceId <= SOURCE_MAP_OUTER_L4_SRC_H)
        val = mappedKey.MAP_OUTER_L4_SRC >>
                ((SOURCE_MAP_OUTER_L4_SRC_H - sourceId)*4);
    else if (sourceId >= SOURCE_MAP_OUTER_L4_DST_L &&
             sourceId <= SOURCE_MAP_OUTER_L4_DST_H)
        val = mappedKey.MAP_OUTER_L4_DST >>
                ((SOURCE_MAP_OUTER_L4_DST_H - sourceId)*4);
    else if (sourceId >= SOURCE_PA_FLAGS_L &&
             sourceId <= SOURCE_PA_FLAGS_H)
    {
        val = state->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId)*4)];
        val |= (state->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId)*4)+1] << 1);
        val |= (state->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId)*4)+2] << 2);
        val |= (state->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId)*4)+3] << 3);
    }
    else if (sourceId >= SOURCE_FFU_SCENARIO_L &&
             sourceId <= SOURCE_FFU_SCENARIO_H)
        /* only 6 bits */
        val = (state->FFU_SCENARIO & 0x3F) >>
                ((SOURCE_FFU_SCENARIO_H - sourceId)*4);
    else if (sourceId == SOURCE_MAP_INNER_PROT)
        val = mappedKey.MAP_INNER_PROT;
    else if (sourceId == SOURCE_MAP_INNER_ETYPE)
        val = mappedKey.MAP_INNER_ETYPE;
    else if (sourceId == SOURCE_MAP_INNER_DMAC_H)
        val = mappedKey.MAP_INNER_DMAC >> 4;
    else if (sourceId == SOURCE_MAP_INNER_DMAC_L)
        val = mappedKey.MAP_INNER_DMAC >> 0;
    else if (sourceId == SOURCE_MAP_INNER_SMAC_H)
        val = mappedKey.MAP_INNER_SMAC >> 4;
    else if (sourceId == SOURCE_MAP_INNER_SMAC_L)
        val = mappedKey.MAP_INNER_SMAC >> 0;
    else if (sourceId == SOURCE_MAP_INNER_DIP)
        val = mappedKey.MAP_INNER_DIP;
    else if (sourceId == SOURCE_MAP_INNER_SIP)
        val = mappedKey.MAP_INNER_SIP;
   // else if (sourceId == SOURCE_MAP_INNER_L3_LENGTH)
   //     val = mappedKey.MAP_INNER_L3_LENGTH;
    else if (sourceId >= SOURCE_MAP_INNER_L4_SRC_L &&
             sourceId <= SOURCE_MAP_INNER_L4_SRC_H)
        val = mappedKey.MAP_INNER_L4_SRC >>
                ((SOURCE_MAP_INNER_L4_SRC_H - sourceId)*4);
    else if (sourceId >= SOURCE_MAP_INNER_L4_DST_L &&
             sourceId <= SOURCE_MAP_INNER_L4_DST_H)
        val = mappedKey.MAP_INNER_L4_DST >>
                ((SOURCE_MAP_INNER_L4_DST_H - sourceId)*4);
    else if (sourceId == SOURCE_EX)
    {
        val = 0;
        FM_SET_UNNAMED_FIELD(val,
                         0,
                         1,
                         state->PA_EX_PARSING_DONE);
        FM_SET_UNNAMED_FIELD(val,
                         1,
                         1,
                         state->PA_EX_TRUNC_HEADER);
        FM_SET_UNNAMED_FIELD(val,
                         2,
                         1,
                         state->PA_EX_DEPTH_EXCEED);
    }
    else if (sourceId == SOURCE_CSUM)
        val = key0.CSUM;
    else if (sourceId == SOURCE_IP_INFO)
    {
        val =  (key0.IP_IS_V6 & 0x3);
        val |= (key0.IP_FITS & 0x3) << 2;
    }
    else
    {
         FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "Unhandled sourceId %d\n", sourceId);
        return;
    }

    if (nybbleIdx < 8)
    {
        WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "sourceId %d. Set key16[%d.%d]=0x%x\n",
                sourceId, keyIdx, keyOff, val);
        FM_SET_UNNAMED_FIELD(ffuKeys->key16[keyIdx], keyOff, 4, val);
    }
    else if (nybbleIdx <= 31)
    {
        WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "sourceId %d. Set key8[%d.%d]=0x%x\n",
                sourceId, keyIdx, keyOff, val);
        FM_SET_UNNAMED_FIELD(ffuKeys->key8[keyIdx], keyOff, 4, val);
    }
} /* end RewriteSourceNybble */



/*****************************************************************************/
/** MapRewrite
 * \ingroup intModel
 *
 * \desc            Write mapping results back to FFU keys
 *
 * \param[in]       model points to the switch data structure
 *
 * \param[in]       realignKeys points to caller-allocated storage which is
 *                  used in mapper instead of parser keys
 *
 * \param[in]       realignKeysValid points to caller-allocated storage which
 *                  is the results after realign parser_keys_v
 *
 * \param[in]       mapScenActions points to the caller-allocated structure
 *                  in which key_rewrite_cfg will be used in this function
 *
 * \param[out]      mappedKey is the mapped key values.
 *
 *****************************************************************************/
static void MapRewrite(hlp_model *model,
                       fm_uint16            *realignKeys,
                       fm_bool              *realignKeysValid,
                       hlpMapScenAction     mapScenAction,
                       hlpMapScenKey0       key0,
                       //fm_byte              rewriteProfile,
                       hlp_modelMapKey      mappedKeys,
                       fm_byte              priority_profile,
                       fm_bool              *isIPv6)
{
    hlp_modelState       *state;
    hlp_modelParserInfo  *parserInfo;
    hlp_modelFfuKeys     *ffuKeys;
    hlp_modelFfuActions  *ffuActions;
    fm_uint32            *scenActPtr;
    fm_uint32            *regPtr;
    fm_int                i;
    fm_int               vpri;
    fm_int               dscp;
    fm_int               mapVpri;
    fm_int               mapDscp;
    fm_uint              otr_opt_flags;
    fm_uint              inr_opt_flags;
    fm_uint16            otr_l3_len;
    fm_uint16            inr_l3_len;

    FM_NOT_USED(realignKeys);
    FM_NOT_USED(realignKeysValid);

    state      = &model->packetState;
    parserInfo = &state->PARSER_INFO;
    ffuKeys    = &state->FFU_KEYS;
    ffuActions = &state->FFU_ACTIONS;

    /*
    for( i = 0; i < HLP_MODEL_FFU_N_ACT4; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "act4[%d] = 0x%x\n", i, ffuActions->act4[i].val);
    }
    */


    /*for( i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key16[%d] = 0x%x\n", i, ffuKeys->key16[i]);
    }



    for( i = 0; i < HLP_MODEL_FFU_N_KEY8; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key8[%d] = 0x%x\n", i, ffuKeys->key8[i]);
    }*/



    /* Rewrite Keys */
    if (mapScenAction.SCENARIO_VALID)
    {

        for (i = 0; i < HLP_MAP_REWRITE_ENTRIES_0; i++)
        {
            scenActPtr =  FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_REWRITE(mapScenAction.REWRITE_PROFILE, i, 0));
            RewriteSourceNybble(model,
                                mappedKeys,
                                key0,
                                i,
                                FM_GET_FIELD(*scenActPtr, HLP_MAP_REWRITE, SRC_ID),
                                &state->FFU_KEYS);

        }
    }

    /*
    for( i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key16[%d] = 0x%x\n", i, ffuKeys->key16[i]);
    }
    */
    /*
    for( i = 0; i < HLP_MODEL_FFU_N_KEY8; i++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key8[%d] = 0x%x\n", i, ffuKeys->key8[i]);
    }
    */
    state->PARSER_ERROR = 0;

    if(mapScenAction.TRIG_VALID == 1)
    {
        /* trig and ip_option mask*/
        for (i = 0; i < 8; i++)
        {
            ffuActions->act1[HLP_MODEL_FFU_ACTION_TRIGGER0 + i].val =
                (mapScenAction.SCEN_TRIG >> i) & 1;
        }

        /* Mask to select which parser flags will generate TRAP_IP_OPTIONS. The formula is:
         otr_opt_flags = {otr_l3_v && !otr_v6 && otr_l3_len > 20, // otr_opt_flags[6] = IPv4 case
                          parser_flags[7:2]}                      // otr_opt_flags[5:0] from parser.
         inr_opt_flags = {inr_l3_v && !inr_v6 && inr_l3_len > 20, // inr_opt_flags[6] = IPv4 case
                          parser_flags[15:10]}                    // inr_opt_flags[5:0] from parser.
         ip_options[0] = |(ip_options_mask & otr_opt_flags)
         ip_options[1] = |(ip_options_mask & inr_opt_flags)
         ffu_to_fwd:  trap_ip_options = ip_options[mod_idx.decap]
        */
        otr_l3_len = 0;
        inr_l3_len = 0;
        FM_SET_UNNAMED_FIELD64(otr_l3_len,
                               0,
                               8,
                               ffuKeys->key8[HLP_MODEL_KEY8_OUTER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(otr_l3_len,
                               8,
                               8,
                               ffuKeys->key8[HLP_MODEL_KEY8_OUTER_LEN]);

        FM_SET_UNNAMED_FIELD64(inr_l3_len,
                               0,
                               8,
                               ffuKeys->key8[HLP_MODEL_KEY8_INNER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(inr_l3_len,
                               8,
                               8,
                               ffuKeys->key8[HLP_MODEL_KEY8_INNER_LEN]);

        //otr_opt_flags = (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V] && !parserInfo->otr_l3_v6 &&
        otr_opt_flags = (state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V] && !isIPv6[0] &&
                        (otr_l3_len > 20)) ? (1 << 6) : 0;
        for (i = 0; i <= 5; i++)
        {
            FM_SET_UNNAMED_FIELD(otr_opt_flags,
                                 i,
                                 1,
                                 state->PA_FLAGS[i + 32]);
        }
        //inr_opt_flags = (state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V] && !parserInfo->inr_l3_v6 &&
        inr_opt_flags = (state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V] && !isIPv6[1] &&
                        (inr_l3_len > 20)) ? (1 << 6) : 0;
        for (i = 0; i <= 5; i++)
        {
            FM_SET_UNNAMED_FIELD(inr_opt_flags,
                                 i,
                                 1,
                                 state->PA_FLAGS[i + 38]);
        }

        WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "otr_l3_len=0x%x, inr_l3_len=0x%x, otr_opt_flags=0x%x, inr_opt_flags=0x%x, ip_options_mask=0x%x\n", otr_l3_len, inr_l3_len, otr_opt_flags, inr_opt_flags, mapScenAction.IP_OPTIONS_MASK)

        state->IP_OPTION[0] = (mapScenAction.IP_OPTIONS_MASK & otr_opt_flags) ? 1:0;
        state->IP_OPTION[1] = (mapScenAction.IP_OPTIONS_MASK & inr_opt_flags) ? 1:0;
        state->PARSER_ERROR = mapScenAction.PARSER_ERROR;
    }

    if(mapScenAction.PRIOS_VALID == 1)
    {
        /* vpri_tgt and dscp_tgt, no valid check is needed */
        if (mapScenAction.VPRI_TGT & 0x4)
        {
            /* NOTE: This is place before (vpri_tgt & 0x1) otherwise
             * vpri will be remapped twice if both bits are set */
            vpri = FM_GET_UNNAMED_FIELD(ffuKeys->key16[HLP_MODEL_KEY16_OUTER_VLAN1], 12, 4);
            regPtr = FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_VPRI(priority_profile, 0));
            mapVpri = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, vpri*4, 4);
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OTR_VLAN1 Vpri %d mapped to %d and set to act4[%d,%d]\n",
                vpri, mapVpri, HLP_MODEL_FFU_ACTION_VPRI_LOW,
                HLP_MODEL_FFU_ACTION_VPRI_HIGH)
            ffuActions->act4[HLP_MODEL_FFU_ACTION_VPRI_LOW].val = mapVpri;
            ffuActions->act4[HLP_MODEL_FFU_ACTION_VPRI_HIGH].val = mapVpri;
        }
        if (mapScenAction.VPRI_TGT & 0x1)
        {
            //ffuKeys->key16[OUTER_VLAN1].vpri = MAP_VPRI[p, key16[OUTER_VLAN1].vpri];
            vpri = FM_GET_UNNAMED_FIELD(ffuKeys->key16[HLP_MODEL_KEY16_OUTER_VLAN1], 12, 4);
            regPtr = FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_VPRI(priority_profile, 0));
            mapVpri = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, vpri*4, 4);
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OTR_VLAN1 Vpri %d mapped to %d and set to key16[%d]\n",
                vpri, mapVpri, HLP_MODEL_KEY16_OUTER_VLAN1)
            FM_SET_UNNAMED_FIELD(ffuKeys->key16[HLP_MODEL_KEY16_OUTER_VLAN1],
                12, 4, mapVpri)
        }
        if (mapScenAction.VPRI_TGT & 0x2)
        {
            vpri = FM_GET_UNNAMED_FIELD(ffuKeys->key16[HLP_MODEL_KEY16_INNER_VLAN1], 12, 4);
            regPtr = FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_VPRI(priority_profile, 0));
            mapVpri = FM_ARRAY_GET_UNNAMED_FIELD(regPtr, vpri*4, 4);
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "INR_VLAN1 Vpri %d mapped to %d and set to key16[%d]\n",
                vpri, mapVpri, HLP_MODEL_KEY16_INNER_VLAN1)
            FM_SET_UNNAMED_FIELD(ffuKeys->key16[HLP_MODEL_KEY16_INNER_VLAN1],
                12, 4, mapVpri);
        }

        if (mapScenAction.DSCP_TGT & 0x4)
        {
            /* NOTE: This is place before (dscp_tgt & 0x1) otherwise
             * dscp will be remapped twice if both bits are set */
            dscp = FM_GET_UNNAMED_FIELD(ffuKeys->key8[HLP_MODEL_KEY8_OUTER_DS], 2, 6);
            regPtr = FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_DSCP_TC(((priority_profile << 6) | dscp), 0));
            mapDscp = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DSCP_TC, DSCP);
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OTR_DSCP 0x%x mapped to 0x%x and set to act4[%d,%d]\n",
                dscp, mapDscp, HLP_MODEL_FFU_ACTION_DSCP_LOW,
                HLP_MODEL_FFU_ACTION_DSCP_HIGH)
            ffuActions->act4[HLP_MODEL_FFU_ACTION_DSCP_LOW].val = mapDscp & 0xF;
            ffuActions->act4[HLP_MODEL_FFU_ACTION_DSCP_HIGH].val = (mapDscp >> 4);
        }
        if (mapScenAction.DSCP_TGT & 0x1)
        {
            dscp = FM_GET_UNNAMED_FIELD(ffuKeys->key8[HLP_MODEL_KEY8_OUTER_DS], 2, 6);
            regPtr = FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_DSCP_TC(((priority_profile << 6) | dscp), 0));
            mapDscp = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DSCP_TC, DSCP);
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "OTR_DSCP 0x%x mapped to 0x%x and set to key8[%d]=0x%x\n",
                dscp, mapDscp, HLP_MODEL_KEY8_OUTER_DS, ffuKeys->key8[HLP_MODEL_KEY8_OUTER_DS])
            FM_SET_UNNAMED_FIELD(ffuKeys->key8[HLP_MODEL_KEY8_OUTER_DS], 2, 6, mapDscp);
        }
        if (mapScenAction.DSCP_TGT & 0x2)
        {
            dscp = FM_GET_UNNAMED_FIELD(ffuKeys->key8[HLP_MODEL_KEY8_INNER_DS], 2, 6);
            regPtr = FM_MODEL_GET_REG_PTR(model,
                            HLP_MAP_DSCP_TC(((priority_profile << 6) | dscp), 0));
            mapDscp = FM_ARRAY_GET_FIELD(regPtr, HLP_MAP_DSCP_TC, DSCP);
            WM_DISPLAY2(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "INR_DSCP dscp %d mapped to 0x%x and set to key8[%d]\n",
                dscp, mapDscp, HLP_MODEL_KEY8_INNER_DS)
            FM_SET_UNNAMED_FIELD(ffuKeys->key8[HLP_MODEL_KEY8_INNER_DS], 2, 6, mapDscp);
        }
    }



    /*for( i = 0; i < HLP_MODEL_FFU_N_KEY8; i++)
    {
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key8[%d] = 0x%x\n", i, ffuKeys->key8[i]);
    }

    for( i = 0; i < HLP_MODEL_FFU_N_KEY16; i++)
    {
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "key16[%d] = 0x%x\n", i, ffuKeys->key16[i]);
    }*/


} /* end MapRewrite */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelMapper
 * \ingroup intModel
 *
 * \desc           Models the Mapper stage of Bali Frame Processing pipeline.
 *
 * \param[in]      model points to the switch data structure.
 *
 *****************************************************************************/
void hlpModelMapper(hlp_model *model)
{
    hlpMapPortCfg         portCfg;
    fm_uint16             realignKeys[HLP_MODEL_N_REALIGN_KEYS];
    fm_bool               realignKeysValid[HLP_MODEL_N_REALIGN_KEYS];
    hlpMapScenKey0        mapScenKey0;
    hlpMapScenKey1        mapScenKey1;
    hlp_modelMapKey       mappedKey;
    hlpMapScenAction      mapScenAction;
    hlp_modelParserInfo   parserInfo;
    hlp_modelState       *state;
    hlp_modelFfuKeys     *ffuKeys;
    hlp_modelFfuActions  *ffuActions;
    fm_bool               isIPv4[HLP_MODEL_N_IS_IP_BITS] = { 0 };
    fm_bool               isIPv6[HLP_MODEL_N_IS_IP_BITS] = { 0 };
    fm_int                i;
    fm_uint               domainIndex;
    fm_int                dglortFromDglortKey;
    fm_uint               priorityProfile;
    //fm_byte               rewriteProfile;
    fm_bool               ihl_ok;
    fm_bool               ihl_fits;
    fm_bool               ptrsErr = 0;

    if(testPlusArgs("HLP_MAPPER_WM_PRINT_VERBOSE") >= 0)
        mapperDisplayVerbose = testPlusArgs("HLP_MAPPER_WM_PRINT_VERBOSE");
    WM_DISPLAY(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "mapperDisplayVerbose=%0d\n", mapperDisplayVerbose);


    state      = &model->packetState;
    FM_CLEAR(mapScenKey0);
    FM_CLEAR(mapScenKey1);
    FM_CLEAR(mapScenAction);
    FM_CLEAR(mappedKey);
    //FM_CLEAR(rewriteProfile);

    WM_DISPLAY3(mapperDisplayVerbose, FM_LOG_CAT_MODEL_MAPPER, "state->PKT_META[0]=0x%x\n", state->PKT_META[0])
    //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "state->PKT_META[0]=0x%x\n", state->PKT_META[0]);

    //INNER IP
    if(state->PA_FLAGS[HLP_PA_FLAGS_INR_L3_V])
    {
        isIPv4[1] = state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER] ? 1 : 0;
        isIPv6[1] = state->PA_KEYS_VALID[HLP_PA_KEYS_INNER_IP_HEADER] ? 0 : 1;
    }
    //OUTER IP
    if(state->PA_FLAGS[HLP_PA_FLAGS_OTR_L3_V])
    {
        isIPv4[0] = state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER] ? 1 : 0;
        isIPv6[0] = state->PA_KEYS_VALID[HLP_PA_KEYS_OUTER_IP_HEADER] ? 0 : 1;
    }

    GetPortCfg(model, &portCfg);

    RealignKeys(model, isIPv4, isIPv6, realignKeys, realignKeysValid, &ihl_ok, &ihl_fits);

    LookupDomainTcam(model, realignKeys, &domainIndex);

    /* Uses PKT_META before being modified by OnpiPhyFunc().
     * However it does not matter since this is only applicable on
     * EGRESS and OnpiPhyFunc is only applicable on INGRESS */
    GetDglortFromDglortKey(model, &dglortFromDglortKey);

    /* Modify PKT_META to be used by InsertDefaults */
    OnpiPhyFunc(model);

    InsertDefaults(model, portCfg, dglortFromDglortKey, realignKeys, realignKeysValid);

    //LookupDomainTcam(model, realignKeys, &domainIndex);

    MapperScalar(model,
                 portCfg,
                 realignKeys,
                 isIPv4,
                 isIPv6,
                 domainIndex,
                 ihl_ok,
                 ihl_fits,
                 &mapScenKey0,
                 &mapScenKey1,
                 &mappedKey,
                 &priorityProfile);

    /* update state PARSER_INFO */
    GetParserInfo(model, realignKeys, isIPv6, &ptrsErr);
    mapScenKey0.PTRS_ERR = ptrsErr;

    /* update state FFU_SCENARIO and FFU_VRID */
    GetScenario(model, realignKeys, mapScenKey0, mapScenKey1, &mapScenAction);

    MapRewrite(model, realignKeys, realignKeysValid, mapScenAction,
        mapScenKey0, mappedKey, priorityProfile, isIPv6);

    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpMapper);

}   /* end hlpModelMapper */
