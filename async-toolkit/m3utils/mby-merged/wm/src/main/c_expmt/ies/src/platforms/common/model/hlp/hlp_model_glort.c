/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_glort.c
 * Creation Date:   June 20, 2012
 * Description:     GLORT stage of HLP white model
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
fm_int glortDisplayVerbose = 1;

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
/** GetGlortCamEntry 
 * \ingroup intModel
 *
 * \desc            Reads GLORT_CAM register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       camIndex points to the entry that needs to be read from CAM.
 *
 * \param[in,out]   camEntry is the struct for GLORT_CAM that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetGlortCamEntry(hlp_model     *model,
                             fm_byte       camIndex,
                             hlpGlortCam   *camEntry)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLORT_CAM(camIndex, 0));
    
    camEntry->KEY_INVERT = FM_GET_FIELD(*regPtr, HLP_GLORT_CAM, KEY_INVERT);
    camEntry->KEY       = FM_GET_FIELD(*regPtr, HLP_GLORT_CAM, KEY);

} /* GetGlortCamEntry */

/*****************************************************************************/
/** GetGlortRamEntry 
 * \ingroup intModel
 *
 * \desc            Reads GLORT_RAM register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       ramIndex points to the entry that needs to be read from RAM.
 *
 * \param[in,out]   ramEntry is the struct for GLORT_RAM that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetGlortRamEntry(hlp_model     *model,
                             fm_byte       ramIndex,
                             hlpGlortRam   *ramEntry)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLORT_RAM(ramIndex, 0));
    
    ramEntry->SKIP_DGLORT_DEC  = FM_ARRAY_GET_BIT(regPtr, HLP_GLORT_RAM, SKIP_DGLORT_DEC);
    ramEntry->HASH_ROTATION   = FM_ARRAY_GET_BIT(regPtr, HLP_GLORT_RAM, HASH_ROTATION);
    ramEntry->DEST_COUNT      = FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_RAM, DEST_COUNT);
    ramEntry->RANGE_SUB_INDEX_A = FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_RAM, RANGE_SUB_INDEX_A);
    ramEntry->RANGE_SUB_INDEX_B = FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_RAM, RANGE_SUB_INDEX_B);
    ramEntry->DEST_INDEX      = FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_RAM, DEST_INDEX);
    ramEntry->STRICT         = FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_RAM, STRICT);
    
    WM_DISPLAY3(glortDisplayVerbose, FM_LOG_CAT_MODEL_GLORT,
    		"GetGlortRamEntry: GLORT_RAM[%0d] SkipDglortDec=%d HashRotation=%0d, DestCount=0x%x, DestIndex=0x%x, Strict=%0d\n",
			ramIndex, ramEntry->SKIP_DGLORT_DEC, ramEntry->HASH_ROTATION, ramEntry->DEST_COUNT, ramEntry->DEST_INDEX, ramEntry->STRICT);
} /* GetGlortRamEntry */

/*****************************************************************************/
/** GetGlortDestTableEntry 
 * \ingroup intModel
 *
 * \desc            Reads GLORT_DEST_TABLE register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       tableIndex points to the entry that needs to be read from
 *                  GLORT_DEST_TABLE.
 *
 * \param[in,out]   ramEntry is the struct for GLORT_DEST_TABLE that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetGlortDestTableEntry(hlp_model            *model,
                                   fm_uint16            tableIndex,
                                   hlpGlortDestTable    *tableEntry)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLORT_DEST_TABLE(tableIndex, 0));
    
    tableEntry->IP_MULTICAST_INDEX = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_DEST_TABLE, IP_MULTICAST_INDEX);
    tableEntry->DEST_MASK = 
        FM_ARRAY_GET_FIELD(regPtr, HLP_GLORT_DEST_TABLE, DEST_MASK);
    
    WM_DISPLAY3(glortDisplayVerbose, FM_LOG_CAT_MODEL_GLORT,
    		"GetGlortDestTableEntry: GLORT_DEST_TABLE[%0d] IP_MulticastIndex=0x%x, DestMask=0x%x\n",
			tableIndex, tableEntry->IP_MULTICAST_INDEX, tableEntry->DEST_MASK);
} /* GetGlortDestTableEntry */

/*****************************************************************************/
/** LookupRamEntry
 * \ingroup intModel
 *
 * \desc            Performs a GLORT_CAM lookup and returns the glortRam entry
 *                  associated with the highest precendence GLORT_CAM hit.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[out]      glortRam points to caller-allocated storage in which this
 *                  function stores a pointer to the glortRam entry associated
 *                  with the highest precedence GLORT_CAM hit.
 *
 * \return          FM_OK in case of a GLORT_CAM hit.
 * \return          FM_ERR_NOT_FOUND otherwise.
 *
 *****************************************************************************/
static fm_status LookupRamEntry(hlp_model       *model,
                                hlpGlortRam     *glortRam)
{
    hlpGlortCam         glortCam;
    hlp_modelState      *state = &model->packetState;
    fm_uint16           mask;
    fm_int              i;


    /* The highest numbered GLORT_CAM entry has highest precendence. */
    for (i = HLP_GLORT_CAM_ENTRIES - 1; i >= 0; i--)
    {
        GetGlortCamEntry(model, i, &glortCam);
        
       	WM_DISPLAY3(glortDisplayVerbose, FM_LOG_CAT_MODEL_GLORT, "LookupRamEntry: GLORT_CAM[%0d] KEY* = 0x%x, KEY_INVERT=0x%x\n", i, glortCam.KEY, glortCam.KEY_INVERT);
        mask = glortCam.KEY ^ glortCam.KEY_INVERT;
        
        if (((glortCam.KEY & glortCam.KEY_INVERT) == 0) && 
             ( (state->IDGLORT & mask) == (glortCam.KEY & mask)))
        {    
            GetGlortRamEntry(model, i, glortRam);

            /*FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "LookupRamEntry: Glort Ram
             * Index = %x\n", i);*/ 
            return FM_OK;
        }
    }
    //if glort_cam is not hit then zero out glort_ram_entry(reflects RTL
    //behavior)
    glortRam->SKIP_DGLORT_DEC = 0;
    glortRam->HASH_ROTATION = 0;
    glortRam->DEST_COUNT = 0;
    glortRam->RANGE_SUB_INDEX_A = 0;
    glortRam->RANGE_SUB_INDEX_B = 0;
    glortRam->DEST_INDEX = 0;
    glortRam->STRICT = 0;

    return FM_ERR_NOT_FOUND;

}   /* end LookupRamEntry */

/*****************************************************************************/
/** LookupDestEntry
 * \ingroup intModel
 *
 * \desc            Performs the GLORT_DEST_TABLE lookup.
 *
 * \param[in]       model points to the switch model state.
 * 
 * \param[in]       glortRam points to the GLORT_RAM entry used to determine
 *                  which GLORT_DEST_TABLE is to be returned.
 * 
 * \param[in,out]   glortDestTable points to caller-allocated storage where
 *                  this function places a pointer to the GLORT_DEST_TABLE
 *                  entry.
 *
 *****************************************************************************/
static void LookupDestEntry(hlp_model           *model,
                            hlpGlortRam         glortRam,
                            hlpGlortDestTable   *glortDestTable)
{
    hlp_modelState  *state = &model->packetState;
    fm_uint32       hash;
    fm_uint16       glortA;
    fm_uint16       glortB;
    fm_uint16       destIndex;
    fm_uint16       lengthA;
    fm_uint16       lengthB;
    fm_uint16       offsetA;
    fm_uint16       offsetB;

    lengthA = (glortRam.RANGE_SUB_INDEX_A >> 4) & 0xF;
    offsetA = glortRam.RANGE_SUB_INDEX_A & 0xF;
    glortA = (state->IDGLORT >> offsetA) & ((1 << lengthA) - 1);
    state->SKIP_DGLORT_DEC = glortRam.SKIP_DGLORT_DEC;

    if (state->STRICT_GLORT_ROUTING)
    {
        lengthB = (glortRam.RANGE_SUB_INDEX_B >> 4) & 0xF;
        offsetB = glortRam.RANGE_SUB_INDEX_B & 0xF;
        glortB = (state->IDGLORT >> offsetB) & ((1 << lengthB) - 1);

        destIndex = glortRam.DEST_INDEX + (glortB << lengthA) + glortA;
        WM_DISPLAY3(glortDisplayVerbose, FM_LOG_CAT_MODEL_GLORT, "LookupDestEntry : lengthA=%0d, glortA=0x%x, lengthB=%0d, glortB=0x%x\n", lengthA, glortA, lengthB, glortB);
    }
    else
    {
        hash = glortRam.HASH_ROTATION ? state->HASH_ROT_B : state->HASH_ROT_A;
        switch (glortRam.DEST_COUNT)
        {
            case 0:  hash %= 16;                   break;
            default: hash %= glortRam.DEST_COUNT; break;
        }
        WM_DISPLAY3(glortDisplayVerbose, FM_LOG_CAT_MODEL_GLORT, "LookupDestEntry : hash = 0x%x, lengthA=%0d, glortA=0x%x\n", hash, lengthA, glortA);
        destIndex = glortRam.DEST_INDEX + (hash << lengthA) + glortA;
    }
    destIndex = destIndex & 0xFFF; //remove carry bit;
    
    GetGlortDestTableEntry(model, destIndex, glortDestTable);

}   /* end LookupDestEntry */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelGlort
 * \ingroup intModel
 *
 * \desc           Models GLORT lookup of HLP Frame Processing pipeline
 *
 * \param[in]      model points to the switch data structure.
 *
 * \return         FM_OK if successful.
 *
 * \return         FM_FAIL otherwise
 *****************************************************************************/
void hlpModelGlort(hlp_model *model)
{
    hlpGlortDestTable       glortDestTable;
    hlpGlortRam             glortRam;
    hlp_modelState          *state = &model->packetState;

    if(testPlusArgs("HLP_GLORT_WM_PRINT_VERBOSE") >= 0)
        glortDisplayVerbose = testPlusArgs("HLP_GLORT_WM_PRINT_VERBOSE");
    WM_DISPLAY(glortDisplayVerbose, FM_LOG_CAT_MODEL_GLORT, "glortDisplayVerbose=%0d\n", glortDisplayVerbose)

    if (LookupRamEntry(model, &glortRam) != FM_OK)
    {
        /* GLORT CAM miss */
        state->GLORT_CAM_MISS = TRUE;
        //return;
    }

    /* GLORT CAM hit */
    state->STRICT_GLORT_ROUTING = (glortRam.STRICT == 3 || glortRam.STRICT == 2) ? 1 : 0;
    state->TARGETED_DETERMINISTIC = (glortRam.STRICT == 2) ? 1 : 0;

    if(state->GLORT_CAM_MISS != TRUE)
    {    
        LookupDestEntry(model, glortRam, &glortDestTable);

        state->GLORT_DMASK = glortDestTable.DEST_MASK; 
        state->IP_MCAST_IDX = glortDestTable.IP_MULTICAST_INDEX;
    } 
    else 
    {
        state->GLORT_DMASK = 0;
        state->IP_MCAST_IDX = 0;
    }    

    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpGlort);

}   /* end hlpModelGlort */
