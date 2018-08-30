/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm4000_fibm_bootstrap.c
 * Creation Date:  June 1, 2008
 * Description:    Bootstrap code for various platforms.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2012 Intel Corporation. All Rights Reserved.
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
 ****************************************************************************/

/* NOTE:
 * This is a standalone file that is shared by examples/fibm and TestPoint
 * This should not call any API functions except basic read/write registers
 * This is provided as an example on how to bootstrap the switch for fibm
 * remote management.
 */

#include <fm_sdk_fm4000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define DUMP_VAL64(name, value)              \
    if (value)                               \
    {                                        \
        printf("%30s: %llu\n", name, value); \
    }

#undef FM_LOG_DEBUG
#define FM_LOG_DEBUG(cat, ...)  printf("Debug: " __VA_ARGS__);
#undef FM_LOG_INFO
#define FM_LOG_INFO(cat, ...)  printf("INFO: " __VA_ARGS__);
#undef FM_LOG_WARNING
#define FM_LOG_WARNING(cat, ...)  printf("WARN: " __VA_ARGS__);
#undef FM_LOG_ERROR
#define FM_LOG_ERROR(cat, ...)  printf("ERROR: " __VA_ARGS__);
#undef FM_LOG_ABORT_ON_ERR
#define FM_LOG_ABORT_ON_ERR(cat, status)                \
    if (status != FM_OK)                                \
    {                                                   \
        FM_LOG_ERROR(cat, "status: %d\n", status);      \
        goto ABORT;                                     \
    }

/*===========================================================================*/
/* Start of FIBM bootstrap code on slave */


/* this table maps platform port to physical port */
static const fm_int   fm85xxepPortRemapTable[] =
{
/* 0   */ 0,
/* 1   */ 14,
/* 2   */ 16,
/* 3   */ 20,
/* 4   */ 22,
/* 5   */ 18,
/* 6   */ 24,
/* 7   */ 17,
/* 8   */ 23,
/* 9   */ 21,
/* 10  */ 19,
/* 11  */ 15,
/* 12  */ 13,
/* 13  */ 2,
/* 14  */ 4,
/* 15  */ 6,
/* 16  */ 8,
/* 17  */ 10,
/* 18  */ 12,
/* 19  */ 9,
/* 20  */ 11,
/* 21  */ 5,
/* 22  */ 7,
/* 23  */ 3,
/* 24  */ 1
};

/* this table maps platform port to physical port */
static const fm_int   monacoPortRemapTable[] =
{
    /* 0   */ 0,
    /* 1   */ 2,
    /* 2   */ 4,
    /* 3   */ 6,
    /* 4   */ 8,
    /* 5   */ 10,
    /* 6   */ 12,
    /* 7   */ 16,
    /* 8   */ 14,
    /* 9   */ 20,
    /* 10  */ 22,
    /* 11  */ 18,
    /* 12  */ 24,
    /* 13  */ 23,
    /* 14  */ 17,
    /* 15  */ 21,
    /* 16  */ 19,
    /* 17  */ 13,
    /* 18  */ 15,
    /* 19  */ 11,
    /* 20  */ 9,
    /* 21  */ 7,
    /* 22  */ 5,
    /* 23  */ 3,
    /* 24  */ 1
};


#define PHY_INIT_PAUSE   1
#define PHY_INIT_END     0

/* Initialization sequence for AEL2005.
 * ***NOTE*** the waits are in fact 1s at the moment, this will be
 * corrected eventually.
 */
static fm_int    aelInitSequence[]     =
{
    /* microInit_mdio_SR_AEL2005C_18.txt */
    /* Contains the MDIO writes necessary to bring up AEL2005C PHY for SR/LR (limiting interface). */
    
    /* 1. Hardware & Software Reset: */
    /* Always hardware reset the PHY upon powerup by holding RESET_N to GND for 100ms
     * after power supplies stabilize. */
    
    /* Make sure TX_ON is asserted before starting any MDIO writes. */
    
    0x10000,0xa040, /* issues a software reset */
    PHY_INIT_PAUSE,100, /* wait for 100ms for device to get out of reset. */
    
    /* 2. Platform-specific MDIO Patches: */ 
    /* (include patches for 10G RX polarity flip, 50Mhz Synth, etc) */
    0x1c017,0xfeb0, /* flip RX_LOS polarity (mandatory patch for SFP+ applications) */
    0x1c007,0x0500, /* flip polarity of lane 0 and lane 2 */
    0x1c001,0x0020, /* flip polarity on HS RX DATA */    
    0x1c013,0xf341, /* invert lxmit clock (mandatory patch) */
    0x1c210,0x8000, /* reset datapath (mandatory patch) */
    0x1c210,0x8100, /* reset datapath (mandatory patch) */
    0x1c210,0x8000, /* reset datapath (mandatory patch) */
    0x1c210,0x0000, /* reset datapath (mandatory patch) */
    PHY_INIT_PAUSE,50, /* wait for 50ms for datapath reset to complete. (mandatory patch) */
    
    /* 3. Transceiver-specific MDIO Patches:  */
    0x1c003,0x181,  /* (bit 7) enable the CDR inc setting in 1.C005 (mandatory patch for SR code) */
    0x1c010,0x448a, /* (bit 14) mask out high BER input from the LOS signal in 1.000A (mandatory patch for SR code) */
    
    /* 4. Transceiver-specific Microcontroller Initialization: */
    0x1c04a,0x5200, /* activate microcontroller and pause */
    PHY_INIT_PAUSE,50, /* wait 50ms for microcontroller before writing in code. */
    
    /* code block starts here: */
    0x1cc00, 0x2ff4,
    0x1cc01, 0x3cd4,
    0x1cc02, 0x2015,
    0x1cc03, 0x3125,
    0x1cc04, 0x6524,
    0x1cc05, 0x27ff,
    0x1cc06, 0x300f,
    0x1cc07, 0x2c8b,
    0x1cc08, 0x300b,
    0x1cc09, 0x4009,
    0x1cc0a, 0x400e,
    0x1cc0b, 0x2f12,
    0x1cc0c, 0x3002,
    0x1cc0d, 0x1002,
    0x1cc0e, 0x2112,
    0x1cc0f, 0x3012,
    0x1cc10, 0x1002,
    0x1cc11, 0x2572,
    0x1cc12, 0x3012,
    0x1cc13, 0x1002,
    0x1cc14, 0xd01e,
    0x1cc15, 0x2772,
    0x1cc16, 0x3012,
    0x1cc17, 0x1002,
    0x1cc18, 0x2004,
    0x1cc19, 0x3c84,
    0x1cc1a, 0x6436,
    0x1cc1b, 0x2007,
    0x1cc1c, 0x3f87,
    0x1cc1d, 0x8676,
    0x1cc1e, 0x40b7,
    0x1cc1f, 0xa746,
    0x1cc20, 0x4047,
    0x1cc21, 0x5673,
    0x1cc22, 0x2982,
    0x1cc23, 0x3002,
    0x1cc24, 0x13d2,
    0x1cc25, 0x8bbd,
    0x1cc26, 0x2802,
    0x1cc27, 0x3012,
    0x1cc28, 0x1002,
    0x1cc29, 0x2032,
    0x1cc2a, 0x3012,
    0x1cc2b, 0x1002,
    0x1cc2c, 0x5cc3,
    0x1cc2d, 0x314,
    0x1cc2e, 0x2942,
    0x1cc2f, 0x3002,
    0x1cc30, 0x1002,
    0x1cc31, 0xd019,
    0x1cc32, 0x2fd2,
    0x1cc33, 0x3002,
    0x1cc34, 0x1002,
    0x1cc35, 0x2a04,
    0x1cc36, 0x3c74,
    0x1cc37, 0x6435,
    0x1cc38, 0x2fa4,
    0x1cc39, 0x3cd4,
    0x1cc3a, 0x6624,
    0x1cc3b, 0x5563,
    0x1cc3c, 0x2d42,
    0x1cc3d, 0x3002,
    0x1cc3e, 0x13d2,
    0x1cc3f, 0x464d,
    0x1cc40, 0x2802,
    0x1cc41, 0x3012,
    0x1cc42, 0x1002,
    0x1cc43, 0x2fd2,
    0x1cc44, 0x3002,
    0x1cc45, 0x1002,
    0x1cc46, 0x2fb4,
    0x1cc47, 0x3cd4,
    0x1cc48, 0x6624,
    0x1cc49, 0x5563,
    0x1cc4a, 0x2d42,
    0x1cc4b, 0x3002,
    0x1cc4c, 0x13d2,
    0x1cc4d, 0x2e72,
    0x1cc4e, 0x3002,
    0x1cc4f, 0x1002,
    0x1cc50, 0x2f72,
    0x1cc51, 0x3002,
    0x1cc52, 0x1002,
    0x1cc53, 0x004,
    0x1cc54, 0x2942,
    0x1cc55, 0x3002,
    0x1cc56, 0x1002,
    0x1cc57, 0x2032,
    0x1cc58, 0x3012,
    0x1cc59, 0x1002,
    0x1cc5a, 0x5cc3,
    0x1cc5b, 0x317,
    0x1cc5c, 0x2f12,
    0x1cc5d, 0x3002,
    0x1cc5e, 0x1002,
    0x1cc5f, 0x2942,
    0x1cc60, 0x3002,
    0x1cc61, 0x1002,
    0x1cc62, 0x22cd,
    0x1cc63, 0x301d,
    0x1cc64, 0x2802,
    0x1cc65, 0x3012,
    0x1cc66, 0x1002,
    0x1cc67, 0x20b2,
    0x1cc68, 0x3012,
    0x1cc69, 0x1002,
    0x1cc6a, 0x5aa3,
    0x1cc6b, 0x2dc2,
    0x1cc6c, 0x3002,
    0x1cc6d, 0x1312,
    0x1cc6e, 0x2d02,
    0x1cc6f, 0x3002,
    0x1cc70, 0x1002,
    0x1cc71, 0x2807,
    0x1cc72, 0x31a7,
    0x1cc73, 0x20c4,
    0x1cc74, 0x3c24,
    0x1cc75, 0x6724,
    0x1cc76, 0x1002,
    0x1cc77, 0x2807,
    0x1cc78, 0x3187,
    0x1cc79, 0x20c4,
    0x1cc7a, 0x3c24,
    0x1cc7b, 0x6724,
    0x1cc7c, 0x1002,
    0x1cc7d, 0x2514,
    0x1cc7e, 0x3c64,
    0x1cc7f, 0x6436,
    0x1cc80, 0xdff4,
    0x1cc81, 0x6436,
    0x1cc82, 0x1002,
    0x1cc83, 0x40a4,
    0x1cc84, 0x643c,
    0x1cc85, 0x4016,
    0x1cc86, 0x8c6c,
    0x1cc87, 0x2b24,
    0x1cc88, 0x3c24,
    0x1cc89, 0x6435,
    0x1cc8a, 0x1002,
    0x1cc8b, 0x2b24,
    0x1cc8c, 0x3c24,
    0x1cc8d, 0x643a,
    0x1cc8e, 0x4025,
    0x1cc8f, 0x8a5a,
    0x1cc90, 0x1002,
    0x1cc91, 0x26d1,
    0x1cc92, 0x3011,
    0x1cc93, 0x1001,
    0x1cc94, 0xc7a0,
    0x1cc95, 0x100,
    0x1cc96, 0xc502,
    0x1cc97, 0x53ac,
    0x1cc98, 0xc503,
    0x1cc99, 0xd5d5,
    0x1cc9a, 0xc600,
    0x1cc9b, 0x2a6d,
    0x1cc9c, 0xc601,
    0x1cc9d, 0x2a4c,
    0x1cc9e, 0xc602,
    0x1cc9f, 0x111,
    0x1cca0, 0xc60c,
    0x1cca1, 0x5900,
    0x1cca2, 0xc710,
    0x1cca3, 0x700,
    0x1cca4, 0xc718,
    0x1cca5, 0x700,
    0x1cca6, 0xc720,
    0x1cca7, 0x4700,
    0x1cca8, 0xc801,
    0x1cca9, 0x7f50,
    0x1ccaa, 0xc802,
    0x1ccab, 0x7760,
    0x1ccac, 0xc803,
    0x1ccad, 0x7fce,
    0x1ccae, 0xc804,
    0x1ccaf, 0x5700,
    0x1ccb0, 0xc805,
    0x1ccb1, 0x5f11,
    0x1ccb2, 0xc806,
    0x1ccb3, 0x4751,
    0x1ccb4, 0xc807,
    0x1ccb5, 0x57e1,
    0x1ccb6, 0xc808,
    0x1ccb7, 0x2700,
    0x1ccb8, 0xc809,
    0x1ccb9, 0x000,
    0x1ccba, 0xc821,
    0x1ccbb, 0x002,
    0x1ccbc, 0xc822,
    0x1ccbd, 0x014,
    0x1ccbe, 0xc832,
    0x1ccbf, 0x1186,
    0x1ccc0, 0xc847,
    0x1ccc1, 0x1e02,
    0x1ccc2, 0xc013,
    0x1ccc3, 0xf341,
    0x1ccc4, 0xc01a,
    0x1ccc5, 0x446,
    0x1ccc6, 0xc024,
    0x1ccc7, 0x1000,
    0x1ccc8, 0xc025,
    0x1ccc9, 0xa00,
    0x1ccca, 0xc026,
    0x1cccb, 0xc0c,
    0x1cccc, 0xc027,
    0x1cccd, 0xc0c,
    0x1ccce, 0xc029,
    0x1cccf, 0x0a0,
    0x1ccd0, 0xc030,
    0x1ccd1, 0xa00,
    0x1ccd2, 0xc03c,
    0x1ccd3, 0x01c,
    0x1ccd4, 0xc005,
    0x1ccd5, 0x7a06,
    0x1ccd6, 0x000,
    0x1ccd7, 0x26d1,
    0x1ccd8, 0x3011,
    0x1ccd9, 0x1001,
    0x1ccda, 0xc620,
    0x1ccdb, 0x000,
    0x1ccdc, 0xc621,
    0x1ccdd, 0x03f,
    0x1ccde, 0xc622,
    0x1ccdf, 0x000,
    0x1cce0, 0xc623,
    0x1cce1, 0x000,
    0x1cce2, 0xc624,
    0x1cce3, 0x000,
    0x1cce4, 0xc625,
    0x1cce5, 0x000,
    0x1cce6, 0xc627,
    0x1cce7, 0x000,
    0x1cce8, 0xc628,
    0x1cce9, 0x000,
    0x1ccea, 0xc62c,
    0x1cceb, 0x000,
    0x1ccec, 0x000,
    0x1cced, 0x2806,
    0x1ccee, 0x3cb6,
    0x1ccef, 0xc161,
    0x1ccf0, 0x6134,
    0x1ccf1, 0x6135,
    0x1ccf2, 0x5443,
    0x1ccf3, 0x303,
    0x1ccf4, 0x6524,
    0x1ccf5, 0x00b,
    0x1ccf6, 0x1002,
    0x1ccf7, 0x2104,
    0x1ccf8, 0x3c24,
    0x1ccf9, 0x2105,
    0x1ccfa, 0x3805,
    0x1ccfb, 0x6524,
    0x1ccfc, 0xdff4,
    0x1ccfd, 0x4005,
    0x1ccfe, 0x6524,
    0x1ccff, 0x1002,
    0x1cd00, 0x5dd3,
    0x1cd01, 0x306,
    0x1cd02, 0x2ff7,
    0x1cd03, 0x38f7,
    0x1cd04, 0x60b7,
    0x1cd05, 0xdffd,
    0x1cd06, 0x00a,
    0x1cd07, 0x1002,
    0x1cd08, 0x000,
 
    /* Unpause the microcontroller to start program */
    0x1ca00,0x0080,
    0x1ca12,0x0000,
  
    /* Secret Registers for Debug
     * - 1.C7A0: margin test threshold. (05/15/08 WWU Changed from 0x100 to relex the test)
     * - 1.CA0B: The wait time between kicking the EDC and reading margin count.
     * - 1.CA09: # of margin count test
     * - 1.CA0E: # of EDC whacks
     */
     
 
     /* / End of sequence marker */
     PHY_INIT_END,   0
};


/* boot control commands */
#define FM_BOOT_COMMAND_SCHED_INIT    0
#define FM_BOOT_COMMAND_ASYNC_REPAIR  1
#define FM_BOOT_COMMAND_INIT_MEM      2
#define FM_BOOT_COMMAND_SYNC_REPAIR   3

#define FM_BOOT_POLL_TIMEOUT          1000024
#define FM_PLL_LOCK_POLL_TIMEOUT      10240

#define FM_PRIV_VAR(x) \
    __ ## x

#define FM_WRITE_CSR( /* fm_switch *  */ switchPtr,                         \
                      /* fm_int       */ sw,                                \
                      /* fm_int       */ reg,                               \
                      /* fm_uint32    */ value,                             \
                      /* fm_status    */ e)                                 \
    if ( ( (e) = switchPtr->WriteUINT32( (sw), (reg), (value) ) ) != FM_OK )\
                 {                                                          \
                     goto ABORT;                                            \
                 }

#define FM_READ_CSR( /* fm_switch *  */ switchPtr,                          \
                     /* fm_int       */ sw,                                 \
                     /* fm_int       */ reg,                                \
                     /* fm_uint32 *  */ ptr,                                \
                     /* fm_status    */ e)                                  \
    if ( ( (e) = switchPtr->ReadUINT32( (sw), (reg), (ptr) ) ) != FM_OK )   \
                 {                                                          \
                     goto ABORT;                                            \
                 }

#define FM_SCAN_SHIFT( /* fm_switch *  */ switchPtr,                        \
                       /* fm_int       */ sw,                               \
                       /* fm_uint32    */ in,                               \
                       /* fm_status    */ e)                                \
    FM_WRITE_CSR( (switchPtr), (sw), FM4000_ASYNC_SCAN_IN, (in), (e) );

#define FM_SCAN_SHIFT_N( /* fm_switch *  */ switchPtr,                         \
                         /* fm_int       */ sw,                                \
                         /* fm_uint32    */ in,                                \
                         /* int          */ count,                             \
                         /* fm_status    */ e)                                 \
    for (int FM_PRIV_VAR(i) = 0 ; FM_PRIV_VAR(i) < (count) ; FM_PRIV_VAR(i)++) \
    {                                                                          \
        FM_SCAN_SHIFT( (switchPtr), (sw), (in), (e) );                         \
    }

#define FM_SCAN_DELAY( /* fm_switch *  */ switchPtr,                 \
                       /* fm_int       */ sw,                        \
                       /* fm_status    */ e)                         \
    {                                                                \
        fm_uint32 value;                                             \
        FM_READ_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_IN, &value, e); \
    }

static fm_status GetHwPort(fm_int sw, fm_int port, fm_int *hwPort)
{

    fm_status status;
    fm_char platformName[32];

    status = fmPlatformGetAttribute(sw, 0, 
                FM_PLATFORM_ATTR_NAME, platformName);
    if (status != FM_OK) 
    {
        return status;
    }

    if (strcasecmp(platformName, "RENO") == 0)
    {
        if ( (port < 0) || (port > 24) )
        {
            return FM_ERR_INVALID_PORT;
        }
        *hwPort = fm85xxepPortRemapTable[port];
        return FM_OK;
    }
    if (strcasecmp(platformName, "monaco") == 0)
    {
        if ( (port < 0) || (port > 24) )
        {
            return FM_ERR_INVALID_PORT;
        }
        *hwPort = monacoPortRemapTable[port];
        return FM_OK;
    }

    printf("Error: GetHwPort - Need to add support for %s platform\n",
                platformName);
    exit(-1);

    return status;

}

static fm_status PollGeneralRegister(fm_int    sw,
                                     fm_uint32 addr,
                                     fm_uint32 bitMask,
                                     fm_uint32 valueMask,
                                     fm_int    maxTimes)
{
    fm_status err;
    fm_uint32 value = 0;
    int       i;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];


    for (i = 0 ; i < maxTimes ; i++)
    {
        err = switchPtr->ReadUINT32(sw, addr, &value);

        if (err != FM_OK)
        {
            return FM_FAIL;
        }

        if ( (value & bitMask) == valueMask )
        {
            return FM_OK;
        }
    }

    /* 1ms delay between reads */
    fmDelay(0, 1000000);

    FM_LOG_FATAL(FM_LOG_CAT_SWITCH_FM4000,
                 "Poll of address 0x%08x timed out in %d reads!\n",
                 addr, maxTimes);

    return FM_FAIL;
} /* PollGeneralRegister */




static fm_status PollBootCtrl(fm_int sw, fm_int maxTimes)
{
    return PollGeneralRegister(sw, FM4000_BOOT_STATUS, 1, 1, maxTimes);
} /* PollBootCtrl */




static fm_status ExecuteBootCommand(fm_int sw, fm_int command)
{
    fm_status err;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];

    err = switchPtr->WriteUINT32(sw, FM4000_BOOT_CTRL, command);

    if (err != FM_OK)
    {
        return err;
    }

    return PollBootCtrl(sw, FM_BOOT_POLL_TIMEOUT);
} /* ExecuteBootCommand */




static fm_status PollFrameHandlerPLLStat(fm_int sw, fm_int maxTimes)
{
    return PollGeneralRegister(sw, FM4000_PLL_FH_STAT, 1, 1, maxTimes);
} /* PollFrameHandlerPLLStat */




static fm_status ProgramFrameHandlerPLL(fm_int sw)
{
    fm_status err;
    fm_uint32 value;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];

    /* From initBoot */
    fm_int    defaultP = FM_AAD_API_FM4000_BOOT_FH_PLL_P;
    fm_int    defaultN = FM_AAD_API_FM4000_BOOT_FH_PLL_N;
    fm_int    defaultM;
    fm_int    ebiClock;
    fm_int    targetFreq;

    ebiClock = FM_AAD_API_SYSTEM_EBI_CLOCK;

    /***************************************************
     * The target frame handler clock is 375 MHz.  The
     * formula to compute the FH clock is:
     *   EBI_CLK * M/N/(2**P)
     * Where:
     *   70 MHz <= (EBI_CLK * M / N) 650 Mhz
     *   0 <= P <= 3
     *   4 <= M <= 127
     *   1 <= N <= 15
     **************************************************/

    /***************************************************
     * Compute defaults in case boot parameters are not
     * set.  We use a default setting of P = 0 and N = 4
     **************************************************/

    targetFreq = FM_AAD_API_FM4000_BOOT_FHFREQ;

    defaultM = (fm_int) (targetFreq * 1.0 * defaultN * (1 << defaultP) / ebiClock);


    err = switchPtr->ReadUINT32(sw, FM4000_PLL_FH_CTRL, &value);

    if (err != FM_OK)
    {
        return err;
    }

    /* zero out bits 14:1 to clear the multipliers and reset the powerdown */
    value &= ~0x7ffe;

    value |= (defaultP & 0x03) << 2;
    value |= (defaultM & 0x7f) << 4;
    value |= (defaultN & 0x0f) << 11;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, "P=%d M=%d N=%d for CLK=%f\n",
                 defaultP,
                 defaultM,
                 defaultN,
                 FM_EBI_MHZ * 1.0);

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, "Wrote PLL_FH_CTRL=0x%x \n", value);

    err = switchPtr->WriteUINT32(sw, FM4000_PLL_FH_CTRL, value);

    if (err != FM_OK)
    {
        return err;
    }

    err = PollFrameHandlerPLLStat(sw, FM_PLL_LOCK_POLL_TIMEOUT);

    if (err != FM_OK)
    {
        return FM_ERR_NO_PLL_LOCK;
    }

    return FM_OK;
} /* ProgramFrameHandlerPLL */




/** fm4000PreAdjustSchedulerTokens
 *
 * \desc        prepares the switch for adjustment of the number of scheduler
 *              tokens
 *
 * \param[in]   sw the switch number that is to be prepared
 *
 * \note        this function depends on ASYNC memory repair, while the
 *              remainder of the boot process depends on this function
 *
 *****************************************************************************/
static fm_status fm4000PreAdjustSchedulerTokens(fm_int sw)
{
    fm_status status;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];


    /* Switch to SCAN mode.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CFG, 0x8001ff2e, status);

    /* Prepare to shift.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CMD, 0x0, status);
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_IN, 0x0, status);
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CMD, 0xff2e, status);

    /* Send in a command to block the TX_QUEUES FREE1 channel.  */
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 1389, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 4, 2, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 132, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 4, 2, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 884, status);

    /* Execute the command and leave SCAN mode.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CFG, 0x0, status)

ABORT:
    return status;
} /* end fm4000PreAdjustSchedulerTokens */




/******************************************************************************
 * fm4000AdjustSchedulerTokens
 *
 * \desc        adjusts the number of scheduler tokens
 *
 * \param[in]   sw the switch number for which to adjust the number of
 *              scheduler tokens
 *
 * \note        this function depends on the scheduler being initialized, while
 *              the remainder of the boot process depends on this function
 *
 *****************************************************************************/
static fm_status fm4000AdjustSchedulerTokens(fm_int sw)
{
    fm_status       status;
    const fm_uint32 dc        = 0;
    const int       numDrains = 3 * (4 + (int) dc) + 4;
    int             i;
    fm_switch      *switchPtr = fmRootApi->fmSwitchStateTable[sw];


    /* Modify the number of per-priority reserved credits.  */
    FM_WRITE_CSR(switchPtr, sw, 0x600FD, dc << 24, status);

    /* Switch to SCAN mode.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CFG, 0x8001ff2e, status);

    /* Prepare to shift.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CMD, 0x0, status);
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_IN, 0x0, status);
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CMD, 0xff2e, status);

    /* Send in the drain command on the TX_QUEUES FREE1 channel.  */
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 1375, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 4, 15, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 119, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 4, 15, status);
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 885, status);

    /* Execute the drain command.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CMD, 0x0, status);

    for (i = 0 ; i < numDrains ; i++)
    {
        FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_IN, 0x0, status)
        FM_SCAN_DELAY(switchPtr, sw, status);
        FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_IN, 0x0, status)
        FM_SCAN_DELAY(switchPtr, sw, status);
    }

    /* Prepare to shift.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CMD, 0xff2e, status);

    /* Put the TX_QUEUES FREE 1 channel back in pass through mode.  */
    FM_SCAN_SHIFT_N(switchPtr, sw, 0, 2409, status);

    /* Execute the command and leave SCAN mode.  */
    FM_WRITE_CSR(switchPtr, sw, FM4000_ASYNC_SCAN_CFG, 0x0, status);

ABORT:
    return status;
} /* fm4000AdjustSchedulerTokens */


#if 0
static fm_status fm4000SetPortSpeed(fm_int  sw,
                                    fm_int  physPort,
                                    fm_int  speed)
{

    fm_uint32 s;
    fm_status err;

    /* set some bits that don't have explicit attrs */
    err = switchPtr->ReadUINT32(sw, FM4000_PCS_CFG_1(physPort), &s);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    /* 2.5Gbps and below all run in one-lane mode, so they need to be
     *  configured differently than 10Gbps */
    if (speed <= 2500)
    {
        FM_SET_BIT(s, FM4000_PCS_CFG_1, ShortPreamble, 1); 
        FM_SET_BIT(s, FM4000_PCS_CFG_1, IgnoreIFGErrors, 1); 
        FM_SET_BIT(s, FM4000_PCS_CFG_1, EnableDIC, 0); 
    }
    else
    {
        /* configure for 10 Gbps mode */
        FM_SET_BIT(s, FM4000_PCS_CFG_1, ShortPreamble, 0); 
        FM_SET_BIT(s, FM4000_PCS_CFG_1, IgnoreIFGErrors, 0); 
        /* FM_SET_BIT(s, FM4000_PCS_CFG_1, EnableDIC, entry->dicEnabled); */
    }

    switch (speed)
    {
        case 10000:
            FM_SET_FIELD(s, FM4000_PCS_CFG_1, DatapathSelect, 0);
            break;

        case 2500:
        case 1000:
            FM_SET_FIELD(s, FM4000_PCS_CFG_1, DatapathSelect, 1);
            break;

        case 100:
            FM_SET_FIELD(s, FM4000_PCS_CFG_1, DatapathSelect, 2);
            break;

        case 10:
            FM_SET_FIELD(s, FM4000_PCS_CFG_1, DatapathSelect, 3);
            break;

    }   /* end switch (speed) */

    err = switchPtr->WriteUINT32(sw, FM4000_PCS_CFG_1(physPort), s);

ABORT:
    
    FM_LOG_EXIT(FM_LOG_CAT_PORT_FM4000, err);

}
#endif


static fm_status fm4000ReleasePortSlave(fm_int  sw,
                                        fm_int  physPort,
                                        fm_bool initDataPath,
                                        fm_int  clockSel)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT_FM4000,
                 "sw=%d physPort=%d initDataPath=%d clockSel=%d\n",
                 sw, physPort, initDataPath, clockSel);


    if ( (clockSel < 0) || (clockSel > 1) )
    {
        FM_LOG_ERROR(FM_LOG_CAT_PORT_FM4000,
                     "Invalid clock selection %d (must be 0 or 1)\n",
                     clockSel);

        err = FM_ERR_INVALID_ARGUMENT;

        goto ABORT;
    }


    /***************************************************
     * We must set the datapath initialization bit
     * before we exit reset.
     **************************************************/

    err = fmWriteUINT32Field(sw,
                             FM4000_EPL_PORT_CTRL(physPort),
                             FM4000_EPL_PORT_CTRL_b_InitializeN,
                             1,
                             initDataPath ? 0 : 1);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    /***************************************************
     * We must set the clock selection bit before we
     * exit reset.
     **************************************************/

    err = fmWriteUINT32Field(sw,
                             FM4000_EPL_PORT_CTRL(physPort),
                             FM4000_EPL_PORT_CTRL_b_ClockSelect,
                             1,
                             clockSel);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    /***************************************************
     * Now put the port out of reset.
     **************************************************/

    err = fmWriteUINT32Field(sw,
                             FM4000_EPL_PORT_CTRL(physPort),
                             FM4000_EPL_PORT_CTRL_b_ResetN,
                             1,
                             1);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    /***************************************************
     * After exiting reset, the datapath initialization
     * bit should always be cleared, since we only need to
     * initialize the data path once per switch reset.
     **************************************************/

    err = fmWriteUINT32Field(sw,
                             FM4000_EPL_PORT_CTRL(physPort),
                             FM4000_EPL_PORT_CTRL_b_InitializeN,
                             1,
                             1);

    if (err != FM_OK)
    {
        goto ABORT;
    }


ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_PORT_FM4000, err);

}   /* end ReleasePort */




#define ROUND_TO_UINT(x)  ( (fm_uint) ( (x) + 0.5 ) )

#define SPS_ME_RX  0x01     /* RX enable */
#define SPS_ME_TX  0x02     /* TX enable */
#define SPS_PWRON  0x04     /* Power on */
#define SPS_BIST   0x08     /* SerDes Built In Self Test */
#define SPS_REMF   0x10     /* Remote Fault */

#define SPS_ME     (SPS_ME_RX | SPS_ME_TX)


/*****************************************************************************/
/** SetSerdesPowerState
 *
 * \desc            This function disables or enables the serdes lanes.
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       physPort is the physical port number
 *
 * \param[in]       state is the desired state:
 *                  TRUE to enable the serdes lanes
 *                  FALSE to disable the serdes lanes
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status SetSerdesPowerState(fm_int  sw,
                                     fm_int  physPort,
                                     fm_bool state)
{
    fm_status status = FM_OK;
    fm_uint32 ctrl2;
    fm_uint32 ds;
    fm_uint32 pcs1;
    fm_uint32 signal;
    fm_uint32 stm;
    fm_uint32 testMode;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_LOG_ENTRY(FM_LOG_CAT_PORT_FM4000,
                 "sw=%d physPort=%d state=%d\n",
                 sw,
                 physPort,
                 state);


    /**************************************************
     * Save the current drive and equalization for the
     * SerDes, then set it to zero to minimize noise
     * from the SerDes when the SerDes is brought out
     * of reset. Otherwise, noise from the SerDes can
     * result in an overflow condition in the SYNCBUF
     * unit of the chip.
     **************************************************/
    status = switchPtr->ReadUINT32(sw, FM4000_SERDES_CTRL_1(physPort), &signal);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_1(physPort), 0x0);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /***************************************************
     * Retrieve the DatapathSelect field to decide which
     * lanes need to be powered.
     **************************************************/
    status = switchPtr->ReadUINT32(sw, FM4000_PCS_CFG_1(physPort), &pcs1);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    ds = FM_GET_FIELD(pcs1, FM4000_PCS_CFG_1, DatapathSelect);

    if (!state)
    {
        /***************************************************
         * Disable the reception of frames to ensure that
         * the MAC unit does not get confused when the
         * SerDes is shut down.
         **************************************************/

        FM_SET_BIT(pcs1, FM4000_PCS_CFG_1, DisableReceiverRS, 1);

        status = switchPtr->WriteUINT32(sw, FM4000_PCS_CFG_1(physPort), pcs1);

        if (status != FM_OK)
        {
            goto ABORT;
        }
    }

    /***************************************************
     * Assert the reset line of the SYNCBUF unit.
     **************************************************/
    status = switchPtr->ReadUINT32(sw, FM4000_SERDES_TEST_MODE(physPort), &stm);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* Cache the current value of the TestMode field.  */
    testMode = FM_GET_FIELD(stm, FM4000_SERDES_TEST_MODE, TestMode);

    FM_SET_FIELD(stm, FM4000_SERDES_TEST_MODE, TestMode, 2);

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_TEST_MODE(physPort), stm);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    status = switchPtr->ReadUINT32(sw, FM4000_SERDES_CTRL_2(physPort), &ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /***************************************************
     * Change the power state for lanes A and B.
     **************************************************/
    if (!state)
    {
        ctrl2 |= (0x3 << FM4000_SERDES_CTRL_2_l_LanePowerDown);
    }
    else
    {
        ctrl2 &= ~(0x3 << FM4000_SERDES_CTRL_2_l_LanePowerDown);

        if (ds)
        {
            /* The EPL is configured to operate at speed of 10 Mb/s, 100 Mb/s
             * or 1 Gb/s.  */
            ctrl2 |= (0x2 << FM4000_SERDES_CTRL_2_l_LanePowerDown);
        }
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* 10us delay.  */
    fmDelay(0, 10000);

    /***************************************************
     * Change the power state of the PLL for lanes A and
     * B.
     **************************************************/
    if (!state)
    {
        FM_SET_BIT(ctrl2, FM4000_SERDES_CTRL_2, PLLResetAB, 1);
    }
    else
    {
        FM_SET_BIT(ctrl2, FM4000_SERDES_CTRL_2, PLLResetAB, 0);
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* 10us delay.  */
    fmDelay(0, 10000);

    /***************************************************
     * Change the reset state for lanes A and B.
     **************************************************/
    if (!state)
    {
        ctrl2 |= (0x3 << FM4000_SERDES_CTRL_2_l_LaneReset);
    }
    else
    {
        ctrl2 &= ~(0x3 << FM4000_SERDES_CTRL_2_l_LaneReset);

        if (ds)
        {
            /* The EPL is configured to operate at speed of 10 Mb/s, 100 Mb/s
             * or 1 Gb/s.  */
            ctrl2 |= (0x2 << FM4000_SERDES_CTRL_2_l_LaneReset);
        }
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* 10us delay.  */
    fmDelay(0, 10000);

    /***************************************************
     * Change the power state for lanes C and D.
     **************************************************/
    if (!state)
    {
        ctrl2 |= (0xC << FM4000_SERDES_CTRL_2_l_LanePowerDown);
    }
    else
    {
        ctrl2 &= ~(0xC << FM4000_SERDES_CTRL_2_l_LanePowerDown);

        if (ds)
        {
            /* The EPL is configured to operate at speed of 10 Mb/s, 100 Mb/s
             * or 1 Gb/s.  */
            ctrl2 |= (0x4 << FM4000_SERDES_CTRL_2_l_LanePowerDown);
        }
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* 10us delay.  */
    fmDelay(0, 10000);

    /***************************************************
     * Change the power state of the PLL for lanes C and
     * D.
     **************************************************/
    if (!state)
    {
        FM_SET_BIT(ctrl2, FM4000_SERDES_CTRL_2, PLLResetCD, 1);
    }
    else
    {
        FM_SET_BIT(ctrl2, FM4000_SERDES_CTRL_2, PLLResetCD, 0);
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* 10us delay.  */
    fmDelay(0, 10000);

    /***************************************************
     * Change the reset state for lanes C and D.
     **************************************************/
    if (!state)
    {
        ctrl2 |= (0xC << FM4000_SERDES_CTRL_2_l_LaneReset);
    }
    else
    {
        ctrl2 &= ~(0xC << FM4000_SERDES_CTRL_2_l_LaneReset);

        if (ds)
        {
            /* The EPL is configured to operate at speed of 10 Mb/s, 100 Mb/s
             * or 1 Gb/s.  */
            ctrl2 |= (0x4 << FM4000_SERDES_CTRL_2_l_LaneReset);
        }
    }

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), ctrl2);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* 10us delay.  */
    fmDelay(0, 10000);

    /***************************************************
     * Restore the previously configured drive and
     * equalization.
     **************************************************/
    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_1(physPort), signal);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /***************************************************
     * De-assert the reset line of SYNCBUF unit.
     **************************************************/
    FM_SET_FIELD(stm, FM4000_SERDES_TEST_MODE, TestMode, testMode);

    status = switchPtr->WriteUINT32(sw, FM4000_SERDES_TEST_MODE(physPort), stm);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    if (state)
    {
        /***************************************************
         * Re-enable frame reception.
         **************************************************/
        FM_SET_BIT(pcs1, FM4000_PCS_CFG_1, DisableReceiverRS, 0);

        status = switchPtr->WriteUINT32(sw, FM4000_PCS_CFG_1(physPort), pcs1);

        if (status != FM_OK)
        {
            goto ABORT;
        }
    }

ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_PORT_FM4000, status);

}   /* end SetSerdesPowerState */




static fm_status fm4000UpdateDrainTx(fm_int sw, fm_int physPort, fm_bool active)
{
    fm_uint32 macCfg2;
    fm_int    err;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];

    err = switchPtr->ReadUINT32(sw, FM4000_MAC_CFG_2(physPort), &macCfg2);

    if (err != FM_OK)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PORT_FM4000,
                     "error %s (%d) from switchPtr->ReadUINT32 for "
                     "MAC_CFG_2 register for physical port %d\n",
                     fmErrorMsg(err), err, physPort);

        return err;
    }

    if (active)
    {
        macCfg2 |= 1 << FM4000_MAC_CFG_2_b_DrainTX;
    }
    else
    {
        macCfg2 &= ~(1 << FM4000_MAC_CFG_2_b_DrainTX);
    }

    err = switchPtr->WriteUINT32(sw, FM4000_MAC_CFG_2(physPort), macCfg2);

    if (err != FM_OK)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PORT_FM4000,
                     "error %s (%d) from fmWriteUINT32 for "
                     "MAC_CFG_2 register for physical port %d\n",
                     fmErrorMsg(err), err, physPort);

        return err;
    }

    return FM_OK;

}   /* end fm4000UpdateDrainTx */


void ealPhyInit(fm_int sw, fm_int port, fm_int physPort, fm_int speed)
{
    fm_int        w1;
    fm_int        w2;
    fm_int        initIndex;
    fm_uint16     modeSel;
    fm_uint16     sgmiiReg;
    fm_uint32     rv;
    fm_switch    *switchPtr = fmRootApi->fmSwitchStateTable[sw];

    /* Program the MDIO on the switch */

    switchPtr->WriteUINT32(sw, FM4000_MDIO_CFG, (1 << 13) + (1 << 12) + 65);

    /* Program the GPIO to output for pins (0..3) */

    switchPtr->WriteUINT32(sw, FM4000_GPIO_CFG, 7);

    initIndex = 0;
    while (aelInitSequence[initIndex] != PHY_INIT_END)
    {
        w1 = aelInitSequence[initIndex++];
        w2 = aelInitSequence[initIndex++];

        if (w1 == PHY_INIT_PAUSE)
        {
            fmDelay(0, w2 * 1000000);
        }
        else
        {
            fmPlatformMdioWrite(sw, 1, port, w1 >> 16, w1 & 0xffff, w2);
        }
    }


    fmPlatformMdioRead(sw, 0,
                           port,
                           1, /* AEL2005_MODE_SEL_DEV */
                           0xC001, /* AEL2005_MODE_SEL */
                           &modeSel);
                        
    fmPlatformMdioRead(sw, 0,
                           port,
                           1, /* AEL2005_UNRETIMED_SGMII_DEV */
                           0xC220, /* AEL2005_UNRETIMED_SGMII */
                           &sgmiiReg);

    FM_LOG_DEBUG(FM_LOG_CAT_PHY,
                 "port=%d read: modeSel=0x%x sgmiiReg=0x%x\n",
                 port, modeSel, sgmiiReg);

    if (speed <= 1000)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PHY,
                     "port=%d writing PHY registers for 1G\n",
                     port);

        /* set bit 6 in mode sel to indicate SGMII mode */
        modeSel |= (1 << 6);

        /***************************************************
         * The SGMII unretimed register doesn't seem to be
         * documented in the spec, but the following bit
         * settings came from NetLogic support.
         **************************************************/

        /* set bits 12,13 */
        sgmiiReg |= (3 << 12);

        /* using XAUI lane 0 => clear bits 9:8 and 5:4 */
        sgmiiReg &= ~((3 << 8) | (3 << 4));

        /* flip RX polarity */
        sgmiiReg |= (3 << 2);
    }
    else
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PHY,
                     "port=%d writing PHY registers for 10G\n",
                     port);

        /* clear bit 6 in mode sel to indicate XAUI mode */
        modeSel &= ~(1 << 6);

        /* clear bits 12,13 */
        sgmiiReg &= ~(3 << 12);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PHY,
                 "port=%d writing: modeSel=0x%x sgmiiReg=0x%x\n",
                 port, modeSel, sgmiiReg);

    fmPlatformMdioWrite(sw, 0,
                            port,
                            1, /* AEL2005_MODE_SEL_DEV */
                            0xC001, /* AEL2005_MODE_SEL */
                            modeSel);
                        
    fmPlatformMdioWrite(sw, 0,
                            port,
                            1, /* AEL2005_UNRETIMED_SGMII_DEV */
                            0xC220, /* AEL2005_UNRETIMED_SGMII */
                            sgmiiReg);

    /***************************************************
     * This seems to be required to happen after the 
     * PHY register is set otherwise it doesn't work..
     **************************************************/
    switchPtr->ReadUINT32(sw, FM4000_SERDES_CTRL_2(physPort), &rv);

    FM_SET_FIELD(rv, FM4000_SERDES_CTRL_2, TX_PolarityReversal,
                 ((speed <= 1000) ? 0xf : 0));

    switchPtr->WriteUINT32(sw, FM4000_SERDES_CTRL_2(physPort), rv);


    return;
}

/*****************************************************************************/
/** fm4000FibmBootstrapSw
 *  \ingroup intPlatform
 *
 * \desc            Function to re-initialize the switch so that it can be
 *                  run as an FIBM slave. This function is provided for
 *                  demonstration puposes, since it is part of the API and
 *                  if the API is already running on the switch, there would
 *                  likely be no need to run it under the control of FIBM.
 *
 * \note            This function assumes that the API of which it is apart
 *                  already has all local state, particularly a switch lock.
 *
 * \param[in]       localSw is the switch number.
 *
 * \param[in]       fibmPort is port which is used for fibm communication.
 *
 * \param[in]       portSpeed is the speed in Mbps to bring up the port at.
 *
 * \param[in]       fibmGlort is the fibm glort for the switch. It is the glort
 *                  that allows the switch to accept and process the fibm request
 *                  messages.
 *
 * \param[in]       fibmDstGlort is the fibm destination glort for the interrupt
 *                  messages. This would be the master fibm glort, in master slave
 *                  configuration. For standalone NIC, it would the the glort of the
 *                  port that is directed connected to the NIC.
 *
 * \param[in]       postIntr is whether to generate interrupt after done bootstrapping
 *                  This can be used by the platform to detect that the switch has 
 *                  bootstrap up succesfully and thus to proceed insert the switch
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000FibmBootstrapSw(fm_int localSw, 
                                fm_int fibmPort, 
                                fm_int portSpeed,
                                fm_uint32 fibmGlort, 
                                fm_uint32 fibmDstGlort,
                                fm_bool postIntr)
{
    fm_status  status;
    fm_uint32  val32;
    fm_int     physPort;
    fm_uint64  rv;
    fm_uint32  cv;
    fm_uint32  physMask;
    fm_uint    camIndex;
    fm_uint    glortIndex;

    fm_status  err = FM_OK;
    fm_int     i;

    fm_uint64  vpdInfo1;
    fm_uint64  vpdInfo2;

    fm_uint32  sliceCfg;
    fm_int     numSegments;
    fm_int     portDisableMask;
    fm_int     serdesHysteresis;
    fm_int     pcsHysteresis;
    fm_bool    fullBoot;
    fm_bool    vpdLocked;
    fm_uint32  value;
    fm_uint64  valueLong;
    fm_int     maskRev = 0;
    fm_bool    doEalPhyInit = FALSE;
    fm_char    platformName[32];
    fm_switch *switchPtr;
    
    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH_FM4000, "sw=%d\n", localSw);

    FM_LOG_INFO(FM_LOG_CAT_SWITCH_FM4000,
                "Bootstrap FIBM switch: port %d, fibmGlort 0x%04x, dstGlort 0x%04x\n",
                fibmPort, 
                fibmGlort, 
                fibmDstGlort);

    VALIDATE_SWITCH_INDEX(localSw);
    VALIDATE_SWITCH_LOCK(localSw);                                                      \
    LOCK_SWITCH(localSw);                                                      \

    switchPtr = fmRootApi->fmSwitchStateTable[localSw];

    /* This is done on the slave switch, but from the perspective of the
     * slave switch, it is writing to its own configuration
     */


    /* Most of the code below are taken partially from fm4000BootSwitch
     *
     * The platform bringup engineer can follow this example to build
     * bootstrap registers list to bring the switch up via eeprom, etc.
     */

    err = fmPlatformGetAttribute(localSw, 
                                 0, 
                                 FM_PLATFORM_ATTR_NAME, 
                                 platformName); 
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    if (strcasecmp(platformName, "monaco") == 0)
    {
        doEalPhyInit = TRUE;
    }

    if (localSw != 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                     "Only switch 0 is support for %s\n",
                     platformName);
        err = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    /* This is where the connection to the master switch */
    err = GetHwPort(localSw, fibmPort, &physPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    /* get parameters for boot */
    numSegments      = FM_AAD_API_FM4000_BOOT_NUMSEGMENTS;
    portDisableMask  = FM_AAD_API_FM4000_BOOT_PORTDISABLEMASK;
    fullBoot         = TRUE;
    serdesHysteresis = FM_AAD_API_FM4000_BOOT_SERDESHYSTERESIS;
    pcsHysteresis    = FM_AAD_API_FM4000_BOOT_PCSHYSTERESIS;

    /* Initialize other bootstrap information */


    /***************************************************
     * Step 0: Take the switch out of reset.
     **************************************************/
    fmPlatformReset(localSw);

    /* Wait 1ms for reset line to stay asserted */
    fmDelay(0, 1000000);

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                 "Taking chip %d out of reset... \n", localSw);

    fmPlatformRelease(localSw);

    /* Wait 10ms to let the fusebox load */
    fmDelay(0, 10000000);

    /***************************************************
     * Step 1: Check VPD_INFO_1 & VPD_INFO_2
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                     "Checking VPD_INFO registers for valid data\n");

        err = switchPtr->ReadUINT64(localSw, FM4000_VPD_INFO_1(0), &vpdInfo1);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

        vpdLocked = vpdInfo1 & 1;
        maskRev   = (vpdInfo1 >> 32) & 0xff;

        err = switchPtr->ReadUINT64(localSw, FM4000_VPD_INFO_2(0), &vpdInfo2);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

        /* Overwrite with default if the fusebox is not burned and locked */
        if (!vpdLocked)
        {
            vpdInfo1 = ( FM_LITERAL_64(1) << 55 ) |                /* FIBM enable */
                       ( (portDisableMask & 0xffffff) << 1 ) |     /* Port disable */
                       ( ( (fm_uint64) numSegments ) << 40 );      /* # Segments */

            FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                         "Writing new default value 0x%llx to VPD_INFO_1\n",
                         vpdInfo1);

            /* write default value in */
            switchPtr->WriteUINT64(localSw, FM4000_VPD_INFO_1(0), vpdInfo1);
        }

        if (!vpdInfo2)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                         "Writing new default value 0x%x to VPD_INFO_2\n",
                         0x215);

            /* write default value in */
            switchPtr->WriteUINT64(localSw, FM4000_VPD_INFO_2(0), 0x215);
        }
    }

    /***************************************************
     * Step 2: Initialize the async memory repair.
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, 
                     "Repairing async memory errors...\n");

        if (maskRev < 3)
        {
            for (i = 0 ; i < FM4000_VPD_ASYNC_RAM_REPAIR_ENTRIES ; i++)
            {
                switchPtr->ReadUINT32(localSw, 
                                      FM4000_VPD_ASYNC_RAM_REPAIR(i), 
                                      &value);

                if (value)
                {
                    switchPtr->WriteUINT32(localSw, 
                                           FM4000_VPD_ASYNC_RAM_REPAIR(i), 
                                           0);
                }
                
            }   /* end for (i = 0 ; i < FM4000_VPD_ASYNC_RAM_REPAIR_ENTRIES... */
            
        }   /* end if (maskRev < 3) */

        err = ExecuteBootCommand(localSw, FM_BOOT_COMMAND_ASYNC_REPAIR);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    if ( fmGetBoolApiAttribute(FM_AAK_API_FM4000_BOOT_ADJUST_SCHED_TOKENS,
                               FM_AAD_API_FM4000_BOOT_ADJUST_SCHED_TOKENS) )
    {
        err = fm4000PreAdjustSchedulerTokens(localSw);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    /***************************************************
     * Step 3: Initialize the scheduler.
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, 
                     "Initializing the scheduler...\n");

        err = ExecuteBootCommand(localSw, FM_BOOT_COMMAND_SCHED_INIT);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    if ( fmGetBoolApiAttribute(FM_AAK_API_FM4000_BOOT_ADJUST_SCHED_TOKENS,
                               FM_AAD_API_FM4000_BOOT_ADJUST_SCHED_TOKENS) )
    {
        err = fm4000AdjustSchedulerTokens(localSw);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    /***************************************************
     * Step 4: Initialize the frame handler PLL.
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                     "Configuring FH PLL and waiting for lock\n");

        err = ProgramFrameHandlerPLL(localSw);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }

    /***************************************************
     * Step 5: Bring all modules out of reset.
     **************************************************/

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                 "Bringing all modules out of reset\n");

    switchPtr->WriteUINT32(localSw, FM4000_SOFT_RESET, 0x0);

    /***************************************************
     * Step 6: Bring MSB/EPL0 out of reset.
     **************************************************/

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                 "Bringing CPU port out of reset\n");

    err = fm4000ReleasePortSlave(localSw, 0, TRUE, 0);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    /***************************************************
     * Step 7: Enable the FFU.
     **************************************************/

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, "Enabling the FFU\n");

    switchPtr->WriteUINT32(localSw, FM4000_SYS_CFG_8, 1);

    /***************************************************
     * Step 8: Initialize the sync memory repair.
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, 
                     "Repairing sync memory errors...\n");

        if (maskRev < 3)
        {
            for (i = 0 ; i < FM4000_VPD_SYNC_RAM_REPAIR_ENTRIES ; i++)
            {
                switchPtr->ReadUINT64(localSw, 
                                      FM4000_VPD_SYNC_RAM_REPAIR(i, 0), 
                                      &valueLong);

                if (valueLong)
                {
                    switchPtr->WriteUINT64(localSw, 
                                           FM4000_VPD_SYNC_RAM_REPAIR(i, 0), 
                                           0);
                }
                
            }   /* end for (i = 0 ; i < FM4000_VPD_SYNC_RAM_REPAIR_ENTRIES... */
            
        }   /* end if (maskRev < 3) */

        err = ExecuteBootCommand(localSw, FM_BOOT_COMMAND_SYNC_REPAIR);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
        
    }   /* end if (fullBoot) */

    /***************************************************
     * Step 9: Initialize the FFU.
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, "Initializing the FFU\n");

        switchPtr->ReadUINT32(localSw, FM4000_FFU_INIT_SLICE, &sliceCfg);
        switchPtr->WriteUINT32(localSw, FM4000_FFU_INIT_SLICE, sliceCfg);
    }

    /***************************************************
     * Step 10: Bring all ports out of reset.
     *          Note that we need to do this before
     *          memory init so that we initialize the
     *          tag table.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
                 "Bringing fibm port %d out of reset at %d Mbps\n", 
                 fibmPort, 
                 portSpeed);

    err = fm4000ReleasePortSlave(localSw, physPort, TRUE, (portSpeed < 2500));
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);

    switchPtr->WriteUINT32( localSw, 
                            FM4000_SERDES_CTRL_3(physPort),
                            ( serdesHysteresis | (pcsHysteresis << 20) ) );

    /***************************************************
     * Step 11: Initialize the memory.
     **************************************************/

    if (fullBoot)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, "Initializing memories...\n");

        err = ExecuteBootCommand(localSw, FM_BOOT_COMMAND_INIT_MEM);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, err);
    }


    /***************************************************
     * Step 12: Enable FIBM.
     **************************************************/
     
    status = switchPtr->ReadUINT32(localSw, FM4000_MSB_CFG, &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    FM_SET_BIT(val32, FM4000_MSB_CFG, attachedCpu, 0);
    FM_SET_BIT(val32, FM4000_MSB_CFG, disableIbm, 0);
    FM_SET_BIT(val32, FM4000_MSB_CFG, ibmGlortEn, 0);
    FM_SET_BIT(val32, FM4000_MSB_CFG, interruptGlortEn, 1);
    status = switchPtr->WriteUINT32(localSw, FM4000_MSB_CFG, val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);


    /* FIBM Source Glort port for response and interrupt frames */
    status = switchPtr->ReadUINT32(localSw, FM4000_MSB_IBM_GLORT, &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    /* Note this should match in the GetFibmSwitchConfig */
    val32  = fibmGlort;
    status = switchPtr->WriteUINT32(localSw, FM4000_MSB_IBM_GLORT, val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* FIBM Dest Glort and Interrupt Interval */
    status = switchPtr->ReadUINT32(localSw, FM4000_MSB_IBM_INT, &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    val32 = (0xFFF << FM4000_MSB_IBM_INT_l_interruptInterval) |
            fibmDstGlort;
    status = switchPtr->WriteUINT32(localSw, FM4000_MSB_IBM_INT, val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* FIBM Ethertype */
    status = switchPtr->ReadUINT32(localSw, FM4000_MSB_INT_FRAME, &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    FM_SET_FIELD(val32, FM4000_MSB_INT_FRAME, etherType, FM_FIBM_ETHERTYPE);
    FM_SET_FIELD(val32, FM4000_MSB_INT_FRAME, islSysPri, FM_FIBM_SYS_PRI);
    status = switchPtr->WriteUINT32(localSw, FM4000_MSB_INT_FRAME, val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Don't drop mgmt ISL on CPU port */
    status = switchPtr->ReadUINT32(localSw, FM4000_PORT_CFG_1(0), &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, 0);
    status = switchPtr->WriteUINT32(localSw, FM4000_PORT_CFG_1(0), val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Use F64 Tag on CPU port */
    status = switchPtr->ReadUINT32(localSw, FM4000_PARSE_CFG(0), &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    FM_SET_FIELD(val32, FM4000_PARSE_CFG, ISLTag, FM4000_ENABLE_F64_TAG);
    status = switchPtr->WriteUINT32(localSw, FM4000_PARSE_CFG(0), val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Don't drop mgmt ISL on FIBM mgmt port */
    status = switchPtr->ReadUINT32(localSw, FM4000_PORT_CFG_1(physPort), &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, 0);
    status = switchPtr->WriteUINT32(localSw, FM4000_PORT_CFG_1(physPort), val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Use F64 Tag on FIBM mgmt port */
    status = switchPtr->ReadUINT32(localSw, FM4000_PARSE_CFG(physPort), &val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    FM_SET_FIELD(val32, FM4000_PARSE_CFG, ISLTag, FM4000_ENABLE_F64_TAG);
    status = switchPtr->WriteUINT32(localSw, FM4000_PARSE_CFG(physPort), val32);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /***************************************************
     * Step 13: Add a CPU entry for slave switch to
     *          receive fibm messages.
     **************************************************/
    glortIndex = FM4000_FIBM_CPU_GLORT_INDEX;
    camIndex   = FM4000_FIBM_CPU_CAM_INDEX;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, 
        "Installing a CAM index %d glort index %d to direct "
        "FIBM request messages (glort 0x%04x) to CPU port\n",
        camIndex, 
        glortIndex, 
        fibmGlort);

    /* Write into the RAM first. */
    rv = 0;

    /* strict */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       Strict,
                       FM_GLORT_ENTRY_TYPE_ISL );

    /* destIndex */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestIndex,
                       glortIndex );

    /* rangeSubIndexA */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexA,
                       0 );

    /* rangeSubIndexB */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexB,
                       0 );

    /* destCount */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestCount,
                       1 );

    /* hashRotation */
    FM_ARRAY_SET_BIT((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       HashRotation,
                       FM_GLORT_ENTRY_HASH_A );

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortRam,
                                       (fm_uint32 *)&rv,
                                       camIndex,
                                       FALSE );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Now write into the CAM. */
    cv = (fibmGlort & 0xffff) |         /* key */
         ( (0xFFFF & 0xffff) << 16 );   /* mask */

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortCam,
                                       &cv,
                                       camIndex,
                                       FALSE );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Now write destination glort table. */
    physMask = (1 << 0); /* CPU port */
    rv = 0;
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_DEST_TABLE,
                       DestMask,
                       physMask );

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortDestTable,
                                       (fm_uint32 *)&rv,
                                       glortIndex,
                                       FALSE );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /***************************************************
     * Step 14: Add an entry for sending back to
     *          the master switch. This entry will be
     *          removed after the master switch
     *          has initialize the CPU entry in its
     *          normal processing path
     **************************************************/
    glortIndex = FM4000_FIBM_MASTER_GLORT_INDEX;
    camIndex   = FM4000_FIBM_MASTER_CAM_INDEX;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, 
                 "Installing a CAM index %d glort index %d to direct FIBM "
                 "response and interrupt messages (glort 0x%04x) to hw port %d\n",
                 camIndex, 
                 glortIndex, 
                 fibmDstGlort, 
                 physPort);

    /* Write into the RAM first. */
    rv = 0;

    /* strict */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       Strict,
                       FM_GLORT_ENTRY_TYPE_ISL );

    /* destIndex */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestIndex,
                       glortIndex );

    /* rangeSubIndexA */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexA,
                       0 );

    /* rangeSubIndexB */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexB,
                       0 );

    /* destCount */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestCount,
                       1 );

    /* hashRotation */
    FM_ARRAY_SET_BIT((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       HashRotation,
                       FM_GLORT_ENTRY_HASH_A );

    status = fmRegCacheWriteSingle1D( localSw,
                                      &fm4000CacheGlortRam,
                                      (fm_uint32 *) &rv,
                                      camIndex,
                                      FALSE );

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);


    /* Now write into the CAM. */
    cv = (fibmDstGlort & 0xFFFF) |  /* key */
         ( (0xFFFF) << 16 );        /* mask - exactly match */

    status = fmRegCacheWriteSingle1D( localSw,
                                      &fm4000CacheGlortCam,
                                      &cv,
                                      camIndex,
                                      FALSE );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);


    /* Now Write destination glort table. */
    physMask = (1 << physPort); /* Which port is connected to the control switch */
    rv = 0;
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_DEST_TABLE,
                       DestMask,
                       physMask );

    status = fmRegCacheWriteSingle1D( localSw,
                                      &fm4000CacheGlortDestTable,
                                      (fm_uint32 *)&rv,
                                      glortIndex,
                                      FALSE );

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /***************************************************
     * Step 15: Miscellaneous port configuration
     *           and bring the link up.
     **************************************************/
     
    /***************************************************
     * Turn off check end bit, due to bali bringup
     * issues. Turn on local fault and remore fault
     * response.
     **************************************************/
    switchPtr->ReadUINT32(localSw, FM4000_PCS_CFG_1(physPort), &value);
    value &= ~(1 << 31);
    value |= (1 << FM4000_PCS_CFG_1_b_EnableLF_Response);
    value |= (1 << FM4000_PCS_CFG_1_b_EnableSendingRF);
    switchPtr->WriteUINT32(localSw, FM4000_PCS_CFG_1(physPort), value);

    /***************************************************
     * Set sync buf config for proper PCS configuration.
     **************************************************/
    switchPtr->WriteUINT32(localSw, FM4000_SYNCBUF_CFG(physPort), 0x3f00);

    /***************************************************
     * Set the jitter watermark to the proper
     * experimentally derived value.
     **************************************************/
    switchPtr->WriteUINT32(localSw, FM4000_JITTER_TIMER(physPort), 0x1c1006);


    /* perform port-specific initializations */
    /* set port specific emphasis defaults */
    switchPtr->WriteUINT32(localSw, FM4000_SERDES_CTRL_1(physPort), 0xAAAA4444);

    switch (portSpeed)
    {
        case 10000:
            /* Just use the raw setting for this register
             * which includes speed, lane ordering, etc
             */
            if (strcasecmp(platformName, "monaco") == 0)
            {
                value = 0x007c0c0a;
            }
            else if (strcasecmp(platformName, "RENO") == 0)
            {
                value = 0x00740a0a;
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                 "Need to add support for platform %s\n", platformName);
                status = FM_FAIL;
            }
            break;
            
        case 1000:
            if (strcasecmp(platformName, "monaco") == 0)
            {
                value = 0x207a0c1a;
            }
            else if (strcasecmp(platformName, "RENO") == 0)
            {
                value = 0x20720a1a;
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                 "Need to add support for platform %s\n", platformName);
                status = FM_FAIL;
            }
            break;
            
        default:
            FM_LOG_ERROR(FM_LOG_CAT_SWITCH_FM4000,
                         "Need to add support for port %d running at %d Mbps\n", 
                         fibmPort, 
                         portSpeed);
            status = FM_FAIL;
            break;
            
    }   /* end switch (portSpeed) */
    
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    status = switchPtr->WriteUINT32(localSw, FM4000_PCS_CFG_1(physPort), value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* MAC enable */
    status = switchPtr->ReadUINT32(localSw, FM4000_MAC_CFG_2(physPort), &value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);
    
    /* bits 0 & 1 control RX & TX MAC */
    value &= ~3;     /* Enable both TX and RX */
    status = switchPtr->WriteUINT32(localSw, FM4000_MAC_CFG_2(physPort), value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Unconditionally set SerDes power-on state */
    status = SetSerdesPowerState(localSw, physPort, SPS_PWRON);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    /* Unconditionally configure PHY interface as UP or not. */
    /* Not needed for FM_PHY_TYPE_BASE_CX4 */
    if (doEalPhyInit)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000, 
                     "Bring up the PHY for fibmPort %d\n",
                     fibmPort);
        ealPhyInit(localSw, fibmPort, physPort, portSpeed);
    }

    fm4000UpdateDrainTx(localSw, physPort, FALSE);

    /***************************************************
     * Step 16: Enable interupt and post an interrupt
     * So interrupt message is sent to the master, which
     * can trigger an switch insertion event
     **************************************************/
     
    if (postIntr)
    {
        FM_LOG_INFO(FM_LOG_CAT_SWITCH_FM4000,
                    "Post interrupt messages to indicate bootstrapping done\n");

        /* Enable interrupt */
        status = switchPtr->WriteUINT32(localSw, FM4000_SW_IM, 0x0);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

        /* Post an interrupt */
        status = switchPtr->WriteUINT32(localSw, FM4000_SW_IP, 0x12345678);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH_FM4000, status);

    }   /* end if (postIntr) */

ABORT:
    UNLOCK_SWITCH(localSw);                                                      \
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH_FM4000, err);
    
} /* fm4000FibmSwInitialize */


/* End of FIBM bootstrap code on slave */
/*===========================================================================*/



/*****************************************************************************/
/** fm4000FibmDumpHwPort
 *
 * \desc            This function dump out raw port stats for fibm debugging
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       logPort is the logical port number
 *
 * \return          NONE
 *
 *****************************************************************************/
fm_status fm4000FibmDumpHwPort(fm_int sw, fm_int logPort)
{
    fm_uint64       result;
    fm_status       status;
    fm_int          port;
    fm_switch      *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    status = GetHwPort(sw, logPort, &port);
    if (status != FM_OK)
    {
        printf("Invalid port %d\n",logPort);
        UNPROTECT_SWITCH(sw);
        return status;
    }

    printf("Switch %u: Port: %d Hw Port: %d\n", sw, logPort, port);

    printf("\n");

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxUcstPktsNonIP(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxUcstPktsNonIP", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxBcstPktsNonIP(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxBcstPktsNonIP", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxMcstPktsNonIP(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxMcstPktsNonIP", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxUcstPktsIPv4(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxUcstPktsIPv4", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxBcstPktsIPv4(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxBcstPktsIPv4", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxMcstPktsIPv4(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxMcstPktsIPv4", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxUcstPktsIPv6(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxUcstPktsIPv6", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxBcstPktsIPv6(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxBcstPktsIPv6", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxMcstPktsIPv6(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxMcstPktsIPv6", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxPausePkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxPausePkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxCBPausePkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxCBPausePkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxSymbolErrors(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxSymbolErrors", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxFCSErrors(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxFCSErrors", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxFrameSizeErrors(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxFrameSizeErrors", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxMinto63(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxMinto63", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx64Pkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx64Pkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx65to127(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx65to127", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx128to255(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx128to255", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx256to511(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx256to511", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx512to1023(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx512to1023", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx1024to1522(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx1024to1522", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx1523to2047(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx1523to2047", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx2048to4095(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx2048to4095", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx4096to8191(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx4096to8191", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx8192to10239(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx8192to10239", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Rx10240toMax(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Rx10240toMax", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxFragments(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxFragments", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxUndersized(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxUndersized", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxOversized(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxOversized", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxOctetsNonIP(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxOctetsNonIP", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxOctetsIPv4(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxOctetsIPv4", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxOctetsIPv6(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxOctetsIPv6", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxOctetsError(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxOctetsError", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxUcstPkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxUcstPkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxBcstPkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxBcstPkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxMcstPkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxMcstPkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxTimeOutDrop(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxTimeOutDrop", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxErrorDrop(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxErrorDrop", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxLoopbackDrop(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxLoopbackDrop", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxMinto63(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxMinto63", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx64Pkts(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx64Pkts", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx65to127(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx65to127", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx128to255(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx128to255", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx256to511(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx256to511", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx512to1023(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx512to1023", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx1024to1522(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx1024to1522", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx1523to2047(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx1523to2047", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx2048to4095(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx2048to4095", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx4096to8191(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx4096to8191", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx8192to10239(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx8192to10239", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Tx10240toMax(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Tx10240toMax", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxErrors(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxErrors", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_FIDForwarded(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("FIDForwarded", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_FloodForwarded(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("FloodForwarded", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_SpeciallyHandled(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("SpeciallyHandled", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_ParseErrDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("ParseErrDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_ParityError(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("ParityError", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Trapped(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Trapped", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_PauseDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("PauseDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_STPDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("STPDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_SecurityViolations(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("SecurityViolations", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_VlanTagDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("VlanTagDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_VlanIngressDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("VlanIngressDrops", result);
                                                     
    status = switchPtr->ReadUINT64(sw, FM4000_STAT_VlanEgressDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("VlanEgressDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_GlortMissDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("GlortMissDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_FFUDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("FFUDrops", result);
                                                        
    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TriggerDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TriggerDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_PolicerDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("PolicerDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TTLDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TTLDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_CMPrivDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("CMPrivDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_SMP0Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("SMP0Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_SMP1Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("SMP1Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxHog0Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxHog0Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RxHog1Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RxHog1Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxHog0Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxHog0Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxHog1Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxHog1Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RateLimit0Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RateLimit0Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_RateLimit1Drops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("RateLimit1Drops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_BadSMPDrops(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("BadSMPDrops", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TriggerRedirects(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TriggerRedirects", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_Others(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("Others", result);

    status = switchPtr->ReadUINT64(sw, FM4000_STAT_TxErrorOctets(port, 0), &result);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, status);
    DUMP_VAL64("TxErrorOctets", result);


ABORT:

    UNPROTECT_SWITCH(sw);

    return status;

} /* end fm4000FibmDumpHwPort */


