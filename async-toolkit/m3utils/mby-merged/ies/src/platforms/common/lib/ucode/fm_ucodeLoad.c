/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_ucodeLoad.c 
 * Creation Date:   September 19, 2011 
 * Description:     Functions for loading microcode images for FM6000+ 
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved.
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

#include <fm_sdk_int.h>
#include <api/internal/fm6000/fm6000_api_regs_int.h>

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
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmPlatformValidateMicrocodeRawImage
 * \ingroup intPlatform
 *
 * \desc            Validate the microcode image that has been previously
 *                  loaded in the switch. The microcode read from the switch
 *                  is compared to the given a raw file input.  The raw
 *                  file format is a text based file.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       image is the path to the microcode raw file. 
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status fmPlatformValidateMicrocodeRawImage(fm_int sw, fm_text image)
{
    fm_switch *  switchPtr;
    FILE *       fp;
    fm_char      buf[512];
    fm_status    status = FM_OK;
    fm_text      ptr;
    fm_text      invalid = NULL;
    fm_uint32    addr;
    fm_uint32    value;
    fm_uint32    readValue;
    fm_uint32    mask;
    fm_int       numRegs = 0;
    fm_int       numDiffRegs = 0;
    fm_uint      s1max;
    fm_char *    context;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d image=%s\n", sw, image);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_PRINT("+------------------------------------------------------+\n");
    FM_LOG_PRINT("| Microcode validation                                 |\n");
    FM_LOG_PRINT("|                                                      |\n");
    FM_LOG_PRINT("| The loaded microcode is read back and compared to    |\n");
    FM_LOG_PRINT("| the original microcode raw file:                     |\n");
    FM_LOG_PRINT("| %52s |\n",image);
    FM_LOG_PRINT("+------------------------------------------------------+\n");

    if ((fp = fopen(image, "rt")) == NULL)
    {
        status = FM_ERR_MICROCODE_IMAGE_FILE_MISSING;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
    }

    while(fgets(buf, 512, fp))
    {
        /* strip off newline */
        buf[strlen(buf) - 1] = 0;

        s1max   = sizeof(buf);
        context = NULL;
        ptr     = FM_STRTOK_S(buf, &s1max, " ", &context);

        if (!ptr)
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        invalid = NULL;
        addr = (fm_uint32) strtoul(ptr, &invalid, 16);

        if (invalid && (*invalid != 0))
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        ptr = FM_STRTOK_S(NULL, &s1max, " ", &context);

        if (!ptr)
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        invalid = NULL;
        value = (fm_uint32) strtoul(ptr, &invalid, 16);

        if (invalid && (*invalid != 0))
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        status = switchPtr->ReadUINT32(switchPtr->switchNumber, 
                                       addr, 
                                       &readValue);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);

        if (readValue != value)
        {
            mask = 0xffffffff;

            if (addr >= FM6000_MOD_CAM(0, 0, 0) &&
                addr <  FM6000_MOD_CAM(FM6000_MOD_CAM_ENTRIES_1, 
                                       FM6000_MOD_CAM_ENTRIES_0, 
                                       FM6000_MOD_CAM_WIDTH))
            {
                /* For the MOD_CAM bits 63:48 and 127:112 are not used,
                   so ignore them for comparison.
                 
                                 Width     Bit
                   +-----------+-------+--------+
                   | KeyInvert |  48   |   47:0 |
                   +-----------+-------+--------+
                   | Key       |  48   | 111:64 |
                   +-----------+-------+--------+
                 */

                if (addr & 0x1)
                {
                    /* Only the 16 lsb are valid for that 32-bit word */
                    mask = 0xffff;
                }
            }
            else if (addr >= FM6000_L2AR_CAM_DMASK(0, 0, 0, 0) &&
                     addr <  FM6000_L2AR_CAM_DMASK(
                                                FM6000_L2AR_CAM_DMASK_ENTRIES_2, 
                                                FM6000_L2AR_CAM_DMASK_ENTRIES_1, 
                                                FM6000_L2AR_CAM_DMASK_ENTRIES_0,
                                                FM6000_L2AR_CAM_DMASK_WIDTH))
            {
                /* For the L2AR_CAM_DMASK bits 63:38 and 127:102 are not used,
                   so ignore them for comparison.

                                 Width     Bit
                   +-----------+-------+--------+
                   | KeyInvert |  38   |   37:0 |
                   +-----------+-------+--------+
                   | Key       |  38   | 101:64 |
                   +-----------+-------+--------+
                 */

                if (addr & 0x1)
                {
                    /* Only the 6 lsb are valid for that 32-bit word */
                    mask = 0x3f;
                }
            }

            if ((readValue & mask) != (value & mask))
            {
                numDiffRegs++;
                FM_LOG_PRINT("Different: original_value 0x%08x "
                             "read_value 0x%08x addr 0x%08x\n",
                             value & mask,
                             readValue & mask,
                             addr);
            }
        }

        numRegs++;
    }

    FM_LOG_PRINT("%d out of %d registers are different\n",
                 numDiffRegs,
                 numRegs);

ABORT:
    if (fp != NULL)
    {
        fclose(fp);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end fmPlatformValidateMicrocodeRawImage */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmPlatformLoadMicrocodeRawImageFile
 * \ingroup intPlatform
 *
 * \desc            Loads a microcode image given a raw file input.  The raw
 *                  file format is a text based file.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       image is the path to the file to be parsed and loaded. 
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformLoadMicrocodeRawImageFile(fm_int sw, fm_text image)
{
    fm_switch *  switchPtr;
    FILE *       fp;
    fm_char      buf[512];
    fm_status    status = FM_OK;
    fm_text      ptr;
    fm_text      invalid = NULL;
    fm_uint32    addr;
    fm_uint32    value;
    fm_timestamp start;
    fm_timestamp end;
    fm_timestamp delta;
    fm_int       numRegs = 0;
    fm_uint      s1max;
    fm_char *    context;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d image=%s\n", sw, image);

    switchPtr = GET_SWITCH_PTR(sw);

    if ((fp = fopen(image, "rt")) == NULL)
    {
        status = FM_ERR_MICROCODE_IMAGE_FILE_MISSING;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
    }

    fmGetTime(&start);

    while(fgets(buf, 512, fp))
    {
        /* strip off newline */
        buf[strlen(buf) - 1] = 0;

        s1max   = sizeof(buf);
        context = NULL;
        ptr     = FM_STRTOK_S(buf, &s1max, " ", &context);

        if (!ptr)
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        invalid = NULL;
        addr = (fm_uint32) strtoul(ptr, &invalid, 16);

        if (invalid && (*invalid != 0))
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        ptr = FM_STRTOK_S(NULL, &s1max, " ", &context);

        if (!ptr)
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        invalid = NULL;
        value = (fm_uint32) strtoul(ptr, &invalid, 16);

        if (invalid && (*invalid != 0))
        {
            status = FM_ERR_MICROCODE_IMAGE_INVALID;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);
        }

        status = switchPtr->WriteUINT32(switchPtr->switchNumber, addr, value);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);

        numRegs++;
    }

    fmGetTime(&end);

    fmSubTimestamps(&end, &start, &delta);

    FM_LOG_PRINT("Loaded %d registers in %3.3f seconds\n",
                 numRegs,
                 delta.sec + (1.0e-6 * delta.usec));

ABORT:
    if (fp != NULL)
    {
        fclose(fp);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end fmPlatformLoadMicrocodeRawImageFile */




/*****************************************************************************/
/** fmPlatformValidateMicrocode
 * \ingroup platform
 * 
 * \chips           FM6000
 *
 * \desc            Called by the API to request the platform layer to
 *                  validate the microcode previously loaded into the device.
 *
 * \param[in]       sw is the switch on which to operate. The switch
 *                  number must have already been validated.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformValidateMicrocode(fm_int sw)
{
    fm_status err = FM_OK;
    fm_text   uImages[3] = { NULL, "./microcode.raw", "../microcode.raw" };
    fm_text   microcodeImage = NULL;
    FILE *    fd;
    fm_int    i;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    uImages[0] = fmGetTextApiAttribute(FM_AAK_API_FM6000_MICROCODE_IMAGE,
                                       FM_AAD_API_FM6000_MICROCODE_IMAGE);

    for ( i = 0 ; i < 3 ; i++ )
    {
        if (strlen(uImages[i]) > 0)
        {
            if ( (fd = fopen(uImages[i], "r")) == NULL )
            {
                continue;
            }
            else
            {
                fclose(fd);
                microcodeImage = uImages[i];
                break;
            }
        }
    }

    if (!microcodeImage)
    {
        UNPROTECT_SWITCH(sw);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_MICROCODE_IMAGE_FILE_MISSING);
    }

    err = fmPlatformValidateMicrocodeRawImage(sw, microcodeImage);
    
    if (err != FM_OK) 
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Error validating microcode image for sw %d from %s!\n",
                     sw, 
                     microcodeImage);
    }

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

} /* end fmPlatformValidateMicrocode */

