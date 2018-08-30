/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            basic.c
 * Creation Date:   2006
 * Description:     A simple test to build and link the API
 *              
 *
 * Copyright (c) 2006 - 2011, Intel Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Intel Corporation nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <fm_sdk.h>
#include <platforms/whiteModelLib/platform_types.h>


/*****************************************************************************/
/* main
 * \ingroup 
 *
 * \desc            Entry point for test program.
 *
 * \param[in]       argc is the number of command-line arguments
 *
 * \param[in]       argv points to an array of command-line argument strings
 *
 * \return          0 for success
 *
 *****************************************************************************/
int main(int argc, char *argv[])
{
    fm_status       err;
    fm_int          sw = 0;
    fm_uint32       val32;
    fm_libCfg       libCfg;

    FM_NOT_USED(argc);
    FM_NOT_USED(argv);

    memset(&libCfg, 0, sizeof(libCfg));
    libCfg.debug = 0;

    err = fmModelLibInit(sw, &libCfg);
    printf("fmModelLibInit: err= %d\n", err);

    err = fmModelReset(sw);
    printf("Reset: err=%d\n", err);

    err= fmModelReadCSR(sw, 0x304, &val32);
    printf("Read: REG[0x304]=%x. err=%d\n", val32, err);

    err = fmModelWriteCSR(sw, 0x0, 0x12345678);
    printf("Write: REG[0]. err=%d\n", err);

    err = fmModelReadCSR(sw, 0x0, &val32);
    printf("Read: REG[0]=%x. err=%d\n", val32, err);

    exit(0);

}   /* end main */
