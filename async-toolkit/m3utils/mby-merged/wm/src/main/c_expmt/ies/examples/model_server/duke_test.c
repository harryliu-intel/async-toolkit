/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * Copyright (c) 2018, Intel Corporation
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "duke_iosf.h"

void testWriteRead(int addr, uint64_t val) {
    printf("****** TEST addr=0x%04x val=0x%08llx *******\n", addr, val);
    dukeHlpWrite(addr, val);
    assert(dukeHlpRead(addr) == val);
}

int main()
{
    uint64_t wbuf[128];
    uint64_t rbuf[128];
    int count;
    int i;

    dukeSetupHandle();
    dukeSetDebug(4);
    dukeCleanRxQueue();

    testWriteRead(0x2010628, 0x2);
    //testWriteRead(0x06000A0, 0x5);
    dukeHlpWrite(0x06000A0, 0x5);
    assert(dukeHlpRead(0x2010628) == 0x2);
    // testWriteRead(0x2010628, 0x4);


#if 0
    count = 4;
    for (i = 0; i < count; ++i)
        wbuf[i] = i * 2;

    // MOD_VID1_MAP[0..4095]
    dukeHlpBlockWrite(0x4108000, wbuf, count);
    dukeHlpBlockRead(0x4108000, rbuf, count);

    for (i = 0; i < count; ++i) {
        assert(wbuf[i] == rbuf[i]);
    }
#endif

    exit(0);
}

