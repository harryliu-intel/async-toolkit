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

#include <stdint.h>

#define DUKE_MSG_CTRL       0x80010430
#define DUKE_MASTER_HDR     0x80010218
#define DUKE_MASTER_DATA    0x80010228
#define DUKE_MASTER_COUNT   0x8001042C
#define DUKE_TARGET_HDR     0x80010000
#define DUKE_TARGET_DATA    0x80010010
#define DUKE_TARGET_COUNT   0x80010214
#define DUKE_TARGET_STATUS  0x80010210

/* Duke interface management */
void dukeWaitForCompletion();
void dukeCleanRxQueue();

/* Duke registers access */
void dukeSetupHandle();
void dukeRead(char *label, uint64_t addr, unsigned int *buf, unsigned int count);
void dukeWrite(char *label, uint64_t addr, unsigned int *buf, unsigned int count);

/* HLP registers access */
long long int dukeHlpRead(int addr);
void dukeHlpWrite(int addr, unsigned long long int val);

void dukeSetDebug(int level);
