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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>

#include "duke_proxy.h"
#include "duke_iosf.h"

#ifdef PLATFORM_DUKE
#ifndef FPGA_OFFSET
#define FPGA_OFFSET 3
#endif
#endif

static void *duke_handle = NULL;
static int debug;

/* dukeSetupHandle() - Setup handle used to access regs over FTDI serial port
 *
 * The function must be called at init time, before performing any register
 * operation on Duke.
 */
void dukeSetupHandle()
{
    struct duke_ft_device* user_ft_devices;
    unsigned int count;
    unsigned int i;

    if (duke_handle) {
        printf("Duke handle has already been created\n");
        return;
    }

    if (DukeGetDevicesCount(&count)) {
        printf("error no devices were found\n");
    }
    if (count < 1) {
        printf("0 devices were found\n");
    }

    user_ft_devices = malloc(sizeof(struct duke_ft_device) * count);
    if (NULL == user_ft_devices) {
        printf("memory allocation failed\n");
    }

    if (DukeGetDevices(user_ft_devices, count)) {
        printf("error unable to get devices\n");
    }

    for (i = 0; i < count; i++) {
        if (strstr(user_ft_devices[i].Description, "Duke System") != NULL &&
                strstr(user_ft_devices[i].Description, " B") != NULL) {

            if (DukeGetHandleBy(&user_ft_devices[i], DukeOpenBySerial,
                        &duke_handle)) {
                printf("%s: error while openeing device. Set silent = 0 in duke_proxy.c\n",
                        __func__);
                exit(0);
            }
            goto CLEANUP;
        }
    }
    printf("no device were found\n");
CLEANUP:
    free(user_ft_devices);

}

/* dukeRead() - Read Duke registers
 *
 * Read count registers from Duke starting from addr and save the result in buf.
 * If debug is enabled, it will print debug information using the input label.
 */
void dukeRead(char *label, uint64_t addr, unsigned int *buf, unsigned int count)
{
    unsigned int i;
    int err;

    if (!label || !buf) {
        printf("Invalid input pointers");
        return;
    }

    if (!count)
        return;

    memset(buf, 0, 4 * count);

    err = DukeReadRegister(duke_handle, FPGA_OFFSET, addr, buf, 1);
    if (err) {
        printf("Failed to read Fpga register, err = %d\n", err);
        return;
    }

    if (debug >= 4)
        printf("Read  duke[0x%lx] = 0x%08x - %s\n", addr, *buf, label);

    for (i = 1; i < count; ++i) {
        err = DukeReadRegister(duke_handle, FPGA_OFFSET, addr + 4*i, buf + i, 1);
        if (err) {
            printf("Failed to read Fpga register, err = %d\n", err);
            return;
        }
        if (debug >= 4)
            printf("Read  duke[0x%lx] = 0x%08x\n", addr + 4*i, buf[i]);
    }
}

/* dukeWrite() - Write Duke registers
 *
 * Write the content of buf into count Duke registers starting from addr.
 * If debug is enabled, it will print debug information using the input label.
 */
void dukeWrite(char *label, uint64_t addr, unsigned int *buf, unsigned int count)
{
    unsigned int bufCheck;
    unsigned int i;
    int err;

    if (!label || !buf) {
        printf("Invalid input pointers");
        return;
    }

    if (!count)
        return;

#if 0 // Single write of multiple registers (faster but not working)
    err = DukeWriteRegister(duke_handle, FPGA_OFFSET, addr, buf, count);
    if (err) {
        printf("Failed to write Fpga register, err = %d\n", err);
        return;
    }
#else // For loop with individual reg writes
    for (i = 0; i < count; ++i) {
        err = DukeWriteRegister(duke_handle, FPGA_OFFSET, addr + 4*i, buf + i, 1);
        if (err) {
            printf("Failed to write Fpga register, err = %d\n", err);
            return;
        }
    }
#endif


    if (debug >= 4) {
        printf("Write duke[0x%lx] = 0x%08x - %s\n", addr, *buf, label);
        for (i = 1; i < count; ++i) {
            printf("Write duke[0x%lx] = 0x%08x\n", addr + 4*i, buf[i]);
        }
    }

#if 1
    if (count == 1)
        return;

    /* Optional: check that all the registers have been actually been written */
    for (i = 0; i < count; ++ i) {
        bufCheck = 0;
        err = DukeReadRegister(duke_handle, FPGA_OFFSET, addr + 4*i, &bufCheck, 1);
        if (err) {
            printf("Failed to read Fpga register, err = %d\n", err);
            return;
        }
        if (bufCheck != buf[i]) {
            printf("Register check failed. Addr 0x%lx - Write 0x%08x - Read 0x%08x\n",
                    addr + 4*i, buf[i], bufCheck);
        }
    }
#endif
}

/* dukeWaitForCompletion() - Wait for operation completion
 *
 * This function waits for the completion of Duke SB-IOSF request.
 * Returns when the response is ready to be read.
 */
void dukeWaitForCompletion()
{
    unsigned char valid;
    int retries = 10;
    uint32_t buf;

    do {
        dukeRead("target status", DUKE_TARGET_STATUS, &buf, 1);
        valid = buf >> 31;
        --retries;
    } while(!valid && retries);

    if (!valid) {
        printf("Did not receive completion from Duke\n");
        // Reset tx and rx byte counters
        dukeRead("master (tx) count", DUKE_MASTER_COUNT, &buf, 1);
        dukeRead("target (rx) count", DUKE_TARGET_COUNT, &buf, 1);
        exit(1);
    }
}

/* dukeCleanRxQueue() - Reset SB-IOSF rx queue in Duke
 *
 * Manually pull all the messages out of the rx queue until it's empty.
 * This function is optional, but can be used at init to make sure we have a
 * clear starting point.
 */
void dukeCleanRxQueue()
{
    uint32_t buf[512];
    int32_t bufLen;
    int32_t retries = 100;

    if (debug >= 4)
        printf("==================== Reset queues ===================\n");

    // Set flush mode to softare
    buf[0] = 0;
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

    do {
        // Flush messages until the rx queue is empty
        buf[0] = 1 << 26;
        dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

        dukeRead("target status", DUKE_TARGET_STATUS, buf, 1);
        if (*buf & (1 << 13)) // rx_fifo is empty
            break;

    } while (--retries);

    if (retries) {
        // Reset tx and rx byte counters
        buf[0] = (1 << 13) | (1 << 14);
        dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);
        dukeRead("master (tx) count", DUKE_MASTER_COUNT, buf, 1);
        dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);
    }
    else {
        printf("Failed to clean the Duke rx queue\n");
    }
}

/* readFpgaReg() - Read HLP register from Duke
 *
 * Returns the content of the HLP register with address addr.
 * This is an high level function only used internally in the model_server
 * (i.e. interrupt handler).
 * The register access from the API is done using SB-IOSF.
 */
long long int dukeHlpRead(int addr)
{
    unsigned char ndw;
    uint32_t buf[512];
    unsigned char be;
    uint64_t ret;
    int32_t i;

    if (debug >= 4)
        printf("==================== Read HLP register  ===================\n");

    be = addr & 0x7 ? 0xf: 0xff;
    buf[0] = 0xc000aa1f;
    buf[1] = 0x00005500;
    buf[2] = ((addr & 0xffff) << 16 ) | be;
    buf[3] = addr >> 16;
    dukeWrite("master head", DUKE_MASTER_HDR, buf, 4);

    buf[0] = 0x0400501f;
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

    dukeWaitForCompletion();

    // dukeRead("message ctrl", DUKE_MSG_CTRL, buf, 1);
    // dukeRead("master (tx) count", DUKE_MASTER_COUNT, buf, 1);
    dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);

    ndw = (buf[0] / 4) - 2;
    dukeRead("target header", DUKE_TARGET_HDR, buf, 2);
    dukeRead("target data", DUKE_TARGET_DATA, buf, ndw);
    ret = buf[1];
    ret = (ret << 32 ) | buf[0];
    return ret;
}

/* writeFpgaReg() - Write HLP register in Duke
 *
 * Write val in the HLP register with address addr.  This is an high level
 * function only used internally in the model_server (i.e. interrupt handler).
 * The register access from the API is done using SB-IOSF.
 *
 * It always assume the operation is successful and does not return any error
 * code.
 */
void dukeHlpWrite(int addr, unsigned long long int val)
{
    unsigned char be;
    uint32_t buf[512];
    int32_t i;

    /* Workaround to prevent the NVM image to overwrite CLASSIFY_BSM_MASTER_SAI */
    if (addr >= 0x180 && addr <= 0x198 && val != 0x2) {
        printf("Attempted to set CLASSIFY_BSM_MASTER_SAI[%d]=0x%llx. Set to 0x2 instead\n",
                (addr - 0x180) / 0x8, val);
        val = 0x2;
    }

    if (debug >= 4)
        printf("==================== Write HLP register  ===================\n");
    be = addr & 0x7 ? 0xf: 0xff;
    buf[0] = 0xc001aa1f;
    buf[1] = 0x00005500;
    buf[2] = ((addr & 0xffff) << 16 ) | be;
    buf[3] = addr >> 16;
    dukeWrite("master head", DUKE_MASTER_HDR, buf, 4);

    buf[0] = val & 0xffffffff;
    buf[1] = val >> 32;
    dukeWrite("master data", DUKE_MASTER_DATA, buf, 2);

#if 1 // Posted request
    buf[0] = 0x0000005f;
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

#else // Non-posted request (this always fails...)
    buf[0] = 0x0400501f;
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

    dukeWaitForCompletion();

    dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);
    dukeRead("target header", DUKE_TARGET_HDR, buf, 2);

    unsigned char rsp;
    // TAG_OFF = 3
    rsp = (((char *)buf)[3] >> 3) & 0x3;
    if (rsp) {
        printf("ERROR: register write operation failed - rsp=%d - addr=0x%04x - val=0x%08llx\n",
                rsp, addr, val);
    }
    dukeRead("master (tx) count", DUKE_MASTER_COUNT, buf, 1);
    dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);
#endif
}

#if 0
void dukeHlpBlockWrite(uint64_t addr, uint64_t *val, unsigned int count)
{
    unsigned char valid;
    unsigned char ndw;
    uint32_t buf[512];
    int32_t i;

    if (count % 2 || count > 124) {
        printf("Count must be even and less than 125\n");
        return;
    }
    ndw = count * 2;

    printf("================= Block Write HLP register  ================\n");
    buf[0] = 0xc011aa1f;
    buf[1] = 0x00005500;
    buf[2] = ((addr & 0xffff) << 16 ) | ndw;
    buf[3] = addr >> 16;
    dukeWrite("master head", DUKE_MASTER_HDR, buf, 4);

    for (i = 0; i < count; ++i) {
        buf[2*i] = val[i] & 0xffffffff;
        buf[2*i+1] = val[i] >> 32;
    }
    dukeWrite("master data", DUKE_MASTER_DATA, buf, ndw);

    buf[0] = 0x0000001f | (ndw << 5);
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

    dukeRead("master (tx) count", DUKE_MASTER_COUNT, buf, 1);
    dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);
}

void dukeHlpBlockRead(uint64_t addr, uint64_t *val, unsigned int count)
{
    unsigned char valid;
    unsigned char ndw;
    uint32_t buf[512];
    int32_t i;

    if (count % 2 || count > 126) {
        printf("Count must be even and less than 127\n");
        return;
    }
    ndw = count * 2;

    printf("================= Block Read HLP register  ================\n");
    buf[0] = 0xc010aa1f;
    buf[1] = 0x00005500;
    buf[1] = 0x00005500;
    buf[2] = ((addr & 0xffff) << 16 ) | ndw;
    buf[3] = addr >> 16;
    dukeWrite("master head", DUKE_MASTER_HDR, buf, 4);

    buf[0] = 0x0400501f;
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

    do {
        // sleep(1);
        dukeRead("target status", DUKE_TARGET_STATUS, buf, 1);
        valid = buf[0] >> 31;
    } while(!valid);

    dukeRead("message ctrl", DUKE_MSG_CTRL, buf, 1);

    dukeRead("master (tx) count", DUKE_MASTER_COUNT, buf, 1);
    dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);
    ndw = (buf[0] / 4) - 2;
    dukeRead("target header", DUKE_TARGET_HDR, buf, 2);
    dukeRead("target data", DUKE_TARGET_DATA, buf, ndw);

    for (i = 0; i < count; ++i) {
        val[i] = buf[2*i+1];
        val[i] = (val[i] << 32 ) | buf[2*i];
    }
}
#endif

void dukeSetDebug(int level) {
    debug = level;
}
