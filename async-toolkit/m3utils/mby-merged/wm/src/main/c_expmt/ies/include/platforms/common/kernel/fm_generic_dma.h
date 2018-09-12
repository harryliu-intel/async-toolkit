/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_dma.h
 * Creation Date:   2005
 * Description:     Header file for generic DMA controller
 *
 *
 * This file is provided under a dual BSD/GPLv2 license.  When using or
 * redistributing this file, you may do so under either license.
 *
 * GPL LICENSE SUMMARY
 *
 * Copyright(c) 2005, 2012 Intel Corporation. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 * The full GNU General Public License is included in this distribution
 * in the file called LICENSE.GPL.
 *
 * BSD LICENSE
 *
 * Copyright(c) 2005, 2012 Intel Corporation. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *   * Neither the name of Intel Corporation nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ****************************************************************************/


#ifndef __FM_FM_GENERIC_DMA_H
#define __FM_FM_GENERIC_DMA_H

/****************************************************************************/
/** \ingroup typeStruct
 *
 * The buffer descriptors are used to exchange frames with the driver.
 *
 * For transmission, the API is responsible to construct the frame
 * exactly as it is intended to be sent using as many buffer descriptors
 * as there are buffers associated with this frame. The driver
 * makes no assumption on the type of switch or
 * the content of the frame.
 *
 * For reception, the API must tell the driver how many
 * bytes (multiple of 4) of the frame shall be placed
 * in the header by a parameter driver and must provide buffers
 * to the driver. The driver will then load the buffers directly placing
 * the frame header asside in the header section.
 **************************************************************************/

#define FM_BD_HEADER_SIZE             8
#define FM_DMA_RX_BUFFER_DESCRIPTORS  64
#define FM_DMA_TX_BUFFER_DESCRIPTORS  64

typedef struct _fm_deviceBufferDescriptor
{
    /** The switch number associated with this buffer. On TX, this is
     *   set by the API and reflects on which switch this buffer shall
     *   be sent. This needs to be set only on the first buffer of a
     *   packet. On RX, this is set by the driver to indicate on which
     *   switch this buffer was received, it is only valid on the first
     *   buffer of a packet. */

    fm_uint16  sw;

    /** The length IN WORDS of this buffer. On TX, this is set by the API and
     *   reflects the number of words valid in this buffer. On RX, this is set
     *   by the API to indicate the size of this buffer and update by the driver
     *   to reflect the number of bytes that were effectively written in this
     *   buffer. */

    fm_uint16  length;

    /** The pointer to the data. Always loaded by the API. */

    fm_uint32 *data;

    /** The length IN WORDS of the header to be transmitted before this buffer is
     *   transmitted. A length of 0 indicate that there are no header. Used
     *   by TX only and must be present only for the first buffer
     *   of the packet. */

    fm_uint16  headerLength;

    /** The header section if needed */

    fm_uint32  header[FM_BD_HEADER_SIZE];

    /** The fm_buffer associated with this BD (not used by driver). */

    void *     buffer;

    /** Indicates that this BD is the last BD of a packet. */

    fm_bool    endOfPacket;

    /** Indicates if this buffer has to be freed once this buffer has
     *   been transmitted (used for TX only) */

    fm_bool    free;

} fm_bufferDescriptor;


/**************************************************
 * DMA transfer data struture.
 *
 * This structure is a common structure visible by
 * both the API and the driver.
 *
 **************************************************/

typedef struct _fm_dmaController
{
    /* Buffer Descriptors */

    volatile fm_bufferDescriptor rxBufferDescriptors[FM_DMA_RX_BUFFER_DESCRIPTORS];
    volatile fm_bufferDescriptor txBufferDescriptors[FM_DMA_TX_BUFFER_DESCRIPTORS];

    /* Buffer Descriptor Indexes for Receive.
     *
     * API owns [rxHead,rxTail-1] where rxHead point to the next BD to retrieve
     * by API. Drivers own [rxTail,rxFree-1] where rxTail will be the next
     * frame to be queued by the driver to the API. The space [rxFree,rxHead-1]
     * doesn't have buffers allocated and is not usable.
     */

    fm_int32                     rxHead;
    fm_int32                     rxTail;
    fm_int32                     rxFree;

    /* Buffer Descriptor Indexes for Transmit.
     *
     * Driver owns [txHead,txTail-1] where txHead point to the next BD to transmit
     * by the driver. The API enqueue a new frame at txTail.
     * API own [txFree,txHead-1] where txFree is the next frame
     * to be freed.
     */

    fm_int32                     txHead;
    fm_int32                     txTail;
    fm_int32                     txFree;

    /* Status */

    fm_bool                      txIdle; /* Set by driver to indicate it is
                                          *  idling and it needs to be waked up
                                          *  by the API to start transmission */
    fm_bool                      rxWait; /* Set by API to indicate that it is
                                          *  idling and need a wake up call from
                                          *  the driver to process incoming
                                          *  packets */
    fm_bool                      rxIdle; /* Set by driver to indicate it has
                                          *  not been started yet or that it is
                                          *  blocked waiting for RX BDs to be
                                          *  freed by the API. The API shall
                                          *  send a signal to the driver once
                                          *  it has prepared BDs. */

    /* The relative distance in bytes for the internal dma controller
     *  structure. Useful for debugging only. */

    fm_uint32                    driverControllerOffset;

} fm_dmaController;


#endif /* __FM_FM_GENERIC_DMA_H */
