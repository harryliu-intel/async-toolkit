/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_generic_netlink_kernel.h
 * Creation Date:   Jan 13, 2009
 * Description:     Header file for generic netlink packet I/O
 *
 * This file is provided under a dual BSD/GPLv2 license.  When using or
 * redistributing this file, you may do so under either license.
 *
 * GPL LICENSE SUMMARY
 *
 * Copyright(c) 2009-2011 Intel Corporation. All rights reserved.
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
 * Copyright(c) 2009-2011 Intel Corporation. All rights reserved.
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
 *****************************************************************************/

#ifndef __FM_FM4000_GENERIC_NETLINK_KERNEL_H
#define __FM_FM4000_GENERIC_NETLINK_KERNEL_H


/**************************************************
 * Netlink packet message header.
 *
 * This structure is a common structure visible by
 * both the API and the driver.
 *
 **************************************************/

struct fm_nlHdr
{
    u_short     sw;             /* switch number */

    u_short     reserved;

    u_int       fm64[2];        /* F64 tag */

};

/* Netlink socket, we use the same number as NETLINK_USERSOCK */
#define FM_NL_SOCK                     2

/* Netlink listening group types when reading from the netlink inteface */
#define FM_NL_GROUP_L2                 1
#define FM_NL_GROUP_IP                 2
#define FM_NL_GROUP_ARP                4
#define FM_NL_GROUP_RAW_CB             8

#define FM_NL_GROUP_ALL                FM_NL_GROUP_L2 | FM_NL_GROUP_RAW_CB | \
                                       FM_NL_GROUP_ARP | FM_NL_GROUP_IP

/**************************************************
 * Functions to send packets from the Focalpoint driver to the 
 * ControlPoint driver.
 *
 **************************************************/

#ifdef __KERNEL__
typedef int (*fm_pktHandler)(struct sk_buff * pkt, u_short vlan);
extern void fm_reg_pkt_handler(fm_pktHandler handler);
extern void fm_unreg_pkt_handler(fm_pktHandler handler);
extern int fm_pkt_send(struct sk_buff *skb, u_short vlan);
#endif /* __KERNEL__ */



#endif /* __FM_FM4000_GENERIC_NETLINK_KERNEL_H */
