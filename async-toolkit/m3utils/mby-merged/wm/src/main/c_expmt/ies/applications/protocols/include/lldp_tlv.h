/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            struct lldp_tlv.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for LLDP TLV.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef LLDP_TLV_H
#define LLDP_TLV_H

#include <stdlib.h>
#include <stdint.h>

#include <lldp_mib.h>

/** LLDP TLV types. Used as a constant in the ''struct lldpTlv'' structure. */
enum lldpTlvType {
    /** End of LLDPDU TLV */
    LLDP_TLV_TYPE_END_OF_LLDPDU = 0,

    /** Chassis-Id TLV */
    LLDP_TLV_TYPE_CHASSIS_ID,

    /** Port-Id TLV */
    LLDP_TLV_TYPE_PORT_ID,

    /** TTL TLV */
    LLDP_TLV_TYPE_TIME_TO_LIVE,

    /** Port-Description TLV */
    LLDP_TLV_TYPE_PORT_DESCRIPTION,

    /** System-Name TLV */
    LLDP_TLV_TYPE_SYSTEM_NAME,

    /** System-Description TLV */
    LLDP_TLV_TYPE_SYSTEM_DESCRIPTION,

    /** System-Capability TLV */
    LLDP_TLV_TYPE_SYSTEM_CAPABILITIES,

    /** Management-Address TLV */
    LLDP_TLV_TYPE_MANAGEMENT_ADDRESS,

    /** Organizationally-Specific TLV */
    LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC = 127,

    /** For internal use only. */
    LLDP_TLV_TYPE_MAX
};

/** Structure used for holding TLV (type-length-value) entry, used by LLDP protocol implementaion. */
struct lldpTlv {
    /** Entry type. */
    unsigned int type;

    /** Number of bytes pointed to by data (the number of bytes in this tlv). */
    size_t len;

    /** Pointer to a tlv data. The tlv data is in network byte order and must
        begin at the first byte of the location pointed to by this structure
        member. */
    uint8_t *data;

    /** Pointer to the next tlv in a chain, or NULL for the last. */
    struct lldpTlv *next;
};

/** Returns a pointer to a newly allocated tlv */
struct lldpTlv *lldpAllocateTlv(size_t len);

/** Releases memory for an allocated TLV */
void lldpFreeTlv(struct lldpTlv *tlv);

/** Releases memory for a chain of TLVs */
void lldpFreeTlvChain(struct lldpTlv *tlvChain);

/** Append TLV to TLV chain */
int lldpAppendTlv(struct lldpTlv *tlvChain, struct lldpTlv *tlv);

/** insert TLV to TLV chain */
int lldpInsertBeforeTlv(struct lldpTlv *tlvChain, struct lldpTlv *tlv, enum lldpTlvType type);

/** Get buffer size needed for tlv chain */
int lldpGetTlvChainSize(const struct lldpTlv *tlvChain);

/** Decode TLVs from buffer, create TLVs chain */
int lldpDecode(uint8_t *buf, size_t size, size_t offset, struct lldpTlv **tlvChain);

/** Get id from TLV */
int lldpDecodeGenericIdTlv(struct lldpTlv *tlv, int *idSubtype, char *id, size_t *idLen, size_t size);

/** Get string from TLV */
int lldpDecodeGenericStringTlv(struct lldpTlv *tlv, char *text, size_t size);

/** Get chassis-id from TLV */
int lldpDecodeChassisTlv(struct lldpTlv *tlv, enum lldpChassisIdSubtype *chassisIdSubtype,
                         char chassisId[256], size_t *chassisIdLen);

/** Get port-id from TLV */
int lldpDecodePortTlv(struct lldpTlv *tlv, enum lldpPortIdSubtype *portIdSubtype,
                      char portId[256], size_t *portIdLen);

/** Get ttl from TLV */
int lldpDecodeTtlTlv(struct lldpTlv *tlv, int *ttl);

/** Get string from TLV */
int lldpDecodeSystemCapTlv(struct lldpTlv *tlv, uint16_t *sysCapSupported,
                           uint16_t *sysCapEnabled);

/** Get manegement address from TLV */
int lldpDecodeMgmtAddressTlv(struct lldpTlv *tlv, int *manAddrSubtype,
                             char manAddr[32], int *manAddrIntfIdSubtype,
                             uint32_t *manAddrIntfId, char manAddrOid[256]);

/** Encode TLVs chain to buffer */
int lldpEncode(uint8_t *buf, size_t size, size_t offset, const struct lldpTlv *tlvChain, int *len);

/** Get id from TLV */
int lldpEncodeGenericIdTlv(struct lldpTlv **tlv, int type,
                           int idSubtype, char *id, int id_strlen);

/** Get string from TLV */
int lldpEncodeGenericStringTlv(struct lldpTlv **tlv, int type, 
                               char *text, int text_strlen);

/** Encode chassis-id to TLV */
int lldpEncodeChassisTlv(struct lldpTlv **tlv, enum lldpChassisIdSubtype chassisIdSubtype, char *chassisId, size_t chassisIdLen);

/** Encode port-id to TLV */
int lldpEncodePortTlv(struct lldpTlv **tlv, enum lldpPortIdSubtype portIdSubtype, char *portId, size_t portIdLen);

/** Encode ttl to TLV */
int lldpEncodeTtlTlv(struct lldpTlv **tlv, int ttl);

/** Encode end-of-lldpdu TLV */
int lldpEncodeEndTlv(struct lldpTlv **tlv);

/** Encode string to TLV */
int lldpEncodeSystemCapTlv(struct lldpTlv **tlv, uint16_t sysCapSupported,
                           uint16_t sysCapEnabled);

/** Encode manegement address to TLV */
int lldpEncodeMgmtAddressTlv(struct lldpTlv **tlv, int manAddrSubtype,
                             char *manAddr, int manAddrIntfIdSubtype,
                             uint32_t manAddrIntfId, char *manAddrOid);

#endif /* LLDP_TLV_H */
