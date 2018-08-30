/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v0_tlv.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for DCBX (Version 0)
 *                  TLV.
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

#ifndef DCBX_V0_TLV_H
#define DCBX_V0_TLV_H

#include <stdint.h>

#include <dcbx_mib.h>

/** DCBX (Version 0) TLV types. Used as a constant in the ''dcbx_v0_tlv'' structure. */
enum dcbxV0TlvSubType {
    /** Control TLV */
    DCBX_V0_TLV_SUBTYPE_CONTROL = 1,

    /** Priority-Groups TLV */
    DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS = 2,

    /** PFC TLV */
    DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL = 3,

    /** Application-Priority TLV */
    DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY = 4,

    /** For internal use only. */
    DCBX_V0_TLV_SUBTYPE_MAX
};

/** DCBX (Version 0) Feature header. */
struct dcbxV0Feature {
    /** Feature operational version. */
    uint8_t operVersion;

    /** Feature maximum version. */
    uint8_t maxVersion;

    /** Feature sub-type. */
    uint8_t subType;

    /** Feature enable indication. */
    bool    enable;

    /** Feature willing indication. */
    bool    willing;

    /** Feature error indication. */
    bool    error;
};

/** Validates valid DCBX-V0 organizationally TLV */
int dcbxV0ValidateOrganizationallyTlv(const struct lldpTlv *tlv);

/** Breaks DCBX organizationally TLV to DCBX sub-TLVs chain */
int dcbxV0Decode(const struct lldpTlv *tlv, struct lldpTlv **tlvChain);

/** Combines DCBX sub-TLVs chain to single DCBX organizationally TLV */
int dcbxV0Encode(const struct lldpTlv *tlvChain, struct lldpTlv **tlv);

/** Decodes DCBX control data from TLV */
int dcbxV0DecodeControlTlv(const struct lldpTlv *tlv, uint8_t *oper_ver, uint8_t *max_ver, uint32_t *seqno, uint32_t *ackno);

/** Encodes DCBX control data to TLV */
int dcbxV0EncodeControlTlv(struct lldpTlv **tlv, uint8_t oper_ver, uint8_t max_ver, uint32_t seqno, uint32_t ackno);

/** Decodes ETS (PG) data from TLV */
int dcbxV0DecodeEtsTlv(const struct lldpTlv *tlv, struct dcbxV0Feature *feature, uint8_t *num_tcs_supported, uint8_t pri_assignment[8], uint8_t bandwidth[8]);

/** Encodes ETS (PG) data to TLV */
int dcbxV0EncodeEtsTlv(struct lldpTlv **tlv, const struct dcbxV0Feature *feature, uint8_t num_tcs_supported, const uint8_t pri_assignment[8], const uint8_t bandwidth[8]);

/** Decodes PFC data from TLV */
int dcbxV0DecodePfcTlv(const struct lldpTlv *tlv, struct dcbxV0Feature *feature, uint8_t *num_tcpfcs_supported, bool enabled[8]);

/** Encodes PFC data to TLV */
int dcbxV0EncodePfcTlv(struct lldpTlv **tlv, const struct dcbxV0Feature *feature, uint8_t num_tcpfcs_supported, const bool enabled[8]);

/** Decodes Application-Priority to TLV */
int dcbxV0DecodeApplicationPriorityTlv(const struct lldpTlv *tlv, struct dcbxV0Feature *feature, uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t *size);

/** Encodes Application-Priority to TLV */
int dcbxV0EncodeApplicationPriorityTlv(struct lldpTlv **tlv, const struct dcbxV0Feature *feature, const uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t size);

#endif /* DCBX_V0_TLV_H */
