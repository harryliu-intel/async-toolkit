/** Vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_tlv.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for DCBX TLV.
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

#ifndef DCBX_V1_TLV_H
#define DCBX_V1_TLV_H

#include <stdint.h>

#include <dcbx_mib.h>

/** DCBX TLV types. Used as a constant in the ''dcbx_tlv'' structure. */
enum dcbxTlvSubType {
    /** Congestion-Notification TLV */
    DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION = 8,

    /** ETS Configuration TLV */
    DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION = 9,

    /** ETS Recommendation TLV */
    DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION = 10,

    /** PFC TLV */
    DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL = 11,

    /** Application-Priority TLV */
    DCBX_V1_TLV_SUBTYPE_APPLICATION_PRIORITY = 12,

    /** For internal use only. */
    DCBX_V1_TLV_SUBTYPE_MAX
};

/** Validates valid DCBX organizationally TLV */
int dcbxV1ValidateOrganizationallyTlv(const struct lldpTlv *tlv);

/** Decodes ETS-Configuation data from TLV */
int dcbxV1DecodeEtsConfigurationTlv(const struct lldpTlv *tlv, bool *willing, bool *cbs, uint8_t *max_tcs, uint8_t pri_assignment[8], uint8_t bandwidth[8], uint8_t tsa_assignment[8]);

/** Encodes ETS-Configuationdata to TLV */
int dcbxV1EncodeEtsConfigurationTlv(struct lldpTlv **tlv, bool willing, bool cbs, uint8_t max_tcs, const uint8_t pri_assignment[8], const uint8_t bandwidth[8], const uint8_t tsa_assignment[8]);

/** Decodes ETS-Recommendation data to TLV */
int dcbxV1DecodeEtsRecommendationTlv(const struct lldpTlv *tlv, uint8_t pri_assignment[8], uint8_t bandwidth[8], uint8_t tsa_assignment[8]);

/** Encodes ETS-Recommendation data to TLV */
int dcbxV1EncodeEtsRecommendationTlv(struct lldpTlv **tlv, const uint8_t pri_assignment[8], const uint8_t bandwidth[8], const uint8_t tsa_assignment[8]);

/** Decodes PFC data to TLV */
int dcbxV1DecodePfcTlv(const struct lldpTlv *tlv, bool *willing, bool *mbc, uint8_t *capability, bool enabled[8]);

/** Encodes PFC data to TLV */
int dcbxV1EncodePfcTlv(struct lldpTlv **tlv, bool willing, bool mbc, uint8_t capability, const bool enabled[8]);

/** Decodes Application-Priority to TLV */
int dcbxV1DecodeApplicationPriorityTlv(const struct lldpTlv *tlv, bool *willing, uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t *size);

/** Encodes Application-Priority to TLV */
int dcbxV1EncodeApplicationPriorityTlv(struct lldpTlv **tlv, bool willing, const uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t size);

/** Decodes Congestion-Notification data to TLV */
int dcbxV1DecodeCnTlv(const struct lldpTlv *tlv, bool cnpv_supported[8], bool cnpv_ready[8]);

/** Encodes Congestion-Notification data to TLV */
int dcbxV1EncodeCnTlv(struct lldpTlv **tlv, const bool cnpv_supported[8], const bool cnpv_ready[8]);

#endif /* DCBX_V1_TLV_H */
