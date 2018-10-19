/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_tlv.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX P802.1Qaz/D2.1 TLVs.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <lldp.h>
#include <lldp_tlv.h>

#include <dcbx_v1_tlv.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

static const uint8_t DCBX_V1_OUI[] = {
    0x00, 0x80, 0xC2
};

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

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV1ValidateOrganizationallyTlv
 * \ingroup dcbx
 *
 * \desc            Validates TLV as valid DCBX P802.1Qaz/D2.1
 *                  Organizationally TLV.
 *
 * \param[in]       tlv is ingress TLV to be validated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1ValidateOrganizationallyTlv(const struct lldpTlv *tlv)
{
    size_t i;

    if (!tlv || tlv->type != LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC ||
        tlv->len < 4) {
        return LLDP_ERR_INVALID_ARG;
    }

    /* we only process organizationally TLVs */
    if (tlv->type != LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC)
        return LLDP_ERR_NOT_FOUND;

    /* check DCBX OUI */
    for (i=0; i<sizeof(DCBX_V1_OUI); i++)
        if (tlv->data[i] != DCBX_V1_OUI[i])
            return LLDP_ERR_NOT_FOUND;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1DecodeEtsTlv
 * \ingroup dcbx
 *
 * \desc            Decodes General ETS data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[in]       type is ETS TLV type.
 *
 * \param[out]      willing points to caller-allocated storage where this
 *                  function should place the ETS willing.
 *
 * \param[out]      cbs points to caller-allocated storage where this
 *                  function should place the ETS CBS.
 *
 * \param[out]      max_tcs points to caller-allocated storage where this
 *                  function should place the ETS max traffic-classes.
 *
 * \param[out]      pri_assignment points to caller-allocated storage where this
 *                  function should place the ETS priority assignment.
 *
 * \param[out]      bandwidth points to caller-allocated storage where this
 *                  function should place the ETS bandwidth allocation.
 *
 * \param[out]      tsa_assignment points to caller-allocated storage where this
 *                  function should place the ETS traffic algorithm selection.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1DecodeEtsTlv(const struct lldpTlv *tlv, enum dcbxTlvSubType type, bool *willing, bool *cbs, uint8_t *max_tcs, uint8_t pri_assignment[8], uint8_t bandwidth[8], uint8_t tsa_assignment[8])
{
    size_t i = 4, n;

    if (dcbxV1ValidateOrganizationallyTlv(tlv) != LLDP_OK ||
        tlv->data[3] != type ||
        tlv->len < 25) {
        return LLDP_ERR_INVALID_ARG;
    }

    if (willing) *willing = (tlv->data[i] & 0x80) != 0;
    if (cbs)     *cbs     = (tlv->data[i] & 0x40) != 0;
    if (max_tcs) *max_tcs = (tlv->data[i] & 0x07);
    i++;

    for (n=0; n<4; n++) {
        pri_assignment[2*n+0] = (tlv->data[i+n] >> 4) & 0x0f;
        pri_assignment[2*n+1] = (tlv->data[i+n]     ) & 0x0f;
    }
    i += n;

    int bandwidth_total = 0;
    for (n=0; n<8; n++) {
        bandwidth[n] = (tlv->data[i+n] & 0xff);
        bandwidth_total += bandwidth[n];
    }
    i += n;

    for (n=0; n<8; n++) {
        tsa_assignment[n] = (tlv->data[i+n] & 0xff);
    }

    if (bandwidth_total != 100)
        return LLDP_ERR_INVALID_ARG;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EncodeEtsTlv
 * \ingroup dcbx
 *
 * \desc            Encodes General ETS TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       type is ETS TLV type.
 *
 * \param[in]       willing is the ETS willing.
 *
 * \param[in]       cbs is the ETS CBS.
 *
 * \param[in]       max_tcs pis the ETS max traffic-classes.
 *
 * \param[in]       pri_assignment is the ETS priority assignment.
 *
 * \param[in]       bandwidth is the ETS bandwidth allocation.
 *
 * \param[in]       tsa_assignment is the ETS traffic algorithm selection.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1EncodeEtsTlv(struct lldpTlv **tlv, enum dcbxTlvSubType type, bool willing, bool cbs, uint8_t max_tcs, const uint8_t pri_assignment[8], const uint8_t bandwidth[8], const uint8_t tsa_assignment[8])
{
    size_t i, n, len = sizeof(DCBX_V1_OUI) + 1 + 21;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC;

    /* setup TLV OUI and sub-type */
    for (i=0; i<sizeof(DCBX_V1_OUI); i++)
        (*tlv)->data[i] = DCBX_V1_OUI[i];
    (*tlv)->data[i++] = type;

    /* setup TLV data */
    (*tlv)->data[i++] = (willing ? 0x80 : 0x00) | 
                        (cbs ? 0x40 : 0x00) |
                        (max_tcs & 0x07);
 
    for (n=0; n<4; n++) {
        (*tlv)->data[i++] = ((pri_assignment[n*2+0] << 4) & 0xf0) |
                            ((pri_assignment[n*2+1]     ) & 0x0f);
    }

    for (n=0; n<8; n++) {
        (*tlv)->data[i++] = bandwidth[n] & 0xff;
    }

    for (n=0; n<8; n++) {
        (*tlv)->data[i++] = tsa_assignment[n] & 0xff;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1DecodeEtsConfigurationTlv
 * \ingroup dcbx
 *
 * \desc            Decodes ETS-Configuration data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      willing points to caller-allocated storage where this
 *                  function should place the ETS willing.
 *
 * \param[out]      cbs points to caller-allocated storage where this
 *                  function should place the ETS CBS.
 *
 * \param[out]      max_tcs points to caller-allocated storage where this
 *                  function should place the ETS max traffic-classes.
 *
 * \param[out]      pri_assignment points to caller-allocated storage where this
 *                  function should place the ETS priority assignment.
 *
 * \param[out]      bandwidth points to caller-allocated storage where this
 *                  function should place the ETS bandwidth allocation.
 *
 * \param[out]      tsa_assignment points to caller-allocated storage where this
 *                  function should place the ETS traffic algorithm selection.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1DecodeEtsConfigurationTlv(const struct lldpTlv *tlv, bool *willing, bool *cbs, uint8_t *max_tcs, uint8_t pri_assignment[8], uint8_t bandwidth[8], uint8_t tsa_assignment[8])
{
    return dcbxV1DecodeEtsTlv(tlv, DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION, willing, cbs, max_tcs, pri_assignment, bandwidth, tsa_assignment);
}

/*****************************************************************************/
/** dcbxV1EncodeEtsConfigurationTlv
 * \ingroup dcbx
 *
 * \desc            Encodes ETS-Configuration TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       willing is the ETS willing.
 *
 * \param[in]       cbs is the ETS CBS.
 *
 * \param[in]       max_tcs pis the ETS max traffic-classes.
 *
 * \param[in]       pri_assignment is the ETS priority assignment.
 *
 * \param[in]       bandwidth is the ETS bandwidth allocation.
 *
 * \param[in]       tsa_assignment is the ETS traffic algorithm selection.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EncodeEtsConfigurationTlv(struct lldpTlv **tlv, bool willing, bool cbs, uint8_t max_tcs, const uint8_t pri_assignment[8], const uint8_t bandwidth[8], const uint8_t tsa_assignment[8])
{
    return dcbxV1EncodeEtsTlv(tlv, DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION, willing, cbs, max_tcs, pri_assignment, bandwidth, tsa_assignment);
}

/*****************************************************************************/
/** dcbxV1DecodeEtsRecommendationTlv
 * \ingroup dcbx
 *
 * \desc            Decodes ETS-Recommendation data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      pri_assignment points to caller-allocated storage where this
 *                  function should place the ETS priority assignment.
 *
 * \param[out]      bandwidth points to caller-allocated storage where this
 *                  function should place the ETS bandwidth allocation.
 *
 * \param[out]      tsa_assignment points to caller-allocated storage where this
 *                  function should place the ETS traffic algorithm selection.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1DecodeEtsRecommendationTlv(const struct lldpTlv *tlv, uint8_t pri_assignment[8], uint8_t bandwidth[8], uint8_t tsa_assignment[8])
{
    return dcbxV1DecodeEtsTlv(tlv, DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION, NULL, NULL, NULL, pri_assignment, bandwidth, tsa_assignment);
}

/*****************************************************************************/
/** dcbxV1EncodeEtsRecommendationTlv
 * \ingroup dcbx
 *
 * \desc            Encodes ETS-Recommendation TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       pri_assignment is the ETS priority assignment.
 *
 * \param[in]       bandwidth is the ETS bandwidth allocation.
 *
 * \param[in]       tsa_assignment is the ETS traffic algorithm selection.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EncodeEtsRecommendationTlv(struct lldpTlv **tlv, const uint8_t pri_assignment[8], const uint8_t bandwidth[8], const uint8_t tsa_assignment[8])
{
    return dcbxV1EncodeEtsTlv(tlv, DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION, 0, 0, 0, pri_assignment, bandwidth, tsa_assignment);
}

/*****************************************************************************/
/** dcbxV1DecodePfcTlv
 * \ingroup dcbx
 *
 * \desc            Decodes PFC data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      willing points to caller-allocated storage where this
 *                  function should place the PFC willing.
 *
 * \param[out]      mbc points to caller-allocated storage where this
 *                  function should place the PFC MBC.
 *
 * \param[out]      pfc_cap points to caller-allocated storage where this
 *                  function should place the PFC capability.
 *
 * \param[out]      pfc_enabled points to caller-allocated storage where this
 *                  function should place the PFC priority enable.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1DecodePfcTlv(const struct lldpTlv *tlv, bool *willing, bool *mbc, uint8_t *pfc_cap, bool pfc_enabled[8])
{
    size_t i = 4, n;

    if (dcbxV1ValidateOrganizationallyTlv(tlv) != LLDP_OK ||
        tlv->data[3] != DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL ||
        tlv->len < 6) {
        return LLDP_ERR_INVALID_ARG;
    }

    *willing = (tlv->data[i] & 0x80) != 0;
    *mbc     = (tlv->data[i] & 0x40) != 0;
    *pfc_cap = (tlv->data[i] & 0x0f);
    i++;

    for (n=0; n<8; n++) {
        pfc_enabled[n] = ((tlv->data[i] >> n) & 1) != 0;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EncodePfcTlv
 * \ingroup dcbx
 *
 * \desc            Encodes PFC TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       willing is the PFC willing.
 *
 * \param[in]       mbc is the PFC MBC.
 *
 * \param[in]       pfc_cap is the PFC capability.
 *
 * \param[in]       pfc_enabled is the PFC priority enable.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EncodePfcTlv(struct lldpTlv **tlv, bool willing, bool mbc, uint8_t pfc_cap, const bool pfc_enabled[8])
{
    size_t i, n, len = sizeof(DCBX_V1_OUI) + 1 + 2;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC;

    /* setup TLV OUI and sub-type */
    for (i=0; i<sizeof(DCBX_V1_OUI); i++)
        (*tlv)->data[i] = DCBX_V1_OUI[i];
    (*tlv)->data[i++] = DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL;

    /* setup TLV data */
    (*tlv)->data[i++] = (willing ? 0x80 : 0x00) | 
                        (mbc     ? 0x40 : 0x00) |
                        (pfc_cap & 0x0f);
 
    (*tlv)->data[i] = 0;
    for (n=0; n<8; n++) if (pfc_enabled[n]) {
        (*tlv)->data[i] |= (1 << n);
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1DecodeApplicationPriorityTlv
 * \ingroup dcbx
 *
 * \desc            Decodes Application-Priority data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      willing points to caller-allocated storage where this
 *                  function should place the Application-Priority willing.
 *
 * \param[out]      apcode points to caller-allocated storage where this
 *                  function should place the Application-Priority entries.
 *
 * \param[out]      size points to caller-allocated storage where this
 *                  function should place the number of apcode entries.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1DecodeApplicationPriorityTlv(const struct lldpTlv *tlv, bool *willing, uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t *size)
{
    size_t i = 4, n;

    if (dcbxV1ValidateOrganizationallyTlv(tlv) != LLDP_OK ||
        tlv->data[3] != DCBX_V1_TLV_SUBTYPE_APPLICATION_PRIORITY ||
        tlv->len < 5) {
        return LLDP_ERR_INVALID_ARG;
    }

    *willing = (tlv->data[i] & 0x80) != 0;
    i++;
    
    *size = (tlv->len - 5) / 3;
    if (*size > DCBX_TLV_MAX_APPLICATION_PRIORITIES)
        *size = DCBX_TLV_MAX_APPLICATION_PRIORITIES;

    for (n=0; n<*size; n++) {
        apcode[n]  = ((tlv->data[i++] & 0xff) << 16);
        apcode[n] |= ((tlv->data[i++] & 0xff) <<  8);
        apcode[n] |= ((tlv->data[i++] & 0xff)      );
    }

    for (;n<DCBX_TLV_MAX_APPLICATION_PRIORITIES; n++)
        apcode[n] = 0;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EncodeApplicationPriorityTlv
 * \ingroup dcbx
 *
 * \desc            Encodes Application-Priority TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       willing is the Application-Priority willing.
 *
 * \param[in]       apcode is the Application-Priority entries.
 *
 * \param[in]       size is the number of apcode entries.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EncodeApplicationPriorityTlv(struct lldpTlv **tlv, bool willing, const uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t size)
{
    size_t i, n, len = sizeof(DCBX_V1_OUI) + 1 + 1 + size * 3;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC;

    /* setup TLV OUI and sub-type */
    for (i=0; i<sizeof(DCBX_V1_OUI); i++)
        (*tlv)->data[i] = DCBX_V1_OUI[i];
    (*tlv)->data[i++] = DCBX_V1_TLV_SUBTYPE_APPLICATION_PRIORITY;

    /* setup TLV data */
    (*tlv)->data[i++] = (willing ? 0x80 : 0x00);
 
    for (n=0; n<size; n++) {
        (*tlv)->data[i++] = (apcode[n] >> 16) & 0xff;
        (*tlv)->data[i++] = (apcode[n] >>  8) & 0xff;
        (*tlv)->data[i++] = (apcode[n]      ) & 0xff;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1DecodeCnTlv
 * \ingroup dcbx
 *
 * \desc            Decodes Congestion-Notification data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      cnpv_supported points to caller-allocated storage where this
 *                  function should place the CN supported CNPVs.
 *
 * \param[out]      cnpv_ready points to caller-allocated storage where this
 *                  function should place the CN supported CNPVs.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1DecodeCnTlv(const struct lldpTlv *tlv, bool cnpv_supported[8], bool cnpv_ready[8])
{
    size_t i = 4, n;

    if (dcbxV1ValidateOrganizationallyTlv(tlv) != LLDP_OK ||
        tlv->data[3] != DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION ||
        tlv->len < 6) {
        return LLDP_ERR_INVALID_ARG;
    }

    for (n=0; n<8; n++) {
        cnpv_supported[n] = ((tlv->data[i+0] >> n) & 1) != 0;
        cnpv_ready[n]     = ((tlv->data[i+1] >> n) & 1) != 0;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EncodeCnTlv
 * \ingroup dcbx
 *
 * \desc            Encodes Congestion-Notification TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[out]      cnpv_supported is the CN supported CNPVs.
 *
 * \param[out]      cnpv_ready is the CN supported CNPVs.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EncodeCnTlv(struct lldpTlv **tlv, const bool cnpv_supported[8], const bool cnpv_ready[8])
{
    size_t i, n, len = sizeof(DCBX_V1_OUI) + 1 + 2;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC;

    /* setup TLV OUI and sub-type */
    for (i=0; i<sizeof(DCBX_V1_OUI); i++)
        (*tlv)->data[i] = DCBX_V1_OUI[i];
    (*tlv)->data[i++] = DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION;

    (*tlv)->data[i+0] = 0;
    (*tlv)->data[i+1] = 0;

    for (n=0; n<8; n++) {
        if (cnpv_supported[n]) (*tlv)->data[i+0] |= (1 << n);
        if (cnpv_ready[n])     (*tlv)->data[i+1] |= (1 << n);
    }

    return LLDP_OK;
}

