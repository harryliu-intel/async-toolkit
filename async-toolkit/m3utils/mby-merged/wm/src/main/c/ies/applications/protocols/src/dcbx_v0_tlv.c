/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v0_tlv.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX Base Protocol v1.01 TLVs.
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

#include <dcbx_v0_tlv.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

static const uint8_t DCBX_V0_OUI[] = {
    0x00, 0x1B, 0x21
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
/** dcbxV0ValidateOrganizationallyTlv
 * \ingroup dcbx
 *
 * \desc            Validates TLV as valid DCBX Base Protocol v1.01
 *                  Organizationally TLV.
 *
 * \param[in]       tlv is ingress TLV to be validated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0ValidateOrganizationallyTlv(const struct lldpTlv *tlv)
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
    for (i=0; i<sizeof(DCBX_V0_OUI); i++)
        if (tlv->data[i] != DCBX_V0_OUI[i])
            return LLDP_ERR_NOT_FOUND;

    /* sub-type should be 0x02 */
    if (tlv->data[i] != 0x02)
        return LLDP_ERR_NOT_FOUND;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0Decode
 * \ingroup dcbx
 *
 * \desc            Breaks DCBX organizationally TLV to DCBX sub-TLVs chain.
 *
 * \param[in]       tlv is the ingress combined DCBX TLV.
 *
 * \param[out]      tlvChain points to caller-allocated storage where this
 *                  function should place a pointer to the dynamic allocated 
 *                  features TLVs chain.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0Decode(const struct lldpTlv *tlv, struct lldpTlv **tlvChain)
{
    if (dcbxV0ValidateOrganizationallyTlv(tlv) != LLDP_OK) {
        *tlvChain = NULL;
        return LLDP_ERR_INVALID_ARG;
    }

    return lldpDecode(tlv->data, tlv->len, sizeof(DCBX_V0_OUI) + 1, tlvChain);
}

/*****************************************************************************/
/** dcbxV0Encode
 * \ingroup dcbx
 *
 * \desc            Combines DCBX sub-TLVs chain to single DCBX 
 *                  organizationally TLV.
 *
 * \param[in]       tlvChain is the feature TLVs chain.
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0Encode(const struct lldpTlv *tlvChain, struct lldpTlv **tlv)
{
    int len;
    unsigned int i;
 
    /* allocate TLV to hold sub-TLVs */
    (*tlv) = lldpAllocateTlv(lldpGetTlvChainSize(tlvChain) + 4);

    /* set TLV type to organizationally TLV */ 
    (*tlv)->type = LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC;

    /* check DCBX OUI */
    for (i=0; i<sizeof(DCBX_V0_OUI); i++)
        (*tlv)->data[i] = DCBX_V0_OUI[i];

    /* sub-type should be 0x02 */
    (*tlv)->data[i++] = 0x02;

    return lldpEncode((*tlv)->data, (*tlv)->len, i, tlvChain, &len);
}

/*****************************************************************************/
/** dcbxV0DecodeControlTlv
 * \ingroup dcbx
 *
 * \desc            Decodes Control data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[in]       oper_ver points to caller-allocated storage where this function
 *                  should place a pointer to the operation version.
 *
 * \param[in]       max_ver points to caller-allocated storage where this function
 *                  should place a pointer to the maximum version.
 *
 * \param[in]       seqno points to caller-allocated storage where this function
 *                  should place a pointer to the sequence-no.
 *
 * \param[in]       ackno points to caller-allocated storage where this function
 *                  should place a pointer to the acknoledge-no.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0DecodeControlTlv(const struct lldpTlv *tlv, uint8_t *oper_ver, uint8_t *max_ver, uint32_t *seqno, uint32_t *ackno)
{
    int i = 0;

    if (!tlv || tlv->type != DCBX_V0_TLV_SUBTYPE_CONTROL ||
        tlv->len < 10) {
        return LLDP_ERR_INVALID_ARG;
    }

    *oper_ver = (tlv->data[i++] & 0xff);
    *max_ver  = (tlv->data[i++] & 0xff);

    *seqno    = (((tlv->data[i+0] & 0xff) << 24) | 
                 ((tlv->data[i+1] & 0xff) << 16) | 
                 ((tlv->data[i+2] & 0xff) <<  8) | 
                 ((tlv->data[i+3] & 0xff)));
    i += 4;

    *ackno    = (((tlv->data[i+0] & 0xff) << 24) | 
                 ((tlv->data[i+1] & 0xff) << 16) | 
                 ((tlv->data[i+2] & 0xff) <<  8) | 
                 ((tlv->data[i+3] & 0xff)));
    i += 4;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EncodeControlTlv
 * \ingroup dcbx
 *
 * \desc            Encodes Control TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       oper_ver is the operation version.
 *
 * \param[in]       max_ver is the maximum version.
 *
 * \param[in]       seqno is the sequence-no.
 *
 * \param[in]       ackno is the acknoledge-no.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EncodeControlTlv(struct lldpTlv **tlv, uint8_t oper_ver, uint8_t max_ver, uint32_t seqno, uint32_t ackno)
{
    size_t i = 0, len = 10;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = DCBX_V0_TLV_SUBTYPE_CONTROL;

    /* setup TLV data */
    (*tlv)->data[i++] = oper_ver & 0xff;
    (*tlv)->data[i++] = max_ver  & 0xff;

    (*tlv)->data[i++] = (seqno >> 24) & 0xff;
    (*tlv)->data[i++] = (seqno >> 16) & 0xff;
    (*tlv)->data[i++] = (seqno >>  8) & 0xff;
    (*tlv)->data[i++] = (seqno      ) & 0xff;

    (*tlv)->data[i++] = (ackno >> 24) & 0xff;
    (*tlv)->data[i++] = (ackno >> 16) & 0xff;
    (*tlv)->data[i++] = (ackno >>  8) & 0xff;
    (*tlv)->data[i++] = (ackno      ) & 0xff;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0DecodeEtsTlv
 * \ingroup dcbx
 *
 * \desc            Decodes ETS (PG) data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      feature points to caller-allocated storage where this
 *                  function should place the ETS feature state.
 *
 * \param[out]      num_tcs_supported points to caller-allocated storage where 
 *                  this function should place the ETS max traffic-classes.
 *
 * \param[out]      pri_assignment points to caller-allocated storage where this
 *                  function should place the ETS priority assignment.
 *
 * \param[out]      bandwidth points to caller-allocated storage where this
 *                  function should place the ETS bandwidth allocation.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0DecodeEtsTlv(const struct lldpTlv *tlv, struct dcbxV0Feature *feature, uint8_t *num_tcs_supported, uint8_t pri_assignment[8], uint8_t bandwidth[8])
{
    size_t n, i = 0;

    if (!tlv || tlv->type != DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS ||
        tlv->len < 17) {
        return LLDP_ERR_INVALID_ARG;
    }

    feature->operVersion = (tlv->data[i++] & 0xff);
    feature->maxVersion  = (tlv->data[i++] & 0xff);
    feature->enable      = (tlv->data[i  ] & 0x80) != 0;
    feature->willing     = (tlv->data[i  ] & 0x40) != 0;
    feature->error       = (tlv->data[i++] & 0x20) != 0;
    feature->subType     = (tlv->data[i++] & 0xff);

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

    *num_tcs_supported = tlv->data[i];

    if (bandwidth_total != 100)
        return LLDP_ERR_INVALID_ARG;

    return LLDP_OK;
}


/*****************************************************************************/
/** dcbxV0EncodeEtsTlv
 * \ingroup dcbx
 *
 * \desc            Encodes ETS (PG) TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       feature is the ETS feature state.
 *
 * \param[in]       num_tcs_supported is the ETS max traffic-classes.
 *
 * \param[in]       pri_assignment is the ETS priority assignment.
 *
 * \param[in]       bandwidth is the ETS bandwidth allocation.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EncodeEtsTlv(struct lldpTlv **tlv, const struct dcbxV0Feature *feature, uint8_t num_tcs_supported, const uint8_t pri_assignment[8], const uint8_t bandwidth[8])
{
    size_t n, i = 0, len = 17;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS;

    /* setup TLV feature header */
    (*tlv)->data[i++] = (feature->operVersion & 0xff);
    (*tlv)->data[i++] = (feature->maxVersion  & 0xff);

    (*tlv)->data[i++] = (feature->enable ? 0x80 : 0x00) |
                        (feature->willing ? 0x40 : 0x00) |
                        (feature->error ? 0x20 : 0x00);

    (*tlv)->data[i++] = (feature->subType & 0xff);

    /* setup TLV feature data */

    for (n=0; n<4; n++) {
        (*tlv)->data[i++] = ((pri_assignment[n*2+0] << 4) & 0xf0) |
                            ((pri_assignment[n*2+1]     ) & 0x0f);
    }

    for (n=0; n<8; n++) {
        (*tlv)->data[i++] = bandwidth[n] & 0xff;
    }

    (*tlv)->data[i++] = num_tcs_supported;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0DecodePfcTlv
 * \ingroup dcbx
 *
 * \desc            Decodes PFC data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      feature points to caller-allocated storage where this
 *                  function should place the PFC feature state.
 *
 * \param[out]      num_tcpfcs_supported points to caller-allocated storage 
 *                  where this function should place the PFC capability.
 *
 * \param[out]      enabled points to caller-allocated storage where this
 *                  function should place the PFC priority enable.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0DecodePfcTlv(const struct lldpTlv *tlv, struct dcbxV0Feature *feature, uint8_t *num_tcpfcs_supported, bool enabled[8])
{
    size_t n, i = 0;

    if (!tlv || tlv->type != DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL ||
        tlv->len < 6) {
        return LLDP_ERR_INVALID_ARG;
    }

    feature->operVersion = (tlv->data[i++] & 0xff);
    feature->maxVersion  = (tlv->data[i++] & 0xff);
    feature->enable      = (tlv->data[i  ] & 0x80) != 0;
    feature->willing     = (tlv->data[i  ] & 0x40) != 0;
    feature->error       = (tlv->data[i++] & 0x20) != 0;
    feature->subType     = (tlv->data[i++] & 0xff);

    for (n=0; n<8; n++) {
        enabled[n] = ((tlv->data[i] >> n) & 1) != 0;
    }
    i++;

    *num_tcpfcs_supported = tlv->data[i];

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EncodePfcTlv
 * \ingroup dcbx
 *
 * \desc            Encodes PFC TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       feature is the PFC feature state.
 *
 * \param[in]       num_tcpfcs_supported is the PFC capability.
 *
 * \param[in]       enabled is the PFC priority enable.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EncodePfcTlv(struct lldpTlv **tlv, const struct dcbxV0Feature *feature, uint8_t num_tcpfcs_supported, const bool enabled[8])
{
    size_t n, i = 0, len = 6;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL;

    /* setup TLV feature header */
    (*tlv)->data[i++] = (feature->operVersion & 0xff);
    (*tlv)->data[i++] = (feature->maxVersion  & 0xff);

    (*tlv)->data[i++] = (feature->enable ? 0x80 : 0x00) |
                        (feature->willing ? 0x40 : 0x00) |
                        (feature->error ? 0x20 : 0x00);

    (*tlv)->data[i++] = (feature->subType & 0xff);

    /* setup TLV feature data */

    (*tlv)->data[i] = 0;
    for (n=0; n<8; n++) if (enabled[n]) {
        (*tlv)->data[i] |= (1 << n);
    }
    i++;

    (*tlv)->data[i++] = (num_tcpfcs_supported & 0xff);

    return LLDP_OK;
}


/*****************************************************************************/
/** dcbxV0DecodeApplicationPriorityTlv
 * \ingroup dcbx
 *
 * \desc            Decodes Application-Priority data from TLV
 *
 * \param[in]       tlv is ingress TLV.
 *
 * \param[out]      feature points to caller-allocated storage where this
 *                  function should place the Application-Priority feature state.
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
int dcbxV0DecodeApplicationPriorityTlv(const struct lldpTlv *tlv, struct dcbxV0Feature *feature, uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t *size)
{
    size_t p, n, i = 0;
    uint32_t apcode_mask, apcode_base;

    if (!tlv || tlv->type != DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY ||
        tlv->len < 4) {
        return LLDP_ERR_INVALID_ARG;
    }

    feature->operVersion = (tlv->data[i++] & 0xff);
    feature->maxVersion  = (tlv->data[i++] & 0xff);
    feature->enable      = (tlv->data[i  ] & 0x80) != 0;
    feature->willing     = (tlv->data[i  ] & 0x40) != 0;
    feature->error       = (tlv->data[i++] & 0x20) != 0;
    feature->subType     = (tlv->data[i++] & 0xff);

    *size = (tlv->len - 4) / 6;
    if (*size > DCBX_TLV_MAX_APPLICATION_PRIORITIES)
        *size = DCBX_TLV_MAX_APPLICATION_PRIORITIES;

    /*
     * Incoming Format (DCBX v0):
     *
     * | 48 .. 32 | 31 .. 26 | 25 24 | 23 .. 16 | 15 ..  8 | 7 .. 0 |
     * | Proto    | OUI23-18 | Sel   | OUI15-08 | OUI07-00 | PriMap |
     *
     *
     * Apcode Format (IEEE DCBX):
     *
     * | 23 24 22 21 | .. | 18 17 16 | 15 .. 0 |
     * | Priority    |    | Sel      | Proto   |
     *
     */
    for (n=0; n<*size; n++) {
        apcode[n]  = ((tlv->data[i++] & 0xff) <<  8);
        apcode[n] |= ((tlv->data[i++] & 0xff)      );
        apcode[n] |= ((tlv->data[i++] & 0x03) << 16);
        i++;
        i++;
        apcode_mask = (tlv->data[i++] & 0xff);
        apcode_base = apcode[n];

        if (!apcode_mask) 
            return LLDP_ERR_INVALID_FORMAT;

        for (p=0; p<8 && apcode_mask; p++, apcode_mask >>= 1) if (apcode_mask & 1)
        {
            apcode[n] = apcode_base | (p << 21);

            if (apcode_mask >> 1) {
                n++;
                (*size)++;
            }
        }
    }
   
    for (;n<DCBX_TLV_MAX_APPLICATION_PRIORITIES; n++)
        apcode[n] = 0;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EncodeApplicationPriorityTlv
 * \ingroup dcbx
 *
 * \desc            Encodes Application-Priority TLV from data
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       feature is the Application-Priority feature state.
 *
 * \param[in]       apcode is the Application-Priority entries.
 *
 * \param[in]       size is the number of apcode entries.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EncodeApplicationPriorityTlv(struct lldpTlv **tlv, const struct dcbxV0Feature *feature, const uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES], size_t size)
{
    size_t n, i = 0, len = 4 + size * 6;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY;

    /* setup TLV feature header */
    (*tlv)->data[i++] = (feature->operVersion & 0xff);
    (*tlv)->data[i++] = (feature->maxVersion  & 0xff);

    (*tlv)->data[i++] = (feature->enable ? 0x80 : 0x00) |
                        (feature->willing ? 0x40 : 0x00) |
                        (feature->error ? 0x20 : 0x00);

    (*tlv)->data[i++] = (feature->subType & 0xff);

    /* setup TLV data */
    for (n=0; n<size; n++) {
        (*tlv)->data[i++] = (apcode[n] >>  8) & 0xff;
        (*tlv)->data[i++] = (apcode[n]      ) & 0xff;
        (*tlv)->data[i++] = ((apcode[n] >> 16) & 0x03) | (DCBX_V0_OUI[0] & 0xfc);
        (*tlv)->data[i++] = (DCBX_V0_OUI[1] & 0xff);
        (*tlv)->data[i++] = (DCBX_V0_OUI[2] & 0xff);
        (*tlv)->data[i++] = 1 << ((apcode[n] >> 21) & 0x07);
    }

    return LLDP_OK;
}

