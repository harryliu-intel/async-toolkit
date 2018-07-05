/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_tlv.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP TLV.
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
#include <string.h>

#include <lldp_int.h>
#include <lldp_tlv.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

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
/** lldpAllocateTlv
 * \ingroup lldp
 *
 * \desc            Allocate a TLV buffer of specified size.
 *
 * \param[in]       len is size of TLV data to allocate.
 *
 * \return          A pointer to an ''struct lldpTlv'' structure.
 * \return          NULL is returned if allocation failed.
 *
 *****************************************************************************/
struct lldpTlv *lldpAllocateTlv(size_t len)
{
    struct lldpTlv *outTlv = NULL;

    outTlv = calloc(1, sizeof(struct lldpTlv));

    if (outTlv) {
        if (len > 0) {
            outTlv->data = calloc(1, len);
            if (outTlv->data)
                outTlv->len = len;
        }
        else outTlv->len = 0;
    }

    return outTlv;
} /* lldpAllocateTlv */

/*****************************************************************************/
/** lldpFreeTlv
 * \ingroup lldp
 *
 * \desc            Release TLV buffer allocation.
 *
 * \note            If the TLV is part of a chain of TLVs, this function
 *                  will not dispose of the other TLVs in the chain.  See
 *                  freeTlvChain for disposing of an entire chain.
 *
 * \param[in]       tlv points to the tlv's ''struct lldpTlv'' structure.
 *
 *****************************************************************************/
void lldpFreeTlv(struct lldpTlv *tlv)
{
    if (tlv) {
        if (tlv->data)
            free(tlv->data);
        free(tlv);
    }
} /* lldpFreeTlv */

/*****************************************************************************/
/** lldpFreeTlvChain
 * \ingroup lldp
 *
 * \desc            Releases an entire chain of TLVs, previously
 *                  allocated with calls to 'lldpAllocateTlv''.
 *
 * \param[in]       tlvChain points to the first TLV in the chain.
 *
 *****************************************************************************/
void lldpFreeTlvChain(struct lldpTlv *tlvChain)
{
    struct lldpTlv *nextTlv;
    struct lldpTlv *curTlv;

    curTlv = tlvChain;

    while (curTlv != NULL) {
        nextTlv = curTlv->next;
        lldpFreeTlv(curTlv);

        curTlv = nextTlv;
    }
} /* end lldpFreeTlvChain */

/*****************************************************************************/
/** lldpAppendTlv
 * \ingroup lldp
 *
 * \desc            Appends a TLV to the end of TLV chain.
 *
 * \param[in]       tlvChain points to the TLV chain.
 *
 * \param[in]       tlv points to the TLV to be appended to the TLV chain.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NOT_FOUND on failure.
 *
 *****************************************************************************/
int lldpAppendTlv(struct lldpTlv *tlvChain, struct lldpTlv *tlv)
{
    struct lldpTlv *curTlv;

    for (curTlv = tlvChain; curTlv && curTlv->next; curTlv = curTlv->next);

    if (curTlv) {
        curTlv->next = tlv;
        return LLDP_OK;
    }

    return LLDP_ERR_NOT_FOUND;
}

/*****************************************************************************/
/** lldpInsertBeforeTlv
 * \ingroup lldp
 *
 * \desc            Inserts a TLV into TLV chain, before the TLV with specified
 *                  type.
 *
 * \param[in]       tlvChain points to the TLV chain.
 *
 * \param[in]       tlv points to the TLV to be inserted to the TLV chain.
 *
 * \param[in]       type is the TLV's type before ''tlv'' should be inserted.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NOT_FOUND on failure.
 *
 *****************************************************************************/
int lldpInsertBeforeTlv(struct lldpTlv *tlvChain, struct lldpTlv *tlv, enum lldpTlvType type)
{
    struct lldpTlv *curTlv;

    for (curTlv = tlvChain; curTlv && curTlv->next; curTlv = curTlv->next) {
        if (curTlv->next->type == type) {
            tlv->next = curTlv->next;
            curTlv->next = tlv;
            return LLDP_OK;
        }
    }

    return LLDP_ERR_NOT_FOUND;

} /* end lldpInsertBeforeTlv */

/*****************************************************************************/
/** lldpGetTlvChainSize
 * \ingroup lldp
 *
 * \desc            Gets total buffer size required to contain the TLV chain.
 *
 * \param[in]       tlvChain points to the TLV chain.
 *
 * \return          Buffer size (in bytes) required to contain the TLV chain.
 *
 *****************************************************************************/
int lldpGetTlvChainSize(const struct lldpTlv *tlvChain)
{
    int len = 0;
    const struct lldpTlv *tlv;

    for (tlv = tlvChain; tlv; tlv = tlv->next)
        len += tlv->len + 2;

    return len;
}

/*****************************************************************************/
/** lldpDecode
 * \ingroup lldp
 *
 * \desc            Decode LLDPDU packet to TLV chain (link-list).
 *
 * \param[in]       buf is the LLDPDU packet data.
 *
 * \param[in]       size is the LLDPDU packet size.
 *
 * \param[in]       offset is the offset at the LLDPDU packet to the LLDPDU
 *                  data (for standard Ethernet packet this will be 18 (6 DA +
 *                  6 SA + 2 EtherType).
 *
 * \param[out]      tlvChain points to caller-allocated storage where this
 *                  function should place a pointer to the dynamic allocated
 *                  TLV chain.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if packet / packet-size is invalid.
 * \return          LLDP_ERR_INVALID_PACKET if invalid packet format.
 *
 *****************************************************************************/
int lldpDecode(uint8_t *buf, size_t size, size_t offset, struct lldpTlv **tlvChain)
{
    int status = LLDP_OK;

    int len, type, l;
    size_t n;
    struct lldpTlv *tlv = NULL;

    n = offset;

    *tlvChain = NULL;

    if (buf == NULL || size == 0 || offset >= size) {
        LLDP_ERR("LLDP: Error decoding packet to TLVs.\n");
        status = LLDP_ERR_INVALID_ARG;
        goto ABORT;
    }

    do {
        /* get TLV type */
        if (n >= size) {
            LLDP_ERR("LLDP: Error decoding TLV type (n:%zu size:%zu).\n", n, size);
            status = LLDP_ERR_INVALID_PACKET;
            goto ABORT;
        }
        type = buf[n++];

        /* get TLV size */
        if (n >= size) {
            LLDP_ERR("LLDP: Error decoding TLV size (n:%zu size:%zu).\n", n, size);
            status = LLDP_ERR_INVALID_PACKET;
            goto ABORT;
        }
        len = buf[n++];

        /* get true TLV type and size */
        if (type & 0x01)
            len += 0x100;
        type = (type >> 1) & 0x7f;

        /* allocate a new TLV */
        if (tlv == NULL) {
            tlv = lldpAllocateTlv(len);
            *tlvChain = tlv;
        } else {
            tlv->next = lldpAllocateTlv(len);
            tlv = tlv->next;
        }

        tlv->type = type;
        tlv->len  = len;

        /* get TLV data */
        for (l = 0; l < len; l++) {
            if (n >= size) {
                LLDP_ERR("LLDP: Error decoding TLV data (type:%d len:%zu l:%d size:%zu).\n", tlv->type, tlv->len, l, size);
                status = LLDP_ERR_INVALID_PACKET;
                goto ABORT;
            }
            tlv->data[l] = buf[n++];
        }

    } while (tlv->type != LLDP_TLV_TYPE_END_OF_LLDPDU && n < size);

ABORT:

    if (status != LLDP_OK && *tlvChain) {
        lldpFreeTlvChain(*tlvChain);
        *tlvChain = NULL;
    }

    return status;
}

/*****************************************************************************/
/** lldpDecodeGenericIdTlv
 * \ingroup lldp
 *
 * \desc            Decode Generic ID TLV to Id and Id-Subtype.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      idSubtype points to caller-allocated storage where this
 *                  function should place the id sub-type.
 *
 * \param[out]      id points to caller-allocated storage where this
 *                  function should place the id.
 *
 * \param[out]      idLen points to caller-allocated storage where this
 *                  function should place the id length.
 *
 * \param[out]      size is the buffer size of id.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpDecodeGenericIdTlv(struct lldpTlv *tlv, int *idSubtype, char *id, size_t *idLen, size_t size)
{
    *idSubtype = tlv->data[0];
    *idLen = tlv->len - 1;
    lldp_strxcpy(id, size, tlv->data + 1, tlv->len - 1);
    return LLDP_OK;
}

/*****************************************************************************/
/** lldpDecodeGenericStringTlv
 * \ingroup lldp
 *
 * \desc            Decode Generic string TLV to string.
 *
 * \note            It is the responsibility of the caller to free the 
 *                  returned string using a call to 'free'.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      text points to caller-allocated storage where this
 *                  function should place the string.
 *
 * \param[in]       size is the buffer size of text.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpDecodeGenericStringTlv(struct lldpTlv *tlv, char *text, size_t size)
{
    lldp_strxcpy(text, size, tlv->data, tlv->len);
    return 0;
}

/*****************************************************************************/
/** lldpDecodeChassisTlv
 * \ingroup lldp
 *
 * \desc            Decode Chassis ID TLV to Id and Id-Subtype.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      chassisIdSubtype points to caller-allocated storage where
 *                  this function should place the id sub-type.
 *
 * \param[out]      chassisId points to caller-allocated storage where this
 *                  function should place the id.
 *
 * \param[out]      chassisIdLen points to caller-allocated storage where this
 *                  function should place a the actual size of the id.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if TLV format is invalid.
 *
 *****************************************************************************/
int lldpDecodeChassisTlv(struct lldpTlv *tlv, enum lldpChassisIdSubtype *chassisIdSubtype, char chassisId[256], size_t *chassisIdLen)
{
    if (!tlv || tlv->type != LLDP_TLV_TYPE_CHASSIS_ID ||
        tlv->len < 2 || tlv->len > 256) {
        return LLDP_ERR_INVALID_ARG;
    }

    return lldpDecodeGenericIdTlv(tlv, (int *)chassisIdSubtype, chassisId, chassisIdLen, 256);
}

/*****************************************************************************/
/** lldpDecodePortTlv
 * \ingroup lldp
 *
 * \desc            Decode Port ID TLV to Id and Id-Subtype.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      portIdSubtype points to caller-allocated storage where
 *                  this function should place the id sub-type.
 *
 * \param[out]      portId points to caller-allocated storage where this
 *                  function should place the id.
 *
 * \param[out]      portIdLen points to caller-allocated storage where this
 *                  function should place a the actual size of the id.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if TLV format is invalid.
 *
 *****************************************************************************/
int lldpDecodePortTlv(struct lldpTlv *tlv, enum lldpPortIdSubtype *portIdSubtype, char portId[256], size_t *portIdLen)
{
    if (!tlv || tlv->type != LLDP_TLV_TYPE_PORT_ID ||
        tlv->len < 2 || tlv->len > 256) {
        return LLDP_ERR_INVALID_ARG;
    }

    return lldpDecodeGenericIdTlv(tlv, (int *)portIdSubtype, portId, portIdLen, 256);
}

/*****************************************************************************/
/** lldpDecodeTtlTlv
 * \ingroup lldp
 *
 * \desc            Decode TTL TLV to ttl.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      ttl points to caller-allocated storage where
 *                  this function should place the ttl value.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if TLV format is invalid.
 *
 *****************************************************************************/
int lldpDecodeTtlTlv(struct lldpTlv *tlv, int *ttl)
{
    if (!tlv || tlv->type != LLDP_TLV_TYPE_TIME_TO_LIVE || tlv->len < 2) {
        return LLDP_ERR_INVALID_ARG;
    }

    *ttl = ((tlv->data[0] & 0xff) << 8) | (tlv->data[1] & 0xff);

    return 0;
}

/*****************************************************************************/
/** lldpDecodeSystemCapTlv
 * \ingroup lldp
 *
 * \desc            Decode System-Capability TLV.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      sysCapSupported points to caller-allocated storage where
 *                  this function should place the supported capabilities mask.
 *
 * \param[out]      sysCapEnabled points to caller-allocated storage where
 *                  this function should place the enabled capabilities mask.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if TLV format is invalid.
 * \return          LLDP_ERR_INVALID_RANGE if enabled caps are not supported.
 *
 *****************************************************************************/
int lldpDecodeSystemCapTlv(struct lldpTlv *tlv,
                           uint16_t *sysCapSupported,
                           uint16_t *sysCapEnabled)
{
    if (!tlv || tlv->type != LLDP_TLV_TYPE_SYSTEM_CAPABILITIES ||
        tlv->len < 4) {
        return LLDP_ERR_INVALID_ARG;
    }

    *sysCapSupported = ((tlv->data[0] & 0xff) << 8) | 
                       ((tlv->data[1] & 0xff));
    *sysCapEnabled =   ((tlv->data[2] & 0xff) << 8) |
                       ((tlv->data[3] & 0xff));

    /* return error if feature is enabled but is not supported */
    if ((*sysCapEnabled) & (~(*sysCapSupported)))
        return LLDP_ERR_INVALID_RANGE;

    return 0;
}

/*****************************************************************************/
/** lldpDecodeMgmtAddressTlv
 * \ingroup lldp
 *
 * \desc            Decode Management-Address TLV.
 *
 * \param[in]       tlv is the TLV to decode.
 *
 * \param[out]      manAddrSubtype points to caller-allocated storage where
 *                  this function should place the mgmt address sub-type.
 *
 * \param[out]      manAddr points to caller-allocated storage where
 *                  this function should place the mgmt address.
 *
 * \param[out]      manAddrIntfIdSubtype points to caller-allocated storage where
 *                  this function should place the interface id sub-type.
 *
 * \param[out]      manAddrIntfId points to caller-allocated storage where
 *                  this function should place the interface id.
 *
 * \param[out]      manAddrOid points to caller-allocated storage where
 *                  this function should place the OID.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if TLV format is invalid.
 *
 *****************************************************************************/
int lldpDecodeMgmtAddressTlv(struct lldpTlv *tlv,
                             int *manAddrSubtype, char manAddr[32],
                             int *manAddrIntfIdSubtype, uint32_t *manAddrIntfId,
                             char manAddrOid[256])
{
    int n = 0, l = 0;

    if (!tlv || tlv->type != LLDP_TLV_TYPE_MANAGEMENT_ADDRESS ||
        tlv->len < 9 || tlv->len > 167) {
        return LLDP_ERR_INVALID_ARG;
    }

    /* get management address */
    n = tlv->data[l++];
    *manAddrSubtype = tlv->data[l];    
    lldp_strxcpy(manAddr, 32, tlv->data + l + 1, n - 1);
  
    l += n;

    /* get interface number */
    *manAddrIntfIdSubtype = tlv->data[l++];
    *manAddrIntfId = (((tlv->data[l  ] & 0xff) << 24) | 
               ((tlv->data[l+1] & 0xff) << 16) | 
               ((tlv->data[l+2] & 0xff) <<  8) | 
               ((tlv->data[l+3] & 0xff)));
    l += 5;

    /* get OID */
    n = tlv->data[l++];
    if (n > 0)
        lldp_strxcpy(manAddrOid, 256, tlv->data + l, n);
    else
        manAddrOid[0] = '\0';

    return 0;
}

/*****************************************************************************/
/** lldpEncode
 * \ingroup lldp
 *
 * \desc            Encode TLV chain to LLDPDU packet buffer.
 *
 * \param[in]       buf is the LLDPDU packet data buffer.
 *
 * \param[in]       size is the LLDPDU packet data buffer size.
 *
 * \param[in]       offset is the offset at the LLDPDU packet to the LLDPDU
 *                  data (for standard Ethernet packet this will be 18 (6 DA +
 *                  6 SA + 2 EtherType).
 *
 * \param[out]      tlvChain points to TLV chain to be encoded into the LLDPDU
 *                  packet data buffer.
 *
 * \param[out]      len points to caller-allocated storage where
 *                  this function should place the actual LLDPDU size.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if packet / packet-size is invalid.
 * \return          LLDP_ERR_INVALID_PACKET if invalid packet format.
 *
 *****************************************************************************/
int lldpEncode(uint8_t *buf, size_t size, size_t offset, const struct lldpTlv *tlvChain, int *len)
{
    size_t l, n;
    const struct lldpTlv *tlv;

    n = offset;

    for (tlv = tlvChain; tlv; tlv = tlv->next) {
        /* set TLV type (and one bit of size) */
        if (n >= size) return LLDP_ERR_INVALID_ARG;
        buf[n++] = (tlv->type << 1) | (tlv->len & 0x100 ? 0x01 : 0x00);

        /* set TLV size */
        if (n >= size) return LLDP_ERR_INVALID_ARG;
        buf[n++] = (tlv->len & 0xff);

        /* set TLV data */
        for (l=0; l<tlv->len; l++) {
            if (n >= size) return LLDP_ERR_INVALID_ARG;
            buf[n++] = tlv->data[l];
        }
    }

    *len = n;

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpEncodeGenericIdTlv
 * \ingroup lldp
 *
 * \desc            Encode Id and Id-Subtype to Generic ID TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 *
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       type is the TLV type.
 *
 * \param[in]       idSubtype is the id sub-type.
 *
 * \param[in]       id is the id.
 *
 * \param[in]       idStrLen is the id length.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 *
 *****************************************************************************/
int lldpEncodeGenericIdTlv(struct lldpTlv **tlv, int type, 
                           int idSubtype, char *id, int idStrLen)
{
    int len = 1 + idStrLen;

    (*tlv) = lldpAllocateTlv(len);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = type;
    (*tlv)->data[0] = idSubtype & 0xff;
    memcpy((*tlv)->data + 1, id, idStrLen);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpEncodeGenericStringTlv
 * \ingroup lldp
 *
 * \desc            Encode string to Generic string TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 * 
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       type is the TLV type.
 *
 * \param[in]       text is the string to encode.
 *
 * \param[in]       textStrLen is the string length.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 * \return          LLDP_ERR_INVALID_ARG if arguments are invalid.
 *
 *****************************************************************************/
int lldpEncodeGenericStringTlv(struct lldpTlv **tlv, int type,
                               char *text, int textStrLen)
{
    (*tlv) = lldpAllocateTlv(textStrLen);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = type;
    memcpy((*tlv)->data, text, textStrLen);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpDecodeChassisTlv
 * \ingroup lldp
 *
 * \desc            Decode Id and Id-Subtype to Chassis ID TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 * 
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       chassisIdSubtype is the chassis-id sub-type.
 *
 * \param[in]       chassisId is the chassis-id.
 *
 * \param[in]       chassisIdLen is the chassis-id length.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 * \return          LLDP_ERR_INVALID_ARG if arguments are invalid.
 *
 *****************************************************************************/
int lldpEncodeChassisTlv(struct lldpTlv **tlv, enum lldpChassisIdSubtype chassisIdSubtype, char *chassisId, size_t chassisIdLen)
{
    if (chassisIdLen < 1 || chassisIdLen > 255)
        return LLDP_ERR_INVALID_ARG;

    return lldpEncodeGenericIdTlv(tlv, LLDP_TLV_TYPE_CHASSIS_ID, 
                                  (int)chassisIdSubtype, chassisId, chassisIdLen);
}

/*****************************************************************************/
/** lldpEncodePortTlv
 * \ingroup lldp
 *
 * \desc            Decode Id and Id-Subtype to Port ID TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 * 
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       portIdSubtype is the port-id sub-type.
 *
 * \param[in]       portId is the port-id.
 *
 * \param[in]       portIdLen is the port-id length.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 * \return          LLDP_ERR_INVALID_ARG if arguments are invalid.
 *
 *****************************************************************************/
int lldpEncodePortTlv(struct lldpTlv **tlv, enum lldpPortIdSubtype portIdSubtype, char *portId, size_t portIdLen)
{
    if (portIdLen < 1 || portIdLen > 255)
        return LLDP_ERR_INVALID_ARG;

    return lldpEncodeGenericIdTlv(tlv, LLDP_TLV_TYPE_PORT_ID, 
                                  (int)portIdSubtype, portId, portIdLen);
}

/*****************************************************************************/
/** lldpEncodeTtlTlv
 * \ingroup lldp
 *
 * \desc            Decode TTL to TTL TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 * 
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[in]       ttl is the LLDPDU time-to-live.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 * \return          LLDP_ERR_INVALID_ARG if arguments are invalid.
 *
 *****************************************************************************/
int lldpEncodeTtlTlv(struct lldpTlv **tlv, int ttl)
{
    (*tlv) = lldpAllocateTlv(2);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_TIME_TO_LIVE;    
    (*tlv)->data[0] = (ttl >> 8) & 0xff;
    (*tlv)->data[1] = (ttl     ) & 0xff;

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpEncodeEndTlv
 * \ingroup lldp
 *
 * \desc            Decode End-of-LLDPDU TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 * 
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 * \return          LLDP_ERR_INVALID_ARG if arguments are invalid.
 *
 *****************************************************************************/
int lldpEncodeEndTlv(struct lldpTlv **tlv)
{
    (*tlv) = lldpAllocateTlv(0);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_END_OF_LLDPDU;

    return LLDP_OK;
}

/* encode string to TLV */
int lldpEncodeSystemCapTlv(struct lldpTlv **tlv, uint16_t sysCapSupported,
                           uint16_t sysCapEnabled)
{
    (*tlv) = lldpAllocateTlv(4);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_SYSTEM_CAPABILITIES;    
    (*tlv)->data[0] = (sysCapSupported >> 8) & 0xff;
    (*tlv)->data[1] = (sysCapSupported     ) & 0xff;
    (*tlv)->data[2] = (sysCapEnabled   >> 8) & 0xff;
    (*tlv)->data[3] = (sysCapEnabled       ) & 0xff;

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpEncodeMgmtAddressTlv
 * \ingroup lldp
 *
 * \desc            Encode managment-address to Management-Address TLV.
 *
 * \note            Caller is responsible for freeing tlv using lldpFreeTlv().
 * 
 * \param[out]      tlv points to caller-allocated storage where this function
 *                  should place a pointer to the dynamic allocated TLV.
 *
 * \param[out]      manAddrSubtype is the mgmt address sub-type.
 *
 * \param[out]      manAddr points is the mgmt address.
 *
 * \param[out]      manAddrIntfIdSubtype is the interface id sub-type.
 *
 * \param[out]      manAddrIntfId points is the interface id.
 *
 * \param[out]      manAddrOid points is the OID.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_INVALID_ARG if TLV format is invalid.
 *
 *****************************************************************************/
int lldpEncodeMgmtAddressTlv(struct lldpTlv **tlv, int manAddrSubtype,
                             char *manAddr, int manAddrIntfIdSubtype,
                             uint32_t manAddrIntfId, char *manAddrOid)
{
    int l = 0;

    int manStrLen = manAddr ? strlen(manAddr) : 0;
    int oidStrLen = manAddrOid ? strlen(manAddrOid) : 0;

    if (manStrLen < 1 || manStrLen > 31 || oidStrLen > 128)
        return LLDP_ERR_INVALID_ARG;

    (*tlv) = lldpAllocateTlv(8 + manStrLen + oidStrLen);
    if (!(*tlv)) return LLDP_ERR_NO_MEMORY;

    (*tlv)->type = LLDP_TLV_TYPE_MANAGEMENT_ADDRESS;    

    (*tlv)->data[l++] = manStrLen & 0xff;
    (*tlv)->data[l++] = manAddrSubtype & 0xff;
    memcpy((*tlv)->data + l, manAddr, manStrLen);
    l += manStrLen;

    (*tlv)->data[l++] = manAddrIntfIdSubtype & 0xff;
    (*tlv)->data[l++] = (manAddrIntfId >> 24) & 0xff;
    (*tlv)->data[l++] = (manAddrIntfId >> 16) & 0xff;
    (*tlv)->data[l++] = (manAddrIntfId >>  8) & 0xff;
    (*tlv)->data[l++] = (manAddrIntfId      ) & 0xff;

    (*tlv)->data[l++] = oidStrLen & 0xff;
    if (oidStrLen > 0)
        memcpy((*tlv)->data + l, manAddrOid, oidStrLen);

    return LLDP_OK;
}

