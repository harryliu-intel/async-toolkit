
#include <stdio.h>

#include "mby_model.h"
#include "mby_srv_handlers.h"
#include "mby_srv_message.h"
#include "mby_srv_utils.h"
#include "mby_srv_log.h"

// TODO what to do with this?
static int debug = 3;

#define NONPOSTED_PORT  0
#define POSTED_PORT     1

/* Offsets into IOSF message */
#define DEST_OFF        0
#define SOURCE_OFF      1
#define OPCODE_OFF      2
#define TAG_OFF         3
#define SAI_OFF         5

/* Relative to dataOff */
#define NDW_OFF         0
#define COMP_DATA_OFF   0

/*IOSF OPCODE */
#define IOSF_REG_READ               0x0
#define IOSF_REG_WRITE              0x1
#define IOSF_REG_BLK_READ           0x10
#define IOSF_REG_BLK_WRITE          0x11
#define IOSF_COMP_NO_DATA           0x20
#define IOSF_COMP_DATA              0x21
/*IOSF response status */
#define IOSF_RSP_SUCCESS        0
#define IOSF_RSP_FAIL           1

#define STRAP_IOSFSB_ENDPT_ID   0xef
#define STRAP_ENDPT_SAI         0xab
#define MAX_READ_BASE_NDW       124
#define MAX_READ_REP_NDW        14

#define PORT_LINK_UP            1

/*Only valid on LE machine */
#define GET_64(p) (*(fm_uint64*)(p))
#define SET_64(p,val) *(fm_uint64*)(p) = val

#define FM_FDS_POLL_TIMEOUT_USEC            1*1000
#define FM_SWITCH_NVM_IMAGE_SIZE            524288

/*****************************************************************************/
/** HandleMsgLinkState
 * \ingroup intModel
 *
 * \desc            Processes link state info.
 *                                                                      \lb\lb
 *                  The control request message format is:
 *                                                                      \lb\lb
 *                  sw field in header is the version number.
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | state (1B)
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       msgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid management message.
 *
 *****************************************************************************/
fm_status HandleMsgLinkState(fm_int               sw,
                             fm_socket           *socket,
                             fm_modelMessage *    imsg,
                             fm_int32             msgLength)
{
    fm_status        status = FM_OK;

    FM_NOT_USED(sw);
    FM_NOT_USED(socket);
    FM_NOT_USED(msgLength);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

#if 0 && MBY_LINK_STATE
    fm_int           linkState;
    fm_int           port;
    port = ntohs(imsg->port);
    linkState = imsg->data[0];

    if (linkState != portLinkState[port])
	printf("PhysPort %d linkState %d oldState %d\n", port, linkState,
	       portLinkState[port]);

    if (port < MAX_PHYS_PORT)
        portLinkState[port] = linkState;

#endif
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgLinkState */

/*****************************************************************************/
/** HandleMsgVersionInfo
 * \ingroup intModel
 *
 * \desc            Processes version info.
 *                                                                      \lb\lb
 *                  The control request message format is:
 *                                                                      \lb\lb
 *                  sw field in header is the version number.
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | version tag string
 *                  +--------------+-----------+--------------+------------+
 *                  The version response has the same format.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       msgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid management message.
 *
 *****************************************************************************/
fm_status HandleMsgVersionInfo(fm_int               sw,
                               fm_socket           *socket,
                               fm_modelMessage *    imsg,
                               fm_int32             msgLength)
{
    fm_status        status = FM_OK;
    fm_modelMessage  emsg;
    fm_int           versionNum;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

    versionNum = imsg->data[0] << 8 | imsg->data[1];
#if 0 // TODO
    if (versionNum != MBY_REG_VERSION)
    {
        printf("****** WARNING ******\n");
        printf("Version number mismatch between API and Server\n");
    }
#endif

    printf("API version 0x%04x. Server version 0x%04x\n",
        versionNum, 0); // HLP_REG_VERSION);
    printf("API VersionTag: %s\n", imsg->data + 2);
    // TODO printf("Server VersionTag: %s\n", fmModelGetVersionTag());
    printf("Connection Socket %d\n", socket->sock);

    FM_CLEAR(emsg);
    FM_MEMCPY_S(&emsg,
                FM_MODEL_MSG_SIZE,
                imsg,
                FM_MODEL_MSG_HEADER_SIZE);

    emsg.data[0] = 1; // FIXME (HLP_REG_VERSION >> 8) & 0xFF;
    emsg.data[1] = 0; // FIXME HLP_REG_VERSION & 0xFF;
    // TODO strcpy((char*)emsg.data + 2, fmModelGetVersionTag());
    msgLength = FM_MODEL_MSG_HEADER_SIZE + strlen((char*)(emsg.data + 2)) + 3;
    emsg.msgLength = htonl(msgLength);

	printf("Version info sent back:\n");
	HexDump((unsigned char *)&emsg, ntohl(emsg.msgLength));
    SendMessage(socket, &emsg, ntohl(emsg.msgLength));

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgVersionInfo */



/*****************************************************************************/
/** HandleMsgIosf
 * \ingroup intModel
 *
 * \desc            Processes register access messages and creates and enqueues
 *                  response messages as necessary.
 *                                                                      \lb\lb
 *                  The management message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | IOSF Message
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       imsgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid management message.
 *
 *****************************************************************************/
fm_status HandleMsgIosf(fm_int               sw,
                        fm_socket           *socket,
                        fm_modelMessage *    imsg,
                        fm_int32             imsgLength)
{
    fm_status        status = FM_OK;
    fm_byte          dest;
    fm_byte          source;
    fm_byte          opcode;
    fm_byte          tag;
    fm_byte          rsp;
    fm_byte          bar;
    fm_byte          fbe;
    fm_byte          exphdr;
    fm_byte          sbe;
    fm_uint16        sai;
    fm_uint32        addr;
    fm_uint64        val64;
    fm_modelMessage  emsg;
    fm_uint          ndw;
    fm_uint          cnt;
    fm_uint          eDataLen;
    fm_uint          iDataLen;
    fm_uint          dataOff;
    fm_uint          ndwOff;
    fm_uint          valOff;
    fm_uint          msgLength;
    fm_bool          saiHdr;
    fm_uint16        port;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p imsgLength=%d\n",
                 sw,
                 (void *) imsg,
                 imsgLength);

    if (debug >=3)
    {
        FM_LOG_PRINT("Request:\n");
        HexDump((fm_byte*)imsg, imsgLength);
    }

     /* Not included in the header if FALSE, similar to HW from AQ point of view */
    saiHdr = TRUE;
    sai = -1;

    port = ntohs(imsg->port);
    dest =  imsg->data[DEST_OFF];
    source =  imsg->data[SOURCE_OFF];
    opcode  = imsg->data[OPCODE_OFF];
    tag = imsg->data[TAG_OFF] & 0x7;
    bar = (imsg->data[TAG_OFF] >> 3) & 0x7;

    dataOff = 4;
    if (saiHdr)
    {
        /* Optional header from ATQ/ARQ perpective */
        exphdr = imsg->data[4] & 0x7F;
        fbe = imsg->data[8] & 0xF;
        sbe = (imsg->data[8] >> 4) & 0xF;

        sai = *(fm_uint16*)(imsg->data + SAI_OFF);
        dataOff = 8;
    }

    rsp = IOSF_RSP_SUCCESS;

    /* Validate dest and source port */
    if (dest != 0x1)
    {
        FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
                "dest=0x%x is not valid strap for HLP - ignoring\n", dest);
	    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }
    if (source != 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "source=0x%x is non-zero\n", source);
            rsp = IOSF_RSP_FAIL;
    }

    /* Validate sai */
    //ValidateSai(sai);

    if (bar)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "bar=0x%x is non-zero\n", bar);
            rsp = IOSF_RSP_FAIL;
    }

    if (saiHdr)
    {
        if (exphdr)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "EXPHDR=0x%x is non-zero\n", exphdr);
                rsp = IOSF_RSP_FAIL;
        }

        if (opcode == IOSF_REG_READ || opcode == IOSF_REG_WRITE)
        {
            if (fbe != 0xF)
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "fbe=0x%x is not 0xF\n", fbe);
                    rsp = IOSF_RSP_FAIL;
            }
            if (!(sbe == 0xF || sbe == 0x0))
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "sbe=0x%x is not 0xF or 0x0\n", sbe);
                    rsp = IOSF_RSP_FAIL;
            }
        }
    }

    addr = (*(fm_uint32*)(imsg->data + dataOff + 4)) >> 12;
    if (addr)
    {
        /* Ignore but check high order bits */
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Addr[47:28]=0x%x is non-zero\n", addr);
        rsp = IOSF_RSP_FAIL;
    }

    FM_CLEAR(emsg);
    eDataLen = dataOff + COMP_DATA_OFF;

    if (rsp == IOSF_RSP_SUCCESS)
    {
        addr = *(fm_uint32*)(imsg->data + dataOff + 2);

        if (debug >= 3)
            printf("dest 0x%x src 0x%x opcode 0x%x tag %d sai 0x%x addr 0x%x\n",
                dest, source, opcode, tag, sai, addr);
//FIXME: how to handle failure in the middle of block operation
        switch (opcode)
        {
            case IOSF_REG_READ:
                if (FM_OK == mbyReadReg(sw, addr, &val64))
                {
                    rsp = IOSF_RSP_SUCCESS;
                    emsg.data[OPCODE_OFF] = IOSF_COMP_DATA;
                    SET_64(emsg.data + eDataLen, val64);
                    eDataLen += 8;
                }
                else
                {
                    rsp = IOSF_RSP_FAIL;
                    emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                }
                break;
            case IOSF_REG_WRITE:
                val64 = GET_64(imsg->data + dataOff + 8);
                if (debug >= 3)
                {
                    printf("Write 0x%x <= 0x%llx\n", addr, val64);
                }
                if (FM_OK == mbyWriteReg(sw, addr, val64))
                {
                    rsp = IOSF_RSP_SUCCESS;
                }
                else
                {
                    rsp = IOSF_RSP_FAIL;
                }
                emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                break;
            case IOSF_REG_BLK_READ:
                emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                rsp = IOSF_RSP_SUCCESS;
                ndw = imsg->data[dataOff + NDW_OFF];
                ndwOff = dataOff + 8; //Start of the multiple read addresses
                iDataLen = imsgLength - FM_MODEL_MSG_HEADER_SIZE;
                if (debug > 4) {
                    printf("#### ndwOff %d iDataLen %d\n", ndwOff, iDataLen);
                }

                if (ndw > MAX_READ_BASE_NDW)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                        "ndw %d is out of range\n", ndw);
                    ndw =  MAX_READ_BASE_NDW;
                    rsp = IOSF_RSP_FAIL;
                    break;
                }

                do
                {
                    if (debug >= 3)
                    {
                        printf("ndw %d addr 0x%x\n", ndw, addr);
                    }
                    if (ndw & 1)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                            "ndw %d is odd value\n", ndw);
                        rsp = IOSF_RSP_FAIL;
                        break;
                    }
                    for (cnt = 0; cnt < ndw/2; cnt++)
                    {
                        if (FM_OK == mbyReadReg(sw, addr + cnt*2*4,
                                                      &val64))
                        {
                            if (debug >= 3)
                                printf("Read: Addr 0x%x = 0x%llx\n",
                                       addr + cnt*2*4, val64);
                            SET_64(emsg.data + eDataLen, val64);
                            eDataLen += 8;
                        }
                        else
                        {
                            /* FIXME: Failure in middle, what should be the response */
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                    }

                    if (ndwOff <= (iDataLen - 4))
                    {
                        ndw = imsg->data[ndwOff] & 0xF;
                        if (ndw > MAX_READ_REP_NDW)
                        {
                            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                "ndw is out of range %d\n", ndw);
                            ndw =  MAX_READ_REP_NDW;
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                        addr = (*(fm_uint32*)(imsg->data+ndwOff)) >> 4;
                    }
                    ndwOff += 4;
                } while (ndwOff <= iDataLen && rsp == IOSF_RSP_SUCCESS);
                rsp = IOSF_RSP_SUCCESS;
                emsg.data[OPCODE_OFF] = IOSF_COMP_DATA;
                break;
            case IOSF_REG_BLK_WRITE:
                emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                rsp = IOSF_RSP_SUCCESS;
                ndw = imsg->data[dataOff + NDW_OFF];
                valOff = dataOff + 8;
                ndwOff = valOff + ndw*4; //Start of the multiple write addresses
                iDataLen = imsgLength - FM_MODEL_MSG_HEADER_SIZE;

                if (ndw > MAX_READ_BASE_NDW)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                        "ndw %d is out of range\n", ndw);
                    ndw =  MAX_READ_BASE_NDW;
                    rsp = IOSF_RSP_FAIL;
                    break;
                }

                do
                {
                    if (debug >= 3)
                    {
                        printf("ndw %d addr 0x%x valOff %d ndwOff %d iDataLen %d\n",
                            ndw, addr, valOff, ndwOff, iDataLen);
                    }
                    if (ndw & 1)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                            "ndw %d is odd value\n", ndw);
                        rsp = IOSF_RSP_FAIL;
                        break;
                    }
					for (cnt = 0; cnt < ndw/2; cnt++)
					{
						val64 = GET_64(imsg->data + valOff + cnt*8);
						if (debug >= 3)
							printf("Write: Addr 0x%x = 0x%llx\n",
									addr + cnt*2*4, val64);
						if (FM_OK != mbyWriteReg(sw, addr + cnt*2*4,
						                               val64))
						{
							/* FIXME: Failure in middle, what should be the response */
							rsp = IOSF_RSP_FAIL;
							break;
						}
					}

                    valOff = valOff + ndw*4 + 4; /* next values */
                    if (valOff <= iDataLen)
                    {
                        ndw = imsg->data[ndwOff] & 0xF;
                        if (ndw > MAX_READ_REP_NDW)
                        {
                            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                "ndw is out of range %d\n", ndw);
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                        addr = (*(fm_uint32*)(imsg->data+ndwOff)) >> 4;
                        ndwOff = valOff + ndw*4; /* Next ndwOff */
                    }
                } while (((valOff + ndw*4) <= iDataLen) &&
                         rsp == IOSF_RSP_SUCCESS);
                break;
            default:
                rsp = IOSF_RSP_FAIL;
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                            "Invalid IOSF opcode 0x%x",
                            opcode);
                break;
        }
    }

    emsg.type = htons(FM_MODEL_MSG_IOSF);
	emsg.version = htons(FM_MODEL_MSG_VERSION);
    msgLength = eDataLen + FM_MODEL_MSG_HEADER_SIZE;
    emsg.msgLength = htonl(msgLength);

    emsg.data[DEST_OFF] = source;
    emsg.data[SOURCE_OFF] = STRAP_IOSFSB_ENDPT_ID;

    /* EH=1, resp, tag */
    emsg.data[TAG_OFF] = (1 << 7) | (rsp << 3) | tag;
    if (saiHdr)
    {
        *(fm_uint16*)(emsg.data + SAI_OFF) = STRAP_ENDPT_SAI;
    }

    if (debug >=3)
    {
        FM_LOG_PRINT("Reply: rsp=%d. posted port=%d fd=%d\n",
                     rsp, port, socket->sock);
        HexDump((fm_byte*)&emsg, msgLength);
    }

    if (port == NONPOSTED_PORT)
    {
        SendMessage(socket, &emsg, msgLength);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgIosf */


static void DecodeTlvPacket(fm_byte              *data,
                            fm_int32              dataLength,
                            fm_byte **            packet,
                            fm_int32 *            pktLength,
                            fm_modelSidebandData *sbData)
{
    fm_int32  length;
    fm_int32  p;
    fm_byte   type;

    p = 0;

    while (p < dataLength)
    {
        type = data[p];
        p += FM_MODEL_DATA_TYPE_SIZE;
        length = ntohl(*( (fm_int32 *) &data[p] ));
        p += FM_MODEL_DATA_LENGTH_SIZE;
        switch (type)
        {
            case FM_MODEL_DATA_PACKET:
                *packet = &data[p];
                *pktLength = length;
                break;

            case FM_MODEL_DATA_SB_ID:
                sbData->idTag = ntohl(*( (fm_uint32 *) &data[p] ));
                break;

            case FM_MODEL_DATA_SB_TC:
                sbData->tc = data[p];
                break;

            case FM_MODEL_PACKET_META:
                FM_MEMCPY_S(sbData->pktMeta,
                     sizeof(sbData->pktMeta),
                     &data[p],
                     length);
                break;
            default:
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Uhandled Packet TLV type %d\n", type);
                break;
        }
        p += length;

    }   /* end while (p < dataLength) */

}   /* end DecodeTlvPacket */


static void DecodePacket(fm_modelMessage *     imsg,
                         fm_int32              msgLength,
                         fm_byte **            packet,
                         fm_int32 *            pktLength,
                         fm_modelSidebandData *sbData)
{
    fm_int32  dataLength;

    dataLength = msgLength - FM_MODEL_MSG_HEADER_SIZE;

    DecodeTlvPacket(imsg->data, dataLength, packet, pktLength, sbData);

}   /* end DecodePacket */


static void FinalizePacket(fm_modelMessage *     emsg,
                           fm_int32              pktLength,
                           fm_modelSidebandData *sbData,
                           fm_int32 *            msgLength)
{
    fm_int32 p;

    p = ( (fm_int32) FM_MODEL_MSG_TLV_SIZE ) + pktLength;

    emsg->data[p] = FM_MODEL_DATA_SB_ID;
    p += FM_MODEL_DATA_TYPE_SIZE;
    *( (fm_int32 *) &emsg->data[p] ) = htonl(sizeof(sbData->idTag));
    p += FM_MODEL_DATA_LENGTH_SIZE;
    *( (fm_uint32 *) &emsg->data[p] ) = htonl(sbData->idTag);
    p += sizeof(sbData->idTag);

    emsg->data[p] = FM_MODEL_DATA_SB_TC;
    p += FM_MODEL_DATA_TYPE_SIZE;
    *( (fm_int32 *) &emsg->data[p] ) = htonl(sizeof(sbData->tc));
    p += FM_MODEL_DATA_LENGTH_SIZE;
    emsg->data[p] = sbData->tc;
    p += sizeof(sbData->tc);

    emsg->data[p] = FM_MODEL_PACKET_META;
    p += FM_MODEL_DATA_TYPE_SIZE;
    *( (fm_int32 *) &emsg->data[p] ) = htonl(sizeof(sbData->pktMeta));
    p += FM_MODEL_DATA_LENGTH_SIZE;
    FM_MEMCPY_S(&emsg->data[p],
                sizeof(sbData->pktMeta),
                sbData->pktMeta,
                sizeof(sbData->pktMeta));
    p += sizeof(sbData->pktMeta);

    *msgLength += p;

}   /* end FinalizePacket */



/*****************************************************************************/
/** HandleMsgPacket
 * \ingroup intModelPktQ
 *
 * \desc            Processes packet and loopback packet messages and creates
 *                  and enqueues egress packet message as necessary.
 *                                                                      \lb\lb
 *                  The packet and loopback packet message format is:
 *                                                                      \lb\lb
 *                  +--------------+--------------------+-------------+
 *                  | header (12B) | sideband data (4B) | packet data |
 *                  +--------------+--------------------+-------------+
 *                                                                      \lb\lb
 *                  The length of the packet data field is variable, but can't
 *                  exceed ''FM_MODEL_MAX_PACKET_SIZE''.
 *
 * \param[in]       sw is the switch to operate on.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       physPort is the logical port on which the packet ingresses
 *                  the model.
 *
 * \param[in]       imsgLength is the length of the received message in units of
 *                  bytes.
 *
 * \param[out]      emsg points to the send message that is to be processed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status HandleMsgPacket(fm_int               sw,
                          fm_modelMessage *    imsg,
                          fm_int               physPort,
                          fm_int32             imsgLength)
{
    fm_modelSidebandData sbData;
    fm_status            status;
    fm_int32             emsgLength;
    fm_int32             pktLength;
    fm_uint16            numPkts;
    fm_byte *            data;
    fm_byte *            packet;
    fm_modelMessage     *emsg;
    fm_modelMessage      msg;
#define MAX_PKT_LB      24
    // fm_pkt_loopback      pktLoopback[MAX_PKT_LB];
    static fm_int        idCounter = 0;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "sw=%d, imsg=%p, physPort=%d, imsgLength=%d\n",
                  sw,
                  (void *) imsg,
                  physPort,
                  imsgLength );

#if 0 && MBY_LINK_STATE  // TODO
    if (portLinkState[physPort] != PORT_LINK_UP)
    {
        FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
                     "TB: Link down %d. Dropping RX packet on phys port %d.\n",
                     portLinkState[physPort], physPort);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }
#endif

    packet = NULL;
    pktLength = 0;
    FM_CLEAR(sbData);

    DecodePacket(imsg, imsgLength, &packet, &pktLength, &sbData);

    if ( ( packet == NULL ) || ( pktLength == 0 ) )
    {
        /* This packet message is too short. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    sbData.idTag = idCounter;
    idCounter++;
    status = mbySendPacket(sw,
                           physPort,
                           packet,
                           pktLength);
                           // ,&sbData);
	status = FM_OK;
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    numPkts = 0;
    emsgLength = 0;
    FM_CLEAR(msg);
    emsg = &msg;

    while (status == FM_OK)
    {
        data = emsg->data;

        /***************************************************
         * Receive the packet(s) egressing from the model.
         **************************************************/
        *data = FM_MODEL_DATA_PACKET;
        packet = &data[FM_MODEL_MSG_TLV_SIZE];
        FM_CLEAR(sbData);

		// TODO
        // status = fmModelReceivePacket(sw,
        //                               &physPort,
        //                               packet,
        //                               &pktLength,
        //                               FM_MODEL_MAX_PACKET_SIZE,
        //                               &sbData);

#if 0 // TODO
	if (portLinkState[physPort] != PORT_LINK_UP)
	{
		FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
			    "TB: Link down %d. Dropping TX packet on phys port %d.\n",
			    portLinkState[physPort], physPort);
		continue;
	}
#endif

        *( (fm_int32 *) &data[FM_MODEL_DATA_TYPE_SIZE] ) = htonl(pktLength);
        /* status value of FM_ERR_SPARE1 is used to indicate that
         * loopback is requested for the current packet */

        /* Check for valid packet. */
        if ( ( ( status == FM_OK ) &&
               ( pktLength >= 0 ) &&
               ( pktLength < FM_MODEL_MAX_PACKET_SIZE ) ) ||
             ( ( status == FM_ERR_NO_MORE ) ) )
        {
            /* Finalize and enqueue the packet. */
            emsgLength = FM_MODEL_MSG_HEADER_SIZE;
            if (status == FM_ERR_NO_MORE)
            {
                emsg->type                        = FM_MODEL_MSG_PACKET_EOT;
                emsgLength                        += 2;
                *( (fm_uint16 *) &emsg->data[0] ) = htons(numPkts);
//FIXME
                continue;
            }
            else
            {
                emsg->type = FM_MODEL_MSG_PACKET;
                numPkts    += 1;

                /* Copy the sideband data into the egress packet. */
                FinalizePacket(emsg, pktLength, &sbData, &emsgLength);
            }

            emsg->msgLength = htonl(emsgLength);
            emsg->version   = htons(FM_MODEL_MSG_VERSION);
            emsg->sw        = htons(sw);
            emsg->port      = htons(physPort);
            emsg->type      = htons(emsg->type);

            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: Egress packet #%d on physPort %d, length %d\n",
                         numPkts,
                         physPort,
                         emsgLength);

            if (debug >= 2)
            {
                printf("Packet sent back to port %d\n", physPort);
                HexDump(emsg->data, ntohl(emsg->msgLength));
            }
            // TODO  if (pktRecvSockets[physPort].sock  < 0)
            // {
            //     printf("Dropping packet egressing port %d\n", physPort);
            // }
            else
            {
                // TODO SendMessage(&pktRecvSockets[physPort] , emsg,
                //              ntohl(emsg->msgLength));
            }
        }
        else
        {
            /* No packet received or a processing error has occurred... */
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: No egress packet, ingress length %d, "
                         "status %d (%s)\n",
                         pktLength,
                         status,
                         fmErrorMsg(status));

            /* Packets may still be queued */
            if (status == FM_ERR_MCAST_INVALID_STATE)
            {
                status = FM_OK;
            }
        }

    }   /* end while (status == FM_OK) */

    if (status == FM_ERR_NO_MORE)
    {
        status = FM_OK;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgPacket */



/*****************************************************************************/
/** HandleMsgSetEgressInfo
 * \ingroup intModelPktQ
 *
 * \desc            Processes egress information messages.
 *                                                                      \lb\lb
 *                  The egress information message format is:
 *                                                                      \lb\lb
 *                  +--------------+---------------+-----------------+
 *                  | header (12B) | TCP port (2B) | hostname (510B) |
 *                  +--------------+---------------+-----------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       imsg points to the message received.
 *
 * \param[in]       type is the message type.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 *
 *****************************************************************************/
fm_status HandleMsgSetEgressInfo(fm_int               sw,
                                 fm_modelMessage *    imsg,
                                 fm_int               type)
{
    fm_status           status = FM_OK;
    fm_uint16           port;
    fm_uint16           networkPort;
    fm_text             host;

    FM_NOT_USED(sw);
    FM_NOT_USED(type);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d, imsg=%p type=%d\n",
                 sw,
                 (void *) imsg,
                 type);

    port        = ntohs(imsg->port);
    if (port >= MAX_PHYS_PORT)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "Egress port %d is out of range\n",
                     port);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    networkPort = ntohs( *( (fm_uint16 *) imsg->data ) );
    host        = (fm_text) &imsg->data[2];

    if (networkPort == FM_MODEL_SOCKET_PORT_DISABLE)
    {
        /**************************************************
         * Shut down the socket on this model egress port.
         **************************************************/
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Disabling egress port %d\n", port);


        /* If there was a socket on this port previously, close it down. */
        if (pktRecvSockets[port].sock != -1)
        {
            fmCloseNetworkConnection(&pktRecvSockets[port]);
        }

        pktRecvSockets[port].sock = -1;
    }
    else
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Setting destination for egress port %d to %s:%d\n",
                     port, host, networkPort);

        if (pktRecvSockets[port].sock >= 0)
        {
            FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                    "Packet receive socket for port %d is active. Close existing one %d.\n",
                    port, pktRecvSockets[port].sock);
            fmCloseNetworkConnection(&pktRecvSockets[port]);
            pktRecvSockets[port].sock = -1;
        }

        fmCreateNetworkClient(&pktRecvSockets[port],
                              FM_SOCKET_TYPE_TCP,
                              host,
                              networkPort);
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Packet connection for port %d created. sock = %d\n",
                     port, pktRecvSockets[port].sock);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgSetEgressInfo */


/*****************************************************************************/
/** ProcessMessage
 * \ingroup intModel
 *
 * \desc            Processes a received message.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       imsg points to the message to be processed.
 *
 * \param[in]       msgLength is the received message length in units of bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if the switch number stored in the
 *                  message is invalid.
 * \return          FM_ERR_INVALID_PORT if the port number stored in the
 *                  message is invalid.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status ProcessMessage(fm_socket *socket, fm_modelMessage *imsg,
                         fm_int32 msgLength)
{
    fm_status            status = FM_OK;
    fm_int               port;
    fm_int               sw;
    fm_int               type;
#ifdef DEBUG_PRINT_RECEIVED_PACKET
    fm_int32             pktLength;
    fm_int               p;
#endif

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "imsg=%p, msgLength=%d\n",
                  (void *) imsg,
                  msgLength );


    if ( msgLength <= ( (fm_int32) FM_MODEL_MSG_HEADER_SIZE ) )
    {
        /* Silent exit */
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

    /* Retrieve switch and port */
    type = ntohs(imsg->type);
    sw   = ntohs(imsg->sw);
    port = ntohs(imsg->port);
    if (msgLength > FM_MODEL_MAX_PACKET_SIZE)
    {
        FM_LOG_PRINT("TB: Length to large: sw=%d port=%d type=%d pktLength=%d\n",
                     sw, port, type, msgLength);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

#ifdef DEBUG_PRINT_RECEIVED_PACKET
    pktLength = msgLength - FM_MODEL_MSG_HEADER_SIZE;

    FM_LOG_PRINT("TB: Received packet datagram: sw=%d port=%d sock=%d type=%d pktLength=%d\n",
                 sw, port, socket->sock, type, pktLength);

    for (p = 0 ; p < pktLength ; p++)
    {
        FM_LOG_PRINT("%02x ", imsg->data[p]);

        if ( p && ( (p % 16) == 15 ) )
        {
            FM_LOG_PRINT("\n");
        }
    }

    FM_LOG_PRINT("\n");

#endif

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
        "TB: process message type=%d\n", type);

    /* Switch according to packet type received */
    switch (type)
    {
        case FM_MODEL_MSG_LINK_STATE:
            status = HandleMsgLinkState(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_SET_EGRESS_INFO:
        case FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH:
            status = HandleMsgSetEgressInfo(sw, imsg, type);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_PACKET_LOOPBACK:
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                "TB: process message type=FM_MODEL_MSG_PACKET_LOOPBACK\n");
            /* FALLTHRU */
        case FM_MODEL_MSG_PACKET:
            // TODO msg_stat.packet++;
            status = HandleMsgPacket(sw, imsg, port, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_IOSF:
            // TODO msg_stat.iosf++;
            status = HandleMsgIosf(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_VERSION_INFO:
            status = HandleMsgVersionInfo(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        default:
            FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                           "Ignoring message with unknown type %d\n",
                           type);

    }   /* end switch (type) */

ABORT:
    if (status != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                       "Unable to handle message: %s\n",
                       fmErrorMsg(status) );
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end ProcessMessage */
