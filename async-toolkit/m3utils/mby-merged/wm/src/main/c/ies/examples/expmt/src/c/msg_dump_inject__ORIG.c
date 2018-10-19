#include <stdio.h>
#include <errno.h>
#include <arpa/inet.h> // ntohl()
#include <fm_sdk.h>
#include <platforms/common/model/fm_model_message.h>

fm_status openMsgDump(const char *msg_file, FILE **msg_fp)
{
    fm_status status = FM_OK;
    
    *msg_fp = fopen(msg_file, "w");

    if (!(*msg_fp))
    {
        printf("Unable to open message dump file '%s' for writing - '%d'\n",
               msg_file, errno);

        status = FM_ERR_INVALID_ARGUMENT;
    }

    return status;
}

fm_status openMsgInject(const char *msg_file, FILE **msg_fp)
{
    fm_status status = FM_OK;
    
    *msg_fp = fopen(msg_file, "r");

    if (!(*msg_fp))
    {
        printf("Unable to open message dump file '%s' for reading - '%d'\n",
               msg_file, errno);

        status = FM_ERR_INVALID_ARGUMENT;
    }

    return status;
}

fm_status writeMsgDump(FILE *msg_fp, fm_modelMessage *msg)
{
    void *buffer = (void *) msg;
    size_t size = ntohl(msg->msgLength);
    size_t count = 1; // one struct at a time

    size_t items = 0uLL;
    if ((msg_fp != NULL) && (size <= sizeof(fm_modelMessage)))
        items = fwrite(buffer, size, count, msg_fp);

    // printf ("writeMsgDump:  size = %9zu, items = %zu\n", size, items);

    fm_status status = (items == count) ? FM_OK : FM_FAIL;

    return status;
}

fm_status readMsgInject(FILE *msg_fp, fm_modelMessage *msg)
{
    void *buffer = (void *) msg;
    size_t size  = sizeof(fm_int32);
    size_t count = 1;

    size_t items = 0uLL;
    if (msg_fp != NULL)
        items = fread(buffer, size, count, msg_fp);

    fm_status status = (items == count) ? FM_OK : FM_FAIL;
    
    // convert network (big-endian) to host (little-endian on x86 hosts):
    size_t msgLength = ntohl(*((fm_int32 *) buffer));

    buffer = (void *) (((fm_byte *) msg) + sizeof(fm_int32)); // skip over msgLength
    size = (msgLength > sizeof(fm_int32)) ? msgLength - sizeof(fm_int32) : 0;

    if ((status == FM_OK) && (msg_fp != NULL) && (size <= sizeof(fm_modelMessage)))
        items = fread(buffer, size, count, msg_fp);

    status = (items == count) ? FM_OK : FM_FAIL;

    return status;
}

fm_status checkMsgInject(fm_modelMessage *msg_ref, fm_modelMessage *msg)
{
    fm_bool is_eq =
        (msg->msgLength == msg_ref->msgLength) &&
        (msg->version   == msg_ref->version) &&
        (msg->type      == msg_ref->type) &&
        (msg->sw        == msg_ref->sw) &&
        (msg->port      == msg_ref->port);

    if (is_eq == TRUE) // compare data only if headers match
    {
        const size_t hdrLength = FM_MODEL_MSG_HEADER_SIZE;
        const size_t msgLength = ntohl(msg->msgLength);
        const size_t pktLength = (msgLength > hdrLength) ? msgLength - hdrLength : 0;
    
        for (size_t i = 0; i < pktLength; i++)
            is_eq = is_eq && (msg->data[i] == msg_ref->data[i]);
    }

    fm_status status = (is_eq) ? FM_OK : FM_FAIL;

    return status;
}

fm_status closeMsgDump(FILE *msg_fp)
{
    fm_status status = (msg_fp == NULL) ? FM_FAIL : FM_OK;
    
    if (msg_fp != NULL)
        fclose(msg_fp);

    return status;
}

fm_status closeMsgInject(FILE *msg_fp)
{
    fm_status status = (msg_fp == NULL) ? FM_FAIL : FM_OK;
    
    if (msg_fp != NULL)
        fclose(msg_fp);

    return status;
}

static void getMsgType(const fm_modelMsgType type, char **name)
{
    // Get message type name:

    switch (type)
    {
    case FM_MODEL_MSG_PACKET:                       *name = "FM_MODEL_MSG_PACKET";                       break;
    case FM_MODEL_MSG_LINK_STATE:                   *name = "FM_MODEL_MSG_LINK_STATE";                   break;
    case FM_MODEL_MSG_SWITCH_STATE:                 *name = "FM_MODEL_MSG_SWITCH_STATE";                 break;
    case FM_MODEL_MSG_SET_EGRESS_INFO:              *name = "FM_MODEL_MSG_SET_EGRESS_INFO";              break;
    case FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH: *name = "FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH"; break;
    case FM_MODEL_MSG_PACKET_LOOPBACK:              *name = "FM_MODEL_MSG_PACKET_LOOPBACK";              break;
    case FM_MODEL_MSG_PACKET_EOT:                   *name = "FM_MODEL_MSG_PACKET_EOT";                   break;
    case FM_MODEL_MSG_MGMT:                         *name = "FM_MODEL_MSG_MGMT";                         break;
    case FM_MODEL_MSG_ATTR:                         *name = "FM_MODEL_MSG_ATTR";                         break;
    case FM_MODEL_MSG_GET_INFO:                     *name = "FM_MODEL_MSG_GET_INFO";                     break;
    case FM_MODEL_MSG_ERROR:                        *name = "FM_MODEL_MSG_ERROR";                        break;
    case FM_MODEL_MSG_IOSF:                         *name = "FM_MODEL_MSG_IOSF";                         break;
    case FM_MODEL_MSG_CTRL:                         *name = "FM_MODEL_MSG_CTRL";                         break;
    case FM_MODEL_MSG_VERSION_INFO:                 *name = "FM_MODEL_MSG_VERSION_INFO";                 break;
    case FM_MODEL_MSG_NVM_READ:                     *name = "FM_MODEL_MSG_NVM_READ";                     break;
    default:                                        *name = "FM_MODEL_MSG_UNKNOWN";                      break;
    }
}
            
void echoMsg(const char* label, fm_modelMessage *msg)
{
    char *msg_type_name = NULL;
    getMsgType (ntohs(msg->type), &msg_type_name);
    
    printf("Message: %s\n", label);
    printf("    msgLength = %d\n", ntohl(msg->msgLength));
    printf("      version = %d\n", ntohs(msg->version));
    printf("         type = %d (%s)\n", ntohs(msg->type), msg_type_name);
    printf("           sw = %d\n", ntohs(msg->sw));
    printf("         port = %d\n", ntohs(msg->port));
    printf("         data = ");
    
    const size_t hdrLength = FM_MODEL_MSG_HEADER_SIZE;
    const size_t msgLength = ntohl(msg->msgLength);
    const size_t pktLength = (msgLength > hdrLength) ? msgLength - hdrLength : 0;
    
    for (size_t i = 0; i < pktLength; i++)
        printf("%02x ", msg->data[i]);
    printf("\n");    
}
