
#include "mby_srv_message.h"
#include "mby_srv_log.h"
#include "mby_srv_handlers.h"

fm_status SendMessage(fm_socket *      socket,
                      fm_modelMessage *emsg,
                      fm_int32         msgLength)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "socket=%p emsg=%p msgLength=%d\n",
                 (void *) socket,
                 (void *) emsg,
                 msgLength);

    status = fmSendNetworkData(socket, emsg, msgLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end SendMessage */


fm_status ReceiveMessage(fm_socket *      socket,
                         fm_bool          closeSocket,
                         fm_modelMessage *imsg)
{
    fm_status status;
    fm_int32  msgLength;
    fm_int    length;
    fm_int    bufferSize;
    fm_int    offset;
    fm_int    recvLength;
    fm_uint16 version;
    fm_byte * data;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "socket=%p imsg=%p\n",
                 (void *) socket,
                 (void *) imsg);

    status = fmReceiveNetworkData(socket,
                                  &imsg->msgLength,
                                  FM_MODEL_MSG_LENGTH_SIZE,
                                  &recvLength);
    if ( closeSocket && ( status == FM_ERR_NO_MORE ) )
    {
        /* The connection has been closed. */
        printf("Connection (fd=%d) is closed\n", socket->sock);
        socket->type = FM_SOCKET_TYPE_CLOSED;
    }
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) FM_MODEL_MSG_LENGTH_SIZE ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %d bytes, received "
                     "%d bytes. fd %d, type %d\n",
                     (fm_int) FM_MODEL_MSG_LENGTH_SIZE,
                     recvLength, socket->sock, socket->type);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    msgLength = ntohl(imsg->msgLength);
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "Received message length of %d bytes\n",
                 msgLength);

    status = fmReceiveNetworkData(socket,
                                  &imsg->version,
                                  FM_MODEL_MSG_VERSION_SIZE,
                                  &recvLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) FM_MODEL_MSG_VERSION_SIZE ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %d bytes, received %d bytes\n",
                     (fm_int) FM_MODEL_MSG_VERSION_SIZE,
                     recvLength);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    version = ntohs(imsg->version);
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "Received message version of %d vs %d\n",
                 version, FM_MODEL_MSG_VERSION);

    if ( version != ( (fm_uint16) FM_MODEL_MSG_VERSION ) )
    {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                       "%hu: Unsupported message version; Ignoring message\n",
                       version);

        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    offset = ( (fm_int) FM_MODEL_MSG_LENGTH_SIZE ) +
             ( (fm_int) FM_MODEL_MSG_VERSION_SIZE );
    data   = ( (fm_byte *) imsg ) + offset;
    length = msgLength - offset;
    bufferSize = FM_MODEL_MSG_SIZE -
                 (FM_MODEL_MSG_LENGTH_SIZE + FM_MODEL_MSG_VERSION_SIZE);
    if (length > bufferSize) {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                "Message length (%d) > buffer size (%d). Data will be lost to avoid overflow\n",
                length, bufferSize);
        length = bufferSize;
    }

    status = fmReceiveNetworkData(socket, data, length, &recvLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) length ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %d bytes, received %d bytes\n",
                     length,
                     recvLength);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end ReceiveMessage */



