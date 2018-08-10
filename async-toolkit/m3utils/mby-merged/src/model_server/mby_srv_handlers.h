
#ifndef _MBY_SRV_HANDLERS_H_
#define _MBY_SRV_HANDLERS_H_

#include "mby_model.h"
#include "mby_srv_socket.h"
#include "mby_srv_message.h"

// TODO find a better place for these global variables
#define MAX_PHYS_PORT                       32

fm_socket pktRecvSockets[MAX_PHYS_PORT];
fm_msg_stats msg_stat;
// TODO static fm_int portLinkState[MAX_PHYS_PORT];

/*
fm_status HandleMsgLinkState(fm_int               sw,
                             fm_socket           *socket,
                             fm_modelMessage *    imsg,
                             fm_int32             msgLength);

fm_status HandleMsgVersionInfo(fm_int               sw,
                               fm_socket           *socket,
                               fm_modelMessage *    imsg,
                               fm_int32             msgLength);
fm_status HandleMsgIosf(fm_int               sw,
                        fm_socket           *socket,
                        fm_modelMessage *    imsg,
                        fm_int32             imsgLength);

fm_status HandleMsgPacket(fm_int               sw,
                          fm_modelMessage *    imsg,
                          fm_int               physPort,
                          fm_int32             imsgLength);

fm_status HandleMsgSetEgressInfo(fm_int               sw,
                                 fm_modelMessage *    imsg,
                                 fm_int               type);
*/

fm_status ProcessMessage(fm_socket *socket, fm_modelMessage *imsg,
                         fm_int32 msgLength);


#endif /* _MBY_SRV_HANDLERS_H_ */
