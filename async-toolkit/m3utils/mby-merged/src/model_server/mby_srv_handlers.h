
#ifndef _MBY_SRV_HANDLERS_H_
#define _MBY_SRV_HANDLERS_H_

#include "mby_model.h"
#include "mby_srv_socket.h"
#include "mby_srv_message.h"

#define MAX_PHYS_PORT                       32

fm_status ProcessMessage(fm_socket *socket, fm_modelMessage *imsg,
                         fm_int32 msgLength);


#endif /* _MBY_SRV_HANDLERS_H_ */
