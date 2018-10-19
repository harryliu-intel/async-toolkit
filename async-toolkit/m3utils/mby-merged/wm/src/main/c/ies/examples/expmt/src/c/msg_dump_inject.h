#ifndef MSG_DUMP_INJECT_H
#define MSG_DUMP_INJECT_H

#include <platforms/common/model/fm_model_message.h>

typedef enum { INGRESS = 0, EGRESS = 1 } msg_direction_t;

fm_status openMsgDump(const msg_direction_t msg_dir, const char *msg_file);
fm_status openMsgInject(const msg_direction_t msg_dir, const char *msg_file);
fm_status writeMsgDump(const msg_direction_t msg_dir, fm_modelMessage *msg);
fm_status readMsgInject(const msg_direction_t msg_dir, fm_modelMessage *msg);
fm_status closeMsgDump(const msg_direction_t msg_dir);
fm_status closeMsgInject(const msg_direction_t msg_dir);

fm_status checkMsgInject(fm_modelMessage *msg_ref, fm_modelMessage *msg);
static void getMsgType(const fm_modelMsgType type, char **name);
void echoMsg(const char* label, fm_modelMessage *msg);

#endif
