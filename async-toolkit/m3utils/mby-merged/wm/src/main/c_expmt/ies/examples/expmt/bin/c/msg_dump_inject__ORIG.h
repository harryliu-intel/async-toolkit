#ifndef MSG_DUMP_INJECT_H
#define MSG_DUMP_INJECT_H

#include <stdio.h>
#include <platforms/common/model/fm_model_message.h>

fm_status openMsgDump(const char *msg_file, FILE **msg_fp);
fm_status writeMsgDump(FILE *msg_fp, fm_modelMessage *msg);
fm_status closeMsgDump(FILE *msg_fp);

fm_status openMsgInject(const char *msg_file, FILE **msg_fp);
fm_status readMsgInject(FILE *msg_fp, fm_modelMessage *msg);
fm_status checkMsgInject(fm_modelMessage *msg_ref, fm_modelMessage *msg);
fm_status closeMsgInject(FILE *msg_fp);

void echoMsg(const char* label, fm_modelMessage *msg);

#endif
