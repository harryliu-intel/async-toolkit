#include <stdio.h>
#include <errno.h>
#include <fm_sdk.h>
#include <platforms/common/model/fm_model_message.h>

#include <msg_dump_inject.h>

int main(int argc, char *argv[])
{
    const char* imsg_file = "foo.imsg";

    FILE *imsg_fp = NULL;

    fm_status sts = FM_OK;

    sts = openMsgDump(imsg_file, &imsg_fp);
        
    const int M = FM_MODEL_MAX_TLV_SIZE + FM_MODEL_MAX_PACKET_SIZE;
    const int N = 10;
    
    fm_modelMessage imsg;

    imsg.msgLength = N + 13; // bytes
    imsg.version   = 2018;
    imsg.type      = 2;
    imsg.sw        = 1;
    imsg.port      = 22;

    for (int i = 0; i < M; i++)
        imsg.data[i] = (fm_byte)((i < N) ? (i + 1) : 0);

    imsg.__size    = N + 13;
    
    sts = writeMsgDump(imsg_fp, &imsg);

    sts = closeMsgDump(imsg_fp);

    // ----------------------------------------

    imsg_fp = NULL;
    
    sts = openMsgInject(imsg_file, &imsg_fp);
    
    fm_modelMessage imsg_ref;

    sts = readMsgInject(imsg_fp, &imsg_ref);
    
    sts = checkMsgInject(&imsg_ref, &imsg);

    sts = closeMsgInject(imsg_fp);
    
    if (sts == FM_OK)
        printf("Test passed.\n");
    else
        printf("Test FAILED!\n");

    int ret = (sts == FM_OK) ? 0 : -1;
    
    return ret;
}
