#include <mby_model.h>
#include <stdint.h>
#include <stdio.h>

#include "borrowed/mby_srv_nvmimg.h"
#include "mby_basic_fwd_init.h"
#include <mby_init.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

// TODO make this const once model allows... or add memcpy if it won't
/*const*/unsigned char sent_packet[1024] =  {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0xcc, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
                0x08, 0x00, 0x45, 0x00, 0x00, 0x3e, 0x0c, 0x65, 0x40, 0x00, 0x40, 0x06,
                0x17, 0x50, 0x0a, 0x01, 0x01, 0x01, 0x0a, 0x02, 0x02, 0x02, 0xe4, 0x52,
                0x30, 0x39, 0xd3, 0x4d, 0xb1, 0x36, 0xfe, 0xa2, 0xa9, 0xfb, 0x80, 0x18,
                0x00, 0xe5, 0x17, 0x36, 0x00, 0x00, 0x01, 0x01, 0x08, 0x0a, 0x00, 0x67,
                0xc1, 0x0e, 0x00, 0x67, 0xbd, 0x56, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x20,
                0x4d, 0x42, 0x59, 0x0a};

const int send_sw = 0;
const int send_port = 0;

#define BASIC_FWD_FAIL_ON_NON_OK(test) \
                if (status != FM_OK) { \
                    printf("FAIL of mby_basic_fwd @ %s\n", test); \
                    printf(COLOR_RED   "[FAIL]"); \
                    printf("--------------------------------------------------------------------------------\n"); \
                    return -1; \
                }

fm_status init()
{
    // TODO verify if that's all that we need
    loadNvmImg("nvm.img");
    init_common();
    basic_fwd_init();
    return FM_OK;
}

fm_status check()
{
    // TODO write
    return FM_OK;
}

int main()
{
    int sw = 0;
    fm_uint32 recv_port;
    const uint32_t recv_pkt_buf_len = 1024;
    unsigned char recv_packet_buf[recv_pkt_buf_len];
    fm_uint32 recv_packet_len;

    fm_status status;

    printf("--------------------------------------------------------------------------------\n");
    printf("mby_basic_fwd test\n");

    status = mbyResetModel(send_sw);
    BASIC_FWD_FAIL_ON_NON_OK("mbyResetModel");

    status = init();
    BASIC_FWD_FAIL_ON_NON_OK("init");

    status = mbySendPacket(send_sw, send_port, sent_packet, sizeof(sent_packet));
    BASIC_FWD_FAIL_ON_NON_OK("mbySendPacket");

    status = mbyReceivePacket(send_sw, &recv_port, recv_packet_buf, &recv_packet_len, recv_pkt_buf_len);
    BASIC_FWD_FAIL_ON_NON_OK("mbyReceivePacket");

    status = check();
    BASIC_FWD_FAIL_ON_NON_OK("check");

    printf(COLOR_GREEN "[pass]");
    printf("--------------------------------------------------------------------------------\n");
    return 0;
}
