#include <stdint.h>
#include <stdio.h>

#include <mby_common.h>
#include <mby_errors.h>
#include <mby_model.h>
#include <mby_init.h>
#include <mby_rxstats.h>

#include "mby_basic_fwd_init.h"

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
const int send_port = 2;

inline static int checkOk (const char * test, const fm_status status)
{
    int rv = (status == FM_OK) ? 0 : -1;

    if (rv == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[fail]");

    printf(COLOR_RESET " %s\n", test);

    return rv;
}

fm_status init(fm_uint32 fwd_port, fm_macaddr dmac)
{
    // TODO verify if that's all that we need
    mby_init_common_regs();
    basic_fwd_init(fwd_port, dmac);
    return FM_OK;
}


fm_status check
(
    fm_uint32 sw,
    fm_uint32 rx_port,
    fm_uint32 tx_port,
    fm_uint32 exp_port,
    fm_uint32 recv_packet_len,
    fm_uint32 exp_packet_len
)
{
    fm_uint32 bank  = 2;
    fm_uint16 index = (rx_port * 16) + STAT_FIDForwarded;
    fm_uint64 val;

    if ((tx_port != exp_port) || (recv_packet_len != exp_packet_len)) {
        printf("TX packet is invalid\n");
        return FM_FAIL;
    }

    // Check RX counters
    mbyReadReg(sw, MBY_RX_STATS_BANK_FRAME(bank, index, 0), &val);
    if (val != 1) {
        printf("Frame counter is invalid\n");
        return FM_FAIL;
    }

    mbyReadReg(sw, MBY_RX_STATS_BANK_BYTE(bank, index, 0), &val);
    if (val != exp_packet_len) {
        printf("Byte counter is invalid\n");
        return FM_FAIL;
    }

    return FM_OK;
}

int main()
{
    int sw = 0;

    const uint32_t recv_pkt_buf_len = 1024;
    unsigned char recv_packet_buf[recv_pkt_buf_len];
    fm_uint32 exp_port = 5;
    fm_uint32 recv_port;
    fm_uint32 exp_packet_len = 1024;
    fm_uint32 recv_packet_len;
    fm_macaddr dmac = 0x000102030405;
    fm_status status = FM_OK;

    printf("--------------------------------------------------------------------------------\n");

    int rv = 0;

    for (fm_uint i = 0; i < 1; i++) {

        status = mbyResetModel(send_sw);
        rv = checkOk("mbyResetModel", status);
        if (rv != 0)
            break;

        status = init(exp_port, dmac);
        rv = checkOk("init", status);
        if (rv != 0)
            break;

        status = mbySendPacket(send_sw, send_port, sent_packet, sizeof(sent_packet));
        rv = checkOk("mbySendPacket", status);
        if (rv != 0)
            break;

        status = mbyReceivePacket(send_sw, &recv_port, recv_packet_buf, &recv_packet_len, recv_pkt_buf_len);
        checkOk("mbyReceivePacket", status);
        if (rv != 0)
            break;

        status = check(send_sw, send_port, recv_port, exp_port, recv_packet_len, exp_packet_len);
        checkOk("check", status);
        if (rv != 0)
            break;
    }

    printf("--------------------------------------------------------------------------------\n");

    if (rv == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[fail]");

    printf(" Basic forwarding tests\n"  COLOR_RESET);

    printf("--------------------------------------------------------------------------------\n");

    return rv;
}
