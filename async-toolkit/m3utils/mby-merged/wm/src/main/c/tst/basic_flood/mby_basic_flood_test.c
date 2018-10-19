#include <stdint.h>
#include <stdio.h>

#include <mby_common.h>
#include <mby_errors.h>
#include <mby_model.h>
#include <mby_rxstats.h>
#include <mby_init.h>
#include <mby_parser.h>
#include <mby_pipeline.h>

#include "mby_basic_flood_init.h"

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

static mby_top_map top_map;

static mbyRxStatsToRxOut rxs2rxo;

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

fm_status init()
{
    // TODO verify if that's all that we need
    mby_init_common_regs(&(top_map.mpp.mgp[0].rx_ppe));
    basic_flood_init(&(top_map.mpp.mgp[0].rx_ppe));

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
    fm_uint16 index = (rx_port * 16) + STAT_FloodForwarded;
    fm_uint64 val;

    if ((tx_port != exp_port) || (recv_packet_len != exp_packet_len)) {
        printf("TX packet is invalid\n");
        return FM_FAIL;
    }

    // Check RX counters
    rx_stats_bank_frame_r * const bank_frame = &(top_map.mpp.mgp[0].rx_ppe.stats.RX_STATS_BANK_FRAME[bank][index]);

    if (bank_frame->FRAME_COUNTER != 1) {
        printf("Frame counter is invalid\n");
        return FM_FAIL;
    }

    rx_stats_bank_byte_r * const bank_byte = &(top_map.mpp.mgp[0].rx_ppe.stats.RX_STATS_BANK_BYTE[bank][index]);

    if (bank_byte->BYTE_COUNTER != exp_packet_len) {
        printf("Byte counter is invalid\n");
        return FM_FAIL;
    }

    return FM_OK;
}

static fm_status sendPacket
(
    const fm_uint32         sw,
    const fm_uint32         port,
    const fm_byte   * const packet,
    const fm_uint32         length
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
        // Top CSR map for tile 0 receive pipeline:
        // TODO use the pipeline associated to the specific ingress port

        mby_ppe_rx_top_map * const rx_top_map = &(top_map.mpp.mgp[0].rx_ppe);
        mby_shm_map        * const shm_map    = &(top_map.mpp.shm);

        // Input struct:
        mbyRxMacToParser mac2par;

        // Populate input:
        mac2par.RX_DATA   = (fm_byte *) packet;
        mac2par.RX_LENGTH = (fm_uint32) length;
        mac2par.RX_PORT   = (fm_uint32) port;

        // Call RX pipeline:
        RxPipeline(rx_top_map, shm_map, &mac2par, &rxs2rxo);
    }
    return sts;
}

static fm_status receivePacket
(
    const fm_uint32         sw,
    fm_uint32       * const port,
    fm_byte         * const packet,
    fm_uint32       * const length,
    const fm_uint32         max_pkt_size
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
        /* TxPipeline function will be called when modifier code is ready. */

        // Select egress port:
        fm_uint32 tx_port = 0;

        for (fm_uint i = 0; i < 18; i++)
            if (rxs2rxo.FNMASK & (1uL << i)) {
                tx_port = i;
                break;
            }

        // Populate output:
        *port   = tx_port;
        *length = rxs2rxo.RX_LENGTH;
    }
    return sts;
}

int main()
{
    int sw = 0;

    const uint32_t recv_pkt_buf_len = 1024;
    unsigned char recv_packet_buf[recv_pkt_buf_len];
    fm_uint32 exp_port = 1;
    fm_uint32 recv_port;
    fm_uint32 exp_packet_len = 1024;
    fm_uint32 recv_packet_len;
    fm_status status = FM_OK;

    printf("--------------------------------------------------------------------------------\n");

    int rv = 0;

    for (fm_uint i = 0; i < 1; i++) {

        status = init();
        rv = checkOk("init", status);
        if (rv != 0)
            break;

        status = sendPacket(send_sw, send_port, sent_packet, sizeof(sent_packet));
        rv = checkOk("sendPacket", status);
        if (rv != 0)
            break;

        status = receivePacket(send_sw, &recv_port, recv_packet_buf, &recv_packet_len, recv_pkt_buf_len);
        rv = checkOk("receivePacket", status);
        if (rv != 0)
            break;

        status = check(send_sw, send_port, recv_port, exp_port, recv_packet_len, exp_packet_len);
        rv = checkOk("check", status);
        if (rv != 0)
            break;
    }

    printf("--------------------------------------------------------------------------------\n");

    if (rv == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[fail]");

    printf(" Basic flood tests\n"  COLOR_RESET);

    printf("--------------------------------------------------------------------------------\n");

    return rv;
}
