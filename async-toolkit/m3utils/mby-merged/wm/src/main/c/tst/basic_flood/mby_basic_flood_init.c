#include "mby_basic_flood_init.h"

#include <mby_bitfield.h>
#include <mby_classifier.h>
#include <mby_common.h>
#include <mby_congmgmt.h>
#include <mby_crc32.h>
#include <mby_hash.h>
#include <mby_mapper.h>
#include <mby_maskgen.h>
#include <mby_model.h>
#include <mby_modifier.h>
#include <mby_nexthop.h>
#include <mby_parser.h>
#include <mby_pipeline.h>
#include <mby_reg_ctrl.h>
#include <mby_rxstats.h>
#include <mby_triggers.h>
#include <mby_txstats.h>

void basic_flood_init
(
    mby_ppe_rx_top_map * const rx_top_map
)
{
    ingress_vid_table_r * const ivid_table = &(rx_top_map->nexthop.INGRESS_VID_TABLE[1]);

    ivid_table->TRAP_IGMP  = 0;
    ivid_table->REFLECT    = 0;
    ivid_table->MEMBERSHIP = 0x3ffff;

    egress_vid_table_r * const evid_table = &(rx_top_map->mst_glort.EGRESS_VID_TABLE[1][0]);

    evid_table->MEMBERSHIP = 0x3ffff;

    ingress_mst_table_r * const ingress_mst_table = &(rx_top_map->mst_glort.INGRESS_MST_TABLE[1]);

    ingress_mst_table->STP_STATE_0 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_1 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_2 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_3 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_4 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_5 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_6 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_7 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_8 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_9 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_10 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_11 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_12 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_13 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_14 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_15 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_16 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_16 = MBY_STP_STATE_FORWARD;

    egress_mst_table_r * const egress_mst_table = &(rx_top_map->mst_glort.EGRESS_MST_TABLE[1][0]);

    egress_mst_table->FORWARDING = 0x3ffff;
}
