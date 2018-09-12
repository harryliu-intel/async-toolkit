#include "mby_basic_fwd_init.h"

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

void basic_fwd_init(fm_uint32 fwd_port, fm_macaddr dmac)
{
    /* Overwrite FWD_PORT_CFG_1 values. */
    for (fm_int i = 0; i < 24; i++)
        mbyWriteReg(0, MBY_FWD_PORT_CFG_1(i, 0), 0x31FFFFF);

    /* Add L2 entry to match on. */
    fm_uint32 ma_table_regs[MBY_MA_TABLE_WIDTH] = { 0 };

    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, OLD_PORT, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, NEW_PORT, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, ENTRY_TYPE, MBY_MA_LOOKUP_ENTRY_TYPE_STATIC);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, TRIG_ID, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, S_GLORT, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, D_GLORT, 0x100 + fwd_port);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, L2_DOMAIN, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, VID, 0);
    FM_ARRAY_SET_FIELD64(ma_table_regs, MBY_MA_TABLE, MAC_ADDRESS, dmac);

    fm_uint16 cam_index = MBY_MA_TABLE_TCAM_SIZE - 1;
    fm_byte   bank      = MBY_MAC_ADDR_BANK_COUNT - 1;
    mbyWriteRegMult(0, MBY_MA_TABLE(bank, cam_index, 0), MBY_MA_TABLE_WIDTH, ma_table_regs);

    // TODO uncomment once these regs exist
    mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
}
