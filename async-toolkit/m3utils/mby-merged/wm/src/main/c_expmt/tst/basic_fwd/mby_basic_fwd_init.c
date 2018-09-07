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

void basic_fwd_init()
{
    // TODO uncomment once these regs exist
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
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
}
