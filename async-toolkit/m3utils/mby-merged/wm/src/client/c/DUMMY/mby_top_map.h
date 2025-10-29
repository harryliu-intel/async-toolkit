#ifndef mby_top_map_INCLUDED
#define mby_top_map_INCLUDED

#include "uint.h"
typedef uint32 field_id;

/* ../src/RegC.m3:179 */
typedef struct {
  uint8 CGRP_ID;
  uint8 CGRP_MASK;
} trigger_condition_cgrp_r;

typedef struct {
  uint8 *CGRP_ID;
  uint8 *CGRP_MASK;
} trigger_condition_cgrp_r__addr;


void
trigger_condition_cgrp_r__init(
  trigger_condition_cgrp_r *p0,
  trigger_condition_cgrp_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_cgrp_r_CGRP_ID__n = 8;
#define trigger_condition_cgrp_r_CGRP_ID__nd    8
typedef uint8 trigger_condition_cgrp_r_CGRP_ID_t;

static const unsigned int trigger_condition_cgrp_r_CGRP_MASK__n = 8;
#define trigger_condition_cgrp_r_CGRP_MASK__nd    8
typedef uint8 trigger_condition_cgrp_r_CGRP_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint14 MTU;
} mtu_table_r;

typedef struct {
  uint14 *MTU;
} mtu_table_r__addr;


void
mtu_table_r__init(
  mtu_table_r *p0,
  mtu_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mtu_table_r_MTU__n = 14;
#define mtu_table_r_MTU__nd    14
typedef uint14 mtu_table_r_MTU_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 DEST_PORT_MASK;
} trigger_direct_map_ctx4_r;

typedef struct {
  uint2 *DEST_PORT_MASK;
} trigger_direct_map_ctx4_r__addr;


void
trigger_direct_map_ctx4_r__init(
  trigger_direct_map_ctx4_r *p0,
  trigger_direct_map_ctx4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_ctx4_r_DEST_PORT_MASK__n = 2;
#define trigger_direct_map_ctx4_r_DEST_PORT_MASK__nd    2
typedef uint2 trigger_direct_map_ctx4_r_DEST_PORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint3 SPEED;
} saf_egress_speed_r;

typedef struct {
  uint3 *SPEED;
} saf_egress_speed_r__addr;


void
saf_egress_speed_r__init(
  saf_egress_speed_r *p0,
  saf_egress_speed_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int saf_egress_speed_r_SPEED__n = 3;
#define saf_egress_speed_r_SPEED__nd    3
typedef uint3 saf_egress_speed_r_SPEED_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 NEW_DEST_GLORT;
  uint16 NEW_DEST_GLORT_MASK;
} trigger_action_glort_r;

typedef struct {
  uint16 *NEW_DEST_GLORT;
  uint16 *NEW_DEST_GLORT_MASK;
} trigger_action_glort_r__addr;


void
trigger_action_glort_r__init(
  trigger_action_glort_r *p0,
  trigger_action_glort_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_action_glort_r_NEW_DEST_GLORT__n = 16;
#define trigger_action_glort_r_NEW_DEST_GLORT__nd    16
typedef uint16 trigger_action_glort_r_NEW_DEST_GLORT_t;

static const unsigned int trigger_action_glort_r_NEW_DEST_GLORT_MASK__n = 16;
#define trigger_action_glort_r_NEW_DEST_GLORT_MASK__nd    16
typedef uint16 trigger_action_glort_r_NEW_DEST_GLORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 EWMA_FRAC;
  uint16 EWMA_WHOLE;
  uint8 INTERVAL;
} cm_tx_ewma_r;

typedef struct {
  uint8 *EWMA_FRAC;
  uint16 *EWMA_WHOLE;
  uint8 *INTERVAL;
} cm_tx_ewma_r__addr;


void
cm_tx_ewma_r__init(
  cm_tx_ewma_r *p0,
  cm_tx_ewma_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_ewma_r_EWMA_FRAC__n = 8;
#define cm_tx_ewma_r_EWMA_FRAC__nd    8
typedef uint8 cm_tx_ewma_r_EWMA_FRAC_t;

static const unsigned int cm_tx_ewma_r_EWMA_WHOLE__n = 16;
#define cm_tx_ewma_r_EWMA_WHOLE__nd    16
typedef uint16 cm_tx_ewma_r_EWMA_WHOLE_t;

static const unsigned int cm_tx_ewma_r_INTERVAL__n = 8;
#define cm_tx_ewma_r_INTERVAL__nd    8
typedef uint8 cm_tx_ewma_r_INTERVAL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 LAG_SIZE;
  uint4 INDEX;
  uint1 HASH_ROTATION;
  uint1 IN_LAG;
} fwd_lag_cfg_r;

typedef struct {
  uint4 *LAG_SIZE;
  uint4 *INDEX;
  uint1 *HASH_ROTATION;
  uint1 *IN_LAG;
} fwd_lag_cfg_r__addr;


void
fwd_lag_cfg_r__init(
  fwd_lag_cfg_r *p0,
  fwd_lag_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_lag_cfg_r_LAG_SIZE__n = 4;
#define fwd_lag_cfg_r_LAG_SIZE__nd    4
typedef uint4 fwd_lag_cfg_r_LAG_SIZE_t;

static const unsigned int fwd_lag_cfg_r_INDEX__n = 4;
#define fwd_lag_cfg_r_INDEX__nd    4
typedef uint4 fwd_lag_cfg_r_INDEX_t;

static const unsigned int fwd_lag_cfg_r_HASH_ROTATION__n = 1;
#define fwd_lag_cfg_r_HASH_ROTATION__nd    1
typedef uint1 fwd_lag_cfg_r_HASH_ROTATION_t;

static const unsigned int fwd_lag_cfg_r_IN_LAG__n = 1;
#define fwd_lag_cfg_r_IN_LAG__nd    1
typedef uint1 fwd_lag_cfg_r_IN_LAG_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 _RSVD0_;
  uint2 LEARN;
  uint4 _RSVD1_;
  uint2 MATCH_VLAN;
  uint2 MATCH_CGRP;
  uint2 MATCH_TC;
  uint2 _RSVD2_;
  uint2 MATCH_DEST_GLORT;
  uint2 MATCH_EGRESS_DOMAIN;
  uint4 _RSVD3_;
  uint1 MATCH_BY_PRECEDENCE;
  uint1 MATCH_RANDOM_NUMBER;
  uint1 MATCH_RANDOM_IF_LESS;
  uint5 MATCH_RANDOM_THRESHOLD;
  uint2 MATCH_TX;
  uint28 _RSVD4_;
} trigger_condition_cfg_r;

typedef struct {
  uint4 *_RSVD0_;
  uint2 *LEARN;
  uint4 *_RSVD1_;
  uint2 *MATCH_VLAN;
  uint2 *MATCH_CGRP;
  uint2 *MATCH_TC;
  uint2 *_RSVD2_;
  uint2 *MATCH_DEST_GLORT;
  uint2 *MATCH_EGRESS_DOMAIN;
  uint4 *_RSVD3_;
  uint1 *MATCH_BY_PRECEDENCE;
  uint1 *MATCH_RANDOM_NUMBER;
  uint1 *MATCH_RANDOM_IF_LESS;
  uint5 *MATCH_RANDOM_THRESHOLD;
  uint2 *MATCH_TX;
  uint28 *_RSVD4_;
} trigger_condition_cfg_r__addr;


void
trigger_condition_cfg_r__init(
  trigger_condition_cfg_r *p0,
  trigger_condition_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_cfg_r__RSVD0___n = 4;
#define trigger_condition_cfg_r__RSVD0___nd    4
typedef uint4 trigger_condition_cfg_r__RSVD0__t;

static const unsigned int trigger_condition_cfg_r_LEARN__n = 2;
#define trigger_condition_cfg_r_LEARN__nd    2
typedef uint2 trigger_condition_cfg_r_LEARN_t;

static const unsigned int trigger_condition_cfg_r__RSVD1___n = 4;
#define trigger_condition_cfg_r__RSVD1___nd    4
typedef uint4 trigger_condition_cfg_r__RSVD1__t;

static const unsigned int trigger_condition_cfg_r_MATCH_VLAN__n = 2;
#define trigger_condition_cfg_r_MATCH_VLAN__nd    2
typedef uint2 trigger_condition_cfg_r_MATCH_VLAN_t;

static const unsigned int trigger_condition_cfg_r_MATCH_CGRP__n = 2;
#define trigger_condition_cfg_r_MATCH_CGRP__nd    2
typedef uint2 trigger_condition_cfg_r_MATCH_CGRP_t;

static const unsigned int trigger_condition_cfg_r_MATCH_TC__n = 2;
#define trigger_condition_cfg_r_MATCH_TC__nd    2
typedef uint2 trigger_condition_cfg_r_MATCH_TC_t;

static const unsigned int trigger_condition_cfg_r__RSVD2___n = 2;
#define trigger_condition_cfg_r__RSVD2___nd    2
typedef uint2 trigger_condition_cfg_r__RSVD2__t;

static const unsigned int trigger_condition_cfg_r_MATCH_DEST_GLORT__n = 2;
#define trigger_condition_cfg_r_MATCH_DEST_GLORT__nd    2
typedef uint2 trigger_condition_cfg_r_MATCH_DEST_GLORT_t;

static const unsigned int trigger_condition_cfg_r_MATCH_EGRESS_DOMAIN__n = 2;
#define trigger_condition_cfg_r_MATCH_EGRESS_DOMAIN__nd    2
typedef uint2 trigger_condition_cfg_r_MATCH_EGRESS_DOMAIN_t;

static const unsigned int trigger_condition_cfg_r__RSVD3___n = 4;
#define trigger_condition_cfg_r__RSVD3___nd    4
typedef uint4 trigger_condition_cfg_r__RSVD3__t;

static const unsigned int trigger_condition_cfg_r_MATCH_BY_PRECEDENCE__n = 1;
#define trigger_condition_cfg_r_MATCH_BY_PRECEDENCE__nd    1
typedef uint1 trigger_condition_cfg_r_MATCH_BY_PRECEDENCE_t;

static const unsigned int trigger_condition_cfg_r_MATCH_RANDOM_NUMBER__n = 1;
#define trigger_condition_cfg_r_MATCH_RANDOM_NUMBER__nd    1
typedef uint1 trigger_condition_cfg_r_MATCH_RANDOM_NUMBER_t;

static const unsigned int trigger_condition_cfg_r_MATCH_RANDOM_IF_LESS__n = 1;
#define trigger_condition_cfg_r_MATCH_RANDOM_IF_LESS__nd    1
typedef uint1 trigger_condition_cfg_r_MATCH_RANDOM_IF_LESS_t;

static const unsigned int trigger_condition_cfg_r_MATCH_RANDOM_THRESHOLD__n = 5;
#define trigger_condition_cfg_r_MATCH_RANDOM_THRESHOLD__nd    5
typedef uint5 trigger_condition_cfg_r_MATCH_RANDOM_THRESHOLD_t;

static const unsigned int trigger_condition_cfg_r_MATCH_TX__n = 2;
#define trigger_condition_cfg_r_MATCH_TX__nd    2
typedef uint2 trigger_condition_cfg_r_MATCH_TX_t;

static const unsigned int trigger_condition_cfg_r__RSVD4___n = 28;
#define trigger_condition_cfg_r__RSVD4___nd    28
typedef uint28 trigger_condition_cfg_r__RSVD4__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 VID;
  uint8 L2_DOMAIN;
} ma_tcn_fifo_1_r;

typedef struct {
  uint12 *VID;
  uint8 *L2_DOMAIN;
} ma_tcn_fifo_1_r__addr;


void
ma_tcn_fifo_1_r__init(
  ma_tcn_fifo_1_r *p0,
  ma_tcn_fifo_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_fifo_1_r_VID__n = 12;
#define ma_tcn_fifo_1_r_VID__nd    12
typedef uint12 ma_tcn_fifo_1_r_VID_t;

static const unsigned int ma_tcn_fifo_1_r_L2_DOMAIN__n = 8;
#define ma_tcn_fifo_1_r_L2_DOMAIN__nd    8
typedef uint8 ma_tcn_fifo_1_r_L2_DOMAIN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint5 SAMPLE_RATE_EXP;
  uint1 SELECT_COMP;
  uint3 QUEUE_DEPTH_START_BIT;
  uint1 QUEUE_DEPTH_MODE;
} cm_apply_qcn_cfg_r;

typedef struct {
  uint5 *SAMPLE_RATE_EXP;
  uint1 *SELECT_COMP;
  uint3 *QUEUE_DEPTH_START_BIT;
  uint1 *QUEUE_DEPTH_MODE;
} cm_apply_qcn_cfg_r__addr;


void
cm_apply_qcn_cfg_r__init(
  cm_apply_qcn_cfg_r *p0,
  cm_apply_qcn_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_qcn_cfg_r_SAMPLE_RATE_EXP__n = 5;
#define cm_apply_qcn_cfg_r_SAMPLE_RATE_EXP__nd    5
typedef uint5 cm_apply_qcn_cfg_r_SAMPLE_RATE_EXP_t;

static const unsigned int cm_apply_qcn_cfg_r_SELECT_COMP__n = 1;
#define cm_apply_qcn_cfg_r_SELECT_COMP__nd    1
typedef uint1 cm_apply_qcn_cfg_r_SELECT_COMP_t;

static const unsigned int cm_apply_qcn_cfg_r_QUEUE_DEPTH_START_BIT__n = 3;
#define cm_apply_qcn_cfg_r_QUEUE_DEPTH_START_BIT__nd    3
typedef uint3 cm_apply_qcn_cfg_r_QUEUE_DEPTH_START_BIT_t;

static const unsigned int cm_apply_qcn_cfg_r_QUEUE_DEPTH_MODE__n = 1;
#define cm_apply_qcn_cfg_r_QUEUE_DEPTH_MODE__nd    1
typedef uint1 cm_apply_qcn_cfg_r_QUEUE_DEPTH_MODE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 UDP_0;
  uint8 UDP_1;
  uint8 UDP_2;
  uint8 TCP_0;
  uint8 TCP_1;
  uint8 TCP_2;
  uint8 SCTP;
} mod_csum_cfg2_r;

typedef struct {
  uint8 *UDP_0;
  uint8 *UDP_1;
  uint8 *UDP_2;
  uint8 *TCP_0;
  uint8 *TCP_1;
  uint8 *TCP_2;
  uint8 *SCTP;
} mod_csum_cfg2_r__addr;


void
mod_csum_cfg2_r__init(
  mod_csum_cfg2_r *p0,
  mod_csum_cfg2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_csum_cfg2_r_UDP_0__n = 8;
#define mod_csum_cfg2_r_UDP_0__nd    8
typedef uint8 mod_csum_cfg2_r_UDP_0_t;

static const unsigned int mod_csum_cfg2_r_UDP_1__n = 8;
#define mod_csum_cfg2_r_UDP_1__nd    8
typedef uint8 mod_csum_cfg2_r_UDP_1_t;

static const unsigned int mod_csum_cfg2_r_UDP_2__n = 8;
#define mod_csum_cfg2_r_UDP_2__nd    8
typedef uint8 mod_csum_cfg2_r_UDP_2_t;

static const unsigned int mod_csum_cfg2_r_TCP_0__n = 8;
#define mod_csum_cfg2_r_TCP_0__nd    8
typedef uint8 mod_csum_cfg2_r_TCP_0_t;

static const unsigned int mod_csum_cfg2_r_TCP_1__n = 8;
#define mod_csum_cfg2_r_TCP_1__nd    8
typedef uint8 mod_csum_cfg2_r_TCP_1_t;

static const unsigned int mod_csum_cfg2_r_TCP_2__n = 8;
#define mod_csum_cfg2_r_TCP_2__nd    8
typedef uint8 mod_csum_cfg2_r_TCP_2_t;

static const unsigned int mod_csum_cfg2_r_SCTP__n = 8;
#define mod_csum_cfg2_r_SCTP__nd    8
typedef uint8 mod_csum_cfg2_r_SCTP_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 TC_BY_VPRI;
} map_vpri_tc_r;

typedef struct {
  uint48 *TC_BY_VPRI;
} map_vpri_tc_r__addr;


void
map_vpri_tc_r__init(
  map_vpri_tc_r *p0,
  map_vpri_tc_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_vpri_tc_r_TC_BY_VPRI__n = 48;
#define map_vpri_tc_r_TC_BY_VPRI__nd    48
typedef uint48 map_vpri_tc_r_TC_BY_VPRI_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY;
  uint8 KEY_TOP;
  uint24 _RSVD0_;
  uint32 KEY_INVERT;
  uint8 KEY_TOP_INVERT;
  uint24 _RSVD1_;
} wcm_tcam_r;

typedef struct {
  uint32 *KEY;
  uint8 *KEY_TOP;
  uint24 *_RSVD0_;
  uint32 *KEY_INVERT;
  uint8 *KEY_TOP_INVERT;
  uint24 *_RSVD1_;
} wcm_tcam_r__addr;


void
wcm_tcam_r__init(
  wcm_tcam_r *p0,
  wcm_tcam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_tcam_r_KEY__n = 32;
#define wcm_tcam_r_KEY__nd    32
typedef uint32 wcm_tcam_r_KEY_t;

static const unsigned int wcm_tcam_r_KEY_TOP__n = 8;
#define wcm_tcam_r_KEY_TOP__nd    8
typedef uint8 wcm_tcam_r_KEY_TOP_t;

static const unsigned int wcm_tcam_r__RSVD0___n = 24;
#define wcm_tcam_r__RSVD0___nd    24
typedef uint24 wcm_tcam_r__RSVD0__t;

static const unsigned int wcm_tcam_r_KEY_INVERT__n = 32;
#define wcm_tcam_r_KEY_INVERT__nd    32
typedef uint32 wcm_tcam_r_KEY_INVERT_t;

static const unsigned int wcm_tcam_r_KEY_TOP_INVERT__n = 8;
#define wcm_tcam_r_KEY_TOP_INVERT__nd    8
typedef uint8 wcm_tcam_r_KEY_TOP_INVERT_t;

static const unsigned int wcm_tcam_r__RSVD1___n = 24;
#define wcm_tcam_r__RSVD1___nd    24
typedef uint24 wcm_tcam_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 PORT;
} rx_pb_port_cfg_r;

typedef struct {
  uint32 *PORT;
} rx_pb_port_cfg_r__addr;


void
rx_pb_port_cfg_r__init(
  rx_pb_port_cfg_r *p0,
  rx_pb_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_pb_port_cfg_r_PORT__n = 32;
#define rx_pb_port_cfg_r_PORT__nd    32
typedef uint32 rx_pb_port_cfg_r_PORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 DESTINATION_MASK;
} fwd_port_cfg_2_4_r;

typedef struct {
  uint1 *DESTINATION_MASK;
} fwd_port_cfg_2_4_r__addr;


void
fwd_port_cfg_2_4_r__init(
  fwd_port_cfg_2_4_r *p0,
  fwd_port_cfg_2_4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_2_4_r_DESTINATION_MASK__n = 1;
#define fwd_port_cfg_2_4_r_DESTINATION_MASK__nd    1
typedef uint1 fwd_port_cfg_2_4_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 DSCP;
  uint3 TC;
} map_dscp_tc_r;

typedef struct {
  uint6 *DSCP;
  uint3 *TC;
} map_dscp_tc_r__addr;


void
map_dscp_tc_r__init(
  map_dscp_tc_r *p0,
  map_dscp_tc_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_dscp_tc_r_DSCP__n = 6;
#define map_dscp_tc_r_DSCP__nd    6
typedef uint6 map_dscp_tc_r_DSCP_t;

static const unsigned int map_dscp_tc_r_TC__n = 3;
#define map_dscp_tc_r_TC__nd    3
typedef uint3 map_dscp_tc_r_TC_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint27 LEVEL;
} trigger_rate_lim_usage_r;

typedef struct {
  uint27 *LEVEL;
} trigger_rate_lim_usage_r__addr;


void
trigger_rate_lim_usage_r__init(
  trigger_rate_lim_usage_r *p0,
  trigger_rate_lim_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_rate_lim_usage_r_LEVEL__n = 27;
#define trigger_rate_lim_usage_r_LEVEL__nd    27
typedef uint27 trigger_rate_lim_usage_r_LEVEL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 saf;
} saf_tc_r;

typedef struct {
  uint64 *saf;
} saf_tc_r__addr;


void
saf_tc_r__init(
  saf_tc_r *p0,
  saf_tc_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int saf_tc_r_saf__n = 64;
#define saf_tc_r_saf__nd    64
typedef uint64 saf_tc_r_saf_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_global_usage_r;

typedef struct {
  uint16 *COUNT;
} cm_global_usage_r__addr;


void
cm_global_usage_r__init(
  cm_global_usage_r *p0,
  cm_global_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_global_usage_r_COUNT__n = 16;
#define cm_global_usage_r_COUNT__nd    16
typedef uint16 cm_global_usage_r_COUNT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 L4_SRC;
  uint3 MAP_PROT;
  uint2 VALID;
  uint16 MAP_L4_SRC;
} map_l4_src_r;

typedef struct {
  uint16 *L4_SRC;
  uint3 *MAP_PROT;
  uint2 *VALID;
  uint16 *MAP_L4_SRC;
} map_l4_src_r__addr;


void
map_l4_src_r__init(
  map_l4_src_r *p0,
  map_l4_src_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_l4_src_r_L4_SRC__n = 16;
#define map_l4_src_r_L4_SRC__nd    16
typedef uint16 map_l4_src_r_L4_SRC_t;

static const unsigned int map_l4_src_r_MAP_PROT__n = 3;
#define map_l4_src_r_MAP_PROT__nd    3
typedef uint3 map_l4_src_r_MAP_PROT_t;

static const unsigned int map_l4_src_r_VALID__n = 2;
#define map_l4_src_r_VALID__nd    2
typedef uint2 map_l4_src_r_VALID_t;

static const unsigned int map_l4_src_r_MAP_L4_SRC__n = 16;
#define map_l4_src_r_MAP_L4_SRC__nd    16
typedef uint16 map_l4_src_r_MAP_L4_SRC_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 GLORT_DEST_MASK;
} glort_dest_table_r0;

typedef struct {
  uint64 *GLORT_DEST_MASK;
} glort_dest_table_r0__addr;


void
glort_dest_table_r0__init(
  glort_dest_table_r0 *p0,
  glort_dest_table_r0__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_dest_table_r0_GLORT_DEST_MASK__n = 64;
#define glort_dest_table_r0_GLORT_DEST_MASK__nd    64
typedef uint64 glort_dest_table_r0_GLORT_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY8_MASK;
} em_key_sel0_r;

typedef struct {
  uint32 *KEY8_MASK;
} em_key_sel0_r__addr;


void
em_key_sel0_r__init(
  em_key_sel0_r *p0,
  em_key_sel0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_key_sel0_r_KEY8_MASK__n = 32;
#define em_key_sel0_r_KEY8_MASK__nd    32
typedef uint32 em_key_sel0_r_KEY8_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 TRIG_ID;
} egress_vid_cfg_r;

typedef struct {
  uint6 *TRIG_ID;
} egress_vid_cfg_r__addr;


void
egress_vid_cfg_r__init(
  egress_vid_cfg_r *p0,
  egress_vid_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int egress_vid_cfg_r_TRIG_ID__n = 6;
#define egress_vid_cfg_r_TRIG_ID__nd    6
typedef uint6 egress_vid_cfg_r_TRIG_ID_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint14 NEIGHBOR_IDX;
  uint12 GROUP_IDX;
  uint8 AGE_COUNTER;
} nexthop_routes_table_r;

typedef struct {
  uint14 *NEIGHBOR_IDX;
  uint12 *GROUP_IDX;
  uint8 *AGE_COUNTER;
} nexthop_routes_table_r__addr;


void
nexthop_routes_table_r__init(
  nexthop_routes_table_r *p0,
  nexthop_routes_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_routes_table_r_NEIGHBOR_IDX__n = 14;
#define nexthop_routes_table_r_NEIGHBOR_IDX__nd    14
typedef uint14 nexthop_routes_table_r_NEIGHBOR_IDX_t;

static const unsigned int nexthop_routes_table_r_GROUP_IDX__n = 12;
#define nexthop_routes_table_r_GROUP_IDX__nd    12
typedef uint12 nexthop_routes_table_r_GROUP_IDX_t;

static const unsigned int nexthop_routes_table_r_AGE_COUNTER__n = 8;
#define nexthop_routes_table_r_AGE_COUNTER__nd    8
typedef uint8 nexthop_routes_table_r_AGE_COUNTER_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint60 INDEX;
} wcm_action_cfg_r;

typedef struct {
  uint60 *INDEX;
} wcm_action_cfg_r__addr;


void
wcm_action_cfg_r__init(
  wcm_action_cfg_r *p0,
  wcm_action_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_action_cfg_r_INDEX__n = 60;
#define wcm_action_cfg_r_INDEX__nd    60
typedef uint60 wcm_action_cfg_r_INDEX_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 PORT_PROFILE;
  uint1 DEFAULT_SGLORT_EN;
  uint16 DEFAULT_SGLORT;
} map_port_cfg_r;

typedef struct {
  uint8 *PORT_PROFILE;
  uint1 *DEFAULT_SGLORT_EN;
  uint16 *DEFAULT_SGLORT;
} map_port_cfg_r__addr;


void
map_port_cfg_r__init(
  map_port_cfg_r *p0,
  map_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_port_cfg_r_PORT_PROFILE__n = 8;
#define map_port_cfg_r_PORT_PROFILE__nd    8
typedef uint8 map_port_cfg_r_PORT_PROFILE_t;

static const unsigned int map_port_cfg_r_DEFAULT_SGLORT_EN__n = 1;
#define map_port_cfg_r_DEFAULT_SGLORT_EN__nd    1
typedef uint1 map_port_cfg_r_DEFAULT_SGLORT_EN_t;

static const unsigned int map_port_cfg_r_DEFAULT_SGLORT__n = 16;
#define map_port_cfg_r_DEFAULT_SGLORT__nd    16
typedef uint16 map_port_cfg_r_DEFAULT_SGLORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 SUBTRIE_PTR;
  uint15 CHILD_BASE_PTR;
  uint2 _RSVD0_;
  uint8 CHILD_PTR_LEN;
} lpm_subtrie_cptr_r;

typedef struct {
  uint15 *SUBTRIE_PTR;
  uint15 *CHILD_BASE_PTR;
  uint2 *_RSVD0_;
  uint8 *CHILD_PTR_LEN;
} lpm_subtrie_cptr_r__addr;


void
lpm_subtrie_cptr_r__init(
  lpm_subtrie_cptr_r *p0,
  lpm_subtrie_cptr_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_subtrie_cptr_r_SUBTRIE_PTR__n = 15;
#define lpm_subtrie_cptr_r_SUBTRIE_PTR__nd    15
typedef uint15 lpm_subtrie_cptr_r_SUBTRIE_PTR_t;

static const unsigned int lpm_subtrie_cptr_r_CHILD_BASE_PTR__n = 15;
#define lpm_subtrie_cptr_r_CHILD_BASE_PTR__nd    15
typedef uint15 lpm_subtrie_cptr_r_CHILD_BASE_PTR_t;

static const unsigned int lpm_subtrie_cptr_r__RSVD0___n = 2;
#define lpm_subtrie_cptr_r__RSVD0___nd    2
typedef uint2 lpm_subtrie_cptr_r__RSVD0__t;

static const unsigned int lpm_subtrie_cptr_r_CHILD_PTR_LEN__n = 8;
#define lpm_subtrie_cptr_r_CHILD_PTR_LEN__nd    8
typedef uint8 lpm_subtrie_cptr_r_CHILD_PTR_LEN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 MAC_ADDR;
} fwd_cpu_mac_r;

typedef struct {
  uint48 *MAC_ADDR;
} fwd_cpu_mac_r__addr;


void
fwd_cpu_mac_r__init(
  fwd_cpu_mac_r *p0,
  fwd_cpu_mac_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_cpu_mac_r_MAC_ADDR__n = 48;
#define fwd_cpu_mac_r_MAC_ADDR__nd    48
typedef uint48 fwd_cpu_mac_r_MAC_ADDR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY_MASK8;
} entropy_hash_cfg0_r;

typedef struct {
  uint32 *KEY_MASK8;
} entropy_hash_cfg0_r__addr;


void
entropy_hash_cfg0_r__init(
  entropy_hash_cfg0_r *p0,
  entropy_hash_cfg0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_cfg0_r_KEY_MASK8__n = 32;
#define entropy_hash_cfg0_r_KEY_MASK8__nd    32
typedef uint32 entropy_hash_cfg0_r_KEY_MASK8_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MASK;
} lpm_key_mask_r;

typedef struct {
  uint64 *MASK;
} lpm_key_mask_r__addr;


void
lpm_key_mask_r__init(
  lpm_key_mask_r *p0,
  lpm_key_mask_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_key_mask_r_MASK__n = 64;
#define lpm_key_mask_r_MASK__nd    64
typedef uint64 lpm_key_mask_r_MASK_t;




/* ../src/RegC.m3:221 */
typedef lpm_key_mask_r lpm_key_mask_rf[20];

typedef lpm_key_mask_r__addr lpm_key_mask_rf__addr[20];


void
lpm_key_mask_rf__init(
  lpm_key_mask_rf *p0,
  lpm_key_mask_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_key_mask_rf_LPM_KEY_MASK__n = 20;
#define lpm_key_mask_rf_LPM_KEY_MASK__nd    20



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 NEW_DEST_MASK;
} trigger_direct_map_adm0_r;

typedef struct {
  uint64 *NEW_DEST_MASK;
} trigger_direct_map_adm0_r__addr;


void
trigger_direct_map_adm0_r__init(
  trigger_direct_map_adm0_r *p0,
  trigger_direct_map_adm0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adm0_r_NEW_DEST_MASK__n = 64;
#define trigger_direct_map_adm0_r_NEW_DEST_MASK__nd    64
typedef uint64 trigger_direct_map_adm0_r_NEW_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 DEST_MASK;
} cm_apply_cpu_trap_mask_4_r;

typedef struct {
  uint1 *DEST_MASK;
} cm_apply_cpu_trap_mask_4_r__addr;


void
cm_apply_cpu_trap_mask_4_r__init(
  cm_apply_cpu_trap_mask_4_r *p0,
  cm_apply_cpu_trap_mask_4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_cpu_trap_mask_4_r_DEST_MASK__n = 1;
#define cm_apply_cpu_trap_mask_4_r_DEST_MASK__nd    1
typedef uint1 cm_apply_cpu_trap_mask_4_r_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 FFU;
  uint6 RESERVED_MAC;
  uint6 ARP_REDIRECT;
  uint6 ICMP;
  uint6 TTL;
  uint6 TRIGGER;
} cm_apply_log_mirror_profile_r;

typedef struct {
  uint6 *FFU;
  uint6 *RESERVED_MAC;
  uint6 *ARP_REDIRECT;
  uint6 *ICMP;
  uint6 *TTL;
  uint6 *TRIGGER;
} cm_apply_log_mirror_profile_r__addr;


void
cm_apply_log_mirror_profile_r__init(
  cm_apply_log_mirror_profile_r *p0,
  cm_apply_log_mirror_profile_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_log_mirror_profile_r_FFU__n = 6;
#define cm_apply_log_mirror_profile_r_FFU__nd    6
typedef uint6 cm_apply_log_mirror_profile_r_FFU_t;

static const unsigned int cm_apply_log_mirror_profile_r_RESERVED_MAC__n = 6;
#define cm_apply_log_mirror_profile_r_RESERVED_MAC__nd    6
typedef uint6 cm_apply_log_mirror_profile_r_RESERVED_MAC_t;

static const unsigned int cm_apply_log_mirror_profile_r_ARP_REDIRECT__n = 6;
#define cm_apply_log_mirror_profile_r_ARP_REDIRECT__nd    6
typedef uint6 cm_apply_log_mirror_profile_r_ARP_REDIRECT_t;

static const unsigned int cm_apply_log_mirror_profile_r_ICMP__n = 6;
#define cm_apply_log_mirror_profile_r_ICMP__nd    6
typedef uint6 cm_apply_log_mirror_profile_r_ICMP_t;

static const unsigned int cm_apply_log_mirror_profile_r_TTL__n = 6;
#define cm_apply_log_mirror_profile_r_TTL__nd    6
typedef uint6 cm_apply_log_mirror_profile_r_TTL_t;

static const unsigned int cm_apply_log_mirror_profile_r_TRIGGER__n = 6;
#define cm_apply_log_mirror_profile_r_TRIGGER__nd    6
typedef uint6 cm_apply_log_mirror_profile_r_TRIGGER_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 MIN_0;
  uint16 MIN_1;
  uint16 MIN_2;
  uint16 MIN_3;
} nexthop_group_min_r;

typedef struct {
  uint16 *MIN_0;
  uint16 *MIN_1;
  uint16 *MIN_2;
  uint16 *MIN_3;
} nexthop_group_min_r__addr;


void
nexthop_group_min_r__init(
  nexthop_group_min_r *p0,
  nexthop_group_min_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_group_min_r_MIN_0__n = 16;
#define nexthop_group_min_r_MIN_0__nd    16
typedef uint16 nexthop_group_min_r_MIN_0_t;

static const unsigned int nexthop_group_min_r_MIN_1__n = 16;
#define nexthop_group_min_r_MIN_1__nd    16
typedef uint16 nexthop_group_min_r_MIN_1_t;

static const unsigned int nexthop_group_min_r_MIN_2__n = 16;
#define nexthop_group_min_r_MIN_2__nd    16
typedef uint16 nexthop_group_min_r_MIN_2_t;

static const unsigned int nexthop_group_min_r_MIN_3__n = 16;
#define nexthop_group_min_r_MIN_3__nd    16
typedef uint16 nexthop_group_min_r_MIN_3_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 WEIGHT_0;
  uint8 WEIGHT_1;
  uint8 WEIGHT_2;
  uint8 WEIGHT_3;
  uint8 WEIGHT_4;
  uint8 WEIGHT_5;
  uint8 WEIGHT_6;
  uint8 WEIGHT_7;
} nexthop_weights_table_r;

typedef struct {
  uint8 *WEIGHT_0;
  uint8 *WEIGHT_1;
  uint8 *WEIGHT_2;
  uint8 *WEIGHT_3;
  uint8 *WEIGHT_4;
  uint8 *WEIGHT_5;
  uint8 *WEIGHT_6;
  uint8 *WEIGHT_7;
} nexthop_weights_table_r__addr;


void
nexthop_weights_table_r__init(
  nexthop_weights_table_r *p0,
  nexthop_weights_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_weights_table_r_WEIGHT_0__n = 8;
#define nexthop_weights_table_r_WEIGHT_0__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_0_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_1__n = 8;
#define nexthop_weights_table_r_WEIGHT_1__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_1_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_2__n = 8;
#define nexthop_weights_table_r_WEIGHT_2__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_2_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_3__n = 8;
#define nexthop_weights_table_r_WEIGHT_3__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_3_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_4__n = 8;
#define nexthop_weights_table_r_WEIGHT_4__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_4_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_5__n = 8;
#define nexthop_weights_table_r_WEIGHT_5__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_5_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_6__n = 8;
#define nexthop_weights_table_r_WEIGHT_6__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_6_t;

static const unsigned int nexthop_weights_table_r_WEIGHT_7__n = 8;
#define nexthop_weights_table_r_WEIGHT_7__nd    8
typedef uint8 nexthop_weights_table_r_WEIGHT_7_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 PENDING;
} trigger_ip_r;

typedef struct {
  uint48 *PENDING;
} trigger_ip_r__addr;


void
trigger_ip_r__init(
  trigger_ip_r *p0,
  trigger_ip_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_ip_r_PENDING__n = 48;
#define trigger_ip_r_PENDING__nd    48
typedef uint48 trigger_ip_r_PENDING_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 SHELL_CTRL_U_ERR;
  uint1 ENTRY_COUNT;
  uint1 TCAM_ERR;
  uint1 TRIGGER;
  uint1 WB_FIFO_PERR;
  uint1 MA_TCN;
} fwd_im_r;

typedef struct {
  uint1 *SHELL_CTRL_U_ERR;
  uint1 *ENTRY_COUNT;
  uint1 *TCAM_ERR;
  uint1 *TRIGGER;
  uint1 *WB_FIFO_PERR;
  uint1 *MA_TCN;
} fwd_im_r__addr;


void
fwd_im_r__init(
  fwd_im_r *p0,
  fwd_im_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_im_r_SHELL_CTRL_U_ERR__n = 1;
#define fwd_im_r_SHELL_CTRL_U_ERR__nd    1
typedef uint1 fwd_im_r_SHELL_CTRL_U_ERR_t;

static const unsigned int fwd_im_r_ENTRY_COUNT__n = 1;
#define fwd_im_r_ENTRY_COUNT__nd    1
typedef uint1 fwd_im_r_ENTRY_COUNT_t;

static const unsigned int fwd_im_r_TCAM_ERR__n = 1;
#define fwd_im_r_TCAM_ERR__nd    1
typedef uint1 fwd_im_r_TCAM_ERR_t;

static const unsigned int fwd_im_r_TRIGGER__n = 1;
#define fwd_im_r_TRIGGER__nd    1
typedef uint1 fwd_im_r_TRIGGER_t;

static const unsigned int fwd_im_r_WB_FIFO_PERR__n = 1;
#define fwd_im_r_WB_FIFO_PERR__nd    1
typedef uint1 fwd_im_r_WB_FIFO_PERR_t;

static const unsigned int fwd_im_r_MA_TCN__n = 1;
#define fwd_im_r_MA_TCN__nd    1
typedef uint1 fwd_im_r_MA_TCN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint5 NUM_SWEEPER_PORTS;
  uint1 SWEEPER_EN;
} cm_global_cfg_r;

typedef struct {
  uint5 *NUM_SWEEPER_PORTS;
  uint1 *SWEEPER_EN;
} cm_global_cfg_r__addr;


void
cm_global_cfg_r__init(
  cm_global_cfg_r *p0,
  cm_global_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_global_cfg_r_NUM_SWEEPER_PORTS__n = 5;
#define cm_global_cfg_r_NUM_SWEEPER_PORTS__nd    5
typedef uint5 cm_global_cfg_r_NUM_SWEEPER_PORTS_t;

static const unsigned int cm_global_cfg_r_SWEEPER_EN__n = 1;
#define cm_global_cfg_r_SWEEPER_EN__nd    1
typedef uint1 cm_global_cfg_r_SWEEPER_EN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 D0_IPV4_EN;
  uint1 D1_IPV4_EN;
  uint1 D2_IPV4_EN;
  uint1 D0_IPV6_EN;
  uint1 D1_IPV6_EN;
  uint1 D2_IPV6_EN;
  uint1 D0_UDP_EN;
  uint1 D1_UDP_EN;
  uint1 D0_TCP_EN;
  uint1 D1_TCP_EN;
  uint1 SCTP_EN;
} mod_csum_en_cfg_r;

typedef struct {
  uint1 *D0_IPV4_EN;
  uint1 *D1_IPV4_EN;
  uint1 *D2_IPV4_EN;
  uint1 *D0_IPV6_EN;
  uint1 *D1_IPV6_EN;
  uint1 *D2_IPV6_EN;
  uint1 *D0_UDP_EN;
  uint1 *D1_UDP_EN;
  uint1 *D0_TCP_EN;
  uint1 *D1_TCP_EN;
  uint1 *SCTP_EN;
} mod_csum_en_cfg_r__addr;


void
mod_csum_en_cfg_r__init(
  mod_csum_en_cfg_r *p0,
  mod_csum_en_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_csum_en_cfg_r_D0_IPV4_EN__n = 1;
#define mod_csum_en_cfg_r_D0_IPV4_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D0_IPV4_EN_t;

static const unsigned int mod_csum_en_cfg_r_D1_IPV4_EN__n = 1;
#define mod_csum_en_cfg_r_D1_IPV4_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D1_IPV4_EN_t;

static const unsigned int mod_csum_en_cfg_r_D2_IPV4_EN__n = 1;
#define mod_csum_en_cfg_r_D2_IPV4_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D2_IPV4_EN_t;

static const unsigned int mod_csum_en_cfg_r_D0_IPV6_EN__n = 1;
#define mod_csum_en_cfg_r_D0_IPV6_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D0_IPV6_EN_t;

static const unsigned int mod_csum_en_cfg_r_D1_IPV6_EN__n = 1;
#define mod_csum_en_cfg_r_D1_IPV6_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D1_IPV6_EN_t;

static const unsigned int mod_csum_en_cfg_r_D2_IPV6_EN__n = 1;
#define mod_csum_en_cfg_r_D2_IPV6_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D2_IPV6_EN_t;

static const unsigned int mod_csum_en_cfg_r_D0_UDP_EN__n = 1;
#define mod_csum_en_cfg_r_D0_UDP_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D0_UDP_EN_t;

static const unsigned int mod_csum_en_cfg_r_D1_UDP_EN__n = 1;
#define mod_csum_en_cfg_r_D1_UDP_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D1_UDP_EN_t;

static const unsigned int mod_csum_en_cfg_r_D0_TCP_EN__n = 1;
#define mod_csum_en_cfg_r_D0_TCP_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D0_TCP_EN_t;

static const unsigned int mod_csum_en_cfg_r_D1_TCP_EN__n = 1;
#define mod_csum_en_cfg_r_D1_TCP_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_D1_TCP_EN_t;

static const unsigned int mod_csum_en_cfg_r_SCTP_EN__n = 1;
#define mod_csum_en_cfg_r_SCTP_EN__nd    1
typedef uint1 mod_csum_en_cfg_r_SCTP_EN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint3 PTR_NUM;
  uint1 FLAG_VALUE;
  uint6 FLAG_NUM;
  uint8 OFFSET;
  uint8 PROTOCOL_ID;
} parser_ext_r;

typedef struct {
  uint3 *PTR_NUM;
  uint1 *FLAG_VALUE;
  uint6 *FLAG_NUM;
  uint8 *OFFSET;
  uint8 *PROTOCOL_ID;
} parser_ext_r__addr;


void
parser_ext_r__init(
  parser_ext_r *p0,
  parser_ext_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ext_r_PTR_NUM__n = 3;
#define parser_ext_r_PTR_NUM__nd    3
typedef uint3 parser_ext_r_PTR_NUM_t;

static const unsigned int parser_ext_r_FLAG_VALUE__n = 1;
#define parser_ext_r_FLAG_VALUE__nd    1
typedef uint1 parser_ext_r_FLAG_VALUE_t;

static const unsigned int parser_ext_r_FLAG_NUM__n = 6;
#define parser_ext_r_FLAG_NUM__nd    6
typedef uint6 parser_ext_r_FLAG_NUM_t;

static const unsigned int parser_ext_r_OFFSET__n = 8;
#define parser_ext_r_OFFSET__nd    8
typedef uint8 parser_ext_r_OFFSET_t;

static const unsigned int parser_ext_r_PROTOCOL_ID__n = 8;
#define parser_ext_r_PROTOCOL_ID__nd    8
typedef uint8 parser_ext_r_PROTOCOL_ID_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 IP_LO;
} map_ip_lo_r;

typedef struct {
  uint64 *IP_LO;
} map_ip_lo_r__addr;


void
map_ip_lo_r__init(
  map_ip_lo_r *p0,
  map_ip_lo_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_ip_lo_r_IP_LO__n = 64;
#define map_ip_lo_r_IP_LO__nd    64
typedef uint64 map_ip_lo_r_IP_LO_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 USAGE_OVER_LIMIT;
  uint1 OVER_LIMIT;
} cm_apply_softdrop_state_r;

typedef struct {
  uint8 *USAGE_OVER_LIMIT;
  uint1 *OVER_LIMIT;
} cm_apply_softdrop_state_r__addr;


void
cm_apply_softdrop_state_r__init(
  cm_apply_softdrop_state_r *p0,
  cm_apply_softdrop_state_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_softdrop_state_r_USAGE_OVER_LIMIT__n = 8;
#define cm_apply_softdrop_state_r_USAGE_OVER_LIMIT__nd    8
typedef uint8 cm_apply_softdrop_state_r_USAGE_OVER_LIMIT_t;

static const unsigned int cm_apply_softdrop_state_r_OVER_LIMIT__n = 1;
#define cm_apply_softdrop_state_r_OVER_LIMIT__nd    1
typedef uint1 cm_apply_softdrop_state_r_OVER_LIMIT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 LAG_GLORT;
  uint4 MASK_SIZE;
  uint3 PORT_FIELD_SIZE;
} map_canonical_glort_cam_r;

typedef struct {
  uint16 *LAG_GLORT;
  uint4 *MASK_SIZE;
  uint3 *PORT_FIELD_SIZE;
} map_canonical_glort_cam_r__addr;


void
map_canonical_glort_cam_r__init(
  map_canonical_glort_cam_r *p0,
  map_canonical_glort_cam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_canonical_glort_cam_r_LAG_GLORT__n = 16;
#define map_canonical_glort_cam_r_LAG_GLORT__nd    16
typedef uint16 map_canonical_glort_cam_r_LAG_GLORT_t;

static const unsigned int map_canonical_glort_cam_r_MASK_SIZE__n = 4;
#define map_canonical_glort_cam_r_MASK_SIZE__nd    4
typedef uint4 map_canonical_glort_cam_r_MASK_SIZE_t;

static const unsigned int map_canonical_glort_cam_r_PORT_FIELD_SIZE__n = 3;
#define map_canonical_glort_cam_r_PORT_FIELD_SIZE__nd    3
typedef uint3 map_canonical_glort_cam_r_PORT_FIELD_SIZE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 TRAP_GLORT;
} cm_apply_trap_glort_r;

typedef struct {
  uint16 *TRAP_GLORT;
} cm_apply_trap_glort_r__addr;


void
cm_apply_trap_glort_r__init(
  cm_apply_trap_glort_r *p0,
  cm_apply_trap_glort_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_trap_glort_r_TRAP_GLORT__n = 16;
#define cm_apply_trap_glort_r_TRAP_GLORT__nd    16
typedef uint16 cm_apply_trap_glort_r_TRAP_GLORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 REG_INDX;
  uint8 REG_SUB_ID;
  uint8 REG_ID;
  uint13 _RSVD0_;
  uint1 OP_TYPE;
  uint1 STATUS;
  uint1 GO_COMPL;
} trigger_direct_map_ctrl_r;

typedef struct {
  uint32 *REG_INDX;
  uint8 *REG_SUB_ID;
  uint8 *REG_ID;
  uint13 *_RSVD0_;
  uint1 *OP_TYPE;
  uint1 *STATUS;
  uint1 *GO_COMPL;
} trigger_direct_map_ctrl_r__addr;


void
trigger_direct_map_ctrl_r__init(
  trigger_direct_map_ctrl_r *p0,
  trigger_direct_map_ctrl_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_ctrl_r_REG_INDX__n = 32;
#define trigger_direct_map_ctrl_r_REG_INDX__nd    32
typedef uint32 trigger_direct_map_ctrl_r_REG_INDX_t;

static const unsigned int trigger_direct_map_ctrl_r_REG_SUB_ID__n = 8;
#define trigger_direct_map_ctrl_r_REG_SUB_ID__nd    8
typedef uint8 trigger_direct_map_ctrl_r_REG_SUB_ID_t;

static const unsigned int trigger_direct_map_ctrl_r_REG_ID__n = 8;
#define trigger_direct_map_ctrl_r_REG_ID__nd    8
typedef uint8 trigger_direct_map_ctrl_r_REG_ID_t;

static const unsigned int trigger_direct_map_ctrl_r__RSVD0___n = 13;
#define trigger_direct_map_ctrl_r__RSVD0___nd    13
typedef uint13 trigger_direct_map_ctrl_r__RSVD0__t;

static const unsigned int trigger_direct_map_ctrl_r_OP_TYPE__n = 1;
#define trigger_direct_map_ctrl_r_OP_TYPE__nd    1
typedef uint1 trigger_direct_map_ctrl_r_OP_TYPE_t;

static const unsigned int trigger_direct_map_ctrl_r_STATUS__n = 1;
#define trigger_direct_map_ctrl_r_STATUS__nd    1
typedef uint1 trigger_direct_map_ctrl_r_STATUS_t;

static const unsigned int trigger_direct_map_ctrl_r_GO_COMPL__n = 1;
#define trigger_direct_map_ctrl_r_GO_COMPL__nd    1
typedef uint1 trigger_direct_map_ctrl_r_GO_COMPL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 FLOWLET_ENABLE;
  uint1 FLOWLET_INT_EN;
  uint8 SWEEPER_RATE;
} nexthop_config_r;

typedef struct {
  uint1 *FLOWLET_ENABLE;
  uint1 *FLOWLET_INT_EN;
  uint8 *SWEEPER_RATE;
} nexthop_config_r__addr;


void
nexthop_config_r__init(
  nexthop_config_r *p0,
  nexthop_config_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_config_r_FLOWLET_ENABLE__n = 1;
#define nexthop_config_r_FLOWLET_ENABLE__nd    1
typedef uint1 nexthop_config_r_FLOWLET_ENABLE_t;

static const unsigned int nexthop_config_r_FLOWLET_INT_EN__n = 1;
#define nexthop_config_r_FLOWLET_INT_EN__nd    1
typedef uint1 nexthop_config_r_FLOWLET_INT_EN_t;

static const unsigned int nexthop_config_r_SWEEPER_RATE__n = 8;
#define nexthop_config_r_SWEEPER_RATE__nd    8
typedef uint8 nexthop_config_r_SWEEPER_RATE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DATA;
} em_hash_cam_r;

typedef struct {
  uint64 *DATA;
} em_hash_cam_r__addr;


void
em_hash_cam_r__init(
  em_hash_cam_r *p0,
  em_hash_cam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_cam_r_DATA__n = 64;
#define em_hash_cam_r_DATA__nd    64
typedef uint64 em_hash_cam_r_DATA_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 MAC;
  uint6 IGNORE_LENGTH;
  uint4 VALID;
  uint8 MAP_MAC;
  uint1 MAC_ROUTABLE;
} map_mac_r;

typedef struct {
  uint48 *MAC;
  uint6 *IGNORE_LENGTH;
  uint4 *VALID;
  uint8 *MAP_MAC;
  uint1 *MAC_ROUTABLE;
} map_mac_r__addr;


void
map_mac_r__init(
  map_mac_r *p0,
  map_mac_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_mac_r_MAC__n = 48;
#define map_mac_r_MAC__nd    48
typedef uint48 map_mac_r_MAC_t;

static const unsigned int map_mac_r_IGNORE_LENGTH__n = 6;
#define map_mac_r_IGNORE_LENGTH__nd    6
typedef uint6 map_mac_r_IGNORE_LENGTH_t;

static const unsigned int map_mac_r_VALID__n = 4;
#define map_mac_r_VALID__nd    4
typedef uint4 map_mac_r_VALID_t;

static const unsigned int map_mac_r_MAP_MAC__n = 8;
#define map_mac_r_MAP_MAC__nd    8
typedef uint8 map_mac_r_MAP_MAC_t;

static const unsigned int map_mac_r_MAC_ROUTABLE__n = 1;
#define map_mac_r_MAC_ROUTABLE__nd    1
typedef uint1 map_mac_r_MAC_ROUTABLE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MIRROR_PORT_MASK0;
} cm_apply_mirror_ecmp_dmask0_r;

typedef struct {
  uint64 *MIRROR_PORT_MASK0;
} cm_apply_mirror_ecmp_dmask0_r__addr;


void
cm_apply_mirror_ecmp_dmask0_r__init(
  cm_apply_mirror_ecmp_dmask0_r *p0,
  cm_apply_mirror_ecmp_dmask0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_mirror_ecmp_dmask0_r_MIRROR_PORT_MASK0__n = 64;
#define cm_apply_mirror_ecmp_dmask0_r_MIRROR_PORT_MASK0__nd    64
typedef uint64 cm_apply_mirror_ecmp_dmask0_r_MIRROR_PORT_MASK0_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY16_MASK;
  uint16 KEY32_MASK;
  uint4 KEY_MASK_SEL;
} em_key_sel1_r;

typedef struct {
  uint32 *KEY16_MASK;
  uint16 *KEY32_MASK;
  uint4 *KEY_MASK_SEL;
} em_key_sel1_r__addr;


void
em_key_sel1_r__init(
  em_key_sel1_r *p0,
  em_key_sel1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_key_sel1_r_KEY16_MASK__n = 32;
#define em_key_sel1_r_KEY16_MASK__nd    32
typedef uint32 em_key_sel1_r_KEY16_MASK_t;

static const unsigned int em_key_sel1_r_KEY32_MASK__n = 16;
#define em_key_sel1_r_KEY32_MASK__nd    16
typedef uint16 em_key_sel1_r_KEY32_MASK_t;

static const unsigned int em_key_sel1_r_KEY_MASK_SEL__n = 4;
#define em_key_sel1_r_KEY_MASK_SEL__nd    4
typedef uint4 em_key_sel1_r_KEY_MASK_SEL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 MAX_TH;
  uint15 MIN_TH;
  uint3 TC;
  uint4 W;
  uint8 UPDATE_INTERVAL;
} cm_aqm_ewma_cfg_r;

typedef struct {
  uint15 *MAX_TH;
  uint15 *MIN_TH;
  uint3 *TC;
  uint4 *W;
  uint8 *UPDATE_INTERVAL;
} cm_aqm_ewma_cfg_r__addr;


void
cm_aqm_ewma_cfg_r__init(
  cm_aqm_ewma_cfg_r *p0,
  cm_aqm_ewma_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_aqm_ewma_cfg_r_MAX_TH__n = 15;
#define cm_aqm_ewma_cfg_r_MAX_TH__nd    15
typedef uint15 cm_aqm_ewma_cfg_r_MAX_TH_t;

static const unsigned int cm_aqm_ewma_cfg_r_MIN_TH__n = 15;
#define cm_aqm_ewma_cfg_r_MIN_TH__nd    15
typedef uint15 cm_aqm_ewma_cfg_r_MIN_TH_t;

static const unsigned int cm_aqm_ewma_cfg_r_TC__n = 3;
#define cm_aqm_ewma_cfg_r_TC__nd    3
typedef uint3 cm_aqm_ewma_cfg_r_TC_t;

static const unsigned int cm_aqm_ewma_cfg_r_W__n = 4;
#define cm_aqm_ewma_cfg_r_W__nd    4
typedef uint4 cm_aqm_ewma_cfg_r_W_t;

static const unsigned int cm_aqm_ewma_cfg_r_UPDATE_INTERVAL__n = 8;
#define cm_aqm_ewma_cfg_r_UPDATE_INTERVAL__nd    8
typedef uint8 cm_aqm_ewma_cfg_r_UPDATE_INTERVAL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 VPRI_BY_VPRI;
} map_vpri_r;

typedef struct {
  uint64 *VPRI_BY_VPRI;
} map_vpri_r__addr;


void
map_vpri_r__init(
  map_vpri_r *p0,
  map_vpri_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_vpri_r_VPRI_BY_VPRI__n = 64;
#define map_vpri_r_VPRI_BY_VPRI__nd    64
typedef uint64 map_vpri_r_VPRI_BY_VPRI_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 MIRROR_DST;
  uint1 TRUNC;
  uint1 PRE_MOD;
  uint1 RECIRCULATE;
  uint1 AUTO_LEARN;
  uint16 METADATA_MASK0;
  uint16 METADATA_VALUE0;
  uint8 METADATA_BYTE_OFFSET0;
  uint5 METADATA_TYPE0;
  uint1 STORE_VSI;
  uint1 STORE_PORT;
} mod_mirror_profile_table0_r;

typedef struct {
  uint12 *MIRROR_DST;
  uint1 *TRUNC;
  uint1 *PRE_MOD;
  uint1 *RECIRCULATE;
  uint1 *AUTO_LEARN;
  uint16 *METADATA_MASK0;
  uint16 *METADATA_VALUE0;
  uint8 *METADATA_BYTE_OFFSET0;
  uint5 *METADATA_TYPE0;
  uint1 *STORE_VSI;
  uint1 *STORE_PORT;
} mod_mirror_profile_table0_r__addr;


void
mod_mirror_profile_table0_r__init(
  mod_mirror_profile_table0_r *p0,
  mod_mirror_profile_table0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_mirror_profile_table0_r_MIRROR_DST__n = 12;
#define mod_mirror_profile_table0_r_MIRROR_DST__nd    12
typedef uint12 mod_mirror_profile_table0_r_MIRROR_DST_t;

static const unsigned int mod_mirror_profile_table0_r_TRUNC__n = 1;
#define mod_mirror_profile_table0_r_TRUNC__nd    1
typedef uint1 mod_mirror_profile_table0_r_TRUNC_t;

static const unsigned int mod_mirror_profile_table0_r_PRE_MOD__n = 1;
#define mod_mirror_profile_table0_r_PRE_MOD__nd    1
typedef uint1 mod_mirror_profile_table0_r_PRE_MOD_t;

static const unsigned int mod_mirror_profile_table0_r_RECIRCULATE__n = 1;
#define mod_mirror_profile_table0_r_RECIRCULATE__nd    1
typedef uint1 mod_mirror_profile_table0_r_RECIRCULATE_t;

static const unsigned int mod_mirror_profile_table0_r_AUTO_LEARN__n = 1;
#define mod_mirror_profile_table0_r_AUTO_LEARN__nd    1
typedef uint1 mod_mirror_profile_table0_r_AUTO_LEARN_t;

static const unsigned int mod_mirror_profile_table0_r_METADATA_MASK0__n = 16;
#define mod_mirror_profile_table0_r_METADATA_MASK0__nd    16
typedef uint16 mod_mirror_profile_table0_r_METADATA_MASK0_t;

static const unsigned int mod_mirror_profile_table0_r_METADATA_VALUE0__n = 16;
#define mod_mirror_profile_table0_r_METADATA_VALUE0__nd    16
typedef uint16 mod_mirror_profile_table0_r_METADATA_VALUE0_t;

static const unsigned int mod_mirror_profile_table0_r_METADATA_BYTE_OFFSET0__n = 8;
#define mod_mirror_profile_table0_r_METADATA_BYTE_OFFSET0__nd    8
typedef uint8 mod_mirror_profile_table0_r_METADATA_BYTE_OFFSET0_t;

static const unsigned int mod_mirror_profile_table0_r_METADATA_TYPE0__n = 5;
#define mod_mirror_profile_table0_r_METADATA_TYPE0__nd    5
typedef uint5 mod_mirror_profile_table0_r_METADATA_TYPE0_t;

static const unsigned int mod_mirror_profile_table0_r_STORE_VSI__n = 1;
#define mod_mirror_profile_table0_r_STORE_VSI__nd    1
typedef uint1 mod_mirror_profile_table0_r_STORE_VSI_t;

static const unsigned int mod_mirror_profile_table0_r_STORE_PORT__n = 1;
#define mod_mirror_profile_table0_r_STORE_PORT__nd    1
typedef uint1 mod_mirror_profile_table0_r_STORE_PORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 SMP0;
  uint1 SMP1;
} cm_pause_gen_state_r;

typedef struct {
  uint1 *SMP0;
  uint1 *SMP1;
} cm_pause_gen_state_r__addr;


void
cm_pause_gen_state_r__init(
  cm_pause_gen_state_r *p0,
  cm_pause_gen_state_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_pause_gen_state_r_SMP0__n = 1;
#define cm_pause_gen_state_r_SMP0__nd    1
typedef uint1 cm_pause_gen_state_r_SMP0_t;

static const unsigned int cm_pause_gen_state_r_SMP1__n = 1;
#define cm_pause_gen_state_r_SMP1__nd    1
typedef uint1 cm_pause_gen_state_r_SMP1_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 DST_MAC;
  uint16 DGLORT;
} nexthop_neighbors_table_0_r;

typedef struct {
  uint48 *DST_MAC;
  uint16 *DGLORT;
} nexthop_neighbors_table_0_r__addr;


void
nexthop_neighbors_table_0_r__init(
  nexthop_neighbors_table_0_r *p0,
  nexthop_neighbors_table_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_neighbors_table_0_r_DST_MAC__n = 48;
#define nexthop_neighbors_table_0_r_DST_MAC__nd    48
typedef uint48 nexthop_neighbors_table_0_r_DST_MAC_t;

static const unsigned int nexthop_neighbors_table_0_r_DGLORT__n = 16;
#define nexthop_neighbors_table_0_r_DGLORT__nd    16
typedef uint16 nexthop_neighbors_table_0_r_DGLORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 INITIAL_OP_ROT;
  uint12 INITIAL_OP_MASK;
  uint16 INITIAL_STATE;
  uint8 INITIAL_PTR;
  uint8 INITIAL_W2_OFFSET;
  uint8 INITIAL_W1_OFFSET;
  uint8 INITIAL_W0_OFFSET;
} parser_port_cfg_r;

typedef struct {
  uint4 *INITIAL_OP_ROT;
  uint12 *INITIAL_OP_MASK;
  uint16 *INITIAL_STATE;
  uint8 *INITIAL_PTR;
  uint8 *INITIAL_W2_OFFSET;
  uint8 *INITIAL_W1_OFFSET;
  uint8 *INITIAL_W0_OFFSET;
} parser_port_cfg_r__addr;


void
parser_port_cfg_r__init(
  parser_port_cfg_r *p0,
  parser_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_port_cfg_r_INITIAL_OP_ROT__n = 4;
#define parser_port_cfg_r_INITIAL_OP_ROT__nd    4
typedef uint4 parser_port_cfg_r_INITIAL_OP_ROT_t;

static const unsigned int parser_port_cfg_r_INITIAL_OP_MASK__n = 12;
#define parser_port_cfg_r_INITIAL_OP_MASK__nd    12
typedef uint12 parser_port_cfg_r_INITIAL_OP_MASK_t;

static const unsigned int parser_port_cfg_r_INITIAL_STATE__n = 16;
#define parser_port_cfg_r_INITIAL_STATE__nd    16
typedef uint16 parser_port_cfg_r_INITIAL_STATE_t;

static const unsigned int parser_port_cfg_r_INITIAL_PTR__n = 8;
#define parser_port_cfg_r_INITIAL_PTR__nd    8
typedef uint8 parser_port_cfg_r_INITIAL_PTR_t;

static const unsigned int parser_port_cfg_r_INITIAL_W2_OFFSET__n = 8;
#define parser_port_cfg_r_INITIAL_W2_OFFSET__nd    8
typedef uint8 parser_port_cfg_r_INITIAL_W2_OFFSET_t;

static const unsigned int parser_port_cfg_r_INITIAL_W1_OFFSET__n = 8;
#define parser_port_cfg_r_INITIAL_W1_OFFSET__nd    8
typedef uint8 parser_port_cfg_r_INITIAL_W1_OFFSET_t;

static const unsigned int parser_port_cfg_r_INITIAL_W0_OFFSET__n = 8;
#define parser_port_cfg_r_INITIAL_W0_OFFSET__nd    8
typedef uint8 parser_port_cfg_r_INITIAL_W0_OFFSET_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY_MASK16;
  uint16 KEY_MASK32;
  uint4 KEY_MASK_PROFILE;
  uint2 SYM_PROFILE;
  uint1 SYMMETRIC;
} entropy_hash_cfg1_r;

typedef struct {
  uint32 *KEY_MASK16;
  uint16 *KEY_MASK32;
  uint4 *KEY_MASK_PROFILE;
  uint2 *SYM_PROFILE;
  uint1 *SYMMETRIC;
} entropy_hash_cfg1_r__addr;


void
entropy_hash_cfg1_r__init(
  entropy_hash_cfg1_r *p0,
  entropy_hash_cfg1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_cfg1_r_KEY_MASK16__n = 32;
#define entropy_hash_cfg1_r_KEY_MASK16__nd    32
typedef uint32 entropy_hash_cfg1_r_KEY_MASK16_t;

static const unsigned int entropy_hash_cfg1_r_KEY_MASK32__n = 16;
#define entropy_hash_cfg1_r_KEY_MASK32__nd    16
typedef uint16 entropy_hash_cfg1_r_KEY_MASK32_t;

static const unsigned int entropy_hash_cfg1_r_KEY_MASK_PROFILE__n = 4;
#define entropy_hash_cfg1_r_KEY_MASK_PROFILE__nd    4
typedef uint4 entropy_hash_cfg1_r_KEY_MASK_PROFILE_t;

static const unsigned int entropy_hash_cfg1_r_SYM_PROFILE__n = 2;
#define entropy_hash_cfg1_r_SYM_PROFILE__nd    2
typedef uint2 entropy_hash_cfg1_r_SYM_PROFILE_t;

static const unsigned int entropy_hash_cfg1_r_SYMMETRIC__n = 1;
#define entropy_hash_cfg1_r_SYMMETRIC__nd    1
typedef uint1 entropy_hash_cfg1_r_SYMMETRIC_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 GLORT_DEST_MASK;
} glort_dest_table_r3;

typedef struct {
  uint64 *GLORT_DEST_MASK;
} glort_dest_table_r3__addr;


void
glort_dest_table_r3__init(
  glort_dest_table_r3 *p0,
  glort_dest_table_r3__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_dest_table_r3_GLORT_DEST_MASK__n = 64;
#define glort_dest_table_r3_GLORT_DEST_MASK__nd    64
typedef uint64 glort_dest_table_r3_GLORT_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 NEW_DEST_MASK;
} trigger_direct_map_adm1_r;

typedef struct {
  uint64 *NEW_DEST_MASK;
} trigger_direct_map_adm1_r__addr;


void
trigger_direct_map_adm1_r__init(
  trigger_direct_map_adm1_r *p0,
  trigger_direct_map_adm1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adm1_r_NEW_DEST_MASK__n = 64;
#define trigger_direct_map_adm1_r_NEW_DEST_MASK__nd    64
typedef uint64 trigger_direct_map_adm1_r_NEW_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MASK;
} em_key_mask_r;

typedef struct {
  uint64 *MASK;
} em_key_mask_r__addr;


void
em_key_mask_r__init(
  em_key_mask_r *p0,
  em_key_mask_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_key_mask_r_MASK__n = 64;
#define em_key_mask_r_MASK__nd    64
typedef uint64 em_key_mask_r_MASK_t;




/* ../src/RegC.m3:221 */
typedef em_key_mask_r em_key_mask_rf[32];

typedef em_key_mask_r__addr em_key_mask_rf__addr[32];


void
em_key_mask_rf__init(
  em_key_mask_rf *p0,
  em_key_mask_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_key_mask_rf_KEY_MASK__n = 32;
#define em_key_mask_rf_KEY_MASK__nd    32



/* ../src/RegC.m3:179 */
typedef struct {
  uint8 SB_TO_SB;
  uint8 EB_TO_SB;
  uint8 WB_TO_SB;
  uint8 NB_TO_NB;
  uint8 EB_TO_NB;
  uint8 WB_TO_NB;
} mesh_arb_rreq_y_r;

typedef struct {
  uint8 *SB_TO_SB;
  uint8 *EB_TO_SB;
  uint8 *WB_TO_SB;
  uint8 *NB_TO_NB;
  uint8 *EB_TO_NB;
  uint8 *WB_TO_NB;
} mesh_arb_rreq_y_r__addr;


void
mesh_arb_rreq_y_r__init(
  mesh_arb_rreq_y_r *p0,
  mesh_arb_rreq_y_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mesh_arb_rreq_y_r_SB_TO_SB__n = 8;
#define mesh_arb_rreq_y_r_SB_TO_SB__nd    8
typedef uint8 mesh_arb_rreq_y_r_SB_TO_SB_t;

static const unsigned int mesh_arb_rreq_y_r_EB_TO_SB__n = 8;
#define mesh_arb_rreq_y_r_EB_TO_SB__nd    8
typedef uint8 mesh_arb_rreq_y_r_EB_TO_SB_t;

static const unsigned int mesh_arb_rreq_y_r_WB_TO_SB__n = 8;
#define mesh_arb_rreq_y_r_WB_TO_SB__nd    8
typedef uint8 mesh_arb_rreq_y_r_WB_TO_SB_t;

static const unsigned int mesh_arb_rreq_y_r_NB_TO_NB__n = 8;
#define mesh_arb_rreq_y_r_NB_TO_NB__nd    8
typedef uint8 mesh_arb_rreq_y_r_NB_TO_NB_t;

static const unsigned int mesh_arb_rreq_y_r_EB_TO_NB__n = 8;
#define mesh_arb_rreq_y_r_EB_TO_NB__nd    8
typedef uint8 mesh_arb_rreq_y_r_EB_TO_NB_t;

static const unsigned int mesh_arb_rreq_y_r_WB_TO_NB__n = 8;
#define mesh_arb_rreq_y_r_WB_TO_NB__nd    8
typedef uint8 mesh_arb_rreq_y_r_WB_TO_NB_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 MIRROR_PROFILE_INDEX0;
  uint6 MIRROR_PROFILE_INDEX1;
  uint6 MIRROR_PROFILE_INDEX2;
  uint6 MIRROR_PROFILE_INDEX3;
} trigger_action_mirror_r;

typedef struct {
  uint6 *MIRROR_PROFILE_INDEX0;
  uint6 *MIRROR_PROFILE_INDEX1;
  uint6 *MIRROR_PROFILE_INDEX2;
  uint6 *MIRROR_PROFILE_INDEX3;
} trigger_action_mirror_r__addr;


void
trigger_action_mirror_r__init(
  trigger_action_mirror_r *p0,
  trigger_action_mirror_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_action_mirror_r_MIRROR_PROFILE_INDEX0__n = 6;
#define trigger_action_mirror_r_MIRROR_PROFILE_INDEX0__nd    6
typedef uint6 trigger_action_mirror_r_MIRROR_PROFILE_INDEX0_t;

static const unsigned int trigger_action_mirror_r_MIRROR_PROFILE_INDEX1__n = 6;
#define trigger_action_mirror_r_MIRROR_PROFILE_INDEX1__nd    6
typedef uint6 trigger_action_mirror_r_MIRROR_PROFILE_INDEX1_t;

static const unsigned int trigger_action_mirror_r_MIRROR_PROFILE_INDEX2__n = 6;
#define trigger_action_mirror_r_MIRROR_PROFILE_INDEX2__nd    6
typedef uint6 trigger_action_mirror_r_MIRROR_PROFILE_INDEX2_t;

static const unsigned int trigger_action_mirror_r_MIRROR_PROFILE_INDEX3__n = 6;
#define trigger_action_mirror_r_MIRROR_PROFILE_INDEX3__nd    6
typedef uint6 trigger_action_mirror_r_MIRROR_PROFILE_INDEX3_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WATERMARK;
} cm_rx_smp_hog_wm_r;

typedef struct {
  uint15 *WATERMARK;
} cm_rx_smp_hog_wm_r__addr;


void
cm_rx_smp_hog_wm_r__init(
  cm_rx_smp_hog_wm_r *p0,
  cm_rx_smp_hog_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_hog_wm_r_WATERMARK__n = 15;
#define cm_rx_smp_hog_wm_r_WATERMARK__nd    15
typedef uint15 cm_rx_smp_hog_wm_r_WATERMARK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 ACTION0;
  uint32 ACTION1;
} wcm_action_r;

typedef struct {
  uint32 *ACTION0;
  uint32 *ACTION1;
} wcm_action_r__addr;


void
wcm_action_r__init(
  wcm_action_r *p0,
  wcm_action_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_action_r_ACTION0__n = 32;
#define wcm_action_r_ACTION0__nd    32
typedef uint32 wcm_action_r_ACTION0_t;

static const unsigned int wcm_action_r_ACTION1__n = 32;
#define wcm_action_r_ACTION1__nd    32
typedef uint32 wcm_action_r_ACTION1_t;




/* ../src/RegC.m3:221 */
typedef wcm_action_r wcm_action_rf[1024];

typedef wcm_action_r__addr wcm_action_rf__addr[1024];


void
wcm_action_rf__init(
  wcm_action_rf *p0,
  wcm_action_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_action_rf_WCM_ACTION__n = 1024;
#define wcm_action_rf_WCM_ACTION__nd    1024



/* ../src/RegC.m3:179 */
typedef struct {
  uint32 PORT;
} tx_voq_port_cfg_r;

typedef struct {
  uint32 *PORT;
} tx_voq_port_cfg_r__addr;


void
tx_voq_port_cfg_r__init(
  tx_voq_port_cfg_r *p0,
  tx_voq_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int tx_voq_port_cfg_r_PORT__n = 32;
#define tx_voq_port_cfg_r_PORT__nd    32
typedef uint32 tx_voq_port_cfg_r_PORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 RSVD;
  uint47 FLAGS;
  uint1 IHL_FITS;
  uint1 IHL_OK;
  uint4 _RSVD0_;
  uint2 CSUM;
  uint3 EX;
  uint1 PTRS_ERR;
  uint4 _RSVD1_;
} map_profile_key0_r;

typedef struct {
  uint1 *RSVD;
  uint47 *FLAGS;
  uint1 *IHL_FITS;
  uint1 *IHL_OK;
  uint4 *_RSVD0_;
  uint2 *CSUM;
  uint3 *EX;
  uint1 *PTRS_ERR;
  uint4 *_RSVD1_;
} map_profile_key0_r__addr;


void
map_profile_key0_r__init(
  map_profile_key0_r *p0,
  map_profile_key0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_profile_key0_r_RSVD__n = 1;
#define map_profile_key0_r_RSVD__nd    1
typedef uint1 map_profile_key0_r_RSVD_t;

static const unsigned int map_profile_key0_r_FLAGS__n = 47;
#define map_profile_key0_r_FLAGS__nd    47
typedef uint47 map_profile_key0_r_FLAGS_t;

static const unsigned int map_profile_key0_r_IHL_FITS__n = 1;
#define map_profile_key0_r_IHL_FITS__nd    1
typedef uint1 map_profile_key0_r_IHL_FITS_t;

static const unsigned int map_profile_key0_r_IHL_OK__n = 1;
#define map_profile_key0_r_IHL_OK__nd    1
typedef uint1 map_profile_key0_r_IHL_OK_t;

static const unsigned int map_profile_key0_r__RSVD0___n = 4;
#define map_profile_key0_r__RSVD0___nd    4
typedef uint4 map_profile_key0_r__RSVD0__t;

static const unsigned int map_profile_key0_r_CSUM__n = 2;
#define map_profile_key0_r_CSUM__nd    2
typedef uint2 map_profile_key0_r_CSUM_t;

static const unsigned int map_profile_key0_r_EX__n = 3;
#define map_profile_key0_r_EX__nd    3
typedef uint3 map_profile_key0_r_EX_t;

static const unsigned int map_profile_key0_r_PTRS_ERR__n = 1;
#define map_profile_key0_r_PTRS_ERR__nd    1
typedef uint1 map_profile_key0_r_PTRS_ERR_t;

static const unsigned int map_profile_key0_r__RSVD1___n = 4;
#define map_profile_key0_r__RSVD1___nd    4
typedef uint4 map_profile_key0_r__RSVD1__t;




/* ../src/RegC.m3:221 */
typedef entropy_hash_cfg1_r entropy_hash_cfg1_rf[64];

typedef entropy_hash_cfg1_r__addr entropy_hash_cfg1_rf__addr[64];


void
entropy_hash_cfg1_rf__init(
  entropy_hash_cfg1_rf *p0,
  entropy_hash_cfg1_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_cfg1_rf_ENTROPY_HASH_CFG1__n = 64;
#define entropy_hash_cfg1_rf_ENTROPY_HASH_CFG1__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint1 PARSING_DONE;
  uint8 EX_OFFSET;
} parser_exc_r;

typedef struct {
  uint1 *PARSING_DONE;
  uint8 *EX_OFFSET;
} parser_exc_r__addr;


void
parser_exc_r__init(
  parser_exc_r *p0,
  parser_exc_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_exc_r_PARSING_DONE__n = 1;
#define parser_exc_r_PARSING_DONE__nd    1
typedef uint1 parser_exc_r_PARSING_DONE_t;

static const unsigned int parser_exc_r_EX_OFFSET__n = 8;
#define parser_exc_r_EX_OFFSET__nd    8
typedef uint8 parser_exc_r_EX_OFFSET_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 KEY_PAIRS;
} entropy_hash_sym32_r;

typedef struct {
  uint64 *KEY_PAIRS;
} entropy_hash_sym32_r__addr;


void
entropy_hash_sym32_r__init(
  entropy_hash_sym32_r *p0,
  entropy_hash_sym32_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_sym32_r_KEY_PAIRS__n = 64;
#define entropy_hash_sym32_r_KEY_PAIRS__nd    64
typedef uint64 entropy_hash_sym32_r_KEY_PAIRS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MIRROR_PORT_MASK1;
} cm_apply_mirror_ecmp_dmask1_r;

typedef struct {
  uint64 *MIRROR_PORT_MASK1;
} cm_apply_mirror_ecmp_dmask1_r__addr;


void
cm_apply_mirror_ecmp_dmask1_r__init(
  cm_apply_mirror_ecmp_dmask1_r *p0,
  cm_apply_mirror_ecmp_dmask1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_mirror_ecmp_dmask1_r_MIRROR_PORT_MASK1__n = 64;
#define cm_apply_mirror_ecmp_dmask1_r_MIRROR_PORT_MASK1__nd    64
typedef uint64 cm_apply_mirror_ecmp_dmask1_r_MIRROR_PORT_MASK1_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 PROT;
  uint3 MAP_PROT;
} map_prot_r;

typedef struct {
  uint8 *PROT;
  uint3 *MAP_PROT;
} map_prot_r__addr;


void
map_prot_r__init(
  map_prot_r *p0,
  map_prot_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_prot_r_PROT__n = 8;
#define map_prot_r_PROT__nd    8
typedef uint8 map_prot_r_PROT_t;

static const unsigned int map_prot_r_MAP_PROT__n = 3;
#define map_prot_r_MAP_PROT__nd    3
typedef uint3 map_prot_r_MAP_PROT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 BYTES;
} nexthop_path_ctrs_r;

typedef struct {
  uint64 *BYTES;
} nexthop_path_ctrs_r__addr;


void
nexthop_path_ctrs_r__init(
  nexthop_path_ctrs_r *p0,
  nexthop_path_ctrs_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_path_ctrs_r_BYTES__n = 64;
#define nexthop_path_ctrs_r_BYTES__nd    64
typedef uint64 nexthop_path_ctrs_r_BYTES_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 MEM_ERROR;
} parser_im_r;

typedef struct {
  uint1 *MEM_ERROR;
} parser_im_r__addr;


void
parser_im_r__init(
  parser_im_r *p0,
  parser_im_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_im_r_MEM_ERROR__n = 1;
#define parser_im_r_MEM_ERROR__nd    1
typedef uint1 parser_im_r_MEM_ERROR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 METADATA_MASK1;
  uint16 METADATA_VALUE1;
  uint8 METADATA_BYTE_OFFSET1;
  uint5 METADATA_TYPE1;
} mod_mirror_profile_table1_r;

typedef struct {
  uint16 *METADATA_MASK1;
  uint16 *METADATA_VALUE1;
  uint8 *METADATA_BYTE_OFFSET1;
  uint5 *METADATA_TYPE1;
} mod_mirror_profile_table1_r__addr;


void
mod_mirror_profile_table1_r__init(
  mod_mirror_profile_table1_r *p0,
  mod_mirror_profile_table1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_mirror_profile_table1_r_METADATA_MASK1__n = 16;
#define mod_mirror_profile_table1_r_METADATA_MASK1__nd    16
typedef uint16 mod_mirror_profile_table1_r_METADATA_MASK1_t;

static const unsigned int mod_mirror_profile_table1_r_METADATA_VALUE1__n = 16;
#define mod_mirror_profile_table1_r_METADATA_VALUE1__nd    16
typedef uint16 mod_mirror_profile_table1_r_METADATA_VALUE1_t;

static const unsigned int mod_mirror_profile_table1_r_METADATA_BYTE_OFFSET1__n = 8;
#define mod_mirror_profile_table1_r_METADATA_BYTE_OFFSET1__nd    8
typedef uint8 mod_mirror_profile_table1_r_METADATA_BYTE_OFFSET1_t;

static const unsigned int mod_mirror_profile_table1_r_METADATA_TYPE1__n = 5;
#define mod_mirror_profile_table1_r_METADATA_TYPE1__nd    5
typedef uint5 mod_mirror_profile_table1_r_METADATA_TYPE1_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 ROOT_PTR;
  uint15 CHILD_BASE_PTR;
  uint2 _RSVD0_;
  uint8 CHILD_PTR_LEN;
} lpm_match_action_r;

typedef struct {
  uint15 *ROOT_PTR;
  uint15 *CHILD_BASE_PTR;
  uint2 *_RSVD0_;
  uint8 *CHILD_PTR_LEN;
} lpm_match_action_r__addr;


void
lpm_match_action_r__init(
  lpm_match_action_r *p0,
  lpm_match_action_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_match_action_r_ROOT_PTR__n = 15;
#define lpm_match_action_r_ROOT_PTR__nd    15
typedef uint15 lpm_match_action_r_ROOT_PTR_t;

static const unsigned int lpm_match_action_r_CHILD_BASE_PTR__n = 15;
#define lpm_match_action_r_CHILD_BASE_PTR__nd    15
typedef uint15 lpm_match_action_r_CHILD_BASE_PTR_t;

static const unsigned int lpm_match_action_r__RSVD0___n = 2;
#define lpm_match_action_r__RSVD0___nd    2
typedef uint2 lpm_match_action_r__RSVD0__t;

static const unsigned int lpm_match_action_r_CHILD_PTR_LEN__n = 8;
#define lpm_match_action_r_CHILD_PTR_LEN__nd    8
typedef uint8 lpm_match_action_r_CHILD_PTR_LEN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint3 JITTER_BITS;
} cm_apply_softdrop_cfg_r;

typedef struct {
  uint3 *JITTER_BITS;
} cm_apply_softdrop_cfg_r__addr;


void
cm_apply_softdrop_cfg_r__init(
  cm_apply_softdrop_cfg_r *p0,
  cm_apply_softdrop_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_softdrop_cfg_r_JITTER_BITS__n = 3;
#define cm_apply_softdrop_cfg_r_JITTER_BITS__nd    3
typedef uint3 cm_apply_softdrop_cfg_r_JITTER_BITS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY;
  uint32 KEY_INVERT;
} parser_ptype_tcam_r;

typedef struct {
  uint32 *KEY;
  uint32 *KEY_INVERT;
} parser_ptype_tcam_r__addr;


void
parser_ptype_tcam_r__init(
  parser_ptype_tcam_r *p0,
  parser_ptype_tcam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ptype_tcam_r_KEY__n = 32;
#define parser_ptype_tcam_r_KEY__nd    32
typedef uint32 parser_ptype_tcam_r_KEY_t;

static const unsigned int parser_ptype_tcam_r_KEY_INVERT__n = 32;
#define parser_ptype_tcam_r_KEY_INVERT__nd    32
typedef uint32 parser_ptype_tcam_r_KEY_INVERT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint14 FLOWLET;
} nexthop_status_r;

typedef struct {
  uint14 *FLOWLET;
} nexthop_status_r__addr;


void
nexthop_status_r__init(
  nexthop_status_r *p0,
  nexthop_status_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_status_r_FLOWLET__n = 14;
#define nexthop_status_r_FLOWLET__nd    14
typedef uint14 nexthop_status_r_FLOWLET_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 PORT;
} rx_voq_port_cfg_r;

typedef struct {
  uint32 *PORT;
} rx_voq_port_cfg_r__addr;


void
rx_voq_port_cfg_r__init(
  rx_voq_port_cfg_r *p0,
  rx_voq_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_voq_port_cfg_r_PORT__n = 32;
#define rx_voq_port_cfg_r_PORT__nd    32
typedef uint32 rx_voq_port_cfg_r_PORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint5 PRIORITY_PROFILE;
} map_domain_profile_r;

typedef struct {
  uint5 *PRIORITY_PROFILE;
} map_domain_profile_r__addr;


void
map_domain_profile_r__init(
  map_domain_profile_r *p0,
  map_domain_profile_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_domain_profile_r_PRIORITY_PROFILE__n = 5;
#define map_domain_profile_r_PRIORITY_PROFILE__nd    5
typedef uint5 map_domain_profile_r_PRIORITY_PROFILE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MASK;
} em_hash_cam_en_r;

typedef struct {
  uint64 *MASK;
} em_hash_cam_en_r__addr;


void
em_hash_cam_en_r__init(
  em_hash_cam_en_r *p0,
  em_hash_cam_en_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_cam_en_r_MASK__n = 64;
#define em_hash_cam_en_r_MASK__nd    64
typedef uint64 em_hash_cam_en_r_MASK_t;




/* ../src/RegC.m3:221 */
typedef em_hash_cam_en_r em_hash_cam_en_rf[32];

typedef em_hash_cam_en_r__addr em_hash_cam_en_rf__addr[32];


void
em_hash_cam_en_rf__init(
  em_hash_cam_en_rf *p0,
  em_hash_cam_en_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_cam_en_rf_HASH_CAM_EN__n = 32;
#define em_hash_cam_en_rf_HASH_CAM_EN__nd    32



/* ../src/RegC.m3:179 */
typedef struct {
  uint12 EVID;
  uint3 MTU_INDEX;
  uint24 MOD_IDX;
  uint8 L2_DOMAIN;
  uint6 L3_DOMAIN;
  uint1 UPDATE_L2_DOMAIN;
  uint1 UPDATE_L3_DOMAIN;
  uint1 MARK_ROUTED;
  uint1 IPV6_ENTRY;
  uint1 ENTRY_TYPE;
} nexthop_neighbors_table_1_r;

typedef struct {
  uint12 *EVID;
  uint3 *MTU_INDEX;
  uint24 *MOD_IDX;
  uint8 *L2_DOMAIN;
  uint6 *L3_DOMAIN;
  uint1 *UPDATE_L2_DOMAIN;
  uint1 *UPDATE_L3_DOMAIN;
  uint1 *MARK_ROUTED;
  uint1 *IPV6_ENTRY;
  uint1 *ENTRY_TYPE;
} nexthop_neighbors_table_1_r__addr;


void
nexthop_neighbors_table_1_r__init(
  nexthop_neighbors_table_1_r *p0,
  nexthop_neighbors_table_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_neighbors_table_1_r_EVID__n = 12;
#define nexthop_neighbors_table_1_r_EVID__nd    12
typedef uint12 nexthop_neighbors_table_1_r_EVID_t;

static const unsigned int nexthop_neighbors_table_1_r_MTU_INDEX__n = 3;
#define nexthop_neighbors_table_1_r_MTU_INDEX__nd    3
typedef uint3 nexthop_neighbors_table_1_r_MTU_INDEX_t;

static const unsigned int nexthop_neighbors_table_1_r_MOD_IDX__n = 24;
#define nexthop_neighbors_table_1_r_MOD_IDX__nd    24
typedef uint24 nexthop_neighbors_table_1_r_MOD_IDX_t;

static const unsigned int nexthop_neighbors_table_1_r_L2_DOMAIN__n = 8;
#define nexthop_neighbors_table_1_r_L2_DOMAIN__nd    8
typedef uint8 nexthop_neighbors_table_1_r_L2_DOMAIN_t;

static const unsigned int nexthop_neighbors_table_1_r_L3_DOMAIN__n = 6;
#define nexthop_neighbors_table_1_r_L3_DOMAIN__nd    6
typedef uint6 nexthop_neighbors_table_1_r_L3_DOMAIN_t;

static const unsigned int nexthop_neighbors_table_1_r_UPDATE_L2_DOMAIN__n = 1;
#define nexthop_neighbors_table_1_r_UPDATE_L2_DOMAIN__nd    1
typedef uint1 nexthop_neighbors_table_1_r_UPDATE_L2_DOMAIN_t;

static const unsigned int nexthop_neighbors_table_1_r_UPDATE_L3_DOMAIN__n = 1;
#define nexthop_neighbors_table_1_r_UPDATE_L3_DOMAIN__nd    1
typedef uint1 nexthop_neighbors_table_1_r_UPDATE_L3_DOMAIN_t;

static const unsigned int nexthop_neighbors_table_1_r_MARK_ROUTED__n = 1;
#define nexthop_neighbors_table_1_r_MARK_ROUTED__nd    1
typedef uint1 nexthop_neighbors_table_1_r_MARK_ROUTED_t;

static const unsigned int nexthop_neighbors_table_1_r_IPV6_ENTRY__n = 1;
#define nexthop_neighbors_table_1_r_IPV6_ENTRY__nd    1
typedef uint1 nexthop_neighbors_table_1_r_IPV6_ENTRY_t;

static const unsigned int nexthop_neighbors_table_1_r_ENTRY_TYPE__n = 1;
#define nexthop_neighbors_table_1_r_ENTRY_TYPE__nd    1
typedef uint1 nexthop_neighbors_table_1_r_ENTRY_TYPE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DROP_MASK;
} trigger_direct_map_adr0_r;

typedef struct {
  uint64 *DROP_MASK;
} trigger_direct_map_adr0_r__addr;


void
trigger_direct_map_adr0_r__init(
  trigger_direct_map_adr0_r *p0,
  trigger_direct_map_adr0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adr0_r_DROP_MASK__n = 64;
#define trigger_direct_map_adr0_r_DROP_MASK__nd    64
typedef uint64 trigger_direct_map_adr0_r_DROP_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY_MASK16;
  uint16 KEY_MASK32;
  uint4 KEY_MASK_PROFILE;
  uint2 SYM_PROFILE;
  uint1 SYMMETRIC;
} entropy_hash_cfg2_r;

typedef struct {
  uint32 *KEY_MASK16;
  uint16 *KEY_MASK32;
  uint4 *KEY_MASK_PROFILE;
  uint2 *SYM_PROFILE;
  uint1 *SYMMETRIC;
} entropy_hash_cfg2_r__addr;


void
entropy_hash_cfg2_r__init(
  entropy_hash_cfg2_r *p0,
  entropy_hash_cfg2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_cfg2_r_KEY_MASK16__n = 32;
#define entropy_hash_cfg2_r_KEY_MASK16__nd    32
typedef uint32 entropy_hash_cfg2_r_KEY_MASK16_t;

static const unsigned int entropy_hash_cfg2_r_KEY_MASK32__n = 16;
#define entropy_hash_cfg2_r_KEY_MASK32__nd    16
typedef uint16 entropy_hash_cfg2_r_KEY_MASK32_t;

static const unsigned int entropy_hash_cfg2_r_KEY_MASK_PROFILE__n = 4;
#define entropy_hash_cfg2_r_KEY_MASK_PROFILE__nd    4
typedef uint4 entropy_hash_cfg2_r_KEY_MASK_PROFILE_t;

static const unsigned int entropy_hash_cfg2_r_SYM_PROFILE__n = 2;
#define entropy_hash_cfg2_r_SYM_PROFILE__nd    2
typedef uint2 entropy_hash_cfg2_r_SYM_PROFILE_t;

static const unsigned int entropy_hash_cfg2_r_SYMMETRIC__n = 1;
#define entropy_hash_cfg2_r_SYMMETRIC__nd    1
typedef uint1 entropy_hash_cfg2_r_SYMMETRIC_t;




/* ../src/RegC.m3:221 */
typedef parser_exc_r parser_exc_rf[16];

typedef parser_exc_r__addr parser_exc_rf__addr[16];


void
parser_exc_rf__init(
  parser_exc_rf *p0,
  parser_exc_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_exc_rf_PARSER_EXC__n = 16;
#define parser_exc_rf_PARSER_EXC__nd    16



/* ../src/RegC.m3:179 */
typedef struct {
  uint3 DEFAULT_PRI;
  uint1 FORCE_DEFAULT_PRI;
  uint8 PRI_SOURCE;
  uint5 PRIORITY_PROFILE;
  uint1 LEARN_MODE;
  uint1 LEARN_EN;
  uint1 UPDATE_DOMAINS;
  uint4 NAD;
  uint9 L3_DOMAIN;
  uint8 L2_DOMAIN;
} map_domain_action0_r;

typedef struct {
  uint3 *DEFAULT_PRI;
  uint1 *FORCE_DEFAULT_PRI;
  uint8 *PRI_SOURCE;
  uint5 *PRIORITY_PROFILE;
  uint1 *LEARN_MODE;
  uint1 *LEARN_EN;
  uint1 *UPDATE_DOMAINS;
  uint4 *NAD;
  uint9 *L3_DOMAIN;
  uint8 *L2_DOMAIN;
} map_domain_action0_r__addr;


void
map_domain_action0_r__init(
  map_domain_action0_r *p0,
  map_domain_action0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_domain_action0_r_DEFAULT_PRI__n = 3;
#define map_domain_action0_r_DEFAULT_PRI__nd    3
typedef uint3 map_domain_action0_r_DEFAULT_PRI_t;

static const unsigned int map_domain_action0_r_FORCE_DEFAULT_PRI__n = 1;
#define map_domain_action0_r_FORCE_DEFAULT_PRI__nd    1
typedef uint1 map_domain_action0_r_FORCE_DEFAULT_PRI_t;

static const unsigned int map_domain_action0_r_PRI_SOURCE__n = 8;
#define map_domain_action0_r_PRI_SOURCE__nd    8
typedef uint8 map_domain_action0_r_PRI_SOURCE_t;

static const unsigned int map_domain_action0_r_PRIORITY_PROFILE__n = 5;
#define map_domain_action0_r_PRIORITY_PROFILE__nd    5
typedef uint5 map_domain_action0_r_PRIORITY_PROFILE_t;

static const unsigned int map_domain_action0_r_LEARN_MODE__n = 1;
#define map_domain_action0_r_LEARN_MODE__nd    1
typedef uint1 map_domain_action0_r_LEARN_MODE_t;

static const unsigned int map_domain_action0_r_LEARN_EN__n = 1;
#define map_domain_action0_r_LEARN_EN__nd    1
typedef uint1 map_domain_action0_r_LEARN_EN_t;

static const unsigned int map_domain_action0_r_UPDATE_DOMAINS__n = 1;
#define map_domain_action0_r_UPDATE_DOMAINS__nd    1
typedef uint1 map_domain_action0_r_UPDATE_DOMAINS_t;

static const unsigned int map_domain_action0_r_NAD__n = 4;
#define map_domain_action0_r_NAD__nd    4
typedef uint4 map_domain_action0_r_NAD_t;

static const unsigned int map_domain_action0_r_L3_DOMAIN__n = 9;
#define map_domain_action0_r_L3_DOMAIN__nd    9
typedef uint9 map_domain_action0_r_L3_DOMAIN_t;

static const unsigned int map_domain_action0_r_L2_DOMAIN__n = 8;
#define map_domain_action0_r_L2_DOMAIN__nd    8
typedef uint8 map_domain_action0_r_L2_DOMAIN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 NEW_DEST_MASK;
} trigger_direct_map_adm2_r;

typedef struct {
  uint64 *NEW_DEST_MASK;
} trigger_direct_map_adm2_r__addr;


void
trigger_direct_map_adm2_r__init(
  trigger_direct_map_adm2_r *p0,
  trigger_direct_map_adm2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adm2_r_NEW_DEST_MASK__n = 64;
#define trigger_direct_map_adm2_r_NEW_DEST_MASK__nd    64
typedef uint64 trigger_direct_map_adm2_r_NEW_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 IP_HI;
} map_ip_hi_r;

typedef struct {
  uint64 *IP_HI;
} map_ip_hi_r__addr;


void
map_ip_hi_r__init(
  map_ip_hi_r *p0,
  map_ip_hi_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_ip_hi_r_IP_HI__n = 64;
#define map_ip_hi_r_IP_HI__nd    64
typedef uint64 map_ip_hi_r_IP_HI_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint24 DROP_MASK;
} trigger_rate_lim_cfg_2_r;

typedef struct {
  uint24 *DROP_MASK;
} trigger_rate_lim_cfg_2_r__addr;


void
trigger_rate_lim_cfg_2_r__init(
  trigger_rate_lim_cfg_2_r *p0,
  trigger_rate_lim_cfg_2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_rate_lim_cfg_2_r_DROP_MASK__n = 24;
#define trigger_rate_lim_cfg_2_r_DROP_MASK__nd    24
typedef uint24 trigger_rate_lim_cfg_2_r_DROP_MASK_t;




/* ../src/RegC.m3:221 */
typedef em_key_sel0_r em_key_sel0_rf[64];

typedef em_key_sel0_r__addr em_key_sel0_rf__addr[64];


void
em_key_sel0_rf__init(
  em_key_sel0_rf *p0,
  em_key_sel0_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_key_sel0_rf_KEY_SEL0__n = 64;
#define em_key_sel0_rf_KEY_SEL0__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint16 NEXT_OP;
  uint16 NEXT_STATE_MASK;
  uint16 NEXT_STATE;
} parser_ana_s_r;

typedef struct {
  uint16 *NEXT_OP;
  uint16 *NEXT_STATE_MASK;
  uint16 *NEXT_STATE;
} parser_ana_s_r__addr;


void
parser_ana_s_r__init(
  parser_ana_s_r *p0,
  parser_ana_s_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ana_s_r_NEXT_OP__n = 16;
#define parser_ana_s_r_NEXT_OP__nd    16
typedef uint16 parser_ana_s_r_NEXT_OP_t;

static const unsigned int parser_ana_s_r_NEXT_STATE_MASK__n = 16;
#define parser_ana_s_r_NEXT_STATE_MASK__nd    16
typedef uint16 parser_ana_s_r_NEXT_STATE_MASK_t;

static const unsigned int parser_ana_s_r_NEXT_STATE__n = 16;
#define parser_ana_s_r_NEXT_STATE__nd    16
typedef uint16 parser_ana_s_r_NEXT_STATE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 ADDR_KEY16_SEL;
} lpm_key_sel2_r;

typedef struct {
  uint64 *ADDR_KEY16_SEL;
} lpm_key_sel2_r__addr;


void
lpm_key_sel2_r__init(
  lpm_key_sel2_r *p0,
  lpm_key_sel2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_key_sel2_r_ADDR_KEY16_SEL__n = 64;
#define lpm_key_sel2_r_ADDR_KEY16_SEL__nd    64
typedef uint64 lpm_key_sel2_r_ADDR_KEY16_SEL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 IDX0_SHIFT_RIGHT;
  uint4 IDX0_MASK_LEN;
  uint3 IDX1_SHIFT_RIGHT;
  uint3 IDX1_MASK_LEN;
  uint16 OUTPUT_MASK;
  uint4 OUTPUT_SHIFT;
} mod_map_dual_cfg_r;

typedef struct {
  uint4 *IDX0_SHIFT_RIGHT;
  uint4 *IDX0_MASK_LEN;
  uint3 *IDX1_SHIFT_RIGHT;
  uint3 *IDX1_MASK_LEN;
  uint16 *OUTPUT_MASK;
  uint4 *OUTPUT_SHIFT;
} mod_map_dual_cfg_r__addr;


void
mod_map_dual_cfg_r__init(
  mod_map_dual_cfg_r *p0,
  mod_map_dual_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_map_dual_cfg_r_IDX0_SHIFT_RIGHT__n = 4;
#define mod_map_dual_cfg_r_IDX0_SHIFT_RIGHT__nd    4
typedef uint4 mod_map_dual_cfg_r_IDX0_SHIFT_RIGHT_t;

static const unsigned int mod_map_dual_cfg_r_IDX0_MASK_LEN__n = 4;
#define mod_map_dual_cfg_r_IDX0_MASK_LEN__nd    4
typedef uint4 mod_map_dual_cfg_r_IDX0_MASK_LEN_t;

static const unsigned int mod_map_dual_cfg_r_IDX1_SHIFT_RIGHT__n = 3;
#define mod_map_dual_cfg_r_IDX1_SHIFT_RIGHT__nd    3
typedef uint3 mod_map_dual_cfg_r_IDX1_SHIFT_RIGHT_t;

static const unsigned int mod_map_dual_cfg_r_IDX1_MASK_LEN__n = 3;
#define mod_map_dual_cfg_r_IDX1_MASK_LEN__nd    3
typedef uint3 mod_map_dual_cfg_r_IDX1_MASK_LEN_t;

static const unsigned int mod_map_dual_cfg_r_OUTPUT_MASK__n = 16;
#define mod_map_dual_cfg_r_OUTPUT_MASK__nd    16
typedef uint16 mod_map_dual_cfg_r_OUTPUT_MASK_t;

static const unsigned int mod_map_dual_cfg_r_OUTPUT_SHIFT__n = 4;
#define mod_map_dual_cfg_r_OUTPUT_SHIFT__nd    4
typedef uint4 mod_map_dual_cfg_r_OUTPUT_SHIFT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 IPV4_0;
  uint8 IPV4_1;
  uint8 IPV4_2;
  uint8 IPV6_0;
  uint8 IPV6_1;
  uint8 IPV6_2;
} mod_csum_cfg1_r;

typedef struct {
  uint8 *IPV4_0;
  uint8 *IPV4_1;
  uint8 *IPV4_2;
  uint8 *IPV6_0;
  uint8 *IPV6_1;
  uint8 *IPV6_2;
} mod_csum_cfg1_r__addr;


void
mod_csum_cfg1_r__init(
  mod_csum_cfg1_r *p0,
  mod_csum_cfg1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_csum_cfg1_r_IPV4_0__n = 8;
#define mod_csum_cfg1_r_IPV4_0__nd    8
typedef uint8 mod_csum_cfg1_r_IPV4_0_t;

static const unsigned int mod_csum_cfg1_r_IPV4_1__n = 8;
#define mod_csum_cfg1_r_IPV4_1__nd    8
typedef uint8 mod_csum_cfg1_r_IPV4_1_t;

static const unsigned int mod_csum_cfg1_r_IPV4_2__n = 8;
#define mod_csum_cfg1_r_IPV4_2__nd    8
typedef uint8 mod_csum_cfg1_r_IPV4_2_t;

static const unsigned int mod_csum_cfg1_r_IPV6_0__n = 8;
#define mod_csum_cfg1_r_IPV6_0__nd    8
typedef uint8 mod_csum_cfg1_r_IPV6_0_t;

static const unsigned int mod_csum_cfg1_r_IPV6_1__n = 8;
#define mod_csum_cfg1_r_IPV6_1__nd    8
typedef uint8 mod_csum_cfg1_r_IPV6_1_t;

static const unsigned int mod_csum_cfg1_r_IPV6_2__n = 8;
#define mod_csum_cfg1_r_IPV6_2__nd    8
typedef uint8 mod_csum_cfg1_r_IPV6_2_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 USED;
} cgrp_used_table_r;

typedef struct {
  uint64 *USED;
} cgrp_used_table_r__addr;


void
cgrp_used_table_r__init(
  cgrp_used_table_r *p0,
  cgrp_used_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cgrp_used_table_r_USED__n = 64;
#define cgrp_used_table_r_USED__nd    64
typedef uint64 cgrp_used_table_r_USED_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 ACTION0;
  uint32 ACTION1;
} em_hash_miss_r;

typedef struct {
  uint32 *ACTION0;
  uint32 *ACTION1;
} em_hash_miss_r__addr;


void
em_hash_miss_r__init(
  em_hash_miss_r *p0,
  em_hash_miss_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_miss_r_ACTION0__n = 32;
#define em_hash_miss_r_ACTION0__nd    32
typedef uint32 em_hash_miss_r_ACTION0_t;

static const unsigned int em_hash_miss_r_ACTION1__n = 32;
#define em_hash_miss_r_ACTION1__nd    32
typedef uint32 em_hash_miss_r_ACTION1_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 PAUSE_ON;
  uint15 PAUSE_OFF;
} cm_rx_smp_pause_wm_r;

typedef struct {
  uint15 *PAUSE_ON;
  uint15 *PAUSE_OFF;
} cm_rx_smp_pause_wm_r__addr;


void
cm_rx_smp_pause_wm_r__init(
  cm_rx_smp_pause_wm_r *p0,
  cm_rx_smp_pause_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_pause_wm_r_PAUSE_ON__n = 15;
#define cm_rx_smp_pause_wm_r_PAUSE_ON__nd    15
typedef uint15 cm_rx_smp_pause_wm_r_PAUSE_ON_t;

static const unsigned int cm_rx_smp_pause_wm_r_PAUSE_OFF__n = 15;
#define cm_rx_smp_pause_wm_r_PAUSE_OFF__nd    15
typedef uint15 cm_rx_smp_pause_wm_r_PAUSE_OFF_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 W0_MASK;
  uint16 W0_VALUE;
  uint16 W1_MASK;
  uint16 W1_VALUE;
} parser_key_w_r;

typedef struct {
  uint16 *W0_MASK;
  uint16 *W0_VALUE;
  uint16 *W1_MASK;
  uint16 *W1_VALUE;
} parser_key_w_r__addr;


void
parser_key_w_r__init(
  parser_key_w_r *p0,
  parser_key_w_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_key_w_r_W0_MASK__n = 16;
#define parser_key_w_r_W0_MASK__nd    16
typedef uint16 parser_key_w_r_W0_MASK_t;

static const unsigned int parser_key_w_r_W0_VALUE__n = 16;
#define parser_key_w_r_W0_VALUE__nd    16
typedef uint16 parser_key_w_r_W0_VALUE_t;

static const unsigned int parser_key_w_r_W1_MASK__n = 16;
#define parser_key_w_r_W1_MASK__nd    16
typedef uint16 parser_key_w_r_W1_MASK_t;

static const unsigned int parser_key_w_r_W1_VALUE__n = 16;
#define parser_key_w_r_W1_VALUE__nd    16
typedef uint16 parser_key_w_r_W1_VALUE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 MAC_MBCAST;
  uint4 MAC_ROUTABLE;
  uint8 DOMAIN_PROFILE;
  uint8 PORT_PROFILE;
  uint1 _RSVD0_;
  uint9 L3_DOMAIN;
  uint8 L2_DOMAIN;
  uint10 PTYPE;
  uint14 _RSVD1_;
} map_profile_key1_r;

typedef struct {
  uint2 *MAC_MBCAST;
  uint4 *MAC_ROUTABLE;
  uint8 *DOMAIN_PROFILE;
  uint8 *PORT_PROFILE;
  uint1 *_RSVD0_;
  uint9 *L3_DOMAIN;
  uint8 *L2_DOMAIN;
  uint10 *PTYPE;
  uint14 *_RSVD1_;
} map_profile_key1_r__addr;


void
map_profile_key1_r__init(
  map_profile_key1_r *p0,
  map_profile_key1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_profile_key1_r_MAC_MBCAST__n = 2;
#define map_profile_key1_r_MAC_MBCAST__nd    2
typedef uint2 map_profile_key1_r_MAC_MBCAST_t;

static const unsigned int map_profile_key1_r_MAC_ROUTABLE__n = 4;
#define map_profile_key1_r_MAC_ROUTABLE__nd    4
typedef uint4 map_profile_key1_r_MAC_ROUTABLE_t;

static const unsigned int map_profile_key1_r_DOMAIN_PROFILE__n = 8;
#define map_profile_key1_r_DOMAIN_PROFILE__nd    8
typedef uint8 map_profile_key1_r_DOMAIN_PROFILE_t;

static const unsigned int map_profile_key1_r_PORT_PROFILE__n = 8;
#define map_profile_key1_r_PORT_PROFILE__nd    8
typedef uint8 map_profile_key1_r_PORT_PROFILE_t;

static const unsigned int map_profile_key1_r__RSVD0___n = 1;
#define map_profile_key1_r__RSVD0___nd    1
typedef uint1 map_profile_key1_r__RSVD0__t;

static const unsigned int map_profile_key1_r_L3_DOMAIN__n = 9;
#define map_profile_key1_r_L3_DOMAIN__nd    9
typedef uint9 map_profile_key1_r_L3_DOMAIN_t;

static const unsigned int map_profile_key1_r_L2_DOMAIN__n = 8;
#define map_profile_key1_r_L2_DOMAIN__nd    8
typedef uint8 map_profile_key1_r_L2_DOMAIN_t;

static const unsigned int map_profile_key1_r_PTYPE__n = 10;
#define map_profile_key1_r_PTYPE__nd    10
typedef uint10 map_profile_key1_r_PTYPE_t;

static const unsigned int map_profile_key1_r__RSVD1___n = 14;
#define map_profile_key1_r__RSVD1___nd    14
typedef uint14 map_profile_key1_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 VALUE0;
  uint8 VALUE1;
  uint8 VALUE2;
  uint8 VALUE3;
  uint8 VALUE4;
  uint8 VALUE5;
  uint8 VALUE6;
  uint8 VALUE7;
} mod_map_dual_r;

typedef struct {
  uint8 *VALUE0;
  uint8 *VALUE1;
  uint8 *VALUE2;
  uint8 *VALUE3;
  uint8 *VALUE4;
  uint8 *VALUE5;
  uint8 *VALUE6;
  uint8 *VALUE7;
} mod_map_dual_r__addr;


void
mod_map_dual_r__init(
  mod_map_dual_r *p0,
  mod_map_dual_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_map_dual_r_VALUE0__n = 8;
#define mod_map_dual_r_VALUE0__nd    8
typedef uint8 mod_map_dual_r_VALUE0_t;

static const unsigned int mod_map_dual_r_VALUE1__n = 8;
#define mod_map_dual_r_VALUE1__nd    8
typedef uint8 mod_map_dual_r_VALUE1_t;

static const unsigned int mod_map_dual_r_VALUE2__n = 8;
#define mod_map_dual_r_VALUE2__nd    8
typedef uint8 mod_map_dual_r_VALUE2_t;

static const unsigned int mod_map_dual_r_VALUE3__n = 8;
#define mod_map_dual_r_VALUE3__nd    8
typedef uint8 mod_map_dual_r_VALUE3_t;

static const unsigned int mod_map_dual_r_VALUE4__n = 8;
#define mod_map_dual_r_VALUE4__nd    8
typedef uint8 mod_map_dual_r_VALUE4_t;

static const unsigned int mod_map_dual_r_VALUE5__n = 8;
#define mod_map_dual_r_VALUE5__nd    8
typedef uint8 mod_map_dual_r_VALUE5_t;

static const unsigned int mod_map_dual_r_VALUE6__n = 8;
#define mod_map_dual_r_VALUE6__nd    8
typedef uint8 mod_map_dual_r_VALUE6_t;

static const unsigned int mod_map_dual_r_VALUE7__n = 8;
#define mod_map_dual_r_VALUE7__nd    8
typedef uint8 mod_map_dual_r_VALUE7_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_smp_usage_r;

typedef struct {
  uint16 *COUNT;
} cm_smp_usage_r__addr;


void
cm_smp_usage_r__init(
  cm_smp_usage_r *p0,
  cm_smp_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_smp_usage_r_COUNT__n = 16;
#define cm_smp_usage_r_COUNT__nd    16
typedef uint16 cm_smp_usage_r_COUNT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 SMP_0;
  uint1 SMP_1;
  uint1 SMP_2;
  uint1 SMP_3;
  uint1 SMP_4;
  uint1 SMP_5;
  uint1 SMP_6;
  uint1 SMP_7;
} cm_sweeper_tc_to_smp_r;

typedef struct {
  uint1 *SMP_0;
  uint1 *SMP_1;
  uint1 *SMP_2;
  uint1 *SMP_3;
  uint1 *SMP_4;
  uint1 *SMP_5;
  uint1 *SMP_6;
  uint1 *SMP_7;
} cm_sweeper_tc_to_smp_r__addr;


void
cm_sweeper_tc_to_smp_r__init(
  cm_sweeper_tc_to_smp_r *p0,
  cm_sweeper_tc_to_smp_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_0__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_0__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_0_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_1__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_1__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_1_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_2__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_2__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_2_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_3__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_3__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_3_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_4__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_4__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_4_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_5__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_5__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_5_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_6__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_6__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_6_t;

static const unsigned int cm_sweeper_tc_to_smp_r_SMP_7__n = 1;
#define cm_sweeper_tc_to_smp_r_SMP_7__nd    1
typedef uint1 cm_sweeper_tc_to_smp_r_SMP_7_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 MASK;
  uint4 SELECT_0;
  uint4 SELECT_1;
  uint4 SELECT_2;
  uint4 SELECT_3;
  uint4 SELECT_4;
  uint12 RSVD1_;
  uint20 PTR;
} em_hash_lookup_r;

typedef struct {
  uint32 *MASK;
  uint4 *SELECT_0;
  uint4 *SELECT_1;
  uint4 *SELECT_2;
  uint4 *SELECT_3;
  uint4 *SELECT_4;
  uint12 *RSVD1_;
  uint20 *PTR;
} em_hash_lookup_r__addr;


void
em_hash_lookup_r__init(
  em_hash_lookup_r *p0,
  em_hash_lookup_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_lookup_r_MASK__n = 32;
#define em_hash_lookup_r_MASK__nd    32
typedef uint32 em_hash_lookup_r_MASK_t;

static const unsigned int em_hash_lookup_r_SELECT_0__n = 4;
#define em_hash_lookup_r_SELECT_0__nd    4
typedef uint4 em_hash_lookup_r_SELECT_0_t;

static const unsigned int em_hash_lookup_r_SELECT_1__n = 4;
#define em_hash_lookup_r_SELECT_1__nd    4
typedef uint4 em_hash_lookup_r_SELECT_1_t;

static const unsigned int em_hash_lookup_r_SELECT_2__n = 4;
#define em_hash_lookup_r_SELECT_2__nd    4
typedef uint4 em_hash_lookup_r_SELECT_2_t;

static const unsigned int em_hash_lookup_r_SELECT_3__n = 4;
#define em_hash_lookup_r_SELECT_3__nd    4
typedef uint4 em_hash_lookup_r_SELECT_3_t;

static const unsigned int em_hash_lookup_r_SELECT_4__n = 4;
#define em_hash_lookup_r_SELECT_4__nd    4
typedef uint4 em_hash_lookup_r_SELECT_4_t;

static const unsigned int em_hash_lookup_r_RSVD1___n = 12;
#define em_hash_lookup_r_RSVD1___nd    12
typedef uint12 em_hash_lookup_r_RSVD1__t;

static const unsigned int em_hash_lookup_r_PTR__n = 20;
#define em_hash_lookup_r_PTR__nd    20
typedef uint20 em_hash_lookup_r_PTR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint9 USAGE;
} ma_tcn_usage_r;

typedef struct {
  uint9 *USAGE;
} ma_tcn_usage_r__addr;


void
ma_tcn_usage_r__init(
  ma_tcn_usage_r *p0,
  ma_tcn_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_usage_r_USAGE__n = 9;
#define ma_tcn_usage_r_USAGE__nd    9
typedef uint9 ma_tcn_usage_r_USAGE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MIRROR_PORT_MASK2;
} cm_apply_mirror_ecmp_dmask2_r;

typedef struct {
  uint64 *MIRROR_PORT_MASK2;
} cm_apply_mirror_ecmp_dmask2_r__addr;


void
cm_apply_mirror_ecmp_dmask2_r__init(
  cm_apply_mirror_ecmp_dmask2_r *p0,
  cm_apply_mirror_ecmp_dmask2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_mirror_ecmp_dmask2_r_MIRROR_PORT_MASK2__n = 64;
#define cm_apply_mirror_ecmp_dmask2_r_MIRROR_PORT_MASK2__nd    64
typedef uint64 cm_apply_mirror_ecmp_dmask2_r_MIRROR_PORT_MASK2_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MEMBERSHIP;
} egress_vid_table_r;

typedef struct {
  uint64 *MEMBERSHIP;
} egress_vid_table_r__addr;


void
egress_vid_table_r__init(
  egress_vid_table_r *p0,
  egress_vid_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int egress_vid_table_r_MEMBERSHIP__n = 64;
#define egress_vid_table_r_MEMBERSHIP__nd    64
typedef uint64 egress_vid_table_r_MEMBERSHIP_t;




/* ../src/RegC.m3:221 */
typedef em_hash_miss_r em_hash_miss_rf[64];

typedef em_hash_miss_r__addr em_hash_miss_rf__addr[64];


void
em_hash_miss_rf__init(
  em_hash_miss_rf *p0,
  em_hash_miss_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_miss_rf_HASH_MISS__n = 64;
#define em_hash_miss_rf_HASH_MISS__nd    64



/* ../src/RegC.m3:221 */
typedef parser_ptype_tcam_r parser_ptype_tcam_rf[64];

typedef parser_ptype_tcam_r__addr parser_ptype_tcam_rf__addr[64];


void
parser_ptype_tcam_rf__init(
  parser_ptype_tcam_rf *p0,
  parser_ptype_tcam_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ptype_tcam_rf_PARSER_PTYPE_TCAM__n = 64;
#define parser_ptype_tcam_rf_PARSER_PTYPE_TCAM__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_1_0_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_1_0_r__addr;


void
fwd_port_cfg_1_0_r__init(
  fwd_port_cfg_1_0_r *p0,
  fwd_port_cfg_1_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_1_0_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_1_0_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_1_0_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 FCMN_SHELL_CTRL_ERR;
  uint1 TCAM_SWEEP_ERR;
  uint3 FGRP_ERR;
  uint3 FGHASH_ERR;
  uint4 HASH_ENTRY_RAM_U_ERR;
  uint4 HASH_ENTRY_RAM_C_ERR;
} wcm_ip_r;

typedef struct {
  uint1 *FCMN_SHELL_CTRL_ERR;
  uint1 *TCAM_SWEEP_ERR;
  uint3 *FGRP_ERR;
  uint3 *FGHASH_ERR;
  uint4 *HASH_ENTRY_RAM_U_ERR;
  uint4 *HASH_ENTRY_RAM_C_ERR;
} wcm_ip_r__addr;


void
wcm_ip_r__init(
  wcm_ip_r *p0,
  wcm_ip_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_ip_r_FCMN_SHELL_CTRL_ERR__n = 1;
#define wcm_ip_r_FCMN_SHELL_CTRL_ERR__nd    1
typedef uint1 wcm_ip_r_FCMN_SHELL_CTRL_ERR_t;

static const unsigned int wcm_ip_r_TCAM_SWEEP_ERR__n = 1;
#define wcm_ip_r_TCAM_SWEEP_ERR__nd    1
typedef uint1 wcm_ip_r_TCAM_SWEEP_ERR_t;

static const unsigned int wcm_ip_r_FGRP_ERR__n = 3;
#define wcm_ip_r_FGRP_ERR__nd    3
typedef uint3 wcm_ip_r_FGRP_ERR_t;

static const unsigned int wcm_ip_r_FGHASH_ERR__n = 3;
#define wcm_ip_r_FGHASH_ERR__nd    3
typedef uint3 wcm_ip_r_FGHASH_ERR_t;

static const unsigned int wcm_ip_r_HASH_ENTRY_RAM_U_ERR__n = 4;
#define wcm_ip_r_HASH_ENTRY_RAM_U_ERR__nd    4
typedef uint4 wcm_ip_r_HASH_ENTRY_RAM_U_ERR_t;

static const unsigned int wcm_ip_r_HASH_ENTRY_RAM_C_ERR__n = 4;
#define wcm_ip_r_HASH_ENTRY_RAM_C_ERR__nd    4
typedef uint4 wcm_ip_r_HASH_ENTRY_RAM_C_ERR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_shared_smp_usage_r;

typedef struct {
  uint16 *COUNT;
} cm_shared_smp_usage_r__addr;


void
cm_shared_smp_usage_r__init(
  cm_shared_smp_usage_r *p0,
  cm_shared_smp_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_shared_smp_usage_r_COUNT__n = 16;
#define cm_shared_smp_usage_r_COUNT__nd    16
typedef uint16 cm_shared_smp_usage_r_COUNT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 POL1_HI;
  uint12 POL0_HI;
  uint8 ELAPSE_SLOT;
  uint16 ELAPSE_CYCLE;
} pol_sweep_r;

typedef struct {
  uint12 *POL1_HI;
  uint12 *POL0_HI;
  uint8 *ELAPSE_SLOT;
  uint16 *ELAPSE_CYCLE;
} pol_sweep_r__addr;


void
pol_sweep_r__init(
  pol_sweep_r *p0,
  pol_sweep_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_sweep_r_POL1_HI__n = 12;
#define pol_sweep_r_POL1_HI__nd    12
typedef uint12 pol_sweep_r_POL1_HI_t;

static const unsigned int pol_sweep_r_POL0_HI__n = 12;
#define pol_sweep_r_POL0_HI__nd    12
typedef uint12 pol_sweep_r_POL0_HI_t;

static const unsigned int pol_sweep_r_ELAPSE_SLOT__n = 8;
#define pol_sweep_r_ELAPSE_SLOT__nd    8
typedef uint8 pol_sweep_r_ELAPSE_SLOT_t;

static const unsigned int pol_sweep_r_ELAPSE_CYCLE__n = 16;
#define pol_sweep_r_ELAPSE_CYCLE__nd    16
typedef uint16 pol_sweep_r_ELAPSE_CYCLE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 FUNCTION_NUM;
  uint2 FUNCTION_TYPE;
  uint2 _RSVD0_;
  uint6 PF_NUM;
  uint1 _RSVD1_;
  uint1 FUNC_VALID;
  uint14 DST_Q;
} mod_mirror_profile_table2_r;

typedef struct {
  uint12 *FUNCTION_NUM;
  uint2 *FUNCTION_TYPE;
  uint2 *_RSVD0_;
  uint6 *PF_NUM;
  uint1 *_RSVD1_;
  uint1 *FUNC_VALID;
  uint14 *DST_Q;
} mod_mirror_profile_table2_r__addr;


void
mod_mirror_profile_table2_r__init(
  mod_mirror_profile_table2_r *p0,
  mod_mirror_profile_table2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_mirror_profile_table2_r_FUNCTION_NUM__n = 12;
#define mod_mirror_profile_table2_r_FUNCTION_NUM__nd    12
typedef uint12 mod_mirror_profile_table2_r_FUNCTION_NUM_t;

static const unsigned int mod_mirror_profile_table2_r_FUNCTION_TYPE__n = 2;
#define mod_mirror_profile_table2_r_FUNCTION_TYPE__nd    2
typedef uint2 mod_mirror_profile_table2_r_FUNCTION_TYPE_t;

static const unsigned int mod_mirror_profile_table2_r__RSVD0___n = 2;
#define mod_mirror_profile_table2_r__RSVD0___nd    2
typedef uint2 mod_mirror_profile_table2_r__RSVD0__t;

static const unsigned int mod_mirror_profile_table2_r_PF_NUM__n = 6;
#define mod_mirror_profile_table2_r_PF_NUM__nd    6
typedef uint6 mod_mirror_profile_table2_r_PF_NUM_t;

static const unsigned int mod_mirror_profile_table2_r__RSVD1___n = 1;
#define mod_mirror_profile_table2_r__RSVD1___nd    1
typedef uint1 mod_mirror_profile_table2_r__RSVD1__t;

static const unsigned int mod_mirror_profile_table2_r_FUNC_VALID__n = 1;
#define mod_mirror_profile_table2_r_FUNC_VALID__nd    1
typedef uint1 mod_mirror_profile_table2_r_FUNC_VALID_t;

static const unsigned int mod_mirror_profile_table2_r_DST_Q__n = 14;
#define mod_mirror_profile_table2_r_DST_Q__nd    14
typedef uint14 mod_mirror_profile_table2_r_DST_Q_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 MEM_ERROR;
} parser_ip_r;

typedef struct {
  uint1 *MEM_ERROR;
} parser_ip_r__addr;


void
parser_ip_r__init(
  parser_ip_r *p0,
  parser_ip_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ip_r_MEM_ERROR__n = 1;
#define parser_ip_r_MEM_ERROR__nd    1
typedef uint1 parser_ip_r_MEM_ERROR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 SRC_ID;
} map_rewrite_r;

typedef struct {
  uint6 *SRC_ID;
} map_rewrite_r__addr;


void
map_rewrite_r__init(
  map_rewrite_r *p0,
  map_rewrite_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_rewrite_r_SRC_ID__n = 6;
#define map_rewrite_r_SRC_ID__nd    6
typedef uint6 map_rewrite_r_SRC_ID_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_PORT_MASK;
} trigger_direct_map_ctx0_r;

typedef struct {
  uint64 *DEST_PORT_MASK;
} trigger_direct_map_ctx0_r__addr;


void
trigger_direct_map_ctx0_r__init(
  trigger_direct_map_ctx0_r *p0,
  trigger_direct_map_ctx0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_ctx0_r_DEST_PORT_MASK__n = 64;
#define trigger_direct_map_ctx0_r_DEST_PORT_MASK__nd    64
typedef uint64 trigger_direct_map_ctx0_r_DEST_PORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DROP_MASK;
} trigger_direct_map_adr1_r;

typedef struct {
  uint64 *DROP_MASK;
} trigger_direct_map_adr1_r__addr;


void
trigger_direct_map_adr1_r__init(
  trigger_direct_map_adr1_r *p0,
  trigger_direct_map_adr1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adr1_r_DROP_MASK__n = 64;
#define trigger_direct_map_adr1_r_DROP_MASK__nd    64
typedef uint64 trigger_direct_map_adr1_r_DROP_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint24 CMD_0;
  uint24 CMD_1;
} mod_profile_command_r;

typedef struct {
  uint24 *CMD_0;
  uint24 *CMD_1;
} mod_profile_command_r__addr;


void
mod_profile_command_r__init(
  mod_profile_command_r *p0,
  mod_profile_command_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_profile_command_r_CMD_0__n = 24;
#define mod_profile_command_r_CMD_0__nd    24
typedef uint24 mod_profile_command_r_CMD_0_t;

static const unsigned int mod_profile_command_r_CMD_1__n = 24;
#define mod_profile_command_r_CMD_1__nd    24
typedef uint24 mod_profile_command_r_CMD_1_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 VLAN_COUNTER;
  uint12 L3_POLICER;
  uint12 L2_POLICER;
  uint8 DOMAIN_PROFILE;
} map_domain_action1_r;

typedef struct {
  uint12 *VLAN_COUNTER;
  uint12 *L3_POLICER;
  uint12 *L2_POLICER;
  uint8 *DOMAIN_PROFILE;
} map_domain_action1_r__addr;


void
map_domain_action1_r__init(
  map_domain_action1_r *p0,
  map_domain_action1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_domain_action1_r_VLAN_COUNTER__n = 12;
#define map_domain_action1_r_VLAN_COUNTER__nd    12
typedef uint12 map_domain_action1_r_VLAN_COUNTER_t;

static const unsigned int map_domain_action1_r_L3_POLICER__n = 12;
#define map_domain_action1_r_L3_POLICER__nd    12
typedef uint12 map_domain_action1_r_L3_POLICER_t;

static const unsigned int map_domain_action1_r_L2_POLICER__n = 12;
#define map_domain_action1_r_L2_POLICER__nd    12
typedef uint12 map_domain_action1_r_L2_POLICER_t;

static const unsigned int map_domain_action1_r_DOMAIN_PROFILE__n = 8;
#define map_domain_action1_r_DOMAIN_PROFILE__nd    8
typedef uint8 map_domain_action1_r_DOMAIN_PROFILE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 _RSVD0_;
  uint6 VID_ID;
  uint3 TC;
  uint3 FRAME_CLASS_MASK;
  uint2 ROUTED_MASK;
  uint4 _RSVD1_;
  uint14 EGRESS_DOMAIN_VALUE;
  uint1 _RSVD2_;
  uint14 EGRESS_DOMAIN_MASK;
  uint5 _RSVD3_;
} trigger_condition_param_r;

typedef struct {
  uint12 *_RSVD0_;
  uint6 *VID_ID;
  uint3 *TC;
  uint3 *FRAME_CLASS_MASK;
  uint2 *ROUTED_MASK;
  uint4 *_RSVD1_;
  uint14 *EGRESS_DOMAIN_VALUE;
  uint1 *_RSVD2_;
  uint14 *EGRESS_DOMAIN_MASK;
  uint5 *_RSVD3_;
} trigger_condition_param_r__addr;


void
trigger_condition_param_r__init(
  trigger_condition_param_r *p0,
  trigger_condition_param_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_param_r__RSVD0___n = 12;
#define trigger_condition_param_r__RSVD0___nd    12
typedef uint12 trigger_condition_param_r__RSVD0__t;

static const unsigned int trigger_condition_param_r_VID_ID__n = 6;
#define trigger_condition_param_r_VID_ID__nd    6
typedef uint6 trigger_condition_param_r_VID_ID_t;

static const unsigned int trigger_condition_param_r_TC__n = 3;
#define trigger_condition_param_r_TC__nd    3
typedef uint3 trigger_condition_param_r_TC_t;

static const unsigned int trigger_condition_param_r_FRAME_CLASS_MASK__n = 3;
#define trigger_condition_param_r_FRAME_CLASS_MASK__nd    3
typedef uint3 trigger_condition_param_r_FRAME_CLASS_MASK_t;

static const unsigned int trigger_condition_param_r_ROUTED_MASK__n = 2;
#define trigger_condition_param_r_ROUTED_MASK__nd    2
typedef uint2 trigger_condition_param_r_ROUTED_MASK_t;

static const unsigned int trigger_condition_param_r__RSVD1___n = 4;
#define trigger_condition_param_r__RSVD1___nd    4
typedef uint4 trigger_condition_param_r__RSVD1__t;

static const unsigned int trigger_condition_param_r_EGRESS_DOMAIN_VALUE__n = 14;
#define trigger_condition_param_r_EGRESS_DOMAIN_VALUE__nd    14
typedef uint14 trigger_condition_param_r_EGRESS_DOMAIN_VALUE_t;

static const unsigned int trigger_condition_param_r__RSVD2___n = 1;
#define trigger_condition_param_r__RSVD2___nd    1
typedef uint1 trigger_condition_param_r__RSVD2__t;

static const unsigned int trigger_condition_param_r_EGRESS_DOMAIN_MASK__n = 14;
#define trigger_condition_param_r_EGRESS_DOMAIN_MASK__nd    14
typedef uint14 trigger_condition_param_r_EGRESS_DOMAIN_MASK_t;

static const unsigned int trigger_condition_param_r__RSVD3___n = 5;
#define trigger_condition_param_r__RSVD3___nd    5
typedef uint5 trigger_condition_param_r__RSVD3__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 NEW_DEST_MASK;
} trigger_direct_map_adm3_r;

typedef struct {
  uint64 *NEW_DEST_MASK;
} trigger_direct_map_adm3_r__addr;


void
trigger_direct_map_adm3_r__init(
  trigger_direct_map_adm3_r *p0,
  trigger_direct_map_adm3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adm3_r_NEW_DEST_MASK__n = 64;
#define trigger_direct_map_adm3_r_NEW_DEST_MASK__nd    64
typedef uint64 trigger_direct_map_adm3_r_NEW_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint5 PORT0;
  uint5 PORT1;
  uint5 PORT2;
  uint5 PORT3;
} cm_rx_smp_usage_max_ctrl_r;

typedef struct {
  uint5 *PORT0;
  uint5 *PORT1;
  uint5 *PORT2;
  uint5 *PORT3;
} cm_rx_smp_usage_max_ctrl_r__addr;


void
cm_rx_smp_usage_max_ctrl_r__init(
  cm_rx_smp_usage_max_ctrl_r *p0,
  cm_rx_smp_usage_max_ctrl_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_usage_max_ctrl_r_PORT0__n = 5;
#define cm_rx_smp_usage_max_ctrl_r_PORT0__nd    5
typedef uint5 cm_rx_smp_usage_max_ctrl_r_PORT0_t;

static const unsigned int cm_rx_smp_usage_max_ctrl_r_PORT1__n = 5;
#define cm_rx_smp_usage_max_ctrl_r_PORT1__nd    5
typedef uint5 cm_rx_smp_usage_max_ctrl_r_PORT1_t;

static const unsigned int cm_rx_smp_usage_max_ctrl_r_PORT2__n = 5;
#define cm_rx_smp_usage_max_ctrl_r_PORT2__nd    5
typedef uint5 cm_rx_smp_usage_max_ctrl_r_PORT2_t;

static const unsigned int cm_rx_smp_usage_max_ctrl_r_PORT3__n = 5;
#define cm_rx_smp_usage_max_ctrl_r_PORT3__nd    5
typedef uint5 cm_rx_smp_usage_max_ctrl_r_PORT3_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MASK;
} entropy_hash_key_mask_r;

typedef struct {
  uint64 *MASK;
} entropy_hash_key_mask_r__addr;


void
entropy_hash_key_mask_r__init(
  entropy_hash_key_mask_r *p0,
  entropy_hash_key_mask_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_key_mask_r_MASK__n = 64;
#define entropy_hash_key_mask_r_MASK__nd    64
typedef uint64 entropy_hash_key_mask_r_MASK_t;




/* ../src/RegC.m3:221 */
typedef entropy_hash_key_mask_r entropy_hash_key_mask_rf[128];

typedef entropy_hash_key_mask_r__addr entropy_hash_key_mask_rf__addr[128];


void
entropy_hash_key_mask_rf__init(
  entropy_hash_key_mask_rf *p0,
  entropy_hash_key_mask_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_key_mask_rf_ENTROPY_HASH_KEY_MASK__n = 128;
#define entropy_hash_key_mask_rf_ENTROPY_HASH_KEY_MASK__nd    128



/* ../src/RegC.m3:179 */
typedef struct {
  uint16 ADDR_KEY32_SEL;
} lpm_key_sel3_r;

typedef struct {
  uint16 *ADDR_KEY32_SEL;
} lpm_key_sel3_r__addr;


void
lpm_key_sel3_r__init(
  lpm_key_sel3_r *p0,
  lpm_key_sel3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_key_sel3_r_ADDR_KEY32_SEL__n = 16;
#define lpm_key_sel3_r_ADDR_KEY32_SEL__nd    16
typedef uint16 lpm_key_sel3_r_ADDR_KEY32_SEL_t;




/* ../src/RegC.m3:221 */
typedef map_rewrite_r map_rewrite_rf[32];

typedef map_rewrite_r__addr map_rewrite_rf__addr[32];


void
map_rewrite_rf__init(
  map_rewrite_rf *p0,
  map_rewrite_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_rewrite_rf_MAP_REWRITE__n = 32;
#define map_rewrite_rf_MAP_REWRITE__nd    32



/* ../src/RegC.m3:179 */
typedef struct {
  uint44 DATA_CNTB;
} pol_direct_map_ctr0_r;

typedef struct {
  uint44 *DATA_CNTB;
} pol_direct_map_ctr0_r__addr;


void
pol_direct_map_ctr0_r__init(
  pol_direct_map_ctr0_r *p0,
  pol_direct_map_ctr0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_ctr0_r_DATA_CNTB__n = 44;
#define pol_direct_map_ctr0_r_DATA_CNTB__nd    44
typedef uint44 pol_direct_map_ctr0_r_DATA_CNTB_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 CURRENT;
} cm_apply_mcast_epoch_r;

typedef struct {
  uint1 *CURRENT;
} cm_apply_mcast_epoch_r__addr;


void
cm_apply_mcast_epoch_r__init(
  cm_apply_mcast_epoch_r *p0,
  cm_apply_mcast_epoch_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_mcast_epoch_r_CURRENT__n = 1;
#define cm_apply_mcast_epoch_r_CURRENT__nd    1
typedef uint1 cm_apply_mcast_epoch_r_CURRENT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 PROTOCOL_ID_0;
  uint9 OFFSET_0;
  uint8 PROTOCOL_ID_1;
  uint9 OFFSET_1;
  uint8 PROTOCOL_ID_2;
  uint9 OFFSET_2;
} mod_profile_field_r;

typedef struct {
  uint8 *PROTOCOL_ID_0;
  uint9 *OFFSET_0;
  uint8 *PROTOCOL_ID_1;
  uint9 *OFFSET_1;
  uint8 *PROTOCOL_ID_2;
  uint9 *OFFSET_2;
} mod_profile_field_r__addr;


void
mod_profile_field_r__init(
  mod_profile_field_r *p0,
  mod_profile_field_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_profile_field_r_PROTOCOL_ID_0__n = 8;
#define mod_profile_field_r_PROTOCOL_ID_0__nd    8
typedef uint8 mod_profile_field_r_PROTOCOL_ID_0_t;

static const unsigned int mod_profile_field_r_OFFSET_0__n = 9;
#define mod_profile_field_r_OFFSET_0__nd    9
typedef uint9 mod_profile_field_r_OFFSET_0_t;

static const unsigned int mod_profile_field_r_PROTOCOL_ID_1__n = 8;
#define mod_profile_field_r_PROTOCOL_ID_1__nd    8
typedef uint8 mod_profile_field_r_PROTOCOL_ID_1_t;

static const unsigned int mod_profile_field_r_OFFSET_1__n = 9;
#define mod_profile_field_r_OFFSET_1__nd    9
typedef uint9 mod_profile_field_r_OFFSET_1_t;

static const unsigned int mod_profile_field_r_PROTOCOL_ID_2__n = 8;
#define mod_profile_field_r_PROTOCOL_ID_2__nd    8
typedef uint8 mod_profile_field_r_PROTOCOL_ID_2_t;

static const unsigned int mod_profile_field_r_OFFSET_2__n = 9;
#define mod_profile_field_r_OFFSET_2__nd    9
typedef uint9 mod_profile_field_r_OFFSET_2_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 GLORT_DEST_MASK;
} glort_dest_table_r1;

typedef struct {
  uint64 *GLORT_DEST_MASK;
} glort_dest_table_r1__addr;


void
glort_dest_table_r1__init(
  glort_dest_table_r1 *p0,
  glort_dest_table_r1__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_dest_table_r1_GLORT_DEST_MASK__n = 64;
#define glort_dest_table_r1_GLORT_DEST_MASK__nd    64
typedef uint64 glort_dest_table_r1_GLORT_DEST_MASK_t;




/* ../src/RegC.m3:221 */
typedef nexthop_weights_table_r nexthop_weights_table_rf[8];

typedef nexthop_weights_table_r__addr nexthop_weights_table_rf__addr[8];


void
nexthop_weights_table_rf__init(
  nexthop_weights_table_rf *p0,
  nexthop_weights_table_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_weights_table_rf_NH_WEIGHTS__n = 8;
#define nexthop_weights_table_rf_NH_WEIGHTS__nd    8



/* ../src/RegC.m3:179 */
typedef struct {
  uint9 HEAD;
} ma_tcn_ptr_head_r;

typedef struct {
  uint9 *HEAD;
} ma_tcn_ptr_head_r__addr;


void
ma_tcn_ptr_head_r__init(
  ma_tcn_ptr_head_r *p0,
  ma_tcn_ptr_head_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_ptr_head_r_HEAD__n = 9;
#define ma_tcn_ptr_head_r_HEAD__nd    9
typedef uint9 ma_tcn_ptr_head_r_HEAD_t;




/* ../src/RegC.m3:221 */
typedef parser_key_w_r parser_key_w_rf[16];

typedef parser_key_w_r__addr parser_key_w_rf__addr[16];


void
parser_key_w_rf__init(
  parser_key_w_rf *p0,
  parser_key_w_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_key_w_rf_PARSER_KEY_W__n = 16;
#define parser_key_w_rf_PARSER_KEY_W__nd    16



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 BITMAP;
} lpm_subtrie_bitmaps_r;

typedef struct {
  uint64 *BITMAP;
} lpm_subtrie_bitmaps_r__addr;


void
lpm_subtrie_bitmaps_r__init(
  lpm_subtrie_bitmaps_r *p0,
  lpm_subtrie_bitmaps_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_subtrie_bitmaps_r_BITMAP__n = 64;
#define lpm_subtrie_bitmaps_r_BITMAP__nd    64
typedef uint64 lpm_subtrie_bitmaps_r_BITMAP_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 TO_RED;
  uint4 TO_YELLOW;
  uint1 DROP_ER;
  uint1 IG;
  uint1 IR;
} pol_vpri_r;

typedef struct {
  uint4 *TO_RED;
  uint4 *TO_YELLOW;
  uint1 *DROP_ER;
  uint1 *IG;
  uint1 *IR;
} pol_vpri_r__addr;


void
pol_vpri_r__init(
  pol_vpri_r *p0,
  pol_vpri_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_vpri_r_TO_RED__n = 4;
#define pol_vpri_r_TO_RED__nd    4
typedef uint4 pol_vpri_r_TO_RED_t;

static const unsigned int pol_vpri_r_TO_YELLOW__n = 4;
#define pol_vpri_r_TO_YELLOW__nd    4
typedef uint4 pol_vpri_r_TO_YELLOW_t;

static const unsigned int pol_vpri_r_DROP_ER__n = 1;
#define pol_vpri_r_DROP_ER__nd    1
typedef uint1 pol_vpri_r_DROP_ER_t;

static const unsigned int pol_vpri_r_IG__n = 1;
#define pol_vpri_r_IG__nd    1
typedef uint1 pol_vpri_r_IG_t;

static const unsigned int pol_vpri_r_IR__n = 1;
#define pol_vpri_r_IR__nd    1
typedef uint1 pol_vpri_r_IR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WATERMARK;
} cm_rx_smp_private_wm_r;

typedef struct {
  uint15 *WATERMARK;
} cm_rx_smp_private_wm_r__addr;


void
cm_rx_smp_private_wm_r__init(
  cm_rx_smp_private_wm_r *p0,
  cm_rx_smp_private_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_private_wm_r_WATERMARK__n = 15;
#define cm_rx_smp_private_wm_r_WATERMARK__nd    15
typedef uint15 cm_rx_smp_private_wm_r_WATERMARK_t;




/* ../src/RegC.m3:221 */
typedef cm_rx_smp_private_wm_r cm_rx_smp_private_wm_rf[2];

typedef cm_rx_smp_private_wm_r__addr cm_rx_smp_private_wm_rf__addr[2];


void
cm_rx_smp_private_wm_rf__init(
  cm_rx_smp_private_wm_rf *p0,
  cm_rx_smp_private_wm_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_private_wm_rf_CM_RX_SMP_PRIVATE_WM__n = 2;
#define cm_rx_smp_private_wm_rf_CM_RX_SMP_PRIVATE_WM__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MIRROR_PORT_MASK3;
} cm_apply_mirror_ecmp_dmask3_r;

typedef struct {
  uint64 *MIRROR_PORT_MASK3;
} cm_apply_mirror_ecmp_dmask3_r__addr;


void
cm_apply_mirror_ecmp_dmask3_r__init(
  cm_apply_mirror_ecmp_dmask3_r *p0,
  cm_apply_mirror_ecmp_dmask3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_mirror_ecmp_dmask3_r_MIRROR_PORT_MASK3__n = 64;
#define cm_apply_mirror_ecmp_dmask3_r_MIRROR_PORT_MASK3__nd    64
typedef uint64 cm_apply_mirror_ecmp_dmask3_r_MIRROR_PORT_MASK3_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_global_usage_max_r;

typedef struct {
  uint16 *COUNT;
} cm_global_usage_max_r__addr;


void
cm_global_usage_max_r__init(
  cm_global_usage_max_r *p0,
  cm_global_usage_max_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_global_usage_max_r_COUNT__n = 16;
#define cm_global_usage_max_r_COUNT__nd    16
typedef uint16 cm_global_usage_max_r_COUNT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 READY;
} ma_tcn_dequeue_r;

typedef struct {
  uint1 *READY;
} ma_tcn_dequeue_r__addr;


void
ma_tcn_dequeue_r__init(
  ma_tcn_dequeue_r *p0,
  ma_tcn_dequeue_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_dequeue_r_READY__n = 1;
#define ma_tcn_dequeue_r_READY__nd    1
typedef uint1 ma_tcn_dequeue_r_READY_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 L4_DST;
  uint3 MAP_PROT;
  uint2 VALID;
  uint16 MAP_L4_DST;
} map_l4_dst_r;

typedef struct {
  uint16 *L4_DST;
  uint3 *MAP_PROT;
  uint2 *VALID;
  uint16 *MAP_L4_DST;
} map_l4_dst_r__addr;


void
map_l4_dst_r__init(
  map_l4_dst_r *p0,
  map_l4_dst_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_l4_dst_r_L4_DST__n = 16;
#define map_l4_dst_r_L4_DST__nd    16
typedef uint16 map_l4_dst_r_L4_DST_t;

static const unsigned int map_l4_dst_r_MAP_PROT__n = 3;
#define map_l4_dst_r_MAP_PROT__nd    3
typedef uint3 map_l4_dst_r_MAP_PROT_t;

static const unsigned int map_l4_dst_r_VALID__n = 2;
#define map_l4_dst_r_VALID__nd    2
typedef uint2 map_l4_dst_r_VALID_t;

static const unsigned int map_l4_dst_r_MAP_L4_DST__n = 16;
#define map_l4_dst_r_MAP_L4_DST__nd    16
typedef uint16 map_l4_dst_r_MAP_L4_DST_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 RSVD;
  uint47 FLAGS;
  uint1 IHL_FITS;
  uint1 IHL_OK;
  uint4 _RSVD0_;
  uint2 CSUM;
  uint3 EX;
  uint1 PTRS_ERR;
  uint4 _RSVD1_;
} map_profile_key_invert0_r;

typedef struct {
  uint1 *RSVD;
  uint47 *FLAGS;
  uint1 *IHL_FITS;
  uint1 *IHL_OK;
  uint4 *_RSVD0_;
  uint2 *CSUM;
  uint3 *EX;
  uint1 *PTRS_ERR;
  uint4 *_RSVD1_;
} map_profile_key_invert0_r__addr;


void
map_profile_key_invert0_r__init(
  map_profile_key_invert0_r *p0,
  map_profile_key_invert0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_profile_key_invert0_r_RSVD__n = 1;
#define map_profile_key_invert0_r_RSVD__nd    1
typedef uint1 map_profile_key_invert0_r_RSVD_t;

static const unsigned int map_profile_key_invert0_r_FLAGS__n = 47;
#define map_profile_key_invert0_r_FLAGS__nd    47
typedef uint47 map_profile_key_invert0_r_FLAGS_t;

static const unsigned int map_profile_key_invert0_r_IHL_FITS__n = 1;
#define map_profile_key_invert0_r_IHL_FITS__nd    1
typedef uint1 map_profile_key_invert0_r_IHL_FITS_t;

static const unsigned int map_profile_key_invert0_r_IHL_OK__n = 1;
#define map_profile_key_invert0_r_IHL_OK__nd    1
typedef uint1 map_profile_key_invert0_r_IHL_OK_t;

static const unsigned int map_profile_key_invert0_r__RSVD0___n = 4;
#define map_profile_key_invert0_r__RSVD0___nd    4
typedef uint4 map_profile_key_invert0_r__RSVD0__t;

static const unsigned int map_profile_key_invert0_r_CSUM__n = 2;
#define map_profile_key_invert0_r_CSUM__nd    2
typedef uint2 map_profile_key_invert0_r_CSUM_t;

static const unsigned int map_profile_key_invert0_r_EX__n = 3;
#define map_profile_key_invert0_r_EX__nd    3
typedef uint3 map_profile_key_invert0_r_EX_t;

static const unsigned int map_profile_key_invert0_r_PTRS_ERR__n = 1;
#define map_profile_key_invert0_r_PTRS_ERR__nd    1
typedef uint1 map_profile_key_invert0_r_PTRS_ERR_t;

static const unsigned int map_profile_key_invert0_r__RSVD1___n = 4;
#define map_profile_key_invert0_r__RSVD1___nd    4
typedef uint4 map_profile_key_invert0_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 POL_SWEEP_LAST;
} pol_sweep_last_r;

typedef struct {
  uint48 *POL_SWEEP_LAST;
} pol_sweep_last_r__addr;


void
pol_sweep_last_r__init(
  pol_sweep_last_r *p0,
  pol_sweep_last_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_sweep_last_r_POL_SWEEP_LAST__n = 48;
#define pol_sweep_last_r_POL_SWEEP_LAST__nd    48
typedef uint48 pol_sweep_last_r_POL_SWEEP_LAST_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 MAC_ADDRESS;
  uint5 PORT;
  uint1 VALID;
} ma_tcn_data_0_r;

typedef struct {
  uint48 *MAC_ADDRESS;
  uint5 *PORT;
  uint1 *VALID;
} ma_tcn_data_0_r__addr;


void
ma_tcn_data_0_r__init(
  ma_tcn_data_0_r *p0,
  ma_tcn_data_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_data_0_r_MAC_ADDRESS__n = 48;
#define ma_tcn_data_0_r_MAC_ADDRESS__nd    48
typedef uint48 ma_tcn_data_0_r_MAC_ADDRESS_t;

static const unsigned int ma_tcn_data_0_r_PORT__n = 5;
#define ma_tcn_data_0_r_PORT__nd    5
typedef uint5 ma_tcn_data_0_r_PORT_t;

static const unsigned int ma_tcn_data_0_r_VALID__n = 1;
#define ma_tcn_data_0_r_VALID__nd    1
typedef uint1 ma_tcn_data_0_r_VALID_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 BUF_SIZE;
} tx_pb_prefetch_r;

typedef struct {
  uint8 *BUF_SIZE;
} tx_pb_prefetch_r__addr;


void
tx_pb_prefetch_r__init(
  tx_pb_prefetch_r *p0,
  tx_pb_prefetch_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int tx_pb_prefetch_r_BUF_SIZE__n = 8;
#define tx_pb_prefetch_r_BUF_SIZE__nd    8
typedef uint8 tx_pb_prefetch_r_BUF_SIZE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_1_1_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_1_1_r__addr;


void
fwd_port_cfg_1_1_r__init(
  fwd_port_cfg_1_1_r *p0,
  fwd_port_cfg_1_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_1_1_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_1_1_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_1_1_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint24 ENABLE;
} wcm_action_cfg_en_r;

typedef struct {
  uint24 *ENABLE;
} wcm_action_cfg_en_r__addr;


void
wcm_action_cfg_en_r__init(
  wcm_action_cfg_en_r *p0,
  wcm_action_cfg_en_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_action_cfg_en_r_ENABLE__n = 24;
#define wcm_action_cfg_en_r_ENABLE__nd    24
typedef uint24 wcm_action_cfg_en_r_ENABLE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint9 GROUP_1;
  uint9 GROUP_2;
  uint9 GROUP_3;
  uint9 GROUP_4;
  uint9 GROUP_5;
  uint9 GROUP_6;
  uint9 GROUP_7;
} mod_profile_group_r;

typedef struct {
  uint9 *GROUP_1;
  uint9 *GROUP_2;
  uint9 *GROUP_3;
  uint9 *GROUP_4;
  uint9 *GROUP_5;
  uint9 *GROUP_6;
  uint9 *GROUP_7;
} mod_profile_group_r__addr;


void
mod_profile_group_r__init(
  mod_profile_group_r *p0,
  mod_profile_group_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_profile_group_r_GROUP_1__n = 9;
#define mod_profile_group_r_GROUP_1__nd    9
typedef uint9 mod_profile_group_r_GROUP_1_t;

static const unsigned int mod_profile_group_r_GROUP_2__n = 9;
#define mod_profile_group_r_GROUP_2__nd    9
typedef uint9 mod_profile_group_r_GROUP_2_t;

static const unsigned int mod_profile_group_r_GROUP_3__n = 9;
#define mod_profile_group_r_GROUP_3__nd    9
typedef uint9 mod_profile_group_r_GROUP_3_t;

static const unsigned int mod_profile_group_r_GROUP_4__n = 9;
#define mod_profile_group_r_GROUP_4__nd    9
typedef uint9 mod_profile_group_r_GROUP_4_t;

static const unsigned int mod_profile_group_r_GROUP_5__n = 9;
#define mod_profile_group_r_GROUP_5__nd    9
typedef uint9 mod_profile_group_r_GROUP_5_t;

static const unsigned int mod_profile_group_r_GROUP_6__n = 9;
#define mod_profile_group_r_GROUP_6__nd    9
typedef uint9 mod_profile_group_r_GROUP_6_t;

static const unsigned int mod_profile_group_r_GROUP_7__n = 9;
#define mod_profile_group_r_GROUP_7__nd    9
typedef uint9 mod_profile_group_r_GROUP_7_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_PORT_MASK;
} trigger_direct_map_ctx1_r;

typedef struct {
  uint64 *DEST_PORT_MASK;
} trigger_direct_map_ctx1_r__addr;


void
trigger_direct_map_ctx1_r__init(
  trigger_direct_map_ctx1_r *p0,
  trigger_direct_map_ctx1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_ctx1_r_DEST_PORT_MASK__n = 64;
#define trigger_direct_map_ctx1_r_DEST_PORT_MASK__nd    64
typedef uint64 trigger_direct_map_ctx1_r_DEST_PORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 PAUSE_ON;
  uint15 PAUSE_OFF;
} cm_shared_smp_pause_wm_r;

typedef struct {
  uint15 *PAUSE_ON;
  uint15 *PAUSE_OFF;
} cm_shared_smp_pause_wm_r__addr;


void
cm_shared_smp_pause_wm_r__init(
  cm_shared_smp_pause_wm_r *p0,
  cm_shared_smp_pause_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_shared_smp_pause_wm_r_PAUSE_ON__n = 15;
#define cm_shared_smp_pause_wm_r_PAUSE_ON__nd    15
typedef uint15 cm_shared_smp_pause_wm_r_PAUSE_ON_t;

static const unsigned int cm_shared_smp_pause_wm_r_PAUSE_OFF__n = 15;
#define cm_shared_smp_pause_wm_r_PAUSE_OFF__nd    15
typedef uint15 cm_shared_smp_pause_wm_r_PAUSE_OFF_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DROP_MASK;
} trigger_direct_map_adr2_r;

typedef struct {
  uint64 *DROP_MASK;
} trigger_direct_map_adr2_r__addr;


void
trigger_direct_map_adr2_r__init(
  trigger_direct_map_adr2_r *p0,
  trigger_direct_map_adr2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adr2_r_DROP_MASK__n = 64;
#define trigger_direct_map_adr2_r_DROP_MASK__nd    64
typedef uint64 trigger_direct_map_adr2_r_DROP_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_MASK;
} cm_apply_cpu_trap_mask_2_r;

typedef struct {
  uint64 *DEST_MASK;
} cm_apply_cpu_trap_mask_2_r__addr;


void
cm_apply_cpu_trap_mask_2_r__init(
  cm_apply_cpu_trap_mask_2_r *p0,
  cm_apply_cpu_trap_mask_2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_cpu_trap_mask_2_r_DEST_MASK__n = 64;
#define cm_apply_cpu_trap_mask_2_r_DEST_MASK__nd    64
typedef uint64 cm_apply_cpu_trap_mask_2_r_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 USED;
} nexthop_used_r;

typedef struct {
  uint64 *USED;
} nexthop_used_r__addr;


void
nexthop_used_r__init(
  nexthop_used_r *p0,
  nexthop_used_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_used_r_USED__n = 64;
#define nexthop_used_r_USED__nd    64
typedef uint64 nexthop_used_r_USED_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint10 PTYPE;
  uint4 EXTRACT_IDX;
} parser_ptype_ram_r;

typedef struct {
  uint10 *PTYPE;
  uint4 *EXTRACT_IDX;
} parser_ptype_ram_r__addr;


void
parser_ptype_ram_r__init(
  parser_ptype_ram_r *p0,
  parser_ptype_ram_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ptype_ram_r_PTYPE__n = 10;
#define parser_ptype_ram_r_PTYPE__nd    10
typedef uint10 parser_ptype_ram_r_PTYPE_t;

static const unsigned int parser_ptype_ram_r_EXTRACT_IDX__n = 4;
#define parser_ptype_ram_r_EXTRACT_IDX__nd    4
typedef uint4 parser_ptype_ram_r_EXTRACT_IDX_t;




/* ../src/RegC.m3:221 */
typedef parser_ptype_ram_r parser_ptype_ram_rf[64];

typedef parser_ptype_ram_r__addr parser_ptype_ram_rf__addr[64];


void
parser_ptype_ram_rf__init(
  parser_ptype_ram_rf *p0,
  parser_ptype_ram_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ptype_ram_rf_PARSER_PTYPE_RAM__n = 64;
#define parser_ptype_ram_rf_PARSER_PTYPE_RAM__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WATERMARK;
} cm_global_wm_r;

typedef struct {
  uint15 *WATERMARK;
} cm_global_wm_r__addr;


void
cm_global_wm_r__init(
  cm_global_wm_r *p0,
  cm_global_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_global_wm_r_WATERMARK__n = 15;
#define cm_global_wm_r_WATERMARK__nd    15
typedef uint15 cm_global_wm_r_WATERMARK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 MAP_PORT;
} map_port_r;

typedef struct {
  uint8 *MAP_PORT;
} map_port_r__addr;


void
map_port_r__init(
  map_port_r *p0,
  map_port_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_port_r_MAP_PORT__n = 8;
#define map_port_r_MAP_PORT__nd    8
typedef uint8 map_port_r_MAP_PORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 FORCE_ON;
  uint2 FORCE_OFF;
} cm_force_pause_cfg_r;

typedef struct {
  uint2 *FORCE_ON;
  uint2 *FORCE_OFF;
} cm_force_pause_cfg_r__addr;


void
cm_force_pause_cfg_r__init(
  cm_force_pause_cfg_r *p0,
  cm_force_pause_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_force_pause_cfg_r_FORCE_ON__n = 2;
#define cm_force_pause_cfg_r_FORCE_ON__nd    2
typedef uint2 cm_force_pause_cfg_r_FORCE_ON_t;

static const unsigned int cm_force_pause_cfg_r_FORCE_OFF__n = 2;
#define cm_force_pause_cfg_r_FORCE_OFF__nd    2
typedef uint2 cm_force_pause_cfg_r_FORCE_OFF_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_rx_smp_usage_r;

typedef struct {
  uint16 *COUNT;
} cm_rx_smp_usage_r__addr;


void
cm_rx_smp_usage_r__init(
  cm_rx_smp_usage_r *p0,
  cm_rx_smp_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_usage_r_COUNT__n = 16;
#define cm_rx_smp_usage_r_COUNT__nd    16
typedef uint16 cm_rx_smp_usage_r_COUNT_t;




/* ../src/RegC.m3:221 */
typedef cm_rx_smp_usage_r cm_rx_smp_usage_rf[2];

typedef cm_rx_smp_usage_r__addr cm_rx_smp_usage_rf__addr[2];


void
cm_rx_smp_usage_rf__init(
  cm_rx_smp_usage_rf *p0,
  cm_rx_smp_usage_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_usage_rf_CM_RX_SMP_USAGE__n = 2;
#define cm_rx_smp_usage_rf_CM_RX_SMP_USAGE__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint8 EXT_SEG_BOUNDARY;
  uint8 EXT_UNKNOWN_PROTID;
  uint8 EXT_DUP_PROTID;
} parser_counters_r;

typedef struct {
  uint8 *EXT_SEG_BOUNDARY;
  uint8 *EXT_UNKNOWN_PROTID;
  uint8 *EXT_DUP_PROTID;
} parser_counters_r__addr;


void
parser_counters_r__init(
  parser_counters_r *p0,
  parser_counters_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_counters_r_EXT_SEG_BOUNDARY__n = 8;
#define parser_counters_r_EXT_SEG_BOUNDARY__nd    8
typedef uint8 parser_counters_r_EXT_SEG_BOUNDARY_t;

static const unsigned int parser_counters_r_EXT_UNKNOWN_PROTID__n = 8;
#define parser_counters_r_EXT_UNKNOWN_PROTID__nd    8
typedef uint8 parser_counters_r_EXT_UNKNOWN_PROTID_t;

static const unsigned int parser_counters_r_EXT_DUP_PROTID__n = 8;
#define parser_counters_r_EXT_DUP_PROTID__nd    8
typedef uint8 parser_counters_r_EXT_DUP_PROTID_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 COUNT;
} trigger_stats_r;

typedef struct {
  uint64 *COUNT;
} trigger_stats_r__addr;


void
trigger_stats_r__init(
  trigger_stats_r *p0,
  trigger_stats_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_stats_r_COUNT__n = 64;
#define trigger_stats_r_COUNT__nd    64
typedef uint64 trigger_stats_r_COUNT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 MD_KEY16_SEL;
} lpm_key_sel0_r;

typedef struct {
  uint64 *MD_KEY16_SEL;
} lpm_key_sel0_r__addr;


void
lpm_key_sel0_r__init(
  lpm_key_sel0_r *p0,
  lpm_key_sel0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_key_sel0_r_MD_KEY16_SEL__n = 64;
#define lpm_key_sel0_r_MD_KEY16_SEL__nd    64
typedef uint64 lpm_key_sel0_r_MD_KEY16_SEL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint36 DATA_CNTP;
} pol_direct_map_ctr1_r;

typedef struct {
  uint36 *DATA_CNTP;
} pol_direct_map_ctr1_r__addr;


void
pol_direct_map_ctr1_r__init(
  pol_direct_map_ctr1_r *p0,
  pol_direct_map_ctr1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_ctr1_r_DATA_CNTP__n = 36;
#define pol_direct_map_ctr1_r_DATA_CNTP__nd    36
typedef uint36 pol_direct_map_ctr1_r_DATA_CNTP_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 TX_PRIVATE;
  uint1 TX_HOG;
  uint1 OVER_SMP_FREE;
  uint1 OVER_SMP_FREE2;
  uint15 QUEUE_DEPTH;
} cm_apply_tx_tc_state_r;

typedef struct {
  uint1 *TX_PRIVATE;
  uint1 *TX_HOG;
  uint1 *OVER_SMP_FREE;
  uint1 *OVER_SMP_FREE2;
  uint15 *QUEUE_DEPTH;
} cm_apply_tx_tc_state_r__addr;


void
cm_apply_tx_tc_state_r__init(
  cm_apply_tx_tc_state_r *p0,
  cm_apply_tx_tc_state_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tx_tc_state_r_TX_PRIVATE__n = 1;
#define cm_apply_tx_tc_state_r_TX_PRIVATE__nd    1
typedef uint1 cm_apply_tx_tc_state_r_TX_PRIVATE_t;

static const unsigned int cm_apply_tx_tc_state_r_TX_HOG__n = 1;
#define cm_apply_tx_tc_state_r_TX_HOG__nd    1
typedef uint1 cm_apply_tx_tc_state_r_TX_HOG_t;

static const unsigned int cm_apply_tx_tc_state_r_OVER_SMP_FREE__n = 1;
#define cm_apply_tx_tc_state_r_OVER_SMP_FREE__nd    1
typedef uint1 cm_apply_tx_tc_state_r_OVER_SMP_FREE_t;

static const unsigned int cm_apply_tx_tc_state_r_OVER_SMP_FREE2__n = 1;
#define cm_apply_tx_tc_state_r_OVER_SMP_FREE2__nd    1
typedef uint1 cm_apply_tx_tc_state_r_OVER_SMP_FREE2_t;

static const unsigned int cm_apply_tx_tc_state_r_QUEUE_DEPTH__n = 15;
#define cm_apply_tx_tc_state_r_QUEUE_DEPTH__nd    15
typedef uint15 cm_apply_tx_tc_state_r_QUEUE_DEPTH_t;




/* ../src/RegC.m3:221 */
typedef cm_apply_tx_tc_state_r cm_apply_tx_tc_state_rf[8];

typedef cm_apply_tx_tc_state_r__addr cm_apply_tx_tc_state_rf__addr[8];


void
cm_apply_tx_tc_state_rf__init(
  cm_apply_tx_tc_state_rf *p0,
  cm_apply_tx_tc_state_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tx_tc_state_rf_CM_APPLY_TX_TC_STATE__n = 8;
#define cm_apply_tx_tc_state_rf_CM_APPLY_TX_TC_STATE__nd    8



/* ../src/RegC.m3:179 */
typedef struct {
  uint3 L2_COLOR_CFG;
  uint3 L3_COLOR_CFG;
} map_domain_pol_cfg_r;

typedef struct {
  uint3 *L2_COLOR_CFG;
  uint3 *L3_COLOR_CFG;
} map_domain_pol_cfg_r__addr;


void
map_domain_pol_cfg_r__init(
  map_domain_pol_cfg_r *p0,
  map_domain_pol_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_domain_pol_cfg_r_L2_COLOR_CFG__n = 3;
#define map_domain_pol_cfg_r_L2_COLOR_CFG__nd    3
typedef uint3 map_domain_pol_cfg_r_L2_COLOR_CFG_t;

static const unsigned int map_domain_pol_cfg_r_L3_COLOR_CFG__n = 3;
#define map_domain_pol_cfg_r_L3_COLOR_CFG__nd    3
typedef uint3 map_domain_pol_cfg_r_L3_COLOR_CFG_t;




/* ../src/RegC.m3:221 */
typedef mod_map_dual_r mod_map_dual_rf[8];

typedef mod_map_dual_r__addr mod_map_dual_rf__addr[8];


void
mod_map_dual_rf__init(
  mod_map_dual_rf *p0,
  mod_map_dual_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_map_dual_rf_MOD_MAP_DUAL__n = 8;
#define mod_map_dual_rf_MOD_MAP_DUAL__nd    8



/* ../src/RegC.m3:221 */
typedef parser_ana_s_r parser_ana_s_rf[16];

typedef parser_ana_s_r__addr parser_ana_s_rf__addr[16];


void
parser_ana_s_rf__init(
  parser_ana_s_rf *p0,
  parser_ana_s_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ana_s_rf_PARSER_ANA_S__n = 16;
#define parser_ana_s_rf_PARSER_ANA_S__nd    16



/* ../src/RegC.m3:179 */
typedef struct {
  uint48 FRAMES;
} cm_apply_drop_count_r;

typedef struct {
  uint48 *FRAMES;
} cm_apply_drop_count_r__addr;


void
cm_apply_drop_count_r__init(
  cm_apply_drop_count_r *p0,
  cm_apply_drop_count_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_drop_count_r_FRAMES__n = 48;
#define cm_apply_drop_count_r_FRAMES__nd    48
typedef uint48 cm_apply_drop_count_r_FRAMES_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 TO_RED;
  uint6 TO_YELLOW;
  uint1 DROP_ER;
  uint1 IG;
  uint1 IR;
} pol_dscp_r;

typedef struct {
  uint6 *TO_RED;
  uint6 *TO_YELLOW;
  uint1 *DROP_ER;
  uint1 *IG;
  uint1 *IR;
} pol_dscp_r__addr;


void
pol_dscp_r__init(
  pol_dscp_r *p0,
  pol_dscp_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_dscp_r_TO_RED__n = 6;
#define pol_dscp_r_TO_RED__nd    6
typedef uint6 pol_dscp_r_TO_RED_t;

static const unsigned int pol_dscp_r_TO_YELLOW__n = 6;
#define pol_dscp_r_TO_YELLOW__nd    6
typedef uint6 pol_dscp_r_TO_YELLOW_t;

static const unsigned int pol_dscp_r_DROP_ER__n = 1;
#define pol_dscp_r_DROP_ER__nd    1
typedef uint1 pol_dscp_r_DROP_ER_t;

static const unsigned int pol_dscp_r_IG__n = 1;
#define pol_dscp_r_IG__nd    1
typedef uint1 pol_dscp_r_IG_t;

static const unsigned int pol_dscp_r_IR__n = 1;
#define pol_dscp_r_IR__nd    1
typedef uint1 pol_dscp_r_IR_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 STP_STATE_0;
  uint2 STP_STATE_1;
  uint2 STP_STATE_2;
  uint2 STP_STATE_3;
  uint2 STP_STATE_4;
  uint2 STP_STATE_5;
  uint2 STP_STATE_6;
  uint2 STP_STATE_7;
  uint2 STP_STATE_8;
  uint2 STP_STATE_9;
  uint2 STP_STATE_10;
  uint2 STP_STATE_11;
  uint2 STP_STATE_12;
  uint2 STP_STATE_13;
  uint2 STP_STATE_14;
  uint2 STP_STATE_15;
  uint2 STP_STATE_16;
  uint2 STP_STATE_17;
} ingress_mst_table_r;

typedef struct {
  uint2 *STP_STATE_0;
  uint2 *STP_STATE_1;
  uint2 *STP_STATE_2;
  uint2 *STP_STATE_3;
  uint2 *STP_STATE_4;
  uint2 *STP_STATE_5;
  uint2 *STP_STATE_6;
  uint2 *STP_STATE_7;
  uint2 *STP_STATE_8;
  uint2 *STP_STATE_9;
  uint2 *STP_STATE_10;
  uint2 *STP_STATE_11;
  uint2 *STP_STATE_12;
  uint2 *STP_STATE_13;
  uint2 *STP_STATE_14;
  uint2 *STP_STATE_15;
  uint2 *STP_STATE_16;
  uint2 *STP_STATE_17;
} ingress_mst_table_r__addr;


void
ingress_mst_table_r__init(
  ingress_mst_table_r *p0,
  ingress_mst_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ingress_mst_table_r_STP_STATE_0__n = 2;
#define ingress_mst_table_r_STP_STATE_0__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_0_t;

static const unsigned int ingress_mst_table_r_STP_STATE_1__n = 2;
#define ingress_mst_table_r_STP_STATE_1__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_1_t;

static const unsigned int ingress_mst_table_r_STP_STATE_2__n = 2;
#define ingress_mst_table_r_STP_STATE_2__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_2_t;

static const unsigned int ingress_mst_table_r_STP_STATE_3__n = 2;
#define ingress_mst_table_r_STP_STATE_3__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_3_t;

static const unsigned int ingress_mst_table_r_STP_STATE_4__n = 2;
#define ingress_mst_table_r_STP_STATE_4__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_4_t;

static const unsigned int ingress_mst_table_r_STP_STATE_5__n = 2;
#define ingress_mst_table_r_STP_STATE_5__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_5_t;

static const unsigned int ingress_mst_table_r_STP_STATE_6__n = 2;
#define ingress_mst_table_r_STP_STATE_6__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_6_t;

static const unsigned int ingress_mst_table_r_STP_STATE_7__n = 2;
#define ingress_mst_table_r_STP_STATE_7__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_7_t;

static const unsigned int ingress_mst_table_r_STP_STATE_8__n = 2;
#define ingress_mst_table_r_STP_STATE_8__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_8_t;

static const unsigned int ingress_mst_table_r_STP_STATE_9__n = 2;
#define ingress_mst_table_r_STP_STATE_9__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_9_t;

static const unsigned int ingress_mst_table_r_STP_STATE_10__n = 2;
#define ingress_mst_table_r_STP_STATE_10__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_10_t;

static const unsigned int ingress_mst_table_r_STP_STATE_11__n = 2;
#define ingress_mst_table_r_STP_STATE_11__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_11_t;

static const unsigned int ingress_mst_table_r_STP_STATE_12__n = 2;
#define ingress_mst_table_r_STP_STATE_12__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_12_t;

static const unsigned int ingress_mst_table_r_STP_STATE_13__n = 2;
#define ingress_mst_table_r_STP_STATE_13__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_13_t;

static const unsigned int ingress_mst_table_r_STP_STATE_14__n = 2;
#define ingress_mst_table_r_STP_STATE_14__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_14_t;

static const unsigned int ingress_mst_table_r_STP_STATE_15__n = 2;
#define ingress_mst_table_r_STP_STATE_15__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_15_t;

static const unsigned int ingress_mst_table_r_STP_STATE_16__n = 2;
#define ingress_mst_table_r_STP_STATE_16__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_16_t;

static const unsigned int ingress_mst_table_r_STP_STATE_17__n = 2;
#define ingress_mst_table_r_STP_STATE_17__nd    2
typedef uint2 ingress_mst_table_r_STP_STATE_17_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 SELECT;
} fwd_ieee_reserved_mac_trap_priority_r;

typedef struct {
  uint64 *SELECT;
} fwd_ieee_reserved_mac_trap_priority_r__addr;


void
fwd_ieee_reserved_mac_trap_priority_r__init(
  fwd_ieee_reserved_mac_trap_priority_r *p0,
  fwd_ieee_reserved_mac_trap_priority_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_ieee_reserved_mac_trap_priority_r_SELECT__n = 64;
#define fwd_ieee_reserved_mac_trap_priority_r_SELECT__nd    64
typedef uint64 fwd_ieee_reserved_mac_trap_priority_r_SELECT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint19 ACTION_BASE_PTR;
} lpm_subtrie_aptr_r;

typedef struct {
  uint19 *ACTION_BASE_PTR;
} lpm_subtrie_aptr_r__addr;


void
lpm_subtrie_aptr_r__init(
  lpm_subtrie_aptr_r *p0,
  lpm_subtrie_aptr_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_subtrie_aptr_r_ACTION_BASE_PTR__n = 19;
#define lpm_subtrie_aptr_r_ACTION_BASE_PTR__nd    19
typedef uint19 lpm_subtrie_aptr_r_ACTION_BASE_PTR_t;




/* ../src/RegC.m3:221 */
typedef lpm_subtrie_aptr_r lpm_subtrie_aptr_rf[512];

typedef lpm_subtrie_aptr_r__addr lpm_subtrie_aptr_rf__addr[512];


void
lpm_subtrie_aptr_rf__init(
  lpm_subtrie_aptr_rf *p0,
  lpm_subtrie_aptr_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_subtrie_aptr_rf_LPM_SUBTRIE_APTR__n = 512;
#define lpm_subtrie_aptr_rf_LPM_SUBTRIE_APTR__nd    512



/* ../src/RegC.m3:179 */
typedef struct {
  uint8 MEM_TO_EB;
  uint8 EB_TO_EB;
  uint8 SB_TO_EB;
  uint8 NB_TO_EB;
  uint8 MEM_TO_WB;
  uint8 WB_TO_WB;
  uint8 SB_TO_WB;
  uint8 NB_TO_WB;
} mesh_arb_rrsp_x_r;

typedef struct {
  uint8 *MEM_TO_EB;
  uint8 *EB_TO_EB;
  uint8 *SB_TO_EB;
  uint8 *NB_TO_EB;
  uint8 *MEM_TO_WB;
  uint8 *WB_TO_WB;
  uint8 *SB_TO_WB;
  uint8 *NB_TO_WB;
} mesh_arb_rrsp_x_r__addr;


void
mesh_arb_rrsp_x_r__init(
  mesh_arb_rrsp_x_r *p0,
  mesh_arb_rrsp_x_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mesh_arb_rrsp_x_r_MEM_TO_EB__n = 8;
#define mesh_arb_rrsp_x_r_MEM_TO_EB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_MEM_TO_EB_t;

static const unsigned int mesh_arb_rrsp_x_r_EB_TO_EB__n = 8;
#define mesh_arb_rrsp_x_r_EB_TO_EB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_EB_TO_EB_t;

static const unsigned int mesh_arb_rrsp_x_r_SB_TO_EB__n = 8;
#define mesh_arb_rrsp_x_r_SB_TO_EB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_SB_TO_EB_t;

static const unsigned int mesh_arb_rrsp_x_r_NB_TO_EB__n = 8;
#define mesh_arb_rrsp_x_r_NB_TO_EB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_NB_TO_EB_t;

static const unsigned int mesh_arb_rrsp_x_r_MEM_TO_WB__n = 8;
#define mesh_arb_rrsp_x_r_MEM_TO_WB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_MEM_TO_WB_t;

static const unsigned int mesh_arb_rrsp_x_r_WB_TO_WB__n = 8;
#define mesh_arb_rrsp_x_r_WB_TO_WB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_WB_TO_WB_t;

static const unsigned int mesh_arb_rrsp_x_r_SB_TO_WB__n = 8;
#define mesh_arb_rrsp_x_r_SB_TO_WB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_SB_TO_WB_t;

static const unsigned int mesh_arb_rrsp_x_r_NB_TO_WB__n = 8;
#define mesh_arb_rrsp_x_r_NB_TO_WB__nd    8
typedef uint8 mesh_arb_rrsp_x_r_NB_TO_WB_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint9 TAIL;
} ma_tcn_ptr_tail_r;

typedef struct {
  uint9 *TAIL;
} ma_tcn_ptr_tail_r__addr;


void
ma_tcn_ptr_tail_r__init(
  ma_tcn_ptr_tail_r *p0,
  ma_tcn_ptr_tail_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_ptr_tail_r_TAIL__n = 9;
#define ma_tcn_ptr_tail_r_TAIL__nd    9
typedef uint9 ma_tcn_ptr_tail_r_TAIL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 ACTION_0;
  uint2 ACTION_1;
  uint2 ACTION_2;
  uint2 ACTION_3;
  uint2 ACTION_4;
  uint2 ACTION_5;
  uint2 ACTION_6;
  uint2 ACTION_7;
  uint2 ACTION_8;
  uint2 ACTION_9;
  uint2 ACTION_10;
  uint2 ACTION_11;
  uint2 ACTION_12;
  uint2 ACTION_13;
  uint2 ACTION_14;
  uint2 ACTION_15;
  uint2 ACTION_16;
  uint2 ACTION_17;
  uint2 ACTION_18;
  uint2 ACTION_19;
  uint2 ACTION_20;
  uint2 ACTION_21;
  uint2 ACTION_22;
  uint2 ACTION_23;
  uint2 ACTION_24;
  uint2 ACTION_25;
  uint2 ACTION_26;
  uint2 ACTION_27;
  uint2 ACTION_28;
  uint2 ACTION_29;
  uint2 ACTION_30;
  uint2 ACTION_31;
  uint2 ACTION_32;
  uint2 ACTION_33;
  uint2 ACTION_34;
  uint2 ACTION_35;
  uint2 ACTION_36;
  uint2 ACTION_37;
  uint2 ACTION_38;
  uint2 ACTION_39;
  uint2 ACTION_40;
  uint2 ACTION_41;
  uint2 ACTION_42;
  uint2 ACTION_43;
  uint2 ACTION_44;
  uint2 ACTION_45;
  uint2 ACTION_46;
  uint2 ACTION_47;
  uint2 ACTION_48;
  uint2 ACTION_49;
  uint2 ACTION_50;
  uint2 ACTION_51;
  uint2 ACTION_52;
  uint2 ACTION_53;
  uint2 ACTION_54;
  uint2 ACTION_55;
  uint2 ACTION_56;
  uint2 ACTION_57;
  uint2 ACTION_58;
  uint2 ACTION_59;
  uint2 ACTION_60;
  uint2 ACTION_61;
  uint2 ACTION_62;
  uint2 ACTION_63;
} fwd_ieee_reserved_mac_action_r;

typedef struct {
  uint2 *ACTION_0;
  uint2 *ACTION_1;
  uint2 *ACTION_2;
  uint2 *ACTION_3;
  uint2 *ACTION_4;
  uint2 *ACTION_5;
  uint2 *ACTION_6;
  uint2 *ACTION_7;
  uint2 *ACTION_8;
  uint2 *ACTION_9;
  uint2 *ACTION_10;
  uint2 *ACTION_11;
  uint2 *ACTION_12;
  uint2 *ACTION_13;
  uint2 *ACTION_14;
  uint2 *ACTION_15;
  uint2 *ACTION_16;
  uint2 *ACTION_17;
  uint2 *ACTION_18;
  uint2 *ACTION_19;
  uint2 *ACTION_20;
  uint2 *ACTION_21;
  uint2 *ACTION_22;
  uint2 *ACTION_23;
  uint2 *ACTION_24;
  uint2 *ACTION_25;
  uint2 *ACTION_26;
  uint2 *ACTION_27;
  uint2 *ACTION_28;
  uint2 *ACTION_29;
  uint2 *ACTION_30;
  uint2 *ACTION_31;
  uint2 *ACTION_32;
  uint2 *ACTION_33;
  uint2 *ACTION_34;
  uint2 *ACTION_35;
  uint2 *ACTION_36;
  uint2 *ACTION_37;
  uint2 *ACTION_38;
  uint2 *ACTION_39;
  uint2 *ACTION_40;
  uint2 *ACTION_41;
  uint2 *ACTION_42;
  uint2 *ACTION_43;
  uint2 *ACTION_44;
  uint2 *ACTION_45;
  uint2 *ACTION_46;
  uint2 *ACTION_47;
  uint2 *ACTION_48;
  uint2 *ACTION_49;
  uint2 *ACTION_50;
  uint2 *ACTION_51;
  uint2 *ACTION_52;
  uint2 *ACTION_53;
  uint2 *ACTION_54;
  uint2 *ACTION_55;
  uint2 *ACTION_56;
  uint2 *ACTION_57;
  uint2 *ACTION_58;
  uint2 *ACTION_59;
  uint2 *ACTION_60;
  uint2 *ACTION_61;
  uint2 *ACTION_62;
  uint2 *ACTION_63;
} fwd_ieee_reserved_mac_action_r__addr;


void
fwd_ieee_reserved_mac_action_r__init(
  fwd_ieee_reserved_mac_action_r *p0,
  fwd_ieee_reserved_mac_action_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_0__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_0__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_0_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_1__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_1__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_1_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_2__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_2__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_2_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_3__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_3__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_3_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_4__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_4__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_4_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_5__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_5__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_5_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_6__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_6__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_6_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_7__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_7__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_7_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_8__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_8__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_8_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_9__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_9__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_9_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_10__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_10__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_10_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_11__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_11__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_11_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_12__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_12__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_12_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_13__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_13__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_13_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_14__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_14__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_14_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_15__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_15__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_15_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_16__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_16__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_16_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_17__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_17__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_17_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_18__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_18__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_18_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_19__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_19__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_19_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_20__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_20__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_20_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_21__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_21__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_21_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_22__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_22__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_22_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_23__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_23__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_23_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_24__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_24__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_24_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_25__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_25__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_25_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_26__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_26__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_26_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_27__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_27__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_27_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_28__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_28__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_28_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_29__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_29__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_29_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_30__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_30__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_30_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_31__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_31__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_31_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_32__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_32__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_32_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_33__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_33__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_33_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_34__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_34__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_34_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_35__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_35__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_35_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_36__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_36__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_36_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_37__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_37__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_37_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_38__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_38__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_38_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_39__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_39__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_39_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_40__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_40__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_40_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_41__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_41__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_41_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_42__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_42__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_42_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_43__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_43__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_43_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_44__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_44__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_44_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_45__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_45__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_45_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_46__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_46__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_46_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_47__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_47__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_47_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_48__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_48__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_48_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_49__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_49__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_49_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_50__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_50__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_50_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_51__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_51__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_51_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_52__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_52__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_52_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_53__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_53__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_53_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_54__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_54__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_54_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_55__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_55__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_55_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_56__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_56__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_56_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_57__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_57__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_57_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_58__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_58__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_58_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_59__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_59__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_59_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_60__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_60__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_60_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_61__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_61__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_61_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_62__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_62__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_62_t;

static const unsigned int fwd_ieee_reserved_mac_action_r_ACTION_63__n = 2;
#define fwd_ieee_reserved_mac_action_r_ACTION_63__nd    2
typedef uint2 fwd_ieee_reserved_mac_action_r_ACTION_63_t;




/* ../src/RegC.m3:221 */
typedef parser_ext_r parser_ext_rf[32];

typedef parser_ext_r__addr parser_ext_rf__addr[32];


void
parser_ext_rf__init(
  parser_ext_rf *p0,
  parser_ext_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ext_rf_PARSER_EXT__n = 32;
#define parser_ext_rf_PARSER_EXT__nd    32



/* ../src/RegC.m3:179 */
typedef struct {
  uint2 MAC_MBCAST;
  uint4 MAC_ROUTABLE;
  uint8 DOMAIN_PROFILE;
  uint8 PORT_PROFILE;
  uint4 _RSVD0_;
  uint6 L3_DOMAIN;
  uint8 L2_DOMAIN;
  uint10 PTYPE;
  uint14 _RSVD1_;
} map_profile_key_invert1_r;

typedef struct {
  uint2 *MAC_MBCAST;
  uint4 *MAC_ROUTABLE;
  uint8 *DOMAIN_PROFILE;
  uint8 *PORT_PROFILE;
  uint4 *_RSVD0_;
  uint6 *L3_DOMAIN;
  uint8 *L2_DOMAIN;
  uint10 *PTYPE;
  uint14 *_RSVD1_;
} map_profile_key_invert1_r__addr;


void
map_profile_key_invert1_r__init(
  map_profile_key_invert1_r *p0,
  map_profile_key_invert1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_profile_key_invert1_r_MAC_MBCAST__n = 2;
#define map_profile_key_invert1_r_MAC_MBCAST__nd    2
typedef uint2 map_profile_key_invert1_r_MAC_MBCAST_t;

static const unsigned int map_profile_key_invert1_r_MAC_ROUTABLE__n = 4;
#define map_profile_key_invert1_r_MAC_ROUTABLE__nd    4
typedef uint4 map_profile_key_invert1_r_MAC_ROUTABLE_t;

static const unsigned int map_profile_key_invert1_r_DOMAIN_PROFILE__n = 8;
#define map_profile_key_invert1_r_DOMAIN_PROFILE__nd    8
typedef uint8 map_profile_key_invert1_r_DOMAIN_PROFILE_t;

static const unsigned int map_profile_key_invert1_r_PORT_PROFILE__n = 8;
#define map_profile_key_invert1_r_PORT_PROFILE__nd    8
typedef uint8 map_profile_key_invert1_r_PORT_PROFILE_t;

static const unsigned int map_profile_key_invert1_r__RSVD0___n = 4;
#define map_profile_key_invert1_r__RSVD0___nd    4
typedef uint4 map_profile_key_invert1_r__RSVD0__t;

static const unsigned int map_profile_key_invert1_r_L3_DOMAIN__n = 6;
#define map_profile_key_invert1_r_L3_DOMAIN__nd    6
typedef uint6 map_profile_key_invert1_r_L3_DOMAIN_t;

static const unsigned int map_profile_key_invert1_r_L2_DOMAIN__n = 8;
#define map_profile_key_invert1_r_L2_DOMAIN__nd    8
typedef uint8 map_profile_key_invert1_r_L2_DOMAIN_t;

static const unsigned int map_profile_key_invert1_r_PTYPE__n = 10;
#define map_profile_key_invert1_r_PTYPE__nd    10
typedef uint10 map_profile_key_invert1_r_PTYPE_t;

static const unsigned int map_profile_key_invert1_r__RSVD1___n = 14;
#define map_profile_key_invert1_r__RSVD1___nd    14
typedef uint14 map_profile_key_invert1_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 KEY;
  uint32 KEY_INVERT;
} lpm_match_tcam_r;

typedef struct {
  uint32 *KEY;
  uint32 *KEY_INVERT;
} lpm_match_tcam_r__addr;


void
lpm_match_tcam_r__init(
  lpm_match_tcam_r *p0,
  lpm_match_tcam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_match_tcam_r_KEY__n = 32;
#define lpm_match_tcam_r_KEY__nd    32
typedef uint32 lpm_match_tcam_r_KEY_t;

static const unsigned int lpm_match_tcam_r_KEY_INVERT__n = 32;
#define lpm_match_tcam_r_KEY_INVERT__nd    32
typedef uint32 lpm_match_tcam_r_KEY_INVERT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint13 HANDLER_ACTION_MASK;
} trigger_condition_amask_2_r;

typedef struct {
  uint13 *HANDLER_ACTION_MASK;
} trigger_condition_amask_2_r__addr;


void
trigger_condition_amask_2_r__init(
  trigger_condition_amask_2_r *p0,
  trigger_condition_amask_2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_amask_2_r_HANDLER_ACTION_MASK__n = 13;
#define trigger_condition_amask_2_r_HANDLER_ACTION_MASK__nd    13
typedef uint13 trigger_condition_amask_2_r_HANDLER_ACTION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 VID;
  uint8 L2_DOMAIN;
} ma_tcn_data_1_r;

typedef struct {
  uint12 *VID;
  uint8 *L2_DOMAIN;
} ma_tcn_data_1_r__addr;


void
ma_tcn_data_1_r__init(
  ma_tcn_data_1_r *p0,
  ma_tcn_data_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_data_1_r_VID__n = 12;
#define ma_tcn_data_1_r_VID__nd    12
typedef uint12 ma_tcn_data_1_r_VID_t;

static const unsigned int ma_tcn_data_1_r_L2_DOMAIN__n = 8;
#define ma_tcn_data_1_r_L2_DOMAIN__nd    8
typedef uint8 ma_tcn_data_1_r_L2_DOMAIN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_1_2_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_1_2_r__addr;


void
fwd_port_cfg_1_2_r__init(
  fwd_port_cfg_1_2_r *p0,
  fwd_port_cfg_1_2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_1_2_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_1_2_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_1_2_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DATA;
} fwd_table_mod_r;

typedef struct {
  uint64 *DATA;
} fwd_table_mod_r__addr;


void
fwd_table_mod_r__init(
  fwd_table_mod_r *p0,
  fwd_table_mod_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_table_mod_r_DATA__n = 64;
#define fwd_table_mod_r_DATA__nd    64
typedef uint64 fwd_table_mod_r_DATA_t;




/* ../src/RegC.m3:221 */
typedef fwd_table_mod_r fwd_table_mod_rf[16384];

typedef fwd_table_mod_r__addr fwd_table_mod_rf__addr[16384];


void
fwd_table_mod_rf__init(
  fwd_table_mod_rf *p0,
  fwd_table_mod_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_table_mod_rf_FWD_TABLE_MOD__n = 16384;
#define fwd_table_mod_rf_FWD_TABLE_MOD__nd    16384



/* ../src/RegC.m3:179 */
typedef struct {
  uint1 GLORT_DEST_MASK;
  uint12 IP_MCAST_IDX;
} glort_dest_table_r4;

typedef struct {
  uint1 *GLORT_DEST_MASK;
  uint12 *IP_MCAST_IDX;
} glort_dest_table_r4__addr;


void
glort_dest_table_r4__init(
  glort_dest_table_r4 *p0,
  glort_dest_table_r4__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_dest_table_r4_GLORT_DEST_MASK__n = 1;
#define glort_dest_table_r4_GLORT_DEST_MASK__nd    1
typedef uint1 glort_dest_table_r4_GLORT_DEST_MASK_t;

static const unsigned int glort_dest_table_r4_IP_MCAST_IDX__n = 12;
#define glort_dest_table_r4_IP_MCAST_IDX__nd    12
typedef uint12 glort_dest_table_r4_IP_MCAST_IDX_t;




/* ../src/RegC.m3:221 */
typedef entropy_hash_cfg2_r entropy_hash_cfg2_rf[64];

typedef entropy_hash_cfg2_r__addr entropy_hash_cfg2_rf__addr[64];


void
entropy_hash_cfg2_rf__init(
  entropy_hash_cfg2_rf *p0,
  entropy_hash_cfg2_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_cfg2_rf_ENTROPY_HASH_CFG2__n = 64;
#define entropy_hash_cfg2_rf_ENTROPY_HASH_CFG2__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint16 FLOOD_UNICAST_GLORT;
  uint16 FLOOD_MULTICAST_GLORT;
  uint16 BROADCAST_GLORT;
} flood_glort_table_r;

typedef struct {
  uint16 *FLOOD_UNICAST_GLORT;
  uint16 *FLOOD_MULTICAST_GLORT;
  uint16 *BROADCAST_GLORT;
} flood_glort_table_r__addr;


void
flood_glort_table_r__init(
  flood_glort_table_r *p0,
  flood_glort_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int flood_glort_table_r_FLOOD_UNICAST_GLORT__n = 16;
#define flood_glort_table_r_FLOOD_UNICAST_GLORT__nd    16
typedef uint16 flood_glort_table_r_FLOOD_UNICAST_GLORT_t;

static const unsigned int flood_glort_table_r_FLOOD_MULTICAST_GLORT__n = 16;
#define flood_glort_table_r_FLOOD_MULTICAST_GLORT__nd    16
typedef uint16 flood_glort_table_r_FLOOD_MULTICAST_GLORT_t;

static const unsigned int flood_glort_table_r_BROADCAST_GLORT__n = 16;
#define flood_glort_table_r_BROADCAST_GLORT__nd    16
typedef uint16 flood_glort_table_r_BROADCAST_GLORT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint60 KEY_PAIRS;
} entropy_hash_sym8_r;

typedef struct {
  uint60 *KEY_PAIRS;
} entropy_hash_sym8_r__addr;


void
entropy_hash_sym8_r__init(
  entropy_hash_sym8_r *p0,
  entropy_hash_sym8_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_sym8_r_KEY_PAIRS__n = 60;
#define entropy_hash_sym8_r_KEY_PAIRS__nd    60
typedef uint60 entropy_hash_sym8_r_KEY_PAIRS_t;




/* ../src/RegC.m3:221 */
typedef entropy_hash_sym8_r entropy_hash_sym8_rf[4];

typedef entropy_hash_sym8_r__addr entropy_hash_sym8_rf__addr[4];


void
entropy_hash_sym8_rf__init(
  entropy_hash_sym8_rf *p0,
  entropy_hash_sym8_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_sym8_rf_ENTROPY_HASH_SYM8__n = 4;
#define entropy_hash_sym8_rf_ENTROPY_HASH_SYM8__nd    4



/* ../src/RegC.m3:221 */
typedef lpm_subtrie_cptr_r lpm_subtrie_cptr_rf[512];

typedef lpm_subtrie_cptr_r__addr lpm_subtrie_cptr_rf__addr[512];


void
lpm_subtrie_cptr_rf__init(
  lpm_subtrie_cptr_rf *p0,
  lpm_subtrie_cptr_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_subtrie_cptr_rf_LPM_SUBTRIE_CPTR__n = 512;
#define lpm_subtrie_cptr_rf_LPM_SUBTRIE_CPTR__nd    512



/* ../src/RegC.m3:179 */
typedef struct {
  uint48 POL_SWEEP_PERIOD_MAX;
} pol_sweep_period_max_r;

typedef struct {
  uint48 *POL_SWEEP_PERIOD_MAX;
} pol_sweep_period_max_r__addr;


void
pol_sweep_period_max_r__init(
  pol_sweep_period_max_r *p0,
  pol_sweep_period_max_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_sweep_period_max_r_POL_SWEEP_PERIOD_MAX__n = 48;
#define pol_sweep_period_max_r_POL_SWEEP_PERIOD_MAX__nd    48
typedef uint48 pol_sweep_period_max_r_POL_SWEEP_PERIOD_MAX_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_PORT_MASK;
} trigger_direct_map_ctx2_r;

typedef struct {
  uint64 *DEST_PORT_MASK;
} trigger_direct_map_ctx2_r__addr;


void
trigger_direct_map_ctx2_r__init(
  trigger_direct_map_ctx2_r *p0,
  trigger_direct_map_ctx2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_ctx2_r_DEST_PORT_MASK__n = 64;
#define trigger_direct_map_ctx2_r_DEST_PORT_MASK__nd    64
typedef uint64 trigger_direct_map_ctx2_r_DEST_PORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 SB_TO_SB;
  uint8 EB_TO_SB;
  uint8 WB_TO_SB;
  uint8 NB_TO_NB;
  uint8 EB_TO_NB;
  uint8 WB_TO_NB;
} mesh_arb_wreq_y_r;

typedef struct {
  uint8 *SB_TO_SB;
  uint8 *EB_TO_SB;
  uint8 *WB_TO_SB;
  uint8 *NB_TO_NB;
  uint8 *EB_TO_NB;
  uint8 *WB_TO_NB;
} mesh_arb_wreq_y_r__addr;


void
mesh_arb_wreq_y_r__init(
  mesh_arb_wreq_y_r *p0,
  mesh_arb_wreq_y_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mesh_arb_wreq_y_r_SB_TO_SB__n = 8;
#define mesh_arb_wreq_y_r_SB_TO_SB__nd    8
typedef uint8 mesh_arb_wreq_y_r_SB_TO_SB_t;

static const unsigned int mesh_arb_wreq_y_r_EB_TO_SB__n = 8;
#define mesh_arb_wreq_y_r_EB_TO_SB__nd    8
typedef uint8 mesh_arb_wreq_y_r_EB_TO_SB_t;

static const unsigned int mesh_arb_wreq_y_r_WB_TO_SB__n = 8;
#define mesh_arb_wreq_y_r_WB_TO_SB__nd    8
typedef uint8 mesh_arb_wreq_y_r_WB_TO_SB_t;

static const unsigned int mesh_arb_wreq_y_r_NB_TO_NB__n = 8;
#define mesh_arb_wreq_y_r_NB_TO_NB__nd    8
typedef uint8 mesh_arb_wreq_y_r_NB_TO_NB_t;

static const unsigned int mesh_arb_wreq_y_r_EB_TO_NB__n = 8;
#define mesh_arb_wreq_y_r_EB_TO_NB__nd    8
typedef uint8 mesh_arb_wreq_y_r_EB_TO_NB_t;

static const unsigned int mesh_arb_wreq_y_r_WB_TO_NB__n = 8;
#define mesh_arb_wreq_y_r_WB_TO_NB__nd    8
typedef uint8 mesh_arb_wreq_y_r_WB_TO_NB_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DROP_MASK;
} trigger_direct_map_adr3_r;

typedef struct {
  uint64 *DROP_MASK;
} trigger_direct_map_adr3_r__addr;


void
trigger_direct_map_adr3_r__init(
  trigger_direct_map_adr3_r *p0,
  trigger_direct_map_adr3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adr3_r_DROP_MASK__n = 64;
#define trigger_direct_map_adr3_r_DROP_MASK__nd    64
typedef uint64 trigger_direct_map_adr3_r_DROP_MASK_t;




/* ../src/RegC.m3:221 */
typedef mod_profile_field_r mod_profile_field_rf[8];

typedef mod_profile_field_r__addr mod_profile_field_rf__addr[8];


void
mod_profile_field_rf__init(
  mod_profile_field_rf *p0,
  mod_profile_field_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_profile_field_rf_MOD_PROFILE_FIELD__n = 8;
#define mod_profile_field_rf_MOD_PROFILE_FIELD__nd    8



/* ../src/RegC.m3:179 */
typedef struct {
  uint3 TRAP_TC;
} fwd_ieee_reserved_mac_cfg_r;

typedef struct {
  uint3 *TRAP_TC;
} fwd_ieee_reserved_mac_cfg_r__addr;


void
fwd_ieee_reserved_mac_cfg_r__init(
  fwd_ieee_reserved_mac_cfg_r *p0,
  fwd_ieee_reserved_mac_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_ieee_reserved_mac_cfg_r_TRAP_TC__n = 3;
#define fwd_ieee_reserved_mac_cfg_r_TRAP_TC__nd    3
typedef uint3 fwd_ieee_reserved_mac_cfg_r_TRAP_TC_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_MASK;
} cm_apply_cpu_trap_mask_3_r;

typedef struct {
  uint64 *DEST_MASK;
} cm_apply_cpu_trap_mask_3_r__addr;


void
cm_apply_cpu_trap_mask_3_r__init(
  cm_apply_cpu_trap_mask_3_r *p0,
  cm_apply_cpu_trap_mask_3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_cpu_trap_mask_3_r_DEST_MASK__n = 64;
#define cm_apply_cpu_trap_mask_3_r_DEST_MASK__nd    64
typedef uint64 cm_apply_cpu_trap_mask_3_r_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint6 MIRROR_PROFILE_IDX;
} fwd_rx_mirror_cfg_r;

typedef struct {
  uint6 *MIRROR_PROFILE_IDX;
} fwd_rx_mirror_cfg_r__addr;


void
fwd_rx_mirror_cfg_r__init(
  fwd_rx_mirror_cfg_r *p0,
  fwd_rx_mirror_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_rx_mirror_cfg_r_MIRROR_PROFILE_IDX__n = 6;
#define fwd_rx_mirror_cfg_r_MIRROR_PROFILE_IDX__nd    6
typedef uint6 fwd_rx_mirror_cfg_r_MIRROR_PROFILE_IDX_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint24 TIME_STORED;
  uint13 ETOK_LO;
  uint3 _RSVD_;
  uint24 ETOK_HI;
} pol_direct_map_pol0_r;

typedef struct {
  uint24 *TIME_STORED;
  uint13 *ETOK_LO;
  uint3 *_RSVD_;
  uint24 *ETOK_HI;
} pol_direct_map_pol0_r__addr;


void
pol_direct_map_pol0_r__init(
  pol_direct_map_pol0_r *p0,
  pol_direct_map_pol0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_pol0_r_TIME_STORED__n = 24;
#define pol_direct_map_pol0_r_TIME_STORED__nd    24
typedef uint24 pol_direct_map_pol0_r_TIME_STORED_t;

static const unsigned int pol_direct_map_pol0_r_ETOK_LO__n = 13;
#define pol_direct_map_pol0_r_ETOK_LO__nd    13
typedef uint13 pol_direct_map_pol0_r_ETOK_LO_t;

static const unsigned int pol_direct_map_pol0_r__RSVD___n = 3;
#define pol_direct_map_pol0_r__RSVD___nd    3
typedef uint3 pol_direct_map_pol0_r__RSVD__t;

static const unsigned int pol_direct_map_pol0_r_ETOK_HI__n = 24;
#define pol_direct_map_pol0_r_ETOK_HI__nd    24
typedef uint24 pol_direct_map_pol0_r_ETOK_HI_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 CAPACITY;
  uint12 RATE_MANTISSA;
  uint3 RATE_EXPONENT;
} trigger_rate_lim_cfg_1_r;

typedef struct {
  uint12 *CAPACITY;
  uint12 *RATE_MANTISSA;
  uint3 *RATE_EXPONENT;
} trigger_rate_lim_cfg_1_r__addr;


void
trigger_rate_lim_cfg_1_r__init(
  trigger_rate_lim_cfg_1_r *p0,
  trigger_rate_lim_cfg_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_rate_lim_cfg_1_r_CAPACITY__n = 12;
#define trigger_rate_lim_cfg_1_r_CAPACITY__nd    12
typedef uint12 trigger_rate_lim_cfg_1_r_CAPACITY_t;

static const unsigned int trigger_rate_lim_cfg_1_r_RATE_MANTISSA__n = 12;
#define trigger_rate_lim_cfg_1_r_RATE_MANTISSA__nd    12
typedef uint12 trigger_rate_lim_cfg_1_r_RATE_MANTISSA_t;

static const unsigned int trigger_rate_lim_cfg_1_r_RATE_EXPONENT__n = 3;
#define trigger_rate_lim_cfg_1_r_RATE_EXPONENT__nd    3
typedef uint3 trigger_rate_lim_cfg_1_r_RATE_EXPONENT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  trigger_rate_lim_cfg_1_r TRIGGER_RATE_LIM_CFG_1[16];
  trigger_rate_lim_usage_r TRIGGER_RATE_LIM_USAGE[16];
} mby_ppe_trig_usage_map;

typedef struct {
  trigger_rate_lim_cfg_1_r__addr TRIGGER_RATE_LIM_CFG_1[16];
  trigger_rate_lim_usage_r__addr TRIGGER_RATE_LIM_USAGE[16];
} mby_ppe_trig_usage_map__addr;


void
mby_ppe_trig_usage_map__init(
  mby_ppe_trig_usage_map *p0,
  mby_ppe_trig_usage_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_trig_usage_map_TRIGGER_RATE_LIM_CFG_1__n = 16;
#define mby_ppe_trig_usage_map_TRIGGER_RATE_LIM_CFG_1__nd    16
typedef trigger_rate_lim_cfg_1_r mby_ppe_trig_usage_map_TRIGGER_RATE_LIM_CFG_1_t[16];

static const unsigned int mby_ppe_trig_usage_map_TRIGGER_RATE_LIM_USAGE__n = 16;
#define mby_ppe_trig_usage_map_TRIGGER_RATE_LIM_USAGE__nd    16
typedef trigger_rate_lim_usage_r mby_ppe_trig_usage_map_TRIGGER_RATE_LIM_USAGE_t[16];




/* ../src/RegC.m3:179 */
typedef struct {
  uint3 NEW_TC;
  uint12 NEW_EVID;
  uint4 RATE_LIMIT_NUM;
  uint3 TRAP_CODE;
} trigger_action_cfg_2_r;

typedef struct {
  uint3 *NEW_TC;
  uint12 *NEW_EVID;
  uint4 *RATE_LIMIT_NUM;
  uint3 *TRAP_CODE;
} trigger_action_cfg_2_r__addr;


void
trigger_action_cfg_2_r__init(
  trigger_action_cfg_2_r *p0,
  trigger_action_cfg_2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_action_cfg_2_r_NEW_TC__n = 3;
#define trigger_action_cfg_2_r_NEW_TC__nd    3
typedef uint3 trigger_action_cfg_2_r_NEW_TC_t;

static const unsigned int trigger_action_cfg_2_r_NEW_EVID__n = 12;
#define trigger_action_cfg_2_r_NEW_EVID__nd    12
typedef uint12 trigger_action_cfg_2_r_NEW_EVID_t;

static const unsigned int trigger_action_cfg_2_r_RATE_LIMIT_NUM__n = 4;
#define trigger_action_cfg_2_r_RATE_LIMIT_NUM__nd    4
typedef uint4 trigger_action_cfg_2_r_RATE_LIMIT_NUM_t;

static const unsigned int trigger_action_cfg_2_r_TRAP_CODE__n = 3;
#define trigger_action_cfg_2_r_TRAP_CODE__nd    3
typedef uint3 trigger_action_cfg_2_r_TRAP_CODE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 MASK;
} trigger_im_r;

typedef struct {
  uint48 *MASK;
} trigger_im_r__addr;


void
trigger_im_r__init(
  trigger_im_r *p0,
  trigger_im_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_im_r_MASK__n = 48;
#define trigger_im_r_MASK__nd    48
typedef uint48 trigger_im_r_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 MD_KEY8_SEL;
  uint32 ADDR_KEY8_SEL;
} lpm_key_sel1_r;

typedef struct {
  uint32 *MD_KEY8_SEL;
  uint32 *ADDR_KEY8_SEL;
} lpm_key_sel1_r__addr;


void
lpm_key_sel1_r__init(
  lpm_key_sel1_r *p0,
  lpm_key_sel1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_key_sel1_r_MD_KEY8_SEL__n = 32;
#define lpm_key_sel1_r_MD_KEY8_SEL__nd    32
typedef uint32 lpm_key_sel1_r_MD_KEY8_SEL_t;

static const unsigned int lpm_key_sel1_r_ADDR_KEY8_SEL__n = 32;
#define lpm_key_sel1_r_ADDR_KEY8_SEL__nd    32
typedef uint32 lpm_key_sel1_r_ADDR_KEY8_SEL_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint24 TC_BY_EXP;
} map_exp_tc_r;

typedef struct {
  uint24 *TC_BY_EXP;
} map_exp_tc_r__addr;


void
map_exp_tc_r__init(
  map_exp_tc_r *p0,
  map_exp_tc_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_exp_tc_r_TC_BY_EXP__n = 24;
#define map_exp_tc_r_TC_BY_EXP__nd    24
typedef uint24 map_exp_tc_r_TC_BY_EXP_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 FCMN_SHELL_CTRL_ERR;
  uint1 TCAM_SWEEP_ERR;
  uint3 FGRP_ERR;
  uint3 FGHASH_ERR;
  uint4 HASH_ENTRY_RAM_U_ERR;
  uint4 HASH_ENTRY_RAM_C_ERR;
} wcm_im_r;

typedef struct {
  uint1 *FCMN_SHELL_CTRL_ERR;
  uint1 *TCAM_SWEEP_ERR;
  uint3 *FGRP_ERR;
  uint3 *FGHASH_ERR;
  uint4 *HASH_ENTRY_RAM_U_ERR;
  uint4 *HASH_ENTRY_RAM_C_ERR;
} wcm_im_r__addr;


void
wcm_im_r__init(
  wcm_im_r *p0,
  wcm_im_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_im_r_FCMN_SHELL_CTRL_ERR__n = 1;
#define wcm_im_r_FCMN_SHELL_CTRL_ERR__nd    1
typedef uint1 wcm_im_r_FCMN_SHELL_CTRL_ERR_t;

static const unsigned int wcm_im_r_TCAM_SWEEP_ERR__n = 1;
#define wcm_im_r_TCAM_SWEEP_ERR__nd    1
typedef uint1 wcm_im_r_TCAM_SWEEP_ERR_t;

static const unsigned int wcm_im_r_FGRP_ERR__n = 3;
#define wcm_im_r_FGRP_ERR__nd    3
typedef uint3 wcm_im_r_FGRP_ERR_t;

static const unsigned int wcm_im_r_FGHASH_ERR__n = 3;
#define wcm_im_r_FGHASH_ERR__nd    3
typedef uint3 wcm_im_r_FGHASH_ERR_t;

static const unsigned int wcm_im_r_HASH_ENTRY_RAM_U_ERR__n = 4;
#define wcm_im_r_HASH_ENTRY_RAM_U_ERR__nd    4
typedef uint4 wcm_im_r_HASH_ENTRY_RAM_U_ERR_t;

static const unsigned int wcm_im_r_HASH_ENTRY_RAM_C_ERR__n = 4;
#define wcm_im_r_HASH_ENTRY_RAM_C_ERR__nd    4
typedef uint4 wcm_im_r_HASH_ENTRY_RAM_C_ERR_t;




/* ../src/RegC.m3:221 */
typedef struct {
  tx_voq_port_cfg_r TX_VOQ_PORT_CFG;
} mby_tx_voq_map;

typedef struct {
  tx_voq_port_cfg_r__addr TX_VOQ_PORT_CFG;
} mby_tx_voq_map__addr;


void
mby_tx_voq_map__init(
  mby_tx_voq_map *p0,
  mby_tx_voq_map__addr *p1,
  void (*f)(void *, int)
);

typedef tx_voq_port_cfg_r mby_tx_voq_map_TX_VOQ_PORT_CFG_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_2_0_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_2_0_r__addr;


void
fwd_port_cfg_2_0_r__init(
  fwd_port_cfg_2_0_r *p0,
  fwd_port_cfg_2_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_2_0_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_2_0_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_2_0_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 VALIDATE_L3_LENGTH;
  uint1 COMPUTE_L4_CSUM;
  uint1 STORE_L4_PARTIAL_CSUM;
  uint2 VALIDATE_L4_CSUM;
  uint1 VALIDATE_IPV4_LENGTH;
  uint1 VALIDATE_IPV4_HDR_LENGTH;
  uint1 VALIDATE_IPV4_HDR_TRUNCATION;
  uint1 VALIDATE_IPV4_HDR_CSUM;
} parser_csum_cfg_r;

typedef struct {
  uint2 *VALIDATE_L3_LENGTH;
  uint1 *COMPUTE_L4_CSUM;
  uint1 *STORE_L4_PARTIAL_CSUM;
  uint2 *VALIDATE_L4_CSUM;
  uint1 *VALIDATE_IPV4_LENGTH;
  uint1 *VALIDATE_IPV4_HDR_LENGTH;
  uint1 *VALIDATE_IPV4_HDR_TRUNCATION;
  uint1 *VALIDATE_IPV4_HDR_CSUM;
} parser_csum_cfg_r__addr;


void
parser_csum_cfg_r__init(
  parser_csum_cfg_r *p0,
  parser_csum_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_csum_cfg_r_VALIDATE_L3_LENGTH__n = 2;
#define parser_csum_cfg_r_VALIDATE_L3_LENGTH__nd    2
typedef uint2 parser_csum_cfg_r_VALIDATE_L3_LENGTH_t;

static const unsigned int parser_csum_cfg_r_COMPUTE_L4_CSUM__n = 1;
#define parser_csum_cfg_r_COMPUTE_L4_CSUM__nd    1
typedef uint1 parser_csum_cfg_r_COMPUTE_L4_CSUM_t;

static const unsigned int parser_csum_cfg_r_STORE_L4_PARTIAL_CSUM__n = 1;
#define parser_csum_cfg_r_STORE_L4_PARTIAL_CSUM__nd    1
typedef uint1 parser_csum_cfg_r_STORE_L4_PARTIAL_CSUM_t;

static const unsigned int parser_csum_cfg_r_VALIDATE_L4_CSUM__n = 2;
#define parser_csum_cfg_r_VALIDATE_L4_CSUM__nd    2
typedef uint2 parser_csum_cfg_r_VALIDATE_L4_CSUM_t;

static const unsigned int parser_csum_cfg_r_VALIDATE_IPV4_LENGTH__n = 1;
#define parser_csum_cfg_r_VALIDATE_IPV4_LENGTH__nd    1
typedef uint1 parser_csum_cfg_r_VALIDATE_IPV4_LENGTH_t;

static const unsigned int parser_csum_cfg_r_VALIDATE_IPV4_HDR_LENGTH__n = 1;
#define parser_csum_cfg_r_VALIDATE_IPV4_HDR_LENGTH__nd    1
typedef uint1 parser_csum_cfg_r_VALIDATE_IPV4_HDR_LENGTH_t;

static const unsigned int parser_csum_cfg_r_VALIDATE_IPV4_HDR_TRUNCATION__n = 1;
#define parser_csum_cfg_r_VALIDATE_IPV4_HDR_TRUNCATION__nd    1
typedef uint1 parser_csum_cfg_r_VALIDATE_IPV4_HDR_TRUNCATION_t;

static const unsigned int parser_csum_cfg_r_VALIDATE_IPV4_HDR_CSUM__n = 1;
#define parser_csum_cfg_r_VALIDATE_IPV4_HDR_CSUM__nd    1
typedef uint1 parser_csum_cfg_r_VALIDATE_IPV4_HDR_CSUM_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 TRAP_TTL1;
  uint1 TRAP_IP_OPTIONS;
} fwd_sys_cfg_router_r;

typedef struct {
  uint2 *TRAP_TTL1;
  uint1 *TRAP_IP_OPTIONS;
} fwd_sys_cfg_router_r__addr;


void
fwd_sys_cfg_router_r__init(
  fwd_sys_cfg_router_r *p0,
  fwd_sys_cfg_router_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_sys_cfg_router_r_TRAP_TTL1__n = 2;
#define fwd_sys_cfg_router_r_TRAP_TTL1__nd    2
typedef uint2 fwd_sys_cfg_router_r_TRAP_TTL1_t;

static const unsigned int fwd_sys_cfg_router_r_TRAP_IP_OPTIONS__n = 1;
#define fwd_sys_cfg_router_r_TRAP_IP_OPTIONS__nd    1
typedef uint1 fwd_sys_cfg_router_r_TRAP_IP_OPTIONS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 TARGET;
  uint16 VALUE;
} map_port_default_r;

typedef struct {
  uint8 *TARGET;
  uint16 *VALUE;
} map_port_default_r__addr;


void
map_port_default_r__init(
  map_port_default_r *p0,
  map_port_default_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_port_default_r_TARGET__n = 8;
#define map_port_default_r_TARGET__nd    8
typedef uint8 map_port_default_r_TARGET_t;

static const unsigned int map_port_default_r_VALUE__n = 16;
#define map_port_default_r_VALUE__nd    16
typedef uint16 map_port_default_r_VALUE_t;




/* ../src/RegC.m3:221 */
typedef map_port_default_r map_port_default_rf[6];

typedef map_port_default_r__addr map_port_default_rf__addr[6];


void
map_port_default_rf__init(
  map_port_default_rf *p0,
  map_port_default_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_port_default_rf_MAP_PORT_DEFAULT__n = 6;
#define map_port_default_rf_MAP_PORT_DEFAULT__nd    6



/* ../src/RegC.m3:179 */
typedef struct {
  uint5 ENTRY_SIZE_1;
  uint5 ENTRY_SIZE_0;
  uint5 HASH_SIZE_1;
  uint5 HASH_SIZE_0;
  uint13 BASE_PTR_1;
  uint13 BASE_PTR_0;
  uint1 MODE;
  uint1 HASH_LO;
  uint1 HASH_HI;
} em_hash_cfg_r;

typedef struct {
  uint5 *ENTRY_SIZE_1;
  uint5 *ENTRY_SIZE_0;
  uint5 *HASH_SIZE_1;
  uint5 *HASH_SIZE_0;
  uint13 *BASE_PTR_1;
  uint13 *BASE_PTR_0;
  uint1 *MODE;
  uint1 *HASH_LO;
  uint1 *HASH_HI;
} em_hash_cfg_r__addr;


void
em_hash_cfg_r__init(
  em_hash_cfg_r *p0,
  em_hash_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_cfg_r_ENTRY_SIZE_1__n = 5;
#define em_hash_cfg_r_ENTRY_SIZE_1__nd    5
typedef uint5 em_hash_cfg_r_ENTRY_SIZE_1_t;

static const unsigned int em_hash_cfg_r_ENTRY_SIZE_0__n = 5;
#define em_hash_cfg_r_ENTRY_SIZE_0__nd    5
typedef uint5 em_hash_cfg_r_ENTRY_SIZE_0_t;

static const unsigned int em_hash_cfg_r_HASH_SIZE_1__n = 5;
#define em_hash_cfg_r_HASH_SIZE_1__nd    5
typedef uint5 em_hash_cfg_r_HASH_SIZE_1_t;

static const unsigned int em_hash_cfg_r_HASH_SIZE_0__n = 5;
#define em_hash_cfg_r_HASH_SIZE_0__nd    5
typedef uint5 em_hash_cfg_r_HASH_SIZE_0_t;

static const unsigned int em_hash_cfg_r_BASE_PTR_1__n = 13;
#define em_hash_cfg_r_BASE_PTR_1__nd    13
typedef uint13 em_hash_cfg_r_BASE_PTR_1_t;

static const unsigned int em_hash_cfg_r_BASE_PTR_0__n = 13;
#define em_hash_cfg_r_BASE_PTR_0__nd    13
typedef uint13 em_hash_cfg_r_BASE_PTR_0_t;

static const unsigned int em_hash_cfg_r_MODE__n = 1;
#define em_hash_cfg_r_MODE__nd    1
typedef uint1 em_hash_cfg_r_MODE_t;

static const unsigned int em_hash_cfg_r_HASH_LO__n = 1;
#define em_hash_cfg_r_HASH_LO__nd    1
typedef uint1 em_hash_cfg_r_HASH_LO_t;

static const unsigned int em_hash_cfg_r_HASH_HI__n = 1;
#define em_hash_cfg_r_HASH_HI__nd    1
typedef uint1 em_hash_cfg_r_HASH_HI_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 TCN_OVERFLOW;
  uint1 PENDING_EVENTS;
} ma_tcn_ip_r;

typedef struct {
  uint1 *TCN_OVERFLOW;
  uint1 *PENDING_EVENTS;
} ma_tcn_ip_r__addr;


void
ma_tcn_ip_r__init(
  ma_tcn_ip_r *p0,
  ma_tcn_ip_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_ip_r_TCN_OVERFLOW__n = 1;
#define ma_tcn_ip_r_TCN_OVERFLOW__nd    1
typedef uint1 ma_tcn_ip_r_TCN_OVERFLOW_t;

static const unsigned int ma_tcn_ip_r_PENDING_EVENTS__n = 1;
#define ma_tcn_ip_r_PENDING_EVENTS__nd    1
typedef uint1 ma_tcn_ip_r_PENDING_EVENTS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 MEM_TO_SB;
  uint8 SB_TO_SB;
  uint8 EB_TO_SB;
  uint8 WB_TO_SB;
  uint8 MEM_TO_NB;
  uint8 NB_TO_NB;
  uint8 EB_TO_NB;
  uint8 WB_TO_NB;
} mesh_arb_rrsp_y_r;

typedef struct {
  uint8 *MEM_TO_SB;
  uint8 *SB_TO_SB;
  uint8 *EB_TO_SB;
  uint8 *WB_TO_SB;
  uint8 *MEM_TO_NB;
  uint8 *NB_TO_NB;
  uint8 *EB_TO_NB;
  uint8 *WB_TO_NB;
} mesh_arb_rrsp_y_r__addr;


void
mesh_arb_rrsp_y_r__init(
  mesh_arb_rrsp_y_r *p0,
  mesh_arb_rrsp_y_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mesh_arb_rrsp_y_r_MEM_TO_SB__n = 8;
#define mesh_arb_rrsp_y_r_MEM_TO_SB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_MEM_TO_SB_t;

static const unsigned int mesh_arb_rrsp_y_r_SB_TO_SB__n = 8;
#define mesh_arb_rrsp_y_r_SB_TO_SB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_SB_TO_SB_t;

static const unsigned int mesh_arb_rrsp_y_r_EB_TO_SB__n = 8;
#define mesh_arb_rrsp_y_r_EB_TO_SB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_EB_TO_SB_t;

static const unsigned int mesh_arb_rrsp_y_r_WB_TO_SB__n = 8;
#define mesh_arb_rrsp_y_r_WB_TO_SB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_WB_TO_SB_t;

static const unsigned int mesh_arb_rrsp_y_r_MEM_TO_NB__n = 8;
#define mesh_arb_rrsp_y_r_MEM_TO_NB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_MEM_TO_NB_t;

static const unsigned int mesh_arb_rrsp_y_r_NB_TO_NB__n = 8;
#define mesh_arb_rrsp_y_r_NB_TO_NB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_NB_TO_NB_t;

static const unsigned int mesh_arb_rrsp_y_r_EB_TO_NB__n = 8;
#define mesh_arb_rrsp_y_r_EB_TO_NB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_EB_TO_NB_t;

static const unsigned int mesh_arb_rrsp_y_r_WB_TO_NB__n = 8;
#define mesh_arb_rrsp_y_r_WB_TO_NB__nd    8
typedef uint8 mesh_arb_rrsp_y_r_WB_TO_NB_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mesh_arb_rreq_y_r MESH_ARB_RREQ_Y[8];
  mesh_arb_wreq_y_r MESH_ARB_WREQ_Y[8];
  mesh_arb_rrsp_y_r MESH_ARB_RRSP_Y[8];
  mesh_arb_rrsp_x_r MESH_ARB_RRSP_X[8];
} mby_mesh_row_map;

typedef struct {
  mesh_arb_rreq_y_r__addr MESH_ARB_RREQ_Y[8];
  mesh_arb_wreq_y_r__addr MESH_ARB_WREQ_Y[8];
  mesh_arb_rrsp_y_r__addr MESH_ARB_RRSP_Y[8];
  mesh_arb_rrsp_x_r__addr MESH_ARB_RRSP_X[8];
} mby_mesh_row_map__addr;


void
mby_mesh_row_map__init(
  mby_mesh_row_map *p0,
  mby_mesh_row_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_mesh_row_map_MESH_ARB_RREQ_Y__n = 8;
#define mby_mesh_row_map_MESH_ARB_RREQ_Y__nd    8
typedef mesh_arb_rreq_y_r mby_mesh_row_map_MESH_ARB_RREQ_Y_t[8];

static const unsigned int mby_mesh_row_map_MESH_ARB_WREQ_Y__n = 8;
#define mby_mesh_row_map_MESH_ARB_WREQ_Y__nd    8
typedef mesh_arb_wreq_y_r mby_mesh_row_map_MESH_ARB_WREQ_Y_t[8];

static const unsigned int mby_mesh_row_map_MESH_ARB_RRSP_Y__n = 8;
#define mby_mesh_row_map_MESH_ARB_RRSP_Y__nd    8
typedef mesh_arb_rrsp_y_r mby_mesh_row_map_MESH_ARB_RRSP_Y_t[8];

static const unsigned int mby_mesh_row_map_MESH_ARB_RRSP_X__n = 8;
#define mby_mesh_row_map_MESH_ARB_RRSP_X__nd    8
typedef mesh_arb_rrsp_x_r mby_mesh_row_map_MESH_ARB_RRSP_X_t[8];




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 EMPTY;
} trigger_rate_lim_empty_r;

typedef struct {
  uint16 *EMPTY;
} trigger_rate_lim_empty_r__addr;


void
trigger_rate_lim_empty_r__init(
  trigger_rate_lim_empty_r *p0,
  trigger_rate_lim_empty_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_rate_lim_empty_r_EMPTY__n = 16;
#define trigger_rate_lim_empty_r_EMPTY__nd    16
typedef uint16 trigger_rate_lim_empty_r_EMPTY_t;




/* ../src/RegC.m3:221 */
typedef cm_rx_smp_pause_wm_r cm_rx_smp_pause_wm_rf[2];

typedef cm_rx_smp_pause_wm_r__addr cm_rx_smp_pause_wm_rf__addr[2];


void
cm_rx_smp_pause_wm_rf__init(
  cm_rx_smp_pause_wm_rf *p0,
  cm_rx_smp_pause_wm_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_pause_wm_rf_CM_RX_SMP_PAUSE_WM__n = 2;
#define cm_rx_smp_pause_wm_rf_CM_RX_SMP_PAUSE_WM__nd    2



/* ../src/RegC.m3:221 */
typedef cm_tx_ewma_r cm_tx_ewma_rf[2];

typedef cm_tx_ewma_r__addr cm_tx_ewma_rf__addr[2];


void
cm_tx_ewma_rf__init(
  cm_tx_ewma_rf *p0,
  cm_tx_ewma_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_ewma_rf_CM_TX_EWMA__n = 2;
#define cm_tx_ewma_rf_CM_TX_EWMA__nd    2



/* ../src/RegC.m3:221 */
typedef wcm_tcam_r wcm_tcam_rf[1024];

typedef wcm_tcam_r__addr wcm_tcam_rf__addr[1024];


void
wcm_tcam_rf__init(
  wcm_tcam_rf *p0,
  wcm_tcam_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_tcam_rf_WCM_TCAM__n = 1024;
#define wcm_tcam_rf_WCM_TCAM__nd    1024



/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WATERMARK;
} cm_tx_tc_hog_wm_r;

typedef struct {
  uint15 *WATERMARK;
} cm_tx_tc_hog_wm_r__addr;


void
cm_tx_tc_hog_wm_r__init(
  cm_tx_tc_hog_wm_r *p0,
  cm_tx_tc_hog_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_tc_hog_wm_r_WATERMARK__n = 15;
#define cm_tx_tc_hog_wm_r_WATERMARK__nd    15
typedef uint15 cm_tx_tc_hog_wm_r_WATERMARK_t;




/* ../src/RegC.m3:221 */
typedef cm_tx_tc_hog_wm_r cm_tx_tc_hog_wm_rf[8];

typedef cm_tx_tc_hog_wm_r__addr cm_tx_tc_hog_wm_rf__addr[8];


void
cm_tx_tc_hog_wm_rf__init(
  cm_tx_tc_hog_wm_rf *p0,
  cm_tx_tc_hog_wm_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_tc_hog_wm_rf_CM_TX_TC_HOG_WM__n = 8;
#define cm_tx_tc_hog_wm_rf_CM_TX_TC_HOG_WM__nd    8



/* ../src/RegC.m3:221 */
typedef em_key_sel1_r em_key_sel1_rf[64];

typedef em_key_sel1_r__addr em_key_sel1_rf__addr[64];


void
em_key_sel1_rf__init(
  em_key_sel1_rf *p0,
  em_key_sel1_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_key_sel1_rf_KEY_SEL1__n = 64;
#define em_key_sel1_rf_KEY_SEL1__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_1_3_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_1_3_r__addr;


void
fwd_port_cfg_1_3_r__init(
  fwd_port_cfg_1_3_r *p0,
  fwd_port_cfg_1_3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_1_3_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_1_3_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_1_3_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 GROUP_TYPE;
  uint14 BASE_INDEX;
  uint1 N_GROUP_SZ_TYPE;
  uint6 N_GROUP_SIZE;
  uint11 WEIGHT_ROW;
  uint6 WEIGHT_ROW_OFFSET;
} nexthop_groups_table_0_r;

typedef struct {
  uint2 *GROUP_TYPE;
  uint14 *BASE_INDEX;
  uint1 *N_GROUP_SZ_TYPE;
  uint6 *N_GROUP_SIZE;
  uint11 *WEIGHT_ROW;
  uint6 *WEIGHT_ROW_OFFSET;
} nexthop_groups_table_0_r__addr;


void
nexthop_groups_table_0_r__init(
  nexthop_groups_table_0_r *p0,
  nexthop_groups_table_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_groups_table_0_r_GROUP_TYPE__n = 2;
#define nexthop_groups_table_0_r_GROUP_TYPE__nd    2
typedef uint2 nexthop_groups_table_0_r_GROUP_TYPE_t;

static const unsigned int nexthop_groups_table_0_r_BASE_INDEX__n = 14;
#define nexthop_groups_table_0_r_BASE_INDEX__nd    14
typedef uint14 nexthop_groups_table_0_r_BASE_INDEX_t;

static const unsigned int nexthop_groups_table_0_r_N_GROUP_SZ_TYPE__n = 1;
#define nexthop_groups_table_0_r_N_GROUP_SZ_TYPE__nd    1
typedef uint1 nexthop_groups_table_0_r_N_GROUP_SZ_TYPE_t;

static const unsigned int nexthop_groups_table_0_r_N_GROUP_SIZE__n = 6;
#define nexthop_groups_table_0_r_N_GROUP_SIZE__nd    6
typedef uint6 nexthop_groups_table_0_r_N_GROUP_SIZE_t;

static const unsigned int nexthop_groups_table_0_r_WEIGHT_ROW__n = 11;
#define nexthop_groups_table_0_r_WEIGHT_ROW__nd    11
typedef uint11 nexthop_groups_table_0_r_WEIGHT_ROW_t;

static const unsigned int nexthop_groups_table_0_r_WEIGHT_ROW_OFFSET__n = 6;
#define nexthop_groups_table_0_r_WEIGHT_ROW_OFFSET__nd    6
typedef uint6 nexthop_groups_table_0_r_WEIGHT_ROW_OFFSET_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 MIRROR_SESSION;
  uint6 MIRROR_PROFILE_IDX;
} fwd_qcn_mirror_cfg_r;

typedef struct {
  uint2 *MIRROR_SESSION;
  uint6 *MIRROR_PROFILE_IDX;
} fwd_qcn_mirror_cfg_r__addr;


void
fwd_qcn_mirror_cfg_r__init(
  fwd_qcn_mirror_cfg_r *p0,
  fwd_qcn_mirror_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_qcn_mirror_cfg_r_MIRROR_SESSION__n = 2;
#define fwd_qcn_mirror_cfg_r_MIRROR_SESSION__nd    2
typedef uint2 fwd_qcn_mirror_cfg_r_MIRROR_SESSION_t;

static const unsigned int fwd_qcn_mirror_cfg_r_MIRROR_PROFILE_IDX__n = 6;
#define fwd_qcn_mirror_cfg_r_MIRROR_PROFILE_IDX__nd    6
typedef uint6 fwd_qcn_mirror_cfg_r_MIRROR_PROFILE_IDX_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint7 SELECT3;
  uint7 SELECT2;
  uint7 SELECT1;
  uint7 SELECT0;
  uint6 SELECT_TOP;
  uint1 START_SET;
  uint1 START_COMPARE;
  uint16 CHUNK_MASK;
} wcm_tcam_cfg_r;

typedef struct {
  uint7 *SELECT3;
  uint7 *SELECT2;
  uint7 *SELECT1;
  uint7 *SELECT0;
  uint6 *SELECT_TOP;
  uint1 *START_SET;
  uint1 *START_COMPARE;
  uint16 *CHUNK_MASK;
} wcm_tcam_cfg_r__addr;


void
wcm_tcam_cfg_r__init(
  wcm_tcam_cfg_r *p0,
  wcm_tcam_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_tcam_cfg_r_SELECT3__n = 7;
#define wcm_tcam_cfg_r_SELECT3__nd    7
typedef uint7 wcm_tcam_cfg_r_SELECT3_t;

static const unsigned int wcm_tcam_cfg_r_SELECT2__n = 7;
#define wcm_tcam_cfg_r_SELECT2__nd    7
typedef uint7 wcm_tcam_cfg_r_SELECT2_t;

static const unsigned int wcm_tcam_cfg_r_SELECT1__n = 7;
#define wcm_tcam_cfg_r_SELECT1__nd    7
typedef uint7 wcm_tcam_cfg_r_SELECT1_t;

static const unsigned int wcm_tcam_cfg_r_SELECT0__n = 7;
#define wcm_tcam_cfg_r_SELECT0__nd    7
typedef uint7 wcm_tcam_cfg_r_SELECT0_t;

static const unsigned int wcm_tcam_cfg_r_SELECT_TOP__n = 6;
#define wcm_tcam_cfg_r_SELECT_TOP__nd    6
typedef uint6 wcm_tcam_cfg_r_SELECT_TOP_t;

static const unsigned int wcm_tcam_cfg_r_START_SET__n = 1;
#define wcm_tcam_cfg_r_START_SET__nd    1
typedef uint1 wcm_tcam_cfg_r_START_SET_t;

static const unsigned int wcm_tcam_cfg_r_START_COMPARE__n = 1;
#define wcm_tcam_cfg_r_START_COMPARE__nd    1
typedef uint1 wcm_tcam_cfg_r_START_COMPARE_t;

static const unsigned int wcm_tcam_cfg_r_CHUNK_MASK__n = 16;
#define wcm_tcam_cfg_r_CHUNK_MASK__nd    16
typedef uint16 wcm_tcam_cfg_r_CHUNK_MASK_t;




/* ../src/RegC.m3:221 */
typedef wcm_tcam_cfg_r wcm_tcam_cfg_rf[64];

typedef wcm_tcam_cfg_r__addr wcm_tcam_cfg_rf__addr[64];


void
wcm_tcam_cfg_rf__init(
  wcm_tcam_cfg_rf *p0,
  wcm_tcam_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_tcam_cfg_rf_WCM_TCAM_CFG__n = 64;
#define wcm_tcam_cfg_rf_WCM_TCAM_CFG__nd    64



/* ../src/RegC.m3:221 */
typedef mod_profile_command_r mod_profile_command_rf[16];

typedef mod_profile_command_r__addr mod_profile_command_rf__addr[16];


void
mod_profile_command_rf__init(
  mod_profile_command_rf *p0,
  mod_profile_command_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_profile_command_rf_MOD_PROFILE_COMMAND__n = 16;
#define mod_profile_command_rf_MOD_PROFILE_COMMAND__nd    16



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_PORT_MASK;
} trigger_direct_map_ctx3_r;

typedef struct {
  uint64 *DEST_PORT_MASK;
} trigger_direct_map_ctx3_r__addr;


void
trigger_direct_map_ctx3_r__init(
  trigger_direct_map_ctx3_r *p0,
  trigger_direct_map_ctx3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_ctx3_r_DEST_PORT_MASK__n = 64;
#define trigger_direct_map_ctx3_r_DEST_PORT_MASK__nd    64
typedef uint64 trigger_direct_map_ctx3_r_DEST_PORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 NEW_DEST_MASK;
  uint1 FILTER_DEST_MASK;
} trigger_direct_map_adm4_r;

typedef struct {
  uint2 *NEW_DEST_MASK;
  uint1 *FILTER_DEST_MASK;
} trigger_direct_map_adm4_r__addr;


void
trigger_direct_map_adm4_r__init(
  trigger_direct_map_adm4_r *p0,
  trigger_direct_map_adm4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adm4_r_NEW_DEST_MASK__n = 2;
#define trigger_direct_map_adm4_r_NEW_DEST_MASK__nd    2
typedef uint2 trigger_direct_map_adm4_r_NEW_DEST_MASK_t;

static const unsigned int trigger_direct_map_adm4_r_FILTER_DEST_MASK__n = 1;
#define trigger_direct_map_adm4_r_FILTER_DEST_MASK__nd    1
typedef uint1 trigger_direct_map_adm4_r_FILTER_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint3 DSCP_TGT;
  uint3 VPRI_TGT;
  uint1 PRIOS_VALID;
  uint7 IP_OPTIONS_MASK;
  uint1 PARSER_ERROR;
  uint8 PROFILE_TRIG;
  uint1 TRIG_VALID;
  uint4 REWRITE_PROFILE;
  uint6 PROFILE;
  uint1 PROFILE_VALID;
} map_profile_action_r;

typedef struct {
  uint3 *DSCP_TGT;
  uint3 *VPRI_TGT;
  uint1 *PRIOS_VALID;
  uint7 *IP_OPTIONS_MASK;
  uint1 *PARSER_ERROR;
  uint8 *PROFILE_TRIG;
  uint1 *TRIG_VALID;
  uint4 *REWRITE_PROFILE;
  uint6 *PROFILE;
  uint1 *PROFILE_VALID;
} map_profile_action_r__addr;


void
map_profile_action_r__init(
  map_profile_action_r *p0,
  map_profile_action_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_profile_action_r_DSCP_TGT__n = 3;
#define map_profile_action_r_DSCP_TGT__nd    3
typedef uint3 map_profile_action_r_DSCP_TGT_t;

static const unsigned int map_profile_action_r_VPRI_TGT__n = 3;
#define map_profile_action_r_VPRI_TGT__nd    3
typedef uint3 map_profile_action_r_VPRI_TGT_t;

static const unsigned int map_profile_action_r_PRIOS_VALID__n = 1;
#define map_profile_action_r_PRIOS_VALID__nd    1
typedef uint1 map_profile_action_r_PRIOS_VALID_t;

static const unsigned int map_profile_action_r_IP_OPTIONS_MASK__n = 7;
#define map_profile_action_r_IP_OPTIONS_MASK__nd    7
typedef uint7 map_profile_action_r_IP_OPTIONS_MASK_t;

static const unsigned int map_profile_action_r_PARSER_ERROR__n = 1;
#define map_profile_action_r_PARSER_ERROR__nd    1
typedef uint1 map_profile_action_r_PARSER_ERROR_t;

static const unsigned int map_profile_action_r_PROFILE_TRIG__n = 8;
#define map_profile_action_r_PROFILE_TRIG__nd    8
typedef uint8 map_profile_action_r_PROFILE_TRIG_t;

static const unsigned int map_profile_action_r_TRIG_VALID__n = 1;
#define map_profile_action_r_TRIG_VALID__nd    1
typedef uint1 map_profile_action_r_TRIG_VALID_t;

static const unsigned int map_profile_action_r_REWRITE_PROFILE__n = 4;
#define map_profile_action_r_REWRITE_PROFILE__nd    4
typedef uint4 map_profile_action_r_REWRITE_PROFILE_t;

static const unsigned int map_profile_action_r_PROFILE__n = 6;
#define map_profile_action_r_PROFILE__nd    6
typedef uint6 map_profile_action_r_PROFILE_t;

static const unsigned int map_profile_action_r_PROFILE_VALID__n = 1;
#define map_profile_action_r_PROFILE_VALID__nd    1
typedef uint1 map_profile_action_r_PROFILE_VALID_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint11 _RSVD0_;
  uint1 L3_LEN_MODE;
  uint1 DEBIT_MODE;
  uint1 PRECEDENCE;
  uint1 COLOR_SELECT;
  uint1 CF;
  uint1 CB;
  uint2 _RSVD1_;
  uint1 CREDIT_L3_LEN_ERR;
  uint1 CREDIT_FRAME_ERR;
  uint1 UNPOLICE_DROP_PRECM;
  uint1 UNPOLICE_DROP_CM;
  uint41 _RSVD2_;
} pol_cfg_r;

typedef struct {
  uint11 *_RSVD0_;
  uint1 *L3_LEN_MODE;
  uint1 *DEBIT_MODE;
  uint1 *PRECEDENCE;
  uint1 *COLOR_SELECT;
  uint1 *CF;
  uint1 *CB;
  uint2 *_RSVD1_;
  uint1 *CREDIT_L3_LEN_ERR;
  uint1 *CREDIT_FRAME_ERR;
  uint1 *UNPOLICE_DROP_PRECM;
  uint1 *UNPOLICE_DROP_CM;
  uint41 *_RSVD2_;
} pol_cfg_r__addr;


void
pol_cfg_r__init(
  pol_cfg_r *p0,
  pol_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_cfg_r__RSVD0___n = 11;
#define pol_cfg_r__RSVD0___nd    11
typedef uint11 pol_cfg_r__RSVD0__t;

static const unsigned int pol_cfg_r_L3_LEN_MODE__n = 1;
#define pol_cfg_r_L3_LEN_MODE__nd    1
typedef uint1 pol_cfg_r_L3_LEN_MODE_t;

static const unsigned int pol_cfg_r_DEBIT_MODE__n = 1;
#define pol_cfg_r_DEBIT_MODE__nd    1
typedef uint1 pol_cfg_r_DEBIT_MODE_t;

static const unsigned int pol_cfg_r_PRECEDENCE__n = 1;
#define pol_cfg_r_PRECEDENCE__nd    1
typedef uint1 pol_cfg_r_PRECEDENCE_t;

static const unsigned int pol_cfg_r_COLOR_SELECT__n = 1;
#define pol_cfg_r_COLOR_SELECT__nd    1
typedef uint1 pol_cfg_r_COLOR_SELECT_t;

static const unsigned int pol_cfg_r_CF__n = 1;
#define pol_cfg_r_CF__nd    1
typedef uint1 pol_cfg_r_CF_t;

static const unsigned int pol_cfg_r_CB__n = 1;
#define pol_cfg_r_CB__nd    1
typedef uint1 pol_cfg_r_CB_t;

static const unsigned int pol_cfg_r__RSVD1___n = 2;
#define pol_cfg_r__RSVD1___nd    2
typedef uint2 pol_cfg_r__RSVD1__t;

static const unsigned int pol_cfg_r_CREDIT_L3_LEN_ERR__n = 1;
#define pol_cfg_r_CREDIT_L3_LEN_ERR__nd    1
typedef uint1 pol_cfg_r_CREDIT_L3_LEN_ERR_t;

static const unsigned int pol_cfg_r_CREDIT_FRAME_ERR__n = 1;
#define pol_cfg_r_CREDIT_FRAME_ERR__nd    1
typedef uint1 pol_cfg_r_CREDIT_FRAME_ERR_t;

static const unsigned int pol_cfg_r_UNPOLICE_DROP_PRECM__n = 1;
#define pol_cfg_r_UNPOLICE_DROP_PRECM__nd    1
typedef uint1 pol_cfg_r_UNPOLICE_DROP_PRECM_t;

static const unsigned int pol_cfg_r_UNPOLICE_DROP_CM__n = 1;
#define pol_cfg_r_UNPOLICE_DROP_CM__nd    1
typedef uint1 pol_cfg_r_UNPOLICE_DROP_CM_t;

static const unsigned int pol_cfg_r__RSVD2___n = 41;
#define pol_cfg_r__RSVD2___nd    41
typedef uint41 pol_cfg_r__RSVD2__t;




/* ../src/RegC.m3:221 */
typedef pol_cfg_r pol_cfg_rf[16];

typedef pol_cfg_r__addr pol_cfg_rf__addr[16];


void
pol_cfg_rf__init(
  pol_cfg_rf *p0,
  pol_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_cfg_rf_POL_CFG__n = 16;
#define pol_cfg_rf_POL_CFG__nd    16



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_MASK;
} cm_apply_cpu_trap_mask_0_r;

typedef struct {
  uint64 *DEST_MASK;
} cm_apply_cpu_trap_mask_0_r__addr;


void
cm_apply_cpu_trap_mask_0_r__init(
  cm_apply_cpu_trap_mask_0_r *p0,
  cm_apply_cpu_trap_mask_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_cpu_trap_mask_0_r_DEST_MASK__n = 64;
#define cm_apply_cpu_trap_mask_0_r_DEST_MASK__nd    64
typedef uint64 cm_apply_cpu_trap_mask_0_r_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint24 VALUE_1;
  uint24 VALUE_2;
} mesh_freepointer_load_address_r;

typedef struct {
  uint24 *VALUE_1;
  uint24 *VALUE_2;
} mesh_freepointer_load_address_r__addr;


void
mesh_freepointer_load_address_r__init(
  mesh_freepointer_load_address_r *p0,
  mesh_freepointer_load_address_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mesh_freepointer_load_address_r_VALUE_1__n = 24;
#define mesh_freepointer_load_address_r_VALUE_1__nd    24
typedef uint24 mesh_freepointer_load_address_r_VALUE_1_t;

static const unsigned int mesh_freepointer_load_address_r_VALUE_2__n = 24;
#define mesh_freepointer_load_address_r_VALUE_2__nd    24
typedef uint24 mesh_freepointer_load_address_r_VALUE_2_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mesh_freepointer_load_address_r MESH_FREEPOINTER_LOAD_ADDRESS[3085];
} mby_freepointer_fifo_map;

typedef struct {
  mesh_freepointer_load_address_r__addr MESH_FREEPOINTER_LOAD_ADDRESS[3085];
} mby_freepointer_fifo_map__addr;


void
mby_freepointer_fifo_map__init(
  mby_freepointer_fifo_map *p0,
  mby_freepointer_fifo_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_freepointer_fifo_map_MESH_FREEPOINTER_LOAD_ADDRESS__n = 3085;
#define mby_freepointer_fifo_map_MESH_FREEPOINTER_LOAD_ADDRESS__nd    3085
typedef mesh_freepointer_load_address_r mby_freepointer_fifo_map_MESH_FREEPOINTER_LOAD_ADDRESS_t[3085];




/* ../src/RegC.m3:221 */
typedef struct {
  mby_freepointer_fifo_map fifo[85];
} mby_freepointer_top_map;

typedef struct {
  mby_freepointer_fifo_map__addr fifo[85];
} mby_freepointer_top_map__addr;


void
mby_freepointer_top_map__init(
  mby_freepointer_top_map *p0,
  mby_freepointer_top_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_freepointer_top_map_fifo__n = 85;
#define mby_freepointer_top_map_fifo__nd    85
typedef mby_freepointer_fifo_map mby_freepointer_top_map_fifo_t[85];




/* ../src/RegC.m3:221 */
typedef struct {
  mby_mesh_row_map mesh[16];
  mby_freepointer_top_map freepointer;
} mby_globals_map;

typedef struct {
  mby_mesh_row_map__addr mesh[16];
  mby_freepointer_top_map__addr freepointer;
} mby_globals_map__addr;


void
mby_globals_map__init(
  mby_globals_map *p0,
  mby_globals_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_globals_map_mesh__n = 16;
#define mby_globals_map_mesh__nd    16
typedef mby_mesh_row_map mby_globals_map_mesh_t[16];

typedef mby_freepointer_top_map mby_globals_map_freepointer_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 GLORT;
  uint16 GLORT_MASK;
} cm_apply_loopback_suppress_r;

typedef struct {
  uint16 *GLORT;
  uint16 *GLORT_MASK;
} cm_apply_loopback_suppress_r__addr;


void
cm_apply_loopback_suppress_r__init(
  cm_apply_loopback_suppress_r *p0,
  cm_apply_loopback_suppress_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_loopback_suppress_r_GLORT__n = 16;
#define cm_apply_loopback_suppress_r_GLORT__nd    16
typedef uint16 cm_apply_loopback_suppress_r_GLORT_t;

static const unsigned int cm_apply_loopback_suppress_r_GLORT_MASK__n = 16;
#define cm_apply_loopback_suppress_r_GLORT_MASK__nd    16
typedef uint16 cm_apply_loopback_suppress_r_GLORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 CFG;
  uint20 _RSVD0_;
  uint13 CTOK_LO;
  uint3 _RSVD1_;
  uint24 CTOK_HI;
} pol_direct_map_pol1_r;

typedef struct {
  uint4 *CFG;
  uint20 *_RSVD0_;
  uint13 *CTOK_LO;
  uint3 *_RSVD1_;
  uint24 *CTOK_HI;
} pol_direct_map_pol1_r__addr;


void
pol_direct_map_pol1_r__init(
  pol_direct_map_pol1_r *p0,
  pol_direct_map_pol1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_pol1_r_CFG__n = 4;
#define pol_direct_map_pol1_r_CFG__nd    4
typedef uint4 pol_direct_map_pol1_r_CFG_t;

static const unsigned int pol_direct_map_pol1_r__RSVD0___n = 20;
#define pol_direct_map_pol1_r__RSVD0___nd    20
typedef uint20 pol_direct_map_pol1_r__RSVD0__t;

static const unsigned int pol_direct_map_pol1_r_CTOK_LO__n = 13;
#define pol_direct_map_pol1_r_CTOK_LO__nd    13
typedef uint13 pol_direct_map_pol1_r_CTOK_LO_t;

static const unsigned int pol_direct_map_pol1_r__RSVD1___n = 3;
#define pol_direct_map_pol1_r__RSVD1___nd    3
typedef uint3 pol_direct_map_pol1_r__RSVD1__t;

static const unsigned int pol_direct_map_pol1_r_CTOK_HI__n = 24;
#define pol_direct_map_pol1_r_CTOK_HI__nd    24
typedef uint24 pol_direct_map_pol1_r_CTOK_HI_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint20 POL_TIME_UNIT;
} pol_time_unit_r;

typedef struct {
  uint20 *POL_TIME_UNIT;
} pol_time_unit_r__addr;


void
pol_time_unit_r__init(
  pol_time_unit_r *p0,
  pol_time_unit_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_time_unit_r_POL_TIME_UNIT__n = 20;
#define pol_time_unit_r_POL_TIME_UNIT__nd    20
typedef uint20 pol_time_unit_r_POL_TIME_UNIT_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DATA;
} fwd_table0_r;

typedef struct {
  uint64 *DATA;
} fwd_table0_r__addr;


void
fwd_table0_r__init(
  fwd_table0_r *p0,
  fwd_table0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_table0_r_DATA__n = 64;
#define fwd_table0_r_DATA__nd    64
typedef uint64 fwd_table0_r_DATA_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 _RSVD0_;
  uint2 ROTATION_A;
  uint2 ROTATION_B;
  uint1 ECMP_ROTATION;
  uint51 _RSVD1_;
} entropy_fwd_hashing_cfg_r;

typedef struct {
  uint8 *_RSVD0_;
  uint2 *ROTATION_A;
  uint2 *ROTATION_B;
  uint1 *ECMP_ROTATION;
  uint51 *_RSVD1_;
} entropy_fwd_hashing_cfg_r__addr;


void
entropy_fwd_hashing_cfg_r__init(
  entropy_fwd_hashing_cfg_r *p0,
  entropy_fwd_hashing_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_fwd_hashing_cfg_r__RSVD0___n = 8;
#define entropy_fwd_hashing_cfg_r__RSVD0___nd    8
typedef uint8 entropy_fwd_hashing_cfg_r__RSVD0__t;

static const unsigned int entropy_fwd_hashing_cfg_r_ROTATION_A__n = 2;
#define entropy_fwd_hashing_cfg_r_ROTATION_A__nd    2
typedef uint2 entropy_fwd_hashing_cfg_r_ROTATION_A_t;

static const unsigned int entropy_fwd_hashing_cfg_r_ROTATION_B__n = 2;
#define entropy_fwd_hashing_cfg_r_ROTATION_B__nd    2
typedef uint2 entropy_fwd_hashing_cfg_r_ROTATION_B_t;

static const unsigned int entropy_fwd_hashing_cfg_r_ECMP_ROTATION__n = 1;
#define entropy_fwd_hashing_cfg_r_ECMP_ROTATION__nd    1
typedef uint1 entropy_fwd_hashing_cfg_r_ECMP_ROTATION_t;

static const unsigned int entropy_fwd_hashing_cfg_r__RSVD1___n = 51;
#define entropy_fwd_hashing_cfg_r__RSVD1___nd    51
typedef uint51 entropy_fwd_hashing_cfg_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WATERMARK;
} cm_tx_tc_private_wm_r;

typedef struct {
  uint15 *WATERMARK;
} cm_tx_tc_private_wm_r__addr;


void
cm_tx_tc_private_wm_r__init(
  cm_tx_tc_private_wm_r *p0,
  cm_tx_tc_private_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_tc_private_wm_r_WATERMARK__n = 15;
#define cm_tx_tc_private_wm_r_WATERMARK__nd    15
typedef uint15 cm_tx_tc_private_wm_r_WATERMARK_t;




/* ../src/RegC.m3:221 */
typedef cm_tx_tc_private_wm_r cm_tx_tc_private_wm_rf[8];

typedef cm_tx_tc_private_wm_r__addr cm_tx_tc_private_wm_rf__addr[8];


void
cm_tx_tc_private_wm_rf__init(
  cm_tx_tc_private_wm_rf *p0,
  cm_tx_tc_private_wm_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_tc_private_wm_rf_CM_TX_TC_PRIVATE_WM__n = 8;
#define cm_tx_tc_private_wm_rf_CM_TX_TC_PRIVATE_WM__nd    8



/* ../src/RegC.m3:179 */
typedef struct {
  uint1 TCN_OVERFLOW;
  uint1 PENDING_EVENTS;
} ma_tcn_im_r;

typedef struct {
  uint1 *TCN_OVERFLOW;
  uint1 *PENDING_EVENTS;
} ma_tcn_im_r__addr;


void
ma_tcn_im_r__init(
  ma_tcn_im_r *p0,
  ma_tcn_im_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_im_r_TCN_OVERFLOW__n = 1;
#define ma_tcn_im_r_TCN_OVERFLOW__nd    1
typedef uint1 ma_tcn_im_r_TCN_OVERFLOW_t;

static const unsigned int ma_tcn_im_r_PENDING_EVENTS__n = 1;
#define ma_tcn_im_r_PENDING_EVENTS__nd    1
typedef uint1 ma_tcn_im_r_PENDING_EVENTS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 STRICT;
  uint12 DEST_INDEX;
  uint8 RANGE_SUB_INDEX_A;
  uint8 RANGE_SUB_INDEX_B;
  uint4 DEST_COUNT;
  uint1 HASH_ROTATION;
  uint1 SKIP_DGLORT_DEC;
} glort_ram_r;

typedef struct {
  uint2 *STRICT;
  uint12 *DEST_INDEX;
  uint8 *RANGE_SUB_INDEX_A;
  uint8 *RANGE_SUB_INDEX_B;
  uint4 *DEST_COUNT;
  uint1 *HASH_ROTATION;
  uint1 *SKIP_DGLORT_DEC;
} glort_ram_r__addr;


void
glort_ram_r__init(
  glort_ram_r *p0,
  glort_ram_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_ram_r_STRICT__n = 2;
#define glort_ram_r_STRICT__nd    2
typedef uint2 glort_ram_r_STRICT_t;

static const unsigned int glort_ram_r_DEST_INDEX__n = 12;
#define glort_ram_r_DEST_INDEX__nd    12
typedef uint12 glort_ram_r_DEST_INDEX_t;

static const unsigned int glort_ram_r_RANGE_SUB_INDEX_A__n = 8;
#define glort_ram_r_RANGE_SUB_INDEX_A__nd    8
typedef uint8 glort_ram_r_RANGE_SUB_INDEX_A_t;

static const unsigned int glort_ram_r_RANGE_SUB_INDEX_B__n = 8;
#define glort_ram_r_RANGE_SUB_INDEX_B__nd    8
typedef uint8 glort_ram_r_RANGE_SUB_INDEX_B_t;

static const unsigned int glort_ram_r_DEST_COUNT__n = 4;
#define glort_ram_r_DEST_COUNT__nd    4
typedef uint4 glort_ram_r_DEST_COUNT_t;

static const unsigned int glort_ram_r_HASH_ROTATION__n = 1;
#define glort_ram_r_HASH_ROTATION__nd    1
typedef uint1 glort_ram_r_HASH_ROTATION_t;

static const unsigned int glort_ram_r_SKIP_DGLORT_DEC__n = 1;
#define glort_ram_r_SKIP_DGLORT_DEC__nd    1
typedef uint1 glort_ram_r_SKIP_DGLORT_DEC_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WM_THRESHOLD;
} cm_apply_tx_tc_qcn_wm_threshold_r;

typedef struct {
  uint15 *WM_THRESHOLD;
} cm_apply_tx_tc_qcn_wm_threshold_r__addr;


void
cm_apply_tx_tc_qcn_wm_threshold_r__init(
  cm_apply_tx_tc_qcn_wm_threshold_r *p0,
  cm_apply_tx_tc_qcn_wm_threshold_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tx_tc_qcn_wm_threshold_r_WM_THRESHOLD__n = 15;
#define cm_apply_tx_tc_qcn_wm_threshold_r_WM_THRESHOLD__nd    15
typedef uint15 cm_apply_tx_tc_qcn_wm_threshold_r_WM_THRESHOLD_t;




/* ../src/RegC.m3:221 */
typedef cm_apply_tx_tc_qcn_wm_threshold_r cm_apply_tx_tc_qcn_wm_threshold_rf[8];

typedef cm_apply_tx_tc_qcn_wm_threshold_r__addr cm_apply_tx_tc_qcn_wm_threshold_rf__addr[8];


void
cm_apply_tx_tc_qcn_wm_threshold_rf__init(
  cm_apply_tx_tc_qcn_wm_threshold_rf *p0,
  cm_apply_tx_tc_qcn_wm_threshold_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tx_tc_qcn_wm_threshold_rf_CM_APPLY_TX_TC_QCN_WM_THRESHOLD__n = 8;
#define cm_apply_tx_tc_qcn_wm_threshold_rf_CM_APPLY_TX_TC_QCN_WM_THRESHOLD__nd    8



/* ../src/RegC.m3:221 */
typedef pol_dscp_r pol_dscp_rf[64];

typedef pol_dscp_r__addr pol_dscp_rf__addr[64];


void
pol_dscp_rf__init(
  pol_dscp_rf *p0,
  pol_dscp_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_dscp_rf_POL_DSCP__n = 64;
#define pol_dscp_rf_POL_DSCP__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint8 SKIP;
  uint8 NEXT_W2_OFFSET;
  uint8 NEXT_W1_OFFSET;
  uint8 NEXT_W0_OFFSET;
} parser_ana_w_r;

typedef struct {
  uint8 *SKIP;
  uint8 *NEXT_W2_OFFSET;
  uint8 *NEXT_W1_OFFSET;
  uint8 *NEXT_W0_OFFSET;
} parser_ana_w_r__addr;


void
parser_ana_w_r__init(
  parser_ana_w_r *p0,
  parser_ana_w_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ana_w_r_SKIP__n = 8;
#define parser_ana_w_r_SKIP__nd    8
typedef uint8 parser_ana_w_r_SKIP_t;

static const unsigned int parser_ana_w_r_NEXT_W2_OFFSET__n = 8;
#define parser_ana_w_r_NEXT_W2_OFFSET__nd    8
typedef uint8 parser_ana_w_r_NEXT_W2_OFFSET_t;

static const unsigned int parser_ana_w_r_NEXT_W1_OFFSET__n = 8;
#define parser_ana_w_r_NEXT_W1_OFFSET__nd    8
typedef uint8 parser_ana_w_r_NEXT_W1_OFFSET_t;

static const unsigned int parser_ana_w_r_NEXT_W0_OFFSET__n = 8;
#define parser_ana_w_r_NEXT_W0_OFFSET__nd    8
typedef uint8 parser_ana_w_r_NEXT_W0_OFFSET_t;




/* ../src/RegC.m3:221 */
typedef parser_ana_w_r parser_ana_w_rf[16];

typedef parser_ana_w_r__addr parser_ana_w_rf__addr[16];


void
parser_ana_w_rf__init(
  parser_ana_w_rf *p0,
  parser_ana_w_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_ana_w_rf_PARSER_ANA_W__n = 16;
#define parser_ana_w_rf_PARSER_ANA_W__nd    16



/* ../src/RegC.m3:179 */
typedef struct {
  uint56 BYTE_COUNTER;
} rx_stats_bank_byte_r;

typedef struct {
  uint56 *BYTE_COUNTER;
} rx_stats_bank_byte_r__addr;


void
rx_stats_bank_byte_r__init(
  rx_stats_bank_byte_r *p0,
  rx_stats_bank_byte_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_stats_bank_byte_r_BYTE_COUNTER__n = 56;
#define rx_stats_bank_byte_r_BYTE_COUNTER__nd    56
typedef uint56 rx_stats_bank_byte_r_BYTE_COUNTER_t;




/* ../src/RegC.m3:221 */
typedef rx_stats_bank_byte_r rx_stats_bank_byte_rf[144];

typedef rx_stats_bank_byte_r__addr rx_stats_bank_byte_rf__addr[144];


void
rx_stats_bank_byte_rf__init(
  rx_stats_bank_byte_rf *p0,
  rx_stats_bank_byte_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_stats_bank_byte_rf_RX_STATS_BANK_BYTE__n = 144;
#define rx_stats_bank_byte_rf_RX_STATS_BANK_BYTE__nd    144



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_2_1_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_2_1_r__addr;


void
fwd_port_cfg_2_1_r__init(
  fwd_port_cfg_2_1_r *p0,
  fwd_port_cfg_2_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_2_1_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_2_1_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_2_1_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 MIRROR_PORT_MASK4;
} cm_apply_mirror_ecmp_dmask4_r;

typedef struct {
  uint1 *MIRROR_PORT_MASK4;
} cm_apply_mirror_ecmp_dmask4_r__addr;


void
cm_apply_mirror_ecmp_dmask4_r__init(
  cm_apply_mirror_ecmp_dmask4_r *p0,
  cm_apply_mirror_ecmp_dmask4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_mirror_ecmp_dmask4_r_MIRROR_PORT_MASK4__n = 1;
#define cm_apply_mirror_ecmp_dmask4_r_MIRROR_PORT_MASK4__nd    1
typedef uint1 cm_apply_mirror_ecmp_dmask4_r_MIRROR_PORT_MASK4_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 RX_PRIVATE;
  uint1 RX_HOG;
} cm_apply_rx_smp_state_r;

typedef struct {
  uint1 *RX_PRIVATE;
  uint1 *RX_HOG;
} cm_apply_rx_smp_state_r__addr;


void
cm_apply_rx_smp_state_r__init(
  cm_apply_rx_smp_state_r *p0,
  cm_apply_rx_smp_state_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_rx_smp_state_r_RX_PRIVATE__n = 1;
#define cm_apply_rx_smp_state_r_RX_PRIVATE__nd    1
typedef uint1 cm_apply_rx_smp_state_r_RX_PRIVATE_t;

static const unsigned int cm_apply_rx_smp_state_r_RX_HOG__n = 1;
#define cm_apply_rx_smp_state_r_RX_HOG__nd    1
typedef uint1 cm_apply_rx_smp_state_r_RX_HOG_t;




/* ../src/RegC.m3:221 */
typedef cm_apply_rx_smp_state_r cm_apply_rx_smp_state_rf[2];

typedef cm_apply_rx_smp_state_r__addr cm_apply_rx_smp_state_rf__addr[2];


void
cm_apply_rx_smp_state_rf__init(
  cm_apply_rx_smp_state_rf *p0,
  cm_apply_rx_smp_state_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_rx_smp_state_rf_CM_APPLY_RX_SMP_STATE__n = 2;
#define cm_apply_rx_smp_state_rf_CM_APPLY_RX_SMP_STATE__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint24 ENABLE_MASK;
} cm_shared_smp_pause_cfg_r;

typedef struct {
  uint24 *ENABLE_MASK;
} cm_shared_smp_pause_cfg_r__addr;


void
cm_shared_smp_pause_cfg_r__init(
  cm_shared_smp_pause_cfg_r *p0,
  cm_shared_smp_pause_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_shared_smp_pause_cfg_r_ENABLE_MASK__n = 24;
#define cm_shared_smp_pause_cfg_r_ENABLE_MASK__nd    24
typedef uint24 cm_shared_smp_pause_cfg_r_ENABLE_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint9 WM;
} ma_tcn_wm_r;

typedef struct {
  uint9 *WM;
} ma_tcn_wm_r__addr;


void
ma_tcn_wm_r__init(
  ma_tcn_wm_r *p0,
  ma_tcn_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_wm_r_WM__n = 9;
#define ma_tcn_wm_r_WM__nd    9
typedef uint9 ma_tcn_wm_r_WM_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 GLOBAL_EX;
  uint1 RXS_0;
  uint1 RXS_1;
  uint1 RXS_2;
  uint1 RXS_3;
  uint1 RXS_4;
  uint1 RXS_5;
  uint1 RXS_6;
  uint1 RXS_7;
} cm_apply_state_r;

typedef struct {
  uint1 *GLOBAL_EX;
  uint1 *RXS_0;
  uint1 *RXS_1;
  uint1 *RXS_2;
  uint1 *RXS_3;
  uint1 *RXS_4;
  uint1 *RXS_5;
  uint1 *RXS_6;
  uint1 *RXS_7;
} cm_apply_state_r__addr;


void
cm_apply_state_r__init(
  cm_apply_state_r *p0,
  cm_apply_state_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_state_r_GLOBAL_EX__n = 1;
#define cm_apply_state_r_GLOBAL_EX__nd    1
typedef uint1 cm_apply_state_r_GLOBAL_EX_t;

static const unsigned int cm_apply_state_r_RXS_0__n = 1;
#define cm_apply_state_r_RXS_0__nd    1
typedef uint1 cm_apply_state_r_RXS_0_t;

static const unsigned int cm_apply_state_r_RXS_1__n = 1;
#define cm_apply_state_r_RXS_1__nd    1
typedef uint1 cm_apply_state_r_RXS_1_t;

static const unsigned int cm_apply_state_r_RXS_2__n = 1;
#define cm_apply_state_r_RXS_2__nd    1
typedef uint1 cm_apply_state_r_RXS_2_t;

static const unsigned int cm_apply_state_r_RXS_3__n = 1;
#define cm_apply_state_r_RXS_3__nd    1
typedef uint1 cm_apply_state_r_RXS_3_t;

static const unsigned int cm_apply_state_r_RXS_4__n = 1;
#define cm_apply_state_r_RXS_4__nd    1
typedef uint1 cm_apply_state_r_RXS_4_t;

static const unsigned int cm_apply_state_r_RXS_5__n = 1;
#define cm_apply_state_r_RXS_5__nd    1
typedef uint1 cm_apply_state_r_RXS_5_t;

static const unsigned int cm_apply_state_r_RXS_6__n = 1;
#define cm_apply_state_r_RXS_6__nd    1
typedef uint1 cm_apply_state_r_RXS_6_t;

static const unsigned int cm_apply_state_r_RXS_7__n = 1;
#define cm_apply_state_r_RXS_7__nd    1
typedef uint1 cm_apply_state_r_RXS_7_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 DEST_GLORT;
  uint16 GLORT_MASK;
} trigger_condition_glort_r;

typedef struct {
  uint16 *DEST_GLORT;
  uint16 *GLORT_MASK;
} trigger_condition_glort_r__addr;


void
trigger_condition_glort_r__init(
  trigger_condition_glort_r *p0,
  trigger_condition_glort_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_glort_r_DEST_GLORT__n = 16;
#define trigger_condition_glort_r_DEST_GLORT__nd    16
typedef uint16 trigger_condition_glort_r_DEST_GLORT_t;

static const unsigned int trigger_condition_glort_r_GLORT_MASK__n = 16;
#define trigger_condition_glort_r_GLORT_MASK__nd    16
typedef uint16 trigger_condition_glort_r_GLORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 FILTER_VLAN_INGRESS;
  uint1 LEARNING_ENABLE;
} fwd_port_cfg_0_r;

typedef struct {
  uint1 *FILTER_VLAN_INGRESS;
  uint1 *LEARNING_ENABLE;
} fwd_port_cfg_0_r__addr;


void
fwd_port_cfg_0_r__init(
  fwd_port_cfg_0_r *p0,
  fwd_port_cfg_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_0_r_FILTER_VLAN_INGRESS__n = 1;
#define fwd_port_cfg_0_r_FILTER_VLAN_INGRESS__nd    1
typedef uint1 fwd_port_cfg_0_r_FILTER_VLAN_INGRESS_t;

static const unsigned int fwd_port_cfg_0_r_LEARNING_ENABLE__n = 1;
#define fwd_port_cfg_0_r_LEARNING_ENABLE__nd    1
typedef uint1 fwd_port_cfg_0_r_LEARNING_ENABLE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint5 PHYS_PORT;
} cm_pause_phys_port_cfg_r;

typedef struct {
  uint5 *PHYS_PORT;
} cm_pause_phys_port_cfg_r__addr;


void
cm_pause_phys_port_cfg_r__init(
  cm_pause_phys_port_cfg_r *p0,
  cm_pause_phys_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_pause_phys_port_cfg_r_PHYS_PORT__n = 5;
#define cm_pause_phys_port_cfg_r_PHYS_PORT__nd    5
typedef uint5 cm_pause_phys_port_cfg_r_PHYS_PORT_t;




/* ../src/RegC.m3:221 */
typedef em_hash_cam_r em_hash_cam_rf[8];

typedef em_hash_cam_r__addr em_hash_cam_rf__addr[8];


void
em_hash_cam_rf__init(
  em_hash_cam_rf *p0,
  em_hash_cam_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int em_hash_cam_rf_HASH_CAM__n = 8;
#define em_hash_cam_rf_HASH_CAM__nd    8



/* ../src/RegC.m3:221 */
typedef entropy_hash_sym32_r entropy_hash_sym32_rf[4];

typedef entropy_hash_sym32_r__addr entropy_hash_sym32_rf__addr[4];


void
entropy_hash_sym32_rf__init(
  entropy_hash_sym32_rf *p0,
  entropy_hash_sym32_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_sym32_rf_ENTROPY_HASH_SYM32__n = 4;
#define entropy_hash_sym32_rf_ENTROPY_HASH_SYM32__nd    4



/* ../src/RegC.m3:179 */
typedef struct {
  uint6 HASH_SIZE;
  uint6 HASH_START;
  uint12 BYTE_DEFAULTS;
} entropy_meta_cfg_r;

typedef struct {
  uint6 *HASH_SIZE;
  uint6 *HASH_START;
  uint12 *BYTE_DEFAULTS;
} entropy_meta_cfg_r__addr;


void
entropy_meta_cfg_r__init(
  entropy_meta_cfg_r *p0,
  entropy_meta_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_meta_cfg_r_HASH_SIZE__n = 6;
#define entropy_meta_cfg_r_HASH_SIZE__nd    6
typedef uint6 entropy_meta_cfg_r_HASH_SIZE_t;

static const unsigned int entropy_meta_cfg_r_HASH_START__n = 6;
#define entropy_meta_cfg_r_HASH_START__nd    6
typedef uint6 entropy_meta_cfg_r_HASH_START_t;

static const unsigned int entropy_meta_cfg_r_BYTE_DEFAULTS__n = 12;
#define entropy_meta_cfg_r_BYTE_DEFAULTS__nd    12
typedef uint12 entropy_meta_cfg_r_BYTE_DEFAULTS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 DESTINATION_MASK;
} fwd_port_cfg_1_4_r;

typedef struct {
  uint1 *DESTINATION_MASK;
} fwd_port_cfg_1_4_r__addr;


void
fwd_port_cfg_1_4_r__init(
  fwd_port_cfg_1_4_r *p0,
  fwd_port_cfg_1_4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_1_4_r_DESTINATION_MASK__n = 1;
#define fwd_port_cfg_1_4_r_DESTINATION_MASK__nd    1
typedef uint1 fwd_port_cfg_1_4_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 R_GROUP_SZ_TYPE;
  uint6 R_GROUP_SIZE;
  uint3 FLOWLET_POLICY;
  uint8 FLOWLET_AGE_RESET;
  uint11 GROUP_MIN_INDEX;
} nexthop_groups_table_1_r;

typedef struct {
  uint1 *R_GROUP_SZ_TYPE;
  uint6 *R_GROUP_SIZE;
  uint3 *FLOWLET_POLICY;
  uint8 *FLOWLET_AGE_RESET;
  uint11 *GROUP_MIN_INDEX;
} nexthop_groups_table_1_r__addr;


void
nexthop_groups_table_1_r__init(
  nexthop_groups_table_1_r *p0,
  nexthop_groups_table_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int nexthop_groups_table_1_r_R_GROUP_SZ_TYPE__n = 1;
#define nexthop_groups_table_1_r_R_GROUP_SZ_TYPE__nd    1
typedef uint1 nexthop_groups_table_1_r_R_GROUP_SZ_TYPE_t;

static const unsigned int nexthop_groups_table_1_r_R_GROUP_SIZE__n = 6;
#define nexthop_groups_table_1_r_R_GROUP_SIZE__nd    6
typedef uint6 nexthop_groups_table_1_r_R_GROUP_SIZE_t;

static const unsigned int nexthop_groups_table_1_r_FLOWLET_POLICY__n = 3;
#define nexthop_groups_table_1_r_FLOWLET_POLICY__nd    3
typedef uint3 nexthop_groups_table_1_r_FLOWLET_POLICY_t;

static const unsigned int nexthop_groups_table_1_r_FLOWLET_AGE_RESET__n = 8;
#define nexthop_groups_table_1_r_FLOWLET_AGE_RESET__nd    8
typedef uint8 nexthop_groups_table_1_r_FLOWLET_AGE_RESET_t;

static const unsigned int nexthop_groups_table_1_r_GROUP_MIN_INDEX__n = 11;
#define nexthop_groups_table_1_r_GROUP_MIN_INDEX__nd    11
typedef uint11 nexthop_groups_table_1_r_GROUP_MIN_INDEX_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint17 SRC_PORT_MASK;
} trigger_condition_rx_r;

typedef struct {
  uint17 *SRC_PORT_MASK;
} trigger_condition_rx_r__addr;


void
trigger_condition_rx_r__init(
  trigger_condition_rx_r *p0,
  trigger_condition_rx_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_rx_r_SRC_PORT_MASK__n = 17;
#define trigger_condition_rx_r_SRC_PORT_MASK__nd    17
typedef uint17 trigger_condition_rx_r_SRC_PORT_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DEST_MASK;
} cm_apply_cpu_trap_mask_1_r;

typedef struct {
  uint64 *DEST_MASK;
} cm_apply_cpu_trap_mask_1_r__addr;


void
cm_apply_cpu_trap_mask_1_r__init(
  cm_apply_cpu_trap_mask_1_r *p0,
  cm_apply_cpu_trap_mask_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_cpu_trap_mask_1_r_DEST_MASK__n = 64;
#define cm_apply_cpu_trap_mask_1_r_DEST_MASK__nd    64
typedef uint64 cm_apply_cpu_trap_mask_1_r_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_tx_tc_usage_r;

typedef struct {
  uint16 *COUNT;
} cm_tx_tc_usage_r__addr;


void
cm_tx_tc_usage_r__init(
  cm_tx_tc_usage_r *p0,
  cm_tx_tc_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_tc_usage_r_COUNT__n = 16;
#define cm_tx_tc_usage_r_COUNT__nd    16
typedef uint16 cm_tx_tc_usage_r_COUNT_t;




/* ../src/RegC.m3:221 */
typedef cm_tx_tc_usage_r cm_tx_tc_usage_rf[8];

typedef cm_tx_tc_usage_r__addr cm_tx_tc_usage_rf__addr[8];


void
cm_tx_tc_usage_rf__init(
  cm_tx_tc_usage_rf *p0,
  cm_tx_tc_usage_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_tx_tc_usage_rf_CM_TX_TC_USAGE__n = 8;
#define cm_tx_tc_usage_rf_CM_TX_TC_USAGE__nd    8



/* ../src/RegC.m3:221 */
typedef lpm_subtrie_bitmaps_r lpm_subtrie_bitmaps_rf[4096];

typedef lpm_subtrie_bitmaps_r__addr lpm_subtrie_bitmaps_rf__addr[4096];


void
lpm_subtrie_bitmaps_rf__init(
  lpm_subtrie_bitmaps_rf *p0,
  lpm_subtrie_bitmaps_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int lpm_subtrie_bitmaps_rf_LPM_SUBTRIE_BITMAPS__n = 4096;
#define lpm_subtrie_bitmaps_rf_LPM_SUBTRIE_BITMAPS__nd    4096



/* ../src/RegC.m3:221 */
typedef struct {
  em_hash_lookup_r EM_HASH_LOOKUP[32768];
  lpm_subtrie_aptr_rf LPM_SUBTRIE_APTR[48];
  lpm_subtrie_cptr_rf LPM_SUBTRIE_CPTR[48];
  lpm_subtrie_bitmaps_rf LPM_SUBTRIE_BITMAPS[48];
  lpm_match_tcam_r LPM_MATCH_TCAM[512];
  lpm_match_action_r LPM_MATCH_ACTION[512];
  lpm_key_mask_rf LPM_KEY_MASK[64];
  lpm_key_sel0_r LPM_KEY_SEL0[64];
  lpm_key_sel1_r LPM_KEY_SEL1[64];
  lpm_key_sel2_r LPM_KEY_SEL2[64];
  lpm_key_sel3_r LPM_KEY_SEL3[64];
} mby_ppe_cgrp_a_nested_map;

typedef struct {
  em_hash_lookup_r__addr EM_HASH_LOOKUP[32768];
  lpm_subtrie_aptr_rf__addr LPM_SUBTRIE_APTR[48];
  lpm_subtrie_cptr_rf__addr LPM_SUBTRIE_CPTR[48];
  lpm_subtrie_bitmaps_rf__addr LPM_SUBTRIE_BITMAPS[48];
  lpm_match_tcam_r__addr LPM_MATCH_TCAM[512];
  lpm_match_action_r__addr LPM_MATCH_ACTION[512];
  lpm_key_mask_rf__addr LPM_KEY_MASK[64];
  lpm_key_sel0_r__addr LPM_KEY_SEL0[64];
  lpm_key_sel1_r__addr LPM_KEY_SEL1[64];
  lpm_key_sel2_r__addr LPM_KEY_SEL2[64];
  lpm_key_sel3_r__addr LPM_KEY_SEL3[64];
} mby_ppe_cgrp_a_nested_map__addr;


void
mby_ppe_cgrp_a_nested_map__init(
  mby_ppe_cgrp_a_nested_map *p0,
  mby_ppe_cgrp_a_nested_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_cgrp_a_nested_map_EM_HASH_LOOKUP__n = 32768;
#define mby_ppe_cgrp_a_nested_map_EM_HASH_LOOKUP__nd    32768
typedef em_hash_lookup_r mby_ppe_cgrp_a_nested_map_EM_HASH_LOOKUP_t[32768];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_APTR__n = 48;
#define mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_APTR__nd    48
typedef lpm_subtrie_aptr_rf mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_APTR_t[48];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_CPTR__n = 48;
#define mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_CPTR__nd    48
typedef lpm_subtrie_cptr_rf mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_CPTR_t[48];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_BITMAPS__n = 48;
#define mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_BITMAPS__nd    48
typedef lpm_subtrie_bitmaps_rf mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_BITMAPS_t[48];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__n = 512;
#define mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__nd    512
typedef lpm_match_tcam_r mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM_t[512];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_MATCH_ACTION__n = 512;
#define mby_ppe_cgrp_a_nested_map_LPM_MATCH_ACTION__nd    512
typedef lpm_match_action_r mby_ppe_cgrp_a_nested_map_LPM_MATCH_ACTION_t[512];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_KEY_MASK__n = 64;
#define mby_ppe_cgrp_a_nested_map_LPM_KEY_MASK__nd    64
typedef lpm_key_mask_rf mby_ppe_cgrp_a_nested_map_LPM_KEY_MASK_t[64];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL0__n = 64;
#define mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL0__nd    64
typedef lpm_key_sel0_r mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL0_t[64];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL1__n = 64;
#define mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL1__nd    64
typedef lpm_key_sel1_r mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL1_t[64];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL2__n = 64;
#define mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL2__nd    64
typedef lpm_key_sel2_r mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL2_t[64];

static const unsigned int mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL3__n = 64;
#define mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL3__nd    64
typedef lpm_key_sel3_r mby_ppe_cgrp_a_nested_map_LPM_KEY_SEL3_t[64];




/* ../src/RegC.m3:179 */
typedef struct {
  uint13 EBS;
  uint2 EBS_UNIT;
  uint1 _RSVD0_;
  uint11 EIR;
  uint3 EIR_UNIT;
  uint34 _RSVD1_;
} pol_direct_map_pol2_r;

typedef struct {
  uint13 *EBS;
  uint2 *EBS_UNIT;
  uint1 *_RSVD0_;
  uint11 *EIR;
  uint3 *EIR_UNIT;
  uint34 *_RSVD1_;
} pol_direct_map_pol2_r__addr;


void
pol_direct_map_pol2_r__init(
  pol_direct_map_pol2_r *p0,
  pol_direct_map_pol2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_pol2_r_EBS__n = 13;
#define pol_direct_map_pol2_r_EBS__nd    13
typedef uint13 pol_direct_map_pol2_r_EBS_t;

static const unsigned int pol_direct_map_pol2_r_EBS_UNIT__n = 2;
#define pol_direct_map_pol2_r_EBS_UNIT__nd    2
typedef uint2 pol_direct_map_pol2_r_EBS_UNIT_t;

static const unsigned int pol_direct_map_pol2_r__RSVD0___n = 1;
#define pol_direct_map_pol2_r__RSVD0___nd    1
typedef uint1 pol_direct_map_pol2_r__RSVD0__t;

static const unsigned int pol_direct_map_pol2_r_EIR__n = 11;
#define pol_direct_map_pol2_r_EIR__nd    11
typedef uint11 pol_direct_map_pol2_r_EIR_t;

static const unsigned int pol_direct_map_pol2_r_EIR_UNIT__n = 3;
#define pol_direct_map_pol2_r_EIR_UNIT__nd    3
typedef uint3 pol_direct_map_pol2_r_EIR_UNIT_t;

static const unsigned int pol_direct_map_pol2_r__RSVD1___n = 34;
#define pol_direct_map_pol2_r__RSVD1___nd    34
typedef uint34 pol_direct_map_pol2_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 SOFT_DROP_ON_SMP_FREE;
  uint1 SOFT_DROP_ON_PRIVATE;
} cm_apply_tx_softdrop_cfg_r;

typedef struct {
  uint1 *SOFT_DROP_ON_SMP_FREE;
  uint1 *SOFT_DROP_ON_PRIVATE;
} cm_apply_tx_softdrop_cfg_r__addr;


void
cm_apply_tx_softdrop_cfg_r__init(
  cm_apply_tx_softdrop_cfg_r *p0,
  cm_apply_tx_softdrop_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tx_softdrop_cfg_r_SOFT_DROP_ON_SMP_FREE__n = 1;
#define cm_apply_tx_softdrop_cfg_r_SOFT_DROP_ON_SMP_FREE__nd    1
typedef uint1 cm_apply_tx_softdrop_cfg_r_SOFT_DROP_ON_SMP_FREE_t;

static const unsigned int cm_apply_tx_softdrop_cfg_r_SOFT_DROP_ON_PRIVATE__n = 1;
#define cm_apply_tx_softdrop_cfg_r_SOFT_DROP_ON_PRIVATE__nd    1
typedef uint1 cm_apply_tx_softdrop_cfg_r_SOFT_DROP_ON_PRIVATE_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DATA;
} fwd_table1_r;

typedef struct {
  uint64 *DATA;
} fwd_table1_r__addr;


void
fwd_table1_r__init(
  fwd_table1_r *p0,
  fwd_table1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_table1_r_DATA__n = 64;
#define fwd_table1_r_DATA__nd    64
typedef uint64 fwd_table1_r_DATA_t;




/* ../src/RegC.m3:221 */
typedef fwd_table1_r fwd_table1_rf[8192];

typedef fwd_table1_r__addr fwd_table1_rf__addr[8192];


void
fwd_table1_rf__init(
  fwd_table1_rf *p0,
  fwd_table1_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_table1_rf_FWD_TABLE1__n = 8192;
#define fwd_table1_rf_FWD_TABLE1__nd    8192



/* ../src/RegC.m3:221 */
typedef cm_rx_smp_hog_wm_r cm_rx_smp_hog_wm_rf[2];

typedef cm_rx_smp_hog_wm_r__addr cm_rx_smp_hog_wm_rf__addr[2];


void
cm_rx_smp_hog_wm_rf__init(
  cm_rx_smp_hog_wm_rf *p0,
  cm_rx_smp_hog_wm_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_hog_wm_rf_CM_RX_SMP_HOG_WM__n = 2;
#define cm_rx_smp_hog_wm_rf_CM_RX_SMP_HOG_WM__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 METADATA_MASK;
} trigger_action_metadata_mask_r;

typedef struct {
  uint64 *METADATA_MASK;
} trigger_action_metadata_mask_r__addr;


void
trigger_action_metadata_mask_r__init(
  trigger_action_metadata_mask_r *p0,
  trigger_action_metadata_mask_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_action_metadata_mask_r_METADATA_MASK__n = 64;
#define trigger_action_metadata_mask_r_METADATA_MASK__nd    64
typedef uint64 trigger_action_metadata_mask_r_METADATA_MASK_t;




/* ../src/RegC.m3:221 */
typedef trigger_action_metadata_mask_r trigger_action_metadata_mask_rf[4];

typedef trigger_action_metadata_mask_r__addr trigger_action_metadata_mask_rf__addr[4];


void
trigger_action_metadata_mask_rf__init(
  trigger_action_metadata_mask_rf *p0,
  trigger_action_metadata_mask_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_action_metadata_mask_rf_TRIGGER_ACTION_METADATA_MASK__n = 4;
#define trigger_action_metadata_mask_rf_TRIGGER_ACTION_METADATA_MASK__nd    4



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 GLORT_DEST_MASK;
} glort_dest_table_r2;

typedef struct {
  uint64 *GLORT_DEST_MASK;
} glort_dest_table_r2__addr;


void
glort_dest_table_r2__init(
  glort_dest_table_r2 *p0,
  glort_dest_table_r2__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_dest_table_r2_GLORT_DEST_MASK__n = 64;
#define glort_dest_table_r2_GLORT_DEST_MASK__nd    64
typedef uint64 glort_dest_table_r2_GLORT_DEST_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint17 MEMBERSHIP;
  uint1 REFLECT;
  uint1 TRAP_IGMP;
} ingress_vid_table_r;

typedef struct {
  uint17 *MEMBERSHIP;
  uint1 *REFLECT;
  uint1 *TRAP_IGMP;
} ingress_vid_table_r__addr;


void
ingress_vid_table_r__init(
  ingress_vid_table_r *p0,
  ingress_vid_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ingress_vid_table_r_MEMBERSHIP__n = 17;
#define ingress_vid_table_r_MEMBERSHIP__nd    17
typedef uint17 ingress_vid_table_r_MEMBERSHIP_t;

static const unsigned int ingress_vid_table_r_REFLECT__n = 1;
#define ingress_vid_table_r_REFLECT__nd    1
typedef uint1 ingress_vid_table_r_REFLECT_t;

static const unsigned int ingress_vid_table_r_TRAP_IGMP__n = 1;
#define ingress_vid_table_r_TRAP_IGMP__nd    1
typedef uint1 ingress_vid_table_r_TRAP_IGMP_t;




/* ../src/RegC.m3:221 */
typedef struct {
  nexthop_config_r NH_CONFIG;
  nexthop_status_r NH_STATUS;
  nexthop_neighbors_table_0_r NH_NEIGHBORS_0[16384];
  nexthop_neighbors_table_1_r NH_NEIGHBORS_1[16384];
  nexthop_groups_table_0_r NH_GROUPS_0[4096];
  nexthop_groups_table_1_r NH_GROUPS_1[4096];
  nexthop_routes_table_r NH_ROUTES[16384];
  nexthop_weights_table_rf NH_WEIGHTS[2048];
  ingress_vid_table_r INGRESS_VID_TABLE[4096];
  flood_glort_table_r FLOOD_GLORT_TABLE[256];
  nexthop_used_r NH_USED[256];
  nexthop_path_ctrs_r NH_PATH_CTRS[16384];
  nexthop_group_min_r NH_GROUP_MIN[512];
  mtu_table_r MTU_TABLE[8];
} mby_ppe_nexthop_map;

typedef struct {
  nexthop_config_r__addr NH_CONFIG;
  nexthop_status_r__addr NH_STATUS;
  nexthop_neighbors_table_0_r__addr NH_NEIGHBORS_0[16384];
  nexthop_neighbors_table_1_r__addr NH_NEIGHBORS_1[16384];
  nexthop_groups_table_0_r__addr NH_GROUPS_0[4096];
  nexthop_groups_table_1_r__addr NH_GROUPS_1[4096];
  nexthop_routes_table_r__addr NH_ROUTES[16384];
  nexthop_weights_table_rf__addr NH_WEIGHTS[2048];
  ingress_vid_table_r__addr INGRESS_VID_TABLE[4096];
  flood_glort_table_r__addr FLOOD_GLORT_TABLE[256];
  nexthop_used_r__addr NH_USED[256];
  nexthop_path_ctrs_r__addr NH_PATH_CTRS[16384];
  nexthop_group_min_r__addr NH_GROUP_MIN[512];
  mtu_table_r__addr MTU_TABLE[8];
} mby_ppe_nexthop_map__addr;


void
mby_ppe_nexthop_map__init(
  mby_ppe_nexthop_map *p0,
  mby_ppe_nexthop_map__addr *p1,
  void (*f)(void *, int)
);

typedef nexthop_config_r mby_ppe_nexthop_map_NH_CONFIG_t;

typedef nexthop_status_r mby_ppe_nexthop_map_NH_STATUS_t;

static const unsigned int mby_ppe_nexthop_map_NH_NEIGHBORS_0__n = 16384;
#define mby_ppe_nexthop_map_NH_NEIGHBORS_0__nd    16384
typedef nexthop_neighbors_table_0_r mby_ppe_nexthop_map_NH_NEIGHBORS_0_t[16384];

static const unsigned int mby_ppe_nexthop_map_NH_NEIGHBORS_1__n = 16384;
#define mby_ppe_nexthop_map_NH_NEIGHBORS_1__nd    16384
typedef nexthop_neighbors_table_1_r mby_ppe_nexthop_map_NH_NEIGHBORS_1_t[16384];

static const unsigned int mby_ppe_nexthop_map_NH_GROUPS_0__n = 4096;
#define mby_ppe_nexthop_map_NH_GROUPS_0__nd    4096
typedef nexthop_groups_table_0_r mby_ppe_nexthop_map_NH_GROUPS_0_t[4096];

static const unsigned int mby_ppe_nexthop_map_NH_GROUPS_1__n = 4096;
#define mby_ppe_nexthop_map_NH_GROUPS_1__nd    4096
typedef nexthop_groups_table_1_r mby_ppe_nexthop_map_NH_GROUPS_1_t[4096];

static const unsigned int mby_ppe_nexthop_map_NH_ROUTES__n = 16384;
#define mby_ppe_nexthop_map_NH_ROUTES__nd    16384
typedef nexthop_routes_table_r mby_ppe_nexthop_map_NH_ROUTES_t[16384];

static const unsigned int mby_ppe_nexthop_map_NH_WEIGHTS__n = 2048;
#define mby_ppe_nexthop_map_NH_WEIGHTS__nd    2048
typedef nexthop_weights_table_rf mby_ppe_nexthop_map_NH_WEIGHTS_t[2048];

static const unsigned int mby_ppe_nexthop_map_INGRESS_VID_TABLE__n = 4096;
#define mby_ppe_nexthop_map_INGRESS_VID_TABLE__nd    4096
typedef ingress_vid_table_r mby_ppe_nexthop_map_INGRESS_VID_TABLE_t[4096];

static const unsigned int mby_ppe_nexthop_map_FLOOD_GLORT_TABLE__n = 256;
#define mby_ppe_nexthop_map_FLOOD_GLORT_TABLE__nd    256
typedef flood_glort_table_r mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t[256];

static const unsigned int mby_ppe_nexthop_map_NH_USED__n = 256;
#define mby_ppe_nexthop_map_NH_USED__nd    256
typedef nexthop_used_r mby_ppe_nexthop_map_NH_USED_t[256];

static const unsigned int mby_ppe_nexthop_map_NH_PATH_CTRS__n = 16384;
#define mby_ppe_nexthop_map_NH_PATH_CTRS__nd    16384
typedef nexthop_path_ctrs_r mby_ppe_nexthop_map_NH_PATH_CTRS_t[16384];

static const unsigned int mby_ppe_nexthop_map_NH_GROUP_MIN__n = 512;
#define mby_ppe_nexthop_map_NH_GROUP_MIN__nd    512
typedef nexthop_group_min_r mby_ppe_nexthop_map_NH_GROUP_MIN_t[512];

static const unsigned int mby_ppe_nexthop_map_MTU_TABLE__n = 8;
#define mby_ppe_nexthop_map_MTU_TABLE__nd    8
typedef mtu_table_r mby_ppe_nexthop_map_MTU_TABLE_t[8];




/* ../src/RegC.m3:221 */
typedef cm_apply_tx_softdrop_cfg_r cm_apply_tx_softdrop_cfg_rf[8];

typedef cm_apply_tx_softdrop_cfg_r__addr cm_apply_tx_softdrop_cfg_rf__addr[8];


void
cm_apply_tx_softdrop_cfg_rf__init(
  cm_apply_tx_softdrop_cfg_rf *p0,
  cm_apply_tx_softdrop_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tx_softdrop_cfg_rf_CM_APPLY_TX_SOFTDROP_CFG__n = 8;
#define cm_apply_tx_softdrop_cfg_rf_CM_APPLY_TX_SOFTDROP_CFG__nd    8



/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_2_2_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_2_2_r__addr;


void
fwd_port_cfg_2_2_r__init(
  fwd_port_cfg_2_2_r *p0,
  fwd_port_cfg_2_2_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_2_2_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_2_2_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_2_2_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint12 VID1_KEY;
  uint1 VID1_VALID;
  uint12 VID2_KEY;
  uint1 VID2_VALID;
  uint18 PORT_KEY;
  uint20 _RSVD0_;
  uint12 VID1_KEY_INVERT;
  uint1 VID1_VALID_INVERT;
  uint12 VID2_KEY_INVERT;
  uint1 VID2_VALID_INVERT;
  uint18 PORT_KEY_INVERT;
  uint20 _RSVD1_;
} map_domain_tcam_r;

typedef struct {
  uint12 *VID1_KEY;
  uint1 *VID1_VALID;
  uint12 *VID2_KEY;
  uint1 *VID2_VALID;
  uint18 *PORT_KEY;
  uint20 *_RSVD0_;
  uint12 *VID1_KEY_INVERT;
  uint1 *VID1_VALID_INVERT;
  uint12 *VID2_KEY_INVERT;
  uint1 *VID2_VALID_INVERT;
  uint18 *PORT_KEY_INVERT;
  uint20 *_RSVD1_;
} map_domain_tcam_r__addr;


void
map_domain_tcam_r__init(
  map_domain_tcam_r *p0,
  map_domain_tcam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_domain_tcam_r_VID1_KEY__n = 12;
#define map_domain_tcam_r_VID1_KEY__nd    12
typedef uint12 map_domain_tcam_r_VID1_KEY_t;

static const unsigned int map_domain_tcam_r_VID1_VALID__n = 1;
#define map_domain_tcam_r_VID1_VALID__nd    1
typedef uint1 map_domain_tcam_r_VID1_VALID_t;

static const unsigned int map_domain_tcam_r_VID2_KEY__n = 12;
#define map_domain_tcam_r_VID2_KEY__nd    12
typedef uint12 map_domain_tcam_r_VID2_KEY_t;

static const unsigned int map_domain_tcam_r_VID2_VALID__n = 1;
#define map_domain_tcam_r_VID2_VALID__nd    1
typedef uint1 map_domain_tcam_r_VID2_VALID_t;

static const unsigned int map_domain_tcam_r_PORT_KEY__n = 18;
#define map_domain_tcam_r_PORT_KEY__nd    18
typedef uint18 map_domain_tcam_r_PORT_KEY_t;

static const unsigned int map_domain_tcam_r__RSVD0___n = 20;
#define map_domain_tcam_r__RSVD0___nd    20
typedef uint20 map_domain_tcam_r__RSVD0__t;

static const unsigned int map_domain_tcam_r_VID1_KEY_INVERT__n = 12;
#define map_domain_tcam_r_VID1_KEY_INVERT__nd    12
typedef uint12 map_domain_tcam_r_VID1_KEY_INVERT_t;

static const unsigned int map_domain_tcam_r_VID1_VALID_INVERT__n = 1;
#define map_domain_tcam_r_VID1_VALID_INVERT__nd    1
typedef uint1 map_domain_tcam_r_VID1_VALID_INVERT_t;

static const unsigned int map_domain_tcam_r_VID2_KEY_INVERT__n = 12;
#define map_domain_tcam_r_VID2_KEY_INVERT__nd    12
typedef uint12 map_domain_tcam_r_VID2_KEY_INVERT_t;

static const unsigned int map_domain_tcam_r_VID2_VALID_INVERT__n = 1;
#define map_domain_tcam_r_VID2_VALID_INVERT__nd    1
typedef uint1 map_domain_tcam_r_VID2_VALID_INVERT_t;

static const unsigned int map_domain_tcam_r_PORT_KEY_INVERT__n = 18;
#define map_domain_tcam_r_PORT_KEY_INVERT__nd    18
typedef uint18 map_domain_tcam_r_PORT_KEY_INVERT_t;

static const unsigned int map_domain_tcam_r__RSVD1___n = 20;
#define map_domain_tcam_r__RSVD1___nd    20
typedef uint20 map_domain_tcam_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 VALUE0;
  uint16 VALUE1;
  uint16 VALUE2;
  uint16 VALUE3;
} mod_map_r;

typedef struct {
  uint16 *VALUE0;
  uint16 *VALUE1;
  uint16 *VALUE2;
  uint16 *VALUE3;
} mod_map_r__addr;


void
mod_map_r__init(
  mod_map_r *p0,
  mod_map_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_map_r_VALUE0__n = 16;
#define mod_map_r_VALUE0__nd    16
typedef uint16 mod_map_r_VALUE0_t;

static const unsigned int mod_map_r_VALUE1__n = 16;
#define mod_map_r_VALUE1__nd    16
typedef uint16 mod_map_r_VALUE1_t;

static const unsigned int mod_map_r_VALUE2__n = 16;
#define mod_map_r_VALUE2__nd    16
typedef uint16 mod_map_r_VALUE2_t;

static const unsigned int mod_map_r_VALUE3__n = 16;
#define mod_map_r_VALUE3__nd    16
typedef uint16 mod_map_r_VALUE3_t;




/* ../src/RegC.m3:221 */
typedef mod_map_r mod_map_rf[512];

typedef mod_map_r__addr mod_map_rf__addr[512];


void
mod_map_rf__init(
  mod_map_rf *p0,
  mod_map_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_map_rf_MOD_MAP__n = 512;
#define mod_map_rf_MOD_MAP__nd    512



/* ../src/RegC.m3:221 */
typedef fwd_table0_r fwd_table0_rf[16384];

typedef fwd_table0_r__addr fwd_table0_rf__addr[16384];


void
fwd_table0_rf__init(
  fwd_table0_rf *p0,
  fwd_table0_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_table0_rf_FWD_TABLE0__n = 16384;
#define fwd_table0_rf_FWD_TABLE0__nd    16384



/* ../src/RegC.m3:221 */
typedef struct {
  fwd_table0_rf FWD_TABLE0[48];
  fwd_table1_rf FWD_TABLE1[16];
  fwd_table_mod_rf FWD_TABLE_MOD[10];
} mby_shm_map;

typedef struct {
  fwd_table0_rf__addr FWD_TABLE0[48];
  fwd_table1_rf__addr FWD_TABLE1[16];
  fwd_table_mod_rf__addr FWD_TABLE_MOD[10];
} mby_shm_map__addr;


void
mby_shm_map__init(
  mby_shm_map *p0,
  mby_shm_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_shm_map_FWD_TABLE0__n = 48;
#define mby_shm_map_FWD_TABLE0__nd    48
typedef fwd_table0_rf mby_shm_map_FWD_TABLE0_t[48];

static const unsigned int mby_shm_map_FWD_TABLE1__n = 16;
#define mby_shm_map_FWD_TABLE1__nd    16
typedef fwd_table1_rf mby_shm_map_FWD_TABLE1_t[16];

static const unsigned int mby_shm_map_FWD_TABLE_MOD__n = 10;
#define mby_shm_map_FWD_TABLE_MOD__nd    10
typedef fwd_table_mod_rf mby_shm_map_FWD_TABLE_MOD_t[10];




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 WATERMARK;
} cm_shared_wm_r;

typedef struct {
  uint15 *WATERMARK;
} cm_shared_wm_r__addr;


void
cm_shared_wm_r__init(
  cm_shared_wm_r *p0,
  cm_shared_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_shared_wm_r_WATERMARK__n = 15;
#define cm_shared_wm_r_WATERMARK__nd    15
typedef uint15 cm_shared_wm_r_WATERMARK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint8 PROTOCOL_ID;
  uint8 OFFSET;
} parser_extract_cfg_r;

typedef struct {
  uint8 *PROTOCOL_ID;
  uint8 *OFFSET;
} parser_extract_cfg_r__addr;


void
parser_extract_cfg_r__init(
  parser_extract_cfg_r *p0,
  parser_extract_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_extract_cfg_r_PROTOCOL_ID__n = 8;
#define parser_extract_cfg_r_PROTOCOL_ID__nd    8
typedef uint8 parser_extract_cfg_r_PROTOCOL_ID_t;

static const unsigned int parser_extract_cfg_r_OFFSET__n = 8;
#define parser_extract_cfg_r_OFFSET__nd    8
typedef uint8 parser_extract_cfg_r_OFFSET_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 PORT;
} saf_ingress_speed_r;

typedef struct {
  uint64 *PORT;
} saf_ingress_speed_r__addr;


void
saf_ingress_speed_r__init(
  saf_ingress_speed_r *p0,
  saf_ingress_speed_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int saf_ingress_speed_r_PORT__n = 64;
#define saf_ingress_speed_r_PORT__nd    64
typedef uint64 saf_ingress_speed_r_PORT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  rx_voq_port_cfg_r RX_VOQ_PORT_CFG;
  saf_ingress_speed_r SAF_INGRESS_SPEED;
  saf_egress_speed_r SAF_EGRESS_SPEED[256];
  saf_tc_r SAF_TC[16];
} mby_rx_voq_map;

typedef struct {
  rx_voq_port_cfg_r__addr RX_VOQ_PORT_CFG;
  saf_ingress_speed_r__addr SAF_INGRESS_SPEED;
  saf_egress_speed_r__addr SAF_EGRESS_SPEED[256];
  saf_tc_r__addr SAF_TC[16];
} mby_rx_voq_map__addr;


void
mby_rx_voq_map__init(
  mby_rx_voq_map *p0,
  mby_rx_voq_map__addr *p1,
  void (*f)(void *, int)
);

typedef rx_voq_port_cfg_r mby_rx_voq_map_RX_VOQ_PORT_CFG_t;

typedef saf_ingress_speed_r mby_rx_voq_map_SAF_INGRESS_SPEED_t;

static const unsigned int mby_rx_voq_map_SAF_EGRESS_SPEED__n = 256;
#define mby_rx_voq_map_SAF_EGRESS_SPEED__nd    256
typedef saf_egress_speed_r mby_rx_voq_map_SAF_EGRESS_SPEED_t[256];

static const unsigned int mby_rx_voq_map_SAF_TC__n = 16;
#define mby_rx_voq_map_SAF_TC__nd    16
typedef saf_tc_r mby_rx_voq_map_SAF_TC_t[16];




/* ../src/RegC.m3:179 */
typedef struct {
  uint44 BYTE_COUNTER;
} rx_stats_vlan_byte_r;

typedef struct {
  uint44 *BYTE_COUNTER;
} rx_stats_vlan_byte_r__addr;


void
rx_stats_vlan_byte_r__init(
  rx_stats_vlan_byte_r *p0,
  rx_stats_vlan_byte_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_stats_vlan_byte_r_BYTE_COUNTER__n = 44;
#define rx_stats_vlan_byte_r_BYTE_COUNTER__nd    44
typedef uint44 rx_stats_vlan_byte_r_BYTE_COUNTER_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 SHELL_CTRL_U_ERR;
  uint1 ENTRY_COUNT;
  uint1 TCAM_ERR;
  uint1 TRIGGER;
  uint1 WB_FIFO_PERR;
  uint1 MA_TCN;
} fwd_ip_r;

typedef struct {
  uint1 *SHELL_CTRL_U_ERR;
  uint1 *ENTRY_COUNT;
  uint1 *TCAM_ERR;
  uint1 *TRIGGER;
  uint1 *WB_FIFO_PERR;
  uint1 *MA_TCN;
} fwd_ip_r__addr;


void
fwd_ip_r__init(
  fwd_ip_r *p0,
  fwd_ip_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_ip_r_SHELL_CTRL_U_ERR__n = 1;
#define fwd_ip_r_SHELL_CTRL_U_ERR__nd    1
typedef uint1 fwd_ip_r_SHELL_CTRL_U_ERR_t;

static const unsigned int fwd_ip_r_ENTRY_COUNT__n = 1;
#define fwd_ip_r_ENTRY_COUNT__nd    1
typedef uint1 fwd_ip_r_ENTRY_COUNT_t;

static const unsigned int fwd_ip_r_TCAM_ERR__n = 1;
#define fwd_ip_r_TCAM_ERR__nd    1
typedef uint1 fwd_ip_r_TCAM_ERR_t;

static const unsigned int fwd_ip_r_TRIGGER__n = 1;
#define fwd_ip_r_TRIGGER__nd    1
typedef uint1 fwd_ip_r_TRIGGER_t;

static const unsigned int fwd_ip_r_WB_FIFO_PERR__n = 1;
#define fwd_ip_r_WB_FIFO_PERR__nd    1
typedef uint1 fwd_ip_r_WB_FIFO_PERR_t;

static const unsigned int fwd_ip_r_MA_TCN__n = 1;
#define fwd_ip_r_MA_TCN__nd    1
typedef uint1 fwd_ip_r_MA_TCN_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 MAC_ADDRESS;
  uint5 PORT;
} ma_tcn_fifo_0_r;

typedef struct {
  uint48 *MAC_ADDRESS;
  uint5 *PORT;
} ma_tcn_fifo_0_r__addr;


void
ma_tcn_fifo_0_r__init(
  ma_tcn_fifo_0_r *p0,
  ma_tcn_fifo_0_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int ma_tcn_fifo_0_r_MAC_ADDRESS__n = 48;
#define ma_tcn_fifo_0_r_MAC_ADDRESS__nd    48
typedef uint48 ma_tcn_fifo_0_r_MAC_ADDRESS_t;

static const unsigned int ma_tcn_fifo_0_r_PORT__n = 5;
#define ma_tcn_fifo_0_r_PORT__nd    5
typedef uint5 ma_tcn_fifo_0_r_PORT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  trigger_rate_lim_cfg_2_r TRIGGER_RATE_LIM_CFG_2[16];
  trigger_action_metadata_mask_rf TRIGGER_ACTION_METADATA_MASK[2];
  trigger_ip_r TRIGGER_IP[2];
  trigger_im_r TRIGGER_IM[2];
  trigger_rate_lim_empty_r TRIGGER_RATE_LIM_EMPTY;
  ma_tcn_fifo_0_r MA_TCN_FIFO_0[512];
  ma_tcn_fifo_1_r MA_TCN_FIFO_1[512];
  ma_tcn_dequeue_r MA_TCN_DEQUEUE;
  ma_tcn_data_0_r MA_TCN_DATA_0;
  ma_tcn_data_1_r MA_TCN_DATA_1;
  ma_tcn_ptr_head_r MA_TCN_PTR_HEAD;
  ma_tcn_ptr_tail_r MA_TCN_PTR_TAIL;
  ma_tcn_ip_r MA_TCN_IP;
  ma_tcn_im_r MA_TCN_IM;
  ma_tcn_wm_r MA_TCN_WM[17];
  ma_tcn_usage_r MA_TCN_USAGE[17];
} mby_ppe_trig_apply_misc_map;

typedef struct {
  trigger_rate_lim_cfg_2_r__addr TRIGGER_RATE_LIM_CFG_2[16];
  trigger_action_metadata_mask_rf__addr TRIGGER_ACTION_METADATA_MASK[2];
  trigger_ip_r__addr TRIGGER_IP[2];
  trigger_im_r__addr TRIGGER_IM[2];
  trigger_rate_lim_empty_r__addr TRIGGER_RATE_LIM_EMPTY;
  ma_tcn_fifo_0_r__addr MA_TCN_FIFO_0[512];
  ma_tcn_fifo_1_r__addr MA_TCN_FIFO_1[512];
  ma_tcn_dequeue_r__addr MA_TCN_DEQUEUE;
  ma_tcn_data_0_r__addr MA_TCN_DATA_0;
  ma_tcn_data_1_r__addr MA_TCN_DATA_1;
  ma_tcn_ptr_head_r__addr MA_TCN_PTR_HEAD;
  ma_tcn_ptr_tail_r__addr MA_TCN_PTR_TAIL;
  ma_tcn_ip_r__addr MA_TCN_IP;
  ma_tcn_im_r__addr MA_TCN_IM;
  ma_tcn_wm_r__addr MA_TCN_WM[17];
  ma_tcn_usage_r__addr MA_TCN_USAGE[17];
} mby_ppe_trig_apply_misc_map__addr;


void
mby_ppe_trig_apply_misc_map__init(
  mby_ppe_trig_apply_misc_map *p0,
  mby_ppe_trig_apply_misc_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_trig_apply_misc_map_TRIGGER_RATE_LIM_CFG_2__n = 16;
#define mby_ppe_trig_apply_misc_map_TRIGGER_RATE_LIM_CFG_2__nd    16
typedef trigger_rate_lim_cfg_2_r mby_ppe_trig_apply_misc_map_TRIGGER_RATE_LIM_CFG_2_t[16];

static const unsigned int mby_ppe_trig_apply_misc_map_TRIGGER_ACTION_METADATA_MASK__n = 2;
#define mby_ppe_trig_apply_misc_map_TRIGGER_ACTION_METADATA_MASK__nd    2
typedef trigger_action_metadata_mask_rf mby_ppe_trig_apply_misc_map_TRIGGER_ACTION_METADATA_MASK_t[2];

static const unsigned int mby_ppe_trig_apply_misc_map_TRIGGER_IP__n = 2;
#define mby_ppe_trig_apply_misc_map_TRIGGER_IP__nd    2
typedef trigger_ip_r mby_ppe_trig_apply_misc_map_TRIGGER_IP_t[2];

static const unsigned int mby_ppe_trig_apply_misc_map_TRIGGER_IM__n = 2;
#define mby_ppe_trig_apply_misc_map_TRIGGER_IM__nd    2
typedef trigger_im_r mby_ppe_trig_apply_misc_map_TRIGGER_IM_t[2];

typedef trigger_rate_lim_empty_r mby_ppe_trig_apply_misc_map_TRIGGER_RATE_LIM_EMPTY_t;

static const unsigned int mby_ppe_trig_apply_misc_map_MA_TCN_FIFO_0__n = 512;
#define mby_ppe_trig_apply_misc_map_MA_TCN_FIFO_0__nd    512
typedef ma_tcn_fifo_0_r mby_ppe_trig_apply_misc_map_MA_TCN_FIFO_0_t[512];

static const unsigned int mby_ppe_trig_apply_misc_map_MA_TCN_FIFO_1__n = 512;
#define mby_ppe_trig_apply_misc_map_MA_TCN_FIFO_1__nd    512
typedef ma_tcn_fifo_1_r mby_ppe_trig_apply_misc_map_MA_TCN_FIFO_1_t[512];

typedef ma_tcn_dequeue_r mby_ppe_trig_apply_misc_map_MA_TCN_DEQUEUE_t;

typedef ma_tcn_data_0_r mby_ppe_trig_apply_misc_map_MA_TCN_DATA_0_t;

typedef ma_tcn_data_1_r mby_ppe_trig_apply_misc_map_MA_TCN_DATA_1_t;

typedef ma_tcn_ptr_head_r mby_ppe_trig_apply_misc_map_MA_TCN_PTR_HEAD_t;

typedef ma_tcn_ptr_tail_r mby_ppe_trig_apply_misc_map_MA_TCN_PTR_TAIL_t;

typedef ma_tcn_ip_r mby_ppe_trig_apply_misc_map_MA_TCN_IP_t;

typedef ma_tcn_im_r mby_ppe_trig_apply_misc_map_MA_TCN_IM_t;

static const unsigned int mby_ppe_trig_apply_misc_map_MA_TCN_WM__n = 17;
#define mby_ppe_trig_apply_misc_map_MA_TCN_WM__nd    17
typedef ma_tcn_wm_r mby_ppe_trig_apply_misc_map_MA_TCN_WM_t[17];

static const unsigned int mby_ppe_trig_apply_misc_map_MA_TCN_USAGE__n = 17;
#define mby_ppe_trig_apply_misc_map_MA_TCN_USAGE__nd    17
typedef ma_tcn_usage_r mby_ppe_trig_apply_misc_map_MA_TCN_USAGE_t[17];




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 HANDLER_ACTION_MASK;
} trigger_condition_amask_1_r;

typedef struct {
  uint32 *HANDLER_ACTION_MASK;
} trigger_condition_amask_1_r__addr;


void
trigger_condition_amask_1_r__init(
  trigger_condition_amask_1_r *p0,
  trigger_condition_amask_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_condition_amask_1_r_HANDLER_ACTION_MASK__n = 32;
#define trigger_condition_amask_1_r_HANDLER_ACTION_MASK__nd    32
typedef uint32 trigger_condition_amask_1_r_HANDLER_ACTION_MASK_t;




/* ../src/RegC.m3:221 */
typedef pol_vpri_r pol_vpri_rf[16];

typedef pol_vpri_r__addr pol_vpri_rf__addr[16];


void
pol_vpri_rf__init(
  pol_vpri_rf *p0,
  pol_vpri_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_vpri_rf_POL_VPRI__n = 16;
#define pol_vpri_rf_POL_VPRI__nd    16



/* ../src/RegC.m3:221 */
typedef entropy_hash_cfg0_r entropy_hash_cfg0_rf[64];

typedef entropy_hash_cfg0_r__addr entropy_hash_cfg0_rf__addr[64];


void
entropy_hash_cfg0_rf__init(
  entropy_hash_cfg0_rf *p0,
  entropy_hash_cfg0_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_cfg0_rf_ENTROPY_HASH_CFG0__n = 64;
#define entropy_hash_cfg0_rf_ENTROPY_HASH_CFG0__nd    64



/* ../src/RegC.m3:179 */
typedef struct {
  uint15 THRESHOLD;
  uint3 TC;
} cm_aqm_dctcp_cfg_r;

typedef struct {
  uint15 *THRESHOLD;
  uint3 *TC;
} cm_aqm_dctcp_cfg_r__addr;


void
cm_aqm_dctcp_cfg_r__init(
  cm_aqm_dctcp_cfg_r *p0,
  cm_aqm_dctcp_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_aqm_dctcp_cfg_r_THRESHOLD__n = 15;
#define cm_aqm_dctcp_cfg_r_THRESHOLD__nd    15
typedef uint15 cm_aqm_dctcp_cfg_r_THRESHOLD_t;

static const unsigned int cm_aqm_dctcp_cfg_r_TC__n = 3;
#define cm_aqm_dctcp_cfg_r_TC__nd    3
typedef uint3 cm_aqm_dctcp_cfg_r_TC_t;




/* ../src/RegC.m3:221 */
typedef cm_aqm_dctcp_cfg_r cm_aqm_dctcp_cfg_rf[2];

typedef cm_aqm_dctcp_cfg_r__addr cm_aqm_dctcp_cfg_rf__addr[2];


void
cm_aqm_dctcp_cfg_rf__init(
  cm_aqm_dctcp_cfg_rf *p0,
  cm_aqm_dctcp_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_aqm_dctcp_cfg_rf_CM_AQM_DCTCP_CFG__n = 2;
#define cm_aqm_dctcp_cfg_rf_CM_AQM_DCTCP_CFG__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint16 KEY;
  uint16 KEY_INVERT;
} glort_cam_r;

typedef struct {
  uint16 *KEY;
  uint16 *KEY_INVERT;
} glort_cam_r__addr;


void
glort_cam_r__init(
  glort_cam_r *p0,
  glort_cam_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int glort_cam_r_KEY__n = 16;
#define glort_cam_r_KEY__nd    16
typedef uint16 glort_cam_r_KEY_t;

static const unsigned int glort_cam_r_KEY_INVERT__n = 16;
#define glort_cam_r_KEY_INVERT__nd    16
typedef uint16 glort_cam_r_KEY_INVERT_t;




/* ../src/RegC.m3:221 */
typedef parser_extract_cfg_r parser_extract_cfg_rf[80];

typedef parser_extract_cfg_r__addr parser_extract_cfg_rf__addr[80];


void
parser_extract_cfg_rf__init(
  parser_extract_cfg_rf *p0,
  parser_extract_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_extract_cfg_rf_PARSER_EXTRACT_CFG__n = 80;
#define parser_extract_cfg_rf_PARSER_EXTRACT_CFG__nd    80



/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_rx_smp_usage_max_r;

typedef struct {
  uint16 *COUNT;
} cm_rx_smp_usage_max_r__addr;


void
cm_rx_smp_usage_max_r__init(
  cm_rx_smp_usage_max_r *p0,
  cm_rx_smp_usage_max_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_usage_max_r_COUNT__n = 16;
#define cm_rx_smp_usage_max_r_COUNT__nd    16
typedef uint16 cm_rx_smp_usage_max_r_COUNT_t;




/* ../src/RegC.m3:221 */
typedef cm_rx_smp_usage_max_r cm_rx_smp_usage_max_rf[2];

typedef cm_rx_smp_usage_max_r__addr cm_rx_smp_usage_max_rf__addr[2];


void
cm_rx_smp_usage_max_rf__init(
  cm_rx_smp_usage_max_rf *p0,
  cm_rx_smp_usage_max_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_rx_smp_usage_max_rf_CM_RX_SMP_USAGE_MAX__n = 2;
#define cm_rx_smp_usage_max_rf_CM_RX_SMP_USAGE_MAX__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint2 DROP_MASK;
} trigger_direct_map_adr4_r;

typedef struct {
  uint2 *DROP_MASK;
} trigger_direct_map_adr4_r__addr;


void
trigger_direct_map_adr4_r__init(
  trigger_direct_map_adr4_r *p0,
  trigger_direct_map_adr4_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_direct_map_adr4_r_DROP_MASK__n = 2;
#define trigger_direct_map_adr4_r_DROP_MASK__nd    2
typedef uint2 trigger_direct_map_adr4_r_DROP_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint36 FRAME_COUNTER;
} rx_stats_vlan_frame_r;

typedef struct {
  uint36 *FRAME_COUNTER;
} rx_stats_vlan_frame_r__addr;


void
rx_stats_vlan_frame_r__init(
  rx_stats_vlan_frame_r *p0,
  rx_stats_vlan_frame_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_stats_vlan_frame_r_FRAME_COUNTER__n = 36;
#define rx_stats_vlan_frame_r_FRAME_COUNTER__nd    36
typedef uint36 rx_stats_vlan_frame_r_FRAME_COUNTER_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 SMP_0;
  uint1 SMP_1;
  uint1 SMP_2;
  uint1 SMP_3;
  uint1 SMP_4;
  uint1 SMP_5;
  uint1 SMP_6;
  uint1 SMP_7;
} cm_apply_tc_to_smp_r;

typedef struct {
  uint1 *SMP_0;
  uint1 *SMP_1;
  uint1 *SMP_2;
  uint1 *SMP_3;
  uint1 *SMP_4;
  uint1 *SMP_5;
  uint1 *SMP_6;
  uint1 *SMP_7;
} cm_apply_tc_to_smp_r__addr;


void
cm_apply_tc_to_smp_r__init(
  cm_apply_tc_to_smp_r *p0,
  cm_apply_tc_to_smp_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_apply_tc_to_smp_r_SMP_0__n = 1;
#define cm_apply_tc_to_smp_r_SMP_0__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_0_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_1__n = 1;
#define cm_apply_tc_to_smp_r_SMP_1__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_1_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_2__n = 1;
#define cm_apply_tc_to_smp_r_SMP_2__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_2_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_3__n = 1;
#define cm_apply_tc_to_smp_r_SMP_3__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_3_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_4__n = 1;
#define cm_apply_tc_to_smp_r_SMP_4__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_4_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_5__n = 1;
#define cm_apply_tc_to_smp_r_SMP_5__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_5_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_6__n = 1;
#define cm_apply_tc_to_smp_r_SMP_6__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_6_t;

static const unsigned int cm_apply_tc_to_smp_r_SMP_7__n = 1;
#define cm_apply_tc_to_smp_r_SMP_7__nd    1
typedef uint1 cm_apply_tc_to_smp_r_SMP_7_t;




/* ../src/RegC.m3:221 */
typedef struct {
  cm_apply_tx_softdrop_cfg_rf CM_APPLY_TX_SOFTDROP_CFG[64];
  cm_apply_tx_tc_state_rf CM_APPLY_TX_TC_STATE[64];
  cm_apply_tx_tc_qcn_wm_threshold_rf CM_APPLY_TX_TC_QCN_WM_THRESHOLD[64];
  cm_apply_rx_smp_state_rf CM_APPLY_RX_SMP_STATE[32];
  cm_apply_mirror_ecmp_dmask0_r CM_APPLY_MIRROR_ECMP_DMASK0[64];
  cm_apply_mirror_ecmp_dmask1_r CM_APPLY_MIRROR_ECMP_DMASK1[64];
  cm_apply_mirror_ecmp_dmask2_r CM_APPLY_MIRROR_ECMP_DMASK2[64];
  cm_apply_mirror_ecmp_dmask3_r CM_APPLY_MIRROR_ECMP_DMASK3[64];
  cm_apply_mirror_ecmp_dmask4_r CM_APPLY_MIRROR_ECMP_DMASK4[64];
  cm_apply_drop_count_r CM_APPLY_DROP_COUNT[32];
  cm_apply_loopback_suppress_r CM_APPLY_LOOPBACK_SUPPRESS[257];
  cm_apply_softdrop_cfg_r CM_APPLY_SOFTDROP_CFG[8];
  cm_apply_softdrop_state_r CM_APPLY_SOFTDROP_STATE[8];
  cm_apply_trap_glort_r CM_APPLY_TRAP_GLORT[16];
  cm_apply_tc_to_smp_r CM_APPLY_TC_TO_SMP;
  cm_apply_mcast_epoch_r CM_APPLY_MCAST_EPOCH;
  cm_apply_state_r CM_APPLY_STATE;
  cm_apply_cpu_trap_mask_0_r CM_APPLY_CPU_TRAP_0_MASK;
  cm_apply_cpu_trap_mask_1_r CM_APPLY_CPU_TRAP_1_MASK;
  cm_apply_cpu_trap_mask_2_r CM_APPLY_CPU_TRAP_2_MASK;
  cm_apply_cpu_trap_mask_3_r CM_APPLY_CPU_TRAP_3_MASK;
  cm_apply_cpu_trap_mask_4_r CM_APPLY_CPU_TRAP_4_MASK;
  cm_apply_log_mirror_profile_r CM_APPLY_LOG_MIRROR_PROFILE;
  cm_apply_qcn_cfg_r CM_APPLY_QCN_CFG;
} mby_ppe_cm_apply_map;

typedef struct {
  cm_apply_tx_softdrop_cfg_rf__addr CM_APPLY_TX_SOFTDROP_CFG[64];
  cm_apply_tx_tc_state_rf__addr CM_APPLY_TX_TC_STATE[64];
  cm_apply_tx_tc_qcn_wm_threshold_rf__addr CM_APPLY_TX_TC_QCN_WM_THRESHOLD[64];
  cm_apply_rx_smp_state_rf__addr CM_APPLY_RX_SMP_STATE[32];
  cm_apply_mirror_ecmp_dmask0_r__addr CM_APPLY_MIRROR_ECMP_DMASK0[64];
  cm_apply_mirror_ecmp_dmask1_r__addr CM_APPLY_MIRROR_ECMP_DMASK1[64];
  cm_apply_mirror_ecmp_dmask2_r__addr CM_APPLY_MIRROR_ECMP_DMASK2[64];
  cm_apply_mirror_ecmp_dmask3_r__addr CM_APPLY_MIRROR_ECMP_DMASK3[64];
  cm_apply_mirror_ecmp_dmask4_r__addr CM_APPLY_MIRROR_ECMP_DMASK4[64];
  cm_apply_drop_count_r__addr CM_APPLY_DROP_COUNT[32];
  cm_apply_loopback_suppress_r__addr CM_APPLY_LOOPBACK_SUPPRESS[257];
  cm_apply_softdrop_cfg_r__addr CM_APPLY_SOFTDROP_CFG[8];
  cm_apply_softdrop_state_r__addr CM_APPLY_SOFTDROP_STATE[8];
  cm_apply_trap_glort_r__addr CM_APPLY_TRAP_GLORT[16];
  cm_apply_tc_to_smp_r__addr CM_APPLY_TC_TO_SMP;
  cm_apply_mcast_epoch_r__addr CM_APPLY_MCAST_EPOCH;
  cm_apply_state_r__addr CM_APPLY_STATE;
  cm_apply_cpu_trap_mask_0_r__addr CM_APPLY_CPU_TRAP_0_MASK;
  cm_apply_cpu_trap_mask_1_r__addr CM_APPLY_CPU_TRAP_1_MASK;
  cm_apply_cpu_trap_mask_2_r__addr CM_APPLY_CPU_TRAP_2_MASK;
  cm_apply_cpu_trap_mask_3_r__addr CM_APPLY_CPU_TRAP_3_MASK;
  cm_apply_cpu_trap_mask_4_r__addr CM_APPLY_CPU_TRAP_4_MASK;
  cm_apply_log_mirror_profile_r__addr CM_APPLY_LOG_MIRROR_PROFILE;
  cm_apply_qcn_cfg_r__addr CM_APPLY_QCN_CFG;
} mby_ppe_cm_apply_map__addr;


void
mby_ppe_cm_apply_map__init(
  mby_ppe_cm_apply_map *p0,
  mby_ppe_cm_apply_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_TX_SOFTDROP_CFG__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_TX_SOFTDROP_CFG__nd    64
typedef cm_apply_tx_softdrop_cfg_rf mby_ppe_cm_apply_map_CM_APPLY_TX_SOFTDROP_CFG_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_TX_TC_STATE__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_TX_TC_STATE__nd    64
typedef cm_apply_tx_tc_state_rf mby_ppe_cm_apply_map_CM_APPLY_TX_TC_STATE_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_TX_TC_QCN_WM_THRESHOLD__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_TX_TC_QCN_WM_THRESHOLD__nd    64
typedef cm_apply_tx_tc_qcn_wm_threshold_rf mby_ppe_cm_apply_map_CM_APPLY_TX_TC_QCN_WM_THRESHOLD_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_RX_SMP_STATE__n = 32;
#define mby_ppe_cm_apply_map_CM_APPLY_RX_SMP_STATE__nd    32
typedef cm_apply_rx_smp_state_rf mby_ppe_cm_apply_map_CM_APPLY_RX_SMP_STATE_t[32];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK0__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK0__nd    64
typedef cm_apply_mirror_ecmp_dmask0_r mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK0_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK1__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK1__nd    64
typedef cm_apply_mirror_ecmp_dmask1_r mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK1_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK2__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK2__nd    64
typedef cm_apply_mirror_ecmp_dmask2_r mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK2_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK3__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK3__nd    64
typedef cm_apply_mirror_ecmp_dmask3_r mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK3_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK4__n = 64;
#define mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK4__nd    64
typedef cm_apply_mirror_ecmp_dmask4_r mby_ppe_cm_apply_map_CM_APPLY_MIRROR_ECMP_DMASK4_t[64];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_DROP_COUNT__n = 32;
#define mby_ppe_cm_apply_map_CM_APPLY_DROP_COUNT__nd    32
typedef cm_apply_drop_count_r mby_ppe_cm_apply_map_CM_APPLY_DROP_COUNT_t[32];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_LOOPBACK_SUPPRESS__n = 257;
#define mby_ppe_cm_apply_map_CM_APPLY_LOOPBACK_SUPPRESS__nd    257
typedef cm_apply_loopback_suppress_r mby_ppe_cm_apply_map_CM_APPLY_LOOPBACK_SUPPRESS_t[257];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_SOFTDROP_CFG__n = 8;
#define mby_ppe_cm_apply_map_CM_APPLY_SOFTDROP_CFG__nd    8
typedef cm_apply_softdrop_cfg_r mby_ppe_cm_apply_map_CM_APPLY_SOFTDROP_CFG_t[8];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_SOFTDROP_STATE__n = 8;
#define mby_ppe_cm_apply_map_CM_APPLY_SOFTDROP_STATE__nd    8
typedef cm_apply_softdrop_state_r mby_ppe_cm_apply_map_CM_APPLY_SOFTDROP_STATE_t[8];

static const unsigned int mby_ppe_cm_apply_map_CM_APPLY_TRAP_GLORT__n = 16;
#define mby_ppe_cm_apply_map_CM_APPLY_TRAP_GLORT__nd    16
typedef cm_apply_trap_glort_r mby_ppe_cm_apply_map_CM_APPLY_TRAP_GLORT_t[16];

typedef cm_apply_tc_to_smp_r mby_ppe_cm_apply_map_CM_APPLY_TC_TO_SMP_t;

typedef cm_apply_mcast_epoch_r mby_ppe_cm_apply_map_CM_APPLY_MCAST_EPOCH_t;

typedef cm_apply_state_r mby_ppe_cm_apply_map_CM_APPLY_STATE_t;

typedef cm_apply_cpu_trap_mask_0_r mby_ppe_cm_apply_map_CM_APPLY_CPU_TRAP_0_MASK_t;

typedef cm_apply_cpu_trap_mask_1_r mby_ppe_cm_apply_map_CM_APPLY_CPU_TRAP_1_MASK_t;

typedef cm_apply_cpu_trap_mask_2_r mby_ppe_cm_apply_map_CM_APPLY_CPU_TRAP_2_MASK_t;

typedef cm_apply_cpu_trap_mask_3_r mby_ppe_cm_apply_map_CM_APPLY_CPU_TRAP_3_MASK_t;

typedef cm_apply_cpu_trap_mask_4_r mby_ppe_cm_apply_map_CM_APPLY_CPU_TRAP_4_MASK_t;

typedef cm_apply_log_mirror_profile_r mby_ppe_cm_apply_map_CM_APPLY_LOG_MIRROR_PROFILE_t;

typedef cm_apply_qcn_cfg_r mby_ppe_cm_apply_map_CM_APPLY_QCN_CFG_t;




/* ../src/RegC.m3:221 */
typedef wcm_action_cfg_r wcm_action_cfg_rf[2];

typedef wcm_action_cfg_r__addr wcm_action_cfg_rf__addr[2];


void
wcm_action_cfg_rf__init(
  wcm_action_cfg_rf *p0,
  wcm_action_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int wcm_action_cfg_rf_WCM_IDX__n = 2;
#define wcm_action_cfg_rf_WCM_IDX__nd    2



/* ../src/RegC.m3:221 */
typedef struct {
  em_hash_lookup_r EM_HASH_LOOKUP[8192];
  wcm_tcam_rf WCM_TCAM[20];
  wcm_action_rf WCM_ACTION[24];
  wcm_tcam_cfg_rf WCM_TCAM_CFG[20];
  wcm_action_cfg_en_r WCM_ACTION_CFG_EN[64];
  wcm_action_cfg_rf WCM_ACTION_CFG[64];
  wcm_ip_r WCM_IP;
  wcm_im_r WCM_IM;
} mby_ppe_cgrp_b_nested_map;

typedef struct {
  em_hash_lookup_r__addr EM_HASH_LOOKUP[8192];
  wcm_tcam_rf__addr WCM_TCAM[20];
  wcm_action_rf__addr WCM_ACTION[24];
  wcm_tcam_cfg_rf__addr WCM_TCAM_CFG[20];
  wcm_action_cfg_en_r__addr WCM_ACTION_CFG_EN[64];
  wcm_action_cfg_rf__addr WCM_ACTION_CFG[64];
  wcm_ip_r__addr WCM_IP;
  wcm_im_r__addr WCM_IM;
} mby_ppe_cgrp_b_nested_map__addr;


void
mby_ppe_cgrp_b_nested_map__init(
  mby_ppe_cgrp_b_nested_map *p0,
  mby_ppe_cgrp_b_nested_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_cgrp_b_nested_map_EM_HASH_LOOKUP__n = 8192;
#define mby_ppe_cgrp_b_nested_map_EM_HASH_LOOKUP__nd    8192
typedef em_hash_lookup_r mby_ppe_cgrp_b_nested_map_EM_HASH_LOOKUP_t[8192];

static const unsigned int mby_ppe_cgrp_b_nested_map_WCM_TCAM__n = 20;
#define mby_ppe_cgrp_b_nested_map_WCM_TCAM__nd    20
typedef wcm_tcam_rf mby_ppe_cgrp_b_nested_map_WCM_TCAM_t[20];

static const unsigned int mby_ppe_cgrp_b_nested_map_WCM_ACTION__n = 24;
#define mby_ppe_cgrp_b_nested_map_WCM_ACTION__nd    24
typedef wcm_action_rf mby_ppe_cgrp_b_nested_map_WCM_ACTION_t[24];

static const unsigned int mby_ppe_cgrp_b_nested_map_WCM_TCAM_CFG__n = 20;
#define mby_ppe_cgrp_b_nested_map_WCM_TCAM_CFG__nd    20
typedef wcm_tcam_cfg_rf mby_ppe_cgrp_b_nested_map_WCM_TCAM_CFG_t[20];

static const unsigned int mby_ppe_cgrp_b_nested_map_WCM_ACTION_CFG_EN__n = 64;
#define mby_ppe_cgrp_b_nested_map_WCM_ACTION_CFG_EN__nd    64
typedef wcm_action_cfg_en_r mby_ppe_cgrp_b_nested_map_WCM_ACTION_CFG_EN_t[64];

static const unsigned int mby_ppe_cgrp_b_nested_map_WCM_ACTION_CFG__n = 64;
#define mby_ppe_cgrp_b_nested_map_WCM_ACTION_CFG__nd    64
typedef wcm_action_cfg_rf mby_ppe_cgrp_b_nested_map_WCM_ACTION_CFG_t[64];

typedef wcm_ip_r mby_ppe_cgrp_b_nested_map_WCM_IP_t;

typedef wcm_im_r mby_ppe_cgrp_b_nested_map_WCM_IM_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint48 POL_TIME;
} pol_time_r;

typedef struct {
  uint48 *POL_TIME;
} pol_time_r__addr;


void
pol_time_r__init(
  pol_time_r *p0,
  pol_time_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_time_r_POL_TIME__n = 48;
#define pol_time_r_POL_TIME__nd    48
typedef uint48 pol_time_r_POL_TIME_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint15 SOFT_DROP_SEGMENT_LIMIT;
  uint15 HOG_SEGMENT_LIMIT;
} cm_softdrop_wm_r;

typedef struct {
  uint15 *SOFT_DROP_SEGMENT_LIMIT;
  uint15 *HOG_SEGMENT_LIMIT;
} cm_softdrop_wm_r__addr;


void
cm_softdrop_wm_r__init(
  cm_softdrop_wm_r *p0,
  cm_softdrop_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_softdrop_wm_r_SOFT_DROP_SEGMENT_LIMIT__n = 15;
#define cm_softdrop_wm_r_SOFT_DROP_SEGMENT_LIMIT__nd    15
typedef uint15 cm_softdrop_wm_r_SOFT_DROP_SEGMENT_LIMIT_t;

static const unsigned int cm_softdrop_wm_r_HOG_SEGMENT_LIMIT__n = 15;
#define cm_softdrop_wm_r_HOG_SEGMENT_LIMIT__nd    15
typedef uint15 cm_softdrop_wm_r_HOG_SEGMENT_LIMIT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  em_hash_cam_rf HASH_CAM[32];
  em_hash_cam_en_rf HASH_CAM_EN[2];
  em_key_sel0_rf KEY_SEL0[2];
  em_key_sel1_rf KEY_SEL1[2];
  em_key_mask_rf KEY_MASK[2];
  em_hash_miss_rf HASH_MISS[2];
  em_hash_cfg_r HASH_CFG[64];
} mby_ppe_cgrp_em_map;

typedef struct {
  em_hash_cam_rf__addr HASH_CAM[32];
  em_hash_cam_en_rf__addr HASH_CAM_EN[2];
  em_key_sel0_rf__addr KEY_SEL0[2];
  em_key_sel1_rf__addr KEY_SEL1[2];
  em_key_mask_rf__addr KEY_MASK[2];
  em_hash_miss_rf__addr HASH_MISS[2];
  em_hash_cfg_r__addr HASH_CFG[64];
} mby_ppe_cgrp_em_map__addr;


void
mby_ppe_cgrp_em_map__init(
  mby_ppe_cgrp_em_map *p0,
  mby_ppe_cgrp_em_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_cgrp_em_map_HASH_CAM__n = 32;
#define mby_ppe_cgrp_em_map_HASH_CAM__nd    32
typedef em_hash_cam_rf mby_ppe_cgrp_em_map_HASH_CAM_t[32];

static const unsigned int mby_ppe_cgrp_em_map_HASH_CAM_EN__n = 2;
#define mby_ppe_cgrp_em_map_HASH_CAM_EN__nd    2
typedef em_hash_cam_en_rf mby_ppe_cgrp_em_map_HASH_CAM_EN_t[2];

static const unsigned int mby_ppe_cgrp_em_map_KEY_SEL0__n = 2;
#define mby_ppe_cgrp_em_map_KEY_SEL0__nd    2
typedef em_key_sel0_rf mby_ppe_cgrp_em_map_KEY_SEL0_t[2];

static const unsigned int mby_ppe_cgrp_em_map_KEY_SEL1__n = 2;
#define mby_ppe_cgrp_em_map_KEY_SEL1__nd    2
typedef em_key_sel1_rf mby_ppe_cgrp_em_map_KEY_SEL1_t[2];

static const unsigned int mby_ppe_cgrp_em_map_KEY_MASK__n = 2;
#define mby_ppe_cgrp_em_map_KEY_MASK__nd    2
typedef em_key_mask_rf mby_ppe_cgrp_em_map_KEY_MASK_t[2];

static const unsigned int mby_ppe_cgrp_em_map_HASH_MISS__n = 2;
#define mby_ppe_cgrp_em_map_HASH_MISS__nd    2
typedef em_hash_miss_rf mby_ppe_cgrp_em_map_HASH_MISS_t[2];

static const unsigned int mby_ppe_cgrp_em_map_HASH_CFG__n = 64;
#define mby_ppe_cgrp_em_map_HASH_CFG__nd    64
typedef em_hash_cfg_r mby_ppe_cgrp_em_map_HASH_CFG_t[64];




/* ../src/RegC.m3:221 */
typedef struct {
  mby_ppe_cgrp_em_map EM;
  mby_ppe_cgrp_b_nested_map B;
} mby_ppe_cgrp_b_map;

typedef struct {
  mby_ppe_cgrp_em_map__addr EM;
  mby_ppe_cgrp_b_nested_map__addr B;
} mby_ppe_cgrp_b_map__addr;


void
mby_ppe_cgrp_b_map__init(
  mby_ppe_cgrp_b_map *p0,
  mby_ppe_cgrp_b_map__addr *p1,
  void (*f)(void *, int)
);

typedef mby_ppe_cgrp_em_map mby_ppe_cgrp_b_map_EM_t;

typedef mby_ppe_cgrp_b_nested_map mby_ppe_cgrp_b_map_B_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mby_ppe_cgrp_em_map EM;
  mby_ppe_cgrp_a_nested_map A;
} mby_ppe_cgrp_a_map;

typedef struct {
  mby_ppe_cgrp_em_map__addr EM;
  mby_ppe_cgrp_a_nested_map__addr A;
} mby_ppe_cgrp_a_map__addr;


void
mby_ppe_cgrp_a_map__init(
  mby_ppe_cgrp_a_map *p0,
  mby_ppe_cgrp_a_map__addr *p1,
  void (*f)(void *, int)
);

typedef mby_ppe_cgrp_em_map mby_ppe_cgrp_a_map_EM_t;

typedef mby_ppe_cgrp_a_nested_map mby_ppe_cgrp_a_map_A_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 IS_IPV6;
  uint2 IP_PROFILE;
  uint4 MAP_IP;
  uint4 VALID;
  uint8 MATCH_LENGTH;
} map_ip_cfg_r;

typedef struct {
  uint1 *IS_IPV6;
  uint2 *IP_PROFILE;
  uint4 *MAP_IP;
  uint4 *VALID;
  uint8 *MATCH_LENGTH;
} map_ip_cfg_r__addr;


void
map_ip_cfg_r__init(
  map_ip_cfg_r *p0,
  map_ip_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int map_ip_cfg_r_IS_IPV6__n = 1;
#define map_ip_cfg_r_IS_IPV6__nd    1
typedef uint1 map_ip_cfg_r_IS_IPV6_t;

static const unsigned int map_ip_cfg_r_IP_PROFILE__n = 2;
#define map_ip_cfg_r_IP_PROFILE__nd    2
typedef uint2 map_ip_cfg_r_IP_PROFILE_t;

static const unsigned int map_ip_cfg_r_MAP_IP__n = 4;
#define map_ip_cfg_r_MAP_IP__nd    4
typedef uint4 map_ip_cfg_r_MAP_IP_t;

static const unsigned int map_ip_cfg_r_VALID__n = 4;
#define map_ip_cfg_r_VALID__nd    4
typedef uint4 map_ip_cfg_r_VALID_t;

static const unsigned int map_ip_cfg_r_MATCH_LENGTH__n = 8;
#define map_ip_cfg_r_MATCH_LENGTH__nd    8
typedef uint8 map_ip_cfg_r_MATCH_LENGTH_t;




/* ../src/RegC.m3:221 */
typedef struct {
  map_port_cfg_r MAP_PORT_CFG[17];
  map_port_default_rf MAP_PORT_DEFAULT[17];
  map_domain_tcam_r MAP_DOMAIN_TCAM[4096];
  map_domain_action0_r MAP_DOMAIN_ACTION0[4096];
  map_domain_action1_r MAP_DOMAIN_ACTION1[4096];
  map_domain_profile_r MAP_DOMAIN_PROFILE[256];
  map_port_r MAP_PORT[17];
  map_mac_r MAP_MAC[96];
  map_ip_lo_r MAP_IP_LO[16];
  map_ip_hi_r MAP_IP_HI[16];
  map_ip_cfg_r MAP_IP_CFG[16];
  map_prot_r MAP_PROT[8];
  map_l4_src_r MAP_L4_SRC[64];
  map_l4_dst_r MAP_L4_DST[64];
  map_exp_tc_r MAP_EXP_TC[32];
  map_dscp_tc_r MAP_DSCP_TC[2048];
  map_vpri_tc_r MAP_VPRI_TC[32];
  map_vpri_r MAP_VPRI[32];
  map_profile_key0_r MAP_PROFILE_KEY0[96];
  map_profile_key_invert0_r MAP_PROFILE_KEY_INVERT0[96];
  map_profile_key1_r MAP_PROFILE_KEY1[96];
  map_profile_key_invert1_r MAP_PROFILE_KEY_INVERT1[96];
  map_profile_action_r MAP_PROFILE_ACTION[96];
  map_domain_pol_cfg_r MAP_DOMAIN_POL_CFG;
  map_rewrite_rf MAP_REWRITE[16];
  map_canonical_glort_cam_r MAP_CANONICAL_GLORT_CAM[16];
} mby_ppe_mapper_map;

typedef struct {
  map_port_cfg_r__addr MAP_PORT_CFG[17];
  map_port_default_rf__addr MAP_PORT_DEFAULT[17];
  map_domain_tcam_r__addr MAP_DOMAIN_TCAM[4096];
  map_domain_action0_r__addr MAP_DOMAIN_ACTION0[4096];
  map_domain_action1_r__addr MAP_DOMAIN_ACTION1[4096];
  map_domain_profile_r__addr MAP_DOMAIN_PROFILE[256];
  map_port_r__addr MAP_PORT[17];
  map_mac_r__addr MAP_MAC[96];
  map_ip_lo_r__addr MAP_IP_LO[16];
  map_ip_hi_r__addr MAP_IP_HI[16];
  map_ip_cfg_r__addr MAP_IP_CFG[16];
  map_prot_r__addr MAP_PROT[8];
  map_l4_src_r__addr MAP_L4_SRC[64];
  map_l4_dst_r__addr MAP_L4_DST[64];
  map_exp_tc_r__addr MAP_EXP_TC[32];
  map_dscp_tc_r__addr MAP_DSCP_TC[2048];
  map_vpri_tc_r__addr MAP_VPRI_TC[32];
  map_vpri_r__addr MAP_VPRI[32];
  map_profile_key0_r__addr MAP_PROFILE_KEY0[96];
  map_profile_key_invert0_r__addr MAP_PROFILE_KEY_INVERT0[96];
  map_profile_key1_r__addr MAP_PROFILE_KEY1[96];
  map_profile_key_invert1_r__addr MAP_PROFILE_KEY_INVERT1[96];
  map_profile_action_r__addr MAP_PROFILE_ACTION[96];
  map_domain_pol_cfg_r__addr MAP_DOMAIN_POL_CFG;
  map_rewrite_rf__addr MAP_REWRITE[16];
  map_canonical_glort_cam_r__addr MAP_CANONICAL_GLORT_CAM[16];
} mby_ppe_mapper_map__addr;


void
mby_ppe_mapper_map__init(
  mby_ppe_mapper_map *p0,
  mby_ppe_mapper_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_mapper_map_MAP_PORT_CFG__n = 17;
#define mby_ppe_mapper_map_MAP_PORT_CFG__nd    17
typedef map_port_cfg_r mby_ppe_mapper_map_MAP_PORT_CFG_t[17];

static const unsigned int mby_ppe_mapper_map_MAP_PORT_DEFAULT__n = 17;
#define mby_ppe_mapper_map_MAP_PORT_DEFAULT__nd    17
typedef map_port_default_rf mby_ppe_mapper_map_MAP_PORT_DEFAULT_t[17];

static const unsigned int mby_ppe_mapper_map_MAP_DOMAIN_TCAM__n = 4096;
#define mby_ppe_mapper_map_MAP_DOMAIN_TCAM__nd    4096
typedef map_domain_tcam_r mby_ppe_mapper_map_MAP_DOMAIN_TCAM_t[4096];

static const unsigned int mby_ppe_mapper_map_MAP_DOMAIN_ACTION0__n = 4096;
#define mby_ppe_mapper_map_MAP_DOMAIN_ACTION0__nd    4096
typedef map_domain_action0_r mby_ppe_mapper_map_MAP_DOMAIN_ACTION0_t[4096];

static const unsigned int mby_ppe_mapper_map_MAP_DOMAIN_ACTION1__n = 4096;
#define mby_ppe_mapper_map_MAP_DOMAIN_ACTION1__nd    4096
typedef map_domain_action1_r mby_ppe_mapper_map_MAP_DOMAIN_ACTION1_t[4096];

static const unsigned int mby_ppe_mapper_map_MAP_DOMAIN_PROFILE__n = 256;
#define mby_ppe_mapper_map_MAP_DOMAIN_PROFILE__nd    256
typedef map_domain_profile_r mby_ppe_mapper_map_MAP_DOMAIN_PROFILE_t[256];

static const unsigned int mby_ppe_mapper_map_MAP_PORT__n = 17;
#define mby_ppe_mapper_map_MAP_PORT__nd    17
typedef map_port_r mby_ppe_mapper_map_MAP_PORT_t[17];

static const unsigned int mby_ppe_mapper_map_MAP_MAC__n = 96;
#define mby_ppe_mapper_map_MAP_MAC__nd    96
typedef map_mac_r mby_ppe_mapper_map_MAP_MAC_t[96];

static const unsigned int mby_ppe_mapper_map_MAP_IP_LO__n = 16;
#define mby_ppe_mapper_map_MAP_IP_LO__nd    16
typedef map_ip_lo_r mby_ppe_mapper_map_MAP_IP_LO_t[16];

static const unsigned int mby_ppe_mapper_map_MAP_IP_HI__n = 16;
#define mby_ppe_mapper_map_MAP_IP_HI__nd    16
typedef map_ip_hi_r mby_ppe_mapper_map_MAP_IP_HI_t[16];

static const unsigned int mby_ppe_mapper_map_MAP_IP_CFG__n = 16;
#define mby_ppe_mapper_map_MAP_IP_CFG__nd    16
typedef map_ip_cfg_r mby_ppe_mapper_map_MAP_IP_CFG_t[16];

static const unsigned int mby_ppe_mapper_map_MAP_PROT__n = 8;
#define mby_ppe_mapper_map_MAP_PROT__nd    8
typedef map_prot_r mby_ppe_mapper_map_MAP_PROT_t[8];

static const unsigned int mby_ppe_mapper_map_MAP_L4_SRC__n = 64;
#define mby_ppe_mapper_map_MAP_L4_SRC__nd    64
typedef map_l4_src_r mby_ppe_mapper_map_MAP_L4_SRC_t[64];

static const unsigned int mby_ppe_mapper_map_MAP_L4_DST__n = 64;
#define mby_ppe_mapper_map_MAP_L4_DST__nd    64
typedef map_l4_dst_r mby_ppe_mapper_map_MAP_L4_DST_t[64];

static const unsigned int mby_ppe_mapper_map_MAP_EXP_TC__n = 32;
#define mby_ppe_mapper_map_MAP_EXP_TC__nd    32
typedef map_exp_tc_r mby_ppe_mapper_map_MAP_EXP_TC_t[32];

static const unsigned int mby_ppe_mapper_map_MAP_DSCP_TC__n = 2048;
#define mby_ppe_mapper_map_MAP_DSCP_TC__nd    2048
typedef map_dscp_tc_r mby_ppe_mapper_map_MAP_DSCP_TC_t[2048];

static const unsigned int mby_ppe_mapper_map_MAP_VPRI_TC__n = 32;
#define mby_ppe_mapper_map_MAP_VPRI_TC__nd    32
typedef map_vpri_tc_r mby_ppe_mapper_map_MAP_VPRI_TC_t[32];

static const unsigned int mby_ppe_mapper_map_MAP_VPRI__n = 32;
#define mby_ppe_mapper_map_MAP_VPRI__nd    32
typedef map_vpri_r mby_ppe_mapper_map_MAP_VPRI_t[32];

static const unsigned int mby_ppe_mapper_map_MAP_PROFILE_KEY0__n = 96;
#define mby_ppe_mapper_map_MAP_PROFILE_KEY0__nd    96
typedef map_profile_key0_r mby_ppe_mapper_map_MAP_PROFILE_KEY0_t[96];

static const unsigned int mby_ppe_mapper_map_MAP_PROFILE_KEY_INVERT0__n = 96;
#define mby_ppe_mapper_map_MAP_PROFILE_KEY_INVERT0__nd    96
typedef map_profile_key_invert0_r mby_ppe_mapper_map_MAP_PROFILE_KEY_INVERT0_t[96];

static const unsigned int mby_ppe_mapper_map_MAP_PROFILE_KEY1__n = 96;
#define mby_ppe_mapper_map_MAP_PROFILE_KEY1__nd    96
typedef map_profile_key1_r mby_ppe_mapper_map_MAP_PROFILE_KEY1_t[96];

static const unsigned int mby_ppe_mapper_map_MAP_PROFILE_KEY_INVERT1__n = 96;
#define mby_ppe_mapper_map_MAP_PROFILE_KEY_INVERT1__nd    96
typedef map_profile_key_invert1_r mby_ppe_mapper_map_MAP_PROFILE_KEY_INVERT1_t[96];

static const unsigned int mby_ppe_mapper_map_MAP_PROFILE_ACTION__n = 96;
#define mby_ppe_mapper_map_MAP_PROFILE_ACTION__nd    96
typedef map_profile_action_r mby_ppe_mapper_map_MAP_PROFILE_ACTION_t[96];

typedef map_domain_pol_cfg_r mby_ppe_mapper_map_MAP_DOMAIN_POL_CFG_t;

static const unsigned int mby_ppe_mapper_map_MAP_REWRITE__n = 16;
#define mby_ppe_mapper_map_MAP_REWRITE__nd    16
typedef map_rewrite_rf mby_ppe_mapper_map_MAP_REWRITE_t[16];

static const unsigned int mby_ppe_mapper_map_MAP_CANONICAL_GLORT_CAM__n = 16;
#define mby_ppe_mapper_map_MAP_CANONICAL_GLORT_CAM__nd    16
typedef map_canonical_glort_cam_r mby_ppe_mapper_map_MAP_CANONICAL_GLORT_CAM_t[16];




/* ../src/RegC.m3:179 */
typedef struct {
  uint13 CBS;
  uint2 CBS_UNIT;
  uint1 _RSVD0_;
  uint11 CIR;
  uint3 CIR_UNIT;
  uint34 _RSVD1_;
} pol_direct_map_pol3_r;

typedef struct {
  uint13 *CBS;
  uint2 *CBS_UNIT;
  uint1 *_RSVD0_;
  uint11 *CIR;
  uint3 *CIR_UNIT;
  uint34 *_RSVD1_;
} pol_direct_map_pol3_r__addr;


void
pol_direct_map_pol3_r__init(
  pol_direct_map_pol3_r *p0,
  pol_direct_map_pol3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_pol3_r_CBS__n = 13;
#define pol_direct_map_pol3_r_CBS__nd    13
typedef uint13 pol_direct_map_pol3_r_CBS_t;

static const unsigned int pol_direct_map_pol3_r_CBS_UNIT__n = 2;
#define pol_direct_map_pol3_r_CBS_UNIT__nd    2
typedef uint2 pol_direct_map_pol3_r_CBS_UNIT_t;

static const unsigned int pol_direct_map_pol3_r__RSVD0___n = 1;
#define pol_direct_map_pol3_r__RSVD0___nd    1
typedef uint1 pol_direct_map_pol3_r__RSVD0__t;

static const unsigned int pol_direct_map_pol3_r_CIR__n = 11;
#define pol_direct_map_pol3_r_CIR__nd    11
typedef uint11 pol_direct_map_pol3_r_CIR_t;

static const unsigned int pol_direct_map_pol3_r_CIR_UNIT__n = 3;
#define pol_direct_map_pol3_r_CIR_UNIT__nd    3
typedef uint3 pol_direct_map_pol3_r_CIR_UNIT_t;

static const unsigned int pol_direct_map_pol3_r__RSVD1___n = 34;
#define pol_direct_map_pol3_r__RSVD1___nd    34
typedef uint34 pol_direct_map_pol3_r__RSVD1__t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 FORWARDING;
} egress_mst_table_r;

typedef struct {
  uint64 *FORWARDING;
} egress_mst_table_r__addr;


void
egress_mst_table_r__init(
  egress_mst_table_r *p0,
  egress_mst_table_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int egress_mst_table_r_FORWARDING__n = 64;
#define egress_mst_table_r_FORWARDING__nd    64
typedef uint64 egress_mst_table_r_FORWARDING_t;




/* ../src/RegC.m3:221 */
typedef egress_mst_table_r egress_mst_table_rf[5];

typedef egress_mst_table_r__addr egress_mst_table_rf__addr[5];


void
egress_mst_table_rf__init(
  egress_mst_table_rf *p0,
  egress_mst_table_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int egress_mst_table_rf_EGRESS_MST_TABLE__n = 5;
#define egress_mst_table_rf_EGRESS_MST_TABLE__nd    5



/* ../src/RegC.m3:179 */
typedef struct {
  uint48 FRAME_COUNTER;
} rx_stats_bank_frame_r;

typedef struct {
  uint48 *FRAME_COUNTER;
} rx_stats_bank_frame_r__addr;


void
rx_stats_bank_frame_r__init(
  rx_stats_bank_frame_r *p0,
  rx_stats_bank_frame_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_stats_bank_frame_r_FRAME_COUNTER__n = 48;
#define rx_stats_bank_frame_r_FRAME_COUNTER__nd    48
typedef uint48 rx_stats_bank_frame_r_FRAME_COUNTER_t;




/* ../src/RegC.m3:221 */
typedef rx_stats_bank_frame_r rx_stats_bank_frame_rf[144];

typedef rx_stats_bank_frame_r__addr rx_stats_bank_frame_rf__addr[144];


void
rx_stats_bank_frame_rf__init(
  rx_stats_bank_frame_rf *p0,
  rx_stats_bank_frame_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_stats_bank_frame_rf_RX_STATS_BANK_FRAME__n = 144;
#define rx_stats_bank_frame_rf_RX_STATS_BANK_FRAME__nd    144



/* ../src/RegC.m3:221 */
typedef struct {
  rx_stats_bank_frame_rf RX_STATS_BANK_FRAME[4];
  rx_stats_bank_byte_rf RX_STATS_BANK_BYTE[4];
  rx_stats_vlan_frame_r RX_STATS_VLAN_FRAME[16128];
  rx_stats_vlan_byte_r RX_STATS_VLAN_BYTE[16128];
} mby_ppe_rx_stats_map;

typedef struct {
  rx_stats_bank_frame_rf__addr RX_STATS_BANK_FRAME[4];
  rx_stats_bank_byte_rf__addr RX_STATS_BANK_BYTE[4];
  rx_stats_vlan_frame_r__addr RX_STATS_VLAN_FRAME[16128];
  rx_stats_vlan_byte_r__addr RX_STATS_VLAN_BYTE[16128];
} mby_ppe_rx_stats_map__addr;


void
mby_ppe_rx_stats_map__init(
  mby_ppe_rx_stats_map *p0,
  mby_ppe_rx_stats_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_rx_stats_map_RX_STATS_BANK_FRAME__n = 4;
#define mby_ppe_rx_stats_map_RX_STATS_BANK_FRAME__nd    4
typedef rx_stats_bank_frame_rf mby_ppe_rx_stats_map_RX_STATS_BANK_FRAME_t[4];

static const unsigned int mby_ppe_rx_stats_map_RX_STATS_BANK_BYTE__n = 4;
#define mby_ppe_rx_stats_map_RX_STATS_BANK_BYTE__nd    4
typedef rx_stats_bank_byte_rf mby_ppe_rx_stats_map_RX_STATS_BANK_BYTE_t[4];

static const unsigned int mby_ppe_rx_stats_map_RX_STATS_VLAN_FRAME__n = 16128;
#define mby_ppe_rx_stats_map_RX_STATS_VLAN_FRAME__nd    16128
typedef rx_stats_vlan_frame_r mby_ppe_rx_stats_map_RX_STATS_VLAN_FRAME_t[16128];

static const unsigned int mby_ppe_rx_stats_map_RX_STATS_VLAN_BYTE__n = 16128;
#define mby_ppe_rx_stats_map_RX_STATS_VLAN_BYTE__nd    16128
typedef rx_stats_vlan_byte_r mby_ppe_rx_stats_map_RX_STATS_VLAN_BYTE_t[16128];




/* ../src/RegC.m3:179 */
typedef struct {
  uint10 XOFF_OR_DROP;
  uint10 XON;
  uint1 LOSSLESS;
} rx_pb_wm_r;

typedef struct {
  uint10 *XOFF_OR_DROP;
  uint10 *XON;
  uint1 *LOSSLESS;
} rx_pb_wm_r__addr;


void
rx_pb_wm_r__init(
  rx_pb_wm_r *p0,
  rx_pb_wm_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_pb_wm_r_XOFF_OR_DROP__n = 10;
#define rx_pb_wm_r_XOFF_OR_DROP__nd    10
typedef uint10 rx_pb_wm_r_XOFF_OR_DROP_t;

static const unsigned int rx_pb_wm_r_XON__n = 10;
#define rx_pb_wm_r_XON__nd    10
typedef uint10 rx_pb_wm_r_XON_t;

static const unsigned int rx_pb_wm_r_LOSSLESS__n = 1;
#define rx_pb_wm_r_LOSSLESS__nd    1
typedef uint1 rx_pb_wm_r_LOSSLESS_t;




/* ../src/RegC.m3:221 */
typedef rx_pb_wm_r rx_pb_wm_rf[8];

typedef rx_pb_wm_r__addr rx_pb_wm_rf__addr[8];


void
rx_pb_wm_rf__init(
  rx_pb_wm_rf *p0,
  rx_pb_wm_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int rx_pb_wm_rf_RX_PB_WM__n = 8;
#define rx_pb_wm_rf_RX_PB_WM__nd    8



/* ../src/RegC.m3:221 */
typedef struct {
  rx_pb_port_cfg_r RX_PB_PORT_CFG;
  rx_pb_wm_rf RX_PB_WM[18];
} mby_rx_pb_map;

typedef struct {
  rx_pb_port_cfg_r__addr RX_PB_PORT_CFG;
  rx_pb_wm_rf__addr RX_PB_WM[18];
} mby_rx_pb_map__addr;


void
mby_rx_pb_map__init(
  mby_rx_pb_map *p0,
  mby_rx_pb_map__addr *p1,
  void (*f)(void *, int)
);

typedef rx_pb_port_cfg_r mby_rx_pb_map_RX_PB_PORT_CFG_t;

static const unsigned int mby_rx_pb_map_RX_PB_WM__n = 18;
#define mby_rx_pb_map_RX_PB_WM__nd    18
typedef rx_pb_wm_rf mby_rx_pb_map_RX_PB_WM_t[18];




/* ../src/RegC.m3:179 */
typedef struct {
  uint16 STATE_MASK;
  uint16 STATE_VALUE;
} parser_key_s_r;

typedef struct {
  uint16 *STATE_MASK;
  uint16 *STATE_VALUE;
} parser_key_s_r__addr;


void
parser_key_s_r__init(
  parser_key_s_r *p0,
  parser_key_s_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_key_s_r_STATE_MASK__n = 16;
#define parser_key_s_r_STATE_MASK__nd    16
typedef uint16 parser_key_s_r_STATE_MASK_t;

static const unsigned int parser_key_s_r_STATE_VALUE__n = 16;
#define parser_key_s_r_STATE_VALUE__nd    16
typedef uint16 parser_key_s_r_STATE_VALUE_t;




/* ../src/RegC.m3:221 */
typedef parser_key_s_r parser_key_s_rf[16];

typedef parser_key_s_r__addr parser_key_s_rf__addr[16];


void
parser_key_s_rf__init(
  parser_key_s_rf *p0,
  parser_key_s_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int parser_key_s_rf_PARSER_KEY_S__n = 16;
#define parser_key_s_rf_PARSER_KEY_S__nd    16



/* ../src/RegC.m3:221 */
typedef struct {
  parser_port_cfg_r PARSER_PORT_CFG[16];
  parser_csum_cfg_r PARSER_CSUM_CFG[16];
  parser_ip_r PARSER_IP;
  parser_im_r PARSER_IM;
  parser_key_w_rf PARSER_KEY_W[32];
  parser_key_s_rf PARSER_KEY_S[32];
  parser_ana_w_rf PARSER_ANA_W[32];
  parser_ana_s_rf PARSER_ANA_S[32];
  parser_exc_rf PARSER_EXC[32];
  parser_ext_rf PARSER_EXT[32];
  parser_ptype_tcam_rf PARSER_PTYPE_TCAM[2];
  parser_ptype_ram_rf PARSER_PTYPE_RAM[2];
  parser_extract_cfg_rf PARSER_EXTRACT_CFG[16];
  parser_counters_r PARSER_COUNTERS;
} mby_ppe_parser_map;

typedef struct {
  parser_port_cfg_r__addr PARSER_PORT_CFG[16];
  parser_csum_cfg_r__addr PARSER_CSUM_CFG[16];
  parser_ip_r__addr PARSER_IP;
  parser_im_r__addr PARSER_IM;
  parser_key_w_rf__addr PARSER_KEY_W[32];
  parser_key_s_rf__addr PARSER_KEY_S[32];
  parser_ana_w_rf__addr PARSER_ANA_W[32];
  parser_ana_s_rf__addr PARSER_ANA_S[32];
  parser_exc_rf__addr PARSER_EXC[32];
  parser_ext_rf__addr PARSER_EXT[32];
  parser_ptype_tcam_rf__addr PARSER_PTYPE_TCAM[2];
  parser_ptype_ram_rf__addr PARSER_PTYPE_RAM[2];
  parser_extract_cfg_rf__addr PARSER_EXTRACT_CFG[16];
  parser_counters_r__addr PARSER_COUNTERS;
} mby_ppe_parser_map__addr;


void
mby_ppe_parser_map__init(
  mby_ppe_parser_map *p0,
  mby_ppe_parser_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_parser_map_PARSER_PORT_CFG__n = 16;
#define mby_ppe_parser_map_PARSER_PORT_CFG__nd    16
typedef parser_port_cfg_r mby_ppe_parser_map_PARSER_PORT_CFG_t[16];

static const unsigned int mby_ppe_parser_map_PARSER_CSUM_CFG__n = 16;
#define mby_ppe_parser_map_PARSER_CSUM_CFG__nd    16
typedef parser_csum_cfg_r mby_ppe_parser_map_PARSER_CSUM_CFG_t[16];

typedef parser_ip_r mby_ppe_parser_map_PARSER_IP_t;

typedef parser_im_r mby_ppe_parser_map_PARSER_IM_t;

static const unsigned int mby_ppe_parser_map_PARSER_KEY_W__n = 32;
#define mby_ppe_parser_map_PARSER_KEY_W__nd    32
typedef parser_key_w_rf mby_ppe_parser_map_PARSER_KEY_W_t[32];

static const unsigned int mby_ppe_parser_map_PARSER_KEY_S__n = 32;
#define mby_ppe_parser_map_PARSER_KEY_S__nd    32
typedef parser_key_s_rf mby_ppe_parser_map_PARSER_KEY_S_t[32];

static const unsigned int mby_ppe_parser_map_PARSER_ANA_W__n = 32;
#define mby_ppe_parser_map_PARSER_ANA_W__nd    32
typedef parser_ana_w_rf mby_ppe_parser_map_PARSER_ANA_W_t[32];

static const unsigned int mby_ppe_parser_map_PARSER_ANA_S__n = 32;
#define mby_ppe_parser_map_PARSER_ANA_S__nd    32
typedef parser_ana_s_rf mby_ppe_parser_map_PARSER_ANA_S_t[32];

static const unsigned int mby_ppe_parser_map_PARSER_EXC__n = 32;
#define mby_ppe_parser_map_PARSER_EXC__nd    32
typedef parser_exc_rf mby_ppe_parser_map_PARSER_EXC_t[32];

static const unsigned int mby_ppe_parser_map_PARSER_EXT__n = 32;
#define mby_ppe_parser_map_PARSER_EXT__nd    32
typedef parser_ext_rf mby_ppe_parser_map_PARSER_EXT_t[32];

static const unsigned int mby_ppe_parser_map_PARSER_PTYPE_TCAM__n = 2;
#define mby_ppe_parser_map_PARSER_PTYPE_TCAM__nd    2
typedef parser_ptype_tcam_rf mby_ppe_parser_map_PARSER_PTYPE_TCAM_t[2];

static const unsigned int mby_ppe_parser_map_PARSER_PTYPE_RAM__n = 2;
#define mby_ppe_parser_map_PARSER_PTYPE_RAM__nd    2
typedef parser_ptype_ram_rf mby_ppe_parser_map_PARSER_PTYPE_RAM_t[2];

static const unsigned int mby_ppe_parser_map_PARSER_EXTRACT_CFG__n = 16;
#define mby_ppe_parser_map_PARSER_EXTRACT_CFG__nd    16
typedef parser_extract_cfg_rf mby_ppe_parser_map_PARSER_EXTRACT_CFG_t[16];

typedef parser_counters_r mby_ppe_parser_map_PARSER_COUNTERS_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint2 FORWARDING_ACTION;
  uint2 TRAP_ACTION;
  uint4 _RSVD0_;
  uint1 TC_ACTION;
  uint1 VLAN_ACTION;
  uint2 LEARNING_ACTION;
  uint1 RATE_LIMIT_ACTION;
  uint4 _RSVD1_;
  uint1 EGRESS_L2_DOMAIN_ACTION;
  uint1 EGRESS_L3_DOMAIN_ACTION;
  uint1 POLICER_ACTION;
  uint1 NO_MODIFY_ACTION;
  uint2 MIRRORING_ACTION0;
  uint2 MIRRORING_ACTION1;
  uint2 MIRRORING_ACTION2;
  uint2 MIRRORING_ACTION3;
  uint35 _RSVD2_;
} trigger_action_cfg_1_r;

typedef struct {
  uint2 *FORWARDING_ACTION;
  uint2 *TRAP_ACTION;
  uint4 *_RSVD0_;
  uint1 *TC_ACTION;
  uint1 *VLAN_ACTION;
  uint2 *LEARNING_ACTION;
  uint1 *RATE_LIMIT_ACTION;
  uint4 *_RSVD1_;
  uint1 *EGRESS_L2_DOMAIN_ACTION;
  uint1 *EGRESS_L3_DOMAIN_ACTION;
  uint1 *POLICER_ACTION;
  uint1 *NO_MODIFY_ACTION;
  uint2 *MIRRORING_ACTION0;
  uint2 *MIRRORING_ACTION1;
  uint2 *MIRRORING_ACTION2;
  uint2 *MIRRORING_ACTION3;
  uint35 *_RSVD2_;
} trigger_action_cfg_1_r__addr;


void
trigger_action_cfg_1_r__init(
  trigger_action_cfg_1_r *p0,
  trigger_action_cfg_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int trigger_action_cfg_1_r_FORWARDING_ACTION__n = 2;
#define trigger_action_cfg_1_r_FORWARDING_ACTION__nd    2
typedef uint2 trigger_action_cfg_1_r_FORWARDING_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_TRAP_ACTION__n = 2;
#define trigger_action_cfg_1_r_TRAP_ACTION__nd    2
typedef uint2 trigger_action_cfg_1_r_TRAP_ACTION_t;

static const unsigned int trigger_action_cfg_1_r__RSVD0___n = 4;
#define trigger_action_cfg_1_r__RSVD0___nd    4
typedef uint4 trigger_action_cfg_1_r__RSVD0__t;

static const unsigned int trigger_action_cfg_1_r_TC_ACTION__n = 1;
#define trigger_action_cfg_1_r_TC_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_TC_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_VLAN_ACTION__n = 1;
#define trigger_action_cfg_1_r_VLAN_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_VLAN_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_LEARNING_ACTION__n = 2;
#define trigger_action_cfg_1_r_LEARNING_ACTION__nd    2
typedef uint2 trigger_action_cfg_1_r_LEARNING_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_RATE_LIMIT_ACTION__n = 1;
#define trigger_action_cfg_1_r_RATE_LIMIT_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_RATE_LIMIT_ACTION_t;

static const unsigned int trigger_action_cfg_1_r__RSVD1___n = 4;
#define trigger_action_cfg_1_r__RSVD1___nd    4
typedef uint4 trigger_action_cfg_1_r__RSVD1__t;

static const unsigned int trigger_action_cfg_1_r_EGRESS_L2_DOMAIN_ACTION__n = 1;
#define trigger_action_cfg_1_r_EGRESS_L2_DOMAIN_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_EGRESS_L2_DOMAIN_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_EGRESS_L3_DOMAIN_ACTION__n = 1;
#define trigger_action_cfg_1_r_EGRESS_L3_DOMAIN_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_EGRESS_L3_DOMAIN_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_POLICER_ACTION__n = 1;
#define trigger_action_cfg_1_r_POLICER_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_POLICER_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_NO_MODIFY_ACTION__n = 1;
#define trigger_action_cfg_1_r_NO_MODIFY_ACTION__nd    1
typedef uint1 trigger_action_cfg_1_r_NO_MODIFY_ACTION_t;

static const unsigned int trigger_action_cfg_1_r_MIRRORING_ACTION0__n = 2;
#define trigger_action_cfg_1_r_MIRRORING_ACTION0__nd    2
typedef uint2 trigger_action_cfg_1_r_MIRRORING_ACTION0_t;

static const unsigned int trigger_action_cfg_1_r_MIRRORING_ACTION1__n = 2;
#define trigger_action_cfg_1_r_MIRRORING_ACTION1__nd    2
typedef uint2 trigger_action_cfg_1_r_MIRRORING_ACTION1_t;

static const unsigned int trigger_action_cfg_1_r_MIRRORING_ACTION2__n = 2;
#define trigger_action_cfg_1_r_MIRRORING_ACTION2__nd    2
typedef uint2 trigger_action_cfg_1_r_MIRRORING_ACTION2_t;

static const unsigned int trigger_action_cfg_1_r_MIRRORING_ACTION3__n = 2;
#define trigger_action_cfg_1_r_MIRRORING_ACTION3__nd    2
typedef uint2 trigger_action_cfg_1_r_MIRRORING_ACTION3_t;

static const unsigned int trigger_action_cfg_1_r__RSVD2___n = 35;
#define trigger_action_cfg_1_r__RSVD2___nd    35
typedef uint35 trigger_action_cfg_1_r__RSVD2__t;




/* ../src/RegC.m3:221 */
typedef struct {
  trigger_condition_cfg_r TRIGGER_CONDITION_CFG[96];
  trigger_condition_param_r TRIGGER_CONDITION_PARAM[96];
  trigger_condition_cgrp_r TRIGGER_CONDITION_CGRP[96];
  trigger_condition_glort_r TRIGGER_CONDITION_GLORT[96];
  trigger_condition_rx_r TRIGGER_CONDITION_RX[96];
  trigger_condition_amask_1_r TRIGGER_CONDITION_AMASK_1[96];
  trigger_condition_amask_2_r TRIGGER_CONDITION_AMASK_2[96];
  trigger_action_cfg_1_r TRIGGER_ACTION_CFG_1[96];
  trigger_action_cfg_2_r TRIGGER_ACTION_CFG_2[96];
  trigger_action_glort_r TRIGGER_ACTION_GLORT[96];
  trigger_action_mirror_r TRIGGER_ACTION_MIRROR[96];
  trigger_stats_r TRIGGER_STATS[96];
  trigger_direct_map_ctrl_r TRIGGER_DIRECT_MAP_CTRL;
  trigger_direct_map_ctx0_r TRIGGER_DIRECT_MAP_CTX0;
  trigger_direct_map_ctx1_r TRIGGER_DIRECT_MAP_CTX1;
  trigger_direct_map_ctx2_r TRIGGER_DIRECT_MAP_CTX2;
  trigger_direct_map_ctx3_r TRIGGER_DIRECT_MAP_CTX3;
  trigger_direct_map_ctx4_r TRIGGER_DIRECT_MAP_CTX4;
  trigger_direct_map_adm0_r TRIGGER_DIRECT_MAP_ADM0;
  trigger_direct_map_adm1_r TRIGGER_DIRECT_MAP_ADM1;
  trigger_direct_map_adm2_r TRIGGER_DIRECT_MAP_ADM2;
  trigger_direct_map_adm3_r TRIGGER_DIRECT_MAP_ADM3;
  trigger_direct_map_adm4_r TRIGGER_DIRECT_MAP_ADM4;
  trigger_direct_map_adr0_r TRIGGER_DIRECT_MAP_ADR0;
  trigger_direct_map_adr1_r TRIGGER_DIRECT_MAP_ADR1;
  trigger_direct_map_adr2_r TRIGGER_DIRECT_MAP_ADR2;
  trigger_direct_map_adr3_r TRIGGER_DIRECT_MAP_ADR3;
  trigger_direct_map_adr4_r TRIGGER_DIRECT_MAP_ADR4;
} mby_ppe_trig_apply_map;

typedef struct {
  trigger_condition_cfg_r__addr TRIGGER_CONDITION_CFG[96];
  trigger_condition_param_r__addr TRIGGER_CONDITION_PARAM[96];
  trigger_condition_cgrp_r__addr TRIGGER_CONDITION_CGRP[96];
  trigger_condition_glort_r__addr TRIGGER_CONDITION_GLORT[96];
  trigger_condition_rx_r__addr TRIGGER_CONDITION_RX[96];
  trigger_condition_amask_1_r__addr TRIGGER_CONDITION_AMASK_1[96];
  trigger_condition_amask_2_r__addr TRIGGER_CONDITION_AMASK_2[96];
  trigger_action_cfg_1_r__addr TRIGGER_ACTION_CFG_1[96];
  trigger_action_cfg_2_r__addr TRIGGER_ACTION_CFG_2[96];
  trigger_action_glort_r__addr TRIGGER_ACTION_GLORT[96];
  trigger_action_mirror_r__addr TRIGGER_ACTION_MIRROR[96];
  trigger_stats_r__addr TRIGGER_STATS[96];
  trigger_direct_map_ctrl_r__addr TRIGGER_DIRECT_MAP_CTRL;
  trigger_direct_map_ctx0_r__addr TRIGGER_DIRECT_MAP_CTX0;
  trigger_direct_map_ctx1_r__addr TRIGGER_DIRECT_MAP_CTX1;
  trigger_direct_map_ctx2_r__addr TRIGGER_DIRECT_MAP_CTX2;
  trigger_direct_map_ctx3_r__addr TRIGGER_DIRECT_MAP_CTX3;
  trigger_direct_map_ctx4_r__addr TRIGGER_DIRECT_MAP_CTX4;
  trigger_direct_map_adm0_r__addr TRIGGER_DIRECT_MAP_ADM0;
  trigger_direct_map_adm1_r__addr TRIGGER_DIRECT_MAP_ADM1;
  trigger_direct_map_adm2_r__addr TRIGGER_DIRECT_MAP_ADM2;
  trigger_direct_map_adm3_r__addr TRIGGER_DIRECT_MAP_ADM3;
  trigger_direct_map_adm4_r__addr TRIGGER_DIRECT_MAP_ADM4;
  trigger_direct_map_adr0_r__addr TRIGGER_DIRECT_MAP_ADR0;
  trigger_direct_map_adr1_r__addr TRIGGER_DIRECT_MAP_ADR1;
  trigger_direct_map_adr2_r__addr TRIGGER_DIRECT_MAP_ADR2;
  trigger_direct_map_adr3_r__addr TRIGGER_DIRECT_MAP_ADR3;
  trigger_direct_map_adr4_r__addr TRIGGER_DIRECT_MAP_ADR4;
} mby_ppe_trig_apply_map__addr;


void
mby_ppe_trig_apply_map__init(
  mby_ppe_trig_apply_map *p0,
  mby_ppe_trig_apply_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_CFG__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_CFG__nd    96
typedef trigger_condition_cfg_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_CFG_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_PARAM__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_PARAM__nd    96
typedef trigger_condition_param_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_PARAM_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_CGRP__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_CGRP__nd    96
typedef trigger_condition_cgrp_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_CGRP_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_GLORT__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_GLORT__nd    96
typedef trigger_condition_glort_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_GLORT_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_RX__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_RX__nd    96
typedef trigger_condition_rx_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_RX_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_AMASK_1__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_AMASK_1__nd    96
typedef trigger_condition_amask_1_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_AMASK_1_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_CONDITION_AMASK_2__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_CONDITION_AMASK_2__nd    96
typedef trigger_condition_amask_2_r mby_ppe_trig_apply_map_TRIGGER_CONDITION_AMASK_2_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_ACTION_CFG_1__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_ACTION_CFG_1__nd    96
typedef trigger_action_cfg_1_r mby_ppe_trig_apply_map_TRIGGER_ACTION_CFG_1_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_ACTION_CFG_2__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_ACTION_CFG_2__nd    96
typedef trigger_action_cfg_2_r mby_ppe_trig_apply_map_TRIGGER_ACTION_CFG_2_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_ACTION_GLORT__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_ACTION_GLORT__nd    96
typedef trigger_action_glort_r mby_ppe_trig_apply_map_TRIGGER_ACTION_GLORT_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_ACTION_MIRROR__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_ACTION_MIRROR__nd    96
typedef trigger_action_mirror_r mby_ppe_trig_apply_map_TRIGGER_ACTION_MIRROR_t[96];

static const unsigned int mby_ppe_trig_apply_map_TRIGGER_STATS__n = 96;
#define mby_ppe_trig_apply_map_TRIGGER_STATS__nd    96
typedef trigger_stats_r mby_ppe_trig_apply_map_TRIGGER_STATS_t[96];

typedef trigger_direct_map_ctrl_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_CTRL_t;

typedef trigger_direct_map_ctx0_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_CTX0_t;

typedef trigger_direct_map_ctx1_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_CTX1_t;

typedef trigger_direct_map_ctx2_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_CTX2_t;

typedef trigger_direct_map_ctx3_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_CTX3_t;

typedef trigger_direct_map_ctx4_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_CTX4_t;

typedef trigger_direct_map_adm0_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADM0_t;

typedef trigger_direct_map_adm1_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADM1_t;

typedef trigger_direct_map_adm2_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADM2_t;

typedef trigger_direct_map_adm3_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADM3_t;

typedef trigger_direct_map_adm4_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADM4_t;

typedef trigger_direct_map_adr0_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADR0_t;

typedef trigger_direct_map_adr1_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADR1_t;

typedef trigger_direct_map_adr2_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADR2_t;

typedef trigger_direct_map_adr3_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADR3_t;

typedef trigger_direct_map_adr4_r mby_ppe_trig_apply_map_TRIGGER_DIRECT_MAP_ADR4_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint4 IDX_SHIFT_RIGHT;
  uint4 IDX_MASK_LEN;
  uint4 BASE_SHIFT_RIGHT;
  uint4 BASE_MASK_LEN;
  uint16 OUTPUT_MASK;
  uint4 OUTPUT_SHIFT;
} mod_map_cfg_r;

typedef struct {
  uint4 *IDX_SHIFT_RIGHT;
  uint4 *IDX_MASK_LEN;
  uint4 *BASE_SHIFT_RIGHT;
  uint4 *BASE_MASK_LEN;
  uint16 *OUTPUT_MASK;
  uint4 *OUTPUT_SHIFT;
} mod_map_cfg_r__addr;


void
mod_map_cfg_r__init(
  mod_map_cfg_r *p0,
  mod_map_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mod_map_cfg_r_IDX_SHIFT_RIGHT__n = 4;
#define mod_map_cfg_r_IDX_SHIFT_RIGHT__nd    4
typedef uint4 mod_map_cfg_r_IDX_SHIFT_RIGHT_t;

static const unsigned int mod_map_cfg_r_IDX_MASK_LEN__n = 4;
#define mod_map_cfg_r_IDX_MASK_LEN__nd    4
typedef uint4 mod_map_cfg_r_IDX_MASK_LEN_t;

static const unsigned int mod_map_cfg_r_BASE_SHIFT_RIGHT__n = 4;
#define mod_map_cfg_r_BASE_SHIFT_RIGHT__nd    4
typedef uint4 mod_map_cfg_r_BASE_SHIFT_RIGHT_t;

static const unsigned int mod_map_cfg_r_BASE_MASK_LEN__n = 4;
#define mod_map_cfg_r_BASE_MASK_LEN__nd    4
typedef uint4 mod_map_cfg_r_BASE_MASK_LEN_t;

static const unsigned int mod_map_cfg_r_OUTPUT_MASK__n = 16;
#define mod_map_cfg_r_OUTPUT_MASK__nd    16
typedef uint16 mod_map_cfg_r_OUTPUT_MASK_t;

static const unsigned int mod_map_cfg_r_OUTPUT_SHIFT__n = 4;
#define mod_map_cfg_r_OUTPUT_SHIFT__nd    4
typedef uint4 mod_map_cfg_r_OUTPUT_SHIFT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mod_profile_command_rf MOD_PROFILE_COMMAND[256];
  mod_profile_field_rf MOD_PROFILE_FIELD[256];
  mod_map_dual_rf MOD_MAP_DUAL[1024];
  mod_map_rf MOD_MAP[4];
  mod_profile_group_r MOD_PROFILE_GROUP[256];
  mod_mirror_profile_table0_r MOD_MIRROR_PROFILE_TABLE0[64];
  mod_mirror_profile_table1_r MOD_MIRROR_PROFILE_TABLE1[64];
  mod_mirror_profile_table2_r MOD_MIRROR_PROFILE_TABLE2[64];
  mod_map_cfg_r MOD_MAP_CFG[4];
  mod_map_dual_cfg_r MOD_MAP_DUAL_CFG;
  mod_csum_cfg1_r MOD_CSUM_CFG1;
  mod_csum_cfg2_r MOD_CSUM_CFG2;
  mod_csum_en_cfg_r MOD_CSUM_EN_CFG;
} mby_ppe_modify_map;

typedef struct {
  mod_profile_command_rf__addr MOD_PROFILE_COMMAND[256];
  mod_profile_field_rf__addr MOD_PROFILE_FIELD[256];
  mod_map_dual_rf__addr MOD_MAP_DUAL[1024];
  mod_map_rf__addr MOD_MAP[4];
  mod_profile_group_r__addr MOD_PROFILE_GROUP[256];
  mod_mirror_profile_table0_r__addr MOD_MIRROR_PROFILE_TABLE0[64];
  mod_mirror_profile_table1_r__addr MOD_MIRROR_PROFILE_TABLE1[64];
  mod_mirror_profile_table2_r__addr MOD_MIRROR_PROFILE_TABLE2[64];
  mod_map_cfg_r__addr MOD_MAP_CFG[4];
  mod_map_dual_cfg_r__addr MOD_MAP_DUAL_CFG;
  mod_csum_cfg1_r__addr MOD_CSUM_CFG1;
  mod_csum_cfg2_r__addr MOD_CSUM_CFG2;
  mod_csum_en_cfg_r__addr MOD_CSUM_EN_CFG;
} mby_ppe_modify_map__addr;


void
mby_ppe_modify_map__init(
  mby_ppe_modify_map *p0,
  mby_ppe_modify_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_modify_map_MOD_PROFILE_COMMAND__n = 256;
#define mby_ppe_modify_map_MOD_PROFILE_COMMAND__nd    256
typedef mod_profile_command_rf mby_ppe_modify_map_MOD_PROFILE_COMMAND_t[256];

static const unsigned int mby_ppe_modify_map_MOD_PROFILE_FIELD__n = 256;
#define mby_ppe_modify_map_MOD_PROFILE_FIELD__nd    256
typedef mod_profile_field_rf mby_ppe_modify_map_MOD_PROFILE_FIELD_t[256];

static const unsigned int mby_ppe_modify_map_MOD_MAP_DUAL__n = 1024;
#define mby_ppe_modify_map_MOD_MAP_DUAL__nd    1024
typedef mod_map_dual_rf mby_ppe_modify_map_MOD_MAP_DUAL_t[1024];

static const unsigned int mby_ppe_modify_map_MOD_MAP__n = 4;
#define mby_ppe_modify_map_MOD_MAP__nd    4
typedef mod_map_rf mby_ppe_modify_map_MOD_MAP_t[4];

static const unsigned int mby_ppe_modify_map_MOD_PROFILE_GROUP__n = 256;
#define mby_ppe_modify_map_MOD_PROFILE_GROUP__nd    256
typedef mod_profile_group_r mby_ppe_modify_map_MOD_PROFILE_GROUP_t[256];

static const unsigned int mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE0__n = 64;
#define mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE0__nd    64
typedef mod_mirror_profile_table0_r mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE0_t[64];

static const unsigned int mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE1__n = 64;
#define mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE1__nd    64
typedef mod_mirror_profile_table1_r mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE1_t[64];

static const unsigned int mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE2__n = 64;
#define mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE2__nd    64
typedef mod_mirror_profile_table2_r mby_ppe_modify_map_MOD_MIRROR_PROFILE_TABLE2_t[64];

static const unsigned int mby_ppe_modify_map_MOD_MAP_CFG__n = 4;
#define mby_ppe_modify_map_MOD_MAP_CFG__nd    4
typedef mod_map_cfg_r mby_ppe_modify_map_MOD_MAP_CFG_t[4];

typedef mod_map_dual_cfg_r mby_ppe_modify_map_MOD_MAP_DUAL_CFG_t;

typedef mod_csum_cfg1_r mby_ppe_modify_map_MOD_CSUM_CFG1_t;

typedef mod_csum_cfg2_r mby_ppe_modify_map_MOD_CSUM_CFG2_t;

typedef mod_csum_en_cfg_r mby_ppe_modify_map_MOD_CSUM_EN_CFG_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mby_ppe_modify_map modify;
} mby_ppe_tx_top_map;

typedef struct {
  mby_ppe_modify_map__addr modify;
} mby_ppe_tx_top_map__addr;


void
mby_ppe_tx_top_map__init(
  mby_ppe_tx_top_map *p0,
  mby_ppe_tx_top_map__addr *p1,
  void (*f)(void *, int)
);

typedef mby_ppe_modify_map mby_ppe_tx_top_map_modify_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 PORT;
} tx_pb_port_cfg_r;

typedef struct {
  uint32 *PORT;
} tx_pb_port_cfg_r__addr;


void
tx_pb_port_cfg_r__init(
  tx_pb_port_cfg_r *p0,
  tx_pb_port_cfg_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int tx_pb_port_cfg_r_PORT__n = 32;
#define tx_pb_port_cfg_r_PORT__nd    32
typedef uint32 tx_pb_port_cfg_r_PORT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  tx_pb_port_cfg_r TX_PB_PORT_CFG;
  tx_pb_prefetch_r TX_PB_PREFETCH[16];
} mby_tx_pb_map;

typedef struct {
  tx_pb_port_cfg_r__addr TX_PB_PORT_CFG;
  tx_pb_prefetch_r__addr TX_PB_PREFETCH[16];
} mby_tx_pb_map__addr;


void
mby_tx_pb_map__init(
  mby_tx_pb_map *p0,
  mby_tx_pb_map__addr *p1,
  void (*f)(void *, int)
);

typedef tx_pb_port_cfg_r mby_tx_pb_map_TX_PB_PORT_CFG_t;

static const unsigned int mby_tx_pb_map_TX_PB_PREFETCH__n = 16;
#define mby_tx_pb_map_TX_PB_PREFETCH__nd    16
typedef tx_pb_prefetch_r mby_tx_pb_map_TX_PB_PREFETCH_t[16];




/* ../src/RegC.m3:221 */
typedef egress_vid_table_r egress_vid_table_rf[5];

typedef egress_vid_table_r__addr egress_vid_table_rf__addr[5];


void
egress_vid_table_rf__init(
  egress_vid_table_rf *p0,
  egress_vid_table_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int egress_vid_table_rf_EGRESS_VID_TABLE__n = 5;
#define egress_vid_table_rf_EGRESS_VID_TABLE__nd    5



/* ../src/RegC.m3:221 */
typedef struct {
  egress_vid_table_rf EGRESS_VID_TABLE[4096];
  egress_vid_cfg_r EGRESS_VID_CFG[4096];
  ingress_mst_table_r INGRESS_MST_TABLE[4096];
  egress_mst_table_rf EGRESS_MST_TABLE[4096];
  glort_dest_table_r0 GLORT_DEST_TABLE0[4096];
  glort_dest_table_r1 GLORT_DEST_TABLE1[4096];
  glort_dest_table_r2 GLORT_DEST_TABLE2[4096];
  glort_dest_table_r3 GLORT_DEST_TABLE3[4096];
  glort_dest_table_r4 GLORT_DEST_TABLE4[4096];
  glort_ram_r GLORT_RAM[512];
  glort_cam_r GLORT_CAM[512];
  cgrp_used_table_r CGRP_USED_TABLE[65];
} mby_ppe_mst_glort_map;

typedef struct {
  egress_vid_table_rf__addr EGRESS_VID_TABLE[4096];
  egress_vid_cfg_r__addr EGRESS_VID_CFG[4096];
  ingress_mst_table_r__addr INGRESS_MST_TABLE[4096];
  egress_mst_table_rf__addr EGRESS_MST_TABLE[4096];
  glort_dest_table_r0__addr GLORT_DEST_TABLE0[4096];
  glort_dest_table_r1__addr GLORT_DEST_TABLE1[4096];
  glort_dest_table_r2__addr GLORT_DEST_TABLE2[4096];
  glort_dest_table_r3__addr GLORT_DEST_TABLE3[4096];
  glort_dest_table_r4__addr GLORT_DEST_TABLE4[4096];
  glort_ram_r__addr GLORT_RAM[512];
  glort_cam_r__addr GLORT_CAM[512];
  cgrp_used_table_r__addr CGRP_USED_TABLE[65];
} mby_ppe_mst_glort_map__addr;


void
mby_ppe_mst_glort_map__init(
  mby_ppe_mst_glort_map *p0,
  mby_ppe_mst_glort_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_mst_glort_map_EGRESS_VID_TABLE__n = 4096;
#define mby_ppe_mst_glort_map_EGRESS_VID_TABLE__nd    4096
typedef egress_vid_table_rf mby_ppe_mst_glort_map_EGRESS_VID_TABLE_t[4096];

static const unsigned int mby_ppe_mst_glort_map_EGRESS_VID_CFG__n = 4096;
#define mby_ppe_mst_glort_map_EGRESS_VID_CFG__nd    4096
typedef egress_vid_cfg_r mby_ppe_mst_glort_map_EGRESS_VID_CFG_t[4096];

static const unsigned int mby_ppe_mst_glort_map_INGRESS_MST_TABLE__n = 4096;
#define mby_ppe_mst_glort_map_INGRESS_MST_TABLE__nd    4096
typedef ingress_mst_table_r mby_ppe_mst_glort_map_INGRESS_MST_TABLE_t[4096];

static const unsigned int mby_ppe_mst_glort_map_EGRESS_MST_TABLE__n = 4096;
#define mby_ppe_mst_glort_map_EGRESS_MST_TABLE__nd    4096
typedef egress_mst_table_rf mby_ppe_mst_glort_map_EGRESS_MST_TABLE_t[4096];

static const unsigned int mby_ppe_mst_glort_map_GLORT_DEST_TABLE0__n = 4096;
#define mby_ppe_mst_glort_map_GLORT_DEST_TABLE0__nd    4096
typedef glort_dest_table_r0 mby_ppe_mst_glort_map_GLORT_DEST_TABLE0_t[4096];

static const unsigned int mby_ppe_mst_glort_map_GLORT_DEST_TABLE1__n = 4096;
#define mby_ppe_mst_glort_map_GLORT_DEST_TABLE1__nd    4096
typedef glort_dest_table_r1 mby_ppe_mst_glort_map_GLORT_DEST_TABLE1_t[4096];

static const unsigned int mby_ppe_mst_glort_map_GLORT_DEST_TABLE2__n = 4096;
#define mby_ppe_mst_glort_map_GLORT_DEST_TABLE2__nd    4096
typedef glort_dest_table_r2 mby_ppe_mst_glort_map_GLORT_DEST_TABLE2_t[4096];

static const unsigned int mby_ppe_mst_glort_map_GLORT_DEST_TABLE3__n = 4096;
#define mby_ppe_mst_glort_map_GLORT_DEST_TABLE3__nd    4096
typedef glort_dest_table_r3 mby_ppe_mst_glort_map_GLORT_DEST_TABLE3_t[4096];

static const unsigned int mby_ppe_mst_glort_map_GLORT_DEST_TABLE4__n = 4096;
#define mby_ppe_mst_glort_map_GLORT_DEST_TABLE4__nd    4096
typedef glort_dest_table_r4 mby_ppe_mst_glort_map_GLORT_DEST_TABLE4_t[4096];

static const unsigned int mby_ppe_mst_glort_map_GLORT_RAM__n = 512;
#define mby_ppe_mst_glort_map_GLORT_RAM__nd    512
typedef glort_ram_r mby_ppe_mst_glort_map_GLORT_RAM_t[512];

static const unsigned int mby_ppe_mst_glort_map_GLORT_CAM__n = 512;
#define mby_ppe_mst_glort_map_GLORT_CAM__nd    512
typedef glort_cam_r mby_ppe_mst_glort_map_GLORT_CAM_t[512];

static const unsigned int mby_ppe_mst_glort_map_CGRP_USED_TABLE__n = 65;
#define mby_ppe_mst_glort_map_CGRP_USED_TABLE__nd    65
typedef cgrp_used_table_r mby_ppe_mst_glort_map_CGRP_USED_TABLE_t[65];




/* ../src/RegC.m3:179 */
typedef struct {
  uint64 DESTINATION_MASK;
} fwd_port_cfg_2_3_r;

typedef struct {
  uint64 *DESTINATION_MASK;
} fwd_port_cfg_2_3_r__addr;


void
fwd_port_cfg_2_3_r__init(
  fwd_port_cfg_2_3_r *p0,
  fwd_port_cfg_2_3_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_port_cfg_2_3_r_DESTINATION_MASK__n = 64;
#define fwd_port_cfg_2_3_r_DESTINATION_MASK__nd    64
typedef uint64 fwd_port_cfg_2_3_r_DESTINATION_MASK_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint60 KEY_PAIRS;
} entropy_hash_sym16_r;

typedef struct {
  uint60 *KEY_PAIRS;
} entropy_hash_sym16_r__addr;


void
entropy_hash_sym16_r__init(
  entropy_hash_sym16_r *p0,
  entropy_hash_sym16_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_sym16_r_KEY_PAIRS__n = 60;
#define entropy_hash_sym16_r_KEY_PAIRS__nd    60
typedef uint60 entropy_hash_sym16_r_KEY_PAIRS_t;




/* ../src/RegC.m3:221 */
typedef entropy_hash_sym16_r entropy_hash_sym16_rf[4];

typedef entropy_hash_sym16_r__addr entropy_hash_sym16_rf__addr[4];


void
entropy_hash_sym16_rf__init(
  entropy_hash_sym16_rf *p0,
  entropy_hash_sym16_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int entropy_hash_sym16_rf_ENTROPY_HASH_SYM16__n = 4;
#define entropy_hash_sym16_rf_ENTROPY_HASH_SYM16__nd    4



/* ../src/RegC.m3:221 */
typedef struct {
  entropy_hash_cfg0_rf ENTROPY_HASH_CFG0[3];
  entropy_hash_cfg1_rf ENTROPY_HASH_CFG1[3];
  entropy_hash_cfg2_rf ENTROPY_HASH_CFG2[3];
  entropy_meta_cfg_r ENTROPY_META_CFG[64];
  entropy_hash_sym8_rf ENTROPY_HASH_SYM8[3];
  entropy_hash_sym16_rf ENTROPY_HASH_SYM16[3];
  entropy_hash_sym32_rf ENTROPY_HASH_SYM32[3];
  entropy_hash_key_mask_rf ENTROPY_HASH_KEY_MASK[3];
  entropy_fwd_hashing_cfg_r ENTROPY_FWD_HASHING_CFG[64];
} mby_ppe_entropy_map;

typedef struct {
  entropy_hash_cfg0_rf__addr ENTROPY_HASH_CFG0[3];
  entropy_hash_cfg1_rf__addr ENTROPY_HASH_CFG1[3];
  entropy_hash_cfg2_rf__addr ENTROPY_HASH_CFG2[3];
  entropy_meta_cfg_r__addr ENTROPY_META_CFG[64];
  entropy_hash_sym8_rf__addr ENTROPY_HASH_SYM8[3];
  entropy_hash_sym16_rf__addr ENTROPY_HASH_SYM16[3];
  entropy_hash_sym32_rf__addr ENTROPY_HASH_SYM32[3];
  entropy_hash_key_mask_rf__addr ENTROPY_HASH_KEY_MASK[3];
  entropy_fwd_hashing_cfg_r__addr ENTROPY_FWD_HASHING_CFG[64];
} mby_ppe_entropy_map__addr;


void
mby_ppe_entropy_map__init(
  mby_ppe_entropy_map *p0,
  mby_ppe_entropy_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_CFG0__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_CFG0__nd    3
typedef entropy_hash_cfg0_rf mby_ppe_entropy_map_ENTROPY_HASH_CFG0_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_CFG1__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_CFG1__nd    3
typedef entropy_hash_cfg1_rf mby_ppe_entropy_map_ENTROPY_HASH_CFG1_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_CFG2__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_CFG2__nd    3
typedef entropy_hash_cfg2_rf mby_ppe_entropy_map_ENTROPY_HASH_CFG2_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_META_CFG__n = 64;
#define mby_ppe_entropy_map_ENTROPY_META_CFG__nd    64
typedef entropy_meta_cfg_r mby_ppe_entropy_map_ENTROPY_META_CFG_t[64];

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_SYM8__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_SYM8__nd    3
typedef entropy_hash_sym8_rf mby_ppe_entropy_map_ENTROPY_HASH_SYM8_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_SYM16__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_SYM16__nd    3
typedef entropy_hash_sym16_rf mby_ppe_entropy_map_ENTROPY_HASH_SYM16_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_SYM32__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_SYM32__nd    3
typedef entropy_hash_sym32_rf mby_ppe_entropy_map_ENTROPY_HASH_SYM32_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_HASH_KEY_MASK__n = 3;
#define mby_ppe_entropy_map_ENTROPY_HASH_KEY_MASK__nd    3
typedef entropy_hash_key_mask_rf mby_ppe_entropy_map_ENTROPY_HASH_KEY_MASK_t[3];

static const unsigned int mby_ppe_entropy_map_ENTROPY_FWD_HASHING_CFG__n = 64;
#define mby_ppe_entropy_map_ENTROPY_FWD_HASHING_CFG__nd    64
typedef entropy_fwd_hashing_cfg_r mby_ppe_entropy_map_ENTROPY_FWD_HASHING_CFG_t[64];




/* ../src/RegC.m3:221 */
typedef cm_aqm_ewma_cfg_r cm_aqm_ewma_cfg_rf[2];

typedef cm_aqm_ewma_cfg_r__addr cm_aqm_ewma_cfg_rf__addr[2];


void
cm_aqm_ewma_cfg_rf__init(
  cm_aqm_ewma_cfg_rf *p0,
  cm_aqm_ewma_cfg_rf__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_aqm_ewma_cfg_rf_CM_AQM_EWMA_CFG__n = 2;
#define cm_aqm_ewma_cfg_rf_CM_AQM_EWMA_CFG__nd    2



/* ../src/RegC.m3:179 */
typedef struct {
  uint16 COUNT;
} cm_mcast_epoch_usage_r;

typedef struct {
  uint16 *COUNT;
} cm_mcast_epoch_usage_r__addr;


void
cm_mcast_epoch_usage_r__init(
  cm_mcast_epoch_usage_r *p0,
  cm_mcast_epoch_usage_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int cm_mcast_epoch_usage_r_COUNT__n = 16;
#define cm_mcast_epoch_usage_r_COUNT__nd    16
typedef uint16 cm_mcast_epoch_usage_r_COUNT_t;




/* ../src/RegC.m3:221 */
typedef struct {
  cm_tx_tc_private_wm_rf CM_TX_TC_PRIVATE_WM[64];
  cm_tx_tc_hog_wm_rf CM_TX_TC_HOG_WM[64];
  cm_rx_smp_private_wm_rf CM_RX_SMP_PRIVATE_WM[32];
  cm_rx_smp_hog_wm_rf CM_RX_SMP_HOG_WM[32];
  cm_rx_smp_pause_wm_rf CM_RX_SMP_PAUSE_WM[32];
  cm_shared_wm_r CM_SHARED_WM[8];
  cm_softdrop_wm_r CM_SOFTDROP_WM[8];
  cm_shared_smp_pause_wm_r CM_SHARED_SMP_PAUSE_WM[2];
  cm_global_wm_r CM_GLOBAL_WM;
  cm_pause_phys_port_cfg_r CM_PAUSE_PHYS_PORT_CFG[32];
  cm_force_pause_cfg_r CM_FORCE_PAUSE_CFG;
  cm_aqm_ewma_cfg_rf CM_AQM_EWMA_CFG[64];
  cm_aqm_dctcp_cfg_rf CM_AQM_DCTCP_CFG[64];
  cm_global_cfg_r CM_GLOBAL_CFG;
  cm_shared_smp_pause_cfg_r CM_SHARED_SMP_PAUSE_CFG[2];
  cm_sweeper_tc_to_smp_r CM_SWEEPER_TC_TO_SMP;
  cm_global_usage_r CM_GLOBAL_USAGE;
  cm_global_usage_max_r CM_GLOBAL_USAGE_MAX;
  cm_mcast_epoch_usage_r CM_MCAST_EPOCH_USAGE[2];
  cm_shared_smp_usage_r CM_SHARED_SMP_USAGE[2];
  cm_smp_usage_r CM_SMP_USAGE[2];
  cm_rx_smp_usage_rf CM_RX_SMP_USAGE[32];
  cm_rx_smp_usage_max_rf CM_RX_SMP_USAGE_MAX[4];
  cm_rx_smp_usage_max_ctrl_r CM_RX_SMP_USAGE_MAX_CTRL;
  cm_pause_gen_state_r CM_PAUSE_GEN_STATE[32];
  cm_tx_tc_usage_rf CM_TX_TC_USAGE[64];
  cm_tx_ewma_rf CM_TX_EWMA[64];
} mby_ppe_cm_usage_map;

typedef struct {
  cm_tx_tc_private_wm_rf__addr CM_TX_TC_PRIVATE_WM[64];
  cm_tx_tc_hog_wm_rf__addr CM_TX_TC_HOG_WM[64];
  cm_rx_smp_private_wm_rf__addr CM_RX_SMP_PRIVATE_WM[32];
  cm_rx_smp_hog_wm_rf__addr CM_RX_SMP_HOG_WM[32];
  cm_rx_smp_pause_wm_rf__addr CM_RX_SMP_PAUSE_WM[32];
  cm_shared_wm_r__addr CM_SHARED_WM[8];
  cm_softdrop_wm_r__addr CM_SOFTDROP_WM[8];
  cm_shared_smp_pause_wm_r__addr CM_SHARED_SMP_PAUSE_WM[2];
  cm_global_wm_r__addr CM_GLOBAL_WM;
  cm_pause_phys_port_cfg_r__addr CM_PAUSE_PHYS_PORT_CFG[32];
  cm_force_pause_cfg_r__addr CM_FORCE_PAUSE_CFG;
  cm_aqm_ewma_cfg_rf__addr CM_AQM_EWMA_CFG[64];
  cm_aqm_dctcp_cfg_rf__addr CM_AQM_DCTCP_CFG[64];
  cm_global_cfg_r__addr CM_GLOBAL_CFG;
  cm_shared_smp_pause_cfg_r__addr CM_SHARED_SMP_PAUSE_CFG[2];
  cm_sweeper_tc_to_smp_r__addr CM_SWEEPER_TC_TO_SMP;
  cm_global_usage_r__addr CM_GLOBAL_USAGE;
  cm_global_usage_max_r__addr CM_GLOBAL_USAGE_MAX;
  cm_mcast_epoch_usage_r__addr CM_MCAST_EPOCH_USAGE[2];
  cm_shared_smp_usage_r__addr CM_SHARED_SMP_USAGE[2];
  cm_smp_usage_r__addr CM_SMP_USAGE[2];
  cm_rx_smp_usage_rf__addr CM_RX_SMP_USAGE[32];
  cm_rx_smp_usage_max_rf__addr CM_RX_SMP_USAGE_MAX[4];
  cm_rx_smp_usage_max_ctrl_r__addr CM_RX_SMP_USAGE_MAX_CTRL;
  cm_pause_gen_state_r__addr CM_PAUSE_GEN_STATE[32];
  cm_tx_tc_usage_rf__addr CM_TX_TC_USAGE[64];
  cm_tx_ewma_rf__addr CM_TX_EWMA[64];
} mby_ppe_cm_usage_map__addr;


void
mby_ppe_cm_usage_map__init(
  mby_ppe_cm_usage_map *p0,
  mby_ppe_cm_usage_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_cm_usage_map_CM_TX_TC_PRIVATE_WM__n = 64;
#define mby_ppe_cm_usage_map_CM_TX_TC_PRIVATE_WM__nd    64
typedef cm_tx_tc_private_wm_rf mby_ppe_cm_usage_map_CM_TX_TC_PRIVATE_WM_t[64];

static const unsigned int mby_ppe_cm_usage_map_CM_TX_TC_HOG_WM__n = 64;
#define mby_ppe_cm_usage_map_CM_TX_TC_HOG_WM__nd    64
typedef cm_tx_tc_hog_wm_rf mby_ppe_cm_usage_map_CM_TX_TC_HOG_WM_t[64];

static const unsigned int mby_ppe_cm_usage_map_CM_RX_SMP_PRIVATE_WM__n = 32;
#define mby_ppe_cm_usage_map_CM_RX_SMP_PRIVATE_WM__nd    32
typedef cm_rx_smp_private_wm_rf mby_ppe_cm_usage_map_CM_RX_SMP_PRIVATE_WM_t[32];

static const unsigned int mby_ppe_cm_usage_map_CM_RX_SMP_HOG_WM__n = 32;
#define mby_ppe_cm_usage_map_CM_RX_SMP_HOG_WM__nd    32
typedef cm_rx_smp_hog_wm_rf mby_ppe_cm_usage_map_CM_RX_SMP_HOG_WM_t[32];

static const unsigned int mby_ppe_cm_usage_map_CM_RX_SMP_PAUSE_WM__n = 32;
#define mby_ppe_cm_usage_map_CM_RX_SMP_PAUSE_WM__nd    32
typedef cm_rx_smp_pause_wm_rf mby_ppe_cm_usage_map_CM_RX_SMP_PAUSE_WM_t[32];

static const unsigned int mby_ppe_cm_usage_map_CM_SHARED_WM__n = 8;
#define mby_ppe_cm_usage_map_CM_SHARED_WM__nd    8
typedef cm_shared_wm_r mby_ppe_cm_usage_map_CM_SHARED_WM_t[8];

static const unsigned int mby_ppe_cm_usage_map_CM_SOFTDROP_WM__n = 8;
#define mby_ppe_cm_usage_map_CM_SOFTDROP_WM__nd    8
typedef cm_softdrop_wm_r mby_ppe_cm_usage_map_CM_SOFTDROP_WM_t[8];

static const unsigned int mby_ppe_cm_usage_map_CM_SHARED_SMP_PAUSE_WM__n = 2;
#define mby_ppe_cm_usage_map_CM_SHARED_SMP_PAUSE_WM__nd    2
typedef cm_shared_smp_pause_wm_r mby_ppe_cm_usage_map_CM_SHARED_SMP_PAUSE_WM_t[2];

typedef cm_global_wm_r mby_ppe_cm_usage_map_CM_GLOBAL_WM_t;

static const unsigned int mby_ppe_cm_usage_map_CM_PAUSE_PHYS_PORT_CFG__n = 32;
#define mby_ppe_cm_usage_map_CM_PAUSE_PHYS_PORT_CFG__nd    32
typedef cm_pause_phys_port_cfg_r mby_ppe_cm_usage_map_CM_PAUSE_PHYS_PORT_CFG_t[32];

typedef cm_force_pause_cfg_r mby_ppe_cm_usage_map_CM_FORCE_PAUSE_CFG_t;

static const unsigned int mby_ppe_cm_usage_map_CM_AQM_EWMA_CFG__n = 64;
#define mby_ppe_cm_usage_map_CM_AQM_EWMA_CFG__nd    64
typedef cm_aqm_ewma_cfg_rf mby_ppe_cm_usage_map_CM_AQM_EWMA_CFG_t[64];

static const unsigned int mby_ppe_cm_usage_map_CM_AQM_DCTCP_CFG__n = 64;
#define mby_ppe_cm_usage_map_CM_AQM_DCTCP_CFG__nd    64
typedef cm_aqm_dctcp_cfg_rf mby_ppe_cm_usage_map_CM_AQM_DCTCP_CFG_t[64];

typedef cm_global_cfg_r mby_ppe_cm_usage_map_CM_GLOBAL_CFG_t;

static const unsigned int mby_ppe_cm_usage_map_CM_SHARED_SMP_PAUSE_CFG__n = 2;
#define mby_ppe_cm_usage_map_CM_SHARED_SMP_PAUSE_CFG__nd    2
typedef cm_shared_smp_pause_cfg_r mby_ppe_cm_usage_map_CM_SHARED_SMP_PAUSE_CFG_t[2];

typedef cm_sweeper_tc_to_smp_r mby_ppe_cm_usage_map_CM_SWEEPER_TC_TO_SMP_t;

typedef cm_global_usage_r mby_ppe_cm_usage_map_CM_GLOBAL_USAGE_t;

typedef cm_global_usage_max_r mby_ppe_cm_usage_map_CM_GLOBAL_USAGE_MAX_t;

static const unsigned int mby_ppe_cm_usage_map_CM_MCAST_EPOCH_USAGE__n = 2;
#define mby_ppe_cm_usage_map_CM_MCAST_EPOCH_USAGE__nd    2
typedef cm_mcast_epoch_usage_r mby_ppe_cm_usage_map_CM_MCAST_EPOCH_USAGE_t[2];

static const unsigned int mby_ppe_cm_usage_map_CM_SHARED_SMP_USAGE__n = 2;
#define mby_ppe_cm_usage_map_CM_SHARED_SMP_USAGE__nd    2
typedef cm_shared_smp_usage_r mby_ppe_cm_usage_map_CM_SHARED_SMP_USAGE_t[2];

static const unsigned int mby_ppe_cm_usage_map_CM_SMP_USAGE__n = 2;
#define mby_ppe_cm_usage_map_CM_SMP_USAGE__nd    2
typedef cm_smp_usage_r mby_ppe_cm_usage_map_CM_SMP_USAGE_t[2];

static const unsigned int mby_ppe_cm_usage_map_CM_RX_SMP_USAGE__n = 32;
#define mby_ppe_cm_usage_map_CM_RX_SMP_USAGE__nd    32
typedef cm_rx_smp_usage_rf mby_ppe_cm_usage_map_CM_RX_SMP_USAGE_t[32];

static const unsigned int mby_ppe_cm_usage_map_CM_RX_SMP_USAGE_MAX__n = 4;
#define mby_ppe_cm_usage_map_CM_RX_SMP_USAGE_MAX__nd    4
typedef cm_rx_smp_usage_max_rf mby_ppe_cm_usage_map_CM_RX_SMP_USAGE_MAX_t[4];

typedef cm_rx_smp_usage_max_ctrl_r mby_ppe_cm_usage_map_CM_RX_SMP_USAGE_MAX_CTRL_t;

static const unsigned int mby_ppe_cm_usage_map_CM_PAUSE_GEN_STATE__n = 32;
#define mby_ppe_cm_usage_map_CM_PAUSE_GEN_STATE__nd    32
typedef cm_pause_gen_state_r mby_ppe_cm_usage_map_CM_PAUSE_GEN_STATE_t[32];

static const unsigned int mby_ppe_cm_usage_map_CM_TX_TC_USAGE__n = 64;
#define mby_ppe_cm_usage_map_CM_TX_TC_USAGE__nd    64
typedef cm_tx_tc_usage_rf mby_ppe_cm_usage_map_CM_TX_TC_USAGE_t[64];

static const unsigned int mby_ppe_cm_usage_map_CM_TX_EWMA__n = 64;
#define mby_ppe_cm_usage_map_CM_TX_EWMA__nd    64
typedef cm_tx_ewma_rf mby_ppe_cm_usage_map_CM_TX_EWMA_t[64];




/* ../src/RegC.m3:179 */
typedef struct {
  uint1 TRAP_MTU_VIOLATIONS;
  uint1 ENABLE_TRAP_PLUS_LOG;
  uint1 DROP_INVALID_SMAC;
  uint1 DROP_MAC_CTRL_ETHERTYPE;
  uint1 STORE_TRAP_ACTION;
} fwd_sys_cfg_1_r;

typedef struct {
  uint1 *TRAP_MTU_VIOLATIONS;
  uint1 *ENABLE_TRAP_PLUS_LOG;
  uint1 *DROP_INVALID_SMAC;
  uint1 *DROP_MAC_CTRL_ETHERTYPE;
  uint1 *STORE_TRAP_ACTION;
} fwd_sys_cfg_1_r__addr;


void
fwd_sys_cfg_1_r__init(
  fwd_sys_cfg_1_r *p0,
  fwd_sys_cfg_1_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int fwd_sys_cfg_1_r_TRAP_MTU_VIOLATIONS__n = 1;
#define fwd_sys_cfg_1_r_TRAP_MTU_VIOLATIONS__nd    1
typedef uint1 fwd_sys_cfg_1_r_TRAP_MTU_VIOLATIONS_t;

static const unsigned int fwd_sys_cfg_1_r_ENABLE_TRAP_PLUS_LOG__n = 1;
#define fwd_sys_cfg_1_r_ENABLE_TRAP_PLUS_LOG__nd    1
typedef uint1 fwd_sys_cfg_1_r_ENABLE_TRAP_PLUS_LOG_t;

static const unsigned int fwd_sys_cfg_1_r_DROP_INVALID_SMAC__n = 1;
#define fwd_sys_cfg_1_r_DROP_INVALID_SMAC__nd    1
typedef uint1 fwd_sys_cfg_1_r_DROP_INVALID_SMAC_t;

static const unsigned int fwd_sys_cfg_1_r_DROP_MAC_CTRL_ETHERTYPE__n = 1;
#define fwd_sys_cfg_1_r_DROP_MAC_CTRL_ETHERTYPE__nd    1
typedef uint1 fwd_sys_cfg_1_r_DROP_MAC_CTRL_ETHERTYPE_t;

static const unsigned int fwd_sys_cfg_1_r_STORE_TRAP_ACTION__n = 1;
#define fwd_sys_cfg_1_r_STORE_TRAP_ACTION__nd    1
typedef uint1 fwd_sys_cfg_1_r_STORE_TRAP_ACTION_t;




/* ../src/RegC.m3:221 */
typedef struct {
  fwd_port_cfg_2_0_r FWD_PORT_CFG_2_0[256];
  fwd_port_cfg_2_1_r FWD_PORT_CFG_2_1[256];
  fwd_port_cfg_2_2_r FWD_PORT_CFG_2_2[256];
  fwd_port_cfg_2_3_r FWD_PORT_CFG_2_3[256];
  fwd_port_cfg_2_4_r FWD_PORT_CFG_2_4[256];
  fwd_lag_cfg_r FWD_LAG_CFG[17];
  fwd_port_cfg_0_r FWD_PORT_CFG_0[17];
  fwd_port_cfg_1_0_r FWD_PORT_CFG_1_0[17];
  fwd_port_cfg_1_1_r FWD_PORT_CFG_1_1[17];
  fwd_port_cfg_1_2_r FWD_PORT_CFG_1_2[17];
  fwd_port_cfg_1_3_r FWD_PORT_CFG_1_3[17];
  fwd_port_cfg_1_4_r FWD_PORT_CFG_1_4[17];
  fwd_sys_cfg_1_r FWD_SYS_CFG_1;
  fwd_cpu_mac_r FWD_CPU_MAC;
  fwd_sys_cfg_router_r FWD_SYS_CFG_ROUTER;
  fwd_rx_mirror_cfg_r FWD_RX_MIRROR_CFG;
  fwd_qcn_mirror_cfg_r FWD_QCN_MIRROR_CFG;
  fwd_ieee_reserved_mac_action_r FWD_IEEE_RESERVED_MAC_ACTION;
  fwd_ieee_reserved_mac_trap_priority_r FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY;
  fwd_ieee_reserved_mac_cfg_r FWD_IEEE_RESERVED_MAC_CFG;
  fwd_ip_r FWD_IP;
  fwd_im_r FWD_IM;
} mby_ppe_fwd_misc_map;

typedef struct {
  fwd_port_cfg_2_0_r__addr FWD_PORT_CFG_2_0[256];
  fwd_port_cfg_2_1_r__addr FWD_PORT_CFG_2_1[256];
  fwd_port_cfg_2_2_r__addr FWD_PORT_CFG_2_2[256];
  fwd_port_cfg_2_3_r__addr FWD_PORT_CFG_2_3[256];
  fwd_port_cfg_2_4_r__addr FWD_PORT_CFG_2_4[256];
  fwd_lag_cfg_r__addr FWD_LAG_CFG[17];
  fwd_port_cfg_0_r__addr FWD_PORT_CFG_0[17];
  fwd_port_cfg_1_0_r__addr FWD_PORT_CFG_1_0[17];
  fwd_port_cfg_1_1_r__addr FWD_PORT_CFG_1_1[17];
  fwd_port_cfg_1_2_r__addr FWD_PORT_CFG_1_2[17];
  fwd_port_cfg_1_3_r__addr FWD_PORT_CFG_1_3[17];
  fwd_port_cfg_1_4_r__addr FWD_PORT_CFG_1_4[17];
  fwd_sys_cfg_1_r__addr FWD_SYS_CFG_1;
  fwd_cpu_mac_r__addr FWD_CPU_MAC;
  fwd_sys_cfg_router_r__addr FWD_SYS_CFG_ROUTER;
  fwd_rx_mirror_cfg_r__addr FWD_RX_MIRROR_CFG;
  fwd_qcn_mirror_cfg_r__addr FWD_QCN_MIRROR_CFG;
  fwd_ieee_reserved_mac_action_r__addr FWD_IEEE_RESERVED_MAC_ACTION;
  fwd_ieee_reserved_mac_trap_priority_r__addr FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY;
  fwd_ieee_reserved_mac_cfg_r__addr FWD_IEEE_RESERVED_MAC_CFG;
  fwd_ip_r__addr FWD_IP;
  fwd_im_r__addr FWD_IM;
} mby_ppe_fwd_misc_map__addr;


void
mby_ppe_fwd_misc_map__init(
  mby_ppe_fwd_misc_map *p0,
  mby_ppe_fwd_misc_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_0__n = 256;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_0__nd    256
typedef fwd_port_cfg_2_0_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_0_t[256];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_1__n = 256;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_1__nd    256
typedef fwd_port_cfg_2_1_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_1_t[256];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_2__n = 256;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_2__nd    256
typedef fwd_port_cfg_2_2_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_2_t[256];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_3__n = 256;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_3__nd    256
typedef fwd_port_cfg_2_3_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_3_t[256];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_4__n = 256;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_4__nd    256
typedef fwd_port_cfg_2_4_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_2_4_t[256];

static const unsigned int mby_ppe_fwd_misc_map_FWD_LAG_CFG__n = 17;
#define mby_ppe_fwd_misc_map_FWD_LAG_CFG__nd    17
typedef fwd_lag_cfg_r mby_ppe_fwd_misc_map_FWD_LAG_CFG_t[17];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_0__n = 17;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_0__nd    17
typedef fwd_port_cfg_0_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_0_t[17];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_0__n = 17;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_0__nd    17
typedef fwd_port_cfg_1_0_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_0_t[17];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_1__n = 17;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_1__nd    17
typedef fwd_port_cfg_1_1_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_1_t[17];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_2__n = 17;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_2__nd    17
typedef fwd_port_cfg_1_2_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_2_t[17];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_3__n = 17;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_3__nd    17
typedef fwd_port_cfg_1_3_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_3_t[17];

static const unsigned int mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_4__n = 17;
#define mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_4__nd    17
typedef fwd_port_cfg_1_4_r mby_ppe_fwd_misc_map_FWD_PORT_CFG_1_4_t[17];

typedef fwd_sys_cfg_1_r mby_ppe_fwd_misc_map_FWD_SYS_CFG_1_t;

typedef fwd_cpu_mac_r mby_ppe_fwd_misc_map_FWD_CPU_MAC_t;

typedef fwd_sys_cfg_router_r mby_ppe_fwd_misc_map_FWD_SYS_CFG_ROUTER_t;

typedef fwd_rx_mirror_cfg_r mby_ppe_fwd_misc_map_FWD_RX_MIRROR_CFG_t;

typedef fwd_qcn_mirror_cfg_r mby_ppe_fwd_misc_map_FWD_QCN_MIRROR_CFG_t;

typedef fwd_ieee_reserved_mac_action_r mby_ppe_fwd_misc_map_FWD_IEEE_RESERVED_MAC_ACTION_t;

typedef fwd_ieee_reserved_mac_trap_priority_r mby_ppe_fwd_misc_map_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_t;

typedef fwd_ieee_reserved_mac_cfg_r mby_ppe_fwd_misc_map_FWD_IEEE_RESERVED_MAC_CFG_t;

typedef fwd_ip_r mby_ppe_fwd_misc_map_FWD_IP_t;

typedef fwd_im_r mby_ppe_fwd_misc_map_FWD_IM_t;




/* ../src/RegC.m3:179 */
typedef struct {
  uint32 REG_INDX;
  uint8 REG_SUB_ID;
  uint8 REG_ID;
  uint13 _RSVD0_;
  uint1 OP_TYPE;
  uint1 STATUS;
  uint1 GO_COMPL;
} pol_direct_map_ctrl_r;

typedef struct {
  uint32 *REG_INDX;
  uint8 *REG_SUB_ID;
  uint8 *REG_ID;
  uint13 *_RSVD0_;
  uint1 *OP_TYPE;
  uint1 *STATUS;
  uint1 *GO_COMPL;
} pol_direct_map_ctrl_r__addr;


void
pol_direct_map_ctrl_r__init(
  pol_direct_map_ctrl_r *p0,
  pol_direct_map_ctrl_r__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int pol_direct_map_ctrl_r_REG_INDX__n = 32;
#define pol_direct_map_ctrl_r_REG_INDX__nd    32
typedef uint32 pol_direct_map_ctrl_r_REG_INDX_t;

static const unsigned int pol_direct_map_ctrl_r_REG_SUB_ID__n = 8;
#define pol_direct_map_ctrl_r_REG_SUB_ID__nd    8
typedef uint8 pol_direct_map_ctrl_r_REG_SUB_ID_t;

static const unsigned int pol_direct_map_ctrl_r_REG_ID__n = 8;
#define pol_direct_map_ctrl_r_REG_ID__nd    8
typedef uint8 pol_direct_map_ctrl_r_REG_ID_t;

static const unsigned int pol_direct_map_ctrl_r__RSVD0___n = 13;
#define pol_direct_map_ctrl_r__RSVD0___nd    13
typedef uint13 pol_direct_map_ctrl_r__RSVD0__t;

static const unsigned int pol_direct_map_ctrl_r_OP_TYPE__n = 1;
#define pol_direct_map_ctrl_r_OP_TYPE__nd    1
typedef uint1 pol_direct_map_ctrl_r_OP_TYPE_t;

static const unsigned int pol_direct_map_ctrl_r_STATUS__n = 1;
#define pol_direct_map_ctrl_r_STATUS__nd    1
typedef uint1 pol_direct_map_ctrl_r_STATUS_t;

static const unsigned int pol_direct_map_ctrl_r_GO_COMPL__n = 1;
#define pol_direct_map_ctrl_r_GO_COMPL__nd    1
typedef uint1 pol_direct_map_ctrl_r_GO_COMPL_t;




/* ../src/RegC.m3:221 */
typedef struct {
  pol_direct_map_ctrl_r POL_DIRECT_MAP_CTRL;
  pol_direct_map_ctr0_r POL_DIRECT_MAP_CTR0;
  pol_direct_map_ctr1_r POL_DIRECT_MAP_CTR1;
  pol_direct_map_pol0_r POL_DIRECT_MAP_POL0;
  pol_direct_map_pol1_r POL_DIRECT_MAP_POL1;
  pol_direct_map_pol2_r POL_DIRECT_MAP_POL2;
  pol_direct_map_pol3_r POL_DIRECT_MAP_POL3;
  pol_cfg_rf POL_CFG[2];
  pol_dscp_rf POL_DSCP[16];
  pol_vpri_rf POL_VPRI[16];
  pol_time_unit_r POL_TIME_UNIT;
  pol_time_r POL_TIME;
  pol_sweep_r POL_SWEEP;
  pol_sweep_last_r POL_SWEEP_LAST;
  pol_sweep_period_max_r POL_SWEEP_PERIOD_MAX;
} mby_ppe_policers_map;

typedef struct {
  pol_direct_map_ctrl_r__addr POL_DIRECT_MAP_CTRL;
  pol_direct_map_ctr0_r__addr POL_DIRECT_MAP_CTR0;
  pol_direct_map_ctr1_r__addr POL_DIRECT_MAP_CTR1;
  pol_direct_map_pol0_r__addr POL_DIRECT_MAP_POL0;
  pol_direct_map_pol1_r__addr POL_DIRECT_MAP_POL1;
  pol_direct_map_pol2_r__addr POL_DIRECT_MAP_POL2;
  pol_direct_map_pol3_r__addr POL_DIRECT_MAP_POL3;
  pol_cfg_rf__addr POL_CFG[2];
  pol_dscp_rf__addr POL_DSCP[16];
  pol_vpri_rf__addr POL_VPRI[16];
  pol_time_unit_r__addr POL_TIME_UNIT;
  pol_time_r__addr POL_TIME;
  pol_sweep_r__addr POL_SWEEP;
  pol_sweep_last_r__addr POL_SWEEP_LAST;
  pol_sweep_period_max_r__addr POL_SWEEP_PERIOD_MAX;
} mby_ppe_policers_map__addr;


void
mby_ppe_policers_map__init(
  mby_ppe_policers_map *p0,
  mby_ppe_policers_map__addr *p1,
  void (*f)(void *, int)
);

typedef pol_direct_map_ctrl_r mby_ppe_policers_map_POL_DIRECT_MAP_CTRL_t;

typedef pol_direct_map_ctr0_r mby_ppe_policers_map_POL_DIRECT_MAP_CTR0_t;

typedef pol_direct_map_ctr1_r mby_ppe_policers_map_POL_DIRECT_MAP_CTR1_t;

typedef pol_direct_map_pol0_r mby_ppe_policers_map_POL_DIRECT_MAP_POL0_t;

typedef pol_direct_map_pol1_r mby_ppe_policers_map_POL_DIRECT_MAP_POL1_t;

typedef pol_direct_map_pol2_r mby_ppe_policers_map_POL_DIRECT_MAP_POL2_t;

typedef pol_direct_map_pol3_r mby_ppe_policers_map_POL_DIRECT_MAP_POL3_t;

static const unsigned int mby_ppe_policers_map_POL_CFG__n = 2;
#define mby_ppe_policers_map_POL_CFG__nd    2
typedef pol_cfg_rf mby_ppe_policers_map_POL_CFG_t[2];

static const unsigned int mby_ppe_policers_map_POL_DSCP__n = 16;
#define mby_ppe_policers_map_POL_DSCP__nd    16
typedef pol_dscp_rf mby_ppe_policers_map_POL_DSCP_t[16];

static const unsigned int mby_ppe_policers_map_POL_VPRI__n = 16;
#define mby_ppe_policers_map_POL_VPRI__nd    16
typedef pol_vpri_rf mby_ppe_policers_map_POL_VPRI_t[16];

typedef pol_time_unit_r mby_ppe_policers_map_POL_TIME_UNIT_t;

typedef pol_time_r mby_ppe_policers_map_POL_TIME_t;

typedef pol_sweep_r mby_ppe_policers_map_POL_SWEEP_t;

typedef pol_sweep_last_r mby_ppe_policers_map_POL_SWEEP_LAST_t;

typedef pol_sweep_period_max_r mby_ppe_policers_map_POL_SWEEP_PERIOD_MAX_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mby_ppe_parser_map parser;
  mby_ppe_mapper_map mapper;
  mby_ppe_cgrp_a_map cgrp_a;
  mby_ppe_cgrp_b_map cgrp_b;
  mby_ppe_entropy_map entropy;
  mby_ppe_nexthop_map nexthop;
  mby_ppe_fwd_misc_map fwd_misc;
  mby_ppe_mst_glort_map mst_glort;
  mby_ppe_policers_map policers;
  mby_ppe_rx_stats_map stats;
  mby_ppe_cm_apply_map cm_apply;
  mby_ppe_cm_usage_map cm_usage;
  mby_ppe_trig_apply_map trig_apply;
  mby_ppe_trig_apply_misc_map trig_apply_misc;
  mby_ppe_trig_usage_map trig_usage;
} mby_ppe_rx_top_map;

typedef struct {
  mby_ppe_parser_map__addr parser;
  mby_ppe_mapper_map__addr mapper;
  mby_ppe_cgrp_a_map__addr cgrp_a;
  mby_ppe_cgrp_b_map__addr cgrp_b;
  mby_ppe_entropy_map__addr entropy;
  mby_ppe_nexthop_map__addr nexthop;
  mby_ppe_fwd_misc_map__addr fwd_misc;
  mby_ppe_mst_glort_map__addr mst_glort;
  mby_ppe_policers_map__addr policers;
  mby_ppe_rx_stats_map__addr stats;
  mby_ppe_cm_apply_map__addr cm_apply;
  mby_ppe_cm_usage_map__addr cm_usage;
  mby_ppe_trig_apply_map__addr trig_apply;
  mby_ppe_trig_apply_misc_map__addr trig_apply_misc;
  mby_ppe_trig_usage_map__addr trig_usage;
} mby_ppe_rx_top_map__addr;


void
mby_ppe_rx_top_map__init(
  mby_ppe_rx_top_map *p0,
  mby_ppe_rx_top_map__addr *p1,
  void (*f)(void *, int)
);

typedef mby_ppe_parser_map mby_ppe_rx_top_map_parser_t;

typedef mby_ppe_mapper_map mby_ppe_rx_top_map_mapper_t;

typedef mby_ppe_cgrp_a_map mby_ppe_rx_top_map_cgrp_a_t;

typedef mby_ppe_cgrp_b_map mby_ppe_rx_top_map_cgrp_b_t;

typedef mby_ppe_entropy_map mby_ppe_rx_top_map_entropy_t;

typedef mby_ppe_nexthop_map mby_ppe_rx_top_map_nexthop_t;

typedef mby_ppe_fwd_misc_map mby_ppe_rx_top_map_fwd_misc_t;

typedef mby_ppe_mst_glort_map mby_ppe_rx_top_map_mst_glort_t;

typedef mby_ppe_policers_map mby_ppe_rx_top_map_policers_t;

typedef mby_ppe_rx_stats_map mby_ppe_rx_top_map_stats_t;

typedef mby_ppe_cm_apply_map mby_ppe_rx_top_map_cm_apply_t;

typedef mby_ppe_cm_usage_map mby_ppe_rx_top_map_cm_usage_t;

typedef mby_ppe_trig_apply_map mby_ppe_rx_top_map_trig_apply_t;

typedef mby_ppe_trig_apply_misc_map mby_ppe_rx_top_map_trig_apply_misc_t;

typedef mby_ppe_trig_usage_map mby_ppe_rx_top_map_trig_usage_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mby_ppe_rx_top_map rx_ppe;
  mby_rx_voq_map rx_voq;
  mby_rx_pb_map rx_pb;
  mby_ppe_tx_top_map tx_ppe;
  mby_tx_voq_map tx_voq;
  mby_tx_pb_map tx_pb;
} mby_mgp_top_map;

typedef struct {
  mby_ppe_rx_top_map__addr rx_ppe;
  mby_rx_voq_map__addr rx_voq;
  mby_rx_pb_map__addr rx_pb;
  mby_ppe_tx_top_map__addr tx_ppe;
  mby_tx_voq_map__addr tx_voq;
  mby_tx_pb_map__addr tx_pb;
} mby_mgp_top_map__addr;


void
mby_mgp_top_map__init(
  mby_mgp_top_map *p0,
  mby_mgp_top_map__addr *p1,
  void (*f)(void *, int)
);

typedef mby_ppe_rx_top_map mby_mgp_top_map_rx_ppe_t;

typedef mby_rx_voq_map mby_mgp_top_map_rx_voq_t;

typedef mby_rx_pb_map mby_mgp_top_map_rx_pb_t;

typedef mby_ppe_tx_top_map mby_mgp_top_map_tx_ppe_t;

typedef mby_tx_voq_map mby_mgp_top_map_tx_voq_t;

typedef mby_tx_pb_map mby_mgp_top_map_tx_pb_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mby_mgp_top_map mgp[2];
  mby_shm_map shm;
} mby_mpp_map;

typedef struct {
  mby_mgp_top_map__addr mgp[2];
  mby_shm_map__addr shm;
} mby_mpp_map__addr;


void
mby_mpp_map__init(
  mby_mpp_map *p0,
  mby_mpp_map__addr *p1,
  void (*f)(void *, int)
);

static const unsigned int mby_mpp_map_mgp__n = 2;
#define mby_mpp_map_mgp__nd    2
typedef mby_mgp_top_map mby_mpp_map_mgp_t[2];

typedef mby_shm_map mby_mpp_map_shm_t;




/* ../src/RegC.m3:221 */
typedef struct {
  mby_globals_map globals;
  mby_mpp_map mpp[1];
} mby_top_map;

typedef struct {
  mby_globals_map__addr globals;
  mby_mpp_map__addr mpp[1];
} mby_top_map__addr;


void
mby_top_map__init(
  mby_top_map *p0,
  mby_top_map__addr *p1,
  void (*f)(void *, int)
);

typedef mby_globals_map mby_top_map_globals_t;

static const unsigned int mby_top_map_mpp__n = 1;
#define mby_top_map_mpp__nd    1
typedef mby_mpp_map mby_top_map_mpp_t[1];



#endif /* !mby_top_map_INCLUDED */
