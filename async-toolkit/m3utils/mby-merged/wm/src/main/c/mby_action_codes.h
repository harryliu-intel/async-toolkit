// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_ACTION_CODES_H
#define MBY_ACTION_CODES_H

#define MBY_AMASK_WIDTH                      38

// Action Codes:
#define MBY_ACTION_NORMAL             0   /* forwarded normally */
#define MBY_ACTION_FLOOD              1   /* flooded due to unknown destination */
#define MBY_ACTION_SPECIAL            2   /* forwarded with fixed destination mask */
#define MBY_ACTION_DROP_PARSE         3   /* drop due to parse error */
#define MBY_ACTION_DROP_PARITY        4   /* dropped due to parity error */
#define MBY_ACTION_TRAP               5   /* trap spec. mcast add., MAC CTL & MAC REMAP CTL */
#define MBY_ACTION_DROP_CONTROL       6   /* dropped pause frame */
#define MBY_ACTION_DROP_STP           7   /* dropped due to spanning tree*/
#define MBY_ACTION_DROP_SV            8   /* dropped due to security violation */
#define MBY_ACTION_MARKER_ERROR_DROPS 9   /* marker packets dropped due to fatal errors */
#define MBY_ACTION_DROP_IV            10   /* dropped due to vlan ingress violation */
#define MBY_ACTION_DROP_EV            11   /* dropped due to vlan egress violation */
#define MBY_ACTION_DROP_CAM           12
#define MBY_ACTION_DROP_CGRP          13   /* dropped due to CGRP flag */
#define MBY_ACTION_DROP_TRIG          14   /* dropped due to a trigger action */
#define MBY_ACTION_DROP_L3_PYLD_LEN   15   /* Single segment frames dropped when the L3 payload length does not agree with the actual packet length */
#define MBY_ACTION_DROP_POLICER       16   /* dropped due the policer_drop flag from pre. */
#define MBY_ACTION_DROP_TTL           17
#define MBY_ACTION_DROP_CM_GLOBAL     18
#define MBY_ACTION_DROP_CM_SMP0       19
#define MBY_ACTION_DROP_CM_SMP1       20
#define MBY_ACTION_DROP_CM_RX_HOG0    21
#define MBY_ACTION_DROP_CM_RX_HOG1    22
#define MBY_ACTION_DROP_CM_TX_HOG0    23
#define MBY_ACTION_DROP_CM_TX_HOG1    24
#define MBY_ACTION_DROP_FRAME_ERR     25
#define MBY_ACTION_REDIRECT_TRIG      26   /* redirected due to trigger match */
#define MBY_ACTION_DROP_DLF           27   /* drop due to flood control of DLF frames */
#define MBY_ACTION_GLORT_FORWARDED    28  /* Glort forwarded. */
#define MBY_ACTION_BANK5_OTHER_DROPS  29  /* Bank 5 Other Drops */
#define MBY_ACTION_DROP_LOOPBACK      30  /* Drop due to port or VLAN reflection disabled. */
#define MBY_ACTION_DROP_L4_CSUM       31  /* Single segment packet drops due to L4 checksum (UDP, TCP, SCTP) failures */

#define MBY_AMASK_DROP_PERR                  ( FM_LITERAL_U64(1) <<  0  )
#define MBY_AMASK_SPECIAL                    ( FM_LITERAL_U64(1) <<  1  )
#define MBY_AMASK_DROP_PARSER_ERR            ( FM_LITERAL_U64(1) <<  2  )
#define MBY_AMASK_TRAP_RESERVED_MAC          ( FM_LITERAL_U64(1) <<  3  )
#define MBY_AMASK_TRAP_RESERVED_MAC_REMAP    ( FM_LITERAL_U64(1) <<  4  )
#define MBY_AMASK_DROP_MAC_CTRL              ( FM_LITERAL_U64(1) <<  7  )
#define MBY_AMASK_DROP_RESERVED_MAC          ( FM_LITERAL_U64(1) <<  8  )
#define MBY_AMASK_DROP_SMAC                  ( FM_LITERAL_U64(1) <<  10 )
//In MBY Functional Specification Reserved <- REVISIT !!!!
//efine MBY_AMASK_DROP_SEC_ADDR              ( FM_LITERAL_U64(1) <<  11 )
//efine MBY_AMASK_DROP_SEC_PORT              ( FM_LITERAL_U64(1) <<  12 )
//efine MBY_AMASK_DROP_STATIC_ADDR           ( FM_LITERAL_U64(1) <<  13 )
//efine MBY_AMASK_DROP_PROVISIONAL           ( FM_LITERAL_U64(1) <<  14 )
#define MBY_AMASK_TRAP_CPU_ADDR              ( FM_LITERAL_U64(1) <<  15 )
#define MBY_AMASK_DROP_IV                    ( FM_LITERAL_U64(1) <<  16 )
#define MBY_AMASK_DROP_INGRESS_STP_NON_LEARN ( FM_LITERAL_U64(1) <<  17 )
#define MBY_AMASK_DROP_INGRESS_STP_LEARN     ( FM_LITERAL_U64(1) <<  18 )
#define MBY_AMASK_DROP_CGRP                  ( FM_LITERAL_U64(1) <<  19 )
#define MBY_AMASK_TRAP_CGRP                  ( FM_LITERAL_U64(1) <<  20 )
#define MBY_AMASK_TRAP_ICMP_TTL              ( FM_LITERAL_U64(1) <<  21 )
#define MBY_AMASK_TRAP_IP_OPTION             ( FM_LITERAL_U64(1) <<  22 )
#define MBY_AMASK_TRAP_MTU_VIO               ( FM_LITERAL_U64(1) <<  23 )
#define MBY_AMASK_TRAP_IGMP                  ( FM_LITERAL_U64(1) <<  24 )
#define MBY_AMASK_TRAP_TTL                   ( FM_LITERAL_U64(1) <<  25 )
#define MBY_AMASK_DROP_TTL                   ( FM_LITERAL_U64(1) <<  26 )
#define MBY_AMASK_DROP_DLF                   ( FM_LITERAL_U64(1) <<  27 )
#define MBY_AMASK_DROP_CAM_MISS              ( FM_LITERAL_U64(1) <<  28 )
#define MBY_AMASK_DROP_NULL_GLORTDEST        ( FM_LITERAL_U64(1) <<  29 )
#define MBY_AMASK_DROP_EV                    ( FM_LITERAL_U64(1) <<  30 )
#define MBY_AMASK_DROP_EGRESS_STP            ( FM_LITERAL_U64(1) <<  32 )
#define MBY_AMASK_DROP_LOOPBACK              ( FM_LITERAL_U64(1) <<  33 )
#define MBY_AMASK_GLORT                      ( FM_LITERAL_U64(1) <<  34 )
#define MBY_AMASK_FLOOD                      ( FM_LITERAL_U64(1) <<  35 )
#define MBY_AMASK_SWITCH_RESERVED_MAC        ( FM_LITERAL_U64(1) <<  36 )
#define MBY_AMASK_FORWARD_NORMAL             ( FM_LITERAL_U64(1) <<  37 )
#define MBY_AMASK_LOG_INGRESS_CGRP           ( FM_LITERAL_U64(1) <<  38 )
#define MBY_AMASK_LOG_RESERVED_MAC           ( FM_LITERAL_U64(1) <<  39 )
#define MBY_AMASK_LOG_ARP_REDIRECT           ( FM_LITERAL_U64(1) <<  40 )
#define MBY_AMASK_LOG_IP_ICMP                ( FM_LITERAL_U64(1) <<  41 )
#define MBY_AMASK_LOG_IP_TTL                 ( FM_LITERAL_U64(1) <<  42 )
#define MBY_AMASK_MIRROR_INGRESS_CGRP        ( FM_LITERAL_U64(1) <<  43 )

#endif // MBY_ACTION_CODES_H
