// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_TRIGGERS_REGS_H
#define MBY_TRIGGERS_REGS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

typedef enum {
    MBY_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_Z = 0,
    MBY_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_NZ = 1,
    MBY_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_EQ = 2,
    MBY_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_NE = 3
} mbyTriggerConditionCfgMatchTx;

typedef enum
{
    MBY_TRIG_ACTION_RATE_LIMIT_AS_IS = 0,
    MBY_TRIG_ACTION_RATE_LIMIT_APPLY
} mbyTriggerActionRateLimit;

typedef enum
{
    MBY_TRIG_ACTION_METADATA_NONE = 0,
	MBY_TRIG_ACTION_METADATA_MASKED_16_SET,
	MBY_TRIG_ACTION_METADATA_MASKED_16_COPY
} mbyTriggerActionMetadata;

typedef enum
{
    MBY_TRIG_ACTION_EGRESS_L2DOMAIN_ALWAYS_UPDATE = 0,
	MBY_TRIG_ACTION_EGRESS_L2DOMAIN_SKIP
} mbyTriggerActionEgressL2Domain;

typedef enum
{
	MBY_TRIG_ACTION_EGRESS_L3DOMAIN_ALWAYS_UPDATE = 0,
	MBY_TRIG_ACTION_EGRESS_L3DOMAIN_SKIP
} mbyTriggerActionEgressL3Domain;

typedef enum
{
	MBY_TRIG_ACTION_POLICER_NONE = 0,
	MBY_TRIG_ACTION_POLICER_DO_NOT_POLICE
} mbyTriggerActionPolicer;

typedef enum
{
	MBY_TRIG_ACTION_NOMODIFY_NONE = 0,
	MBY_TRIG_ACTION_NOMODIFY_DO_NOT_MODIFY
} mbyTriggerActionNoModify;

typedef enum
{
    MBY_TRIG_ACTION_TYPE_FORWARD = 0,
    MBY_TRIG_ACTION_TYPE_TRAP,
    MBY_TRIG_ACTION_TYPE_MIRROR,
    MBY_TRIG_ACTION_TYPE_PRI,
    MBY_TRIG_ACTION_TYPE_VLAN,
    MBY_TRIG_ACTION_TYPE_LEARN,
    MBY_TRIG_ACTION_TYPE_RATE
} mbyTriggerActionType;

typedef enum
{
    MBY_TRIGGER_DIRECT_MAP_CTRL_STATUS_SUCCESSFUL = 0,
    MBY_TRIGGER_DIRECT_MAP_CTRL_STATUS_ADDRESS_OUT_OF_RANGE
} mbyTriggerDirectMapCtrlStatus;

typedef enum
{
    MBY_TRIGGER_DIRECT_MAP_CTRL_OP_TYPE_READ = 0,
    MBY_TRIGGER_DIRECT_MAP_CTRL_OP_TYPE_WRITE
} mbyTriggerDirectMapCtrlOpType;

typedef enum
{
    MBY_TRIGGER_DIRECT_MAP_CTRL_REG_ID_TRIGGER_CONDITION_TX = 0,
    MBY_TRIGGER_DIRECT_MAP_CTRL_REG_ID_TRIGGER_ACTION_DMASK,
    MBY_TRIGGER_DIRECT_MAP_CTRL_REG_ID_TRIGGER_ACTION_DROP
} mbyTriggerDirectMapCtrlRegId;

// Structs:

typedef struct _mbyTriggerConditionCfg
{
  mbyTriggerConditionCfgMatchTx MATCH_TX;
  fm_byte                       MATCH_RANDOM_THRESHOLD;
  fm_bool                       MATCH_RANDOM_IF_LESS;
  fm_bool                       MATCH_RANDOM_NUMBER;
  fm_bool                       MATCH_BY_PRECEDENCE;
  fm_byte                       MATCH_EGRESS_DOMAIN;
  fm_byte                       MATCH_DEST_GLORT;
  fm_byte                       MATCH_TC;
  fm_byte                       MATCH_CGRP;
  fm_byte                       MATCH_VLAN;
  fm_byte                       LEARN;
} mbyTriggerConditionCfg;

typedef struct _mbyTriggerConditionParam
{
  fm_uint16                     EGRESS_DOMAIN_MASK;
  fm_uint16                     EGRESS_DOMAIN_VALUE;
  fm_byte                       FTYPE_MASK;
  fm_byte                       ROUTED_MASK;
  fm_byte                       FRAME_CLASS_MASK;
  fm_byte                       TC;
  fm_byte                       VID_ID;
} mbyTriggerConditionParam;

typedef struct _mbyTriggerConditionCGRP
{
  fm_byte                       CGRP_MASK;
  fm_byte                       CGRP_ID;
} mbyTriggerConditionCGRP;

typedef struct _mbyTriggerConditionGlort
{
  fm_uint16                     GLORT_MASK;
  fm_uint16                     DEST_GLORT;
} mbyTriggerConditionGlort;

typedef struct _mbyTriggerConditionRx
{
  fm_uint32                     SRC_PORT_MASK;
} mbyTriggerConditionRx;

typedef struct _mbyTriggerConditionAmask1
{
  fm_uint32                     HANDLER_ACTION_MASK;
} mbyTriggerConditionAmask1;

typedef struct _mbyTriggerConditionAmask2
{
  fm_uint32                     HANDLER_ACTION_MASK;
} mbyTriggerConditionAmask2;

typedef struct _mbyTriggerIp
{
  fm_uint64                     PENDING;
} mbyTriggerIp;

typedef struct _mbyTriggerStats
{
  fm_uint64                     COUNT;
} mbyTriggerStats;

typedef struct _mbyTriggerDirectMapCtrl
{
    fm_byte                       GO_COMPL;
    mbyTriggerDirectMapCtrlStatus STATUS;
    mbyTriggerDirectMapCtrlOpType OP_TYPE;
    mbyTriggerDirectMapCtrlRegId  REG_ID;
    fm_uint32                     REG_INDX;
} mbyTriggerDirectMapCtrl;

typedef struct _mbyTriggerDirectMapCtx
{
    fm_uint64                    DEST_PORT_MASK[5];
} mbyTriggerDirectMapCtx;

typedef struct _mbyTriggerDirectMapAdm
{
    fm_byte                      FILTER_DEST_MASK;
    fm_uint64                    NEW_DEST_MASK[MBY_DMASK_REGISTERS];
} mbyTriggerDirectMapAdm;

typedef struct _mbyTriggerDirectMapAdr
{
    fm_uint64                    DROP_MASK[MBY_DMASK_REGISTERS];
} mbyTriggerDirectMapAdr;

typedef struct mbyTriggerActionsStruct
{
    /** TRIGGERS forwarding action. */
    mbyTriggerActionForwarding          forwardingAction;
    fm_uint16                           newDestGlort;
    fm_uint16                           newDestGlortMask;
    fm_uint64                           newDestMask[5];
    fm_bool                             filterDestMask;
    fm_uint64                           dropMask[5];

    /** TRIGGERS trap action. */
    mbyTriggerActionTrap                trapAction;
    fm_byte                             cpuCode;
    fm_byte                             trapCode;

    /** TRIGGERS mirroring action. */
    mbyTriggerActionMirroring           mirroringAction0;
    mbyTriggerActionMirroring           mirroringAction1;
    mbyTriggerActionMirroring           mirroringAction2;
    mbyTriggerActionMirroring           mirroringAction3;

    fm_byte                             mirrorProfileIndex0;
    fm_byte                             mirrorProfileIndex1;
    fm_byte                             mirrorProfileIndex2;
    fm_byte                             mirrorProfileIndex3;

    /** TRIGGERS ISL switch priority action. */
    mbyTriggerActionTC                  TCAction;
    fm_byte                             newTC;

    /** TRIGGERS VLAN action. */
    mbyTriggerActionVlan                vlanAction;
    fm_uint16                           newVlan;

    /** TRIGGERS learning action. */
    mbyTriggerActionLearning            learningAction;

    /** TRIGGERS rate limiter action */
    mbyTriggerActionRateLimit           rateLimitAction;
    fm_byte                             newRateLimitNum;

    /** TRIGGERS l2 domain action */
	mbyTriggerActionEgressL2Domain      egressL2DomainAction;
    mbyTriggerActionEgressL3Domain      egressL3DomainAction;

    /** TRIGGERS policer action */
	mbyTriggerActionPolicer             policerAction;

    /** TRIGGERS noModify action */
    mbyTriggerActionNoModify            noModifyAction;

} mbyTriggerActions;

// Functions:

mbyTriggerConditionCfg mbyTrigGetConditionCfg
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerConditionParam mbyTrigGetConditionParam
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerConditionCGRP mbyTrigGetConditionCGRP
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerConditionGlort mbyTrigGetConditionGlort
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerConditionRx mbyTrigGetConditionRx
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerConditionAmask1 mbyTrigGetConditionAmask1
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerConditionAmask2 mbyTrigGetConditionAmask2
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerDirectMapCtx mbyTriggerDirectMapCtrlCtxReadOperation
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

mbyTriggerActions mbyTriggerGetActions
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
);

#endif /* MBY_TRIGGERS_H */
