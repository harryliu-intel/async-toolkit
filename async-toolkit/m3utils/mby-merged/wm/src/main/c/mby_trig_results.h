/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_TRIG_RESULTS_H
#define MBY_TRIG_RESULTS_H

#include "fm_types.h"
#include "mby_dmask_regs.h"

typedef enum mbyTriggerActionForwardingEnum
{
    MBY_TRIG_ACTION_FORWARDING_AS_IS = 0,
    MBY_TRIG_ACTION_FORWARDING_FORWARD,
    MBY_TRIG_ACTION_FORWARDING_REDIRECT,
    MBY_TRIG_ACTION_FORWARDING_DROP

} mbyTriggerActionForwarding;

typedef enum mbyTriggerActionTrapEnum
{
    MBY_TRIG_ACTION_TRAP_AS_IS = 0,
    MBY_TRIG_ACTION_TRAP_TRAP,
    MBY_TRIG_ACTION_TRAP_LOG,
    MBY_TRIG_ACTION_TRAP_REVERT

} mbyTriggerActionTrap;

typedef enum mbyTriggerActionMirroringEnum
{
    MBY_TRIG_ACTION_MIRRORING_AS_IS = 0,
    MBY_TRIG_ACTION_MIRRORING_MIRROR,
    MBY_TRIG_ACTION_MIRRORING_CANCEL

} mbyTriggerActionMirroring;

typedef enum mbyTriggerActionTCEnum
{
    MBY_TRIG_ACTION_TC_AS_IS = 0,
    MBY_TRIG_ACTION_TC_REASSIGN

} mbyTriggerActionTC;

typedef enum mbyTriggerActionVlanEnum
{
    MBY_TRIG_ACTION_VLAN_AS_IS = 0,
    MBY_TRIG_ACTION_VLAN_REASSIGN

} mbyTriggerActionVlan;

typedef enum mbyTriggerActionLearningEnum
{
    MBY_TRIG_ACTION_LEARNING_AS_IS = 0,
    MBY_TRIG_ACTION_LEARNING_DONT_LEARN,
    MBY_TRIG_ACTION_LEARNING_FORCE_LEARN

} mbyTriggerActionLearning;

typedef struct mbyEgressVidTableCfgStruct
{
  fm_byte              TRIG_ID;
  fm_uint64            MEMBERSHIP[MBY_DMASK_REGISTERS];

} mbyEgressVidTableCfg;

typedef struct mbyTriggerResultsStruct
{
    fm_uint32                           action;
    mbyTriggerActionForwarding          forwardingAction;
    fm_uint16                           destGlort;
    fm_uint64                           destMask[MBY_DMASK_REGISTERS];
    fm_bool                             filterDestMask;
    mbyTriggerActionTrap                trapAction;
    fm_byte                             cpuCode;
    fm_byte                             trapCode;
    fm_bool                             logAction;
    mbyTriggerActionMirroring           mirroringAction0;
    mbyTriggerActionMirroring           mirroringAction1;
    fm_bool                             rxMirror;
    fm_byte                             mirrorProfileIndex0;
    fm_byte                             mirrorProfileIndex1;
    fm_bool                             mirror0ProfileV;
    fm_bool                             mirror1ProfileV;
    fm_byte                             mirror0ProfileIdx;
    fm_byte                             mirror1ProfileIdx;
    mbyTriggerActionTC                  TCAction;
    fm_byte                             TC;
    mbyTriggerActionVlan                vlanAction;
    fm_uint16                           vlan;
    mbyTriggerActionLearning            learningAction;
    fm_bool                             rateLimitAction;
    fm_byte                             rateLimitNum;
    fm_byte                             egressL2DomainAction;
    fm_byte                             egressL3DomainAction;
    fm_byte                             qcnValid0;
    fm_byte                             qcnValid1;
    fm_byte                             policerAction;
    fm_bool                             update_l2_domain;
    fm_bool                             update_l3_domain;

} mbyTriggerResults;

#endif // MBY_TRUG_RESULTS_H
