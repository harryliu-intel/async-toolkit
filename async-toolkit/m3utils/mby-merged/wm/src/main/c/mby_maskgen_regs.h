// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_MASKGEN_REGS_H
#define MBY_MASKGEN_REGS_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_trig_results.h"

// Enums:

typedef enum mbyGlortRamStrictEnum
{
    MBY_GLORT_RAM_STRICT_HASHED                 = 0,
    MBY_GLORT_RAM_STRICT_RSVD                   = 1,
    MBY_GLORT_RAM_STRICT_TARGETED_DETERMINISTIC = 2,
    MBY_GLORT_RAM_STRICT_DETERMINISTIC          = 3

} mbyGlortRamStrict;

// Structs:

typedef struct mbyGlortRamStruct
{
    fm_bool           SKIP_DGLORT_DEC;
    fm_bool           HASH_ROTATION;
    fm_byte           DEST_COUNT;
    fm_byte           RANGE_SUB_INDEX_B;
    fm_byte           RANGE_SUB_INDEX_A;
    fm_uint16         DEST_INDEX;
    mbyGlortRamStrict STRICT;

} mbyGlortRam;

typedef struct mbyGlortCamStruct
{
    fm_uint16         KEY_INVERT;
    fm_uint16         KEY;

} mbyGlortCam;

typedef struct mbyGlortDestTableStruct
{
    fm_uint16         IP_MULTICAST_INDEX;
    fm_uint64         DEST_MASK[MBY_DMASK_REGISTERS];

} mbyGlortDestTable;

typedef struct mbyFwdLagCfgStruct
{
    fm_bool           IN_LAG;
    fm_bool           HASH_ROTATION;
    fm_byte           INDEX;
    fm_byte           LAG_SIZE;

} mbyFwdLagCfg;

typedef struct mbyFwdSysCfgRouterStruct
{
    fm_bool           TRAP_IP_OPTIONS;
    fm_byte           TRAP_TTL1;

} mbyFwdSysCfgRouter;

typedef struct mbyFwdSysCfg1Struct
{
    fm_bool           STORE_TRAP_ACTION;
    fm_bool           DROP_MAC_CTRL_ETHERTYPE;
    fm_bool           DROP_INVALID_SMAC;
    fm_bool           ENABLE_TRAP_PLUS_LOG;
    fm_bool           TRAP_MTU_VIOLATIONS;

} mbyFwdSysCfg1;

typedef struct mbyFwdPortCfgStruct
{
    fm_bool           LEARNING_ENABLE;
    fm_bool           FILTER_VLAN_INGRESS;
    fm_uint32         DESTINATION_MASK[MBY_DMASK_REGISTERS];

} mbyFwdPortCfg;

typedef struct mbyFwdPortCfg2Struct
{
    fm_uint32         DESTINATION_MASK;

} mbyFwdPortCfg2;

typedef struct mbyCmApplyLoopbackSuppressStruct
{
    fm_uint16         GLORT_MASK;
    fm_uint16         GLORT;

} mbyCmApplyLoopbackSuppress;

typedef struct mbyCpuTrapMaskStruct
{
    fm_uint64         DEST_MASK[MBY_DMASK_REGISTERS];

} mbyCpuTrapMask;

typedef struct mbyMirrorEcmpDmaskStruct
{
    fm_uint64         Mirror_port_mask[MBY_DMASK_REGISTERS];

} mbyMirrorEcmpDmask;

// Functions:

mbyGlortRam getGlortRamEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_byte                       const ram_index
);

mbyGlortCam getGlortCamEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_byte                             cam_index
);

mbyGlortDestTable getGlortDestTableEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_uint16                           table_index
);

mbyEgressVidTableCfg getEvidTableCfgEntry
(
    mby_ppe_mst_glort_map const * const mst_glort,
    fm_uint16                           vid
);

mbyFwdPortCfg getPortCfg
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint32                    const port // RX port
);

mbyFwdPortCfg2 getPortCfg2
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint16                    const l2_edomain
);

mbyFwdSysCfg1 getSysCfg1
(
    mby_ppe_fwd_misc_map const * const fwd_misc
);

mbyFwdSysCfgRouter getSysCfgRouter
(
    mby_ppe_fwd_misc_map const * const fwd_misc
);

mbyFwdLagCfg getLagCfg
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint32                    const port
);

mbyCmApplyLoopbackSuppress getLoopbackSuppress
(
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_uint32                    const port // logical port
);

mbyCpuTrapMask getCpuTrapMask
(
    mby_ppe_cm_apply_map const * const cm_apply
);

mbyMirrorEcmpDmask getMirrorEcmpDmask
(
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_uint16                    const mirror_profile_idx
);

#endif /* MBY_MASKGEN_REGS_H */
