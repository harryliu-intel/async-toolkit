// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_mapper.h"

static fm_uint32 rotateKey(fm_uint key, fm_uint rot) {
    return ((key << rot) | (key >> (32 - rot)));
}

static fm_uint32 generateMask(fm_uint start, fm_uint len) {
    fm_uint32 mask = 0;
    for (fm_uint j = start; j < (start + len); j++)
        mask |= (1 << j);
    return mask;
}

static void realignKeys
(
    const mbyParserToMapper * const in,
    const fm_bool                   is_ipv4           [MBY_N_IS_IP_BITS],
    const fm_bool                   is_ipv6           [MBY_N_IS_IP_BITS],
    fm_uint16                       realigned_keys    [MBY_N_REALIGN_KEYS],
    fm_bool                         realigned_keys_vld[MBY_N_REALIGN_KEYS],
    fm_bool                       * ihl_ok,
    fm_bool                       * ihl_fits
)
{
    for (fm_uint i = 0; i < N_REALIGN_KEYS; i++) {
        realigned_keys    [i] = in->PA_KEYS[i];
        realigned_keys_vld[i] = in->PA_KEYS_VALID[i];
    }

    // Realign for IPv4:
    
    if (is_ipv4[1]) // Inner IP Header
    {
        realigned_keys    [MBY_RE_KEYS_INNER_IP_TTL_PROT  ] = in->PA_KEYS      [MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_TTL_PROT  ] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 4] = 0;

        fm_byte un0 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 8, 8, un0);

        // MISC
        fm_bool innerIhlNot5 = (FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER], 8, 4) != 5);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 4, 1, innerIhlNot5);

        fm_bool un1 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + 3], 14, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 5, 1, un1);

        fm_bool innerHf = !FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + 3], 0, 13);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 6, 1, innerHf);

        fm_bool un2 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + 3], 13, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 7, 1, un2);

        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_DS_FLOW] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER];

        // FLOW
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 0, 4, 0);

        realigned_keys    [MBY_RE_KEYS_INNER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 4] = 0;
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 5] = 0;

        // Inner DIP
        realigned_keys    [MBY_RE_KEYS_INNER_DIP       ] = in->PA_KEYS      [MBY_PA_KEYS_INNER_SIPDIP + 2];
        realigned_keys_vld[MBY_RE_KEYS_INNER_DIP       ] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_SIPDIP + 2];
        realigned_keys    [MBY_RE_KEYS_INNER_DIP    + 1] = in->PA_KEYS      [MBY_PA_KEYS_INNER_SIPDIP + 3];
        realigned_keys_vld[MBY_RE_KEYS_INNER_DIP    + 1] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_SIPDIP + 3];
        realigned_keys_vld[MBY_PA_KEYS_INNER_SIPDIP + 2] = 0;
        realigned_keys_vld[MBY_PA_KEYS_INNER_SIPDIP + 3] = 0;
    }

    if (is_ipv4[0]) // Outer IP Header
    {
        realigned_keys    [MBY_RE_KEYS_OUTER_IP_TTL_PROT  ] = in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 4];
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_TTL_PROT  ] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER + 4];

        fm_byte un0 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 8, 8, un0);

        // MISC
        fm_byte ihl = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER], 8, 4);
        *ihl_ok   = (ihl >= 5);
        *ihl_fits = (realigned_keys[MBY_RE_KEYS_OUTER_IP_LEN] >= (4 * ihl));

        fm_bool outerIhlNot5 = (ihl != 5);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 4, 1, outerIhlNot5);

        fm_bool un1 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 3], 14, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 5, 1, un1);
                             
        fm_bool outerHf = !FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 3], 0, 13);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 6, 1, outerHf);

        fm_bool un2 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 3], 13, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 7, 1, un2);

        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_DS_FLOW] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER];

        // FLOW
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 0, 4, 0);

        realigned_keys    [MBY_RE_KEYS_OUTER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 5] = 0;

        // Outer DIP
        realigned_keys    [MBY_RE_KEYS_OUTER_DIP       ] = in->PA_KEYS      [MBY_PA_KEYS_OUTER_SIPDIP + 2];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_DIP       ] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_SIPDIP + 2];
        realigned_keys    [MBY_RE_KEYS_OUTER_DIP    + 1] = in->PA_KEYS      [MBY_PA_KEYS_OUTER_SIPDIP + 3];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_DIP    + 1] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_SIPDIP + 3];        
        realigned_keys_vld[MBY_PA_KEYS_OUTER_SIPDIP + 2] = 0;
        realigned_keys_vld[MBY_PA_KEYS_OUTER_SIPDIP + 3] = 0;
     }

    // Realign for IPv6:

    if (is_ipv6[1]) // Inner IP Header
    {
        // TTL
        fm_byte un0 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + 5], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 8, 8, un0);

        // PROT
        fm_uint32 uo1 = (in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER + 1]) ? 1 : 5;
        fm_byte   un1 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + uo1], 8, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 0, 8, un1);

        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_TTL_PROT  ] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER + 5];
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 5] = 0;
        
        // LEN
        realigned_keys    [MBY_RE_KEYS_INNER_IP_LEN       ] = in->PA_KEYS      [MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_LEN       ] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 4] = 0;
        
        // DS
        fm_byte un2 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + 2], 4, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 8, 8, un2);

        // MISC
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 4, 4, 0);
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_DS_FLOW] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER + 2];

        // FLOW
        fm_byte un3 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_INNER_IP_HEADER + 2], 0, 4);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 0, 4, un3);

        realigned_keys    [MBY_RE_KEYS_INNER_IP_FLOW] = in->PA_KEYS      [MBY_PA_KEYS_INNER_IP_HEADER + 3];
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_FLOW] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER + 3];
    }

    if (is_ipv6[0]) // Outer IP Header
    {
        // TTL
        fm_byte un0 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 5], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 8, 8, un0);

        // PROT
        fm_uint32 uo1 = (in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER + 1]) ? 1 : 5;
        fm_byte   un1 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + uo1], 8, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8, un1);

        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 5] = 0;
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_TTL_PROT  ] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER + 5];

        // LEN
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realigned_keys    [MBY_RE_KEYS_OUTER_IP_LEN       ] = in->PA_KEYS      [MBY_PA_KEYS_OUTER_IP_HEADER + 4];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_LEN       ] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER + 4];

        // DS
        fm_byte un2 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 2], 4, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 8, 8, un2);

        // MISC
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 4, 4, 0);

        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_DS_FLOW] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER + 2];

        // FLOW
        fm_byte un3 = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_IP_HEADER + 2], 0, 4);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 0, 4, un3);

        realigned_keys    [MBY_RE_KEYS_OUTER_IP_FLOW] = in->PA_KEYS      [MBY_PA_KEYS_OUTER_IP_HEADER + 3];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_FLOW] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER + 3];
    }
}

static void lookUpDomainTcam
(
    const map_domain_tcam_r tcam[mby_ppe_mapper_map_MAP_DOMAIN_TCAM__nd],
    const mbyParserToMapper * const in,
    fm_uint                       * tcam_idx
)
{
    fm_byte   port      = in->RX_PORT;
    fm_uint16 vid1      = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_VLAN1], 0, 12);
    fm_uint16 vid2      = FM_GET_UNNAMED_FIELD(in->PA_KEYS[MBY_PA_KEYS_OUTER_VLAN2], 0, 12);
    fm_byte   vid1Valid = in->PA_FLAGS[MBY_PA_FLAGS_OTR_L2_VLAN1];
    fm_byte   vid2Valid = in->PA_FLAGS[MBY_PA_FLAGS_OTR_L2_VLAN2];

    *tcam_idx = 0; // no match

    // The highest numbered DOMAIN_CAM entry has highest precedence
    for (fm_int i = (mby_ppe_mapper_map_MAP_DOMAIN_TCAM__nd  - 1); i >= 0; i--)
    {
        const map_domain_tcam_r domainTcam = tcam[i];

        fm_byte   maskPort      = domainTcam.PORT_KEY   ^ domainTcam.PORT_KEY_INVERT;
        fm_byte   maskVid1Valid = domainTcam.VID1_VALID ^ domainTcam.VID1_VALID_INVERT;
        fm_uint16 maskVid1      = domainTcam.VID1_KEY   ^ domainTcam.VID1_KEY_INVERT;
        fm_byte   maskVid2Valid = domainTcam.VID2_VALID ^ domainTcam.VID2_VALID_INVERT;
        fm_uint16 maskVid2      = domainTcam.VID2_KEY   ^ domainTcam.VID2_KEY_INVERT;

        // conditions for a TCAM hit:
        fm_bool c0 = (((domainTcam.PORT_KEY    & domainTcam.PORT_KEY_INVERT) == 0) &&
                      (((1 << port) & maskPort) & (domainTcam.PORT_KEY & maskPort)));
        fm_bool c1 = (((domainTcam.VID1_VALID & domainTcam.VID1_VALID_INVERT) == 0) &&
                      ((vid1Valid & maskVid1Valid) == (domainTcam.VID1_VALID & maskVid1Valid)));
        fm_bool c2 = (((domainTcam.VID1_KEY    & domainTcam.VID1_KEY_INVERT) == 0) &&
                      ((vid1 & maskVid1) == (domainTcam.VID1_KEY & maskVid1)));
        fm_bool c3 = (((domainTcam.VID2_VALID  & domainTcam.VID2_VALID_INVERT) == 0) &&
                      ((vid2Valid & maskVid2Valid) == (domainTcam.VID2_VALID & maskVid2Valid)));
        fm_bool c4 = (((domainTcam.VID2_KEY    & domainTcam.VID2_KEY_INVERT)   == 0) &&
                      ((vid2 & maskVid2) == (domainTcam.VID2_KEY & maskVid2)));
        fm_bool c5 = ((FM_GET_UNNAMED_FIELD(domainTcam._RSVD0_, 0, 6) == 0) &&
                      (FM_GET_UNNAMED_FIELD(domainTcam._RSVD1_, 0, 6) == 0));

        if (c0 && c1 && c2 && c3 && c4 && c5) {
            *tcam_idx = i; // TCAM hit
            break; // out of for i loop
        }
    }
}

static void insertDefaults
(
    const mby_ppe_mapper_map_MAP_PORT_DEFAULT_t    port_defs ,
    const mbyParserToMapper                      * const in,
    mbyMapperToClassifier                        * const out,
    const map_port_cfg_r                         * port_cfg,
    fm_uint16                                      realigned_keys    [MBY_N_REALIGN_KEYS],
    fm_bool                                        realigned_keys_vld[MBY_N_REALIGN_KEYS]
)
{
    for (fm_uint i = 0; i < MBY_FFU_N_ACT24; i++) {
        out->FFU_ACTIONS.act24[i].prec = 1;
        out->FFU_ACTIONS.act24[i].val = 0;
    }

    for (fm_uint i = 0; i < MBY_FFU_N_ACT4; i++) {
        out->FFU_ACTIONS.act4[i].prec = 1;
        out->FFU_ACTIONS.act4[i].val = 0;
    }
    for (fm_uint i = 0; i < MBY_FFU_N_ACT1; i++) {
        out->FFU_ACTIONS.act1[i].prec = 1;
        out->FFU_ACTIONS.act1[i].val = 0;
    }

    // Clear SGLORT and DGLORT
    realigned_keys[MBY_RE_KEYS_SGLORT] = 0;
    realigned_keys[MBY_RE_KEYS_DGLORT] = 0;

    // Apply defaults on keys
    for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_PORT_DEFAULT__nd; i++)
    {
        const map_port_default_r port_defaults = port_defs[in->RX_PORT][i];

        fm_byte target = port_defaults.TARGET;
        if (target > MBY_DEFAULT_TARGET_FORCE_KEYS_H)
            continue;

        // target 0..79 apply keys if they are not valid
        if (target <= MBY_DEFAULT_TARGET_KEYS_H)
        {
            if (realigned_keys_vld[target] == 0)
            {
                realigned_keys    [target] = port_defaults.VALUE;
                realigned_keys_vld[target] = 1;
            }
            else if ( (target == MBY_RE_KEYS_OUTER_VLAN1) &&
                      (FM_GET_UNNAMED_FIELD(realigned_keys[target], 0, 12) == 0) )
            {
                fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, 0, 12);
                FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 0, 12, un0);
                realigned_keys_vld[target] = 1;
            }
        }
        // target 80..95 force key value with port default value even if key is valid from parser */
        else if ( (target <= MBY_DEFAULT_TARGET_FORCE_KEYS_H) &&
                  (target >= MBY_DEFAULT_TARGET_FORCE_KEYS_L) )
        {
            realigned_keys    [target - 68] = port_defaults.VALUE;
            realigned_keys_vld[target - 68] = 1;
        }
    }

    // Set DS actions to outer_ip.ds
    if (realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_DS_FLOW])
    {
        fm_uint32 un0 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 10, 4);
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act4[MBY_FFU_ACTION_DSCP_LOW].val, 0, 4, un0);

        fm_uint32 un1 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 14, 2);
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act4[MBY_FFU_ACTION_DSCP_HIGH].val, 0, 2, un1);
    }

    // Set VPRI/VID actions
    if (realigned_keys_vld[MBY_RE_KEYS_OUTER_VLAN1])
    {
        fm_uint32 un0 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 12, 4);
        out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VPRI_LOW].val = un0;
            
        fm_uint32 un1 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 12, 4); // 12? <-- REVISIT!!!
        out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VPRI_HIGH].val = un1;
            
        fm_uint32 un2 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1],  0, 4);
        out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VID_LOW].val = un2;
                                     
        fm_uint32 un3 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1],  4, 4);
        out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VID_MID].val = un3;

        fm_uint32 un4 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1],  8, 4);
        out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VID_HIGH].val = un4;
    }

    // Apply defaults on actions
    for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_PORT_DEFAULT__nd; i++)
    {
        const map_port_default_r port_defaults = port_defs[in->RX_PORT][i];

        fm_byte target = port_defaults.TARGET;
        if (target <= MBY_DEFAULT_TARGET_FORCE_KEYS_H)
           continue;

        // target 96...105 matches lower act24[0...10]
        if ( (target <= MBY_DEFAULT_TARGET_ACT24_L_H) &&
             (target >= MBY_DEFAULT_TARGET_ACT24_L_L) &&
             ((target - MBY_DEFAULT_TARGET_ACT24_L_L) < MBY_FFU_N_ACT24)) {
            fm_uint32 un0 = port_defaults.VALUE;
            FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[target - 96].val, 0, 16, un0);
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT24_U_H) &&
                  (target >= MBY_DEFAULT_TARGET_ACT24_U_L) &&
                  ((target - MBY_DEFAULT_TARGET_ACT24_U_L) < MBY_FFU_N_ACT24)) {
            // target 112...121 matches upper act24[0...10]
            fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, 0, 8);
            FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[target - 112].val, 16, 8, un0);
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT4_4_H) &&
                  (target >= MBY_DEFAULT_TARGET_ACT4_4_L) &&
                  ((target - MBY_DEFAULT_TARGET_ACT4_4_L) < MBY_FFU_N_ACT4)) {
            for (fm_uint j = 0; j < 4; j++) {
                if ((target - MBY_DEFAULT_TARGET_ACT4_4_L + j) < MBY_FFU_N_ACT4) {
                    fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, j * 4, 4);
                    out->FFU_ACTIONS.act4[target - MBY_DEFAULT_TARGET_ACT4_4_L + j].val = un0;
                }
            }
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT4_2_H) &&
                  (target >= MBY_DEFAULT_TARGET_ACT4_2_L) &&
                  ((target - MBY_DEFAULT_TARGET_ACT4_2_L) < MBY_FFU_N_ACT4)) {
            for (fm_uint j = 0; j < 2; j++) {
                if ((target - MBY_DEFAULT_TARGET_ACT4_2_L + j) < MBY_FFU_N_ACT4) {
                    fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, j * 4, 4);
                    out->FFU_ACTIONS.act4[target - MBY_DEFAULT_TARGET_ACT4_2_L + j].val = un0;
                }
            }
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT4_1_H) &&
             (target >= MBY_DEFAULT_TARGET_ACT4_1_L)  &&
             ((target - MBY_DEFAULT_TARGET_ACT4_1_L) < MBY_FFU_N_ACT4)) {
            fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, 0, 4);
            out->FFU_ACTIONS.act4[target - MBY_DEFAULT_TARGET_ACT4_1_L].val = un0;
        }
        else if ( target == MBY_DEFAULT_TARGET_ACT1_FLAGS) {
            for (fm_uint j = 0; j < 16; j++) {
                fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, j, 1);
                out->FFU_ACTIONS.act1[j].val = un0;
            }
        }
    }

    if (port_cfg->DEFAULT_SGLORT_EN) {
        realigned_keys    [MBY_RE_KEYS_SGLORT] = port_cfg->DEFAULT_SGLORT;
        realigned_keys_vld[MBY_RE_KEYS_SGLORT] = 1;
    }

    // Set FFU keys from realigned_keys

    // KEY16
    for (fm_uint i = 0; i < MBY_FFU_N_KEY16; i++)
         out->FFU_KEYS.key16[i] = realigned_keys[i];
 
    // KEY8
    for (fm_uint i = 0; i < MBY_FFU_N_KEY8; i += 2) {
        out->FFU_KEYS.key8[i  ] = FM_GET_UNNAMED_FIELD(realigned_keys[(i >> 1) + MBY_RE_KEYS_GENERAL_8B], 8, 8);
        out->FFU_KEYS.key8[i+1] = FM_GET_UNNAMED_FIELD(realigned_keys[(i >> 1) + MBY_RE_KEYS_GENERAL_8B], 0, 8);
    }

    // KEY32
    for (fm_uint i = 0; i < MBY_FFU_N_KEY32; i++) {
        FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key32[i], 16, 16, realigned_keys[i * 2 + MBY_RE_KEYS_OUTER_SIP]);
        FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key32[i],  0, 16, realigned_keys[i * 2 + MBY_RE_KEYS_OUTER_SIP + 1]);
    }
}

/** Extract a field of 64 or fewer bits from an unnamed 64-bit value. */
#define FM_GET_UNNAMED_FIELD64(lvalue, start, len) \
    ((lvalue >> (start)) & ((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1))) 

static fm_int getTcFromPriSource
(
    const mby_ppe_mapper_map    *        q,
    const mbyParserToMapper     * const in,
    mbyMapperToClassifier       * const out,
    const fm_uint                       domain_index,
    const fm_uint16                     realigned_keys[MBY_N_REALIGN_KEYS],
    const fm_byte                       priority_profile
)
{
    fm_byte priSource = q->MAP_DOMAIN_ACTION0[domain_index].PRI_SOURCE;
    fm_bool forceDefaultPri = q->MAP_DOMAIN_ACTION0[domain_index].FORCE_DEFAULT_PRI;

    fm_int  tc = -1;

    // Traffic class mapping - TC is derived from 4 possible sources.
    for (fm_uint i = 0; i < 4; i++)
    {
        switch (FM_GET_UNNAMED_FIELD(priSource, (6 - (i * 2)), 2))
        {
            case TC_SOURCE_VPRI:
                if (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L2_VLAN1]) {
                    uint4 vpri = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 12, 4);
                    tc = FM_GET_UNNAMED_FIELD64(q->MAP_VPRI_TC[priority_profile].TC_BY_VPRI, (vpri * 3), 3);
                }
                break;

            case TC_SOURCE_MPLS:
                if (in->PA_FLAGS[MBY_PA_FLAGS_OTR_MPLS_V])   {
                    uint3 exp = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_MPLS + 1], 9, 3);
                    tc = FM_GET_UNNAMED_FIELD(q->MAP_EXP_TC[priority_profile].TC_BY_EXP, (exp * 3), 3);
                }
                break;

            case TC_SOURCE_DSCP:
                if (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V])     {
                    uint6 dscp = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 10, 6);
                    tc = q->MAP_DSCP_TC[priority_profile * 64 + dscp].TC;
                }
                break;

            case TC_SOURCE_META:
                // TODO: Handle metadata for packets received from CPU
                break;
        }

        if (tc >= 0)
            break;
    }

    out->NO_PRI_ENC = 0;

    if ((forceDefaultPri) || (tc < 0)) {
        tc = q->MAP_DOMAIN_ACTION0[domain_index].DEFAULT_PRI;
        out->NO_PRI_ENC = 1;
    }

    return tc;
}

static void
compressMac(
       const  mby_ppe_mapper_map_MAP_MAC_t     map_mac,
       const fm_uint16                         realigned_keys[MBY_N_REALIGN_KEYS],
       const uint                              kp,
       const uint2                             whch,
       fm_byte                               * mappedMac,
       mbyMapProfKey1                  * const map_prof_key1,
       fm_bool                               * oMulticast,
       fm_bool                               * oBroadcast
       )
{
  fm_macaddr keyMac = 0;
  FM_SET_UNNAMED_FIELD64(keyMac,  0, 16, realigned_keys[kp + 2]);
  FM_SET_UNNAMED_FIELD64(keyMac, 16, 16, realigned_keys[kp + 1]);
  FM_SET_UNNAMED_FIELD64(keyMac, 32, 16, realigned_keys[kp    ]);

  if (oMulticast) *oMulticast = fmModelIsMulticastMacAddress(keyMac);
  if (oBroadcast) *oBroadcast = fmModelIsBroadcastMacAddress(keyMac);

  for (fm_int i = mby_ppe_mapper_map_MAP_MAC__nd - 1; i >= 0; i--) {
    const map_mac_r m = map_mac[i];
    
    uint64  mask  = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << m.IGNORE_LENGTH;

    if (((m.VALID >> whch) & 1) && (m.MAC == (keyMac & mask))) {
        *mappedMac = m.MAP_MAC; 
        FM_SET_UNNAMED_FIELD(map_prof_key1->MAC_ROUTABLE, whch, 1, m.MAC_ROUTABLE);
        break;
    }
  }
}


       

static void mapScalar
(
    const mby_ppe_mapper_map *       q,
    const mbyParserToMapper  * const in,
    mbyMapperToClassifier    * const out,
    const map_port_cfg_r     *       port_cfg,
    const fm_uint16                  realigned_keys[MBY_N_REALIGN_KEYS],
    const fm_bool                    is_ipv4[MBY_N_IS_IP_BITS],
    const fm_bool                    is_ipv6[MBY_N_IS_IP_BITS],
    const fm_uint                    domain_index,
    const fm_bool                    ihl_ok,
    const fm_bool                    ihl_fits,
    mbyMapProfKey0           * const map_prof_key0,
    mbyMapProfKey1           * const map_prof_key1,
    mbyMappedKey             * const mapped_key,
    fm_byte                  * const pri_profile
)
{
    // initialize
    mapped_key->MAP_INNER_PROT = 0;
    mapped_key->MAP_OUTER_PROT = 0;

    fm_uint64 headerIPhi = 0;
    fm_uint64 headerIPlo = 0;

    mapped_key->MAP_PORT = q->MAP_PORT[in->RX_PORT].MAP_PORT;

    // Compress PROT to 3 bits, highest index match wins
    for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_PROT__nd; i++)   {
        const map_prot_r map_prot = q->MAP_PROT[i];
        
        if (in->PA_FLAGS[MBY_PA_FLAGS_INR_L3_V] &&
            (map_prot.PROT == FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 0, 8)))
          mapped_key->MAP_INNER_PROT = map_prot.MAP_PROT;
        
        if (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V] &&
            (map_prot.PROT == FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8)))
          mapped_key->MAP_OUTER_PROT = map_prot.MAP_PROT;
    }

    fm_bool oDmacMulticast, oDmacBroadcast;

    compressMac(q->MAP_MAC, realigned_keys, MBY_RE_KEYS_OUTER_DMAC, 0, &(mapped_key->MAP_OUTER_DMAC), map_prof_key1, &oDmacMulticast, &oDmacBroadcast);
    compressMac(q->MAP_MAC, realigned_keys, MBY_RE_KEYS_OUTER_SMAC, 1, &(mapped_key->MAP_OUTER_SMAC), map_prof_key1, NULL, NULL);
    if (in->PA_FLAGS[MBY_PA_FLAGS_INR_L2_V]) {
      compressMac(q->MAP_MAC, realigned_keys, MBY_RE_KEYS_INNER_DMAC, 2, &(mapped_key->MAP_INNER_DMAC), map_prof_key1, NULL, NULL);
      compressMac(q->MAP_MAC, realigned_keys, MBY_RE_KEYS_INNER_SMAC, 3, &(mapped_key->MAP_INNER_SMAC), map_prof_key1, NULL, NULL);
    }

    // Map outer L4 SRC ports
    if (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V])   {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_OUTER_L4SRC];
        mapped_key->MAP_OUTER_L4_SRC = realigned_keys[MBY_RE_KEYS_OUTER_L4SRC];
        
        for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_L4_SRC__nd; i++)        {
          const map_l4_src_r cur       = q->MAP_L4_SRC[i];
          fm_bool            next_ok   = ((i + 1) != mby_ppe_mapper_map_MAP_L4_SRC__nd);
          const map_l4_src_r nxt       = next_ok ? q->MAP_L4_SRC[i+1] : (const map_l4_src_r) { 0 };
          
          if ((cur.VALID & 1) && (mapped_key->MAP_OUTER_PROT == cur.MAP_PROT)) {
            fm_uint32 next_map  = (next_ok) ? nxt.L4_SRC : 0;
            fm_uint32 next_prot = (next_ok) ? nxt.MAP_PROT : 0;

            if ((cur.L4_SRC <= temp) && (next_ok || (mapped_key->MAP_OUTER_PROT != next_prot) || (temp < next_map)))
              mapped_key->MAP_OUTER_L4_SRC = cur.MAP_L4_SRC;
          }
        }
    }

    // Map inner L4 SRC ports
    if (in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V])   {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_INNER_L4SRC];
        mapped_key->MAP_INNER_L4_SRC = realigned_keys[MBY_RE_KEYS_INNER_L4SRC];

        
        for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_L4_SRC__nd; i++)        {
          const map_l4_src_r cur       = q->MAP_L4_SRC[i];
          fm_bool            next_ok   = ((i + 1) != mby_ppe_mapper_map_MAP_L4_SRC__nd);
          const map_l4_src_r nxt       = next_ok ? q->MAP_L4_SRC[i+1] : (const map_l4_src_r) { 0 };
          
          if (((cur.VALID >> 1) & 1) && (mapped_key->MAP_INNER_PROT == cur.MAP_PROT)) {
            fm_uint32 next_map  = (next_ok) ? nxt.L4_SRC : 0;
            fm_uint32 next_prot = (next_ok) ? nxt.MAP_PROT : 0;

            if ((cur.L4_SRC <= temp) && (next_ok || (mapped_key->MAP_INNER_PROT != next_prot) || (temp < next_map)))
              mapped_key->MAP_INNER_L4_SRC = cur.MAP_L4_SRC;
          }
        }
    }

    //////////////////////////////////////////////////////////////////////

    // Map outer L4 DST ports
    if (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V])
    {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_OUTER_L4DST];
        mapped_key->MAP_OUTER_L4_DST = realigned_keys[MBY_RE_KEYS_OUTER_L4DST];

        for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_L4_DST__nd; i++)
        {
          const map_l4_dst_r cur = q->MAP_L4_DST[i];
          fm_bool   next_ok   = ((i + 1) != mby_ppe_mapper_map_MAP_L4_DST__nd);
          const map_l4_dst_r nxt = next_ok ? q->MAP_L4_DST[i+1] : (const map_l4_dst_r) { 0 };
          
          if ((cur.VALID & 1) && (mapped_key->MAP_OUTER_PROT == cur.MAP_PROT))  {
            fm_uint32 next_map  = (next_ok) ? nxt.L4_DST : 0;
            fm_uint32 next_prot = (next_ok) ? nxt.MAP_PROT : 0;
            
            if ((cur.L4_DST <= temp) && (next_ok || (mapped_key->MAP_OUTER_PROT != next_prot) || (temp < next_map)))
              mapped_key->MAP_OUTER_L4_DST = cur.MAP_L4_DST;
          }
        }
    }

    // Map inner L4 DST ports
    if (in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V])
    {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_INNER_L4DST];
        mapped_key->MAP_INNER_L4_DST = realigned_keys[MBY_RE_KEYS_INNER_L4DST];

        for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_L4_DST__nd; i++)
        {
          const map_l4_dst_r cur = q->MAP_L4_DST[i];
          fm_bool   next_ok   = ((i + 1) != mby_ppe_mapper_map_MAP_L4_DST__nd);
          const map_l4_dst_r nxt = next_ok ? q->MAP_L4_DST[i+1] : (const map_l4_dst_r) { 0 };
          
          if (((cur.VALID >> 1) & 1) && (mapped_key->MAP_INNER_PROT == cur.MAP_PROT))  {
            fm_uint32 next_map  = (next_ok) ? nxt.L4_DST : 0;
            fm_uint32 next_prot = (next_ok) ? nxt.MAP_PROT : 0;
            
            if ((cur.L4_DST <= temp) && (next_ok || (mapped_key->MAP_INNER_PROT != next_prot) || (temp < next_map)))
              mapped_key->MAP_INNER_L4_DST = cur.MAP_L4_DST;
          }
        }
    }

    // Drive profile_key_t
    FM_SET_UNNAMED_FIELD(map_prof_key0->EX, 0, 1, in->PA_EX_PARSING_DONE);
    FM_SET_UNNAMED_FIELD(map_prof_key0->EX, 1, 1, in->PA_EX_TRUNC_HEADER);
    FM_SET_UNNAMED_FIELD(map_prof_key0->EX, 2, 1, in->PA_EX_DEPTH_EXCEED);

    fm_bool ip_fits = FALSE;
    ip_fits = in->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V] && in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V] &&
        ((in->PA_PTRS[MBY_PA_PTRS_OTR_L4_PTR] - in->PA_PTRS[MBY_PA_PTRS_OTR_L3_PTR]) <= 56);
    FM_SET_UNNAMED_FIELD(map_prof_key0->IP_FITS, 0, 1, ip_fits);

    ip_fits = in->PA_FLAGS[MBY_PA_FLAGS_INR_L3_V] & in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V] &
        ((in->PA_PTRS[MBY_PA_PTRS_INR_L4_PTR] - in->PA_PTRS[MBY_PA_PTRS_INR_L3_PTR]) <= 56);
    FM_SET_UNNAMED_FIELD(map_prof_key0->IP_FITS, 1, 1, ip_fits);

    map_prof_key0->IHL_OK   = ihl_ok;
    map_prof_key0->IHL_FITS = ihl_fits;

    for (fm_uint i = 0; i < (MBY_N_PARSER_FLAGS - 1); i++) {
        FM_SET_UNNAMED_FIELD64(map_prof_key0->FLAGS, i, 1, in->PA_FLAGS[i+1]);
    }

    for(fm_uint i = 0; i < MBY_N_IS_IP_BITS; i++) {
        fm_uint32 un0 = FM_GET_UNNAMED_FIELD(in->PA_CSUM_OK, i, 1);
        FM_SET_UNNAMED_FIELD64(map_prof_key0->CSUM, i, 1, (un0 | (!is_ipv4[i])));
    }

    map_prof_key1->PORT_PROFILE = port_cfg->PORT_PROFILE;

    FM_SET_UNNAMED_FIELD(map_prof_key1->MAC_MBCAST, 0, 1, oDmacMulticast);
    FM_SET_UNNAMED_FIELD(map_prof_key1->MAC_MBCAST, 1, 1, oDmacBroadcast);

    // domain TCAM - domain_index is always valid here
    const map_domain_action0_r map_domain_action0 = q->MAP_DOMAIN_ACTION0[domain_index];
    
    fm_byte priority_profile;

    // TODO: how to get L2 and L3 domains from metadata.
    if (map_domain_action0.UPDATE_DOMAINS) {
        FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(in->PKT_META + 5*4),  0, 4, map_domain_action0.OPERATOR_ID);
        FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(in->PKT_META + 5*4),  4, 9, map_domain_action0.L2_DOMAIN);
        FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)(in->PKT_META + 5*4), 13, 6, map_domain_action0.L3_DOMAIN);

        priority_profile = map_domain_action0.PRIORITY_PROFILE;
    } else {
        fm_byte l2_domain = FM_ARRAY_GET_UNNAMED_FIELD(((fm_uint32*)(in->PKT_META + 5*4)), 4, 9);

        priority_profile = q->MAP_DOMAIN_PROFILE[l2_domain].PRIORITY_PROFILE;
    }

    out->PRIORITY_PROFILE = priority_profile;

    map_prof_key1->L2_DOMAIN = FM_ARRAY_GET_UNNAMED_FIELD(((fm_uint32*)(in->PKT_META + 5*4)),  4, 9);
    map_prof_key1->L3_DOMAIN = FM_ARRAY_GET_UNNAMED_FIELD(((fm_uint32*)(in->PKT_META + 5*4)), 13, 6);

    out->FFU_ACTIONS.act1[MBY_FFU_ACTION_LEARN].val = map_domain_action0.LEARN_EN;

    out->LEARN_MODE = map_domain_action0.LEARN_MODE;

    fm_byte tc = getTcFromPriSource(q, in, out, domain_index, realigned_keys, priority_profile);
    out->FFU_ACTIONS.act4[MBY_FFU_ACTION_TC].val = tc;

    const map_domain_action1_r map_domain_action1 = q->MAP_DOMAIN_ACTION1[domain_index];

    fm_uint16 l2Policer           = map_domain_action1.L2_POLICER;
    fm_uint16 l3Policer           = map_domain_action1.L3_POLICER;
    map_prof_key1->DOMAIN_PROFILE = map_domain_action1.DOMAIN_PROFILE;
    out->L2_IVLAN1_CNT_INDEX      = map_domain_action1.VLAN_COUNTER;

    if (l2Policer != 0) {
        // if l2_policer is nonzero, then the default POLICER[0] action is (bank=0, index=l2_policer).
        out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER0].val = 0;
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER0].val, 23,  1, 1);
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER0].val, 20,  3, q->MAP_DOMAIN_POL_CFG.L2_COLOR_CFG);
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER0].val,  0, 12, (l2Policer & 0xFFF));
    }

    if (l3Policer != 0) {
        // if l3_policer is nonzero, then the default POLICER[1] action is (bank=5, index=l3_policer).
        out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER1].val = 0;
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER1].val, 23,  1, 1);
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER1].val,  0, 12, (l3Policer & 0xFFF));
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER1].val, 20,  3, q->MAP_DOMAIN_POL_CFG.L3_COLOR_CFG);
        FM_SET_UNNAMED_FIELD(out->FFU_ACTIONS.act24[MBY_FFU_ACTION_POLICER1].val, 16,  4, 5);
    }

    // Write PKT_META to keys: MD write block
    // TODO: may be removed in the future.
    for (fm_uint i = 0; i < 32; i++)
        out->FFU_KEYS.key8[63-i] = in->PKT_META[i];

    *pri_profile = priority_profile;
}

static void encodeLength
(
    const fm_byte   hdrLen,
    const fm_bool   hdrValid,
    const fm_byte   min,
    const fm_byte   max,
    const fm_byte   offset,
    const fm_bool   fits,
    fm_bool * const ptrsErr,
    fm_byte * const len
)
{
    if ((max < min) || (max < offset)) { /* WARNING: max is less than min, or max is less than offset */ }

    *len = 0; // default

    if (!fits || (*ptrsErr) || !hdrValid)
        ;
    else if (hdrLen > max)
        *len  = ((max >> 2) & 0x3f) - ((offset >> 2) & 0x3f);
    else if (hdrLen < min)
        *ptrsErr = TRUE;
    else if ((hdrLen & 3) != (offset & 3))
        *len = ((hdrLen - offset) >> 2) & 0x3f; // 5 bits
    else
        *len = ((hdrLen >> 2) & 0x3f) - ((offset >> 2) & 0x3f);
}

static void getParserInfo
(
    const mby_ppe_mapper_map_MAP_LEN_LIMIT_t MAP_LEN_LIMIT,
    const mbyParserToMapper * const in, 
    const fm_uint16                 realigned_keys[MBY_N_REALIGN_KEYS],
    const fm_bool                   is_ipv6[MBY_N_IS_IP_BITS],
    mbyMapProfKey0          * const map_prof_key0,
    mbyParserInfo           * const parser_info
)
{
    fm_byte outerProt = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8);

    fm_bool tcp[2] = { FALSE };
    fm_bool udp[2] = { FALSE };

    tcp[0] = (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V]) && (outerProt == MBY_PROT_TCP);
    udp[0] = (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V]) && (outerProt == MBY_PROT_UDP);

    fm_byte innerProt = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 0, 8);

    tcp[1] = (in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V]) && (innerProt == MBY_PROT_TCP);
    udp[1] = (in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V]) && (innerProt == MBY_PROT_UDP);

    fm_byte ptrs[8] = { 0 };
    ptrs[MBY_PA_INFO_OTR_L2] = 0;
    for (fm_int i = MBY_PA_INFO_OTR_MPLS; i <= MBY_PA_INFO_INR_L4; i++)
        ptrs[i] = in->PA_PTRS[i];

    fm_bool hdrValid[8] = { FALSE };

    hdrValid[MBY_PA_INFO_OTR_L2]   = TRUE;
    hdrValid[MBY_PA_INFO_OTR_MPLS] = in->PA_FLAGS[MBY_PA_FLAGS_OTR_MPLS_V];
    hdrValid[MBY_PA_INFO_OTR_L3]   = in->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V];
    hdrValid[MBY_PA_INFO_OTR_L4]   = in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V];
    hdrValid[MBY_PA_INFO_INR_L2]   = in->PA_FLAGS[MBY_PA_FLAGS_INR_L2_V];
    hdrValid[MBY_PA_INFO_INR_MPLS] = in->PA_FLAGS[MBY_PA_FLAGS_INR_MPLS_V];
    hdrValid[MBY_PA_INFO_INR_L3]   = in->PA_FLAGS[MBY_PA_FLAGS_INR_L3_V];
    hdrValid[MBY_PA_INFO_INR_L4]   = in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V];

    fm_byte hdrLen[8] = { 0 };
    fm_byte nextHdrStart = in->PA_ADJ_SEG_LEN;
    for (fm_int i = MBY_PA_INFO_INR_L4; i >= MBY_PA_INFO_OTR_L2; i--) {
        hdrLen[i] = nextHdrStart - ptrs[i];
        if (in->PA_PTRS_VALID[i])
            nextHdrStart = ptrs[i];
    }

    /* The parser always sets pr.ptrs[OTR_L3] to indicate the end of the MPLS
       stack (either BOS or the point where the parser has decided to stop
       looking deeper in the MPLS stack).
       parser_info will require pr.ptrs[OTR_L3] to determine the end of the
       MPLS stack in cases where the IP header is not recognized or has an EOS
       exception, or the MPLS stack was not fully parsed. */

    // maximum number of bytes per config

    const map_len_limit_r limits = MAP_LEN_LIMIT[in->RX_PORT];
    
    fm_byte hdrLenLimit[8] = { 0 };

    hdrLenLimit[MBY_PA_INFO_OTR_L2]   = limits.OTR_L2_LEN_LIMIT * 4 + 14;
    hdrLenLimit[MBY_PA_INFO_INR_L2]   = limits.INR_L2_LEN_LIMIT * 4 + 14;

    hdrLenLimit[MBY_PA_INFO_OTR_MPLS] = limits.OTR_MPLS_LEN_LIMIT * 4;
    hdrLenLimit[MBY_PA_INFO_INR_MPLS] = limits.INR_MPLS_LEN_LIMIT * 4;

    hdrLenLimit[MBY_PA_INFO_OTR_L3]   = MBY_OTR_L3_LEN_LIMIT * 4;
    hdrLenLimit[MBY_PA_INFO_INR_L3]   = MBY_INR_L3_LEN_LIMIT * 4;

    hdrLenLimit[MBY_PA_INFO_OTR_L4]   = (tcp[0]) ? MBY_L4_TCP_MIN_SIZE : MBY_OTR_TUN_LEN_LIMIT * 4;
    hdrLenLimit[MBY_PA_INFO_INR_L4]   = (tcp[1]) ? MBY_L4_TCP_MIN_SIZE : MBY_L4_MIN_SIZE;

    fm_bool fits = TRUE;
    fm_bool ptrsErr = FALSE;

    // otr_l2
    encodeLength
    (
        hdrLen[MBY_PA_INFO_OTR_L2],
        hdrValid[MBY_PA_INFO_OTR_L2],
        14,
        hdrLenLimit[MBY_PA_INFO_OTR_L2],
        10,
        fits,
        &ptrsErr,
        &parser_info->otr_l2_len
    );

    parser_info->otr_l2_len    &= 0x7;
    parser_info->otr_l2_vlan1   = (parser_info->otr_l2_len != 0) && in->PA_FLAGS[MBY_PA_FLAGS_OTR_L2_VLAN1];
    parser_info->otr_l2_vlan2   = (parser_info->otr_l2_len != 0) && in->PA_FLAGS[MBY_PA_FLAGS_OTR_L2_VLAN2];
    parser_info->otr_l2_v2first = (parser_info->otr_l2_len != 0) && in->PA_FLAGS[MBY_PA_FLAGS_OTR_L2_V2FIRST];

    if (parser_info->otr_l2_len > 5) { /* WARNING: illegal parser_info->otr_l2_len */ }

    // otr_mpls
    encodeLength
    (
        hdrLen[MBY_PA_INFO_OTR_MPLS],
        hdrValid[MBY_PA_INFO_OTR_MPLS],
        0,
        hdrLenLimit[MBY_PA_INFO_OTR_MPLS],
        0,
        fits,
        &ptrsErr,
        &parser_info->otr_mpls_len
    );

    parser_info->otr_mpls_len &= 0x7;

    if (parser_info->otr_mpls_len > 7) { /* WARNING: illegal parser_info->otr_mpls_len */ }

    // otr_l3
    encodeLength
    (
        hdrLen[MBY_PA_INFO_OTR_L3],
        hdrValid[MBY_PA_INFO_OTR_L3],
        20,
        hdrLenLimit[MBY_PA_INFO_OTR_L3],
        0,
        fits,
        &ptrsErr,
        &parser_info->otr_l3_len
    );

    parser_info->otr_l3_len &= 0xF;
    parser_info->otr_l3_v6   = is_ipv6[0];

    if (parser_info->otr_l3_len > 14) { /* WARNING: illegal parser_info->otr_l3_len */ }
    
    // otr_l4
    parser_info->otr_l4_udp = fits && !ptrsErr && (hdrLen[MBY_PA_INFO_OTR_L4] >=  8) && in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V] && udp[0];
    parser_info->otr_l4_tcp = fits && !ptrsErr && (hdrLen[MBY_PA_INFO_OTR_L4] >= 18) && in->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V] && tcp[0];

    fm_byte otrL4Min    = (tcp[0]) ? MBY_L4_TCP_MIN_SIZE : ((udp[0]) ? MBY_L4_MIN_SIZE : 4);
    fm_byte otrL4Offset = (tcp[0]) ? MBY_L4_TCP_MIN_SIZE : ((udp[0]) ? MBY_L4_MIN_SIZE : 0);

    encodeLength
    (
        hdrLen[MBY_PA_INFO_OTR_L4],
        hdrValid[MBY_PA_INFO_OTR_L4],
        otrL4Min,
        hdrLenLimit[MBY_PA_INFO_OTR_L4],
        otrL4Offset,
        fits,
        &ptrsErr,
        &parser_info->otr_tun_len
    );

    parser_info->otr_tun_len &= 0x1F;

    if (parser_info->otr_tun_len > 18) { /* WARNING: illegal parser_info->otr_tun_len */ }

    // stop if Outer L4 is present and not UDP
    fits &= (udp[0] || !hdrValid[MBY_PA_INFO_OTR_L4]);

    // inr_l2
    encodeLength
    (
        hdrLen[MBY_PA_INFO_INR_L2],
        hdrValid[MBY_PA_INFO_INR_L2],
        14,
        hdrLenLimit[MBY_PA_INFO_INR_L2],
        10,
        fits,
        &ptrsErr,
        &parser_info->inr_l2_len
    );

    parser_info->inr_l2_len    &= 0x7;
    parser_info->inr_l2_vlan1   = (parser_info->inr_l2_len != 0) && in->PA_FLAGS[MBY_PA_FLAGS_INR_L2_VLAN1];
    parser_info->inr_l2_vlan2   = (parser_info->inr_l2_len != 0) && in->PA_FLAGS[MBY_PA_FLAGS_INR_L2_VLAN2];
    parser_info->inr_l2_v2first = (parser_info->inr_l2_len != 0) && in->PA_FLAGS[MBY_PA_FLAGS_INR_L2_V2FIRST];

    if (parser_info->inr_l2_len > 5) { /* WARNING: illegal parser_info->inr_l2_len */ }

    // inr_mpls
    encodeLength
    (
        hdrLen[MBY_PA_INFO_INR_MPLS],
        hdrValid[MBY_PA_INFO_INR_MPLS],
        0,
        hdrLenLimit[MBY_PA_INFO_INR_MPLS],
        0,
        fits,
        &ptrsErr,
        &parser_info->inr_mpls_len
    );

    parser_info->inr_mpls_len &= 0x7;
    if (parser_info->inr_mpls_len > 7) {/* WARNING: illegal parser_info->inr_mpls_len */ }

    // inr_l3
    encodeLength
    (
        hdrLen[MBY_PA_INFO_INR_L3],
        hdrValid[MBY_PA_INFO_INR_L3],
        20,
        hdrLenLimit[MBY_PA_INFO_INR_L3],
        0,
        fits,
        &ptrsErr,
        &parser_info->inr_l3_len
    );

    parser_info->inr_l3_len &= 0xF;
    parser_info->inr_l3_v6   = is_ipv6[1];

    if (parser_info->inr_l3_len > 14) { /* WARNING: illegal parser_info->inr_l3_len */ }

    // inr_l4
    fm_byte inrL4Len = 0;

    encodeLength
    (
        hdrLen[MBY_PA_INFO_INR_L4],
        hdrValid[MBY_PA_INFO_INR_L4],
        ((tcp[1]) ? MBY_L4_TCP_MIN_SIZE : MBY_L4_MIN_SIZE),
        hdrLenLimit[MBY_PA_INFO_INR_L4],
        0,
        fits,
        &ptrsErr,
        &inrL4Len
    );

    parser_info->inr_l4_udp = inrL4Len && in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V] && udp[1];
    parser_info->inr_l4_tcp = inrL4Len && in->PA_FLAGS[MBY_PA_FLAGS_INR_L4_V] && tcp[1];

    map_prof_key0->PTRS_ERR = ptrsErr;
}

static void getProfile
(
        const mby_ppe_mapper_map *       q,
        const mbyParserToMapper * const in,
        mbyMapperToClassifier   * const out,
        const fm_uint16                 realigned_keys[MBY_N_REALIGN_KEYS],
        const mbyMapProfKey0            key0,
        const mbyMapProfKey1            key1,
        mbyMapProfAction        * const mapProfAction
)
{
    fm_byte profileIdx = 0;
    fm_byte trigIdx    = 0;
    fm_byte priosIdx   = 0;

    for (fm_int i = mby_ppe_mapper_map_MAP_PROFILE_KEY0__nd - 1; i >= 0; i--)
    {
      const map_profile_key0_r          profKey0   = q->MAP_PROFILE_KEY0[i];
      const map_profile_key_invert0_r   profMask0  = q->MAP_PROFILE_KEY_INVERT0[i];
      const map_profile_key1_r          profKey1   = q->MAP_PROFILE_KEY1[i];
      const map_profile_key_invert1_r   profMask1  = q->MAP_PROFILE_KEY_INVERT1[i];
      const map_profile_action_r        profAction = q->MAP_PROFILE_ACTION[i];

#define CHECK(z,field,mask)                                   \
        if ( (((~key##z .field ) & (mask)) & profKey##z.field ) || \
             (   key##z.field             & profMask##z.field ) )     \
            continue

      CHECK(0,PTRS_ERR, 0x1);
      CHECK(0,EX, 0x7);
      CHECK(0,CSUM, 0x3);
      CHECK(0,IHL_OK, 0x1);
      CHECK(0,IHL_FITS, 0x1);
      CHECK(0,FLAGS, 0x7FFFFFFFFFFF);
      CHECK(1,L2_DOMAIN, ~0);
      CHECK(1,L3_DOMAIN,0x3F);
      CHECK(1,PORT_PROFILE, 0xF);
      CHECK(1,DOMAIN_PROFILE, 0xFF);
      CHECK(1,MAC_ROUTABLE, 0xF);
      CHECK(1,MAC_MBCAST, 0x3);

#undef CHECK
      // TODO: add packet type check
      
      if (profAction.PROFILE_VALID && (profileIdx == 0))        {
        profileIdx                     = i;
        mapProfAction->PROFILE_VALID   = 1;
        mapProfAction->PROFILE         = profAction.PROFILE;
        mapProfAction->REWRITE_PROFILE = profAction.REWRITE_PROFILE;
      }
      
      if (profAction.TRIG_VALID  && (trigIdx == 0))        {
        trigIdx                        = i;
        mapProfAction->TRIG_VALID      = 1;
        mapProfAction->PROFILE_TRIG    = profAction.PROFILE_TRIG;
        mapProfAction->IP_OPTIONS_MASK = profAction.IP_OPTIONS_MASK;
        mapProfAction->PARSER_ERROR    = profAction.PARSER_ERROR;
      }
      
      if (profAction.PRIOS_VALID && (priosIdx == 0))        {
        priosIdx                   = i;
        mapProfAction->PRIOS_VALID = 1;
        mapProfAction->VPRI_TGT    = profAction.VPRI_TGT;
        mapProfAction->DSCP_TGT    = profAction.DSCP_TGT;
      }
      
      // Found all types
      if ((profileIdx > 0) && (trigIdx > 0) && (priosIdx > 0))
        break;
    } // rof (i=0...)
    
    out->FFU_SCENARIO = mapProfAction->PROFILE;

    // Set Scenario action
    for (fm_uint i = 0; i < 6; i++)
        out->FFU_ACTIONS.act1[MBY_FFU_ACTION_SCENARIO0 + i].val = (out->FFU_SCENARIO >> i) & 1;
}

static void rewriteSourceNybble
(
    const mbyParserToMapper * const in,
    mbyMapperToClassifier   * const out,
    const fm_bool             is_ipv6[MBY_N_IS_IP_BITS],
    const mbyMappedKey        mapped_key,
    const mbyMapProfKey0      map_prof_key0,
    const fm_uint             nybble_idx,
    const fm_uint             sourceId
)
{
    fm_uint keyIdx = 0;
    fm_uint keyOff = 0;

    if (nybble_idx < 4)
    {
        keyIdx = 13;
        keyOff = nybble_idx * 4;
    }
    else if (nybble_idx < 8)
    {
        keyIdx = 19;
        keyOff = (nybble_idx % 4) * 4;
    }
    else if (nybble_idx < 24)
    {
        keyIdx = (nybble_idx - 8) / 2;
        keyOff = (nybble_idx % 2) * 4;
    }
    else if (nybble_idx <= 31)
    {
        keyIdx = 16 + (nybble_idx - 24) / 2;
        keyOff = (nybble_idx % 2) * 4;
    }
    else
        return;

    fm_byte val    = 0;

    if (sourceId == SOURCE_NOOP)
        return;
    else if (sourceId == SOURCE_MAP_OUTER_PROT)
        val = mapped_key.MAP_OUTER_PROT;
    else if (sourceId == SOURCE_MAP_OUTER_DMAC_H)
        val = mapped_key.MAP_OUTER_DMAC >> 4;
    else if (sourceId == SOURCE_MAP_OUTER_DMAC_L)
        val = mapped_key.MAP_OUTER_DMAC >> 0;
    else if (sourceId == SOURCE_MAP_OUTER_SMAC_H)
        val = mapped_key.MAP_OUTER_SMAC >> 4;
    else if (sourceId == SOURCE_MAP_OUTER_SMAC_L)
        val = mapped_key.MAP_OUTER_SMAC >> 0;
    else if (sourceId == SOURCE_MAP_PORT_H)
        val = mapped_key.MAP_PORT >> 4;
    else if (sourceId == SOURCE_MAP_PORT_L)
            val = mapped_key.MAP_PORT >> 0;
    else if (sourceId >= SOURCE_MAP_OUTER_L4_SRC_L &&
             sourceId <= SOURCE_MAP_OUTER_L4_SRC_H)
        val = mapped_key.MAP_OUTER_L4_SRC >> ((SOURCE_MAP_OUTER_L4_SRC_H - sourceId) * 4);
    else if (sourceId >= SOURCE_MAP_OUTER_L4_DST_L &&
             sourceId <= SOURCE_MAP_OUTER_L4_DST_H)
        val = mapped_key.MAP_OUTER_L4_DST >> ((SOURCE_MAP_OUTER_L4_DST_H - sourceId) * 4);
    else if (sourceId >= SOURCE_PA_FLAGS_L &&
             sourceId <= SOURCE_PA_FLAGS_H)
    {
        val  = in->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId) * 4)];
        val |= (in->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId) * 4) + 1] << 1);
        val |= (in->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId) * 4) + 2] << 2);
        val |= (in->PA_FLAGS[((SOURCE_PA_FLAGS_H - sourceId) * 4) + 3] << 3);
    }
    else if (sourceId == SOURCE_FFU_PROFILE_L)
        /* only 6 bits */
        val = (out->FFU_SCENARIO & 0x3F) >> 4;
    else if (sourceId == SOURCE_FFU_PROFILE_H)
        /* only 6 bits */
        val = (out->FFU_SCENARIO & 0x3F) >> 0;
    else if (sourceId == SOURCE_MAP_INNER_PROT)
        val = mapped_key.MAP_INNER_PROT;
    else if (sourceId == SOURCE_MAP_INNER_DMAC_H)
        val = mapped_key.MAP_INNER_DMAC >> 4;
    else if (sourceId == SOURCE_MAP_INNER_DMAC_L)
        val = mapped_key.MAP_INNER_DMAC >> 0;
    else if (sourceId == SOURCE_MAP_INNER_SMAC_H)
        val = mapped_key.MAP_INNER_SMAC >> 4;
    else if (sourceId == SOURCE_MAP_INNER_SMAC_L)
        val = mapped_key.MAP_INNER_SMAC >> 0;
    else if (sourceId >= SOURCE_MAP_INNER_L4_SRC_L &&
             sourceId <= SOURCE_MAP_INNER_L4_SRC_H)
        val = mapped_key.MAP_INNER_L4_SRC >> ((SOURCE_MAP_INNER_L4_SRC_H - sourceId) * 4);
    else if (sourceId >= SOURCE_MAP_INNER_L4_DST_L &&
             sourceId <= SOURCE_MAP_INNER_L4_DST_H)
        val = mapped_key.MAP_INNER_L4_DST >> ((SOURCE_MAP_INNER_L4_DST_H - sourceId) * 4);
    else if (sourceId == SOURCE_EX)
    {
        val = 0;
        FM_SET_UNNAMED_FIELD(val, 0, 1, in->PA_EX_PARSING_DONE);
        FM_SET_UNNAMED_FIELD(val, 1, 1, in->PA_EX_TRUNC_HEADER);
        FM_SET_UNNAMED_FIELD(val, 2, 1, in->PA_EX_DEPTH_EXCEED);
    }
    else if (sourceId == SOURCE_CSUM)
        val = map_prof_key0.CSUM;
    else if (sourceId == SOURCE_IP_INFO)
    {
        FM_SET_UNNAMED_FIELD64(val, 0, 1, is_ipv6[1]);
        FM_SET_UNNAMED_FIELD64(val, 1, 1, is_ipv6[0]);
        val |= (map_prof_key0.IP_FITS & 0x3) << 2;
    }
    /* TODO: add packet type handle. */
    else
    {
         /* Unhandled sourceId. */
        return;
    }

    if (nybble_idx < 8)    {
        FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key16[keyIdx], keyOff, 4, val);
    }
    else if (nybble_idx <= 31)    {
        FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key8[keyIdx], keyOff, 4, val);
    }
}

static void mapRewrite
(
    const mby_ppe_mapper_map    *        q,
    const mbyParserToMapper * const in,
    mbyMapperToClassifier   * const out,
    const fm_uint16                 realigned_keys[MBY_N_REALIGN_KEYS],
    const fm_bool                   realigned_keys_vld[MBY_N_REALIGN_KEYS],
    const fm_bool                   is_ipv6[MBY_N_IS_IP_BITS],
    mbyMapProfAction                map_prof_action,
    mbyMapProfKey0                  map_prof_key0,
    mbyMappedKey                    mapped_key,
    fm_byte                         priority_profile
)
{
    // Rewrite Keys
    if (map_prof_action.PROFILE_VALID)
    {
        for (fm_uint i = 0; i < mby_ppe_mapper_map_MAP_REWRITE__nd ; i++)
        {
            uint6 source_id  = q->MAP_REWRITE[map_prof_action.REWRITE_PROFILE][i].SRC_ID;
            fm_uint nybble_idx = i;

            rewriteSourceNybble
            (
                in,
                out,
                is_ipv6,
                mapped_key,
                map_prof_key0,
                nybble_idx,
                source_id
            );
        }
    }

    out->PARSER_ERROR = 0;

    if (map_prof_action.TRIG_VALID == 1)
    {
        fm_uint   otr_opt_flags;
        fm_uint   inr_opt_flags;
        fm_uint16 otr_l3_len;
        fm_uint16 inr_l3_len;

        // trig and ip_option mask
        for (fm_uint i = 0; i < 8; i++)
            out->FFU_ACTIONS.act1[MBY_FFU_ACTION_TRIGGER0 + i].val = (map_prof_action.PROFILE_TRIG >> i) & 1;

        otr_l3_len = 0;
        inr_l3_len = 0;

        FM_SET_UNNAMED_FIELD64(otr_l3_len, 0, 8, out->FFU_KEYS.key8[MBY_FFU_KEY8_OUTER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(otr_l3_len, 8, 8, out->FFU_KEYS.key8[MBY_FFU_KEY8_OUTER_LEN]);
        FM_SET_UNNAMED_FIELD64(inr_l3_len, 0, 8, out->FFU_KEYS.key8[MBY_FFU_KEY8_INNER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(inr_l3_len, 8, 8, out->FFU_KEYS.key8[MBY_FFU_KEY8_INNER_LEN]);

        otr_opt_flags = (in->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V] && !is_ipv6[0] && (otr_l3_len > 20)) ? (1 << 6) : 0;
        inr_opt_flags = (in->PA_FLAGS[MBY_PA_FLAGS_INR_L3_V] && !is_ipv6[1] && (inr_l3_len > 20)) ? (1 << 6) : 0;
        
        for (fm_uint i = 0; i <= 5; i++) {
            FM_SET_UNNAMED_FIELD(otr_opt_flags, i, 1, in->PA_FLAGS[i + 32]);
            FM_SET_UNNAMED_FIELD(inr_opt_flags, i, 1, in->PA_FLAGS[i + 38]);
        }

        out->IP_OPTION[0] = (map_prof_action.IP_OPTIONS_MASK & otr_opt_flags) ? 1 : 0;
        out->IP_OPTION[1] = (map_prof_action.IP_OPTIONS_MASK & inr_opt_flags) ? 1 : 0;
        out->PARSER_ERROR = map_prof_action.PARSER_ERROR;
    }

    if (map_prof_action.PRIOS_VALID == 1)
    {
        const uint64 map_vpri = q->MAP_VPRI[priority_profile].VPRI_BY_VPRI;
        
        /* vpri_tgt and dscp_tgt, no valid check is needed */
        if (map_prof_action.VPRI_TGT & 0x4) {
            /* NOTE: This is place before (vpri_tgt & 0x1) otherwise
             * vpri will be remapped twice if both bits are set */
            fm_int vpri     = FM_GET_UNNAMED_FIELD(out->FFU_KEYS.key16[MBY_FFU_KEY16_OUTER_VLAN1], 12, 4);
            fm_int map_vpri = FM_GET_UNNAMED_FIELD64(map_vpri, vpri * 4, 4);
            out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VPRI_LOW].val = map_vpri;
            out->FFU_ACTIONS.act4[MBY_FFU_ACTION_VPRI_HIGH].val = map_vpri;
        }

        if (map_prof_action.VPRI_TGT & 0x1) {
            fm_int vpri     = FM_GET_UNNAMED_FIELD(out->FFU_KEYS.key16[MBY_FFU_KEY16_OUTER_VLAN1], 12, 4);
            fm_int map_vpri = FM_GET_UNNAMED_FIELD64(map_vpri, vpri * 4, 4);
            FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key16[MBY_FFU_KEY16_OUTER_VLAN1], 12, 4, map_vpri)
        }

        if (map_prof_action.VPRI_TGT & 0x2) {
            fm_int vpri     = FM_GET_UNNAMED_FIELD(out->FFU_KEYS.key16[MBY_FFU_KEY16_INNER_VLAN1], 12, 4);
            fm_int map_vpri = FM_GET_UNNAMED_FIELD64(map_vpri, vpri * 4, 4);
            FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key16[MBY_FFU_KEY16_INNER_VLAN1], 12, 4, map_vpri);
        }

        if (map_prof_action.DSCP_TGT & 0x4) {
          /* NOTE: This is place before (dscp_tgt & 0x1) otherwise
             * dscp will be remapped twice if both bits are set */
            uint6 dscp = FM_GET_UNNAMED_FIELD(out->FFU_KEYS.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6);
            uint6 map_dscp = q->MAP_DSCP_TC[priority_profile * 64 + dscp].DSCP;
            out->FFU_ACTIONS.act4[MBY_FFU_ACTION_DSCP_LOW].val  = map_dscp & 0xF;
            out->FFU_ACTIONS.act4[MBY_FFU_ACTION_DSCP_HIGH].val = (map_dscp >> 4);
        }

        if (map_prof_action.DSCP_TGT & 0x1)        {
            fm_int dscp = FM_GET_UNNAMED_FIELD(out->FFU_KEYS.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6);
            uint6 map_dscp = q->MAP_DSCP_TC[priority_profile * 64 + dscp].DSCP;
            FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6, map_dscp);
        }

        if (map_prof_action.DSCP_TGT & 0x2)        {
            fm_int dscp = FM_GET_UNNAMED_FIELD(out->FFU_KEYS.key8[MBY_FFU_KEY8_INNER_DS], 2, 6);
            uint6 map_dscp = q->MAP_DSCP_TC[priority_profile * 64 + dscp].DSCP;
            FM_SET_UNNAMED_FIELD(out->FFU_KEYS.key8[MBY_FFU_KEY8_INNER_DS], 2, 6, map_dscp);
        }
    }
}

void
mbyMapper
(
    const mby_ppe_mapper_map    *        q,
    const mbyParserToMapper     * const in, 
          mbyMapperToClassifier * const mapper_to_classifier,
          mbyParserToModifier   * const parser_to_modifier
)
{
    const map_port_cfg_r portCfg = q->MAP_PORT_CFG[in->RX_PORT];
   
    fm_bool isIPv4[MBY_N_IS_IP_BITS] = { FALSE };
    fm_bool isIPv6[MBY_N_IS_IP_BITS] = { FALSE };

    // INNER IP
    if(in->PA_FLAGS[MBY_PA_FLAGS_INR_L3_V]) {
        isIPv4[1] = in->PA_KEYS_VALID[MBY_PA_KEYS_INNER_IP_HEADER];
        isIPv6[1] = !isIPv4[1];
    }

    // OUTER IP
    if(in->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V]) {
        isIPv4[0] = in->PA_KEYS_VALID[MBY_PA_KEYS_OUTER_IP_HEADER];
        isIPv6[0] = !isIPv4[0];
    }

    fm_uint16 realigned_keys    [MBY_N_REALIGN_KEYS];
    fm_bool   realigned_keys_vld[MBY_N_REALIGN_KEYS];

    fm_bool ihl_ok   = FALSE;
    fm_bool ihl_fits = FALSE;

    // TODO: Realign keys stage will probably be removed when spec provide more details.
    realignKeys
    (
        in,
        isIPv4,
        isIPv6,
        realigned_keys,
        realigned_keys_vld,
        &ihl_ok,
        &ihl_fits
    );

    fm_uint domain_index = 0;

    lookUpDomainTcam
    (
        q->MAP_DOMAIN_TCAM,
        in,
        &domain_index
    );

    insertDefaults
    (
        q->MAP_PORT_DEFAULT,
        in,
        mapper_to_classifier,
        &portCfg,
        realigned_keys,
        realigned_keys_vld
    );

    mbyMapProfKey0 map_prof_key0 = { 0 };
    mbyMapProfKey1 map_prof_key1 = { 0 };
    mbyMappedKey   mapped_key = { 0 };

    fm_byte pri_profile = 0; // priority profile

    mapScalar
    (
        q,
        in,
        mapper_to_classifier,
        &portCfg,
        realigned_keys,
        isIPv4,
        isIPv6,
        domain_index,
        ihl_ok,
        ihl_fits,
        &map_prof_key0,
        &map_prof_key1,
        &mapped_key,
        &pri_profile
    );
    
    getParserInfo
    (
        q->MAP_LEN_LIMIT,
        in,
        realigned_keys,
        isIPv6,
        &map_prof_key0,
        &parser_to_modifier->PARSER_INFO
    );

    mbyMapProfAction map_prof_action;

    map_prof_action.PROFILE         = 0;
    map_prof_action.PROFILE_VALID   = FALSE;
    map_prof_action.REWRITE_PROFILE = FALSE;

    getProfile
    (
        q,
        in,
        mapper_to_classifier,
        realigned_keys,
        map_prof_key0,
        map_prof_key1,
        &map_prof_action
    );

    mapRewrite
    (
        q,
        in,
        mapper_to_classifier,
        realigned_keys,
        realigned_keys_vld,
        isIPv6,
        map_prof_action,
        map_prof_key0,
        mapped_key,
        pri_profile
    );
}
