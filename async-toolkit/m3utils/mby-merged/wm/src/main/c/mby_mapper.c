// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_parser.h"
#include "mby_mapper.h"
#include "mby_classifier.h"

static mbyMapPortCfg getPortCfg
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            rx_port
)
{
    mbyMapPortCfg port_cfg;

    map_port_cfg_r const * const map_port_cfg = &(mapper_map->MAP_PORT_CFG[rx_port]);

    port_cfg.DEFAULT_SGLORT    = map_port_cfg->DEFAULT_SGLORT;
    port_cfg.DEFAULT_SGLORT_EN = map_port_cfg->DEFAULT_SGLORT_EN;
    port_cfg.PORT_PROFILE      = map_port_cfg->PORT_PROFILE;
    return port_cfg;
}

static mbyMapPortDefaults getPortDefaults
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            rx_port,
    fm_byte   const            entry
)
{
    mbyMapPortDefaults port_defaults;

    map_port_default_r const * const map_port_default = &(mapper_map->MAP_PORT_DEFAULT[rx_port][entry]);

    port_defaults.TARGET = map_port_default->TARGET;
    port_defaults.VALUE  = map_port_default->VALUE;
    return port_defaults;
}

static fm_byte getPort
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            rx_port
)
{
    fm_byte port = 0;
    map_port_r const * const map_port = &(mapper_map->MAP_PORT[rx_port]);

    port = map_port->MAP_PORT;
    return port;
}

static mbyMapProt getProt
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            entry
)
{
    mbyMapProt prot;

    map_prot_r const * const map_prot = &(mapper_map->MAP_PROT[entry]);

    prot.PROT     = map_prot->PROT;
    prot.MAP_PROT = map_prot->MAP_PROT;
    return prot;
}

static mbyMapDomainProfile getDomainProfile
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            l2_domain
)
{
    mbyMapDomainProfile domain_profile;

    map_domain_profile_r const * const map_domain_profile = &(mapper_map->MAP_DOMAIN_PROFILE[l2_domain]);

    domain_profile.PRIORITY_PROFILE = map_domain_profile->PRIORITY_PROFILE;
    return domain_profile;
}

static mbyMapDomainAction0 getDomainAction0
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            index
)
{
    mbyMapDomainAction0 domain_action;

    map_domain_action0_r const * const map_domain_action0 = &(mapper_map->MAP_DOMAIN_ACTION0[index]);

    domain_action.L2_DOMAIN         = map_domain_action0->L2_DOMAIN;
    domain_action.L3_DOMAIN         = map_domain_action0->L3_DOMAIN;
    domain_action.NAD               = map_domain_action0->NAD;
    domain_action.UPDATE_DOMAINS    = map_domain_action0->UPDATE_DOMAINS;
    domain_action.LEARN_EN          = map_domain_action0->LEARN_EN;
    domain_action.LEARN_MODE        = map_domain_action0->LEARN_MODE;
    domain_action.PRIORITY_PROFILE  = map_domain_action0->PRIORITY_PROFILE;
    domain_action.PRI_SOURCE        = map_domain_action0->PRI_SOURCE;
    domain_action.FORCE_DEFAULT_PRI = map_domain_action0->FORCE_DEFAULT_PRI;
    domain_action.DEFAULT_PRI       = map_domain_action0->DEFAULT_PRI;
    return domain_action;
}

static mbyMapDomainAction1 getDomainAction1
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            index
)
{
    mbyMapDomainAction1 domain_action;

    map_domain_action1_r const * const map_domain_action1 = &(mapper_map->MAP_DOMAIN_ACTION1[index]);

    domain_action.DOMAIN_PROFILE    = map_domain_action1->DOMAIN_PROFILE;
    domain_action.L2_POLICER        = map_domain_action1->L2_POLICER;
    domain_action.L3_POLICER        = map_domain_action1->L3_POLICER;
    domain_action.VLAN_COUNTER      = map_domain_action1->VLAN_COUNTER;
    return domain_action;
}

static mbyMapDomainTcam getDomainTcamEntry
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            index
)
{
    mbyMapDomainTcam cam_entry;
    map_domain_tcam_r const * const map_domain_tcam = &(mapper_map->MAP_DOMAIN_TCAM[index]);

    cam_entry._RSVD1_           = map_domain_tcam->_RSVD1_;
    cam_entry.PORT_KEY_INVERT   = map_domain_tcam->PORT_KEY_INVERT;
    cam_entry.VID2_VALID_INVERT = map_domain_tcam->VID2_VALID_INVERT;
    cam_entry.VID2_KEY_INVERT   = map_domain_tcam->VID2_KEY_INVERT;
    cam_entry.VID1_VALID_INVERT = map_domain_tcam->VID1_VALID_INVERT;
    cam_entry.VID1_KEY_INVERT   = map_domain_tcam->VID1_KEY_INVERT;
    cam_entry._RSVD0_           = map_domain_tcam->_RSVD0_;
    cam_entry.PORT_KEY          = map_domain_tcam->PORT_KEY;
    cam_entry.VID2_VALID        = map_domain_tcam->VID2_VALID;
    cam_entry.VID2_KEY          = map_domain_tcam->VID2_KEY;
    cam_entry.VID1_VALID        = map_domain_tcam->VID1_VALID;
    cam_entry.VID1_KEY          = map_domain_tcam->VID1_KEY;
    return cam_entry;
}

static mbyMapMac getMac
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            entry
)
{
    mbyMapMac mac;

    map_mac_r const * const map_mac = &(mapper_map->MAP_MAC[entry]);

    mac.MAC_ROUTABLE  = map_mac->MAC_ROUTABLE;
    mac.MAP_MAC       = map_mac->MAP_MAC;
    mac.VALID         = map_mac->VALID;
    mac.IGNORE_LENGTH = map_mac->IGNORE_LENGTH;
    mac.MAC           = map_mac->MAC;
    return mac;
}

// --------------------------------------------------------------------------------

static void realignKeys
(
    const fm_uint16 pa_keys           [MBY_N_PARSER_KEYS],
    const fm_bool   pa_keys_valid     [MBY_N_PARSER_KEYS],
    const fm_bool   is_ipv4           [MBY_N_IS_IP_BITS],
    const fm_bool   is_ipv6           [MBY_N_IS_IP_BITS],
    fm_uint16       realigned_keys    [MBY_N_REALIGN_KEYS],
    fm_bool         realigned_keys_vld[MBY_N_REALIGN_KEYS],
    fm_bool * const ihl_ok,
    fm_bool * const ihl_fits
)
{
    for (fm_uint i = 0; i < N_REALIGN_KEYS; i++) {
        realigned_keys    [i] = pa_keys[i];
        realigned_keys_vld[i] = pa_keys_valid[i];
    }

    // Realign for IPv4:

    if (is_ipv4[1]) // Inner IP Header
    {
        realigned_keys    [MBY_RE_KEYS_INNER_IP_TTL_PROT  ] = pa_keys      [MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_TTL_PROT  ] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 4] = 0;

        fm_byte un0 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 8, 8, un0);

        // MISC
        fm_bool innerIhlNot5 = (FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER], 8, 4) != 5);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 4, 1, innerIhlNot5);

        fm_bool un1 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + 3], 14, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 5, 1, un1);

        fm_bool innerHf = !FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + 3], 0, 13);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 6, 1, innerHf);

        fm_bool un2 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + 3], 13, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 7, 1, un2);

        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_DS_FLOW] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER];

        // FLOW
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 0, 4, 0);

        realigned_keys    [MBY_RE_KEYS_INNER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 4] = 0;
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 5] = 0;

        // Inner DIP
        realigned_keys    [MBY_RE_KEYS_INNER_DIP       ] = pa_keys      [MBY_PA_KEYS_INNER_SIPDIP + 2];
        realigned_keys_vld[MBY_RE_KEYS_INNER_DIP       ] = pa_keys_valid[MBY_PA_KEYS_INNER_SIPDIP + 2];
        realigned_keys    [MBY_RE_KEYS_INNER_DIP    + 1] = pa_keys      [MBY_PA_KEYS_INNER_SIPDIP + 3];
        realigned_keys_vld[MBY_RE_KEYS_INNER_DIP    + 1] = pa_keys_valid[MBY_PA_KEYS_INNER_SIPDIP + 3];
        realigned_keys_vld[MBY_PA_KEYS_INNER_SIPDIP + 2] = 0;
        realigned_keys_vld[MBY_PA_KEYS_INNER_SIPDIP + 3] = 0;
    }

    if (is_ipv4[0]) // Outer IP Header
    {
        realigned_keys    [MBY_RE_KEYS_OUTER_IP_TTL_PROT  ] = pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 4];
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_TTL_PROT  ] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER + 4];

        fm_byte un0 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 8, 8, un0);

        // MISC
        fm_byte ihl = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER], 8, 4);
        *ihl_ok   = (ihl >= 5);
        *ihl_fits = (realigned_keys[MBY_RE_KEYS_OUTER_IP_LEN] >= (4 * ihl));

        fm_bool outerIhlNot5 = (ihl != 5);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 4, 1, outerIhlNot5);

        fm_bool un1 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 3], 14, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 5, 1, un1);

        fm_bool outerHf = !FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 3], 0, 13);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 6, 1, outerHf);

        fm_bool un2 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 3], 13, 1);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 7, 1, un2);

        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_DS_FLOW] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER];

        // FLOW
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 0, 4, 0);

        realigned_keys    [MBY_RE_KEYS_OUTER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_FLOW      ] = 0;
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 5] = 0;

        // Outer DIP
        realigned_keys    [MBY_RE_KEYS_OUTER_DIP       ] = pa_keys      [MBY_PA_KEYS_OUTER_SIPDIP + 2];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_DIP       ] = pa_keys_valid[MBY_PA_KEYS_OUTER_SIPDIP + 2];
        realigned_keys    [MBY_RE_KEYS_OUTER_DIP    + 1] = pa_keys      [MBY_PA_KEYS_OUTER_SIPDIP + 3];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_DIP    + 1] = pa_keys_valid[MBY_PA_KEYS_OUTER_SIPDIP + 3];
        realigned_keys_vld[MBY_PA_KEYS_OUTER_SIPDIP + 2] = 0;
        realigned_keys_vld[MBY_PA_KEYS_OUTER_SIPDIP + 3] = 0;
     }

    // Realign for IPv6:

    if (is_ipv6[1]) // Inner IP Header
    {
        // TTL
        fm_byte un0 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + 5], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 8, 8, un0);

        // PROT
        fm_uint32 uo1 = (pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER + 1]) ? 1 : 5;
        fm_byte   un1 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + uo1], 8, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 0, 8, un1);

        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_TTL_PROT  ] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER + 5];
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 5] = 0;

        // LEN
        realigned_keys    [MBY_RE_KEYS_INNER_IP_LEN       ] = pa_keys      [MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_LEN       ] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER + 4];
        realigned_keys_vld[MBY_PA_KEYS_INNER_IP_HEADER + 4] = 0;

        // DS
        fm_byte un2 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + 2], 4, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 8, 8, un2);

        // MISC
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 4, 4, 0);
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_DS_FLOW] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER + 2];

        // FLOW
        fm_byte un3 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_INNER_IP_HEADER + 2], 0, 4);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_DS_FLOW], 0, 4, un3);

        realigned_keys    [MBY_RE_KEYS_INNER_IP_FLOW] = pa_keys      [MBY_PA_KEYS_INNER_IP_HEADER + 3];
        realigned_keys_vld[MBY_RE_KEYS_INNER_IP_FLOW] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER + 3];
    }

    if (is_ipv6[0]) // Outer IP Header
    {
        // TTL
        fm_byte un0 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 5], 0, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 8, 8, un0);

        // PROT
        fm_uint32 uo1 = (pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER + 1]) ? 1 : 5;
        fm_byte   un1 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + uo1], 8, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8, un1);

        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 5] = 0;
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_TTL_PROT  ] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER + 5];

        // LEN
        realigned_keys_vld[MBY_PA_KEYS_OUTER_IP_HEADER + 4] = 0;
        realigned_keys    [MBY_RE_KEYS_OUTER_IP_LEN       ] = pa_keys      [MBY_PA_KEYS_OUTER_IP_HEADER + 4];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_LEN       ] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER + 4];

        // DS
        fm_byte un2 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 2], 4, 8);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 8, 8, un2);

        // MISC
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 4, 4, 0);

        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_DS_FLOW] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER + 2];

        // FLOW
        fm_byte un3 = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_IP_HEADER + 2], 0, 4);
        FM_SET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 0, 4, un3);

        realigned_keys    [MBY_RE_KEYS_OUTER_IP_FLOW] = pa_keys      [MBY_PA_KEYS_OUTER_IP_HEADER + 3];
        realigned_keys_vld[MBY_RE_KEYS_OUTER_IP_FLOW] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER + 3];
    }
}

static fm_uint32 lookUpDomainTcam
(
    mby_ppe_mapper_map * const mapper_map,
    fm_uint32 const            rx_port,
    fm_uint16 const            pa_keys [MBY_N_PARSER_KEYS],
    fm_bool   const            pa_flags[MBY_N_PARSER_FLGS]
)
{

    fm_uint16 vid1      = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_VLAN1], 0, 12);
    fm_uint16 vid2      = FM_GET_UNNAMED_FIELD(pa_keys[MBY_PA_KEYS_OUTER_VLAN2], 0, 12);
    fm_byte   vid1Valid = pa_flags[MBY_PA_FLAGS_OTR_L2_VLAN1];
    fm_byte   vid2Valid = pa_flags[MBY_PA_FLAGS_OTR_L2_VLAN2];

    fm_uint32 tcam_idx = 0; // TCAM miss (no match)

    // The highest numbered DOMAIN_CAM entry has highest precedence
    for (fm_int i = (MBY_MAP_DOMAIN_TCAM_ENTRIES - 1); i >= 0; i--)
    {
        fm_uint32 index = i; // conv. to unsigned
        mbyMapDomainTcam domainTcam = getDomainTcamEntry(mapper_map, index);
        fm_byte   maskPort      = domainTcam.PORT_KEY   ^ domainTcam.PORT_KEY_INVERT;
        fm_byte   maskVid1Valid = domainTcam.VID1_VALID ^ domainTcam.VID1_VALID_INVERT;
        fm_uint16 maskVid1      = domainTcam.VID1_KEY   ^ domainTcam.VID1_KEY_INVERT;
        fm_byte   maskVid2Valid = domainTcam.VID2_VALID ^ domainTcam.VID2_VALID_INVERT;
        fm_uint16 maskVid2      = domainTcam.VID2_KEY   ^ domainTcam.VID2_KEY_INVERT;

        // conditions for a TCAM hit:
        fm_bool c0 = (((domainTcam.PORT_KEY    & domainTcam.PORT_KEY_INVERT) == 0) &&
                      (((1 << rx_port) & maskPort) & (domainTcam.PORT_KEY & maskPort)));
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
            tcam_idx = index; // TCAM hit
            break; // out of for i loop
        }
    }

    return tcam_idx;
}

static void insertDefaults
(
    mby_ppe_mapper_map   * const mapper_map,
    const fm_uint32              rx_port,
    const mbyMapPortCfg          port_cfg,
    mbyClassifierActions * const ffu_actions,
    mbyClassifierKeys    * const ffu_keys,
    fm_uint16                    realigned_keys    [MBY_N_REALIGN_KEYS],
    fm_bool                      realigned_keys_vld[MBY_N_REALIGN_KEYS])
{
    for (fm_uint i = 0; i < MBY_CGRP_ACT24; i++) {
        ffu_actions->act24[i].prec = 1;
        ffu_actions->act24[i].val = 0;
    }

    for (fm_uint i = 0; i < MBY_CGRP_ACT4; i++) {
        ffu_actions->act4[i].prec = 1;
        ffu_actions->act4[i].val = 0;
    }
    for (fm_uint i = 0; i < MBY_CGRP_ACT1; i++) {
        ffu_actions->act1[i].prec = 1;
        ffu_actions->act1[i].val = 0;
    }

    // Clear SGLORT and DGLORT
    realigned_keys[MBY_RE_KEYS_SGLORT] = 0;
    realigned_keys[MBY_RE_KEYS_DGLORT] = 0;

    // Apply defaults on keys
    for (fm_uint i = 0; i < MBY_MAP_PORT_DEFAULT_ENTRIES_0; i++)
    {
        fm_uint32 entry = i; // conv. to unsigned
        mbyMapPortDefaults port_defaults = getPortDefaults(mapper_map, rx_port, entry);
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
        FM_SET_UNNAMED_FIELD(ffu_actions->act4[MBY_CGRP_ACTION_DSCP_LOW].val, 0, 4, un0);

        fm_uint32 un1 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 14, 2);
        FM_SET_UNNAMED_FIELD(ffu_actions->act4[MBY_CGRP_ACTION_DSCP_HIGH].val, 0, 2, un1);
    }

    // Set VPRI/VID actions
    if (realigned_keys_vld[MBY_RE_KEYS_OUTER_VLAN1])
    {
        fm_uint32 un0 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 12, 4);
        ffu_actions->act4[MBY_CGRP_ACTION_VPRI_LOW].val = un0;

        fm_uint32 un1 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 12, 4); // 12? <-- REVISIT!!!
        ffu_actions->act4[MBY_CGRP_ACTION_VPRI_HIGH].val = un1;

        fm_uint32 un2 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1],  0, 4);
        ffu_actions->act4[MBY_CGRP_ACTION_VID_LOW].val = un2;

        fm_uint32 un3 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1],  4, 4);
        ffu_actions->act4[MBY_CGRP_ACTION_VID_MID].val = un3;

        fm_uint32 un4 = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1],  8, 4);
        ffu_actions->act4[MBY_CGRP_ACTION_VID_HIGH].val = un4;
    }

    // Apply defaults on actions
    for (fm_uint i = 0; i < MBY_MAP_PORT_DEFAULT_ENTRIES_0; i++)
    {
        fm_uint32 entry = i; // conv. to unsigned
        mbyMapPortDefaults port_defaults = getPortDefaults(mapper_map, rx_port, entry);
        fm_byte target = port_defaults.TARGET;
        if (target <= MBY_DEFAULT_TARGET_FORCE_KEYS_H)
           continue;

        // target 96...105 matches lower act24[0...10]
        if ( (target <= MBY_DEFAULT_TARGET_ACT24_L_H) &&
             (target >= MBY_DEFAULT_TARGET_ACT24_L_L) &&
             ((target - MBY_DEFAULT_TARGET_ACT24_L_L) < MBY_CGRP_ACT24)) {
            fm_uint32 un0 = port_defaults.VALUE;
            FM_SET_UNNAMED_FIELD(ffu_actions->act24[target - 96].val, 0, 16, un0);
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT24_U_H) &&
                  (target >= MBY_DEFAULT_TARGET_ACT24_U_L) &&
                  ((target - MBY_DEFAULT_TARGET_ACT24_U_L) < MBY_CGRP_ACT24)) {
            // target 112...121 matches upper act24[0...10]
            fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, 0, 8);
            FM_SET_UNNAMED_FIELD(ffu_actions->act24[target - 112].val, 16, 8, un0);
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT4_4_H) &&
                  (target >= MBY_DEFAULT_TARGET_ACT4_4_L) &&
                  ((target - MBY_DEFAULT_TARGET_ACT4_4_L) < MBY_CGRP_ACT4)) {
            for (fm_uint j = 0; j < 4; j++) {
                if ((target - MBY_DEFAULT_TARGET_ACT4_4_L + j) < MBY_CGRP_ACT4) {
                    fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, j * 4, 4);
                    ffu_actions->act4[target - MBY_DEFAULT_TARGET_ACT4_4_L + j].val = un0;
                }
            }
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT4_2_H) &&
                  (target >= MBY_DEFAULT_TARGET_ACT4_2_L) &&
                  ((target - MBY_DEFAULT_TARGET_ACT4_2_L) < MBY_CGRP_ACT4)) {
            for (fm_uint j = 0; j < 2; j++) {
                if ((target - MBY_DEFAULT_TARGET_ACT4_2_L + j) < MBY_CGRP_ACT4) {
                    fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, j * 4, 4);
                    ffu_actions->act4[target - MBY_DEFAULT_TARGET_ACT4_2_L + j].val = un0;
                }
            }
        }
        else if ( (target <= MBY_DEFAULT_TARGET_ACT4_1_H) &&
             (target >= MBY_DEFAULT_TARGET_ACT4_1_L)  &&
             ((target - MBY_DEFAULT_TARGET_ACT4_1_L) < MBY_CGRP_ACT4)) {
            fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, 0, 4);
            ffu_actions->act4[target - MBY_DEFAULT_TARGET_ACT4_1_L].val = un0;
        }
        else if ( target == MBY_DEFAULT_TARGET_ACT1_FLAGS) {
            for (fm_uint j = 0; j < 16; j++) {
                fm_uint32 un0 = FM_GET_UNNAMED_FIELD(port_defaults.VALUE, j, 1);
                ffu_actions->act1[j].val = un0;
            }
        }
    }

    if (port_cfg.DEFAULT_SGLORT_EN) {
        realigned_keys    [MBY_RE_KEYS_SGLORT] = port_cfg.DEFAULT_SGLORT;
        realigned_keys_vld[MBY_RE_KEYS_SGLORT] = 1;
    }

    // Set FFU keys from realigned_keys

    // KEY16
    for (fm_uint i = 0; i < MBY_CGRP_KEY16; i++)
         ffu_keys->key16[i] = realigned_keys[i];

    // KEY8
    for (fm_uint i = 0; i < MBY_CGRP_KEY8; i += 2) {
        ffu_keys->key8[i  ] = FM_GET_UNNAMED_FIELD(realigned_keys[(i >> 1) + MBY_RE_KEYS_GENERAL_8B], 8, 8);
        ffu_keys->key8[i+1] = FM_GET_UNNAMED_FIELD(realigned_keys[(i >> 1) + MBY_RE_KEYS_GENERAL_8B], 0, 8);
    }

    // KEY32
    for (fm_uint i = 0; i < MBY_CGRP_KEY32; i++) {
        FM_SET_UNNAMED_FIELD(ffu_keys->key32[i], 16, 16, realigned_keys[i * 2 + MBY_RE_KEYS_OUTER_SIP]);
        FM_SET_UNNAMED_FIELD(ffu_keys->key32[i],  0, 16, realigned_keys[i * 2 + MBY_RE_KEYS_OUTER_SIP + 1]);
    }
}

static void getTcFromPriSource
(
    mby_ppe_mapper_map * const mapper_map,
    fm_bool   const            pa_flags[MBY_N_PARSER_FLGS],
    fm_uint32 const            domain_index,
    fm_uint16 const            realigned_keys[MBY_N_REALIGN_KEYS],
    fm_byte   const            pri_profile,
    fm_bool            * const no_pri_enc,
    fm_byte            * const traffic_class
)
{
    mbyMapDomainAction0 domain_action0 = getDomainAction0(mapper_map, domain_index);
    fm_byte  pri_source = domain_action0.PRI_SOURCE;
    fm_int32 tc         = -1;    // traffic class
    fm_bool  np         = FALSE; // no priority encoding

    // Traffic class mapping - TC is derived from 4 possible sources.
    for (fm_uint i = 0; i < 4; i++)
    {
        switch (FM_GET_UNNAMED_FIELD(pri_source, (6 - (i * 2)), 2))
        {
            case TC_SOURCE_VPRI:
                if (pa_flags[MBY_PA_FLAGS_OTR_L2_VLAN1])
                {
                    fm_uint32 vpri = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_VLAN1], 12, 4);
                    fm_uint64 tc_by_vpri = mapper_map->MAP_VPRI_TC[pri_profile].TC_BY_VPRI;
                    tc = FM_GET_UNNAMED_FIELD64(tc_by_vpri, (vpri * 3), 3);
                }
                break;

            case TC_SOURCE_MPLS:
                if (pa_flags[MBY_PA_FLAGS_OTR_MPLS_V])
                {
                    fm_uint32 exp = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_MPLS + 1], 9, 3);
                    fm_uint64 tc_by_exp = mapper_map->MAP_EXP_TC[pri_profile].TC_BY_EXP;
                    tc = FM_GET_UNNAMED_FIELD64(tc_by_exp, (exp * 3), 3);
                }
                break;

            case TC_SOURCE_DSCP:
                if (pa_flags[MBY_PA_FLAGS_OTR_L3_V])
                {
                    fm_uint32 dscp = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_DS_FLOW], 10, 6);
                    fm_uint32 indx = ((pri_profile << 6) | dscp);
                    tc = mapper_map->MAP_DSCP_TC[indx].TC;
                }
                break;

            case TC_SOURCE_META:
                // TODO: Handle metadata for packets received from CPU
                break;
        }

        if (tc >= 0)
            break;
    }

    if (domain_action0.FORCE_DEFAULT_PRI || (tc < 0)) {
        tc = domain_action0.DEFAULT_PRI;
        np = TRUE;
    }

    *traffic_class = tc;
    *no_pri_enc    = np;
}

static void mapScalar
(
    mby_ppe_mapper_map   * const mapper_map,
    fm_uint32     const          rx_port,
    mbyMapPortCfg const          port_cfg,
    fm_bool       const          pa_ex_parsing_done,
    fm_bool       const          pa_ex_trunc_header,
    fm_bool       const          pa_ex_depth_exceed,
    fm_bool       const          pa_csum_ok,
    fm_bool       const          pa_flags[MBY_N_PARSER_FLGS],
    fm_byte       const          pa_ptrs [MBY_N_PARSER_PTRS],
    fm_uint16     const          realigned_keys[MBY_N_REALIGN_KEYS],
    fm_bool       const          is_ipv4[MBY_N_IS_IP_BITS],
    fm_bool       const          is_ipv6[MBY_N_IS_IP_BITS],
    fm_uint32     const          domain_index,
    fm_bool       const          ihl_ok,
    fm_bool       const          ihl_fits,
    mbyClassifierActions * const ffu_actions,
    mbyMapProfKey0       * const map_prof_key0,
    mbyMapProfKey1       * const map_prof_key1,
    mbyMappedKey         * const mapped_key,
    fm_byte              * const pri_profile,
    fm_bool              * const no_pri_enc,
    fm_byte              * const traffic_class,
    fm_byte              * const operator_id,
    fm_uint16            * const l2_domain,
    fm_byte              * const l3_domain,
    fm_bool              * const learn_mode,
    fm_uint16            * const l2_ivlan1_cnt
)
{
    // initialize
    mapped_key->MAP_INNER_PROT = 0;
    mapped_key->MAP_OUTER_PROT = 0;
    mapped_key->MAP_PORT = getPort(mapper_map, rx_port);

    // Compress PROT to 3 bits, highest index match wins
    for (fm_uint i = 0; i < MBY_MAP_PROT_ENTRIES; i++)
    {
        fm_uint32 prot_entry = i;
        mbyMapProt prot = getProt(mapper_map, prot_entry);
        if (pa_flags[MBY_PA_FLAGS_INR_L3_V] &&
            (prot.PROT == FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 0, 8)))
            mapped_key->MAP_INNER_PROT = prot.MAP_PROT;

        if (pa_flags[MBY_PA_FLAGS_OTR_L3_V] &&
            (prot.PROT == FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8)))
            mapped_key->MAP_OUTER_PROT = prot.MAP_PROT;
    }

    // Compress outer DMAC to 4 bits
    fm_macaddr key_mac = 0;
    FM_SET_UNNAMED_FIELD64(key_mac,  0, 16, realigned_keys[MBY_RE_KEYS_OUTER_DMAC + 2]);
    FM_SET_UNNAMED_FIELD64(key_mac, 16, 16, realigned_keys[MBY_RE_KEYS_OUTER_DMAC + 1]);
    FM_SET_UNNAMED_FIELD64(key_mac, 32, 16, realigned_keys[MBY_RE_KEYS_OUTER_DMAC    ]);

    fm_bool oDmacMulticast = isMulticastMacAddress(key_mac);
    fm_bool oDmacBroadcast = isBroadcastMacAddress(key_mac);

    for (fm_int i = MBY_MAP_MAC_ENTRIES - 1; i >= 0; i--)
    {
        fm_uint32 entry = i; // conf. to unsigned
        mbyMapMac mac = getMac(mapper_map, entry);
        fm_uint64 mask = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << mac.IGNORE_LENGTH;

        if ((mac.VALID & 1) && (mac.MAC == (key_mac & mask))) {
            mapped_key->MAP_OUTER_DMAC  = mac.MAP_MAC;
            map_prof_key1->MAC_ROUTABLE = mac.MAC_ROUTABLE;
            break;
        }
    }

    // Compress outer SMAC to 4 bits
    key_mac = 0;
    FM_SET_UNNAMED_FIELD64(key_mac,  0, 16, realigned_keys[MBY_RE_KEYS_OUTER_SMAC + 2]);
    FM_SET_UNNAMED_FIELD64(key_mac, 16, 16, realigned_keys[MBY_RE_KEYS_OUTER_SMAC + 1]);
    FM_SET_UNNAMED_FIELD64(key_mac, 32, 16, realigned_keys[MBY_RE_KEYS_OUTER_SMAC    ]);

    for (fm_int i = MBY_MAP_MAC_ENTRIES - 1; i >= 0; i--)
    {
        fm_uint32 entry = i; // conf. to unsigned
        mbyMapMac mac = getMac(mapper_map, entry);
        fm_uint64 mask = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << mac.IGNORE_LENGTH;
        if (((mac.VALID >> 1) & 1) && (mac.MAC == (key_mac  & mask))) {
            mapped_key->MAP_OUTER_SMAC  = mac.MAP_MAC;
            map_prof_key1->MAC_ROUTABLE = (((fm_byte) mac.MAC_ROUTABLE) << 1);
            break;
        }
    }

    if (pa_flags[MBY_PA_FLAGS_INR_L2_V])
    {
        // Compress inner DMAC
        key_mac = 0;
        FM_SET_UNNAMED_FIELD64(key_mac,  0, 16, realigned_keys[MBY_RE_KEYS_INNER_DMAC + 2]);
        FM_SET_UNNAMED_FIELD64(key_mac, 16, 16, realigned_keys[MBY_RE_KEYS_INNER_DMAC + 1]);
        FM_SET_UNNAMED_FIELD64(key_mac, 32, 16, realigned_keys[MBY_RE_KEYS_INNER_DMAC    ]);

        for (fm_int i = MBY_MAP_MAC_ENTRIES - 1; i >= 0; i--)
        {
            fm_uint32 entry = i; // conf. to unsigned
            mbyMapMac mac = getMac(mapper_map, entry);
            fm_uint64 mask = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << mac.IGNORE_LENGTH;
            if (((mac.VALID >> 2) & 1 ) && (mac.MAC == (key_mac & mask))) {
                mapped_key->MAP_INNER_DMAC  = mac.MAP_MAC;
                map_prof_key1->MAC_ROUTABLE = (((fm_byte) mac.MAC_ROUTABLE) << 2);
                break;
            }
        }

        // Compress inner SMAC
        key_mac = 0;
        FM_SET_UNNAMED_FIELD64(key_mac,  0, 16, realigned_keys[MBY_RE_KEYS_INNER_SMAC + 2]);
        FM_SET_UNNAMED_FIELD64(key_mac, 16, 16, realigned_keys[MBY_RE_KEYS_INNER_SMAC + 1]);
        FM_SET_UNNAMED_FIELD64(key_mac, 32, 16, realigned_keys[MBY_RE_KEYS_INNER_SMAC + 0]);

        for (fm_int i = MBY_MAP_MAC_ENTRIES - 1; i >= 0; i--)
        {
            fm_uint32 entry = i; // conf. to unsigned
            mbyMapMac mac = getMac(mapper_map, entry);
            fm_uint64 mask = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) << mac.IGNORE_LENGTH;
            if (((mac.VALID >> 3) & 1 ) && (mac.MAC == (key_mac & mask))) {
                mapped_key->MAP_INNER_SMAC  = mac.MAP_MAC;
                map_prof_key1->MAC_ROUTABLE = (((fm_byte) mac.MAC_ROUTABLE) << 3);
                break;
            }
        }
    }

    // Map outer L4 SRC ports
    if (pa_flags[MBY_PA_FLAGS_OTR_L4_V])
    {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_OUTER_L4SRC];
        mapped_key->MAP_OUTER_L4_SRC = realigned_keys[MBY_RE_KEYS_OUTER_L4SRC];

        for (fm_uint i = 0; i < MBY_MAP_L4_SRC_ENTRIES; i++)
        {
            fm_uint32 curr_l4_src = mapper_map->MAP_L4_SRC[i].MAP_L4_SRC;
            fm_byte   valid       = mapper_map->MAP_L4_SRC[i].VALID;
            fm_uint32 curr_prot   = mapper_map->MAP_L4_SRC[i].MAP_PROT;
            fm_uint32 curr_map    = mapper_map->MAP_L4_SRC[i].L4_SRC;
            if ((valid & 1) && (mapped_key->MAP_OUTER_PROT == curr_prot))
            {
                fm_bool next_ok = ((i + 1) != MBY_MAP_L4_SRC_ENTRIES);
                fm_uint32 next_map  = (next_ok) ? mapper_map->MAP_L4_SRC[i+1].L4_SRC   : 0;
                fm_uint32 next_prot = (next_ok) ? mapper_map->MAP_L4_SRC[i+1].MAP_PROT : 0;
                if ((curr_map <= temp) && (next_ok || (mapped_key->MAP_OUTER_PROT != next_prot) || (temp < next_map)))
                    mapped_key->MAP_OUTER_L4_SRC = curr_l4_src;
            }
        }
    }

    // Map inner L4 SRC ports
    if (pa_flags[MBY_PA_FLAGS_INR_L4_V])
    {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_INNER_L4SRC];
        mapped_key->MAP_INNER_L4_SRC = realigned_keys[MBY_RE_KEYS_INNER_L4SRC];

        for (fm_uint i = 0; i < MBY_MAP_L4_SRC_ENTRIES; i++)
        {
            fm_uint32 curr_l4_src = mapper_map->MAP_L4_SRC[i].MAP_L4_SRC;
            fm_byte   valid       = mapper_map->MAP_L4_SRC[i].VALID;
            fm_uint32 curr_prot   = mapper_map->MAP_L4_SRC[i].MAP_PROT;
            fm_uint32 curr_map    = mapper_map->MAP_L4_SRC[i].L4_SRC;
            if (((valid >> 1) & 1) && (mapped_key->MAP_INNER_PROT == curr_prot))
            {
                fm_bool next_ok   = ((i + 1) != MBY_MAP_L4_SRC_ENTRIES);
                fm_uint32 next_map  = (next_ok) ? mapper_map->MAP_L4_SRC[i+1].L4_SRC   : 0;
                fm_uint32 next_prot = (next_ok) ? mapper_map->MAP_L4_SRC[i+1].MAP_PROT : 0;
                if ((curr_map <= temp) && (next_ok || (mapped_key->MAP_INNER_PROT != next_prot) || (temp < next_map)))
                    mapped_key->MAP_INNER_L4_SRC = curr_l4_src;
            }
        }
    }

    // Map outer L4 DST ports
    if (pa_flags[MBY_PA_FLAGS_OTR_L4_V])
    {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_OUTER_L4DST];
        mapped_key->MAP_OUTER_L4_DST = realigned_keys[MBY_RE_KEYS_OUTER_L4DST];

        for (fm_uint i = 0; i < MBY_MAP_L4_DST_ENTRIES; i++)
        {
            fm_uint32 curr_l4_dst = mapper_map->MAP_L4_DST[i].MAP_L4_DST;
            fm_byte   valid       = mapper_map->MAP_L4_DST[i].VALID;
            fm_uint32 curr_prot   = mapper_map->MAP_L4_DST[i].MAP_PROT;
            fm_uint32 curr_map    = mapper_map->MAP_L4_DST[i].L4_DST;
            if ((valid & 1) && (mapped_key->MAP_OUTER_PROT == curr_prot))
            {
                fm_bool   next_ok   = ((i + 1) != MBY_MAP_L4_DST_ENTRIES);
                fm_uint32 next_map  = (next_ok) ? mapper_map->MAP_L4_DST[i+1].L4_DST   : 0;
                fm_uint32 next_prot = (next_ok) ? mapper_map->MAP_L4_DST[i+1].MAP_PROT : 0;
                if ((curr_map <= temp) && (next_ok || (mapped_key->MAP_OUTER_PROT != next_prot) || (temp < next_map)))
                    mapped_key->MAP_OUTER_L4_DST = curr_l4_dst;
            }
        }
    }

    // Map inner L4 DST ports
    if (pa_flags[MBY_PA_FLAGS_INR_L4_V])
    {
        fm_uint32 temp = realigned_keys[MBY_RE_KEYS_INNER_L4DST];
        mapped_key->MAP_INNER_L4_DST = realigned_keys[MBY_RE_KEYS_INNER_L4DST];

        for (fm_uint i = 0; i < MBY_MAP_L4_DST_ENTRIES; i++)
        {
            fm_uint32 curr_l4_dst = mapper_map->MAP_L4_DST[i].MAP_L4_DST;
            fm_byte   valid       = mapper_map->MAP_L4_DST[i].VALID;
            fm_uint32 curr_prot   = mapper_map->MAP_L4_DST[i].MAP_PROT;
            fm_uint32 curr_map    = mapper_map->MAP_L4_DST[i].L4_DST;
            if (((valid >> 1) & 1) && (mapped_key->MAP_INNER_PROT == curr_prot))
            {
                fm_bool   next_ok   = ((i + 1) != MBY_MAP_L4_DST_ENTRIES);
                fm_uint32 next_map  = (next_ok) ? mapper_map->MAP_L4_DST[i+1].L4_DST   : 0;
                fm_uint32 next_prot = (next_ok) ? mapper_map->MAP_L4_DST[i+1].MAP_PROT : 0;
                if ((curr_map <= temp) && (next_ok || (mapped_key->MAP_INNER_PROT != next_prot) || (temp < next_map)))
                    mapped_key->MAP_INNER_L4_DST = curr_l4_dst;
            }
        }
    }

    // Drive profile_key_t
    FM_SET_UNNAMED_FIELD(map_prof_key0->EX, 0, 1, pa_ex_parsing_done);
    FM_SET_UNNAMED_FIELD(map_prof_key0->EX, 1, 1, pa_ex_trunc_header);
    FM_SET_UNNAMED_FIELD(map_prof_key0->EX, 2, 1, pa_ex_depth_exceed);

    fm_bool ip_fits = FALSE;
    ip_fits = pa_flags[MBY_PA_FLAGS_OTR_L3_V] && pa_flags[MBY_PA_FLAGS_OTR_L4_V] &&
        ((pa_ptrs[MBY_PA_PTRS_OTR_L4_PTR] - pa_ptrs[MBY_PA_PTRS_OTR_L3_PTR]) <= 56);
    FM_SET_UNNAMED_FIELD(map_prof_key0->IP_FITS, 0, 1, ip_fits);

    ip_fits = pa_flags[MBY_PA_FLAGS_INR_L3_V] & pa_flags[MBY_PA_FLAGS_INR_L4_V] &
        ((pa_ptrs[MBY_PA_PTRS_INR_L4_PTR] - pa_ptrs[MBY_PA_PTRS_INR_L3_PTR]) <= 56);
    FM_SET_UNNAMED_FIELD(map_prof_key0->IP_FITS, 1, 1, ip_fits);

    map_prof_key0->IHL_OK   = ihl_ok;
    map_prof_key0->IHL_FITS = ihl_fits;

    for (fm_uint i = 0; i < (MBY_N_PARSER_FLGS - 1); i++)
        FM_SET_UNNAMED_FIELD64(map_prof_key0->FLAGS, i, 1, pa_flags[i+1]);

    for(fm_uint i = 0; i < MBY_N_IS_IP_BITS; i++) {
        fm_uint32 un0 = FM_GET_UNNAMED_FIELD(pa_csum_ok, i, 1);
        FM_SET_UNNAMED_FIELD64(map_prof_key0->CSUM, i, 1, (un0 | (!is_ipv4[i])));
    }

    map_prof_key1->PORT_PROFILE = port_cfg.PORT_PROFILE;

    FM_SET_UNNAMED_FIELD(map_prof_key1->MAC_MBCAST, 0, 1, oDmacMulticast);
    FM_SET_UNNAMED_FIELD(map_prof_key1->MAC_MBCAST, 1, 1, oDmacBroadcast);

    // domain TCAM - domain_index is always valid here
    mbyMapDomainAction0 domain_action0 = getDomainAction0(mapper_map, domain_index);
    if (domain_action0.UPDATE_DOMAINS)
    {
        *operator_id = domain_action0.NAD;
        *l2_domain   = domain_action0.L2_DOMAIN;
        *l3_domain   = domain_action0.L3_DOMAIN;
        *pri_profile = domain_action0.PRIORITY_PROFILE;
    }
    else
    {
        mbyMapDomainProfile domain_profile = getDomainProfile(mapper_map, *l2_domain);
        *pri_profile = domain_profile.PRIORITY_PROFILE;
    }

    map_prof_key1->L2_DOMAIN = *l2_domain;
    map_prof_key1->L3_DOMAIN = *l3_domain;

    ffu_actions->act1[MBY_CGRP_ACTION_LEARN_NOTIFY].val = domain_action0.LEARN_EN;

    *learn_mode = domain_action0.LEARN_MODE;

    getTcFromPriSource
    (
        mapper_map,
        pa_flags,
        domain_index,
        realigned_keys,
        *pri_profile,
        no_pri_enc,
        traffic_class
    );

    ffu_actions->act4[MBY_CGRP_ACTION_TC].val = *traffic_class;

    mbyMapDomainAction1 domain_action1 = getDomainAction1(mapper_map, domain_index);
    fm_uint16 l2Policer           = domain_action1.L2_POLICER;
    fm_uint16 l3Policer           = domain_action1.L3_POLICER;
    map_prof_key1->DOMAIN_PROFILE = domain_action1.DOMAIN_PROFILE;
    *l2_ivlan1_cnt                = domain_action1.VLAN_COUNTER;

    fm_byte l2_color_cfg = mapper_map->MAP_DOMAIN_POL_CFG.L2_COLOR_CFG;
    fm_byte l3_color_cfg = mapper_map->MAP_DOMAIN_POL_CFG.L3_COLOR_CFG;

    if (l2Policer != 0) {
        // if l2_policer is nonzero, then the default POLICER[0] action is (bank=0, index=l2_policer).
        ffu_actions->act24[MBY_CGRP_ACTION_POLICER0].val = 0;
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER0].val, 23,  1, 1);
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER0].val, 20,  3, l2_color_cfg);
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER0].val,  0, 12, (l2Policer & 0xFFF));
    }

    if (l3Policer != 0) {
        // if l3_policer is nonzero, then the default POLICER[1] action is (bank=5, index=l3_policer).
        ffu_actions->act24[MBY_CGRP_ACTION_POLICER1].val = 0;
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER1].val, 23,  1, 1);
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER1].val,  0, 12, (l3Policer & 0xFFF));
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER1].val, 20,  3, l3_color_cfg);
        FM_SET_UNNAMED_FIELD(ffu_actions->act24[MBY_CGRP_ACTION_POLICER1].val, 16,  4, 5);
    }
}

static void encodeLength
(
    const fm_byte   hdr_len,
    const fm_bool   hdr_valid,
    const fm_byte   min,
    const fm_byte   max,
    const fm_byte   offset,
    const fm_bool   fits,
    fm_bool * const ptrs_err,
    fm_byte * const len
)
{
    if ((max < min) || (max < offset)) { /* WARNING: max is less than min, or max is less than offset */ }

    *len = 0; // default

    if (!fits || (*ptrs_err) || !hdr_valid)
        ;
    else if (hdr_len > max)
        *len  = ((max >> 2) & 0x3f) - ((offset >> 2) & 0x3f);
    else if (hdr_len < min)
        *ptrs_err = TRUE;
    else if ((hdr_len & 3) != (offset & 3))
        *len = ((hdr_len - offset) >> 2) & 0x3f; // 5 bits
    else
        *len = ((hdr_len >> 2) & 0x3f) - ((offset >> 2) & 0x3f);
}

static void getParserInfo
(
    mby_ppe_mapper_map      * const mapper_map,
    mbyMapperToClassifier   * const out,
    const fm_uint32                 rx_port,
    const fm_byte                   pa_adj_seg_len,
    const fm_bool                   pa_flags      [MBY_N_PARSER_FLGS],
    const fm_byte                   pa_ptrs       [MBY_N_PARSER_PTRS],
    const fm_bool                   pa_ptrs_valid [MBY_N_PARSER_PTRS],
    const fm_uint16                 realigned_keys[MBY_N_REALIGN_KEYS],
    const fm_bool                   is_ipv6       [MBY_N_IS_IP_BITS],
    mbyMapProfKey0          * const map_prof_key0,
    mbyParserInfo           * const parser_info
)
{
    fm_byte outerProt = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_OUTER_IP_TTL_PROT], 0, 8);

    fm_bool tcp[2] = { FALSE };
    fm_bool udp[2] = { FALSE };

    tcp[0] = (pa_flags[MBY_PA_FLAGS_OTR_L4_V]) && (outerProt == MBY_PROT_TCP);
    udp[0] = (pa_flags[MBY_PA_FLAGS_OTR_L4_V]) && (outerProt == MBY_PROT_UDP);

    fm_byte innerProt = FM_GET_UNNAMED_FIELD(realigned_keys[MBY_RE_KEYS_INNER_IP_TTL_PROT], 0, 8);

    tcp[1] = (pa_flags[MBY_PA_FLAGS_INR_L4_V]) && (innerProt == MBY_PROT_TCP);
    udp[1] = (pa_flags[MBY_PA_FLAGS_INR_L4_V]) && (innerProt == MBY_PROT_UDP);

    fm_byte ptrs[8] = { 0 };
    ptrs[MBY_PA_INFO_OTR_L2] = 0;
    for (fm_int i = MBY_PA_INFO_OTR_MPLS; i <= MBY_PA_INFO_INR_L4; i++)
        ptrs[i] = pa_ptrs[i];

    fm_bool hdr_valid[8] = { FALSE };

    hdr_valid[MBY_PA_INFO_OTR_L2]   = TRUE;
    hdr_valid[MBY_PA_INFO_OTR_MPLS] = pa_flags[MBY_PA_FLAGS_OTR_MPLS_V];
    hdr_valid[MBY_PA_INFO_OTR_L3]   = pa_flags[MBY_PA_FLAGS_OTR_L3_V];
    hdr_valid[MBY_PA_INFO_OTR_L4]   = pa_flags[MBY_PA_FLAGS_OTR_L4_V];
    hdr_valid[MBY_PA_INFO_INR_L2]   = pa_flags[MBY_PA_FLAGS_INR_L2_V];
    hdr_valid[MBY_PA_INFO_INR_MPLS] = pa_flags[MBY_PA_FLAGS_INR_MPLS_V];
    hdr_valid[MBY_PA_INFO_INR_L3]   = pa_flags[MBY_PA_FLAGS_INR_L3_V];
    hdr_valid[MBY_PA_INFO_INR_L4]   = pa_flags[MBY_PA_FLAGS_INR_L4_V];

    fm_byte hdr_len[8] = { 0 };
    fm_byte next_hdr_start = pa_adj_seg_len;
    for (fm_int i = MBY_PA_INFO_INR_L4; i >= MBY_PA_INFO_OTR_L2; i--) {
        hdr_len[i] = next_hdr_start - ptrs[i];
        if (pa_ptrs_valid[i])
            next_hdr_start = ptrs[i];
    }

    /* The parser always sets pr.ptrs[OTR_L3] to indicate the end of the MPLS
       stack (either BOS or the point where the parser has decided to stop
       looking deeper in the MPLS stack).
       parser_info will require pr.ptrs[OTR_L3] to determine the end of the
       MPLS stack in cases where the IP header is not recognized or has an EOS
       exception, or the MPLS stack was not fully parsed. */

    // maximum number of bytes per config
    fm_byte hdr_len_limit[8] = { 0 };
    map_len_limit_r const * const map_len_limit = &(mapper_map->MAP_LEN_LIMIT[rx_port]);

    hdr_len_limit[MBY_PA_INFO_OTR_L2]   = (map_len_limit->OTR_L2_LEN_LIMIT) * 4 + 14;
    hdr_len_limit[MBY_PA_INFO_INR_L2]   = (map_len_limit->INR_L2_LEN_LIMIT) * 4 + 14;

    hdr_len_limit[MBY_PA_INFO_OTR_MPLS] = (map_len_limit->OTR_MPLS_LEN_LIMIT) * 4;
    hdr_len_limit[MBY_PA_INFO_INR_MPLS] = (map_len_limit->INR_MPLS_LEN_LIMIT) * 4;

    hdr_len_limit[MBY_PA_INFO_OTR_L3]   = MBY_OTR_L3_LEN_LIMIT * 4;
    hdr_len_limit[MBY_PA_INFO_INR_L3]   = MBY_INR_L3_LEN_LIMIT * 4;

    hdr_len_limit[MBY_PA_INFO_OTR_L4]   = (tcp[0]) ? MBY_L4_TCP_MIN_SIZE : MBY_OTR_TUN_LEN_LIMIT * 4;
    hdr_len_limit[MBY_PA_INFO_INR_L4]   = (tcp[1]) ? MBY_L4_TCP_MIN_SIZE : MBY_L4_MIN_SIZE;

    fm_bool fits = TRUE;
    fm_bool ptrs_err = FALSE;

    // otr_l2
    encodeLength
    (
        hdr_len[MBY_PA_INFO_OTR_L2],
        hdr_valid[MBY_PA_INFO_OTR_L2],
        14,
        hdr_len_limit[MBY_PA_INFO_OTR_L2],
        10,
        fits,
        &ptrs_err,
        &parser_info->otr_l2_len
    );

    parser_info->otr_l2_len    &= 0x7;
    parser_info->otr_l2_vlan1   = (parser_info->otr_l2_len != 0) && pa_flags[MBY_PA_FLAGS_OTR_L2_VLAN1];
    parser_info->otr_l2_vlan2   = (parser_info->otr_l2_len != 0) && pa_flags[MBY_PA_FLAGS_OTR_L2_VLAN2];
    parser_info->otr_l2_v2first = (parser_info->otr_l2_len != 0) && pa_flags[MBY_PA_FLAGS_OTR_L2_V2FIRST];

    if (parser_info->otr_l2_len > 5) { /* WARNING: illegal parser_info->otr_l2_len */ }

    // otr_mpls
    encodeLength
    (
        hdr_len[MBY_PA_INFO_OTR_MPLS],
        hdr_valid[MBY_PA_INFO_OTR_MPLS],
        0,
        hdr_len_limit[MBY_PA_INFO_OTR_MPLS],
        0,
        fits,
        &ptrs_err,
        &parser_info->otr_mpls_len
    );

    parser_info->otr_mpls_len &= 0x7;

    if (parser_info->otr_mpls_len > 7) { /* WARNING: illegal parser_info->otr_mpls_len */ }

    // otr_l3
    encodeLength
    (
        hdr_len[MBY_PA_INFO_OTR_L3],
        hdr_valid[MBY_PA_INFO_OTR_L3],
        20,
        hdr_len_limit[MBY_PA_INFO_OTR_L3],
        0,
        fits,
        &ptrs_err,
        &parser_info->otr_l3_len
    );

    parser_info->otr_l3_len &= 0xF;
    parser_info->otr_l3_v6   = is_ipv6[0];

    if (parser_info->otr_l3_len > 14) { /* WARNING: illegal parser_info->otr_l3_len */ }

    // otr_l4
    parser_info->otr_l4_udp = fits && !ptrs_err && (hdr_len[MBY_PA_INFO_OTR_L4] >=  8) && pa_flags[MBY_PA_FLAGS_OTR_L4_V] && udp[0];
    parser_info->otr_l4_tcp = fits && !ptrs_err && (hdr_len[MBY_PA_INFO_OTR_L4] >= 18) && pa_flags[MBY_PA_FLAGS_OTR_L4_V] && tcp[0];

    fm_byte otr_l4_min    = (tcp[0]) ? MBY_L4_TCP_MIN_SIZE : ((udp[0]) ? MBY_L4_MIN_SIZE : 4);
    fm_byte otr_l4_offset = (tcp[0]) ? MBY_L4_TCP_MIN_SIZE : ((udp[0]) ? MBY_L4_MIN_SIZE : 0);

    encodeLength
    (
        hdr_len[MBY_PA_INFO_OTR_L4],
        hdr_valid[MBY_PA_INFO_OTR_L4],
        otr_l4_min,
        hdr_len_limit[MBY_PA_INFO_OTR_L4],
        otr_l4_offset,
        fits,
        &ptrs_err,
        &parser_info->otr_tun_len
    );

    parser_info->otr_tun_len &= 0x1F;

    if (parser_info->otr_tun_len > 18) { /* WARNING: illegal parser_info->otr_tun_len */ }

    // stop if Outer L4 is present and not UDP
    fits &= (udp[0] || !hdr_valid[MBY_PA_INFO_OTR_L4]);

    // inr_l2
    encodeLength
    (
        hdr_len[MBY_PA_INFO_INR_L2],
        hdr_valid[MBY_PA_INFO_INR_L2],
        14,
        hdr_len_limit[MBY_PA_INFO_INR_L2],
        10,
        fits,
        &ptrs_err,
        &parser_info->inr_l2_len
    );

    parser_info->inr_l2_len    &= 0x7;
    parser_info->inr_l2_vlan1   = (parser_info->inr_l2_len != 0) && pa_flags[MBY_PA_FLAGS_INR_L2_VLAN1];
    parser_info->inr_l2_vlan2   = (parser_info->inr_l2_len != 0) && pa_flags[MBY_PA_FLAGS_INR_L2_VLAN2];
    parser_info->inr_l2_v2first = (parser_info->inr_l2_len != 0) && pa_flags[MBY_PA_FLAGS_INR_L2_V2FIRST];

    if (parser_info->inr_l2_len > 5) { /* WARNING: illegal parser_info->inr_l2_len */ }

    // inr_mpls
    encodeLength
    (
        hdr_len[MBY_PA_INFO_INR_MPLS],
        hdr_valid[MBY_PA_INFO_INR_MPLS],
        0,
        hdr_len_limit[MBY_PA_INFO_INR_MPLS],
        0,
        fits,
        &ptrs_err,
        &parser_info->inr_mpls_len
    );

    parser_info->inr_mpls_len &= 0x7;
    if (parser_info->inr_mpls_len > 7) {/* WARNING: illegal parser_info->inr_mpls_len */ }

    // inr_l3
    encodeLength
    (
        hdr_len[MBY_PA_INFO_INR_L3],
        hdr_valid[MBY_PA_INFO_INR_L3],
        20,
        hdr_len_limit[MBY_PA_INFO_INR_L3],
        0,
        fits,
        &ptrs_err,
        &parser_info->inr_l3_len
    );

    parser_info->inr_l3_len &= 0xF;
    parser_info->inr_l3_v6   = is_ipv6[1];

    if (parser_info->inr_l3_len > 14) { /* WARNING: illegal parser_info->inr_l3_len */ }

    // inr_l4
    fm_byte min_size = (tcp[1]) ? MBY_L4_TCP_MIN_SIZE : MBY_L4_MIN_SIZE;
    fm_byte inr_l4_len = 0;

    encodeLength
    (
        hdr_len[MBY_PA_INFO_INR_L4],
        hdr_valid[MBY_PA_INFO_INR_L4],
        min_size,
        hdr_len_limit[MBY_PA_INFO_INR_L4],
        0,
        fits,
        &ptrs_err,
        &inr_l4_len
    );

    parser_info->inr_l4_udp = (inr_l4_len > 0) && pa_flags[MBY_PA_FLAGS_INR_L4_V] && udp[1];
    parser_info->inr_l4_tcp = (inr_l4_len > 0) && pa_flags[MBY_PA_FLAGS_INR_L4_V] && tcp[1];

    map_prof_key0->PTRS_ERR = ptrs_err;
}

static void getProfile
(
    mby_ppe_mapper_map      * const mapper_map,
    mbyMapperToClassifier   * const out,
    fm_uint16 const                 realigned_keys[MBY_N_REALIGN_KEYS],
    mbyMapProfKey0 const            key0,
    mbyMapProfKey1 const            key1,
    mbyClassifierActions    * const ffu_actions,
    mbyMapProfAction        * const map_prof_action,
    fm_byte                 * const ffu_profile
)
{
    // initialize struct:
    map_prof_action->PROFILE_VALID   = FALSE;
    map_prof_action->PROFILE         = 0;
    map_prof_action->REWRITE_PROFILE = FALSE;
    map_prof_action->TRIG_VALID      = FALSE;
    map_prof_action->PROFILE_TRIG    = 0;
    map_prof_action->PARSER_ERROR    = FALSE;
    map_prof_action->IP_OPTIONS_MASK = 0;
    map_prof_action->PRIOS_VALID     = FALSE;
    map_prof_action->VPRI_TGT        = 0;
    map_prof_action->DSCP_TGT        = 0;

    fm_byte profileIdx = 0;
    fm_byte trigIdx    = 0;
    fm_byte priosIdx   = 0;

    for (fm_int i = MBY_MAP_PROFILE_KEY0_ENTRIES - 1; i >= 0; i--)
    {
        map_profile_key0_r        profKey0   = mapper_map->MAP_PROFILE_KEY0        [i];
        map_profile_key_invert0_r profMask0  = mapper_map->MAP_PROFILE_KEY_INVERT0 [i];
        map_profile_key1_r        profKey1   = mapper_map->MAP_PROFILE_KEY1        [i];
        map_profile_key_invert1_r profMask1  = mapper_map->MAP_PROFILE_KEY_INVERT1 [i];
        map_profile_action_r      profAction = mapper_map->MAP_PROFILE_ACTION      [i];

        if (((~key0.PTRS_ERR)       & profKey0.PTRS_ERR)       || (key0.PTRS_ERR       & profMask0.PTRS_ERR))
            continue;

        if (((~key0.EX)             & profKey0.EX)             || (key0.EX             & profMask0.EX))
            continue;

        if (((~key0.CSUM)           & profKey0.CSUM)           || (key0.CSUM           & profMask0.CSUM))
            continue;

        if (((~key0.IHL_OK)         & profKey0.IHL_OK)         || (key0.IHL_OK         & profMask0.IHL_OK))
            continue;

        if (((~key0.IHL_FITS)       & profKey0.IHL_FITS)       || (key0.IHL_FITS       & profMask0.IHL_FITS))
            continue;

        if (((~key0.FLAGS)          & profKey0.FLAGS)          || (key0.FLAGS          & profMask0.FLAGS))
            continue;

        if (((~key1.L2_DOMAIN)      & profKey1.L2_DOMAIN)      || (key1.L2_DOMAIN      & profMask1.L2_DOMAIN))
            continue;

        if (((~key1.L3_DOMAIN)      & profKey1.L3_DOMAIN)      || (key1.L3_DOMAIN      & profMask1.L3_DOMAIN))
            continue;

        if (((~key1.PORT_PROFILE)   & profKey1.PORT_PROFILE)   || (key1.PORT_PROFILE   & profMask1.PORT_PROFILE))
            continue;

        if (((~key1.DOMAIN_PROFILE) & profKey1.DOMAIN_PROFILE) || (key1.DOMAIN_PROFILE & profMask1.DOMAIN_PROFILE))
            continue;

        if (((~key1.MAC_ROUTABLE)   & profKey1.MAC_ROUTABLE)   || (key1.MAC_ROUTABLE   & profMask1.MAC_ROUTABLE))
            continue;

        if (((~key1.MAC_MBCAST)     & profKey1.MAC_MBCAST)     || (key1.MAC_MBCAST     & profMask1.MAC_MBCAST))
            continue;

        // TODO: add packet type check

        if (profAction.PROFILE_VALID && (profileIdx == 0))
        {
            profileIdx                       = i;
            map_prof_action->PROFILE_VALID   = 1;
            map_prof_action->PROFILE         = profAction.PROFILE;
            map_prof_action->REWRITE_PROFILE = profAction.REWRITE_PROFILE;
        }

        if (profAction.TRIG_VALID && (trigIdx == 0))
        {
            trigIdx                          = i;
            map_prof_action->TRIG_VALID      = 1;
            map_prof_action->PROFILE_TRIG    = profAction.PROFILE_TRIG;
            map_prof_action->IP_OPTIONS_MASK = profAction.IP_OPTIONS_MASK;
            map_prof_action->PARSER_ERROR    = profAction.PARSER_ERROR;
        }

        if (profAction.PRIOS_VALID && (priosIdx == 0))
        {
            priosIdx                     = i;
            map_prof_action->PRIOS_VALID = 1;
            map_prof_action->VPRI_TGT    = profAction.VPRI_TGT;
            map_prof_action->DSCP_TGT    = profAction.DSCP_TGT;
        }
        // Found all types
        if ((profileIdx > 0) && (trigIdx > 0) && (priosIdx > 0))
            break;
    }

    *ffu_profile = map_prof_action->PROFILE;

    // Set Scenario action
    for (fm_uint i = 0; i < 6; i++)
        ffu_actions->act1[MBY_CGRP_ACTION_PROFILE0 + i].val = ((*ffu_profile) >> i) & 1;
}

static void rewriteSourceNybble
(
    const fm_bool             pa_ex_parsing_done,
    const fm_bool             pa_ex_trunc_header,
    const fm_bool             pa_ex_depth_exceed,
    const fm_bool             pa_flags[MBY_N_PARSER_FLGS],
    const fm_bool             is_ipv6[MBY_N_IS_IP_BITS],
    const mbyMappedKey        mapped_key,
    const mbyMapProfKey0      map_prof_key0,
    const fm_uint32           nybble_idx,
    const fm_uint32           source_id,
    const fm_byte             ffu_profile,
    mbyClassifierKeys * const ffu_keys
)
{
    fm_uint32 key_idx = 0;
    fm_uint32 key_off = 0;

    if (nybble_idx < 4)
    {
        key_idx = 13;
        key_off = nybble_idx * 4;
    }
    else if (nybble_idx < 8)
    {
        key_idx = 19;
        key_off = (nybble_idx % 4) * 4;
    }
    else if (nybble_idx < 24)
    {
        key_idx = (nybble_idx - 8) / 2;
        key_off = (nybble_idx % 2) * 4;
    }
    else if (nybble_idx <= 31)
    {
        key_idx = 16 + (nybble_idx - 24) / 2;
        key_off = (nybble_idx % 2) * 4;
    }
    else
        return;

    fm_byte val = 0;

    if (source_id == SOURCE_NOOP)
        return;
    else if (source_id == SOURCE_MAP_OUTER_PROT)
        val = mapped_key.MAP_OUTER_PROT;
    else if (source_id == SOURCE_MAP_OUTER_DMAC_H)
        val = mapped_key.MAP_OUTER_DMAC >> 4;
    else if (source_id == SOURCE_MAP_OUTER_DMAC_L)
        val = mapped_key.MAP_OUTER_DMAC;
    else if (source_id == SOURCE_MAP_OUTER_SMAC_H)
        val = mapped_key.MAP_OUTER_SMAC >> 4;
    else if (source_id == SOURCE_MAP_OUTER_SMAC_L)
        val = mapped_key.MAP_OUTER_SMAC;
    else if (source_id == SOURCE_MAP_PORT_H)
        val = mapped_key.MAP_PORT >> 4;
    else if (source_id == SOURCE_MAP_PORT_L)
            val = mapped_key.MAP_PORT;
    else if (source_id >= SOURCE_MAP_OUTER_L4_SRC_L &&
             source_id <= SOURCE_MAP_OUTER_L4_SRC_H)
        val = mapped_key.MAP_OUTER_L4_SRC >> ((SOURCE_MAP_OUTER_L4_SRC_H - source_id) * 4);
    else if (source_id >= SOURCE_MAP_OUTER_L4_DST_L &&
             source_id <= SOURCE_MAP_OUTER_L4_DST_H)
        val = mapped_key.MAP_OUTER_L4_DST >> ((SOURCE_MAP_OUTER_L4_DST_H - source_id) * 4);
    else if (source_id >= SOURCE_PA_FLAGS_L &&
             source_id <= SOURCE_PA_FLAGS_H)
    {
        val  =  pa_flags[((SOURCE_PA_FLAGS_H - source_id) * 4)];
        val |= (pa_flags[((SOURCE_PA_FLAGS_H - source_id) * 4) + 1] << 1);
        val |= (pa_flags[((SOURCE_PA_FLAGS_H - source_id) * 4) + 2] << 2);
        val |= (pa_flags[((SOURCE_PA_FLAGS_H - source_id) * 4) + 3] << 3);
    }
    else if (source_id == SOURCE_FFU_PROFILE_L)
        /* only 6 bits */
        val = (ffu_profile & 0x3F) >> 4;
    else if (source_id == SOURCE_FFU_PROFILE_H)
        /* only 6 bits */
        val = (ffu_profile & 0x3F) >> 0;
    else if (source_id == SOURCE_MAP_INNER_PROT)
        val = mapped_key.MAP_INNER_PROT;
    else if (source_id == SOURCE_MAP_INNER_DMAC_H)
        val = mapped_key.MAP_INNER_DMAC >> 4;
    else if (source_id == SOURCE_MAP_INNER_DMAC_L)
        val = mapped_key.MAP_INNER_DMAC >> 0;
    else if (source_id == SOURCE_MAP_INNER_SMAC_H)
        val = mapped_key.MAP_INNER_SMAC >> 4;
    else if (source_id == SOURCE_MAP_INNER_SMAC_L)
        val = mapped_key.MAP_INNER_SMAC >> 0;
    else if (source_id >= SOURCE_MAP_INNER_L4_SRC_L &&
             source_id <= SOURCE_MAP_INNER_L4_SRC_H)
        val = mapped_key.MAP_INNER_L4_SRC >> ((SOURCE_MAP_INNER_L4_SRC_H - source_id) * 4);
    else if (source_id >= SOURCE_MAP_INNER_L4_DST_L &&
             source_id <= SOURCE_MAP_INNER_L4_DST_H)
        val = mapped_key.MAP_INNER_L4_DST >> ((SOURCE_MAP_INNER_L4_DST_H - source_id) * 4);
    else if (source_id == SOURCE_EX)
    {
        val = 0;
        FM_SET_UNNAMED_FIELD(val, 0, 1, pa_ex_parsing_done);
        FM_SET_UNNAMED_FIELD(val, 1, 1, pa_ex_trunc_header);
        FM_SET_UNNAMED_FIELD(val, 2, 1, pa_ex_depth_exceed);
    }
    else if (source_id == SOURCE_CSUM)
        val = map_prof_key0.CSUM;
    else if (source_id == SOURCE_IP_INFO)
    {
        FM_SET_UNNAMED_FIELD64(val, 0, 1, is_ipv6[1]);
        FM_SET_UNNAMED_FIELD64(val, 1, 1, is_ipv6[0]);
        val |= (map_prof_key0.IP_FITS & 0x3) << 2;
    }
    else
    {
        // unhandled source_id
    }

    if        (nybble_idx < 8) {
        FM_SET_UNNAMED_FIELD(ffu_keys->key16[key_idx], key_off, 4, val);
    } else if (nybble_idx <= 31) {
        FM_SET_UNNAMED_FIELD(ffu_keys->key8[ key_idx], key_off, 4, val);
    }
}

static void mapRewrite
(
    mby_ppe_mapper_map    * const mapper_map,
    mbyMapperToClassifier * const out,
    fm_bool const                 pa_ex_parsing_done,
    fm_bool const                 pa_ex_trunc_header,
    fm_bool const                 pa_ex_depth_exceed,
    fm_bool const                 pa_flags           [MBY_N_PARSER_FLGS],
    fm_uint16 const               realigned_keys     [MBY_N_REALIGN_KEYS],
    fm_bool const                 realigned_keys_vld [MBY_N_REALIGN_KEYS],
    fm_bool const                 is_ipv6            [MBY_N_IS_IP_BITS],
    fm_byte const                 ffu_profile,
    mbyMapProfAction const        map_prof_action,
    mbyMapProfKey0 const          map_prof_key0,
    mbyMappedKey const            mapped_key,
    fm_byte const                 pri_profile,
    fm_bool                       ip_option[2],
    fm_bool               * const parser_error,
    mbyClassifierActions  * const ffu_actions,
    mbyClassifierKeys     * const ffu_keys
)
{
    // Rewrite Keys
    if (map_prof_action.PROFILE_VALID)
    {
        for (fm_uint i = 0; i < MBY_MAP_REWRITE_ENTRIES_0; i++)
        {
            fm_byte   j = map_prof_action.REWRITE_PROFILE;
            fm_byte   source_id = mapper_map->MAP_REWRITE[j][i].SRC_ID;
            fm_uint32 nybble_idx = i;

            rewriteSourceNybble
            (
                pa_ex_parsing_done,
                pa_ex_trunc_header,
                pa_ex_depth_exceed,
                pa_flags,
                is_ipv6,
                mapped_key,
                map_prof_key0,
                nybble_idx,
                source_id,
                ffu_profile,
                ffu_keys
            );
        }
    }

    if (map_prof_action.TRIG_VALID == 1)
    {
        fm_uint32 otr_opt_flags;
        fm_uint32 inr_opt_flags;
        fm_uint16 otr_l3_len;
        fm_uint16 inr_l3_len;

        // trig and ip_option mask
        for (fm_uint i = 0; i < 8; i++)
            ffu_actions->act1[MBY_CGRP_ACTION_TRIGGER0 + i].val = (map_prof_action.PROFILE_TRIG >> i) & 1;

        otr_l3_len = 0;
        inr_l3_len = 0;

        FM_SET_UNNAMED_FIELD64(otr_l3_len, 0, 8, ffu_keys->key8[MBY_CGRP_KEY8_OUTER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(otr_l3_len, 8, 8, ffu_keys->key8[MBY_CGRP_KEY8_OUTER_LEN]);
        FM_SET_UNNAMED_FIELD64(inr_l3_len, 0, 8, ffu_keys->key8[MBY_CGRP_KEY8_INNER_LEN+1]);
        FM_SET_UNNAMED_FIELD64(inr_l3_len, 8, 8, ffu_keys->key8[MBY_CGRP_KEY8_INNER_LEN]);

        otr_opt_flags = (pa_flags[MBY_PA_FLAGS_OTR_L3_V] && !is_ipv6[0] && (otr_l3_len > 20)) ? (1 << 6) : 0;
        inr_opt_flags = (pa_flags[MBY_PA_FLAGS_INR_L3_V] && !is_ipv6[1] && (inr_l3_len > 20)) ? (1 << 6) : 0;

        for (fm_uint i = 0; i <= 5; i++) {
            FM_SET_UNNAMED_FIELD(otr_opt_flags, i, 1, pa_flags[i + 32]);
            FM_SET_UNNAMED_FIELD(inr_opt_flags, i, 1, pa_flags[i + 38]);
        }

        ip_option[0]  = (map_prof_action.IP_OPTIONS_MASK & otr_opt_flags) ? 1 : 0;
        ip_option[1]  = (map_prof_action.IP_OPTIONS_MASK & inr_opt_flags) ? 1 : 0;
        *parser_error = map_prof_action.PARSER_ERROR;
    }

    if (map_prof_action.PRIOS_VALID == 1)
    {
        if (map_prof_action.VPRI_TGT & 0x4)
        {
            fm_byte vpri = FM_GET_UNNAMED_FIELD(ffu_keys->key16[MBY_CGRP_KEY16_OUTER_VLAN1], 12, 4);
            fm_uint64 vpri_by_vpri = mapper_map->MAP_VPRI[pri_profile].VPRI_BY_VPRI;
            fm_byte   map_vpri     = FM_GET_UNNAMED_FIELD64(vpri_by_vpri, vpri * 4, 4);
            ffu_actions->act4[MBY_CGRP_ACTION_VPRI_LOW ].val = map_vpri;
            ffu_actions->act4[MBY_CGRP_ACTION_VPRI_HIGH].val = map_vpri;
        }

        if (map_prof_action.VPRI_TGT & 0x1)
        {
            fm_byte vpri = FM_GET_UNNAMED_FIELD(ffu_keys->key16[MBY_CGRP_KEY16_OUTER_VLAN1], 12, 4);
            fm_uint64 vpri_by_vpri = mapper_map->MAP_VPRI[pri_profile].VPRI_BY_VPRI;
            fm_byte   map_vpri     = FM_GET_UNNAMED_FIELD64(vpri_by_vpri, vpri * 4, 4);
            FM_SET_UNNAMED_FIELD(ffu_keys->key16[MBY_CGRP_KEY16_OUTER_VLAN1], 12, 4, map_vpri)
        }

        if (map_prof_action.VPRI_TGT & 0x2)
        {
            fm_byte vpri = FM_GET_UNNAMED_FIELD(ffu_keys->key16[MBY_CGRP_KEY16_INNER_VLAN1], 12, 4);
            fm_uint64 vpri_by_vpri = mapper_map->MAP_VPRI[pri_profile].VPRI_BY_VPRI;
            fm_byte   map_vpri     = FM_GET_UNNAMED_FIELD64(vpri_by_vpri, vpri * 4, 4);
            FM_SET_UNNAMED_FIELD(ffu_keys->key16[MBY_CGRP_KEY16_INNER_VLAN1], 12, 4, map_vpri);
        }

        if (map_prof_action.DSCP_TGT & 0x4)
        {
            fm_byte dscp = FM_GET_UNNAMED_FIELD(ffu_keys->key8[MBY_CGRP_KEY8_OUTER_DS], 2, 6);
            fm_byte indx = ((pri_profile << 6) | dscp);
            fm_byte map_dscp = mapper_map->MAP_DSCP_TC[indx].DSCP;
            ffu_actions->act4[MBY_CGRP_ACTION_DSCP_LOW ].val =  map_dscp & 0xF;
            ffu_actions->act4[MBY_CGRP_ACTION_DSCP_HIGH].val = (map_dscp >> 4);
        }

        if (map_prof_action.DSCP_TGT & 0x1)
        {
            fm_byte dscp = FM_GET_UNNAMED_FIELD(ffu_keys->key8[MBY_CGRP_KEY8_OUTER_DS], 2, 6);
            fm_byte indx = ((pri_profile << 6) | dscp);
            fm_byte map_dscp = mapper_map->MAP_DSCP_TC[indx].DSCP;
            FM_SET_UNNAMED_FIELD(ffu_keys->key8[MBY_CGRP_KEY8_OUTER_DS], 2, 6, map_dscp);
        }

        if (map_prof_action.DSCP_TGT & 0x2)
        {
            fm_byte dscp = FM_GET_UNNAMED_FIELD(ffu_keys->key8[MBY_CGRP_KEY8_INNER_DS], 2, 6);
            fm_byte indx = ((pri_profile << 6) | dscp);
            fm_byte map_dscp = mapper_map->MAP_DSCP_TC[indx].DSCP;
            FM_SET_UNNAMED_FIELD(ffu_keys->key8[MBY_CGRP_KEY8_INNER_DS], 2, 6, map_dscp);
        }
    }
}

/**
 * Mapper stage implementation.
 *
 * @param[in]   parser_map  Pointer to mapper register map (read only).
 * @param[in]   in          Pointer to input structure     (read only).
 * @param[out]  out         Pointer to output structure.
 */
void Mapper
(
    mby_ppe_mapper_map          * const mapper_map,
    mbyParserToMapper const     * const in,
    mbyMapperToClassifier       * const out
)
{
    // Read inputs:
    const fm_byte           pa_adj_seg_len     = in->PA_ADJ_SEG_LEN;
    const fm_bool           pa_csum_ok         = in->PA_CSUM_OK;
    const fm_bool           pa_ex_depth_exceed = in->PA_EX_DEPTH_EXCEED;
    const fm_bool           pa_ex_parsing_done = in->PA_EX_PARSING_DONE;
    const fm_byte           pa_ex_stage        = in->PA_EX_STAGE;   // unsed <-- REVISIT!!!
    const fm_bool           pa_ex_trunc_header = in->PA_EX_TRUNC_HEADER;
    const fm_bool   * const pa_flags           = in->PA_FLAGS;                 // [MBY_N_PARSER_FLGS]
    const fm_uint16 * const pa_keys            = in->PA_KEYS;                  // [MBY_N_PARSER_KEYS]
    const fm_bool   * const pa_keys_valid      = in->PA_KEYS_VALID;            // [MBY_N_PARSER_KEYS]
    const fm_byte   * const pa_ptrs            = in->PA_HDR_PTRS.OFFSET;       // [MBY_N_PARSER_PTRS]
    const fm_bool   * const pa_ptrs_valid      = in->PA_HDR_PTRS.OFFSET_VALID; // [MBY_N_PARSER_PTRS]
    const fm_uint32         rx_port            = in->RX_PORT;

    // Outer MPLS valid flag for use in the Classifier:
    fm_bool otr_mpls_v = pa_flags[MBY_PA_FLAGS_OTR_MPLS_V];

    mbyMapPortCfg port_cfg = getPortCfg(mapper_map, rx_port);

    fm_bool is_ipv4[MBY_N_IS_IP_BITS] = { FALSE };
    fm_bool is_ipv6[MBY_N_IS_IP_BITS] = { FALSE };

    // INNER IP
    if (pa_flags[MBY_PA_FLAGS_INR_L3_V]) {
        is_ipv4[1] = pa_keys_valid[MBY_PA_KEYS_INNER_IP_HEADER];
        is_ipv6[1] = !is_ipv4[1];
    }

    // OUTER IP
    if (pa_flags[MBY_PA_FLAGS_OTR_L3_V]) {
        is_ipv4[0] = pa_keys_valid[MBY_PA_KEYS_OUTER_IP_HEADER];
        is_ipv6[0] = !is_ipv4[0];
    }

    fm_uint16 realigned_keys    [MBY_N_REALIGN_KEYS] = { 0 };
    fm_bool   realigned_keys_vld[MBY_N_REALIGN_KEYS] = { FALSE };

    fm_bool ihl_ok   = FALSE;
    fm_bool ihl_fits = FALSE;

    // TODO: Realign keys stage will probably be removed when spec provide more details.
    realignKeys
    (
        pa_keys,
        pa_keys_valid,
        is_ipv4,
        is_ipv6,
        realigned_keys,
        realigned_keys_vld,
        &ihl_ok,
        &ihl_fits
    );

    fm_uint32 domain_index =
    lookUpDomainTcam
    (
        mapper_map,
        rx_port,
        pa_keys,
        pa_flags
    );

    mbyClassifierActions ffu_actions = { 0 };
    mbyClassifierKeys    ffu_keys    = { 0 };

    insertDefaults
    (
        mapper_map,
        rx_port,
        port_cfg,
        &ffu_actions,
        &ffu_keys,
        realigned_keys,
        realigned_keys_vld
    );

    mbyMapProfKey0 map_prof_key0 = { 0 };
    mbyMapProfKey1 map_prof_key1 = { 0 };
    mbyMappedKey   mapped_key    = { 0 };

    fm_byte   pri_profile   = 0;
    fm_bool   no_pri_enc    = FALSE;
    fm_byte   traffic_class = 0;
    fm_byte   nad           = 0;
    fm_uint16 l2_domain     = 0;
    fm_byte   l3_domain     = 0;
    fm_bool   learn_mode    = FALSE;
    fm_uint16 l2_ivlan1_cnt = 0;

    mapScalar
    (
        mapper_map,
        rx_port,
        port_cfg,
        pa_ex_parsing_done,
        pa_ex_trunc_header,
        pa_ex_depth_exceed,
        pa_csum_ok,
        pa_flags,
        pa_ptrs,
        realigned_keys,
        is_ipv4,
        is_ipv6,
        domain_index,
        ihl_ok,
        ihl_fits,
        &ffu_actions,
        &map_prof_key0,
        &map_prof_key1,
        &mapped_key,
        &pri_profile,
        &no_pri_enc,
        &traffic_class,
        &nad,
        &l2_domain,
        &l3_domain,
        &learn_mode,
        &l2_ivlan1_cnt
    );

    mbyParserInfo parser_info = { 0 };

    getParserInfo
    (
        mapper_map,
        out,
        rx_port,
        pa_adj_seg_len,
        pa_flags,
        pa_ptrs,
        pa_ptrs_valid,
        realigned_keys,
        is_ipv6,
        &map_prof_key0,
        &parser_info
    );

    mbyMapProfAction map_prof_action = { 0 };
    fm_byte ffu_profile = 0;

    getProfile
    (
        mapper_map,
        out,
        realigned_keys,
        map_prof_key0,
        map_prof_key1,
        &ffu_actions,
        &map_prof_action,
        &ffu_profile
    );

    fm_bool ip_option[2] = { FALSE };
    fm_bool parser_error = FALSE;

    mapRewrite
    (
        mapper_map,
        out,
        pa_ex_parsing_done,
        pa_ex_trunc_header,
        pa_ex_depth_exceed,
        pa_flags,
        realigned_keys,
        realigned_keys_vld,
        is_ipv6,
        ffu_profile,
        map_prof_action,
        map_prof_key0,
        mapped_key,
        pri_profile,
        ip_option,
        &parser_error,
        &ffu_actions,
        &ffu_keys
    );

    // Write outputs:

    out->FFU_ACTIONS      = ffu_actions;
    out->FFU_KEYS         = ffu_keys;
    out->FFU_PROFILE      = ffu_profile;
    out->IP_OPTION[0]     = ip_option[0];
    out->IP_OPTION[1]     = ip_option[1];
    out->L2_IDOMAIN       = l2_domain;
    out->L2_IVLAN1_CNT    = l2_ivlan1_cnt;
    out->L3_IDOMAIN       = l3_domain;
    out->LEARN_MODE       = learn_mode;
    out->NO_PRI_ENC       = no_pri_enc;
    out->NAD              = nad;
    out->OTR_MPLS_V       = otr_mpls_v;
    out->PARSER_ERROR     = parser_error;
    out->PARSER_INFO      = parser_info;
    out->PRIORITY_PROFILE = pri_profile;
    out->RX_PORT          = rx_port;
    out->TRAFFIC_CLASS    = traffic_class;

    // Pass thru:

    out->PARITY_ERROR     = FALSE; // parked at 0 <-- REVISIT!!!
    out->PA_DROP          = in->PA_DROP;
    out->PA_HDR_PTRS      = in->PA_HDR_PTRS;
    out->PA_L3LEN_ERR     = in->PA_L3LEN_ERR;
    out->RX_DATA          = in->RX_DATA;
    out->RX_LENGTH        = in->RX_LENGTH;
}
