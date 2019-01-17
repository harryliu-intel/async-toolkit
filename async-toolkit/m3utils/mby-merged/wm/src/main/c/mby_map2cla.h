// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_MAP2CLA_H
#define MBY_MAP2CLA_H

#include "fm_types.h"
#include "mby_cgrp_types.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyMapperToClassifierStruct
{
    mbyClassifierActions CLASSIFIER_ACTIONS;///< Classifier actions
    mbyClassifierKeys    CLASSIFIER_KEYS;   ///< Classifier TCAM lookup keys
    fm_byte              PACKET_PROFILE;    ///< 6-bit packet profile ID
    fm_bool              IP_OPTION[2];      ///< Trap_ip_iptions count
    fm_uint16            L2_IDOMAIN;        ///< L2 ingress domain
    fm_uint16            L2_IVLAN1_CNT;     ///< Ingress VLAN counter
    fm_byte              L3_IDOMAIN;        ///< L3 ingress domain
    fm_bool              LEARN_MODE;        ///< Learning mode: 0=SVL, 1=IVL
    fm_bool              NAD;               ///< 4-bit NAD (network addressing domain)
    fm_bool              NO_PRI_ENC;        ///< Mapper priority encoding
    fm_bool              OTR_MPLS_V;        ///< Parser outer MPLS packet valid
    fm_bool              PARSER_ERROR;      ///< Header parse error
    mbyParserInfo        PARSER_INFO;       ///< Parser info structure
    fm_byte              PRIORITY_PROFILE;  ///< 5-bit classifier priority profile
    fm_uint32            RX_PORT;           ///< Ingress port
    fm_byte              TRAFFIC_CLASS;     ///< 3-bit traffic class
    // pass-thru:
    fm_bool              PARITY_ERROR;      ///< parity error detected flag
    fm_bool              PA_DROP;           ///< checksum validation error, drop pkt in tail
    mbyParserHdrPtrs     PA_HDR_PTRS;       ///< parser header pointers
    fm_bool              PA_L3LEN_ERR;      ///< l3 length error
    fm_uint32            RX_LENGTH;         ///< Ingress packet data length [bytes]

} mbyMapperToClassifier;

#endif // MBY_MAP2CLA_H
