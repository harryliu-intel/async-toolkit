/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_LOG_TYPE_H
#define MBY_LOG_TYPE_H

#define MBY_LOG_TYPE_TRIG_LOG_ACTION  (1 << 0)
#define MBY_LOG_TYPE_CGRP             (1 << 1)
#define MBY_LOG_TYPE_RESERVED_MAC     (1 << 2)
#define MBY_LOG_TYPE_ARP_REDIRECT     (1 << 3)
#define MBY_LOG_TYPE_ICMP             (1 << 4)
#define MBY_LOG_TYPE_TTL_IP_MC        (1 << 5)
#define MBY_LOG_TYPE_IP_UCST_L2_MCST  (1 << 7) /* EAC TBR */

#endif // MBY_LOG_TYPE_H
