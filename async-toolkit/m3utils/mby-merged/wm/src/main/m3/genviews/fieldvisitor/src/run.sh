#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

VISITOR=../AMD64_LINUX/fieldvisitor

$VISITOR  -m hlp_top_map -o hlp_top_map.mapfields &
$VISITOR  -m mby_top_map -o mby_top_map.mapfields &
$VISITOR  -m mby_ppe_rx_top_map -o mby_ppe_rx_top_map.mapfields &
$VISITOR  -m mby_ppe_tx_top_map -o mby_ppe_tx_top_map.mapfields &

wait
