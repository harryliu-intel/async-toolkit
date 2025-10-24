# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



TARG=$1
shift

if     [ "$TARG" = "mby"   ]; then
    ana_map="mby_top_map"
    mapf="mby"
    bits=30
elif   [ "$TARG" = "rx_ppe" ]; then
    ana_map="mby_ppe_rx_top_map"
    mapf="rx_ppe"
    bits=28
elif   [ "$TARG" = "tx_ppe" ]; then
    ana_map="mby_ppe_tx_top_map"
    mapf="tx_ppe"
    bits=28
else
    echo "targ must be mby, rx_ppe, or tx_ppe"
    exit 1
fi
