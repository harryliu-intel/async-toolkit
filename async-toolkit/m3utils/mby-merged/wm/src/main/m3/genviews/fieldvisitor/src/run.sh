#!/bin/sh -x


../AMD64_LINUX/visitor  -m hlp_top_map -o hlp_top_map.mapfields &
../AMD64_LINUX/visitor  -m mby_top_map -o mby_top_map.mapfields &
../AMD64_LINUX/visitor  -m mby_ppe_rx_top_map -o mby_ppe_rx_top_map.mapfields &
../AMD64_LINUX/visitor  -m mby_ppe_tx_top_map -o mby_ppe_tx_top_map.mapfields &


wait
