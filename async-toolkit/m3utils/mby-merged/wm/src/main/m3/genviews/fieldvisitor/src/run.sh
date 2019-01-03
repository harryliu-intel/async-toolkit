#!/bin/sh -x
VISITOR=../AMD64_LINUX/fieldvisitor

$VISITOR  -m hlp_top_map -o hlp_top_map.mapfields &
$VISITOR  -m mby_top_map -o mby_top_map.mapfields &
$VISITOR  -m mby_ppe_rx_top_map -o mby_ppe_rx_top_map.mapfields &
$VISITOR  -m mby_ppe_tx_top_map -o mby_ppe_tx_top_map.mapfields &

wait
