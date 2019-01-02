#!/bin/sh -x
VISITOR=../AMD64_LINUX/fieldvisitor

$VISITOR  -m hlp -o hlp.mapfields &
$VISITOR  -m mby -o mby.mapfields &
$VISITOR  -m rx-ppe -o rx_ppe.mapfields &
$VISTIOR  -m tx-ppe -o tx_ppe.mapfields &


wait
