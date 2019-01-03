#!/bin/sh -x


../AMD64_LINUX/visitor  -m hlp -o hlp.mapfields &
../AMD64_LINUX/visitor  -m mby -o mby.mapfields &
../AMD64_LINUX/visitor  -m rx_ppe -o rx_ppe.mapfields &
../AMD64_LINUX/visitor  -m tx_ppe -o tx_ppe.mapfields &


wait
