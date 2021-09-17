#!/bin/sh -x

awk '{print $3}' ../../decode_chipid/data > svl_power.data
awk '{print $1 + 36.0}' svl_power.data > svl_power_margined.data

../AMD64_LINUX/histogram svl_power_margined 10 < svl_power_margined.data
