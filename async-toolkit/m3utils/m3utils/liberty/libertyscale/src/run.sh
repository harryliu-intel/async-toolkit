#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

time ../AMD64_LINUX/libertyscale -i lib1.lib -o lib167.lib -timing_type min_pulse_width -values ocv_sigma_rise_constraint -values ocv_sigma_fall_constraint -factor 0.167
