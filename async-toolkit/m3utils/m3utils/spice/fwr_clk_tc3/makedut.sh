# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

sed -e 's/\(^v.*\)0.3$/\1vminer/' -e 's/\(^v.*\)0.6$/\1vchannel/' -e 's/\(^v.*\)0.75$/\1vnotch/' -e 's/\(^v.*\)1.2$/\1vanalog/' dut.sp.tmpl.orig > dut.sp.tmpl
