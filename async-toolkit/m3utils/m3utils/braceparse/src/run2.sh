# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

time ../AMD64_LINUX/netlist2goxrpt -f example2.net -t transistor.cells -T gox -w '(lambda(nfin) (if (= nfin 1) 30000 (- (* 30000 nfin) 22000)))' -l 1 -r test5
