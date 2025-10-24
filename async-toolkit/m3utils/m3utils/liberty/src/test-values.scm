; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(load "types.scm")
(load "liberty-utils.scm")

(define rd (FileRd.Open "lib783_i0s_160h_50pp_seq_ulvt_tttt_0p300v_85c_tttt_cmax_ccslnt.lib"))

(define *lib* (LibertyParse.Parse rd))
