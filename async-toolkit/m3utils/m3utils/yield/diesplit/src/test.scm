; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(load "yield.scm" "display")
(define x (Mpfr.New 200))
(Mpfr.SetLR x 0 'N)
(Mpfr.Div x x x 'N)

(dis (Mpfr.Format x 10 'N) dnl)
