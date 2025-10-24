; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(require-modules "m3")
(load "tfc-yield.scm")
(load "yield.scm")
(load "direct-yield.scm")
(load "reports-code.scm")
(load "defs-21ww07.scm")

(load "tfc-yield-2.scm")
(load "reports-21ww25.scm")

(define params `((,stapper-d0 ,*alpha*)))

(report-twodie)
