; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SET DESTINATION

;;; whither do we want the output to go
(define deriv-dir "../AMD64_LINUX/")
;;(define deriv-dir "./out/")

;; clear the derived.m3m once per program run, not once per compile
(wr-close (filewr-open (sa deriv-dir "derived.m3m")))

