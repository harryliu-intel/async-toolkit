; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;
;;
;; To use this code to zero the numbers in a .lib file, also:
;; setenv SCANZERO 

(load "types.scm")
(load "liberty-utils.scm")

(define rd (FileRd.Open "N5_TYPE1_LVL.lib"))

(define lib (LibertyParse.Parse rd))

(Rd.Close rd)

(define wr (FileWr.Open "out.lib"))


(Wr.PutText wr (format-comp lib))

(Wr.Close wr)

