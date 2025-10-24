; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;; script file


(load "dir2.scm")

(load "problem-definition.scm")

(load "dir3.scm")

(reset)
(go)
(setup-minimization!)
(search-loop)



(load "do-final-output.scm")
