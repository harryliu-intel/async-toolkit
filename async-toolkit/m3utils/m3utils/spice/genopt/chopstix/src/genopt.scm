; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0


(dis "**********  Setting up genopt environment  **********" dnl)

(GenOpt.OptInit)
(QuadOpt.OptInit)
(QuadRobust.OptInit)

(GenOpt.SetCallback (make-cb-obj))

(dis "**********  Done setting up genopt environment  **********" dnl)
