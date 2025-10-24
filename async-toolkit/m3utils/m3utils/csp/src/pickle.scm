; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (pickle-globals! pfx)
  (let ((wr (FileWr.Open (sa pfx ".dmp"))))
    (dis "dumping compiler state for " pfx dnl)
    (dump-environment wr)
    (Wr.Close wr)
    )
  )

(define (unpickle-globals! pfx)
  (let ((rd (FileRd.Open (sa pfx ".dmp"))))
    (dis "loading compiler state for " pfx dnl)
    (load-environment! rd)
    (Rd.Close rd)
    (loaddata1!)
    )
  )
  
