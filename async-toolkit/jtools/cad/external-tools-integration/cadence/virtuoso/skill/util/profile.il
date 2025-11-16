; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Profiling macro, usage: (Profile "file.prof" (Function ...))
(defmacro Profile (file @key (profileField ''time)
                        @rest body)
  `(let (ret)
     (profileReset)
     (profile ,profileField)
     ret=(errset ,@body)
     (profileSummary ?children t ?file ,file)
     (car ret)
     )
  )
