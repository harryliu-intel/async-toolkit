; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define constants
  `((constants parser-sizes
               u64 ;; doesnt matter here
               ((m3 MbyParserSizes))

               (
                (n-meta 32)
                (n-keys 84)
                (n-flags 48)
                (n-ptrs 8)
                )
               )))

(compile! constants)



