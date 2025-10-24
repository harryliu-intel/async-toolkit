; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define vb16 (VaryBits.IntBits (bn 16)))
(define vb8 (VaryBits.IntBits (bn 8)))
(VaryBits.Union vb8 vb16)
(VaryBits.Format (VaryBits.Union vb8 vb16))
(VaryBits.Format (VaryBits.FromInterval (FiniteInterval.Construct (bn 8) (bn 16))))
(VaryBits.Format (VaryBits.IntBits (bn 8)))

 (VaryBits.Format (VaryBits.FromIntervalPos (FiniteInterval.Construct (bn -1) (bn 0))))
