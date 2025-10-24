; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0


(define np  
	`((fanout .,(Name.ParseText "alloc.freelist.fifo_in.arb.l[3,0].0"))
	 (fanin . ,(Name.ParseText "alloc.freelist.fifo_in.arb.bufL[2,0]._rd"))
	 (outDir . Up)
	 (inDir . Down) )
)

(Circuit.AddOverride np 10 11)



