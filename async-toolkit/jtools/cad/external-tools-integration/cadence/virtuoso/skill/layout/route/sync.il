; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Sync-to-Netlist with default options.
;
; Copyright 2010 Fulcrum Microsystems.  All rights reserved.

; obsolete form for backward compatibility
(defun SyncToNetlist (@key (CV (geGetEditCellView)) (old nil) (heapSize nil))
  (UpdateFloorplan ?CV CV)
  )

; obsolete form for backward compatibility
(defun nrSyncToNetlist (CV @key (routed nil) (heapSize nil))
  (UpdateFloorplan ?CV CV)
  )
