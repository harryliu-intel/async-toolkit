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
