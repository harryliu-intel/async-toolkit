; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0



;shortcuts
(defun wcv () ( geGetWindowCellView ) ) ;Window Cell View
(defun ss () ( geGetSelectedSet ) ) ; Selected Set
(defun fp () (View "floorplan") (LoadWires))

(defun FulcrumDefaults ()
  ;ssh, don't tell anyone!
  ExpertUser=nil
)


