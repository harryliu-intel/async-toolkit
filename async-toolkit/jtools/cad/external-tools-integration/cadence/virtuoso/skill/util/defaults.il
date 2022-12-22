

;shortcuts
(defun wcv () ( geGetWindowCellView ) ) ;Window Cell View
(defun ss () ( geGetSelectedSet ) ) ; Selected Set
(defun fp () (View "floorplan") (LoadWires))

(defun FulcrumDefaults ()
  ;ssh, don't tell anyone!
  ExpertUser=nil
)


