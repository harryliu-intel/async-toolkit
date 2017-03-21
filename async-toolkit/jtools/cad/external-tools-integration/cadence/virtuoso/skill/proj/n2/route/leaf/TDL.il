(load "~/cds_wd/N2/n2/reroute.il")

; post-processing to remove M2 fill and delete incorrect GND power hookups
(defun PatchTDL (@key (CV (geGetEditCellView)))
  (let (bbox x0 x1)
    bbox = CV->prBoundary->bBox
    x0 = (car (car  bbox)) + 0.42
    x1 = (car (cadr bbox)) - 0.42
    (foreach x CV->shapes (when x->lpp==(list "m2" "fill") (dbDeleteObject x)))
    (foreach x (setof via CV->vias via->net->name=="GND" && (car via->xy)>=x0 && (car via->xy)<=x1)
             (dbDeleteObject x)
             )
    (dbSave CV)
    CV = (MakeSimpleAbstract ?topMetal 1 ?CV CV)
    )
  )

; route all the TDL's
(for i 1 12
     CV = (RouteTDL (sprintf nil "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L%d-R.10000" 2*i))
     (PatchTDL ?CV CV)
     )

(exit)
