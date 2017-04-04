; print out bbox of all V7 touching top-level m8:pin PowerGroundNets
(defun FindV7 (@key (CV (geGetEditCellView)))
  (let (tempCV inLayerMap outLayerMap file bbox pout n)
    ; flatten v7 shapes
    tempCV = (dbOpenCellViewByType CV->libName CV->cellName "temp_FindV7" "maskLayout" "w")
    inLayerMap = (list (list "via7" (list Via[7] "drawing")
                                    (list Via[7] "vdd")
                                    (list Via[7] "gnd")
                                    )
                       )
    outLayerMap = (list (list "via7" (list Via[7] "drawing")))
    (ICVLayerProcessorSimple CV tempCV "mergeShape" inLayerMap outLayerMap)
    (dbSave tempCV)

    ; find which power supply they overlap and print their net and bbox
    n = 0
    file = (strcat (ConfigFileGetValue TheCDSConfigTable "TEMP") "/" CV->cellName ".v7.out")
    pout = (outfile file)
    (foreach pin (setof x CV->shapes
                        x->objType=="rect" && x->pin &&
                        x->lpp==(list "m8" "pin") &&
                        (member x->net->name PowerGroundNets)!=nil)
             (foreach overlap (dbGetOverlaps tempCV pin->bBox (list "v7" "drawing"))
                      bbox = overlap->bBox
                      (fprintf pout "%s %g %g %g %g\n" pin->net->name
                               (car (car bbox)) (cadr (car bbox))
                               (car (cadr bbox)) (cadr (cadr bbox)))
                      n++
                      )
             )
    (close pout)
    n
    )
  )
