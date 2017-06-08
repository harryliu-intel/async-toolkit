; Draw dummy v8, m9 fill over CHIP_HALO regions
(defun HaloV8 (@key (CV (geGetEditCellView)))
  (let (via8 v9_w v9_h m9_h m9_w x y do_via sx sy blah inst odd)
    inst = (dbFindAnyInstByName CV "vias")
    (when inst (dbDeleteObject inst))
    overhang = 0.52 ; minimum m8 overhang from center of v8
    viaCV = (dbOpenCellViewByType CV->libName (strcat CV->cellName "_vias") "layout" "maskLayout" "w")
    via8 = (techFindViaDefByName (techGetTechFile CV) "via8")
    v8_w = 0.88
    v8_h = 0.38
    m9_w = 1.08
    m9_h = 1.596-0.56
    (foreach mosaic (setof x CV->instances
                           x->cellName=="globals.CHIP_HALO.fill" ||
                           x->cellName=="globals.CHIP_HALO.fill_b")
      (when (car mosaic->tileArray)!="R0" (geSelectObject mosaic) (error "Only R0"))
      (when mosaic->uX!=1.68 (geSelectObject mosaic) (error "Only 1.68um x pitch"))
      (when mosaic->uY!=1.596 (geSelectObject mosaic) (error "Only 1.596um y pitch"))
      (for r 0 mosaic->rows-1
        (for c 0 mosaic->columns-1
          sx = (car  mosaic->xy)+1.68*c + 0.84
          sy = (cadr mosaic->xy)+1.596*r + 0.798
          x = (floor sx/1.68)
          y = (floor sy/1.596)
          do_via = (mod (mod x 2)+2 2)==(mod (mod y 2)+2 2)
          odd = (mod (mod y 2)+2 2)==1
          top = (r==mosaic->rows-1)
          bot = (r==0)
          (cond ( top &&  bot yu=0.56 yd=0) ; kind of a hack for 1 row tall CHIP_HALO mosaic
                ( odd && !bot yd=0.56 yu=0)
                ( odd &&  bot yu=0.56 yd=0)
                (!odd && !top yu=0.56 yd=0)
                (!odd &&  top yd=0.56 yu=0)
                )
          blah = (when do_via
                   (dbCreateVia viaCV via8 sx:sy "R0"
                                (list (list "cutWidth" v8_w) (list "cutHeight" v8_h)
                                      (list "layer2Enc" (m9_w-v8_w)/2:(m9_h-v8_h)/2)))
                   )
          (dbReplaceProp blah "autogen" "boolean" t)
          (dbCreateRect viaCV (list "m9" "drawing")
                        (list sx-m9_w/2:sy-m9_h/2-yd sx+m9_w/2:sy+m9_h/2+yu))
          (dbReplaceProp blah "autogen" "boolean" t)
          )
        )
      )
    (leMergeShapes (setof x viaCV->shapes x->lpp==(list "m9" "drawing")))
    (dbSave viaCV)
    (dbCreateInst CV viaCV "vias" 0:0 "R0")
    (dbSave CV)
    viaCV
    )
  t
  )
