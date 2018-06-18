  v9a = 1.85
  v9b = 7.4
  v9c = 3.7

  north_y_coords = (list 48*1.596 65.5*1.596 76*1.596 86*1.596 96*1.596 106*1.596 116*1.596 126.5*1.596 144*1.596 162*1.596)
  north_tm1_height = (list 20*1.596 7*1.596 6*1.596 6*1.596 6*1.596 6*1.596 6*1.596 7*1.596 20*1.596 8*1.596)
  north_v9 = (list v9b v9c v9c v9c v9c v9c v9c v9c v9b v9c)
  north_tm1 = (list north_y_coords north_tm1_height north_v9)
  north_x = (list -3924.48+2.5*1.68 3707.76)

  south_y_coords = (list -282*1.596 -270*1.596 -258*1.596 -240*1.596 -222.5*1.596 -212*1.596 -202*1.596 -192*1.596 -182*1.596 -172*1.596 -161.5*1.596 -144*1.596 -126*1.596 -114*1.596 -102*1.596)
  south_tm1_height = (list 8*1.596 8*1.596 8*1.596 20*1.596 7*1.596 6*1.596 6*1.596 6*1.596 6*1.596 6*1.596 7*1.596 20*1.596 8*1.596 8*1.596 8*1.596)
  south_v9 = (list v9a v9a v9a v9b v9a v9a v9a v9a v9a v9a v9a v9b v9a v9a v9a)
  south_tm1 = (list south_y_coords south_tm1_height south_v9)
  south_x = (list -3911.04+2.5*1.68 3707.76)

(defun FillNorthV9 () (FillV9 north_tm1 north_x))
(defun FillSouthV9 () (FillV9 south_tm1 south_x))
 
(defun FillV9 (tm1list xlist @key (CV (geGetEditCellView)))
 (let (y_coords tm1_height v9_type xstart xend n x h w v9a v9b v9c clear ovrtm ovrtmf ovrm9 ovrce ovrtmw ovrtmw fig)

  y_coords = (nth 0 tm1list)
  tm1_height = (nth 1 tm1list)
  v9_type = (nth 2 tm1list)
  xstart = (car xlist)
  xend = (cadr xlist)

  (DeleteAutoGen)


      ; first pass looks for space empty of both m9 and tm1
  n = 0
  (foreach y y_coords
    x = xstart
    (while x < xend

      h = (nth n tm1_height)
      w = 8*1.68

      clear = t
      ovrtm = (dbGetTrueOverlaps CV (list x-w/2-1.68:y-h/2 x+w/2+1.68:y+h/2) (list "tm1" "drawing") 32 )
      ovrm9 = (dbGetTrueOverlaps CV (list x-1.68/2:y-h/2 x+1.68/2:y+h/2) (list "m9" "drawing") 32 )
      ovrce = (dbGetTrueOverlaps CV (list x-1.68/2:y-h/2 x+1.68/2:y+h/2) (list "ce2" "drawing") 32 )
      (when ovrtm || ovrm9 || ovrce clear=nil)

      (when clear
        h = (max (nth n tm1_height) (nth n v9_type)+6 ) ; 6 is because min v9 enclosure is 3.0u
        w = (max 4*1.68 6+0.8) 
        fig = (dbCreateRect CV Scratch1LPP (list x-w/2:y-h/2 x+w/2:y+h/2) )
	(dbReplaceProp fig  "autogen" "boolean" t)
        fig = (dbCreateRect CV (list "m9" "fill") (list x-0.54:y-h/2 x+0.54:y+h/2) )
	(dbReplaceProp fig  "autogen" "boolean" t)
        h = (nth n v9_type)
        fig = (dbCreateRect CV (list "v9" "drawing") (list x-0.4:y-h/2 x+0.4:y+h/2) )
	(dbReplaceProp fig  "autogen" "boolean" t)
      )
      x = x + 6.72
    )
    n++
  )

  (foreach shape CV->shapes
    (when shape->lpp==Scratch1LPP
      shape->lpp = (list "tm1" "fill")
    )
  )


  ; second pass avoids first pass and makes sure you are inside a tm1 route
  n = 0
  (foreach y y_coords
    x = xstart
    (while x < xend

      h = (nth n tm1_height)
      w = 8*1.68

      clear = t
      ovrtmf = (dbGetTrueOverlaps CV (list x-w/2-1.68:y-h/2 x+w/2+1.68:y+h/2) (list "tm1" "fill") 32 )
      ovrv9 = (dbGetTrueOverlaps CV (list x-w/2-1.68:y-h/2 x+w/2+1.68:y+h/2) (list "v9" "drawing") 32 ) 
      ovrm9 = (dbGetTrueOverlaps CV (list x-1.68/2:y-h/2 x+1.68/2:y+h/2) (list "m9" "drawing") 32 )
      ovrce = (dbGetTrueOverlaps CV (list x-1.68/2:y-h/2 x+1.68/2:y+h/2) (list "ce2" "drawing") 32 )
      (when ovrtmf || ovrv9 || ovrm9 || ovrce clear=nil)

      (when clear
        ;now make sure you don't create drc by placing vias that 
	;are too close to the existing end of a routed wire
        ovrtmw = (dbGetTrueOverlaps CV (list x-3.402:y-h/2 x-3.401:y+h/2) (list "tm1" "drawing") 32 )
        ovrtme = (dbGetTrueOverlaps CV (list x+3.401:y-h/2 x+3.402:y+h/2) (list "tm1" "drawing") 32 )
	(when ovrtmw && ovrtme
          h = (max (nth n tm1_height) (nth n v9_type)+6 )
          w = (max 4*1.68 6+0.8) 
          fig = (dbCreateRect CV (list "m9" "fill") (list x-0.54:y-h/2 x+0.54:y+h/2) )
	  (dbReplaceProp fig  "autogen" "boolean" t)
          h = (nth n v9_type)
          fig = (dbCreateRect CV Scratch1LPP (list x-0.4:y-h/2 x+0.4:y+h/2) )
	  (dbReplaceProp fig  "autogen" "boolean" t)
	)
      )
      x = x + 6.72
    )
    n++
  )

  (foreach shape CV->shapes
    (when shape->lpp==Scratch1LPP
      shape->lpp = (list "v9" "drawing")
    )
  )

      ; yuck. a third pass of floating ones is needed over the pads
  n = 0
  (foreach y y_coords
    x = xstart
    (while x < xend

      h = (nth n tm1_height)
      w = 8*1.68

      clear = t
      ovrtm = (dbGetTrueOverlaps CV (list x-w/2-1.68:y-h/2 x+w/2+1.68:y+h/2) (list "tm1" "drawing") 32 )
      ovrtmf = (dbGetTrueOverlaps CV (list x-w/2-1.68:y-h/2 x+w/2+1.68:y+h/2) (list "tm1" "fill") 32 )
      (when ovrtm || ovrtmf clear=nil)

      (when clear
        h = (max (nth n tm1_height) (nth n v9_type)+6 ) ; 6 is because min v9 enclosure is 3.0u
        w = (max 4*1.68 6+0.8) 
        fig = (dbCreateRect CV (list "tm1" "fill") (list x-w/2:y-h/2 x+w/2:y+h/2) )
	(dbReplaceProp fig  "autogen" "boolean" t)
        ;fig = (dbCreateRect CV (list "m9" "fill") (list x-0.54:y-h/2 x+0.54:y+h/2) )
	;(dbReplaceProp fig  "autogen" "boolean" t)
        h = (nth n v9_type)
        fig = (dbCreateRect CV (list "v9" "drawing") (list x-0.4:y-h/2 x+0.4:y+h/2) )
	(dbReplaceProp fig  "autogen" "boolean" t)
      )
      x = x + 6.72*2
    )
    n++
  )

  ; hack to allow me to manually paint filler tm1 and still get dummy vias on it 
  ; without messing up the earlier overlap checks 
  CVtm = (dbFindAnyInstByName CV "m9tm")
  (foreach shape CVtm->shapes
    (when shape->lpp==Scratch2LPP
      fig = (dbCreateRect CV (list "tm1" "fill") shape->bBox )
    )
  )


  (dbSave CV)
 )
)

(defun Check672Align (@key (CV (geGetEditCellView)))
 (let (x cnt)
  cnt=0
  (foreach inst CV->instances
    x=(car inst->xy)
    x=(floor x*1000)
    (when (mod x 6720)!=0
      cnt++
      (printf "%s aint right. x=%f\n" inst->name (car inst->xy))
      (when nil 
        inst->xy = (list (floor x/6720)*6.72 (cadr inst->xy))
      )
    )
  )
  cnt
 )
)
