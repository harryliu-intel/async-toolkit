; draw bumps given a pattern and 2D list of bump names
(defun DrawBumps (names
                  @key
                  (stagger nil)
                  (xpitch 96*1.68)
                  (ypitch 96*1.596)
                  (cellName "globals.BUMP.2")
                  (filename "bumps.txt")
                  (CV (geGetEditCellView)))
  (let (file rows cols xoff yoff x y name instname bump bumpNum)
    bumpNum = (makeTable "bumpNum" 0)
    bump = (dbOpenCellViewByType (GetCastLibName cellName) cellName "layout")
    file = (outfile filename)
    (fprintf file "# %s\n" CV->cellName)
    rows = (length names)
    cols = (length (car names))
    xoff = (if stagger xpitch/4 xpitch/2)
    yoff = ypitch/2
    (dbCreatePRBoundary CV (RectGetPoints (list 0:0 cols*xpitch:rows*ypitch)))
    (for row 0 rows-1
         y = yoff + ypitch*row
         (for col 0 cols-1
              x = xoff + xpitch*col + (if stagger && (mod row 2)==1 xpitch/2 0)
              name = (nth col (nth row names))
              (when name
                instname = (if bumpNum[name]==0 (sprintf nil "bump_%s" name)
                             (sprintf nil "bump_%s_%d" name bumpNum[name]))
                (dbCreateInst CV bump instname x:y "R0")
                (dbCreateLabel CV (list "tm1" "drawing") x:y name "centerCenter" "R0" "stick" 15)
                bumpNum[name]++
                (fprintf file "%s : %g %g\n" name x y)
                )
              )
         )
    (close file)
    (dbSave CV)
    )
  t
  )

; label top-level bumps based on subcells of *_bumps
(defun LabelBumps (@key (CV (geGetEditCellView)))
  (let (inst transform x y rect pinfile finished pcre num)
    num =0
    pcre = (pcreCompile ".*_bumps")
    finished = (makeTable "finished" nil)
    pinfile = (outfile "n2_bumps.die")
    (DeleteAutoGen ?CV CV)
    (foreach inst (setof x CV->instances (pcreExecute pcre x->cellName))
             transform = (dbGetInstTransform inst)
             (foreach fig (setof x inst->master->shapes x->objType=="label" && x->lpp==(list "tm1" "drawing"))
                      fig = (dbCopyFig fig CV transform)
                      (dbReplaceProp fig "autogen" "boolean" t)
                      (cond (finished[fig->xy] (dbDeleteObject fig)) ; delete duplicates
                            (t
                             finished[fig->xy] = t
                             fig->lpp = (list "tm1" "drawing")
                             fig->height = 8
                             x = (car fig->xy)
                             y = (cadr fig->xy)
                             rect = (dbCreateRect CV (list "tm1" "drawing")
                                                  (list x-1.68:y-1.596 x+1.68:y+1.596))
                             rect->net = (dbMakeNet CV fig->theLabel)
                             (dbCreatePin rect->net rect)
                             (dbReplaceProp rect "autogen" "boolean" t)
                             (fprintf pinfile "%d bumpR1_85x85um %g %g 0 UNDEFINED %s, | (GRD,GRID) (PRB,true)
 (BUMPCELL,DUMMY) (LAYER_TYPE,C4BUMP_WBPAD) (NET_EXT,%s) (TSV,false)\n"
                                      num++ x y fig->theLabel fig->theLabel)                             
                             )
                            )
                      )
             )
    (close pinfile)
    )
  t
  )

;;;;;;;;;;;;;;;;;;;; Create bumps for N2 assuming square pitch, 6 bumps tile width ;;;;;;;;;;;;;;;

(defun DrawBumpsN2 ()
  
  ; SOUTH bumps for FPIO, resn, clk (2 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.south" "chip.n2.south.SOUTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list (list nil    "resn"  "ip" "op" "clk"   "VSS")
                   (list "VDDM" "VDDIOS" "id" "od" "VDDIOS" "VDDM"))
             ?filename "n2_bumps_s.txt" ?CV CV)


  ; Lakemont bumps (2 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.lakemont" "chip.n2.lakemont.LAKEMONT.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list (list "VSS"  "VSS"  "VSS" "VSS" "VSS"  "VSS")
                   (list "VDDA" "VDDM" "VDD" "VDD" "VDDM" "VDDM")
                   )
             ?filename "n2_bumps_lakemont.txt" ?CV CV)

  ; MESH bumps (45 rows tall)
  CV = (dbOpenCellViewByType "core.neuro.mesh" "core.neuro.mesh.MESH.20000_bumps" "layout" "maskLayout" "w")
  bumps = nil
  (for y 0 44
       (cond ((mod y 4)==1 rail="VDD")
             ((mod y 4)==3 rail="VDDM")
             (t rail="VSS")
             )
       row = nil
       (for x 0 23 row = (cons rail row))
       bumps = (append bumps (list row))
       )
  (DrawBumps bumps ?filename "n2_bumps_mesh.txt" ?CV CV)

  ; NW bumps for bd(17) x input (3 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.north" "chip.n2.north.NW.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list (list "VDD" "VDD" "VDD" "VDD" "VDDION" "VDDION" "VDDION" "VDDION" "VDD" "VDD" "VDD" "VDD")
                   (list nil   "x15" "x13" "x11" "x9"     "VSS"    "VSS"    "xa"     "x7"  "x5"  "x3"  "x1")
                   (list "x16" "x14" "x12" "x10" "x8"     "VDDION" "VDDION" "xq"     "x6"  "x4"  "x2"  "x0")
                   )
             ?filename "n2_bumps_nw.txt" ?CV CV)

  ; NE bumps for bd(17) y output (3 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.north" "chip.n2.north.NE.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list (list "VDD" "VDD" "VDD" "VDD" "VDDION" "VDDION"  "VDDION" "VDDION" "VDD" "VDD" "VDD" "VDD")
                   (list "y0"  "y2"  "y4"  "y6"  "yq"     "VSS"     "VSS"    "y8"     "y10" "y12" "y14" "y16")
                   (list "y1"  "y3"  "y5"  "y7"  "ya"     "VDDION"  "VDDION" "y9"     "y11" "y13" "y15" nil)
                   )
             ?filename "n2_bumps_ne.txt" ?CV CV)
  )
