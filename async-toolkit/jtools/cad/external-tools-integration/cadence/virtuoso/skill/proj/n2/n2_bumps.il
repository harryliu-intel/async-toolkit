; draw bumps given a pattern and 2D list of bump names
(defun DrawBumps (names
                  @key
                  (stagger nil)
                  (xpitch 96*1.68)
                  (ypitch 96*1.596)
                  (delta_west -32*1.68) ; optionally shift westmost bumps
                  (delta_east  32*1.68) ; optionally shift eastmost bumps
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
              x = xoff + xpitch*col + (if stagger && (mod row 2)==1 xpitch/2 0) +
                (if col==0 delta_west 0) + (if col==cols-1 delta_east 0)
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

  ; SOUTH bumps (48 wide by 3 tall).  Includes S 2*bd(9), FPIO+clk+resn, E 2*bd(9)
  CV = (dbOpenCellViewByType "chip.n2.south" "chip.n2.south.SOUTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list
		(list
                 ; PIO (2*9 bumps)
                 nil         "Y[1].D[7]" "Y[1].D[5]" "VDDIO" "Y[1].C.a" "VDDIO" "Y[1].D[3]" "Y[1].D[1]" "VDDIO"
                 "X[1].D[0]" "X[1].D[2]" "X[1].D[4]" "VDDIO" "X[1].C.q" "VDDIO" "X[1].D[6]" "X[1].D[8]" "VDDIO"
                 ; unused (6 bumps)
                 "VDDIO" "VDDIO" "VDDIO" "VDDIO" "VDDIO" "VDDIO"
                 ; FPIO (6 bumps)
                 "VDDIO" "resn" "VDDIO" "VDDIO" "clk" "VDDIO"
                 ; PIO (2*9 bumps)
                 "VDDIO"     "X[0].D[7]" "X[0].D[5]" "VDDIO" "X[0].C.a" "VDDIO" "X[0].D[3]" "X[0].D[1]" "VDDIO"
                 "Y[0].D[0]" "Y[0].D[2]" "Y[0].D[4]" "VDDIO" "Y[0].C.q" "VDDIO" "Y[0].D[6]" "Y[0].D[8]" "VDDIO"
		)
		(list
                 ; PIO (2*9 bumps) (the nil is to avoid bump symmetry, but would otherwise be VDDM)
                 nil   "Y[1].D[8]" "Y[1].D[6]" "GND" "Y[1].C.q" "GND" "Y[1].D[4]" "Y[1].D[2]" "Y[1].D[0]"
                 "Vdd" "X[1].D[1]" "X[1].D[3]" "GND" "X[1].C.a" "GND" "X[1].D[5]" "X[1].D[7]" "VDDM"
                 ; unused (6 bumps)
                 "GND" "GND" "GND" "GND" "GND" "GND"
                 ; FPIO (6 bumps)
		 "ip" "id" "GND" "GND" "od" "op"
                 ; PIO (2*9 bumps)
                 "VDDM" "X[0].D[8]" "X[0].D[6]" "GND" "X[0].C.q" "GND" "X[0].D[4]" "X[0].D[2]" "X[0].D[0]"
                 "Vdd"  "Y[0].D[1]" "Y[0].D[3]" "GND" "Y[0].C.a" "GND" "Y[0].D[5]" "Y[0].D[7]" "VDDM"
		)
		(list
                 ; 4 empty boxes, possibly to become memory or T0A
                 "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 ; IO
                 "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 ; LAKEMONT's
                 "VDDA" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 "VDDA" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 "VDDA" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
                 )
		)
             ?filename "n2_bumps_s.txt" ?CV CV
	     ;?delta_west 0
	     ;?delta_east 0
	     )

  ; MESH bumps (48 wide by 45 rows tall, starts and ends with a row of GND)
  CV = (dbOpenCellViewByType "core.neuro.mesh" "core.neuro.mesh.MESH.20000_bumps" "layout" "maskLayout" "w")
  bumps = nil
  (for y 0 44
       (cond ((mod y 4)==1 rail="Vdd")
             ((mod y 4)==3 rail="VDDM")
             (t rail="GND")
             )
       row = nil
       (for x 0 47 row = (cons rail row))
       bumps = (append bumps (list row))
       )
  (DrawBumps bumps ?filename "n2_bumps_mesh.txt" ?CV CV
	     ;?delta_west 0
	     ;?delta_east 0
	     )

  ; NORTH bumps (48 wide by 2 tall)
  CV = (dbOpenCellViewByType "chip.n2.north" "chip.n2.north.NORTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list
              (list
               ; PIO
               "VDDM"      "Y[0].D[7]" "Y[0].D[5]" "GND" "Y[0].C.a" "GND" "Y[0].D[3]" "Y[0].D[1]" "Vdd"
               "X[0].D[0]" "X[0].D[2]" "X[0].D[4]" "GND" "X[0].C.q" "GND" "X[0].D[6]" "X[0].D[8]" "VDDM"
               ; unused
               "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
               "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
               ; PIO
               "VDDM"      "X[1].D[7]" "X[1].D[5]" "GND" "X[1].C.a" "GND" "X[1].D[3]" "X[1].D[1]" "Vdd"
               "Y[1].D[0]" "Y[1].D[2]" "Y[1].D[4]" "GND" "Y[1].C.q" "GND" "Y[1].D[6]" "Y[1].D[8]" "VDDM"
               )
              (list
               ; PIO
               "VDDIO" "Y[0].D[8]" "Y[0].D[6]" "VDDIO" "Y[0].C.q" "VDDIO" "Y[0].D[4]" "Y[0].D[2]" "Y[0].D[0]"
               "VDDIO" "X[0].D[1]" "X[0].D[3]" "VDDIO" "X[0].C.a" "VDDIO" "X[0].D[5]" "X[0].D[7]" "VDDIO"
               ; unused
               "VDDIO" "VDDIO" "VDDIO" "VDDIO" "VDDIO" "VDDIO"
               "VDDIO" "VDDIO" "VDDIO" "VDDIO" "VDDIO" "VDDIO"
               ; PIO
               "VDDIO" "X[1].D[8]" "X[1].D[6]" "VDDIO" "X[1].C.q" "VDDIO" "X[1].D[4]" "X[1].D[2]" "X[1].D[0]"
               "VDDIO" "Y[1].D[1]" "Y[1].D[3]" "VDDIO" "Y[1].C.a" "VDDIO" "Y[1].D[5]" "Y[1].D[7]" nil
               )
              )
             ?filename "n2_bumps_n.txt" ?CV CV
	     ;?delta_west 0
	     ;?delta_east 0
	     )
)

(defun bmp ()
 (load "vip/proj/n2/n2_bumps.il")
 (DrawBumpsN2)
)
