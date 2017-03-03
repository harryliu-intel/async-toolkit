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
  
  ; SOUTH bumps includes S bd(9), E bd(9), FPIO, resn, clk (2 rows tall) plus a row of LMT/T0A
  CV = (dbOpenCellViewByType "chip.n2.south" "chip.n2.south.SOUTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list
		(list 
		      nil
		      "Y[1].D[7]" "Y[1].D[5]" "VDDIO" "Y[1].C.a" "VDDIO" "Y[1].D[3]" "Y[1].D[1]" 
		      "VDDIO"
		      "X[1].D[0]" "X[1].D[2]"  "X[1].D[4]" "VDDIO" "X[1].C.q" "VDDIO" "X[1].D[6]" "X[1].D[8]" 
		      "VDDIO"
		      "Vdd" "Vdd" "Vdd" "Vdd" "Vdd"
		      "GND" "resn" "VDDIO" "VDDIO" "clk" "GND" "GND"

		      "VDDIO"
		      "X[0].D[7]" "X[0].D[5]" "VDDIO" "X[0].C.a" "VDDIO" "X[0].D[3]" "X[0].D[1]"
		      "VDDIO" 
		      "Y[0].D[0]" "Y[0].D[2]" "Y[0].D[4]" "VDDIO" "Y[0].C.q" "VDDIO" "Y[0].D[6]" "Y[0].D[8]"
		      "VDDIO"
		)
		(list 
		      nil ; "VDDM" this is the missing bump for asymmetry
		      "Y[1].D[8]" "Y[1].D[6]" "GND" "Y[1].C.q" "GND" "Y[1].D[4]" "Y[1].D[2]" "Y[1].D[0]"
		      "Vdd"
		      "X[1].D[1]" "X[1].D[3]" "GND" "X[1].C.a" "GND" "X[1].D[5]" "X[1].D[7]" "VDDM"

		      "GND" "GND" "GND" "GND" "GND" "GND"
		      "id" "od" "GND" "GND" "ip" "op"
		      
		      "VDDM"
		      "X[0].D[8]" "X[0].D[6]" "GND" "X[0].C.q" "GND" "X[0].D[4]" "X[0].D[2]" "X[0].D[0]"
		      "Vdd" "Y[0].D[1]" "Y[0].D[3]" "GND" "Y[0].C.a" "GND" "Y[0].D[5]" "Y[0].D[7]" "VDDM"
		)

		(list 
		      "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
		      "Vdd" "Vdd" "VDDM" "VDDM" "Vdd" "Vdd"
		      "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
		      "Vdd" "Vdd" "VDDM" "VDDM" "Vdd" "Vdd"
		      "VDDM" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
		      "VDDA" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
		      "VDDA" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
		      "VDDA" "VDDM" "Vdd" "Vdd" "VDDM" "VDDM"
		      "Vdd"
		)

		)
             ?filename "n2_bumps_s.txt" ?CV CV
	     ;?delta_west 0
	     ;?delta_east 0
	     )

  ; MESH bumps (45 rows tall)
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

  ; NORTH bumps for bd(17) x input (2 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.north" "chip.n2.north.NORTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list 
                   (list "VDDM"
		   	 "Y[0].D[7]" "Y[0].D[5]" "GND" "Y[0].C.a" "GND" "Y[0].D[3]" "Y[0].D[1]" "Vdd"
		         "X[0].D[0]" "X[0].D[2]" "X[0].D[4]" "GND" "X[0].C.q" "GND" "X[0].D[6]" "X[0].D[8]" "VDDM"
			 "VDDM" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "VDDM"
			 "VDDM"
			 "X[1].D[7]" "X[1].D[5]" "GND" "X[1].C.a" "GND" "X[1].D[3]" "X[1].D[1]" "GND"
			 "Y[1].D[0]" "Y[1].D[2]" "Y[1].D[4]" "GND" "Y[1].C.q" "GND" "Y[1].D[6]" "Y[1].D[8]" "VDDM"
		   	 )
                   (list "VDDION"
		   	 "Y[0].D[8]" "Y[0].D[6]" "VDDIO" "Y[0].C.q" "VDDIO" "Y[0].D[4]" "Y[0].D[2]" "Y[0].D[0]" "VDDIO"
		         "X[0].D[1]" "X[0].D[3]" "VDDIO" "X[0].C.a" "VDDIO" "X[0].D[5]" "X[0].D[7]" "VDDIO"
			 "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND"
			 "VDDIO"
			 "X[1].D[8]" "X[1].D[6]" "VDDIO" "X[1].C.q" "VDDIO" "X[1].D[4]" "X[1].D[2]" "X[1].D[0]" "VDDIO"
			 "Y[1].D[1]" "Y[1].D[3]" "VDDIO" "Y[1].C.a" "VDDIO" "Y[1].D[5]" "Y[1].D[7]" 
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
