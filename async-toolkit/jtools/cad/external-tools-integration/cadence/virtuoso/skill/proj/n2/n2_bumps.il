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
  
  ; SOUTH bumps includes S bd(9), E bd(9), FPIO, resn, clk (2 rows tall) plus a row of LMT/T0A
  CV = (dbOpenCellViewByType "chip.n2.south" "chip.n2.south.SOUTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list
		(list 
		      nil   "sy7" "sy5" "VDDIOS" "VDDIOS" "sya" "sy3"  "sy1" 
		      "GND"
		      "sx0" "sx2"  "sxq" "VDDIOS" "VDDIOS" "sx4" "sx6" "sx8" 

		      "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd"
		      "GND" "resn" "VDDIOS" "VDDIOS" "clk" "GND"
		      "Vdd" 

		      "ex8" "ex6" "ex4" "VDDIOS" "VDDIOS" "exq" "ex2" "ex0"
		      "GND" "ey1" "ey3" "eya" "VDDIOS" "VDDIOS" "ey5" "ey7" "GND"
		)
		(list 
		      "sy8" "sy6" "sy4" "GND" "GND" "syq" "sy2" "sy0"
		      "GND"
		      "sx1" "sx3" "sxa" "GND" "GND" "sx5" "sx7" "GND"

		      "GND" "GND" "GND" "GND" "GND" "GND" "GND"
		      "id" "od" "GND" "GND" "ip" "op"
		      "GND" 

		      "GND" "ex7" "ex5" "GND" "GND" "exa" "ex3" "ex1"
		      "GND" "ey0" "ey2" "eyq" "GND" "GND" "ey4" "ey6" "ey8"
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
		)

		)
             ?filename "n2_bumps_s.txt" ?CV CV)





  ; MESH bumps (45 rows tall)
  CV = (dbOpenCellViewByType "core.neuro.mesh" "core.neuro.mesh.MESH.20000_bumps" "layout" "maskLayout" "w")
  bumps = nil
  (for y 0 44
       (cond ((mod y 4)==1 rail="VDD")
             ((mod y 4)==3 rail="VDDM")
             (t rail="GND")
             )
       row = nil
       (for x 0 47 row = (cons rail row))
       bumps = (append bumps (list row))
       )
  (DrawBumps bumps ?filename "n2_bumps_mesh.txt" ?CV CV)

  ; NORTH bumps for bd(17) x input (2 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.north" "chip.n2.north.NORTH.20000_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list 
                   (list nil
		   	 "wy7" "wy5" "GND"    "GND"    "wya" "wy3" "wy1" "GND"
		         "wx0" "wx2" "wxq" "GND" "GND" "wx4" "wx6" "wx8" "Vdd"
			 "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd" "Vdd"
			 "nx7" "nx5" "GND" "GND" "nxa" "nx3" "nx1" "GND"
			 "ny0" "ny2" "nyq" "GND" "GND" "ny4" "ny6" "ny8"
		   	 )
                   (list  
		   	 "wy8" "wy6" "wy4" "VDDION" "VDDION" "wyq" "wy2" "wy0" "GND"
		         "wx1" "wx3" "wxa" "VDDION" "VDDION" "wx5" "wx7" "GND"
			 "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND" "GND"
			 "nx8" "nx6" "nx4" "VDDION" "VDDION" "nxq" "nx2" "nx0" "GND"
			 "ny1" "ny3" "nya" "VDDION" "VDDION" "ny5" "ny7" 
			 )
		   )
             ?filename "n2_bumps_n.txt" ?CV CV)


)


(defun bmp () 
 (load "vip/proj/n2/n2_bumps.il")
 (DrawBumpsN2)
)
