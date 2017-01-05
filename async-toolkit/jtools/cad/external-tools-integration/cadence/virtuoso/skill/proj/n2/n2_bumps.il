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
  
  ; SOUTH bumps includes S bd(17), E bd(9), FPIO, resn, clk (2 rows tall) plus a row of LMT/T0A
  CV = (dbOpenCellViewByType "chip.n2.south" "chip.n2.south.SOUTH.20001_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list
		(list 
		      nil   "sx15" "sx13" "sx11" "sx9" "VDDIOS" "VDDIOS" "sxa" "sx7"  "sx5"  "sx3"  "sx1" 
		      "VSS"
		      "sy1" "sy3"  "sy5"  "sy7"  "VDDIOS" "VDDIOS" "sya" "sy9" "sy11" "sy13" "sy15"
		      "VSS" "resn" "VDDIOS" "VDDIOS" "clk" "VSS"
		      nil "ey7" "ey5" "VDDIOS" "VDDIOS" "eya" "ey3" "ey1"
		      nil "ex1" "ex3" "exa" "VDDIOS" "VDDIOS" "ex5" "ex7" nil
		)
		(list 
		      "sx16" "sx14" "sx12" "sx10" "sx8" "GND" "GND" "sxq" "sx6" "sx4" "sx2" "sx0" 
		      "VSS"
		      "sy0" "sy2" "sy4" "sy6" "VSS" "VSS" "syq" "sy8" "sy10" "sy12" "sy14" "sy16" 
		      "id" "od" "ip" "op" nil 
		      nil "ey6" "ey4" "VSS" "VSS" "eyq" "ey2" "ey0"
		      nil "ex0" "ex2" "exq" "VSS" "VSS" "ex4" "ex6" "ex8"
		)

		(list 
		      "VDDM" "GND" "Vdd" "GND"
		      "VDDM" "GND" "Vdd" "GND"
		      "VDDM" "GND" "Vdd" "GND"
		      "VDDM" "GND" "Vdd" "GND"
		      "VDDM" "GND" "Vdd" "GND"
		      "VDDM" "GND" "Vdd" "GND"
		      "VDDM" "GND" "VDDM" "GND" "VDDM"
		)

		)
             ?filename "n2_bumps_s.txt" ?CV CV)





  ; MESH bumps (45 rows tall)
  CV = (dbOpenCellViewByType "core.neuro.mesh" "core.neuro.mesh.MESH.20001_bumps" "layout" "maskLayout" "w")
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
  ;(DrawBumps bumps ?filename "n2_bumps_mesh.txt" ?CV CV)

  ; NORTH bumps for bd(17) x input (2 rows tall)
  CV = (dbOpenCellViewByType "chip.n2.north" "chip.n2.north.NORTH.20001_bumps" "layout" "maskLayout" "w")
  (DrawBumps (list 
                   (list nil nil
		   	 "wy7" "wy5" "VSS"    "VSS"    "wya" "wy3" "wy1" "VSS"
		         "wx1" "wx3" "wxa" "VSS" "VSS" "wx5" "wx7"
			 nil nil nil nil nil nil nil
			 "nx15" "nx13" "nx11" "nx9" "VSS" "VSS" "nxa" "nx7" "nx5" "nx3" "nx1" "VSS"
			 "ny1" "ny3" "ny5" "ny7" "VSS" "VSS" "nya" "ny9" "ny11" "ny13" "ny15" "VSS" "VSS"
		   	 )
                   (list nil nil 
		   	 "wy6" "wy4" "VDDION" "VDDION" "wyq" "wy2" "wy0" "VSS"
		         "wx0" "wx2" "wxq" "VDDION" "VDDION" "wx4" "wx6" "wx8"
			 nil nil nil nil nil
			 "nx16" "nx14" "nx12" "nx10" "nx8" "VDDION" "VDDION" "nxq" "nx6" "nx4" "nx2" "nx0" "VSS"
			 "ny0" "ny2" "ny4" "ny6" "VDDION" "VDDION" "nyq" "ny8" "ny10" "ny12" "ny14" "ny16" 
			 )
		   )
             ?filename "n2_bumps_n.txt" ?CV CV)


)


(defun bmp () 
 (load "vip/proj/n2/n2_bumps.il")
 (DrawBumpsN2)
)
