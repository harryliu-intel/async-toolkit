(defun DrawBumps (names
                  @key
                  (stagger nil)
                  (xpitch 96*1.68)
                  (ypitch 100*1.596)
                  (cellName "globals.BUMP.0")
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
              instname = (if bumpNum[name]==0 (sprintf nil "bump_%s" name)
                           (sprintf nil "bump_%s_%d" name bumpNum[name]))
              (dbCreateInst CV bump instname x:y "R0")
              (dbCreateLabel CV (list "tm1" "drawing") x:y name "centerCenter" "R0" "stick" 15)
              bumpNum[name]++
              (fprintf file "%s : %g %g\n" name x y)
              )
         )
    (close file)
    (dbSave CV)
    )
  t
  )

;;;;;;;;;;;;;;;;;;;; Create bumps for N2 assumign square pitch, 6 bumps tile width ;;;;;;;;;;;;;;;

(defun DrawSouthBumps ()
  (DrawBumps (list (list "Vdd" "VDDIO" "ip" "op" "VDDIO" "Vdd")
                   (list "GND" "resn"  "id" "od" "clk"   "GND"))
             ?filename "n2_bumps_south.txt")
  )

(defun DrawNorthBumps ()
  (DrawBumps (list (list "VDDIO" "D[0]" "D[2]"  "D[4]"  "D[6]"  "VDDIO"
                         "C.q"   "D[8]" "D[10]" "D[12]" "D[14]" "VDDIO")
                   (list "GND"   "D[1]" "D[3]"  "D[5]"  "D[7]"  "GND"
                         "C.a"   "D[9]" "D[11]" "D[13]" "D[15]" "GND")
                   )
             ?filename "n2_bumps_north.txt")
  )
