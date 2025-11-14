; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; plot a cell in SVG format
(defun PlotSVG (@key (CV (geGetEditCellView)) (power nil) (peak nil))
  (let (map fin name inst density area max_density
            grid TEMP file fout bbox bx0 by0 bx1 by1 x0 y0 x1 y1 w h color fill)
    map=(makeTable "power" nil)
    max_density=0.0
    (when power
      fin=(infile power)
      (unless fin (error "can't read %s\n" power))
      (while (fscanf fin "%s %g" name p)
        inst=(dbFindAnyInstByName CV name)
        (when inst
          (unless inst->master->prBoundary (error "instance %s has no prBoundary\n" name))
          area=(PrbArea ?CV inst->master)
          density=p/area
          max_density=(max max_density density)
          map[inst]=density
          )
        )
      (close fin)
      )
    (printf "peak power density %g\n",max_density);
    (when max_density==0 max_density=1.0)
    (when peak max_density=peak)
    grid=(GCD (round GridPolyPitch/MfgGrid) (round GridDiffPitch/MfgGrid)) * MfgGrid
    TEMP=(ConfigFileGetValue TheCDSConfigTable "TEMP")
    file=(sprintf nil "%s/%s.svg",TEMP,CV->cellName);
    fout=(outfile file)
    (foreach inst CV->instances
             (unless inst->master->prBoundary (error "instance %s has no prBoundary\n" inst->name))
             bbox=(BBoxCombine bbox (dbTransformBBox inst->master->prBoundary->bBox inst->transform))
             )
    bx0=(car (car bbox))
    by0=(cadr (car bbox))
    bx1=(car (cadr bbox))
    by1=(cadr (cadr bbox))
    w=(round (bx1-bx0)/grid)
    h=(round (by1-by0)/grid)
    (fprintf fout "<svg version=\"1.1\" width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">\n" w h)
    (fprintf fout "<rect width=\"100%%\" height=\"100%%\" color=\"black\"/>\n")
    (foreach inst CV->instances
             bbox=(dbTransformBBox inst->master->prBoundary->bBox inst->transform)
             x0=(car (car bbox))-bx0
             y0=by1-(cadr (car bbox))
             x1=(car (cadr bbox))-bx0
             y1=by1-(cadr (cadr bbox))
             x0=(round x0/grid)
             y0=(round y0/grid)
             x1=(round x1/grid)
             y1=(round y1/grid)
             color="white"
             (cond (power==nil fill="#808080")
                   (map[inst]
                    fill=map[inst]/max_density
                    fill=(min fill 1.0)
                    fill=(sqrt fill) ; nonlinear for more dynamic range
                    fill=(round 255*fill)
                    fill=(sprintf nil "#%02x%02x%02x" fill fill fill)
                    )
                   (t fill="red")
                   )
             (fprintf fout "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" stroke=\"%s\" fill=\"%s\"/>\n"
                      x0 y1 (x1-x0) (y0-y1) color fill)
             )
    (fprintf fout "</svg>\n")
    (close fout)
    )
  t
  )

; write instance names suitable for power measurement later
(defun WriteInstancesSVG (@key (CV (geGetEditCellView)) (random_power nil))
  (let (TEMP file f)
    TEMP=(ConfigFileGetValue TheCDSConfigTable "TEMP")
    file=(sprintf nil "%s/%s.dat",TEMP,CV->cellName);
    f=(outfile file)
    (foreach inst CV->instances
             (cond (random_power (fprintf f "%s %g\n" inst->name (random 1000)/1000.0))
                   (t            (fprintf f "%s\n" inst->name))
                   )
             )
    (close f)
    file
    )
  )
