; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun InstanceAlignInstance ( Instance AlignmentUnitInUserUnits )
  (let (
        ( ObjectToAlign
          (if ( equal ( getq Instance objType ) "mosaicInst" )
              ( getq Instance mosaic )
            Instance ) ) )
    (when ObjectToAlign
      (let (
            ( CurrPos ( getq ObjectToAlign xy ) ) )
        (let (
              ( CurrX ( car CurrPos ) )
              ( CurrY ( cadr CurrPos ) ) )
          (let (
                ( NewX ( times 
                         ( float AlignmentUnitInUserUnits )
                         ( round
                           ( quotient
                             CurrX
                             ( float AlignmentUnitInUserUnits ) ) ) ) )
                ( NewY ( times 
                         ( float AlignmentUnitInUserUnits )
                         ( round
                           ( quotient
                             CurrY
                             ( float AlignmentUnitInUserUnits ) ) ) ) ) )
            ( dbSetq ObjectToAlign ( list NewX NewY ) xy ) ) ) ) ) ) )



(defun ReportInstances ()
 (let (cv all_cells unique_cells count filename file 
       lcv fcv layout_area floorplan_area ratio total cmd)
  cv=(geGetWindowCellView)
  filename=(sprintf nil "%s.instances" cv->cellName)
  file=(outfile filename)

  all_cells=cv->instances
  (foreach inst all_cells
    (when !(IsInList inst->cellName unique_cells)
      unique_cells=(cons inst->cellName unique_cells)
    )
  )

  (foreach inst unique_cells
    count=0
    (foreach inst2 all_cells
      (when inst==inst2->cellName
        count=count+1
      )
    )
    lcv=(nrOpenCellViewReadableByName inst "layout")
    fcv=(nrOpenCellViewReadableByName inst "floorplan")
    layout_area=nil
    floorplan_area=nil
    (when lcv 
      layout_area=(GetCellArea lcv)
    )
    (when fcv
      floorplan_area=(GetCellArea fcv)
    )
    (when layout_area && floorplan_area
      ratio=layout_area/floorplan_area
      total=layout_area*count
      (fprintf file "%s\t%d\t%2.2f\t%4.2f\n" inst count ratio total)
      (printf       "%s\t%d\t%2.2f\t%4.2f\n" inst count ratio total)
    )
    (when !layout_area && floorplan_area
      total=floorplan_area*count
      (fprintf file "%s\t%d\t%s\t%4.2f\n" inst count "NA" total)
      (printf       "%s\t%d\t%s\t%4.2f\n" inst count "NA" total)
    )
  )
  (close file)
  cmd=(sprintf nil "sort -k2n,2 %s > %s.count" filename filename)
  (shell cmd)
  cmd=(sprintf nil "sort -k3n,3 %s > %s.ratio" filename filename)
  (shell cmd)
  cmd=(sprintf nil "sort -k4n,4 %s > %s.total" filename filename)
  (shell cmd)
  t
 )
)
