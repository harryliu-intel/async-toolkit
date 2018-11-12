defun( GetBboxArea (bbox)
 let( (xbl ybl xtr ytr area)

  xbl = caar(bbox)
  ybl = cadar(bbox)
  xtr = caadr(bbox)
  ytr = cadadr(bbox)
  area = (xbl-xtr)*(ybl-ytr)
  area
))


defun( GetFigArea (fig)
 let((minX minY maxX maxY layerName layerPurpose thisArea 
      boundingBox points xbl ybl xtr ytr x1 x2 y1 y2 prb)
  minX = 1.0e10
  minY = 1.0e10
  maxX = -1.0e10
  maxY = -1.0e10
  layerName = car(fig->lpp)
  layerPurpose = cadr(fig->lpp)
  thisArea = 0.0

  case( fig~>objType
    ( "rect"
       boundingBox =fig~>bBox
       xbl = caar(boundingBox)
       ybl = cadar(boundingBox)
       xtr = caadr(boundingBox)
       ytr = cadadr(boundingBox)
       thisArea = (xbl-xtr)*(ybl-ytr)
       if((xbl<minX) then minX=xbl)
       if((ybl<minY) then minY=ybl)
       if((xtr>maxX) then maxX=xtr)
       if((ytr>maxY) then maxY=ytr)
       if((abs(thisArea) == 0.00) 
         printf("Warning:found zero area shape! %s (%f %f)\n" layerName xbl ybl))
    )
    ( "polygon" || "PRBoundary"
       points = listToVector(fig~>points)
       for(k 0  length(points)-1              
         if((k < length(points)-1) then k2=k+1 else k2=0)
         x1 = car(points[k])
         y1 = cadr(points[k])
         x2 = car(points[k2])
         y2 = cadr(points[k2])
         thisArea =  thisArea+ (x1*y2-x2*y1) * 0.5
         if((x1<minX) then minX=x1)
         if((y1<minY) then minY=y1)
         if((x1>maxX) then maxX=x1)
         if((y1>maxY) then maxY=y1))
       thisArea = abs(thisArea)
       if((thisArea == 0.00) 
         printf("Warning:found zero area shape! %s (%f %f)\n" layerName x1 y1))
    )
    ( "path"
       points = listToVector(fig~>points)
       for(k 0  length(points)-2
         k2=k+1             x1 = car(points[k])
         y1 = cadr(points[k])
         x2 = car(points[k2])
         y2 = cadr(points[k2])
         thisArea =  thisArea+ max(abs(x1-x2) abs(y1-y2))*fig~>width
         if((x1<minX) then minX=x1)
         if((y1<minY) then minY=y1)
         if((x2>maxX) then maxX=x2)
         if((y2>maxY) then maxY=y2)                    
         if((abs(thisArea) == 0.00) 
           printf("Warning:found zero area shape! %s (%f %f)\n" layerName x1 y1))
       )
    )
    ( "inst"
      prb = fig~>master~>prBoundary
       points = listToVector(prb~>points)
       for(k 0  length(points)-1              
         if((k < length(points)-1) then k2=k+1 else k2=0)
         x1 = car(points[k])
         y1 = cadr(points[k])
         x2 = car(points[k2])
         y2 = cadr(points[k2])
         thisArea =  thisArea+ (x1*y2-x2*y1) * 0.5
         if((x1<minX) then minX=x1)
         if((y1<minY) then minY=y1)
         if((x1>maxX) then maxX=x1)
         if((y1>maxY) then maxY=y1))
       thisArea = abs(thisArea)
       if((thisArea == 0.00) 
         printf("Warning: No prBoundary object found ! \n")); %s (%f %f)\n" layerName x1 y1))
    )
    ( "clusterBoundary" || "areaBlockage"
       points = listToVector(fig~>points)
       for(k 0  length(points)-1              
         if((k < length(points)-1) then k2=k+1 else k2=0)
         x1 = car(points[k])
         y1 = cadr(points[k])
         x2 = car(points[k2])
         y2 = cadr(points[k2])
         thisArea =  thisArea+ (x1*y2-x2*y1) * 0.5
         if((x1<minX) then minX=x1)
         if((y1<minY) then minY=y1)
         if((x1>maxX) then maxX=x1)
         if((y1>maxY) then maxY=y1))
       thisArea = abs(thisArea)
       if((thisArea == 0.00) 
         printf("Warning: No prBoundary object found ! \n")); %s (%f %f)\n" layerName x1 y1))
    )
    ( "label"
       nil
    )
    ( t
      printf("Warning: Area of \"%s\" are not calculated.\n" fig~>objType)
      nil
    )
  )
  thisArea
))


defun( GetSelectedArea ()
 let(( inst area )
   
   foreach( inst geGetSelectedSet()
     area = GetFigArea(inst)
     if(inst~>isAnyInst 
      then
      (printf "%s -- %3.3fmm^2\n" inst~>cellName area/1000000)
      else
      (printf "%3.3fmm^2\n" area/1000000)
      )
   )
t
 ))


defun( GetCellArea (cv)
 ;returns the prbound area. if there is more than one, returns the largest.
 ;if there is none, returns the bbox
 let( (bbox prb found_prb area prbmax prboundary)

  bbox = GetBboxArea(cv->bBox)

  found_prb = nil
  prbmax = 0.0

 prboundary = GetPrbound(cv)
 when( prboundary~>objType == "PRBoundary"
      found_prb = t
      prb = GetFigArea(prboundary)
      when( prbmax<prb prbmax = prb )
  )

 when( !found_prb
  foreach( shape cv->shapes
    if( car(shape->lpp)=="prBoundary" && cadr(shape->lpp)=="drawing" && shape~>objType != "label" then
      when( found_prb printf("Warning: found multiple prbounds in %s\n" cv->cellName) )
      found_prb = t
      prb = GetFigArea(shape)
      when( prbmax<prb prbmax = prb )
    )
  )
 )

  if( found_prb then
    area = prbmax
  else
    area = bbox
    printf("Warning: no prbound, using bbox for %s\n" cv->cellName)
  )
  area
))


defun( GetPhantomArea (cv)
 let( (area macroarea areas)

  macroarea = 0.0
  foreach( shape cv->shapes
    when( car(shape->lpp)=="prBoundary" && cadr(shape->lpp)=="boundary"
      macroarea = macroarea+GetFigArea(shape)
    )
  )
  areas = list( GetCellArea(cv) macroarea )
  areas
))


defun( PrintCellArea (name areas depth)
  printf("%s %3.2f %3.2f\n" name car(areas)/1000000 cadr(areas)/1000000 )
)


defun( ReportAreaRecursive (cv depth @key (file nil))
 let( (instances areas LibCellsToIgnore cell uniqueInstances)

  areas = GetPhantomArea(cv)

  if(!file then
    PrintCellArea(cv->cellName areas depth)
  else
    PrintCellAreaToHtml(cv->cellName areas depth file)
  )

  LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
  instances = car( NameFilterInstances( cv->instances LibCellsToIgnore ))
  uniqueInstances = nil
  foreach( inst instances
      if( !member( inst~>cellName uniqueInstances~>cellName ) then
        uniqueInstances= cons( inst uniqueInstances )
      )
  )
  foreach( inst uniqueInstances
    cell = nrOpenCellViewReadable(inst->libName inst->cellName inst->viewName)
    ReportAreaRecursive(cell depth+1 ?file file)
  )
))


defun( ReportPhantomArea ()
  let( (cv)
  cv = geGetWindowCellView()
  ReportAreaRecursive(cv 0)
  t
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; functions to spit out html for Alta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


defun( ReportAltaArea ()
  let( (cv file)

  file = (outfile "/nfs/site/disks/wdisk.83/pankala1/area/area.html")
  (if !file then
    (printf "File %s is not writeable." filename)
  else
    (fprintf file "%s" AreaHtmlHeader())

    cv = nrOpenCellViewReadable("chip.rrc" "chip.rrc.RRC.1000" "floorplan")
    ReportAreaRecursive(cv 0 ?file file)

    fprintf(file "%s" AreaHtmlFooter())
    close(file)
  )
  t
))


defun( PrintCellAreaToHtml (name areas depth file)
 let( (cell base)
  cell = "<tr><td>"
  for( i 0 depth
    cell = strcat(cell " &nbsp&nbsp&nbsp&nbsp ")
  )
  base = cadr( reverse( parseString( name "." ) ))
  cell = strcat(cell base)
  cell = strcat(cell sprintf(nil "</td><td>%3.2f</td>" car(areas)/1000000) )
  cell = strcat(cell sprintf(nil "<td>%3.2f</td></tr>\n" cadr(areas)/1000000) )
  fprintf(file "%s" cell)
))


defun( AreaHtmlHeader ()
 (let (header)
   header = sprintf(nil "<!-- file generated by skill/layout/geometry/area.il -->\n\n")
   header = strcat(header "<p>This table is generated from the checked-in phantom view of chip.alta.alta.ALTA.1000")
   header = strcat(header "<p>Please file bugs regarding discrepancies to Steve in Alta Physical Design:Block Area<p>")
   header = strcat(header "<html>\n<table border=1>\n")
   header = strcat(header "<tr><td width=70%><b>Cell</b></td>")
   header = strcat(header "<td width=15%><b>Area</b></td><td width=15%><b>Macros</b></td></tr>\n")
   header
))


defun( AreaHtmlFooter ()
 (let (footer)
   footer = "\n</html>\n</table>\n"
   footer
))

(defun printPrPolygonTilePoints (xGrow yGrow)
  CV=(geGetEditCellView)
  points=(betterGrowPoints CV->prBoundary->points xGrow:yGrow)
  (dbCreatePolygon CV (list "y1" "drawing") points)
  shapes = (setof x CV->shapes x->layerName=="y1")
  tiles = (dbLayerTile CV "y1" shapes)
  (foreach tile tiles (printf "%g %g %g %g\n" (leftEdge tile->bBox) (bottomEdge tile->bBox) (rightEdge tile->bBox) (topEdge tile->bBox)))
  shapes = (setof x CV->shapes x->layerName=="y1")
  (foreach shape shapes (dbDeleteObject shape))
)

(defun GetPrPolygons ()
  (printPrPolygonTilePoints 0 0)
)

(defun GetDrcPolygons ()
  (printPrPolygonTilePoints -0.42 -0.399)
)

; compute total routed cell area (recurse down to prBoundary)
(defun CellArea (@key (CV (geGetEditCellView)) (map (makeTable "area" 0.0)) (includeTop nil))
  (let (area instances)
    area=0
    instances = (setof x CV->instances x->libName!=TechLibName)
    (cond (map[CV->cellName]!=0.0 area=map[CV->cellName])
          (includeTop && CV->prBoundary area=(PrbArea ?CV CV))
          (instances
           (foreach inst instances
                    area = area + (CellArea ?CV inst->master ?map map ?includeTop t)
                    )
           )
          )
    area
    )
  )

; compute Bbox area
(defun BboxArea (@key (CV (geGetEditCellView)))
  (((car (cadr CV->bBox))-(car (car CV->bBox))) *
   ((cadr (cadr CV->bBox))-(cadr (car CV->bBox))))
  )

; query prBoundary area
(defun PrbArea (@key (CV (geGetEditCellView)))
  CV->prBoundary->area
  )

; compute desnity factor compard to bBox
(defun DensityFactor (@key (CV (geGetEditCellView)))
  (CellArea ?CV CV) / (PrbArea ?CV CV)
  )
