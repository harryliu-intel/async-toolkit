; Copyright 2008 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( flFloorplanCopy ( dstCellView srcCellName 
                        @key
                        (srcLibName "")
                        (srcViewName "")
                        (Recursive nil)
                        )
  prog((srcCellView dstInst srcInst unfoundInstances processedCells x subDstCellView subSrcCellView xalign yalign)
    if(srcLibName == "" then srcLibName=car( NameParseCellName( srcCellName )))
    if(srcViewName == "" then srcViewName="floorplan")
    srcCellView=nrOpenCellViewReadable( srcLibName srcCellName srcViewName)
    if( srcCellView == nil then 
      printf("Error: %s(%s) %s view not found. \n" srcCellName srcLibName srcViewName)
      return(0)
    )
    if( dstCellView==srcCellView then return(t))
    unfoundInstances=nil
    processedCells=nil
    x=0
    foreach( dstInst dstCellView~>instances
      srcInst=dbFindAnyInstByName( srcCellView dstInst~>name )
      if( srcInst then
        dstInst~>xy=srcInst~>xy
        dstInst~>orient=srcInst~>orient
        if( car(dstInst~>xy)>x then x= car(dstInst~>xy))
        if( !member( srcInst~>cellName processedCells) then
          processedCells=cons( srcInst~>cellName processedCells) 
          if( dstInst~>master~>instances then
            if( Recursive then
              subDstCellView=nrOpenCellViewWritable( dstInst~>libName dstInst~>cellName dstInst~>viewName)
              if( subDstCellView then
                   flFloorplanCopy( subDstCellView srcInst~>cellName
                                    ?srcLibName srcInst~>libName
                                    ?srcViewName srcInst~>viewName
                                    ?Recursive Recursive)
                   
              else 
                   printf("Error: Can not open %s(%s) %s for write. \n" dstInst~>libName dstInst~>cellName dstInst~>viewName) 
              )
            )
          )
        )
      else
        unfoundInstances=cons( dstInst unfoundInstances )
      )
    )
    xalign=(dbGetPropByName srcCellView "XAlignGrid")->value
    yalign=(dbGetPropByName srcCellView "YAlignGrid")->value
    if( xalign then (dbReplaceProp dstCellView "XAlignGrid" "float" xalign))
    if( yalign then (dbReplaceProp dstCellView "YAlignGrid" "float" yalign))
    return(t)
  )
)

defun( ssFloorplanTemplating ( srcLibName srcCellName srcViewName 
                               dstLibName dstCellName dstViewname )
; this is flFloorplanTemplateing interface for super size package
  let((dstCellView)
    dstCellView=nrOpenCellViewWritable( dstLibName dstCellName dstViewname )
    if( dstCellView == nil then
      printf("Error: cellview %s(%s) %s not writable.\n" dstCellName dstLibName dstViewname)    
    else
      flFloorplanTemplating( dstCellView srcCellName 
                         ?srcLibName srcLibName 
                         ?srcViewName srcViewName )
    )
  )
)


defun( flFloorplanTemplating ( dstCellView srcCellName
                        @key
                        (srcLibName "")
                        (srcViewName "")
                        (ResolveOverlap t)
                        (SnapXGrid 0.0)
                        (SnapXInst nil)
                        (Recursive nil)
                        (DeletePins nil)
                        (SkipCellList nil)
                        )
  prog((TEMP projectName SkipCellList xtotal returnCode)
    returnCode=flFloorplanCopy( dstCellView srcCellName
                     ?srcLibName srcLibName
                     ?srcViewName srcViewName
                     ?Recursive Recursive
                   )
    if( returnCode==0 then return(0))
    if( DeletePins then
      foreach( shape dstCellView~>shapes 
        if( shape~>layerName!="prBoundary" then
          dbDeleteObject( shape )
        ) 
      )
    )
    if( ResolveOverlap==nil then return(t))
    TEMP=ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    createDir( sprintf( nil "%s/glpsol/" TEMP))
    projectName=dstCellView~>cellName
    xtotal=flResolveOverlap( dstCellView
           ?SkipCellList SkipCellList
           ?LPMacroFileName sprintf( nil "%s/glpsol/%s.lpt" TEMP projectName) 
           ?LPOutputFileName sprintf( nil "%s/glpsol/%s.out" TEMP projectName)
           ?LPLogFileName sprintf( nil "%s/glpsol/%s.log" TEMP projectName)
           ?LPmemoFileName sprintf( nil "%s/glpsol/%s.memo" TEMP projectName)
           ?SaveFileName sprintf( nil "%s/glpsol/%s.autosave" TEMP projectName)
           ?BlockLayer list("LOGO" "drawing")
           ?SaveCellView t
           ?AbutCellKeepOrder t
           ?SingleHierarchy nil
           ?SnapXGrid SnapXGrid
           )
    if( xtotal!=0 then
      flResolveOverlap( dstCellView
           ?SkipCellList SkipCellList
           ?LPMacroFileName sprintf( nil "%s/glpsol/%s.lpt" TEMP projectName) 
           ?LPOutputFileName sprintf( nil "%s/glpsol/%s.out" TEMP projectName)
           ?LPLogFileName sprintf( nil "%s/glpsol/%s.log" TEMP projectName)
           ?LPmemoFileName sprintf( nil "%s/glpsol/%s.memo" TEMP projectName)
           ?SaveFileName sprintf( nil "%s/glpsol/%s.autosave" TEMP projectName)
           ?BlockLayer list("LOGO" "drawing")
           ?SaveCellView t
           ?AbutCellKeepOrder t
           ?SingleHierarchy nil
           ?xtotal xtotal
           ?Mode 1
           ?SnapXGrid SnapXGrid
           )
    )
    if( SnapXInst then
      fixXAlignment( ?cv dstCellView )
    )
    return(t)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;; Simpler Versions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; template from another cell or view
(defun TemplateFrom (@key (CV (geGetEditCellView))
                          (libName nil) (cellName nil) (viewName "floorplan"))
  (let (fCV finst)
    (unless libName libName=CV->libName)
    (unless cellName cellName=CV->cellName)
    fCV = (dbOpenCellViewByType libName cellName viewName "maskLayout" "r")
    (unless fCV (error "Can't open %s %s %s\n" libName cellName viewName))
    (foreach inst CV->instances
             finst = (dbFindAnyInstByName fCV inst->name)
             (when finst
               inst->xy = finst->xy
               inst->orient = finst->orient
               inst->status = finst->status
               )
             )
    )
  t
  )

; template from another subtype of current cell
(defun TemplateSubtype (subtype @key (CV (geGetEditCellView)))
  (let (cellName tCV tinst)
    cellName = CV->cellName
    cellName = (cdr (reverse (parseString cellName ".")))
    cellName = (cons subtype cellName)
    cellName = (buildString (reverse cellName) ".")
    (TemplateFrom ?cellName cellName ?CV CV)
    )
  CV
  )
