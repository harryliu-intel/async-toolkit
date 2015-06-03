; Copyright 2005 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; IMPORTANT: while much of this LOOKs like it would work for naming the abstract
;            view something else, it does not really work!

defun( nrPointTransform ( point transform )
  let((xy orient temp )
    xy=car(transform)
    function=caddar(setof(orient ORIENTLIST car(orient)==cadr(transform)))
    if( function then
      temp=apply( function list(point) )
      temp=list( car(temp)+car(xy) cadr(temp)+cadr(xy))
    else
      printf("Invalid orient.\n")
      temp=nil
    )
    temp
  )
)

defun( nrPointListTransform (pointList transform)
  foreach( mapcar point pointList nrPointTransform( point transform ))
)

defun( nrRectListTransform (rectList transform )
  foreach( mapcar bBox rectList nrPointListTransform( bBox transform ))
)

defun( nrConvertPathToRectList ( path )
  "returns a list of rectangles"
  let((returnList width beginExt endExt points x1 y1 x2 y2 temp)
  returnList=nil
  width=path~>width
  beginExt=path~>beginExt-width/2
  endExt=path~>endExt-width/2
  points=listToVector(path~>points)
  for( i 0 length(points)-2
    x1=car(points[i])
    y1=cadr(points[i])
    x2=car(points[i+1])
    y2=cadr(points[i+1])
    if((i==0) then 
      cond(
        ((x1==x2 && y1>y2) y1=y1+beginExt)
        ((x1==x2 && y1<y2) y1=y1-beginExt)
        ((y1==y2 && x1>x2) x1=x1+beginExt)
        ((y1==y2 && x1<x2) x1=x1-beginExt)
      )
    )
    if((i==length(points)-2) then 
      cond(
        ((x1==x2 && y1>y2) y2=y2-endExt)
        ((x1==x2 && y1<y2) y2=y2+endExt)
        ((y1==y2 && x1>x2) x2=x2-endExt)
        ((y1==y2 && x1<x2) x2=x2+endExt)
      )
    )

    cond( 
      ((x1==x2 && y1>y2) temp=y2 y2=y1 y1=temp)
      ((y1==y2 && x1>x2) temp=x2 x2=x1 x1=temp)
      ((x1!=x2 && y1!=y2) printf("Error: none straight path:") println(path))
    )
    returnList=cons( list( x1-width/2:y1-width/2  x2+width/2:y2+width/2) returnList)
  )
  returnList
  )
)

defun( nrConvertPolygonToRectList ( poly )
  "returns a list of rectangles"
  let((temp bBox returnList)
    foreach( mapcar temp dbLayerTile( poly~>cellView poly~>lpp list(poly)) bBox=temp~>bBox dbDeleteObject(temp) bBox)
  )
)



defun( nrGetInstanceNetShapes ( inst instNetName layerName )
  let((cellView shapes temp)
    cellView=inst~>master
    shapes=setof( shape cellView~>shapes 
      shape~>net~>name==instNetName && shape~>layerName==layerName && shape~>purpose=="drawing")
    rectList=foreach( mapcan shape shapes 
      cond(
        (shape~>objType=="rect" list(shape~>bBox))
        (shape~>objType=="path" nrConvertPathToRectList(shape))
        (shape~>objType=="polygon" nrConvertPolygonToRectList(shape))
        (t printf( "Warning: Non rect/path/polygon shape will be ignored!\n") nil)
      )
    )
    rectList=nrRectListTransform(rectList inst~>transform)
  )
)

; check if a rectangle overlaps with a pin within a maxPinLength radius
; returns nil for rectangles contained inside pins for some unfathomable reason
defun( nrFilterRect ( pinRect rect maxPinLength
                    @key
                    ( MustOverlap t )
                    )
  let((returnRect pinX1 pinX2 pinY1 pinY2 x1 x2 y1 y2 xMin yMin xMax yMax)
    pinX1=min(caar(pinRect) caadr(pinRect))
    pinY1=min(cadar(pinRect) cadadr(pinRect))
    pinX2=max(caar(pinRect) caadr(pinRect))
    pinY2=max(cadar(pinRect) cadadr(pinRect))

    xMin=pinX2-maxPinLength
    yMin=pinY2-maxPinLength
    xMax=pinX1+maxPinLength
    yMax=pinY1+maxPinLength

    x1=min(caar(rect) caadr(rect))
    y1=min(cadar(rect) cadadr(rect))
    x2=max(caar(rect) caadr(rect))
    y2=max(cadar(rect) cadadr(rect))
    
    if( x1>xMax || x2<xMin || y1>yMax || y2<yMin then 
      returnRect=nil 
    else
      x1=max(x1 xMin)
      y1=max(y1 yMin)
      x2=min(x2 xMax)
      y2=min(y2 yMax)
      if(x1>=pinX1 && y1>=pinY1 && x2<=pinX2 && y2<=pinY2 then
        returnRect=nil
      else
        returnRect=list( x1:y1 x2:y2 )
      )
      if(x1==x2 || y1==y2 then
        returnRect=nil
      )
      if( MustOverlap && (x1>pinX2 || x2<pinX1 || y1>pinY2 || y2<pinY1) then 
        returnRect=nil
      )      
    )
    returnRect
  )
)

defun( nrInplacePins ( cellView 
                     @key
                     ( PowerNets nil )
                     ( PinExtension t )
                     )
  let((net layerName rectList)
    foreach( pin cellView~>terminals
      net=pin~>net
      if(PowerNets || (net~>name != "Vdd" && net~>name!="GND") then
        layerName=car(pin~>pins~>fig~>layerName)
        foreach( instTerm net~>allInstTerms
          if(layerName==nil layerName=car(instTerm~>term~>pins~>fig~>layerName))
          if( PinExtension then
            rectList=nrGetInstanceNetShapes(instTerm~>inst instTerm~>name layerName)
            rectList=foreach( mapcan rect rectList 
              foreach( mapcar pinRect pin~>pins~>fig~>bBox 
                nrFilterRect( pinRect rect 10000 ?MustOverlap t )))
          else
            rectList=pin~>pins~>fig~>bBox
          )
          rectList=setof( rect rectList rect )          
          foreach( rect rectList
            dbAddFigToNet( dbCreateRect( cellView list(layerName "drawing") rect ) net )
          )
        )
      )
    )
  )
)

; Copy pins to pins with "net" purpose, which are used by the Assura
; deck to extend pins into touching metal.
(defun nrCreateAbstractPin
  (CV @key (PowerNets nil))
  (let (Vdd GND net)
    Vdd = (dbMakeNet CV "Vdd")
    GND = (dbMakeNet CV "GND")
    (foreach term CV->terminals
      net = term->net
      (when PowerNets || (net != Vdd && net != GND)
        (foreach pinFig term~>pins~>fig
          if( pinFig then 
            pinFig = (dbCopyFig pinFig CV)
            pinFig->lpp = (list (car pinFig->lpp) "net")
            (dbCreatePin net pinFig)
          )
        )
      )
    )
  t
  )
)

; is this type a contact or pcell?
(defun isPdkCell (CellView)
  (or CellView->libName==TechLibName
      CellView->libName==GateLibraryName
      CellView->libName==StackLibraryName
      )
  )

; Copies CellView to abstract_prep view, copys overlapping
; abstract_edit/abstract subcells, flattens them, swaps "net" to
; "drawing", returns new abstract_prep CellView.
(defun nrPrepareAbstract (CellView @key (keepLayoutSubcells t) (abstractView "abstract"))
  (let (views hasAbstractView hasAbstractEditView)
    ; copy to abstract_prep view
    CellView=(betterCopyCellView CellView CellView->libName CellView->cellName
                             "abstract_prep" nil nil t )
    
    ; substitute abstract_edit or abstract subcells and flatten
    (rexCompile "\\.wires\\.")
    (foreach inst CellView->instances
             (cond ((and !(isPdkCell inst->master) !(rexExecute inst->cellName))
                    views = (dbAllCellViews inst->master->lib inst->master->cellName)
                    hasAbstractEditView = (setof view views view=="abstract_edit")
                    hasAbstractView = (setof view views view==abstractView)
                    (cond (hasAbstractEditView
                           (cond (keepLayoutSubcells inst = (dbCopyFig inst nil)))
                           inst->master = (dbOpenCellViewByType inst->master->lib
                                                                inst->master->cellName
                                                                "abstract_edit")
                           (dbFlattenInst inst 1 nil nil nil))
                          (hasAbstractView
                           (cond (keepLayoutSubcells inst = (dbCopyFig inst nil)))
                           inst->master = (dbOpenCellViewByType inst->master->lib
                                                                inst->master->cellName
                                                                abstractView)
                           (dbFlattenInst inst 1 nil nil nil))
                          (t
                           (printf "WARNING: subcell %s of type %s has no abstract view\n",
                                   inst->name inst->cellName ))
                          )
                    )
                   )
             )

    ; replace "net" layer with "drawing" layer
    (foreach shape CellView->shapes
             (cond (shape->purpose=="net" shape->purpose="drawing"))
             )
    )

  ; return abstract_prep view
  CellView
  )

defun( nrCreateLeafAbstract ( libName cellName viewName
                            @key
                            ( DeleteInstances t )
                            ( DeleteDrawingLayers t )
                            ( AssuraSets nil )
                            ( AssuraRuleFile nil )
                            ( maxPinLength 9.6 )
                            ( useAbstractSubcells nil )
                            ( keepLayoutSubcells t )
                            ( PowerNets t )
                            ( PowerGridPitch PowerGridPitch )
                            ( targetViewName "abstract" )
                            )
  let(()



    nrCreateMidAbstract( libName cellName viewName
                            ?DeleteInstances DeleteInstances
                            ?DeleteDrawingLayers DeleteDrawingLayers 
                            ?AssuraSets AssuraSets 
                            ?AssuraRuleFile AssuraRuleFile 
                            ?maxPinLength maxPinLength 
                            ?useAbstractSubcells useAbstractSubcells 
                            ?keepLayoutSubcells keepLayoutSubcells 
                            ?PowerNets PowerNets 
                            ?PowerGridPitch PowerGridPitch
                            ?targetViewName targetViewName 
                       )
  )
)

defun( nrCreateMidAbstract ( libName cellName viewName 
                            @key
                            ( DeleteInstances t )
                            ( DeleteDrawingLayers t )
                            ( AssuraSets nil )
                            ( AssuraRuleFile nil )
                            ( maxPinLength 9.6 )
                            ( useAbstractSubcells nil )
                            ( keepLayoutSubcells t )
                            ( PowerNets nil )
                            ( PowerGridPitch PowerGridPitch )
                            ( targetViewName "abstract" )
                            ( CDCPowerGrid nil )
                            ( allbloat nil )
                            ( labelPins t )
                            )
  let((gec3Foreign srcCellView AssuraLayerMappings TempDirForThisRun AssuraRunLog 
       ErrorStr prBound bBox netShapes connectedNetShapes overlapPin bBox)
    if(targetViewName != "abstract" printf("Error: generating abstract with view other than 'abstract' does not work, trying anyway.\n"))
    srcCellView = dbOpenCellViewByType( libName cellName viewName)
    (cond (useAbstractSubcells
           srcCellView = (nrPrepareAbstract srcCellView
                                            ?keepLayoutSubcells keepLayoutSubcells ?abstractView targetViewName)))
    CellView=nrOpenCellViewWritable( libName cellName targetViewName)
    if( CellView && srcCellView then
      dbClose(CellView)
      CellView=betterCopyCellView( srcCellView libName cellName targetViewName nil nil t )
      printf("Creating abstract pin...\n")
      nrCreateAbstractPin( CellView ?PowerNets PowerNets)

      AssuraLayerMappings=cons( 
        list( "bound" list( car( BoundaryLPP ) "boundary" ) )
        append( 
          foreach( mapcan LPP MetalLPPs
            sscanf( car( LPP ) MetalLayerFormat Num )
            list( 
              list( sprintf( nil "m%dKeepout" Num ) list( car( LPP ) "boundary" ) )
              list( sprintf( nil "m%dAddPin" Num ) list( car( LPP ) "net" ) )
            )               
          )
          foreach( mapcan LPP list( Metal4LPP Metal5LPP Metal6LPP Metal7LPP Metal8LPP )
            sscanf( car( LPP ) MetalLayerFormat Num )
            list( 
              list( sprintf( nil "m%dVdd" Num ) list( car( LPP ) "vdd" ) )
              list( sprintf( nil "m%dGnd" Num ) list( car( LPP ) "gnd" ) )
            )
          )               
        )
      )
      TempDirForThisRun=makeTempFileName( 
            sprintf( nil "%s/ASSURA.XXXXXX" ConfigFileGetValue( TheCDSConfigTable "TEMP" )))
      createDir( TempDirForThisRun ) 
      if( AssuraRuleFile==nil then AssuraRuleFile= sprintf( nil "%s/share/Fulcrum/cell_automation/abstract_mid.rul" 
            ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT" )))
      AssuraRunLog= sprintf( nil "%s/assura.log" TempDirForThisRun )
      ErrorStr= AssuraRunAssuraLayerProcessor( 
              CellView 
              CellView~>libName
              CellView~>cellName
              "abstract_temp"
              AssuraRuleFile
              TempDirForThisRun
              AssuraLayerMappings
              nil 
              ?AssuraSets AssuraSets
              ?AssuraRunLog AssuraRunLog
              ?LeaveMess nil
              ?allbloat allbloat
             ) 
      
      AssuraCopyShapesFromCellView( nrOpenCellViewReadable(libName cellName "abstract_temp") CellView nil)

      cv= ddGetObj(libName cellName "abstract_temp")
      if(cv then ddDeleteObj(cv))     
      (cond (useAbstractSubcells ddDeleteObj( ddGetObj(libName cellName "abstract_prep") )))

      ; delete mosaics
      if( DeleteInstances foreach( inst CellView~>instances 
        if( inst~>objType=="mosaicInst" dbDeleteObject( inst~>mosaic ) dbDeleteObject( inst ))))

      ; delete vias
      if( DeleteInstances foreach( inst CellView~>vias 
        if( inst~>objType=="mosaicInst" dbDeleteObject( inst~>mosaic ) dbDeleteObject( inst ))))

      ; delete shapes that aren't "boundary" or "net" purpose (BUG 17554)
      if( DeleteDrawingLayers then
        foreach( shape CellView~>shapes 
          if( shape~>purpose!="net" && shape~>purpose!="boundary" dbDeleteObject( shape )))
      )

      ; Make pins out of net shapes touching original pins.  Requires
      ; the original pin shapes to be rectangles, unfortunately.
      ; Deletes the original pins in favor of touching net shapes.
      netShapes = (setof shape CellView~>shapes shape~>purpose=="net")
      connectedNetShapes = (setof shape netShapes shape~>net)
      (foreach shape connectedNetShapes
        (cond (shape->objType!="rect"
                (printf "WARNING: can't attach net shapes to non-rectangle pin %s\n"
                        shape->net->name)
                )
              (t
                ; find overlapping net shapes
                overlaps = (dbGetTrueOverlaps CellView shape->bBox shape->lpp 0)
                (foreach overlap overlaps
                  (when !overlap->net
                    (dbCreatePin shape->net overlap)
                    (when labelPins (LabelPin CellView overlap ?bBox shape->bBox))
                    )
                  )
                ; delete original pin shapes in favor of connected ones
                (cond (overlaps  (dbDeleteObject shape))
                      (labelPins (LabelPin CellView shape))
                      )
                )
              )
        )

      ; Convert vdd and gnd purpose to power pins.
      leMergeShapes( setof( shape CellView~>shapes shape~>purpose=="gnd"))
      leMergeShapes( setof( shape CellView~>shapes shape~>purpose=="vdd"))
      foreach( shape CellView~>shapes  
        if( shape~>purpose=="gnd" then 
          shape~>purpose="net"
          dbCreatePin( dbFindNetByName( CellView GNDNetName ) shape )
        )
        if( shape~>purpose=="vdd" then 
          shape~>purpose="net"
          dbCreatePin( dbFindNetByName( CellView VddNetName ) shape )
        )
      )
      if( CDCPowerGrid then
        prBoundd=setof( shape CellView~>shapes shape~>layerName=="prBoundary" && shape~>purpose=="drawing")
        if( prBoundd then
          top=cadr(cadar(prBoundd~>bBox))
          right=car(cadar(prBoundd~>bBox))
          shape = dbCreateRect( CellView list( "M1" "net" ) list( list( 0.06 -0.05 ) list( right-0.06 0.09 ) ) )
          p = dbCreatePin( dbFindNetByName( CellView GNDNetName ) shape )
          dbCreateRect( CellView list( "M1" "net" ) list( list( 0.06 -0.05 ) list( right-0.06 0.09 ) ) )
          shape = dbCreateRect( CellView list( "M1" "net" ) list( list( 0.06 top-0.09 ) list( right-0.06 top+0.05 ) ) )
          if( abs( top/4.4 - floor(top/4.4) ) < 0.1 then
              p=dbCreatePin( dbFindNetByName( CellView GNDNetName ) shape )
          )
          if( abs( top/4.4 - floor(top/4.4) ) > 0.1 then
              p=dbCreatePin( dbFindNetByName( CellView VddNetName ) shape )
          )
          dbCreateRect( CellView list( "M1" "net" ) list( list( 0.06 top-0.09 ) list( right-0.06 top+0.05 ) ) )
        )
      )

      ; copy m1 keepout to OVERLAP
      m1by=setof( lpp CellView~>lpps lpp~>layerName=="M1" && lpp~>purpose=="boundary")
      foreach( lpp m1by
        foreach( shape lpp~>shapes
          when( shape~>objType=="rect"
            m1points = shape~>bBox
            dbCreateRect( CellView OverlapLPP m1points )
          )
          when( shape~>objType=="polygon"
            m1points = shape~>points
            dbCreatePolygon( CellView OverlapLPP m1points )
          )
        )
      )

      ; merge keepout
      leMergeShapes( setof( shape CellView~>shapes shape~>purpose=="boundary"))
      prBound=setof( shape CellView~>shapes shape~>layerName=="prBoundary" &&
                           shape~>purpose=="boundary")
      if( prBound then bBox=car(prBound)~>bBox else bBox=CellView~>bBox )

      ; set properties
      sprintf( gec3Foreign "%s %f %f" CellView~>cellName caar(bBox) cadar(bBox) )
      dbReplacePropList( CellView list( 
              list("prCellClass" "string" "block")
              list("prCellType" "string" "macro")
              list("maskLayoutSubType" "string" targetViewName)
              list("symmetry" "string" "X Y")
              list("gec3Foreign" "string" gec3Foreign)
              )
      )
 
      ; delete pins with no shapes
      foreach( pin CellView~>terminals
        if( pin~>pins~>fig == nil then
          printf("Delete pin with no shape: %s\n" pin~>name ) 
          dbDeleteObject( pin ) 
        )
      )

      ; combine "prBoundary" "boundary" shapes into a single rectangle (BUG 17829)
      bBox = nil
      (foreach shape CellView->shapes
               (when (and shape->layerName=="prBoundary" shape->purpose=="boundary")
                 bBox = (BBoxCombine shape->bBox bBox)
                 (dbDeleteObject shape)
                 )
               )
      (when bBox (dbCreateRect CellView (list "prBoundary" "boundary") bBox))

      ; save and return abstract CellView
      (dbSave CellView)
      CellView

    else
      printf("%s(%s) is not writable.\n" cellName targetViewName)
      nil
    )
  )
)

; Convert a figure to a rectangle list
(defun nrConvertFigToRectList (fig)
  (cond (fig->objType=="rect" (list fig->bBox)) 
        (fig->objType=="path" (nrConvertPathToRectList fig))
        (fig->objType=="inst" (list fig->bBox))
        (fig->objType=="polygon" (nrConvertPolygonToRectList fig))
        (t (printf "Error: illegal pin shape for pin %s (%s).\n"
                   instTerm->name thisInst->name) nil)
        )
  )

; Find subcell pins connecting to given net.  Returns a list of lists,
; where the inner list is a layerName followed by a list of rectangles
; in the parent coordinate system.
(defun nrFindPinShapes (net pinLPPs)
  (let (layerName inst transform tmp pinRectList lppList ret)
    (foreach lpp pinLPPs
      layerName = (car lpp)
      pinRectList = nil
      (foreach instTerm net->instTerms
        inst = instTerm->inst
        (when inst->libName!=TechLibName
          transform = (geGetInstTransform inst)
          (foreach pin instTerm->term->pins
            (when (car pin->fig->lpp)==layerName
              tmp = (nrConvertFigToRectList pin->fig)
              tmp = (nrRectListTransform tmp transform)
              pinRectList = (append tmp pinRectList)
              )
            )
          )
        )
      (when pinRectList ret = (cons (list layerName pinRectList) ret))
      )
    ret
    )
  )

defun( nrFindInstPinShapes ( thisInst pinName pinLPPs )
; Find all Rectagles of subcell pin that connects to a paticular top level net
  let((realInstances pinFigs pinRectList thisPinFigs shapes lpp)
    terminalInstName=makeTable("a" "")
    terminalInstName["METAL1"]="m1_T"
    terminalInstName["METAL2"]="m2_T"
    terminalInstName["METAL3"]="m3_T"
    terminalInstName["METAL4"]="m4_T"
    terminalInstName["METAL5"]="m5_T"
    terminalInstName["METAL6"]="m6_T"
    terminalInstName["METAL7"]="m7_T"
    terminalInstName["METAL8"]="m8_T"
    foreach( mapcan lpp pinLPPs      
        instPins=setof( terminal thisInst~>master~>terminals terminal~>name==pinName )
        pinFigs=foreach( mapcan instPin instPins instPin~>pins~>fig )
        thisPinFigs=setof( pinFig pinFigs pinFig~>layerName==car(lpp) || (pinFig~>objType=="inst" && pinFig~>cellName==terminalInstName[car(lpp)]))
        pinRectList=foreach( mapcan pinFig thisPinFigs 
          cond(
            (pinFig~>objType=="rect" list(pinFig~>bBox) ) 
            (pinFig~>objType=="path" nrConvertPathToRectList( pinFig ) )
            (pinFig~>objType=="inst" list(pinFig~>bBox) )
            (pinFig~>objType=="polygon" nrConvertPolygonToRectList( pinFig ))
            (t printf("Error: illegal pin shape for pin %s (%s).\n" instTerm~>name thisInst~>name) nil)
            )
        ) 
      shapes=nrRectListTransform(pinRectList thisInst~>transform )
      if( shapes list( list( car(lpp) shapes)) nil)
    )
  )
)

defun( nrConvertRectToPath ( rect 
                            @key
                           (minWidth nil)
                           )
  let((returnRect x1 x2 y1 y2 x y w)
    x1=min(caar(rect) caadr(rect))
    y1=min(cadar(rect) cadadr(rect))
    x2=max(caar(rect) caadr(rect))
    y2=max(cadar(rect) cadadr(rect))
    x= (x2+x1)/2 
    y= (y2+y1)/2 
    if(x2-x1<=y2-y1 then
            w= x2-x1
            if(minWidth then y1=max( y1 y-0.12) y2=min(y2 y+0.12))
            returnPath=list( list( x:y1 x:y2 ) w )
    else
            w= y2-y1
            if(minWidth then x1=max( x1 x-0.12) x2=min( x2 x+0.12))
            returnPath=list( list( x1:y x2:y ) w )
    )
    returnPath
  )
)

defun( nrFindOverlapRect ( thisRect rectList maxDistance @key (maxWidth 2*DefaultWiringWidth))
; Find all rectangles in rectList that overlaps thisRect
  let((returnRect pinX1 pinX2 pinY1 pinY2 x1 x2 y1 y2 xMin yMin xMax yMax x y w)
    pinX1=min(caar(thisRect) caadr(thisRect))
    pinY1=min(cadar(thisRect) cadadr(thisRect))
    pinX2=max(caar(thisRect) caadr(thisRect))
    pinY2=max(cadar(thisRect) cadadr(thisRect))

    xMin=pinX1-maxDistance-1e-9
    yMin=pinY1-maxDistance-1e-9
    xMax=pinX2+maxDistance+1e-9
    yMax=pinY2+maxDistance+1e-9

    foreach( mapcan rect rectList
      x1=min(caar(rect) caadr(rect))
      y1=min(cadar(rect) cadadr(rect))
      x2=max(caar(rect) caadr(rect))
      y2=max(cadar(rect) cadadr(rect))
      returnPath=nil
      if( x1>xMax || x2<xMin || y1>yMax || y2<yMin then 
        returnPath=nil 
      else
        cond(
          ((pinX1<=x1+1e-9 && pinX2+1e-9>=x2) || (pinX1+1e-9>=x1 && pinX2<=x2+1e-9)
            ; need to add 1e-9 to avoid rounding error in float numbers
            x= (min(pinX2 x2)+max(pinX1 x1))/2 
            w= min(maxWidth min(pinX2 x2)-max(pinX1 x1))
            returnPath=list( list( x:min(pinY2 y2)-0.12 x:max(pinY1 y1)+0.12 ) w )
          )
          ((pinY1<=y1+1e-9 && pinY2+1e-9>=y2) || (pinY1+1e-9>=y1 && pinY2<=y2+1e-9) 
            ; need to add 1e-9 to avoid rounding error in float numbers
            y= (min(pinY2 y2)+max(pinY1 y1))/2 
            w= min(pinY2 y2)-max(pinY1 y1)
            returnPath=list( list( min(pinX2 x2)-0.12:y max(pinX1 x1)+0.12:y ) w )
          )
          ( t returnPath=nil )
        )
      )
      if( returnPath list(returnPath) nil )
    )
  )
)

defun( nrCreateNeighborNetConnector ( cellView maxDistance pinLPPs
                  @key (maxFanout nil) ; set to 2 to avoid NanoRoute confusion
                       (maxWidth 2*DefaultWiringWidth)
                       (reportPort nil))
  let((pinRectList totalRectList newNetShapes rect figCount)
    figCount=0
    nets=setof( net cellView~>nets net->term==nil && net->figs==nil &&
                    (maxFanout==nil || net->instTermCount<=maxFanout))
    foreach( net nets
      MasterRectList=nrFindPinShapes( net pinLPPs )
      foreach( pinRectList MasterRectList
        layerName=car(pinRectList)
        pinRectList=cadr(pinRectList) 
        totalRectList=pinRectList
        newNetShapes=foreach( mapcan thisRect totalRectList
          pinRectList=setof( rect pinRectList rect!=thisRect )
          nrFindOverlapRect( thisRect pinRectList maxDistance ?maxWidth maxWidth)
        )
        foreach( returnPath newNetShapes
          if(returnPath then
            if( reportPort then
              fprintf(reportPort "Net %s: Create neighbor connector: %L\n" net->name returnPath)
              figCount=figCount+1
            else
              path=dbCreatePath( cellView list(layerName "drawing") car(returnPath) cadr(returnPath) "truncateExtend" )  
              if( path then 
                 dbAddFigToNet( path net )
                 figCount=figCount+1
              else printf("Warning: no valid returnPath %L (net %s).\n" returnPath net->name)
              )          
            )
          )
        )
      )
    )
    if( reportPort then
      if( figCount == 0 then fprintf(reportPort "PASS: No neighbor connector needed in %s.\n" cellView~>cellName)
      else fprintf(reportPort "ERROR: %d neighbor connectors needed in cell %s.\n" figCount cellView~>cellName))
    else
      dbSave( cellView )
      printf("Done: Total %d shapes added.\n" figCount)
    )
  )
)

defun( nrCreateSingleNetConnector ( cellView pinLPPs 
                            @key
                            (reportOnly nil)
                            (reportPort poport)
                            (MaxHeapSize 2000)
      )
; Put a single metal piece over pins that does not route to other nets
; so that extraction spice will have top level net.
  let((pinRectList totalRectList newNetShapes rect pathpoints figCount CDLFile CellListFile DoneFunc Command
      instances inst uniqueInstances OutputFile p
      danglingNodeList netName instName pinName cellName net thisInst layerName pathpoints path)
    figCount=0
    if( reportOnly then 
          fprintf(reportPort "Processing %s ...\n" cellView~>cellName)
    )
    CDLFile=sprintf( nil "%s/%s.cdl"
                     ConfigFileGetValue( TheCDSConfigTable "TEMP" )
                     cellView~>cellName )
    CellListFile=sprintf( nil "%s/%s.cell_list"
                     ConfigFileGetValue( TheCDSConfigTable "TEMP" )
                     cellView~>cellName )
    OutputFile=sprintf( nil "%s/%s.dangling_out"
                     ConfigFileGetValue( TheCDSConfigTable "TEMP" )
                     cellView~>cellName )
    when( !isFile( CDLFile )
      Command=sprintf( nil
                      "%s/cast2cdl --cast-path=%s --cell=%s --cadence-name --output=%s --max-heap-size=%dM --name-map=%s/share/Fulcrum/lve/transistor.map --process-dependent-name --64"
                      PackageGetBinRoot()
                      ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                      cellView~>cellName
                      CDLFile
                      MaxHeapSize
                      ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT" )
                      ) 
       printf( "%s\n" Command )
       system( Command )
    )
    instances=setof( inst cellView~>instances inst~>libName!=TechLibName && inst~>libName!="gate" && inst~>libName!="stack" )

    p=outfile(CellListFile)
    uniqueInstances=nil
    foreach( inst instances~>cellName
      if( !member(inst uniqueInstances) then
         uniqueInstances=cons( inst  uniqueInstances)
         fprintf( p "%s\n" inst )
      )
    )
    close(p)    

    Command=sprintf( nil "%s/dangling %s %s %s" PackageGetBinRoot() CDLFile CellListFile OutputFile)
    printf( "%s\n" Command )
    system( Command )

    danglingNodeList=nil
    load(OutputFile)

    foreach( node danglingNodeList
      netName=car(node)
      instName=cadr(node)
      pinName=caddr(node)
      cellName=cadddr(node)
      net=dbFindNetByName( cellView netName )
      if( net==nil then
        printf("Net %s does not exist. Creating Net.\n" netName)
        if( reportOnly then 
          fprintf(reportPort "dbCreateNet %s\n" netName)
        else
          net=dbCreateNet( cellView netName )
        )
      )
      if(net~>figs==nil && net~>pins==nil && length(net~>allInstTerms)<=1 then
        thisInst=dbFindAnyInstByName( cellView instName )
        if( thisInst==nil then printf( "Error: Inst %s does not exit.\n" instName )
        else
          MasterRectList=nrFindInstPinShapes( thisInst pinName pinLPPs)
          foreach( pinRectList MasterRectList
            layerName=car(pinRectList)
            pinRectList=cadr(pinRectList) 
            newNetShapes=foreach( mapcan thisRect pinRectList
            list( nrConvertRectToPath( thisRect ?minWidth t ) ) )
          )      
            foreach( returnPath newNetShapes
              if(returnPath then
                if( reportOnly then 
                  fprintf(reportPort "Net %s: Dangling Node found at ( %s %L )\n" netName netName returnPath)
                  figCount=figCount+1
                else
                  pathpoints = PathRoundPointsToGrid( car(returnPath) )
                  path=dbCreatePath( cellView list(layerName "drawing") pathpoints cadr(returnPath) "truncateExtend" )  
                  if( path then
                    dbAddFigToNet( path net )
                    figCount=figCount+1
                  else printf("Warning: no valid returnPath %L (net %s).\n" returnPath net->name)
                  )          
                )
              )
            )
          
        )
      )
    )
    if( reportOnly then
      if( figCount == 0 then fprintf(reportPort "PASS: No dangling node found in cell %s.\n" cellView~>cellName) 
      else fprintf(reportPort "ERROR: %d dangling nodes found in cell %s.\n" figCount cellView~>cellName))
    else
      dbSave( cellView )
      printf("Done: Total %d shapes added.\n" figCount)   
    )
  )
)

defun( nrCreateAbstractCB ( CellView 
                            @key
                           (ERASE_POWERGRID t)
                           (PIN_CUTOUT 0.42) 
                           (PIN_LENGTH 9.6)
                           (M1BLOAT -1.0)
                           (M2BLOAT -1.0)
                           (M3BLOAT -1.0)
                           (M4BLOAT 4.8)
                           (M5BLOAT 4.8)
                           (M6BLOAT 4.8)
                           (M7BLOAT 4.8)
                           (M8BLOAT 0.0)
                           (useAbstractSubcells t)
                           (keepLayoutSubcells t)
                           (PowerNets nil)
                           (M4PowerNets nil)
                           (M5PowerNets nil)
                           (M6PowerNets nil)
                           (M7PowerNets nil)
                           (PowerGridPitch PowerGridPitch)
                           (NO_HOLES_ABOVE_PINS nil)
                           (targetViewName "abstract")
                           (CDCPowerGrid nil)
                           (allbloat nil)
                           (labelPins t)
                          )

    (let ( TEMP FulcrumPDKRoot RuleFileTemplate AssuraRuleFile p_in p_out)

      TEMP=ConfigFileGetValue( TheCDSConfigTable "TEMP" )
      FulcrumPDKRoot=ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT")
      RuleFileTemplate= sprintf( nil "%s/share/Fulcrum/cell_automation/abstract_mid.rul" 
            ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT" ))
      if( !isFile(RuleFileTemplate) then 
        error(sprintf(nil "Do File Template %s does not exist!" RuleFileTemplate))
      )
      AssuraRuleFile=sprintf( nil "%s/%s_abstract.rul" TEMP CellView~>cellName )

      p_in=infile(RuleFileTemplate)
      if( !p_in then 
        errorMsg=sprintf( nil "Can Read File %s!\n" RuleFileTemplate) 
        error(errorMsg)
      )      
      p_out=outfile(AssuraRuleFile)
      if( !p_out then 
        errorMsg=sprintf( nil "Can Write To File %s!\n" AssuraRuleFile) 
        error(errorMsg)
      )      

      ; replace parameters of the template rul file
      while( p_in && gets(line p_in)
        cond(
          ( rexMatchp( "^PIN_CUTOUT=" line)   line=sprintf( nil "PIN_CUTOUT=%f\n" (float PIN_CUTOUT)))
          ( rexMatchp( "^PIN_LENGTH=" line)   line=sprintf( nil "PIN_LENGTH=%f\n" (float PIN_LENGTH)))
          ( rexMatchp( "^M1BLOAT=" line)      line=sprintf( nil "M1BLOAT=%f\n"    (float M1BLOAT)))
          ( rexMatchp( "^M2BLOAT=" line)      line=sprintf( nil "M2BLOAT=%f\n"    (float M2BLOAT)))
          ( rexMatchp( "^M3BLOAT=" line)      line=sprintf( nil "M3BLOAT=%f\n"    (float M3BLOAT)))
          ( rexMatchp( "^M4BLOAT=" line)      line=sprintf( nil "M4BLOAT=%f\n"    (float M4BLOAT)))
          ( rexMatchp( "^M5BLOAT=" line)      line=sprintf( nil "M5BLOAT=%f\n"    (float M5BLOAT)))
          ( rexMatchp( "^M6BLOAT=" line)      line=sprintf( nil "M6BLOAT=%f\n"    (float M6BLOAT)))
          ( rexMatchp( "^M7BLOAT=" line)      line=sprintf( nil "M7BLOAT=%f\n"    (float M7BLOAT)))
          ( rexMatchp( "^M8BLOAT=" line)      line=sprintf( nil "M8BLOAT=%f\n"    (float M8BLOAT)))
          ( rexMatchp( "^CDCPowerGrid=" line) line=sprintf( nil "CDCPowerGrid=%s\n"
                                                           if( CDCPowerGrid "t" "nil")))
          )
        fprintf(p_out "%s" line)
      )
      close(p_in)
      close(p_out)
      
      printf( "Assura Rule File %s is created.\n" AssuraRuleFile)
   
      nrCreateMidAbstract(CellView~>libName CellView~>cellName CellView~>viewName
             ?DeleteInstances t 
             ?DeleteDrawingLayers t 
             ?AssuraSets
                setof( Set
                  list("DRAW_KEEPOUT" "KEEP_KEEPOUT" 
                    when( ERASE_POWERGRID "ERASE_POWER_GRID")
                    when( NO_HOLES_ABOVE_PINS "NO_HOLES_ABOVE_PINS")
                    when( M4PowerNets "M4POWER_PIN" )
                    when( M5PowerNets "M5POWER_PIN" )
                    when( M6PowerNets "M6POWER_PIN" )
                    when( M7PowerNets "M7POWER_PIN" )
                  )
                  stringp( Set ) )
             ?AssuraRuleFile AssuraRuleFile       
             ?maxPinLength  PIN_LENGTH      
             ?useAbstractSubcells useAbstractSubcells
             ?keepLayoutSubcells keepLayoutSubcells
             ?PowerNets PowerNets
             ?PowerGridPitch PowerGridPitch
             ?targetViewName targetViewName
             ?CDCPowerGrid CDCPowerGrid
             ?allbloat allbloat
             ?labelPins labelPins
    )
  )
)

;;;;;;;;;;;;;;;;;;; Process abstract view before LEF export ;;;;;;;;;;;;;;;;;;;;;;

; is this a M3/M5/M7 power stripe?
defun( isPowerStripe (fig)
  prog((y1 y2 track)
    if( or( fig~>layerName=="M3" fig~>layerName=="M5" fig~>layerName=="M7") then
       y1=bottomEdge(fig~>bBox)
       y2=topEdge(fig~>bBox)
       track=round(y1/2.88)
       if( y1>=track*2.88-0.18 && y2<=track*2.88+0.18 then return(t))
    )
    return(nil)
  )
)

; Create an abstract_tmp view, deleting power stripes, converting power to keepout
defun( createAbstractTmp ( abstractView )
  let((abstractTmpView powerNets pin thisPin)
     powerNets=list("Vdd" "GND")
     ; allows this to work where the originating dfII dir is not writable
; this may not be compatible with later versions
;     abstractTmpView=dbOpenCellViewByType( abstractView~>libName
;              abstractView~>libName abstractView~>cellName "abstract_tmp" nil "s" )
     abstractTmpView=betterCopyCellView( abstractView
              "abstract_tmp" abstractView~>cellName "abstract_tmp" nil nil t )
;     abstractTmpView=betterCopyCellView( abstractView
;              abstractView~>libName abstractView~>cellName "abstract_tmp" nil nil t )
     foreach( pin abstractTmpView~>terminals
       if( member( pin~>net~>name powerNets ) then
          foreach( thisPin pin~>pins
            if( isPowerStripe(thisPin~>fig) then
               ; delete power pins that overlap power grid stripes
               dbDeleteObject(thisPin~>fig)
            else
               ; turn other power pins into keepout
               thisPin~>fig~>purpose="boundary"
            )
            dbDeleteObject(thisPin)
          )
       )
     )
     dbSave(abstractTmpView)
     abstractTmpView
   )
)

; Create abstract_tmp views for a list of abstract views
defun( copyViewToAbstractTmp (leflist pg_cellName)
  let((p_in p_out line temp tempCV tempLib)
    p_in=infile(leflist)
    p_out=outfile(strcat( leflist ".tmp"))
    tempLib=ddGetObj("abstract_tmp")
    while( gets( line p_in )
      temp= parseString( StringUtilChompNewline(line) " ")
      libName=car(temp)
      cellName=cadr(temp)
      viewName=caddr(temp)
      cellView=nrOpenCellViewReadable(libName cellName viewName)
      if(cellView then
         if( tempLib == nil then
             tempLib=dbCreateLib("abstract_tmp", strcat( ConfigFileGetValue( TheCDSConfigTable "TEMP" ) "/abstract_tmp"))
             techBindTechFile(tempLib techGetTechLibName(ddGetObj(libName))))
         if( cellName then
           tempCV=createAbstractTmp(cellView)
         ; allows this to work where the originating dfII dir is not writable
           fprintf( p_out "%s %s %s\n" tempCV~>libName tempCV~>cellName tempCV~>viewName)
         else
           fprintf( p_out "%s %s %s\n" libName cellName viewName)
         )
      else 
         printf("Not Readable: %s %s %s\n" libName cellName viewName)
      )
    )
    close(p_in)
    close(p_out)
  )
)

; Delete a list of abstract_tmp views
defun( deleteAbstractTmp (leflist)
  let((p_in p_out line temp cv)
    p_in=infile(leflist)
    while( gets( line p_in )
      temp= parseString( StringUtilChompNewline(line) " ")
      libName=car(temp)
      cellName=cadr(temp)
      cv=ddGetObj(libName cellName "abstract_tmp")
      if( cv then ddDeleteObj(cv))
    )
    close(p_in)
  )
)
