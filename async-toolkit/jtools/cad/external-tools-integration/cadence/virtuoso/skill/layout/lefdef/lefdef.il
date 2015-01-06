; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; lefdef package

; globals

defNxWarn=0
defNxErr=0

(defvar  powerNetGlobal list( "VDD" "VDDS" "VDDF" "Vdd" "AVDD" "VDDH" "VDDH[0]" "VDDH[1]" ))
(defvar groundNetGlobal list( "GND" "VSS"  "AGND" ))

;**********************************************************************
;  sub functions
;**********************************************************************
defun( defnWarn ( nwarn ) 
    if(nwarn < 0 then
         defNxWarn=0
    else
        defNxWarn = defNxWarn + nwarn
    )
    defNxWarn
)

defun( defnErr ( nerr ) 
    if(nerr < 0 then
         defNxErr=0
    else
        defNxErr = defNxErr + nerr
    )
    defNxErr
)

; defPoint2String function will print out a point with format of "( x y )".
defun( defPoint2String ( point units )
  sprintf( nil "( %.0f %.0f )" car(point)*units cadr(point)*units )
)
defun( defRectArea ( rect )
  let((point1 point2 x1 x2 y1 y2)
        point1=car(rect)
        point2=cadr(rect)
        x1=car(point1) y1=cadr(point1) x2=car(point2) y2=cadr(point2)
        abs((x2-x1)*(y2-y1))
  )
)

defun( defGenerateC2VTable ( cellName ignoreNetlist )
   let( ( map mapfile tempdir Command )
      map = nil
      tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
      mapfile = strcat( tempdir "/" cellName ".mapping.il" )
      Command=sprintf(  nil
          "cast2skill --cadence-name --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only  --max-heap-size=%dM --64"
          ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
          cellName
          tempdir
          ( nrGetMaxHeapSize )
          )
      if( ignoreNetlist then
         Command = strcat( Command " --ignore-netlist" ) )
      if( ! isFile( mapfile ) then
              ( shell Command ) )
      if( isFile( mapfile ) then
         load( mapfile )
         map = PinCastToVerilogTable() )
      map )
)

defun( defGenerateP2DTable ( cellName ignoreNetlist )
   let( ( map mapfile tempdir Command )
      map = nil
      tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
      mapfile = strcat( tempdir "/" cellName ".pindirection.il" )
      Command=sprintf(  nil
          "cast2skill --cadence-name --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only  --max-heap-size=%dM --64"
          ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
          cellName
          tempdir
          ( nrGetMaxHeapSize )
          )
      if( ignoreNetlist then
         Command = strcat( Command " --ignore-netlist" ) )
      if( ! isFile( mapfile ) then
              ( shell Command ) )
      if( isFile( mapfile ) then
         load( mapfile )
         map = PinDirectionTable() )
      map )
)

defun( lefPath2String ( path )
  returnString=""
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
    returnString=sprintf(nil "%s        RECT %.3f %.3f %.3f %.3f ;\n"
      returnString
      x1-width/2 y1-width/2 
      x2+width/2 y2+width/2 
    )
  )
  returnString
)

defun( lefRect2String ( fig )
  sprintf(nil "        RECT %.3f %.3f %.3f %.3f ;\n"
    caar(fig~>bBox) cadar(fig~>bBox)
    caadr(fig~>bBox) cadadr(fig~>bBox)
  )
)

defun( lefPolygon2String ( fig )
  points=listToVector(fig~>points)
  if((length(points)==4) then
    x0=car(points[0]) y0=cadr(points[0])
    x1=car(points[1]) y1=cadr(points[1])
    x2=car(points[2]) y2=cadr(points[2])
    x3=car(points[3]) y3=cadr(points[3])
  )
  if((length(points)==4 && x0==x1 && x2==x3 && y1==y2 && y3==y0) ||
    (length(points)==4 && x1==x2 && x3==x0 && y0==y1 && y2==y3) then
    returnString=sprintf( nil "        RECT %.3f %.3f %.3f %.3f ;\n"
      min(x0 x2) min(y0 y2) max(x0 x2) max(y0 y2)) 
  else
    returnString="        POLYGON "
    foreach( point fig~>points
      returnString=sprintf( nil "%s %.3f %.3f" returnString car(point) cadr(point))
    )
    returnString=sprintf( nil "%s ;\n" returnString)
  )
  returnString
)

defun( lefFig2String ( pout figs layerName )
  thisFigs=setof( fig figs
                      (layerName==nil || fig~>layerName==layerName) &&
                      fig~>objType!="label" )
  if((thisFigs!=nil) then
    foreach( fig thisFigs
      fprintf( pout "        LAYER %s ;\n" fig~>layerName)
      cond(
          ((fig~>objType == "path" || fig~>objType == "pathSeg") 
            fprintf( pout "%s" lefPath2String(fig))
            shapes=setof( shape shapes shape!=fig)              
          )
          ((fig~>objType == "rect")
            fprintf( pout "%s" lefRect2String(fig))
            shapes=setof( shape shapes shape!=fig)              
          )
          ((fig~>objType == "polygon")
            fprintf( pout "%s" lefPolygon2String(fig))
            shapes=setof( shape shapes shape!=fig)              
          )
          ( t
            printf( "Error: illegal shape %s\n" fig~>objType)
          )
        )
    )
  )
)

defun( defShapeToText ( pout shapes transform )
  let((shape point bBox points line)
    foreach( shape shapes
      if(shape~>objType=="rect" then
        if( transform then bBox=nrPointListTransform(shape~>bBox transform)
        else bBox=shape~>bBox )
        fprintf( pout "    RECT ( %.0f %.0f ) ( %.0f %.0f )\n"
            leftEdge(bBox)*1000.0 bottomEdge(bBox)*1000.0
            rightEdge(bBox)*1000.0 topEdge(bBox)*1000.0 )
      )
      if(shape~>objType=="polygon" then
        if( transform then points=nrPointListTransform(shape~>points transform)
        else points=shape~>points )
        fprintf( pout "    POLYGON" )
        foreach( point points
          fprintf( pout " ( %.0f %.0f )" car(point)*1000.0 cadr(point)*1000.0 )
        )
        fprintf( pout "\n" )
      )
    )
    t
  )
)

; report via orientation
(defun defIsRotatedVia (via)
  (and via->libName==TechLibName
       via->orient!="R0" via->orient!="R180"
       via->orient!="MX" via->orient!="MY")
  )

; rotate a via 90 degrees
(defun defRotateVia (via swapViaTable @key ( warnonly nil))
  (let (pair rest)
    pair = (car swapViaTable)
    rest = (cdr swapViaTable)
    (cond (via->cellName==(car pair)->cellName
           via->master=(cadr pair)
           via->orient="R0")
          (via->cellName==(cadr pair)->cellName
           via->master=(car pair)
           via->orient="R0")
          (rest (defRotateVia via rest ?warnonly warnonly ))
          (warnonly (printf "Warning: can't rotate via %s\n" via->cellName) defnWarn(1))
          (t defnErr(1) (error (sprintf nil "ERROR: can't rotate via %s" via->cellName)))
          )
    )
  t
  )

; return master of a via
(defun defViaMaster (name)
  (dbOpenCellViewByType TechLibName name "layout" "maskLayout" "r")
  )
; return master of a via
(defun defViaMasterS (name)
  (dbOpenCellViewByType TechLibName name "symbolic" "maskLayout" "r")
  )

; make vias R0 before DEF export
(defun defCanonicalizeVias (CV @key ( warnonly nil ) )
  (let (swapViaTable)
    swapViaTable = (list (list (defViaMaster "M2_M1_R0")            (defViaMaster "M2_M1_R90"))
                         (list (defViaMaster "M3_M2_R0")            (defViaMaster "M3_M2_R90"))
                         (list (defViaMaster "M4_M3_R0")            (defViaMaster "M4_M3_R90"))
                         (list (defViaMaster "M5_M4_R0")            (defViaMaster "M5_M4_R90"))
                         (list (defViaMaster "M6_M5_R0")            (defViaMaster "M6_M5_R90"))
                         (list (defViaMaster "M7_M6_R0")            (defViaMaster "M7_M6_R90"))
                         (list (defViaMaster "M2_M1_H")             (defViaMaster "M2_M1_V"))
                         (list (defViaMaster "M3_M2_H")             (defViaMaster "M3_M2_V"))
                         (list (defViaMaster "M4_M3_H")             (defViaMaster "M4_M3_V"))
                         (list (defViaMaster "M5_M4_H")             (defViaMaster "M5_M4_V"))
                         (list (defViaMaster "M6_M5_H")             (defViaMaster "M6_M5_V"))
                         (list (defViaMaster "M7_M6_H")             (defViaMaster "M7_M6_V"))
                         (list (defViaMaster "M2_M1_STACK")         (defViaMaster "M2_M1_STACK_R90"))
                         (list (defViaMaster "M3_M2_STACK")         (defViaMaster "M3_M2_STACK_R90"))
                         (list (defViaMaster "M4_M3_STACK")         (defViaMaster "M4_M3_STACK_R90"))
                         (list (defViaMaster "M5_M4_STACK")         (defViaMaster "M5_M4_STACK_R90"))
                         (list (defViaMaster "M6_M5_STACK")         (defViaMaster "M6_M5_STACK_R90"))
                         (list (defViaMaster "M7_M6_STACK")         (defViaMaster "M7_M6_STACK_R90"))
                         (list (defViaMasterS "3W_M2_M1_STACK")     (defViaMasterS "3W_M2_M1_STACK_R90"))
                         (list (defViaMasterS "3W_M3_M2_STACK")     (defViaMasterS "3W_M3_M2_STACK_R90"))
                         (list (defViaMasterS "3W_M4_M3_STACK")     (defViaMasterS "3W_M4_M3_STACK_R90"))
                         (list (defViaMasterS "3W_M5_M4_STACK")     (defViaMasterS "3W_M5_M4_STACK_R90"))
                         (list (defViaMasterS "3W_M6_M5_STACK")     (defViaMasterS "3W_M6_M5_STACK_R90"))
                         (list (defViaMasterS "3W_M7_M6_STACK")     (defViaMasterS "3W_M7_M6_STACK_R90"))
                         (list (defViaMasterS "RESET_3W3S_M4_M3_H") (defViaMasterS "RESET_3W3S_M4_M3_V"))
                         (list (defViaMaster "M2_M1_2CUT_H")        (defViaMaster "M2_M1_2CUT_V"))
                         (list (defViaMaster "M3_M2_2CUT_H")        (defViaMaster "M3_M2_2CUT_V"))
                         (list (defViaMaster "M4_M3_2CUT_H")        (defViaMaster "M4_M3_2CUT_V"))
                         (list (defViaMaster "M5_M4_2CUT_H")        (defViaMaster "M5_M4_2CUT_V"))
                         (list (defViaMaster "M6_M5_2CUT_H")        (defViaMaster "M6_M5_2CUT_V"))
                         (list (defViaMaster "M7_M6_2CUT_H")        (defViaMaster "M7_M6_2CUT_V")))
    (foreach via (CV->instances)
             (cond ((via~>master~>libName == "tsmc65" && via~>prop ) defnWarn(1) printf( "Warning: via %s %s at (%.3f,%.3f) has modified properties, def may be not be valid.\n" via~>baseName via~>master~>cellName car(via~>xy) cadr(via~>xy))))
             (cond ((defIsRotatedVia via) (defRotateVia via swapViaTable ?warnonly warnonly)))
             )
    )
  t
  )

(defun exportClusterRegions (defPort cellView)
  let( ( cl cvClusters bnd I rects rect   ) 
   
    fprintf(defPort, "REGIONS %d ;\n", length(cellView~>clusters))

    foreach( cl cellView~>clusters
       fprintf(defPort, "- %s ", cl~>name)
       foreach( bnd cl~>boundaries 
         rects = convertPolyObjToRect(bnd)
         foreach( rect rects 
       fprintf(defPort, "( %.0f %.0f ) ( %.0f %.0f ) ", 
               caar(rect)*UNITS, cadar(rect)*UNITS,
               caadr(rect)*UNITS, cadadr(rect)*UNITS)
          )
        )
       fprintf(defPort, "+ TYPE GUIDE ;\n")
     )

    fprintf(defPort, "END REGIONS\n\n")

t
 )
)

(defun exportPRBoundary (defPort cellView)
  let( ( rect rects I pr ) 
   fprintf(defPort, "DIEAREA ")
   rects = convertPolyObjToRect(cellView~>prBoundary)
   pr = cellView~>prBoundary
   for( I 0 length(pr~>points)-1
      when( nth(I pr~>points)
        fprintf(defPort, "( %.0f %.0f ) ", car(nth(I pr~>points))*UNITS, cadr(nth(I pr~>points))*UNITS)
        )
      )
   fprintf(defPort, ";\n\n")
t
 )
)

(defun exportPlacementBlockages (defPort cellView)
  let( ( blk pBlockage )
    I = 0
    pBlockage = setof(blk cellView~>blockages blk~>type=="placement")
    
    fprintf( pout "BLOCKAGES %d ;\n" length(pBlockage))
    when( pBlockage
      fprintf( pout "- PLACEMENT \n")
     )
    foreach( blk pBlockage
      I = I+1
      if( blk~>nPoints == 4
       then
         fprintf(defPort, "  RECT ( %.0f %.0f ) ( %.0f %.0f ) ", 
               caar(blk~>bBox)*UNITS, cadar(blk~>bBox)*UNITS,
               caadr(blk~>bBox)*UNITS, cadadr(blk~>bBox)*UNITS)
       else
          println("WARNING: POLYGON BLOCKAGES are not supported in DEF !!!!")
;         fprintf(defPort, "  POLYGON ")
;         for( I 0 length(blk~>points)-1
;         when( nth(I blk~>points)
;         fprintf(defPort, "( %.0f %.0f ) ", car(nth(I blk~>points))*UNITS, cadr(nth(I blk~>points))*UNITS)
;            )
;           )
         )
      if( I == length(pBlockage)
       then
        fprintf(defPort, "; \n")
       else
        fprintf(defPort, "\n")
      )
     )
    fprintf( pout "END BLOCKAGES\n\n" length(pBlockage))
t
 )
)

(defun exportRoutingBlockages (defPort cellView)
  let( ( blk pBlockage )
    I = 0
    pBlockage = setof(blk cellView~>blockages blk~>type=="routing")
    
    fprintf( pout "BLOCKAGES %d ;\n" length(pBlockage))
    foreach( blk pBlockage
      I = I+1
      if( blk~>nPoints == 4
       then
;        when( blk~>BlockSignalRouting
         fprintf( pout "  - LAYER %s  \n" blk~>layer)
         fprintf(defPort, "  RECT ( %.0f %.0f ) ( %.0f %.0f ) ; \n", 
               caar(blk~>bBox)*UNITS, cadar(blk~>bBox)*UNITS,
               caadr(blk~>bBox)*UNITS, cadadr(blk~>bBox)*UNITS)
;            )
         fprintf( pout "  - LAYER %s + PUSHDOWN \n" blk~>layer)
         fprintf(defPort, "  RECT ( %.0f %.0f ) ( %.0f %.0f ) ; \n", 
               caar(blk~>bBox)*UNITS, cadar(blk~>bBox)*UNITS,
               caadr(blk~>bBox)*UNITS, cadadr(blk~>bBox)*UNITS)
       else
          println("WARNING: POLYGON BLOCKAGES are not supported in DEF !!!!")
         )
     )
    fprintf( pout "END BLOCKAGES\n\n" length(pBlockage))
t
 )
)

(defun convertPolyObjToRect ( obj )
  let( ( sct rects newlist rect )
    sct = dbCreatePolygon(obj~>cellView "y4" obj~>points)
    rects = dbLayerTile(obj~>cellView "y5" list(sct))
    newlist = rects~>bBox
    dbDeleteObject(sct)
    foreach(rect rects dbDeleteObject(rect))
 newlist
 )
)

(defun exportDomainAssociation ( defPort powerNets gndNets CV )
 (let ( pNet inst pPin  )

    fprintf( defPort "SPECIALNETS %d ;\n" length(powerNets)+length(gndNets) )
    foreach( pNet powerNets 
      fprintf( defPort "- %s ( * %s )\n" pNet, pNet )
       foreach( inst CV~>instances
        foreach( pPin powerNetGlobal 
         when( dbGetPropByName(inst strcat("powerPin_" pPin))~>value == pNet
          fprintf( defPort " ( %s %s )\n" inst~>name, pPin )
         )
        )
      )
      fprintf( defPort " + USE POWER ;\n" pNet, pNet )
    )

    foreach( pNet gndNets 
      fprintf( defPort "- %s ( * %s )\n" pNet, pNet )
       foreach( inst CV~>instances
        foreach( pPin groundNetGlobal 
         when( dbGetPropByName(inst strcat("groundPin_" pPin))~>value == pNet
          fprintf( defPort " ( %s %s )\n" inst~>name, pPin )
         )
        )
      )
      fprintf( defPort " + USE GROUND ;\n" pNet, pNet )
    )

    fprintf( defPort "END SPECIALNETS \n\n" )

t
 )
)

;**********************************************************************
;  main lef functions
;**********************************************************************


defun( lefInstanceOut ( pout libName cellName viewName 
                        @key 
                        ( Verbose nil ) 
                      )

let( (( cellView dbOpenCellViewByType( libName cellName viewName )))
  printf("generating lef macro for %s %s (%s).\n" libName cellName viewName)
  fprintf( pout "MACRO %s\n" cellView~>cellName )
  fprintf( pout "    CLASS CORE ;\n" )

  bBox=cellView->prBoundary->bBox

  x1=caar(bBox)
  y1=cadar(bBox)
  x2=caadr(bBox)
  y2=cadadr(bBox)

  fprintf( pout "    FOREIGN %s %.3f %.3f ;\n" cellView~>cellName x1 y1 )
  fprintf( pout "    ORIGIN %.3f %.3f ;\n" -x1 -y1 )
  fprintf( pout "    SIZE %.3f BY %.3f ;\n" x2-x1 y2-y1)
  fprintf( pout "    SYMMETRY X Y R90 ;\n" )
  fprintf( pout "    SITE CoreSite ;\n" )
  
  pins=cellView~>terminals
  nets=cellView~>nets
  shapes=cellView~>shapes 
  pinShapes=nil 

  foreach( pin pins
    fprintf( pout "    PIN %s\n" pin~>name)
    direction=upperCase(pin~>direction)
    if((direction!="INPUT" && direction!="OUTPUT") direction="INOUT")
    fprintf( pout "        DIRECTION %s ;\n" direction)
    (cond (pin~>name=="Vdd" (fprintf pout "        USE power ;\n"))
          (pin~>name=="GND" (fprintf pout "        USE ground ; \n"))
          )
    thisFigs= setof( fig shapes fig~>net~>name==pin~>net~>name)
    fprintf( pout "        PORT\n" )
    if( thisFigs==nil then
      printf("Error: no fig for pin %s.\n" pin~>name)
    else
      lefFig2String( pout thisFigs nil)
      pinShapes=append( pinShapes thisFigs )
    )  
    fprintf( pout "        END\n" )
    fprintf( pout "    END %s\n" pin~>name)
  )
  fprintf( pout "    OBS\n" )
  obsShapes=setof( shape shapes !member( shape pinShapes ) && member( shape~>lpp MetalLPPs))
  foreach( lpp MetalLPPs lefFig2String( pout shapes car(lpp)))
  fprintf( pout "    END\n" )
  fprintf( pout "END %s\n\n" cellView~>cellName)
)
)

;**********************************************************************
;  main lef function: lefLefOut
;**********************************************************************

defun( lefLefOut ( libName cellName viewName
                    outfileName
                    @key
                    ( Verbose nil )
                    ( Overwrite t )
                  )

  prog( (cellView)
    cellView=nrOpenCellViewReadable( libName cellName viewName )
    if( cellView==nil then 
        printf("Error: Can not open cellview %s\n" inst~>cellName)
      return(nil)
    )
    instances= setof( thisInst cellView~>instances 
          thisInst~>viewName!="symbolic"&&
          thisInst~>libName!=TechLibName)

    uniqueInstances=nil
    foreach( inst instances
      if( !member( inst~>cellName uniqueInstances~>cellName ) then 
        uniqueInstances=cons( inst uniqueInstances )
      )
    )
    abstract_missing=nil
    foreach( inst uniqueInstances
      if( nrOpenCellViewReadable( inst~>libName inst~>cellName "abstract" ) == nil then
        abstract_missing=cons( inst~>cellName abstract_missing )
        printf("Error: Missing Abstract view %s\n" inst~>cellName)
      )
    )
    if( abstract_missing then return(nil))
    if( Overwrite==nil then 
      p_in=infile(outfileName)
      if( p_in then 
        close(p_in) 
        printf("Not Overwrite Existing Lef File: %s\n" outfileName)
        return(nil) 
      )
    )
    pout=outfile(outfileName)
    foreach( inst uniqueInstances
         lefInstanceOut( pout inst~>libName inst~>cellName "abstract"
                       ?Verbose Verbose)
    )
    fprintf( pout "END LIBRARY\n")
    close(pout)
    printf( "Successfully created LEF file: %s\n" outfileName )
    return(t)  
  )  
)

defun( vendorBase ( cellName )
  let( ( baseName )
     baseName = cellName
     if( rexMatchp( "^vendor\\." baseName ) then
        rexCompile( "\\.[0-9]*$" )
        baseName = rexReplace( baseName "" 0 )
        rexCompile( "^.*\\." )
        baseName = rexReplace( baseName "" 0 )
     )
     baseName
  )
)

;**********************************************************************
;  main def function: defDefOut
;**********************************************************************

defun( defDefOut (  libName cellName viewName
                    outfileName                         
                    @key 
                    ( Verbose nil ) 
                    ( DIVIDERCHAR "|" ) 
                    ( BUSBITCHARS "<>" )
                    ( UNITS 1000.0 )                      
                    ( OutputNets t ) 
                    ( OutputPins t ) 
                    ( AllSpecialNets t ) 
                    ( PowerPins t ) 
                    ( transform nil ) 
                    ( UseModuleName nil ) 
                    ( Synchronous nil ) 
                    ( PinMap nil ) 
                    ( DirMap nil ) 
                    ( BlockageCellView nil ) ;BlockageCellView is PowerGridCellView since our placement and routing blockages are in PowerGrid Cell
                    ( Warnonly nil )
                    ( DialogBox t ) ; pop up dialog box on warnings or errors
                    ( VoltageDomain nil )
                    ( powerNets list("Vdd") )
                    ( gndNets   list("GND") )
                   )
 let( (pin_nocast pin_nodef pout pinserr 
         cellView cellView1 foundTieOffCell instances prelayouts
         dieArea dieAreaXY1 dieAreaXY2 powerStr pathSegs
         specialNets sourceDist temp macroType
      )
  defnErr(-1) ; reset error count
  defnWarn(-1) ; reset warning count
  pin_nocast=0
  pin_nodef=0
  prelayouts=nil

  printf("generating def for %s %s (%s).\n" libName cellName viewName)
  cellView=dbOpenCellViewByType( libName cellName viewName )
  cellView1=nil
  foundTieOffCell=nil
  noRotateCellnames=list( "M2_M1" "M2_M1min" 
                          "M3_M2" "M3_M2min"
                          "M4_M3" "M4_M3min"
                          "M5_M4" "M5_M4min"
                          "M6_M5" "M6_M5min"
                          "M7_M6" "M7_M6min"
                        )
  if( transform then
     cellView1=dbCopyCellView( cellView cellView~>libName cellView~>cellName strcat(cellView~>viewName "_" cadr(transform)))
     foreach( fig cellView1~>shapes
       dbMoveFig( fig cellView1 transform)
     )
     foreach( prbobj list(cellView1~>prBoundary)
       dbMoveFig( prbobj cellView1 transform)
     )
     foreach( blockages cellView1~>blockages
       dbMoveFig( blockages cellView1 transform)
     )
     foreach( inst cellView1~>instances
       if( member( inst~>cellName noRotateCellnames ) then
         dbMoveFig( inst cellView1 list(car(transform) "R0" 1.0))
       else
         dbMoveFig( inst cellView1 transform)
       )
     )
     DeleteLabels(?CV cellView1)
     defCanonicalizeVias( cellView1 ?warnonly Warnonly )
     dbSave(cellView1)
     cellView=cellView1
  )

  pout = outfile(outfileName)
  if((pout == nil) then 
     defnErr(1)
     error( sprintf( nil "Error: can not open %s for write.\n" outfileName))
  )

  ; HEADER
  fprintf( pout "# INTEL TOP SECRET\n" )
  fprintf( pout "# Copyright 2012 Intel Corporation. All Rights Reserved.\n" )
  fprintf( pout "#\n" )
  fprintf( pout "# created by Fulcrum defOut program on %s.\n" getCurrentTime() )
  fprintf( pout "#  LIBRARY  : %s\n" cellView~>libName )
  fprintf( pout "#  CELLNAME : %s\n" cellView~>cellName )
  fprintf( pout "#  VIEWNAME : %s\n" cellView~>viewName )
  if( transform then
     fprintf( pout "#  transform : %L\n" transform )
  )
  fprintf( pout "#\n" )
  if(Synchronous then fprintf( pout "VERSION 5.6 ;\n") else fprintf( pout "VERSION 5.5 ;\n"))
  fprintf( pout "NAMESCASESENSITIVE ON ;\n")
  fprintf( pout "DIVIDERCHAR \"%s\" ;\n" DIVIDERCHAR )
  fprintf( pout "BUSBITCHARS \"%s\" ;\n" BUSBITCHARS )
  fprintf( pout "DESIGN %s ;\n" if( UseModuleName then
          cadr( reverse( parseString( cellView~>cellName "." )))
          else cellView~>cellName ))
  fprintf( pout "TECHNOLOGY %s ;\n" TechLibName)
  fprintf( pout "UNITS DISTANCE MICRONS %.0f ;\n" UNITS )

  ; The DIEAREA is computed to enclose everything and be powergrid-aligned
  dieArea=cellView~>prBoundary~>bBox
  if( dieArea==nil then dieArea=cellView~>bBox )
;  dieArea = BBoxSnapOutwardToGrid(cellView~>bBox GridPitch)
  dieAreaXY1 = car(dieArea)
  dieAreaXY2 = cadr(dieArea)

if(!Synchronous
 then
  fprintf( pout "\n" )
  fprintf( pout "DIEAREA ( %.0f %.0f ) ( %.0f %.0f ) ;\n" 
    car(dieAreaXY1)*UNITS 
    cadr(dieAreaXY1)*UNITS
    car(dieAreaXY2)*UNITS 
    cadr(dieAreaXY2)*UNITS )
  fprintf( pout "\n" )
 else
  exportPRBoundary(pout cellView)
  exportClusterRegions(pout cellView)
 )
  ; COMPONENTS
  printf("generating COMPONENTS...\n")
  instances= setof( thisInst cellView~>instances 
          thisInst~>viewName!="symbolic"&&
          thisInst~>libName!=TechLibName)
  fprintf( pout "COMPONENTS %d ;\n" length(instances) )
  foreach( inst instances

    if( inst~>status == "firm"
     then
       macroType = "FIXED"
     else
       macroType = "PLACED"
     )

    thisOrient=setof( orient ORIENTLIST car(orient)==inst~>orient )
    orientFunction=lambda((point) point)
    if((thisOrient!=nil) then 
      thisOrient=car(thisOrient)
      orientFunction=caddr(thisOrient)
      thisOrient=cadr(thisOrient)
    else printf( "Warning: instance %s (%s) has illegal orient. Use \"N\" instead.\n" 
                 inst~>name inst~>cellName )
      thisOrient="N"
    )

    when( inst->viewName=="prelayout"
      prelayouts=t
    )

    ; instance position is defined as lower_left corner of instance 
    ; prBoundary layer in the cellView. So it is not exactly inst~>xy.
    ; In our cases, often lower_left corner of prBoundary is (0 -0.48),
    ; so the instance position should be inst~>xy+(0 -0.48).

    unless( IsGuideInst(inst) ; don't export bus script guides
    instMaster=nrOpenCellViewReadable( inst~>libName inst~>cellName "abstract_edit")
    if( instMaster==nil then instMaster=nrOpenCellViewReadable( inst~>libName inst~>cellName "abstract"))
    if( instMaster==nil then 
      printf("Error: abstract view of %s does not exist!\n" inst~>cellName)
      instMaster=nrOpenCellViewReadable( inst~>libName inst~>cellName inst~>viewName)
    )
    prBound=setof( lpp instMaster~>lpps lpp~>layerName=="prBoundary" )
    if(instMaster->prBoundary || (prBound !=nil) then
      prBoundBox=nil
	  if( instMaster->prBoundary then
        if( instMaster->prBoundary then
          prBoundBox=instMaster->prBoundary->bBox)
      else
        foreach( thisLayer prBound
          foreach( shape thisLayer~>shapes
            if((shape~>objType!="label" && prBoundBox==nil) then
              prBoundBox=shape~>bBox
            )
          )
        )
      )
      if((prBoundBox!=nil && inst~>xy!=nil) then 
        let((point1 point2 x y w1 h1 w2 h2)
          point1=car(prBoundBox)
          point2=cadr(prBoundBox)
          w1=car(point2)-car(point1)
          h1=cadr(point2)-cadr(point1)
          point1=apply( orientFunction list(point1))
          point2=apply( orientFunction list(point2))        
          x=min( car(point1)  car(point2))+ car(inst~>xy)
          y=min(cadr(point1) cadr(point2))+cadr(inst~>xy)
          xy=list(x y)
          w2=caadr(instMaster~>bBox)-caar(instMaster~>bBox)
          h2=cadadr(instMaster~>bBox)-cadar(instMaster~>bBox)
        )
      else 
        xy=car(inst~>bBox)
        printf("Warning: %s (%s) does not have rectangle prBoundary. Use bBox instead.\n"
          inst~>name inst~>cellName)
      )
    else 
      xy=car(inst~>bBox)        
      printf("Warning: %s (%s) does not have prBoundary layer. Use bBox instead.\n"
          inst~>name inst~>cellName)

    )

    instName=inst~>name
    if( Synchronous then
        new=instName
        if(rexCompile( "\\." )
            instName = rexReplace( new "/" 0 ) ) )
    ; If wiring cell, set SOURCE DIST flag as it is a physical component
    if( cadr(NameFilterInstances( list(inst) WiringCellLibCellPairRegExs)) then
        sourceDist=" + SOURCE DIST"
    else
        sourceDist=""
    )

    ; NOTE: all vendor cells have the FQCN converted to the BASE name! (bug 15615)
    temp=sprintf( nil "- %s %s%s + %s %s %s ;\n" 
      instName vendorBase(inst~>cellName) sourceDist macroType defPoint2String(xy UNITS)
      thisOrient )
    if( Verbose printf( "%s" temp ) )
    fprintf( pout "%s" temp )
   )
  )
  fprintf( pout "END COMPONENTS\n\n")

  ; PINS
  pinShapes = makeTable( "pinShapes" nil) ; track which shapes are PINS
  if( OutputPins then
    pinserr=outfile("DEFpins.err")
    printf("generating PINS...\n")
    pins= cellView~>terminals
    if( !PowerPins then pins=setof( pin pins pin~>name!="Vdd" && pin~>name!="GND"))
    fprintf( pout "PINS %d ;\n" length(pins) )
    if( DirMap then
        ValidPins=(let (
            ( NewTable ( makeTable `bla ( arrayref DirMap `unbound ) ) ) )
        ( foreach Key DirMap
                  ( setarray NewTable Key 0 ) )
        NewTable ) )
    foreach( pin pins
      direction=upperCase(pin~>direction)
      if((direction=="INPUTOUTPUT") then direction="INOUT")
      if((direction!="INPUT" && direction!="OUTPUT" && direction!="INOUT" ) then 
        printf("Warning: illegal pin direction \"%s\" found at pin %s. Set it to \"INOUT\".\n" 
          direction pin~>name) 
        direction="INOUT"
      )
      if( DirMap then
         let( ( dir )
            dir = arrayref( DirMap pin~>name )
            if( dir then
                direction = dir
                if((pin~>pinCount > 0) then 
                    ( setarray ValidPins pin~>name 1 ) )
            else
                fprintf( pinserr "Error: pin %s is not in cast\n" pin~>name )
                pin_nocast=pin_nocast+1
                defnErr(1)
            )
         )
      )
      if((pin~>pinCount == 0) then 
        fprintf( pout "- %s + NET %s + DIRECTION %s \n  ;\n"
          pin~>name pin~>net~>name direction )
          fprintf( pinserr "Warning: pin %s has no shapes associated to it.\n" 
            pin~>name) 
          defnWarn(1)
      else
        pinName=pin~>name
        pinNetName=pin~>net~>name
        if( PinMap then
            (let ( vlg )
                vlg = arrayref( PinMap pin~>name )
                if( vlg != nil then
                    pinName = vlg
                )
                vlg = arrayref( PinMap pin~>net~>name )
                if( vlg != nil then
                    pinNetName = vlg
                )
            )
        )
        thisPinName=pinName
        pinCount=0
        pinFigs=pin~>pins~>fig
        foreach( pinFig pinFigs
          cond(
            ((pinFig~>objType=="rect" || ((pinFig~>objType=="path" || pinFig~>objType=="pathSeg" ) && pinFig~>nPoints==2)) 
              pinbBox=pinFig~>bBox
              pinx1=caar(pinbBox)
              piny1=cadar(pinbBox)
              pinx2=caadr(pinbBox)
              piny2=cadadr(pinbBox)
              pinxc=floor((pinx1+pinx2)/2)
              pinyc=floor((piny1+piny2)/2)
                fprintf( pout "- %s + NET %s + DIRECTION %s "
                thisPinName pinNetName direction )
              when( pin~>name=="GND"
                fprintf( pout "+ USE GROUND " ))
              when( pin~>name=="Vdd"
                fprintf( pout "+ USE POWER " ))
                fprintf( pout "+ LAYER %s %s %s "
                pinFig~>layerName defPoint2String( list(pinx1-pinxc piny1-pinyc) UNITS) 
                defPoint2String( list(pinx2-pinxc piny2-pinyc) UNITS) )
                fprintf( pout "+ FIXED %s N ;\n"
                defPoint2String( list(pinxc pinyc) UNITS) )
              pinShapes[pinFig]=t
            )
            ( t printf( "Error: non rect pin fig found at pin %s.\n" pinName )
            )
          )
          pinCount=pinCount+1
          thisPinName=sprintf( nil "%s.extra%d" pinName pinCount )
        )
      )
    )
    if( DirMap then
        ( foreach Key ValidPins
            if( arrayref( ValidPins Key )==0 then
                fprintf( pinserr "Warning: Cast Pin %s Not in DEF\n" Key )
                pin_nodef=pin_nodef+1
                defnWarn(1)
            ) ) )
    fprintf( pout "END PINS\n\n")
  )

  ; NETS
  if( OutputNets then
    printf("generating NETS...\n")

    when( GetPathsForNet(nil)
      printf("\n!!!-------------------------------------------------------\n")
      printf("Paths found with no connectivity.\n")
      printf("These will not be represented in DEF.\n")
      printf("VirtuosoXL removes connectivity from floating wires,\n")
      printf("  so be sure to close it and redraw wires before export.\n")
      printf("----------------------------------------------------------\n")
      defnErr(1)
    )

    FullViaWidth=(ContactExtension *2 + ContactCutWidth)*MicronsPerMeter 

    specialNets=nil

    nets= cellView~>nets
    if( !PowerPins then nets=setof( net nets net~>name!="Vdd" && net~>name!="GND"))
    fprintf( pout "NETS %d ;\n" length(nets) )
    foreach( net nets
      netName = net~>name
      netTermName = net~>term~>name
      if( PinMap then
          (let ( vlg )
              vlg = arrayref( PinMap net~>name )
              if( vlg != nil then
                  netName = vlg
              )
          )
          if( netTermName then
              (let ( vlg )
                  vlg = arrayref( PinMap net~>term~>name )
                  if( vlg != nil then
                      netTermName = vlg
                  )
              )
          )
      )
      fprintf( pout "- %s\n" netName)
      powerStr = ""
      when(Synchronous 
        when( member(netName powerNetGlobal)   powerStr = "+ USE POWER " fprintf( pout "+ USE POWER\n"  ) )
        when( member(netName groundNetGlobal)  powerStr = "+ USE GROUND" fprintf( pout "+ USE GROUND\n" ) )
       )
 
      specialNetString=sprintf( nil "- %s %s\n" netName powerStr)

      if( Verbose printf( "- %s\n" netName))

      if((net~>term!=nil) then
        fprintf( pout "  ( PIN %s )\n" netTermName)
      )
      viaInstances=setof( instTerm net~>allInstTerms instTerm~>inst~>viewName=="symbolic" ||
          instTerm~>inst~>libName==TechLibName)
      realInstances=setof( instTerm net~>allInstTerms instTerm~>inst~>viewName!="symbolic" &&
          instTerm~>inst~>libName!=TechLibName)
      foreach( instTerm realInstances
        fprintf( pout "  ( %s %s )\n" instTerm~>inst~>name instTerm~>name)
      )
      figs=append( net~>term~>pins~>fig net~>figs ) 

      vias=append( setof( fig figs fig~>objType=="inst" || fig~>objType=="stdVia") viaInstances~>inst)
      paths=setof( fig figs fig~>objType=="path" )
      pathSegs=setof( fig figs fig~>objType=="pathSeg" )
      rects=setof( fig figs fig~>objType=="rect" || (fig~>objType=="polygon" && fig~>nPoints<=5) )
      polys=setof( fig figs fig~>objType=="polygon" && fig~>nPoints>5 )
      others=setof( fig figs fig~>objType!="inst" && fig~>objType!="stdVia" && fig~>objType!="path" && fig~>objType!="pathSeg" && fig~>objType!="rect" && fig~>objType!="polygon" && fig~>objType!="route" && fig~>objType!="steiner")

      rects=setof( rect rects defRectArea(rect~>bBox)>FullViaWidth*FullViaWidth*1.01 || rect~>purpose=="pin")
      polys=setof( poly polys defRectArea(poly~>bBox)>FullViaWidth*FullViaWidth*1.01 || poly~>purpose=="pin")

      rects=setof( rect rects !pinShapes[rect])
      paths=setof( path paths !pinShapes[path])
      pathSegs=setof( path pathSegs !pinShapes[path])

      if((others!=nil) then
        println(others~>??)
;       error( sprintf( nil "Error: non path-or-inst net fig found at net %s.\n" 
;                net~>name ))
;       defnErr(1)
      )

      ; convert net shapes to DEF
      if( Synchronous
       then
        ROUTED="+ FIXED"
        specialNetROUTED="+ FIXED"
       else
        ROUTED="+ ROUTED"
        specialNetROUTED="+ ROUTED"
       )
      ; paths
      foreach( path paths
        if( !AllSpecialNets then
          fprintf( pout "  %s %s " ROUTED path~>layerName ) 
          ROUTED="  NEW"
          foreach( point path~>points
            fprintf( pout "%s " defPoint2String(point UNITS))
            lastPoint=point
          )
          fprintf( pout "\n" )
        else
          specialNetString=sprintf( nil "%s  %s %s %.0f " specialNetString specialNetROUTED path~>layerName path~>width*UNITS) 
          specialNetROUTED="  NEW"
          foreach( point path~>points
            specialNetString=sprintf( nil "%s%s " specialNetString defPoint2String(point UNITS))
            lastPoint=point
          )
          specialNetString=sprintf( nil "%s\n" specialNetString )
        )
      )

      foreach( path pathSegs
        if( !AllSpecialNets then
          fprintf( pout "  %s %s " ROUTED path~>layerName ) 
          ROUTED="  NEW"
          fprintf( pout "%s %s " defPoint2String(path~>beginPt UNITS) defPoint2String(path~>endPt UNITS))
          fprintf( pout "\n" )
        else
          specialNetString=sprintf( nil "%s  %s %s %.0f " specialNetString specialNetROUTED path~>layerName path~>width*UNITS) 
          specialNetROUTED="  NEW"
          specialNetString=sprintf( nil "%s%s %s " specialNetString defPoint2String(path~>beginPt UNITS) defPoint2String(path~>endPt UNITS))
          specialNetString=sprintf( nil "%s\n" specialNetString )
        )
      )

      ; rectangles
      foreach( rect rects
        point1=car(rect~>bBox)
        point2=cadr(rect~>bBox)
        x1=car(point1) y1=cadr(point1) x2=car(point2) y2=cadr(point2)
        length=x2-x1 width=y2-y1
        if((length>width) then 
          point1=list( x1+width/2 (y1+y2)/2 )
          point2=list( x2-width/2 (y1+y2)/2 )
        else
          width=length length=y2-y1
          point1=list( (x1+x2)/2 y1+width/2 )
          point2=list( (x1+x2)/2 y2-width/2 )
        )
        if( !AllSpecialNets then
          fprintf( pout "  %s %s " ROUTED rect~>layerName ) 
          ROUTED="  NEW"
          fprintf( pout "%s %s " defPoint2String(point1 UNITS) defPoint2String(point2 UNITS))
          lastPoint=point2
          fprintf( pout "\n" )
        else
          specialNetString=sprintf( nil "%s  %s %s %.0f " specialNetString specialNetROUTED rect~>layerName width*UNITS) 
          specialNetROUTED="  NEW"
          specialNetString=sprintf( nil "%s%s %s " specialNetString defPoint2String(point1 UNITS) defPoint2String(point2 UNITS))
          lastPoint=point2
          specialNetString=sprintf( nil "%s\n" specialNetString )
        )          
      )

      ; vias
      if( !AllSpecialNets then
        foreach( via vias
          thisLpp=setof( lpp VIALPPS rexMatchp(cadr(lpp) via~>viaHeader~>viaDefName) )
          fprintf( pout "  %s %s %s %s\n"
            ROUTED caar(thisLpp) defPoint2String( via~>xy UNITS) via~>viaHeader~>viaDefName )
          ROUTED="  NEW"
        )
        vias=nil
      else
        foreach( via vias
          thisLpp=setof( lpp VIALPPS rexMatchp(cadr(lpp) via~>viaHeader~>viaDefName) )
          specialNetString=sprintf( nil "%s  %s %s 0 %s %s\n" specialNetString
            specialNetROUTED caar(thisLpp) defPoint2String( via~>xy UNITS) via~>viaHeader~>viaDefName )
          specialNetROUTED="  NEW"
        ) 
        vias=nil
        if( Verbose printf("%s" specialNetString))
        specialNets=cons( specialNetString specialNets)
      )
      fprintf( pout "  ;\n")
    )
    fprintf( pout "END NETS\n\n")
    if( specialNets then
      printf("generating SPECIALNETS...\n")
      fprintf( pout "SPECIALNETS %d ;\n" length(specialNets) )
      foreach( line specialNets fprintf( pout "%s  ;\n" line) )
      fprintf( pout "END SPECIALNETS\n\n")
    )  

  )

  when(VoltageDomain exportDomainAssociation(pout powerNets gndNets cellView))

  when(Synchronous exportPlacementBlockages(pout cellView))
  when(Synchronous   exportRoutingBlockages(pout cellView))

  if( BlockageCellView then
    ;BlockageCellView is same as PowerGridCellView
    blockageNum=0
    overlapShapes=setof( shape BlockageCellView~>shapes shape~>layerName=="OVERLAP")
    if( overlapShapes then
      blockageNum=blockageNum+1
    )
    foreach( metalLpp MetalLPPs
      metalShapes=setof( shape BlockageCellView~>shapes shape~>layerName==car(metalLpp) && shape~>purpose=="boundary")
      if( metalShapes then
        blockageNum=blockageNum+2
      )
    )
   when(!Synchronous
    if( blockageNum>0 then
      fprintf( pout "BLOCKAGES %d ;\n" blockageNum)

      if( overlapShapes then
        fprintf( pout "  - PLACEMENT\n")
        defShapeToText( pout overlapShapes transform)
        fprintf( pout "  ;\n")
      )
      foreach( metalLpp MetalLPPs
        metalShapes=setof( shape BlockageCellView~>shapes shape~>layerName==car(metalLpp) && shape~>purpose=="boundary")
        if( metalShapes then
           if( Synchronous 
            then
              fprintf( pout "  - LAYER %s + PUSHDOWN\n" car(metalLpp))
            else
              fprintf( pout "  - LAYER %s + PUSHDOWN\n" car(metalLpp))
              )
           defShapeToText( pout metalShapes transform)
           fprintf( pout "  ;\n")
           if( Synchronous 
            then
              fprintf( pout "  - LAYER %s \n" car(metalLpp))
            else
              fprintf( pout "  - LAYER %s \n" car(metalLpp))
              )
          defShapeToText( pout metalShapes transform)
          fprintf( pout "  ;\n")
        )
      )
      fprintf( pout "END BLOCKAGES\n\n")      
     )
    )
  )
  fprintf( pout "END DESIGN\n")
  close(pout)
  when(OutputPins close(pinserr))

  when(prelayouts
    defnWarn(1)
    printf("\n!!!-------------------------------------------------------\n")
    printf("Warning: Instances found that are \"prelayout\" view. Check flattening.\n")
    printf("----------------------------------------------------------\n")
  )

  when(FindBusOverlaps()
    defnWarn(1)
    printf("\n!!!-------------------------------------------------------\n")
    printf("Warning: Pin/wire overlaps found. Look for error markers.\n")
    printf("----------------------------------------------------------\n")
  )

  if( pin_nocast > 0 || pin_nodef > 0 then
    defnWarn(1)
    printf("\n!!!-------------------------------------------------------\n")
    printf("Warning: %d DEF pins are not in cast -- %d cast pins are not in DEF.\n
              Check DEFpins.err for details.\n\n" pin_nocast pin_nodef )
    printf("----------------------------------------------------------\n")
  )

  if( defnErr(0) > 0 || defnWarn(0) > 0 then
      printf("File %s is generated sucessfully with %d error(s) and %d warning(s)\n" outfileName defnErr(0) defnWarn(0))
  else
      printf("File %s is generated sucessfully.\n" outfileName)
  )
  if( cellView1 then 
    ddDeleteObj( ddGetObj( cellView1~>libName cellView1~>cellName cellView1~>viewName ""))
  )
  if( (boundp 'UIStack ) then
  if( (and DialogBox (or defnWarn(0)>0 defnErr(0)>0)) then
    lefdefErrorDialog = hiDisplayAppDBox(
        ?name 'defDefOutErrorDialogBox
        ?dboxText "Errors or Warnings in defOut, check CIW"
        ?dboxBanner "Errors/Warnings in defOut"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'Close
  ) ) )
  ; return t if no errors
  defnErr(0)==0
 )
)

;;;;;;;;;;;;;;;;;;;;;;
(defun exportTotemLef ( cellName viewName  )
 (let ( CV temp_dir lefPort box tpid gdsName dfIILoc fp_dir time libName )

  libName = car(NameParseCellName(cellName))
  printf("generating totem Lef for %s %s (%s).\n\n" libName cellName viewName)

  CV = dbOpenCellViewByType( libName cellName viewName )  
  time = parseString(dbGetFileTimeStamp(CV) " ")
  temp_dir  = ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ; strcat(root_dir "temp/")
  tpid = (NameStartRename "cell" "cadence" "gds2")
  gdsName = (NameDoRename tpid cellName)
  dfIILoc   = ( ConfigFileGetValue TheCDSConfigTable "DFII_DIR" )
  fp_dir    = NameGetViewDirFromDFIIDirAndCellNameAndView( dfIILoc cellName viewName )
  lefPort = outfile(strcat( fp_dir "/" "cell_totem.lef" ))

  fprintf( lefPort "## Auto generated LEF as reference for Totem CMM Flow\n" )
  fprintf( lefPort "## Created On: %s\n\n",dbGetFileTimeStamp(CV))
  fprintf( lefPort "VERSION 5.6 ;     \n" )
  fprintf( lefPort "BUSBITCHARS \"<>\" ;\n" )
  fprintf( lefPort "DIVIDERCHAR \"|\" ; \n\n" )
  fprintf( lefPort "MACRO %s\n" gdsName )
  fprintf( lefPort "  CLASS BLOCK ;\n" )
  box = GetPrbound(CV)~>bBox
  if( box == nil then
    box=CV->bBox
  )
  fprintf( lefPort "  ORIGIN %.3f %.3f ;\n", caar(box), cadar(box) ) 
  fprintf( lefPort "  SIZE %.3f BY %.3f ;\n", BBoxGetHeight(box), BBoxGetWidth(box) ) 
  fprintf( lefPort "END %s\n\n" GDSIIHierMakeNameGDSIISafe(cellName) )
  fprintf( lefPort "END LIBRARY" )

  close(lefPort)

t
 )
)
