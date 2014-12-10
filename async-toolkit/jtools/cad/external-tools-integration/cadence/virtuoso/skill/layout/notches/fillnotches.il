; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: FillNotches
; Parameter: CellView (CVId)
; Return:  CellView (VIId)
;
; Fill Notches

defun( FillNotches ( CellView 
                     @key 
                     ( NOTCH t ) 
                     ( POLY t ) 
                     ( AREA t ) 
                     ( EXTENSION t ) 
                   )

    (let (
          ( MetalLPPs ( list Metal1LPP Metal2LPP Metal3LPP ) ) )
      ( NotchesFillNotchesOnCellView
        CellView        
        ( append 
          (if PolyLPP && POLY ( list ( list "polyfix" PolyLPP )
                             ( list "notchpoly" PolyLPP ) ) )          
          ( append
            (if AREA
            ( mapcar
              (lambda ( MetalLPP )
                ( rexCompile "METAL" )
                ( list ( rexReplace ( car MetalLPP ) "area" 0 )  MetalLPP ) )
              MetalLPPs ) )
            ( append
              (if NOTCH
              ( mapcar
                (lambda ( MetalLPP )
                  ( rexCompile "METAL" )
                  ( list ( rexReplace ( car MetalLPP ) "notch" 0 )  MetalLPP ) )
                MetalLPPs ) )
              (if EXTENSION
              ( mapcar
                (lambda ( MetalLPP )
                  ( rexCompile "METAL" )
                  ( list ( rexReplace ( car MetalLPP ) "extension" 0 )  MetalLPP ) )
                MetalLPPs ) ) 
             ) 
           ) 
        ) 
        ( append
          ( mapcar 
            (lambda ( MetalLPP )
              ( car MetalLPP ) )
            MetalLPPs )
          ( setof Set 
             ( list (if NOTCH "NOTCH") (if AREA "AREA") (if EXTENSION "EXTENSION") (if POLY "POLY") ) 
            ( stringp Set )
          ) )
        NotchDRCRuleFile
        WorkingDir )

      ( techBindTechFile
        CellView
        TechLibName
        "techfile.cds"
        t )
                                       
      ( dbSave CellView )
    )  
CellView 
)

defun( FillNotches65 ( CellView 
                     @key 
                     ( DOUBLE_VIA t ) 
                     ( NOTCH t ) 
                     ( POLY t ) 
                     ( AREA nil ) 
                     ( EXTENSION nil ) 
                   )

    (let (
          ( MetalLPPs ( list Metal1LPP Metal2LPP Metal3LPP ) ) 
          ( MetalViaLPPs ( list Metal1LPP Metal2LPP Metal3LPP Via12LPP Via23LPP) )         )
      ( NotchesFillNotchesOnCellView
        CellView        
        ( foreach mapcan Set 
            (list
              (if PolyLPP && POLY ( list ( list "polyfix" PolyLPP )
                             ( list "notchpoly" PolyLPP ) ) )          
              (if AREA
              ( mapcar
                (lambda ( MetalLPP )
                  ( rexCompile "M" )
                  ( list ( rexReplace ( car MetalLPP ) "area" 0 )  MetalLPP ) )
                MetalLPPs ) )  
              (if NOTCH
              ( mapcar
                (lambda ( MetalLPP )
                  ( rexCompile "M" )
                  ( list ( rexReplace ( car MetalLPP ) "notch" 0 )  MetalLPP ) )
                MetalLPPs ) )
              (if EXTENSION
              ( mapcar
                (lambda ( MetalLPP )
                  ( rexCompile "M" )
                  ( list ( rexReplace ( car MetalLPP ) "extension" 0 )  MetalLPP ) )
                MetalLPPs ) ) 
              (if DOUBLE_VIA
              list( 
                list( "add_via1" Via12LPP ) 
                list( "add_via2" Via23LPP ) 
                list( "add_via1_m1" Metal1LPP ) 
                list( "add_via1_m2" Metal2LPP ) 
                list( "add_via2_m2" Metal2LPP ) 
                list( "add_via2_m3" Metal3LPP ) 
                ) ) 
             )
             Set  
         )  
        ( append
          ( mapcar 
            (lambda ( MetalLPP )
              ( car MetalLPP ) )
            MetalViaLPPs )
          ( setof Set 
             ( list (if NOTCH "NOTCH") (if AREA "AREA") (if EXTENSION "EXTENSION") (if POLY "POLY") (if DOUBLE_VIA "DOUBLE_VIA" )) 
            ( stringp Set )
          ) )
        NotchDRCRuleFile
        WorkingDir )

      ( techBindTechFile
        CellView
        TechLibName
        "techfile.cds"
        t )
                                       
      ( dbSave CellView )
    )  
CellView 
)

defun( nrFindInstancesOverlapShape ( shape instances )
  let( (xMin yMin xMax yMax x1 x2 y1 y2 )
    xMin=min(caar(shape~>bBox) caadr(shape~>bBox))
    yMin=min(cadar(shape~>bBox) cadadr(shape~>bBox))
    xMax=max(caar(shape~>bBox) caadr(shape~>bBox))
    yMax=max(cadar(shape~>bBox) cadadr(shape~>bBox))

    foreach( mapcan inst instances
      x1=min(caar(inst~>bBox) caadr(inst~>bBox))
      y1=min(cadar(inst~>bBox) cadadr(inst~>bBox))
      x2=max(caar(inst~>bBox) caadr(inst~>bBox))
      y2=max(cadar(inst~>bBox) cadadr(inst~>bBox))
      cond(
        ( x1>xMax || x2<xMin || y1>yMax || y2<yMin nil )
        ( t list( inst ) )
      )
    )
  )
)

defun(  nrReplaceVia ( CellView viaCellViewMaster viaAvailShapes viaInstances LPP1 LPP2 width1 height1 width2 height2)
  let( ( Grid xx yy thisVia )
      Grid=ManufacturingGrid*MicronsPerMeter

      foreach( shape viaAvailShapes
        thisVia=nrFindInstancesOverlapShape( shape viaInstances )
        cond(
          ( thisVia==nil  printf( "Found No via overlaping shape %L" shape~>bBox ))
          ( length(thisVia)>1  printf( "Found more than one via overlaping shape %L" shape~>bBox ))
          ( t
            thisVia=car(thisVia)
            xx=(caar(shape~>bBox)+caadr(shape~>bBox))/2
            yy=(cadar(shape~>bBox)+cadadr(shape~>bBox))/2
            xx=floor(xx/Grid)*Grid
            yy=floor(yy/Grid)*Grid
   	    dbCreateParamInst(CellView viaCellViewMaster nil 
		(xx:yy) "R0" 1 
		list(
		    list("column" "int" 1) 
		    list("row" "int" 1) 
		)
	    )
	    if( width1>0 && height1>0 then 
              dbCreateRect(CellView LPP1 
			list((xx-width1/2:yy-height1/2) (xx+width1/2:yy+height1/2)))
            )
	    if( width2>0 && height2>0 then 
   	      dbCreateRect(CellView LPP2 
			list((xx-width2/2:yy-height2/2) (xx+width2/2:yy+height2/2))) 
            )           
            viaInstances=setof( inst viaInstances inst!=thisVia )            
            dbDeleteObject(thisVia)
            dbDeleteObject(shape)
          )
        )
      )
   viaInstances

   )
)

defun( nrFixViaExtension ( CellView 
                        @key 
                        ( SkipAssuraRun nil ) 
                        ( CONT t ) 
                        ( VIA12 t ) 
                        ( VIA23 t ) 
     )

    (let ( Via12Instances newVia1_1 newVia1_2 newVia1_3 newVia1_4 via12Master
           Via23Instances newVia2_1 newVia2_2 newVia2_3 newVia2_4 via23Master
           contInstances newCont_1 newCont_2 contMaster)
     if( SkipAssuraRun==nil then
       NotchesFillNotchesOnCellView(
        CellView
        append( if( CONT list( list( "newCont_1" list( "CONT" "dummy1"))
                               list( "newCont_2" list( "CONT" "dummy2"))))
        append( if( VIA12 list( list( "newVia1_1" list( "VIA12" "dummy1"))
                                list( "newVia1_2" list( "VIA12" "dummy2"))
                                list( "newVia1_3" list( "VIA12" "dummy3"))
                                list( "newVia1_4" list( "VIA12" "dummy4"))))
                if( VIA23 list( list( "newVia2_1" list( "VIA23" "dummy1"))
                                list( "newVia2_2" list( "VIA23" "dummy2"))
                                list( "newVia2_3" list( "VIA23" "dummy3"))
                                list( "newVia2_4" list( "VIA23" "dummy4"))))
        ))
        setof( Set 
          list( if( CONT "CONT") if( VIA12 "VIA12") if( VIA23 "VIA23") ) 
          stringp( Set )
        )       
        NotchDRCRuleFile
        WorkingDir )  

      ( techBindTechFile
        CellView
        TechLibName
        "techfile.cds"
        t )
      dbSave( CellView )
     )
                                       
;      ViaInstances= cadr( NameFilterInstances( CellView~>instances ContactLibCellExpressionPairs ))
;      ViaPolyInstances= setof( inst ViaInstances inst~>cellName==CCAR_M1POLY1ContactName )
      newCont_1 = setof( shape CellView~>shapes shape~>layerName=="CONT" && shape~>purpose=="dummy1") 
      newCont_2 = setof( shape CellView~>shapes shape~>layerName=="CONT" && shape~>purpose=="dummy2") 

      if( newCont_1 || newCont_2 then

        contInstances= setof( inst CellView~>instances inst~>cellName==CCAR_M1POLY1ContactName )
        contMaster = dbOpenCellViewByType(TechLibName CCAR_M1POLY1ContactName "layout" "maskLayout")
 
        contInstances=nrReplaceVia( CellView contMaster newCont_1 contInstances PolyLPP Metal1LPP 0 0 0.26 0.17)
        contInstances=nrReplaceVia( CellView contMaster newCont_2 contInstances PolyLPP Metal1LPP 0 0 0.17 0.26)
      )

      newVia1_1 = setof( shape CellView~>shapes shape~>layerName=="VIA12" && shape~>purpose=="dummy1") 
      newVia1_2 = setof( shape CellView~>shapes shape~>layerName=="VIA12" && shape~>purpose=="dummy2") 
      newVia1_3 = setof( shape CellView~>shapes shape~>layerName=="VIA12" && shape~>purpose=="dummy3") 
      newVia1_4 = setof( shape CellView~>shapes shape~>layerName=="VIA12" && shape~>purpose=="dummy4") 

      if( newVia1_1 || newVia1_2 || newVia1_3 || newVia1_4 then

        Via12Instances= setof( inst CellView~>instances inst~>cellName==CCAR_M2M1ViaName )
        via12Master = dbOpenCellViewByType(TechLibName CCAR_M2M1ViaName "layout" "maskLayout")
 
        Via12Instances=nrReplaceVia( CellView via12Master newVia1_1 Via12Instances Metal1LPP Metal2LPP 0.29 0.21 0.2 0.29)
        Via12Instances=nrReplaceVia( CellView via12Master newVia1_2 Via12Instances Metal1LPP Metal2LPP 0.21 0.29 0.2 0.29)
        Via12Instances=nrReplaceVia( CellView via12Master newVia1_3 Via12Instances Metal1LPP Metal2LPP 0.29 0.21 0.29 0.2)
        Via12Instances=nrReplaceVia( CellView via12Master newVia1_4 Via12Instances Metal1LPP Metal2LPP 0.21 0.29 0.29 0.2)
      )

      newVia2_1 = setof( shape CellView~>shapes shape~>layerName=="VIA23" && shape~>purpose=="dummy1") 
      newVia2_2 = setof( shape CellView~>shapes shape~>layerName=="VIA23" && shape~>purpose=="dummy2") 
      newVia2_3 = setof( shape CellView~>shapes shape~>layerName=="VIA23" && shape~>purpose=="dummy3") 
      newVia2_4 = setof( shape CellView~>shapes shape~>layerName=="VIA23" && shape~>purpose=="dummy4")

      if( newVia2_1 || newVia2_2 || newVia2_3 || newVia2_4 then
  
        Via23Instances= setof( inst CellView~>instances inst~>cellName==CCAR_M3M2ViaName )
        via23Master = dbOpenCellViewByType(TechLibName CCAR_M3M2ViaName "layout" "maskLayout") 

        Via23Instances=nrReplaceVia( CellView via23Master newVia2_1 Via23Instances Metal2LPP Metal3LPP 0.2 0.29 0.29 0.2)
        Via23Instances=nrReplaceVia( CellView via23Master newVia2_2 Via23Instances Metal2LPP Metal3LPP 0.29 0.2 0.29 0.2)
        Via23Instances=nrReplaceVia( CellView via23Master newVia2_3 Via23Instances Metal2LPP Metal3LPP 0.2 0.29 0.2 0.29)
        Via23Instances=nrReplaceVia( CellView via23Master newVia2_4 Via23Instances Metal2LPP Metal3LPP 0.29 0.2 0.2 0.29)
     )
   dbSave( CellView )

   CellView 
   )  
)
        
