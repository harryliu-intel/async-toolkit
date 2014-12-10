; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
<pre>
****************
Fill Overview
****************

There are 3 types of fill. 
All of them place fill only in areas marked with ( "y9" "drawing" ), which we call the 
fill boundary.

In the order they are typically run.
* Fill grid (<a href=$src/hw/layout/tsmc13/pdk/Fulcrum/filler/fillergrid.rul>assura rules </a>)
  This is placed everywhere where there is non-power-grid metal.
  Powergrid is on M3-M7.  M3-M5 have a 4.8x9.6 pitch.  M6-M8 have a 9.6x9.6 pitch.
  Powergrid is signaled by the existence of yN layers 
  (e.g ("y3" "drawing" ) means that there is M3 powergrid here.  
  Care must be taken not to draw these layers anywhere but the STANDARD powergrid!
  There was a case where a M7_wide powergrid cell had y7 drawing where it shouldn't
  and this caused shorts.
  
  The cells are placed per metal layer.
  (e.g. globals.fill.M3 is the fill for metal 3)
  We also place via cells between layers, that avoids non-power grid metal on 
  the 2 layers it connects.  
  (e.g. globals.fill.VIA34 has vias that connects the M3 to M4 powergrid.
  The via cells also have redundant metal in them so we dont have any floating vias.

  Each fill cell is the same size as its metal's powergrid pitch.
  (e.g. globals.fill.M3 is 4.8x9.6)

  
  The cells are placed with assura using generateRectangle() which just tiles them all in
  the fill boundary, and then gets rid of rectangles that are within 1.2u of non-powergrid
  shapes.  Assura returns with a bunch of rectangles which are postprocessed into fillergrid cells.
  
  There is another post-processing stage called mosaicification, which converts these instances
  into mosaics.  To run this, Do the following:
  * From layout: ( dbWriteSkill cellview "file1" "w" "5.0" )
  * Run fulcrum mosaicify file1 file2
  * From layout, open an empty cellview where you want the mosaics, nad ( load "file2" )
    If you have multiple cellviews and you want to merge all their mosaics, just cat
    together the results of dbWriteSkill. 

* Fill cells (<a href=$src/hw/layout/tsmc13/pdk/Fulcrum/filler/fillercells.rul>assura rules </a>)
  This is placed 0.6u away from existing metal.  It just uses the standard assura
  generateFill().  The fill is non-optimal but is space-efficient because it consists
  of mosaics. 


* Fill shapes (<a href=$src/hw/layout/tsmc13/pdk/Fulcrum/filler/fillershapes.rul>assura rules </a>)
  This is placed 0.6u away from existing metal.  It is more optimal than filler cells
  but it consists of flat shapes.  Care is taken to avoid spacing <=0.4, area<=1.44, 
  width<=0.6, and vertices>4.  Particularly useful for place and route cells.
  

****************
How to run fill
****************

Currently, the interface is through the CIW in cadence.  For chip level stuff you'll 
probably want to start cadence with
CDS_AUTO_64BIT=ALL /usr/local/cadence/scripts/cadence.latest.run layout


Just run ( Fill cellview ?Type type ?Layers layers )

cellview is a cellview dbId
type is one of ("grid" "cells" "shapes")
layers is t or nil and it means the following for the different types

grid/nil   : od through m2
grid/t     : via23 through m7, dont export gates/stacks

cells/nil  : od through m1
cells/t    : m2 through m7, don't export gates/stacks

shapes/t   : m2 though m3, don't export gates/stacks
shapes/nil : m4 through m7, don't export gates/stacks

First the CIW will tell you the name of the library to look for the result...
something like FILL_grid0
The library will be in your cds.config TEMP directory, and the assura logs
will be in some AssuraRuXXXXX directory inside your TEMP directory.


Then it will probably complain about some libraries or something(don't worry),
and then it will say:

"running assura"

Then it will run for a while (3-4 hours or so for the chip, depending on the type of run)
and come back with:

"done with assura"

And you're good to go.
</pre>
*/

(defun FillerGridAlignInstances ( CellView
                                  FillerGridLibCellExpressionPairs
                                  FillerGridAlignmentInUserUnits )
  (let (
        ( FilterRet ( NameFilterInstances
                      ( getq CellView instances )
                      FillerGridLibCellExpressionPairs ) ) )
    (let (
          ( FillerGridInstances ( cadr FilterRet ) ) )
      ( foreach
        Instance
        FillerGridInstances
        ( InstanceAlignInstance Instance FillerGridAlignmentInUserUnits ) ) ) ) )

(defun FillerGridMakeFillerGrid ( SrcCellView
                                  TargetLibName
                                  PDKPackageRoot
                                  TempDir
                                  FillerGridLib
                                  FillerGridPolyCellName
                                  FillerGridM1CellName
                                  FillerGridM2CellName
                                  FillerGridM3CellName
                                  FillerGridM4CellName
                                  FillerGridM5CellName
                                  FillerGridM6CellName
                                  FillerGridM7CellName
                                  FillerGridContCellName
                                  FillerGridVia12CellName
                                  FillerGridVia23CellName
                                  FillerGridVia34CellName
                                  FillerGridVia45CellName
                                  FillerGridVia56CellName
                                  FillerGridVia67CellName
                                  FillerGridViewName
                                  FillerGridAlignmentInUserUnits
                                  FillerGridLPP
                                  @key 
                                  ( LeaveMess nil )
                                  ( M2M7 nil )
                                  ( ODM2 nil )
                                  )
  (let (
        ( RuleFile
          ( sprintf
            nil "%s/share/Fulcrum/filler/fillergrid.rul" PDKPackageRoot ) )
        ( ErrorStr nil ) )

    ( setq
      ErrorStr
      ( AssuraRunAssuraLayerProcessor
        SrcCellView
        TargetLibName
        ( getq SrcCellView cellName )
        ( getq SrcCellView viewName )
        RuleFile
        TempDir
        ( append
          (when ODM2
            ( list 
              ( list "polyFill" PolyLPP )
              ( list "m1Fill" Metal1LPP )
              ( list "contFill" ContactLPP )
              ( list "m2Fill" Metal2LPP )
              ( list "via12Fill" Via12LPP )
              ) )
          (when M2M7
            ( list 
              ( list "m3Fill" Metal3LPP )
              ( list "m4Fill" Metal4LPP )
              ( list "m5Fill" Metal5LPP )
              ( list "m6Fill" Metal6LPP )
              ( list "m7Fill" Metal7LPP )
              ( list "via23Fill" Via23LPP )
              ( list "via34Fill" Via34LPP )
              ( list "via45Fill" Via45LPP )
              ( list "via56Fill" Via56LPP )
              ( list "via67Fill" Via67LPP )
              ) ) )

        ( append
          (when ODM2
            ( list 
              ( list
                ( list FillerGridLib FillerGridPolyCellName FillerGridViewName )
                "polyFill" )
              ( list
                ( list FillerGridLib FillerGridM1CellName FillerGridViewName )
                "m1Fill" )
              ( list
                ( list FillerGridLib FillerGridM2CellName FillerGridViewName )
                "m2Fill" )
              ( list
                ( list FillerGridLib FillerGridContCellName FillerGridViewName )
                "contFill" )
              ( list
                ( list FillerGridLib FillerGridVia12CellName FillerGridViewName )
                "via12Fill" ) ) )
            (when M2M7
              ( list 
                ( list
                  ( list FillerGridLib FillerGridM3CellName FillerGridViewName )
                  "m3Fill" )
                ( list
                  ( list FillerGridLib FillerGridM4CellName FillerGridViewName )
                  "m4Fill" )
                ( list
                  ( list FillerGridLib FillerGridM5CellName FillerGridViewName )
                  "m5Fill" )
                ( list
                  ( list FillerGridLib FillerGridM6CellName FillerGridViewName )
                  "m6Fill" )
                ( list
                  ( list FillerGridLib FillerGridM7CellName FillerGridViewName )
                  "m7Fill" )

                ( list
                  ( list FillerGridLib FillerGridVia23CellName FillerGridViewName )
                  "via23Fill" )
                ( list
                  ( list FillerGridLib FillerGridVia34CellName FillerGridViewName )
                  "via34Fill" )
                ( list
                  ( list FillerGridLib FillerGridVia45CellName FillerGridViewName )
                  "via45Fill" )
                ( list
                  ( list FillerGridLib FillerGridVia56CellName FillerGridViewName )
                  "via56Fill" )
                ( list
                  ( list FillerGridLib FillerGridVia67CellName FillerGridViewName )
                  "via67Fill" )
           ) ) )
        ?AssuraSets ( append (when ODM2 ( list "ODM2" ) ) ( when M2M7 ( list "M2M7" ) ) )
        ?CustomFill t
        ?NoPCells ( not ODM2 )
        ?LeaveMess LeaveMess
        ?AssuraRunLog "assura.log"
        ) )
    ( println ErrorStr ) ) )



(defun FillerMakeFillerCells ( SrcCellView
                               TargetLibName
                               PDKPackageRoot
                               TempDir
                               FillerGridLib
                               FillerGridODPolyBigCellName
                               FillerGridODPolySmallCellName
                               FillerLPP
                               @key 
                               ( LeaveMess nil )
                               ( M2M7 nil )
                               ( ODM1 nil ) )
  (let (
        ( RuleFile
          ( sprintf
            nil
            "%s/share/Fulcrum/filler/fillercells.rul"
            PDKPackageRoot ) )
        ( ErrorStr nil ) )

    ( setq
      ErrorStr
      ( AssuraRunAssuraLayerProcessor
        SrcCellView
        TargetLibName
        ( getq SrcCellView cellName )
        ( getq SrcCellView viewName )
        RuleFile
        TempDir
        ( append
          (when ODM1
            ( list
              ( list "odFillBig"   DiffLPP )
              ( list "odFillSmall" DiffLPP )
              ( list "m1Fill" Metal1LPP ) ) )
          (when M2M7
            ( list
              ( list "m2Fill" Metal2LPP )
              ( list "m3Fill" Metal3LPP )
              ( list "m4Fill" Metal4LPP )
              ( list "m5Fill" Metal5LPP )
              ( list "m6Fill" Metal6LPP )
              ( list "m7Fill" Metal7LPP )
              ) ) )

        (when ODM1
          ( list
            ( list
              ( list FillerGridLib FillerGridODPolyBigCellName FillerGridViewName )
              "odFillBig" )
            ( list
              ( list FillerGridLib FillerGridODPolySmallCellName FillerGridViewName )
              "odFillSmall" )

            ( list
              ( list FillerGridLib "globals.fill.M1BOX" FillerGridViewName )
              "m1Fill_FILL" )
            ( list
              ( list FillerGridLib "globals.fill.M2BOX" FillerGridViewName )
              "m2Fill_FILL" )
            ( list
              ( list FillerGridLib "globals.fill.M3BOX" FillerGridViewName )
              "m3Fill_FILL" )
            ( list
              ( list FillerGridLib "globals.fill.M4BOX" FillerGridViewName )
              "m4Fill_FILL" )
            ( list
              ( list FillerGridLib "globals.fill.M5BOX" FillerGridViewName )
              "m5Fill_FILL" )
            ( list
              ( list FillerGridLib "globals.fill.M6BOX" FillerGridViewName )
              "m6Fill_FILL" )
            ( list
              ( list FillerGridLib "globals.fill.M7BOX" FillerGridViewName )
              "m7Fill_FILL" )
            
            ) )

        ?AssuraSets ( append (when ODM1 ( list "ODM1" ) ) ( when M2M7 ( list "M2M7" ) ) )
        ?Promote t
        ?NoPCells ( not ODM1 )
        ?LeaveMess LeaveMess 
        ) )
    (cond (
           ErrorStr
           ( println ErrorStr ) ) ) ) )


(defun FillerMakeFillerShapes ( SrcCellView
                                TargetLibName
                                PDKPackageRoot
                                TempDir
                                @key 
                                ( LeaveMess nil )
                                ( M2M3 nil )
                                ( M4M7 nil ) )
  (let (
        ( RuleFile
          ( sprintf
            nil
            "%s/share/Fulcrum/filler/fillershapes.rul"
            PDKPackageRoot ) )
        ( ErrorStr nil ) )

    ( setq
      ErrorStr
      ( AssuraRunAssuraLayerProcessor
        SrcCellView
        TargetLibName
        ( getq SrcCellView cellName )
        ( getq SrcCellView viewName )
        RuleFile
        TempDir
        ( list
          ( list "m2Fill" ( list ( car Metal2LPP ) "dummy" ) )
          ( list "m3Fill" ( list ( car Metal3LPP ) "dummy" ) )
          ( list "m4Fill" ( list ( car Metal4LPP ) "dummy" ) )
          ( list "m5Fill" ( list ( car Metal5LPP ) "dummy" ) )
          ( list "m6Fill" ( list ( car Metal6LPP ) "dummy" ) )
          ( list "m7Fill" ( list ( car Metal7LPP ) "dummy" ) )
          )
        nil
        ?AssuraSets ( append (when M2M3 ( list "M2M3" ) ) ( when M4M7 ( list "M4M7" ) ) )
        ?NoPCells t
        ?LeaveMess LeaveMess
        ) )
    (cond (
           ErrorStr
           ( println ErrorStr ) ) ) ) )

; creates a cell in library
; Fill_<type>XXXXXX

(defun Fill ( CellView
              @key
              ( Type "grid" )
              ( Layers nil )
              ( LeaveMess nil )
              )
  (let (
        ( FillerGridLib "globals" )
        ( FillerGridViewName "layout" )
        ( TempDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
        ( PDKPackageRoot ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )

        ( FillerGridPolyCellName  "globals.fill.DCAP" )
        ( FillerGridM1CellName   "globals.fill.M1" )
        ( FillerGridM2CellName   "globals.fill.M2" ) 
        ( FillerGridM3CellName  "globals.fill.M3" )
        ( FillerGridM4CellName  "globals.fill.M4" )
        ( FillerGridM5CellName  "globals.fill.M5" )
        ( FillerGridM6CellName  "globals.fill.M6" )
        ( FillerGridM7CellName  "globals.fill.M7" )
        
        ( FillerGridContCellName    "globals.fill.DCAP" )
        ( FillerGridVia12CellName   "globals.fill.M12" )
        ( FillerGridVia23CellName   "globals.fill.M23" )
        ( FillerGridVia34CellName   "globals.fill.M34" )
        ( FillerGridVia45CellName   "globals.fill.M45" )
        ( FillerGridVia56CellName   "globals.fill.M56" )
        ( FillerGridVia67CellName   "globals.fill.M67" )

        ( FillerGridAlignmentInUserUnits 9.6 )
        ( FillerLPP  ( list "y0" "drawing" ) )

        ( FillerGridODPolyBigCellName "globals.fill.ODPOLY_big" )
        ( FillerGridODPolySmallCellName "globals.fill.ODPOLY_small" ) )

  (let (
        ( TargetLibName 
          ( LibCreateTempLibraryFromCellView 
            CellView 
            ( strcat "FILL_" Type )
            TempDir ) ) )
    
    (cond (
           ( equal Type "grid" )
           ( FillerGridMakeFillerGrid 
             CellView
             TargetLibName
             PDKPackageRoot
             TempDir
             FillerGridLib
             FillerGridPolyCellName
             FillerGridM1CellName
             FillerGridM2CellName
             FillerGridM3CellName
             FillerGridM4CellName
             FillerGridM5CellName
             FillerGridM6CellName
             FillerGridM7CellName
             FillerGridContCellName
             FillerGridVia12CellName
             FillerGridVia23CellName
             FillerGridVia34CellName
             FillerGridVia45CellName
             FillerGridVia56CellName
             FillerGridVia67CellName
             FillerGridViewName
             FillerGridAlignmentInUserUnits
             FillerLPP
             ?LeaveMess LeaveMess
             ?M2M7 Layers
             ?ODM2 ( not Layers )  )
           )
          (
           ( equal Type "cells" )
           ( FillerMakeFillerCells
             CellView
             TargetLibName
             PDKPackageRoot
             TempDir
             FillerGridLib
             FillerGridODPolyBigCellName
             FillerGridODPolySmallCellName
             FillerLPP
             ?LeaveMess LeaveMess
             ?M2M7 Layers
             ?ODM1 ( not Layers ) ) )
          (
           ( equal Type "shapes" )
           ( FillerMakeFillerShapes
             CellView
             TargetLibName
             PDKPackageRoot
             TempDir
             ?LeaveMess LeaveMess
             ?M2M3 Layers
             ?M4M7 ( not Layers ) ) )
 ) ) ) )
