; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PCellGetParameters ( PCell )
  ( PropGetParameters
    ( getq ( getq PCell master ) superMaster ) ) )

(defun PCellGetParameterType ( PCellSuperMaster ParameterName )
  (let (
        ( Params ( PropGetParameters PCellSuperMaster ) ) )
    (let (
          ( TheParam ( car
                       ( setof
                         Param
                         Params
                         ( equal ( getq Param name ) ParameterName ) ) ) ) )
      (when TheParam
        ( getq TheParam valueType ) ) ) ) )

(defun PCellGetSuperMasterForInstance ( Instance )
  ( getq ( getq Instance master ) superMaster ) )


(defun PCellGetParamTypeForSchematicInstance ( Instance ParamName )
  (let (
        ( LibName ( getq Instance libName ) )
        ( CellName ( getq Instance cellName ) )
        ( LayoutViewName "layout" ) )
    (let (
          ( LayoutViewDDObj ( ddGetObj LibName CellName LayoutViewName ) ) )
      (when ( and
              LayoutViewDDObj
              ( getq LayoutViewDDObj files ) )
        (let (
              ( LayoutCellView ( dbOpenCellViewByType
                                 LibName
                                 CellName
                                 LayoutViewName
                                 nil
                                 "r" ) ) )
          (when LayoutCellView
            ( PCellGetParameterType LayoutCellView ParamName ) ) ) ) ) ) )

(defun PCellGetParameterList ( PCell )
  ( PropGetPropertyListFromListOfProps
    ( list ( PropGetParameters 
             ( PCellGetSuperMasterForInstance PCell ) )
           ( getq PCell prop ) ) ) )

(defun PCellSetParameterList ( PCell ParameterList )
  ( foreach 
    Parameter
    ParameterList
    ( dbReplaceProp 
      Instance
      ( car Parameter )
      ( cadr Parameter )
      ( caddr Parameter ) ) ) )

(defun PCellGetParameterValue ( PCell ParameterName )
  ( PropGetPropValueFromListOfProps
    ( list ( getq PCell prop )
           ( PropGetParameters
             ( PCellGetSuperMasterForInstance PCell ) ) )
    ParameterName ) )


;we have code organized by cell
;we want certain gate/model pairs to be made
;we can either
;1) get code location from gate/model pair OR
;2) get gate/model pair from code location

(defun PCellCreateProcessLibs ( PDKRoot 
                                Technology 
                                DFIIDir 
                                DirectivesFile )
  (let (
        ( Directives ( CellInfoGetTableForFileName
                       DirectivesFile ) ) )
    ( load ( sprintf 
             nil
             "%s/share/Fulcrum/skill/pcell.il"
             PDKRoot ) )
    ( PDKCreateProcessLibs 
      ( PackageGetBinRoot )
      PDKRoot Technology DFIIDir Directives )
    ) )
