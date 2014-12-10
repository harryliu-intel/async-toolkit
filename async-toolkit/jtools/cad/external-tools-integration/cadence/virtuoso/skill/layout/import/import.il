; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun ImportMungeCellInstances ( CellToMunge CellViewTable )
  (let (
        ( InstancesToMunge ( getq CellToMunge instances ) ) )
    ( foreach
      InstanceToMunge
      InstancesToMunge
      (let (
            ( CurrentMasterLibName ( getq InstanceToMunge libName ) )
            ( CurrentMasterCellName ( getq InstanceToMunge cellName ) )
            ( CurrentMasterViewName ( getq InstanceToMunge viewName ) ) )
        (let (
              ( NewMasterCellView ( arrayref
                                    CellViewTable
                                    ( list
                                      CurrentMasterLibName
                                      CurrentMasterCellName
                                      CurrentMasterViewName ) ) ) )
          (when NewMasterCellView
            ( dbSetq InstanceToMunge NewMasterCellView master ) ) ) ) ) ) )


(defun ImportBuildLibCellViewMappingTableFromCellNameMappingTable ( CellNameTable
                                                                    SrcLibName
                                                                    SrcViewName
                                                                    TargetLibFunc
                                                                    TargetLibFuncParams
                                                                    TargetView )
  (let (
        ( Result ( makeTable "LibCellViewTable" nil ) ) )
    ( foreach
      CellNameEntry
      ( tableToList CellNameTable )
      (let (
            ( OldCellName ( car CellNameEntry ) )
            ( NewCellName ( cadr CellNameEntry ) ) )
        (unless ( equal OldCellName NewCellName )       
          (let (
                ( NewLibName ( apply
                               TargetLibFunc
                               ( cons
                                 OldCellName
                                 ( cons
                                   NewCellName
                                   TargetLibFuncParams ) ) ) ) )
            (when NewLibName
              ( setarray
                Result
                ( list
                  SrcLibName
                  OldCellName
                  SrcViewName )
                ( list
                  NewLibName
                  NewCellName
                  TargetView ) ) ) ) ) ) )
    Result ) )

(defun ImportBuildCellViewTableFromLibCellViewMappingTable ( LibCellViewMappingTable
                                                             TechnologyLibraryName
                                                             DFIIDir )
  (let (
        ( ErrorStr nil )
        ( Result ( makeTable "CellViewTable" nil ) ) )
    ( foreach
      LibCellViewEntry
      ( tableToList LibCellViewMappingTable )
      (unless ErrorStr
        (let (
              ( OldLibCellViewTripple ( car LibCellViewEntry ) )
              ( NewLibCellViewTripple ( cadr LibCellViewEntry ) ) )
          (let (
                ( OldLibName ( car OldLibCellViewTripple ) )
                ( OldCellName ( cadr OldLibCellViewTripple ) )
                ( OldViewName ( caddr OldLibCellViewTripple ) )
                ( NewLibName ( car NewLibCellViewTripple ) )
                ( NewCellName ( cadr NewLibCellViewTripple ) )
                ( NewViewName ( caddr NewLibCellViewTripple ) ) )
            (let (
                  ( OldCellViewDDObj ( ddGetObj OldLibName OldCellName OldViewName ) ) )
              (if ( and
                    OldCellViewDDObj
                    ( getq OldCellViewDDObj files ) )
                  (let (
                        ( OldCellView (let (
                                            ( MyCellView ( dbOpenCellViewByType
                                                           OldLibName
                                                           OldCellName
                                                           OldViewName
                                                           nil
                                                           "r" ) ) )
                                        (unless MyCellView
                                          ( setq
                                            ErrorStr
                                            ( sprintf
                                              nil
                                              "Unable to open %L %L %L."
                                              OldLibName
                                              OldCellName
                                              OldViewName ) ) )
                                        MyCellView ) )
                        ( NewLibDDObj (let (
                                            ( ExistingLibDDObj ( ddGetObj NewLibName ) ) )
                                        (if ExistingLibDDObj
                                            ExistingLibDDObj
                                          
                                          (let (
                                                ( NewLibDir ( NameGetLibDirFromDFIIDirAndCellName
                                                            DFIIDir
                                                            NewCellName ) ) )
                                            (if NewLibDir
                                                (if ( isDir NewLibDir )
                                                    ( setq
                                                      ErrorStr
                                                      ( sprintf
                                                        nil
                                                        "Library %L does not exist, but directory %L does."
                                                        NewLibName
                                                        NewLibDir ) )
                                                  (if ( equal
                                                        ( FileUtilMakeDir
                                                          NewLibDir
                                                          t )
                                                        0 )
                                                      (let (
                                                            ( NewLibDDObj ( ddCreateLib
                                                                            NewLibName
                                                                            NewLibDir ) ) )
                                                        (if NewLibDDObj
                                                            (let ()
                                                              ( techBindTechFile NewLibDDObj TechnologyLibraryName )
                                                              NewLibDDObj )
                                                          ( setq
                                                            ErrorStr
                                                            ( sprintf
                                                              nil
                                                              "Unable to create library %L in directory %L."
                                                              NewLibName
                                                              NewLibDir ) ) ) )
                                                    ( setq
                                                      ErrorStr
                                                      ( sprintf
                                                        nil
                                                        "Unable to create directory %L for library %L."
                                                        NewLibDir
                                                        NewLibName ) ) ) )
                                              ( setq
                                                ErrorStr
                                                ( sprintf
                                                  nil
                                                  "Unable to determine what directory library %L should be in."
                                                  NewLibName ) ) ) ) ) ) ) )
                    (unless ErrorStr
                      (let (
                            ( NewCellView ( dbCopyCellView
                                            OldCellView
                                            NewLibName
                                            NewCellName
                                            NewViewName
                                            nil
                                            nil
                                            t ) ) )
                        (if NewCellView
                            ( setarray 
                              Result
                              ( list OldLibName OldCellName OldViewName )
                              NewCellView )
                          ( setq
                            ErrorStr
                            ( sprintf
                              nil
                              "Unable to copy %L %L %L to %L %L %L."
                              OldLibName
                              OldCellName
                              OldViewName
                              NewLibName
                              NewCellName
                              NewViewName ) ) ) ) ) )
                ( setq
                  ErrorStr
                  ( sprintf
                    nil
                    "%L %L %L does not exist."
                    OldLibName
                    OldCellName
                    OldViewName ) ) ) ) ) ) ) )
    (if ErrorStr
        ErrorStr
      Result ) ) )
                                    
            
(defun ImportMungeCellViewsInCellViewTable ( CellViewTable )
  ( foreach
    CellViewTableEntry
    ( tableToList
      CellViewTable )
    (let (
          ( CellViewToMunge ( cadr CellViewTableEntry ) ) )
      ( ImportMungeCellInstances
        CellViewToMunge
        CellViewTable ) ) )
  ( foreach
    CellViewTableEntry
    ( tableToList
      CellViewTable )
    (let (
          ( CellViewToSaveAndClose ( cadr CellViewTableEntry ) ) )
      ( dbSave CellViewToSaveAndClose )
      ( dbPurge CellViewToSaveAndClose ) ) )
  nil )
