; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun NameStartRename ( type from to )
  (let (
        ( Cmd 
          ( sprintf
            nil
            "%s/rename --type=%s --from=%s --to=%s"
            ( PackageGetBinRoot ) type from to) ) )
    (let (
          ( CID
            ( ipcBeginProcess Cmd ) ) )
      (ipcWaitForProcess CID)
      (unless ( ipcIsAliveProcess CID )
        (error "rename failed" ) )
      CID ) ) )

(defun NameDoRename ( CID name )
  (unless
      ( ipcWriteProcess 
        CID
        ( sprintf nil "%s\n" name ) )
    (error "rename failed" ) )
  ( StringUtilReadLine CID 10 )
  )

(defun NameStartCastToCadenceCellName ( )
  (let (
        ( Cmd 
          ( sprintf
            nil
            "%s/rename --type=cell --from=cast --to=cadence"
            ( PackageGetBinRoot ) ) ) )
    (let (
          ( CID
            ( ipcBeginProcess Cmd ) ) )
      (ipcWaitForProcess CID)
      (unless ( ipcIsAliveProcess CID )
        (error "rename failed" ) )
      CID ) ) )

(defun NameDoCastToCadenceCellName ( CID CellName )
  (unless
      ( ipcWriteProcess 
        CID
        ( sprintf nil "%s\n" CellName ) )
    (error "rename failed" ) )
  ( StringUtilReadLine CID 10 )
  )

(defun NameGetViewDirFromDFIIDirAndCellView ( DFIIDir CellView )
  ( NameGetViewDirFromDFIIDirAndCellNameAndView 
    DFIIDir
    ( getq CellView cellName )
    ( getq CellView viewName ) ) )

(defun NameGetViewDirFromDFIIDirAndCellNameAndView ( DFIIDir CellName ViewName )
  ( sprintf nil "%s/%s" 
            ( NameGetCellDirFromDFIIDirAndCellName 
              DFIIDir
              CellName )
             ViewName ) )

(defun NameGetCellDirFromDFIIDirAndCellName ( DFIIDir CellName )
  (let (
        ( LibCellPair ( NameParseCellName CellName ) ) )
    (let (
          ( LibDir ( buildString ( parseString ( car LibCellPair ) "." ) "/" ) )
          ( CellDir ( buildString ( parseString 
                                    ( buildString ( parseString 
                                                    ( cadr LibCellPair ) "." ) "#2e" )
                                    "-" ) "#2d" ) ) )
      ( sprintf nil "%s/%s/%s" DFIIDir LibDir CellDir  ) ) ) )


(defun NameGetLibDirFromDFIIDirAndLibName ( DFIIDir LibName )
  ( rexCompile "\\." )
  ( strcat
    ( PathMakePathAbsolute DFIIDir )
    ( strcat
      "/"
      ( rexReplace LibName "/" 0 ) ) ) )

(defun NameGetLibDirFromDFIIDirAndCellName ( DFIIDir CellName )
  (let (
        ( LibName ( car
                    ( NameParseCellName
                      CellName ) ) ) )
    (when LibName
      ( NameGetLibDirFromDFIIDirAndLibName DFIIDir LibName ) ) ) )
  

(defun NameGetDFIIDirFromCellName ( CellName )
  ( NameGetDFIIDirFromLibName 
    ( car ( NameParseCellName CellName ) ) ) )

(defun NameGetDFIIDirFromLibName ( LibName )
  (let (
        ( LibDirObj ( ddGetObj LibName ) ) )
    (if ( null LibDirObj )
        (error ( sprintf 
                 nil
                 "NameGetDFIIDirFromLibName: LibName %s not valid"
                 LibName ) )
      (let (
            ( Path ( ddGetObjWritePath LibDirObj ) ) )
        (if ( null Path )
            (error "NameGetDFIIDirectoryFromLibName: Lib path not valid" )
          (let (
                ( ParsedLibName ( parseString LibName "." ) )
                ( ParsedPath ( parseString Path "/" ) ) )
            (let ( 
                  ( DFIIPath 
                    ( buildString 
                      ( ListRemoveLastNElements 
                        ParsedPath 
                        ( length ParsedLibName ) ) "/" ) ) )
              (if ( equal ( substring Path 1 1 ) "/" )
                  DFIIPath = ( strcat "/" DFIIPath ))
              if( rexMatchp( "^/mnt/fulcrum/home" DFIIPath) then
                DFIIPath = substring( DFIIPath 13 strlen(DFIIPath)-12))   
              DFIIPath
 ) ) ) ) ) ) )


(defun NameGetCellPathFromCellName ( CellName )
  (let (
        ( LibCellPair ( NameParseCellName CellName ) ) )
    (let (
          ( CellDirObj ( ddGetObj ( car LibCellPair ) ( cadr LibCellPair ) ) ) )
      (if ( null CellDirObj )
          (error "NameGetCellDirectoryFromCellName: CellName not valid" )
        (let (
              ( Path ( ddGetObjWritePath CellDirObj ) ) )
          (if ( null Path )
              (error "NameGetCellDirectoryFromCellName: Cell path not valid" )
            Path ) ) ) ) ) )

(defun NameGetCellPathFromCellNameAndView ( CellName ViewName )
  (let (
        ( LibCellPair ( NameParseCellName CellName ) ) )
    (let (
          ( CellDirObj ( ddGetObj ( car LibCellPair ) ( cadr LibCellPair ) ViewName ) ) )
      (if ( null CellDirObj )
          (error "NameGetCellDirectoryFromCellNameAndView: CellName or ViewName not valid" )
        (let (
              ( Path ( ddGetObjWritePath CellDirObj ) ) )
          (if ( null Path )
              (error "NameGetCellDirectoryFromCellNameAndView: Cell path not valid" )
            Path ) ) ) ) ) )


(defun NameParseCellName ( CellName )
  (let (
        ( Components ( parseString CellName "." ) ) )
    (let (
          ( RComponents ( reverse Components ) ) )
      (let ( 
            ( RLibNameComponents ( cdr ( cdr RComponents ) ) )
            ( LibName nil ) )
        if( rexMatchp("^[a-z_0-9]+$" car(RLibNameComponents))==nil then
           RLibNameComponents=cdr(RLibNameComponents)
        )
        ( foreach
          Component
          ( reverse RLibNameComponents )
          (if LibName
              ( setq LibName ( sprintf nil "%s.%s" LibName Component ) )
            ( setq LibName Component ) ) )
        ( list LibName CellName ) ) ) ) )

(defun NameFilterInstanceTripples ( InstanceTripples LibCellExpressionPairs )
 (let (
        ( FilteredInstanceTripples nil )
        ( PassingInstanceTripples InstanceTripples ) )
    ( foreach
      LibCellExpressionPair
      LibCellExpressionPairs
      (let (
            ( LibExpression ( car LibCellExpressionPair ) )
            ( CellExpression ( cadr LibCellExpressionPair ) )
            ( FilteredByLib nil )
            ( PassedByCurrPair nil ) )
        ( rexCompile LibExpression )
        ( foreach
          InstanceTripple
          PassingInstanceTripples
          (if ( rexExecute ( cadr InstanceTripple ) )
              ( setq FilteredByLib ( cons InstanceTripple FilteredByLib ) )
            ( setq PassedByCurrPair ( cons InstanceTripple PassedByCurrPair ) ) ) )
        ( rexCompile CellExpression )
        ( foreach
          InstanceTripple
          FilteredByLib
          (if ( rexExecute ( caddr InstanceTripple ) )
              ( setq FilteredInstanceTripples ( cons InstanceTripple FilteredInstanceTripples ) )
            ( setq PassedByCurrPair ( cons InstanceTripple PassedByCurrPair ) ) ) )
        ( setq PassingInstanceTripples PassedByCurrPair ) ) )
    ( list PassingInstanceTripples FilteredInstanceTripples ) ) )

(defun NameFilterObjects ( Objects LibCellExpressionPairs LibNameFunc CellNameFunc )

  (let (
        ( FilteredObjects nil )
        ( PassingObjects Objects ) )
    ( foreach
      LibCellExpressionPair
      LibCellExpressionPairs
      (let (
            ( LibExpression ( car LibCellExpressionPair ) )
            ( CellExpression ( cadr LibCellExpressionPair ) )
            ( FilteredByLib nil )
            ( PassedByCurrPair nil ) )
        ( rexCompile LibExpression )
        ( foreach
          Object
          PassingObjects
          (if ( rexExecute ( apply LibNameFunc ( list Object ) ) )
              ( setq FilteredByLib ( cons Object FilteredByLib ) )
            ( setq PassedByCurrPair ( cons Object PassedByCurrPair ) ) ) )
        ( rexCompile CellExpression )
        ( foreach
          Object
          FilteredByLib
          (if ( rexExecute ( apply CellNameFunc ( list Object ) ) )
              ( setq FilteredObjects ( cons Object FilteredObjects ) )
            ( setq PassedByCurrPair ( cons Object PassedByCurrPair ) ) ) )
        ( setq PassingObjects PassedByCurrPair ) ) )
    ( list PassingObjects FilteredObjects ) ) )

(defun NameFilterInstances ( Instances LibCellExpressionPairs )
  ( NameFilterObjects
    Instances
    LibCellExpressionPairs
    (lambda ( Instance )
      ( getq Instance libName ) )
    (lambda ( Instance )
      ( getq Instance cellName ) ) ) )

      
(defun NameFilterInstanceMasters ( InstanceMasters LibCellExpressionPairs )
 ( NameFilterObjects
    InstanceMasters
    LibCellExpressionPairs
    (lambda ( InstanceMaster )
      ( getq InstanceMaster libName ) )
    (lambda ( InstanceMaster )
      ( getq InstanceMaster cellName ) ) ) )


(defun NameTablifyInstancesFullNamesFromFilterResult ( FilterResult
                                                       TargetTable )
  (let (
        ( NonFoldableInstances ( car FilterResult ) )
        ( FoldableInstances ( cadr FilterResult ) ) )
    ( foreach
      Instance
      NonFoldableInstances
      ( setarray
        TargetTable
        ( getq Instance name )
        ( NameCanonicalizeNonFoldableInstanceName
          ( getq Instance name ) ) ) )
    ( foreach
      Instance
      FoldableInstances
      ( setarray
        TargetTable 
        ( getq Instance name )
        ( NameCanonicalizeNonFoldableInstanceName
          ( getq Instance name ) ) ) )
    t ) )

(defun NameTablifyInstancesFullNames ( Instances
                                       TargetTable
                                       FoldableLibCellExpressionPairs )
  (let (
        ( FilterResult ( NameFilterInstances
                         Instances
                         FoldableLibCellExpressionPairs ) ) )
    ( NameTablifyInstancesFullNamesFromFilterResult
      FilterResult
      TargetTable ) ) )


(defun NameTablifyInstancesNamesFromFilterResult ( FilterResult
                                                   TargetTable )
  (let (
        ( NonFoldableInstances ( car FilterResult ) )
        ( FoldableInstances ( cadr FilterResult ) ) )
    ( foreach
      Instance
      NonFoldableInstances
      ( setarray
        TargetTable
        ( getq Instance name )
        ( NameCanonicalizeNonFoldableInstanceName
          ( getq Instance name ) ) ) )
    ( foreach
      Instance
      FoldableInstances
      ( setarray
        TargetTable 
        ( getq Instance name )
        ( NameCanonicalizeFoldableInstanceName
          ( getq Instance name ) ) ) )
    t ) )

(defun NameTablifyInstancesNames ( Instances
                                   TargetTable
                                   FoldableLibCellExpressionPairs )
  (let (
        ( FilterResult ( NameFilterInstances
                         Instances
                         FoldableLibCellExpressionPairs ) ) )
    ( NameTablifyInstancesNamesFromFilterResult
      FilterResult
      TargetTable ) ) )

(defun NameTablifyInstancesFromFilterResult ( FilterResult
                                              TargetTable )
  (let (
        ( NonFoldableInstances ( car FilterResult ) )
        ( FoldableInstances ( cadr FilterResult ) ) )
    ( foreach
      Instance
      NonFoldableInstances
      ( setarray 
        TargetTable
        ( NameCanonicalizeNonFoldableInstanceName ( getq Instance name ) )
        ( list Instance ) ) )
    ( foreach
      Instance
      FoldableInstances
      (let (
            ( CanonicalInstanceName ( NameCanonicalizeFoldableInstanceName ( getq Instance name ) ) ) )
        
        ( setarray
          TargetTable
          CanonicalInstanceName
          ( cons Instance ( arrayref TargetTable CanonicalInstanceName ) ) ) ) )
    t ) )

(defun NameTablifyInstancesTable ( Instances
                                   TargetTable
                                   FoldableLibCellExpressionPairs )
  (let (
        ( FilterResult ( NameFilterInstances
                         Instances
                         FoldableLibCellExpressionPairs ) ) )
    ( NameTablifyInstancesFromFilterResult
      FilterResult
      TargetTable ) ) )


(defun NameTablifyInstancesAndInstancesNames ( Instances
                                               FoldableLibCellExpressionPairs
                                               TargetInstancesTable
                                               TargetInstancesNameTable
                                               TargetInstancesFullNameTable
                                               )
  (let (
        ( FilterResult ( NameFilterInstances
                         Instances
                         FoldableLibCellExpressionPairs ) ) )
    ( NameTablifyInstancesFromFilterResult
      FilterResult
      TargetInstancesTable )
    ( NameTablifyInstancesNamesFromFilterResult
      FilterResult
      TargetInstancesNameTable )
    ( NameTablifyInstancesFullNamesFromFilterResult
      FilterResult
      TargetInstancesFullNameTable )
 ) )


(defun NameMakeInstancesNameTable ( Instances 
                                    FoldableLibCellExpressionPairs )
  (let (
        ( Result ( makeTable "instancesNameTable" nil ) ) )
    ( NameTablifyInstancesNames
      Instances
      Result
      FoldableLibCellExpressionPairs ) 
    Result ) )

(defun NameMakeInstancesTable ( Instances
                                FoldableLibCellExpressionPairs )
  (let (
        ( Result ( makeTable "instancesTable" nil ) ) )
    ( NameTablifyInstances
      Instances
      Result
      FoldableLibCellExpressionPairs )
    Result ) )

(defun NameMakeInstancesFullNameTable ( Instances
                                        FoldableLibCellExpressionPairs )
  (let (
        ( Result ( makeTable "instancesFullNameTable" nil ) ) )
    ( NameTablifyInstancesFullNames
      Instances
      Result
      FoldableLibCellExpressionPairs )
    Result ) )

(defun NameMakeEmptyInstanceMap ()
  ( list
    ( makeTable "instancesTable" nil )
    ( makeTable "instancesNameTable" nil )
    ( makeTable "instancesFullNameTable" nil )
    ) )

(defun NamePopulateInstanceMapWithInstances ( InstanceMap
                                              Instances
                                              FoldableLibCellExpressionPairs )
  ( NameTablifyInstancesAndInstancesNames
    Instances
    FoldableLibCellExpressionPairs
    ( car InstanceMap )
    ( cadr InstanceMap )
    ( caddr InstanceMap )
 ) )

(defun NamePopulateInstanceMapWithCellView ( InstanceMap
                                             CellView
                                             FoldableLibCellExpressionPairs )
  ( NameTablifyInstancesAndInstancesNames
    ( getq CellView instances )
    FoldableLibCellExpressionPairs
    ( car InstanceMap )
    ( cadr InstanceMap )
    ( caddr InstanceMap )
 ) )


(defun NameMakeInstanceMapFromInstances ( Instances
                                          FoldableLibCellExpressionPairs )
  (let (
        ( Ret ( NameMakeEmptyInstanceMap ) ) )
    ( NamePopulateInstanceMapWithInstances
      Ret
      Instances
      FoldableLibCellExpressionPairs )
    Ret ) )

(defun NameMakeInstanceMapFromCellView ( CellView
                                         FoldableLibCellExpressionPairs )
  (let (
        ( Ret ( NameMakeEmptyInstanceMap ) ) )
    ( NamePopulateInstanceMapWithCellView
      Ret
      CellView
      FoldableLibCellExpressionPairs )
    Ret ) )

(defun NameGetInstancesForCanonicalName ( InstanceMap
                                          CanonicalName )
  ( arrayref
    ( car InstanceMap )
    CanonicalName ) )


(defun NameGetCanonicalInstanceNameForInstanceName ( InstanceMap
                                                     InstanceName )
  ( arrayref
    ( cadr InstanceMap )
    InstanceName ) )

(defun NameGetCanonicalInstanceNameForInstance ( InstanceMap
                                                 Instance )
  ( NameGetCanonicalInstanceNameForInstanceName
    InstanceMap
    ( getq Instance name ) ) )

(defun NameGetFullCanonicalInstanceNameForInstanceName ( InstanceMap
                                                         InstanceName )
  ( arrayref
    ( caddr InstanceMap )
    InstanceName ) )

(defun NameGetFullCanonicalInstanceNameForInstance ( InstanceMap
                                                     Instance )
  ( NameGetFullCanonicalInstanceNameForInstanceName
    InstanceMap
    ( getq Instance name ) ) )

(defun NameGetCanonicalInstanceNames ( InstanceMap )
  ( ListApplyFuncToListAndAccumulateResults
    ( tableToList ( car InstanceMap ) )
    (lambda
      ( InstanceEntry )
      ( car InstanceEntry ) )
    nil ) )

(defun NameAddNonFoldableInstanceToInstanceMap ( InstanceMap Instance )
  (let (
        ( InstanceTable ( car InstanceMap ) )
        ( InstanceNameTable ( cadr InstanceMap ) )
        ( InstanceFullNameTable ( caddr InstanceMap ) )
        ( CanonicalInstanceName ( NameCanonicalizeNonFoldableInstanceName
                                  ( getq Instance name ) ) ) )
    ( setarray
      InstanceTable
      CanonicalInstanceName
      ( list Instance ) )
    ( setarray
      InstanceNameTable
      ( getq Instance name )
      CanonicalInstanceName )
    ( setarray
      InstanceFullNameTable
      ( getq Instance name )
      CanonicalInstanceName ) ) )
                                  
(defun NameCanonicalizeNonFoldableInstanceName ( Name )
  (let (   
        ( Result "" ) )
    ( for 
      StrIndex 
                                        ;avoid leading |      
      (if ( equal ( substring Name 1 1 ) "|" )
          2
        1 )
      ( strlen Name )
      (let (
            ( CurrChar ( substring Name StrIndex 1 ) ) )
        ( setq
          Result
          ( strcat
            Result
            (cond
             (
                                        ;convert ( to [
              ( equal CurrChar "(" )
              "[" )
             (
              ( equal CurrChar ")" )
              "]" )
             (
              t
              CurrChar ) ) ) ) ) )
    Result ) )

(defun NameCanonicalizeFoldableInstanceName ( Name )

  (let (
        ( Components ( reverse ( parseString Name "." ) ) ) )
    (let (
          ( RootName ( buildString ( reverse (if ( rexMatchp "^[0-9]*$" ( car Components ) )
                                                 ( cdr Components )
                                               Components ) )
                                   "." ) ) )
      ( NameCanonicalizeNonFoldableInstanceName RootName ) ) ) )







(defun NameTablifyInstances ( CellView )
  (let (
        ( TargetTable ( makeTable `instances nil ) ) )
    ( foreach Instance ( getq CellView instances )
              (let (
                    ( CanonName ( NameCanonicalizeInstanceName ( getq Instance name ) ) ) )
                ( setarray TargetTable 
                           CanonName
                           ( cons Instance ( arrayref TargetTable CanonName ) ) ) ) )
    TargetTable ) )


(defun NameGetLayoutInstancesFromNetListInstanceName ( InstanceMapping InstanceName )
  ( NameGetInstancesFromInstanceName ( car InstanceMapping ) InstanceName ) )

(defun NameGetNetListInstancesFromLayoutInstanceName ( InstanceMapping InstanceName )
  ( NameGetInstancesFromInstanceName ( cadr InstanceMapping ) InstanceName ) )

(defun NameGetInstancesFromInstanceName ( CanonTable InstanceName )
  ( arrayref CanonTable ( NameCanonicalizeInstanceName InstanceName ) ) )

(defun NameCreateCellInstanceMapping ( ConnectivityCellView LayoutCellView )
  (let (
        ( LayoutTable ( NameTablifyInstances LayoutCellView ) )
        ( ConnectivityTable ( NameTablifyInstances ConnectivityCellView ) ) )
    ( list LayoutTable ConnectivityTable ) ) )
