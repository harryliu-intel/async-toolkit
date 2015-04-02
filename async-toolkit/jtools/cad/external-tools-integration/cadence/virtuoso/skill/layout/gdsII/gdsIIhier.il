; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
<pre>
 This file contains skill code for preparing layout for export to GDSII or for converted layout imported from GDSII to normal layout cells.

 When Exporting layout, all cell names, node name, and instance names
 are changed based on mappings stored in skill name tables.  There is
 one skill name table per cell to be converted.  All the skill name tables
 are in the same directory.  Each skill name table is named cellname.names.il.
 Each skill name table file defines three tables:
    CadenceNodeToGDSIINodeTable
       maps node name in source layout to node name in exported layout.
    CadenceCellToGDSIICellTable
       maps cell name in source layout to cell name in exported layout.
    CadenceInstanceToGDSIIInstanceTable
       maps instance name in source layout to instance name in exported layout.
 During export each cell to be exported is copied to a temporary library.  The cell's
 name in the temporary library will be it's export name.
 The copy of the cell will then be opened and all instances will have their masters
 redefined to be the copy of their master in the temporary library.
The copy of the cell will also have all of its labels texts and instance
 names changed based on what is in the CadenceNodeToGDSIINodeTable and 
 CadenceInstanceToGDSIIInstanceTable tables.
 Importing is the reverse of exporting.  skill name table files
are used as in export, but the keys in all the tables are names from
 the gdsII and the values are names to use in the dfII layout.
</pre>
*/

(defun GDSIIHierUnRodObjects ( CellView )
  (let (
        ( Shapes ( getq CellView shapes ) ) )
    ( foreach
      RodObj
      ( rodGetNamedShapes CellView )
      ( rodUnAlign RodObj )
      ( rodUnNameShape RodObj ) ) )
  (let (
        ( Instances ( getq CellView instances ) ) )
    ( foreach
      Instance
      Instances
      (let (
            ( RodObj ( rodGetObj Instance ) ) )
        (when RodObj
          ( rodUnAlign RodObj )
          (when ( getq RodObj align )
            ( rodUnNameShape RodObj ) ) ) ) ) ) )

(defun GDSIIHierParseCellName ( CellName )
  (let (
        ( Components ( parseString CellName "." ) ) )
    (let (
          ( RComponents ( reverse Components ) ) )
      (let ( 
            ( RLibNameComponents ( cdr ( cdr RComponents ) ) )
            ( LibName nil ) )
        ( foreach
          Component
          ( reverse RLibNameComponents )
          (if LibName
              ( setq LibName ( sprintf nil "%s.%s" LibName Component ) )
            ( setq LibName Component ) ) )
        ( list LibName CellName ) ) ) ) )


(defun GDSIIHierMakeNameGDSIISafe ( CellName )
  (let (
        ( NumChars ( strlen CellName ) )
        ( CurrIndex 1 ) 
        ( Result "" ) 
        ( bracketNumber 0 )
        CurrChar
        ConvertedChar
       )
    bracketNumber= 0
    (while ( and 
             Result
             ( not ( lessp NumChars CurrIndex ) ) )
        CurrChar=substring( CellName CurrIndex 1 ) 
        ConvertedChar= (cond
                               (
                                ( or 
                                  ( StringUtilIsCharLetter CurrChar )
                                  ( StringUtilIsCharDigit CurrChar ) )
                                CurrChar )
                               ( ( equal "$" CurrChar ) "_24_" )
                               ( ( equal "." CurrChar ) "_D_" )
                               ( ( equal "_" CurrChar ) if( bracketNumber==0 "_U_" "_C_") )
                               ( ( equal "-" CurrChar ) 
                                     CurrChar=substring( CellName CurrIndex 2 ) 
                                     CurrIndex=CurrIndex+1
                                     if( CurrChar=="-L" then         
                                       bracketNumber=bracketNumber+1
                                       CurrChar=if( bracketNumber==1 "_L_" "_7b_")
                                     else
                                       bracketNumber=bracketNumber-1
                                       CurrChar=if( bracketNumber==0 "_R_" "_7d_")
                                     )
                                     CurrChar
                               )
                               ( ( equal "(" CurrChar ) "_L_" )
                               ( ( equal ")" CurrChar ) "_R_" )
                               ( ( equal "{" CurrChar ) "_7b_" )
                               ( ( equal "}" CurrChar ) "_7d_" )
                               ( ( equal "," CurrChar ) "_C_" )
                               ( t nil ) 
                       )  
          (if ConvertedChar
              Result= strcat( Result ConvertedChar ) 
              Result= nil ) 
        CurrIndex= CurrIndex + 1 
    ) 
    Result ) )
         
(defun GDSIIHierGetTableFileForCellView ( CellView TableDirectory )
  ( GDSIIHierGetTableFileForCellName ( getq CellView cellName ) TableDirectory ) )

(defun GDSIIHierGetTableFileForCellName ( CellName TableDirectory )
  ( sprintf nil "%s/%s.names.il" TableDirectory CellName ) )

(defun GDSIIHierGetPartialExtractTableFileForCellName ( CellName TableDirectory )
  ( sprintf nil "%s/%s.partial.il" TableDirectory CellName ) )

(defun GDSIIHierGetPartialExtractTableFileForCellView ( CellView TableDirectory )
  ( GDSIIHierGetPartialExtractTableFileForCellName ( getq CellView cellName ) TableDirectory ) )

(defun GDSIIHierGetLVSNodesTableFileForCellName ( CellName TableDirectory )
  ( sprintf nil "%s/%s.lvsNodes.il" TableDirectory CellName ) )

(defun GDSIIHierGetLVSNodesTableFileForCellView ( CellView TableDirectory )
  ( GDSIIHierGetLVSNodesTableFileForCellName ( getq CellView cellName ) TableDirectory ) )

(defun GDSIIHierGetGDSIIImportMungedCellView ( CadenceCellName
                                               ViewName )
  (let (
        ( CadenceLibName ( car ( NameParseCellName CadenceCellName ) ) ) )
    (when CadenceLibName
      ( dbOpenCellViewByType
        CadenceLibName
        CadenceCellName
        ViewName
        nil
        "r" ) ) ) )

(defun GDSIIHierGetGDSIIExportMungedCellView ( GDSIICellName
                                               ViewName
                                               GDSIIExportLib )
  ( dbOpenCellViewByType
    GDSIIExportLib
    GDSIICellName
    ViewName
    nil
    "r" ) )
      
                                         

(defun GDSIIHierMungeCellInstances ( CellViewToMunge
                                     CellTable
                                     InstanceTable
                                     GetMungedCellViewFunc
                                     GetMungedCellViewFuncParams
                                     LibCellExpressionPairsToIgnore )
  (let (
        ( ErrorStr nil ) )
    ( foreach
      Instance
      ( UpdateNetlistFilterInstances
        CellViewToMunge
        LibCellExpressionPairsToIgnore )
      (unless ErrorStr
        (let (
              ( CurrMasterName ( getq Instance cellName ) ) )
          (let (
                ( MungedMasterNameFromTable ( arrayref
                                              CellTable
                                              CurrMasterName ) ) )
            ;don't munge globals instances
            (let ( 
                  ( MungedMasterName 
                    (if ( equal 
                          ( getq Instance libName )
                          "globals" )
                        CurrMasterName
                      (if MungedMasterNameFromTable
                          MungedMasterNameFromTable
                        ( GDSIIHierMakeNameGDSIISafe
                          CurrMasterName ) ) ) ) )
              (when ( and
                      MungedMasterName
                      ( not ( equal MungedMasterName CurrMasterName ) ) )
                (let (
                      ( MungedMasterCellView
                        ( apply
                          GetMungedCellViewFunc
                          ( cons
                            MungedMasterName
                            ( cons
                              ( getq Instance viewName )
                              GetMungedCellViewFuncParams ) ) ) ) )
                  (if MungedMasterCellView
                      ( dbSetq Instance MungedMasterCellView master )
                    ( setq
                      ErrorStr
                      ( sprintf
                        nil
                        "Unable to get cell %L for instance %L in cell %L"
                        MungedMasterName
                        ( getq Instance name )
                        ( getq CellViewToMunge cellName ) ) ) ) ) ) ) ) ) ) )


    ( foreach
      Instance
      ( getq CellViewToMunge instances )
      (unless ErrorStr
        (let (
              ( CurrInstanceName ( NameCanonicalizeNonFoldableInstanceName ( getq Instance name ) ) ) )
          (let (
                ( MungedInstanceNameFromTable ( arrayref
                                                InstanceTable
                                                CurrInstanceName ) ) )
            (let ( 
                  ( MungedInstanceName (if MungedInstanceNameFromTable
                                           MungedInstanceNameFromTable
                                         (if ( equal
                                                 ( StringUtilGetFirstChar CurrInstanceName )
                                                 "|" )
                                             ( arrayref
                                               InstanceTable
                                               ( StringUtilGetAllButFirstChar
                                                 CurrInstanceName ) )
                                           ( GDSIIHierMakeNameGDSIISafe
                                             CurrInstanceName ) ) ) ) )
              (unless ( or
                        ( not MungedInstanceName )
                        ( equal MungedInstanceName CurrInstanceName ) )
                ( dbSetq Instance MungedInstanceName name ) ) ) ) ) ) )

    ErrorStr ) )


(defun GDSIIHierMungeCellLabels ( CellViewToMunge NodeTable )
  (let (
        ( TextDisplaysAndLabels ( setof
                                  Shape
                                  ( getq CellViewToMunge shapes )
                                  ( or
                                    ( equal ( getq Shape objType ) "label" )
                                    ( equal ( getq Shape objType ) "textDisplay" ) ) ) ) )
    ( foreach
      TextOrLabel
      TextDisplaysAndLabels
      (if ( equal "label" ( getq TextOrLabel objType ) )
          (let (
                ( LabelObj TextOrLabel ) )
            (let (
                  ( NodeName ( getq LabelObj theLabel ) ) )
              (let (
                    ( MungedNameFromTable ( arrayref NodeTable NodeName ) ) )
                (if MungedNameFromTable
                    (unless ( equal NodeName MungedNameFromTable )
                      ( dbSetq LabelObj MungedNameFromTable theLabel ) )
                  ( dbDeleteObject LabelObj ) ) ) ) )
        (let (
              ( TextObj TextOrLabel ) )
          (let (
                ( AssociateObj ( getq TextObj associate ) )
                ( AssociateField ( stringToSymbol ( getq TextObj text ) ) )
                ( LayerPP ( getq TextObj lpp ) )
                ( Location ( getq TextObj xy ) )
                ( Orientation ( getq TextObj orient ) )
                ( Font ( getq TextObj font ) )
                ( Height ( getq TextObj height ) )
                ( Justification ( getq TextObj justify ) ) )
            (let (
                  ( NodeName ( get AssociateObj AssociateField ) ) )
              (let (
                    ( MungedNameFromTable ( arrayref NodeTable NodeName ) ) )
                ( dbDeleteObject TextObj )
                (when MungedNameFromTable
                  (unless ( equal NodeName MungedNameFromTable )
                    ( dbCreateLabel
                      CellViewToMunge
                      LayerPP
                      Location
                      MungedNameFromTable
                      Justification
                      Orientation
                      Font
                      Height ) ) ) ) ) ) ) ) ) )
  nil )

(defun GDSIIHierDeleteInstanceTerms ( CellViewToMunge )
  ( foreach
    Instance
    ( getq CellViewToMunge instances )
    ( foreach
      InstTerm
      ( getq Instance conns )
      ( dbDeleteObject InstTerm ) ) ) )


(defun GDSIIHierFindNet ( CellViewToMunge NetName )
  ( car
    ( setof
      Net
      ( getq CellViewToMunge nets )
      ( equal
        ( getq Net name )
        NetName ) ) ) )

(defun GDSIIHierMungeCellConnectivity ( CellViewToMunge NodeTable )
  ( GDSIIHierDeleteInstanceTerms CellViewToMunge )
  (let (
        ( NetsTable ( makeTable "netstable" nil ) ) )
    (let (
          ( ErrorStr nil )
          ( NetsToDeleteAndRename ( ListApplyFuncToListAndAccumulateResult
                                    ( getq CellViewToMunge nets )
                                    (lambda
                                      ( Net CurrResult NetsTable NodeTable )
                                      (let (
                                            ( NewNetName ( arrayref NodeTable ( getq Net name ) ) ) )
                                        (if NewNetName
                                            (let ()
                                              (if ( equal NewNetName ( getq Net name ) )
                                                  CurrResult
                                                (let (
                                                      ( NewResult
                                                        ( list
                                                          ( car CurrResult )
                                                          ( cons
                                                            ( list Net NewNetName )
                                                            ( cadr CurrResult ) ) ) ) )
                                                  ( setarray NetsTable ( getq Net name ) Net )
                                                  NewResult ) ) )
                                          ( list
                                            ( cons
                                              Net
                                              ( car CurrResult ) )
                                            ( cadr CurrResult ) ) ) ) )
                                    ( list NetsTable NodeTable )
                                    ( list nil nil ) ) ) )
      
      ( foreach
        Net
        ( car NetsToDeleteAndRename )
        (let (
              ( NetTerm ( getq Net term ) ) )
          (when NetTerm
            ( dbDeleteObject NetTerm ) )
          ( dbDeleteObject Net ) ) )
      
      ( foreach
        NetRename
        ( cadr NetsToDeleteAndRename )
        (let (
              ( Net ( car NetRename ) )
              ( NewNetName ( cadr NetRename ) ) )
          (if NewNetName
              (let (
                    ( ExistingNet ( arrayref NetsTable NewNetName ) ) )
                (if ExistingNet
                    ( setq
                      ErrorStr
                      ( sprintf
                        nil
                        "Unable to rename \"%s\" to \"%s\" because net \"%s\" aleady existed.\n"
                        ( getq Net name )
                        NewNetName
                        NewNetName ) )
                  (let (
                        ( ExistingTerm ( getq Net term ) )
                        ( ExistingPins ( getq Net pins ) )
                        ( NewNet ( dbCreateNet CellViewToMunge NewNetName ) ) )
                    ( setarray NetsTable NewNetName NewNet )
                    (when ExistingTerm
                      ( dbCreateTerm
                        NewNet
                        NewNetName
                        ( getq ExistingTerm direction ) ) )
                    
                    ( foreach
                      ExistingPin
                      ExistingPins
                      (let (
                            ( ExistingShape ( getq ExistingPin fig ) ) )
                        ( dbDeleteObject ExistingPin )
                        ( dbCreatePin NewNet ExistingShape ) ) )
                    (let (
                          ( NetTerm ( getq Net term ) ) )
                      (when NetTerm
                        ( dbDeleteObject NetTerm ) ) )
                    ( setarray NetsTable ( getq Net name ) nil )
                    ( dbDeleteObject Net ) ) ) )
            ( setq
              ErrorStr
              ( sprintf
                nil
                "Unable to get new net name for \"%s\".\n"
                ( getq Net name ) ) ) ) ) )
      ErrorStr ) ) )
    
            

(defun GDSIIHierMungeCellCopy ( CellViewToMunge
                                NodeTable
                                CellTable
                                InstanceTable
                                GetMungedCellViewFunc
                                GetMungedCellViewFuncParams
                                LibCellExpressionPairsToIgnore )
  (let (
        ( ErrorStr nil ) )
    ( GDSIIHierUnRodObjects CellViewToMunge )

    (unless ErrorStr
      ( printf "Munging Instances for %L\n" ( getq CellViewToMunge cellName ) )
      ( setq ErrorStr ( GDSIIHierMungeCellInstances 
                        CellViewToMunge 
                        CellTable
                        InstanceTable
                        GetMungedCellViewFunc
                        GetMungedCellViewFuncParams
                        LibCellExpressionPairsToIgnore ) ) )
    (unless ErrorStr
      ( printf "Munging labels for %L\n" ( getq CellViewToMunge cellName ) )
      ( setq ErrorStr ( GDSIIHierMungeCellLabels CellViewToMunge NodeTable ) ) )
    (unless ErrorStr
      ( printf "Munging Connectivity for %L\n" ( getq CellViewToMunge cellName ) )
      ( setq ErrorStr ( GDSIIHierMungeCellConnectivity CellViewToMunge NodeTable ) ) )
    (when ErrorStr
      ( printf "ERROR: %L\n" ErrorStr ) )
    ErrorStr ) )
                      

(defun GDSIIHierGetMungedCellNameForCellName ( TableDirectory
                                               CellName )
  (let (
        ( TableFile ( GDSIIHierGetTableFileForCellName CellName TableDirectory ) ) )
    (when ( isReadable TableFile )
      ( load TableFile )
      ( arrayref CadenceCellToGDSIICellTable CellName ) ) ) )

(defun GDSIIHierFixInstancesForPartialExtract ( CellView
                                                SubTypeTable
                                                InstancesToDelete
                                              )
  (let (
        ( ErrorStr nil ) )
    ( foreach
      Instance
      ( getq CellView instances )
      (when ( and
              ( equal ( getq Instance objType ) "inst" )
              ( null ErrorStr ) 
              ( equal ( getq Instance libName ) ( getq CellView libName ) )
              ( null ( getq ( getq Instance master ) superMaster ) )
              (let (
                    ( MasterCellView ( getq Instance master ) ) )
                (let (
                      ( IncludeInPartialExtractProp ( dbSearchPropByName
                                                      MasterCellView
                                                      "includeInPartialExtract" ) ) )
                  (if IncludeInPartialExtractProp
                      ( not ( equal ( getq IncludeInPartialExtractProp value ) "TRUE" ) )
                    t ) ) ) )

        (when ( equal ( getq Instance name ) "t" )
          ( printf "Hello, the instance name is \"t\"!!!!\n" ) )
        (let (
              ( NewMasterCellName ( arrayref SubTypeTable ( getq Instance name ) ) ) )
          (if NewMasterCellName
              (let (
                    ( NewMasterCellViewDDObj ( ddGetObj 
                                               ( getq CellView libName ) 
                                               NewMasterCellName 
                                               ( getq Instance viewName ) ) ) )
                (when ( equal ( getq Instance name ) "t" )
                  ( printf 
                    "%L %L %L %L\n"
                    ( getq CellView cellName )
                    ( getq Instance name )
                    ( getq Instance cellName )
                    NewMasterCellName ) )
                (if ( and
                      NewMasterCellViewDDObj
                      ( getq NewMasterCellViewDDObj files ) )
                    (let (
                          ( NewMasterCellView ( dbOpenCellViewByType
                                                ( getq CellView libName ) 
                                                NewMasterCellName 
                                                ( getq Instance viewName )
                                                nil
                                                "r" ) ) )
                      (when ( equal ( getq Instance name ) "t" )
                        ( printf 
                          "%L %L %L %L %L %L\n"
                          ( getq CellView cellName )
                          ( getq Instance name )
                          ( getq Instance cellName )
                          NewMasterCellName
                          NewMasterCellView
                          (when NewMasterCellView
                            ( getq NewMasterCellView cellName ) ) ) )
                      (if NewMasterCellView
                          ( dbSetq Instance NewMasterCellView master )
                        ( setq
                          ErrorStr
                          ( sprintf
                            nil
                            "Unable to open cell view for %L %L for instance %L in %L %L."
                            NewMasterCellName 
                            ( getq Instance viewName )
                            ( getq Instance name )
                            ( getq CellView cellName )
                            ( getq CellView viewName ) ) ) ) )
                  ( setq
                    ErrorStr
                    ( sprintf
                      nil
                      "cell view for %L %L for instance %L in %L %L does not exist."
                      NewMasterCellName 
                      ( getq Instance viewName )
                      ( getq Instance name )
                      ( getq CellView cellName )
                      ( getq CellView viewName ) ) ) ) )
            (let ()
              ( printf "Deleting instance %L\n" ( getq Instance name ) )
              ;don't delete now because wew may have to complete the nets
              ;( dbDeleteObject Instance )
              ( setarray InstancesToDelete ( getq Instance name ) t )
              ) ) ) ) )
    ErrorStr
 ) )

(defun GDSIIHierDoLVSNodes ( CellView
                             LVSNodesLabelsTable
                             LVSNodesTable )
  (let (
        ( FinishedLabels ( makeTable "labelTable" nil ) )
        ( LVSNodes ( LVSNodesGetLVSNodes LVSNodesTable ) ) )
    
    ( foreach
      Instance
      ( getq CellView instances )
      (let (
            ( LVSNodesInstance ( LVSNodesGetLVSNodesInstance
                                 LVSNodesTable
                                 ( getq Instance name ) ) ) )
        (when LVSNodesInstance
          ( foreach
            LVSNodePair
            ( LVSNodesGetLVSNodesInstanceConnectionPairs
              LVSNodesInstance )
            (let (
                  ( LVSNode ( car LVSNodePair ) )
                  ( LVSNodeInInstance ( cadr LVSNodePair ) ) )
              (let (
                    ( LabelObjInInstance ( arrayref
                                           ( arrayref
                                             LVSNodesLabelsTable
                                             ( getq Instance cellName ) )
                                           LVSNodeInInstance ) ) )
                (when LabelObjInInstance
                  (let (
                        ( LabelPosInInstance ( getq LabelObjInInstance xy ) )
                        ( LabelLayerInInstance ( getq LabelObjInInstance lpp ) )
                        ( LabelOrientationInInstance ( getq LabelObjInInstance orient ) )
                        ( LabelFontInInstance ( getq LabelObjInInstance font ) )
                        ( LabelHeightInInstance ( getq LabelObjInInstance height ) )
                        ( LabelJustificationInInstance ( getq LabelObjInInstance justify ) ) )
                    ( setarray
                      FinishedLabels
                      LVSNode
                      ( dbCreateLabel
                        CellView
                        LabelLayerInInstance
                        ( dbTransformPoint
                          LabelPosInInstance
                          ( getq
                            Instance
                            transform ) )
                        LVSNode
                        LabelJustificationInInstance
                        LabelOrientationInInstance
                        LabelFontInInstance
                        LabelHeightInInstance )
                      ) ) ) ) ) ) ) ) )

    ( ListFindElementsInList
      ( getq CellView shapes )
      LVSNodes
      (lambda
        ( Shape LVSNode LabelTable )
        (when ( equal
                ( getq Shape objType )
                "label" )
          (when ( equal
                  ( getq Shape theLabel )
                  LVSNode )
            (if ( arrayref
                  LabelTable
                  LVSNode )
                t
              (let ()
                ( setarray
                  LabelTable
                  LVSNode
                  Shape )
                t ) ) ) ) )
      ( list FinishedLabels ) )
              
    ( setarray
      LVSNodesLabelsTable
      ( getq CellView cellName )
      FinishedLabels ) )
  nil )
      
;Copies a hierarchy of cells rooted by RootCellView to a temporary directory
;and changes all the cell names, instance names, and label texts as dictacted
;by the contents of the skill name table files in TableDirectory.
(defun GDSIIHierCopyAndMungeCellsForExport ( TableDirectory 
                                             GDSIILibraryName
                                             RootCellView
                                             LibCellExpressionPairsToIgnore
                                             GateLibName
                                             StackLibName
                                             AbstractFunc
                                             AbstractArgs
                                             NetNamesToAvoid
                                             DeletedInstanceLPP
                                           )
  (let (
        ( LVSNodesLabelsTable ( makeTable "labelTablesTable" nil ) )
        ; Get bottoms up list of cells to export.
        ( CellViewsToCopy ( HierarchyGetAllCellsInTree
                            RootCellView
                            LibCellExpressionPairsToIgnore ) )
        ( FilesInTableDirectory ( getDirFiles TableDirectory ) )
        ( TargetLibraryDDObj ( ddGetObj GDSIILibraryName ) )
        ( TopLevelCellView nil )
        ( CellsToCompleteBrokenNets ( makeTable `foo nil ) )
        ( ErrorStr nil ) )
    
    (when ( and FilesInTableDirectory TargetLibraryDDObj CellViewsToCopy )
      (unless ErrorStr
        ( foreach
          CellViewToCopy
          CellViewsToCopy
          (unless ErrorStr
            (let (
                  ; Get the skill name table file for the current cell.
                  ( TableFile ( GDSIIHierGetTableFileForCellView 
                                CellViewToCopy 
                                TableDirectory ) ) 
                  ( CellName ( getq CellViewToCopy cellName ) ) )
              (if ( isReadable TableFile )
                  (let ()
                    ( printf "Loading %L\n" TableFile ) 
                    ( load TableFile ) )
                (let ()
                  ( printf "Unable to get name file %L for %L\n" TableFile CellName )
                  ( defvar CadenceNodeToGDSIINodeTable ( makeTable "foo" nil ) )
                  ( defvar CadenceCellToGDSIICellTable ( makeTable "bar" nil ) )
                  ( defvar CadenceInstanceToGDSIIInstanceTable ( makeTable "baz" nil ) )
                  ( setarray 
                    CadenceCellToGDSIICellTable 
                    CellName
                    ( GDSIIHierMakeNameGDSIISafe CellName ) ) ) )
              (let (
                    ( GDSIICellName
                      ( arrayref 
                        CadenceCellToGDSIICellTable 
                        ( getq CellViewToCopy cellName ) ) ) )
                (if GDSIICellName
                    ;Copy the current cell to its new name in the
                    ;temporary library.
                    (let (
                          ( CellViewCopy ( betterCopyCellView 
                                           CellViewToCopy
                                           GDSIILibraryName
                                           GDSIICellName
                                           ( getq CellViewToCopy viewName )
                                           nil
                                           nil
                                           t ) ) )
                      (if CellViewCopy
                          (let ()
                            ( dbReplaceProp
                              CellViewCopy
                              "includeInPartialExtract"
                              "boolean"
                              ( not ( isReadable TableFile ) ) )
                            (when ( and
                                    ( equal
                                      ( getq CellViewToCopy libName )
                                      ( getq RootCellView libName ) )
                                    ( equal
                                      ( getq CellViewToCopy cellName )
                                      ( getq RootCellView cellName ) )
                                    ( equal
                                      ( getq CellViewToCopy viewName )
                                      ( getq RootCellView viewName ) ) )
                              ( setq TopLevelCellView CellViewCopy ) )
                            
                            ;don't munge if it's only got globals or other weird cells
                            (when ( or
                                    ( null
                                      ( getq CellViewCopy instanceMasters ) )
                                    ( exists
                                      Master
                                      ( getq CellViewCopy instanceMasters )
                                      (let ()
                                        (unless Master
                                          ( setq
                                            ErrorStr 
                                            ( sprintf
                                              nil
                                              "An instance is invalid in %L %L %L (look for flashing white boxes!)"
                                              ( getq CellViewToCopy libName )
                                              ( getq CellViewToCopy cellName )
                                              ( getq CellViewToCopy viewName ) ) ) )
                                        ( and 
                                          ( null ErrorStr )
                                          ;( not ( equal ( getq Master libName ) "globals" ) )
                                          ( car ( NameParseCellName 
                                                  ( getq Master cellName ) ) ) ) ) ) )

                            ;Munge the instances to point at cells in the temporary library,
                            ;rename instance names, and rename node names.
                              ( setq ErrorStr ( GDSIIHierMungeCellCopy 
                                                CellViewCopy
                                                CadenceNodeToGDSIINodeTable
                                                CadenceCellToGDSIICellTable
                                                CadenceInstanceToGDSIIInstanceTable
                                                (lambda
                                                  ( GDSIICellName
                                                    ViewName
                                                    GDSIIExportLib )
                                                  ( GDSIIHierGetGDSIIExportMungedCellView
                                                    GDSIICellName
                                                    ViewName
                                                    GDSIIExportLib ) )
                                                ( list GDSIILibraryName )
                                                LibCellExpressionPairsToIgnore ) ) )

                            ;assura3.1.2 renders lvs-nodes unnecessary
                            (unless ( or t ErrorStr )
                              (let (
                                    ( LVSNodesTableFile 
                                      ( GDSIIHierGetLVSNodesTableFileForCellView
                                        CellViewToCopy
                                        TableDirectory ) ) )


                                (when ( isReadable LVSNodesTableFile )
                                  ( printf "Loading %L\n" LVSNodesTableFile )
                                  ( load LVSNodesTableFile )
                                  ( setq
                                    ErrorStr
                                    ( GDSIIHierDoLVSNodes
                                      CellViewCopy
                                      LVSNodesLabelsTable
                                      GDSIILVSNodesTable ) ) ) ) )

                            (unless ErrorStr
                              ( dbSave CellViewCopy )
                              (let (
                                    ( PartialExtractTableFile ( GDSIIHierGetPartialExtractTableFileForCellView
                                                                CellViewToCopy
                                                                TableDirectory ) ) )
                                (when ( isReadable PartialExtractTableFile )
                                  ( printf "Loading %L\n" PartialExtractTableFile )
                                  ( load PartialExtractTableFile )
                                  ( foreach
                                    SubTypeName
                                    PartialExtractTable
                                    (let (
                                          ( SubTypeCellView ( betterCopyCellView
                                                              CellViewCopy
                                                              GDSIILibraryName
                                                              SubTypeName
                                                              ( getq CellViewCopy viewName )
                                                              nil
                                                              nil
                                                              t ) )
                                                              
                                          ( InstancesToDelete ( makeTable `foo nil ) )
                                          ( SubTypeTable ( arrayref PartialExtractTable SubTypeName ) ) )
                                      (let ( 
                                            ( ErrorStr
                                              ( GDSIIHierFixInstancesForPartialExtract
                                                SubTypeCellView
                                                SubTypeTable
                                                InstancesToDelete
                                                ) ) )

                                        ;cell is 'broken' so draw labels on top level
                                        ;and later run assura to complete the nets
                                        (when ( tableToList InstancesToDelete )
                                          ( setarray
                                            CellsToCompleteBrokenNets
                                            ( getq SubTypeCellView cellName )
                                            InstancesToDelete ) )
                                        
                                      (unless ErrorStr
                                        ( dbSave SubTypeCellView )
                                        (when ( and
                                                ( equal
                                                  ( getq CellViewToCopy libName )
                                                  ( getq RootCellView libName ) )
                                                ( equal
                                                  ( getq CellViewToCopy cellName )
                                                ( getq RootCellView cellName ) )
                                                ( equal
                                                  ( getq CellViewToCopy viewName )
                                                  ( getq RootCellView viewName ) ) )
                                          ( setq TopLevelCellView SubTypeCellView ) ) ) ) ) )

                                  ( defvar PartialExtractTable ( makeTable "partialExtractTable" nil ) ) ) ) ) )
                                              
                        ( setq 
                          ErrorStr
                          ( sprintf
                            nil
                            "Could not copy %L %L %L to %L %L %L" 
                            ( getq CellViewToCopy libName )
                            ( getq CellViewToCopy cellName )
                            ( getq CellViewToCopy viewName )
                            GDSIILibraryName 
                            GDSIICellName
                            ( getq CellViewToCopy viewName ) ) ) ) )
                  ( setq
                    ErrorStr
                    ( sprintf
                      nil
                      "Could not get GDSII name for %L %L %L"
                      ( getq CellViewToCopy libName )
                      ( getq CellViewToCopy cellName )
                      ( getq CellViewToCopy viewName ) ) ) ) ) ) ) ) )

    ;complete potentially broken nets by drawing labels for all ports of broken subtypes,
    ; and using assura to gather all shapes attached to these labels
      (unless ErrorStr
        (when ( and t ( tableToList CellsToCompleteBrokenNets ) )
        (let (
              ( CopyOfTopLevelCellView
                ( betterCopyCellView
                  TopLevelCellView
                  TempLibName
                  ( getq TopLevelCellView cellName )
                  "pre-abstract" ) ) )
          (let (
                ( LabelFuncFactory
                  ( GDSIIHierGetLabelFuncFactory 
                  CopyOfTopLevelCellView 
                  CellsToCompleteBrokenNets
                  NetNamesToAvoid
                  ) ) )

          ( foreach
            SubTypeName
            CellsToCompleteBrokenNets
            (let (
                  ( InstanceNamesToDelete
                    ( arrayref CellsToCompleteBrokenNets SubTypeName ) )
                  ( SubTypeCellView
                    (cond (
                           ( equal
                             SubTypeName
                             ( getq TopLevelCellView cellName ) )
                           CopyOfTopLevelCellView )
                          (
                           ( dbOpenCellViewByType
                             GDSIILibraryName                      
                             SubTypeName
                             ( getq TopLevelCellView viewName )
                             nil
                             "a" ) ) ) ) )
              
              ( foreach 
                InstanceNameToDelete
                InstanceNamesToDelete
                
                ( dbCreateRect
                  SubTypeCellView
                  DeletedInstanceLPP
                  ( getq
                    ( dbFindAnyInstByName
                      SubTypeCellView
                      InstanceNameToDelete )
                    bBox ) ) )
                  
              ( dbSave SubTypeCellView )
              ) )

          CP =    CellsToCompleteBrokenNets
          LFF = LabelFuncFactory

        ( ConductorPropogateLabelAndTerminals
          CopyOfTopLevelCellView
          LabelFuncFactory
          1024
          LibCellExpressionPairsToIgnore
         )

        ( dbSave
          CopyOfTopLevelCellView )

        (when t
        (let (
              ( Abstract 
                ( apply AbstractFunc 
                        ( cons CopyOfTopLevelCellView AbstractArgs ) ) ) )
        ( dbSave
          Abstract )

          ;copy non-pin, non-label shapes and instances(vias)
          ( foreach
            Fig
            ( append
              ( setof
                Shape
                ( getq Abstract shapes )
                ( and
                  ( not
                    ( equal ( getq Shape objType ) "label" ) )
                  ( not ( getq Shape pin ) ) ) )
              ( getq Abstract instances ) )
            ( dbCopyFig
              Fig
              TopLevelCellView ) ) ) )
        ) ) )
    
      ( dbSave TopLevelCellView )
      )
    (unless ErrorStr
    ;delete instances
    ( foreach
      SubTypeName
      CellsToCompleteBrokenNets
      (let (
            ( InstanceNamesToDelete
              ( arrayref CellsToCompleteBrokenNets SubTypeName ) )
            ( SubTypeCellView
              ( dbOpenCellViewByType
                GDSIILibraryName
                SubTypeName
                ( getq TopLevelCellView viewName )
                nil
                "a" ) ) )

        ( foreach 
          InstanceNameToDelete
          InstanceNamesToDelete

          ( dbDeleteObject 
            ( dbFindAnyInstByName
              SubTypeCellView
              InstanceNameToDelete ) ) )
        ( dbSave SubTypeCellView )
        ) ) )
    )
    (if ErrorStr
        ErrorStr
      TopLevelCellView ) ) )


(defun GDSIIHierGetLabelFuncFactory ( TopLevelCellView 
                                      CellsToCompleteBrokenNets
                                      NetNamesToAvoid
                                       )
  ( eval
    ( ExpressionReplaceSymbolsWithValues
      `(lambda ( CellName 
                 InstanceName )
         (let (
               ( InstancesToDelete
                 ( arrayref
                   CellsToCompleteBrokenNets
                   CellName ) ) )
           (let (
                 ( CellNetNamesToAvoid `NetNamesToAvoid )
                 ( Purpose
                   (cond (
                          ( and 
                            ( tablep InstancesToDelete )
                            ( arrayref 
                              InstancesToDelete
                              InstanceName ) )
                         "drawing" )
                         ( "net" ) ) ) )
             ( eval
               ( ExpressionReplaceSymbolsWithValues
                 `(lambda ( NetName 
                            Position 
                            LPP )
                   ( dbCreateLabel
                     TopLevelCellView
                     ( list 
                       ( car ( PinUtilGetValidLayer LPP  ) )
                       Purpose
                       )
                     Position
                     ;pruposely have different labels on NetNamesToAvoid
                     ;so they aren't retrieved by geomGetNet( "XXX*" )

                     (cond (
                            ( exists NetNameToAvoid `CellNetNamesToAvoid
                                     ( equal NetNameToAvoid NetName ) )
                            ( strcat (cond ( CellName ) ( "ZZZ" ) ) "." NetName ) )
                           (
                            "XXX" ) )
                     "centerCenter"
                     "R0"
                     "stick"
                     0.1  ) )
                 ( list `Purpose `CellName `CellNetNamesToAvoid  )
                 ) ) ) ) )
      ( list `CellsToCompleteBrokenNets
             `TopLevelCellView 
             `NetNamesToAvoid )
      ) ) )


;Munges and streams out a cell hierarchy rooted by RootCellView.
;Needs StreamOutScript to actually do the streaming.
;The StreamOutScript is given five arguments:
;  1. Library containing cell to stream.
;  2. Cell to stream
;  3. View of cell to stream.
;  4. User units per database unit
;  5. Name of the user unit.

; See top of this file for top level description of export process.

(defun GDSIIHierCopyAndMungeAndStreamCellViewForExport ( GDSIIDataDir
                                                         TempLibPrefix
                                                         TempLibDir
                                                         RootCellView
                                                         RootCellOutputName
                                                         LibCellExpressionPairsToIgnore
                                                         GateLibName
                                                         StackLibName
                                                         AbstractFunc
                                                         AbstractArgs
                                                         NetNamesToAvoid
                                                         DeletedInstanceLPP
                                                         StreamOutScript
                                                         )
  (let (
        ( TempLibName ( LibCreateTempLibrary
                        ( techGetTechFile RootCellView )
                        TempLibPrefix
                        TempLibDir ) ) 
        ( CellName ( getq RootCellView cellName ) )
        ( ViewName ( getq RootCellView viewName ) ) 
        ( UserUnitsPerDBU ( quotient 1.0 ( getq RootCellView DBUPerUU ) ) ) 
        ( UserUnits ( getq RootCellView userUnits ) ) )
    (let (
          ; Copy all the cells and change all the names.
          ( TopLevelCellViewForExport ( GDSIIHierCopyAndMungeCellsForExport
                                        GDSIIDataDir
                                        TempLibName
                                        RootCellView
                                        LibCellExpressionPairsToIgnore
                                        GateLibName
                                        StackLibName
                                        AbstractFunc
                                        AbstractArgs
                                        NetNamesToAvoid
                                        DeletedInstanceLPP
                                        ) ) )
      (if ( stringp TopLevelCellViewForExport )
          TopLevelCellViewForExport
        (let (
              ( StreamOutCmd ( sprintf
                               nil
                               "%s \"%s\" \"%s\" \"%s\" %f \"%s\""
                               StreamOutScript
                               TempLibName
                               RootCellOutputName
                               ViewName
                               UserUnitsPerDBU
                               UserUnits ) ) )
          (unless ( equal
                    RootCellOutputName
                    ( getq TopLevelCellViewForExport cellName ) )
            (let (
                  ( CopyOfRoot
                    ( betterCopyCellView
                      TopLevelCellViewForExport
                      TempLibName
                      RootCellOutputName
                      ViewName
                      nil
                      nil
                      t ) ) )
              ( dbSave CopyOfRoot )
              ( dbPurge CopyOfRoot ) ) )
          
          (let (
                ( ExitStatus 
                  ( system StreamOutCmd )
                  )
                )
            ;( ddDeleteObj ( ddGetObj TempLibName ) )
            (unless ( equal ExitStatus 0 )
              "ERROR: pipo failed" 
              ) ) ) )
       ) ) )

;Wrapper for GDSIIHierCopyAndMungeAndStreamCellViewForExport that
;takes a CellTripple ( A list containg library name, cell name, and view name )
;instead of an open cell view object.
(defun GDSIIHierCopyAndMungeAndStreamCellTrippleForExport ( GDSIIDataDir
                                                            TempLibPrefix
                                                            TempLibDir
                                                            RootLibName
                                                            RootCellName
                                                            RootViewName
                                                            RootCellOutputName
                                                            LibCellExpressionPairsToIgnore
                                                            GateLibName
                                                            StackLibName
                                                            AbstractFunc
                                                            AbstractArgs
                                                            NetNamesToAvoid
                                                            DeletedInstanceLPP
                                                            StreamOutScript )
  (let (
        ( CellViewDDObj ( ddGetObj RootLibName RootCellName RootViewName "pc.db" ) ) )
    (if ( and
          CellViewDDObj
          ( ddIsObjReadable CellViewDDObj ) )
        (let (
              ( RootCellView ( dbOpenCellViewByType
                               RootLibName 
                               RootCellName 
                               RootViewName
                               nil
                               "r" ) ) )
          
          ( GDSIIHierCopyAndMungeAndStreamCellViewForExport
            GDSIIDataDir
            TempLibPrefix
            TempLibDir
            RootCellView
            (if ( null RootCellOutputName )
                (let (                
                      ( TableFile ( GDSIIHierGetTableFileForCellView 
                                    RootCellView
                                    GDSIIDataDir ) ) )
                  ( load TableFile )
                  ( arrayref 
                    CadenceCellToGDSIICellTable 
                    ( getq RootCellView cellName ) ) )
              RootCellOutputName )
            LibCellExpressionPairsToIgnore
            GateLibName
            StackLibName
            AbstractFunc
            AbstractArgs
            NetNamesToAvoid
            DeletedInstanceLPP
            StreamOutScript ) )
      (let (
            ( ErrorStr
              ( sprintf 
                nil
                "ERROR: %L %L %L does not exist."
                RootLibName
                RootCellName
                RootViewName ) ) )
        ErrorStr
      ) ) ) )


; Wrapper for GDSIIHierCopyAndMungeAndStreamCellTrippleForExport that passes
; in options from PDK.
(defun GDSIIHierCopyAndMungeAndStreamCellTrippleForExportWithPDKInfo ( GDSIIDataDir
                                                                       TempLibPrefix
                                                                       TempLibDir
                                                                       RootLibName
                                                                       RootCellName
                                                                       RootViewName
                                                                       RootCellOutputName
                                                                       StreamOutScript 
                                                                       @key
                                                                       ( AssuraRunLog "/dev/null" ) )


  Ret = ( GDSIIHierCopyAndMungeAndStreamCellTrippleForExport
    GDSIIDataDir
    TempLibPrefix
    TempLibDir
    RootLibName
    RootCellName
    RootViewName
    RootCellOutputName
    ( append
      TransistorLibCellPairs
      ( append 
        ContactLibCellExpressionPairs
        ( append 
          GateLibCellPairRegExs
          StackLibCellPairRegExs ) ) )
    GateLibraryName
    StackLibraryName
    `GDSIIHierCreateNetCompletionView
    ( list
      ( sprintf
        nil
        "%s/share/Fulcrum/cell_automation/netCompletion.rul"
        FulcrumPDKRoot )
      ViaLayerFormat
      MetalLayerFormat
      MetalLPPs
      ViaLPPs
      TempLibDir
      AssuraRunLog
     )
    ( list GNDNetName VddNetName )
    ( list "y0" "drawing" )
    StreamOutScript )
  ( printf "Ret = %L\n" Ret )
  Ret 
  
 )


(defun GDSIIHierCreateNetCompletionView ( CellView
                                          RuleFile
                                          ViaLayerFormat
                                          MetalLayerFormat
                                          MetalLPPs
                                          ViaLPPs
                                          WorkingDir
                                          AssuraRunLog )
  (let (
        ( TempLibName
          ( LibCreateTempLibraryFromCellView CellView "ABSTRACT" WorkingDir ) )
        ( AssuraLayerMappings
          ( append
            ( ListNonDestructiveMapCan
              (lambda ( LPP )
                (let (
                      ( Num 0 ) )
                  ( sscanf ( car LPP ) MetalLayerFormat Num )
                  ( list
                    ( list ( sprintf nil "m%dComplete" Num )
                             LPP ) ) ) )
                MetalLPPs )
              ( ListNonDestructiveMapCan
                (lambda ( LPP )
                  (let (
                        ( Num 1 )
                        )
                    ( sscanf ( car LPP ) ViaLayerFormat Num  )
                    ( list
                      ( list ( sprintf nil "via%dComplete"
                                       Num
                                       )
                             LPP ) ) ) )
                ViaLPPs ) )
            ) )
    (let (
          ( ErrorStr
            ( AssuraRunAssuraLayerProcessor
              CellView
              TempLibName
              CellView->cellName
              "netCompletion"
              RuleFile
              WorkingDir
              AssuraLayerMappings
              nil
              ?AssuraRunLog AssuraRunLog
              ) ) )
      (if ErrorStr
          ErrorStr
        ( dbOpenCellViewByType
          TempLibName
          CellView->cellName
          "netCompletion"
          nil
          "a" ) ) ) ) )


(defun GDSIIHierMakeLibrary ( NewLibName DFIIDir TechnologyLibraryName )
  (let (
        ( ExistingLibDDObj ( ddGetObj NewLibName ) ) )
    (if ExistingLibDDObj
        ExistingLibDDObj
      
      (let (
            ( NewLibDir ( NameGetLibDirFromDFIIDirAndLibName
                          DFIIDir
                          NewLibName ) ) )
        (if NewLibDir
            (if ( isDir NewLibDir ) then
                (let (
                        ( NewLibDDObj ( ddCreateLib
                                        NewLibName
                                        NewLibDir ) ) )
                    (if NewLibDDObj
                        (let ()
                          ( techBindTechFile NewLibDDObj TechnologyLibraryName )
                          NewLibDDObj )
                      ( sprintf
                        nil
                        "Unable to create library %L in directory %L."
                        NewLibName
                        NewLibDir ) ) )
                ( sprintf
                  nil
                  "Library %L does not exist, but directory %L does."
                  NewLibName
                  NewLibDir )
                
             else
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
                      ( sprintf
                        nil
                        "Unable to create library %L in directory %L."
                        NewLibName
                        NewLibDir ) ) )
                ( sprintf
                  nil
                  "Unable to create directory %L for library %L."
                  NewLibDir
                  NewLibName ) ) )
          ( sprintf
            nil
            "Unable to determine what directory library %L should be in."
            NewLibName ) ) ) ) ) )


(defun GDSIIHierCopyAndMungeCellsForImport ( TableDirectory 
                                             DFIIDir
                                             LibCellViewTripplesToImport
                                             TechnologyLibraryName 
                                             BackStopCellNameMappingsTable )
  (let (
        ( CellViewsToCopy ( HierarchyGetCellDescendantsOfLibCellViewTripples
                            LibCellViewTripplesToImport
                            ( list )
                            (lambda
                              ( CellView )
                              nil )
                            nil ) )
        ( FilesInTableDirectory ( getDirFiles TableDirectory ) )
        ( ErrorStr nil ) )
    (when ( and FilesInTableDirectory CellViewsToCopy )
      (unless ErrorStr
        ( foreach
          CellViewToCopy
          CellViewsToCopy
          (unless ErrorStr
            (let (
                  ( TableFile ( GDSIIHierGetTableFileForCellView 
                                CellViewToCopy 
                                TableDirectory ) ) 
                  ( CellName ( getq CellViewToCopy cellName ) ) )
              (if ( isReadable TableFile )
                  (let ()
                    ( printf "Loading %L\n" TableFile ) 
                    ( load TableFile ) )
                (let ()
                  ( printf "Unable to get name file %L for %L\n" TableFile CellName )
                  ( defvar CadenceNodeToGDSIINodeTable ( makeTable "foo" nil ) )
                  ( defvar CadenceCellToGDSIICellTable ( makeTable "bar" nil ) )
                  ( defvar CadenceInstanceToGDSIIInstanceTable ( makeTable "baz" nil ) )
                  ( setarray 
                    CadenceCellToGDSIICellTable 
                    CellName
                    ( arrayref BackStopCellNameMappingsTable CellName ) ) ) )
              (let (
                    ( CadenceCellName
                      ( arrayref 
                        CadenceCellToGDSIICellTable 
                        ( getq CellViewToCopy cellName ) ) ) )
                (if CadenceCellName
                    (let (
                          ( CadenceLibName ( car ( NameParseCellName CadenceCellName ) ) ) )
                      (let (
                            ( CadenceLibDDObj ( GDSIIHierMakeLibrary
                                                CadenceLibName
                                                DFIIDir
                                                TechnologyLibraryName ) ) )
                        (if ( stringp CadenceLibDDObj )
                            ( setq ErrorStr CadenceLibDDObj )
                          (let (
                                ( CellViewCopy ( betterCopyCellView 
                                                 CellViewToCopy
                                                 CadenceLibName
                                                 CadenceCellName
                                                 ( getq CellViewToCopy viewName )
                                                 nil
                                                 nil
                                                 t ) ) )
                            (if CellViewCopy
                                (let (
                                      ( TempCellTable ( makeTable "cellTable" nil ) ) )
                                  ( foreach
                                    BackstopEntry
                                    ( tableToList BackStopCellNameMappingsTable )
                                    ( setarray TempCellTable ( car BackstopEntry ) ( cadr BackstopEntry ) ) )
                                  ( foreach
                                    Entry
                                    ( tableToList CadenceCellToGDSIICellTable )
                                    ( setarray TempCellTable ( car Entry ) ( cadr Entry ) ) )
                                  ( setq ErrorStr ( GDSIIHierMungeCellCopy 
                                                    CellViewCopy
                                                    CadenceNodeToGDSIINodeTable
                                                    TempCellTable
                                                    CadenceInstanceToGDSIIInstanceTable
                                                    (lambda
                                                      ( CadenceCellName
                                                        ViewName )
                                                      ( GDSIIHierGetGDSIIImportMungedCellView
                                                        CadenceCellName
                                                        ViewName ) )
                                                    nil
                                                    ( list ) ) )
                                  (unless ErrorStr
                                    ( dbSave CellViewCopy )
                                    ( dbPurge CellViewCopy ) ) )
                              ( setq 
                                ErrorStr
                                ( sprintf
                                  nil
                                  "Could not copy %L %L %L to %L %L %L" 
                                  ( getq CellViewToCopy libName )
                                  ( getq CellViewToCopy cellName )
                                  ( getq CellViewToCopy viewName )
                                  CadenceLibraryName 
                                  CadenceCellName
                                  ( getq CellViewToCopy viewName ) ) ) ) ) ) ) )
                  ( setq
                    ErrorStr
                    ( sprintf
                      nil
                      "Could not get Cadence name for %L %L %L"
                      ( getq CellViewToCopy libName )
                      ( getq CellViewToCopy cellName )
                      ( getq CellViewToCopy viewName ) ) ) ) ) ) ) ) ) )
    ErrorStr ) )

(defun GDSIIHierCopyAndMungeLibraryForImport ( GDSIIDataDir 
                                               DFIIDir
                                               LibName
                                               TechnologyLibraryName
                                               BackStopCellNameMappings )
  (let (
        ( BackStopCellNameMappingsTable
          ( makeTable "Cell Name Mappings" nil ) )
        ( LibDDObj ( ddGetObj LibName ) ) )
    ( foreach
      Mapping
      BackStopCellNameMappings
      ( setarray
        BackStopCellNameMappingsTable
        ( car Mapping )
        ( cadr Mapping ) ) )
    (when LibDDObj
      (let (
            ( LibCellViewTripples
              ( ListApplyFuncToListAndAccumulateResult
                ( getq LibDDObj cells )
                (lambda
                  ( CellDDObj CurrResult )
                  ( ListApplyFuncToListAndAccumulateResult
                    ( getq CellDDObj views )
                    (lambda ( ViewDDObj CurrResult )
                      ( cons
                        ( list
                          ( getq ( getq ( getq ViewDDObj cell ) lib ) name )
                          ( getq ( getq ViewDDObj cell ) name )
                          ( getq ViewDDObj name ) )
                        CurrResult ) )
                    nil
                    CurrResult ) )
                nil
                nil ) ) )
        ( GDSIIHierCopyAndMungeCellsForImport
          GDSIIDataDir 
          DFIIDir
          LibCellViewTripples
          TechnologyLibraryName
          BackStopCellNameMappingsTable ) ) ) ) )

(defun GDSIIHierCopyAndMungeLibraryForImportUsingPDKInfo ( GDSIIDataDir
                                                           DFIIDir
                                                           LibName
                                                           BackStopCellNameMappings )
  ( GDSIIHierCopyAndMungeLibraryForImport
    GDSIIDataDir
    DFIIDir
    LibName
    TechLibName
    BackStopCellNameMappings ) )
                         

; ( GDSIIHierCopyAndMungeCells "~/foo" "GDSIIOut" ( geGetWindowCellView ) ( list "tsmc13lg" "cross_vias" ) ( list ) "gate" )
;( GDSIIHierCopyAndMungeCells "~/cds/tc3/names" "GDSIIOut" ( geGetWindowCellView ) ( list "tsmc13lg" "cross_vias" ) ( list ) "gate" )


; ( ListRemoveDuplicatesFromStringList ( ListApplyFuncToListAndAccumulateResults ( HierarchyGetAllCellsInTree ( geGetWindowCellView ) ( list ) ) ( lambda ( CellView ) ( getq CellView libName ) ) nil ) )
