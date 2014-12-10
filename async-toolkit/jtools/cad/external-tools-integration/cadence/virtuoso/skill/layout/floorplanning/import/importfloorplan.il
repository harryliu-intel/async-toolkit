; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun ImportFloorPlanTablifyInstances ( Instances TargetTable )
  ( foreach
    Instance
    Instances
    ( setarray
      TargetTable
      ( MungeInstanceName ( getq Instance name ) )
      Instance ) ) )
(defun ImportFloorPlanTablifyNets ( CellView TargetTable )
  ( foreach
    Net
    ( getq CellView nets )
    ( setarray TargetTable 
               ( getq Net name ) 
               Net ) ) )
(defun ImportFloorPlanTablifyTerminals ( CellView TargetTable )
  ( foreach
    Term
    ( getq CellView terminals )
    ( setarray TargetTable 
               ( getq Term name ) 
               Term ) ) )

(defun ImportFloorPlanTablifyInstanceTerms ( Instance TargetTable )
  ( foreach
    InstTerm
    ( getq Instance conns )
    ( setarray TargetTable 
               ( getq ( getq InstTerm term ) name ) 
               InstTerm
               ) ) )

(defun ImportFloorPlanUpdateCellInstances ( ConnectivityCell
                                            ConnectivityInstancesTable
                                            ConnectivityCellInstances
                                            NewCell
                                            NewCellInstancesTable
                                            NewCellInstances
                                            NewBoundaryLPP
                                            DefCellWidth 
                                            DefCellHeight 
                                            OldCell
                                            ScalingFactor )
  ( dbComputeBBox NewCell )
  (let (
        ( OldInstances (when OldCell ( getq OldCell instances ) ) )
        ( OldInstancesTable ( makeTable "baz" nil ) )
        ( ErrorStr nil )
        ( NewCellBBox ( getq NewCell bBox ) ) )
    ( ImportFloorPlanTablifyInstances OldInstances OldInstancesTable )
    (let (
          ( InstancesToDelete ( setof
                                NewInstance
                                NewCellInstances
                                ( null
                                  ( arrayref ConnectivityInstancesTable ( getq NewInstance name ) ) ) ) )
          ( InstancesToCreate ( setof
                                ConnectivityInstance
                                ConnectivityCellInstances
                                ( null
                                  ( arrayref NewCellInstancesTable ( getq ConnectivityInstance name ) ) ) ) )
          ( CurrX ( RectGetRight NewCellBBox ) ) )
      ( foreach
        InstanceToDelete
        InstancesToDelete
        (let ()
          ( setarray NewCellInstanceTable ( getq InstanceToDelete name ) nil )
          ( dbDeleteObject
            InstanceToDelete ) ) )

      ( foreach
        InstanceToCreate
        InstancesToCreate
        (unless ErrorStr
          (let (
                ( NewMasterCell ( dbOpenCellViewByType
                                  ( getq ( getq InstanceToCreate master ) lib )
                                  ( getq InstanceToCreate cellName )
                                  ( getq NewCell viewName ) nil "r" ) ) 
                ( OldInstance ( arrayref OldInstancesTable ( getq InstanceToCreate name ) ) ) )
            (if NewMasterCell
                (if OldInstance
                    (let (
                          ( ScaledXPos ( times ( car ( getq OldInstance xy ) ) ScalingFactor ) )
                          ( ScaledYPos ( times ( car ( cdr ( getq OldInstance xy ) ) ) ScalingFactor ) ) )
                      (let (
                            ( NewInstance ( dbCreateInst
                                            NewCell
                                            NewMasterCell
                                            ( getq InstanceToCreate name )
                                            ( list ScaledXPos ScaledYPos )
                                            ( getq OldInstance orient ) ) ) )
                        ( setarray NewCellInstancesTable ( getq InstanceToCreate name ) NewInstance ) ) )
                  (let (
                        ( NewInstance ( dbCreateInst
                                        NewCell
                                        NewMasterCell
                                        ( getq InstanceToCreate name )
                                        ( list 0 0 )
                                        "R0" ) ) )
                    ( setarray NewCellInstancesTable ( getq InstanceToCreate name ) NewInstance )
                    ( dbMoveFig
                      NewInstance
                      nil
                      ( list
                        ( list
                          ( difference
                            CurrX
                            ( RectGetLeft
                              ( getq NewInstance bBox ) ) )
                          0 ) 
                        "R0" ) )
                    ( setq CurrX ( plus CurrX ( RectGetWidth ( getq NewInstance bBox ) ) ) ) ) )
              ( setq 
                ErrorStr 
                ( sprintf 
                  nil 
                  ( strcat
                    "Unable to find \"%s\" \"%s\" \"%s\""
                    " to create instance \"%s\" in \"%s\" \"%s\" \"%s\"."
                    )
                  ( getq InstanceToCreate libName )
                  ( getq InstanceToCreate cellName )
                  ( getq NewCell viewName )
                  ( getq InstanceToCreate name )
                  ( getq NewCell libName )
                  ( getq NewCell cellName )
                  ( getq NewCell viewName ) ) ) ) ) ) ) )
    (when ( and
            ( null ErrorStr )
            ( null ( getq NewCell instances ) )
            ( null ( getq NewCell shapes ) ) )
      (if OldCell
          (let (
                ( OldCellBBox ( getq OldCell bBox ) ) )
            ( dbCreateRect 
              NewCell
              NewBoundaryLPP
              ( list
                ( list
                  ( times ScalingFactor ( RectGetLeft OldCellBBox ) )
                  ( times ScalingFactor ( RectGetBottom OldCellBBox ) ) )
                ( list
                  ( times ScalingFactor ( RectGetRight OldCellBBox ) )
                  ( times ScalingFactor ( RectGetTop OldCellBBox ) ) ) ) ) )
        ( dbCreateRect
          NewCell
          NewBoundaryLPP
          ( list
            ( list 0 0 )
            ( list DefCellWidth DefCellHeight ) ) ) ) 
      )
    ErrorStr ) )




(defun ImportFloorPlanCompareConnections ( InstTerm1 InstTerm2 )  
  ( and
    InstTerm1
    InstTerm2
    ( equal 
      ( getq ( getq InstTerm1 net ) name )
      ( getq ( getq InstTerm2 net ) name ) )
    ( equal
      ( getq ( getq InstTerm1 term ) name )
      ( getq ( getq InstTerm2 term ) name ) ) ) )
      

(defun ImportFloorPlanUpdateConnectivity ( ConnectivityCell
                                           ConnectivityCellInstances
                                           NewCell
                                           NewCellInstancesTable )
  (let (
        ( ErrorStr nil ) )
    ( foreach
      ConnectivityInstance
      ConnectivityCellInstances
      (unless ErrorStr
        (let (
              ( NewInstance ( arrayref NewCellInstancesTable ( getq ConnectivityInstance name ) ) ) )
          (if ( not ( null NewInstance ) )
              (let (
                    ( ConnectivityInstanceConnectionsTable ( makeTable "bar" nil ) )
                    ( NewInstanceConnectionsTable ( makeTable "foo" nil ) ) )
                ( ImportFloorPlanTablifyInstanceTerms 
                  ConnectivityInstance 
                  ConnectivityInstanceConnectionsTable )
                ( ImportFloorPlanTablifyInstanceTerms 
                  NewInstance 
                  NewInstanceConnectionsTable )
                (let (
                      ( ConnectionsToDelete ( setof
                                              NewInstanceConnection
                                              ( getq NewInstance conns )
                                              ( not
                                                ( ImportFloorPlanCompareConnections
                                                  NewInstanceConnection
                                                  ( arrayref 
                                                    ConnectivityInstanceConnectionsTable
                                                    ( getq
                                                      ( getq NewInstanceConnection term )
                                                      name ) ) ) ) ) )
                                                
                      ( ConnectionsToCreate ( setof
                                              ConnectivityInstanceConnection
                                              ( getq ConnectivityInstance conns )
                                              ( not
                                                ( ImportFloorPlanCompareConnections
                                                  ConnectivityInstanceConnection
                                                  ( arrayref 
                                                    NewInstanceConnectionsTable
                                                    ( getq
                                                      ( getq ConnectivityInstanceConnection term )
                                                      name ) ) ) ) ) ) )
                  
                  ( foreach
                    ConnectionToDelete
                    ConnectionsToDelete
                    ( dbDeleteObject ConnectionToDelete ) )

                  ( foreach
                    ConnectionToCreate
                    ConnectionsToCreate
                    ( dbCreateConnByName
                      ( dbMakeNet NewCell ( getq ( getq ConnectionToCreate net ) name ) )
                      NewInstance
                      ( getq ( getq ConnectionToCreate term ) name )
                      ) ) ) )
            (let ()
              ( setq ErrorStr ( sprintf 
                                nil
                                "Unable to find instance \"%s\" in \"%s\" \"%s\" \"%s"
                                ( getq ConnectivityInstance name )
                                ( getq NewCell libName )
                                ( getq NewCell cellName )
                                ( getq NewCell viewName )
                                 ) )
              ( printf "\n%L\n" NewInstance )
              ( foreach
                Key
                NewCellInstancesTable
                ( printf "%L %L\n" Key ( arrayref NewCellInstancesTable Key ) ) )
              ) ) ) ) )
      
    (unless ErrorStr
      (let (
            ( ConnectivityNetsTable ( makeTable "foo" nil ) )
            ( NewNetsTable ( makeTable "bar" nil ) )
            )
        ( ImportFloorPlanTablifyNets ConnectivityCell ConnectivityNetsTable )
        ( ImportFloorPlanTablifyNets NewCell NewNetsTable )
        
        (let (
              ( NetsToDelete ( setof
                               NewNet
                               ( getq NewCell nets )
                               ( null
                                 ( arrayref ConnectivityNetsTable ( getq NewNet name ) ) ) ) )
              ( NetsToCreate ( setof
                               ConnectivityNet
                               ( getq ConnectivityCell nets )
                               ( null
                                 ( arrayref NewNetsTable ( getq ConnectivityNet name ) ) ) ) ) )
          ( foreach
            NetToDelete
            NetsToDelete
            ( dbDeleteObject NetToDelete )
            )
          ( foreach
            NetToCreate
            NetsToCreate
            ( dbCreateNet NewCell ( getq NetToCreate name ) ) ) ) ) )
    (unless ErrorStr
      (let (
            ( ConnectivityTerminalsTable ( makeTable "bar" nil ) )
            ( NewTerminalsTable ( makeTable "foo" nil ) ) )
        ( ImportFloorPlanTablifyTerminals
          ConnectivityCell
          ConnectivityTerminalsTable ) 
        ( ImportFloorPlanTablifyTerminals
          NewCell
          NewTerminalsTable )

        (let (
              ( TerminalsToDelete ( setof
                                    NewTerminal
                                    ( getq NewCell terminals )
                                    ( null
                                      ( arrayref
                                        ConnectivityTerminalsTable
                                        ( getq NewTerminal name ) ) ) ) )
              ( TerminalsToCreate ( setof
                                    ConnectivityTerminal
                                    ( getq ConnectivityCell terminals )
                                    ( null
                                      ( arrayref
                                        NewTerminalsTable
                                        ( getq ConnectivityTerminal name ) ) ) ) )
              )
          ( foreach
            TerminalToDelete
            TerminalsToDelete
            ( dbDeleteObject TerminalToDelete ) )

          ( foreach
            TerminalToCreate
            TerminalsToCreate
            ( dbCreateTerm
              ( dbMakeNet NewCell ( getq ( getq TerminalToCreate net ) name ) )
              ( getq TerminalToCreate name )
              ( getq TerminalToCreate direction ) ) ) ) ) )
    ErrorStr
    ) )

(defun ImportFloorPlanUpdateCell ( ConnectivityCell
                                   LibsToIgnore
                                   CellsToIgnore
                                   NewCell
                                   NewBoundaryLPP
                                   DefCellWidth 
                                   DefCellHeight 
                                   OldCell
                                   ScalingFactor )
  (let (
        ( ErrorStr nil )
        ( NewCellInstances ( UpdateNetlistFilterInstances
                             NewCell
                             LibsToIgnore 
                             CellsToIgnore ) )
        ( NewCellInstanceTable ( makeTable "foo" nil ) )
        ( ConnectivityInstances ( UpdateNetlistFilterInstances
                                  ConnectivityCell
                                  LibsToIgnore 
                                  CellsToIgnore ) )
        ( ConnectivityInstancesTable  ( makeTable "bar" nil ) ) )

    ( ImportFloorPlanTablifyInstances NewCellInstances NewCellInstanceTable )
    ( ImportFloorPlanTablifyInstances ConnectivityInstances ConnectivityInstancesTable )
    (let (
          ( ShouldDoCell ( or
                           ( equal ( getq NewCell mode ) "a" )
                           ( equal ( getq NewCell mode ) "w" )
                           ( dbReopen NewCell "a" ) ) ) )
      (if ShouldDoCell
          (let ()
            (unless ErrorStr
              ( setq 
                ErrorStr 
                ( ImportFloorPlanUpdateCellInstances
                  ConnectivityCell
                  ConnectivityInstancesTable
                  ConnectivityInstances
                  NewCell
                  NewCellInstanceTable
                  NewCellInstances
                  NewBoundaryLPP
                  DefCellWidth 
                  DefCellHeight 
                  OldCell
                  ScalingFactor ) ) )
            (unless ErrorStr
              ( setq
                ErrorStr
                ( ImportFloorPlanUpdateConnectivity
                  ConnectivityCell
                  ConnectivityInstances
                  NewCell
                  NewCellInstanceTable ) ) )
            (unless ErrorStr
              ( dbComputeBBox NewCell )
              ( dbSave NewCell )
              ( dbPurge NewCell ) ) )
        ( printf 
          "Could not open %s %s %s for writing, skipping.\n" 
          ( getq NewCell libName )
          ( getq NewCell cellName )
          ( getq NewCell viewName ) ) ) )
    ErrorStr ) )

(defun ImportFloorPlan ( RootConnectivityCellView 
                         LibsToIgnore
                         CellsToIgnore
                         ResultViewName 
                         ResultBoundaryLPP
                         DefCellWidth
                         DefCellHeight
                         CellMappingTable
                         ScalingFactor )
  (let (
        ( BottomUpListOfCells ( HierarchyGetAllCellsInTree
                                RootConnectivityCellView
                                LibsToIgnore ) )
        ( ErrorStr nil ) )
    ( foreach
      ConnectivityCell
      BottomUpListOfCells
      (unless ErrorStr
        (let (
              ( OldCell ( arrayref CellMappingTable ( getq ConnectivityCell cellName ) ) )
              ( NewCellDDObj ( ddGetObj
                               ( getq ConnectivityCell libName )
                               ( getq ConnectivityCell cellName )
                               ResultViewName
                               "pc.db"
                               nil
                               "r" ) ) )
          (if ( or ( null NewCellDDObj ) ( ddIsObjReadable NewCellDDObj ) )
              (when ( or ( null NewCellDDObj ) ( ddIsObjWritable NewCellDDObj ) )
                (let (
                      ( NewCell ( dbOpenCellViewByType
                                  ( getq ConnectivityCell lib )
                                  ( getq ConnectivityCell cellName )
                                  ResultViewName
                                  "maskLayout"
                                  (if NewCellDDObj
                                      "a"
                                    "w" ) ) ) )
                  ( setq ErrorStr
                         ( ImportFloorPlanUpdateCell
                           ConnectivityCell
                           LibsToIgnore
                           CellsToIgnore
                           NewCell
                           ResultBoundaryLPP
                           DefCellWidth
                           DefCellHeight
                           OldCell
                           ScalingFactor ) ) ) )
            ( setq 
              ErrorStr 
              ( sprintf 
                nil 
                "\"%s\" \"%s\" \"%s\" exists but is not readable."
                ( getq ConnectivityCell lib )
                ( getq ConnectivityCell cellName )
                ResultViewName ) ) ) ) ) )
    ErrorStr ) )


(defun ImportFloorPlanWrapper ( RootConnectivityLibName
                                RootConnectivityCellName
                                RootConnectivityViewName
                                LibsToIgnore
                                CellsToIgnore
                                ResultViewName 
                                ResultBoundaryLPP
                                DefCellWidth
                                DefCellHeight
                                RootReferenceLibName
                                RootReferenceCellName
                                RootReferenceViewName
                                ScalingFactor )
  (let (
        ( RootConnectivityCellView ( dbOpenCellViewByType
                                     RootConnectivityLibName
                                     RootConnectivityCellName
                                     RootConnectivityViewName
                                     nil
                                     "r" ) )
        ( RootReferenceCellView (when ( and
                                        RootReferenceLibName
                                        RootReferenceCellName
                                        RootReferenceViewName
                                        )
                                  ( dbOpenCellViewByType
                                    RootReferenceLibName
                                    RootReferenceCellName
                                    RootReferenceViewName
                                    nil
                                    "r" ) ) )
        ( MappingTable ( makeTable "foo" nil ) ) )
    ( GetCellNameMappings
      RootConnectivityCellView
      LibsToIgnore
      RootReferenceCellView
      MappingTable )
    ( ImportFloorPlan
      RootConnectivityCellView
      LibsToIgnore
      CellsToIgnore
      ResultViewName
      ResultBoundaryLPP
      DefCellWidth
      DefCellHeight
      MappingTable
      ScalingFactor ) ) )


;( ImportFloorPlanWrapper "chip.tc3.alu" "chip.tc3.alu.ALU.0" "netlist" ( list "tsmc13lg" "gate" ) ( list ) "foo-floorplan" ( list "prBoundary" "drawing" ) 38.4 38.4 nil nil nil 1 )
