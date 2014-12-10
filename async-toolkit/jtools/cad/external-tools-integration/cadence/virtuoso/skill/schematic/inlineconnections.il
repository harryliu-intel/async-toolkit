; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$




(defun InlineConnectionsGetInstancesFromInlinedInstance ( NameOfInstanceToInline
                                                          OriginalNetlistTable
                                                          TargetNetlistTable
                                                          GetNetlistTableFunc
                                                          GetNetlistTableFuncParams )
  (let (
        ( InstanceToInline ( NetlistTableGetInstance OriginalNetlistTable NameOfInstanceToInline ) ) )
    (if InstanceToInline
        (let (
              ( MasterCellName ( NetlistTableGetInstanceMasterCellName InstanceToInline ) )
              ( OriginalConnections ( NetlistTableGetInstanceConnectionsTable InstanceToInline ) ) )
          (let (
                ( MasterNetlistTable ( apply
                                       GetNetlistTableFunc
                                       ( cons
                                         MasterCellName
                                         GetNetlistTableFuncParams ) ) ) )
            (if ( tablep MasterNetlistTable )
                (let (
                      ( MasterCellInstances ( NetlistTableGetInstancesTable MasterNetlistTable ) ) )
                  ( foreach
                    InstanceName
                    MasterCellInstances
                    (let (
                          ( NewInstanceName ( sprintf
                                              nil
                                              "%s.%s"
                                              NameOfInstanceToInline
                                              InstanceName ) ) 
                          ( CurrInstance ( NetlistTableGetInstance MasterNetlistTable InstanceName ) ) )
                      (let (
                            ( NewInstance ( NetlistTableCreateInstanceInNetlist
                                            TargetNetlistTable
                                            NewInstanceName
                                            ( NetlistTableGetInstanceMasterLibName CurrInstance )
                                            ( NetlistTableGetInstanceMasterCellName CurrInstance ) ) ) )
                        ( foreach
                          ParamPair
                          ( NetlistTableGetInstanceParametersPairs CurrInstance )
                          (let (
                                ( ParamName ( car ParamPair ) )
                                ( ParamValue ( cadr ParamPair ) ) )
                            ( NetlistTableAddParameterToInstance NewInstance ParamName ParamValue ) ) )
                        ( foreach
                          ConnectionPair
                          ( NetlistTableGetInstanceConnectionPairs CurrInstance )
                          (let (
                                ( TerminalName ( car ConnectionPair ) )
                                ( NetNameInInlinedInstance ( cadr ConnectionPair ) ) )
                            (let (
                                  ( NetNameInContainingCell ( NetlistTableGetNetNameForInstanceTerminalName
                                                              InstanceToInline
                                                              NetNameInInlinedInstance ) ) )
                              (if NetNameInContainingCell
                                  ( NetlistTableAddConnectionToInstance
                                    NewInstance
                                    TerminalName
                                    NetNameInContainingCell )
                                (let (
                                      ( NewNetName ( sprintf nil "%s.%s" NameOfInstanceToInline NetNameInInlinedInstance ) ) )
                                  ( NetlistTableAddNet TargetNetlistTable NewNetName )
                                  ( NetlistTableAddConnectionToInstance NewInstance TerminalName NewNetName ) ) ) ) ) ) ) ) )
                  nil )
              MasterNetlistTable ) ) )
      ( sprintf nil "Unable to find the instance \"%s\" NetlistTable." NameOfInstanceToInline ) ) ) )

(defun InlineConnectionsTransformNetlistTable ( OriginalNetlistTable
                                                GetNetlistTableFunc
                                                GetNetlistTableFuncParams
                                                ShouldInlineConnectionsFunc
                                                ShouldInlineConnectionsFuncParams )
  (let (
        ( ErrorStr nil )
        ( LibName ( NetlistTableGetLibName OriginalNetlistTable ) )
        ( CellName ( NetlistTableGetCellName OriginalNetlistTable ) ) )
    (let (
          ( NewNetlistTable ( NetlistTableCreateEmptyNetlistTable
                              ( NetlistTableGetLibName OriginalNetlistTable )
                              ( NetlistTableGetCellName OriginalNetlistTable ) ) ) )
      (let (
            ( OriginalInstanceTripples ( NetlistTableGetInstanceTripples OriginalNetlistTable ) ) )
        (unless ErrorStr
          ( foreach
            InstanceTripple
            OriginalInstanceTripples
            (let (
                  ( InstanceName ( car InstanceTripple ) )
                  ( InstanceMasterLibName ( cadr InstanceTripple ) )
                  ( InstanceMasterCellName ( caddr InstanceTripple ) ) )
              (if ( apply
                    ShouldInlineConnectionsFunc
                    ( cons
                      LibName
                      ( cons
                        CellName
                        ( cons
                          InstanceName
                          ( cons
                            InstanceMasterLibName
                            ( cons
                              InstanceMasterCellName
                              ShouldInlineConnectionsFuncParams ) ) ) ) ) )
                  ( setq 
                    ErrorStr 
                    ( InlineConnectionsGetInstancesFromInlinedInstance
                      InstanceName
                      OriginalNetlistTable
                      NewNetlistTable
                      GetNetlistTableFunc
                      GetNetlistTableFuncParams ) )
                ( NetlistTableCopyInstanceInToNetlist
                  ( NetlistTableGetInstance
                    OriginalNetlistTable
                    InstanceName )
                  NewNetlistTable
                  InstanceName ) ) ) ) ) )
      ( foreach
        NetName
        ( NetlistTableGetNetNames OriginalNetlistTable )
        ( NetlistTableAddNet NewNetlistTable NetName ) )
      ( foreach
        TerminalName
        ( NetlistTableGetTerminalNames OriginalNetlistTable )
        ( NetlistTableAddTerminal NewNetlistTable TerminalName ) )
      ( foreach
        StatisticPair
        ( NetlistTableGetStatisticsPairs OriginalNetlistTable )
        ( NetlistTableAddStatistic
          NewNetlistTable
          ( car StatisticPair )
          ( cadr StatisticPair ) ) )
      (if ErrorStr
          ErrorStr
        NewNetlistTable ) ) ) )


(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransformMakeStackFrame ( NetlistTable
                                                                                 InstanceTripples )
  ( list NetlistTable InstanceTripples ) )

(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransformPush ( Stack NetlistTable InstanceTripples )
  ( cons
    ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformMakeStackFrame NetlistTable InstanceTripples )
    Stack ) )

(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartPush ( Stack
                                                                            VisitedTable
                                                                            NetlistTable
                                                                            ShouldInlineConnectionsFunc
                                                                            ShouldInlineConnectionsFuncParam )
  (let (
        ( LibName ( NetlistTableGetLibName NetlistTable ) )
        ( CellName ( NetlistTableGetCellName NetlistTable ) )
        ( InstanceTripples ( NetlistTableGetInstanceTripples NetlistTable ) ) )
    (if ( arrayref VisitedTable ( list LibName CellName ) )
        Stack
      (let (
            ( InstanceTripplesToVisit ( setof
                                        InstanceTripple
                                        InstanceTripples
                                        (let (
                                              ( InstanceName ( car InstanceTripple ) )
                                              ( InstanceMasterLibName ( cadr InstanceTripple ) )
                                              ( InstanceMasterCellName ( caddr InstanceTripple ) ) )
                                          ( and
                                            ( not
                                              ( arrayref 
                                                VisitedTable
                                                ( list 
                                                  InstanceMasterLibName 
                                                  InstanceMasterCellName ) ) )
                                            ( apply
                                              ShouldInlineConnectionsFunc
                                                ( cons
                                                  LibName
                                                  ( cons
                                                    CellName
                                                    ( cons
                                                      InstanceName
                                                      ( cons
                                                        InstanceMasterLibName
                                                        ( cons
                                                          InstanceMasterCellName
                                                          ShouldInlineConnectionsFuncParams ) ) ) ) ) ) ) ) ) ) )
        ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformPush
          Stack
          NetlistTable
          InstanceTripplesToVisit ) ) ) ) )

(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartGetCurrNetlistTable ( Stack )
  ( car ( car Stack ) ) )

(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartGetCurrInstanceTripple ( Stack )
  ( car ( cadr ( car Stack ) ) ) )

(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartNextInstanceTripple ( Stack )
  ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformPush
    ( cdr Stack )
    ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartGetCurrNetlistTable Stack )
    ( cdr ( cadr ( car Stack ) ) ) ) )

(defun InlineConnectionsGetOrderedListOfNetlistTablesToTransform ( RootNetlistTable
                                                                   SkillNetlistDir
                                                                   ShouldInlineConnectionsFunc
                                                                   ShouldInlineConnectionsFuncParams )
  (let (
        ( Result nil )
        ( ErrorStr nil )
        
        ( VisitedTable ( makeTable "visitedTable" nil ) ) )
    (let (
          ( Stack ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartPush
                    nil
                    VisitedTable
                    RootNetlistTable
                    ShouldInlineConnectionsFunc
                    ShouldInlineConnectionsFuncParams ) ) )
      (while ( and ( null ErrorStr ) Stack )
        (let (
              ( CurrNetlistTable ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartGetCurrNetlistTable
                                   Stack ) )
              ( CurrInstanceTripple ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartGetCurrInstanceTripple
                                      Stack ) ) )
          (if CurrInstanceTripple
              (let (
                    ( CurrInstanceName ( car CurrInstanceTripple ) )
                    ( CurrInstanceMasterLibName ( cadr CurrInstanceTripple ) )
                    ( CurrInstanceMasterCellName ( caddr CurrInstanceTripple ) ) )
                (let (
                      ( MasterNetlistTable ( NetlistTableGetSkillNetlistTableForCellName
                                             CurrInstanceMasterCellName
                                             SkillNetlistDir ) ) )
                  (if ( tablep MasterNetlistTable )
                      (let ()
                        ( setq 
                          Stack 
                          ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartNextInstanceTripple Stack ) )
                        ( setq
                          Stack
                          ( InlineConnectionsGetOrderedListOfNetlistTablesToTransformSmartPush 
                            Stack
                            VisitedTable
                            MasterNetlistTable
                            ShouldInlineConnectionsFunc
                            ShouldInlineConnectionsFuncParams ) ) )
                    ( setq ErrorStr MasterNetlistTable ) ) ) )
            (let (
                  ( LibName ( NetlistTableGetLibName CurrNetlistTable ) )
                  ( CellName ( NetlistTableGetCellName CurrNetlistTable ) ) )
              ( setq Result ( tconc Result CurrNetlistTable ) )
              ( setarray VisitedTable ( list LibName CellName ) t )
              ( setq Stack ( cdr Stack ) ) ) ) ) ) )
    ( car Result ) ) )

(defun InlineConnectionsGetTableOfTransformedNetlistTables ( RootNetlistTable
                                                             SkillNetlistDir
                                                             ShouldInlineConnectionsFunc
                                                             ShouldInlineConnectionsFuncParams )
  (let (
        ( ErrorStr nil )
        ( NetlistTablesToTransform ( InlineConnectionsGetOrderedListOfNetlistTablesToTransform
                                     RootNetlistTable
                                     SkillNetlistDir
                                     ShouldInlineConnectionsFunc
                                     ShouldInlineConnectionsFuncParams ) )
        ( TableOfNetlistTables ( makeTable "netlistTablesTable" nil ) ) )
    ( foreach
      NetlistTable
      NetlistTablesToTransform
      (unless ErrorStr
        (let (
              ( TransformedNetlistTable ( InlineConnectionsTransformNetlistTable
                                          NetlistTable
                                          (lambda ( CellName TableOfNetlistTables )
                                            (let (
                                                  ( NetlistTable ( arrayref TableOfNetlistTables CellName ) ) )
                                              (if NetlistTable
                                                  NetlistTable
                                                ( sprintf
                                                  nil
                                                  "%L has not been transformed yet."
                                                  CellName ) ) ) )
                                          ( list TableOfNetlistTables )
                                          ShouldInlineConnectionsFunc
                                          ShouldInlineConnectionsFuncParams ) ) )
          (if ( tablep TransformedNetlistTable )
              (let ()
                ( setarray
                  TableOfNetlistTables 
                  ( NetlistTableGetCellName TransformedNetlistTable )
                  TransformedNetlistTable ) )
            ( setq ErrorStr TransformedNetlistTable  ) ) ) ) )
    (if ErrorStr
        ErrorStr
      TableOfNetlistTables ) ) )

(defun InlineConnectionsHierTransformTable ( RootNetlistTable 
                                             ShouldInlineConnectionsFunc
                                             ShouldInlineConnectionsFuncParams
                                             SkillNetlistDir )
  (let (
        ( ErrorStr nil )
        ( TableOfNetlistTables ( InlineConnectionsGetTableOfTransformedNetlistTables
                                 RootNetlistTable
                                 SkillNetlistDir
                                 ShouldInlineConnectionsFunc
                                 ShouldInlineConnectionsFuncParams ) ) )
    (when ( not ( tablep TableOfNetlistTables ) )
      ( setq ErrorStr TableOfNetlistTables ) )
    (if ErrorStr
        ErrorStr
      (let (
            ( TransformedNetlistTable ( arrayref
                                        TableOfNetlistTables
                                        ( NetlistTableGetCellName RootNetlistTable ) ) ) )
        (if ( tablep TransformedNetlistTable )
            TransformedNetlistTable
          ( sprintf
            nil
            "Root Netlist Table for %L was never transformed."
            ( NetlistTableGetCellName RootNetlistTable ) ) ) ) ) ) )
    


(defun InlineConnectionsGetNetlistTable ( CellName 
                                          SkillNetlistDir 
                                          ShouldInlineConnectionsFunc
                                          ShouldInlineConnectionsFuncParams )
  (let (
        ( OriginalNetlistTable ( NetlistTableGetSkillNetlistTableForCellName CellName SkillNetlistDir ) ) )
    (if ( tablep OriginalNetlistTable ) 
        ( InlineConnectionsTransformNetlistTable 
          OriginalNetlistTable 
          SkillNetlistDir 
          ShouldInlineConnectionsFunc 
          ShouldInlineConnectionsFuncParams )
      OriginalNetlistTable ) ) )

            
                
