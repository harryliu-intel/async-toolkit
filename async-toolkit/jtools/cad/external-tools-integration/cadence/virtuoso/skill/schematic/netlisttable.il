; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun NetlistTableGetCellName ( NetlistTable )
  NetlistTable["cn"]  
)

(defun NetlistTableGetLibName ( NetlistTable )
  NetlistTable["ln"]
)

(defun NetlistTableGetInstancesTable ( NetlistTable )
  NetlistTable["i"] 
)

(defun NetlistTableGetInstanceTripples ( NetlistTable )
  ( ListApplyFuncToListAndAccumulateResults
    ( tableToList ( NetlistTableGetInstancesTable NetlistTable ) )
    (lambda
      ( TableEntry )
      ( list
        ( car TableEntry )
        ( NetlistTableGetInstanceMasterLibName ( cadr TableEntry ) )
        ( NetlistTableGetInstanceMasterCellName ( cadr TableEntry ) ) ) )
    nil ) )

(defun NetlistTableGetInstance ( NetlistTable InstanceName )
  NetlistTable["i"][InstanceName]
)

(defun NetlistTableGetInstanceMasterLibName ( Instance )
  (when Instance Instance["ln"]  ) )

(defun NetlistTableGetInstanceInNetlistMasterLibName ( NetlistTable InstanceName )
  ( NetlistTableGetInstanceMasterLibName NetlistTable["i"][InstanceName] )
)

(defun NetlistTableGetInstanceMasterCellName ( Instance )
  (when Instance Instance["cn"] ) 
)

(defun NetlistTableGetInstanceInNetlistMasterCellName ( NetlistTable InstanceName )
  ( NetlistTableGetInstanceMasterCellName NetlistTable["i"][InstanceName] )
)

(defun NetlistTableGetInstanceParametersTable ( Instance )
  (when Instance Instance["p"] ) 
)

(defun NetlistTableGetInstanceParametersPairs ( Instance )
  ( tableToList ( NetlistTableGetInstanceParametersTable Instance ) ) )

(defun NetlistTableGetInstanceInNetlistParametersPairs ( NetlistTable InstanceName )
  ( NetlistTableGetInstanceParametersPairs NetlistTable["i"][InstanceName] )
)

(defun NetlistTableGetInstanceParameterValue ( Instance ParameterName )
  (when Instance
    ( arrayref ( NetlistTableGetInstanceParametersTable Instance ) ParameterName ) ) )

(defun NetlistTableGetInstanceConnectionsTable ( Instance )
  (when Instance Instance["c"] ) 
)

(defun NetlistTableGetInstanceConnectionPairs ( Instance )
  ( tableToList ( NetlistTableGetInstanceConnectionsTable Instance ) ) )

(defun NetlistTableGetInstanceInNetlistConnectionPairs ( NetlistTable InstanceName )
  ( NetlistTableGetInstanceConnectionPairs
    ( NetlistTableGetInstance NetlistTable InstanceName ) ) )

(defun NetlistTableGetNetNameForInstanceTerminalName ( Instance TerminalName )
  ( arrayref ( NetlistTableGetInstanceConnectionsTable Instance ) TerminalName ) )

(defun NetlistTableGetNetNameForInstanceInNetlistTerminalName ( NetlistTable InstanceName TerminalName )
  ( NetlistTableGetNetNameForInstanceTerminalName
    ( NetlistTableGetInstance NetlistTable InstanceName )
    TerminalName ) )

(defun NetlistTableGetNetsTable ( NetlistTable )
  ( arrayref NetlistTable "n" ) )

(defun NetlistTableGetNetNames ( NetlistTable )
  ( ListApplyFuncToListAndAccumulateResults
    ( tableToList ( NetlistTableGetNetsTable NetlistTable ) )
    (lambda
      ( NetEntry )
      ( car NetEntry ) )
    nil ) )

(defun NetlistTableGetTerminalNames ( NetlistTable )
  ( ListApplyFuncToListAndAccumulateResults
    ( setof
      NetEntry
      ( tableToList ( NetlistTableGetNetsTable NetlistTable ) )
      ( car ( cadr NetEntry ) ) )
    (lambda
      ( NetEntry )
      ( car NetEntry ) )
    nil ) )

(defun NetlistTableNetExists ( NetlistTable NetName )
  ( arrayref ( NetlistTableGetNetsTable NetlistTable ) NetName ) )

(defun NetlistTableTerminalExists ( NetlistTable TerminalName )
  ( car ( arrayref ( NetlistTableGetNetsTable NetlistTable ) TerminalName ) ) )


(defun NetlistTableGetStatisticsTable ( NetlistTable )
  ( arrayref NetlistTable "s" ) )

(defun NetlistTableGetStatistic ( NetlistTable StatisticName )
  ( arrayref ( NetlistTableGetStatisticsTable NetlistTable ) StatisticName ) )

(defun NetlistTableGetStatisticsPairs ( NetlistTable )
  ( tableToList ( NetlistTableGetStatisticsTable NetlistTable ) ) )

(defun NetlistTableGetTotalTransistorGateArea ( NetlistTable )
  ( NetlistTableGetStatistic NetlistTable "a" ) )

(defun NetlistTableGetTotalTransistorWidth ( NetlistTable )
  ( NetlistTableGetStatistic NetlistTable "w" ) )

(defun NetlistTableGetTotalTransistorLength ( NetlistTable )
  ( NetlistTableGetStatistic NetlistTable "l" ) )

(defun NetlistTableGetTotalTransistorCount ( NetlistTable )
  ( NetlistTableGetStatistic NetlistTable "c" ) )

(defun NetlistTableGetCellWidth ( NetlistTable )
  ( NetlistTableGetStatistic NetlistTable "cw" ) )

(defun NetlistTableGetCellHeight ( NetlistTable )
  ( NetlistTableGetStatistic NetlistTable "ch" ) )




(defun NetlistTableCreateEmptyNetlistTable ( LibName CellName )
  (let (
        ( NewNetlistTable ( makeTable "skillNetlistTable" nil ) ) )
    ( setarray NewNetlistTable "ln" LibName )
    ( setarray NewNetlistTable "cn" CellName )
    ( setarray NewNetlistTable "i" ( makeTable "instancesTable" nil ) )
    ( setarray NewNetlistTable "n" ( makeTable "netsTable" nil ) )
    ( setarray NewNetlistTable "s" ( makeTable "statisticsTable" nil ) )
    NewNetlistTable ) )

(defun NetlistTableAddTerminal ( NetlistTable TerminalName )
  ( setarray ( NetlistTableGetNetsTable NetlistTable ) TerminalName ( list t ) ) )

(defun NetlistTableAddNet ( NetlistTable NetName )
  ( setarray ( NetlistTableGetNetsTable NetlistTable ) NetName ( list nil ) ) )

(defun NetlistTableCreateInstance ( InstanceName MasterLibName MasterCellName )
  (let (
        ( NewInstance ( makeTable "instanceTable" nil ) ) )
    ( setarray NewInstance "n" InstanceName )
    ( setarray NewInstance "ln" MasterLibName )
    ( setarray NewInstance "cn" MasterCellName )
    ( setarray NewInstance "c" ( makeTable "connectionsTable" nil ) )
    ( setarray NewInstance "p" ( makeTable "parametersTable" nil ) )
    NewInstance ) )

(defun NetlistTableCreateInstanceInNetlist ( NetlistTable InstanceName MasterLibName MasterCellName )
  (let (
        ( NewInstance ( NetlistTableCreateInstance
                        InstanceName
                        MasterLibName
                        MasterCellName ) ) )
    ( setarray
      ( NetlistTableGetInstancesTable
        NetlistTable )
      InstanceName
      NewInstance )
    NewInstance ) )

(defun NetlistTableAddConnectionToInstance ( Instance TerminalName NetName )
  ( setarray
    ( NetlistTableGetInstanceConnectionsTable Instance )
    TerminalName
    NetName ) )

(defun NetlistTableAddConnectionToInstanceInNetlist ( NetlistTable InstanceName TerminalName NetName )
  (unless ( NetlistTableNetExists NetlistTable NetName )
    ( NetlistTableAddNet NetlistTable NetName ) )
  ( NetlistTableAddConnectionToInstance
    ( NetlistTableGetInstance
      NetlistTable
      InstanceName )
    TerminalName
    NetName ) )

(defun NetlistTableAddParameterToInstance ( Instance ParamName ParamValue )
  ( setarray
    ( NetlistTableGetInstanceParametersTable
      Instance )
    ParamName
    ParamValue ) )

(defun NetlistTableAddParameterToInstanceInNetlist ( NetlistTable InstanceName ParamName ParamValue )
  ( NetlistTableAddParameterToInstance
    ( NetlistTableGetInstance NetlistTable InstanceName )
    ParamName
    ParamValue ) )

(defun NetlistTableAddStatistic ( NetlistTable StatisticName StatisticValue )
  ( setarray
    ( NetlistTableGetStatisticsTable NetlistTable )
    StatisticName
    StatisticValue ) )

(defun NetlistTableSetTotalTransistorGateArea ( NetlistTable TotalGateArea )
  ( NetlistTableAddStatistic NetlistTable "a" TotalGateArea ) )

(defun NetlistTableSetTotalTransistorWidth ( NetlistTable TotalTransistorWidth )
  ( NetlistTableAddStatistic NetlistTable "w" TotalTransistorWidth ) )

(defun NetlistTableSetTotalTransistorLength ( NetlistTable TotalTransistorLength )
  ( NetlistTableAddStatistic NetlistTable "l" TotalTransistorLength ) )

(defun NetlistTableSetTotalTransistorCount ( NetlistTable TotalTransistorCount )
  ( NetlistTableAddStatistic NetlistTable "c" TotalTransistorCount ) )

(defun NetlistTableSetCellWidth ( NetlistTable CellWidth )
  ( NetlistTableAddStatistic NetlistTable "cw" CellWidth ) )

(defun NetlistTableSetCellHeight ( NetlistTable CellHeight )
  ( NetlistTableAddStatistic NetlistTable "ch" CellHeight ) )



(defun NetlistTableCopyInstanceInToNetlist ( SrcInstance TargetNetlistTable TargetIntanceName )
  (let (
        ( InstanceMasterLibName ( NetlistTableGetInstanceMasterLibName SrcInstance ) )
        ( InstanceMasterCellName ( NetlistTableGetInstanceMasterCellName SrcInstance ) ) 
        ( ParamPairs ( NetlistTableGetInstanceParametersPairs SrcInstance ) )
        ( ConnectionPairs ( NetlistTableGetInstanceConnectionPairs SrcInstance ) ) )
    (let (
          ( NewInstance ( NetlistTableCreateInstanceInNetlist
                          TargetNetlistTable
                          TargetIntanceName
                          InstanceMasterLibName
                          InstanceMasterCellName ) ) )
      ( foreach
        ParamPair
        ParamPairs
        (let (
              ( ParamName ( car ParamPair ) )
              ( ParamValue ( cadr ParamPair ) ) )
          ( NetlistTableAddParameterToInstance NewInstance ParamName ParamValue ) ) )

      ( foreach
        ConnectionPair
        ConnectionPairs
        (let (
              ( TerminalName ( car ConnectionPair ) )
              ( NetName ( cadr ConnectionPair ) ) )
          (unless ( NetlistTableNetExists TargetNetlistTable NetName )
            ( NetlistTableAddNet TargetNetlistTable NetName ) )
          ( NetlistTableAddConnectionToInstance
            NewInstance
            TerminalName
            NetName ) ) )
      NewInstance ) ) )





(defun NetlistTableGetSkillNetlistForCellName ( CellName SkillNetlistDir )
  ( sprintf nil "%s/%s.netlist.il" SkillNetlistDir CellName ) )


(defun NetlistTableGetSkillNetlistTableForCellName ( CellName SkillNetlistDir )
  (let (
        ( SkillNetlistFileName ( NetlistTableGetSkillNetlistForCellName
                                 CellName
                                 SkillNetlistDir ) ) )
    (if ( isReadable
          SkillNetlistFileName )
        (let ()
          (defvar NetListTable nil )
          ( load SkillNetlistFileName )
          (if NetListTable
              NetListTable 
            ( sprintf
              nil
              "Could not load %L even though it is readable."
              SkillNetlistFileName ) ) )
      ( sprintf
        nil
        "%L is not a readable file."
        SkillNetlistFileName ) ) ) )
      
